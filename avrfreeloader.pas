{ Free implementation of the proprietary "AVRootLoader" protocol.

  Copyright (C) 2014 Bernd Kreuss <prof7bit@gmail.com>

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General
  Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option)
  any later version with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with
  independent modules to produce an executable, regardless of the license terms of these independent modules,and to
  copy and distribute the resulting executable under terms of your choice, provided that you also meet, for each
  linked independent module, the terms and conditions of the license of that module. An independent module is a module
  which is not derived from or based on this library. If you modify this library, you may extend this exception to
  your version of the library, but you are not obligated to do so. If you do not wish to do so, delete this exception
  statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
  details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to
  the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit AVRFreeLoader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComPort, syncobjs, PartsDescription, XTEA;

const
  DAY           = 1;
  HOUR          = DAY / 24;
  MINUTE        = HOUR / 60;
  SECOND        = MINUTE / 60;
  MILLI_SECOND  = SECOND / 1000;

  INTERVAL_SEND_BOOTSIGN        = 300 * MILLI_SECOND;
  INTERVAL_SEND_KEEPALIVE       = 300 * MILLI_SECOND;
  TIME_WAIT_AFTER_LAST_RECEIVE  =  30 * MILLI_SECOND;
  TIMEOUT_KEEPALIVE             =   2 * INTERVAL_SEND_KEEPALIVE;

  // return codes from bootloader
  SUCCESS             = $30;
  ERRORVERIFY         = $C0;
  ERRORCOMMAND        = $C1;
  ERRORCRC            = $C2;
  ERRORBOUNDS         = $C3;
  ERRORCRYPT          = $C4;
  ERRORPROG           = $C5;
  ERRORVERSION        = $C6;
  ERRORUNKNOWN        = $CF;

  // these flags may be set in the lower
  // nibble of the return code of the
  // bootsign response message.
  HAS_CRYPT           = $01;
  CRYPT_FLASH         = $02;
  CRYPT_EEPROM        = $04;
  HAS_VERSIONING      = $08;

type
  TConsoleCallback = procedure(Msg: String) of Object;
  TWorkerThread = class;
  TState = (
    stDisconnected,
    stConnecting,
    stConnected,
    stDisconnecting
  );

  { TAVRFreeLoader }

  TAVRFreeLoader = class(TComponent)
    constructor Create(AOwner: TComponent); override;
    procedure Connect;
    procedure Disconnect;
  public
    Callback: TConsoleCallback;
    Port: String;
    Baud: Integer;
    BootSign: String;
    Password: String;
    destructor Destroy; override;
  private
    WorkerThread: TWorkerThread;
    ComPort: TSimpleComPort;
    procedure Print(Txt: String);
  end;

  { TWorkerThread }

  TWorkerThread = class(TThread)
    FreeLoader: TAVRFreeLoader;
    ComPort: TSimpleComPort;
    State: TState;
    constructor Create(AFreeLoader: TAVRFreeLoader);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Lock;
    procedure Unlock;
  private
    Part: TPartDescription;
    AppVer: UInt32;
    BootMsg: String;
    BootVersion: Byte;
    BootPages: Byte;
    FLock: TCriticalSection;
    TimeLastReceive: Double;
    TimeLastBootsign: TDateTime;
    TimeLastKeepalive: TDateTime;
    TimeLastKeepaliveResponse: TDateTime;
    SyncPrintText: String;
    ReceiveBuffer: String;
    ReceivedMessage: String;
    function GetReceivedMessage: String;
    procedure CheckAction;
    procedure Print(Txt: String);
    procedure SyncPrint;
    procedure OnBootsignResponse(Data: String);
    procedure OnKeepaliveResponse(Data: String);
    procedure SendBootsign;
    procedure SendKeepalive;
    function AddCRC(Data: String): String;
    procedure SendWithCRC(Data: String);
    procedure Send(Data: String);
  end;

implementation

procedure CRC16_Update(var CRC: Word; Data: Byte);
var
  I: Integer;
begin
  CRC := CRC xor (Word(Data));
  for I := 0 to 7 do begin
    if (CRC and 1) <> 0 then
      CRC := (CRC shr 1) xor $A001
    else
      CRC := CRC shr 1;
  end;
end;

function FormatHex(Binary: String): String;
var
  C: Char;
begin
  Result := '';
  for C in Binary do begin
    Result += IntToHex(Ord(C), 2) + ' ';
  end;
end;

{ TWorkerThread }

constructor TWorkerThread.Create(AFreeLoader: TAVRFreeLoader);
begin
  inherited Create(True);
  FLock := TCriticalSection.Create;
  FreeLoader := AFreeLoader;
  ComPort := FreeLoader.ComPort;
  State := stDisconnected;
end;

destructor TWorkerThread.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TWorkerThread.Execute;
var
  B: Byte = 0;
begin
  repeat
    if ComPort.IsOpen then begin
      if ComPort.Receice(1, B) = 1 then begin
        ReceiveBuffer += Chr(B);
        TimeLastReceive := Now;
      end;
    end
    else begin
      Sleep(100);
    end;
    if Length(ReceiveBuffer) > 0 then begin
      if Now - TimeLastReceive > TIME_WAIT_AFTER_LAST_RECEIVE then begin
        ReceivedMessage := ReceiveBuffer;
        ReceiveBuffer := '';
      end;
    end;
    Lock;
    CheckAction;
    Unlock;
  until Terminated;
end;

procedure TWorkerThread.Lock;
begin
  FLock.Acquire;
end;

procedure TWorkerThread.Unlock;
begin
  FLock.Release;
end;

function TWorkerThread.GetReceivedMessage: String;
begin
  Result := ReceivedMessage;
  ReceivedMessage := '';
end;

procedure TWorkerThread.CheckAction;
var
  S: TState;
  Msg: String;
begin
  S := State;

  if S = stConnecting then begin
    if Now - TimeLastBootsign > INTERVAL_SEND_BOOTSIGN then
      SendBootsign;
    Msg := GetReceivedMessage;
    if Msg <> '' then
      OnBootsignResponse(Msg);
  end

  else if S = stConnected then begin
    if Now - TimeLastKeepalive > INTERVAL_SEND_KEEPALIVE then
      SendKeepalive;
    Msg := GetReceivedMessage;
    if Msg <> '' then
      OnKeepaliveResponse(Msg);
    if (Now - TimeLastKeepaliveResponse) > TIMEOUT_KEEPALIVE then begin
      Print('keepalive timeout');
      State := stDisconnecting;
    end;
  end

  else if S = stDisconnecting then begin
    SendWithCRC(#0#1);
    ComPort.Close;
    State := stDisconnected;
    Print('disconnected');
  end;
end;

procedure TWorkerThread.Print(Txt: String);
begin
  SyncPrintText := Txt;
  Synchronize(@SyncPrint);
end;

procedure TWorkerThread.SyncPrint;
begin
  FreeLoader.Print(SyncPrintText);
end;

procedure TWorkerThread.OnBootsignResponse(Data: String);
var
  RetCode: Byte;
  L: Integer;
  Signature: UInt16;
  OK: Boolean = True;

  procedure InvalidResponse;
  begin
    Print('invalid response from Bootloader: ' + FormatHex(Data));
    State := stDisconnecting;
    OK := False;
  end;

  procedure OldVersion;
  begin
    Print(Format('bootloader version too old: %d (sorry)', [BootVersion]));
    State := stDisconnecting;
    OK := False;
  end;

begin
  L := Length(Data);
  if L < 5 then
    InvalidResponse;

  RetCode := Ord(Data[L]);
  if (RetCode and $F0) <> SUCCESS then
    InvalidResponse;

  if (RetCode and HAS_VERSIONING) <> 0 then begin
    // 3: BootMsg + BootInfo + Version + SUCCESS
    if L < 9 then
      InvalidResponse;

    AppVer := Ord(Data[L-4]) or Ord(Data[L-3]) shl 8 or Ord(Data[L-2]) shl 16 or Ord(Data[L-1]) shl 24;
    BootVersion := Ord(Data[L-6]);
    BootPages := Ord(Data[L-5]);
    Signature := Ord(Data[L-7]) or Ord(Data[L-8]) shl 8;
    SetString(BootMsg, PChar(Data), L-9);
  end

  else begin
    if L = 5 then begin
      // BootInfo + SUCCESS
      BootVersion := Ord(Data[3]);
      if BootVersion < 2 then
        OldVersion;

      BootPages := Ord(Data[4]);
      Signature := Ord(Data[2]) or Ord(Data[1]) shl 8;
    end

    else if (Data[5] = Data[L]) and (Ord(Data[3]) = 2) then begin
      // 2: BootInfo + SUCCESS + BootMsg + SUCCESS
      BootVersion := Ord(Data[3]);
      BootPages := Ord(Data[4]);
      Signature := Ord(Data[2]) or Ord(Data[1]) shl 8;
      SetString(BootMsg, PChar(@Data[6]), L-6);
    end
    else begin
      // 3: BootMsg + BootInfo + SUCCESS
      BootVersion := Ord(Data[L-2]);
      if BootVersion < 3 then
        InvalidResponse;

      BootPages := Ord(Data[L-1]);
      Signature := Ord(Data[L-3]) or Ord(Data[L- 4]) shl 8;
      SetString(BootMsg, PChar(Data), L-5);
    end;
  end;

  if BootVersion < 2 then
    OldVersion;

  if OK then begin
    Part := GetPartDescription(Signature);
    if Part.Signature = Signature then begin
      Print('connected to ' + Part.Name);
      State := stConnected;
      TimeLastKeepaliveResponse := Now;
    end
    else begin
      Print(Format('Unknown part: %4x', [Signature]));
      State := stDisconnecting;
    end;
  end;
end;

procedure TWorkerThread.OnKeepaliveResponse(Data: String);
begin
  if Data <> chr(ERRORCOMMAND) then begin
    // yes, you are seeing right, the correct
    // response to keepalive must be ERRORCOMMAND!
    Print('Received invalid reply to keepalive: ' + FormatHex(Data));
    State := stDisconnecting;
  end
  else
    TimeLastKeepaliveResponse := Now;
end;

procedure TWorkerThread.SendBootsign;
var
  Data: String;
begin
  Data := FreeLoader.BootSign;
  if Length(Data) mod 2 > 0 then
    Data += #0;
  Data := #0#0#0#0#0#0#0#0#0#13 + AddCRC(Data);
  Send(Data);
  TimeLastBootsign := Now;
end;

procedure TWorkerThread.SendKeepalive;
begin
  SendWithCRC(#$fd#$00);
  TimeLastKeepalive := Now;
end;

function TWorkerThread.AddCRC(Data: String): String;
var
  CRC: UInt16;
  C: Char;
begin
  CRC := 0;
  for C in Data do begin
    CRC16_Update(CRC, Ord(C));
  end;
  Result := Data + chr(lo(CRC)) + chr(hi(CRC));
end;

procedure TWorkerThread.SendWithCRC(Data: String);
begin
  Send(AddCRC(Data));
end;

procedure TWorkerThread.Send(Data: String);
begin
  if ComPort.IsOpen then
    ComPort.Send(Data);
end;

{ TAVRFreeLoader }

constructor TAVRFreeLoader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ComPort := TSimpleComPort.Create(Self);
  WorkerThread := TWorkerThread.Create(Self);
  WorkerThread.Start;
end;

destructor TAVRFreeLoader.Destroy;
begin
  WorkerThread.Terminate;
  WorkerThread.WaitFor;
  WorkerThread.Free;
  inherited Destroy;
end;

procedure TAVRFreeLoader.Connect;
begin
  ComPort.Open(Port, Baud, 8, 'N', 2);
  if ComPort.IsOpen then begin
    WorkerThread.Lock;
    if WorkerThread.State = stDisconnected then begin
      Print('trying to connect');
      WorkerThread.State := stConnecting;
      // the actual action happens in the
      // CheckAction() method of the worker
      // thread which is called periodically
      // all the time
    end
    else
      Print('already trying to connect');
    WorkerThread.Unlock;
  end
  else
    Print('Error opening serial port :-(');
end;

procedure TAVRFreeLoader.Disconnect;
begin
  WorkerThread.Lock;
  if WorkerThread.State <> stDisconnected then begin
    WorkerThread.State := stDisconnecting;
  end;
  WorkerThread.Unlock;
end;

procedure TAVRFreeLoader.Print(Txt: String);
begin
  if Assigned(Callback) then
    Callback(Txt)
  else
    WriteLn(Txt);
end;

end.

