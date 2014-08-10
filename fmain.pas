{ Test program for the AVRFreeLoader component

  Copyright (C) 2014 Bernd Kreuss <prof7bit@gmail.com>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit FMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, StdCtrls,
  AVRFreeLoader, SynEditKeyCmds, ComPort, IniFiles;

type

  { TForm_Main }

  TForm_Main = class(TForm)
    Button_Connect: TButton;
    Button_Disconnect: TButton;
    Combo_Port: TComboBox;
    SynEdit: TSynEdit;
    procedure Button_ConnectClick(Sender: TObject);
    procedure Button_DisconnectClick(Sender: TObject);
    procedure Combo_PortChange(Sender: TObject);
    procedure Combo_PortGetItems(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Ini: TIniFile;
    RootLoader: TAVRFreeLoader;
    procedure OnDebugMessage(Msg: String);
    procedure WriteToMemo(Txt: String);
  public
  end;

var
  Form_Main: TForm_Main;

implementation

{$R *.lfm}

{ TForm_Main }

procedure TForm_Main.FormCreate(Sender: TObject);
begin
  Ini := TIniFile.Create('AVRFreeLoader.ini');
  Ini.CacheUpdates := True;
  Ini.CaseSensitive := False;
  EnumerateSerialPorts(Combo_Port.Items);
  Combo_Port.Text := Ini.ReadString('System', 'Port', '');
  RootLoader := TAVRFreeLoader.Create(Self);
  RootLoader.Callback := @OnDebugMessage;
end;

procedure TForm_Main.FormDestroy(Sender: TObject);
begin
  Ini.UpdateFile;
  Ini.Free;
end;

procedure TForm_Main.OnDebugMessage(Msg: String);
begin
  WriteToMemo(Msg);
end;

procedure TForm_Main.WriteToMemo(Txt: String);
begin
  SynEdit.Lines.Add(Txt);
  SynEdit.ExecuteCommand(ecEditorBottom, '', nil);
end;

procedure TForm_Main.Button_ConnectClick(Sender: TObject);
begin
  with RootLoader do begin
    Port := Combo_Port.Text;
    Baud := ini.ReadInteger('Bootloader', 'Baud', 9600);
    BootSign := ini.ReadString('Bootloader', 'Bootsign', 'BOOTLOADER');
    Password := ini.ReadString('Bootloader', 'Password', '');
    Connect;
  end;
end;

procedure TForm_Main.Button_DisconnectClick(Sender: TObject);
begin
  RootLoader.Disconnect;
end;

procedure TForm_Main.Combo_PortChange(Sender: TObject);
begin
  Ini.WriteString('System', 'Port', Combo_Port.Text);
  Ini.UpdateFile;
end;

procedure TForm_Main.Combo_PortGetItems(Sender: TObject);
var
  Prev: String;
begin
  Prev := Combo_Port.Text;
  EnumerateSerialPorts(Combo_Port.Items);
  Combo_Port.Text := Prev;
end;

end.

