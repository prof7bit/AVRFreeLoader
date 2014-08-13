{ encrypt data with the modified XTEA algorithm as described in
  the AVRootLoader.txt file that was found in the original zip}
unit XTEA;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure TEAEnc(const Source; out Dest; var Feedback; const Key);
function TEAEncBuffer(Source: String; const Key): String;

implementation

{ TEAenc implementation as found in the original AVRootloader.txt }
procedure TEAEnc(const Source; out Dest; var Feedback; const Key);
const
  TEA_Delta: UInt32 = $9E3779B9;
type
  TLongArray = array[0..3] of UInt32;
  PLongArray = ^TLongArray;
var
  I: Integer;
  A,B,Sum: UInt32;
  S,D,F,K: PLongArray;
begin
  Sum := 0;
  S := @Source;
  D := @Dest;
  F := @Feedback;
  K := @Key;
  A := S^[0] xor F^[0];
  B := S^[1] xor F^[1];
  for I := 0 to 15 do
  begin
    Inc(A, (((B shl 4) xor (B shr 5) + B) xor (Sum + K^[Sum and 3])));
    Inc(Sum, TEA_Delta);
    Inc(B, (((A shl 4) xor (A shr 5) + A) xor (Sum + K^[(Sum shr 11) and 3])));
  end;
  A := A xor F^[0];
  B := B xor F^[1];
  for I := 0 to 15 do
  begin
    Inc(A, (((B shl 4) xor (B shr 5) + B) xor (Sum + K^[Sum and 3])));
    Inc(Sum, TEA_Delta);
    Inc(B, (((A shl 4) xor (A shr 5) + A) xor (Sum + K^[(Sum shr 11) and 3])));
  end;
  F^[0] := F^[0] xor A;
  F^[1] := F^[1] xor B;
  D^[0] := A;
  D^[1] := B;
end;

function TEAEncBuffer(Source: String; const Key): String;
var
  Remaining: Integer;
  SrcPtr, DstPtr: ^UInt64;
  Feedback: UInt64;
begin
  Remaining := Length(Source) mod 8;
  if Remaining > 0 then begin
    SetLength(Source, Length(Source) + 8 - Remaining);
  end;

  Remaining := Length(Source);
  SetLength(Result, Remaining);
  SrcPtr := @Source[1];
  DstPtr := @Result[1];
  Feedback := 0;
  while Remaining > 0 do begin
    TEAEnc(SrcPtr^, DstPtr^, Feedback, Key);
    Inc(SrcPtr);
    Inc(DstPtr);
    Dec(Remaining, 8);
  end;
end;

end.

