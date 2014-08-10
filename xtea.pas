{ TEAenc implementation as found in the original AVRootloader.txt
  unmodified, just fixed the weird missing pointer dereferencing,
  Delphi allows such confusing wrongness, Free Pascal insists on
  properly written and formally correct code.
}
unit XTEA;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure TEAEnc(const Source; var Dest; var Feedback; const Key);

implementation

procedure TEAEnc(const Source; var Dest; var Feedback; const Key);
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

end.

