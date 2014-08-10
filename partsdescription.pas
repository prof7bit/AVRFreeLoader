unit PartsDescription;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPartDescription = record
    Signature: Integer;
    Name: String;
    Flash: Integer;
    EEprom: Integer;
    PageSize: Integer;
    HasBootSection: Boolean;
  end;

function GetPartDescription(Signature: UInt16): TPartDescription;

implementation

function GetPartDescription(Signature: UInt16): TPartDescription;
var
  List: TStringList;
  Line: TStringList;
  SLine: String;
  SSig: String;
  Key: String;
begin
  Result.Signature := -1;
  Result.Name := 'not found';
  SSig := LowerCase(IntToHex(Signature, 4));
  List := TStringList.Create;
  Line := TStringList.Create;
  Line.Delimiter := ',';
  try
    List.LoadFromFile('PartsDescription.csv');
    for SLine in List do begin
      Line.DelimitedText := SLine;
      Key := Line.Strings[0];
      if Length(Key) > 3 then begin
        if LowerCase(LeftStr(Key, 4)) = SSig then begin
          Result.Signature := Signature;
          Result.Name := Line.Strings[1];
          Result.Flash := StrToInt(Line.Strings[2]);
          Result.EEprom := StrToInt(Line.Strings[3]);
          Result.PageSize := StrToInt(Line.Strings[6]);
          Result.HasBootSection := StrToInt(Line.Strings[7]) <> 0;
          break;
        end;
      end;
    end;
  except
    //
  end;
  Line.Free;
  List.Free;
end;

end.

