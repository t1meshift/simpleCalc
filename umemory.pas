unit UMemory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMemory }

  TMemory = class
    Value: extended;
    constructor Create;
    procedure Save(Val: extended);
    function Read: extended;
    procedure Clear;
    procedure Add(Val: extended);
    procedure Sub(Val: extended);
  end;

var
  Memory: TMemory;

implementation

constructor TMemory.Create;
begin
  Value := 0;
end;

procedure TMemory.Save(Val: extended);
begin
  Value := Val;
end;

function TMemory.Read: extended;
begin
  Result := Value;
end;

procedure TMemory.Clear;
begin
  Value := 0;
end;

procedure TMemory.Add(Val: extended);
begin
  Value := Value + Val;
end;

procedure TMemory.Sub(Val: extended);
begin
  Value := Value - Val;
end;

initialization
  Memory := TMemory.Create;
end.

