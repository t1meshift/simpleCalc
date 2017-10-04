unit UMemory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMemory }

  TMemory = class
    Value: double;
    constructor Create;
    procedure Save(Val: double);
    function Read: double;
    procedure Clear;
    procedure Add(Val: double);
    procedure Sub(Val: double);
  end;

var
  Memory: TMemory;

implementation

constructor TMemory.Create;
begin
  Value := 0;
end;

procedure TMemory.Save(Val: double);
begin
  Value := Val;
end;

function TMemory.Read: double;
begin
  Result := Value;
end;

procedure TMemory.Clear;
begin
  Value := 0;
end;

procedure TMemory.Add(Val: double);
begin
  Value := Value + Val;
end;

procedure TMemory.Sub(Val: double);
begin
  Value := Value - Val;
end;

initialization
  Memory := TMemory.Create;
end.

