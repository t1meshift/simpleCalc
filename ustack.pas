unit UStack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TStack }

  TStack = class
    Container: array of string;
    public
      constructor Create;
      function Pop: string;
      procedure Push(val: string);
  end;

implementation

{ TStack }

constructor TStack.Create;
begin
  //Dummy
end;

function TStack.Pop: string;
begin
  if (Length(Container) = 0) then
    Result := ''
  else
  begin
    Result := Container[Length(Container)-1];
    SetLength(Container, Length(Container)-1);
  end;
end;

procedure TStack.Push(val: string);
begin
  SetLength(Container, Length(Container)+1);
  Container[Length(Container)-1] := val;
end;

end.
