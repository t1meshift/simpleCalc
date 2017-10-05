unit URPNParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UStack, Dialogs;

type

  TQueueNode = record
    Value: string;
    Next: ^TQueueNode;
  end;

  { TRPNQueue }

  TRPNQueue = class
    RootNode: ^TQueueNode;
    constructor Create;
    destructor Destroy; override;
    procedure Flush;
    procedure Push(val: string);
    function Pop: string;
  end;

function ParseRPN(OpsQueue: TRPNQueue): extended;

implementation

function ParseRPN(OpsQueue: TRPNQueue): extended;
var
  Ops: TRPNQueue;
  Stack: TStack;
  StackValue: string;
  t, FirstOperand, SecondOperand, OpResult: extended;
begin
  Ops := OpsQueue;
  OpResult := 0;
  Stack := TStack.Create;
  StackValue := Ops.Pop;
  while (StackValue <> '') do
  begin
    if (TryStrToFloat(StackValue, t)) then
      Stack.Push(StackValue)
    else
    begin
      case StackValue of
        '+','-','*','/':
        begin
          SecondOperand := StrToFloat(Stack.Pop);
          FirstOperand := StrToFloat(Stack.Pop);

          case StackValue of
            '+':
              OpResult := FirstOperand + SecondOperand;
            '-':
              OpResult := FirstOperand - SecondOperand;
            '*':
              OpResult := FirstOperand * SecondOperand;
            '/':
              OpResult := FirstOperand / SecondOperand;
          end;
          Stack.Push(FloatToStr(OpResult));
        end;
      end;
    end;
    StackValue := Ops.Pop;
  end;
  Result := StrToFloat(Stack.Pop);
  FreeAndNil(Stack);
  Ops := nil;
end;

{ TRPNQueue }

constructor TRPNQueue.Create;
begin
  RootNode := nil;
end;

destructor TRPNQueue.Destroy;
begin
  inherited Destroy;
  Flush;
end;

procedure TRPNQueue.Flush;
begin
  while (Pop <> '') do
  begin
  end;
end;

procedure TRPNQueue.Push(val: string);
var
  node, prevnode: ^TQueueNode;
begin
  if (RootNode = nil) then
  begin
    New(RootNode);
    RootNode^.Value := val;
    RootNode^.Next := nil;
  end
  else
  begin
    prevnode := RootNode;

    repeat
      node := prevnode^.Next;
      if (node <> nil) then
        prevnode := node;
    until node = nil;

    New(prevnode^.Next);
    prevnode^.Next^.Value := val;
    prevnode^.Next^.Next := nil;
  end;
end;

function TRPNQueue.Pop: string;
var
  t: ^TQueueNode;
begin
  if (RootNode = nil) then
    Result := ''
  else
  begin
    Result := RootNode^.Value;
    t := RootNode^.Next;
    Dispose(RootNode);
    RootNode := t;
  end;
end;
end.
