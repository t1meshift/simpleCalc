unit USimpleMode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Menus, ActnList, Clipbrd, LCLType, URPNParser, UMemory, UHistory, UAbout;

type

  { TSimpleModeForm }

  TSimpleModeForm = class(TForm)
    CopyMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    HistoryMenuItem: TMenuItem;
    DelimeterMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    BackspaceButton: TSpeedButton;
    CEButton: TSpeedButton;
    AddButton: TSpeedButton;
    CalcMenu: TMainMenu;
    EditSubMenu: TMenuItem;
    HelpSubMenu: TMenuItem;
    PercentButton: TSpeedButton;
    EqualButton: TSpeedButton;
    CalcScreenLabel: TLabel;
    MemoryValueLabel: TLabel;
    ReverseNumButton: TSpeedButton;
    SubButton: TSpeedButton;
    MulButton: TSpeedButton;
    DivButton: TSpeedButton;
    Num6Button: TSpeedButton;
    Num1Button: TSpeedButton;
    Num2Button: TSpeedButton;
    Num3Button: TSpeedButton;
    Num0Button: TSpeedButton;
    CommaButton: TSpeedButton;
    CButton: TSpeedButton;
    SwitchSignButton: TSpeedButton;
    SqrtSpeedButton: TSpeedButton;
    Num7Button: TSpeedButton;
    Num8Button: TSpeedButton;
    Num9Button: TSpeedButton;
    Num4Button: TSpeedButton;
    Num5Button: TSpeedButton;
    MemorySubButton: TSpeedButton;
    MemorySaveButton: TSpeedButton;
    MemoryAddButton: TSpeedButton;
    MemoryReadButton: TSpeedButton;
    MemoryClearButton: TSpeedButton;
    HistoryScreenLabel: TLabel;
    BtnPanel: TPanel;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure ArithmOpClick(Sender: TObject);
    procedure BkspClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure CommaClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure DigitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HistoryMenuItemClick(Sender: TObject);
    procedure MemoryClick(Sender: TObject);
    procedure OnKeyboardInput(Sender: TObject; var Key: char);
    procedure InputHandle(Key: char);
    procedure MemoryHandle(MemAction: char);
    procedure ArithmHandle(ArithmAction: char);
    procedure ClearLastNumber;
    procedure ClearAll;
    procedure PasteMenuItemClick(Sender: TObject);
    procedure PercentButtonClick(Sender: TObject);
    procedure ReverseNumButtonClick(Sender: TObject);
    procedure ThrowCalcError(Message: string);
    procedure SqrtSpeedButtonClick(Sender: TObject);
    procedure SwitchSignButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SimpleModeForm: TSimpleModeForm;
  LastResult, LastOperand: extended; //Operands
  PercentValue: extended; // percent value we've taken from the operation
  Operation: char; //Last operation
  StartOfEnter, ScreenModified, EqualWasCalled, CalcError: boolean; //Flags
  OpQueue: TRPNQueue; //operations queue


implementation

{$R *.lfm}

{ TSimpleModeForm }

procedure TSimpleModeForm.FormCreate(Sender: TObject);
begin
  ClearAll;
  OpQueue := TRPNQueue.Create;
end;

procedure TSimpleModeForm.HistoryMenuItemClick(Sender: TObject);
begin
  HistoryForm.Show;
end;

procedure TSimpleModeForm.MemoryClick(Sender: TObject);
begin
  MemoryHandle(TSpeedButton(Sender).Caption[2]);
end;

procedure TSimpleModeForm.DigitClick(Sender: TObject);
begin
  //An evil hack for digit buttons
  InputHandle(TSpeedButton(Sender).Caption[1]);
end;

procedure TSimpleModeForm.BkspClick(Sender: TObject);
begin
  InputHandle(#8);
end;

procedure TSimpleModeForm.ArithmOpClick(Sender: TObject);
begin
  ArithmHandle(TSpeedButton(Sender).Caption[1]);
end;

procedure TSimpleModeForm.AboutMenuItemClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TSimpleModeForm.ClearClick(Sender: TObject);
begin
  case TSpeedButton(Sender).Caption of
    'C': ClearAll;
    'CE': ClearLastNumber;
  end;
end;

procedure TSimpleModeForm.CommaClick(Sender: TObject);
begin
  InputHandle(',');
end;

procedure TSimpleModeForm.CopyMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText := CalcScreenLabel.Caption;
end;

procedure TSimpleModeForm.OnKeyboardInput(Sender: TObject; var Key: char);
begin
  InputHandle(Key);
end;

procedure TSimpleModeForm.InputHandle(Key: char);  // input handler, unites keyboard and button enter
var
  s: string;
  t: extended;
begin
  if CalcError then exit;
  case Key of
    // just digits
    '0'..'9':
    begin
      if (not ScreenModified) or EqualWasCalled then
      begin
        ClearLastNumber;
        ScreenModified := true;
        EqualWasCalled := false;
      end;
      if (pos('E', CalcScreenLabel.Caption) <> 0) or (Length(CalcScreenLabel.Caption) > 20) then exit;
      if (CalcScreenLabel.Caption = '0') then
        CalcScreenLabel.Caption := Key
      else
        CalcScreenLabel.Caption := CalcScreenLabel.Caption + Key;
    end;

    // backspace symbol
    #8:
    begin
      if (not ScreenModified) then exit;
      if (pos('E', CalcScreenLabel.Caption) <> 0)
      or (CalcScreenLabel.Caption = '0')
      or (Length(CalcScreenLabel.Caption) = 1)
      or ((Length(CalcScreenLabel.Caption) = 2) and (CalcScreenLabel.Caption[1] = '-')) then
      begin
        ClearLastNumber;
      end
      else
      begin
        s := CalcScreenLabel.Caption;
        Delete(s, Length(s), 1);
        CalcScreenLabel.Caption := s;
      end;
    end;

    //comma symbol
    '.', ',':
    begin
      if (not ScreenModified) or EqualWasCalled then
      begin
        ClearLastNumber;
        ScreenModified := true;
        EqualWasCalled := false;
      end;
      if (pos(DefaultFormatSettings.DecimalSeparator, CalcScreenLabel.Caption) <> 0)
      or (pos('E', CalcScreenLabel.Caption) <> 0)
      or (not TryStrToFloat(CalcScreenLabel.Caption, t)) then
        exit;
      CalcScreenLabel.Caption := CalcScreenLabel.Caption + DefaultFormatSettings.DecimalSeparator;
    end;

    '+','-','*','/','=',#13:
    begin
      ArithmHandle(Key);
    end;

    chr(VK_ESCAPE):
      ClearAll;
  end;
end;

procedure TSimpleModeForm.MemoryHandle(MemAction: char); // memory buttons handler
var
  MemVal: extended;
begin
  if CalcError then exit;
  case MemAction of
    'C':
      Memory.Clear;
    'R':
      begin
       //TODO memory reading
       if EqualWasCalled then
       begin
         StartOfEnter := true;
         ScreenModified := true;
         EqualWasCalled := false;
       end;
       if StartOfEnter or ScreenModified then
         CalcScreenLabel.Caption := FloatToStr(Memory.Read);
       if (not ScreenModified) then
       begin
         LastOperand := Memory.Read;
         CalcScreenLabel.Caption := FloatToStr(Memory.Read);
       end;
      end;
    'S':
      if TryStrToFloat(CalcScreenLabel.Caption, MemVal) then
        Memory.Save(MemVal);
    '+':
      if TryStrToFloat(CalcScreenLabel.Caption, MemVal) then
        Memory.Add(MemVal);
    '-':
      if TryStrToFloat(CalcScreenLabel.Caption, MemVal) then
        Memory.Sub(MemVal);
  end;
  MemoryValueLabel.Caption := 'Memory: ' + FloatToStr(Memory.Read);
end;

procedure TSimpleModeForm.ArithmHandle(ArithmAction: char);
var
  ts: string;
  t: extended;
begin
  //TODO ПЕРЕПИСАТЬ ВСЕ НА ОБРАТНУЮ ПОЛЬСКУЮ ЗАПИСЬ
  if (not TryStrToFloat(CalcScreenLabel.Caption, t)) then exit; //if there is any error do nothing
  try
    case ArithmAction of
      '+','-','*','/':
        begin
          if (StartOfEnter or EqualWasCalled) then
          begin
            OpQueue.Push(FloatToStr(t)); //push it as first operand
            HistoryScreenLabel.Caption := FloatToStr(t) + ' ' + ArithmAction;
            LastResult := t;
            StartOfEnter := false;
            EqualWasCalled := false;
            ScreenModified := false;
          end
          else
          begin
            if (ScreenModified) then
            begin
              OpQueue.Push(FloatToStr(t));
              OpQueue.Push(Operation);
              HistoryScreenLabel.Caption := HistoryScreenLabel.Caption + ' ' + FloatToStr(t) + ' ' + ArithmAction;
              LastResult := ParseRPN(OpQueue);
              LastOperand := t;
              OpQueue.Push(FloatToStr(LastResult));
              CalcScreenLabel.Caption := FloatToStr(LastResult);
              ScreenModified := false;
            end
            else
            begin
              ts := HistoryScreenLabel.Caption;
              ts[Length(ts)] := ArithmAction;
              HistoryScreenLabel.Caption := ts;
            end;
          end;
          Operation := ArithmAction;
        end;

      '=', #13:
        begin
          if (StartOfEnter) then exit;
          if (EqualWasCalled) then begin
            //OpQueue.Flush;
            OpQueue.Push(FloatToStr(t));
            OpQueue.Push(FloatToStr(LastOperand));
            OpQueue.Push(Operation);
            LastResult := ParseRPN(OpQueue);

            CalcScreenLabel.Caption := FloatToStr(LastResult);
          end
          else
          begin
            OpQueue.Push(FloatToStr(t));
            OpQueue.Push(Operation);

            LastResult := ParseRPN(OpQueue);
            LastOperand := t;

            History.AddItem(HistoryScreenLabel.Caption + ' ' + FloatToStr(LastOperand), FloatToStr(LastResult));

            HistoryScreenLabel.Caption := '';
            CalcScreenLabel.Caption := FloatToStr(LastResult);

            EqualWasCalled := true;
            OpQueue.Flush;
          end;
        end;
    end;
  except
    on EOverflow do
      ThrowCalcError('Overflow');
    on EDivByZero do
      ThrowCalcError('Division by 0 is impossible');
    on EMathError do
      ThrowCalcError('Incorrect operation');
  end;
end;

procedure TSimpleModeForm.ClearLastNumber;
begin
  if not CalcError then
  begin
    CalcScreenLabel.Caption := '0';
    ScreenModified := true;
  end;
end;

procedure TSimpleModeForm.ClearAll;
begin
  //CE + clear operation and memory
  CalcError := false;
  ClearLastNumber;
  HistoryScreenLabel.Caption := '';
  LastResult := 0;
  LastOperand := 0;
  StartOfEnter := true;
  EqualWasCalled := false;
  Operation := #0;
end;

procedure TSimpleModeForm.PasteMenuItemClick(Sender: TObject);
var
  TextToPaste: string;
  i: UInt32;
  CommaPassed: boolean;
begin
  if (Clipboard.AsText <> '') and (not CalcError) then
  begin
    //filtering an input
    CommaPassed := false;
    TextToPaste := '';
    for i := 1 to Length(Clipboard.AsText) do
      case Clipboard.AsText[i] of
        '0'..'9':
          TextToPaste := TextToPaste + Clipboard.AsText[i];
        else
          if (Clipboard.AsText[i] = DefaultFormatSettings.DecimalSeparator) and (not CommaPassed) then
          begin
            CommaPassed := true;
            TextToPaste := TextToPaste + Clipboard.AsText[i];
          end;
      end;
    if (TextToPaste <> '') then CalcScreenLabel.Caption := TextToPaste;
  end;
end;

procedure TSimpleModeForm.PercentButtonClick(Sender: TObject);
var
  t: extended;
begin
  if (not TryStrToFloat(CalcScreenLabel.Caption, t)) then exit;
  try
    PercentValue := t;
    LastOperand := LastResult * (PercentValue / 100);
    CalcScreenLabel.Caption := FloatToStr(LastOperand);
    if (EqualWasCalled) then
      LastResult := LastOperand;
  except
    on EOverflow do
      ThrowCalcError('Overflow');
    on EMathError do
      ThrowCalcError('Incorrect operation');
  end;
end;

procedure TSimpleModeForm.ReverseNumButtonClick(Sender: TObject);
var
  t: extended;
begin
  if (not TryStrToFloat(CalcScreenLabel.Caption, t)) then exit;
  try
  CalcScreenLabel.Caption := FloatToStr(1/t);
  ScreenModified := true;
  except
    on EDivByZero do
      ThrowCalcError('Division by 0 is impossible');
    on EOverflow do
      ThrowCalcError('Overflow');
    on EMathError do
      ThrowCalcError('Incorrect operation');
  end;
end;

procedure TSimpleModeForm.ThrowCalcError(Message: string);
begin
  ClearAll;
  CalcScreenLabel.Caption := Message;
  CalcError := true;
end;

procedure TSimpleModeForm.SqrtSpeedButtonClick(Sender: TObject);
var
  t: extended;
begin
  if (not TryStrToFloat(CalcScreenLabel.Caption, t)) then exit;
  try
  CalcScreenLabel.Caption := FloatToStr(sqrt(t));
  ScreenModified := true;
  except
    on EOverflow do
      ThrowCalcError('Overflow');
    on EMathError do
      ThrowCalcError('Incorrect operation');
  end;
end;

procedure TSimpleModeForm.SwitchSignButtonClick(Sender: TObject);
var
  t: extended;
begin
  if (not TryStrToFloat(CalcScreenLabel.Caption, t)) then exit;
  try
  CalcScreenLabel.Caption := FloatToStr(-t);
  ScreenModified := true;
  except
    on EOverflow do
      ThrowCalcError('Overflow');
    on EMathError do
      ThrowCalcError('Incorrect operation');
  end;
end;

end.

