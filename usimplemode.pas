unit USimpleMode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Menus, ActnList, UMemory;

type

  { TSimpleModeForm }

  TSimpleModeForm = class(TForm)
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
    procedure ArithmOpClick(Sender: TObject);
    procedure BkspClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure CommaClick(Sender: TObject);
    procedure DigitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MemoryClick(Sender: TObject);
    procedure OnKeyboardInput(Sender: TObject; var Key: char);
    procedure InputHandle(Key: char);
    procedure MemoryHandle(MemAction: char);
    procedure ArithmHandle(ArithmAction: char);
    procedure ClearLastNumber;
    procedure ClearAll;
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
  FirstOperand, SecondOperand: double; //Operands
  PercentValue: double; // percent value we've taken from the operation
  Operation: char; //Last operation
  StartOfEnter, HasSecondOperand, EqualWasCalled, CalcError: boolean; //Flags


implementation

{$R *.lfm}



{ TSimpleModeForm }

procedure TSimpleModeForm.FormCreate(Sender: TObject);
begin
  ClearAll;
end;

procedure TSimpleModeForm.MemoryClick(Sender: TObject);
begin
  MemoryHandle(TButton(Sender).Caption[2]);
end;

procedure TSimpleModeForm.DigitClick(Sender: TObject);
begin
  //An evil hack for digit buttons
  InputHandle(TButton(Sender).Caption[1]);
end;

procedure TSimpleModeForm.BkspClick(Sender: TObject);
begin
  InputHandle(#8);
end;

procedure TSimpleModeForm.ArithmOpClick(Sender: TObject);
begin
  ArithmHandle(TButton(Sender).Caption[1]);
end;

procedure TSimpleModeForm.ClearClick(Sender: TObject);
begin
  case TButton(Sender).Caption of
    'C': ClearAll;
    'CE': ClearLastNumber;
  end;
end;

procedure TSimpleModeForm.CommaClick(Sender: TObject);
begin
  InputHandle(',');
end;

procedure TSimpleModeForm.OnKeyboardInput(Sender: TObject; var Key: char);
begin
  //CalcScreenLabel.Caption:=IntToStr(ord(Key)) + '(' + Key + ')';
  InputHandle(Key);
end;

procedure TSimpleModeForm.InputHandle(Key: char);  // input handler, unites keyboard and button enter
var
  s: string;
  t: double;
begin
  if CalcError then exit;
  case Key of
    // just digits
    '0'..'9':
    begin
      if HasSecondOperand then
      begin
        ClearLastNumber;
        HasSecondOperand := false;
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
      if HasSecondOperand then exit;
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
      if HasSecondOperand then
      begin
        ClearLastNumber;
        HasSecondOperand := false;
      end;
      if (pos(DefaultFormatSettings.DecimalSeparator, CalcScreenLabel.Caption) <> 0)
      or (pos('E', CalcScreenLabel.Caption) <> 0)
      or (not TryStrToFloat(CalcScreenLabel.Caption, t)) then
        exit;
      CalcScreenLabel.Caption := CalcScreenLabel.Caption + DefaultFormatSettings.DecimalSeparator;
    end;

    '+','-','*','/','=',#13:
    begin
      if HasSecondOperand and (Key <> #13) and (Key <> '=') then
      begin
        if (Key = Operation) and not EqualWasCalled then exit;
        HasSecondOperand := false;
      end;
      ArithmHandle(Key);
    end;

    //TODO: lots of buttons
  end;
end;

procedure TSimpleModeForm.MemoryHandle(MemAction: char); // memory buttons handler
var
  MemVal: double;
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
         HasSecondOperand := false;
         EqualWasCalled := false;
       end;
       if StartOfEnter or (not HasSecondOperand) then
         CalcScreenLabel.Caption := FloatToStr(Memory.Read);
       if HasSecondOperand then
       begin
         SecondOperand := Memory.Read;
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
  t: double;
begin
  if (not TryStrToFloat(CalcScreenLabel.Caption, t)) then exit; //if there is any error do nothing
  try
    case ArithmAction of
      '+','-','*','/':
        begin
          if StartOfEnter or EqualWasCalled then
          begin
            EqualWasCalled := false;
            FirstOperand := t;
            StartOfEnter := false;
            HistoryScreenLabel.Caption := FloatToStr(FirstOperand) + ' ' + ArithmAction + ' ';
            HasSecondOperand := true;
            SecondOperand := t;
          end
          else
          begin
            if (not HasSecondOperand) then
            begin
              SecondOperand := t;
              HasSecondOperand := true;
            end;
            case ArithmAction of
              '+':
                FirstOperand := FirstOperand + SecondOperand;
              '-':
                FirstOperand := FirstOperand - SecondOperand;
              '*':
                FirstOperand := FirstOperand * SecondOperand;
              '/':
              if (SecondOperand = 0) then begin
                ThrowCalcError('Division by 0 is impossible');
                exit;
              end
              else
                FirstOperand := FirstOperand / SecondOperand;
            end;
            if (not HasSecondOperand) then
              HistoryScreenLabel.Caption := FloatToStr(FirstOperand) + ' ' + ArithmAction + ' ';
          end;
          Operation := ArithmAction;
        end;

      '=', #13:
        begin
          if StartOfEnter then exit;
          if (not HasSecondOperand) then
          begin
            SecondOperand := t;
            HasSecondOperand := true;
          end;
           case Operation of
             '+':
               FirstOperand := FirstOperand + SecondOperand;
             '-':
               FirstOperand := FirstOperand - SecondOperand;
             '*':
               FirstOperand := FirstOperand * SecondOperand;
             '/':
             begin
               if (SecondOperand = 0) then
               begin
                 ThrowCalcError('Division by 0 is impossible');
                 exit;
               end;
               FirstOperand := FirstOperand / SecondOperand;
             end;
           end;
           HistoryScreenLabel.Caption := '';
           CalcScreenLabel.Caption := FloatToStr(FirstOperand);
           EqualWasCalled := true;
        end;
    end;
  except on EOverflow do
    ThrowCalcError('Overflow');
  end;
end;

procedure TSimpleModeForm.ClearLastNumber;
begin
  if CalcError then exit;
  CalcScreenLabel.Caption := '0';
end;

procedure TSimpleModeForm.ClearAll;
begin
  //CE + clear operation and memory
  HistoryScreenLabel.Caption := '';
  CalcScreenLabel.Caption := '0';
  FirstOperand := 0;
  SecondOperand := 0;
  StartOfEnter := true;
  HasSecondOperand := false;
  EqualWasCalled := false;
  CalcError := false;
  Operation := #0;
end;

procedure TSimpleModeForm.PercentButtonClick(Sender: TObject);
var
  t: double;
begin
  if (not TryStrToFloat(CalcScreenLabel.Caption, t)) then exit;
  try
    if (not HasSecondOperand) then
    begin
      PercentValue := t;
      SecondOperand := FirstOperand * (t / 100);
      CalcScreenLabel.Caption := FloatToStr(SecondOperand);
      HasSecondOperand := true;
    end
    else
    begin
      if (EqualWasCalled) then
      begin
        SecondOperand := FirstOperand;
        PercentValue := SecondOperand;
        case Operation of
          '+':
              FirstOperand := FirstOperand + (FirstOperand * (PercentValue / 100));
          '-':
              FirstOperand := FirstOperand - (FirstOperand * (PercentValue / 100));
          '*','/':
              FirstOperand := FirstOperand * (PercentValue / 100);
        end;
        CalcScreenLabel.Caption := FloatToStr(FirstOperand);
        HasSecondOperand := false;
      end;
    end;
  except
    on EOverflow do
      ThrowCalcError('Overflow');
    on EMathError do
      ThrowCalcError('Incorrect operation');
  end;
end;

procedure TSimpleModeForm.ReverseNumButtonClick(Sender: TObject);
var
  t: double;
begin
  if (not TryStrToFloat(CalcScreenLabel.Caption, t)) then exit;
  try
  CalcScreenLabel.Caption := FloatToStr(1/t);
  HasSecondOperand := false;
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
  t: double;
begin
  if (not TryStrToFloat(CalcScreenLabel.Caption, t)) then exit;
  try
  CalcScreenLabel.Caption := FloatToStr(sqrt(t));
  HasSecondOperand := false;
  except
    on EOverflow do
      ThrowCalcError('Overflow');
    on EMathError do
      ThrowCalcError('Incorrect operation');
  end;
end;

procedure TSimpleModeForm.SwitchSignButtonClick(Sender: TObject);
var
  t: double;
begin
  if (not TryStrToFloat(CalcScreenLabel.Caption, t)) then exit;
  try
  CalcScreenLabel.Caption := FloatToStr(-t);
  HasSecondOperand := false;
  except
    on EOverflow do
      ThrowCalcError('Overflow');
    on EMathError do
      ThrowCalcError('Incorrect operation');
  end;
end;

end.

