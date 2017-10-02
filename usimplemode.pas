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
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SimpleModeForm: TSimpleModeForm;
  FirstOperand, SecondOperand: double;
  Operation: char;
  StartOfEnter, HasSecondOperand, EqualWasCalled: boolean; //Flags


implementation

{$R *.lfm}



{ TSimpleModeForm }

procedure TSimpleModeForm.FormCreate(Sender: TObject);
begin
  //Start number
  CalcScreenLabel.Caption := '0';
  Operation := #0; //does not exist
  StartOfEnter := true;
  HasSecondOperand := false;
  EqualWasCalled := false;
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

procedure TSimpleModeForm.InputHandle(Key: char);
var
  s: string;
  t: double;
begin
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
      if (pos(',', CalcScreenLabel.Caption) <> 0)
      or (pos('E', CalcScreenLabel.Caption) <> 0)
      or (not TryStrToFloat(CalcScreenLabel.Caption, t)) then
        exit;
      CalcScreenLabel.Caption := CalcScreenLabel.Caption + ',';
    end;

    '+','-','*','/',#13:
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

procedure TSimpleModeForm.MemoryHandle(MemAction: char);
var
  MemVal: double;
begin
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
  case ArithmAction of
    '+':
      begin
        if StartOfEnter or EqualWasCalled then
        begin
          EqualWasCalled := false;
          FirstOperand := t;
          StartOfEnter := false;
          HistoryScreenLabel.Caption := FloatToStr(FirstOperand) + ' + ';
          HasSecondOperand := true;
        end
        else
        begin
          if HasSecondOperand then
          begin
            FirstOperand := FirstOperand + SecondOperand;
            exit;
          end
          else
          begin
            SecondOperand := t;
            HasSecondOperand := true;
            FirstOperand := FirstOperand + SecondOperand;
            HistoryScreenLabel.Caption := FloatToStr(FirstOperand) + ' + ';
          end;
        end;
        Operation := '+';
      end;

    '-':
    begin
      if StartOfEnter or EqualWasCalled then
      begin
        EqualWasCalled := false;
        FirstOperand := t;
        StartOfEnter := false;
        HistoryScreenLabel.Caption := FloatToStr(FirstOperand) + ' - ';
        HasSecondOperand := true;
      end
      else
      begin
        if HasSecondOperand then
        begin
          FirstOperand := FirstOperand - SecondOperand;
          exit;
        end
        else
        begin
          SecondOperand := t;
          HasSecondOperand := true;
          FirstOperand := FirstOperand - SecondOperand;
          HistoryScreenLabel.Caption := FloatToStr(FirstOperand) + ' - ';
        end;
      end;
      Operation := '-';
    end;

    '=', #13:
      begin
        if StartOfEnter then exit;
         case Operation of
           '+':
           begin
             if (not HasSecondOperand) then
             begin
               SecondOperand := t;
               HasSecondOperand := true;
             end;
             FirstOperand := FirstOperand + SecondOperand;
           end;
           '-':
           begin
             if (not HasSecondOperand) then
             begin
               SecondOperand := t;
               HasSecondOperand := true;
             end;
             FirstOperand := FirstOperand - SecondOperand;
           end;
         end;
         HistoryScreenLabel.Caption := '';
         CalcScreenLabel.Caption := FloatToStr(FirstOperand);
         EqualWasCalled := true;
      end;
  end;
end;

procedure TSimpleModeForm.ClearLastNumber;
begin
  CalcScreenLabel.Caption := '0';
end;

procedure TSimpleModeForm.ClearAll;
begin
  //CE + clear operation and memory
  ClearLastNumber;
  FirstOperand := 0;
  SecondOperand := 0;
  StartOfEnter := true;
  HasSecondOperand := false;
  EqualWasCalled := false;
  Operation := #0;
  MemoryHandle('C');
end;

end.

