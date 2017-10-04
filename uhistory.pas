unit UHistory;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Clipbrd;

type

  { THistoryForm }

  THistoryForm = class(TForm)
    ClearHistoryButton: TButton;
    HistoryListBox: TListBox;
    procedure ClearHistoryButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure HistoryListBoxDblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  THistoryItem = record
    Expression: string;
    OpResult: string;
  end;

  THistoryItems = array of THistoryItem;

  THistory = record
    private
      HistoryList: THistoryItems;
    public
      procedure AddItem(Expression: string; OpResult: string);
      function GetAll: THistoryItems;
      procedure Clear;
  end;

var
  HistoryForm: THistoryForm;
  History: THistory;

implementation

{$R *.lfm}

{ THistoryForm }

procedure THistoryForm.FormActivate(Sender: TObject);
var
  i: UInt32;
  his: THistoryItems;
begin
  his := History.GetAll;
  if (Length(his) < 1) then exit;
  for i := Length(his)-1 downto 0 do
    HistoryListBox.AddItem(his[i].Expression + #13 + ' = ' + his[i].OpResult, nil);
end;

procedure THistoryForm.HistoryListBoxDblClick(Sender: TObject);
var
  t: TListBox;
begin
  t := TListBox(Sender);
  if (t.ItemIndex = -1) then exit;
  Clipboard.AsText := t.Items.Strings[t.ItemIndex];
end;

procedure THistoryForm.ClearHistoryButtonClick(Sender: TObject);
begin
  History.Clear;
  HistoryListBox.Clear;
end;

{ THistoryForm }

procedure THistory.AddItem(Expression: string; OpResult: string);
begin
  SetLength(HistoryList, Length(HistoryList)+1);
  HistoryList[Length(HistoryList)-1].Expression := Expression;
  HistoryList[Length(HistoryList)-1].OpResult := OpResult;
end;

function THistory.GetAll: THistoryItems;
begin
  Result := HistoryList;
end;

procedure THistory.Clear;
begin
  SetLength(HistoryList, 0);
end;

initialization

end.

