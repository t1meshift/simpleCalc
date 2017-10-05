program simpleCalc;

{$mode objfpc}{$H+}


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, USimpleMode, UMemory, UAbout, UHistory, URPNParser, UStack
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='SimpleCalc';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TSimpleModeForm, SimpleModeForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(THistoryForm, HistoryForm);
  Application.Run;
end.

