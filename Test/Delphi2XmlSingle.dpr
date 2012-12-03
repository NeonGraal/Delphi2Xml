program Delphi2XmlSingle;

{$APPTYPE CONSOLE}

uses
  FastMM4,
  GuiTestRunner,
  TextTestRunner,
  System.StrUtils,
  Winapi.Windows,
  D2X in '..\Source\D2X.pas',
  D2X.Test in 'D2X.Test.pas',
  D2X.Utils in 'D2X.Utils.pas',
  D2X.IO.Actual.Test in 'D2X.IO.Actual.Test.pas',
  D2X.IO.Test in 'D2X.IO.Test.pas',
  D2X.IO.Actual in '..\Source\D2X.IO.Actual.pas',
  D2X.IO in '..\Source\D2X.IO.pas',
  D2X.Param in '..\Source\D2X.Param.pas';

{$R *.RES}

begin
  if (ParamCount > 0) and ContainsStr(ParamStr(1), 'gui') then
  begin
    FreeConsole;
    GuiTestRunner.RunRegisteredTests;
  end
  else
    with TextTestRunner.RunRegisteredTests do
      Free;

end.

