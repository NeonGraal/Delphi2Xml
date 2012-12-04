program Delphi2XmlSingle;

{$APPTYPE CONSOLE}

uses
  FastMM4,
  GuiTestRunner,
  TextTestRunner,
  System.StrUtils,
  Winapi.Windows,
  D2X in '..\Source\D2X.pas',
  D2X.IO.Actual in '..\Source\D2X.IO.Actual.pas',
  D2X.IO in '..\Source\D2X.IO.pas',
  D2X.IO.Options in '..\Source\D2X.IO.Options.pas',
  D2X.Param in '..\Source\D2X.Param.pas',
  D2X.Params in '..\Source\D2X.Params.pas',
  Test.IO.Options in 'Test.IO.Options.pas',
  Test.Utils in 'Test.Utils.pas',
  Test.Global in 'Test.Global.pas';

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
