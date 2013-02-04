program Delphi2Xml.Handlers.Tests;

{$APPTYPE CONSOLE}

uses
  FastMM4 in '..\..\FastMM4\FastMM4.pas',
  GuiTestRunner,
  TextTestRunner,
  System.StrUtils,
  Winapi.Windows,
  FastMM4Messages in '..\..\FastMM4\FastMM4Messages.pas',
  Test.Handlers.Tests in '..\Test.Handlers.Tests.pas',
  Test.Handlers in '..\Source\Test.Handlers.pas',
  D2X.Handlers in '..\..\Source\D2X.Handlers.pas';

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
