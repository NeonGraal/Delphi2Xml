program Delphi2Xml.Options.Tests;

{$APPTYPE CONSOLE}

uses
  FastMM4 in '..\..\FastMM4\FastMM4.pas',
  GuiTestRunner,
  TextTestRunner,
  System.StrUtils,
  Winapi.Windows,
  FastMM4Messages in '..\..\FastMM4\FastMM4Messages.pas',
  Test.Options.Tests in '..\Test.Options.Tests.pas',
  D2X.Options in '..\..\Source\D2X.Options.pas',
  Test.Options in '..\Source\Test.Options.pas';

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
