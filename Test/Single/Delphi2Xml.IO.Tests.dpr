program Delphi2Xml.IO.Tests;

{$APPTYPE CONSOLE}

uses
  FastMM4 in '..\..\FastMM4\FastMM4.pas',
  GuiTestRunner,
  TextTestRunner,
  System.StrUtils,
  Winapi.Windows,
  FastMM4Messages in '..\..\FastMM4\FastMM4Messages.pas',
  Test.IO.Tests in '..\Test.IO.Tests.pas',
  D2X.IO in '..\..\Source\D2X.IO.pas',
  Test.IO in '..\Source\Test.IO.pas';

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
