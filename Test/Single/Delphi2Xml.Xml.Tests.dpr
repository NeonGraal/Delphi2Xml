program Delphi2Xml.Xml.Tests;

{$APPTYPE CONSOLE}

uses
  FastMM4 in '..\..\FastMM4\FastMM4.pas',
  GuiTestRunner,
  TextTestRunner,
  System.StrUtils,
  Winapi.Windows,
  FastMM4Messages in '..\..\FastMM4\FastMM4Messages.pas',
  Test.Xml.Tests in '..\Test.Xml.Tests.pas',
  D2X.Xml in '..\..\Source\D2X.Xml.pas';

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
