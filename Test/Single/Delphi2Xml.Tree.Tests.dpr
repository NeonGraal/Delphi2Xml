program Delphi2Xml.Tree.Tests;

{$APPTYPE CONSOLE}

uses
  FastMM4 in '..\..\FastMM4\FastMM4.pas',
  GuiTestRunner,
  TextTestRunner,
  System.StrUtils,
  Winapi.Windows,
  FastMM4Messages in '..\..\FastMM4\FastMM4Messages.pas',
  Test.Tree.Tests in '..\Test.Tree.Tests.pas',
  D2X.Tree in '..\..\Source\D2X.Tree.pas',
  Test.Tree.Xml.Tests in '..\Test.Tree.Xml.Tests.pas',
  D2X.Tree.Xml in '..\..\Source\D2X.Tree.Xml.pas',
  Test.Tree.Json.Tests in '..\Test.Tree.Json.Tests.pas',
  D2X.Tree.Json in '..\..\Source\D2X.Tree.Json.pas';

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
