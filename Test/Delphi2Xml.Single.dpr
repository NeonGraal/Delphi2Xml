program Delphi2Xml.Single;

{$APPTYPE CONSOLE}

uses
  FastMM4 in '..\FastMM4\FastMM4.pas',
  GuiTestRunner,
  TextTestRunner,
  System.StrUtils,
  Winapi.Windows,
  FastMM4Messages in '..\FastMM4\FastMM4Messages.pas',
  Test.Processor.Tests in 'Test.Processor.Tests.pas';

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
