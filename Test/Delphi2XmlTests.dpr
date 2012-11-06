program Delphi2XmlTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  GuiTestRunner,
  TextTestRunner,
  System.StrUtils,
  Winapi.Windows,
  D2XOptionsTest in 'D2XOptionsTest.pas',
  D2XTest in 'D2XTest.pas',
  D2X in '..\Source\D2X.pas',
  D2XParam in '..\Source\D2XParam.pas',
  D2XParamTest in 'D2XParamTest.pas',
  D2XOptions in '..\Source\D2XOptions.pas',
  D2XUtils in 'D2XUtils.pas',
  D2XParser in '..\Source\D2XParser.pas',
  D2XParserTest in 'D2XParserTest.pas',
  D2XProcessor in '..\Source\D2XProcessor.pas',
  D2XProcessorTest in 'D2XProcessorTest.pas',
  D2Xml in '..\Source\D2Xml.pas',
  D2XmlTest in 'D2XmlTest.pas',
  D2XHandler in '..\Source\D2XHandler.pas',
  D2XHandlerTest in 'D2XHandlerTest.pas',
  D2XHandlers in '..\Source\D2XHandlers.pas',
  D2XHandlersTest in 'D2XHandlersTest.pas',
  D2XProcessors in '..\Source\D2XProcessors.pas',
  D2XProcessorsTest in 'D2XProcessorsTest.pas',
  D2XParamProcessor in '..\Source\D2XParamProcessor.pas';

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
