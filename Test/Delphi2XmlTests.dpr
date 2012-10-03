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
  D2XOptions in '..\Source\D2XOptions.pas';

{$R *.RES}

begin
  if (ParamCount > 0) and ContainsStr(ParamStr(1), 'con') then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
  begin
    FreeConsole;
    GuiTestRunner.RunRegisteredTests;
  end;
end.

