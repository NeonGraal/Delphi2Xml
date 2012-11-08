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
  D2X in '..\Source\D2X.pas',
  D2X.Handler in '..\Source\D2X.Handler.pas',
  D2X.Handler.Test in 'D2X.Handler.Test.pas',
  D2X.Handlers in '..\Source\D2X.Handlers.pas',
  D2X.Handlers.Test in 'D2X.Handlers.Test.pas',
  D2X.Options in '..\Source\D2X.Options.pas',
  D2X.Options.Test in 'D2X.Options.Test.pas',
  D2X.Param in '..\Source\D2X.Param.pas',
  D2X.Param.Test in 'D2X.Param.Test.pas',
  D2X.ParamProcessor in '..\Source\D2X.ParamProcessor.pas',
  D2X.ParamProcessor.Test in 'D2X.ParamProcessor.Test.pas',
  D2X.Parser in '..\Source\D2X.Parser.pas',
  D2X.Parser.Test in 'D2X.Parser.Test.pas',
  D2X.Processor in '..\Source\D2X.Processor.pas',
  D2X.Processor.Test in 'D2X.Processor.Test.pas',
  D2X.Processors in '..\Source\D2X.Processors.pas',
  D2X.Processors.Test in 'D2X.Processors.Test.pas',
  D2X.Test in 'D2X.Test.pas',
  D2X.Utils in 'D2X.Utils.pas',
  D2X.Xml in '..\Source\D2X.Xml.pas',
  D2X.Xml.Test in 'D2X.Xml.Test.pas';

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
