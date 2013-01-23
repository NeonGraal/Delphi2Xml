program Delphi2XmlTests;

{$APPTYPE CONSOLE}

uses
  FastMM4 in '..\FastMM4\FastMM4.pas',
  GuiTestRunner,
  TextTestRunner,
  System.StrUtils,
  Winapi.Windows,
  FastMM4Messages in '..\FastMM4\FastMM4Messages.pas',
  D2X in '..\Source\D2X.pas',
  D2X.Handler in '..\Source\D2X.Handler.pas',
  D2X.Handlers in '..\Source\D2X.Handlers.pas',
  D2X.IO in '..\Source\D2X.IO.pas',
  D2X.IO.Actual in '..\Source\D2X.IO.Actual.pas',
  D2X.IO.Options in '..\Source\D2X.IO.Options.pas',
  D2X.Options in '..\Source\D2X.Options.pas',
  D2X.Param in '..\Source\D2X.Param.pas',
  D2X.Params in '..\Source\D2X.Params.pas',
  D2X.Parser in '..\Source\D2X.Parser.pas',
  D2X.Processor in '..\Source\D2X.Processor.pas',
  D2X.Processors in '..\Source\D2X.Processors.pas',
  D2X.Xml in '..\Source\D2X.Xml.pas',
  Test.Global in 'Test.Global.pas',
  Test.Handler.Tests in 'Test.Handler.Tests.pas',
  Test.Handlers.Tests in 'Test.Handlers.Tests.pas',
  Test.IO.Actual.Tests in 'Test.IO.Actual.Tests.pas',
  Test.IO.Options.Tests in 'Test.IO.Options.Tests.pas',
  Test.IO.Tests in 'Test.IO.Tests.pas',
  Test.Options.Tests in 'Test.Options.Tests.pas',
  Test.Param in 'Test.Param.pas',
  Test.Params.Tests in 'Test.Params.Tests.pas',
  Test.Parser.Tests in 'Test.Parser.Tests.pas',
  Test.Processor.Tests in 'Test.Processor.Tests.pas',
  Test.Processors.Tests in 'Test.Processors.Tests.pas',
  Test.Utils in 'Test.Utils.pas',
  Test.Xml.Tests in 'Test.Xml.Tests.pas',
  D2X.RunParam in '..\Source\D2X.RunParam.pas',
  Test.RunParam.Tests in 'Test.RunParam.Tests.pas',
  Test.Constants in 'Test.Constants.pas',
  Test.Handler in 'Test.Handler.pas',
  Test.Handlers in 'Test.Handlers.pas',
  Test.IO in 'Test.IO.pas',
  Test.Options in 'Test.Options.pas',
  Test.Parser in 'Test.Parser.pas',
  Test.Processor in 'Test.Processor.pas',
  Test.Xml in 'Test.Xml.pas';

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
