program Delphi2Xml.Tests;

{$APPTYPE CONSOLE}

uses
  FastMM4 in '..\FastMM4\FastMM4.pas',
  GuiTestRunner,
  TextTestRunner,
  System.StrUtils,
  Winapi.Windows,
  FastMM4Messages in '..\FastMM4\FastMM4Messages.pas',
  D2X.Global in '..\Source\D2X.Global.pas',
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
  D2X.RunParam in '..\Source\D2X.RunParam.pas',
  Test.Constants in 'Source\Test.Constants.pas',
  Test.Global.Tests in 'Test.Global.Tests.pas',
  Test.Handler in 'Source\Test.Handler.pas',
  Test.Handlers in 'Source\Test.Handlers.pas',
  Test.Handlers.Tests in 'Test.Handlers.Tests.pas',
  Test.IO in 'Source\Test.IO.pas',
  Test.IO.Actual.Tests in 'Test.IO.Actual.Tests.pas',
  Test.IO.Options.Tests in 'Test.IO.Options.Tests.pas',
  Test.IO.Tests in 'Test.IO.Tests.pas',
  Test.Options in 'Source\Test.Options.pas',
  Test.Options.Tests in 'Test.Options.Tests.pas',
  Test.Param in 'Source\Test.Param.pas',
  Test.Params.Tests in 'Test.Params.Tests.pas',
  Test.Parser in 'Source\Test.Parser.pas',
  Test.Parser.Tests in 'Test.Parser.Tests.pas',
  Test.Processor in 'Source\Test.Processor.pas',
  Test.Processor.Tests in 'Test.Processor.Tests.pas',
  Test.RunParam.Tests in 'Test.RunParam.Tests.pas',
  Test.Utils in 'Source\Test.Utils.pas',
  Test.Tree.Tests in 'Test.Tree.Tests.pas',
  Test.Tree.Json.Tests in 'Test.Tree.Json.Tests.pas',
  Test.Tree.Json in 'Source\Test.Tree.Json.pas',
  Test.Tree.Xml.Tests in 'Test.Tree.Xml.Tests.pas',
  Test.Global in 'Source\Test.Global.pas',
  Test.IO.Options in 'Source\Test.IO.Options.pas',
  D2X.Tree.Json in '..\Source\D2X.Tree.Json.pas',
  D2X.Tree in '..\Source\D2X.Tree.pas',
  D2X.Tree.Xml in '..\Source\D2X.Tree.Xml.pas',
  Test.Tree in 'Source\Test.Tree.pas',
  Test.Tree.Xml in 'Source\Test.Tree.Xml.pas';

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
