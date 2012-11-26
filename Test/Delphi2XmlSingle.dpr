program Delphi2XmlSingle;

{$APPTYPE CONSOLE}

uses
  FastMM4,
  GuiTestRunner,
  TextTestRunner,
  System.StrUtils,
  Winapi.Windows,
  D2X.Streams.Test in 'D2X.Streams.Test.pas',
  D2X in '..\Source\D2X.pas',
  D2X.Stream in '..\Source\D2X.Stream.pas',
  D2X.Streams in '..\Source\D2X.Streams.pas',
  D2X.Test in 'D2X.Test.pas',
  D2X.Utils in 'D2X.Utils.pas';

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

