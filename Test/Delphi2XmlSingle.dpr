program Delphi2XmlSingle;

{$APPTYPE CONSOLE}

uses
  GuiTestRunner,
  TextTestRunner,
  System.StrUtils,
  Winapi.Windows,
  D2X.Stream.Test in 'D2X.Stream.Test.pas',
  D2X in '..\Source\D2X.pas',
  D2X.Stream in '..\Source\D2X.Stream.pas';

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

