program Delphi2Xml;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.StrUtils,
  System.SysUtils,
  System.Win.ComObj,
  Xml.adomxmldom,
  D2XProcessor in 'D2XProcessor.pas';

var
  prc: TD2XProcessor;
  i: Integer;

begin
  CoInitializeEx(nil, 0);
  try
    prc := TD2XProcessor.Create;
    try
      for i := 1 to ParamCount do
        if (Length(ParamStr(i)) > 1) and CharInSet(ParamStr(i)[1], ['-', '/']) then
          prc.Options.ParseOption(ParamStr(i))
        else
          prc.ProcessFile(ParamStr(i));
    finally
      prc.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
