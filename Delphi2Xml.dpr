program Delphi2Xml;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Win.ComObj,
  Xml.adomxmldom,
  D2XProcessor in 'D2XProcessor.pas';

var
  prc: TD2XProcessor;

begin
  CoInitializeEx(nil, 0);
  try
    prc := TD2XProcessor.Create;
    try
      prc.SetXmlProxy;
      prc.ProcessFile('..\..\D2XProcessor.pas');
    finally
      prc.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
