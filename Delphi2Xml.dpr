program Delphi2Xml;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.StrUtils,
  System.SysUtils,
  System.Win.ComObj,
  Xml.adomxmldom,
  D2XProcessor in 'D2XProcessor.pas',
  D2XParser in 'D2XParser.pas';

var
  prc: TD2XProcessor;
  i: Integer;
  fParamsOk : Boolean;

begin
  CoInitializeEx(nil, 0);
  prc := TD2XProcessor.Create;
  try
    prc.BeginProcessing;
    fParamsOk := True;
    for i := 1 to ParamCount do
      fParamsOk := prc.ProcessParam(ParamStr(i)) and fParamsOk;
    prc.EndProcessing;
    if not fParamsOk then
      prc.Options.ShowOptions;
  finally
    prc.Free;
  end;

end.
