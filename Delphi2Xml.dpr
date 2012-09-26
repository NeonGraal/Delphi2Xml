program Delphi2Xml;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.StrUtils,
  System.SysUtils,
//  System.Win.ComObj,
//  Xml.adomxmldom,
  D2XProcessor in 'Source\D2XProcessor.pas',
  D2XParser in 'Source\D2XParser.pas',
  D2XOptions in 'Source\D2XOptions.pas',
  D2Xml in 'Source\D2Xml.pas';

var
  prc: TD2XProcessor;
  i: Integer;
  fParamsOk : Boolean;

begin
//  CoInitializeEx(nil, 0);
  prc := TD2XProcessor.Create;
  try
    fParamsOk := True;
    for i := 1 to ParamCount do
      fParamsOk := prc.ProcessParam(ParamStr(i), 'Param', i) and fParamsOk;
    prc.EndProcessing;
    if not fParamsOk then
      prc.Options.ShowOptions;
  finally
    prc.Free;
  end;

end.
