program Delphi2Xml;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.Classes,
  System.StrUtils,
  System.SysUtils,
  Winapi.Windows,
//  System.Win.ComObj,
//  Xml.adomxmldom,
  D2XProcessor in 'Source\D2XProcessor.pas',
  D2XParser in 'Source\D2XParser.pas',
  D2XOptions in 'Source\D2XOptions.pas',
  D2Xml in 'Source\D2Xml.pas';

var
  prc: TD2XProcessor;
  i: Integer;
  bOk : Boolean;
  sOut: THandleStream;

begin
//  CoInitializeEx(nil, 0);
  sOut := nil;
  prc := nil;

  try
    sOut := THandleStream.Create(GetStdHandle(STD_OUTPUT_HANDLE));
    prc := TD2XProcessor.Create;
    prc.Options.SetLog(sOut);
    bOk := True;
    for i := 1 to ParamCount do
      bOk := prc.ProcessParam(ParamStr(i), 'Param', i) and bOk;
    prc.EndProcessing;
    if not bOk then
      prc.Options.ShowOptions;
  finally
    FreeAndNil(prc);
    FreeAndNil(sOut);
  end;

end.
