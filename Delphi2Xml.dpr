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
  fParamsOk : Boolean;
  ffRec: TSearchRec;
  filePath: string;

begin
  CoInitializeEx(nil, 0);
  prc := TD2XProcessor.Create;
  try
    prc.BeginProcessing;
    fParamsOk := True;
    for i := 1 to ParamCount do
      try
        if (Length(ParamStr(i)) > 1) and CharInSet(ParamStr(i)[1], ['-', '/'])
        then
          fParamsOk := prc.Options.ParseOption(ParamStr(i)) and fParamsOk
        else if FileExists(ParamStr(i)) then
          prc.ProcessFile(ParamStr(i))
        else if FindFirst(ParamStr(i), faAnyFile, ffRec) = 0 then
          try
            filePath := ExtractFilePath(ParamStr(i));
            repeat
              prc.ProcessFile(filePath + ffRec.Name);
            until FindNext(ffRec) <> 0;
          finally
            FindClose(ffRec);
          end
        else
          Writeln('Unknown file ', ParamStr(i));
      except
        on E: Exception do
          Writeln('EXCEPTION (', E.ClassName, ') processing "', ParamStr(i),
            '" : ', E.Message);
      end;
    prc.EndProcessing;
    if not fParamsOk then
      prc.Options.ShowOptions;
  finally
    prc.Free;
  end;

end.
