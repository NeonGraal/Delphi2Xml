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
  ffRec: TSearchRec;
  filePath: string;

begin
  CoInitializeEx(nil, 0);
  prc := TD2XProcessor.Create;
  try
    prc.BeginProcessing;
    for i := 1 to ParamCount do
      try
        if (Length(ParamStr(i)) > 1) and CharInSet(ParamStr(i)[1], ['-', '/'])
        then
          prc.Options.ParseOption(ParamStr(i))
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
  finally
    prc.Free;
  end;

end.
