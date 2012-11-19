program Delphi2Xml;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.Classes,
  System.StrUtils,
  System.SysUtils,
  Winapi.Windows,
  D2X in 'Source\D2X.pas',
  D2X.Handler in 'Source\D2X.Handler.pas',
  D2X.Handlers in 'Source\D2X.Handlers.pas',
  D2X.Options in 'Source\D2X.Options.pas',
  D2X.Param in 'Source\D2X.Param.pas',
  D2X.Parser in 'Source\D2X.Parser.pas',
  D2X.Processor in 'Source\D2X.Processor.pas',
  D2X.Processors in 'Source\D2X.Processors.pas',
  D2X.Xml in 'Source\D2X.Xml.pas';

var
  opts: TD2XRunOptions;
  i: Integer;
  bOk: Boolean;
  sOut: THandleStream;

begin
  //  CoInitializeEx(nil, 0);
  sOut := nil;
  opts := nil;

  try
    sOut := THandleStream.Create(GetStdHandle(STD_OUTPUT_HANDLE));
    opts := TD2XRunOptions.Create;
    opts.JoinLog(TD2XLogger.Create(sOut));
    bOk := True;
    for i := 1 to ParamCount do
      bOk := opts.ProcessParam(ParamStr(i), 'Param', i) and bOk;
    opts.EndProcessing;
    if not bOk then
      with TStreamWriter.Create(sOut) do
        try
          WriteLine('Errors ocurred processing parameters');
        finally
          Free;
        end;
  finally
    FreeAndNil(opts);
    FreeAndNil(sOut);
  end;

end.
