program Delphi2Xml;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  FastMM4 in 'FastMM4\FastMM4.pas',
  System.Classes,
  System.StrUtils,
  System.SysUtils,
  Winapi.Windows,
  FastMM4Messages in 'FastMM4\FastMM4Messages.pas',
  D2X.Global in 'Source\D2X.Global.pas',
  D2X.Handler in 'Source\D2X.Handler.pas',
  D2X.Handlers in 'Source\D2X.Handlers.pas',
  D2X.Options in 'Source\D2X.Options.pas',
  D2X.Params in 'Source\D2X.Params.pas',
  D2X.Parser in 'Source\D2X.Parser.pas',
  D2X.Processor in 'Source\D2X.Processor.pas',
  D2X.IO in 'Source\D2X.IO.pas',
  D2X.IO.Actual in 'Source\D2X.IO.Actual.pas',
  D2X.Param in 'Source\D2X.Param.pas',
  D2X.IO.Options in 'Source\D2X.IO.Options.pas',
  D2X.RunParam in 'Source\D2X.RunParam.pas',
  D2X.Tree.Json in 'Source\D2X.Tree.Json.pas',
  D2X.Tree in 'Source\D2X.Tree.pas',
  D2X.Tree.Xml in 'Source\D2X.Tree.Xml.pas',
  D2X.IO.ErrorLog in 'Source\D2X.IO.ErrorLog.pas';

var
  opts: TD2XRunParam;
  i: Integer;
  bOk: Boolean;
  sOut: THandleStream;

begin
  //  CoInitializeEx(nil, 0);
  sOut := nil;
  opts := nil;

  try
    sOut := THandleStream.Create(GetStdHandle(STD_OUTPUT_HANDLE));
    opts := TD2XRunParam.Create;
    opts.JoinLog(TD2XLogger.Create.StartLog(sOut));
    opts.InitOptions(TD2XFileOptions.Create);
    if ParamCount > 0 then
    begin
      bOk := True;
      for i := 1 to ParamCount do
        bOk := opts.ProcessParam(ParamStr(i), i) and bOk;
      opts.EndRunProcessing;
      if not bOk then
        with TStreamWriter.Create(sOut) do
          try
            WriteLine('Errors ocurred processing parameters');
          finally
            Free;
          end;
    end
    else
      opts.DescribeAll;
  finally
    FreeAndNil(opts);
    FreeAndNil(sOut);
  end;

end.
