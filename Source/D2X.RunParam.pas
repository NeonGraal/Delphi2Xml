unit D2X.RunParam;

interface

uses
  D2X.Global,
  D2X.IO,
  D2X.Options;

type
  TD2XRunParam = class(TD2XLogger)
  private
    function ProcessParamsFile(pFileOrExtn: string): Boolean;
    function ProcessItem(pStr, pFrom: string; pIdx: Integer): Boolean;

  protected
    fOpts: TD2XOptions;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure InitOptions(pFileOpts: ID2XIOFactory);

    procedure EndProcessing;

    function ProcessParam(pStr: string; pIdx: Integer): Boolean;

    procedure DescribeAll;

  end;

implementation

uses
  System.Classes,
  System.StrUtils,
  System.SysUtils;

{ TD2XRunParam }

constructor TD2XRunParam.Create;
begin
  inherited Create;

  fOpts := TD2XOptions.Create;
end;

procedure TD2XRunParam.DescribeAll;
begin
  fOpts.DescribeAll;
end;

destructor TD2XRunParam.Destroy;
begin
  FreeAndNil(fOpts);

  inherited;
end;

procedure TD2XRunParam.EndProcessing;
begin
  fOpts.EndProcessing;
end;

procedure TD2XRunParam.InitOptions(pFileOpts: ID2XIOFactory);
begin
  fOpts.JoinLog(Self);
  fOpts.InitProcessors(pFileOpts);
end;

function TD2XRunParam.ProcessItem(pStr, pFrom: string; pIdx: Integer): Boolean;
begin
  Result := False;
  try
    if (Length(pStr) > 1) then
      case pStr[1] of
        '-', '/':
          Result := fOpts.ProcessOption(Copy(pStr, 2));
        '@':
          Result := ProcessParamsFile(Copy(pStr, 2));
        '#':
          Result := True;
      else
        Result := fOpts.ProcessParam(pStr, pFrom + '-' + IntToStr(pIdx));
      end
    else
      Result := fOpts.ProcessParam(pStr, pFrom + '-' + IntToStr(pIdx));
  except
    on E: Exception do
      Log('EXCEPTION (%s) processing %s@%d "%s" : %s', [E.ClassName, pFrom, pIdx, pStr,
          E.Message]);
  end;
end;

function TD2XRunParam.ProcessParam(pStr: string; pIdx: Integer): Boolean;
var
  lStr: string;
begin
  if (Length(pStr) > 1) and CharInSet(pStr[1], ['"', '''']) then
  begin
    lStr := Copy(pStr, 1, 1);
    pStr := ReplaceStr(Copy(pStr, 2, Length(pStr) - 2), lStr + lStr, lStr);
  end;
  Result := ProcessItem(pStr, '(Param)', pIdx);
end;

function TD2XRunParam.ProcessParamsFile(pFileOrExtn: string): Boolean;
var
  lSL: TStringList;
  i: Integer;
  lFile: ID2XIOFile;
begin
  lSL := nil;
  lFile := nil;
  Result := True;
  try
    lSL := TStringList.Create;
    lFile := fOpts.ConfigFileOrExtn(pFileOrExtn);
    lSL.LoadFromStream(lFile.ReadFrom.BaseStream);
    for i := 0 to lSL.Count - 1 do
      Result := ProcessItem(lSL[i], lFile.Description, i + 1) and Result;
  finally
    DisposeOf(lFile);
    FreeAndNil(lSL);
  end;
end;

end.
