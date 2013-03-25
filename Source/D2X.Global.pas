unit D2X.Global;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils;

type
  TD2XParseMode = (pmFull, pmUses);

  TD2XElapsedMode = (emNone, emTotal, emDir, emFile, emProcessing, emQuiet);

  TD2XResultPer = (rpFile, rpGroup, rpWildcard, rpSubDir, rpDir, rpParam, rpRun);

  TD2X = class
    class function Zero<T>: T;
    class function ToLabel<T>(pVal: T): string;
    class function CnvDflt<T>(pStr: string; pDflt: T; out pVal: T): Boolean;
    class function CnvEnum<T>(pStr: string; pDflt: T; out pVal: T): Boolean;
  end;

  TD2XInterfaced = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

  end;

  ID2XLogger = interface
    ['{75BB3877-8B9A-4E1A-B7AE-25FCC93CA28C}']
    procedure JoinLog(pLogger: ID2XLogger);
    procedure Log(pFmt: string; pArgs: array of const; pLine: Boolean = True);
  end;

  TD2XLogger = class(TD2XInterfaced, ID2XLogger)
  private
    fMyWriter: TTextWriter;
    fLog: TTextWriter;
    fLogger: ID2XLogger;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function StartLog(pStream: TStream): TD2XLogger; overload;
    function StartLog(pString: TStringBuilder): TD2XLogger; overload;
    function StartLog(pWriter: TTextWriter): TD2XLogger; overload;
    function StartLog(pLogger: ID2XLogger): TD2XLogger; overload;
    procedure StopLog;

    procedure JoinLog(pLogger: ID2XLogger);
    procedure Log(pFmt: string; pArgs: array of const; pLine: Boolean = True);

  end;

  TD2XFlagRef = reference to function: Boolean;

  TD2XStringRef = reference to function: string;

function MakeFileName(pStr, pDflt: string): string;

implementation

uses
  System.Rtti,
  System.StrUtils,
  System.TypInfo;

function MakeFileName(pStr, pDflt: string): string;
begin
  if pStr = '' then
    Result := pDflt
  else
    if ExtractFileExt(pStr) > '' then
      Result := pStr
    else
      Result := pStr + pDflt;
end;

{ TD2X }

class function TD2X.CnvDflt<T>(pStr: string; pDflt: T; out pVal: T): Boolean;
begin
  pVal := pDflt;
  Result := True;
end;

class function TD2X.CnvEnum<T>(pStr: string; pDflt: T; out pVal: T): Boolean;
var
  lTI: PTypeInfo;
  lTD: PTypeData;
  i: Integer;
  lV: TValue;
  lName: string;
begin
  lTI := TypeInfo(T);
  Assert(tkEnumeration = lTI.Kind, 'CnvEnum requires an Enumeration Type');
  Result := pStr > '';
  pVal := pDflt;
  if Result then
  begin
    lTD := GetTypeData(lTI);
    for i := lTD.MinValue to lTD.MaxValue do
    begin
      lName := Copy(GetEnumName(lTI, i), 3, 99);
      if StartsText(pStr, lName) then
      begin
        lV := TValue.FromOrdinal(lTI, i);
        pVal := lV.AsType<T>;
        Exit;
      end;
    end;
  end;
end;

class function TD2X.ToLabel<T>(pVal: T): string;
var
  lV: TValue;
begin
  lV := TValue.From<T>(pVal);
  Result := Copy(lV.ToString, 3, 99);
end;

class function TD2X.Zero<T>: T;
begin
  Result := TValue.Empty.AsType<T>;
end;

{ TD2XLogger }

constructor TD2XLogger.Create;
begin
  inherited;

  fMyWriter := nil;
  fLog := nil;
  fLogger := nil;
end;

destructor TD2XLogger.Destroy;
begin
  StopLog;

  inherited;
end;

procedure TD2XLogger.JoinLog(pLogger: ID2XLogger);
begin
  fLogger := pLogger;
end;

procedure TD2XLogger.Log(pFmt: string; pArgs: array of const; pLine: Boolean);
begin
  if Assigned(fLogger) then
    fLogger.Log(pFmt, pArgs, pLine)
  else
    if Assigned(fLog) then
      if pLine then
        fLog.WriteLine(pFmt, pArgs)
      else
        fLog.Write(pFmt, pArgs);
end;

function TD2XLogger.StartLog(pStream: TStream): TD2XLogger;
begin
  Result := Self;
  StopLog;
  if Assigned(pStream) then
  begin
    fMyWriter := TStreamWriter.Create(pStream);
    fLog := fMyWriter;
  end;
end;

function TD2XLogger.StartLog(pWriter: TTextWriter): TD2XLogger;
begin
  Result := Self;
  StopLog;
  fLog := pWriter;
end;

function TD2XLogger.StartLog(pLogger: ID2XLogger): TD2XLogger;
begin
  Result := Self;
  StopLog;
  JoinLog(pLogger);
end;

function TD2XLogger.StartLog(pString: TStringBuilder): TD2XLogger;
begin
  Result := Self;
  StopLog;
  if Assigned(pString) then
  begin
    fMyWriter := TStringWriter.Create(pString);
    fLog := fMyWriter;
  end;
end;

procedure TD2XLogger.StopLog;
begin
  FreeAndNil(fMyWriter);
  fLog := nil;
  fLogger := nil;
end;

{ TD2XInterfaced }

function TD2XInterfaced.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TD2XInterfaced._AddRef: Integer;
begin
  Result := -1;
end;

function TD2XInterfaced._Release: Integer;
begin
  Result := -1;
end;

end.
