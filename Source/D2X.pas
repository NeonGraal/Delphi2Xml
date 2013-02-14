unit D2X;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
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

  TD2XCheckRef = reference to function: Boolean;
  TD2XStringRef = reference to function: string;
  TD2XStringCheckRef = reference to function(pStr: string): Boolean;
  TD2XLogMessage = reference to procedure(pType, pMsg: string; pX, pY: Integer);

  TStrIntPair = TPair<string, Integer>;
  TStrIntDict = TDictionary<string, Integer>;
  TPairLogRef = reference to function(pPair: TStrIntPair): string;

procedure OutputStrIntDict(pDict: TStrIntDict; pStream: TStreamWriter; pFunc: TPairLogRef);

function MakeFileName(pStr, pDflt: string): string;
function TidyFilename(pFilename: string): string;

function ExtractGroup(pName: string): string;

implementation

uses
  System.RegularExpressions,
  System.StrUtils,
  System.TypInfo;

procedure OutputStrIntDict(pDict: TStrIntDict; pStream: TStreamWriter; pFunc: TPairLogRef);
var
  lP: TStrIntPair;
begin
  Assert(Assigned(pStream), 'Need a Stream');
  Assert(Assigned(pFunc), 'Need a Function');
  Assert(Assigned(pDict), 'Need a Dictionary');
  if pDict.Count > 0 then
    with TStringList.Create do
      try
        for lP in pDict do
          if lP.Value > 0 then
            Values[lP.Key] := pFunc(lP);
        Sort;
        SaveToStream(pStream.BaseStream);
      finally
        Free;
      end;
end;

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

function TidyFilename(pFilename: string): string;
begin
  Result := ReplaceStr(ReplaceStr(ReplaceStr(ReplaceStr(pFilename, '*', ''), '.', ''), '?',
      ''), '\', '-');
end;

var
  gGroupRE: TRegEx;

function ExtractGroup(pName: string): string;
var
  lM: TMatch;
begin
  Result := '';
  lM := gGroupRE.Match(ExtractFileName(pName));
  if lM.Success then
    Result := lM.Groups[1].Value;
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

initialization
  gGroupRE := TRegEx.Create('^(\w+\.\w+)\.');
end.
