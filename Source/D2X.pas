unit D2X;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils;

type
  TD2XParseMode = (pmFull, pmUses);

  TD2XElapsedMode = (emNone, emQuiet, emTotal, emProcessing);

  TD2XResultPer = (rpFile, rpWildcard, rpSubDir, rpDir, rpParam, rpRun);

  TD2X = class
    class function Zero<T>: T;
    class function ToLabel<T>(pVal: T): string;
    class function CnvDflt<T>(pStr: string; pDflt: T; out pVal: T): Boolean;
  end;

  ID2XLogger = interface
    procedure JoinLog(pLogger: ID2XLogger);
    procedure Log(pFmt: string; pArgs: array of const; pLine: Boolean = True);
  end;

  TD2XLogger = class(TInterfacedObject, ID2XLogger)
  private
    fMyWriter: TTextWriter;
    fLog: TTextWriter;
    fLogger: ID2XLogger;

  public
    constructor Create; overload; virtual;
    constructor Create(pStream: TStream); overload;
    constructor Create(pString: TStringBuilder); overload;
    constructor Create(pWriter: TTextWriter); overload;
    constructor Create(pLogger: ID2XLogger); overload;
    destructor Destroy; override;

    procedure StartLog(pStream: TStream); overload;
    procedure StartLog(pString: TStringBuilder); overload;
    procedure StartLog(pWriter: TTextWriter); overload;
    procedure StartLog(pLogger: ID2XLogger); overload;
    procedure StopLog;

    procedure JoinLog(pLogger: ID2XLogger);
    procedure Log(pFmt: string; pArgs: array of const; pLine: Boolean = True);

    procedure Lock;
    procedure Unlock;
  end;

  TD2XCheckRef = reference to function: Boolean;
  TD2XStringRef = reference to function: string;
  TD2XNamedStringRef = reference to function(pFile: String): string;
  TD2XStringCheckRef = reference to function(pStr: string): Boolean;
  TD2XLogMessage = reference to procedure(pType, pMsg: string; pX, pY: Integer);

  TStrIntPair = TPair<string, Integer>;
  TStrIntDict = TDictionary<string, Integer>;
  TPairLogMethod = reference to function(pPair: TStrIntPair): string;

procedure OutputStrIntDict(pDict: TStrIntDict; pStream: TStream; pFunc: TPairLogMethod);

function MakeFileName(pStr, pDflt: string): string;
function TidyFilename(pFilename: string): string;

implementation

uses
  System.StrUtils;

procedure OutputStrIntDict(pDict: TStrIntDict; pStream: TStream; pFunc: TPairLogMethod);
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
        SaveToStream(pStream);
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
  Result := ReplaceStr(ReplaceStr(ReplaceStr(pFilename, '*', ''), '.', ''), '?', '');
end;

{ TD2X }

class function TD2X.CnvDflt<T>(pStr: string; pDflt: T; out pVal: T): Boolean;
begin
  pVal := pDflt;
  Result := True;
end;

class function TD2X.ToLabel<T>(pVal: T): string;
var
  lV: TValue;
begin
  lV := TValue.From<T>(pVal);
  Result := Copy(lV.ToString, 3, 99);
end;

{ TD2XLogger }

constructor TD2XLogger.Create;
begin
  inherited;

  fMyWriter := nil;
  fLog := nil;
  fLogger := nil;
end;

constructor TD2XLogger.Create(pStream: TStream);
begin
  Create;

  StartLog(pStream);
end;

constructor TD2XLogger.Create(pWriter: TTextWriter);
begin
  Create;

  StartLog(pWriter);
end;

constructor TD2XLogger.Create(pLogger: ID2XLogger);
begin
  Create;

  StartLog(pLogger);
end;

constructor TD2XLogger.Create(pString: TStringBuilder);
begin
  Create;

  StartLog(pString);
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

procedure TD2XLogger.Lock;
begin
  _AddRef;
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

procedure TD2XLogger.StartLog(pStream: TStream);
begin
  StopLog;
  if Assigned(pStream) then
  begin
    fMyWriter := TStreamWriter.Create(pStream);
    fLog := fMyWriter;
  end;
end;

procedure TD2XLogger.StartLog(pWriter: TTextWriter);
begin
  StopLog;
  fLog := pWriter;
end;

procedure TD2XLogger.StartLog(pLogger: ID2XLogger);
begin
  StopLog;
  JoinLog(pLogger);
end;

procedure TD2XLogger.StartLog(pString: TStringBuilder);
begin
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

procedure TD2XLogger.Unlock;
begin
  _Release;
end;

class function TD2X.Zero<T>: T;
begin
  Result := TValue.Empty.AsType<T>;
end;

end.
