unit D2X.Params;

interface

uses
  System.Classes,
  System.Types,
  D2X,
  D2X.IO,
  D2X.Param;

type
  TD2XFlagsParam = class(TD2XParam)
  public
    constructor Create(pCode, pLabel, pSample, pDescr: String;
      pParser: TD2XStringCheckRef); override;
    constructor CreateFlags(pFlags: TD2XFlagDefines);
    destructor Destroy; override;

    procedure Describe(pL: ID2XLogger); override;
    procedure Report(pL: ID2XLogger); override;
    procedure Output(pSL: TStringList); override;
    function IsDefault: Boolean; override;
    procedure Zero; override;
    procedure Reset; override;

  protected
    function GetSample: String; override;
    function GetFormatted(pDefault: Boolean): String; override;

  private
    fFlags: TD2XFlagDefines;
    fValues: array of TD2XBoolFlag;

    function GetByCode(pCode: String): ID2XFlag;
    function GetByLabel(pLabel: String): ID2XFlag;

    function ParseFlags(pStr: String): Boolean;

    function ProcessLabels(pStr: String): Boolean;
    function ProcessCodes(pStr: String): Boolean;

  public
    property ByCode[pCode: String]: ID2XFlag read GetByCode;
    property ByLabel[pLabel: String]: ID2XFlag read GetByLabel;
  end;

  TD2XBooleanParam = class(TD2XSingleParam<Boolean>, ID2XFlag)
  public
    constructor CreateParam(pCode, pLabel, pSample, pDescr: String; pDefault: Boolean;
      pConverter: TD2XSingleParam<Boolean>.TspConverter;
      pFormatter: TD2XSingleParam<Boolean>.TspFormatter;
      pValidator: TD2XSingleParam<Boolean>.TspValidator); override;
    constructor CreateBool(pCode, pLabel, pDescr: String; pDefault: Boolean = False);

  private
    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);
  end;

  TD2XStringParam = class(TD2XSingleParam<String>)
  public
    constructor CreateParam(pCode, pLabel, pSample, pDescr: String; pDefault: String;
      pConverter: TD2XSingleParam<String>.TspConverter;
      pFormatter: TD2XSingleParam<String>.TspFormatter;
      pValidator: TD2XSingleParam<String>.TspValidator); override;
    constructor CreateStr(pCode, pLabel, pSample, pDescr, pDefault: String;
      pConverter: TD2XSingleParam<String>.TspConverter;
      pValidator: TD2XSingleParam<String>.TspValidator);

  private
    function ConvertString(pStr: String; pDflt: String; out pVal: String): Boolean;
    function FormatString(pVal: String): String;
  end;

  TD2XFlaggedStringParam = class(TD2XSingleParam<String>, ID2XFlag)
  public type
    TfspFormatter = reference to function(pFlag: Boolean; pVal: String): String;

  public
    constructor CreateParam(pCode, pLabel, pSample, pDescr: String; pDefault: String;
      pConverter: TD2XSingleParam<String>.TspConverter;
      pFormatter: TD2XSingleParam<String>.TspFormatter;
      pValidator: TD2XSingleParam<String>.TspValidator); override;
    constructor CreateFlagStr(pCode, pLabel, pSample, pDescr, pStrDefault: String;
      pFlagDefault: Boolean; pStrConverter: TD2XSingleParam<String>.TspConverter;
      pStrValidator: TD2XSingleParam<String>.TspValidator; pFormatter: TfspFormatter);

    procedure Reset; override;
    function IsDefault: Boolean; override;
    procedure Zero; override;

  protected
    function GetSample: String; override;
    function GetFormatted(pDefault: Boolean): String; override;

  private
    fFlagDefault: Boolean;
    fFlag: Boolean;
    fStrConverter: TD2XSingleParam<String>.TspConverter;
    fFlagFormatter: TfspFormatter;

    function ConvertString(pStr: String; pDflt: String; out pVal: String): Boolean;
    function FormatFlagString(pFlag: Boolean; pVal: String): String;
    function FormatString(pVal: String): String;

    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);

  public
    property FlagValue: Boolean read fFlag write fFlag;
  end;

  TD2XDefinesParam = class(TD2XSingleParam<Boolean>, ID2XFlag)
  public
    constructor CreateParam(pCode, pLabel, pSample, pDescr: String; pDefault: Boolean;
      pConverter: TD2XSingleParam<Boolean>.TspConverter;
      pFormatter: TD2XSingleParam<Boolean>.TspFormatter;
      pValidator: TD2XSingleParam<Boolean>.TspValidator); override;
    constructor CreateDefines(pCode, pLabel: String; pDefinesFileName: TD2XNamedStreamRef);
    destructor Destroy; override;

    procedure Report(pL: ID2XLogger); override;
    procedure Output(pSL: TStringList); override;
    procedure Reset; override;
    procedure Zero; override;

  private type
    TDefineProc = procedure(pDef: String) of object;

  private
    fDefines: TStringList;
    fDefinesFileName: TD2XNamedStreamRef;

    function ConvertDefines(pStr: String; pDflt: Boolean; out pVal: Boolean): Boolean;
    function FormatDefines(pVal: Boolean): String;

    procedure IncludeDefine(pDef: String);
    procedure RemoveDefine(pDef: String);

    procedure IncludeDefines(pDefs: String);
    procedure RemoveDefines(pDefs: String);

    procedure ProcessDefines(pDefs: String; pFunc: TDefineProc);
    procedure LoadDefines(pName: String; pFunc: TDefineProc);

    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);

  public
    property Defines: TStringList read fDefines;
  end;

implementation

uses
  System.StrUtils,
  System.SysUtils;

function ConvertBoolean(pStr: String; pDflt: Boolean;
  out pVal: Boolean): Boolean;
begin
  Result := True;
  if Length(pStr) > 0 then
    case pStr[1] of
      '+', 'T', 't', 'Y', 'y', '1':
        pVal := True;
      '-', 'F', 'f', 'N', 'n', '0':
        pVal := False;
      '?', 'D':
        pVal := pDflt;
    else
      Result := False;
    end
  else
    pVal := True;
end;

function FormatBoolean(pVal: Boolean): String;
begin
  Result := IfThen(pVal, '+', '-');
end;

{ TD2XBooleanParam }

constructor TD2XBooleanParam.CreateBool(pCode, pLabel, pDescr: String; pDefault: Boolean);
begin
  inherited CreateParam(pCode, pLabel, '[+|-]', pDescr, pDefault, ConvertBoolean,
    FormatBoolean, nil);
end;

constructor TD2XBooleanParam.CreateParam(pCode, pLabel, pSample, pDescr: String;
  pDefault: Boolean; pConverter: TD2XSingleParam<Boolean>.TspConverter;
  pFormatter: TD2XSingleParam<Boolean>.TspFormatter;
  pValidator: TD2XSingleParam<Boolean>.TspValidator);
begin
  raise EInvalidParam.Create('Need to use correct constructor');
end;

function TD2XBooleanParam.GetFlag: Boolean;
begin
  Result := fValue;
end;

procedure TD2XBooleanParam.SetFlag(pVal: Boolean);
begin
  SetValue(pVal);
end;

{ TD2XStringParam }

function TD2XStringParam.ConvertString(pStr: String; pDflt: String; out pVal: String): Boolean;
begin
  Result := True;
  pVal := pStr;
end;

constructor TD2XStringParam.CreateParam(pCode, pLabel, pSample, pDescr, pDefault: String;
  pConverter: TD2XSingleParam<String>.TspConverter;
  pFormatter: TD2XSingleParam<String>.TspFormatter;
  pValidator: TD2XSingleParam<String>.TspValidator);
begin
  raise EInvalidParam.Create('Need to use correct constructor');
end;

constructor TD2XStringParam.CreateStr(pCode, pLabel, pSample, pDescr, pDefault: String;
  pConverter: TD2XSingleParam<String>.TspConverter;
  pValidator: TD2XSingleParam<String>.TspValidator);
begin
  if Assigned(pConverter) then
    inherited CreateParam(pCode, pLabel, pSample, pDescr, pDefault, pConverter, FormatString,
      pValidator)
  else
    inherited CreateParam(pCode, pLabel, pSample, pDescr, pDefault, ConvertString,
      FormatString, pValidator);
end;

function TD2XStringParam.FormatString(pVal: String): String;
begin
  Result := pVal;
end;

{ TD2XFlaggedStringParam }

function TD2XFlaggedStringParam.ConvertString(pStr: String; pDflt: String;
  out pVal: String): Boolean;
begin
  Result := False;
  pVal := fValue;
  if (pStr = '') or (pStr = '+') or (pStr = '-') then
  begin
    fFlag := pStr <> '-';
    if Assigned(fStrConverter) then
      Result := fStrConverter(fValue, pDflt, pVal)
    else
      Result := True;
  end;
  if (Length(pStr) >= 1) and (pStr[1] = ':') then
  begin
    fFlag := True;
    if Assigned(fStrConverter) then
      Result := fStrConverter(Copy(pStr, 2, Length(pStr)), pDflt, pVal)
    else
    begin
      pVal := Copy(pStr, 2, Length(pStr));
      Result := True;
    end;
  end;
end;

constructor TD2XFlaggedStringParam.CreateFlagStr(pCode, pLabel, pSample, pDescr,
  pStrDefault: String; pFlagDefault: Boolean;
  pStrConverter: TD2XSingleParam<String>.TspConverter;
  pStrValidator: TD2XSingleParam<String>.TspValidator; pFormatter: TfspFormatter);
begin
  fStrConverter := pStrConverter;
  fFlagDefault := pFlagDefault;
  if Assigned(pFormatter) then
    fFlagFormatter := pFormatter
  else
    fFlagFormatter := FormatFlagString;

  inherited CreateParam(pCode, pLabel, pSample, pDescr, pStrDefault, ConvertString,
    FormatString, pStrValidator);
end;

constructor TD2XFlaggedStringParam.CreateParam(pCode, pLabel, pSample, pDescr,
  pDefault: String;
  pConverter: TD2XSingleParam<String>.TspConverter;
  pFormatter: TD2XSingleParam<String>.TspFormatter;
  pValidator: TD2XSingleParam<String>.TspValidator);
begin
  raise EInvalidParam.Create('Need to use correct constructor');
end;

function TD2XFlaggedStringParam.FormatFlagString(pFlag: Boolean; pVal: String): String;
begin
  if pFlag then
    if pVal > '' then
      Result := ':' + pVal
    else
      Result := '+'
  else
    if pVal > '' then
      Result := '-(' + pVal + ')'
    else
      Result := '-';
end;

function TD2XFlaggedStringParam.FormatString(pVal: String): String;
begin
  raise EInvalidParam.Create('Incorrect call to TD2XFlaggedStringParam.FormatString');
end;

function TD2XFlaggedStringParam.GetFlag: Boolean;
begin
  Result := fFlag;
end;

function TD2XFlaggedStringParam.GetFormatted(pDefault: Boolean): String;
begin
  if pDefault then
    Result := fFlagFormatter(fFlagDefault, fDefault)
  else
    Result := fFlagFormatter(fFlag, fValue);
end;

function TD2XFlaggedStringParam.GetSample: String;
begin
  Result := '[+-]:' + fSample;
end;

function TD2XFlaggedStringParam.IsDefault: Boolean;
begin
  Result := (fValue = fDefault) and (fFlag = fFlagDefault);
end;

procedure TD2XFlaggedStringParam.Reset;
begin
  inherited;

  fFlag := fFlagDefault;
end;

procedure TD2XFlaggedStringParam.SetFlag(pVal: Boolean);
begin
  fFlag := pVal;
end;

procedure TD2XFlaggedStringParam.Zero;
begin
  Value := '';
  fFlag := False;
end;

{ TD2XDefinesParam }

function TD2XDefinesParam.ConvertDefines(pStr: String; pDflt: Boolean;
  out pVal: Boolean): Boolean;
var
  lStr: String;
begin
  Result := False;
  if (pStr = '!') or (pStr = ':') then
  begin
    Result := True;
    fDefines.Clear;
    pVal := pStr = ':';
  end
  else
    if Length(pStr) > 1 then
    begin
      lStr := System.Copy(pStr, 2, Length(pStr));
      Result := True;
      pVal := True;
      case pStr[1] of
        '+':
          IncludeDefines(lStr);
        '-':
          RemoveDefines(lStr);
        ':':
          begin
            fDefines.Clear;
            LoadDefines(MakeFileName(lStr, '.def'), IncludeDefine);
          end;
      else
        begin
          Result := False;
          pVal := False;
        end;
      end;
    end;
end;

constructor TD2XDefinesParam.CreateDefines(pCode, pLabel: String;
  pDefinesFileName: TD2XNamedStreamRef);
begin
  fDefines := TStringList.Create;
  fDefines.Sorted := True;

  inherited CreateParam(pCode, pLabel, '[+-!:]<def>', 'Add(+), Remove(-), Clear(!) or Load(:) '
      + pLabel, False, ConvertDefines, FormatDefines, nil);

  fDefinesFileName := pDefinesFileName;
end;

constructor TD2XDefinesParam.CreateParam(pCode, pLabel, pSample, pDescr: String;
  pDefault: Boolean; pConverter: TD2XSingleParam<Boolean>.TspConverter;
  pFormatter: TD2XSingleParam<Boolean>.TspFormatter;
  pValidator: TD2XSingleParam<Boolean>.TspValidator);
begin
  raise EInvalidParam.Create('Need to use correct constructor');
end;

destructor TD2XDefinesParam.Destroy;
begin
  FreeAndNil(fDefines);

  inherited;
end;

function TD2XDefinesParam.FormatDefines(pVal: Boolean): String;
begin
  Result := '';
end;

function TD2XDefinesParam.GetFlag: Boolean;
begin
  Result := fValue;
end;

procedure TD2XDefinesParam.IncludeDefine(pDef: String);
begin
  pDef := UpperCase(pDef);
  if fDefines.IndexOf(pDef) < 0 then
    fDefines.Add(pDef);
end;

procedure TD2XDefinesParam.IncludeDefines(pDefs: String);
begin
  ProcessDefines(pDefs, IncludeDefine);
end;

procedure TD2XDefinesParam.LoadDefines(pName: String; pFunc: TDefineProc);
var
  lF: ID2XFile;
  lSL: TStringList;
  lS: String;
begin
  lF := fDefinesFileName(pName);
  if Assigned(lF) then
    try
      lSL := TStringList.Create;
      lSL.LoadFromStream(lF.ReadFrom.BaseStream);
      for lS in lSL do
        pFunc(lS);
    finally
      FreeAndNil(lSL);
      DisposeOf(lF);
    end;
end;

procedure TD2XDefinesParam.Output(pSL: TStringList);
begin
  if Value then
  begin
    pSL.Add('-' + fCode + ':');
    fDefines.Sort;
    pSL.Add('-' + fCode + '+' + fDefines.CommaText);
  end;
end;

procedure TD2XDefinesParam.ProcessDefines(pDefs: String; pFunc: TDefineProc);
var
  lSL: TStringList;
  lS: String;
begin
  if StartsText('~', pDefs) then
    LoadDefines(Copy(pDefs, 2, Length(pDefs)), pFunc)
  else
  begin
    lSL := TStringList.Create;
    try
      lSL.CommaText := pDefs;
      for lS in lSL do
        pFunc(lS);
    finally
      FreeAndNil(lSL);
    end;
  end;
end;

procedure TD2XDefinesParam.RemoveDefine(pDef: String);
var
  lIdx: Integer;
begin
  lIdx := fDefines.IndexOf(UpperCase(pDef));
  if lIdx >= 0 then
    fDefines.Delete(lIdx);
end;

procedure TD2XDefinesParam.RemoveDefines(pDefs: String);
begin
  ProcessDefines(pDefs, RemoveDefine);
end;

procedure TD2XDefinesParam.Report(pL: ID2XLogger);
var
  lS: String;
  w: Integer;

  procedure WriteWidth(pStr: String);
  begin
    pL.Log('%s', [pStr], False);
    Inc(w, Length(pStr));
  end;

begin
  if Value then
    if fDefines.Count < 1 then
      pL.Log('Use NO %s', [fLabel])
    else
    begin
      pL.Log('Use these %s:', [fLabel]);
      w := 0;
      fDefines.Sort;
      for lS in fDefines do
      begin
        if w = 0 then
          WriteWidth('    ')
        else
          if (w + Length(lS)) > 78 then
          begin
            pL.Log('', []);
            w := 0;
            WriteWidth('    ');
          end
          else
            WriteWidth(', ');
        WriteWidth(lS);
      end;
      pL.Log('', []);
    end
  else
    pL.Log('Use default %s', [fLabel]);
end;

procedure TD2XDefinesParam.Reset;
begin
  inherited;

  fDefines.Clear;
end;

procedure TD2XDefinesParam.SetFlag(pVal: Boolean);
begin
  SetValue(pVal);
end;

procedure TD2XDefinesParam.Zero;
begin
  inherited;

  fDefines.Clear;
end;

{ TD2XFlagsParam }

constructor TD2XFlagsParam.Create(pCode, pLabel, pSample, pDescr: String;
  pParser: TD2XStringCheckRef);
begin
  raise EInvalidParam.Create('Need to use correct constructor');
end;

constructor TD2XFlagsParam.CreateFlags(pFlags: TD2XFlagDefines);
var
  i: Integer;
begin
  if Length(pFlags) < 1 then
    raise EInvalidParam.Create('Need to initialize some Flags');

  inherited Create('F', 'Flags', '[+Code*-Code*|:Label,*!Label,*]', 'Flags', ParseFlags);

  SetLength(fFlags, Length(pFlags));
  SetLength(fValues, Length(pFlags));

  for i := 0 to High(pFlags) do
  begin
    fFlags[i] := pFlags[i];
    fValues[i] := TD2XBoolFlag.Create;
    ID2XFlag(fValues[i]).Flag := pFlags[i].FlagDefault;
  end;
end;

procedure TD2XFlagsParam.Describe(pL: ID2XLogger);
var
  i: Integer;
begin
  pL.Log('  %1s%-28s %s', [fCode, GetSample, fDescr]);
  pL.Log('    %-4s %-15s %-3s %s', ['Code', 'Label', 'Def', 'Description']);
  for i := 0 to High(fFlags) do
    with fFlags[i] do
      pL.Log('    %-4s %-15s %-3s %s', [FlagCode, FlagLabel, IfThen(FlagDefault, '+', '-'),
          FlagDescr]);
end;

destructor TD2XFlagsParam.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(fValues) do
    FreeAndNil(fValues[i]);

  inherited;
end;

function TD2XFlagsParam.GetByCode(pCode: String): ID2XFlag;
var
  i: Integer;
begin
  for i := 0 to High(fFlags) do
    if pCode = fFlags[i].FlagCode then
      Result := fValues[i];
end;

function TD2XFlagsParam.GetByLabel(pLabel: String): ID2XFlag;
var
  i: Integer;
begin
  for i := 0 to High(fFlags) do
    if pLabel = fFlags[i].FlagLabel then
      Result := fValues[i];
end;

function TD2XFlagsParam.GetFormatted(pDefault: Boolean): String;
var
  i: Integer;
  lSep: String;
begin
  Result := '';
  lSep := '+';
  for i := 0 to High(fFlags) do
    if (pDefault and fFlags[i].FlagDefault) or (not pDefault and ID2XFlag(fValues[i]).Flag)
    then
    begin
      Result := Result + lSep + fFlags[i].FlagCode;
      lSep := '';
    end;
  lSep := '-';
  for i := 0 to High(fFlags) do
    if (pDefault and not fFlags[i].FlagDefault) or not(pDefault or ID2XFlag(fValues[i]).Flag)
    then
    begin
      Result := Result + lSep + fFlags[i].FlagCode;
      lSep := '';
    end;
end;

function TD2XFlagsParam.GetSample: String;
begin
  Result := fSample;
end;

function TD2XFlagsParam.IsDefault: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(fFlags) do
    Result := Result and (ID2XFlag(fValues[i]).Flag = fFlags[i].FlagDefault);
end;

procedure TD2XFlagsParam.Output(pSL: TStringList);
var
  i: Integer;
  lS, lSep: String;
begin
  pSL.Add('-F!');
  lS := '-F';
  lSep := '+';
  for i := 0 to High(fFlags) do
    if not fFlags[i].FlagDefault and (ID2XFlag(fValues[i]).Flag <> fFlags[i].FlagDefault) then
    begin
      lS := lS + lSep + fFlags[i].FlagCode;
      lSep := '';
    end;
  lSep := '-';
  for i := 0 to High(fFlags) do
    if fFlags[i].FlagDefault and (ID2XFlag(fValues[i]).Flag <> fFlags[i].FlagDefault) then
    begin
      lS := lS + lSep + fFlags[i].FlagCode;
      lSep := '';
    end;
  if lS > '-F' then
    pSL.Add(lS);
end;

function TD2XFlagsParam.ParseFlags(pStr: String): Boolean;
var
  lStr: String;
begin
  Result := False;
  if (Length(pStr) > 1 ) and (pStr[1] = '!') then
  begin
    Result := True;
    Reset;
    lStr := System.Copy(pStr, 2, Length(pStr));
  end
  else
    lStr := pStr;
  if Length(lStr) > 1 then
  begin
    if lStr[1] = ':' then
      Result := ProcessLabels(System.Copy(lStr, 2, Length(lStr)))
    else
      Result := ProcessCodes(lStr);
  end;
end;

function TD2XFlagsParam.ProcessCodes(pStr: String): Boolean;
var
  lVal: Boolean;
  lFlag: ID2XFlag;
  i: Integer;
begin
  lVal := True;
  Result := True;
  for i := 1 to Length(pStr) do
    case pStr[i] of
      '+', '-':
        lVal := pStr[i] = '+';
    else
      lFlag := ByCode[pStr[i]];
      if Assigned(lFlag) then
        lFlag.Flag := lVal
      else
        Result := False;
    end;
end;

function TD2XFlagsParam.ProcessLabels(pStr: String): Boolean;
var
  lSL: TStringList;
  lS, lLabel: String;
  lVal: Boolean;
  lFlag: ID2XFlag;
begin
  Result := True;
  lSL := TStringList.Create;
  try
    lSL.CommaText := pStr;
    for lS in lSL do
    begin
      lVal := True;
      if StartsText('+', lS) or StartsText('-', lS) then
      begin
        lVal := StartsText('+', lS);
        lLabel := System.Copy(lS, 2, Length(lS));
      end
      else
        lLabel := lS;

      if EndsText('+', lLabel) or EndsText('-', lLabel) then
      begin
        lVal := EndsText('+', lLabel);
        lLabel := System.Copy(lS, 1, Length(lLabel) - 1);
      end;
      lFlag := ByLabel[lLabel];
      if Assigned(lFlag) then
        lFlag.Flag := lVal
      else
        Result := False;
    end;
  finally
    FreeAndNil(lSL);
  end;
end;

procedure TD2XFlagsParam.Report(pL: ID2XLogger);
var
  i: Integer;
  lS, lSep: String;
begin
  lS := '';
  lSep := '';
  for i := 0 to High(fFlags) do
  begin
    lS := lS + lSep + fFlags[i].FlagLabel;
    if ID2XFlag(fValues[i]).Flag then
      lS := lS + '+'
    else
      lS := lS + '-';
    lSep := ',';
  end;
  pL.Log(' %-15s %s', [fLabel, lS]);
end;

procedure TD2XFlagsParam.Reset;
var
  i: Integer;
begin
  for i := 0 to High(fFlags) do
    ID2XFlag(fValues[i]).Flag := fFlags[i].FlagDefault;
end;

procedure TD2XFlagsParam.Zero;
var
  i: Integer;
begin
  for i := 0 to High(fFlags) do
    ID2XFlag(fValues[i]).Flag := False;
end;

end.
