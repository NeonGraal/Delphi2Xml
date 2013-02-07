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
    procedure Output(pSL: TStringList); override;
    function IsDefault: Boolean; override;
    procedure Zero; override;
    procedure Reset; override;

  protected
    function GetFormatted(pDefault: Boolean): String; override;
    function GetReportDetails(pStr: String = ''): String; override;

  private
    fFlags: TD2XFlagDefines;
    fValues: array of TD2XBoolFlag;

    function IndexCode(pCode: Char): Integer;
    function IndexLabel(pLabel: String): Integer;

    function GetByCode(pCode: Char): ID2XFlag;
    function GetByLabel(pLabel: String): ID2XFlag;

    function ParseFlags(pStr: String): Boolean;

    function ProcessLabels(pStr: String): Boolean;
    function ProcessCodes(pStr: String): Boolean;

  public
    property ByCode[pCode: Char]: ID2XFlag read GetByCode;
    property ByLabel[pLabel: String]: ID2XFlag read GetByLabel;
  end;

  TD2XListParam = class(TD2XParam)
  public
    constructor Create(pCode, pLabel, pSample, pDescr: String;
      pParser: TD2XStringCheckRef); override;
    constructor CreateList(pCode, pLabel, pDescr, pExtn: String;
      pListFileName: TD2XNamedStreamRef);
    destructor Destroy; override;

    procedure Output(pSL: TStringList); override;
    function IsDefault: Boolean; override;
    procedure Zero; override;
    procedure Reset; override;

  protected type
    TItemProc = procedure(pItem: String) of object;

  protected
    function GetReportDetails(pStr: String = ''): String; override;

  private
    fList: TStringList;
    fExtn: String;
    fListFileName: TD2XNamedStreamRef;

    function ParseList(pStr: String): Boolean;

    procedure LoadItems(pFilename: String; pFunc: TItemProc);
    procedure ProcessItems(pItems: String; pFunc: TItemProc);

    procedure AddItem(pItem: String);

  public
    property List: TStringList read fList;
  end;

  TD2XStringParam = class(TD2XSingleParam<String>)
  public
    constructor CreateParam(pCode, pLabel, pSample, pDescr: String; pDefault: String;
      pConverter: TD2XSingleParam<String>.TspConverter;
      pFormatter: TD2XSingleParam<String>.TspFormatter); override;
    constructor CreateParamValid(pCode, pLabel, pSample, pDescr: String; pDefault: String;
      pConverter: TD2XSingleParam<String>.TspConverter;
      pFormatter: TD2XSingleParam<String>.TspFormatter;
      pValidator: TD2XSingleParam<String>.TspValidator); override;
    constructor CreateParamOnSet(pCode, pLabel, pSample, pDescr: String; pDefault: String;
      pConverter: TD2XSingleParam<String>.TspConverter;
      pFormatter: TD2XSingleParam<String>.TspFormatter;
      pOnSet: TD2XSingleParam<String>.TspOnSet); override;
    constructor CreateStr(pCode, pLabel, pSample, pDescr, pDefault: String;
      pConverter: TD2XSingleParam<String>.TspConverter);
    constructor CreateStrValid(pCode, pLabel, pSample, pDescr, pDefault: String;
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
      pFormatter: TD2XSingleParam<String>.TspFormatter); override;
    constructor CreateParamValid(pCode, pLabel, pSample, pDescr: String; pDefault: String;
      pConverter: TD2XSingleParam<String>.TspConverter;
      pFormatter: TD2XSingleParam<String>.TspFormatter;
      pStrValidator: TD2XSingleParam<String>.TspValidator); override;
    constructor CreateParamOnSet(pCode, pLabel, pSample, pDescr: String; pDefault: String;
      pConverter: TD2XSingleParam<String>.TspConverter;
      pFormatter: TD2XSingleParam<String>.TspFormatter;
      pOnSet: TD2XSingleParam<String>.TspOnSet); override;
    constructor CreateFlagStr(pCode, pLabel, pSample, pDescr, pStrDefault: String;
      pFlagDefault: Boolean; pStrConverter: TD2XSingleParam<String>.TspConverter);
    constructor CreateFlagStrFmt(pCode, pLabel, pSample, pDescr, pStrDefault: String;
      pFlagDefault: Boolean; pStrConverter: TD2XSingleParam<String>.TspConverter;
      pFormatter: TfspFormatter);

    procedure Convert(pStr: String); override;
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
      pFormatter: TD2XSingleParam<Boolean>.TspFormatter); override;
    constructor CreateDefines(pCode, pLabel: String; pDefinesFileName: TD2XNamedStreamRef);
    destructor Destroy; override;

    procedure Output(pSL: TStringList); override;
    procedure Reset; override;
    procedure Zero; override;

  private type
    TDefineProc = procedure(pDef: String) of object;

  protected
    function GetReportDetails(pStr: String = ''): String; override;

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

{ TD2XStringParam }

function TD2XStringParam.ConvertString(pStr: String; pDflt: String; out pVal: String): Boolean;
begin
  Result := True;
  pVal := pStr;
end;

constructor TD2XStringParam.CreateParam(pCode, pLabel, pSample, pDescr, pDefault: String;
  pConverter: TD2XSingleParam<String>.TspConverter;
  pFormatter: TD2XSingleParam<String>.TspFormatter);
begin
  InvalidConstructor;
end;

constructor TD2XStringParam.CreateParamOnSet(pCode, pLabel, pSample, pDescr, pDefault: String;
  pConverter: TD2XSingleParam<String>.TspConverter;
  pFormatter: TD2XSingleParam<String>.TspFormatter; pOnSet: TD2XSingleParam<String>.TspOnSet);
begin
  InvalidConstructor;
end;

constructor TD2XStringParam.CreateParamValid(pCode, pLabel, pSample, pDescr, pDefault: String;
  pConverter: TD2XSingleParam<String>.TspConverter;
  pFormatter: TD2XSingleParam<String>.TspFormatter;
  pValidator: TD2XSingleParam<String>.TspValidator);
begin
  InvalidConstructor;
end;

constructor TD2XStringParam.CreateStr(pCode, pLabel, pSample, pDescr, pDefault: String;
  pConverter: TD2XSingleParam<String>.TspConverter);
begin
  if Assigned(pConverter) then
    inherited CreateParam(pCode, pLabel, pSample, pDescr, pDefault, pConverter, FormatString)
  else
    inherited CreateParam(pCode, pLabel, pSample, pDescr, pDefault, ConvertString,
      FormatString);
end;

constructor TD2XStringParam.CreateStrValid(pCode, pLabel, pSample, pDescr, pDefault: String;
  pConverter: TD2XSingleParam<String>.TspConverter;
  pValidator: TD2XSingleParam<String>.TspValidator);
begin
  if Assigned(pConverter) then
    inherited CreateParamValid(pCode, pLabel, pSample, pDescr, pDefault, pConverter,
      FormatString, pValidator)
  else
    inherited CreateParamValid(pCode, pLabel, pSample, pDescr, pDefault, ConvertString,
      FormatString, pValidator);
end;

function TD2XStringParam.FormatString(pVal: String): String;
begin
  Result := pVal;
end;

{ TD2XFlaggedStringParam }

procedure TD2XFlaggedStringParam.Convert(pStr: String);
var
  lVal: String;
begin
  if Assigned(fStrConverter) then
  begin
    if fStrConverter(pStr, fDefault, lVal) then
      Value := lVal;
  end
  else
    Value := pStr;
end;

function TD2XFlaggedStringParam.ConvertString(pStr: String; pDflt: String;
  out pVal: String): Boolean;
begin
  Result := False;
  if (pStr = '') or (pStr = '+') or (pStr = '-') or (pStr = '!') then
  begin
    Result := True;
    if pStr = '!' then
      Reset
    else
      fFlag := pStr <> '-';
    pVal := fValue;
  end
  else
    if pStr[1] = ':' then
    begin
      if Assigned(fStrConverter) then
        Result := fStrConverter(Copy(pStr, 2, Length(pStr)), pDflt, pVal)
      else
      begin
        pVal := Copy(pStr, 2, Length(pStr));
        Result := True;
      end;
      if Result then
        fFlag := True;
    end;
end;

constructor TD2XFlaggedStringParam.CreateFlagStr(pCode, pLabel, pSample, pDescr,
  pStrDefault: String; pFlagDefault: Boolean;
  pStrConverter: TD2XSingleParam<String>.TspConverter);
begin
  fStrConverter := pStrConverter;
  fFlagDefault := pFlagDefault;
  fFlagFormatter := FormatFlagString;

  inherited CreateParam(pCode, pLabel, pSample, pDescr, pStrDefault, ConvertString,
    FormatString);
end;

constructor TD2XFlaggedStringParam.CreateFlagStrFmt(pCode, pLabel, pSample, pDescr,
  pStrDefault: String; pFlagDefault: Boolean;
  pStrConverter: TD2XSingleParam<String>.TspConverter; pFormatter: TfspFormatter);
begin
  inherited CreateParam(pCode, pLabel, pSample, pDescr, pStrDefault, ConvertString,
    FormatString);

  CheckParam(Assigned(pFormatter), 'Formatter');

  fStrConverter := pStrConverter;
  fFlagDefault := pFlagDefault;
  fFlagFormatter := pFormatter;
end;

constructor TD2XFlaggedStringParam.CreateParam(pCode, pLabel, pSample, pDescr,
  pDefault: String;
  pConverter: TD2XSingleParam<String>.TspConverter;
  pFormatter: TD2XSingleParam<String>.TspFormatter);
begin
  InvalidConstructor;
end;

constructor TD2XFlaggedStringParam.CreateParamOnSet(pCode, pLabel, pSample, pDescr,
  pDefault: String; pConverter: TD2XSingleParam<String>.TspConverter;
  pFormatter: TD2XSingleParam<String>.TspFormatter; pOnSet: TD2XSingleParam<String>.TspOnSet);
begin
  InvalidConstructor;
end;

constructor TD2XFlaggedStringParam.CreateParamValid(pCode, pLabel, pSample, pDescr,
  pDefault: String; pConverter: TD2XSingleParam<String>.TspConverter;
  pFormatter: TD2XSingleParam<String>.TspFormatter;
  pStrValidator: TD2XSingleParam<String>.TspValidator);
begin
  InvalidConstructor;
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
  Convert('');
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
      + pLabel, False, ConvertDefines, FormatDefines);

  fDefinesFileName := pDefinesFileName;
end;

constructor TD2XDefinesParam.CreateParam(pCode, pLabel, pSample, pDescr: String;
  pDefault: Boolean; pConverter: TD2XSingleParam<Boolean>.TspConverter;
  pFormatter: TD2XSingleParam<Boolean>.TspFormatter);
begin
  InvalidConstructor;
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

function TD2XDefinesParam.GetReportDetails(pStr: String): String;
var
  lS: String;
  w: Integer;
  lSW: TStringWriter;

  procedure WriteWidth(pStr: String);
  begin
    lSW.Write(pStr);
    Inc(w, Length(pStr));
  end;

begin
  if Value then
    if fDefines.Count < 1 then
      Result := 'NONE'
    else
      try
        lSW := TStringWriter.Create;
        w := 0;
        fDefines.Sort;
        for lS in fDefines do
        begin
          if w > 0 then
            if (w + Length(lS)) > 78 then
            begin
              lSW.WriteLine(',');
              w := 0;
              WriteWidth('                       ');
            end
            else
              WriteWidth(', ');
          WriteWidth(lS);
        end;
        Result := lSW.ToString;
      finally
        lSW.Free;
      end
  else
    Result := 'Default';
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
  InvalidConstructor;
end;

constructor TD2XFlagsParam.CreateFlags(pFlags: TD2XFlagDefines);
var
  i: Integer;
begin
  if Length(pFlags) < 1 then
    raise EInvalidParam.Create('Need to initialize some Flags');

  inherited Create('F', 'Flags', '<codes> | :<labels>', 'Flags', ParseFlags);

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
  pL.Log('  %1s%-31s %s', [fCode, GetSample, fDescr]);
  for i := 0 to High(fFlags) do
    with fFlags[i] do
      pL.Log('    %1s %-12s %-15s %s', [FlagCode, FlagLabel, IfThen(FlagDefault, '+', '-'),
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

function TD2XFlagsParam.GetByCode(pCode: Char): ID2XFlag;
var
  i: Integer;
begin
  i := IndexCode(pCode);
  if i < 0 then
    Result := nil
  else
    Result := fValues[i];
end;

function TD2XFlagsParam.GetByLabel(pLabel: String): ID2XFlag;
var
  i: Integer;
begin
  i := IndexLabel(pLabel);
  if i < 0 then
    Result := nil
  else
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

function TD2XFlagsParam.GetReportDetails(pStr: String): String;
var
  i: Integer;
  lSL: TStringList;
begin
  lSL := TStringList.Create;
  try
    if pStr > '' then
    begin
      i := IndexLabel(pStr);
      if i < 0 then
        i := IndexCode(pStr[1]);
      if i >= 0 then
      begin
        if ID2XFlag(fValues[i]).Flag then
          lSL.Add(fFlags[i].FlagLabel + '+')
        else
          lSL.Add(fFlags[i].FlagLabel + '-');
      end;
    end
    else
      for i := 0 to High(fFlags) do
        if ID2XFlag(fValues[i]).Flag then
          lSL.Add(fFlags[i].FlagLabel + '+')
        else
          lSL.Add(fFlags[i].FlagLabel + '-');

    lSL.Sort;
    Result := lSL.CommaText;
  finally
    lSL.Free;
  end;
end;

function TD2XFlagsParam.IndexCode(pCode: Char): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(fFlags) do
    if UpCase(pCode) = fFlags[i].FlagCode then
      Result := i;
end;

function TD2XFlagsParam.IndexLabel(pLabel: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(fFlags) do
    if CompareText(pLabel, fFlags[i].FlagLabel) = 0 then
      Result := i;
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
  if not IsDefault then
  begin
    pSL.Add('-F!');
    lS := '-F';
    lSep := '+';
    for i := 0 to High(fFlags) do
      if not fFlags[i].FlagDefault and (ID2XFlag(fValues[i]).Flag <> fFlags[i].FlagDefault)
      then
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
end;

function TD2XFlagsParam.ParseFlags(pStr: String): Boolean;
var
  lStr: String;
begin
  Result := False;
  if (Length(pStr) > 0) and (pStr[1] = '!') then
  begin
    Result := True;
    Reset;
    lStr := System.Copy(pStr, 2, Length(pStr));
  end
  else
    lStr := pStr;
  if Length(lStr) > 0 then
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

{ TD2XListParam }

procedure TD2XListParam.AddItem(pItem: String);
begin
  if fList.IndexOf(pItem) < 0 then
    fList.Add(pItem);
end;

procedure TD2XListParam.ProcessItems(pItems: String; pFunc: TItemProc);
var
  lSL: TStringList;
  lS: String;
begin
  lSL := TStringList.Create;
  try
    lSL.CommaText := pItems;
    for lS in lSL do
      pFunc(lS);
  finally
    lSL.Free;
  end;
end;

constructor TD2XListParam.Create(pCode, pLabel, pSample, pDescr: String;
  pParser: TD2XStringCheckRef);
begin
  InvalidConstructor;
end;

constructor TD2XListParam.CreateList(pCode, pLabel, pDescr, pExtn: String;
  pListFileName: TD2XNamedStreamRef);
begin
  inherited Create(pCode, pLabel, '[!:]<list>', 'Clear(!), Load(:,' + pExtn +
      ') or Add items to ' + pDescr, ParseList);

  fList := TStringList.Create;
  fExtn := pExtn;
  fListFileName := pListFileName;
end;

destructor TD2XListParam.Destroy;
begin
  FreeAndNil(fList);

  inherited;
end;

function TD2XListParam.GetReportDetails(pStr: String): String;
var
  lS: String;
  w: Integer;
  lSW: TStringWriter;

  procedure WriteWidth(pStr: String);
  begin
    lSW.Write(pStr);
    Inc(w, Length(pStr));
  end;

begin
  if fList.Count < 1 then
    Result := ''
  else
    try
      lSW := TStringWriter.Create;
      w := 0;
      fList.Sort;
      for lS in fList do
      begin
        if w > 0 then
          if (w + Length(lS)) > 78 then
          begin
            lSW.WriteLine(',');
            w := 0;
            WriteWidth('                       ');
          end
          else
            WriteWidth(', ');
        WriteWidth(lS);
      end;
      Result := lSW.ToString;
    finally
      lSW.Free;
    end
end;

function TD2XListParam.IsDefault: Boolean;
begin
  Result := fList.Count = 0;
end;

procedure TD2XListParam.LoadItems(pFilename: String; pFunc: TItemProc);
var
  lF: ID2XFile;
  lSL: TStringList;
  lS: String;
begin
  lF := fListFileName(pFilename);
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

procedure TD2XListParam.Output(pSL: TStringList);
begin
  pSL.Add('-' + fCode + ':');
  if fList.Count > 0 then
  begin
    fList.Sort;
    pSL.Add('-' + fCode + '+' + fList.CommaText);
  end;
end;

function TD2XListParam.ParseList(pStr: String): Boolean;
begin
  Result := False;
  if (pStr = '!') or (pStr = ':') then
  begin
    Result := True;
    fList.Clear;
  end
  else
    if Length(pStr) > 1 then
    begin
      Result := True;
      if pStr[1] = ':' then
      begin
        fList.Clear;
        LoadItems(MakeFileName(System.Copy(pStr, 2, Length(pStr)), fExtn), AddItem);
      end
      else
        ProcessItems(pStr, AddItem);
    end;
end;

procedure TD2XListParam.Reset;
begin
  fList.Clear;
end;

procedure TD2XListParam.Zero;
begin
  fList.Clear;
end;

end.
