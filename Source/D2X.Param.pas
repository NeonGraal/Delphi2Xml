unit D2X.Param;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  D2X;

type
  EInvalidParam = class(Exception);

  ID2XFlag = interface
    ['{1986FC52-3D32-4988-BE5E-9C936D0890C5}']
    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);

    property Flag: Boolean read GetFlag write SetFlag;
  end;

  TD2XParam = class(TD2XInterfaced)
  public
    constructor Create(pCode, pLabel, pSample, pDescr: string;
      pParser: TD2XStringCheckRef); virtual;

    function IsCode(pStr: string): Boolean;
    function Parse(pStr: string): Boolean;
    procedure Describe(pL: ID2XLogger); virtual;
    procedure Report(pL: ID2XLogger); virtual;
    procedure Output(pSL: TStringList); virtual;
    function ToString: string; override;
    function IsDefault: Boolean; virtual;
    procedure Zero; virtual;
    procedure Reset; virtual;

  protected
    fCode, fLabel, fSample, fDescr: string;
    fParser: TD2XStringCheckRef;

    function GetSample: string; virtual;
    function GetFormatted(pDefault: Boolean): string; virtual;

  public
    property ParamLabel: string read fLabel;

  end;

  TD2XParams = class(TObjectList<TD2XParam>)
  public
    function ForCode(pCode: string): TD2XParam;
    procedure DescribeAll(pL: ID2XLogger);
    procedure ReportAll(pL: ID2XLogger);
    procedure OutputAll(pSL: TStringList);
    procedure ResetAll;
    procedure ZeroAll;
  end;

  TD2XResettableParam = class(TD2XParam)
  public type
    TspSetter = reference to procedure;
  public
    constructor Create(pCode, pLabel, pSample, pDescr: string;
      pParser: TD2XStringCheckRef); override;
    constructor CreateReset(pCode, pLabel, pSample, pDescr: string;
      pParser: TD2XStringCheckRef; pResetter, pZeroer: TspSetter);

    procedure Reset; override;
    procedure Zero; override;
  private
    fResetter, fZeroer: TspSetter;
  end;

  TD2XSingleParam<T> = class(TD2XParam)
  public type
    TspConverter = reference to function(pStr: string; pDflt: T; out pVal: T): Boolean;
    TspFormatter = reference to function(pVal: T): string;
    TspValidator = reference to function(pVal: T): Boolean;

  public
    constructor Create(pCode, pLabel, pSample, pDescr: string;
      pParser: TD2XStringCheckRef); override;
    constructor CreateParam(pCode, pLabel, pSample, pDescr: string; pDefault: T;
      pConverter: TspConverter; pFormatter: TspFormatter; pValidator: TspValidator); virtual;

    procedure Report(pL: ID2XLogger); override;
    procedure Output(pSL: TStringList); override;
    function IsDefault: Boolean; override;
    procedure Reset; override;
    procedure Zero; override;

  protected
    fFormatter: TspFormatter;

    function GetFormatted(pDefault: Boolean): string; override;

  private
    fDefault: T;
    fConverter: TspConverter;
    fValidator: TspValidator;
    fValue: T;

    function CheckValue(pVal: T): Boolean;
    function ConvertAndSet(pStr: string): Boolean;
    procedure SetValue(const pVal: T);

  public
    property Value: T read fValue write SetValue;
  end;

  TD2XBooleanParam = class(TD2XSingleParam<Boolean>, ID2XFlag)
  public
    constructor CreateParam(pCode, pLabel, pSample, pDescr: string; pDefault: Boolean;
      pConverter: TD2XSingleParam<Boolean>.TspConverter;
      pFormatter: TD2XSingleParam<Boolean>.TspFormatter;
      pValidator: TD2XSingleParam<Boolean>.TspValidator); override;
    constructor CreateBool(pCode, pLabel, pDescr: string; pDefault: Boolean = False);

  private
    function ConvertBoolean(pStr: string; pDflt: Boolean; out pVal: Boolean): Boolean;
    function FormatBoolean(pVal: Boolean): string;

    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);
  end;

  TD2XStringParam = class(TD2XSingleParam<string>)
  public
    constructor CreateParam(pCode, pLabel, pSample, pDescr: string; pDefault: string;
      pConverter: TD2XSingleParam<string>.TspConverter;
      pFormatter: TD2XSingleParam<string>.TspFormatter;
      pValidator: TD2XSingleParam<string>.TspValidator); override;
    constructor CreateStr(pCode, pLabel, pSample, pDescr, pDefault: string;
      pConverter: TD2XSingleParam<string>.TspConverter;
      pValidator: TD2XSingleParam<string>.TspValidator);

  private
    function ConvertString(pStr: string; pDflt: string; out pVal: string): Boolean;
    function FormatString(pVal: string): string;
  end;

  TD2XFlaggedStringParam = class(TD2XSingleParam<string>, ID2XFlag)
  public type
    TfspFormatter = reference to function(pFlag: Boolean; pVal: string): string;

  public
    constructor CreateParam(pCode, pLabel, pSample, pDescr: string; pDefault: string;
      pConverter: TD2XSingleParam<string>.TspConverter;
      pFormatter: TD2XSingleParam<string>.TspFormatter;
      pValidator: TD2XSingleParam<string>.TspValidator); override;
    constructor CreateFlagStr(pCode, pLabel, pSample, pDescr, pStrDefault: string;
      pFlagDefault: Boolean; pStrConverter: TD2XSingleParam<string>.TspConverter;
      pStrValidator: TD2XSingleParam<string>.TspValidator; pFormatter: TfspFormatter);

    procedure Reset; override;
    function IsDefault: Boolean; override;
    procedure Zero; override;

  protected
    function GetSample: string; override;
    function GetFormatted(pDefault: Boolean): string; override;

  private
    fFlagDefault: Boolean;
    fFlag: Boolean;
    fStrConverter: TD2XSingleParam<string>.TspConverter;
    fFlagFormatter: TfspFormatter;

    function ConvertString(pStr: string; pDflt: string; out pVal: string): Boolean;
    function FormatFlagString(pFlag: Boolean; pVal: string): string;
    function FormatString(pVal: string): string;

    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);

  public
    property FlagValue: Boolean read fFlag write fFlag;
  end;

  TD2XDefinesParam = class(TD2XSingleParam<Boolean>, ID2XFlag)
  public
    constructor CreateParam(pCode, pLabel, pSample, pDescr: string; pDefault: Boolean;
      pConverter: TD2XSingleParam<Boolean>.TspConverter;
      pFormatter: TD2XSingleParam<Boolean>.TspFormatter;
      pValidator: TD2XSingleParam<Boolean>.TspValidator); override;
    constructor CreateDefines(pCode, pLabel: string; pDefinesFileName: TD2XNamedStringRef);

    procedure Report(pL: ID2XLogger); override;
    procedure Output(pSL: TStringList); override;
    procedure Reset; override;
    procedure Zero; override;

  private
    fDefines: TStringList;
    fDefinesFileName: TD2XNamedStringRef;

    function ConvertDefines(pStr: string; pDflt: Boolean; out pVal: Boolean): Boolean;
    function FormatDefines(pVal: Boolean): string;

    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);

  public
    property Defines: TStringList read fDefines;
  end;

  TD2XBoolFlag = class(TInterfacedObject, ID2XFlag)
  private
    fFlag: Boolean;
    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);
  end;

implementation

uses
  System.Generics.Defaults,
  System.StrUtils;

{ TD2XSingleParam<T> }

function TD2XSingleParam<T>.CheckValue(pVal: T): Boolean;
begin
  if Assigned(fValidator) then
    Result := fValidator(pVal)
  else
    Result := True;
end;

function TD2XSingleParam<T>.ConvertAndSet(pStr: string): Boolean;
var
  lVal: T;
begin
  Result := fConverter(pStr, fDefault, lVal) and CheckValue(lVal);
  if Result then
    fValue := lVal;
end;

constructor TD2XSingleParam<T>.Create(pCode, pLabel, pSample, pDescr: string;
  pParser: TD2XStringCheckRef);
begin
  raise EInvalidParam.Create('Need to use correct constructor');
end;

constructor TD2XSingleParam<T>.CreateParam(pCode, pLabel, pSample, pDescr: string; pDefault: T;
  pConverter: TspConverter; pFormatter: TspFormatter;
  pValidator: TspValidator);
begin
  inherited Create(pCode, pLabel, pSample, pDescr, ConvertAndSet);

  if not Assigned(pConverter) then
    raise EInvalidParam.Create('Need a Converter');
  if not Assigned(pFormatter) then
    raise EInvalidParam.Create('Need a Formatter');

  fDefault := pDefault;
  fConverter := pConverter;
  fFormatter := pFormatter;
  fValidator := pValidator;

  Reset;
end;

function TD2XSingleParam<T>.GetFormatted(pDefault: Boolean): string;
begin
  if pDefault then
    Result := fFormatter(fDefault)
  else
    Result := fFormatter(fValue);
end;

function TD2XSingleParam<T>.IsDefault: Boolean;
var
  lComparer: IEqualityComparer<T>;
begin
  lComparer := TEqualityComparer<T>.Default;
  Result := lComparer.Equals(fValue, fDefault);
end;

procedure TD2XSingleParam<T>.Output(pSL: TStringList);
begin
  if Assigned(pSL) then
    if not IsDefault then
      pSL.Add('-' + ToString);
end;

procedure TD2XSingleParam<T>.Report(pL: ID2XLogger);
begin
  pL.Log(' %-15s %s', [fLabel, GetFormatted(False)]);
end;

procedure TD2XSingleParam<T>.Reset;
begin
  if CheckValue(fDefault) then
    fValue := fDefault
  else
    raise EInvalidParam.Create('Invalid default value');
end;

procedure TD2XSingleParam<T>.SetValue(const pVal: T);
begin
  if CheckValue(pVal) then
    fValue := pVal
  else
    raise EInvalidParam.Create('Invalid value');
end;

procedure TD2XSingleParam<T>.Zero;
begin
  fValue := TD2X.Zero<T>;
end;

{ TD2XBooleanParam }

function TD2XBooleanParam.ConvertBoolean(pStr: string; pDflt: Boolean;
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

constructor TD2XBooleanParam.CreateBool(pCode, pLabel, pDescr: string; pDefault: Boolean);
begin
  inherited CreateParam(pCode, pLabel, '[+|-]', pDescr, pDefault, ConvertBoolean,
    FormatBoolean, nil);
end;

constructor TD2XBooleanParam.CreateParam(pCode, pLabel, pSample, pDescr: string;
  pDefault: Boolean; pConverter: TD2XSingleParam<Boolean>.TspConverter;
  pFormatter: TD2XSingleParam<Boolean>.TspFormatter;
  pValidator: TD2XSingleParam<Boolean>.TspValidator);
begin
  raise EInvalidParam.Create('Need to use correct constructor');
end;

function TD2XBooleanParam.FormatBoolean(pVal: Boolean): string;
begin
  Result := IfThen(pVal, '+', '-');
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

function TD2XStringParam.ConvertString(pStr: string; pDflt: string; out pVal: string): Boolean;
begin
  Result := True;
  pVal := pStr;
end;

constructor TD2XStringParam.CreateParam(pCode, pLabel, pSample, pDescr, pDefault: string;
  pConverter: TD2XSingleParam<string>.TspConverter;
  pFormatter: TD2XSingleParam<string>.TspFormatter;
  pValidator: TD2XSingleParam<string>.TspValidator);
begin
  raise EInvalidParam.Create('Need to use correct constructor');
end;

constructor TD2XStringParam.CreateStr(pCode, pLabel, pSample, pDescr, pDefault: string;
  pConverter: TD2XSingleParam<string>.TspConverter;
  pValidator: TD2XSingleParam<string>.TspValidator);
begin
  if Assigned(pConverter) then
    inherited CreateParam(pCode, pLabel, pSample, pDescr, pDefault, pConverter, FormatString,
      pValidator)
  else
    inherited CreateParam(pCode, pLabel, pSample, pDescr, pDefault, ConvertString,
      FormatString, pValidator);
end;

function TD2XStringParam.FormatString(pVal: string): string;
begin
  Result := pVal;
end;

{ TD2XParam }

constructor TD2XParam.Create(pCode, pLabel, pSample, pDescr: string;
  pParser: TD2XStringCheckRef);
begin
  if pCode = '' then
    raise EInvalidParam.Create('Need a Code');
  if pLabel = '' then
    raise EInvalidParam.Create('Need a Label');
  if not Assigned(pParser) then
    raise EInvalidParam.Create('Need a Parser');

  fCode := pCode;
  fLabel := pLabel;
  fSample := pSample;
  fDescr := pDescr;
  fParser := pParser;
end;

procedure TD2XParam.Describe(pL: ID2XLogger);
begin
  pL.Log('  %1s%-12s %-15s %s', [fCode, GetSample, GetFormatted(True), fDescr]);
end;

function TD2XParam.GetFormatted(pDefault: Boolean): string;
begin
  Result := '';
end;

function TD2XParam.GetSample: string;
begin
  Result := fSample;
end;

function TD2XParam.IsCode(pStr: string): Boolean;
begin
  Result := StartsText(fCode, pStr);
end;

function TD2XParam.IsDefault: Boolean;
begin
  Result := True;
end;

procedure TD2XParam.Output(pSL: TStringList);
begin

end;

function TD2XParam.Parse(pStr: string): Boolean;
begin
  Result := IsCode(pStr) and fParser(Copy(pStr, Length(fCode) + 1, Length(pStr)));
end;

procedure TD2XParam.Report(pL: ID2XLogger);
begin
  //
end;

procedure TD2XParam.Reset;
begin
  //
end;

function TD2XParam.ToString: string;
begin
  Result := fCode + GetFormatted(False);
end;

procedure TD2XParam.Zero;
begin
  //
end;

{ TD2XParams }

procedure TD2XParams.DescribeAll(pL: ID2XLogger);
var
  lP: TD2XParam;
  lBase: string;
begin
  lBase := ChangeFileExt(ExtractFileName(ParamStr(0)), '');

  pL.Log('Usage: %s [ Option | @Params | mFilename | Wildcard ] ... ', [lBase]);
  pL.Log('Options:        %-15s Description', ['Default']);
  for lP in Self do
    lP.Describe(pL);
  pL.Log('  Definitions:', []);
  pL.Log('    <f/e> If value begins with "." is appended to global name to give file name', []
    );
end;

function TD2XParams.ForCode(pCode: string): TD2XParam;
var
  lP: TD2XParam;
begin
  Result := nil;
  for lP in Self do
    if lP.IsCode(pCode) then
      Result := lP;
end;

procedure TD2XParams.OutputAll(pSL: TStringList);
var
  lP: TD2XParam;
begin
  if Assigned(pSL) then
    for lP in Self do
      lP.Output(pSL);
end;

procedure TD2XParams.ReportAll(pL: ID2XLogger);
var
  lP: TD2XParam;
begin
  pL.Log('Current option settings:', []);
  for lP in Self do
    lP.Report(pL);
end;

procedure TD2XParams.ResetAll;
var
  lP: TD2XParam;
begin
  for lP in Self do
    lP.Reset;
end;

procedure TD2XParams.ZeroAll;
var
  lP: TD2XParam;
begin
  for lP in Self do
    lP.Zero;
end;

{ TD2XFlaggedStringParam }

function TD2XFlaggedStringParam.ConvertString(pStr: string; pDflt: string;
  out pVal: string): Boolean;
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
  pStrDefault: string; pFlagDefault: Boolean;
  pStrConverter: TD2XSingleParam<string>.TspConverter;
  pStrValidator: TD2XSingleParam<string>.TspValidator; pFormatter: TfspFormatter);
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
  pDefault: string;
  pConverter: TD2XSingleParam<string>.TspConverter;
  pFormatter: TD2XSingleParam<string>.TspFormatter;
  pValidator: TD2XSingleParam<string>.TspValidator);
begin
  raise EInvalidParam.Create('Need to use correct constructor');
end;

function TD2XFlaggedStringParam.FormatFlagString(pFlag: Boolean; pVal: string): string;
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

function TD2XFlaggedStringParam.FormatString(pVal: string): string;
begin
  raise EInvalidParam.Create('Incorrect call to TD2XFlaggedStringParam.FormatString');
end;

function TD2XFlaggedStringParam.GetFlag: Boolean;
begin
  Result := fFlag;
end;

function TD2XFlaggedStringParam.GetFormatted(pDefault: Boolean): string;
begin
  if pDefault then
    Result := fFlagFormatter(fFlagDefault, fDefault)
  else
    Result := fFlagFormatter(fFlag, fValue);
end;

function TD2XFlaggedStringParam.GetSample: string;
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

{ TD2XResettableParam }

constructor TD2XResettableParam.Create(pCode, pLabel, pSample, pDescr: string;
  pParser: TD2XStringCheckRef);
begin
  raise EInvalidParam.Create('Need to use correct constructor');
end;

constructor TD2XResettableParam.CreateReset(pCode, pLabel, pSample, pDescr: string;
  pParser: TD2XStringCheckRef; pResetter, pZeroer: TspSetter);
begin
  inherited Create(pCode, pLabel, pSample, pDescr, pParser);

  if not Assigned(pResetter) then
    raise EInvalidParam.Create('Need a Resetter');
  if not Assigned(pZeroer) then
    raise EInvalidParam.Create('Need a Zeroer');

  fResetter := pResetter;
  fZeroer := pZeroer;
end;

procedure TD2XResettableParam.Reset;
begin
  fResetter;
end;

procedure TD2XResettableParam.Zero;
begin
  fZeroer;
end;

function TD2XBoolFlag.GetFlag: Boolean;
begin
  Result := fFlag;
end;

procedure TD2XBoolFlag.SetFlag(pVal: Boolean);
begin
  fFlag := pVal;
end;

{ TD2XDefinesParam }

function TD2XDefinesParam.ConvertDefines(pStr: string; pDflt: Boolean;
  out pVal: Boolean): Boolean;
var
  lStr: string;
  lIdx: Integer;
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
      case pStr[1] of
        '+':
          begin
            Result := True;
            pVal := True;
            if fDefines.IndexOf(lStr) < 0 then
              fDefines.Add(lStr);
          end;
        '-':
          begin
            Result := True;
            lIdx := fDefines.IndexOf(lStr);
            if lIdx >= 0 then
            begin
              fDefines.Delete(lIdx);
              pVal := True;
            end;
          end;
        ':':
          begin
            Result := True;
            pVal := True;
            fDefines.LoadFromFile(fDefinesFileName(MakeFileName(lStr, '.def')));
          end;
      end;
    end;
end;

constructor TD2XDefinesParam.CreateDefines(pCode, pLabel: string; pDefinesFileName: TD2XNamedStringRef);
begin
  fDefines := TStringList.Create;

  inherited CreateParam(pCode, pLabel, '[+-!:]<def>', 'Add(+), Remove(-), Clear(!) or Load(:) '
      + pLabel, False, ConvertDefines, FormatDefines, nil);

  fDefinesFileName := pDefinesFileName;
end;

constructor TD2XDefinesParam.CreateParam(pCode, pLabel, pSample, pDescr: string;
  pDefault: Boolean; pConverter: TD2XSingleParam<Boolean>.TspConverter;
  pFormatter: TD2XSingleParam<Boolean>.TspFormatter;
  pValidator: TD2XSingleParam<Boolean>.TspValidator);
begin
  raise EInvalidParam.Create('Need to use correct constructor');
end;

function TD2XDefinesParam.FormatDefines(pVal: Boolean): string;
begin
  Result := '';
end;

function TD2XDefinesParam.GetFlag: Boolean;
begin
  Result := fValue;
end;

procedure TD2XDefinesParam.Output(pSL: TStringList);
var
  lS: string;
begin
  if Value then
  begin
    pSL.Add('-D:');
    fDefines.Sort;
    for lS in fDefines do
      pSL.Add('-D+' + lS);
  end;
end;

procedure TD2XDefinesParam.Report(pL: ID2XLogger);
var
  lS: string;
  w: Integer;

  procedure WriteWidth(pStr: string);
  begin
    pL.Log('%s', [pStr], False);
    Inc(w, Length(pStr));
  end;

begin
  if Value then
    if fDefines.Count < 1 then
      pL.Log('Use NO Defines', [])
    else
    begin
      pL.Log('Use these Defines:', []);
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
    pL.Log('Use default Defines', []);
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

end.
