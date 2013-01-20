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
    procedure Report(pL: ID2XLogger; pStr: string = '');
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
    function GetReportDetails(pStr: string = ''): string; virtual;

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

    procedure Output(pSL: TStringList); override;
    function IsDefault: Boolean; override;
    procedure Reset; override;
    procedure Zero; override;

  protected
    fFormatter: TspFormatter;

    function GetFormatted(pDefault: Boolean): string; override;
    function GetReportDetails(pStr: string = ''): string; override;

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

  TD2XBoolFlag = class(TD2XInterfaced, ID2XFlag)
  private
    fFlag: Boolean;
    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);
  end;

  TD2XFlagDefine = record
    FlagCode: Char;
    FlagLabel, FlagDescr: string;
    FlagDefault: Boolean;
  end;

  TD2XFlagDefines = array of TD2XFlagDefine;

function FlagDef(pCode: Char; pLabel, pDescr: string; pDefault: Boolean = False)
  : TD2XFlagDefine;

implementation

uses
  System.Generics.Defaults,
  System.StrUtils;

function FlagDef(pCode: Char; pLabel, pDescr: string; pDefault: Boolean = False)
  : TD2XFlagDefine;
begin
  Result.FlagCode := UpCase(pCode);
  Result.FlagLabel := pLabel;
  Result.FlagDescr := pDescr;
  Result.FlagDefault := pDefault;
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
  pL.Log('  %-16s %-15s %s', [fCode + GetSample, GetFormatted(True), fDescr]);
end;

function TD2XParam.GetFormatted(pDefault: Boolean): string;
begin
  Result := '';
end;

function TD2XParam.GetReportDetails(pStr: string): string;
begin
  Result := '~~~~';
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

procedure TD2XParam.Report(pL: ID2XLogger; pStr: string);
var
  lS: string;
begin
  lS := GetReportDetails(pStr);
  if lS <> '~~~~' then
    pL.Log('  %-20s %s', [fLabel, lS]);
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
  pL.Log('Options:           %-15s Description', ['Default']);
  for lP in Self do
    lP.Describe(pL);
  pL.Log('  Definitions:', []);
  pL.Log('    <codes> Flag codes, optionally interspersed with "+" or "-"', []);
  pL.Log('    <labels> Comma list of Flag Labels, each optionally prefixed or suffixed with "+" or "-"',
    []);
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

{ TD2XBoolFlag }

function TD2XBoolFlag.GetFlag: Boolean;
begin
  Result := fFlag;
end;

procedure TD2XBoolFlag.SetFlag(pVal: Boolean);
begin
  fFlag := pVal;
end;

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

function TD2XSingleParam<T>.GetReportDetails(pStr: string): string;
begin
  Result := GetFormatted(False);
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

end.
