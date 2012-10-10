unit D2XParam;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils;

type
  EInvalidParam = class(Exception);

  TD2XParam = class
  public type
    TpParser = function(pVal: string): Boolean of object;

  public
    constructor Create(pCode, pLabel, pSample, pDescr: string; pParser: TpParser); virtual;

    function IsCode(pStr: string): Boolean;
    function Parse(pStr: string): Boolean;
    function Describe: string; virtual;
    function Report: string; virtual;

  protected
    fCode, fLabel, fSample, fDescr: string;
    fParser: TpParser;

    function GetSample: string; virtual;
    function GetFormatted(pDefault: Boolean): string; virtual;

  public
    property ParamLabel: string read fLabel;

  end;

  TD2XParams = class(TObjectList<TD2XParam>)
  private
    fLog: TTextWriter;

  public
    constructor Create;

    function ForCode(pCode: string): TD2XParam;
    procedure DescribeAll;
    procedure ReportAll;
    procedure ResetAll;

    property Log: TTextWriter read fLog write fLog;
  end;

  TD2XResettableParam = class(TD2XParam)
  public
    procedure Reset; virtual;
  end;

  TD2XSingleParam<T> = class(TD2XResettableParam)
  public type
    TspConverter = function(pStr: string; pDflt: T; out pVal: T): Boolean of object;
    TspFormatter = function(pVal: T): string of object;
    TspValidator = function(pVal: T): Boolean of object;

  public
    constructor Create(pCode, pLabel, pSample, pDescr: string; pParser: TD2XParam.TpParser); override;
    constructor CreateParam(pCode, pLabel, pSample, pDescr: string; pDefault: T;
      pConverter: TspConverter; pFormatter: TspFormatter; pValidator: TspValidator);

    procedure Reset; override;
    function Report: string; override;

    function ToString: string; override;

  protected
    function GetFormatted(pDefault: Boolean): string; override;

  private
    fDefault: T;
    fConverter: TspConverter;
    fFormatter: TspFormatter;
    fValidator: TspValidator;
    fValue: T;

    function CheckValue(pVal: T): Boolean;
    function ConvertAndSet(pStr: string): Boolean;
    procedure SetValue(const pVal: T);

  public
    property Value: T read fValue write SetValue;
  end;

  TD2XBooleanParam = class(TD2XSingleParam<Boolean>)
  public
    constructor CreateBool(pCode, pLabel, pDescr: string; pDefault: Boolean = False);

  private
    function ConvertBoolean(pStr: string; pDflt: Boolean; out pVal: Boolean): Boolean;
    function FormatBoolean(pVal: Boolean): string;
  end;

  TD2XStringParam = class(TD2XSingleParam<string>)
  public
    constructor CreateStr(pCode, pLabel, pSample, pDescr, pDefault: string;
      pConverter: TD2XSingleParam<string>.TspConverter;
      pValidator: TD2XSingleParam<string>.TspValidator);

  private
    function ConvertString(pStr: string; pDflt: string; out pVal: string): Boolean;
    function FormatString(pVal: string): string;
  end;

  TD2XFlaggedStringParam = class(TD2XSingleParam<string>)
  public type
    TfspFormatter = function(pFlag: Boolean; pVal: string): string of object;

  public
    constructor CreateFlagStr(pCode, pLabel, pSample, pDescr, pStrDefault: string;
      pFlagDefault: Boolean; pStrConverter: TD2XSingleParam<string>.TspConverter;
      pStrValidator: TD2XSingleParam<string>.TspValidator; pFormatter: TfspFormatter);

    procedure Reset; override;

  protected
    function GetSample: string; override;
    function GetFormatted(pDefault: Boolean): string; override;

  private
    fFlagDefault: Boolean;
    fFlag: Boolean;
    fStrConverter: TD2XSingleParam<string>.TspConverter;
    fFormatter: TfspFormatter;

    function ConvertString(pStr: string; pDflt: string; out pVal: string): Boolean;
    function FormatFlagString(pFlag: Boolean; pVal: string): string;
    function FormatString(pVal: string): string;

  public
    property Flag: Boolean read fFlag write fFlag;
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
  pParser: TD2XParam.TpParser);
begin
  raise EInvalidParam.Create('Need to use specific constructor');
end;

constructor TD2XSingleParam<T>.CreateParam(pCode, pLabel, pSample, pDescr: string; pDefault: T;
  pConverter: TspConverter; pFormatter: TspFormatter; pValidator: TspValidator);
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

function TD2XSingleParam<T>.Report: string;
begin
  Result := Format(' %-15s %s', [fLabel, GetFormatted(False)]);
end;

procedure TD2XSingleParam<T>.Reset;
begin
  if CheckValue(fDefault) then
    fValue := fDefault
  else
    inherited;
end;

procedure TD2XSingleParam<T>.SetValue(const pVal: T);
begin
  if CheckValue(pVal) then
    fValue := pVal
  else
    raise EInvalidParam.Create('Invalid value');
end;

function TD2XSingleParam<T>.ToString: string;
begin
  Result := GetFormatted(False);
end;

{ TD2XBooleanParam }

function TD2XBooleanParam.ConvertBoolean(pStr: string; pDflt: Boolean; out pVal: Boolean): Boolean;
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
  CreateParam(pCode, pLabel, '[+|-]', pDescr, pDefault, ConvertBoolean, FormatBoolean, nil);
end;

function TD2XBooleanParam.FormatBoolean(pVal: Boolean): string;
begin
  Result := IfThen(pVal, '+', '-');
end;

{ TD2XStringParam }

function TD2XStringParam.ConvertString(pStr: string; pDflt: string; out pVal: string): Boolean;
begin
  Result := True;
  pVal := pStr;
end;

constructor TD2XStringParam.CreateStr(pCode, pLabel, pSample, pDescr, pDefault: string;
  pConverter: TD2XSingleParam<string>.TspConverter;
  pValidator: TD2XSingleParam<string>.TspValidator);
begin
  if Assigned(pConverter) then
    CreateParam(pCode, pLabel, pSample, pDescr, pDefault, pConverter, FormatString, pValidator)
  else
    CreateParam(pCode, pLabel, pSample, pDescr, pDefault, ConvertString, FormatString,
      pValidator);
end;

function TD2XStringParam.FormatString(pVal: string): string;
begin
  Result := pVal;
end;

{ TD2XParam }

constructor TD2XParam.Create(pCode, pLabel, pSample, pDescr: string; pParser: TpParser);
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

function TD2XParam.Describe: string;
begin
  Result := Format('  %1s%-12s %-15s %s', [fCode, GetSample,
      GetFormatted(True), fDescr]);
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

function TD2XParam.Parse(pStr: string): Boolean;
begin
  Result := IsCode(pStr) and fParser(Copy(pStr, Length(fCode) + 1, Length(pStr)));
end;

function TD2XParam.Report: string;
begin
  Result := '';
end;

  { TD2XParams }

constructor TD2XParams.Create;
begin
  inherited Create(True);
end;

procedure TD2XParams.DescribeAll;
var
  lP: TD2XParam;
begin
  if not Assigned(fLog) then
    Exit;

  for lP in Self do
    fLog.WriteLine(lP.Describe);
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

procedure TD2XParams.ReportAll;
var
  lP: TD2XParam;
begin
  if not Assigned(fLog) then
    Exit;

  for lP in Self do
    if lP.Report > '' then
      fLog.WriteLine(lP.Report);
end;

procedure TD2XParams.ResetAll;
var
  lP: TD2XParam;
begin
  for lP in Self do
    if lP is TD2XResettableParam then
      TD2XResettableParam(lP).Reset;
end;

{ TD2XFlaggedStringParam }

function TD2XFlaggedStringParam.ConvertString(pStr: string; pDflt: string; out pVal: string): Boolean;
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
    fFormatter := pFormatter
  else
    fFormatter := FormatFlagString;

  CreateParam(pCode, pLabel, pSample, pDescr, pStrDefault, ConvertString, FormatString,
    pStrValidator);
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

function TD2XFlaggedStringParam.GetFormatted(pDefault: Boolean): string;
begin
  if pDefault then
    Result := fFormatter(fFlagDefault, fDefault)
  else
    Result := fFormatter(fFlag, fValue);
end;

function TD2XFlaggedStringParam.GetSample: string;
begin
  Result := '[+-]:' + fSample;
end;

procedure TD2XFlaggedStringParam.Reset;
begin
  inherited;

  fFlag := fFlagDefault;
end;

{ TD2XResettableParam }

procedure TD2XResettableParam.Reset;
begin
  raise EInvalidParam.Create('Invalid default value');
end;

end.
