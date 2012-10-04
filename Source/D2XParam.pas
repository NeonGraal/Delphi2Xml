unit D2XParam;

interface

uses
  System.Generics.Collections,
  System.SysUtils;

type
  EInvalidParam = class(Exception);

  TD2XParam = class
  public type
    TpParser = function(pVal: string): Boolean of object;

  public
    constructor Create(pCode, pLabel, pDescr: string; pParser: TpParser); virtual;

    function IsCode(pStr: string): Boolean;
    function Parse(pStr: string): Boolean;
    function Describe: string; virtual;
    function Report: string; virtual;

  protected
    fCode, fLabel, fDescr: string;
    fParser: TpParser;
  end;

  TD2XParams = class(TObjectList<TD2XParam>)
  public
    constructor Create;

    function ForCode(pCode: string): TD2XParam;
    function DescribeAll: string;
    function ReportAll: string;
  end;

  TD2XSingleParam<T> = class(TD2XParam)
  public type
    TspConverter = function(pStr: string): T of object;
    TspFormatter = function(pVal: T): string of object;
    TspValidator = function(pVal: T): Boolean of object;

  public
    constructor Create(pCode, pLabel, pDescr: string; pParser: TD2XParam.TpParser); override;
    constructor CreateParam(pCode, pLabel, pSample, pDescr: string; pDefault: T;
      pConverter: TspConverter; pFormatter: TspFormatter; pValidator: TspValidator);

    procedure Reset; virtual;
    function Describe: string; override;
    function Report: string; override;

    function Value: T;
    function ToString: string; override;

  private
    fSample: string;
    fDefault: T;
    fConverter: TspConverter;
    fFormatter: TspFormatter;
    fValidator: TspValidator;
    fValue: T;

    function CheckValue(pVal: T): Boolean;
    function ConvertAndSet(pStr: string): Boolean;
  end;

  TD2XBooleanParam = class(TD2XSingleParam<Boolean>)
  public
    constructor CreateBool(pCode, pLabel, pDescr: string);

  private
    function ConvertBoolean(pVal: string): Boolean;
    function FormatBoolean(pVal: Boolean): string;
  end;

  TD2XStringParam = class(TD2XSingleParam<string>)
  public
    constructor CreateStr(pCode, pLabel, pSample, pDescr, pDefault: string;
      pValidator: TD2XSingleParam<string>.TspValidator);

  private
    function SimpleString(pVal: string): string;
  end;

implementation

uses
  System.Classes,
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
  lVal := fConverter(pStr);
  Result := CheckValue(lVal);
  if Result then
    fValue := lVal;
end;

constructor TD2XSingleParam<T>.Create(pCode, pLabel, pDescr: string;
  pParser: TD2XParam.TpParser);
begin
  raise EInvalidParam.Create('Need to use specific constructor');
end;

constructor TD2XSingleParam<T>.CreateParam(pCode, pLabel, pSample, pDescr: string; pDefault: T;
  pConverter: TspConverter; pFormatter: TspFormatter; pValidator: TspValidator);
begin
  inherited Create(pCode, pLabel, pDescr, ConvertAndSet);

  if not Assigned(pConverter) then
    raise EInvalidParam.Create('Need a Converter');
  if not Assigned(pFormatter) then
    raise EInvalidParam.Create('Need a Formatter');

  fSample := pSample;
  fDefault := pDefault;
  fConverter := pConverter;
  fFormatter := pFormatter;
  fValidator := pValidator;

  Reset;
end;

function TD2XSingleParam<T>.Describe: string;
begin
  Result := Format('  -%s %-15s %-15s %-15s %s', [fCode, fLabel, fSample,
      fFormatter(fDefault), fDescr]);
end;

function TD2XSingleParam<T>.Report: string;
begin
  Result := Format(' %-15s %s', [fLabel, fFormatter(fValue)]);
end;

procedure TD2XSingleParam<T>.Reset;
begin
  if CheckValue(fDefault) then
    fValue := fDefault
  else
    raise EInvalidParam.Create('Invalid default value');
end;

function TD2XSingleParam<T>.ToString: string;
begin
  Result := fFormatter(fValue);
end;

function TD2XSingleParam<T>.Value: T;
begin
  Result := fValue;
end;

{ TD2XBooleanParam }

function TD2XBooleanParam.ConvertBoolean(pVal: string): Boolean;
begin
  if Length(pVal) > 0 then
    case pVal[1] of
      '+', 'T', 't', 'Y', 'y', '1':
        Result := True;
      '-', 'F', 'f', 'N', 'n', '0':
        Result := False;
    else
      raise EParserError.Create('Invalid Value for Boolean');
    end
  else
    Result := True;
end;

constructor TD2XBooleanParam.CreateBool(pCode, pLabel, pDescr: string);
begin
  CreateParam(pCode, pLabel, '[+|-]', pDescr, False, ConvertBoolean, FormatBoolean, nil);
end;

function TD2XBooleanParam.FormatBoolean(pVal: Boolean): string;
begin
  Result := IfThen(pVal, '+', '-');
end;

{ TD2XStringParam }

constructor TD2XStringParam.CreateStr(pCode, pLabel, pSample, pDescr, pDefault: string;
  pValidator: TD2XSingleParam<string>.TspValidator);
begin
  CreateParam(pCode, pLabel, pSample, pDescr, pDefault, SimpleString, SimpleString,
    pValidator);
end;

function TD2XStringParam.SimpleString(pVal: string): string;
begin
  Result := pVal;
end;

{ TD2XParam }

constructor TD2XParam.Create(pCode, pLabel, pDescr: string; pParser: TpParser);
begin
  if pCode = '' then
    raise EInvalidParam.Create('Need a Code');
  if pLabel = '' then
    raise EInvalidParam.Create('Need a Label');
  if not Assigned(pParser) then
    raise EInvalidParam.Create('Need a Parser');

  fCode := pCode;
  fLabel := pLabel;
  fDescr := pDescr;
  fParser := pParser;
end;

function TD2XParam.Describe: string;
begin
  Result := Format('  -%s %-45s   %s', [fCode, fLabel, fDescr]);
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

type
  TD2XParamEqCmp = class(TComparer<TD2XParam>)
    function Compare(const pL, pR: TD2XParam): Integer; override;
  end;

{ TD2XParams }

constructor TD2XParams.Create;
begin
  inherited Create(TD2XParamEqCmp.Create, True);
end;

function TD2XParams.DescribeAll: string;
var
  lP: TD2XParam;
begin
  Result := '';
  for lP in Self do
    Result := Result + lP.Describe + #13#10;
end;

function TD2XParams.ForCode(pCode: string): TD2XParam;
var
  lP: TD2XParam;
begin
  Result := nil;
  for lP in Self do
    if lP.fCode = pCode then
      Result := lP;
end;

function TD2XParams.ReportAll: string;
var
  lP: TD2XParam;
begin
  Result := '';
  for lP in Self do
    if lP.Report > '' then
      Result := Result + lP.Report + #13#10;
end;

{ TD2XParamEqCmp }

function TD2XParamEqCmp.Compare(const pL, pR: TD2XParam): Integer;
begin
  Result := CompareStr(pL.fCode, pR.fCode);
end;

end.
