unit D2XParam;

interface

uses
  System.SysUtils;

type
  EInvalidParam = class(Exception);

  TD2XSingleParam<T> = class
  public type
    TParamGetter = function: T of object;
    TParamSetter = function(pVal: T): Boolean of object;
    TParamConverter = function(pStr: string): T of object;
    TParamFormatter = function(pVal: T): string of object;

  public
    constructor Create;
    constructor CreateParam(pCode, pLabel, pSample, pDescr: string; pDefault: T;
      pGetter: TParamGetter; pSetter: TParamSetter; pConverter: TParamConverter;
      pFormatter: TParamFormatter);

    procedure Reset; virtual;
    function Describe: string; virtual;
    function Report: string; virtual;
    function IsCode(pStr: string): Boolean;
    function Parse(pStr: string): Boolean;

    function Value: T;
    function ToString: string; override;

  private
    fCode, fLabel, fSample, fDescr: string;
    fDefault: T;
    fGetter: TParamGetter;
    fSetter: TParamSetter;
    fConverter: TParamConverter;
    fFormatter: TParamFormatter;

    function ConvertAndSet(pStr: string): Boolean;
  end;

  TD2XBooleanParam = class(TD2XSingleParam<Boolean>)
  public
    constructor CreateBool(pCode, pLabel, pSample, pDescr: string;
      pGetter: TD2XSingleParam<Boolean>.TParamGetter;
      pSetter: TD2XSingleParam<Boolean>.TParamSetter);

  private
    function ConvertBoolean(pVal: string): Boolean;
    function FormatBoolean(pVal: Boolean): string;
  end;

  TD2XSimpleStringParam = class(TD2XSingleParam<string>)
  public
    constructor CreateStr(pCode, pLabel, pSample, pDescr, pDefault: string;
      pGetter: TD2XSingleParam<string>.TParamGetter;
      pSetter: TD2XSingleParam<string>.TParamSetter);

  private
    function SimpleString(pVal: string): string;
  end;

implementation

uses
  System.Classes,
  System.StrUtils;

{ TD2XSingleParam<T> }

function TD2XSingleParam<T>.ConvertAndSet(pStr: string): Boolean;
begin
  Result := fSetter(fConverter(pStr));
end;

constructor TD2XSingleParam<T>.Create;
begin
  raise EInvalidParam.Create('Need to use Specific constructor');
end;

constructor TD2XSingleParam<T>.CreateParam(pCode, pLabel, pSample, pDescr: string; pDefault: T;
  pGetter: TParamGetter; pSetter: TParamSetter; pConverter: TParamConverter;
  pFormatter: TParamFormatter);
begin
  if not Assigned(pGetter) then
    raise EInvalidParam.Create('Need a Getter');
  if not Assigned(pSetter) then
    raise EInvalidParam.Create('Need a Setter');
  if not Assigned(pConverter) then
    raise EInvalidParam.Create('Need a Converter');
  if not Assigned(pFormatter) then
    raise EInvalidParam.Create('Need a Formatter');

  if pCode = '' then
    raise EInvalidParam.Create('Need a Code');
  if pLabel = '' then
    raise EInvalidParam.Create('Need a Label');

  fCode := pCode;
  fLabel := pLabel;
  fSample := pSample;
  fDescr := pDescr;
  fDefault := pDefault;
  fGetter := pGetter;
  fSetter := pSetter;
  fConverter := pConverter;
  fFormatter := pFormatter;

  Reset;
end;

function TD2XSingleParam<T>.Describe: string;
begin
  Result := Format('  -%s %-15s %-15s %-15s %s', [fCode, fLabel, fSample,
      fFormatter(fDefault), fDescr]);
end;

function TD2XSingleParam<T>.IsCode(pStr: string): Boolean;
begin
  Result := StartsText(fCode, pStr);
end;

function TD2XSingleParam<T>.Parse(pStr: string): Boolean;
begin
  Result := IsCode(pStr) and ConvertAndSet(Copy(pStr, Length(fCode) + 1, Length(pStr)));
end;

function TD2XSingleParam<T>.Report: string;
begin
  Result := Format(' %-15s %s', [fLabel, fFormatter(fGetter)]);
end;

procedure TD2XSingleParam<T>.Reset;
begin
  if not fSetter(fDefault) then
    raise EInvalidParam.Create('Invalid default value');
end;

function TD2XSingleParam<T>.ToString: string;
begin
  Result := fFormatter(fGetter);
end;

function TD2XSingleParam<T>.Value: T;
begin
  Result := fGetter;
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

constructor TD2XBooleanParam.CreateBool(pCode, pLabel, pSample, pDescr: string;
  pGetter: TD2XSingleParam<Boolean>.TParamGetter;
  pSetter: TD2XSingleParam<Boolean>.TParamSetter);
begin
  CreateParam(pCode, pLabel, pSample, pDescr, False, pGetter, pSetter, ConvertBoolean,
    FormatBoolean);
end;

function TD2XBooleanParam.FormatBoolean(pVal: Boolean): string;
begin
  Result := IfThen(pVal, '+', '-');
end;

{ TD2XSimpleStringParam }

constructor TD2XSimpleStringParam.CreateStr(pCode, pLabel, pSample, pDescr, pDefault: string;
  pGetter: TD2XSingleParam<string>.TParamGetter;
  pSetter: TD2XSingleParam<string>.TParamSetter);
begin
  CreateParam(pCode, pLabel, pSample, pDescr, pDefault, pGetter, pSetter, SimpleString,
    SimpleString);
end;

function TD2XSimpleStringParam.SimpleString(pVal: string): string;
begin
  Result := pVal;
end;

end.
