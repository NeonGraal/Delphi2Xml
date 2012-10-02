unit D2XParam;

interface

type
  TD2XSingleParam<T> = class
  public
    type
      TParamGetter = function: string of object;
      TParamSetter = function(pVal: T): Boolean of object;
      TParamConverter = function(pStr: string): T of object;

    constructor Create(pCode, pLabel, pDescr: string; pDefault: string; pGetter: TParamGetter;
      pSetter: TParamSetter; pConverter: TParamConverter);

    procedure Reset;
    function Describe: String;
    function Report: String;
    function Parse(pStr: String): Boolean;

  private
    fCode, fLabel, fDescr: string;
    fDefault: string;
    fGetter: TParamGetter;
    fSetter: TParamSetter;
    fConverter: TParamConverter
  end;

implementation

uses
  System.StrUtils,
  System.SysUtils;

{ TD2XSingleParam<T> }

constructor TD2XSingleParam<T>.Create(pCode, pLabel, pDescr: string; pDefault: string;
  pGetter: TParamGetter; pSetter: TParamSetter; pConverter: TParamConverter);
begin
  fCode := pCode;
  fLabel := pLabel;
  fDescr := pDescr;
  fDefault := pDefault;
  fGetter := pGetter;
  fSetter := pSetter;
  fConverter := pConverter;

  Reset;
end;

function TD2XSingleParam<T>.Describe: String;
begin
  Result := Format('  -%s %-15s %-15s %s', [fCode, fLabel, fDefault, fDescr]);
end;

function TD2XSingleParam<T>.Parse(pStr: String): Boolean;
begin
  Result := StartsText(fCode, pStr)
    and fSetter(fConverter(Copy(pStr, Length(fCode)+1, Length(pStr))));
end;

function TD2XSingleParam<T>.Report: String;
begin
  Result := Format(' %-15s %s', [fLabel, fGetter]);
end;

procedure TD2XSingleParam<T>.Reset;
begin
  fSetter(fConverter(fDefault));
end;

end.
