unit Test.Options;

interface

uses
  D2X.Options,
  D2X.Param,
  System.Classes;

type
  TTestBoolFlag = class(TInterfacedObject, ID2XFlag)
  private
    fFlag: Boolean;

    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);
  end;

  TTestOptions = class(TD2XOptions)
    function ParamValue(pOpt: string): string;

    property ParserDefines: TStringList read GetParserDefines;
    property HeldDefines: TStringList read GetHeldDefines;
  end;

implementation

{ TTestBoolFlag }

function TTestBoolFlag.GetFlag: Boolean;
begin
  Result := fFlag;
end;

procedure TTestBoolFlag.SetFlag(pVal: Boolean);
begin
  fFlag := pVal;
end;

{ TTestOptions }

function TTestOptions.ParamValue(pOpt: string): string;
var
  lPrm: TD2XParam;
begin
  lPrm := fParams.ForCode(Copy(pOpt, 1, 1));
  if Assigned(lPrm) then
    Result := lPrm.ToString
  else
    Result := '';
end;

end.
