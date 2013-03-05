unit Test.Options;

interface

uses
  D2X.Flag,
  D2X.Global,
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
    property ParserDefines: TD2XStringListRef read fGetParserDefines;
    property HeldDefines: TD2XStringListRef read fGetHeldDefines;
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

end.
