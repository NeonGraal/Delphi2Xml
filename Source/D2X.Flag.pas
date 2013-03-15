unit D2X.Flag;

interface

uses
  D2X.Global;

type
  ID2XFlag = interface
    ['{1986FC52-3D32-4988-BE5E-9C936D0890C5}']
    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);

    property Flag: Boolean read GetFlag write SetFlag;
  end;

  TD2XFlagRef = reference to function: Boolean;

  TD2XBoolFlag = class(TD2XInterfaced, ID2XFlag)
  private
    fFlag: Boolean;
    function GetFlag: Boolean;
  public
    procedure SetFlag(pVal: Boolean);
  end;

function FlagToRef(pFlag: ID2XFlag): TD2XFlagRef;

implementation

function FlagToRef(pFlag: ID2XFlag): TD2XFlagRef;
begin
  Result := function: Boolean
    begin
      result := pFlag.Flag;
    end;
end;

{ TD2XBoolFlag }

function TD2XBoolFlag.GetFlag: Boolean;
begin
  result := fFlag;
end;

procedure TD2XBoolFlag.SetFlag(pVal: Boolean);
begin
  fFlag := pVal;
end;

end.
