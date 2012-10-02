unit D2X;

interface

uses
  System.Rtti;

type
  TD2X = class
    class function ToLabel<T>(pVal: T): string;
  end;

implementation

class function TD2X.ToLabel<T>(pVal: T): string;
var
  lV: TValue;
begin
  lV := TValue.From<T>(pVal);
  Result := Copy(lV.ToString, 3, 99);
end;

end.
