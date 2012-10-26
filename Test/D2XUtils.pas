unit D2XUtils;

interface

uses
  System.Classes,
  System.Generics.Collections;

type
  TStrIntPair = TPair<string, Integer>;
  TStrIntDict = TDictionary<string, Integer>;
  TPairLogMethod = reference to function(pPair: TStrIntPair): string;

function ReduceString(pStr: string): string;
procedure OutputStrIntDict(pDict: TStrIntDict; pStream: TStream; pFunc: TPairLogMethod);

implementation

uses
  System.RegularExpressions,
  System.SysUtils;

function ReduceString(pStr: string): string;
begin
  Result := Trim(TRegEx.Replace(pStr, '\s+', ' ', []));
end;

procedure OutputStrIntDict(pDict: TStrIntDict; pStream: TStream; pFunc: TPairLogMethod);
var
  lP: TStrIntPair;
begin
  Assert(Assigned(pStream), 'Need a Stream');
  Assert(Assigned(pFunc), 'Need a Function');
  Assert(Assigned(pDict), 'Need a Dictionary');
  if pDict.Count > 0 then
    with TStringList.Create do
      try
        for lP in pDict do
          if lP.Value > 0 then
            Values[lP.Key] := pFunc(lP);
        Sort;
        SaveToStream(pStream);
      finally
        Free;
      end;
end;

end.
