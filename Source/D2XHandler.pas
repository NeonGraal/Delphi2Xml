unit D2XHandler;

interface

uses
  System.Classes,
  System.Generics.Collections;

type
  TD2XHandler = class
  public type
    ThStreamCreator = reference to function: TStream;
  public
    procedure Copy(pFrom: TD2XHandler); virtual;

    procedure BeginProcessing; virtual;
    procedure EndProcessing(pOutput: ThStreamCreator); virtual;

    procedure BeginFile; virtual;
    procedure EndFile(pOutput: ThStreamCreator); virtual;

    procedure BeginResults; virtual;
    procedure EndResults(pFile: string); virtual;

    function CheckMethod(pMethod: string): Boolean; virtual;
    procedure BeginMethod(pMethod: string); virtual;
    procedure EndMethod(pMethod: string); virtual;
  end;

implementation

uses
  System.SysUtils;

{ TD2XHandler }

procedure TD2XHandler.BeginMethod(pMethod: string);
begin

end;

procedure TD2XHandler.BeginFile;
begin

end;

procedure TD2XHandler.BeginProcessing;
begin

end;

procedure TD2XHandler.BeginResults;
begin

end;

function TD2XHandler.CheckMethod(pMethod: string): Boolean;
begin
  Result := True;
end;

procedure TD2XHandler.Copy(pFrom: TD2XHandler);
begin

end;

procedure TD2XHandler.EndMethod(pMethod: string);
begin

end;

procedure TD2XHandler.EndFile(pOutput: ThStreamCreator);
begin

end;

procedure TD2XHandler.EndProcessing(pOutput: ThStreamCreator);
begin

end;

procedure TD2XHandler.EndResults(pFile: string);
begin

end;

end.
