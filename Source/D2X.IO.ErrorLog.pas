unit D2X.IO.ErrorLog;

interface

uses
  D2X.IO;

type
  TD2XErrorLog = class
  private
    fIOFact: ID2XIOFactory;

    fFilename: string;
    fMethod: string;

    fLastType, fLastMsg: string;
    fLastX, fLastY: Integer;

  public
    constructor Create(pIOFact: ID2XIOFactory);

    procedure SetFilename(pFilename: string);
    procedure SetMethod(pMethod: string);
    procedure ResetFile;

    procedure LogMessage(pType, pMsg: string; pX, pY: Integer);

  end;

implementation

uses
  D2X.Global;

{ TD2XErrorLog }

constructor TD2XErrorLog.Create(pIOFact: ID2XIOFactory);
begin
  fIOFact := pIOFact;
  ResetFile;
end;

procedure TD2XErrorLog.LogMessage(pType, pMsg: string; pX, pY: Integer);
var
  lErrFile: ID2XIOFile;
  lExists: Boolean;
  lDS: TD2XInterfaced;
begin
  if ((pX = 0) and (pY = 0) and ((fLastType <> pType) or (fLastMsg <> pMsg))) or (fLastX <> pX)
    or (fLastY <> pY) then
  begin
    lErrFile := fIOFact.LogFileOrExtn('.err');
    try
      lExists := lErrFile.Exists;
      with lErrFile.WriteTo(True) do
      begin
        if not lExists then
          WriteLine('Filename,Timestamp,Line,Char,Method,Type,Message');
        write(fFilename);
        write(',');
        write(fIOFact.GetNow);
        write(',');
        write(pY);
        write(',');
        write(pX);
        write(',');
        write(fMethod);
        write(',');
        write(pType);
        write(',');
        WriteLine(pMsg);
      end;
    finally
      if Assigned(lErrFile) then
      begin
        lDS := lErrFile as TD2XInterfaced;
        lErrFile := nil;
        lDS.Free;
      end;
    end;
    fLastType := pType;
    fLastMsg := pMsg;
    fLastX := pX;
    fLastY := pY;
  end;
end;

procedure TD2XErrorLog.ResetFile;
begin
  fLastType := '~';
  fLastMsg := '~';
  fLastX := -2;
  fLastY := -2;
end;

procedure TD2XErrorLog.SetFilename(pFilename: string);
begin
  fFilename := pFilename;
end;

procedure TD2XErrorLog.SetMethod(pMethod: string);
begin
  fMethod := pMethod;
end;

end.
