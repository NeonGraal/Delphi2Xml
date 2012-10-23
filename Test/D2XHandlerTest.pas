unit D2XHandlerTest;

interface

implementation

uses
  D2XHandler,
  System.Classes,
  System.Generics.Collections,
  TestFramework;

type
  TD2XHandlerTester = class(TD2XHandler)
  public
    CalledBeginProcessing: Boolean;
    CalledEndProcessing: Boolean;
    CalledBeginFile: Boolean;
    CalledEndFile: Boolean;
    CalledBeginResults: Boolean;
    CalledEndResults: Boolean;
    CalledCheckMethod: Boolean;
    CalledDoBeginMethod: Boolean;
    CalledDoEndMethod: Boolean;
    CalledCopy: Boolean;

    procedure BeginProcessing; override;
    procedure EndProcessing(pOutput: TD2XHandler.ThStreamCreator); override;
    procedure BeginFile; override;
    procedure EndFile(pOutput: TD2XHandler.ThStreamCreator); override;
    procedure BeginResults; override;
    procedure EndResults(pFile: String); override;
    function CheckMethod(pMethod: String): Boolean; override;
    procedure BeginMethod(pMethod: String); override;
    procedure EndMethod(pMethod: String); override;

    procedure Copy(pFrom: TD2XHandler); override;

  end;

  TestTD2XHandler = class(TTestCase)
  protected
    FD2XHandler: TD2XHandler;
    FD2XTester: TD2XHandlerTester;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBeginProcessing;
    procedure TestEndProcessing;
    procedure TestBeginFile;
    procedure TestEndFile;
    procedure TestBeginResults;
    procedure TestEndResults;
    procedure TestCheckMethod;
    procedure TestBeginMethod;
    procedure TestEndMethod;
    procedure TestCopy;
  end;

{ TestTD2XHandler }

procedure TestTD2XHandler.SetUp;
begin
  FD2XTester := TD2XHandlerTester.Create;
  FD2XHandler := FD2XTester;
end;

procedure TestTD2XHandler.TearDown;
begin
  FD2XHandler.Free;
  FD2XHandler := nil;
end;

procedure TestTD2XHandler.TestBeginProcessing;
begin
  FD2XHandler.BeginProcessing;

  CheckTrue(FD2XTester.CalledBeginProcessing, 'Called Begin Processing');
end;

procedure TestTD2XHandler.TestEndProcessing;
begin
  FD2XHandler.EndProcessing(function: TStream begin Result := nil; end);

  CheckTrue(FD2XTester.CalledEndProcessing, 'Called End Processing');
end;

procedure TestTD2XHandler.TestBeginResults;
begin
  FD2XHandler.BeginResults;

  CheckTrue(FD2XTester.CalledBeginResults, 'Called Begin Results');
end;

procedure TestTD2XHandler.TestCheckMethod;
begin
  FD2XHandler.CheckMethod('');

  CheckTrue(FD2XTester.CalledCheckMethod, 'Called Check Method');
end;

procedure TestTD2XHandler.TestCopy;
begin
  FD2XHandler.Copy(nil);

  CheckTrue(FD2XTester.CalledCopy, 'Called Copy');
end;

procedure TestTD2XHandler.TestBeginMethod;
begin
  FD2XHandler.BeginMethod('');

  CheckTrue(FD2XTester.CalledDoBeginMethod, 'Called Begin Method');
end;

procedure TestTD2XHandler.TestEndMethod;
begin
  FD2XHandler.EndMethod('');

  CheckTrue(FD2XTester.CalledDoEndMethod, 'Called End Method');
end;

procedure TestTD2XHandler.TestEndResults;
begin
  FD2XHandler.EndResults('');

  CheckTrue(FD2XTester.CalledEndResults, 'Called End Results');
end;

procedure TestTD2XHandler.TestBeginFile;
begin
  FD2XHandler.BeginFile;

  CheckTrue(FD2XTester.CalledBeginFile, 'Called Begin File');
end;

procedure TestTD2XHandler.TestEndFile;
var
  pOutput: TStream;
begin
  FD2XHandler.EndFile(function: TStream begin Result := nil; end);

  CheckTrue(FD2XTester.CalledEndFile, 'Called End File');
end;

{ TD2XHandlerTester }

procedure TD2XHandlerTester.BeginMethod(pMethod: String);
begin
  inherited;

  CalledDoBeginMethod := true;
end;

procedure TD2XHandlerTester.BeginFile;
begin
  inherited;

  CalledBeginFile := true;
end;

procedure TD2XHandlerTester.BeginProcessing;
begin
  inherited;

  CalledBeginProcessing := true;
end;

procedure TD2XHandlerTester.BeginResults;
begin
  inherited;

  CalledBeginResults := true;
end;

function TD2XHandlerTester.CheckMethod(pMethod: String): Boolean;
begin
  CalledCheckMethod := True;

  Result := inherited CheckMethod(pMethod);
end;

procedure TD2XHandlerTester.Copy(pFrom: TD2XHandler);
begin
  inherited;

  CalledCopy := true;
end;

procedure TD2XHandlerTester.EndMethod(pMethod: String);
begin
  inherited;

  CalledDoEndMethod := true;
end;

procedure TD2XHandlerTester.EndFile(pOutput: TD2XHandler.ThStreamCreator);
begin
  inherited;

  CalledEndFile := true;
end;

procedure TD2XHandlerTester.EndProcessing(pOutput: TD2XHandler.ThStreamCreator);
begin
  inherited;

  CalledEndProcessing := true;
end;

procedure TD2XHandlerTester.EndResults(pFile: String);
begin
  inherited;

  CalledEndResults := true;
end;

initialization

// Register any test cases with the test runner
RegisterTests('Handlers', [TestTD2XHandler.Suite]);

end.
