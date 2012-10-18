unit D2XHandlerTest;

interface

implementation

uses
  D2XHandler,
  System.Generics.Collections,
  TestFramework;

type
  TD2XHandlerTester = class(TD2XHandler)
  public
    CalledBeginProcessing: Boolean;
    CalledEndProcessing: Boolean;
    CalledBeginResults: Boolean;
    CalledEndResults: Boolean;
    CalledCheckMethod: Boolean;
    CalledDoBeginMethod: Boolean;
    CalledDoEndMethod: Boolean;
    CalledCopy: Boolean;

    procedure BeginProcessing; override;
    procedure EndProcessing; override;
    procedure BeginResults; override;
    procedure EndResults(pFile: String); override;
    function CheckMethod(pMethod: String): Boolean; override;
    procedure DoBeginMethod(pMethod: String); override;
    procedure DoEndMethod(pMethod: String); override;

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
    procedure TestBeginResults;
    procedure TestEndResults;
    procedure TestCheckMethod;
    procedure TestDoBeginMethod;
    procedure TestDoEndMethod;
    procedure TestBeginMethod;
    procedure TestEndMethod;
    procedure TestCopy; virtual;
  end;

  TestTD2XHandlers = class(TestTD2XHandler)
  strict private
    FD2XHandlers: TD2XHandlers;
  public
    procedure SetUp; override;
  published
    procedure TestAdd;
    procedure TestCopy; override;
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
  FD2XHandler.EndProcessing;

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

procedure TestTD2XHandler.TestDoBeginMethod;
begin
  FD2XHandler.DoBeginMethod('');

  CheckTrue(FD2XTester.CalledDoBeginMethod, 'Called Begin Method');
end;

procedure TestTD2XHandler.TestDoEndMethod;
begin
  FD2XHandler.DoEndMethod('');

  CheckTrue(FD2XTester.CalledDoEndMethod, 'Called End Method');
end;

procedure TestTD2XHandler.TestEndResults;
begin
  FD2XHandler.EndResults('');

  CheckTrue(FD2XTester.CalledEndResults, 'Called End Results');
end;

procedure TestTD2XHandler.TestBeginMethod;
begin
  FD2XHandler.BeginMethod('');

  CheckTrue(FD2XTester.CalledCheckMethod, 'Called Check Method');
  CheckTrue(FD2XTester.CalledDoBeginMethod, 'Called Begin Method');
end;

procedure TestTD2XHandler.TestEndMethod;
begin
  FD2XHandler.EndMethod('');

  CheckTrue(FD2XTester.CalledCheckMethod, 'Called Check Method');
  CheckTrue(FD2XTester.CalledDoEndMethod, 'Called End Method');
end;

{ TestTD2XHandlers }

procedure TestTD2XHandlers.SetUp;
begin
  FD2XHandlers := TD2XHandlers.Create;
  FD2XTester := TD2XHandlerTester.Create;
  FD2XHandlers.Add(FD2XTester);
  FD2XHandler := FD2XHandlers;
end;

procedure TestTD2XHandlers.TestAdd;
begin
  CheckIs(FD2XTester, TD2XHandlerTester, 'Is TD2XHandler');
end;

procedure TestTD2XHandlers.TestCopy;
begin
  FD2XHandlers.Add(TD2XHandlerTester.Create);

  inherited;
end;

{ TD2XHandlerTester }

procedure TD2XHandlerTester.DoBeginMethod(pMethod: String);
begin
  inherited;

  CalledDoBeginMethod := true;
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

procedure TD2XHandlerTester.DoEndMethod(pMethod: String);
begin
  inherited;

  CalledDoEndMethod := true;
end;

procedure TD2XHandlerTester.EndProcessing;
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
RegisterTests('Handlers', [TestTD2XHandler.Suite, TestTD2XHandlers.Suite]);

end.
