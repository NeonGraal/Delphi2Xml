unit Test.Handler.Tests;

interface

implementation

uses
  D2X.Handler,
  System.SysUtils,
  Test.Handler,
  TestFramework;

type
  TestTD2XHandler = class(TTestCase)
  protected
    FD2XHandler: TD2XHandler;
    FD2XTester: TD2XHandlerTester;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDescription;
    procedure TestUseProxy;
    procedure TestBeginProcessing;
    procedure TestEndProcessing;
    procedure TestBeginFile;
    procedure TestEndFile;
    procedure TestBeginResults;
    procedure TestEndResults;
    procedure TestCheckBeforeMethod;
    procedure TestCheckAfterMethod;
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
  FreeAndNil(FD2XHandler);
end;

procedure TestTD2XHandler.TestBeginProcessing;
begin
  FD2XHandler.BeginProcessing(nil);

  CheckTrue(FD2XTester.CalledBeginProcessing, 'Called Begin Processing');
end;

procedure TestTD2XHandler.TestEndProcessing;
begin
  FD2XHandler.EndProcessing(nil);

  CheckTrue(FD2XTester.CalledEndProcessing, 'Called End Processing');
end;

procedure TestTD2XHandler.TestBeginResults;
begin
  FD2XHandler.BeginResults;

  CheckTrue(FD2XTester.CalledBeginResults, 'Called Begin Results');
end;

procedure TestTD2XHandler.TestCheckAfterMethod;
begin
  FD2XHandler.CheckAfterMethod('');

  CheckTrue(FD2XTester.CalledCheckAfterMethod, 'Called Check After Method');
end;

procedure TestTD2XHandler.TestCheckBeforeMethod;
begin
  FD2XHandler.CheckBeforeMethod('');

  CheckTrue(FD2XTester.CalledCheckBeforeMethod, 'Called Check Before Method');
end;

procedure TestTD2XHandler.TestCopy;
begin
  FD2XHandler.Copy(nil);

  CheckTrue(FD2XTester.CalledCopy, 'Called Copy');
end;

procedure TestTD2XHandler.TestDescription;
begin
  CheckEqualsString('Handler Tester', FD2XHandler.Description, 'Description');
end;

procedure TestTD2XHandler.TestBeginMethod;
begin
  FD2XHandler.BeginMethod('');

  CheckTrue(FD2XTester.CalledBeginMethod, 'Called Begin Method');
end;

procedure TestTD2XHandler.TestEndMethod;
begin
  FD2XHandler.EndMethod('');

  CheckTrue(FD2XTester.CalledEndMethod, 'Called End Method');
end;

procedure TestTD2XHandler.TestEndResults;
begin
  FD2XHandler.EndResults(nil);

  CheckTrue(FD2XTester.CalledEndResults, 'Called End Results');
end;

procedure TestTD2XHandler.TestUseProxy;
begin
  CheckFalse(FD2XHandler.UseProxy, 'Use Proxy');
end;

procedure TestTD2XHandler.TestBeginFile;
begin
  FD2XHandler.BeginFile('', nil);

  CheckTrue(FD2XTester.CalledBeginFile, 'Called Begin File');
end;

procedure TestTD2XHandler.TestEndFile;
begin
  FD2XHandler.EndFile('', nil);

  CheckTrue(FD2XTester.CalledEndFile, 'Called End File');
end;

initialization

// Register any test cases with the test runner
RegisterTests('Handlers', [TestTD2XHandler.Suite]);

end.
