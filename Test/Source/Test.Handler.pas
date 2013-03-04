unit Test.Handler;

interface

uses
  CastaliaPasLexTypes,
  D2X.Flag,
  D2X.Global,
  D2X.Handler,
  D2X.IO,
  D2X.Parser;

type
  TD2XHandlerTester = class(TD2XLogger, ID2XHandler, ID2XProcessing, ID2XFiles, ID2XResults,
    ID2XMethods, ID2XChecks, ID2XTrees)
  public
    CalledEndProcessing, CalledBeginFile, CalledEndFile, CalledBeginResults, CalledEndResults,
      CalledAddAttr, CalledAddText, CalledRollBackTo, CalledTrimChildren,
      CalledCheckBeforeMethod, CalledCheckAfterMethod, CalledBeginMethod, CalledEndMethod,
      CalledLexerInclude, CalledParserMessage, CreateStreams: Boolean;

    function Description: String;
    procedure EndProcessing(pOutput: TStreamWriterRef);
    procedure BeginFile(pFile: String; pInput: TStreamReaderRef);
    procedure EndFile(pFile: String; pOutput: TStreamWriterRef);
    procedure BeginResults;
    procedure EndResults(pOutput: TStreamWriterRef);
    procedure AddAttr(pName: String; pValue: String);
    procedure AddText(pText: String);
    procedure RollBackTo(pElement: String);
    procedure TrimChildren(pElement: String);
    function CheckBeforeMethod(pMethod: String): Boolean;
    function CheckAfterMethod(pMethod: String): Boolean;
    procedure BeginMethod(pMethod: String);
    procedure EndMethod(pMethod: String);

  end;

  TD2XParserHandlerTester = class(TD2XLogger, ID2XHandler, ID2XParser)
  public
    CalledInitParser: Boolean;

    function Description: String;
    procedure InitParser(pParser: TD2XDefinesParser; pActive: ID2XFlag);
  end;

implementation

{ TD2XHandlerTester }

procedure TD2XHandlerTester.BeginMethod(pMethod: String);
begin
  CalledBeginMethod := true;
  inherited;
end;

procedure TD2XHandlerTester.AddAttr(pName, pValue: String);
begin
  CalledAddAttr := true;
end;

procedure TD2XHandlerTester.AddText(pText: String);
begin
  CalledAddText := true;
end;

procedure TD2XHandlerTester.BeginFile(pFile: String; pInput: TStreamReaderRef);
begin
  CalledBeginFile := true;
  inherited;
  if CreateStreams then
    pInput;
end;

procedure TD2XHandlerTester.BeginResults;
begin
  CalledBeginResults := true;
  inherited;
end;

function TD2XHandlerTester.CheckAfterMethod(pMethod: String): Boolean;
begin
  CalledCheckAfterMethod := true;

  Result := true;
end;

function TD2XHandlerTester.CheckBeforeMethod(pMethod: String): Boolean;
begin
  CalledCheckBeforeMethod := true;

  Result := true;
end;

function TD2XHandlerTester.Description: String;
begin
  Result := 'Handler Tester';
end;

procedure TD2XHandlerTester.EndMethod(pMethod: String);
begin
  CalledEndMethod := true;
  inherited;
end;

procedure TD2XHandlerTester.EndFile(pFile: String; pOutput: TStreamWriterRef);
begin
  CalledEndFile := true;
  inherited;
  if CreateStreams then
    pOutput;
end;

procedure TD2XHandlerTester.EndProcessing(pOutput: TStreamWriterRef);
begin
  CalledEndProcessing := true;
  inherited;
  if CreateStreams then
    pOutput;
end;

procedure TD2XHandlerTester.EndResults(pOutput: TStreamWriterRef);
begin
  CalledEndResults := true;
  inherited;
  if CreateStreams then
    pOutput;
end;

procedure TD2XHandlerTester.RollBackTo(pElement: String);
begin
  CalledRollBackTo := true;
end;

procedure TD2XHandlerTester.TrimChildren(pElement: String);
begin
  CalledTrimChildren := true;
end;

{ TD2XParserHandlerTester }

function TD2XParserHandlerTester.Description: String;
begin
  Result := 'Parser Handler Tester';
end;

procedure TD2XParserHandlerTester.InitParser(pParser: TD2XDefinesParser; pActive: ID2XFlag);
begin
  CalledInitParser := true;
end;

end.
