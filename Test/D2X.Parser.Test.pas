unit D2X.Parser.Test;

interface

uses
  D2X.Parser,
  TestFramework;

type
  TD2XParserTestCase = class(TTestCase)
  strict private
    fText, fAttrName, fAttrValue: string;
    fParser: TD2XDefinesParser;

    procedure OnAddText(pText: string);
    procedure OnAddAttribute(pName, pValue: string);

  protected
    procedure InitParser(pParser: TD2XDefinesParser);

    procedure CheckText(pExpected, pLabel: string);
    procedure CheckAttribute(pExpName, pExpValue, pLabel: string);
    procedure CheckDefines(pExpected, pLabel: string);
  end;

implementation

uses
  CastaliaPasLex,
  CastaliaPasLexTypes,
  CastaliaSimplePasPar,
  System.Classes;

type
  TestTD2XDefinesParser = class(TD2XParserTestCase)
  strict private
    FD2XDefinesParser: TD2XDefinesParser;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUsedUnitName;
    procedure TestMainUnitName;
    procedure TestMainUsedUnitExpression;
    procedure TestParseFile;

    procedure TestGetLexerDefines;
    procedure TestProcessString;
  end;

  TestTD2XUsesParser = class(TD2XParserTestCase)
  strict private
    FD2XUsesParser: TD2XUsesParser;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUsedUnitName;
    procedure TestMainUnitName;
    procedure TestMainUsedUnit;
    procedure TestMainUsedUnitExpression;
  end;

  TestTD2XFullParser = class(TD2XParserTestCase)
  strict private
    FD2XFullParser: TD2XFullParser;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUsedUnitName;
    procedure TestMainUnitName;
    procedure TestMainUsedUnitExpression;
  end;

{ TestTD2XDefinesParser }

procedure TestTD2XDefinesParser.SetUp;
begin
  FD2XDefinesParser := TD2XDefinesParser.Create;
  InitParser(FD2XDefinesParser);
end;

procedure TestTD2XDefinesParser.TearDown;
begin
  FD2XDefinesParser.Free;
  FD2XDefinesParser := nil;
end;

procedure TestTD2XDefinesParser.TestGetLexerDefines;
var
  pDefs: TStringList;
begin
  pDefs := TStringList.Create;
  try
    FD2XDefinesParser.GetLexerDefines(pDefs);

    CheckEqualsString('VER230,WIN32,CPU386,MSWINDOWS,CONDITIONALEXPRESSIONS,UNICODE',
      pDefs.CommaText, 'Default Lexer Defines');
  finally
    pDefs.Free;
  end;
end;

procedure TestTD2XDefinesParser.TestMainUnitName;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUnitName';
  pContents := 'unit Test; end.';

  FD2XDefinesParser.ProcessString(pFilename, pContents);

  CheckText('Test', 'Main Unit Name');
  CheckAttribute('', '', 'Blank');
end;

procedure TestTD2XDefinesParser.TestMainUsedUnitExpression;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUsedUnitExpression';
  pContents := 'program Test; uses Test2; begin end.';

  FD2XDefinesParser.ProcessString(pFilename, pContents);

  CheckText('Test2', 'Main Used Unit Name');
  CheckAttribute('', '', 'Blank');
end;

procedure TestTD2XDefinesParser.TestParseFile;
var
  pContents: string;
  pFilename: string;
begin
  FD2XDefinesParser.StartDefines.Clear;
  pFilename := 'TestParseFile';
  pContents := 'unit Test; end.';

  FD2XDefinesParser.ProcessString(pFilename, pContents);

  CheckDefines('', 'Cleared Lexer');
end;

procedure TestTD2XDefinesParser.TestProcessString;
var
  pContents: string;
  pFilename: string;
begin
  FD2XDefinesParser.StartDefines.Clear;
  pFilename := 'TestProcessString';
  pContents := 'unit Test; {$DEFINE HELLO} end.';

  FD2XDefinesParser.ProcessString(pFilename, pContents);
  CheckDefines('HELLO', 'Current');
end;

procedure TestTD2XDefinesParser.TestUsedUnitName;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestUsedUnitName';
  pContents := 'unit Test; interface uses Test1; end.';

  FD2XDefinesParser.ProcessString(pFilename, pContents);

  CheckText('Test1', 'Used Unit Name');
  CheckAttribute('', '', 'Blank');
end;

{ TestTD2XUsesParser }

procedure TestTD2XUsesParser.SetUp;
begin
  FD2XUsesParser := TD2XUsesParser.Create;
  InitParser(FD2XUsesParser);
end;

procedure TestTD2XUsesParser.TearDown;
begin
  FD2XUsesParser.Free;
  FD2XUsesParser := nil;
end;

procedure TestTD2XUsesParser.TestUsedUnitName;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestUsedUnitName';
  pContents := 'unit Test; interface uses Test1; implementation end.';

  FD2XUsesParser.ProcessString(pFilename, pContents);

  CheckText('Test1', 'Used Unit Name');
  CheckAttribute('', '', 'Blank');
end;

procedure TestTD2XUsesParser.TestMainUnitName;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUnitName';
  pContents := 'unit Test; implementation end.';

  FD2XUsesParser.ProcessString(pFilename, pContents);

  CheckText('Test', 'Main Unit Name');
  CheckAttribute('', '', 'Blank');
end;

procedure TestTD2XUsesParser.TestMainUsedUnit;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUsedUnitExpression';
  pContents := 'program Test; uses Test2; begin end.';

  FD2XUsesParser.ProcessString(pFilename, pContents);

  CheckText('Test2', 'Main Used File Name');
  CheckAttribute('', '', 'Blank');
end;

procedure TestTD2XUsesParser.TestMainUsedUnitExpression;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUsedUnitExpression';
  pContents := 'program Test; uses Test2 in ''Test2.pas''; begin end.';

  FD2XUsesParser.ProcessString(pFilename, pContents);

  CheckText('''Test2.pas''', 'Main Used File Name');
  CheckAttribute('', '', 'Blank');
end;

{ TestTD2XFullParser }

procedure TestTD2XFullParser.SetUp;
begin
  FD2XFullParser := TD2XFullParser.Create;
  InitParser(FD2XFullParser);
end;

procedure TestTD2XFullParser.TearDown;
begin
  FD2XFullParser.Free;
  FD2XFullParser := nil;
end;


procedure TestTD2XFullParser.TestMainUnitName;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUnitName';
  pContents := 'unit Test; implementation end.';

  FD2XFullParser.ProcessString(pFilename, pContents);

  CheckText('Test', 'Used Unit Name');
  CheckAttribute('', '', 'Blank');
end;

procedure TestTD2XFullParser.TestMainUsedUnitExpression;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUsedUnitExpression';
  pContents := 'program Test; uses Test2; begin end.';

  FD2XFullParser.ProcessString(pFilename, pContents);

  CheckText('Test2', 'Used Unit Name');
  CheckAttribute('', '', 'Blank');
end;

procedure TestTD2XFullParser.TestUsedUnitName;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestUsedUnitName';
  pContents := 'unit Test; interface uses Test1; implementation end.';

  FD2XFullParser.ProcessString(pFilename, pContents);

  CheckText('Test1', 'Used Unit Name');
  CheckAttribute('', '', 'Blank');
end;

{ TD2XParserTestCase }

procedure TD2XParserTestCase.CheckAttribute(pExpName, pExpValue, pLabel: string);
begin
  CheckEqualsString(pExpName, fAttrName, pLabel + ' Name');
  CheckEqualsString(pExpValue, fAttrValue, pLabel + ' Value');
end;

procedure TD2XParserTestCase.CheckDefines(pExpected, pLabel: string);
var
  pDefs: TStringList;
begin
  pDefs := TStringList.Create;
  try
    fParser.GetLexerDefines(pDefs);
    CheckEqualsString(pExpected, pDefs.CommaText, pLabel + ' Defines');
  finally
    pDefs.Free;
  end;
end;

procedure TD2XParserTestCase.CheckText(pExpected, pLabel: string);
begin
  CheckEqualsString(pExpected, fText, pLabel);
end;

procedure TD2XParserTestCase.InitParser(pParser: TD2XDefinesParser);
begin
  fParser := pParser;
  pParser.AddText := OnAddText;
  pParser.AddAttribute := OnAddAttribute;
end;

procedure TD2XParserTestCase.OnAddAttribute(pName, pValue: string);
begin
  fAttrName := pName;
  fAttrValue := pValue;
end;

procedure TD2XParserTestCase.OnAddText(pText: string);
begin
  fText := pText;
end;

initialization

// Register any test cases with the test runner
RegisterTests('Parser', [TestTD2XDefinesParser.Suite, TestTD2XUsesParser.Suite ,
      TestTD2XFullParser.Suite ]);

end.
