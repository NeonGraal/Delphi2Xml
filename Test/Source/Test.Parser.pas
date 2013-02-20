unit Test.Parser;

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
  System.Classes;

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

end.

