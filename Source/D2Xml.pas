unit D2Xml;

interface

uses
  System.Classes,
  System.Generics.Collections,
  Xml.XMLIntf;
// Xml.XMLDoc,

type
  TD2XmlNode = class
  strict private
    fText: string;
    fParent: TD2XmlNode;

  strict protected
    fTag: string;

    constructor Create(pTag: string; pParent: TD2XmlNode); virtual;

  protected
    procedure WriteXml(pXml: TTextWriter; pOptions: TXMLDocOptions; pIndent: Integer); virtual;

  public
    function AddChild(pTag: string): TD2XmlNode; virtual;
    function AddAttribute(pTag: string): TD2XmlNode; virtual;
    function HasChildNodes: Boolean; virtual;

    property ParentNode: TD2XmlNode read fParent;
    property LocalName: string read fTag;
    property Text: string read fText write fText;

  end;

  TD2XmlAttribute = class(TD2XmlNode)
  protected
    procedure WriteXml(pXml: TTextWriter; pOptions: TXMLDocOptions; pIndent: Integer); override;
  end;

  TD2XmlElement = class(TD2XmlNode)
  private
    fChildren: TObjectList<TD2XmlNode>;
    fAtttributes: TObjectList<TD2XmlNode>;

  strict protected
    constructor Create(pTag: string; pParent: TD2XmlNode); override;

  protected
    procedure WriteXml(pXml: TTextWriter; pOptions: TXMLDocOptions; pIndent: Integer); override;

  public
    destructor Destroy; override;

    function AddChild(pTag: string): TD2XmlNode; override;
    function AddAttribute(pTag: string): TD2XmlNode; override;
    function HasChildNodes: Boolean; override;
  end;

  TD2XmlDoc = class(TD2XmlElement)
  private
    FOptions: TXMLDocOptions;

    function GetXml: TStringStream;

  public
    constructor CreateDoc;
    function AddChild(pTag: string): TD2XmlNode; override;

    property Options: TXMLDocOptions read FOptions write FOptions;
    property Xml: TStringStream read GetXml;

  end;

function NewXmlDocument: TD2XmlDoc;

implementation

uses
  Data.Cloud.CloudAPI,
  System.StrUtils,
  System.SysUtils;

function NewXmlDocument: TD2XmlDoc;
begin
  Result := TD2XmlDoc.CreateDoc;
end;

{ TD2XmlDoc }

function TD2XmlDoc.AddChild(pTag: string): TD2XmlNode;
begin
  if fTag = '' then
  begin
    fTag := pTag;
    Result := Self;
  end
  else
    Result := inherited AddChild(pTag);
end;

constructor TD2XmlDoc.CreateDoc;
begin
  Create('', nil);
end;

function TD2XmlDoc.GetXml: TStringStream;
var
  lWriter: TStreamWriter;
begin
  Result := TStringStream.Create;

  lWriter := TStreamWriter.Create(Result);
  try
    lWriter.Write('<?xml version="1.0"?>');
    WriteXml(lWriter, FOptions, 0);
  finally
    FreeAndNil(lWriter);
  end;
end;

{ TD2XmlNode }

function TD2XmlNode.AddAttribute(pTag: string): TD2XmlNode;
begin
  Result := nil;
end;

function TD2XmlNode.AddChild(pTag: string): TD2XmlNode;
begin
  Result := nil;
end;

constructor TD2XmlNode.Create(pTag: string; pParent: TD2XmlNode);
begin
  fTag := pTag;
  fParent := pParent;
end;

function TD2XmlNode.HasChildNodes: Boolean;
begin
  Result := False;
end;

procedure TD2XmlNode.WriteXml(pXml: TTextWriter; pOptions: TXMLDocOptions; pIndent: Integer);
begin
  //
end;

{ TD2XmlElement }

function TD2XmlElement.AddAttribute(pTag: string): TD2XmlNode;
begin
  Result := TD2XmlAttribute.Create(pTag, Self);
  fAtttributes.Add(Result);
end;

function TD2XmlElement.AddChild(pTag: string): TD2XmlNode;
begin
  Result := TD2XmlElement.Create(pTag, Self);
  fChildren.Add(Result);
end;

constructor TD2XmlElement.Create(pTag: string; pParent: TD2XmlNode);
begin
  inherited;

  fChildren := TObjectList<TD2XmlNode>.Create;
  fAtttributes := TObjectList<TD2XmlNode>.Create;
end;

destructor TD2XmlElement.Destroy;
begin
  FreeAndNil(fAtttributes);
  FreeAndNil(fChildren);

  inherited;
end;

function TD2XmlElement.HasChildNodes: Boolean;
begin
  Result := fChildren.Count > 0;
end;

procedure TD2XmlElement.WriteXml(pXml: TTextWriter; pOptions: TXMLDocOptions;
  pIndent: Integer);
var
  lN: TD2XmlNode;

  procedure CheckIndent;
  begin
    if doNodeAutoIndent in pOptions then
    begin
      pXml.WriteLine;
      pXml.Write(DupeString('  ', pIndent));
    end;
  end;

begin
  if fTag > '' then
  begin
    CheckIndent;
    pXml.Write('<');
    pXml.Write(fTag);
    if fAtttributes.Count > 0 then
      for lN in fAtttributes do
        lN.WriteXml(pXml, pOptions, pIndent + 1);

    if (fChildren.Count > 0) or (Text > '') then
    begin
      pXml.Write('>');
      if Text > '' then
        pXml.Write(Text)
      else
      begin
        for lN in fChildren do
          lN.WriteXml(pXml, pOptions, pIndent + 1);
        CheckIndent;
      end;
      pXml.Write('</');
      pXml.Write(fTag);
      pXml.Write('>');
    end
    else
      pXml.Write(' />');
  end
  else
    pXml.Write(Text);
end;

{ TD2XmlAttribute }

procedure TD2XmlAttribute.WriteXml(pXml: TTextWriter; pOptions: TXMLDocOptions;
  pIndent: Integer);
begin
  pXml.Write(' ');
  pXml.Write(LocalName);
  pXml.Write('="');
  pXml.Write(XmlEscape(Text));
  pXml.Write('"');
end;

end.
