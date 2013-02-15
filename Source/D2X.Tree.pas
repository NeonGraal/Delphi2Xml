unit D2X.Tree;

interface

uses
  System.Classes,
  System.Generics.Collections,
  Xml.XMLIntf;
// Xml.XMLDoc,

type
  TD2XmlDoc = class;

  TD2XmlNode = class
  strict private
    fParent: TD2XmlNode;
    fXml: TStringStream;

  strict protected
    fTag: string;
    fText: string;
    fDoc: TD2XmlDoc;

    constructor CreateTag(pTag: string; pParent: TD2XmlNode); virtual;

  protected
    procedure MakeXml(pW: TTextWriter); virtual;
    function GetXml: TStringStream;

  public
    constructor Create;
    destructor Destroy; override;

    function AddChild(pTag: string): TD2XmlNode; virtual;
    function AddAttribute(pTag: string): TD2XmlNode; virtual;
    function HasChildNodes: Boolean; virtual;
    procedure TrimChildren(pElement: string); virtual;

    property ParentNode: TD2XmlNode read fParent;
    property LocalName: string read fTag;
    property Text: string read fText write fText;
    property Xml: TStringStream read GetXml;
  end;

  TD2XmlAttribute = class(TD2XmlNode)
  protected
    procedure MakeXml(pW: TTextWriter); override;
  end;

  TD2XmlElement = class(TD2XmlNode)
  private
    fChildren: TObjectList<TD2XmlNode>;
    fAtttributes: TObjectList<TD2XmlNode>;

  strict protected
    constructor CreateTag(pTag: string; pParent: TD2XmlNode = nil); override;

  protected
    procedure MakeXml(pW: TTextWriter); override;

  public
    destructor Destroy; override;

    function AddChild(pTag: string): TD2XmlNode; override;
    function AddAttribute(pTag: string): TD2XmlNode; override;
    function HasChildNodes: Boolean; override;
    procedure TrimChildren(pElement: string); override;
  end;

  TD2XmlDoc = class(TD2XmlElement)
  private
    FOptions: TXMLDocOptions;

  protected
    procedure MakeXml(pW: TTextWriter); override;

  public
    constructor CreateDoc;
    function AddChild(pTag: string): TD2XmlNode; override;

    property Options: TXMLDocOptions read FOptions write FOptions;

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
  CreateTag('');
  fDoc := Self;
end;

procedure TD2XmlDoc.MakeXml(pW: TTextWriter);
begin
  pW.WriteLine('<?xml version="1.0"?>');
  inherited;
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

constructor TD2XmlNode.Create;
begin
  Assert(False, 'Invalid constructor called');
end;

constructor TD2XmlNode.CreateTag(pTag: string; pParent: TD2XmlNode);
begin
  fTag := pTag;
  fParent := pParent;
  if Assigned(fParent) then
    fDoc := pParent.fDoc;
  fXml := nil;
end;

destructor TD2XmlNode.Destroy;
begin
  FreeAndNil(fXml);

  inherited;
end;

function TD2XmlNode.GetXml: TStringStream;
var
  lW: TStreamWriter;
begin
  if not Assigned(fXml) then
    try
      fXml := TStringStream.Create;
      lW := TStreamWriter.Create(fXml);
      MakeXml(lW)
    finally
      FreeAndNil(lW);
    end;

  fXml.Position := 0;
  Result := fXml;
end;

function TD2XmlNode.HasChildNodes: Boolean;
begin
  Result := False;
end;

procedure TD2XmlNode.MakeXml(pW: TTextWriter);
begin

end;

procedure TD2XmlNode.TrimChildren(pElement: string);
begin

end;

{ TD2XmlElement }

function TD2XmlElement.AddAttribute(pTag: string): TD2XmlNode;
begin
  Assert(Assigned(fAtttributes), 'AddAttribute called after Xml Generated');

  Result := TD2XmlAttribute.CreateTag(pTag, Self);
  fAtttributes.Add(Result);
end;

function TD2XmlElement.AddChild(pTag: string): TD2XmlNode;
begin
  Assert(Assigned(fChildren), 'AddChild called after Xml Generated');

  Result := TD2XmlElement.CreateTag(pTag, Self);
  fChildren.Add(Result);
end;

constructor TD2XmlElement.CreateTag(pTag: string; pParent: TD2XmlNode);
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
  Assert(Assigned(fChildren), 'HasChildNodes called after Xml Generated');

  Result := fChildren.Count > 0;
end;

procedure TD2XmlElement.MakeXml(pW: TTextWriter);
var
  lN: TD2XmlNode;
  lR: TStreamReader;
  lS: string;
begin
  if fTag > '' then
  begin
    pW.Write('<');
    pW.Write(fTag);
    if fAtttributes.Count > 0 then
      for lN in fAtttributes do
        lN.MakeXml(pW);
    FreeAndNil(fAtttributes);

    if (fChildren.Count > 0) or (fText > '') then
    begin
      pW.Write('>');
      if fText > '' then
        pW.Write(fText)
      else
      begin
        if doNodeAutoIndent in fDoc.Options then
        begin
          pW.WriteLine;
          for lN in fChildren do
            try
              lR := TStreamReader.Create(lN.GetXml);
              lR.BaseStream.Seek(0, soBeginning);
              while not lR.EndOfStream do
              begin
                lS := lR.ReadLine;
                pW.WriteLine('  ' + lS);
              end;
            finally
              FreeAndNil(lR);
            end;
        end
        else
          for lN in fChildren do
            pW.Write(lN.GetXml.DataString);
        FreeAndNil(fChildren);
      end;
      pW.Write('</');
      pW.Write(fTag);
      pW.Write('>');
    end
    else
      pW.Write(' />');
    if doNodeAutoIndent in fDoc.Options then
      pW.WriteLine;
  end
  else
    pW.Write(fText);
end;

procedure TD2XmlElement.TrimChildren(pElement: string);
var
  i: Integer;
begin
  Assert(Assigned(fChildren), 'TrimChildren called after Xml Generated');

  for i := fChildren.Count - 1 downto 0 do
    if fChildren[i].LocalName = pElement then
      fChildren.Delete(i);
end;

{ TD2XmlAttribute }

procedure TD2XmlAttribute.MakeXml(pW: TTextWriter);
begin
  pW.Write(' ');
  pW.Write(fTag);
  pW.Write('="');
  pW.Write(XmlEscape(fText));
  pW.Write('"');
end;

end.
