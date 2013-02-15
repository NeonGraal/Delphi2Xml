unit D2X.Tree.Json;

interface

uses
  System.Classes,
  System.Generics.Collections,
  Xml.XMLIntf;
// Xml.XMLDoc,

type
  TD2JsonDoc = class;

  TD2JsonNode = class
  strict private
    fParent: TD2JsonNode;
    fJson: TStringStream;

  strict protected
    fTag: string;
    fText: string;
    fDoc: TD2JsonDoc;

    constructor CreateTag(pTag: string; pParent: TD2JsonNode); virtual;

  protected
    procedure MakeJson(pW: TTextWriter); virtual;
    function GetJson: TStringStream;

  public
    constructor Create;
    destructor Destroy; override;

    function AddChild(pTag: string): TD2JsonNode; virtual;
    function AddAttribute(pTag: string): TD2JsonNode; virtual;
    function HasChildNodes: Boolean; virtual;
    procedure TrimChildren(pElement: string); virtual;

    property ParentNode: TD2JsonNode read fParent;
    property LocalName: string read fTag;
    property Text: string read fText write fText;
    property Json: TStringStream read GetJson;
  end;

  TD2JsonAttribute = class(TD2JsonNode)
  protected
    procedure MakeJson(pW: TTextWriter); override;
  end;

  TD2JsonElement = class(TD2JsonNode)
  private
    fChildren: TObjectList<TD2JsonNode>;
    fAtttributes: TObjectList<TD2JsonNode>;

  strict protected
    constructor CreateTag(pTag: string; pParent: TD2JsonNode = nil); override;

  protected
    procedure MakeJson(pW: TTextWriter); override;

  public
    destructor Destroy; override;

    function AddChild(pTag: string): TD2JsonNode; override;
    function AddAttribute(pTag: string): TD2JsonNode; override;
    function HasChildNodes: Boolean; override;
    procedure TrimChildren(pElement: string); override;
  end;

  TD2JsonDoc = class(TD2JsonElement)
  private
//    FOptions: TJsonDocOptions;

  protected
    procedure MakeJson(pW: TTextWriter); override;

  public
    constructor CreateDoc;
    function AddChild(pTag: string): TD2JsonNode; override;

//    property Options: TJsonDocOptions read FOptions write FOptions;

  end;

function NewJsonDocument: TD2JsonDoc;

implementation

uses
  Data.Cloud.CloudAPI,
  System.StrUtils,
  System.SysUtils;

function NewJsonDocument: TD2JsonDoc;
begin
  Result := TD2JsonDoc.CreateDoc;
end;

{ TD2JsonDoc }

function TD2JsonDoc.AddChild(pTag: string): TD2JsonNode;
begin
  if fTag = '' then
  begin
    fTag := pTag;
    Result := Self;
  end
  else
    Result := inherited AddChild(pTag);
end;

constructor TD2JsonDoc.CreateDoc;
begin
  CreateTag('');
  fDoc := Self;
end;

procedure TD2JsonDoc.MakeJson(pW: TTextWriter);
begin
  pW.WriteLine('<?Json version="1.0"?>');
  inherited;
end;

{ TD2JsonNode }

function TD2JsonNode.AddAttribute(pTag: string): TD2JsonNode;
begin
  Result := nil;
end;

function TD2JsonNode.AddChild(pTag: string): TD2JsonNode;
begin
  Result := nil;
end;

constructor TD2JsonNode.Create;
begin
  Assert(False, 'Invalid constructor called');
end;

constructor TD2JsonNode.CreateTag(pTag: string; pParent: TD2JsonNode);
begin
  fTag := pTag;
  fParent := pParent;
  if Assigned(fParent) then
    fDoc := pParent.fDoc;
  fJson := nil;
end;

destructor TD2JsonNode.Destroy;
begin
  FreeAndNil(fJson);

  inherited;
end;

function TD2JsonNode.GetJson: TStringStream;
var
  lW: TStreamWriter;
begin
  if not Assigned(fJson) then
    try
      fJson := TStringStream.Create;
      lW := TStreamWriter.Create(fJson);
      MakeJson(lW)
    finally
      FreeAndNil(lW);
    end;

  fJson.Position := 0;
  Result := fJson;
end;

function TD2JsonNode.HasChildNodes: Boolean;
begin
  Result := False;
end;

procedure TD2JsonNode.MakeJson(pW: TTextWriter);
begin

end;

procedure TD2JsonNode.TrimChildren(pElement: string);
begin

end;

{ TD2JsonElement }

function TD2JsonElement.AddAttribute(pTag: string): TD2JsonNode;
begin
  Assert(Assigned(fAtttributes), 'AddAttribute called after Json Generated');

  Result := TD2JsonAttribute.CreateTag(pTag, Self);
  fAtttributes.Add(Result);
end;

function TD2JsonElement.AddChild(pTag: string): TD2JsonNode;
begin
  Assert(Assigned(fChildren), 'AddChild called after Json Generated');

  Result := TD2JsonElement.CreateTag(pTag, Self);
  fChildren.Add(Result);
end;

constructor TD2JsonElement.CreateTag(pTag: string; pParent: TD2JsonNode);
begin
  inherited;

  fChildren := TObjectList<TD2JsonNode>.Create;
  fAtttributes := TObjectList<TD2JsonNode>.Create;
end;

destructor TD2JsonElement.Destroy;
begin
  FreeAndNil(fAtttributes);
  FreeAndNil(fChildren);

  inherited;
end;

function TD2JsonElement.HasChildNodes: Boolean;
begin
  Assert(Assigned(fChildren), 'HasChildNodes called after Json Generated');

  Result := fChildren.Count > 0;
end;

procedure TD2JsonElement.MakeJson(pW: TTextWriter);
var
  lN: TD2JsonNode;
  lR: TStreamReader;
  lS: string;
begin
  if fTag > '' then
  begin
    pW.Write('<');
    pW.Write(fTag);
    if fAtttributes.Count > 0 then
      for lN in fAtttributes do
        lN.MakeJson(pW);
    FreeAndNil(fAtttributes);

    if (fChildren.Count > 0) or (fText > '') then
    begin
      pW.Write('>');
      if fText > '' then
        pW.Write(fText)
      else
      begin
//        if doNodeAutoIndent in fDoc.Options then
//        begin
//          pW.WriteLine;
//          for lN in fChildren do
//            try
//              lR := TStreamReader.Create(lN.GetJson);
//              lR.BaseStream.Seek(0, soBeginning);
//              while not lR.EndOfStream do
//              begin
//                lS := lR.ReadLine;
//                pW.WriteLine('  ' + lS);
//              end;
//            finally
//              FreeAndNil(lR);
//            end;
//        end
//        else
          for lN in fChildren do
            pW.Write(lN.GetJson.DataString);
        FreeAndNil(fChildren);
      end;
      pW.Write('</');
      pW.Write(fTag);
      pW.Write('>');
    end
    else
      pW.Write(' />');
//    if doNodeAutoIndent in fDoc.Options then
//      pW.WriteLine;
  end
  else
    pW.Write(fText);
end;

procedure TD2JsonElement.TrimChildren(pElement: string);
var
  i: Integer;
begin
  Assert(Assigned(fChildren), 'TrimChildren called after Json Generated');

  for i := fChildren.Count - 1 downto 0 do
    if fChildren[i].LocalName = pElement then
      fChildren.Delete(i);
end;

{ TD2JsonAttribute }

procedure TD2JsonAttribute.MakeJson(pW: TTextWriter);
begin
  pW.Write(' ');
  pW.Write(fTag);
  pW.Write('="');
//  pW.Write(JsonEscape(fText));
  pW.Write(fText);
  pW.Write('"');
end;

end.
