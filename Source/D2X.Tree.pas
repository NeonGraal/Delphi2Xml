unit D2X.Tree;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils;

type
  ETreeWriter = class(Exception);

  TD2XTreeOption = (toNone, toAutoIndent);
  TD2XTreeOptions = set of TD2XTreeOption;

  TD2XTreeWriterClass = class of TD2XTreeWriter;

  TD2XTreeDoc = class;

  TD2XTreeNode = class
  strict private
    fParent: TD2XTreeNode;
    fStream: TStringStream;

  protected
    fWriter: TD2XTreeWriterClass;
    fTag: string;
    fText: string;
    fDoc: TD2XTreeDoc;

    constructor CreateTag(pTag: string; pParent: TD2XTreeNode); virtual;

    function GetStream: TStringStream;

  public
    constructor Create;
    destructor Destroy; override;

    function AddChild(pTag: string): TD2XTreeNode; virtual;
    function AddAttribute(pTag: string): TD2XTreeNode; virtual;
    function HasChildren: Boolean; virtual;
    function HasAttributes: Boolean; virtual;
    procedure TrimChildren(pElement: string); virtual;

  public
    property ParentNode: TD2XTreeNode read fParent;
    property LocalName: string read fTag;
    property Text: string read fText write fText;
    property Stream: TStringStream read GetStream;
  end;

  TD2XTreeElement = class(TD2XTreeNode)
  private
    fChildren: TObjectList<TD2XTreeNode>;
    fAtttributes: TObjectList<TD2XTreeNode>;

  strict protected
    constructor CreateTag(pTag: string; pParent: TD2XTreeNode = nil); override;

  public type
    TEachProc = reference to procedure(pChild: TD2XTreeNode);

  public
    destructor Destroy; override;

    function AddChild(pTag: string): TD2XTreeNode; override;
    function AddAttribute(pTag: string): TD2XTreeNode; override;
    function HasChildren: Boolean; override;
    function HasAttributes: Boolean; override;
    procedure TrimChildren(pElement: string); override;
  end;

  TD2XTreeDoc = class(TD2XTreeElement)
  private
    fOptions: TD2XTreeOptions;

  public
    constructor CreateDoc(pWriter: TD2XTreeWriterClass);

    function AddChild(pTag: string): TD2XTreeNode; override;

    procedure AddOptions(pOptions: TD2XTreeOptions);
    procedure RemoveOptions(pOptions: TD2XTreeOptions);

  end;

  TD2XTreeWriter = class
  protected
    fW: TTextWriter;

    procedure WriteAttribute(pNode: TD2XTreeElement; pAttr: TD2XTreeNode); virtual;
    procedure WriteElement(pNode: TD2XTreeElement);
    procedure WriteDoc(pNode: TD2XTreeDoc); virtual;

    procedure WriteHead(pNode: TD2XTreeElement); virtual;
    procedure WriteElHead(pNode: TD2XTreeElement); virtual;
    procedure WriteChildLine(pNode: TD2XTreeElement; pLine: string; pFirst: Boolean); virtual;
    procedure WriteElText(pNode: TD2XTreeElement); virtual;
    procedure WriteElFoot(pNode: TD2XTreeElement); virtual;
    procedure WriteAttrHead(pNode: TD2XTreeElement); virtual;
    procedure WriteText(pNode: TD2XTreeElement); virtual;
    procedure WriteRawText(pNode: TD2XTreeElement); virtual;

    procedure WriteChildren(pNode: TD2XTreeElement);
    procedure WriteAttributes(pNode: TD2XTreeElement);

    function HasOption(pNode: TD2XTreeElement; pOption: TD2XTreeOption): Boolean;

  public
    constructor Create(pStream: TStringStream); virtual;
    destructor Destroy; override;

    procedure WriteNode(pNode: TD2XTreeNode);
  end;

function NewTreeDocument: TD2XTreeDoc;

implementation

function NewTreeDocument: TD2XTreeDoc;
begin
  Result := TD2XTreeDoc.CreateDoc(TD2XTreeWriter);
end;

{ TD2XTreeDoc }

function TD2XTreeDoc.AddChild(pTag: string): TD2XTreeNode;
begin
  if fTag = '' then
  begin
    fTag := pTag;
    Result := Self;
  end
  else
    Result := inherited AddChild(pTag);
end;

procedure TD2XTreeDoc.AddOptions(pOptions: TD2XTreeOptions);
begin
  fOptions := fOptions + pOptions;
end;

constructor TD2XTreeDoc.CreateDoc(pWriter: TD2XTreeWriterClass);
begin
  CreateTag('');
  fDoc := Self;
  fWriter := pWriter;
end;

procedure TD2XTreeDoc.RemoveOptions(pOptions: TD2XTreeOptions);
begin
  fOptions := fOptions - pOptions;
end;

{ TD2XTreeNode }

function TD2XTreeNode.AddAttribute(pTag: string): TD2XTreeNode;
begin
  Result := nil;
end;

function TD2XTreeNode.AddChild(pTag: string): TD2XTreeNode;
begin
  Result := nil;
end;

constructor TD2XTreeNode.Create;
begin
  Assert(False, 'Invalid constructor called');
end;

constructor TD2XTreeNode.CreateTag(pTag: string; pParent: TD2XTreeNode);
begin
  fTag := pTag;
  fParent := pParent;
  if Assigned(fParent) then
  begin
    fDoc := pParent.fDoc;
    fWriter := pParent.fWriter;
  end;
  fStream := nil;
end;

destructor TD2XTreeNode.Destroy;
begin
  FreeAndNil(fStream);

  inherited;
end;

function TD2XTreeNode.GetStream: TStringStream;
var
  lW: TD2XTreeWriter;
begin
  if not Assigned(fStream) then
    if Assigned(fWriter) then
      try
        fStream := TStringStream.Create;
        lW := fWriter.Create(fStream);
        lW.WriteNode(Self);
      finally
        FreeAndNil(lW);
      end
    else
      raise ETreeWriter.Create('Writer not set');

  fStream.Position := 0;
  Result := fStream;
end;

function TD2XTreeNode.HasAttributes: Boolean;
begin
  Result := False;
end;

function TD2XTreeNode.HasChildren: Boolean;
begin
  Result := False;
end;

procedure TD2XTreeNode.TrimChildren(pElement: string);
begin

end;

{ TD2XTreeElement }

function TD2XTreeElement.AddAttribute(pTag: string): TD2XTreeNode;
begin
  Assert(Assigned(fAtttributes), 'AddAttribute called after Stream Generated: ' +
      Stream.DataString);

  Result := TD2XTreeNode.CreateTag(pTag, Self);
  fAtttributes.Add(Result);
end;

function TD2XTreeElement.AddChild(pTag: string): TD2XTreeNode;
begin
  Assert(Assigned(fChildren), 'AddChild called after Stream Generated: ' + Stream.DataString);

  Result := TD2XTreeElement.CreateTag(pTag, Self);
  fChildren.Add(Result);
end;

constructor TD2XTreeElement.CreateTag(pTag: string; pParent: TD2XTreeNode);
begin
  inherited;

  fChildren := TObjectList<TD2XTreeNode>.Create;
  fAtttributes := TObjectList<TD2XTreeNode>.Create;
end;

destructor TD2XTreeElement.Destroy;
begin
  FreeAndNil(fAtttributes);
  FreeAndNil(fChildren);

  inherited;
end;

function TD2XTreeElement.HasAttributes: Boolean;
begin
  Assert(Assigned(fAtttributes), 'HasAttributes called after Stream Generated: ' +
      Stream.DataString);

  Result := fAtttributes.Count > 0;
end;

function TD2XTreeElement.HasChildren: Boolean;
begin
  Assert(Assigned(fChildren), 'HasChildren called after Stream Generated: ' +
      Stream.DataString);

  Result := fChildren.Count > 0;
end;

procedure TD2XTreeElement.TrimChildren(pElement: string);
var
  i: Integer;
begin
  Assert(Assigned(fChildren), 'TrimChildren called after Stream Generated: ' +
      Stream.DataString);

  for i := fChildren.Count - 1 downto 0 do
    if fChildren[i].LocalName = pElement then
      fChildren.Delete(i);
end;

{ TD2XTreeWriter }

constructor TD2XTreeWriter.Create(pStream: TStringStream);
begin
  fW := TStreamWriter.Create(pStream);
end;

destructor TD2XTreeWriter.Destroy;
begin
  FreeAndNil(fW);

  inherited;
end;

function TD2XTreeWriter.HasOption(pNode: TD2XTreeElement; pOption: TD2XTreeOption): Boolean;
begin
  Result := pOption in pNode.fDoc.fOptions;
end;

procedure TD2XTreeWriter.WriteAttrHead(pNode: TD2XTreeElement);
begin
  WriteElHead(pNode);
  WriteElFoot(pNode);
end;

procedure TD2XTreeWriter.WriteAttribute(pNode: TD2XTreeElement; pAttr: TD2XTreeNode);
begin
  if HasOption(pNode, toAutoIndent) then
    fW.Write(' ');
  fW.Write('@');
  fW.Write(pAttr.fTag);
  if pAttr.fText > '' then
  begin
    fW.Write(':');
    fW.Write(pAttr.fText);
  end;
  fW.Write(';');
  if HasOption(pNode, toAutoIndent) then
    fW.WriteLine;
end;

procedure TD2XTreeWriter.WriteAttributes(pNode: TD2XTreeElement);
var
  lAttr: TD2XTreeNode;
begin
  if pNode.HasAttributes then
    for lAttr in pNode.fAtttributes do
      WriteAttribute(pNode, lAttr);
  FreeAndNil(pNode.fAtttributes);
end;

procedure TD2XTreeWriter.WriteChildLine(pNode: TD2XTreeElement; pLine: string;
  pFirst: Boolean);
begin
  if HasOption(pNode, toAutoIndent) then
    fW.WriteLine(' ' + pLine)
  else
    fW.Write(pLine);
end;

procedure TD2XTreeWriter.WriteChildren(pNode: TD2XTreeElement);
var
  lNode: TD2XTreeNode;
  lR: TStreamReader;
  lS: string;
  lFirst: Boolean;
begin
  for lNode in pNode.fChildren do
    try
      lR := TStreamReader.Create(lNode.GetStream);
      lR.BaseStream.Seek(0, soBeginning);
      lFirst := True;
      while not lR.EndOfStream do
      begin
        lS := lR.ReadLine;
        WriteChildLine(pNode, lS, lFirst);
        lFirst := False;
      end;
    finally
      FreeAndNil(lR);
    end;
  FreeAndNil(pNode.fChildren);
end;

procedure TD2XTreeWriter.WriteDoc(pNode: TD2XTreeDoc);
begin
  WriteElement(pNode);
end;

procedure TD2XTreeWriter.WriteElement(pNode: TD2XTreeElement);
begin
  if pNode.fTag > '' then
  begin
    if pNode.HasChildren then
      try
        WriteElHead(pNode);
        WriteChildren(pNode);
        if pNode.fText > '' then
          WriteElText(pNode);
      finally
        WriteElFoot(pNode);
      end
    else
      if pNode.fText > '' then
        WriteText(pNode)
      else
        if pNode.HasAttributes then
          WriteAttrHead(pNode)
        else
          WriteHead(pNode);
  end
  else
    WriteRawText(pNode);
end;

procedure TD2XTreeWriter.WriteElFoot(pNode: TD2XTreeElement);
begin
  fW.Write('>');
end;

procedure TD2XTreeWriter.WriteElHead(pNode: TD2XTreeElement);
begin
  fW.Write('$');
  fW.Write(pNode.fTag);
  fW.Write('<');
  if HasOption(pNode, toAutoIndent) then
    fW.WriteLine;
  WriteAttributes(pNode);
end;

procedure TD2XTreeWriter.WriteElText(pNode: TD2XTreeElement);
begin
  fW.Write(':');
  fW.Write(pNode.fText);
  fW.Write(';');
  if HasOption(pNode, toAutoIndent) then
    fW.WriteLine;
end;

procedure TD2XTreeWriter.WriteHead(pNode: TD2XTreeElement);
begin
  fW.Write('$');
  fW.Write(pNode.fTag);
end;

procedure TD2XTreeWriter.WriteNode(pNode: TD2XTreeNode);
begin
  if pNode is TD2XTreeDoc then
    WriteDoc(TD2XTreeDoc(pNode))
  else
    if pNode is TD2XTreeElement then
      WriteElement(TD2XTreeElement(pNode))
    else
      raise ETreeWriter.Create('Invalid class in WriteNode: ' + pNode.ClassName);
end;

procedure TD2XTreeWriter.WriteRawText(pNode: TD2XTreeElement);
begin
  fW.Write(pNode.fText);
  if HasOption(pNode, toAutoIndent) then
    fW.WriteLine;
end;

procedure TD2XTreeWriter.WriteText(pNode: TD2XTreeElement);
begin
  if pNode.HasAttributes then
  begin
    WriteElHead(pNode);
    WriteElText(pNode);
    WriteElFoot(pNode);
  end
  else
  begin
    WriteHead(pNode);
    WriteElText(pNode);
  end;
end;

end.
