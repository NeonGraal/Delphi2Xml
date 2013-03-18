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

  TD2XTreeWriter = class;

  TD2XTreeDoc = class;

  TD2XTreeNode = class
  strict private
    fParent: TD2XTreeNode;
    fStream: TStringStream;

  protected
    fWriter: TD2XTreeWriter;
    fTag: string;
    fText: string;
    fDoc: TD2XTreeDoc;

    constructor CreateTag(pTag: string; pParent: TD2XTreeNode); virtual;

  public
    constructor Create;
    destructor Destroy; override;

    function AddChild(pTag: string): TD2XTreeNode; virtual;
    function AddAttribute(pTag: string): TD2XTreeNode; virtual;
    function HasChildren: Boolean; virtual;
    function HasAttributes: Boolean; virtual;
    procedure TrimChildren(pElement: string); virtual;

    //  protected
    function GetStream: TStringStream;

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

  TD2XTreeWriterClass = class of TD2XTreeWriter;

  TD2XTreeDoc = class(TD2XTreeElement)
  private
    fOptions: TD2XTreeOptions;

  public
    constructor CreateDoc(pWriter: TD2XTreeWriterClass);
    destructor Destroy; override;

    function AddChild(pTag: string): TD2XTreeNode; override;

    procedure AddOptions(pOptions: TD2XTreeOptions);
    procedure RemoveOptions(pOptions: TD2XTreeOptions);

  end;

  TD2XTreeWriter = class
  protected type
    TEachProc = reference to procedure(pChild: TD2XTreeNode);

  protected
    procedure WriteAttribute(pNode: TD2XTreeNode; pW: TTextWriter); virtual;
    procedure WriteElement(pNode: TD2XTreeElement; pW: TTextWriter); virtual;
    procedure WriteDoc(pNode: TD2XTreeDoc; pW: TTextWriter); virtual;

    procedure ForeachChild(pNode: TD2XTreeElement; pEach: TEachProc);
    procedure ForeachAttribute(pNode: TD2XTreeElement; pEach: TEachProc);

    function HasOption(pNode: TD2XTreeElement; pOption: TD2XTreeOption): Boolean;

  public
    procedure WriteNode(pNode: TD2XTreeNode; pW: TTextWriter);
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
  fWriter := pWriter.Create;
end;

destructor TD2XTreeDoc.Destroy;
begin
  FreeAndNil(fWriter);

  inherited;
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
  lW: TStreamWriter;
begin
  if not Assigned(fStream) then
    try
      fStream := TStringStream.Create;
      lW := TStreamWriter.Create(fStream);
      fWriter.WriteNode(Self, lW);
    finally
      FreeAndNil(lW);
    end;

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

procedure TD2XTreeWriter.ForeachAttribute(pNode: TD2XTreeElement; pEach: TEachProc);
var
  lNode: TD2XTreeNode;
begin
  if pNode.HasAttributes then
    for lNode in pNode.fAtttributes do
      pEach(lNode);
  FreeAndNil(pNode.fAtttributes);
end;

procedure TD2XTreeWriter.ForeachChild(pNode: TD2XTreeElement; pEach: TEachProc);
var
  lNode: TD2XTreeNode;
begin
  for lNode in pNode.fChildren do
    pEach(lNode);
  FreeAndNil(pNode.fChildren);
end;

function TD2XTreeWriter.HasOption(pNode: TD2XTreeElement; pOption: TD2XTreeOption): Boolean;
begin
  Result := pOption in pNode.fDoc.fOptions;
end;

procedure TD2XTreeWriter.WriteAttribute(pNode: TD2XTreeNode; pW: TTextWriter);
begin
  pW.Write('@');
  pW.Write(pNode.fTag);
  if pNode.fText > '' then
  begin
    pW.Write(':');
    pW.Write(pNode.fText);
  end;
  pW.Write(';');
end;

procedure TD2XTreeWriter.WriteDoc(pNode: TD2XTreeDoc; pW: TTextWriter);
begin
  WriteElement(pNode, pW);
end;

procedure TD2XTreeWriter.WriteElement(pNode: TD2XTreeElement; pW: TTextWriter);
begin
  if pNode.fTag > '' then
  begin
    pW.Write('$');
    pW.Write(pNode.fTag);
    if pNode.HasChildren or pNode.HasAttributes then
    begin
      pW.Write('<');
      if HasOption(pNode, toAutoIndent) then
        pW.WriteLine;
      ForeachAttribute(pNode,
          procedure(pAttr: TD2XTreeNode)
        begin
          if HasOption(pNode, toAutoIndent) then
            pW.Write(' ');
          WriteAttribute(pAttr, pW);
          if HasOption(pNode, toAutoIndent) then
            pW.WriteLine;
        end);
      ForeachChild(pNode,
        procedure(pChild: TD2XTreeNode)
        var
          lR: TStreamReader;
          lS: string;
        begin
          try
            lR := TStreamReader.Create(pChild.GetStream);
            lR.BaseStream.Seek(0, soBeginning);
            while not lR.EndOfStream do
            begin
              lS := lR.ReadLine;
              if HasOption(pNode, toAutoIndent) then
                pW.WriteLine(' ' + lS)
              else
                PW.Write(lS);
            end;
          finally
            FreeAndNil(lR);
          end;
        end);
      pW.Write('>');
    end;
    if pNode.fText > '' then
    begin
      pW.Write(':');
      pW.Write(pNode.fText);
      pW.Write(';');
      if HasOption(pNode, toAutoIndent) then
        pW.WriteLine;
    end;
  end
  else
  begin
    pW.Write(pNode.fText);
    pW.Write(';');
    if HasOption(pNode, toAutoIndent) then
      pW.WriteLine;
  end;
end;

procedure TD2XTreeWriter.WriteNode(pNode: TD2XTreeNode; pW: TTextWriter);
begin
  if pNode is TD2XTreeDoc then
    WriteDoc(TD2XTreeDoc(pNode), pW)
  else
    if pNode is TD2XTreeElement then
      WriteElement(TD2XTreeElement(pNode), pW)
    else
      raise ETreeWriter.Create('Invalid class in WriteNode: ' + pNode.ClassName);
end;

end.
