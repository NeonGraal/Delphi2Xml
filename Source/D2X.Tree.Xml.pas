unit D2X.Tree.Xml;

interface

uses
  D2X.Tree,
  System.Classes;

type
  TD2XXmlWriter = class(TD2XTreeWriter)
  protected
    procedure WriteAttribute(pNode: TD2XTreeNode; pW: TTextWriter); override;
    procedure WriteElement(pNode: TD2XTreeElement; pW: TTextWriter); override;
    procedure WriteDoc(pNode: TD2XTreeDoc; pW: TTextWriter); override;

  end;

implementation

uses
  Data.Cloud.CloudAPI,
  System.SysUtils;

{ TD2XXmlWriter }

procedure TD2XXmlWriter.WriteAttribute(pNode: TD2XTreeNode; pW: TTextWriter);
begin
  pW.Write(' ');
  pW.Write(pNode.LocalName);
  pW.Write('="');
  pW.Write(XmlEscape(pNode.Text));
  pW.Write('"');
end;

procedure TD2XXmlWriter.WriteDoc(pNode: TD2XTreeDoc; pW: TTextWriter);
begin
  pW.WriteLine('<?xml version="1.0"?>');
  inherited;
end;

procedure TD2XXmlWriter.WriteElement(pNode: TD2XTreeElement; pW: TTextWriter);
begin
  if pNode.LocalName > '' then
  begin
    pW.Write('<');
    pW.Write(pNode.LocalName);
    ForeachAttribute(pNode,
        procedure(pAttr: TD2XTreeNode)
      begin
        WriteAttribute(pAttr, pW);
      end);

    if pNode.HasChildren or (pNode.Text > '') then
    begin
      pW.Write('>');
      if pNode.Text > '' then
        pW.Write(pNode.Text)
      else
      begin
        if HasOption(pNode, toAutoIndent) then
        begin
          pW.WriteLine;
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
                  pW.WriteLine('  ' + lS);
                end;
              finally
                FreeAndNil(lR);
              end;
            end);
        end
        else
          ForeachChild(pNode,
            procedure(pChild: TD2XTreeNode)
            begin
              pW.Write(pChild.GetStream.DataString);
            end);
      end;
      pW.Write('</');
      pW.Write(pNode.LocalName);
      pW.Write('>');
    end
    else
      pW.Write(' />');
    if HasOption(pNode, toAutoIndent) then
      pW.WriteLine;
  end
  else
    pW.Write(pNode.Text);
end;

end.
