unit D2X.Tree.Json;

interface

uses
  D2X.Tree,
  System.Classes;

type
  TD2XJsonWriter = class(TD2XTreeWriter)
  protected
    procedure WriteAttribute(pNode: TD2XTreeNode; pW: TTextWriter); override;
    procedure WriteElement(pNode: TD2XTreeElement; pW: TTextWriter); override;
    //    procedure WriteDoc(pNode: TD2XTreeDoc; pW: TTextWriter); override;

  end;

implementation

uses
  Data.Cloud.CloudAPI,
  System.SysUtils;

{ TD2XJsonWriter }

procedure TD2XJsonWriter.WriteAttribute(pNode: TD2XTreeNode; pW: TTextWriter);
begin
  pW.Write('_');
  pW.Write(pNode.LocalName);
  pW.Write(':"');
  //  pW.Write(JsonEscape(pNode.Text));
  pW.Write(pNode.Text);
  pW.Write('"');
end;

type
  TProcRef = reference to procedure;

procedure TD2XJsonWriter.WriteElement(pNode: TD2XTreeElement; pW: TTextWriter);
var
  lS: string;
  lWS: TProcRef;

begin
  if pNode.LocalName > '' then
  begin
    pW.Write(pNode.LocalName);
    pW.Write(':');
    if pNode.HasChildren or pNode.HasAttributes then
    begin
      pW.Write('{');
      lWS := procedure
        begin
          if HasOption(pNode, toAutoIndent) then
            pW.WriteLine;
          pW.Write(lS);
          lS := ',';
        end;
      if HasOption(pNode, toAutoIndent) then
        lS := '  '
      else
        lS := '';
      ForeachAttribute(pNode,
          procedure(pAttr: TD2XTreeNode)
        begin
          lWS;
          WriteAttribute(pAttr, pW);
        end);

      if pNode.Text > '' then
      begin
        lWS;
        pW.Write('_:"');
        pW.Write(pNode.Text);
        pW.Write('"');
      end;
      if HasOption(pNode, toAutoIndent) then
      begin
        pW.WriteLine;
        ForeachChild(pNode,
          procedure(pChild: TD2XTreeNode)
          var
            lR: TStreamReader;
            lS: string;
          begin
            lWS;
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
            lWS;
            pW.Write(pChild.GetStream.DataString);
          end);
      pW.Write('}');
      lWS := nil;
    end
    else
    begin
      pW.Write('"');
      pW.Write(pNode.Text);
      pW.Write('"');
    end;
    if HasOption(pNode, toAutoIndent) then
      pW.WriteLine;
  end
  else
    pW.Write(pNode.Text);
end;

end.
