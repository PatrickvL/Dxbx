unit uDxbxXml;

interface

uses
  SysUtils, Classes, xmldom, XMLIntf, msxmldom, XMLDoc, uXbe;

type
  TDxbxXml = class(TDataModule)
    XMLDocument: TXMLDocument;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure CreateXmlXbeDump(aFileName: string; aXbe : TXbe);

  end;

var
  DxbxXml: TDxbxXml;


procedure XML_WriteString(const aXMLNode: IXMLNode; const aElementName: string; const aString: string);
function XML_ReadString(const aXMLNode: IXMLNode; const aElementName: string): string;


implementation

{$R *.dfm}

{ TDxbxXml }

procedure TDxbxXml.CreateXmlXbeDump(aFileName: string; aXbe: TXbe);
var
  XmlRootNode: IXmlNode;
  lIndex: Integer;
  Version: string;
begin
  XMLDocument.Active := False;
  XMLDocument.Active := True;
  XmlRootNode := XMLDocument.AddChild('XDKINFO');

  XML_WriteString(XmlRootNode, 'Name', m_szAsciiTitle);

  for lIndex := 0 to aXbe.m_Header.dwLibraryVersions - 1 do
  begin
    Version := IntToStr(aXbe.m_LibraryVersion[lIndex].wMajorVersion) + '.' +
      IntToStr(aXbe.m_LibraryVersion[lIndex].wMinorVersion) + '.' +
      IntToStr(aXbe.m_LibraryVersion[lIndex].wBuildVersion);

    case lIndex of
      0: XML_WriteString(XmlRootNode, 'XAPILIB', Version);
      1: XML_WriteString(XmlRootNode, 'XBOXKRNL', Version);
      2: XML_WriteString(XmlRootNode, 'LIBCMT', Version);
      3: XML_WriteString(XmlRootNode, 'D3D8', Version);
      4: XML_WriteString(XmlRootNode, 'XGRAPHC', Version);
      5: XML_WriteString(XmlRootNode, 'DSOUND', Version);
      6: XML_WriteString(XmlRootNode, 'XMV', Version);
    end;
  end;

  XMLDocument.SaveToFile(aFileName);
end;

function XML_ReadString(const aXMLNode: IXMLNode;
  const aElementName: string): string;
var
  SubNode: IXMLNode;
begin
  SubNode := aXMLNode.ChildNodes.FindNode(aElementName);
  if Assigned(SubNode) then
    Result := SubNode.Text
  else
    Result := '';
end;

procedure XML_WriteString(const aXMLNode: IXMLNode; const aElementName,
  aString: string);
var
  SubNode: IXMLNode;
begin
  SubNode := aXMLNode.AddChild(aElementName);
  SubNode.Text := aString;
end;

end.
