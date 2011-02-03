(*
    This file is part of Dxbx - a XBox emulator written in Delphi (ported over from cxbx)
    Copyright (C) 2007 Shadow_tj and other members of the development team.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
unit uDxbxXml;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Classes,
  SysUtils, // IntToStr
  xmldom,
  XMLIntf,
  msxmldom,
  XMLDoc,
  // Dxbx
  uConsts,
  uXbe;

type
  TDxbxXml = class(TDataModule)
    XMLDocument: TXMLDocument;
  public
    procedure CreateXmlXbeDumpAsText(var aText: String; aXbe: TXbe; aFileName: string);
    procedure CreateXmlXbeDump(aFileName: string; aXbe: TXbe);
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
  XbeLibraryVersion: PXbeLibraryVersion;
  i: Integer;
  LibName: string;
  Version: string;
begin
  XMLDocument.Active := False;
  XMLDocument.Active := True;
  XmlRootNode := XMLDocument.AddChild('XBEINFO');

  XML_WriteString(XmlRootNode, 'DumpInfo', DumpToolString);
  XML_WriteString(XmlRootNode, 'FileName', aFileName);
  XML_WriteString(XmlRootNode, 'Title', m_szAsciiTitle);

  XmlRootNode := XmlRootNode.AddChild('XDKVersions');

  if aXbe.m_Header.dwLibraryVersionsAddr > aXbe.m_Header.dwBaseAddr then
  begin
    XbeLibraryVersion := PXbeLibraryVersion(@(aXbe.RawData[aXbe.m_Header.dwLibraryVersionsAddr - aXbe.m_Header.dwBaseAddr]));
    for i := 0 to aXbe.m_Header.dwLibraryVersions - 1 do
    begin
      LibName := string(Copy(XbeLibraryVersion.szName, 1, XBE_LIBRARYNAME_MAXLENGTH));
      Version := IntToStr(XbeLibraryVersion.wMajorVersion) + '.' +
        IntToStr(XbeLibraryVersion.wMinorVersion) + '.' +
        IntToStr(XbeLibraryVersion.wBuildVersion);
      Inc(XbeLibraryVersion);

      XML_WriteString(XmlRootNode, Trim(LibName), Version);
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

procedure TDxbxXml.CreateXmlXbeDumpAsText(var aText: String; aXbe: TXbe; aFileName: string);
var
  XmlRootNode: IXmlNode;
  XbeLibraryVersion: PXbeLibraryVersion;
  i: Integer;
  LibName: string;
  Version: string;
begin
  XMLDocument.Active := False;
  XMLDocument.Active := True;
  XmlRootNode := XMLDocument.AddChild('XBEINFO');

  XML_WriteString(XmlRootNode, 'DumpInfo', DumpToolString);
  XML_WriteString(XmlRootNode, 'FileName', aFileName);
  XML_WriteString(XmlRootNode, 'Title', m_szAsciiTitle);

  XmlRootNode := XmlRootNode.AddChild('XDKVersions');

  if aXbe.m_Header.dwLibraryVersionsAddr > aXbe.m_Header.dwBaseAddr then
  begin
    XbeLibraryVersion := PXbeLibraryVersion(@(aXbe.RawData[aXbe.m_Header.dwLibraryVersionsAddr - aXbe.m_Header.dwBaseAddr]));
    for i := 0 to aXbe.m_Header.dwLibraryVersions - 1 do
    begin
      LibName := string(Copy(XbeLibraryVersion.szName, 1, XBE_LIBRARYNAME_MAXLENGTH));
      Version := IntToStr(XbeLibraryVersion.wMajorVersion) + '.' +
        IntToStr(XbeLibraryVersion.wMinorVersion) + '.' +
        IntToStr(XbeLibraryVersion.wBuildVersion);
      Inc(XbeLibraryVersion);

      XML_WriteString(XmlRootNode, Trim(LibName), Version);
    end;
  end;

  XMLDocument.SaveToXML(aText);
end;

end.
