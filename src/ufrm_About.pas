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
unit ufrm_About;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Types, SysUtils, Classes, Controls, ExtCtrls, Forms, Graphics, jpeg,
  // Dxbx
  uConsts;


type
  Tfrm_About = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  TGraphicHelper = class helper for TGraphic
    procedure LoadFromResourceName(Instance: THandle; const ResName: string);
  end;

var
  frm_About: Tfrm_About;

function GetJPEGResource(const aResourceName: string): TJPEGImage;

implementation

var
  JPEGImage: TJPEGImage = nil;

function GetJPEGResource(const aResourceName: string): TJPEGImage;
begin
  if JPEGImage = nil then
    JPEGImage := TJPEGImage.Create;

  JPEGImage.LoadFromResourceName(MainInstance, aResourceName);
  Result := JPEGImage;
end;


procedure TGraphicHelper.LoadFromResourceName(Instance: THandle; const ResName: string);
var
  Stream: TCustomMemoryStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

{$R *.dfm}

procedure Tfrm_About.FormCreate(Sender: TObject);
var
  JPEGImage: TJPEGImage;
begin
  Caption := 'Dxbx version ' + _DXBX_VERSION;

  JPEGImage := GetJPEGResource('About');
  Self.Width := JPEGImage.Width;
  Self.Height := JPEGImage.Height;
  Image1.SetBounds(0, 0, JPEGImage.Width, JPEGImage.Height);
  Image1.Canvas.Draw(0, 0, JPEGImage);
end;

// TODO : Add a scroller with some informative text to live things up a bit.
// Also, I would like to see the fireflies animate, but that's not really important for now.

initialization

finalization

  FreeAndNil(JPEGImage);

end.
