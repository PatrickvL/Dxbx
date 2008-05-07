unit ufrm_VideoConfig;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  //
  DirectDraw;

type
  Tfrm_VideoConfig = class(TForm)
    GroupBox1: TGroupBox;
    lbl_DisplayAdapter: TLabel;
    edt_DisplayAdapter: TComboBox;
    lbl_Direct3DDevice: TLabel;
    edt_Direct3dDevice: TComboBox;
    lbl_VideoResolution: TLabel;
    edt_VideoResolution: TComboBox;
    btn_Accept: TButton;
    btn_Cancel: TButton;
    lbl_OtherOptions: TLabel;
    chk_FullScreen: TCheckBox;
    chk_VSync: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboReadOnly(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FDirectDraw: IDirectDraw7;
  public
    { Public declarations }
  end;

var
  frm_VideoConfig: Tfrm_VideoConfig;

implementation

{$R *.DFM}

//------------------------------------------------------------------------------

function EnumDevices(lpGUID: PGUID; lpDriverDescription,
  lpDriverName: PAnsiChar; lpContext: Pointer; Monitor: HMonitor): Bool; stdcall;
begin
  TStringList(lpContext).Add(lpDriverDescription);
  Result := True;
end; // EnumDevices


//------------------------------------------------------------------------------

function EnumModeusCallBack(const lpDDSurfaceDesc: TDDSurfaceDesc2;
  lpContext: Pointer): HResult; stdcall;
begin
  TStringList(lpContext).Add(IntToStr(lpDDSurfaceDesc.dwWidth) + ' X ' +
    IntToStr(lpDDSurfaceDesc.dwHeight) + ', ' +
    IntToStr(lpDDSurfaceDesc.ddpfPixelFormat.dwRGBBitCount) +
    ' bits/pixel');
  Result := DDENUMRET_OK;
end; // EnumModeusCallBack

//------------------------------------------------------------------------------

procedure Tfrm_VideoConfig.FormCreate(Sender: TObject);
var
  tempDirectDraw: IDirectDraw;
begin
  DirectDrawEnumerateEx(EnumDevices, edt_DisplayAdapter.Items, 0);
  edt_DisplayAdapter.ItemIndex := 0;

  DirectDrawCreate(nil, tempDirectDraw, FDirectDraw);
  try
    tempDirectDraw.QueryInterface(IID_IDIRECTDRAW7, FDirectDraw);
  finally
    tempDirectDraw := nil;
  end;
  FDirectDraw.EnumDisplayModes(0, nil, edt_VideoResolution.Items, EnumModeusCallBack);

  edt_VideoResolution.ItemIndex := edt_VideoResolution.Items.Count - 1;
end; // Tfrm_VideoConfig

//------------------------------------------------------------------------------

procedure Tfrm_VideoConfig.ComboReadOnly(Sender: TObject; var Key: Char);
begin
  Key := #0;
end; // Tfrm_VideoConfig.ComboReadOnly

//------------------------------------------------------------------------------

end.
