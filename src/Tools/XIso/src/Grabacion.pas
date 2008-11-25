{
   xISO
   Copyright 1984, 1986, 1989, 1992, 2000, 2001, 2002
   Free Software Foundation, Inc.

   This file is part of xISO, made it by Yursoft.com

   xISO is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Bison is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Bison; see the file COPYING.  If not, write to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.
}


unit Grabacion;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  // XIso
  CDROM, WIN32ASPI,
  TextConsts;


type
  TForm4 = class(TForm)
    Panel1: TPanel;
    BotonGrabar: TButton;
    cGrabadoras: TComboBox;
    Label1: TLabel;
    Bevel1: TBevel;
    Button2: TButton;
    Label2: TLabel;
    ProgressBar1: TProgressBar;
    eImagenISO: TEdit;
    Label3: TLabel;
    SpeedButton1: TSpeedButton;
    Bevel2: TBevel;
    cVelocidad: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ComboBox1: TComboBox;
    Bevel3: TBevel;
    Label7: TLabel;
    Label8: TLabel;
    Bevel4: TBevel;
    OpenDialog1: TOpenDialog;
    lContador: TLabel;
    tContadorGrabacion: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure BotonGrabarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure cGrabadorasChange(Sender: TObject);
    procedure tContadorGrabacionTimer(Sender: TObject);
  private
    procedure AssignDevicesToList();
  end;

  // Estructuras SCSI
  TDevice = record
    Nombre: string;
    HA: Byte;
    SCSI: Byte;
    LUN: Byte;
    Grabadora: Boolean;
    Letra: Char;
    VelocidadActual: Integer;
    VelocidadMaxima: Integer;
  end;

  TTiempo = record
    Hours: Byte;
    Minutes: Byte;
    Seconds: Byte;
  end;

var
  Form4: TForm4;
  Unidad: TCDROM;
  Devices: array[0..255] of TDevice;
  NumberOfDevices: Integer;
  Contador: TTiempo;

implementation

{$R *.dfm}

procedure EscanearSCSI();
var
  i, j, k, l: Byte;
  DeviceType: TDeviceType;
  DeviceInfo: TDeviceInfo;
  InfoExtra: TCDROMInfo;
begin
  with Unidad do
  begin
    l := 0;
    for i := 0 to 7 do
      for j := 0 to 7 do
        for k := 0 to 7 do
        begin
          DeviceType := GetDeviceType(i, j, k);
          if (DeviceType <> dt_CDROM) then
            Continue;
            
          DeviceInfo := Inquiry(i, j, k);
          InfoExtra := Unidad.ModeSense(i, j, k);

          Devices[l].Nombre := Format('%d:%d:%d %s %s %s', [i, j, k, DeviceInfo.VendorID, DeviceInfo.ProductID, DeviceInfo.Revision]);
          Devices[l].HA := i;
          Devices[l].SCSI := j;
          Devices[l].LUN := k;
          Devices[l].Letra := #00;
          Devices[l].Grabadora := InfoExtra.SupportWriteCDR = True;
          l := l + 1;
        end;
  end;
  
  NumberOfDevices := l;
end;

procedure TForm4.AssignDevicesToList();
var
  i: Integer;
begin
  for i := 0 to NumberOfDevices - 1 do
  begin
    // if Devices[i].Grabadora then
      Form4.cGrabadoras.Items.Add(Devices[i].Nombre);
  end;

  Form4.cGrabadoras.ItemIndex := 0;
end;

procedure AsignarSCSIID(var HA, SCSI, LUN: Byte);
begin
  HA := Devices[Form4.cGrabadoras.ItemIndex].HA;
  SCSI := Devices[Form4.cGrabadoras.ItemIndex].SCSI;
  LUN := Devices[Form4.cGrabadoras.ItemIndex].LUN;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  Unidad := TCDROM.Create(Self);
  if (Unidad <> nil) and (Unidad.ASPISupport) then
  begin
    Unidad.ErrorEnable := True;
    Unidad.TimeOut_ExecSCSICommand := 4 * 60 * 1000;
    Unidad.TimeOut_GetDeviceType := 5000;
    Unidad.TimeOut_GetHostAdapterInquiry := 5000;
    Unidad.TimeOut_ServiceAbort := 1000;
    Unidad.TimeOut_ResetDevice := 1000;
    Unidad.TimeOut_GetDiskInfo := 5000;
    Unidad.TimeOut_RescanBus := 1000;
  end
  else
  begin
    MessageBox(Self.Handle, PChar(SInstalarASPI), nil, MB_OK or MB_ICONWARNING);
  end;

  FillChar(Contador, SizeOf(Contador), 0);
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm4.BotonGrabarClick(Sender: TObject);
var
  HA, SCSI, LUN: Byte;
  P: TModeSelectEscribir;
  ISOImageFile: TFileStream;
  total, leido: Integer;
  Velocidad: Word;
  Buffer: array[0..($1B * 2352) - 1] of Byte;
  LBA: Integer;
  CDINFO: TCDInfo;
  PISTAINFO: TInfoPista;
begin
  if (eImagenISO.Text = '') or (not FileExists(eImagenISO.Text)) then
    Exit;
    
  AsignarSCSIID(HA, SCSI, LUN);
  ISOImageFile := TFileStream.Create(eImagenISO.Text, fmOpenRead);
  try
    ISOImageFile.Seek(0, soFromBeginning);
    if (ISOImageFile.Size mod 2048) <> 0 then
    begin
      MessageBox(Self.Handle, PChar(SGraNoSector2048), nil, MB_OK or MB_ICONWARNING);
      Exit;
    end;

    CDINFO := Unidad.ReadDiscInformation(HA, SCSI, LUN);
    if CDINFO.EstadoDisco <> ed_Vacio then
    begin
      MessageBox(Self.Handle, PChar(SGraNoDisco), nil, MB_OK or MB_ICONWARNING);
      Exit;
    end;

    BotonGrabar.Enabled := False;

    PISTAINFO := Unidad.ReadTrackInformation(HA, SCSI, LUN, 1, 1);

    if cVelocidad.ItemIndex = 0 then
      Velocidad := $FFFF
    else
    begin
      Velocidad := StrToInt(Copy(cVelocidad.Text, 1, Length(cVelocidad.Text) - 1));
      Velocidad := (Velocidad shr 8) or (Velocidad shl 8);
    end;

    if not Unidad.SetCDSpeed(HA, SCSI, LUN, $FFFF, Velocidad) then
      ShowMessage('no se puso la velocidad');

    P.Op1 := 1; // Track at once
    P.Op2 := 4;
    P.TipoBloque := 8; // 8 = MODO 1 2048
    P.LinkSize := 0;
    P.IAC := 0;
    P.FormatoSesion := 0; //0 = Modo CDDA o CDROM; $10 = CD-I; $20 = XA
    P.TamPaquete := 0; // Solo para Packet Writing (Only Packet Writing)
    P.DuracionPausa := $96; // 150 sectores= 2 Seconds ( 150 sectors= 2 seconds)
    P.MCN := #00 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #00;
    P.ISRC := #00 + #00 + #32 + #32 + #32 + #32 + #32 + #48 + #48 + #48 + #48 + #48 + #48 + #48 + #00 + #00;
    FillChar(p.SubHeader, 4, 0);
    P.SubHeader[3] := 8;
    FillChar(p.VendorSpecific, 4, 0);

    if not Unidad.ModeSelectEscribir(HA, SCSI, LUN, P) then
    begin
            //ShowMessage('No se escribio los parametros de escritura.');
      ShowMessage('Couldn´t Write Writing Parameters.');
      Exit;
    end;

          //CDROM1.ReadDiscInformation(0,0,0);
          //CDROM1.ReadTrackInformation(0,0,0,$FF,1);

         { t := CDROm1.LBA2MSF(750);
          Q[0]:=$41; Q[1]:=0; Q[2]:=0; Q[3]:=$14; Q[4]:=0; Q[5]:=0; Q[6]:=0; Q[7]:=0;
          Q[8]:=$41; Q[9]:=1; Q[10]:=0; Q[11]:=$11; Q[12]:=0; Q[13]:=0; Q[14]:=0; Q[15]:=0;
          Q[16]:=$41; Q[17]:=1; Q[18]:=1; Q[19]:=$11; Q[20]:=0; Q[21]:=0; Q[22]:=2; Q[23]:=0;
          Q[24]:=$41; Q[25]:=$AA; Q[26]:=1; Q[27]:=$14; Q[28]:=0; Q[29]:=t.Minute; Q[30]:=t.Second+2; Q[31]:=t.Frame;

          pQ := @Q;
          if not CDROM1.SendCuesheet(1,0,0,pQ,4) then ShowMessage('No SendCuesheet');    }

    FillChar(Buffer, SizeOf(buffer), 0);

    LBA := 0;
          //LBA := PISTAINFO.SigLBAGrabable + 75*150 + 150;
    Unidad.TimeOut_ExecSCSICommand := 120000;

    Total := LBA + (ISOImageFile.Size div 2048); //LBA + (ISOImageFile.Size div 2048) (Image Size div 2048) ;
    ProgressBar1.Max := Total;
    tContadorGrabacion.Enabled := True;
    while LBA < Total do
    begin
      Leido := ISOImageFile.Read(Buffer, 2048 * 16);
      Leido := Leido div 2048;
      if not Unidad.Write10(HA, SCSI, LUN, LBA, Leido, @Buffer) then
      begin
        ShowMessage('Error grabando sector: ' + Inttostr(LBA));
        Break;
      end;
      LBA := LBA + Leido;
      ProgressBar1.StepBy(Leido);
      Application.ProcessMessages;
    end;
  finally
    FreeAndNil(ISOImageFile);
  end;

  if not Unidad.SynchronizeCache(HA, SCSI, LUN, 0, 0) then
    ShowMessage('Sinchronization Error.');
    //ShowMessage('Error al sincronizar.');

  Unidad.TimeOut_ExecSCSICommand := -1;
  Unidad.CloseSessionTrack(HA, SCSI, LUN, True, False, 0);

  while not Unidad.TestUnitReady(HA, SCSI, LUN) do
  begin
    //Caption := 's';
  end;

  tContadorGrabacion.Enabled := False;
  Progressbar1.Position := 0;
  MessageBox(Self.Handle, PCHAR(SGraFinGrabacion), nil, MB_OK or MB_ICONINFORMATION);
  lContador.Caption := '00:00:00';
  FillChar(Contador, SizeOf(Contador), 0);
  BotonGrabar.Enabled := True;
end;

procedure TForm4.FormShow(Sender: TObject);
begin
  EscanearSCSI();
  AssignDevicesToList();
end;

procedure TForm4.SpeedButton1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then
    Exit;

  eImagenISO.Text := OpenDialog1.FileName;
end;

procedure TForm4.cGrabadorasChange(Sender: TObject);
begin
  BotonGrabar.Enabled := Devices[Form4.cGrabadoras.ItemIndex].Grabadora = True;
end;

procedure TForm4.tContadorGrabacionTimer(Sender: TObject);
begin
  Inc(Contador.Seconds);
  if Contador.Seconds > 60 then
  begin
    Inc(Contador.Minutes);
    Contador.Seconds := 0;
  end;
  if Contador.Minutes > 60 then
  begin
    Inc(Contador.Hours);
    Contador.Minutes := 0;
  end;

  lContador.Caption := Format('%.2d:%.2d:%.2d', [Contador.Hours, Contador.Minutes, Contador.Seconds]);
end;

end.
