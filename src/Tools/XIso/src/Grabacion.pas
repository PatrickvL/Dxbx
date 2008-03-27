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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CDROM, WIN32ASPI, ComCtrls, Buttons;

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
    procedure AsignarUnidadesAListas();
  public
    { Public declarations }
  end;

  // Estructuras SCSI
  TUnidades = record
    Nombre: string;
    HA: byte;
    SCSI: byte;
    LUN: byte;
    Grabadora: boolean;
    Letra: char;
    VelocidadActual: integer;
    VelocidadMaxima: integer;
  end;

  TTiempo = record
    Horas: byte;
    Minutos: byte;
    Segundos: byte;
  end;

var
  Form4: TForm4;
  Unidad: TCDROM;
  Unidades: array[0..255] of TUnidades;
  CantidadUnidades: integer;
  Contador: TTiempo;

implementation

{$R *.dfm}

uses Textos;

procedure EscanearSCSI();
var
  i, j, k, l: byte;
  TipoUnidad: TDeviceType;
  InfoUnidad: TDeviceInfo;
  InfoExtra: TCDROMInfo;
begin
  with Unidad do
  begin
    l := 0;
    for i := 0 to 7 do
      for j := 0 to 7 do
        for k := 0 to 7 do
        begin
          TipoUnidad := GetDeviceType(i, j, k);
          if (TipoUnidad <> dt_CDROM) then continue;
          InfoUnidad := Inquiry(i, j, k);
          InfoExtra := Unidad.ModeSense(i, j, k);

          Unidades[l].Nombre := Format('%d:%d:%d %s %s %s', [i, j, k, InfoUnidad.VendorID, InfoUnidad.ProductID, InfoUnidad.Revision]);
          Unidades[l].HA := i;
          Unidades[l].SCSI := j;
          Unidades[l].LUN := k;
          Unidades[l].Letra := #00;
          Unidades[l].Grabadora := InfoExtra.SupportWriteCDR = True;
          l := l + 1;
        end;
  end;
  CantidadUnidades := l;
end;

procedure TForm4.AsignarUnidadesAListas();
var
  i: integer;
begin
  for i := 0 to CantidadUnidades - 1 do
  begin
          //if Unidades[i].Grabadora then
    Form4.cGrabadoras.Items.Add(Unidades[i].Nombre);
  end;
  Form4.cGrabadoras.ItemIndex := 0;
end;

procedure AsignarSCSIID(var HA, SCSI, LUN: byte);
begin
  HA := Unidades[Form4.cGrabadoras.ItemIndex].HA;
  SCSI := Unidades[Form4.cGrabadoras.ItemIndex].SCSI;
  LUN := Unidades[Form4.cGrabadoras.ItemIndex].LUN;
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
    MessageBox(Self.Handle, PChar(rcEngInstalarASPI), nil, MB_OK or MB_ICONWARNING);
  end;
  Fillchar(Contador, sizeof(Contador), 0);
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm4.BotonGrabarClick(Sender: TObject);
var
  HA, SCSI, LUN: byte;
  P: TModeSelectEscribir;
  Imagen: TFileStream;
  total, leido: integer;
  Velocidad: word;
  Buffer: array[0..($1B * 2352) - 1] of byte;
  LBA: integer;
  CDINFO: TCDInfo;
  PISTAINFO: TInfoPista;
begin
  if (eImagenISO.Text = '') or (not FileExists(eImagenISO.Text)) then Exit;
  AsignarSCSIID(HA, SCSI, LUN);
  Imagen := TFileStream.Create(eImagenISO.Text, fmOpenRead);
  Imagen.Seek(0, soFromBeginning);
  if (Imagen.Size mod 2048) <> 0 then
  begin
    MessageBox(Self.Handle, PChar(rcEngGraNoSector2048), nil, MB_OK or MB_ICONWARNING);
    Imagen.Free;
    Exit;
  end;

  CDINFO := Unidad.ReadDiscInformation(HA, SCSI, LUN);
  if CDINFO.EstadoDisco <> ed_Vacio then
  begin
    MessageBox(Self.Handle, PChar(rcEngGraNoDisco), nil, MB_OK or MB_ICONWARNING);
    Imagen.Free;
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
    showmessage('no se puso la velocidad');

  P.Op1 := 1; // Track at once
  P.Op2 := 4;
  P.TipoBloque := 8; // 8 = MODO 1 2048
  P.LinkSize := 0;
  P.IAC := 0;
  P.FormatoSesion := 0; //0 = Modo CDDA o CDROM; $10 = CD-I; $20 = XA
  P.TamPaquete := 0; // Solo para Packet Writing (Only Packet Writing)
  P.DuracionPausa := $96; // 150 sectores= 2 segundos ( 150 sectors= 2 seconds)
  P.MCN := #00 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #32 + #00;
  P.ISRC := #00 + #00 + #32 + #32 + #32 + #32 + #32 + #48 + #48 + #48 + #48 + #48 + #48 + #48 + #00 + #00;
  Fillchar(p.SubHeader, 4, 0);
  P.SubHeader[3] := 8;
  Fillchar(p.VendorSpecific, 4, 0);

  if not Unidad.ModeSelectEscribir(HA, SCSI, LUN, P) then
  begin
          //Showmessage('No se escribio los parametros de escritura.');
    Showmessage('Couldn´t Write Writing Parameters.');
    Imagen.Free;
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
        if not CDROM1.SendCuesheet(1,0,0,pQ,4) then showmessage('No SendCuesheet');    }

  Fillchar(Buffer, sizeof(buffer), 0);

  LBA := 0;
        //LBA := PISTAINFO.SigLBAGrabable + 75*150 + 150;
  Unidad.TimeOut_ExecSCSICommand := 120000;

  Total := LBA + (Imagen.Size div 2048); //LBA + (Imagen.Size div 2048) (Image Size div 2048) ;
  ProgressBar1.Max := Total;
  tContadorGrabacion.Enabled := True;
  while LBA < Total do
  begin
    Leido := Imagen.Read(Buffer, 2048 * 16);
    Leido := Leido div 2048;
    if not Unidad.Write10(HA, SCSI, LUN, LBA, Leido, @Buffer) then
    begin
      Showmessage('Error grabando sector: ' + Inttostr(LBA));
      Break;
    end;
    LBA := LBA + Leido;
    ProgressBar1.StepBy(Leido);
    Application.ProcessMessages;
  end;

  if not Unidad.SynchronizeCache(HA, SCSI, LUN, 0, 0) then
    Showmessage('Sinchronization Error.');
          //Showmessage('Error al sincronizar.');
  Unidad.TimeOut_ExecSCSICommand := -1;
  Unidad.CloseSessionTrack(HA, SCSI, LUN, True, False, 0);
  Imagen.Free;
  while not Unidad.TestUnitReady(HA, SCSI, LUN) do
  begin
          //Caption := 's';
  end;
  tContadorGrabacion.Enabled := False;
  Progressbar1.Position := 0;
  MessageBox(Self.Handle, PCHAR(rcEngGraFinGrabacion), nil, MB_OK or MB_ICONINFORMATION);
  lContador.Caption := '00:00:00';
  Fillchar(Contador, sizeof(Contador), 0);
  BotonGrabar.Enabled := True;
end;

procedure TForm4.FormShow(Sender: TObject);
begin
  EscanearSCSI();
  AsignarUnidadesAListas();
end;

procedure TForm4.SpeedButton1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  eImagenISO.Text := OpenDialog1.Filename;
end;

procedure TForm4.cGrabadorasChange(Sender: TObject);
begin
  BotonGrabar.Enabled := Unidades[Form4.cGrabadoras.ItemIndex].Grabadora = True;
end;

procedure TForm4.tContadorGrabacionTimer(Sender: TObject);
var
  h, m, s: string;
begin
  Inc(Contador.Segundos);
  if Contador.Segundos > 60 then
  begin
    Inc(Contador.Minutos);
    Contador.Segundos := 0;
  end;
  if Contador.Minutos > 60 then
  begin
    Inc(Contador.Horas);
    Contador.Minutos := 0;
  end;

  h := IntToStr(Contador.Horas);
  m := IntToStr(Contador.Minutos);
  s := IntToStr(Contador.Segundos);
  if Contador.Horas < 10 then
    h := '0' + h;
  if Contador.Minutos < 10 then
    m := '0' + m;
  if Contador.Segundos < 10 then
    s := '0' + s;

  lContador.Caption := Format('%s:%s:%s', [h, m, s]);
end;

end.
