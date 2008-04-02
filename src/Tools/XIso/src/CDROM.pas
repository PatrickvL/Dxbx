//------------------------------------------------------------------------------
//  title : CD-ROM Device Utility Component for Delphi 4
//
//  author: H.Gotou(E-mail: hgotou@ibm.net)
//  date  : Jun 30,1999
//  rev.  : 26.06.2001 por Yursoft  (yursoft@yursoft.com)
//------------------------------------------------------------------------------
//  note  : 1)This component requires installing Win32ASPI.pas and wnaspi32.dll.
//------------------------------------------------------------------------------
//  history :
//    1.00.00 - May 05,1999
//          1) 1st release.
//   26.06.01  Añadido opcion de elegir tipo de sector y subcanal
//             Introducidas nuevos comandos y mejorado algunos.
//             Traducidos la mayoria de textos y comentarios.      
//
//-----------------------------------------------------------------------------
//
//  más notas: 25-01-2003 - Publicación
//
//----------------------------------------------------------------------------

unit CDROM;

interface

uses
  Windows, SysUtils, Classes, MMSystem, Win32ASPI;

const
//------------------------------------------------
// Definicion de constantes
//------------------------------------------------
//---------------
//   Constantes
//---------------
//Constantes Dispositivo
  Max_Devices     = 256;  //Numero maximo de dispositivos
//Constantes CD-ROM
  ReadIn_Track    = $00;  //Pista Read-In
  ReadOut_Track   = $aa;  //Pista Read-Out
  FrameByte       = 2352;
//---------------
//   OpCodes (Escritos como SDK Adaptec)
//---------------
//OpCodes comunes
  SCSI_TestUnitReady      = $00;  //Prueba de preparado la Unidad
  SCSI_RequestSence       = $03;  //Request Sense
  SCSI_Inquiry            = $12;  //SCSI Inquiry
  SCSI_ModeSelect         = $55;  //Mode Select (10)
  SCSI_ModeSense          = $5a;  //Mode Sense
  SCSI_GetConfiguration   = $46;  //Obtiene las diferentes configuraciones de la unidad
  SCSI_HayCD              = $00;  //Hay CD metido o no
//OpCodes CD-ROM
  SCSI_MediumRemoval      = $1e;  //Prohibicion/Permiso de sacar medio
  SCSI_ReadSubchannel     = $42;  //Leer Subcanal
  SCSI_ReadTOC            = $43;  //Leer TOC
  SCSI_ReadATIP           = $43;  //Leer ATIP
  SCSI_Seek10             = $2b;  //Posiciona el trasductor (lente) en el LBA indicado
  SCSI_Seek6              = $0b;  //Posiciona el trasductor (lente) en el LBA indicado
  SCSI_ReadCDMSF          = $b9;  //Leer CD (MSF)
  SCSI_ReadCD             = $be;  //Leer CD (LBA)
  SCSI_Read10             = $28;  //Read 10
  SCSI_Read12             = $a8;  //Read 12
  SCSI_ReadCapacity       = $25;  //Devuelve la capacidad del medio
  SCSI_SetReadAhead       = $a7;  //Lee sectores al cache
  SCSI_ReadDiscInformation= $51;  //Lee la informacion del CD
  SCSI_ReadTrackInformation = $52;//Devuelve las caracteristicas de una pista
  SCSI_SetLimits          = $33;  //Establece los limites LBA donde va operar los siguientes comandos
  SCSI_SendCuesheet       = $5d;  //Envia la informacion relativa al TOC del CD, para grabarlo.
  SCSI_Write10            = $2a;  //Grabar sector (10)
  SCSI_SynchronizeCache   = $35;  //Sincroniza la cache para que todos los datos de esta sean grabados
  SCSI_CloseSessionTrack  = $5b;  //Cierra sesion/pista
//OpCodes CD Audio
  SCSI_PlayAudioMSF       = $47;  //Reproduce Audio segun MSF
  SCSI_PlayAudioTrack     = $48;  //Reproduce Pista de Audio por Indice
  SCSI_PauseResume        = $4b;  //Pausa/Resumir
  SCSI_PlayAudio          = $a5;  //Play Audio 12
//OpCodes Opcionales de CD-ROM
  SCSI_StartStopUnit      = $1b;  //Para/Enciende Unidad
  SCSI_SetCDSpeed         = $bb;  //Establece Velocidad CDRom
  SCSI_LoadUnloadCD       = $a6;  //Abre o cierra el cambiador (bandeja)
  SCSI_SendEvent          = $5d;  //Envia evento mecanico
//OpCodes Grabacion de CD-ROM
  SCSI_Blank              = $a1;  //Permite borrar un CDRW

//-------------------------
//* Funciones aun No soportadas
//-------------------------
//SCSI Common Functions
  SCSI_ModeSelect6        = $15;  //Mode Select (not supported)
  SCSI_ModeSense6         = $1a;  //Mode Sense (6)  
  SCSI_SendDiagnostic     = $1d;  //Send Diagnostic (not supported)
//CD-ROM Functions
  SCSI_RecordedCapacity   = $25;  //Read CD Recorded Capacity (not supported)
  SCSI_ReadHeader         = $44;  //Read Header (not supported)
  SCSI_MechanismStatus    = $bd;  //Mechanism Status (not supported)
//Audio CD Functions
  SCSI_PlayAudio10        = $45;  //Play Audio 10 (not supported)
//CD-ROM Optional Functions
  SCSI_ReadMasterCue      = $59;  //Read Master Cue (not supported)
  SCSI_ReadBufferCapacity = $5c;  //Read Buffer Capacity (not supported)
  SCSI_PlayCD             = $bc;  //Play CD (not supported)
//------------------------------------------------

type
//------------------------------------------------
// Clase Error
//------------------------------------------------
  ECDROMError = class(EWin32ASPIError);
//------------------------------------------------
  TTipoSubcanal=  (sb_Ninguno=0,
                   sb_RAW=1,
                   sb_Q=2,
                   sb_PW=4);
                   
  TBorradoCDRW =  (tb_Completo,
                    tb_Rapido,
                    tb_Pista,
                    tb_DesReservarPista,
                    tb_TrozoPista,
                    tb_AbrirUltSesion,
                    tb_BorrarSesion);
                    
  TDeviceStatus =  (ds_Connected,
                    ds_NotConnected,
                    ds_NotSupported,
                    ds_Unknown,
                    ds_Reserved);
  {TDeviceType =    (dt_DirectAccess,
                    dt_SequencialAccess,
                    dt_Printer,
                    dt_Processor,
                    dt_WriteOnceDisc,
                    dt_CDROM,
                    dt_Scanner,
                    dt_OpticalMemory,
                    dt_MediaChangerDrive,
                    dt_Communication,
                    dt_Unknown,
                    dt_Reserved);    }
  TDataFormat  =   (df_CCS,
                    df_SCSI1,
                    df_SCSI2,
                    df_Unknown,
                    df_Reserved);
  TDeviceInfo = record
    HA_ID:        Byte;
    SCSI_ID:      Byte;
    LUN:          Byte;
    DeviceStatus: TDeviceStatus;  //Target Status
    DeviceType:   TDeviceType;    //Target Device Type
    DataFormat:   TDataFormat;    //Response Data Format (SCSI version)
    ISOVersion:   Byte;           //Version ISO
    ECMAVersion:  Byte;           //Version ECMA
    ATAPIVersion: Byte;           //Version ATAPI
    ANSIVersion:  Byte;           //Version ANSI
    Removable:    Boolean;        //Removable Media Using
    AENC:         Boolean;        //AEN function Support
    TrmlOP:       Boolean;        //Terminate I/O Process Message Support
    RelAdr:       Boolean;        //Relative Block Address function Support
    WBus32:       Boolean;        //32bit Bus Support
    WBus16:       Boolean;        //16bit Bus Support
    SyncTx:       Boolean;        //Synchronous Transfer Support
    Linked:       Boolean;        //Linked Command Support
    CmdQue:       Boolean;        //Command Queue with Tag Support
    SftRst:       Boolean;        //Software Reset Condition Support
    VendorID:     string;         //Vendor Identifier
    ProductID:    string;         //Product Identifier
    Revision:     string;         //Product Revision
  end;
  TMechanismType =  ( mt_Caddy,               //Caddy type loading mechanism
                      mt_Tray,                //Tray type loading mechanism
                      mt_PopUp,               //Pop-up type loading mechanism
                      mt_DiscChanger,         //Changer with individually changeable discs
                      mt_CartridgeChanger,    //Changer with cartridge Mechanism
                      mt_Reserved,
                      mt_Unknown
                    );
  TCDROMInfo = record
    LoadingMechanismType    : TMechanismType; //Tipo de mecanismo de carga
    DigitalPort1            : Boolean;        //Support DigitalPort1(IEC958)
    DigitalPort2            : Boolean;        //Support DigitalPort2(IEC958)
    MaxReadSpeed            : Word;           //Maximun Read Transfer Rate (kB/sec)
    CurrentReadSpeed        : Word;           //Current Read Transfer Rate (kB/sec)
    MaxWriteSpeed           : Word;           //Maximun Write Transfer Rate (kB/sec)
    CurrentWriteSpeed       : Word;           //Current Write Transfer Rate (kB/sec)
    DataBufferSize          : Word;           //Buffer Size of Data Stream
    ChannelVolumeControl    : Boolean;        //Support Control Independent Channel Volume
    ChannelMuteControl      : Boolean;        //Support Control Independent Channel Mute
    VolumeLevels            : Word;           //Supported Volume Levels Value
    BCKpolarity             : Boolean;        //Bit Clock Polarity (true:positive/false:negative)
    LRCKform                : Boolean;        //L/R Clock Format (true:High=LeftChannel/false:High=RightChannel)
    LSBfirst                : Boolean;        //Data Format (true:LSB first/false:MSB first)
    BCKlength               : Byte;           //Bit Clock Length per Channel
    SupportWriteCDR         : Boolean;        //Support Write CD-R disc
    SupportWriteCDRW        : Boolean;        //Support Write CD-RW disc
    SupportTestWrite        : Boolean;        //Support Test Write
    SupportReadCDR          : Boolean;        //Support Read CD-R disc
    SupportReadCDRW         : Boolean;        //Support Read CD-RW disc
    SupportMethod2          : Boolean;        //Support Read Packet Wrote Disc
    SupportMode2Form1       : Boolean;        //Support Read Mode2 Form1 disc
    SupportMode2Form2       : Boolean;        //Support Read Mode2 Form2 disc
    SupportMultiSession     : Boolean;        //Support Read MultiSession disc
    SupportReadUPC          : Boolean;        //Support Read UPC/EAN code
    SupportReadISRC         : Boolean;        //Support Read ISRC code
    SupportReadBarCode      : Boolean;        //Support Read BarCode ID-code
    SoftwareSlotSelection   : Boolean;        //Support Load/Unload command behavior of empty slot
    SupportChangerDiscReport: Boolean;        //Support Changer Report Disc Present by Mechanism Status command
    SupportEject            : Boolean;        //Support Eject command
    SupportLock             : Boolean;        //Support Lock Media by Prevent/Allow command
    LockStatus              : Boolean;        //Current Lock Status (true:prevent/false:allow)
    PreventJumper           : Boolean;        //Prevent/Allow Jumper setting (true:prevent/false:allow)
    SupportC2pointer        : Boolean;        //Support Read CD with C2 Pointer command
    SupportReadCDDA         : Boolean;        //Support Read CDDA data
    AccurateCDDA            : Boolean;        //Support Repeated Read CD command (Assured Position)
    SupportReadSubcode      : Boolean;        //Support Transfer Subcode Data Block
    SubcodeECC              : Boolean;        //Enable Error Correction of Subcode
    EnablePlayAudio         : Boolean;        //Enable issue PlayAudio/ReadSubcode command
    EnableComposite         : Boolean;        //Enable Trasfer Mixed Audio and Video Data Stream
  end;
  TMSF = record
    Minute  : Byte;
    Second  : Byte;
    Frame   : Byte;
  end;
  TISRCcode = record
    CountryCode   : string;     //Country Code
    VendorCode    : string;     //Owner Code
    RecordingYear : string;     //Recording Year
    SerialNumber  : string;     //Serial Number
  end;
  TAudioStatus = (as_NoSupport, as_Play, as_Pause, as_Stop, as_ErrorHalt, as_NoReport, as_Unknown);
  TSubChannel = record
    ADR           : Byte;         //Sub-channel(Q) Information Type
    AudioStatus   : TAudioStatus; //Audio Playing Status
    Track         : Byte;         //Current Track
    Index         : Byte;         //Current Index
    UPCcode       : string;       //UPC/EAN code
    ISRCcode      : TISRCcode;    //ISRC code
    ATime         : TMSF;         //Absolute Address
    PTime         : TMSF;         //Relative Address
    SubRAW        : array[0..11] of byte; // Devuelve el subcanal tal cual
  end;
  TSesionInfo= record
    Sesion        : Byte;
    ADR           : Byte;
    DigitalCopy   : Boolean;    //Copy Control (true:permitted/false:prohibited)
    AudioData     : Boolean;    //Contents Attribute (true:Audio/false:Data)
    AudioChannel  : Byte;       //Channel of Audio
    Emphasis      : Boolean;    //Pre-Emphasis (true:emphasized/false:no emphasized)
    ISRCcode      : TISRCcode;  //ISRC code
    TNO           : Byte;       // TrackNO
    Point         : Byte;
    AMin          : Byte;
    ASec          : Byte;
    AFrame        : Byte;
    ALBA          : Longword;

    HOUR          : Byte;
    PHOUR         : Byte;

    PMIN          : Byte;
    PSEC          : Byte;
    PFRAME        : Byte;
    PLBA          : Longword;
  end;
  TCCD = record
    PistaInicial  : byte;
    PistaFinal    : byte;
    Pista         : byte;
    AddressMSF    : TMSF;
    LBA           : longword; 
  end;
  TTrackInfo = record
    StartAddress  : TMSF;       //Track Start Address
    StartAddressLBA: longword;
    Sesion        : Byte;
    DigitalCopy   : Boolean;    //Copy Control (true:permitted/false:prohibited)
    AudioData     : Boolean;    //Contents Attribute (true:Audio/false:Data)
    AudioChannel  : Byte;       //Channel of Audio
    Emphasis      : Boolean;    //Pre-Emphasis (true:emphasized/false:no emphasized)
    ADR           : Byte;       //Tipo de informacion de Sub-channel(Q)
    ISRCcode      : TISRCcode;  //ISRC code
  end;
  TTOCInfo = record
    TrackNumber : Byte;
    Matriz      : array[0..$800]of byte;    
    StartTrack  : Byte;
    EndTrack    : Byte;
    TotalTime   : TMSF;
    Sesiones    : Byte;
    PrimeraSesion: Byte;
    UltimaSesion: Byte;
    SesionesInfo: array [0..99] of TSesionInfo;
    LeadOut     : TSesionInfo;
    UPCcode     : string;       //Codigo UPC/EAN
    TrackInfo   : array [0..99] of TTrackInfo;
  end;

  TATIP = record
    PotenciaEscritura: byte;
    DDCD: boolean;
    VelocidadRef: byte;
    URU: Boolean;
    TipoDisco: byte;
    SubTipoDisco: byte;
    A1Valido: boolean;
    A2Valido: boolean;
    A3Valido: boolean;    
    A1: longword;
    A2: longword;
    A3: longword;
    ATIPInicioLeadIn: TMSF;
    ATIPInicioLeadOut: TMSF;
    S4: longword;
  end;

  TCDLimites = record
    Atip: byte;
    LeadInMIN: byte;
    LeadInSEG: byte;
    LeadInFRA: byte;
    LeadOutMIN: byte;
    LeadOutSEG: byte;
    LeadOutFRA: byte;
  end;

  TTiposMedio = ( tm_CDEstampado,
                   tm_CDR,
                   tm_CDRW,
                   tm_OtrosCD,
                   tm_OtrosMedios,
                   tm_NoMedio,
                   tm_ReservadoDVD,
                   tm_ReservadoOtrosMedios,
                   tm_NoMedio2);

  TCDInfoAntiguo = record
    TipoMedio: TTiposMedio;
    Reservado: byte;
    LongDatos: Word;
    LimitesCD: TCDLimites;
    ATIP: TATIP;
  end;

  TOPC = record
    Velocidad: word;  // En Kbytes
    ValoresOPC: array[0..5] of byte;
  end;

  TEstadoUltimaSesion = ( us_Vacio,
                          us_Incompleta,
                          us_ReserDanyada,
                          us_Completada );

  TEstadoDisco = ( ed_Vacio,
                   ed_Incompleto,
                   ed_Completo,
                   ed_Otros );

  TTipoDisco = ( td_CDROM,
                 td_CDI,
                 td_CDROMXAoDDCD,
                 td_Indefinido );

  TBGEstado = ( bg_NoFormateado,
                bg_SinCompletar,
                bg_FormatoEnProgreso,
                bg_Completo );

  TCDInfo = record
    Borrable: boolean; // Indica si se puede borrar
    EstadoUltimaSesion: TEstadoUltimaSesion;
    EstadoDisco: TEstadoDisco;
    NumeroPrimeraPista: byte;
    NumeroSesiones: word;
    PrimeraPistaEnUltimaSesion: word;
    UltimaPistaEnUltimaSesion: word;
    DID_V: boolean; // Disc ID Valid (True= Valido Identificador de Disco, False=No Valido)
    DBC_V: boolean; // Disc Bar Code (True= Codigo de Barras Valido, False= No valido)
    URU: boolean;  // Unrestricted Use Disc (Disco sin restringimiento de Uso) True=Sin, False=Con
    DBit: byte;
    BGFormatoEstado: TBGEstado; // Background Format State. 
    TipoDisco: TTipoDisco;
    IDDisco: integer;
    InicioLeadInUltimaSesion: integer;
    InicioLeadOut: integer;
    CodigoBarras: int64;
    NumOPC: integer; // Optimun Power Calibration (Calibracion Optima de Potencia)
    OPC: array[0..99] of TOPC;
  end;

  TCapacidad = record
    UltimoLBA: integer;
    TamanoSector: integer; 
  end;

  TQSubcanalADR = ( qs_NoDevuelto,
                    qs_PosicionActual,
                    qs_MediaCatalog,
                    qs_ISRC,
                    qs_Reservado );

  TIPTipoModo = ( tm_LBA=0,
                  tm_LeadIn=1,
                  tm_Pista=1,
                  tm_PistaInvisible=1,
                  tm_Sesion=2 );
  TInfoPista = record
    NumeroPista: word;
    NumeroSesion: word;
    Danyado: Boolean;
    Copia: Boolean;
    ModoPista: TQSubcanalADR;
    RT: Boolean;
    Blank: Boolean;
    PaqueteInc: Boolean;
    FP: Boolean;  // Paquete Incremental de tamaño fijado
    ModoDatos: byte;
    LRA_V: Boolean;
    NWA_V: Boolean;
    LBAPista: integer;
    SigLBAGrabable: integer;
    BloquesLibres: integer;
    TamanoPista: integer;
    TamanoBloquePInc: integer;
    UltimaLBAGrabado: integer;
    
    RestricGrabaParam: byte;
    EstadoPista: byte;
    DefSigLBAGrabable: byte;
  end;

  //--- Estructuras de respuestas de READ TOC ---

  TOC00 = record
    Reservado: byte;
    AdrControl: byte;
    Pista: byte;
    Reservado2: byte;
    InicioLBA: integer;
    InicioMSF: TMSF;
  end;

  TOC01 = record
    Reservado: byte;
    AdrControl: byte;
    Sesion: byte;   // Primera Pista de la Ultima Sesion Cerrada
    Reservado2: byte;
    InicioLBA: integer;
    InicioMSF: TMSF;
  end;

  TOC02 = record
    NumeroSesion: byte;
    AdrControl: byte;
    TNO: byte;
    POINT: byte;
    Min: byte;
    Sec: byte;
    Frame: byte;
    Zero: byte;  //Bits 7-4: HOUR, Bits 3-0: PHOUR
    PMIN: byte;
    PSEC: byte;
    PFRAME: byte;
  end;

  ReadTOC00 = record
    Tamano: word;
    PistaInicial: byte;
    PistaFinal: byte;
    DatosPistas: array[0..99] of TOC00;
  end;

  ReadTOC01 = record
    Tamano: word;
    SesionPrimera: byte;
    SesionUltima: byte;
    DatosSesion: array[0..99] of TOC01;    
  end;

  ReadTOC02 = record
    Tamano: word;
    PistaInicial: byte;
    PistaFinal: byte;
    DatosPistas: array[0..99] of TOC02;
  end;

  TModeSelectEscribir = record
    CodigoPagina: byte; // $05
    LongitudPagina: byte; // $32
    Op1: byte; // 7:reser. 6:BUFE 5:LS_V 4:Simulacion 3-0:TipoEscritura
    Op2: byte; // 7-6:Multisesion 5:FP 4:Copy 3-0:ModoPista
    TipoBloque: byte; // 7-4:reser. 3-0:TipoBloque
    LinkSize: byte; //Link Size ?¿
    IAC: byte; //7-6:reser. 5-0:Codigo Aplicacion del Iniciador
    FormatoSesion: byte; // Formato sesion
    TamPaquete: integer; // Tamaño del Paquete
    DuracionPausa: word; // Duracion de pausa entre cada pista
    MCN: string[16]; // Media Catalog Number
    ISRC: string[16]; // International Standard Recordind Code
    SubHeader: array[0..3] of byte; // SubHeader ¿?
    VendorSpecific: array[0..3] of byte; // Especifico del fabricante
  end;

{$A-}
  TCueSheet = record
    CTLADR: byte;
    TNO: byte;
    Index: byte;
    DataForm: byte;
    SCMS: byte;
    AMIN: byte;
    ASEC: byte;
    AFRAME: byte;
    MCN: byte;
    ISRC: byte;
  end;
{$A+}
  TArray = array[0..$FF] of byte;
  TMensajeSense = procedure (SK, ASC, ASCQ: byte; Error: string) of object;
//------------------------------------------------
// CLASE CDROM
//------------------------------------------------
  TCDROM = class(TWin32ASPI)
  private
    //---Eventos
    FEnError: TMensajeSense;
    FOnTestUnitReadyFinish: TNotifyEvent;
    FOnRequestSenceFinish: TNotifyEvent;
    FOnInquiryFinish: TNotifyEvent;
    FOnReadSubChannelFinish: TNotifyEvent;
    FOnReadTOCFinish: TNotifyEvent;
    FOnModeSenseFinish: TNotifyEvent;
    FOnReadCDMSFFinish: TNotifyEvent;
    FOnReadCDFinish: TNotifyEvent;
    FOnPlayAudioMSFFinish: TNotifyEvent;
    FOnPlayAudioTrackFinish: TNotifyEvent;
    FOnPauseResumeFinish: TNotifyEvent;
    FOnPlayAudioFinish: TNotifyEvent;
    FOnSetStartStopUnitFinish: TNotifyEvent;
    FOnSetCDSpeedFinish: TNotifyEvent;
    procedure MensajeSenseKey(SK: byte; ASC: byte; ASCQ: byte; TextoSense: string);    
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    //---Utilidades---
    function MSF2LBA(MSF: TMSF): Longword;  //MSF to LBA Utility Function (No Offset)
    function LBA2MSF(LBA: Integer): TMSF;   //LBA to MSF Utility Function (No Offset)
    //---Comandos SCSI---
    function TestUnitReady(HA_IDx, SCSI_IDx, LUNx: Byte): Boolean;                                          //Test Unit Ready Command
    function RequestSence(HA_IDx, SCSI_IDx, LUNx: Byte): Byte;                                              //Request Sence Command
    function Inquiry(HA_IDx, SCSI_IDx, LUNx: Byte): TDeviceInfo;                                            //Inquiry Command
    function ReadSubChannel(HA_IDx, SCSI_IDx, LUNx, ADR, Track: Byte): TSubChannel;                         //Read SubChannel Command
    function ReadTOC(HA_IDx, SCSI_IDx, LUNx: Byte): TTOCInfo;                                               //Read TOC Command
    function ReadATIP(HA_IDx, SCSI_IDx, LUNx: Byte): TATIP;     
    function ModeSense(HA_IDx, SCSI_IDx, LUNx: Byte): TCDROMInfo;                                           //ModeSense Command
    function ReadCDMSF(HA_IDx, SCSI_IDx, LUNx: Byte; TopMSF, EndMSF: TMSF; pCDDABuffer: Pointer): Boolean;  //Read CD(MSF) Command
    function ReadCD(HA_IDx, SCSI_IDx, LUNx: Byte; TopLBA, EndLBA: Integer; T_Sector: Byte; Subcanal: TTipoSubcanal; pCDDABuffer: Pointer): Boolean;  //Read CD(LBA) Command
    function PlayAudioMSF(HA_IDx, SCSI_IDx, LUNx: Byte; TopMSF, EndMSF: TMSF): Boolean;                     //Play Audio(MSF) Command
    function PlayAudioTrack(HA_IDx, SCSI_IDx, LUNx: Byte; TopTrack, EndTrack: Byte): Boolean;               //Play Audio Track Command
    function PauseResume(HA_IDx, SCSI_IDx, LUNx: Byte; Resume: Boolean): Boolean;                           //Pause/Resume Command
    function PlayAudio(HA_IDx, SCSI_IDx, LUNx: Byte; TopLBA, EndLBA: Integer): Boolean;                     //Play Audio(LBA) Command
    function SetStartStopUnit(HA_IDx, SCSI_IDx, LUNx: Byte; Eject: Boolean; MotorStop: Boolean): Boolean;   //Set Start/Stop Unit Command
    function SetCDSpeed(HA_IDx, SCSI_IDx, LUNx: Byte; ReadSpeed, WriteSpeed: Word): Boolean;                //Establece la velocidad
    function Read10(HA_IDx, SCSI_IDx, LUNx: Byte; TopLBA, EndLBA: Integer; pCDDABuffer: Pointer): Boolean;  //Lee CD (LBA)
    function Read12(HA_IDx, SCSI_IDx, LUNx: Byte; TopLBA, EndLBA: Integer; pCDDABuffer: Pointer): Boolean;  //Lee CD (LBA)
    function ModeSelect10_Errores(HA_IDx, SCSI_IDx, LUNx, Reintentos, Recuperar: Byte): Boolean;
    function ModeSense_Errores(HA_IDx, SCSI_IDx, LUNx, Reintentos: Byte): TArray;
    function LoadUnload(HA_IDx, SCSI_IDx, LUNx: Byte; Cargar: Boolean): Boolean;
    function BorrarCDRW(HA_IDx, SCSI_IDx, LUNx: byte; TipoBorrado: TBorradoCDRW; PistaOsector: integer): Boolean;
    function SendEvent(HA_IDx, SCSI_IDx, LUNx: Byte): Boolean;
    function ReadCapacity(HA_IDx, SCSI_IDx, LUNx: Byte; LBA: integer): TCapacidad;
    function SetReadAhead(HA_IDx, SCSI_IDx, LUNx: Byte; LBAIndice, LBACache: integer): Boolean;
    function PreventAllowRemoveMedium(HA_IDx, SCSI_IDx, LUNx: Byte; Prevenir: Boolean): Boolean;
    function ReadTrackInformation(HA_IDx, SCSI_IDx, LUNx: Byte; LBA: integer; Tipo: byte): TInfoPista;
    function Seek6(HA_IDx, SCSI_IDx, LUNx: Byte; LBA: integer): Boolean;
    function Seek10(HA_IDx, SCSI_IDx, LUNx: Byte; LBA: integer): Boolean;
    function SetLimits(HA_IDx, SCSI_IDx, LUNx: Byte; LBAInicial, LBAFinal: integer; Leer,Escribir: Boolean): Boolean;
    function HayCDMetido(HA_IDx, SCSI_IDx, LUNx: Byte): boolean;
    function ModeSelectEscribir(HA,SCSI,LUN: byte; Parametros: TModeSelectEscribir): boolean;
    function SendCuesheet(HA,SCSI,LUN: byte; Parametros: pointer; pistas: integer): boolean;
    function Write10(HA,SCSI,LUN: byte; TopLBA: Integer; Cantidad: word; pBuffer: Pointer): Boolean;
    function SynchronizeCache(HA,SCSI,LUN: byte; LBA: integer; Cantidad: word): boolean;
    function CloseSessionTrack(HA,SCSI,LUN: byte; Sesion: boolean; Pista: boolean; nSesionPista: word): boolean;
    function ReadDVDStructure(HA_IDx, SCSI_IDx, LUNx: Byte; Buffer: Pointer; Formato: integer = 0): Boolean;    

    function ReadDiscInformation(HA_IDx, SCSI_IDx, LUNx: Byte): TCDInfo;
    function GetConfiguration(HA_IDx, SCSI_IDx, LUNx: Byte; Opcion: word): TArray;

    //---Eventos
    property EnError: TMensajeSense read FEnError write FEnError;          //Manejador del evento EnError
    property OnTestUnitReadyFinish: TNotifyEvent read FOnTestUnitReadyFinish write FOnTestUnitReadyFinish;          //Event handler of finishing TestUnitReady command
    property OnRequestSenceFinish: TNotifyEvent read FOnRequestSenceFinish write FOnRequestSenceFinish;             //Event handler of finishing RequestSence command
    property OnInquiryFinish: TNotifyEvent read FOnInquiryFinish write FOnInquiryFinish;                            //Event handler of finishing Inquiry Command
    property OnReadSubChannelFinish: TNotifyEvent read FOnReadSubChannelFinish write FOnReadSubChannelFinish;       //Event handler of finishing ReadSubChannel command
    property OnReadTOCFinish: TNotifyEvent read FOnReadTOCFinish write FOnReadTOCFinish;                            //Event handler of finishing ReadTOC command
    property OnModeSenseFinish: TNotifyEvent read FOnModeSenseFinish write FOnModeSenseFinish;                      //Event handler of finishing ModeSense command
    property OnReadCDMSFFinish: TNotifyEvent read FOnReadCDMSFFinish write FOnReadCDMSFFinish;                      //Event handler of finishing ReadCDMSF command
    property OnReadCDFinish: TNotifyEvent read FOnReadCDFinish write FOnReadCDFinish;                               //Event handler of finishing ReadCD command
    property OnPlayAudioMSFFinish: TNotifyEvent read FOnPlayAudioMSFFinish write FOnPlayAudioMSFFinish;             //Event handler of finishing PlayAudioMSF command
    property OnPlayAudioTrackFinish: TNotifyEvent read FOnPlayAudioTrackFinish write FOnPlayAudioTrackFinish;       //Event handler of finishing PlayAudioTrack command
    property OnPauseResumeFinish: TNotifyEvent read FOnPauseResumeFinish write FOnPauseResumeFinish;                //Event handler of finishing PauseResume command
    property OnPlayAudioFinish: TNotifyEvent read FOnPlayAudioFinish write FOnPlayAudioFinish;                      //Event handler of finishing PlayAudio command
    property OnSetStartStopUnitFinish: TNotifyEvent read FOnSetStartStopUnitFinish write FOnSetStartStopUnitFinish; //Event handler of finishing SetStartStopUnit command
    property OnSetCDSpeedFinish: TNotifyEvent read FOnSetCDSpeedFinish write FOnSetCDSpeedFinish;                   //Event handler of finishing SetCDSpeed command
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CDROM', [TCDROM]);
end;

{ TCDROM }

constructor TCDROM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCDROM.Destroy;
begin
  inherited Destroy;
end;

//**********************
//***  Utilidades    ***
//**********************
function TCDROM.LBA2MSF(LBA: Integer): TMSF;
begin
  Result.Minute := (LBA div 75) div 60;
  Result.Second := (LBA - (Result.Minute * 75 * 60)) div 75;
  Result.Frame  := LBA - (Result.Minute * 75 * 60) - (Result.Second * 75);
end;

function TCDROM.MSF2LBA(MSF: TMSF): Longword;
begin
  Result := (MSF.Minute * 60 * 75) + (MSF.Second * 75) + MSF.Frame;
end;

function Bytes2Integer(a,b,c,d: byte): integer;
begin
     result := (a shl 24) or (b shl 16) or (c shl 8) or d;
end;

//*********************
//***  Errores      ***
//*********************     
const
         ASC0000 = 'No hay informacion adicional';
         ASC0100 = 'No hay señal de Indice/Sector';
         ASC0200 = 'Busqueda no completada';
         ASC0300 = 'Fallo de escritura en el periferico';
         ASC0400 = 'Unidad no preparada, causa no reportada';
         ASC0500 = 'Unidad no responde a la seleccion';
         ASC0600 = 'Referencia de posicion no encontrada';
         ASC0700 = 'Multiples dispositivos seleccionados';
         ASC0800 = 'Comunicacion con la unidad fallida';
         ASC0900 = 'Error al seguir la pista';
         ASC0A00 = 'Desbordamiento en el buffer del Log de errores';
         ASC0B00 = 'Cuidado';
         ASC0C00 = 'Error de escritura';
         ASC0D00 = 'Error detectado por un tercer un iniciador del dispositivo';
         ASC1000 = 'Error CRC o EDC';
         ASC1100 = 'Irrecuperable error de lectura';
         ASC1200 = 'Marca de direccion no encontrada en el campo ID';
         ASC1300 = 'Marca de direccion no encontrada en el campo Datos';
         ASC1400 = 'Entidad de escritura no encontrada';
         ASC1500 = 'Error de posicionamiento';
         ASC1600 = 'Error en marca de sincronizacion de datos';
         ASC1700 = 'Datos recuperados sin aplicar correccion de errores';
         ASC1800 = 'Datos recuperados aplicando correccion de errores';
         ASC1900 = 'Error en lista de defectos';                
         ASC1A00 = 'Error en tamaño de la lista de parametros';
         ASC1B00 = 'Error en transferencia sincronizada de datos';
         ASC1C00 = 'Lista de defectos no encontrada';
         ASC1D00 = 'Datos no coinciden al verificar los datos';
         ASC1E00 = 'ID recuperado con correccion de errores';
         ASC1F00 = 'Transferido parcialmente la lista de defectos';
         ASC2000 = 'Codigo de operacion de comando no valido';
         ASC2100 = 'LBA fuera de rango';
         ASC2200 = 'Funcion ilegal (usar 20 00, 24 00, o 26 00)';
         ASC2400 = 'Campo ilegal en CDB';
         ASC2500 = 'Unidad no soportada';
         ASC2600 = 'Campo no valido en la lista de parametros';
         ASC2700 = 'Protegido contra escritura';
         ASC2800 = 'No esta preparado para hacer el cambio, el medio puede ser cambiado';
         ASC2900 = 'Ha ocurrido encendido, reinicio, o reinicio de bus';
         ASC2A00 = 'Parametros cambiados';
         ASC2B00 = 'La copia no se puede ejecutar desde que la unidad no se desconecto';
         ASC2C00 = 'Error en la secuencia de comandos';
         ASC2D00 = 'Error al sobreescribir una posicion';
         ASC2E00 = 'Tiempo insuficiente para realizar la operacion';
         ASC2F00 = 'Comandos eliminados por otro iniciador';
         ASC3000 = 'Medio instalado incompatible';
         ASC3100 = 'Formato del medio corrupto';
         ASC3200 = 'No hay disponible un sustituto de defectos';
         ASC3300 = 'Duracion de la cinta incorrecta';
         ASC3400 = 'Error al cerrar';
         ASC3500 = 'Error en servicios de cierre';
         ASC3600 = 'La cinta, la tinta o el toner esta defectuoso';
         ASC3700 = 'Parametros redondeados';
         ASC3800 = 'Notificacion del evento de estado';
         ASC3900 = 'Salvado de parametros (PS) no soportado';
         ASC3A00 = 'Medio no presente';
         ASC3B00 = 'Error de posicionamiento secuencial';
         ASC3D00 = 'Bits no validos en el mensaje de identificacion';
         ASC3E00 = 'La unidad aun no se ha autoconfigurado';
         ASC3F00 = 'Las condiciones para las operaciones en destino, han cambiado';
         ASC4000 = 'Fallo de RAM (usar 40 NN)';
         ASC4100 = 'La ruta de los datos ha fallado';
         ASC4200 = 'Encendido o autotest ha fallado';
         ASC4300 = 'Mensaje de error';
         ASC4400 = 'Fallo interno del destino';
         ASC4500 = 'Fallo al re/seleccionar';
         ASC4600 = 'No se ha completado correctamente el reinicio por software';
         ASC4700 = 'Error SCSI de paridad';
         ASC4800 = 'Recibido mensaje de error';
         ASC4900 = 'Mensaje de error no valido';
         ASC4A00 = 'Error en una fase del comando';
         ASC4B00 = 'Error en una fase de los datos';
         ASC4C00 = 'Fallo autoconfiguracion de la unidad';
         ASC4E00 = 'Intento de solapamiento de datos';
         ASC5000 = 'Error al escribir añadido';
         ASC5100 = 'Error de borrado';
         ASC5200 = 'Fallo de cartucho';
         ASC5300 = 'Fallo al cargar o expulsar medio';
         ASC5400 = 'Fallo de SCSI al sistema';
         ASC5500 = 'Fallo recurso del sistema';
         ASC5700 = 'Imposible recuperar Tabla de Contenidos (TOC)';
         ASC5800 = 'Generacion no existe';
         ASC5900 = 'Lectura de bloque actualizado';
         ASC5A00 = 'Peticion de operador o el estado a cambiado';
         ASC5B00 = 'Excepcion de logeo';
         ASC5C00 = 'Ha cambiado el estado RPL';
         ASC5D00 = 'Excedido prediccion del umbral';
         ASC5E00 = 'Bajo consumo activado';
         ASC6000 = 'Fallo de lampara';
         ASC6100 = 'Error en la adquisicion de video';
         ASC6200 = 'Error de la cabeza de escaneo';
         ASC6300 = 'Encontrado fin del area de datos de usuario';
         ASC6400 = 'Modo ilegal para esta pista';
         ASC6500 = 'Fallo de voltaje';
         ASC6600 = 'Tapa abierto en el alimentador de hojas';
         ASC6700 = 'Fallo de configuracion';
         ASC6800 = 'Unidad no configurada';
         ASC6900 = 'Perdida de datos en la unidad';
         ASC6A00 = 'Informativo, mirar en el log';
         ASC6B00 = 'Ha ocurrido un cambio de estado';
         ASC6C00 = 'Ha ocurrido un fallo de reestructuracion';
         ASC6D00 = 'Ha ocurrido un fallo de recalculacion';
         ASC6E00 = 'Fallo de comandos a la unidad';
         ASC6F00 = 'Intercambio de la clave de proteccion ha fallado';
         ASC7100 = 'Excepcion de descompresion en ID del algoritmo largo';
         ASC7200 = 'Cerrado de sesion fallido';
         ASC7300 = 'Error en control del CD';

procedure TCDROM.MensajeSenseKey(SK: byte; ASC: byte; ASCQ: byte; TextoSense: string);
 procedure M(SK, ASC, ASCQ: byte; Texto: string);
 begin
       if Assigned(FEnError) then
          FEnError(SK, ASC, ASCQ, Texto);
 end;
var
        s: string;
begin
        case (ASC) of
          $00: s := ASC0000;
          $01: s := ASC0100;
          $02: s := ASC0200;
          $03: s := ASC0300;
          $04: s := ASC0400;
          $05: s := ASC0500;
          $06: s := ASC0600;
          $07: s := ASC0700;
          $08: s := ASC0800;
          $09: s := ASC0900;
          $0A: s := ASC0A00;
          $0B: s := ASC0B00;
          $0C: s := ASC0C00;
          $0D: s := ASC0D00;
        //  $0E: s := ASC0E00;
        //  $0F: s := ASC0F00;
          $10: s := ASC1000;
          $11: s := ASC1100;
          $12: s := ASC1200;
          $13: s := ASC1300;
          $14: s := ASC1400;
          $15: s := ASC1500;
          $16: s := ASC1600;
          $17: s := ASC1700;
          $18: s := ASC1800;
          $19: s := ASC1900;
          $1A: s := ASC1A00;
          $1B: s := ASC1B00;
          $1C: s := ASC1C00;
          $1D: s := ASC1D00;
          $1E: s := ASC1E00;
          $1F: s := ASC1F00;
          $20: s := ASC2000;
          $21: s := ASC2100;
          $22: s := ASC2200;
       //   $23: s := ASC2300;
          $24: s := ASC2400;
          $25: s := ASC2500;
          $26: s := ASC2600;
          $27: s := ASC2700;
          $28: s := ASC2800;
          $29: s := ASC2900;
          $2A: s := ASC2A00;
          $2B: s := ASC2B00;
          $2C: s := ASC2C00;
          $2D: s := ASC2D00;
          $2E: s := ASC2E00;
          $2F: s := ASC2F00;
          $30: s := ASC3000;
          $31: s := ASC3100;
          $32: s := ASC3200;
          $33: s := ASC3300;
          $34: s := ASC3400;
          $35: s := ASC3500;
          $36: s := ASC3600;
          $37: s := ASC3700;
          $38: s := ASC3800;
          $39: s := ASC3900;
          $3A: s := ASC3A00;
          $3B: s := ASC3B00;
       //   $3C: s := ASC3C00;
          $3D: s := ASC3D00;
          $3E: s := ASC3E00;
          $3F: s := ASC3F00;
          $40: s := ASC4000;
          $41: s := ASC4100;
          $42: s := ASC4200;
          $43: s := ASC4300;
          $44: s := ASC4400;
          $45: s := ASC4500;
          $46: s := ASC4600;
          $47: s := ASC4700;
          $48: s := ASC4800;
          $49: s := ASC4900;
          $4A: s := ASC4A00;
          $4B: s := ASC4B00;
          $4C: s := ASC4C00;
        //  $4D: s := ASC4D00;
          $4E: s := ASC4E00;
        //  $4F: s := ASC4F00;
          $50: s := ASC5000;
          $51: s := ASC5100;
          $52: s := ASC5200;
          $53: s := ASC5300;
          $54: s := ASC5400;
          $55: s := ASC5500;
        //  $56: s := ASC5600;
          $57: s := ASC5700;
          $58: s := ASC5800;
          $59: s := ASC5900;
          $5A: s := ASC5A00;
          $5B: s := ASC5B00;
          $5C: s := ASC5C00;
          $5D: s := ASC5D00;
          $5E: s := ASC5E00;
        //  $5F: s := ASC5F00;
          $60: s := ASC6000;
          $61: s := ASC6100;
          $62: s := ASC6200;
          $63: s := ASC6300;
          $64: s := ASC6400;
          $65: s := ASC6500;
          $66: s := ASC6600;
          $67: s := ASC6700;
          $68: s := ASC6800;
          $69: s := ASC6900;
          $6A: s := ASC6A00;
          $6B: s := ASC6B00;
          $6C: s := ASC6C00;
          $6D: s := ASC6D00;
          $6E: s := ASC6E00;
          $6F: s := ASC6F00;
       //   $70: s := ASC7000;
          $71: s := ASC7100;
          $72: s := ASC7200;
          $73: s := ASC7300;
       //   $74: s := ASC7400;
       //   $75: s := ASC7500;
       //   $76: s := ASC7600;
       //   $77: s := ASC7700;
       //   $78: s := ASC7800;
       //   $79: s := ASC7900;
       //   $7A: s := ASC7A00;
       //   $7B: s := ASC7B00;
       //   $7C: s := ASC7C00;
       //   $7D: s := ASC7D00;
       //   $7E: s := ASC7E00;
       //   $7F: s := ASC7F00;
          else s := '';  
        end;
         MessageBox(0,PChar(s),nil,MB_OK);
        M(SK, ASC, ASCQ, s);
end;

//*********************
//*** SCSI Command ****
//*********************
//-----------------------
// SCSI Common Functions
//-----------------------
function TCDROM.TestUnitReady(HA_IDx, SCSI_IDx, LUNx: Byte): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
begin
  Result := False;
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := 0;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 6;
  SRBPacket.CDBByte[0] := SCSI_TESTUNITREADY;
  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
    begin
      Result := True;
    end
    else
    begin
      if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
      begin
        if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
        begin
          Result := True;
        end;
      end;
    end;
  end
  else
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('La unidad no esta preparada.');
      Error.ErrorCode := $00030000;
      //raise Error;
      MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], '');
    end;
  end;
  If Assigned(FOnTestUnitReadyFinish) then
  begin
    FOnTestUnitReadyFinish(Self);
  end;
end;

function TCDROM.RequestSence(HA_IDx, SCSI_IDx, LUNx: Byte): Byte;
var
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
begin
  Result := $ff;
  //---Inicializacion de SCSI Request Block (SRB)---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId   := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun    := LUNx;
  SRBPacket.SRB_BufLen := $ff;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 6;
  SRBPacket.CDBByte[0] := SCSI_RequestSence;
  SRBPacket.CDBByte[4] := $ff;
  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if ((Buffer[0] and $7f) = $70) or ((Buffer[0] and $7f) = $71) then
    begin
         Result := Buffer[2] and $0f;
    end;
//  end
//  else
//  begin
    if ErrorEnable then
    begin
      Result := Buffer[2] and $0f;
      case Result of
        0:      MensajeSenseKey(result,Buffer[12],Buffer[13],'Correcto');
        2:      MensajeSenseKey(result,Buffer[12],Buffer[13],'Unidad no preparada');
        4:      MensajeSenseKey(result,Buffer[12],Buffer[13],'Error de Hardware.');
        5:      MensajeSenseKey(result,Buffer[12],Buffer[13],'Parametro ilegal al solicitar informacion a la unidad.');
        6:      MensajeSenseKey(result,Buffer[12],Buffer[13],'La unidad ha sido resetada o ha cambiado de medio.');
        7:      MensajeSenseKey(result,Buffer[12],Buffer[13],'Datos protegidos.');
        11:     MensajeSenseKey(result,Buffer[12],Buffer[13],'Ha sido abortado el comando.');
        14:     MensajeSenseKey(result,Buffer[12],Buffer[13],'Los datos de salida son distintos a los del origen.');
        else    MensajeSenseKey(result,Buffer[12],Buffer[13],'Error desconocido al solicitar informacion a la unidad.');
      end;
    end;
  end;
  if Assigned(FOnRequestSenceFinish) then
  begin
    FOnRequestSenceFinish(Self);
  end;
end;

function TCDROM.Inquiry(HA_IDx, SCSI_IDx, LUNx: Byte): TDeviceInfo;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
  n, qualify, DeviceType, DataFormat: Byte;
begin
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := 36;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 6;
  SRBPacket.CDBByte[0] := SCSI_INQUIRY;
  SRBPacket.CDBByte[4] := 36;
{  SRBPacket.CDBByte[1] := $AA;
  SRBPacket.CDBByte[2] := 0;
  SRBPacket.CDBByte[3] := 0;
  SRBPacket.CDBByte[4] := $7A;//36;
  SRBPacket.CDBByte[5] := $3b;
  SRBPacket.CDBByte[6] := $4A;
  SRBPacket.CDBByte[7] := 00;
  SRBPacket.CDBByte[8] := $20;
  SRBPacket.CDBByte[9] := $A8;
  SRBPacket.CDBByte[10] := $02;
  SRBPacket.CDBByte[11] := $00;   }

  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    Result.HA_ID := HA_IDx;
    Result.SCSI_ID := SCSI_IDx;
    Result.LUN := LUNx;
    qualify := (Buffer[0] and $e0) shr 5; //Comprueba el estado de la unidad
    case qualify of
      0:    Result.DeviceStatus := ds_Connected;
      1:    Result.DeviceStatus := ds_NotConnected;
      2:    Result.DeviceStatus := ds_Reserved;
      3:    Result.DeviceStatus := ds_NotSupported;
      else  Result.DeviceStatus := ds_Unknown;
    end;
    DeviceType := Buffer[0] and $1f;  //Comprueba el tipo de unidad
    case DeviceType of
      0:    Result.DeviceType := dt_DirectAccess;
      1:    Result.DeviceType := dt_SequencialAccess;
      2:    Result.DeviceType := dt_Printer;
      3:    Result.DeviceType := dt_Processor;
      4:    Result.DeviceType := dt_WriteOnceDisc;
      5:    Result.DeviceType := dt_CDROM;
      6:    Result.DeviceType := dt_Scanner;
      7:    Result.DeviceType := dt_OpticalMemory;
      8:    Result.DeviceType := dt_MediaChangerDrive;
      9:    Result.DeviceType := dt_Communication;
      else  Result.DeviceType := dt_Unknown;
    end;
    DataFormat := Buffer[3] and $0f;  //Comprueba el soporte de comandos
    case DataFormat of
      0:    Result.DataFormat := df_SCSI1;
      1:    Result.DataFormat := df_CCS;
      2:    Result.DataFormat := df_SCSI2;
      else  Result.DataFormat := df_Unknown;
    end;
    Result.Removable  := ((Buffer[1] and $80) = $80);   //check removable media
    Result.ISOVersion := (Buffer[2] and $C0);           //Version ISO
    Result.ECMAVersion:= (Buffer[2] and $38);           //Version ECMA
    Result.ANSIVersion := (Buffer[2] and $07);          //Version ANSI
    Result.ATAPIVersion:= (Buffer[2] and $F0);          //Version ATAPI
    Result.AENC       := ((Buffer[3] and $80) = $80);   //check AEN recieve function support
    Result.TrmlOP     := ((Buffer[3] and $40) = $40);   //check terminate I/O process message support
    Result.RelAdr     := ((Buffer[7] and $80) = $80);   //check relative block address function support
    Result.WBus32     := ((Buffer[7] and $40) = $40);   //check 32 bit wide transfer mode support
    Result.WBus16     := ((Buffer[7] and $20) = $20);   //check 16 bit wide transfer mode support
    Result.SyncTx     := ((Buffer[7] and $10) = $10);   //check synchronous transfer support
    Result.Linked     := ((Buffer[7] and $08) = $08);   //check linked command support
    Result.CmdQue     := ((Buffer[7] and $02) = $02);   //check command queue with tag support
    Result.SftRst     := ((Buffer[7] and $01) = $01);   //check soft reset condition support
    Result.VendorID := '';
    for n := 8 to 15 do
        Result.VendorID := Result.VendorID + char(Buffer[n]);

    Result.ProductID := '';
    for n := 16 to 31 do
        Result.ProductID := Result.ProductID + char(Buffer[n]);

    Result.Revision := '';
    for n := 32 to 35 do
        Result.Revision := Result.Revision + char(Buffer[n]);
  end
  else
  begin
    Result.HA_ID := $ff;
    Result.SCSI_ID := $ff;
    Result.LUN := $ff;
    Result.DeviceStatus := ds_Unknown;
    Result.DeviceType := dt_Unknown;
    Result.DataFormat := df_Unknown;
    Result.Removable := false;
    Result.ECMAVersion := 0;
    Result.ATAPIVersion := 0;
    Result.ANSIVersion := 0;
    Result.ISOVersion := 0;
    Result.AENC := false;
    Result.TrmlOP := false;
    Result.RelAdr := false;
    Result.WBus32 := false;
    Result.WBus16 := false;
    Result.SyncTx := false;
    Result.Linked := false;
    Result.CmdQue := false;
    Result.SftRst := false;
    Result.VendorID := '';
    Result.ProductID := '';
    Result.Revision := '';
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al realizar la peticion de informacion.');
      Error.ErrorCode := $00031200;
      raise Error;
    end;
  end;
  if Assigned(FOnInquiryFinish) then
  begin
    FOnInquiryFinish(Self);
  end;
end;

//------------------
// Funciones CD-ROM 
//------------------
function TCDROM.ReadDVDStructure(HA_IDx, SCSI_IDx, LUNx: Byte; Buffer: Pointer; Formato: integer = 0): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  xResult: Boolean;
begin

  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);

  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufPointer := Buffer;
  SRBPacket.SRB_BufLen := 2048;
  SRBPacket.SRB_CDBLen := 12;
	SRBPacket.CDBByte[0] := $AD;
        SRBPacket.CDBByte[6] := 0; //Capa
        SRBPacket.CDBByte[7] := Formato;     
        SRBPacket.CDBByte[8] := $08;
        SRBPacket.CDBByte[9] := 00;

  //---Ejecutar Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);

  Result := False;
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
	  begin
	    Result := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result := True;
              end;
            end;
	  end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al ejecutar comando "Read DVD Structure".');
      Error.ErrorCode := $0003be00;
      raise Error;
    end;
  end;
end;

function TCDROM.PreventAllowRemoveMedium(HA_IDx, SCSI_IDx, LUNx: Byte; Prevenir: Boolean): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  xResult: Boolean;
  Prev: byte;
begin
  //---Conversion de parametros---
  if Prevenir then Prev := 1 else Prev := 0;
  
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);

  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_CDBLen := 12;
	SRBPacket.CDBByte[0] := SCSI_MediumRemoval;
        SRBPacket.CDBByte[4] := Prev;

  //---Ejecutar Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);

  Result := False;
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
	  begin
	    Result := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result := True;
              end;
            end;
	  end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al ejecutar comando "Read10".');
      Error.ErrorCode := $0003be00;
      raise Error;
    end;
  end;
end;

function TCDROM.Seek6(HA_IDx, SCSI_IDx, LUNx: Byte; LBA: integer): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  xResult: Boolean;
  LBA0, LBA1, LBA2: Byte;
begin
  Result := False;
  //---Conversion de Parametros---
  LBA2 := ($00ff0000 and LBA) shr 16;
  LBA1 := ($0000ff00 and LBA) shr 8;
  LBA0 := ($000000ff and LBA);

  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := 0;
  SRBPacket.SRB_BufPointer := nil;
  SRBPacket.SRB_CDBLen := 6;
	SRBPacket.CDBByte[0] := SCSI_Seek6;
	SRBPacket.CDBByte[1] := LBA2;  // MSB LBA 
	SRBPacket.CDBByte[2] := LBA1;
	SRBPacket.CDBByte[3] := LBA0;  // LSB LBA
	SRBPacket.CDBByte[4] := 0;
	SRBPacket.CDBByte[5] := 0;

  //---Ejecutar Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
	  begin
	    Result := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result := True;
              end;
            end;
	  end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al ejecutar comando "Seek6"');
      Error.ErrorCode := $0003be00;
      raise Error;
    end;
  end;
end;

function TCDROM.Seek10(HA_IDx, SCSI_IDx, LUNx: Byte; LBA: integer): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  xResult: Boolean;
  LBA0, LBA1, LBA2, LBA3: Byte;
begin
  Result := False;
  //---Conversion de Parametros---
  LBA3 := ($ff000000 and LBA) shr 24;
  LBA2 := ($00ff0000 and LBA) shr 16;
  LBA1 := ($0000ff00 and LBA) shr 8;
  LBA0 := ($000000ff and LBA);

  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := 0;
  SRBPacket.SRB_BufPointer := nil;
  SRBPacket.SRB_CDBLen := 10;
	SRBPacket.CDBByte[0] := SCSI_Seek10;
	SRBPacket.CDBByte[1] := 0;
	SRBPacket.CDBByte[2] := LBA3;   // MSB LBA
	SRBPacket.CDBByte[3] := LBA2;
	SRBPacket.CDBByte[4] := LBA1;
	SRBPacket.CDBByte[5] := LBA0;   // LSB LBA

  //---Ejecutar Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
	  begin
	    Result := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result := True;
              end;
            end;
	  end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al ejecutar comando "Seek10"');
      Error.ErrorCode := $0003be00;
      //raise Error;
      MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], '');
    end;
  end;
end;

function TCDROM.HayCDMetido(HA_IDx, SCSI_IDx, LUNx: Byte): boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: array[0..SENSE_LEN-1] of byte;
  pBuffer: pointer;
  xResult: Boolean;
begin
  Result := False;
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  pBuffer := @Buffer;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := SENSE_LEN;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 6;
	SRBPacket.CDBByte[0] := SCSI_HayCD;

  //---Ejecutar Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then Result := True
    else
    begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then  Result := True;
            end;
    end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al comprobar la existencia de un CD.');
      Error.ErrorCode := $0003be00;
      //raise Error;
      //MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], '');
    end;
  end;
end;

{
  SetLimits: Establece los limites LBA donde los comando siguientes van a poder
             moverse.
}
function TCDROM.SetLimits(HA_IDx, SCSI_IDx, LUNx: Byte; LBAInicial, LBAFinal: integer; Leer,Escribir: Boolean): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  xResult: Boolean;
  LBA0, LBA1, LBA2, LBA3, LBAf0, LBAf1: Byte;
  LeerEscribir: byte;
begin
  Result := False;
  LBAFinal := LBAFinal - LBAInicial;
  if Leer then LeerEscribir := $02 else LeerEscribir := $00;
  if Escribir then LeerEscribir := LeerEscribir + $01;
  LeerEscribir := LeerEscribir + (LUNx shl $05);
  //---Conversion de Parametros---
  LBA3 := ($ff000000 and LBAInicial) shr 24;
  LBA2 := ($00ff0000 and LBAInicial) shr 16;
  LBA1 := ($0000ff00 and LBAInicial) shr 8;
  LBA0 := ($000000ff and LBAInicial);

  LBAf1 := ($0000ff00 and LBAFinal) shr 8;
  LBAf0 := ($000000ff and LBAFinal);

  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := 0;
  SRBPacket.SRB_BufPointer := nil;
  SRBPacket.SRB_CDBLen := 10;
	SRBPacket.CDBByte[0] := SCSI_SetLimits;
	SRBPacket.CDBByte[1] := LeerEscribir;
	SRBPacket.CDBByte[2] := LBA3;   // MSB LBA INICIO
	SRBPacket.CDBByte[3] := LBA2;
	SRBPacket.CDBByte[4] := LBA1;
	SRBPacket.CDBByte[5] := LBA0;   // LSB LBA INICIO
        SRBPacket.CDBByte[7] := LBAf1;   // LSB LBA INICIO
        SRBPacket.CDBByte[8] := LBAf0;   // LSB LBA INICIO

  //---Ejecutar Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
	  begin
	    Result := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result := True;
              end;
            end;
	  end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al ejecutar comando "SetLimits"');
      Error.ErrorCode := $0003be00;
      raise Error;
    end;
  end;
end;

function TCDROM.Read10(HA_IDx, SCSI_IDx, LUNx: Byte; TopLBA, EndLBA: Integer; pCDDABuffer: Pointer): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  xResult: Boolean;
  Size: Integer;
  LBA0, LBA1, LBA2, LBA3, Size0, Size1: Byte;
begin
  Result := False;
  //---Conversion de Parametros---
  Size := EndLBA - TopLBA;
  LBA3 := ($ff000000 and TopLBA) shr 24;
  LBA2 := ($00ff0000 and TopLBA) shr 16;
  LBA1 := ($0000ff00 and TopLBA) shr 8;
  LBA0 := ($000000ff and TopLBA);
  Size1 := ($0000ff00 and Size) shr 8;
  Size0 := ($000000ff and Size);
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := 2048 * Size;
  SRBPacket.SRB_BufPointer := pCDDABuffer;
  SRBPacket.SRB_CDBLen := 12;
	SRBPacket.CDBByte[0] := SCSI_Read10;
	SRBPacket.CDBByte[1] := 0;
	SRBPacket.CDBByte[2] := LBA3;   // MSB LBA INICIO
	SRBPacket.CDBByte[3] := LBA2;
	SRBPacket.CDBByte[4] := LBA1;
	SRBPacket.CDBByte[5] := LBA0;   // LSB LBA INICIO
	SRBPacket.CDBByte[6] := 0;  // MSB LBA FINAL
	SRBPacket.CDBByte[7] := Size1;
	SRBPacket.CDBByte[8] := Size0;
        SRBPacket.CDBByte[9] := 0;  // LSB LBA FINAL
  //---Ejecutar Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
	  begin
	    Result := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result := True;
              end;
            end;
	  end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      ServiceAbort(HA_IDx, pSRBPacket);
      Error := ECDROMError.Create('Error al ejecutar comando "Read10".');
      Error.ErrorCode := $0003be00;
      //raise Error;
      result := False;      
    end;
  end;
  If Assigned(FOnReadCDFinish) then
  begin
    FOnReadCDFinish(Self);
  end;
end;

function TCDROM.Read12(HA_IDx, SCSI_IDx, LUNx: Byte; TopLBA, EndLBA: Integer; pCDDABuffer: Pointer): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  xResult: Boolean;
  Size: Integer;
  LBA0, LBA1, LBA2, LBA3, Size0, Size1, Size2, Size3: Byte;
begin
  Result := False;
  //---Conversion de Parametros---
  Size := EndLBA - TopLBA;
  LBA3 := ($ff000000 and TopLBA) shr 24;
  LBA2 := ($00ff0000 and TopLBA) shr 16;
  LBA1 := ($0000ff00 and TopLBA) shr 8;
  LBA0 := ($000000ff and TopLBA);
  Size3 := ($00ff0000 and Size) shr 24;
  Size2 := ($00ff0000 and Size) shr 16;
  Size1 := ($0000ff00 and Size) shr 8;
  Size0 := ($000000ff and Size);
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := Size*2048;
  SRBPacket.SRB_BufPointer := pCDDABuffer;
  SRBPacket.SRB_CDBLen := 12;
	SRBPacket.CDBByte[0] := SCSI_Read12;
	SRBPacket.CDBByte[1] := 0;      // Bits: 4-DPO, 3-FUA, 2,1-Reservado, 0-RELADR
	SRBPacket.CDBByte[2] := LBA3;   // MSB LBA INICIO
	SRBPacket.CDBByte[3] := LBA2;
	SRBPacket.CDBByte[4] := LBA1;
	SRBPacket.CDBByte[5] := LBA0;   // LSB LBA INICIO
	SRBPacket.CDBByte[6] := Size3;  // MSB LBA FINAL
	SRBPacket.CDBByte[7] := Size2;
	SRBPacket.CDBByte[8] := Size1;
        SRBPacket.CDBByte[9] := Size0;  // LSB LBA FINAL
        SRBPacket.CDBByte[10] := 0;     // Bits: 7- Streaming ?
        SRBPacket.CDBByte[11] := 0;     // Control
  //---Ejecutar Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
	  begin
	    Result := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result := True;
              end;
            end;
	  end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      ServiceAbort(HA_IDx, pSRBPacket);
      Error := ECDROMError.Create('Error al ejecutar comando "Read12".');
      Error.ErrorCode := $0003be00;
      //raise Error;
      result := False;
    end;
  end;
  If Assigned(FOnReadCDFinish) then
  begin
    FOnReadCDFinish(Self);
  end;
end;

function TCDROM.ReadCapacity(HA_IDx, SCSI_IDx, LUNx: Byte; LBA: integer): TCapacidad;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  xResult: Boolean;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;  
  LBA0, LBA1, LBA2, LBA3: Byte;
begin
  Fillchar(Result, sizeof(Result), 0);
  //---Conversion de Parametros---
  LBA3 := ($ff000000 and LBA) shr 24;
  LBA2 := ($00ff0000 and LBA) shr 16;
  LBA1 := ($0000ff00 and LBA) shr 8;
  LBA0 := ($000000ff and LBA);
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := 8;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 10;
	SRBPacket.CDBByte[0] := SCSI_ReadCapacity;
	SRBPacket.CDBByte[1] := 0;      // Bits: 0-RELADR
	SRBPacket.CDBByte[2] := LBA3;   // MSB LBA INICIO
	SRBPacket.CDBByte[3] := LBA2;
	SRBPacket.CDBByte[4] := LBA1;
	SRBPacket.CDBByte[5] := LBA0;   // LSB LBA INICIO
	SRBPacket.CDBByte[6] := 0;
        SRBPacket.CDBByte[7] := 0;
	SRBPacket.CDBByte[8] := 0;      // Bits: 0-PMI
        SRBPacket.CDBByte[9] := 0;      // Control          
  //---Ejecutar Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
	  begin
	    Result.UltimoLBA := Bytes2Integer(Buffer[0],
                                              Buffer[1],
                                              Buffer[2],
                                              Buffer[3]);
            Result.TamanoSector := Bytes2Integer(Buffer[4],
                                                 Buffer[5],
                                                 Buffer[6],
                                                 Buffer[7]);
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result.UltimoLBA := Bytes2Integer(Buffer[0],
                                                  Buffer[1],
                                                  Buffer[2],
                                                  Buffer[3]);
                Result.TamanoSector := Bytes2Integer(Buffer[4],
                                                     Buffer[5],
                                                     Buffer[6],
                                                     Buffer[7]);
              end;
            end;
	  end;
  end;
  if not xResult then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al ejecutar comando "LeerCapacidad".');
      Error.ErrorCode := $0003be00;
      raise Error;
    end;
  end;
  if Assigned(FOnReadCDFinish) then
  begin
    FOnReadCDFinish(Self);
  end;
end;

function TCDROM.ReadSubChannel(HA_IDx, SCSI_IDx, LUNx, ADR, Track: Byte): TSubChannel;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
  n: Byte;
begin
  Result.ADR := $ff;
  Result.AudioStatus := as_Unknown;
  Result.Track := $ff;
  Result.Index := $ff;
  Result.UPCcode := '';
  Result.ISRCcode.CountryCode := '';
  Result.ISRCcode.VendorCode := '';
  Result.ISRCcode.RecordingYear := '';
  Result.ISRCcode.SerialNumber := '';
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId       := HA_IDx;
  SRBPacket.SRB_Target     := SCSI_IDx;
  SRBPacket.SRB_Lun        := LUNx;
  SRBPacket.SRB_BufLen     := 48;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen     := 10;
  SRBPacket.CDBByte[0]     := SCSI_ReadSubchannel;
	SRBPacket.CDBByte[1] := $02;              // Select MSF Address
	SRBPacket.CDBByte[2] := $40;              // Select Sub-channel Q
	SRBPacket.CDBByte[3] := ADR;              // Select Data Format ($02:UPC/EAN code/$03:ISRC code)
  case ADR of
    $00:  //Get All Subcode Data
    begin
  	  SRBPacket.CDBByte[6] := Track;          // Track Number
  	  SRBPacket.CDBByte[8] := 48;               // Load Data Size
    end;
    $01:  //Get Current Position
    begin
  	  SRBPacket.CDBByte[8] := 16;               // Load Data Size
    end;
    $02:  //Get UPC/EAN code
    begin
  	  SRBPacket.CDBByte[8] := 24;               // Load Data Size
    end;
    $03:  //Get ISRC code
    begin
  	  SRBPacket.CDBByte[6] := Track;            // Track Number
  	  SRBPacket.CDBByte[8] := 24;               // Load Data Size
    end;
  end;
  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    case ADR of
      $00:  //Get All Subcode Data
      begin
        case Buffer[1] of
          $00:
          begin
            Result.AudioStatus := as_NoSupport;
          end;
          $11:
          begin
            Result.AudioStatus := as_Play;
          end;
          $12:
          begin
            Result.AudioStatus := as_Pause;
          end;
          $13:
          begin
            Result.AudioStatus := as_Stop;
          end;
          $14:
          begin
            Result.AudioStatus := as_ErrorHalt;
          end;
          $15:
          begin
            Result.AudioStatus := as_NoReport;
          end;
          else
          begin
            Result.AudioStatus := as_Unknown;
          end;
        end;
        Result.ADR := Buffer[5] shr 4;
        Result.Track := Buffer[6];
        Result.Index := Buffer[7];
        Result.ATime.Minute := Buffer[9];
        Result.ATime.Second := Buffer[10];
        Result.ATime.Frame := Buffer[11];
        Result.PTime.Minute := Buffer[13];
        Result.PTime.Second := Buffer[14];
        Result.PTime.Frame := Buffer[15];
        if (Buffer[16] and $80) = $80 then
	      begin
	        for n := 17 to 31 do
	        begin
	          Result.UPCcode := Result.UPCcode + char(Buffer[n]);
	        end;
	      end;
	      if (Buffer[32] and $80) = $80 then
	      begin
	        for n := 33 to 34 do
	        begin
	          Result.ISRCcode.CountryCode := Result.ISRCcode.CountryCode + char(Buffer[n]);
	        end;
	        for n := 35 to 37 do
	        begin
	          Result.ISRCcode.VendorCode := Result.ISRCcode.VendorCode + char(Buffer[n]);
	        end;
	        for n := 38 to 39 do
	        begin
	          Result.ISRCcode.RecordingYear := Result.ISRCcode.RecordingYear + char(Buffer[n]);
	        end;
	        for n := 40 to 43 do
	        begin
	          Result.ISRCcode.SerialNumber := Result.ISRCcode.SerialNumber + char(Buffer[n]);
	        end;
        end;
      end;
      $01:  //Get Current Position
      begin
        case Buffer[1] of
          $00:
          begin
            Result.AudioStatus := as_NoSupport;
          end;
          $11:
          begin
            Result.AudioStatus := as_Play;
          end;
          $12:
          begin
            Result.AudioStatus := as_Pause;
          end;
          $13:
          begin
            Result.AudioStatus := as_Stop;
          end;
          $14:
          begin
            Result.AudioStatus := as_ErrorHalt;
          end;
          $15:
          begin
            Result.AudioStatus := as_NoReport;
          end;
          else
          begin
            Result.AudioStatus := as_Unknown;
          end;
        end;
        Result.ADR := Buffer[5] shr 4;
        Result.Track := Buffer[6];
        Result.Index := Buffer[7];
        Result.ATime.Minute := Buffer[9];
        Result.ATime.Second := Buffer[10];
        Result.ATime.Frame  := Buffer[11];
        Result.PTime.Minute := Buffer[13];
        Result.PTime.Second := Buffer[14];
        Result.PTime.Frame  := Buffer[15];
      end;
      $02:  //Get UPC/EAN code
      begin
        if (Buffer[8] and $80) = $80 then
	      begin
	        for n := 9 to 23 do
	        begin
	          Result.UPCcode := Result.UPCcode + char(Buffer[n]);
	        end;
	      end;
      end;
      $03:  //Get ISRC code
      begin
	      if (Buffer[8] and $80) = $80 then
	      begin
	        for n := 9 to 10 do
	        begin
	          Result.ISRCcode.CountryCode := Result.ISRCcode.CountryCode + char(Buffer[n]);
	        end;
	        for n := 11 to 13 do
	        begin
	          Result.ISRCcode.VendorCode := Result.ISRCcode.VendorCode + char(Buffer[n]);
	        end;
	        for n := 14 to 15 do
	        begin
	          Result.ISRCcode.RecordingYear := Result.ISRCcode.RecordingYear + char(Buffer[n]);
	        end;
	        for n := 16 to 19 do
	        begin
	          Result.ISRCcode.SerialNumber := Result.ISRCcode.SerialNumber + char(Buffer[n]);
	        end;
        end;
      end;
    end;
    xResult := False;
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
    begin
	    xResult := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        xResult := True;
        end;
	    end;
    end;
  end;
  if not xResult then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al leer el Subcanal');
      Error.ErrorCode := $00034200;
      raise Error;
    end;
  end;
  If Assigned(FOnReadSubChannelFinish) then
  begin
    FOnReadSubChannelFinish(Self);
  end;
end;

function TCDROM.GetConfiguration(HA_IDx, SCSI_IDx, LUNx: Byte; Opcion: word): TArray;
var
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult, R: boolean;
  i: integer;
begin
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := 36;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 10;

  SRBPacket.CDBByte[0] := SCSI_GetConfiguration;
  SRBPacket.CDBByte[1] := 2;
  SRBPacket.CDBByte[2] := Hi(Opcion);
  SRBPacket.CDBByte[3] := Lo(Opcion);
  SRBPacket.CDBByte[7] := 0;
  SRBPacket.CDBByte[8] := 36;
  SRBPacket.CDBByte[9] := 0;
  // Ejecutamos el comando
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
        for i := 0 to 35 do
            result[i] := buffer[i];
  end;

  R := False;
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
    begin
	    R := True;
    end
    else
    begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	         R := True;
    end;
  end;
  if not R then
  begin
    if ErrorEnable then
    begin
      RequestSence(HA_IDx, SCSI_IDx, LUNx);
    end;
  end;
end;

function TCDROM.ReadTrackInformation(HA_IDx, SCSI_IDx, LUNx: Byte; LBA: integer; Tipo: byte): TInfoPista;
var
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult, R: boolean;
  LBA0, LBA1, LBA2, LBA3: Byte;
begin
  //---Conversion Parametros---
  LBA3 := ($ff000000 and LBA) shr 24;
  LBA2 := ($00ff0000 and LBA) shr 16;
  LBA1 := ($0000ff00 and LBA) shr 8;
  LBA0 := ($000000ff and LBA);
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := 36;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 10;

  SRBPacket.CDBByte[0] := SCSI_ReadTrackInformation;
  SRBPacket.CDBByte[1] := Tipo;
  SRBPacket.CDBByte[2] := LBA3;
  SRBPacket.CDBByte[3] := LBA2;
  SRBPacket.CDBByte[4] := LBA1;
  SRBPacket.CDBByte[5] := LBA0;
  SRBPacket.CDBByte[7] := 0;
  SRBPacket.CDBByte[8] := 36;

  // Ejecutamos el comando
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
       Result.NumeroPista := (Buffer[32] shl 8) or Buffer[2];
       Result.NumeroSesion := (Buffer[33] shl 8) or Buffer[3];
       Result.Danyado := ((Buffer[5] and $20) = $20);
       Result.Copia := ((Buffer[5] and $10) = $10);

       if (Buffer[5] and $05) > 3 then Result.ModoPista := qs_Reservado
       else Result.ModoPista := TQSubcanalADR(Buffer[5] and $05);

       Result.RT := (Buffer[6] and $80) = $80;
       Result.Blank := (Buffer[6] and $40) = $40;
       Result.PaqueteInc := (Buffer[6] and $20) = $20;
       Result.FP := (Buffer[6] and $10) = $10;

       Result.ModoDatos := Buffer[6] and $05;
       Result.LRA_V := (Buffer[7] and $02) = $02;
       Result.NWA_V := (Buffer[7] and $01) = $01;
       Result.LBAPista := Bytes2Integer(Buffer[8], Buffer[9], Buffer[10], Buffer[11]);
       Result.SigLBAGrabable := Bytes2Integer(Buffer[12], Buffer[13], Buffer[14], Buffer[15]);
       Result.BloquesLibres := Bytes2Integer(Buffer[16], Buffer[17], Buffer[18], Buffer[19]);
       Result.TamanoBloquePInc := Bytes2Integer(Buffer[20], Buffer[21], Buffer[22], Buffer[23]);
       Result.TamanoPista := Bytes2Integer(Buffer[24], Buffer[25], Buffer[26], Buffer[27]);
       Result.UltimaLBAGrabado := Bytes2Integer(Buffer[28], Buffer[29], Buffer[30], Buffer[31]);

       Result.RestricGrabaParam := (Buffer[6] and $E0) shr 5;
       Result.EstadoPista := (Buffer[6] and $E0) shr 4;
       Result.DefSigLBAGrabable := (((Buffer[6] and $E0) shr 4) shl 1) or (Buffer[7] and $01);
  end;

  R := False;
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
    begin
	    R := True;
    end
    else
    begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	         R := True;
    end;
  end;
  if not R then
  begin
    if ErrorEnable then
    begin
      MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], '');
    end;
  end;
end;

function TCDROM.ReadTOC(HA_IDx, SCSI_IDx, LUNx: Byte): TTOCInfo;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
  tno, x, i: Byte;
  msf: TMSF;
  prueba: integer;
begin
  Result.Sesiones := $ff;
  Result.PrimeraSesion := $ff;
  Result.UltimaSesion := $ff;      
  Result.TrackNumber := $ff;
  Result.StartTrack := $ff;
  Result.EndTrack := $ff;
  Result.TotalTime.Minute := 0;
  Result.TotalTime.Second := 0;
  Result.TotalTime.Frame := 0;
  Result.UPCcode := '';
  for tno := 0 to 99 do
  begin
    Result.TrackInfo[tno].StartAddress.Minute := 0;
    Result.TrackInfo[tno].StartAddress.Second := 0;
    Result.TrackInfo[tno].StartAddress.Frame  := 0;
    Result.TrackInfo[tno].DigitalCopy  := false;
    Result.TrackInfo[tno].AudioData    := false;
    Result.TrackInfo[tno].AudioChannel := 0;
    Result.TrackInfo[tno].Emphasis     := false;
    Result.TrackInfo[tno].ADR := 0;
    Result.TrackInfo[tno].ISRCcode.CountryCode := '';
    Result.TrackInfo[tno].ISRCcode.VendorCode := '';
    Result.TrackInfo[tno].ISRCcode.RecordingYear := '';
    Result.TrackInfo[tno].ISRCcode.SerialNumber := '';
  end;
//Paso CloneCD: Obtener informacion del TOC en modo LBA
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := $324;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 10;
  SRBPacket.CDBByte[0] := SCSI_ReadTOC;
  SRBPacket.CDBByte[1] := 0;                  // 2= MSF 0= LBA
  SRBPacket.CDBByte[2] := 0;                  // FORMATO DE TOC/PMA/ATIP
	SRBPacket.CDBByte[6] := ReadIn_Track; // PISTA/SESION INICIAL
	SRBPacket.CDBByte[7] := $06;          // TAMAÑO TOC MSB
	SRBPacket.CDBByte[8] := $8c;          // TAMAÑO TOC LSB
  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    Result.StartTrack := Buffer[2];
    Result.EndTrack := Buffer[3];
    Result.TrackNumber := Buffer[3] - Buffer[2] + 1;
    x := 4;
    for tno := Result.StartTrack to Result.EndTrack do
    begin
      Result.TrackInfo[tno].Emphasis    := ((Buffer[x + 1] and $01) = $01);
      Result.TrackInfo[tno].DigitalCopy := ((Buffer[x + 1] and $02) = $02);
      Result.TrackInfo[tno].AudioData   := not((Buffer[x + 1] and $04) = $04);
      if (Buffer[x + 1] and $08) = $08 then
      begin
        Result.TrackInfo[tno].AudioChannel := 4;
      end
      else
      begin
        Result.TrackInfo[tno].AudioChannel := 2;
      end;
      Result.TrackInfo[tno].ADR := Buffer[x + 1] shr 4;
      Result.TrackInfo[tno].StartAddressLBA := Bytes2Integer(Buffer[x + 4],
                                                             Buffer[x + 5],
                                                             Buffer[x + 6],
                                                             Buffer[x + 7]);
      x := x + 8;
    end;
    xResult := False;

    if SRBPacket.SRB_TargetStat = STATUS_GOOD then xResult := True
    else
    begin
  	  if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
          begin
               if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
               begin
                    xResult := True;
               end;
          end;
    end;
  end;
  if not xResult then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al leer la Tabla de Contenidos (TOC) - Pistas LBA');
      Error.ErrorCode := $00034300;
      //raise Error;
      MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], 'Pistas LBA');
    end;
  end;


//1º Paso: Obtener informacion del TOC
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := $324;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 10;
  SRBPacket.CDBByte[0] := SCSI_ReadTOC;
  SRBPacket.CDBByte[1] := 2;                  // SELECCIONAR FORMATO MSF
  SRBPacket.CDBByte[2] := 0;                  // FORMATO DE TOC/PMA/ATIP
	SRBPacket.CDBByte[6] := ReadIn_Track; // PISTA/SESION INICIAL
	SRBPacket.CDBByte[7] := $03;          // TAMAÑO TOC MSB
	SRBPacket.CDBByte[8] := $24;          // TAMAÑO TOC LSB
        SRBPacket.CDBByte[9] := $00;          // Control ($80 o $40)
  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    Result.StartTrack := Buffer[2];
    Result.EndTrack := Buffer[3];
    Result.TrackNumber := Buffer[3] - Buffer[2] + 1;
    x := 4;
    for tno := Result.StartTrack to Result.EndTrack do
    begin
      Result.TrackInfo[tno].Emphasis    := ((Buffer[x + 1] and $01) = $01);
      Result.TrackInfo[tno].DigitalCopy := ((Buffer[x + 1] and $02) = $02);
      Result.TrackInfo[tno].AudioData   := not((Buffer[x + 1] and $04) = $04);
      if (Buffer[x + 1] and $08) = $08 then
      begin
        Result.TrackInfo[tno].AudioChannel := 4;
      end
      else
      begin
        Result.TrackInfo[tno].AudioChannel := 2;
      end;
      Result.TrackInfo[tno].ADR := Buffer[x + 1] shr 4;
      Result.TrackInfo[tno].StartAddress.Minute := Buffer[x + 5];
      Result.TrackInfo[tno].StartAddress.Second := Buffer[x + 6];
      Result.TrackInfo[tno].StartAddress.Frame  := Buffer[x + 7];
      x := x + 8;
    end;
    xResult := False;

    if SRBPacket.SRB_TargetStat = STATUS_GOOD then xResult := True
    else
    begin
  	  if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
          begin
               if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
               begin
                    xResult := True;
               end;
          end;
    end;
  end;
  if not xResult then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al leer la Tabla de Contenidos (TOC) - Pistas MSF');
      Error.ErrorCode := $00034300;
      MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], 'Pistas MSF');
    end;
  end;
  
//2º Paso - Leer el Tiempo Total del Disco
  if xResult then
  begin
    //---Inicializacion de SCSI Request Block---
    pSRBPacket := @SRBPacket;
    PacketSize := SizeOf(SRBPacket);
    InitSRB(pSRBPacket, PacketSize);
    pBuffer := @Buffer;
    BufferSize := SizeOf(Buffer);
    InitSRB(pBuffer, BufferSize);
    //---Establece SCSI Request Block (SRB)---
    SRBPacket.SRB_HaId := HA_IDx;
    SRBPacket.SRB_Target := SCSI_IDx;
    SRBPacket.SRB_Lun := LUNx;
    SRBPacket.SRB_BufLen := 12;
    SRBPacket.SRB_BufPointer := pBuffer;
    SRBPacket.SRB_CDBLen := 10;
    SRBPacket.CDBByte[0] := SCSI_ReadTOC;
    SRBPacket.CDBByte[1] := 2;                    // SELECT MSF ADDRESS
    SRBPacket.CDBByte[2] := 0;                    // Formato TOC
    SRBPacket.CDBByte[6] := ReadOut_Track;        // NUMERO DE SESION
    SRBPacket.CDBByte[8] := 12;                   // LOAD TOC SIZE
    //---Ejecutamos Comando SCSI---
    xResult := ExecSCSICommand(pSRBPacket);
    if xResult then
    begin
      xResult := False;
      Result.TotalTime.Minute := Buffer[9];
      Result.TotalTime.Second := Buffer[10];
      Result.TotalTime.Frame  := Buffer[11];
      
      if SRBPacket.SRB_TargetStat = STATUS_GOOD then
      begin
	      xResult := True;
      end
      else
      begin
	      if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	      begin
	        if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	        begin
	          xResult := True;
                end;
	      end;
      end;
    end;
    if not xResult then
    begin
      if ErrorEnable then
      begin
        Error := ECDROMError.Create('Error al leer la Tabla de Contenidos (TOC) - Tiempo Total');
        Error.ErrorCode := $00034301;
        //raise Error;
        MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], '');
        //RequestSence(HA_IDx, SCSI_IDx, LUNx);
      end;
    end;
  end;
//3º Paso - Leer Sesiones
  if xResult then
  begin
    //---Inicializacion de SCSI Request Block---
    pSRBPacket := @SRBPacket;
    PacketSize := SizeOf(SRBPacket);
    InitSRB(pSRBPacket, PacketSize);
    pBuffer := @Buffer;
    BufferSize := SizeOf(Buffer);
    InitSRB(pBuffer, BufferSize);
    //---Establece SCSI Request Block (SRB)---
    SRBPacket.SRB_HaId := HA_IDx;
    SRBPacket.SRB_Target := SCSI_IDx;
    SRBPacket.SRB_Lun := LUNx;
    SRBPacket.SRB_BufLen := $800;
    SRBPacket.SRB_BufPointer := pBuffer;
    SRBPacket.SRB_CDBLen := 10;
    SRBPacket.CDBByte[0] := SCSI_ReadTOC;
    SRBPacket.CDBByte[1] := 2;                    // Selecciona formato MSF
    SRBPacket.CDBByte[2] := 2;                    // Formato FULL TOC
    SRBPacket.CDBByte[6] := 0;                    // NUMERO DE SESION
    SRBPacket.CDBByte[7] := $80;                  // MSB
    SRBPacket.CDBByte[8] := $00;                  // LSB

    //---Ejecutamos Comando SCSI---
    xResult := ExecSCSICommand(pSRBPacket);
    if xResult then
    begin
      xResult := False;
      Result.Sesiones := Buffer[3] - Buffer[2] + 1;
      Result.PrimeraSesion := Buffer[2];
      Result.UltimaSesion := Buffer[3];

      x := 4;
      for tno := Result.PrimeraSesion to Result.UltimaSesion+2 do
      begin
        Result.SesionesInfo[tno].Emphasis    := ((Buffer[x + 1] and $01) = $01);
        Result.SesionesInfo[tno].DigitalCopy := ((Buffer[x + 1] and $02) = $02);
        Result.SesionesInfo[tno].AudioData   := not((Buffer[x + 1] and $04) = $04);
        if (Buffer[x + 1] and $08) = $08 then Result.SesionesInfo[tno].AudioChannel := 4
        else Result.SesionesInfo[tno].AudioChannel := 2;
        Result.SesionesInfo[tno].ADR := Buffer[x + 1] shr 4;
        Result.SesionesInfo[tno].TNO := Buffer[x + 2];
        Result.SesionesInfo[tno].Point := Buffer[x + 3];
        Result.SesionesInfo[tno].AMin := Buffer[x + 4];
        Result.SesionesInfo[tno].ASec := Buffer[x + 5];
        Result.SesionesInfo[tno].AFrame := Buffer[x + 6];
        MSF.Minute := Buffer[x + 4];
        MSF.Second := Buffer[x + 5];
        MSF.Frame := Buffer[x + 6];
        Result.SesionesInfo[tno].ALBA := MSF2LBA(MSF);
        Result.SesionesInfo[tno].HOUR := Buffer[x + 7] shr 4;
        Result.SesionesInfo[tno].PHOUR := (Buffer[x + 7] shl 4) shr 4;
        Result.SesionesInfo[tno].PMIN := Buffer[x + 8];
        Result.SesionesInfo[tno].PSEC := Buffer[x + 9];
        Result.SesionesInfo[tno].PFRAME := Buffer[x + 10];
        MSF.Minute := Buffer[x + 8];
        MSF.Second := Buffer[x + 9];
        MSF.Frame := Buffer[x + 10];
        Result.SesionesInfo[tno].PLBA := MSF2LBA(MSF);
        x := x + 11;
      end;

      Result.SesionesInfo[Result.UltimaSesion+2].PLBA := MSF2LBA(Result.TotalTime);

      i := Result.StartTrack;      
      for tno := Result.PrimeraSesion to Result.UltimaSesion do
      begin
        // Vemos en que sesion esta cada pista
        while (MSF2LBA(Result.TrackInfo[i].StartAddress) < Result.SesionesInfo[tno+2].PLBA) and
              (i <= 99) do
        begin
              Result.TrackInfo[i].Sesion := tno;
              inc(i);
        end;
      end;

      if SRBPacket.SRB_TargetStat = STATUS_GOOD then
      begin
	      xResult := True;
      end
      else
      begin
	      if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	      begin
	        if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	        begin
	          xResult := True;
                end;
	      end;
      end;
    end;
    if not xResult then
    begin
      if ErrorEnable then
      begin
        Error := ECDROMError.Create('Error al leer la Tabla de Contenidos (TOC) - Sesiones');
        Error.ErrorCode := $00034301;
       // raise Error;
        MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], '');       
        //RequestSence(HA_IDx, SCSI_IDx, LUNx);
      end;
    end;
  end;

//3º Paso - Leer Sesiones
  if xResult then
  begin
    //---Inicializacion de SCSI Request Block---
    pSRBPacket := @SRBPacket;
    PacketSize := SizeOf(SRBPacket);
    InitSRB(pSRBPacket, PacketSize);
    pBuffer := @Buffer;
    BufferSize := SizeOf(Buffer);
    InitSRB(pBuffer, BufferSize);
    //---Establece SCSI Request Block (SRB)---
    SRBPacket.SRB_HaId := HA_IDx;
    SRBPacket.SRB_Target := SCSI_IDx;
    SRBPacket.SRB_Lun := LUNx;
    SRBPacket.SRB_BufLen := $800;
    SRBPacket.SRB_BufPointer := pBuffer;
    SRBPacket.SRB_CDBLen := 10;
    SRBPacket.CDBByte[0] := SCSI_ReadTOC;
    SRBPacket.CDBByte[1] := 2;                    // Selecciona formato MSF
    SRBPacket.CDBByte[2] := 2;                    // Formato FULL TOC
    SRBPacket.CDBByte[6] := 0;                    // NUMERO DE SESION
    SRBPacket.CDBByte[7] := $80;                  // MSB
    SRBPacket.CDBByte[8] := $00;                  // LSB
    SRBPacket.CDBByte[9] := $80;                  // CONTROL BYTE (SI PONEMOS ESTE VALOR DEVUELVE TODO!!!PERFECTO PARA LAS SESIONES Y TAL

    //---Ejecutamos Comando SCSI---
    xResult := ExecSCSICommand(pSRBPacket);
    if xResult then
    begin
      xResult := False;
      Result.Sesiones := Buffer[3] - Buffer[2] + 1;
      Result.PrimeraSesion := Buffer[2];
      Result.UltimaSesion := Buffer[3];

      for prueba := 0 to 2048 do
      begin
            Result.Matriz[prueba] := buffer[prueba];
      end;

      if SRBPacket.SRB_TargetStat = STATUS_GOOD then
      begin
	      xResult := True;
      end
      else
      begin
	      if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	      begin
	        if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	        begin
	          xResult := True;
                end;
	      end;
      end;
    end;
    if not xResult then
    begin
      if ErrorEnable then
      begin
        Error := ECDROMError.Create('Error al leer la Tabla de Contenidos (TOC) - Sesiones');
        Error.ErrorCode := $00034301;
       // raise Error;
        MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], '');       
        //RequestSence(HA_IDx, SCSI_IDx, LUNx);
      end;
    end;
  end;

  If Assigned(FOnReadTOCFinish) then
  begin
    FOnReadTOCFinish(Self);
  end;
end;

function TCDROM.ReadATIP(HA_IDx, SCSI_IDx, LUNx: byte): TATIP;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
  x: Byte;
begin
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := $30;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 10;
  SRBPacket.CDBByte[0] := SCSI_ReadATIP;
  SRBPacket.CDBByte[1] := 2;                  // SELECCIONAR FORMATO MSF
  SRBPacket.CDBByte[2] := $04;                // FORMATO DE TOC/PMA/ATIP
	SRBPacket.CDBByte[6] := 0;
	SRBPacket.CDBByte[7] := $00;          // TAMAÑO TOC MSB
	SRBPacket.CDBByte[8] := $30;          // TAMAÑO TOC LSB
  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    x := 4;
    Result.PotenciaEscritura := (Buffer[x+0] shr 4);
    Result.VelocidadRef := Buffer[x+0] and $07;
    Result.URU := (Buffer[x+1] and $40) = $40;
    Result.TipoDisco := Buffer[x+2] and $40;
    Result.SubTipoDisco := Buffer[x+2] shr $03;
    Result.A1Valido := (Buffer[x+2] and $01) = $01;
    Result.A2Valido := (Buffer[x+2] and $02) = $02;
    Result.A3Valido := (Buffer[x+2] and $03) = $03;
    Result.ATIPInicioLeadIn.Minute := Buffer[x+3];
    Result.ATIPInicioLeadIn.Second := Buffer[x+4];
    Result.ATIPInicioLeadIn.Frame := Buffer[x+6];
    Result.ATIPInicioLeadOut.Minute := Buffer[x+8];
    Result.ATIPInicioLeadOut.Second := Buffer[x+9];
    Result.ATIPInicioLeadOut.Frame := Buffer[x+10];
    Result.A1 := Bytes2Integer(0, Buffer[x+12], Buffer[x+13], Buffer[x+14]);
    Result.A2 := Bytes2Integer(0, Buffer[x+16], Buffer[x+17], Buffer[x+18]);
    Result.A3 := Bytes2Integer(0, Buffer[x+20], Buffer[x+21], Buffer[x+22]);
    Result.S4 := Bytes2Integer(0, Buffer[x+24], Buffer[x+25], Buffer[x+26]);

    xResult := False;

    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
    begin
      xResult := True;
    end
    else
    begin
  	  if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
          begin
               if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
               begin
                    xResult := True;
               end;
          end;
    end;
  end;
  if not xResult then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al leer ATIP');
      Error.ErrorCode := $00034300;
      //RequestSence(HA_IDx, SCSI_IDx, LUNx);
      raise Error;
    end;
  end;
end;

function TCDROM.ReadDiscInformation(HA_IDx, SCSI_IDx, LUNx: Byte): TCDInfo;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
begin
  //---Inicializacion de SCSI Request Block (SRB)---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer    := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId   := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun    := LUNx;

  SRBPacket.SRB_BufLen := 50;
  SRBPacket.SRB_BufPointer := pBuffer;

  SRBPacket.SRB_CDBLen := 10;
  SRBPacket.CDBByte[0] := SCSI_ReadDiscInformation;
  SRBPacket.CDBByte[7] := 0;                  //Tamaño Pagina - Byte Alto
  SRBPacket.CDBByte[8] := 50;                 //Tamaño Pagina - Byte Bajo
  SRBPacket.CDBByte[9] := $00;                //Control Byte

  xResult := ExecSCSICommand(pSRBPacket);

  if xResult then
  begin
        result.Borrable := Boolean((Buffer[2] shl 3) shr 7);
        case ((Buffer[2] shl 4) shr 6) of
          $00: result.EstadoUltimaSesion := us_Vacio;
          $01: result.EstadoUltimaSesion := us_Incompleta;
          $02: result.EstadoUltimaSesion := us_ReserDanyada;
          $03: result.EstadoUltimaSesion := us_Completada;
        end;

        case ((Buffer[2] shl 6) shr 6) of
          $00: result.EstadoDisco := ed_Vacio;
          $01: result.EstadoDisco := ed_Incompleto;
          $02: result.EstadoDisco := ed_Completo;
          $03: result.EstadoDisco := ed_Otros;
        end;

        result.NumeroPrimeraPista := Buffer[3];
        result.NumeroSesiones := (Buffer[9] shl 8) or Buffer[4];
        result.PrimeraPistaEnUltimaSesion := (Buffer[10] shl 8) or Buffer[5];
        result.UltimaPistaEnUltimaSesion := (Buffer[11] shl 8) or Buffer[6];

        result.DID_V := Boolean(Buffer[7] shr 7);
        result.DBC_V := Boolean((Buffer[7]shl 1) shr 7);
        result.URU := Boolean((Buffer[7]shl 2) shr 7);
        result.DBit := (Buffer[7]shl 5) shr 7;

        case ((Buffer[7]shl 6) shr 7) of
           $00: result.BGFormatoEstado := bg_NoFormateado;
           $01: result.BGFormatoEstado := bg_SinCompletar;
           $02: result.BGFormatoEstado := bg_FormatoEnProgreso;
           $03: result.BGFormatoEstado := bg_Completo;
        end;

        case (Buffer[8]) of
          $00: result.TipoDisco := td_CDROM;
          $01: result.TipoDisco := td_CDI;
          $02: result.TipoDisco := td_CDROMXAoDDCD;
          $03: result.TipoDisco := td_Indefinido;
          else result.TipoDisco := td_Indefinido;
        end;

        result.IDDisco := ((Buffer[12] shl 24) or
                           (Buffer[13] shl 16) or
                           (Buffer[14] shl 8) or
                           (Buffer[15]));

        result.InicioLeadInUltimaSesion := ((Buffer[16] shl 24) or
                                            (Buffer[17] shl 16) or
                                            (Buffer[18] shl 8) or
                                            (Buffer[19]));

        result.InicioLeadOut := ((Buffer[20] shl 24) or
                                 (Buffer[21] shl 16) or
                                 (Buffer[22] shl 8) or
                                 (Buffer[23]));

    // CREO QUE YA TA ARREGLADO....
        result.CodigoBarras := ((((( (((Buffer[24] shl 8) or
                                 (Buffer[25] shl 8)) or
                                 (Buffer[26] shl 8)) or
                                 (Buffer[27] shl 8)) or
                                 (Buffer[28] shl 8)) or
                                 (Buffer[29] shl 8)) or
                                 (Buffer[30] shl 8)) or
                                 (Buffer[31]) );

        result.NumOPC := Buffer[33];

    // FALTA IMPLEMENTAR UNA ESTRUCTURA COMO VIENE EN LA PAGINA
    // 208 (172) del PDF de MMC3, CON LAS ENTRADAS DEL "OPC"    

  end
  else
  begin 
       if ErrorEnable then
       begin
                Error := ECDROMError.Create('Error al pedir Informacion del CD');
                Error.ErrorCode := $00035a00;
                raise Error;
       end;
  end;
end;

//---Devuelve las caracteristicas del cdrom---
function TCDROM.ModeSense(HA_IDx, SCSI_IDx, LUNx: Byte): TCDROMInfo;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
  offset: Byte;
begin
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer    := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId   := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun    := LUNx;
  SRBPacket.SRB_BufLen := 38;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 10;
  SRBPacket.CDBByte[0] := SCSI_ModeSense;
  SRBPacket.CDBByte[1] := (LUNx shl 5) + $00; //sin Block Descriptor
  SRBPacket.CDBByte[2] := $2A;                //$2A = CD Capabilities and Mechanical Status Page
  SRBPacket.CDBByte[7] := 0;                  //Page Size - High Byte
  SRBPacket.CDBByte[8] := 38;                 //Page Size - Low Byte
  SRBPacket.CDBByte[9] := $00;                //Control Byte
  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    offset := 8 + Buffer[7];
    Result.SupportReadCDR       := ((Buffer[offset+2] and $01) = $01);  //check Read CD-R disc support
    Result.SupportReadCDRW      := ((Buffer[offset+2] and $02) = $02);  //check Read CD-RW disc support
    Result.SupportMethod2       := ((Buffer[offset+2] and $04) = $04);  //check Read Packet Wrote Disc support
    Result.SupportWriteCDR      := ((Buffer[offset+3] and $01) = $01);  //check Write CD-R disc support
    Result.SupportWriteCDRW     := ((Buffer[offset+3] and $02) = $02);  //check Write CD-RW disc support
    Result.SupportTestWrite     := ((Buffer[offset+3] and $04) = $04);  //check Test Write support
    Result.EnablePlayAudio      := ((Buffer[offset+4] and $01) = $01);  //check issue PlayAudio/ReadSubcode command enable
    Result.EnableComposite      := ((Buffer[offset+4] and $02) = $02);  //check Trasfer Mixed Audio and Video Data Stream enable
    Result.DigitalPort1         := ((Buffer[offset+4] and $04) = $04);  //check Digital Port1 (IEC958) support
    Result.DigitalPort2         := ((Buffer[offset+4] and $08) = $08);  //check Digital Port2 (IEC958) support
    Result.SupportMode2Form2    := ((Buffer[offset+4] and $10) = $10);  //check Read Mode2 Form2 disc support
    Result.SupportMode2Form1    := ((Buffer[offset+4] and $20) = $20);  //check Read Mode2 Form1 disc support
    Result.SupportMultiSession  := ((Buffer[offset+4] and $40) = $40);  //check Read MultiSession disc support
    Result.SupportReadCDDA      := ((Buffer[offset+5] and $01) = $01);  //check Read CDDA data support
    Result.AccurateCDDA         := ((Buffer[offset+5] and $02) = $02);  //check Repeated Read CD command (Assured Position) support
    Result.SupportReadSubcode   := ((Buffer[offset+5] and $04) = $04);  //check Transfer Subcode Data Block support
    Result.SubcodeECC           := ((Buffer[offset+5] and $08) = $08);  //check Error Correction of Subcode enable
    Result.SupportC2pointer     := ((Buffer[offset+5] and $10) = $10);  //check Read CD with C2 Pointer command support
    Result.SupportReadISRC      := ((Buffer[offset+5] and $20) = $20);  //check Read ISRC ID-code support
    Result.SupportReadUPC       := ((Buffer[offset+5] and $40) = $40);  //check Read UPC ID-code support
    Result.SupportReadBarCode   := ((Buffer[offset+5] and $80) = $80);  //check Read BarCode ID-code support
    Result.SupportLock          := ((Buffer[offset+6] and $01) = $01);  //check Lock Media by Prevent/Allow command support
    Result.LockStatus           := ((Buffer[offset+6] and $02) = $02);  //check Current Lock Status (true:prevent/false:allow)
    Result.PreventJumper        := ((Buffer[offset+6] and $04) = $04);  //check Prevent/Allow Jumper setting (true:prevent/false:allow)
    Result.SupportEject         := ((Buffer[offset+6] and $08) = $08);  //check Eject command support
    case (Buffer[offset+6] and $e0) of         //check Loading MEchanism Type
      $00:  Result.LoadingMechanismType := mt_Caddy;
      $20:  Result.LoadingMechanismType := mt_Tray;
      $40:  Result.LoadingMechanismType := mt_PopUp;
      $80:  Result.LoadingMechanismType := mt_DiscChanger;
      $a0:  Result.LoadingMechanismType := mt_CartridgeChanger;
      else  Result.LoadingMechanismType := mt_Reserved;
    end;
    Result.ChannelVolumeControl     := ((Buffer[offset+7] and $01) = $01);  //check Control Independent Channel Volume
    Result.ChannelMuteControl       := ((Buffer[offset+7] and $02) = $02);  //check Control Independent Channel Mute
    Result.SupportChangerDiscReport := ((Buffer[offset+7] and $04) = $04);  //check Report Changer Disc Present by Mechanism Status command support
    Result.SoftwareSlotSelection    := ((Buffer[offset+7] and $08) = $08);  //check Load/Unload command behavior of empty slot support
    Result.MaxReadSpeed       := Buffer[offset+8]  * $100 + Buffer[offset+9];
    Result.VolumeLevels       := Buffer[offset+10] * $100 + Buffer[offset+11];
    Result.DataBufferSize     := Buffer[offset+12] * $100 + Buffer[offset+13];
    Result.CurrentReadSpeed   := Buffer[offset+14] * $100 + Buffer[offset+15];
    Result.BCKpolarity  := ((Buffer[offset+17] and $02) = $02);   //check Bit Clock Polarity
    Result.LRCKform     := ((Buffer[offset+17] and $04) = $04);   //check L/R Clock Format
    Result.LSBfirst     := ((Buffer[offset+17] and $08) = $08);   //check Digital Data Format
    case (Buffer[offset+17] and $30) of         //check Bit Clock Length per Channel
      $00:  Result.BCKlength := 32;
      $10:  Result.BCKlength := 16;
      $20:  Result.BCKlength := 24;
      $30:  Result.BCKlength := 24;
    end;
    Result.MaxWriteSpeed      := Buffer[offset+18] * $100 + Buffer[offset+19];
    Result.CurrentWriteSpeed  := Buffer[offset+20] * $100 + Buffer[offset+21];
  end
  else
  begin
    Result.LoadingMechanismType := mt_Unknown;
    Result.DigitalPort1 := false;
    Result.DigitalPort2 := false;
    Result.MaxReadSpeed := 0;
    Result.CurrentReadSpeed := 0;
    Result.MaxWriteSpeed := 0;
    Result.CurrentWriteSpeed := 0;
    Result.DataBufferSize := 0;
    Result.ChannelVolumeControl := false;
    Result.ChannelMuteControl := false;
    Result.VolumeLevels := 0;
    Result.BCKpolarity := false;
    Result.LRCKform := false;
    Result.LSBfirst := false;
    Result.BCKlength := 0;
    Result.SupportWriteCDR := false;
    Result.SupportWriteCDRW := false;
    Result.SupportTestWrite := false;
    Result.SupportReadCDR := false;
    Result.SupportReadCDRW := false;
    Result.SupportMethod2 := false;
    Result.SupportMode2Form1 := false;
    Result.SupportMode2Form2 := false;
    Result.SupportMultiSession := false;
    Result.SupportReadISRC := false;
    Result.SupportReadUPC := false;
    Result.SupportReadBarCode := false;
    Result.SoftwareSlotSelection := false;
    Result.SupportChangerDiscReport := false;
    Result.SupportEject := false;
    Result.SupportLock := false;
    Result.LockStatus := false;
    Result.PreventJumper := false;
    Result.SupportC2pointer := false;
    Result.SupportReadCDDA := false;
    Result.AccurateCDDA := false;
    Result.SupportReadSubcode := false;
    Result.SubcodeECC := false;
    Result.EnablePlayAudio := false;
    Result.EnableComposite := false;
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al realizar Modo Sense');
      Error.ErrorCode := $00035a00;
      raise Error;
    end;
  end;
  If Assigned(FOnModeSenseFinish) then
  begin
    FOnModeSenseFinish(Self);
  end;
end;

//---Introduce las nuevos valores de correcion de errores---
function TCDROM.ModeSelect10_Errores(HA_IDx, SCSI_IDx, LUNx, Reintentos, Recuperar: Byte): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
begin
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer    := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);

  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId   := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun    := LUNx;
  SRBPacket.SRB_BufLen := 16;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 12;
  SRBPacket.CDBByte[0] := SCSI_ModeSelect;
  SRBPacket.CDBByte[1] := $10;                           //No salva la pagina en ROM
  SRBPacket.CDBByte[7] := 0;                         //Tamaño de la Pagina - Byte alto
  SRBPacket.CDBByte[8] := 16;                        //Tamaño de la Pagina - Byte bajo
  SRBPacket.CDBByte[9] := $00;

  Buffer[0] := 0;
  Buffer[1] := 0;
  Buffer[2] := 0;
  Buffer[3] := 0;
  Buffer[4] := 0;
  Buffer[5] := 0;
  Buffer[6] := 0;
  Buffer[7] := 0;

  Buffer[8] := $1;          // Codigo de la pagina de Correccion de Errores= $1
  Buffer[9] := 6;           // Longitud de la pagina
  Buffer[10]:= Recuperar;   // Tipo de correccion de errores
  Buffer[11]:= Reintentos;  // Numero de reintentos por hardware
  Buffer[12]:= 0;           // Reservado
  Buffer[13]:= 0;           // Reservado
  Buffer[14]:= 0;           // Reservado
  Buffer[15]:= 0;           // Reservado

{  Buffer[16]:= 0;           // Reintentos de escritura
  Buffer[17]:= 0;           // Reservado
  Buffer[18]:= 0;           // MSB TimeOut de R.Lectura
  Buffer[19]:= 0;           // LSB TimeOut de R.Lectura
                                                         }

  //---Ejecuta el Comando SCSI---
  Result := False;
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
      if SRBPacket.SRB_TargetStat = STATUS_GOOD then
      begin
	   Result := True;
      end
      else
      begin
           if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
           begin
                if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	           Result := True;
           end;
      end;
  end
  else
  begin
       if ErrorEnable then
       begin
            result := False;
            Error := ECDROMError.Create('Error al realizar la peticion de Errores');
            Error.ErrorCode := $00035a00;
            //raise Error;
       end;
  end;

  if Assigned(FOnModeSenseFinish) then FOnModeSenseFinish(Self);
end;

function TCDROM.ModeSelectEscribir(HA,SCSI,LUN: byte; Parametros: TModeSelectEscribir): boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
  PS1,PS2,PS3,PS4: byte;
  i: integer;
begin
  PS4 := ($ff000000 and Parametros.TamPaquete) shr 24;
  PS3 := ($00ff0000 and Parametros.TamPaquete) shr 16;
  PS2 := ($0000ff00 and Parametros.TamPaquete) shr 8;
  PS1 := ($000000ff and Parametros.TamPaquete);

  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer    := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);

  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId   := HA;
  SRBPacket.SRB_Target := SCSI;
  SRBPacket.SRB_Lun    := LUN;
  SRBPacket.SRB_Flags  := SRB_DIR_OUT;
  SRBPacket.SRB_BufLen := 64;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 12;
  SRBPacket.CDBByte[0] := SCSI_ModeSelect;
  SRBPacket.CDBByte[1] := $10;                      
  SRBPacket.CDBByte[7] := 0;                         //Tamaño de la Pagina - Byte alto
  SRBPacket.CDBByte[8] := 80;                       //Tamaño de la Pagina - Byte bajo
  SRBPacket.CDBByte[9] := 0;

  Buffer[0] := 0;
  Buffer[1] := 0;
  Buffer[2] := 0;
  Buffer[3] := 0;

  Buffer[4] := 0;
  Buffer[5] := 0;
  Buffer[6] := 0;
  Buffer[7] := 0;
  Buffer[8] := 0;
  Buffer[9] := 0;
  Buffer[10] := 0;
  Buffer[11] := 0;

  Buffer[12] := 5;            // Codigo Pagina
  Buffer[13] := $32;           // Longitud de la pagina
  Buffer[14]:= Parametros.Op1;
  Buffer[15]:= Parametros.Op2;
  Buffer[17]:= Parametros.TipoBloque;
  Buffer[18]:= Parametros.LinkSize;
  Buffer[19]:= 0;
  Buffer[20]:= Parametros.IAC;
  Buffer[21]:= Parametros.FormatoSesion;
  Buffer[22]:= 0;
  Buffer[23]:= PS4;       
  Buffer[24]:= PS3;
  Buffer[25]:= PS2;
  Buffer[26]:= PS1;
  Buffer[27]:= Hi(Parametros.DuracionPausa);
  Buffer[28]:= Lo(Parametros.DuracionPausa);

  for i := 0 to 15 do
      Buffer[29+i] := Ord(Parametros.MCN[i+1]);
  for i := 0 to 15 do
      Buffer[45+i] := Ord(Parametros.ISRC[i+1]);
  for i := 0 to 3 do
      Buffer[61+i] := Parametros.SubHeader[i];
  for i := 0 to 3 do
      Buffer[64+i] := Parametros.VendorSpecific[i];


  //---Ejecuta el Comando SCSI---
  Result := False;
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
      if SRBPacket.SRB_TargetStat = STATUS_GOOD then
      begin
	   Result := True;
      end
      else
      begin
           if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
           begin
                if RequestSence(HA, SCSI, LUN) = 0 then
	           Result := True;
           end;
      end;
  end
  else
  begin
       if ErrorEnable then
       begin
            result := False;
            Error := ECDROMError.Create('Error al realizar la peticion de Paginas de Escritura');
            Error.ErrorCode := $00035a00;
            //raise Error;

            MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], '');
       end;
  end;

  if Assigned(FOnModeSenseFinish) then FOnModeSenseFinish(Self);
end;

function TCDROM.SendCuesheet(HA,SCSI,LUN: byte; Parametros: pointer; pistas: integer): boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
begin
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer    := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);

  Move(Parametros^,pBuffer^,8*pistas);

  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId   := HA;
  SRBPacket.SRB_Target := SCSI;
  SRBPacket.SRB_Lun    := LUN;
  SRBPacket.SRB_Flags  := SRB_DIR_OUT;
  SRBPacket.SRB_BufLen := 8*Pistas;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 10;
  SRBPacket.CDBByte[0] := SCSI_SendCuesheet;
  SRBPacket.CDBByte[1] := 0;
  SRBPacket.CDBByte[7] := 0;                   //Tamaño de la Pagina - Byte alto
  SRBPacket.CDBByte[8] := 8*Pistas;            //Tamaño de la Pagina - Byte bajo
  SRBPacket.CDBByte[9] := $00;


  //---Ejecuta el Comando SCSI---
  Result := False;
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
      if SRBPacket.SRB_TargetStat = STATUS_GOOD then
      begin
	   Result := True;
      end
      else
      begin
           if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
           begin
                if RequestSence(HA, SCSI, LUN) = 0 then
	           Result := True;
           end;
      end;
  end
  else
  begin
       if ErrorEnable then
       begin
            result := False;
            Error := ECDROMError.Create('Error al realizar SendCuesheet');
            Error.ErrorCode := $00035a00;
            //raise Error;
            MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], '');
       end;
  end;

  if Assigned(FOnModeSenseFinish) then FOnModeSenseFinish(Self);
end;

function TCDROM.ModeSense_Errores(HA_IDx, SCSI_IDx, LUNx, Reintentos: Byte): TArray;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
  i      : integer;
begin
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer    := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);

  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId   := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun    := LUNx;
  SRBPacket.SRB_BufLen := 30;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 12;
  SRBPacket.CDBByte[0] := SCSI_ModeSense;
  SRBPacket.CDBByte[1] := (LUNx shl 5) + $00; //sin Block Descriptor
  SRBPacket.CDBByte[2] := $1;                //$2A = CD Capabilities and Mechanical Status Page
  SRBPacket.CDBByte[7] := 0;                  //Page Size - High Byte
  SRBPacket.CDBByte[8] := 30;                 //Page Size - Low Byte
  SRBPacket.CDBByte[9] := $00;                //Control Byte

  //---Ejecuta el Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
      if SRBPacket.SRB_TargetStat = STATUS_GOOD then
      begin
           for i := 0 to 15 do
               result[i] := buffer[i];
      end
      else
      begin
           if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
           begin
                if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
                begin
                     for i := 0 to 15 do
                         result[i] := buffer[i];
                end;
           end;
      end;
  end
  else
  begin
       if ErrorEnable then
       begin
            Error := ECDROMError.Create('Error al realizar modo sense de correccion de errores');
            Error.ErrorCode := $00035a00;
            raise Error;
       end;
  end;

  if Assigned(FOnModeSenseFinish) then
  begin
    FOnModeSenseFinish(Self);
  end;
end;

function TCDROM.BorrarCDRW(HA_IDx, SCSI_IDx, LUNx: byte; TipoBorrado: TBorradoCDRW; PistaOsector: integer): Boolean;
var
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
begin
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer    := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);

  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId   := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun    := LUNx;
  SRBPacket.SRB_BufLen := 30;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 12;
  SRBPacket.CDBByte[0] := SCSI_BLANK;
  SRBPacket.CDBByte[1] := ($1 shl 4) + byte(TipoBorrado);
  SRBPacket.CDBByte[2] := Hi(Hi(PistaOsector));
  SRBPacket.CDBByte[3] := Lo(Hi(PistaOsector));
  SRBPacket.CDBByte[4] := Hi(Lo(PistaOsector));
  SRBPacket.CDBByte[5] := Lo(Lo(PistaOsector));

  //---Ejecuta el Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);

  if Assigned(FOnModeSenseFinish) then
  begin
    FOnModeSenseFinish(Self);
  end; 
end;

function TCDROM.ReadCDMSF(HA_IDx, SCSI_IDx, LUNx: Byte; TopMSF, EndMSF: TMSF; pCDDABuffer: Pointer): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  xResult: Boolean;
  TopLBA, EndLBA, Size: Integer;
begin
  Result := False;
  //---parameter convert---
  TopLBA := MSF2LBA(TopMSF);
  EndLBA := MSF2LBA(EndMSF);
  Size := EndLBA - TopLBA;
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := FrameByte * Size;
  SRBPacket.SRB_BufPointer := pCDDABuffer;
  SRBPacket.SRB_CDBLen := 12;
  SRBPacket.CDBByte[0] := SCSI_ReadCDMSF;
	SRBPacket.CDBByte[3] := TopMSF.Minute; // Start Minute
	SRBPacket.CDBByte[4] := TopMSF.Second; // Start Second
	SRBPacket.CDBByte[5] := TopMSF.Frame;  // Start Frame
	SRBPacket.CDBByte[6] := EndMSF.Minute; // End Minute
	SRBPacket.CDBByte[7] := EndMSF.Second; // End Second
	SRBPacket.CDBByte[8] := EndMSF.Frame;  // End Frame
	SRBPacket.CDBByte[9] := $10;           // Transfer User Data Only
  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
    begin
	    Result := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result := True;
        end;
      end;
	  end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al leer en formato MSF.');
      Error.ErrorCode := $0003b900;
      raise Error;
    end;
  end;
  if Assigned(FOnReadCDMSFFinish) then
  begin
    FOnReadCDMSFFinish(Self);
  end;
end;

function TCDROM.ReadCD(HA_IDx, SCSI_IDx, LUNx: Byte; TopLBA, EndLBA: Integer; T_Sector: Byte; Subcanal: TTipoSubcanal; pCDDABuffer: Pointer): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  xResult: Boolean;
  Size: Integer;
  LBA0, LBA1, LBA2, LBA3, Size0, Size1, Size2: Byte;
  SCanal: Integer;
begin
  Result := False;
  //---Conversion de parametros---
  Size := EndLBA - TopLBA;
  LBA3 := ($ff000000 and TopLBA) shr 24;
  LBA2 := ($00ff0000 and TopLBA) shr 16;
  LBA1 := ($0000ff00 and TopLBA) shr 8;
  LBA0 := ($000000ff and TopLBA);
  Size2:= ($00ff0000 and Size) shr 16;
  Size1:= ($0000ff00 and Size) shr 8;
  Size0:= ($000000ff and Size);
  case Subcanal of
     sb_Ninguno: SCanal := 0;
     sb_RAW: SCanal := 96;
     sb_Q: SCanal := 16;
     sb_PW: SCanal := 96;
     else SCanal := 0;
  end;
  //---Inicializacion del SRB (SCSI Request Block)---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  //---Establecer SRB (SCSI Request Block)---
  SRBPacket.SRB_HaId   := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun    := LUNx;
  SRBPacket.SRB_BufLen := (FrameByte+SCanal) * (Size);  // Calcula el tamaño del Buffer para extraer.
  SRBPacket.SRB_BufPointer := pCDDABuffer;   // Puntero al Buffer
  SRBPacket.SRB_CDBLen := 12;                // Longitud del CDB
	SRBPacket.CDBByte[0] := SCSI_ReadCD; // Comando a realizar
	SRBPacket.CDBByte[1] := 0;           // Tipo de Sector esperado
	SRBPacket.CDBByte[2] := LBA3;        // LBA Inicio HH
	SRBPacket.CDBByte[3] := LBA2;        // LBA Inicio HL
	SRBPacket.CDBByte[4] := LBA1;        // LBA Inicio LH
	SRBPacket.CDBByte[5] := LBA0;        // LBA Inicio LL
	SRBPacket.CDBByte[6] := Size2;       // Read Size HL
	SRBPacket.CDBByte[7] := Size1;       // Read Size LH
	SRBPacket.CDBByte[8] := Size0;       // Read Size LL
        { 7: Cabecera Sync, 6,5: Cabecera, 4: Datos (2048 bytes), 3: EDC/ECC, 2,1: Error Flag, 0: Reservado }
        SRBPacket.CDBByte[9] := T_Sector;    // Modo de extraccion $F8=RAW 2352 bytes $78=2336 bytes $10=2048 bytes
        SRBPacket.CDBByte[10]:= Byte(SubCanal); // Subcanal $1=RAW P-W 96 bytes, $2=Subcanal Q: 16 bytes, $4=P-W R-W: 96 bytes

  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
       if SRBPacket.SRB_TargetStat = STATUS_GOOD then Result := True
       else
	   begin
	        if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	        begin
	             if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
                          Result := True;
                end;
	   end;
  end;

  if not Result then
  begin
       if ErrorEnable then
       begin
            ServiceAbort(HA_IDx, pSRBPacket);
            Error := ECDROMError.Create('Ha ocurrido un error al realizar el comando de lectura.');
            Error.ErrorCode := $0003be00;
            //raise Error;
            MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], '');
       end;
  end;

  if Assigned(FOnReadCDFinish) then FOnReadCDFinish(Self);
end;

function TCDROM.Write10(HA,SCSI,LUN: byte; TopLBA: Integer; Cantidad: word; pBuffer: Pointer): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  xResult: Boolean;
  LBA0, LBA1, LBA2, LBA3: Byte;
begin
  Result := False;
  //---Conversion de parametros---
  LBA3 := ($ff000000 and TopLBA) shr 24;
  LBA2 := ($00ff0000 and TopLBA) shr 16;
  LBA1 := ($0000ff00 and TopLBA) shr 8;
  LBA0 := ($000000ff and TopLBA);

  //---Inicializacion del SRB (SCSI Request Block)---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  //---Establecer SRB (SCSI Request Block)---
  SRBPacket.SRB_HaId   := HA;
  SRBPacket.SRB_Target := SCSI;
  SRBPacket.SRB_Lun    := LUN;
  SRBPacket.SRB_Flags  := SRB_DIR_OUT;
  SRBPacket.SRB_BufLen := (FrameByte * Cantidad);  // Calcula el tamaño del Buffer para extraer.
  SRBPacket.SRB_BufPointer := pBuffer;   // Puntero al Buffer
  SRBPacket.SRB_CDBLen := 10;                // Longitud del CDB
	SRBPacket.CDBByte[0] := SCSI_Write10; // Comando a realizar
	SRBPacket.CDBByte[1] := 0;
	SRBPacket.CDBByte[2] := LBA3;        // LBA Inicio HH
	SRBPacket.CDBByte[3] := LBA2;        // LBA Inicio HL
	SRBPacket.CDBByte[4] := LBA1;        // LBA Inicio LH
	SRBPacket.CDBByte[5] := LBA0;        // LBA Inicio LL
	SRBPacket.CDBByte[6] := 0;
	SRBPacket.CDBByte[7] := hi(Cantidad);
	SRBPacket.CDBByte[8] := lo(Cantidad);
        SRBPacket.CDBByte[9] := 0;

  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
       if SRBPacket.SRB_TargetStat = STATUS_GOOD then Result := True
       else
	   begin
	        if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	        begin
	             if RequestSence(HA, SCSI, LUN) = 0 then
                          Result := True;
                end;
	   end;
  end;

  if not Result then
  begin
       if ErrorEnable then
       begin
            //ServiceAbort(HA, pSRBPacket);
            Error := ECDROMError.Create('Ha ocurrido un error al realizar el comando de grabacion.');
            Error.ErrorCode := $0003be00;
            //raise Error;
            MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], '');
       end;
  end;

  if Assigned(FOnReadCDFinish) then FOnReadCDFinish(Self);
end;

function TCDROM.SynchronizeCache(HA,SCSI,LUN: byte; LBA: integer; Cantidad: word): boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  xResult: Boolean;
  LBA0, LBA1, LBA2, LBA3: Byte;
begin
  Result := False;
  //---Conversion de parametros---
  LBA3 := ($ff000000 and LBA) shr 24;
  LBA2 := ($00ff0000 and LBA) shr 16;
  LBA1 := ($0000ff00 and LBA) shr 8;
  LBA0 := ($000000ff and LBA);

  //---Inicializacion del SRB (SCSI Request Block)---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  //---Establecer SRB (SCSI Request Block)---
  SRBPacket.SRB_HaId   := HA;
  SRBPacket.SRB_Target := SCSI;
  SRBPacket.SRB_Lun    := LUN;
  SRBPacket.SRB_BufLen := 0;
  SRBPacket.SRB_BufPointer := nil;   // Puntero al Buffer
  SRBPacket.SRB_CDBLen := 10;                // Longitud del CDB
	SRBPacket.CDBByte[0] := SCSI_SynchronizeCache; // Comando a realizar
	SRBPacket.CDBByte[1] := 2;           // $2: Actua inmediatamente
	SRBPacket.CDBByte[2] := LBA3;        // LBA Inicio HH
	SRBPacket.CDBByte[3] := LBA2;        // LBA Inicio HL
	SRBPacket.CDBByte[4] := LBA1;        // LBA Inicio LH
	SRBPacket.CDBByte[5] := LBA0;        // LBA Inicio LL
	SRBPacket.CDBByte[6] := 0;
	SRBPacket.CDBByte[7] := Hi(Cantidad);
	SRBPacket.CDBByte[8] := Lo(Cantidad);
        SRBPacket.CDBByte[9] := 0;

  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
       if SRBPacket.SRB_TargetStat = STATUS_GOOD then Result := True
       else
	   begin
	        if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	        begin
	             if RequestSence(HA, SCSI, LUN) = 0 then
                          Result := True;
                end;
	   end;
  end;

  if not Result then
  begin
       if ErrorEnable then
       begin
            ServiceAbort(HA, pSRBPacket);
            Error := ECDROMError.Create('Ha ocurrido un error al realizar el comando de sincronizar cache');
            Error.ErrorCode := $0003be00;
            raise Error;
            MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], '');
       end;
  end;

  if Assigned(FOnReadCDFinish) then FOnReadCDFinish(Self);
end;

function TCDROM.CloseSessionTrack(HA,SCSI,LUN: byte; Sesion: boolean; Pista: boolean; nSesionPista: word): boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  xResult: Boolean;
  S,P: byte;
begin
  Result := False;
  S := 0;
  P := 0;
  if Sesion then S := 1;
  if Pista  then P := 1;
  
  //---Inicializacion del SRB (SCSI Request Block)---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  //---Establecer SRB (SCSI Request Block)---
  SRBPacket.SRB_HaId   := HA;
  SRBPacket.SRB_Target := SCSI;
  SRBPacket.SRB_Lun    := LUN;
  SRBPacket.SRB_BufLen := 0;
  SRBPacket.SRB_BufPointer := nil;           // Puntero nulo de buffer
  SRBPacket.SRB_CDBLen := 10;                // Longitud del CDB
	SRBPacket.CDBByte[0] := SCSI_CloseSessionTrack; // Comando a realizar
	SRBPacket.CDBByte[1] := 1;           // $1: Actua inmediatamente
	SRBPacket.CDBByte[2] := (S shl 1) or P; // Sesion Pista
	SRBPacket.CDBByte[3] := 0;
	SRBPacket.CDBByte[4] := Hi(nSesionPista);        // MSB nº Sesion/Pista
	SRBPacket.CDBByte[5] := Lo(nSesionPista);        // LSB nº Sesion/Pista
	SRBPacket.CDBByte[6] := 0;
	SRBPacket.CDBByte[7] := 0;
	SRBPacket.CDBByte[8] := 0;
        SRBPacket.CDBByte[9] := 0;

  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
       if SRBPacket.SRB_TargetStat = STATUS_GOOD then Result := True
       else
	   begin
	        if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	        begin
	             if RequestSence(HA, SCSI, LUN) = 0 then
                          Result := True;
                end;
	   end;
  end;

  if not Result then
  begin
       if ErrorEnable then
       begin
            ServiceAbort(HA, pSRBPacket);
            Error := ECDROMError.Create('Ha ocurrido un error al realizar el comando de cerrar pista/sesion');
            Error.ErrorCode := $0003be00;
            raise Error;
            MensajeSenseKey(SRBPacket.SenseArea[2]and $0f, SRBPacket.SenseArea[12],SRBPacket.SenseArea[13], '');
       end;
  end;

  if Assigned(FOnReadCDFinish) then FOnReadCDFinish(Self);
end;

//------------------
//Funciones de Audio
//------------------
function TCDROM.PlayAudioMSF(HA_IDx, SCSI_IDx, LUNx: Byte; TopMSF, EndMSF: TMSF): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
begin
  Result := False;
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := 0;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 10;
  SRBPacket.CDBByte[0] := SCSI_PlayAudioMSF;
	SRBPacket.CDBByte[3] := TopMSF.Minute; // Start Minute
	SRBPacket.CDBByte[4] := TopMSF.Second; // Start Second
	SRBPacket.CDBByte[5] := TopMSF.Frame;  // Start Frame
	SRBPacket.CDBByte[6] := EndMSF.Minute; // End Minute
	SRBPacket.CDBByte[7] := EndMSF.Second; // End Second
	SRBPacket.CDBByte[8] := EndMSF.Frame;  // End Frame
  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
    begin
	    Result := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result := True;
        end;
	    end;
	  end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al intentar reproducir la pista de audio."');
      Error.ErrorCode := $00034700;
      raise Error;
    end;
  end;
  If Assigned(FOnPlayAudioMSFFinish) then
  begin
    FOnPlayAudioMSFFinish(Self);
  end;
end;

function TCDROM.PlayAudioTrack(HA_IDx, SCSI_IDx, LUNx, TopTrack, EndTrack: Byte): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
begin
  Result := False;
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := 0;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 10;
  SRBPacket.CDBByte[0] := SCSI_PlayAudioTrack;
	SRBPacket.CDBByte[4] := TopTrack;   // Start Track
	SRBPacket.CDBByte[5] := 0;          // Start Index
	SRBPacket.CDBByte[7] := EndTrack;   // End Track
	SRBPacket.CDBByte[8] := 1;          // End Index
	SRBPacket.CDBByte[9] := 0;          // Control
  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
    begin
	    Result := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result := True;
        end;
	    end;
	  end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al intentar reproducir la pista de audio."');
      Error.ErrorCode := $00034800;
      raise Error;
    end;
  end;
  If Assigned(FOnPlayAudioTrackFinish) then
  begin
    FOnPlayAudioTrackFinish(Self);
  end;
end;

function TCDROM.PauseResume(HA_IDx, SCSI_IDx, LUNx: Byte; Resume: Boolean): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
begin
  Result := False;
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := 0;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 10;
  SRBPacket.CDBByte[0] := SCSI_PauseResume;
  if Resume then
  begin
  	SRBPacket.CDBByte[8] := $01;  // Resume Pause
  end
  else
  begin
  	SRBPacket.CDBByte[8] := $00;  // Pause
  end;
  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
    begin
	    Result := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result := True;
        end;
	    end;
	  end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al intentar pausar la pista de audio."');
      Error.ErrorCode := $00034b00;
      raise Error;
    end;
  end;
  If Assigned(FOnPauseResumeFinish) then
  begin
    FOnPauseResumeFinish(Self);
  end;
end;

function TCDROM.PlayAudio(HA_IDx, SCSI_IDx, LUNx: Byte; TopLBA, EndLBA: Integer): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
  Size: Longword;
  LBA0, LBA1, LBA2, LBA3, Size0, Size1, Size2, Size3: Byte;
begin
  Result := False;
  //---parameter convert---
  Size := EndLBA - TopLBA;
  //---parameter convert---
  LBA3 := ($ff000000 and TopLBA) shr 24;
  LBA2 := ($00ff0000 and TopLBA) shr 16;
  LBA1 := ($0000ff00 and TopLBA) shr 8;
  LBA0 := ($000000ff and TopLBA);
  Size3 := ($ff000000 and Size) shr 24;
  Size2 := ($00ff0000 and Size) shr 16;
  Size1 := ($0000ff00 and Size) shr 8;
  Size0 := ($000000ff and Size);
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := 0;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 12;
  SRBPacket.CDBByte[0] := SCSI_PlayAudio;
	SRBPacket.CDBByte[2] := LBA3;   // Start LBA HH
	SRBPacket.CDBByte[3] := LBA2;   // Start LBA HL
	SRBPacket.CDBByte[4] := LBA1;   // Start LBA LH
	SRBPacket.CDBByte[5] := LBA0;   // Start LBA LL
	SRBPacket.CDBByte[6] := Size3;  // Read Size HH
	SRBPacket.CDBByte[7] := Size2;  // Read Size HL
	SRBPacket.CDBByte[8] := Size1;  // Read Size LH
	SRBPacket.CDBByte[9] := Size0;  // Read Size LL
  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
    begin
	    Result := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result := True;
        end;
	    end;
	  end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('No se ha podido reproducir la pista audio seleccionada.');
      Error.ErrorCode := $0003a500;
      raise Error;
    end;
  end;
  If Assigned(FOnPlayAudioFinish) then
  begin
    FOnPlayAudioFinish(Self);
  end;
end;

//---------------------------
//CD-ROM Funciones Opcionales
//---------------------------
function TCDROM.SetStartStopUnit(HA_IDx, SCSI_IDx, LUNx: Byte; Eject: Boolean; MotorStop: Boolean): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
begin
  Result := False;
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := $ff;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 6;
  SRBPacket.CDBByte[0] := SCSI_StartStopUnit;
  if Eject then
  begin
		SRBPacket.CDBByte[4] := $02;                // Eject Media
  end
  else
  begin
    if MotorStop then
    begin
  		SRBPacket.CDBByte[4] := $00;                // Stop Motor
    end
    else
    begin
  		SRBPacket.CDBByte[4] := $03;                // Load Media
    end;
  end;
  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
    begin
	    Result := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result := True;
        end;
      end;
	  end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al intentar arrancar/parar la unidad');
      Error.ErrorCode := $00031b00;
      raise Error;
    end;
  end;
  If Assigned(FOnSetStartStopUnitFinish) then
  begin
    FOnSetStartStopUnitFinish(Self);
  end;
end;

function TCDROM.SetCDSpeed(HA_IDx, SCSI_IDx, LUNx: Byte; ReadSpeed, WriteSpeed: Word): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
  Speed0, Speed1: Byte;
  Escrit0, Escrit1: Byte;
begin
  Result := False;
  //---Conversion de parametros---
  Speed0 := (ReadSpeed and $ff00) shr 8;
  Speed1 := (ReadSpeed and $00ff);
  Escrit0 := (WriteSpeed and $ff00) shr 8;
  Escrit1 := (WriteSpeed and $00ff);
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece SCSI Request Block (SRB)---
  SRBPacket.SRB_HaId := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun := LUNx;
  SRBPacket.SRB_BufLen := $ff;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 12;
  SRBPacket.CDBByte[0] := SCSI_SetCDSpeed;
        SRBPacket.CDBByte[1] := 0;                  // 0=CAV, 1= CLV    
	SRBPacket.CDBByte[2] := Speed0;             // Velocidad Lectura H
	SRBPacket.CDBByte[3] := Speed1;             // Velocidad Lectura L
	SRBPacket.CDBByte[4] := Escrit0;            // Velocidad Escritura H
	SRBPacket.CDBByte[5] := Escrit1;            // Velocidad Escritura L
  //---Ejecutamos Comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);

  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
    begin
	    Result := True;
	  end
	  else
	  begin
	    if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
	    begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
	      begin
	        Result := True;
        end;
	    end;
	  end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := ECDROMError.Create('Error al establecer la nueva velocidad de lectura.');
      Error.ErrorCode := $0003bb00;
     // raise Error;
    end;
  end;
  If Assigned(FOnSetCDSpeedFinish) then
  begin
    FOnSetCDSpeedFinish(Self);
  end;
end;

function TCDROM.LoadUnload(HA_IDx, SCSI_IDx, LUNx: Byte; Cargar: Boolean): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
  Carga: byte;
begin
  Result := False;
  //---Parametros---
  if Cargar then Carga := $3 else Carga := $2;
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece el Bloque SCSI---
  SRBPacket.SRB_HaId   := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun    := LUNx;
  SRBPacket.SRB_BufLen := $ff;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 12;
        SRBPacket.CDBByte[0] := SCSI_LoadUnloadCD;
        SRBPacket.CDBByte[1] := $1;
	SRBPacket.CDBByte[4] := Carga;
	SRBPacket.CDBByte[8] := $0;
  //---Ejecuta el comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
       Result := True
    else
    begin
         if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
         begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
                 Result := True;
         end;
    end;
  end;

  if not Result then
  begin
       if ErrorEnable then
       begin
            Error := ECDROMError.Create('Error al abrir bandeja.');
            Error.ErrorCode := $0003bb00;
            raise Error;
       end;
  end;

  if Assigned(FOnSetCDSpeedFinish) then
  begin
       FOnSetCDSpeedFinish(Self);
  end;
end;

function TCDROM.SendEvent(HA_IDx, SCSI_IDx, LUNx: Byte): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
begin
  Result := False;
  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  //---Establece el Bloque SCSI---
  SRBPacket.SRB_HaId   := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun    := LUNx;
  SRBPacket.SRB_BufLen := $ff;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 12;
        SRBPacket.CDBByte[0] := $5D;
        SRBPacket.CDBByte[1] := $1;
	SRBPacket.CDBByte[8] := 0;
        SRBPacket.CDBByte[9] := 0;
	SRBPacket.CDBByte[11] := $0;
  //---Ejecuta el comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
       Result := True
    else
    begin
         if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
         begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
                 Result := True;
         end;
    end;
  end;

  if not Result then
  begin
       if ErrorEnable then
       begin
            Error := ECDROMError.Create('Error al enviar evento.');
            Error.ErrorCode := $0003bb00;
            raise Error;
       end;
  end;

  if Assigned(FOnSetCDSpeedFinish) then
  begin
       FOnSetCDSpeedFinish(Self);
  end;
end;

function TCDROM.SetReadAhead(HA_IDx, SCSI_IDx, LUNx: Byte; LBAIndice, LBACache: integer): Boolean;
var
  Error: ECDROMError;
  SRBPacket: TSRB_ExecSCSICmd;
  pSRBPacket: PSRB_ExecSCSICmd;
  PacketSize: Longword;
  Buffer: TSRBBuffer;
  pBuffer: PSRBBuffer;
  BufferSize: Longword;
  xResult: Boolean;
  LBA0, LBA1, LBA2, LBA3: byte;
  LBAC0, LBAC1, LBAC2, LBAC3: byte;
begin
  Result := False;
  //---Conversion de parametros---
  LBA3 := ($ff000000 and LBAIndice) shr 24;
  LBA2 := ($00ff0000 and LBAIndice) shr 16;
  LBA1 := ($0000ff00 and LBAIndice) shr 8;
  LBA0 := ($000000ff and LBAIndice);

  LBAC3 := ($ff000000 and LBACache) shr 24;
  LBAC2 := ($00ff0000 and LBACache) shr 16;
  LBAC1 := ($0000ff00 and LBACache) shr 8;
  LBAC0 := ($000000ff and LBACache);

  //---Inicializacion de SCSI Request Block---
  pSRBPacket := @SRBPacket;
  PacketSize := SizeOf(SRBPacket);
  InitSRB(pSRBPacket, PacketSize);
  pBuffer := @Buffer;
  BufferSize := SizeOf(Buffer);
  InitSRB(pBuffer, BufferSize);
  
  //---Establece el Bloque SCSI---
  SRBPacket.SRB_HaId   := HA_IDx;
  SRBPacket.SRB_Target := SCSI_IDx;
  SRBPacket.SRB_Lun    := LUNx;
  SRBPacket.SRB_BufLen := $ff;
  SRBPacket.SRB_BufPointer := pBuffer;
  SRBPacket.SRB_CDBLen := 12;
        SRBPacket.CDBByte[0] := SCSI_SetReadAhead;
        SRBPacket.CDBByte[1] := 0;
        SRBPacket.CDBByte[2] := LBA3;
        SRBPacket.CDBByte[3] := LBA2;
	SRBPacket.CDBByte[4] := LBA1;
        SRBPacket.CDBByte[5] := LBA0;
        SRBPacket.CDBByte[6] := LBAC3;
        SRBPacket.CDBByte[7] := LBAC2;
        SRBPacket.CDBByte[8] := LBAC1;
        SRBPacket.CDBByte[9] := LBAC0;
	SRBPacket.CDBByte[11] := $0;

  //---Ejecuta el comando SCSI---
  xResult := ExecSCSICommand(pSRBPacket);
  if xResult then
  begin
    if SRBPacket.SRB_TargetStat = STATUS_GOOD then
       Result := True
    else
    begin
         if SRBPacket.SRB_TargetStat = STATUS_CHKCOND then
         begin
	      if RequestSence(HA_IDx, SCSI_IDx, LUNx) = 0 then
                 Result := True;
         end;
    end;
  end;

  if not Result then
  begin
       if ErrorEnable then
       begin
            Error := ECDROMError.Create('Error al establecer "Read Ahead".');
            Error.ErrorCode := $0003bb00;
            raise Error;
       end;
  end;

  if Assigned(FOnSetCDSpeedFinish) then
  begin
       FOnSetCDSpeedFinish(Self);
  end;
end;

end.

