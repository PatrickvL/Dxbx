//------------------------------------------------------------------------------
//  title : Componente Win32 ASPI para Delphi 4/5/6/7
//
//  autor : H.Gotou(E-mail: hgotou@ibm.net)
//  date  : Jun.30,1999
//  rev.  : 26.06.2001 por Yursoft  (yursoft@yursoft.com)
//------------------------------------------------------------------------------
//  note  : 1)This component requires installing Win32ASPI.pas and wnaspi32.dll.
//------------------------------------------------------------------------------
//  history :
//    1.00.00 - May 05,1999
//          1) 1st release.
//    1.01.00 - Jun.30,1999
//          1) Add 'Time-out control' properties.
//          2) Add event handlers of each function.
//    17.01.02 - ???
//          1) Retocando algunas definiciones y constantes
//----------------------------------------------------------------------------
//
//  más notas: 25-01-2003 - Publicación
//
//----------------------------------------------------------------------------
unit Win32ASPI;

interface

uses
  Windows, SysUtils, Classes, MMSystem;

const
//------------------------------------------------------------------------------
// Definicion de constantes
//------------------------------------------------------------------------------
//Constantes ASPI
  SENSE_LEN	            = 14;           //Default sense buffer size
  SRB_DIR_SCSI              = $00;          //Direction determined by SCSI command
  SRB_POSTING	            = $01;          //Enable ASPI posting
  SRB_DATA_SG_LIST          = $02;          //Data buffer points to  scatter-gather list
  SRB_ENABLE_RESIDUAL_COUNT = $04;          //Enable residual byte count
  SRB_DIR_IN                = $08;          //Transfer from SCSI target to host(on Read)
  SRB_DIR_OUT               = $10;          //Transfer from host to SCSI target(on Write)
  SRB_EVENT_NOTIFY          = $40;          //Enable ASPI event notification reporting
  WM_ASPIPOST               = $4D42;        //ASPI Post message
//Funciones ASPI
  SC_HA_INQUIRY       = $00;                //Host adapter inquiry
  SC_GET_DEV_TYPE     = $01;                //Get device type
  SC_EXEC_SCSI_CMD    = $02;                //Execute SCSI command
  SC_ABORT_SRB        = $03;                //Abort an SRB
  SC_RESET_DEV        = $04;                //SCSI bus device reset
  SC_GET_DISK_INFO    = $06;                //Get Disk information(Windows95/98 only)
  SC_RESCAN_SCSI_BUS  = $07;                //Rescan SCSI bus device(WindowsNT only)
//Resultados ASPI
  SS_PENDING        = $00;                  //Being processed
  SS_COMP	    = $01;                  //Completed without error
  SS_ABORTED        = $02;                  //Aborted
  SS_ABORT_FAIL     = $03;                  //Unable to abort
  SS_ERR            = $04;                  //Completed with error
  SS_INVALID_CMD    = $80;                  //Invalid ASPI command
  SS_INVALID_HA     = $81;                  //Invalid host adapter number
  SS_NO_DEVICE      = $82;                  //SCSI device not installed
  SS_INVALID_SRB    = $E0;                  //Invalid parameter set in SRB
  SS_BUFFER_ALIGN   = $E1;                  //Buffer alignment inconsistent with HA_Unique
  SS_ILLEGAL_MODE   = $E2;                  //Ignore ASPI for Win32(for Win16 only)
  SS_NO_ASPI	    = $E3;                  //Ignore ASPI for Win32(for Win16 only)
  SS_FAILED_INIT    = $E4;                  //Ignore ASPI for Win32(for Win16 only)
  SS_ASPI_IS_BUSY   = $E5;                  //No resources available to execute command(Must reissue command later)
  SS_BUFFER_TO_BIG  = $E6;                  //Buffer size is to big to handle

//Estados del adaptador Host
  HASTAT_OK	    = $00;	            //Host adapter did not detect an error
  HASTAT_TRANSACT   = $09;                  //Timed out while SRB was waiting to be processed
  HASTAT_SRB_TO	    = $0B;                  //Host adapter timed out while processing
  HASTAT_MREJECT    = $0D;                  //Received Message Reject while processing
  HASTAT_BUS_RESET  = $0E;                  //Bus reset was detected
  HASTAT_PARITY_ERR = $0F;                  //Parity error was detected
  HASTAT_SENSE_ERR  = $10;                  //Host adapter failed in issuing REQUEST SENSE
  HASTAT_SEL_TO	    = $11;	            //Selection timeout
  HASTAT_DO_DU	    = $12;	            //Data overrun or underrun
  HASTAT_BUS_FREE   = $13;	            //Unexpected bus free
  HASTAT_PHASE_ERR  = $14;                  //Target bus phase sequence failure

//Estado Target
  STATUS_GOOD	    = $00;                  //Status Good
  STATUS_CHKCOND    = $02;                  //Check Condition
  STATUS_CONDMET    = $04;                  //Condition Met
  STATUS_BUSY	    = $08;                  //Busy
  STATUS_INTERM	    = $10;                  //Intermediate
  STATUS_INTCDMET   = $14;                  //Intermediate-condition met
  STATUS_RESCONF    = $18;                  //Reservation conflict
  STATUS_COMTERM    = $22;                  //Command Terminated
  STATUS_QFULL	    = $28;                  //Queue full
//------------------------------------------------------------------------------

type
//------------------------------------------------
// Clase Error
//------------------------------------------------
  EWin32ASPIError = class(Exception)          //Component General Error
    ErrorCode: Integer;
  end;
  ESystemError = class(EWin32ASPIError);      //Windows System Error
  EAspiError   = class(EWin32ASPIError);      //ASPI DLL Error
//------------------------------------------------
// Clase de uso interno
//------------------------------------------------
//Tipos de dispositivos  
  TDeviceType =    (dt_DirectAccess,
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
                    dt_Reserved);
  THAInfo = record                          //Host Adapter Information
	  SCSI_ID     : Byte;	            //SCSI ID of Host Adapter
	  ManagerID   : string;             //String describing the Manager
	  Identifier  : string;             //String describing the Host Adapter
	  Align       : Word;               //Buffer Alignment Mask Size
          TxReport    : Boolean;            //Data Transfer Status Report Function Validity
	  MaxTarget   : Byte;               //Maximun Target Number
	  MaxPacket   : Longword;           //Maximum Packet Size
  end;
  THAInfoList = array [0..255] of THAInfo;  //Host Adapter Information List
  TTargetType = Byte;                       //ASPI Target Type
  TDiskInfo = record                        //Hard Disk Information
	  Heads       : Byte;               //Hard Disk Head Numbers
	  Sectors     : Byte;	            //Hard Disk Sector Numbers
  end;                 //$03ff
  TSRBBuffer = array [0..$03ff] of Byte;    //SCSI Request Block Buffer
  PSRBBuffer = ^TSRBBuffer;                 //Pointer of SCSI Request Block Buffer
  TCDBByte = array [0..15] of Byte;         //SCSI Command Description Block
  PCDBByte = ^TCDBByte;                     //Pointer of SCSI Command Description Block
//------------------------------------------------------------------------------
// SCSI Request Block Class
//------------------------------------------------------------------------------
//1) HOST ADAPTER INQUIRY - SC_HA_INQUIRY
  TSRB_HAInquiry = record
	  SRB_Cmd       : Byte;	                  //ASPI command code
	  SRB_Status    : Byte;	                  //ASPI command status byte
	  SRB_HaId      : Byte;	                  //ASPI host adapter number
	  SRB_Flags     : Byte;	                  //ASPI request flags
	  SRB_Hdr_Rsvd  : Longword;               //Reserved(=0)
	  HA_Count      : Byte;	                  //Number of host adapters present
	  HA_SCSI_ID    : Byte;	                  //SCSI ID of host adapter
	  HA_ManagerId  : array [0..15] of Byte;  //String describing the manager
	  HA_Identifier : array [0..15] of Byte;  //String describing the host adapter
	  HA_Unique     : array [0..15] of Byte;  //Host Adapter Unique parameters
	  HA_Rsvd1      : Word;
  end;
  PSRB_HAInquiry = ^TSRB_HAInquiry;
//2) GET DEVICE TYPE - SC_GET_DEV_TYPE
  TSRB_GDEVBlock = record
    SRB_Cmd         : Byte;	            //ASPI command code
	  SRB_Status      : Byte;	    //ASPI command status byte
	  SRB_HaId        : Byte;	    //ASPI host adapter number
	  SRB_Flags       : Byte;	    //Reserved(=0)
	  SRB_Hdr_Rsvd    : Longword;	    //Reserved(=0)
	  SRB_Target      : Byte;	    //Target's SCSI ID
	  SRB_Lun         : Byte;	    //Target's LUNx number
	  SRB_DeviceType  : Byte;	    //Target's peripheral device type
	  SRB_Rsvd1       : Byte;
  end;
  PSRB_GDEVBlock = ^TSRB_GDEVBlock;
//3) EXECUTE SCSI COMMAND - SC_EXEC_SCSI_CMD
  TSRB_ExecSCSICmd = record
    SRB_Cmd         : Byte;		                        //ASPI command code
	  SRB_Status      : Byte;	                        //ASPI command status byte
	  SRB_HaId        : Byte;		                //ASPI host adapter number
	  SRB_Flags       : Byte;		                //ASPI request flags
	  SRB_Hdr_Rsvd    : Longword;	                        //Reserved(=0)
	  SRB_Target      : Byte;	                        //Target SCSI ID
	  SRB_Lun         : Byte;		                //Target LUNx number
	  SRB_Rsvd1       : Word;		                //Reserved for Alignment
	  SRB_BufLen      : Longword;	                        //Data Allocation Length
	  SRB_BufPointer  : Pointer;                            //Data Buffer Pointer
	  SRB_SenseLen    : Byte;	                        //Sense Allocation Length
	  SRB_CDBLen      : Byte;	                        //CDB Length
	  SRB_HaStat      : Byte;	                        //Host Adapter Status
	  SRB_TargetStat  : Byte;	                        //Target Status
	  SRB_PostProc    : Longword;	                        //Post routine
	  SRB_Rsvd2       : Pointer;	                        //Reserved
	  SRB_Rsvd3       : array [0..15] of Byte;              //Reserved for alignment(=ALL 0)
	  CDBByte         : TCDBByte;                           //SCSI Command Description Block
          SenseArea       : array [0..SENSE_LEN+1] of Byte;     //Request Sense buffer
  end;
  PSRB_ExecSCSICmd = ^TSRB_ExecSCSICmd;
//4) ABORT SRB - SC_ABORT_SRB
  TSRB_Abort = record
	  SRB_Cmd       : Byte;		    //ASPI command code
	  SRB_Status    : Byte;	            //ASPI command status byte
	  SRB_HaId      : Byte;		    //ASPI host adapter number
	  SRB_Flags     : Byte;		    //Reserved(=0)
	  SRB_Hdr_Rsvd  : Longword;	    //Reserved(=0)
	  SRB_ToAbort   : Pointer;	    //Pointer to SRB to abort
  end;
  PSRB_Abort = ^TSRB_Abort;
//5) BUS DEVICE RESET - SC_RESET_DEV
  TSRB_BusDeviceReset = record
	  SRB_Cmd       : Byte;                         //ASPI command code
	  SRB_Status    : Byte;	                        //ASPI command status byte
	  SRB_HaId      : Byte;		                //ASPI host adapter number
	  SRB_Flags     : Byte;		                //Reserved(=0)
	  SRB_Hdr_Rsvd  : Longword;	                //Reserved(=0)
	  SRB_Target    : Byte;	                        //Target SCSI ID
	  SRB_Lun       : Byte;		                //Target LUNx number
	  SRB_Rsvd1     : array [0..11] of Byte;        //Reserved for Alignment
	  SRB_HaStat    : Byte; 	                //Host Adapter Status
	  SRB_TargStat  : Byte;	                        //Target Status
	  SRB_PostProc  : Pointer;	                //Post routine
	  SRB_Rsvd2     : Pointer;	                //Reserved
	  SRB_Rsvd3     : array [0..31] of Byte;        //Reserved(=ALL 0)
  end;
  PSRB_BusDeviceReset = ^TSRB_BusDeviceReset;
//6) GET DISK INFORMATION - SC_GET_DISK_INFO
  TSRB_GetDiskInfo = record
	  SRB_Cmd             : Byte;		              //ASPI command code
	  SRB_Status          : Byte;	                      //ASPI command status byte
	  SRB_HaId            : Byte;		              //ASPI host adapter number
	  SRB_Flags           : Byte;		              //Reserved(=0)
	  SRB_Hdr_Rsvd        : Longword;	              //Reserved(=0)
	  SRB_Target          : Byte;	                      //Target SCSI ID
	  SRB_Lun             : Byte;		              //Target LUNx number
	  SRB_DriveFlags      : Byte;	                      //Driver flags
	  SRB_Int13HDriveInfo : Byte;                         //Host Adapter Status
	  SRB_Heads           : Byte;		              //Preferred number of heads translation
	  SRB_Sectors         : Byte;	                      //Preferred number of sectors translation
	  SRB_Rsvd1           : array [0..9] of Byte;         //Reserved(=ALL 0)
  end;
  PSRB_GetDiskInfo = ^TSRB_GetDiskInfo;
//7) RESET SCSI BUS - SC_RESCAN_SCSI_BUS
  TSRB_RescanPort = record
	  SRB_Cmd             : Byte;		              //ASPI command code
	  SRB_Status          : Byte;	                      //ASPI command status byte
	  SRB_HaId            : Byte;		              //ASPI host adapter number
	  SRB_Flags           : Byte;		              //Reserved(=0)
	  SRB_Hdr_Rsvd        : Longword;	              //Reserved(=0)
  end;
  PSRB_RescanPort = ^TSRB_RescanPort;
//------------------------------------------------------------------------------
//------------------------------------------------
// Main Class
//------------------------------------------------
  TWin32ASPI = class(TComponent)
  private
    //---Campo de Control
    FErrorEnable: boolean;
    FTimeOut_GetHostAdapterInquiry: Integer;
    FTimeOut_GetDeviceType: Integer;
    FTimeOut_ExecSCSICommand: Integer;
    FTimeOut_ServiceAbort:  Integer;
    FTimeOut_ResetDevice: Integer;
    FTimeOut_GetDiskInfo: Integer;
    FTimeOut_RescanBus: Integer;
    //---Plataforma
    FCPU: string;
    FTotalMemory: string;
    FOperatingSystem: string;
    FOsVersion: string;
    FOsBuild: string;
    //---ASPI
    FASPISupport: Boolean;
    FHostAdapterCount: Byte;
    FHostAdapterStatus: Boolean;
    FHostAdapterInfo: THAInfoList;
    FTargetTypeInfo: TTargetType;
    FDiskInfo: TDiskInfo;
    //---Eventos
    FOnGetHostAdapterInquiryFinish: TNotifyEvent;
    FOnGetDeviceTypeFinish: TNotifyEvent;
    FOnExecSCSICommandFinish: TNotifyEvent;
    FOnServiceAbortFinish: TNotifyEvent;
    FOnResetDeviceFinish: TNotifyEvent;
    FOnGetDiskInfoFinish: TNotifyEvent;
    FOnRescanBusFinish: TNotifyEvent;
  protected
  public
    //---Platform Information
    property CPU: string read FCPU;                                 //Tipo CPU
    property TotalMemory: string read FTotalMemory;                 //Physical Memory Size
    property OperatingSystem: string read FOperatingSystem;         //Operating System
    property OsVersion: string read FOsVersion;                     //OS Version
    property OsBuild: string read FOsBuild;                         //OS Build Number
    //---ASPI Information
    property ASPISupport: Boolean read FASPISupport;                //ASPI Function Validity
    property HostAdapterCount: Byte read FHostAdapterCount;         //Host Adapter Count Number
    property HostAdapterStatus: Boolean read FHostAdapterStatus;    //Host Adapter Status
    property HostAdapterInfo: THAInfoList read FHostAdapterInfo;    //Host Adapter Information
    property TargetTypeInfo: TTargetType read FTargetTypeInfo;      //Target Type Information
    property DiskInfo: TDiskInfo read FDiskInfo;                    //Target Disk Information
    //---Component Method
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //---Platform Information Function
    procedure GetSysInfo;                                           //Get Platform Information
    //---ASPI Information Function
    function GetHostAdapterCount: Integer;                          //Get Host adapter numbers - result: numbers(0-$ff:numbers/-1:error)
    procedure InitSRB(pPacket: Pointer; Size: Longword);            //Initialize SRB Buffer(Databuffer for SCSI-Commands)
    procedure InitCDB(pCDB: PCDBByte);                              //Initialize CDB
    //ASPI Function
    function GetHostAdapterInquiry(HA_IDx: Byte): Boolean;            //Get host adapter inquiry
    function GetDeviceType(HA_IDx, SCSI_IDx, LUNx: Byte): TDeviceType;    //Get target device type
    function ExecSCSICommand(pSRBPacket: PSRB_ExecSCSICmd): Boolean;  //Execute SCSI Command
    function ServiceAbort(HA_IDx: Byte; PPacket: Pointer): Boolean;   //Cancel SRB request
    function ResetDevice(HA_IDx, SCSI_IDx, LUNx: Byte): Boolean;      //Reset target device
    function GetDiskInfo(HA_IDx, SCSI_IDx, LUNx: Byte): Boolean;      //Get SCSI Disk Information (Windows95/98 only)
    function RescanBus(HA_IDx: Byte): Boolean;                        //Re-scan SCSI Bus (WindowsNT only)
  published
    //---Control Property
    property ErrorEnable: Boolean read FErrorEnable write FErrorEnable default False;                                                //Enable/Disable of class error
    property TimeOut_GetHostAdapterInquiry: Integer read FTimeOut_GetHostAdapterInquiry write FTimeOut_GetHostAdapterInquiry;        //Time-out value of GetHostAdapterInquiry function
    property TimeOut_GetDeviceType: Integer read FTimeOut_GetDeviceType write FTimeOut_GetDeviceType;                                //Time-out value of GetDeviceType function
    property TimeOut_ExecSCSICommand: Integer read FTimeOut_ExecSCSICommand write FTimeOut_ExecSCSICommand;                          //Time-out value of ExecSCSICommand function
    property TimeOut_ServiceAbort: Integer read FTimeOut_ServiceAbort write FTimeOut_ServiceAbort;                                   //Time-out value of ServiceAbort function
    property TimeOut_ResetDevice: Integer read FTimeOut_ResetDevice write FTimeOut_ResetDevice;                                      //Time-out value of ResetDevice function
    property TimeOut_GetDiskInfo: Integer read FTimeOut_GetDiskInfo write FTimeOut_GetDiskInfo;                                      //Time-out value of GetDiskInfo function
    property TimeOut_RescanBus: Integer read FTimeOut_RescanBus write FTimeOut_RescanBus;                                            //Time-out value of RescanBus function
    //---Event Handler
    property OnGetHostAdapterInquiryFinish: TNotifyEvent read FOnGetHostAdapterInquiryFinish write FOnGetHostAdapterInquiryFinish;   //Event handler of finishing GetHostAdapterInquiry function
    property OnGetDeviceTypeFinish: TNotifyEvent read FOnGetDeviceTypeFinish write FOnGetDeviceTypeFinish;                           //Event handler of finishing GetDeviceType function
    property OnExecSCSICommandFinish: TNotifyEvent read FOnExecSCSICommandFinish write FOnExecSCSICommandFinish;                     //Event handler of finishing ExecSCSICommand function
    property OnServiceAbortFinish: TNotifyEvent read FOnServiceAbortFinish write FOnServiceAbortFinish;                              //Event handler of finishing ServiceAbort function
    property OnResetDeviceFinish: TNotifyEvent read FOnResetDeviceFinish write FOnResetDeviceFinish;                                 //Event handler of finishing ResetDevice function
    property OnGetDiskInfoFinish: TNotifyEvent read FOnGetDiskInfoFinish write FOnGetDiskInfoFinish;                                 //Event handler of finishing GetDiskInfo function
    property OnRescanBusFinish: TNotifyEvent read FOnRescanBusFinish write FOnRescanBusFinish;                                       //Event handler of finishing RescanBus function
  end;
//------------------------------------------------
  TGetASPI32SupportInfo = function: Longword; cdecl;
  TSendASPI32Command = function(PPacket: Pointer): Longword; cdecl;

var
  Wnaspi32Handle: THandle;
  GetASPI32SupportInfo: TGetASPI32SupportInfo;
  SendASPI32Command: TSendASPI32Command;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CDROM', [TWin32ASPI]);
end;

{ TWin32ASPI }

constructor TWin32ASPI.Create(AOwner: TComponent);
var
  Error: ESystemError;
  Count: Integer;
  n: Byte;
begin
  inherited Create(AOwner);
  FTimeOut_GetHostAdapterInquiry := 1000;
  FTimeOut_GetDeviceType := 1000;
  FTimeOut_ExecSCSICommand := 10000;
  FTimeOut_ServiceAbort := 1000;
  FTimeOut_ResetDevice := 1000;
  FTimeOut_GetDiskInfo := 1000;
  FTimeOut_RescanBus := 1000;
//---Platform Information Section Initialization
  FCPU := 'None';
  FTotalMemory := '0 MByte';
  FOperatingSystem := 'None';
  FOsVersion := '0.0';
  FOsBuild := '0000';
  GetSysInfo;
//---Inicializacion Seccion de Informacion ASPI
  FASPISupport := False;
  FHostAdapterCount := 0;
  FHostAdapterStatus := False;
  for n := 0 to 255 do
  begin
          FHostAdapterInfo[n].SCSI_ID := 0;
	  FHostAdapterInfo[n].ManagerID := '';
	  FHostAdapterInfo[n].Identifier := '';
	  FHostAdapterInfo[n].Align     := $0000;
          FHostAdapterInfo[n].TxReport  := False;
	  FHostAdapterInfo[n].MaxTarget := $00;
	  FHostAdapterInfo[n].MaxPacket := $00000000;
  end;
  FTargetTypeInfo := 0;
  FDiskInfo.Heads := 0;
  FDiskInfo.Sectors := 0;
//---Cargar Wnaspi32.dll
  Wnaspi32Handle := LoadLibrary('wnaspi32.dll');
  if Wnaspi32Handle <> 0 then
  begin
    @GetASPI32SupportInfo := GetProcAddress(Wnaspi32Handle, 'GetASPI32SupportInfo');
    @SendASPI32Command    := GetProcAddress(Wnaspi32Handle, 'SendASPI32Command');
    if (@GetASPI32SupportInfo <> nil) and (@SendASPI32Command <> nil) then
    begin
      FASPISupport := True;
    end
    else
    begin
      Error := ESystemError.Create('Error : No se ha encontrado la DLL "wnaspi32.dll"');
      Error.ErrorCode := $00000001;
      raise Error;
    end;
  end;
//---Inquiry tipo Get Host Adapter
  if FASPISupport then
  begin
    Count := GetHostAdapterCount;
    if Count > 0 then
    begin
      FHostAdapterCount := Count and $000000ff;
      for n := 0 to FHostAdapterCount - 1 do
      begin
        FHostAdapterStatus := GetHostAdapterInquiry(n);
      end;
    end;
  end;
end;

destructor TWin32ASPI.Destroy;
begin
//---Liberamos Wnaspi32.dll
  if Wnaspi32Handle <> 0 then
  begin
    FreeLibrary(Wnaspi32Handle);
  end;
//---
  inherited Destroy;
end;

//-------------------------------
// Platform Information Function
//-------------------------------
procedure TWin32ASPI.GetSysInfo;
var
   SI : TSystemInfo;
   OS : TOSVersionInfo;
   MemStat : TMemoryStatus;
begin
   GetSystemInfo(SI);
   case Si.dwProcessorType of
      386 : FCPU := 'i386';
      486 : FCPU := 'i486';
      586 : FCPU := 'Pentium';
   end;
   MemStat.dwLength := SizeOf(TMemoryStatus);
   GlobalMemoryStatus(MemStat);
   FTotalMemory := Format('%d MByte', [Trunc(MemStat.dwTotalPhys/1024/1024)]);
   OS.dwOSVersionInfoSize := SizeOf(TOSVERSIONINFO);
   GetVersionEx(OS);
   case OS.dwPlatformId of
      VER_PLATFORM_WIN32s        : FOperatingSystem := 'Windows 3.1';
      VER_PLATFORM_WIN32_WINDOWS : FOperatingSystem := 'Windows 95/98/ME';
      VER_PLATFORM_WIN32_NT	 : FOperatingSystem := 'Windows NT/2000/XP';
   end;
   FOsVersion := Format ('%d.%d', [OS.dwMajorVersion, OS.dwMinorVersion]);
   FOsBuild := Format('%d', [LOWORD(OS.dwBuildNumber)]);
end;

//---------------------------
// ASPI Information Function
//---------------------------
function TWin32ASPI.GetHostAdapterCount: Integer;
var
  x: Integer;
begin
  Result := -1;
  if FASPISupport then
  begin
    x := GetASPI32SupportInfo;
    if ((x and $0000ff00) shr 8) = $00000001 then
    begin
      Result := x and $000000ff;
    end;
  end;
end;

procedure TWin32ASPI.InitSRB(pPacket: Pointer; Size: Longword);
var
  i: Longword;
  P: PSRBBuffer;
begin
  P := pPacket;
  for i := 0 to Size-1 do
  begin
    P^[i] := 0;
  end;
end;

procedure TWin32ASPI.InitCDB(pCDB: PCDBByte);
var
  n: Byte;
begin
  for n := 0 to 15 do
  begin
    pCDB^[n] := 0;
  end;
end;

function TWin32ASPI.GetHostAdapterInquiry(HA_IDx: Byte): Boolean;
var
  Error: EAspiError;
  SRBPacket: TSRB_HAInquiry;
  pSRBPacket: PSRB_HAInquiry;
  PacketSize: Longword;
  ASPIStatus: Longword;
  StartTime, CurrentTime, LapseTime, LimitTime: Integer;
  Executing, TimeoutError: Boolean;
  n: Byte;
  xString: string;
begin
  Result := False;
  TimeoutError := False;
  LimitTime := FTimeOut_GetHostAdapterInquiry;
  if FASPISupport then
  begin
    //---Command Packet---
    pSRBPacket := @SRBPacket;
    PacketSize := SizeOf(SRBPacket);
    InitSRB(pSRBPacket, PacketSize);
    SRBPacket.SRB_Cmd := SC_HA_INQUIRY;
    SRBPacket.SRB_HaId := HA_IDx;
    //---Execute Command---
    ASPIStatus := SS_PENDING;
    Executing := True;
    StartTime := TimeGetTime;
    SendASPI32Command(pSRBPacket);
    while Executing do
    begin
      ASPIStatus := SRBPacket.SRB_Status;
      CurrentTime := TimeGetTime;
      LapseTime := CurrentTime - StartTime;
      if (LapseTime > LimitTime) or (LapseTime < 0) then
      begin
        TimeoutError := True;
      end;
      if (ASPIStatus <> SS_PENDING) or (TimeoutError) then
      begin
        Executing := False;
      end;
    end;
    //---Check Result---
    if TimeoutError then
    begin
      ServiceAbort(HA_IDx, pSRBPacket);
    end
    else
    begin
      If ASPIStatus = SS_COMP then
      begin
        Result := True;
        FHostAdapterInfo[HA_IDx].SCSI_ID := SRBPacket.HA_SCSI_ID;
        xString := '';
        for n := 0 to 15 do
        begin
          xString := xString + Chr(SRBPacket.HA_ManagerId[n]);
        end;
        FHostAdapterInfo[HA_IDx].ManagerID := xString;
        xString := '';
        for n := 0 to 15 do
        begin
          xString := xString + Chr(SRBPacket.HA_Identifier[n]);
        end;
        FHostAdapterInfo[HA_IDx].Identifier := xString;
	      FHostAdapterInfo[HA_IDx].Align := (SRBPacket.HA_Unique[1] * $0100) + SRBPacket.HA_Unique[0];
        if (SRBPacket.HA_Unique[2] and $02) = $02 then
        begin
          FHostAdapterInfo[HA_IDx].TxReport  := true;
        end
        else
        begin
          FHostAdapterInfo[HA_IDx].TxReport  := False;
        end;
        if SRBPacket.HA_Unique[3] = $00 then
        begin
	        FHostAdapterInfo[HA_IDx].MaxTarget := $08;
        end
        else
        begin
	        FHostAdapterInfo[HA_IDx].MaxTarget := SRBPacket.HA_Unique[3];
        end;
	      FHostAdapterInfo[HA_IDx].MaxPacket := (SRBPacket.HA_Unique[7] * $01000000) + (SRBPacket.HA_Unique[6] * $00010000) +
                                            (SRBPacket.HA_Unique[5] * $00000100) + SRBPacket.HA_Unique[4];
      end;
    end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := EAspiError.Create('Se ha producido un error al intentar acceder a la informacion del adaptador.');
      Error.ErrorCode := $00010001;
      raise Error;
    end;
  end;
  If Assigned(FOnGetHostAdapterInquiryFinish) then
  begin
    FOnGetHostAdapterInquiryFinish(Self);
  end;
end;

function TWin32ASPI.GetDeviceType(HA_IDx, SCSI_IDx, LUNx: Byte): TDeviceType;
var
  SRBPacket: TSRB_GDEVBlock;
  pSRBPacket: PSRB_GDEVBlock;
  PacketSize: Longword;
  ASPIStatus: Longword;
  StartTime, CurrentTime, LapseTime, LimitTime: Integer;
  Executing, TimeoutError: Boolean;
begin
  Result := dt_Unknown;
  TimeoutError := False;
  LimitTime := FTimeOut_GetDeviceType;
  if FASPISupport then
  begin
    //---Command Packet---
    pSRBPacket := @SRBPacket;
    PacketSize := SizeOf(SRBPacket);
    InitSRB(pSRBPacket, PacketSize);
    SRBPacket.SRB_Cmd    := SC_GET_DEV_TYPE;
    SRBPacket.SRB_HaId   := HA_IDx;
    SRBPacket.SRB_Target := SCSI_IDx;
    SRBPacket.SRB_Lun    := LUNx;
    //---Execute Command---
    ASPIStatus := SS_PENDING;
    Executing := True;
    StartTime := TimeGetTime;
    SendASPI32Command(pSRBPacket);
    while Executing do
    begin
      ASPIStatus := SRBPacket.SRB_Status;
      CurrentTime := TimeGetTime;
      LapseTime := CurrentTime - StartTime;
      if (LapseTime > LimitTime) or (LapseTime < 0) then
      begin
        TimeoutError := True;
      end;
      if (ASPIStatus <> SS_PENDING) or (TimeoutError) then
      begin
        Executing := False;
      end;
    end;
    //---Check Result---
    if TimeoutError then
    begin
      ServiceAbort(HA_IDx, pSRBPacket);
    end
    else
    begin
      If ASPIStatus = SS_COMP then
      begin
        FTargetTypeInfo := SRBPacket.SRB_DeviceType;
        Result := TDeviceType(SRBPacket.SRB_DeviceType);
      end;
    end;
  end;
  {if not Result then
  begin
    if ErrorEnable then
    begin
      Error := EAspiError.Create('Error al ejecutar el comando "GetDeviceType"');
      Error.ErrorCode := $00010002;
      raise Error;
    end;
  end;   }
  If Assigned(FOnGetDeviceTypeFinish) then
  begin
    FOnGetDeviceTypeFinish(Self);
  end;
end;

function TWin32ASPI.ExecSCSICommand(pSRBPacket: PSRB_ExecSCSICmd): Boolean;
var
  Error: EAspiError;
  ASPIStatus: Longword;
  hEvent, WaitStatus, LimitTime: Longword;
begin
  Result := False;
  LimitTime := FTimeOut_ExecSCSICommand;
  hEvent := CreateEvent(nil, False, False, nil);
  if not(hEvent = 0) and ASPISupport then
  begin
    //---Paquete del Comando---
    pSRBPacket^.SRB_Cmd      := SC_EXEC_SCSI_CMD;
    pSRBPacket^.SRB_Flags    := pSRBPacket^.SRB_Flags or SRB_EVENT_NOTIFY;
    pSRBPacket^.SRB_SenseLen := SENSE_LEN;
    pSRBPacket^.SRB_PostProc := hEvent;

    //---Ejecuta el Comando---
    ASPIStatus := SendASPI32Command(pSRBPacket);
    if ASPIStatus = SS_PENDING then
    begin                                         // Modificado para que espere
      if LimitTime = -1 then
        WaitStatus := WaitForSingleObject(hEvent, Infinite)
      else
        WaitStatus := WaitForSingleObject(hEvent, LimitTime); //Infinite);
      if WaitStatus = WAIT_TIMEOUT then
      begin
  	    ServiceAbort(pSRBPacket^.SRB_HaId, pSRBPacket);
      end
      else
      begin
        if pSRBPacket^.SRB_Status = SS_COMP then
        begin
            Result := True;
        end;
      end;
    end;
    CloseHandle(hEvent);
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := EAspiError.Create('Ha ocurrido un error al ejecutar el comando "ExecSCSICommand".');
      Error.ErrorCode := $00010003;
      //raise Error;
    end;   
  end;
  If Assigned(FOnExecSCSICommandFinish) then
  begin
    FOnExecSCSICommandFinish(Self);
  end;
end;

function TWin32ASPI.ServiceAbort(HA_IDx: Byte; pPacket: Pointer): Boolean;
var
  Error: EAspiError;
  SRBPacket: TSRB_Abort;
  pSRBPacket: PSRB_Abort;
  PacketSize: Longword;
  ASPIStatus: Longword;
  StartTime, CurrentTime, LapseTime, LimitTime: Integer;
  Executing, TimeoutError: Boolean;
begin
  Result := False;
  TimeoutError := False;
  LimitTime := FTimeOut_ServiceAbort;
  if FASPISupport then
  begin
    //---Command Packet---
    pSRBPacket := @SRBPacket;
    PacketSize := SizeOf(SRBPacket);
    InitSRB(pSRBPacket, PacketSize);
    SRBPacket.SRB_Cmd     := SC_ABORT_SRB;
    SRBPacket.SRB_HaId    := HA_IDx;
    SRBPacket.SRB_ToAbort := PPacket;
    //---Execute Command---
    ASPIStatus := SS_PENDING;
    Executing := True;
    StartTime := TimeGetTime;
    SendASPI32Command(pSRBPacket);
    while Executing do
    begin
      ASPIStatus := SRBPacket.SRB_Status;
      CurrentTime := TimeGetTime;
      LapseTime := CurrentTime - StartTime;
      if (LapseTime > LimitTime) or (LapseTime < 0) then
      begin
        TimeoutError := True;
      end;
      if (ASPIStatus <> SS_PENDING) or (TimeoutError) then
      begin
        Executing := False;
      end;
    end;
    //---Check Result---
    If ASPIStatus = SS_COMP then
    begin
      Result := True;
    end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := EAspiError.Create('Error al ejecutar el comando "ServiceAbort"');
      Error.ErrorCode := $00010004;
      raise Error;
    end;
  end;
  If Assigned(FOnServiceAbortFinish) then
  begin
    FOnServiceAbortFinish(Self);
  end;
end;

function TWin32ASPI.ResetDevice(HA_IDx, SCSI_IDx, LUNx: Byte): Boolean;
var
  Error: EAspiError;
  SRBPacket: TSRB_BusDeviceReset;
  pSRBPacket: PSRB_BusDeviceReset;
  PacketSize: Longword;
  ASPIStatus: Longword;
  StartTime, CurrentTime, LapseTime, LimitTime: Integer;
  Executing, TimeoutError: Boolean;
begin
  Result := False;
  TimeoutError := False;
  LimitTime := FTimeOut_ResetDevice;
  if FASPISupport then
  begin
    //---Command Packet---
    pSRBPacket := @SRBPacket;
    PacketSize := SizeOf(SRBPacket);
    InitSRB(pSRBPacket, PacketSize);
    SRBPacket.SRB_Cmd := SC_RESET_DEV;
    SRBPacket.SRB_HaId := HA_IDx;
    SRBPacket.SRB_Target := SCSI_IDx;
    SRBPacket.SRB_Lun := LUNx;
    //---Execute Command---
    ASPIStatus := SS_PENDING;
    Executing := True;
    StartTime := TimeGetTime;
    SendASPI32Command(pSRBPacket);
    while Executing do
    begin
      ASPIStatus := SRBPacket.SRB_Status;
      CurrentTime := TimeGetTime;
      LapseTime := CurrentTime - StartTime;
      if (LapseTime > LimitTime) or (LapseTime < 0) then
      begin
        TimeoutError := True;
      end;
      if (ASPIStatus <> SS_PENDING) or (TimeoutError) then
      begin
        Executing := False;
      end;
    end;
    //---Check Result---
    if TimeoutError then
    begin
      ServiceAbort(HA_IDx, pSRBPacket);
    end
    else
    begin
      If ASPIStatus = SS_COMP then
      begin
        Result := True;
      end;
    end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := EAspiError.Create('Error al ejecutar el comando "ResetDevice"');
      Error.ErrorCode := $00010005;
      raise Error;
    end;
  end;
  If Assigned(FOnResetDeviceFinish) then
  begin
    FOnResetDeviceFinish(Self);
  end;
end;

function TWin32ASPI.GetDiskInfo(HA_IDx, SCSI_IDx, LUNx: Byte): Boolean;
var
  Error: EAspiError;
  SRBPacket: TSRB_GetDiskInfo;
  pSRBPacket: PSRB_GetDiskInfo;
  PacketSize: Longword;
  ASPIStatus: Longword;
  StartTime, CurrentTime, LapseTime, LimitTime: Integer;
  Executing, TimeoutError: Boolean;
begin
  Result := False;
  TimeoutError := False;
  LimitTime := FTimeOut_GetDiskInfo;
  if FASPISupport and (FOperatingSystem = 'Windows 95/98/ME') then
  begin
    //---Command Packet---
    pSRBPacket := @SRBPacket;
    PacketSize := SizeOf(SRBPacket);
    InitSRB(pSRBPacket, PacketSize);
    SRBPacket.SRB_Cmd := SC_GET_DISK_INFO;
    SRBPacket.SRB_HaId := HA_IDx;
    SRBPacket.SRB_Target := SCSI_IDx;
    SRBPacket.SRB_Lun := LUNx;
    //---Execute Command---
    ASPIStatus := SS_PENDING;
    Executing := True;
    StartTime := TimeGetTime;
    SendASPI32Command(pSRBPacket);
    while Executing do
    begin
      ASPIStatus := SRBPacket.SRB_Status;
      CurrentTime := TimeGetTime;
      LapseTime := CurrentTime - StartTime;
      if (LapseTime > LimitTime) or (LapseTime < 0) then
      begin
        TimeoutError := True;
      end;
      if (ASPIStatus <> SS_PENDING) or (TimeoutError) then
      begin
        Executing := False;
      end;
    end;
    //---Check Result---
    if TimeoutError then
    begin
      ServiceAbort(HA_IDx, pSRBPacket);
    end
    else
    begin
      If ASPIStatus = SS_COMP then
      begin
        Result := True;
        FDiskInfo.Heads := SRBPacket.SRB_Heads;
        FDiskInfo.Sectors := SRBPacket.SRB_Sectors;
      end;
    end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := EAspiError.Create('Error al ejecutar el comando "GetDiskInfo"');
      Error.ErrorCode := $00010006;
      raise Error;
    end;
  end;
  If Assigned(FOnGetDiskInfoFinish) then
  begin
    FOnGetDiskInfoFinish(Self);
  end;
end;

function TWin32ASPI.RescanBus(HA_IDx: Byte): Boolean;
var
  Error: EAspiError;
  SRBPacket: TSRB_RescanPort;
  PSRBPacket: PSRB_RescanPort;
  PacketSize: Longword;
  ASPIStatus: Longword;
  StartTime, CurrentTime, LapseTime, LimitTime: Integer;
  Executing, TimeoutError: Boolean;
begin
  Result := False;
  TimeoutError := False;
  LimitTime := FTimeOut_RescanBus;
  if FASPISupport and (FOperatingSystem = 'Windows NT/2000/XP') then
  begin
    //---Command Packet---
    pSRBPacket := @SRBPacket;
    PacketSize := SizeOf(SRBPacket);
    InitSRB(pSRBPacket, PacketSize);
    SRBPacket.SRB_Cmd := SC_RESCAN_SCSI_BUS;
    SRBPacket.SRB_HaId := HA_IDx;
    //---Execute Command---
    ASPIStatus := SS_PENDING;
    Executing := True;
    StartTime := TimeGetTime;
    SendASPI32Command(pSRBPacket);
    while Executing do
    begin
      ASPIStatus := SRBPacket.SRB_Status;
      CurrentTime := TimeGetTime;
      LapseTime := CurrentTime - StartTime;
      if (LapseTime > LimitTime) or (LapseTime < 0) then
      begin
        TimeoutError := True;
      end;
      if (ASPIStatus <> SS_PENDING) or (TimeoutError) then
      begin
        Executing := False;
      end;
    end;
    //---Check Result---
    if TimeoutError then
    begin
      ServiceAbort(HA_IDx, pSRBPacket);
    end
    else
    begin
      If ASPIStatus = SS_COMP then
      begin
        Result := True;
      end;
    end;
  end;
  if not Result then
  begin
    if ErrorEnable then
    begin
      Error := EAspiError.Create('Error al ejecutar el comando "ReScanBus"');
      Error.ErrorCode := $00010007;
      raise Error;
    end;
  end;
  If Assigned(FOnRescanBusFinish) then
  begin
    FOnRescanBusFinish(Self);
  end;
end;

end.
