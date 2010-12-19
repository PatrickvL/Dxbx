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
unit uEmuKrnlIo;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  SysUtils,
  // Jedi Win32API
  JwaWinType,
  JwaWinBase,
  JwaWinNT,
  JwaNative,
  JwaNTStatus,
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uTypes,
  uDxbxUtils,
  uLog,
  uDxbxKrnlUtils,
  uEmuFS,
  uEmuFile,
  uEmuKrnl,
  uDxbxKrnl;

// TODO -oDXBX: Translate all other IRP defines from ReactOS
const IRP_MJ_MAXIMUM_FUNCTION = $1B;

type _IRP = packed record
    cType: CSHORT;
    Size: USHORT;
(*
    MdlAddress: P_MDL;
    Flags: ULONG;
    union begin
      MasterIrp: P_IRP;
      IrpCount: LongInt; // volatile
      SystemBuffer: PVOID;
    end;
    ThreadListEntry: LIST_ENTRY;
    IoStatus: IO_STATUS_BLOCK;
    RequestorMode: KPROCESSOR_MODE;
    PendingReturned: _BOOLEAN;
    StackCount: CHAR;
    CurrentLocation: CHAR;
    Cancel: _BOOLEAN;
    CancelIrql: KIRQL;
    ApcEnvironment: CCHAR;
    AllocationFlags: UCHAR;
    UserIosb: PIO_STATUS_BLOCK;
    UserEvent: PKEVENT;

    // TODO -oDXBX: Translate all below
    union begin
      struct begin
        PIO_APC_ROUTINE  UserApcRoutine;
        PVOID  UserApcContext;
       end; AsynchronousParameters;
      LARGE_INTEGER  AllocationSize;
     end; Overlay;
    volatile PDRIVER_CANCEL  CancelRoutine;
    PVOID  UserBuffer;
    union begin
      struct begin
        _ANONYMOUS_UNION union begin
          KDEVICE_QUEUE_ENTRY  DeviceQueueEntry;
          _ANONYMOUS_STRUCT struct begin
              DriverContext: array [0..4-1] of PVOID;
           end; DUMMYSTRUCTNAME;
         end; DUMMYUNIONNAME;
        PETHREAD  Thread;
        PCHAR  AuxiliaryBuffer;
        _ANONYMOUS_STRUCT struct begin
          LIST_ENTRY  ListEntry;
          _ANONYMOUS_UNION union begin
            struct _IO_STACK_LOCATION  *CurrentStackLocation;
            ULONG  PacketType;
           end; DUMMYUNIONNAME;
         end; DUMMYSTRUCTNAME;
        struct _FILE_OBJECT  *OriginalFileObject;
       end; Overlay;
      KAPC  Apc;
      PVOID  CompletionKey;
     end; Tail;
*)
  end; // packed size = 3
  IRP = _IRP;
  PIRP = ^IRP;

  PDRIVER_EXTENSION = INT_PTR; // TODO -oDXBX: Lookup in ReactOS
  PFAST_IO_DISPATCH = INT_PTR; // TODO -oDXBX: Lookup in ReactOS
  PDRIVER_INITIALIZE = INT_PTR; // TODO -oDXBX: Lookup in ReactOS
  PDRIVER_STARTIO = INT_PTR; // TODO -oDXBX: Lookup in ReactOS
  PDRIVER_DELETEDEVICE = INT_PTR; // TODO -oDXBX: Lookup in ReactOS
  PDRIVER_DISMOUNTVOLUME = INT_PTR; // TODO -oDXBX: Lookup in ReactOS
  PDRIVER_UNLOAD = INT_PTR; // TODO -oDXBX: Lookup in ReactOS
  PDRIVER_DISPATCH = INT_PTR; // TODO -oDXBX: Lookup in ReactOS
  PIO_TIMER_ROUTINE = INT_PTR; // TODO -oDXBX: Lookup in ReactOS
  PVPB = INT_PTR; // TODO -oDXBX: Lookup in ReactOS
  DEVICE_TYPE = ULONG;
  KDEVICE_QUEUE = INT_PTR; // TODO -oDXBX: Lookup in ReactOS
  KEVENT = INT_PTR; // TODO -oDXBX: Lookup in ReactOS
  PKEVENT = ^KEVENT;
  PDEVOBJ_EXTENSION = INT_PTR; // TODO -oDXBX: Lookup in ReactOS

  PDEVICE_OBJECT = ^DEVICE_OBJECT; // forward;
  PPDEVICE_OBJECT = ^PDEVICE_OBJECT;

// I/O Timer Object
{type} _IO_TIMER = packed record
// Source: ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:100
    Type_: USHORT;
    TimerEnabled: USHORT;
    IoTimerList: LIST_ENTRY;
    TimerRoutine: PIO_TIMER_ROUTINE;
    Context: PVOID;
    DeviceObject: PDEVICE_OBJECT;
  end; // packed size = 24
  IO_TIMER = _IO_TIMER;
  PIO_TIMER = ^IO_TIMER;

{type} _DRIVER_OBJECT = packed record
// Branch:Dxbx  Translator:PatrickvL  Done:100
    DriverStartIo: PDRIVER_STARTIO;
    DriverDeleteDevice: PDRIVER_DELETEDEVICE;
    DriverDismountVolume: PDRIVER_DISMOUNTVOLUME;
    MajorFunction: array [0..IRP_MJ_MAXIMUM_FUNCTION] of PDRIVER_DISPATCH;
  end; // packed size = ?
  DRIVER_OBJECT = _DRIVER_OBJECT;
  PDRIVER_OBJECT = ^DRIVER_OBJECT;

{type} _DEVICE_OBJECT = packed record
// Branch:Dxbx  Translator:PatrickvL  Done:100
    Type_: CSHORT;
    Size: USHORT;
    ReferenceCount: LONG;
    DriverObject: PDRIVER_OBJECT;
    MountedOrSelfDevice: PDEVICE_OBJECT;
    CurrentIrp: PIRP;
    Flags: ULONG;
    DeviceExtension: PVOID;
    DeviceType: UCHAR;
    StackSize: CCHAR;
    DeletePending: _BOOLEAN;
    SectorSize: USHORT;
    AlignmentRequirement: ULONG;
    DeviceQueue: KDEVICE_QUEUE;
    DeviceLock: KEVENT;
    StartIoKey: ULONG;
  end; // packed size = ?
  DEVICE_OBJECT = _DEVICE_OBJECT;

type _FILE_OBJECT = packed record
// Source: XBMC  Branch:Dxbx  Translator:PatrickvL  Done:100
    Type_: CSHORT;
    Size: CSHORT;
    DeviceObject: PDEVICE_OBJECT;
   // ...
  end; // packed size = 6
  FILE_OBJECT = _FILE_OBJECT;
  PFILE_OBJECT = ^FILE_OBJECT;

type _SHARE_ACCESS = packed record
// Source: ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:100
    OpenCount: ULONG;
    Readers: ULONG;
    Writers: ULONG;
    Deleters: ULONG;
    SharedRead: ULONG;
    SharedWrite: ULONG;
    SharedDelete: ULONG;
  end; // packed size = 28
  SHARE_ACCESS = _SHARE_ACCESS;
  PSHARE_ACCESS = ^SHARE_ACCESS;

var {064}xboxkrnl_IoCompletionObjectType: POBJECT_TYPE = NULL; // TODO -oDxbx : What should we initialize this to?
// Source:?  Branch:Dxbx  Translator:PatrickvL  Done:0

var {070}xboxkrnl_IoDeviceObjectType: POBJECT_TYPE = NULL; // TODO -oDxbx : What should we initialize this to?
// Source:?  Branch:Dxbx  Translator:PatrickvL  Done:0

var {071}xboxkrnl_IoFileObjectType: POBJECT_TYPE = NULL; // TODO -oDxbx : What should we initialize this to?
// Source:?  Branch:Dxbx  Translator:PatrickvL  Done:0

function {059} xboxkrnl_IoAllocateIrp(
  StackSize: CCHAR
  ): PIRP; stdcall;
function {060} xboxkrnl_IoBuildAsynchronousFsdRequest(
  MajorFunction: ULONG;
  DeviceObject: PDEVICE_OBJECT;
  Buffer: PVOID; // OUT OPTIONAL
  Length: ULONG; // OPTIONAL,
  StartingOffset: PLARGE_INTEGER; // OPTIONAL
  IoStatusBlock: PIO_STATUS_BLOCK // OPTIONAL
  ): PIRP; stdcall;
function {061} xboxkrnl_IoBuildDeviceIoControlRequest(
  IoControlCode: ULONG;
  DeviceObject: PDEVICE_OBJECT;
  InputBuffer: PVOID; // OPTIONAL,
  InputBufferLength: ULONG;
  OutputBuffer: PVOID; // OUT OPTIONAL
  OutputBufferLength: ULONG;
  InternalDeviceIoControl: _BOOLEAN;
  Event: PKEVENT;
  IoStatusBlock: PIO_STATUS_BLOCK // OUT
  ): PIRP; stdcall;
function {062} xboxkrnl_IoBuildSynchronousFsdRequest(
  MajorFunction: ULONG;
  DeviceObject: PDEVICE_OBJECT;
  Buffer: PVOID; // OUT OPTIONAL,
  Length: ULONG; // OPTIONAL,
  StartingOffset: PLARGE_INTEGER; // OPTIONAL,
  Event: PKEVENT;
  IoStatusBlock: PIO_STATUS_BLOCK // OUT
  ): PIRP; stdcall;
function {063} xboxkrnl_IoCheckShareAccess(
  DesiredAccess: ACCESS_MASK;
  DesiredShareAccess: ULONG;
  FileObject: PFILE_OBJECT; // OUT
  ShareAccess: PSHARE_ACCESS; // OUT
  Update: _BOOLEAN
  ): NTSTATUS; stdcall;
function {065} xboxkrnl_IoCreateDevice(
  DriverObject: PDRIVER_OBJECT;
  DeviceExtensionSize: ULONG;
  DeviceName: PUNICODE_STRING;
  DeviceType: DEVICE_TYPE;
  DeviceCharacteristics: ULONG;
  Exclusive: _BOOLEAN;
  DeviceObject: PPDEVICE_OBJECT // OUT
  ): NTSTATUS; stdcall;
function {066} xboxkrnl_IoCreateFile(
  FileHandle: PHANDLE; // OUT
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  AllocationSize: PLARGE_INTEGER;
  FileAttributes: ULONG;
  ShareAccess: ULONG;
  Disposition: ULONG;
  CreateOptions: ULONG;
  Options: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_IoCreateSymbolicLink(
  SymbolicLinkName: PSTRING;
  DeviceName: PSTRING
  ): NTSTATUS; stdcall;
function xboxkrnl_IoDeleteDevice(
  DeviceObject: PDEVICE_OBJECT
): NTSTATUS; stdcall;
function xboxkrnl_IoDeleteSymbolicLink(
  SymbolicLinkName: PSTRING
  ): NTSTATUS; stdcall;
function xboxkrnl_IoFreeIrp(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoInitializeIrp(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoInvalidDeviceRequest(
  DeviceObject: PDEVICE_OBJECT;
  Irp: PIRP
): NTSTATUS; stdcall;
function xboxkrnl_IoQueryFileInformation(
  FileObject: PFILE_OBJECT;
  FileInformationClass: FILE_INFORMATION_CLASS;
  Length: ULONG;
  FileInformation: PVOID; // OUT
  ReturnedLength: PULONG // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_IoQueryVolumeInformation(
  FileObject: PFILE_OBJECT;
  FsInformationClass: FS_INFORMATION_CLASS;
  Length: ULONG;
  FsInformation: PVOID; // OUT
  ReturnedLength: PULONG // OUT
  ): NTSTATUS; stdcall;
function xboxkrnl_IoQueueThreadIrp(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoRemoveShareAccess(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoSetIoCompletion(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoSetShareAccess(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoStartNextPacket(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoStartNextPacketByKey(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoStartPacket(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IoSynchronousDeviceIoControlRequest(
  IoControlCode: ULONG ;
  DeviceObject: PDEVICE_OBJECT;
  InputBuffer: PVOID ; // OPTIONAL
  InputBufferLength: ULONG;
  OutputBuffer: PVOID; // OPTIONAL
  OutputBufferLength: ULONG;
  unknown_use_zero: PDWORD; // OPTIONAL
  InternalDeviceIoControl: _BOOLEAN
  ): NTSTATUS; stdcall;
function xboxkrnl_IoSynchronousFsdRequest(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE
function xboxkrnl_IofCallDriver(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  Irp: PIRP;
  DeviceObject: PDEVICE_OBJECT
  ): NTSTATUS; register;
procedure xboxkrnl_IofCompleteRequest(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  PriorityBoost: CCHAR;
  Irp: PIRP
  ); register;
function xboxkrnl_IoDismountVolume(
  DeviceObject: PDEVICE_OBJECT
): NTSTATUS; stdcall;
function xboxkrnl_IoDismountVolumeByName(
  VolumeName: PSTRING
  ): NTSTATUS; stdcall;
function xboxkrnl_IoMarkIrpMustComplete(): NTSTATUS; stdcall; // UNKNOWN_SIGNATURE

implementation

const lfUnit = lfDxbx or lfKernel;

function {059} xboxkrnl_IoAllocateIrp(
  StackSize: CCHAR
  ): PIRP; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('IoAllocateIrp');
  Result := nil;
  EmuSwapFS(fsXbox);
end;

function {060} xboxkrnl_IoBuildAsynchronousFsdRequest(
  MajorFunction: ULONG;
  DeviceObject: PDEVICE_OBJECT;
  Buffer: PVOID; // OUT OPTIONAL
  Length: ULONG; // OPTIONAL,
  StartingOffset: PLARGE_INTEGER; // OPTIONAL
  IoStatusBlock: PIO_STATUS_BLOCK // OPTIONAL
  ): PIRP; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('IoBuildAsynchronousFsdRequest');
  Result := nil;
  EmuSwapFS(fsXbox);
end;

function {061} xboxkrnl_IoBuildDeviceIoControlRequest(
  IoControlCode: ULONG;
  DeviceObject: PDEVICE_OBJECT;
  InputBuffer: PVOID; // OPTIONAL,
  InputBufferLength: ULONG;
  OutputBuffer: PVOID; // OUT OPTIONAL
  OutputBufferLength: ULONG;
  InternalDeviceIoControl: _BOOLEAN;
  Event: PKEVENT;
  IoStatusBlock: PIO_STATUS_BLOCK // OUT
  ): PIRP; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('IoBuildDeviceIoControlRequest');
  Result := nil;
  EmuSwapFS(fsXbox);
end;

function {062} xboxkrnl_IoBuildSynchronousFsdRequest(
  MajorFunction: ULONG;
  DeviceObject: PDEVICE_OBJECT;
  Buffer: PVOID; // OUT OPTIONAL,
  Length: ULONG; // OPTIONAL,
  StartingOffset: PLARGE_INTEGER; // OPTIONAL,
  Event: PKEVENT;
  IoStatusBlock: PIO_STATUS_BLOCK // OUT
  ): PIRP; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('IoBuildSynchronousFsdRequest');
  Result := nil;
  EmuSwapFS(fsXbox);
end;

function {063} xboxkrnl_IoCheckShareAccess(
  DesiredAccess: ACCESS_MASK;
  DesiredShareAccess: ULONG;
  FileObject: PFILE_OBJECT; // OUT
  ShareAccess: PSHARE_ACCESS; // OUT
  Update: _BOOLEAN
  ): NTSTATUS; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoCheckShareAccess');
  EmuSwapFS(fsXbox);
end;

function {065} xboxkrnl_IoCreateDevice(
  DriverObject: PDRIVER_OBJECT;
  DeviceExtensionSize: ULONG;
  DeviceName: PUNICODE_STRING;
  DeviceType: DEVICE_TYPE;
  DeviceCharacteristics: ULONG;
  Exclusive: _BOOLEAN;
  DeviceObject: PPDEVICE_OBJECT // OUT
  ): NTSTATUS; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  LogBegin('EmuKrnl : IoCreateDevice').
    _(DriverObject, 'DriverObject').
    _(DeviceExtensionSize, 'DeviceExtensionSize').
    _(DeviceName, 'DeviceName').
    _(DeviceType, 'DeviceType').
    _(DeviceCharacteristics, 'DeviceCharacteristics').
    _(Exclusive, 'Exclusive').
    _(DeviceObject, 'DeviceObject').
  LogEnd();

  Result := Unimplemented('IoCreateDevice');

  EmuSwapFS(fsXbox);
end;

function {066} xboxkrnl_IoCreateFile
(
  FileHandle: PHANDLE; // OUT
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES;
  IoStatusBlock: PIO_STATUS_BLOCK; // OUT
  AllocationSize: PLARGE_INTEGER;
  FileAttributes: ULONG;
  ShareAccess: ULONG;
  Disposition: ULONG;
  CreateOptions: ULONG;
  Options: ULONG
): NTSTATUS; stdcall;
// Source:Cxbx  Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  LogBegin('EmuKrnl : IoCreateFile').
    _(FileHandle, 'FileHandle').
    _ACCESS_MASK(DesiredAccess, 'DesiredAccess').
    _(ObjectAttributes, 'ObjectAttributes').
    _(IoStatusBlock, 'IoStatusBlock').
    _(AllocationSize, 'AllocationSize').
    _FileAttributes(FileAttributes, 'FileAttributes').
    _ShareAccess(ShareAccess, 'ShareAccess').
    _CreateDisposition(Disposition, 'Disposition').
    _CreateOptions(CreateOptions, 'CreateOptions').
    _(Options, 'Options').
  LogEnd();
{$ENDIF}

  Result := STATUS_SUCCESS;

  // TODO -oCXBX: Try redirecting to NtCreateFile if this function ever is run into
  DxbxKrnlCleanup('IoCreateFile not implemented');

  EmuSwapFS(fsXbox);
end;

// IoCreateSymbolicLink:
// Creates a symbolic link in the object namespace.
// NtCreateSymbolicLinkObject is much harder to use than this simple
// function, so just use this one.
//
// Differences from NT: Uses ANSI_STRING instead of UNICODE_STRING.
function xboxkrnl_IoCreateSymbolicLink
(
  SymbolicLinkName: PSTRING;
  DeviceName: PSTRING
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:60
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuKrnl : IoCreateSymbolicLink').
      _(SymbolicLinkName, 'SymbolicLinkName').
      _(DeviceName, 'DeviceName').
    LogEnd();

  Result := DxbxCreateSymbolicLink(PSTRING_String(SymbolicLinkName), PSTRING_String(DeviceName));

  if MayLog(lfUnit or lfReturnValue) then
    DbgPrintf('IoCreateSymbolicLink returns 0x%.8x (%s)', [Result, NTStatusToString(Result)]);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoDeleteDevice(
  DeviceObject: PDEVICE_OBJECT
): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoDeleteDevice');
  EmuSwapFS(fsXbox);
end;

// IoDeleteSymbolicLink:
// Creates a symbolic link in the object namespace.  Deleting symbolic links
// through the Nt* functions is a pain, so use this instead.
//
// Differences from NT: Uses ANSI_STRING instead of UNICODE_STRING.
function xboxkrnl_IoDeleteSymbolicLink
(
  SymbolicLinkName: PSTRING
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  EmuNtSymbolicLinkObject: TEmuNtSymbolicLinkObject;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  LogBegin('EmuKrnl : IoDeleteSymbolicLink').
    _(SymbolicLinkName, 'SymbolicLinkName').
  LogEnd();
{$ENDIF}

  EmuNtSymbolicLinkObject := FindNtSymbolicLinkObjectByName(PSTRING_String(SymbolicLinkName));
  if Assigned(EmuNtSymbolicLinkObject) then
    // Destroy the object once all handles to it are closed too :
    Result := EmuNtSymbolicLinkObject.NtClose
  else
    Result := STATUS_OBJECT_NAME_NOT_FOUND;

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoFreeIrp(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoFreeIrp');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoInitializeIrp(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoInitializeIrp');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoInvalidDeviceRequest(
  DeviceObject: PDEVICE_OBJECT;
  Irp: PIRP
): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoInvalidDeviceRequest');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoQueryFileInformation(
  FileObject: PFILE_OBJECT;
  FileInformationClass: FILE_INFORMATION_CLASS;
  Length: ULONG;
  FileInformation: PVOID; // OUT
  ReturnedLength: PULONG // OUT
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  // Dxbx note : This is almost identical to NtQueryInformationFile
  Result := Unimplemented('IoQueryFileInformation');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoQueryVolumeInformation(
  FileObject: PFILE_OBJECT;
  FsInformationClass: FS_INFORMATION_CLASS;
  Length: ULONG;
  FsInformation: PVOID; // OUT
  ReturnedLength: PULONG // OUT
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  // Dxbx note : This is almost identical to NtQueryVolumeInformationFile
  // DxbxPC2XB_FS_INFORMATION
  Result := Unimplemented('IoQueryVolumeInformation');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoQueueThreadIrp(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoQueueThreadIrp');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoRemoveShareAccess(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoRemoveShareAccess');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoSetIoCompletion(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoSetIoCompletion');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoSetShareAccess(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoSetShareAccess');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoStartNextPacket(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoStartNextPacket');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoStartNextPacketByKey(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoStartNextPacketByKey');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoStartPacket(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoStartPacket');
  EmuSwapFS(fsXbox);
end;

// IoSynchronousDeviceIoControlRequest:
// NICE.  Makes kernel driver stuff sooooo much easier.  This does a
// blocking IOCTL on the specified device.
//
// New to the XBOX.
function xboxkrnl_IoSynchronousDeviceIoControlRequest(
  IoControlCode: ULONG ;
  DeviceObject: PDEVICE_OBJECT;
  InputBuffer: PVOID ; // OPTIONAL
  InputBufferLength: ULONG;
  OutputBuffer: PVOID; // OPTIONAL
  OutputBufferLength: ULONG;
  unknown_use_zero: PDWORD; // OPTIONAL
  InternalDeviceIoControl: _BOOLEAN
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoSynchronousDeviceIoControlRequest');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoSynchronousFsdRequest(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoSynchronousFsdRequest');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IofCallDriver(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}Irp: PIRP;
  {1 ECX}DeviceObject: PDEVICE_OBJECT
  ): NTSTATUS; register; // fastcall simulation - See Translation guide
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IofCallDriver');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_IofCompleteRequest(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}PriorityBoost: CCHAR;
  {1 ECX}Irp: PIRP
  ); register; // fastcall simulation - See Translation guide
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('IofCompleteRequest');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoDismountVolume(
  DeviceObject: PDEVICE_OBJECT
): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoDismountVolume');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoDismountVolumeByName
(
  VolumeName: PSTRING
): NTSTATUS; stdcall;
// Source:OpenXDK  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
  LogBegin('EmuKrnl : IoDismountVolumeByName').
    _(VolumeName, 'VolumeName').
  LogEnd();
{$ENDIF}

  // TODO -oCXBX: Anything?
  Result := STATUS_SUCCESS;

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_IoMarkIrpMustComplete(): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('IoMarkIrpMustComplete');
  EmuSwapFS(fsXbox);
end;

end.
