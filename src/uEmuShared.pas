unit uEmuShared;

interface


//procedure SetXbePath(const path : String ); export;
function EmuSharedInit : Boolean;
procedure EmuSharedCleanup;


implementation

Uses
  uEnums, uLog, Dialogs;    

Var
  m_XbePath : String;
  g_EmuSharedRefCount : Integer;

function EmuSharedInit : Boolean;
begin
  CreateLogs(ltKernel);
  WriteLog('EmuSharedInit');

  // Ensure initialization only occurs once

  // Prevent multiple initializations
   // if(hMapObject != NULL)
   //     return;

    // ******************************************************************
    // * Create the shared memory "file"
    // ******************************************************************
    {
        hMapObject = CreateFileMapping
        ( 
            INVALID_HANDLE_VALUE,   // Paging file
            NULL,                   // default security attributes
            PAGE_READWRITE,         // read/write access
            0,                      // size: high 32 bits
            sizeof(EmuShared),      // size: low 32 bits
            "Local\\EmuShared"      // name of map object
        );

        if(hMapObject == NULL)
			EmuCleanup("Could not map shared memory!");

        if(GetLastError() == ERROR_ALREADY_EXISTS)
            init = false;
    }

    // ******************************************************************
    // * Memory map this file
    // ******************************************************************
    {
//      g_EmuShared = (EmuShared*.MapViewOfFile
        (
            hMapObject,     // object to map view of
            FILE_MAP_WRITE, // read/write access
            0,              // high offset:  map from
            0,              // low offset:   beginning
            0               // default: map entire file
        );

        if(g_EmuShared == NULL) 
			EmuCleanup("Could not map view of shared memory!");
    }

    // ******************************************************************
    // * Executed only on first initialization of shared memory
    // ******************************************************************
   { if init then
       EmuShared();
                }
   Inc ( g_EmuSharedRefCount );
end;

procedure EmuSharedCleanup;
begin
  WriteLog ( 'EmuSharedCleanup' );
  Dec ( g_EmuSharedRefCount );

(*  if(g_EmuSharedRefCount = 0)
    EmuShared();

  UnmapViewOfFile(g_EmuShared); *)
  CloseLogs;
end;

(*procedure SetXbePath(const path : String ); export;
begin
  { TODO : Need to be translated from c to delphi }
  //Lock();
  m_XbePath := Path;
  //Unlock();
end;*)


end.
