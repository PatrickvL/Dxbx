unit DbgConsole;

interface

uses
  // Delphi
  Windows,
  // Dxbx
  uLog,
  uResourceTracker;

Type
  PDbgConsole = ^TDbConsole;
  TDbConsole = Class
  private
    m_szInput: Array[0..1024 -1] of AnsiChar;
    m_cur: UInt;
  public
    procedure DbgConsole;
    // process commands
    procedure Process;
    // parse an individual command
    procedure ParseCommand;
    // reset input buffer & display prompt
    procedure Reset;
  End;


{$ifdef _DEBUG_TRACK_VB) or defined(_DEBUG_TRACK_PB)}
  _ETAction = (
    ETA_ENABLE  = 0,
    ETA_DISABLE = 1,
    ETA_SHOW    = 2
  );
  ETAction = _ETAction;

  procedure EnableTracker(trackTotal: ResourceTracker; tracker: ResourceTracker; a: integer; b:integer; action: ETAction);
{$endif}

implementation


{$ifdef _DEBUG_TRACK_VB) or defined(_DEBUG_TRACK_PB)}

procedure EnableTracker(trackTotal: ResourceTracker; tracker: ResourceTracker; a: integer; b:integer; action:ETAction);
var
  v: Integer;
  i: Integer;
  cur: PRTNode;
begin
  trackTotal.Lock();

  cur := trackTotal.getHead();

  for v := 0 to v < a do
  begin
    if (cur = nil) or (cur.pNext = nil) then
      break;

    cur := cur.pNext;
  end;

  if((a = v) and (cur <> nil) and (cur.pNext <> nil)) then
  begin
    while a <= b do
    begin
      if((cur = nil) or (cur.pNext = nil)) then
        break;

      if(action = ETA_ENABLE) then
        Dbgprintf('DxbxDbg: %.02d (0x%.08X) enabled', [a, cur.pResource])
      else if(action = ETA_DISABLE) then
        Dbgprintf('DxbxDbg: %.02d (0x%.08X) disabled', [a, cur.pResource])
      else if(action = ETA_SHOW) then
        Dbgprintf('DxbxDbg: %.02d (0x%.08X) queued for show info..', [a, cur.pResource]);

      if(action = ETA_ENABLE) then
      begin
        tracker.remove(cur.pResource);
      end
      else
      begin
        tracker.insert(cur.pResource);
      end;

      cur := cur.pNext;
      inc(a);
    end;
  end
  else
  begin
    DbgPrintf('DxbxDbg: # out of range');
  end;

  trackTotal.Unlock();
end;

{$endif}


{ TDbConsole }

procedure TDbConsole.DbgConsole;
begin
  m_cur = 0;

  DbgPrintf('DxbxDbg');
//  fflush(stdout);

  m_szInput[0] = '#0';
end;

procedure TDbConsole.ParseCommand;
begin
(*    printf("\n");

    char szCmd[32];

    szCmd[0] = '\0';

    sscanf(m_szInput, "%s", szCmd);

    // TODO: as command list grows, turn into static string/ptr lookup

    if(_stricmp(szCmd, "h") == 0 || _stricmp(szCmd, "help") == 0)
    {
        printf("CxbxDbg: \n");
        printf("CxbxDbg: Cxbx Debug Command List:\n");
        printf("CxbxDbg: \n");
        printf("CxbxDbg:  Help            [H]     : Show Command List\n");
        printf("CxbxDbg:  Quit/Exit       [Q]     : Stop Emulation\n");
        printf("CxbxDbg:  Trace           [T]     : Toggle Debug Trace\n");

        #ifdef _DEBUG_TRACK_VB
        printf("CxbxDbg:  ListVB          [LVB]   : List Active Vertex Buffers\n");
        printf("CxbxDbg:  DisableVB       [DVB #] : Disable Active Vertex Buffer(s)\n");
        printf("CxbxDbg:  EnableVB        [EVB #] : Enable Active Vertex Buffer(s)\n");
        printf("CxbxDbg:  DumpStreamCache [DSC]   : Dumps the patched streams cache\n");
        #endif

        #ifdef _DEBUG_TRACK_PB
        printf("CxbxDbg:  ListPB          [LPB]   : List Active Push Buffers\n");
        printf("CxbxDbg:  ShowPB          [SPB #] : Show Push Buffer(s)\n");
        printf("CxbxDbg:  DisablePB       [DPB #] : Disable Push Buffer(s)\n");
        printf("CxbxDbg:  EnablePB        [EPB #] : Enable Push Buffer(s)\n");
        printf("CxbxDbg:  ClearPB         [CPB]   : Clear Push Buffer List\n");
        #endif

        #ifdef _DEBUG_ALLOC
        printf("CxbxDbg:  DumpMem         [DMEM]  : Dump the heap allocation tracking table\n");
        #endif // _DEBUG_ALLOCC

        printf("CxbxDbg:  CLS\n");
        printf("CxbxDbg: \n");
        printf("CxbxDbg: # denotes parameter of form [#] or [#-#]\n");
        printf("CxbxDbg: \n");
    }
    else if(_stricmp(szCmd, "q") == 0 || _stricmp(szCmd, "quit") == 0 || _stricmp(szCmd, "exit") == 0)
    {
        printf("CxbxDbg: Goodbye...\n");
        CxbxKrnlCleanup(NULL);
    }
    else if(_stricmp(szCmd, "t") == 0 || _stricmp(szCmd, "trace") == 0)
    {
        g_bPrintfOn = !g_bPrintfOn;
        printf("CxbxDbg: Trace is now %s\n", g_bPrintfOn ? "ON" : "OFF");
    }
    #ifdef _DEBUG_TRACK_VB
    else if(_stricmp(szCmd, "lvb") == 0 || _stricmp(szCmd, "ListVB") == 0)
    {
        int v=0;

        g_VBTrackTotal.Lock();

        RTNode *cur = g_VBTrackTotal.getHead();

        while(cur != NULL && cur->pNext != NULL)
        {
            bool enabled = !g_VBTrackDisable.exists(cur->pResource);

            printf("CxbxDbg: %.02d : 0x%.08X (%s)\n", v++, cur->pResource, enabled ? "enabled" : "disabled");

            cur = cur->pNext;
        }

        g_VBTrackTotal.Unlock();
    }
    else if(_stricmp(szCmd, "dvb") == 0 || _stricmp(szCmd, "DisableVB") == 0)
    {
        int n=0, m=0;

        int c = sscanf(m_szInput, "%*s %d-%d", &n, &m);

        if(c == 1)
        {
            EnableTracker(g_VBTrackTotal, g_VBTrackDisable, n, n, ETA_DISABLE);
        }
        else if(c == 2)
        {
            EnableTracker(g_VBTrackTotal, g_VBTrackDisable, n, m, ETA_DISABLE);
        }
        else
        {
            printf("CxbxDbg: Syntax Incorrect (dvb #)\n");
        }
    }
    else if(_stricmp(szCmd, "evb") == 0 || _stricmp(szCmd, "EnableVB") == 0)
    {
        int n=0, m=0;

        int c = sscanf(m_szInput, "%*s %d-%d", &n, &m);

        if(c == 1)
        {
            EnableTracker(g_VBTrackTotal, g_VBTrackDisable, n, n, ETA_ENABLE);
        }
        else if(c == 2)
        {
            EnableTracker(g_VBTrackTotal, g_VBTrackDisable, n, m, ETA_ENABLE);
        }
        else
        {
            printf("CxbxDbg: Syntax Incorrect (dvb #)\n");
        }
    }
    else if(_stricmp(szCmd, "dsc") == 0 || _stricmp(szCmd, "DumpStreamCache") == 0)
    {
        XTL::VertexPatcher::DumpCache();
    }
    #endif
    #ifdef _DEBUG_TRACK_PB
    else if(_stricmp(szCmd, "lpb") == 0 || _stricmp(szCmd, "ListPB") == 0)
    {
        int v=0;

        g_PBTrackTotal.Lock();

        RTNode *cur = g_PBTrackTotal.getHead();

        while(cur != NULL && cur->pNext != NULL)
        {
            bool enabled = !g_PBTrackDisable.exists(cur->pResource);

            printf("CxbxDbg: %.02d : 0x%.08X (%s)\n", v++, cur->pResource, enabled ? "enabled" : "disabled");

            cur = cur->pNext;
        }

        g_PBTrackTotal.Unlock();
    }
    else if(_stricmp(szCmd, "spb") == 0 || _stricmp(szCmd, "ShowPB") == 0)
    {
        int n=0, m=0;

        int c = sscanf(m_szInput, "%*s %d-%d", &n, &m);

        if(c == 1)
        {
            EnableTracker(g_PBTrackTotal, g_PBTrackShowOnce, n, n, ETA_SHOW);
        }
        else if(c == 2)
        {
            EnableTracker(g_PBTrackTotal, g_PBTrackShowOnce, n, m, ETA_SHOW);
        }
        else
        {
            printf("CxbxDbg: Syntax Incorrect (spb #)\n");
        }
    }
    else if(_stricmp(szCmd, "dpb") == 0 || _stricmp(szCmd, "DisablePB") == 0)
    {
        int n=0, m=0;

        int c = sscanf(m_szInput, "%*s %d-%d", &n, &m);

        if(c == 1)
        {
            EnableTracker(g_PBTrackTotal, g_PBTrackDisable, n, n, ETA_DISABLE);
        }
        else if(c == 2)
        {
            EnableTracker(g_PBTrackTotal, g_PBTrackDisable, n, m, ETA_DISABLE);
        }
        else
        {
            printf("CxbxDbg: Syntax Incorrect (dpb #)\n");
        }
    }
    else if(_stricmp(szCmd, "epb") == 0 || _stricmp(szCmd, "EnablePB") == 0)
    {
        int n=0, m=0;

        int c = sscanf(m_szInput, "%*s %d-%d", &n, &m);

        if(c == 1)
        {
            EnableTracker(g_PBTrackTotal, g_PBTrackDisable, n, n, ETA_ENABLE);
        }
        else if(c == 2)
        {
            EnableTracker(g_PBTrackTotal, g_PBTrackDisable, n, m, ETA_ENABLE);
        }
        else
        {
            printf("CxbxDbg: Syntax Incorrect (dpb #)\n");
        }
    }
    else if(_stricmp(szCmd, "cpb") == 0 || _stricmp(szCmd, "ClearPB") == 0)
    {
        g_PBTrackTotal.clear();

        printf("CxbxDbg: Push Buffer List Cleared!\n");
    }
    #endif
    #ifdef _DEBUG_ALLOC
    else if(_stricmp(szCmd, "dmem") == 0 || _stricmp(szCmd, "DumpMem") == 0)
    {
        int Full;
        int c = sscanf(m_szInput, "%*s %d", &Full);
        CxbxAllocDump(Full != 0);
    }
    #endif // _DEBUG_ALLOC
    else if(_stricmp(szCmd, "cls") == 0)
    {
        // clear screen using system call
        system("cls");
    }
    else
    {
        printf("CxbxDbg: Cmd \"%s\" not recognized!\n", szCmd);
    }
*)
end;

procedure TDbConsole.Process;
begin
   // process all queued key presses
(*    while(_kbhit())
    {
        char c = _getche();

        if(c == '\r')
        {
            ParseCommand();

            printf("CxbxDbg> ");
            fflush(stdout);

            m_szInput[0] = '\0';
            m_cur = 0;
        }
        else if(c == '\b')
        {
            if(m_cur > 0)
            {
                printf(" \b");

                m_szInput[--m_cur] = '\0';
            }
            else
            {
                printf(" ");
            }
        }
        else
        {
            m_szInput[m_cur++] = c;
            m_szInput[m_cur] = '\0';
        }
    } *)
end;

procedure TDbConsole.Reset;
begin
(*  m_cur = 0;

  fflush(stdout);

  printf("\n");

  printf("CxbxDbg> ");
  fflush(stdout);

  m_szInput[0] = '\0'; *)
end;

{$ifdef _DEBUG_TRACK_VB) or defined(_DEBUG_TRACK_PB)}

{$endif}

end.
