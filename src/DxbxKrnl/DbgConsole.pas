unit DbgConsole;

interface

uses
  // Delphi
  Windows,
  SysUtils,
  StrUtils,
  // Dxbx
  uLog,
  uDxbxKrnlUtils,
  uDxbxUtils,
  uEmuAlloc,
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
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
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
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:90
begin
  m_cur := 0;

  DbgPrintf('DxbxDbg');
//  fflush(stdout);

  m_szInput[0] := #0;
end;

procedure TDbConsole.ParseCommand;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:80
var
  szCmd : Array [0..32-1] of AnsiChar;
  Full, v, n, m, c: Integer;
  cur: PRTNode;
  enabled: Boolean;
begin
  Dbgprintf('');


  szCmd[0] := #0;

(*  sscanf(m_szInput, '%s', szCmd); *)

  // TODO: as command list grows, turn into static string/ptr lookup
  if (CompareStr(szCmd, 'h') = 0) or (CompareStr(szCmd, 'help') = 0) then
  begin
    Dbgprintf('DxbxDbg: ');
    Dbgprintf('DxbxDbg: Cxbx Debug Command List:');
    Dbgprintf('DxbxDbg: ');
    Dbgprintf('DxbxDbg:  Help            [H]     : Show Command List');
    Dbgprintf('DxbxDbg:  Quit/Exit       [Q]     : Stop Emulation');
    Dbgprintf('DxbxDbg:  Trace           [T]     : Toggle Debug Trace');

    {$ifdef _DEBUG_TRACK_VB}
    Dbgprintf('DxbxDbg:  ListVB          [LVB]   : List Active Vertex Buffers');
    Dbgprintf('DxbxDbg:  DisableVB       [DVB #] : Disable Active Vertex Buffer(s)');
    Dbgprintf('DxbxDbg:  EnableVB        [EVB #] : Enable Active Vertex Buffer(s)');
    Dbgprintf('DxbxDbg:  DumpStreamCache [DSC]   : Dumps the patched streams cache');
    {$endif}

    {$ifdef _DEBUG_TRACK_PB}
    Dbgprintf('DxbxDbg:  ListPB          [LPB]   : List Active Push Buffers');
    Dbgprintf('DxbxDbg:  ShowPB          [SPB #] : Show Push Buffer(s)');
    Dbgprintf('DxbxDbg:  DisablePB       [DPB #] : Disable Push Buffer(s)');
    Dbgprintf('DxbxDbg:  EnablePB        [EPB #] : Enable Push Buffer(s)');
    Dbgprintf('DxbxDbg:  ClearPB         [CPB]   : Clear Push Buffer List');
    {$endif}

    {$ifdef _DEBUG_ALLOC}
    Dbgprintf('DxbxDbg:  DumpMem         [DMEM]  : Dump the heap allocation tracking table');
    {$endif} // _DEBUG_ALLOCC

    Dbgprintf('DxbxDbg:  CLS');
    Dbgprintf('DxbxDbg: ');
    Dbgprintf('DxbxDbg: # denotes parameter of form [#] or [#-#]');
    Dbgprintf('DxbxDbg: ');
  end
  else if (CompareStr(szCmd, 'q') = 0) or (CompareStr(szCmd, 'quit') = 0) or (CompareStr(szCmd, 'exit') = 0) then
  begin
    Dbgprintf('DxbxDbg: Goodbye...');
    DxbxKrnlCleanup('');
  end
  else if (CompareStr(szCmd, 't') = 0) or (CompareStr(szCmd, 'trace') = 0) then
  begin
(*        g_bPrintfOn = not g_bPrintfOn;
      Dbgprintf('DxbxDbg: Trace is now %s', ifthen(g_bPrintfOn, 'ON', 'OFF')); *)
  end
  {$ifdef _DEBUG_TRACK_VB}
  else if(CompareStr(szCmd, 'lvb') = 0) or (CompareStr(szCmd, 'ListVB') = 0) then
  begin
    v := 0;

    g_VBTrackTotal.Lock();

    cur := g_VBTrackTotal.getHead();

    while(cur <> nil) and (cur.pNext <> nil) do
    begin
      enabled := not g_VBTrackDisable.exists(cur.pResource);

      Inc(v);
      Dbgprintf('DxbxDbg: %.02d : 0x%.08X (%s)', [v, cur.pResource, ifthen(enabled, 'enabled', 'disabled')]);

      cur := cur.pNext;
    end;

    g_VBTrackTotal.Unlock();
  end
  else if(CompareStr(szCmd, 'dvb') = 0) or (CompareStr(szCmd, 'DisableVB') = 0) then
  begin
    n := 0;
    m := 0;

    c := sscanf(m_szInput, '%*s %d-%d', @n, @m);

    case  c of
      1 : EnableTracker(g_VBTrackTotal, g_VBTrackDisable, n, n, ETA_DISABLE);
      2 : EnableTracker(g_VBTrackTotal, g_VBTrackDisable, n, m, ETA_DISABLE);
    else
      Dbgprintf('DxbxDbg: Syntax Incorrect (dvb #)');
    end;
  end
  else if(CompareStr(szCmd, 'evb') = 0) or (CompareStr(szCmd, 'EnableVB') = 0) then
  begin
    n := 0;
    m := 0;

    c := sscanf(m_szInput, '%*s %d-%d', @n, @m);

    case c of
      1 : EnableTracker(g_VBTrackTotal, g_VBTrackDisable, n, n, ETA_ENABLE);
      2 : EnableTracker(g_VBTrackTotal, g_VBTrackDisable, n, m, ETA_ENABLE);
    else
      Dbgprintf('DxbxDbg: Syntax Incorrect (dvb #)');
    end;
  end;
  else if(CompareStr(szCmd, 'dsc') = 0) or (CompareStr(szCmd, 'DumpStreamCache') = 0) then
  begin
     (* XTL_VertexPatcher. DumpCache(); *)
  end
  {$endif}
  {$ifdef _DEBUG_TRACK_PB}
  else if(CompareStr(szCmd, 'lpb') = 0) or (CompareStr(szCmd, 'ListPB') = 0) then
  begin
    v := 0;

    g_PBTrackTotal.Lock();

    cur := g_PBTrackTotal.getHead();

    while(cur <> nil) and (cur.pNext <> nil) then
    begin
      enabled := not g_PBTrackDisable.exists(cur.pResource);

      Inc(v);
      Dbgprintf('DxbxDbg: %.02d : 0x%.08X (%s)', [v, cur.pResource, ifthen(enabled, 'enabled', 'disabled']);

      cur := cur.pNext;
    end;

    g_PBTrackTotal.Unlock();
  end
  else if(CompareStr(szCmd, 'spb') = 0) or (CompareStr(szCmd, 'ShowPB') = 0) then
  begin
    n := 0;
    m := 0;

    c := sscanf(m_szInput, '%*s %d-%d', @n, @m);

    case c of
      1 : EnableTracker(g_PBTrackTotal, g_PBTrackShowOnce, n, n, ETA_SHOW);
      2 : EnableTracker(g_PBTrackTotal, g_PBTrackShowOnce, n, m, ETA_SHOW);
    else
      Dbgprintf('DxbxDbg: Syntax Incorrect (spb #)');
    end;
  end
  else if(CompareStr(szCmd, 'dpb') = 0) or (CompareStr(szCmd, 'DisablePB') = 0) then
  begin
    n := 0;
    m := 0;

    c := sscanf(m_szInput, '%*s %d-%d', @n, @m);

    case c of
      1 : EnableTracker(g_PBTrackTotal, g_PBTrackDisable, n, n, ETA_DISABLE);
      2 : EnableTracker(g_PBTrackTotal, g_PBTrackDisable, n, m, ETA_DISABLE);
    else
      Dbgprintf('DxbxDbg: Syntax Incorrect (dpb #)');
    end;
  end
  else if(CompareStr(szCmd, 'epb') = 0) or (CompareStr(szCmd, 'EnablePB') = 0) then
  begin
    n := 0;
    m := 0;

    c := sscanf(m_szInput, '%*s %d-%d', @n, @m);

    case c of
      1 : EnableTracker(g_PBTrackTotal, g_PBTrackDisable, n, n, ETA_ENABLE);
      2 : EnableTracker(g_PBTrackTotal, g_PBTrackDisable, n, m, ETA_ENABLE);
    else
      Dbgprintf('DxbxDbg: Syntax Incorrect (dpb #)');
    end;
  end
  else if(CompareStr(szCmd, 'cpb') = 0) or (CompareStr(szCmd, 'ClearPB') = 0) then
  begin
    g_PBTrackTotal.clear();

    Dbgprintf('DxbxDbg: Push Buffer List Cleared!');
  end
  {$endif}
  {$ifdef _DEBUG_ALLOC}
  else if(CompareStr(szCmd, 'dmem') = 0) or (CompareStr(szCmd, 'DumpMem') = 0) then
  begin
    c := sscanf(m_szInput, '%*s %d', @Full);
    DxbxAllocDump(Full <> 0);
  end
  {$endif} // _DEBUG_ALLOC
  else if(CompareStr(szCmd, 'cls') = 0) then
  begin
      // clear screen using system call
(*        system('cls'); *)
  end
  else
  begin
      Dbgprintf('DxbxDbg: Cmd \%s\ not recognized!', [szCmd]);
  end;
end;

procedure TDbConsole.Process;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:0
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
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:80
begin
  m_cur := 0;

//  fflush(stdout);

  Dbgprintf('');

  Dbgprintf('DxbxDbg> ');
//  fflush(stdout);

  m_szInput[0] = #0;
end;

end.
