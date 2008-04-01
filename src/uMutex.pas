unit uMutex;

interface

procedure Lock;
procedure Unlock;

implementation

procedure Lock;
begin
{
    while(true)
    {
        // Grab the lock, letting us look at the variables
        while(InterlockedCompareExchange((LPVOID*)&m_MutexLock, (LPVOID)1, (LPVOID)0))
//        while(InterlockedCompareExchange((LPLONG)&m_MutexLock, (LONG)1, (LONG)0))
            Sleep(1);

        // Are we the the new owner?
        if (!m_OwnerProcess)
        {
            // Take ownership
            InterlockedExchange(&m_OwnerProcess, (LONG)GetCurrentProcessId());
            InterlockedExchange(&m_OwnerThread, (LONG)GetCurrentThreadId());
            InterlockedExchange(&m_LockCount, 1);

            // Unlock the mutex itself
            InterlockedExchange(&m_MutexLock, 0);

            return;
        }

        // If a different process owns this mutex right now, unlock
        // the mutex lock and wait.  The reading need not be
        // interlocked.
(*        if ((m_OwnerProcess != (LONG) GetCurrentProcessId()) ||
            (m_OwnerThread  != (LONG) GetCurrentThreadId()))
        {
            // Unlock the mutex itself
            InterlockedExchange(&m_MutexLock, 0);

            // Wait and try again
            Sleep(1);
            continue;
        }

        // The mutex was already locked, but by us.  Just increment
        // the lock count and unlock the mutex itself.
        InterlockedIncrement(&m_LockCount);
        InterlockedExchange(&m_MutexLock, 0);

        return;
    }      *)
end;

procedure Unlock;
begin
({
    // Grab the lock, letting us look at the variables
    while(InterlockedCompareExchange((LPVOID*)&m_MutexLock, (LPVOID)1, (LPVOID)0))
//    while (InterlockedCompareExchange((LPLONG)&m_MutexLock, (LONG)1, (LONG)0))
        Sleep(1);

    // Decrement the lock count
    if (!InterlockedDecrement(&m_LockCount))
    {
        // Mark the mutex as now unused
        InterlockedExchange(&m_OwnerProcess, 0);
        InterlockedExchange(&m_OwnerThread, 0);
    }

    // Unlock the mutex itself
(*    InterlockedExchange(&m_MutexLock, 0);
} *)
end;




end.
