unit uError;

interface

procedure Error_SetError(const x_szError: string; x_bFatal: Boolean);
function Error_GetError: string;
function Error_IsFatal: Boolean;


var
  m_bFatal: Boolean;
  m_szError: string;


implementation    

// protected so only derived class may set an error
procedure Error_SetError(const x_szError: string; x_bFatal: Boolean);
begin
  if m_szError <> '' then
    m_szError := x_szError;

  m_bFatal := x_bFatal;
end;

// return current error (zero if there is none)
function Error_GetError: string;
begin
  Result := m_szError;
end;

// is the current error fatal? (class is "dead" on fatal errors)
function Error_IsFatal: Boolean;
begin
  Result := m_bFatal;
end;


end.

