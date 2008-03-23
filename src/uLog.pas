unit uLog;

interface

uses
  uEnums;

procedure WriteLog(aText: string; aLogType: LogType = ltKernel); external 'LoggingScreens.dll';
procedure CreateLogs; external 'LoggingScreens.dll';
procedure CloseLogs; external 'LoggingScreens.dll';

procedure doLogKernelToFile(aValue: Boolean); external 'LoggingScreens.dll';
procedure doGuiToFile(aValue: Boolean); external 'LoggingScreens.dll';

function hasLogKernelToFile: Boolean; external 'LoggingScreens.dll';
function hasGuiToFile: Boolean; external 'LoggingScreens.dll';

implementation


end.
