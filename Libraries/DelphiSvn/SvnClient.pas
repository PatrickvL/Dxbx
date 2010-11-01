{**********************************************************************************************************************}
{                                                                                                                      }
{ delphisvn: Subversion plugin for CodeGear Delphi                                                                     }
{                                                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); you may not use     }
{ this file except in compliance with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either      }
{ express or implied. See the License for the specific language governing rights and limitations under the License.    }
{                                                                                                                      }
{ The Original Code is SvnClient.pas.                                                                                  }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{   Uwe Schuster (uschuster)                                                                                           }
{   Christian Wimmer                                                                                                   }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains helper routines and classes encapsulating Subversion API calls.                                   }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnClient;

interface

{$INCLUDE Compilers.inc}

uses
  Windows, Classes, SysUtils, Contnrs, SyncObjs,
  apr, svn_client;

const
  DefaultPropValDelimiter = ';';

type
  TSvnClient = class;
  TSvnItem = class;

  TSvnBlameItem = class
  private
    FAuthor: string;
    FLine: string;
    FLineNo: Int64;
    FRevision: Integer;
    FTime: TDateTime;
  public
    property Author: string read FAuthor;
    property Line: string read FLine;
    property LineNo: Int64 read FLineNo;
    property Revision: Integer read FRevision;
    property Time: TDateTime read FTime;
  end;

  TSvnHistoryItem = class
  private
    FAuthor: string;
    FBlame: TList;
    FBlameError: string;
    FBlameNotify: TNotifyEvent;
    FBlameThread: TThread;
    FCancelBlame: Boolean;
    FDestroying: Boolean;
    FFile: string;
    FLogMessage: string;
    FOwner: TSvnItem;
    FRevision: Integer;
    FTime: TDateTime;

    procedure BlameCallback(Sender: TObject; LineNo: Int64; Revision: TSvnRevNum; const Author: string; Time: TDateTime;
      const Line: string; var Cancel: Boolean);
    procedure BlameThreadTerminate(Sender: TObject);
    procedure ClearBlame;
    function GetBlameCount: Integer;
    function GetBlameItems(Index: Integer): TSvnBlameItem;
    procedure ReloadBlame; overload;
    procedure ReloadBlame(ASvnClient: TSvnClient; const APathName: string; ABaseRevision: Integer); overload;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CancelBlame;
    function GetFile: string;
    function HasBlameLoaded: Boolean;
    function IsLoadingBlame: Boolean;
    procedure StartLoadingBlame(ANotify: TNotifyEvent = nil);

    property Author: string read FAuthor;
    property BlameCount: Integer read GetBlameCount;
    property BlameError: string read FBlameError;
    property BlameItems[Index: Integer]: TSvnBlameItem read GetBlameItems;
    property LogMessage: string read FLogMessage;
    property Owner: TSvnItem read FOwner;
    property Revision: Integer read FRevision;
    property Time: TDateTime read FTime;
  end;

  TSvnItem = class
  private
    FAbsent: Boolean;
    FBaseRevision: Integer;
    FCheckSum: string;
    FCommitAuthor: string;
    FCommittedRevision: Integer;
    FCommitTime: TDateTime;
    FConflictNewFile: string;
    FConflictOldFile: string;
    FConflictWorkingFile: string;
    FCopied: Boolean;
    FCopiedFromRevision: Integer;
    FCopiedFromURL: string;
    FDeleted: Boolean;
    FDestroyNotifications: TList;
    FFileAttr: Cardinal;
    FHistory: TList;
    FIncomplete: Boolean;
    FItems: TList;
    FKind: TSvnNodeKind;
    FLargeImageIndex: Integer;
    FLastCommitAuthor: string;
    FLastCommitRevision: Integer;
    FLastCommitTime: TDateTime;
    FLockComment: string;
    FLockDAVComment: Boolean;
    FLocked: Boolean;
    FLockExpirationTime: TDateTime;
    FLockOwner: string;
    FLockPath: string;
    FLockTime: TDateTime;
    FLockToken: string;
    FParent: TSvnItem;
    FPathName: string;
    FPropRejectFile: string;
    FProps: TStringList;
    FPropStatus: TSvnWCStatusKind;
    FPropTime: TDateTime;
    FPropValDelimiter: Char;
    FReloadExternals: TList;
    FReloadGlobalExternals: TList;
    FReloadRecursive: Boolean;
    FReloadStack: TStack;
    FReloadUnversioned: TList;
    FRemotePropStatus: TSvnWCStatusKind;
    FRemoteTextStatus: TSvnWCStatusKind;
    FRepository: string;
    FSchedule: TSvnWCSchedule;
    FSmallImageIndex: Integer;
    FSvnClient: TSvnClient;
    FSvnPathName: string;
    FSwitched: Boolean;
    FTag: Integer;
    FTextStatus: TSvnWCStatusKind;
    FTextTime: TDateTime;
    FURL: string;
    FUUID: string;

    procedure ClearHistory;
    procedure ClearItems;
    function GetCount: Integer;
    function GetExternal: Boolean;
    function GetFileAttr: Cardinal;
    function GetHistoryCount: Integer;
    function GetHistoryItems(Index: Integer): TSvnHistoryItem;
    function GetIsDirectory: Boolean;
    function GetItems(Index: Integer): TSvnItem;
    function GetPropCount: Integer;
    function GetPropNames(Index: Integer): string;
    function GetPropValueFromIndex(Index: Integer): string;
    function GetPropValues(const Name: string): string;
    procedure LoadStatus(const Status: TSvnWcStatus2);
    procedure LoadUnversionedPaths;
    procedure ReloadHistory;
    procedure ReloadProps;
    procedure SetPropValues(const Name, Value: string);
    procedure SortItems(Recurse: Boolean);
  protected
    procedure DoDestroyNotifications; virtual;
    procedure DoWCStatus(Path: PAnsiChar; const Status: TSvnWcStatus2);
  public
    constructor Create(ASvnClient: TSvnClient; AParent: TSvnItem; const APathName: string; Recurse: Boolean = False;
      Update: Boolean = False); overload;
    constructor Create(ASvnClient: TSvnClient; AParent: TSvnItem; const ASvnPathName: string;
      const Status: TSvnWcStatus2); overload;
    destructor Destroy; override;

    function Add(Item: TSvnItem): Integer;
    procedure AddDestroyNotification(Notification: TNotifyEvent);
    procedure Clear;
    function GetBaseFile: string;
    function GetCommittedFile: string;
    function IndexOf(Item: TSvnItem): Integer; overload;
    function IndexOf(const PathName: string; SvnPath: Boolean = False): Integer; overload;
    procedure Reload(Recurse: Boolean = False; Update: Boolean = False);
    procedure ReloadStatus;
    procedure Remove(Item: TSvnItem);
    procedure RemoveDestroyNotification(Notification: TNotifyEvent);
    procedure Resolved(Recurse: Boolean = False);

    property Absent: Boolean read FAbsent;
    property BaseRevision: Integer read FBaseRevision;
    property CheckSum: string read FCheckSum;
    property CommitAuthor: string read FCommitAuthor;
    property CommittedRevision: Integer read FCommittedRevision;
    property CommitTime: TDateTime read FCommitTime;
    property ConflictNewFile: string read FConflictNewFile;
    property ConflictOldFile: string read FConflictOldFile;
    property ConflictWorkingFile: string read FConflictWorkingFile;
    property Copied: Boolean read FCopied;
    property CopiedFromRevision: Integer read FCopiedFromRevision;
    property CopiedFromURL: string read FCopiedFromURL;
    property Count: Integer read GetCount;
    property Deleted: Boolean read FDeleted;
    property External: Boolean read GetExternal;
    property FileAttr: Cardinal read GetFileAttr;
    property HistoryCount: Integer read GetHistoryCount;
    property HistoryItems[Index: Integer]: TSvnHistoryItem read GetHistoryItems;
    property Incomplete: Boolean read FIncomplete;
    property Items[Index: Integer]: TSvnItem read GetItems; default;
    property IsDirectory: Boolean read GetIsDirectory;
    property Kind: TSvnNodeKind read FKind;
    property LargeImageIndex: Integer read FLargeImageIndex write FLargeImageIndex;
    property LastCommitAuthor: string read FLastCommitAuthor;
    property LastCommitRevision: Integer read FLastCommitRevision;
    property LastCommitTime: TDateTime read FLastCommitTime;
    property LockComment: string read FLockComment;
    property LockDAVComment: Boolean read FLockDAVComment;
    property LockExpirationTime: TDateTime read FLockExpirationTime;
    property Locked: Boolean read FLocked;
    property LockOwner: string read FLockOwner;
    property LockPath: string read FLockPath;
    property LockTime: TDateTime read FLockTime;
    property LockToken: string read FLockToken;
    property Parent: TSvnItem read FParent;
    property PathName: string read FPathName;
    property PropCount: Integer read GetPropCount;
    property PropNames[Index: Integer]: string read GetPropNames;
    property PropRejectFile: string read FPropRejectFile;
    property PropStatus: TSvnWCStatusKind read FPropStatus;
    property PropTime: TDateTime read FPropTime;
    property PropValDelimiter: Char read FPropValDelimiter write FPropValDelimiter default ';';
    property PropValueFromIndex[Index: Integer]: string read GetPropValueFromIndex;
    property PropValues[const Name: string]: string read GetPropValues write SetPropValues;
    property RemotePropStatus: TSvnWCStatusKind read FRemotePropStatus;
    property RemoteTextStatus: TSvnWCStatusKind read FRemoteTextStatus;
    property Repository: string read FRepository;
    property Schedule: TSvnWCSchedule read FSchedule;
    property SmallImageIndex: Integer read FSmallImageIndex write FSmallImageIndex;
    property SvnClient: TSvnClient read FSvnClient;
    property SvnPathName: string read FSvnPathName;
    property Switched: Boolean read FSwitched;
    property TextTime: TDateTime read FTextTime;
    property Tag: Integer read FTag write FTag;
    property TextStatus: TSvnWCStatusKind read FTextStatus;
    property URL: string read FURL;
    property UUID: string read FUUID;
  end;

  TSvnListItem = class(TObject)
  private
    FAbsolutePath: string;
    FCreatedRevision: TSvnRevNum;
    FHasProps: Boolean;
    FPath: string;
    FKind: TSvnNodeKind;
    FLastAuthor: string;
    FLockComment: string;
    FLockDAVComment: Boolean;
    FLockExpirationTime: TDateTime;
    FLocked: Boolean;
    FLockOwner: string;
    FLockPath: string;
    FLockTime: TDateTime;
    FLockToken: string;
    FSize: TSvnFileSize;
    FTime: TDateTime;
  public
    property AbsolutePath: string read FAbsolutePath write FAbsolutePath;
    property CreatedRevision: TSvnRevNum read FCreatedRevision write FCreatedRevision;
    property HasProps: Boolean read FHasProps write FHasProps;
    property Path: string read FPath write FPath;
    property Kind: TSvnNodeKind read FKind write FKind;
    property LastAuthor: string read FLastAuthor write FLastAuthor;
    property LockComment: string read FLockComment write FLockComment;
    property LockDAVComment: Boolean read FLockDAVComment write FLockDAVComment;
    property Locked: Boolean read FLocked write FLocked;
    property LockExpirationTime: TDateTime read FLockExpirationTime write FLockExpirationTime;
    property LockOwner: string read FLockOwner write FLockOwner;
    property LockPath: string read FLockPath write FLockPath;
    property LockTime: TDateTime read FLockTime write FLockTime;
    property LockToken: string read FLockToken write FLockToken;
    property Size: TSvnFileSize read FSize write FSize;
    property Time: TDateTime read FTime write FTime;
  end;

  TSvnList = class(TObject)
  private
    FDepth: TSvnDepth;
    FDirEntryFields: DWORD;
    FFetchLocks: Boolean;
    FItems: TObjectList;
    FPathName: string;
    FSvnClient: TSvnClient;
    procedure Clear;
    procedure ListCallback(Sender: TObject; Path: string; DirEntry: TSvnDirEnt; Locked: Boolean; LockData: TSvnLock;
      AbsPath: string; var Cancel: Boolean);
    function GetCount: Integer;
    function GetItems(Index: Integer): TSvnListItem;
  public
    constructor Create(ASvnClient: TSvnClient);
    destructor Destroy; override;
    procedure LoadList(const APathName: string; ADepth: TSvnDepth; AFetchLocks: Boolean; ADirEntryFields: DWORD = SVN_DIRENT_ALL);
    property Count: Integer read GetCount;
    property Depth: TSvnDepth read FDepth;
    property DirEntryFields: DWORD read FDirEntryFields;
    property FetchLocks: Boolean read FFetchLocks;
    property Items[Index: Integer]: TSvnListItem read GetItems; default;
    property PathName: string read FPathName;
  end;

  TSvnItemArray = array of TSvnItem;

  TSSLServerTrustFailures = set of (sslCertNotYetValid, sslCertExpired, sslCertHostNameMismatch,
    sslCertAuthorityUnknown, sslCertOther);

  TLoginPromptEvent = procedure(Sender: TObject; const Realm: string; var UserName, Password: string;
    var Cancel, Save: Boolean) of object;
  TUserNamePromptEvent = procedure(Sender: TObject; const Realm: string; var UserName: string;
    var Cancel, Save: Boolean) of object;
  TSSLServerTrustPrompt = procedure(Sender: TObject; const Realm: string; const CertInfo: TSvnAuthSSLServerCertInfo;
    Failures: TSSLServerTrustFailures; var Cancel, Save: Boolean) of object;
  TSSLClientCertPrompt = procedure(Sender: TObject; const Realm: string; var CertFileName: string;
    var Cancel, Save: Boolean) of object;
  TSSLClientPasswordPrompt = procedure(Sender: TObject; const Realm: string; var Password: string;
    var Cancel, Save: Boolean) of object;

  TSvnBlameCallback = procedure(Sender: TObject; LineNo: Int64; Revision: TSvnRevNum; const Author: string;
    Time: TDateTime; const Line: string; var Cancel: Boolean) of object;
  TSvnListCallback = procedure(Sender: TObject; Path: string; DirEntry: TSvnDirEnt; Locked: Boolean; LockData: TSvnLock;
    AbsPath: string; var Cancel: Boolean) of object;
  TSvnNotifyCallback = procedure(Sender: TObject; const Path, MimeType: string; Action: TSvnWcNotifyAction;
    Kind: TSvnNodeKind; ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum; var Cancel: Boolean)
    of object;
  TSvnStatusCallback = procedure(Sender: TObject; Item: TSvnItem; var Cancel: Boolean) of object;

  TSvnCancelCallback = procedure(Sender: TObject; var Cancel: Boolean) of object;
  TSvnProgressCallback = procedure(Sender: TObject; Progress, Total: TAprOff) of object;

  TSvnClient = class
  private
    FAllocator: PAprAllocator;
    FAprLibLoaded: Boolean;
    FCancelled: Boolean;
    FCommitLogMessage: string;
    FConfigDir: string;
    FCtx: PSvnClientCtx;
    FExternals: TStrings;
    FListStrings: TStrings;
    FPassword: string;
    FPool: PAprPool; // main pool
    FPoolUtf8: PAprPool; // pool for UTF-8 routines
    FRecurseUnversioned: Boolean;
    FSvnClientLibLoaded: Boolean;
    FUserName: string;

    FBlameCallback: TSvnBlameCallback;
    FBlameSubPool: PAprPool;
    FListCallback: TSvnListCallback;
    FNotifyCallback: TSvnNotifyCallback;
    FStatusCallback: TSvnStatusCallback;

    FOnLoginPrompt: TLoginPromptEvent;
    FOnSSLClientCertPrompt: TSSLClientCertPrompt;
    FOnSSLClientPasswordPrompt: TSSLClientPasswordPrompt;
    FOnSSLServerTrustPrompt: TSSLServerTrustPrompt;
    FOnUserNamePrompt: TUserNamePromptEvent;

    FOnCancel: TSvnCancelCallback;
    FOnProgress: TSvnProgressCallback;

    procedure GetExternalsCallback(Sender: TObject; Item: TSvnItem; var Cancel: Boolean);
    function GetInitialized: Boolean;
    procedure GetListCallback(Sender: TObject; Path: string; DirEntry: TSvnDirEnt; Locked: Boolean; LockData: TSvnLock;
      AbsPath: string; var Cancel: Boolean);
  protected
    function DoBlame(LineNo: Int64; Revision: TSvnRevNum; const Author, Date, Line: string): Boolean;
    function DoCancel: Boolean;
    function DoList(Path: string; DirEntry: TSvnDirEnt; Locked: Boolean; LockData: TSvnLock; AbsPath: string): Boolean;
    function DoLoginPrompt(const Realm: string; var UserName, Password: string; var Save: Boolean): Boolean; virtual;
    function DoNotify(const Path, MimeType: string; Action: TSvnWcNotifyAction; Kind: TSvnNodeKind;
      ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum): Boolean; virtual;
    procedure DoProgress(Progress, Total: TAprOff);
    function DoSSLClientCertPrompt(const Realm: string; var CertFileName: string; var Save: Boolean): Boolean; virtual;
    function DoSSLClientPasswordPrompt(const Realm: string; var Password: string; var Save: Boolean): Boolean; virtual;
    function DoSSLServerTrustPrompt(const Realm: string; const CertInfo: TSvnAuthSSLServerCertInfo;
      Failures: TSSLServerTrustFailures; var Save: Boolean): Boolean; virtual;
    function DoUserNamePrompt(const Realm: string; var UserName: string; var Save: Boolean): Boolean; virtual;
    function DoWCStatus(Path: PAnsiChar; const Status: TSvnWcStatus2): Boolean;

    property Cancelled: Boolean read FCancelled;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const PathName: string; Recurse: Boolean = False; Force: Boolean = False; NoIgnore: Boolean = False;
      SubPool: PAprPool = nil);
    procedure Blame(const PathName: string; Callback: TSvnBlameCallback; StartRevision: TSvnRevNum = 1;
      EndRevision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil);
    procedure Checkout(const PathName, TargetDir: string; Callback: TSvnNotifyCallback = nil; Recurse: Boolean = True;
      IgnoreExternals: Boolean = False; Revision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil);
    procedure Cleanup(const PathName: string; SubPool: PAprPool = nil);
    procedure Copy(SourcesNames: TStrings; const DstPath: string; SubPool: PAprPool = nil);
    function Commit(PathNames: TStrings; const LogMessage: string; Callback: TSvnNotifyCallback = nil;
      Recurse: Boolean = True; KeepLocks: Boolean = False; SubPool: PAprPool = nil): Boolean;
    procedure Delete(PathNames: TStrings; Force: Boolean = False; KeepLocal: Boolean = False; SubPool: PAprPool = nil);
    procedure Export(const PathName, TargetDir: string; Callback: TSvnNotifyCallback = nil; Overwrite: Boolean = False;
      Recurse: Boolean = True; IgnoreExternals: Boolean = False; Revision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil);
    procedure Finalize;
    procedure GetExternals(const PathName: string; Externals: TStrings; Recurse: Boolean = True);
    function GetModifications(const PathName: string; Callback: TSvnStatusCallback = nil;
      Recurse: Boolean = True; Update: Boolean = False; IgnoreExternals: Boolean = False;
      RecurseUnversioned: Boolean = False; SubPool: PAprPool = nil): TSvnRevNum;
    procedure GetProps(Props: PAprArrayHeader; Strings: TStrings; SubPool: PAprPool = nil;
      Delimiter: Char = DefaultPropValDelimiter);
    procedure Initialize(const AConfigDir: string = ''; Auth: PSvnAuthBaton = nil);
    function IsPathVersioned(const PathName: string): Boolean;
    function GetPathRevision(const PathName: string): TSvnOptRevision;
    procedure List(const PathName: string; Depth: TSvnDepth; FetchLocks: Boolean; DirEntryFields: DWORD = SVN_DIRENT_ALL;
      Callback: TSvnListCallback = nil; Revision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil); overload;
    procedure List(const PathName: string; Depth: TSvnDepth; FetchLocks: Boolean; ListStrings: TStrings;
      DirEntryFields: DWORD = SVN_DIRENT_ALL; Revision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil); overload;
    function MatchGlobalIgnores(const PathName: string; SubPool: PAprPool = nil): Boolean;
    procedure Move(SrcPathNames: TStrings; const DstPath: string; SubPool: PAprPool = nil);
    function NativePathToSvnPath(const NativePath: string; SubPool: PAprPool = nil): string;
    function PathNamesToAprArray(PathNames: TStrings; SubPool: PAprPool = nil): PAprArrayHeader; overload;
    function PathNamesToAprArray(const PathNames: array of string; SubPool: PAprPool = nil): PAprArrayHeader; overload;
    procedure Revert(PathNames: TStrings; Callback: TSvnNotifyCallback = nil; Recurse: Boolean = True;
      SubPool: PAprPool = nil);
    procedure Resolved(const SvnPath: string; Recurse: Boolean = False; SubPool: PAprPool = nil);
    function SvnPathToNativePath(const SvnPath: string; SubPool: PAprPool = nil): string;
    procedure Update(PathNames: TStrings; Callback: TSvnNotifyCallback = nil; Recurse: Boolean = True;
      IgnoreExternals: Boolean = False; SubPool: PAprPool = nil);

    property Allocator: PAprAllocator read FAllocator;
    property ConfigDir: string read FConfigDir;
    property Ctx: PSvnClientCtx read FCtx;
    property Initialized: Boolean read GetInitialized;
    property Password: string read FPassword write FPassword;
    property Pool: PAprPool read FPool;
    property PoolUtf8: PAprPool read FPoolUtf8;
    property RecurseUnversioned: Boolean read FRecurseUnversioned;
    property UserName: string read FUserName write FUserName;

    property OnLoginPrompt: TLoginPromptEvent read FOnLoginPrompt write FOnLoginPrompt;
    property OnSSLClientCertPrompt: TSSLClientCertPrompt read FOnSSLClientCertPrompt write FOnSSLClientCertPrompt;
    property OnSSLClientPasswordPrompt: TSSLClientPasswordPrompt read FOnSSLClientPasswordPrompt
      write FOnSSLClientPasswordPrompt;
    property OnSSLServerTrustPrompt: TSSLServerTrustPrompt read FOnSSLServerTrustPrompt write FOnSSLServerTrustPrompt;
    property OnUserNamePrompt: TUserNamePromptEvent read FOnUserNamePrompt write FOnUserNamePrompt;

    property OnCancel: TSvnCancelCallback read FOnCancel write FOnCancel;
    property OnProgress: TSvnProgressCallback read FOnProgress write FOnProgress;
  end;

resourcestring
  SNodeKindNone = 'None';
  SNodeKindFile = 'File';
  SNodeKindDir = 'Directory';
  SNodeKindUnknown = 'Unknown';

  SWcStatusNone = '';
  SWcStatusUnversioned = 'Unversioned';
  SWcStatusNormal = 'Normal';
  SWcStatusAdded = 'Added';
  SWcStatusMissing = 'Missing';
  SWcStatusDeleted = 'Deleted';
  SWcStatusReplaced = 'Replaced';
  SWcStatusModified = 'Modified';
  SWcStatusMerged = 'Merged';
  SWcStatusConflicted = 'Conflicted';
  SWcStatusIgnored = 'Ignored';
  SWcStatusObstructed = 'Obstructed';
  SWcStatusExternal = 'External';
  SWcStatusIncomplete = 'Incomplete';

  SWcNotifyAdd = 'Added';
  SWcNotifyCopy = 'Copied';
  SWcNotifyDelete = 'Deleted';
  SWcNotifyRestore = 'Restored';
  SWcNotifyRevert = 'Reverted';
  SWcNotifyFailedRevert = 'Revert Failed';
  SWcNotifyResolved = 'Resolved';
  SWcNotifySkip = 'Skipped';
  SWcNotifyUpdateDelete = 'Deleted';
  SWcNotifyUpdateAdd = 'Added';
  SWcNotifyUpdateUpdate = 'Updated';
  SWcNotifyUpdateCompleted = 'Completed';
  SWcNotifyUpdateExternal = 'External Updated';
  SWcNotifyStatusCompleted = 'Completed';
  SWcNotifyStatusExternal = 'External';
  SWcNotifyCommitModified = 'Modified';
  SWcNotifyCommitAdded = 'Added';
  SWcNotifyCommitDeleted = 'Deleted';
  SWcNotifyCommitReplaced = 'Replaced';
  SWcNotifyCommitPostfixTxdelta = 'File Sent';
  SWcNotifyBlameRevision = 'Blame Revision';
  SWcNotifyLocked = 'Locked';
  SWcNotifyUnlocked = 'Unlocked';
  SWcNotifyFailedLock = 'Lock Failed';
  SWcNotifyFailedUnlock = 'Unlock Failed';
  SWcNotifyExists = 'Exists';
  SWcNotifyChangelistSet = 'Changelist Set';
  SWcNotifyChangelistClear = 'Changelist Clear';
  SWcNotifyChangelistMoved = 'Changelist Moved';
  SWcNotifyMergeBegin = 'Merge Begin';
  SWcNotifyForeignMergeBegin = 'Foreign Merge Begin';
  SWcNotifyUpdateReplace = 'Update Replace';

const
  SvnLineBreak = #10;
  SvnPathDelim = '/';
  NodeKindStrings: array[TSvnNodeKind] of string = (SNodeKindNone, SNodeKindFile, SNodeKindDir, SNodeKindUnknown);

function AprTimeToDateTime(AprTime: TAprTime): TDateTime;
function DateTimeToAprTime(Value: TDateTime): TAprTime;
function SvnStrToDateTime(const S: string; Pool: PAprPool): TDateTime;
function TzToUTCDateTime(Value: TDateTime): TDateTime;
function UTCToTzDateTime(Value: TDateTime): TDateTime;

function FileAttrStr(Attr: Cardinal): string;
function NotifyActionStr(Action: TSvnWcNotifyAction): string;
function StatusKindStr(Status: TSvnWCStatusKind): string;
function SvnExcludeTrailingPathDelimiter(const S: string): string;
function SvnExtractFileDrive(const SvnFileName: string): string;
function SvnExtractFileName(const SvnFileName: string): string;
function SvnExtractFilePath(const SvnFileName: string): string;
function SvnIncludeTrailingPathDelimiter(const S: string): string;

implementation

uses
  RTLConsts, ActiveX, ComObj, ShlObj, TypInfo, WinSock;

type
  PMethod = ^TMethod;

{ helper routines }

//----------------------------------------------------------------------------------------------------------------------

function TzToUTCDateTime(Value: TDateTime): TDateTime;

var
  TZ: TTimeZoneInformation;

begin
  Result := Value;

  case GetTimeZoneInformation(TZ) of
    TIME_ZONE_ID_DAYLIGHT:
      Result := Result + (TZ.Bias + TZ.DaylightBias) / MinsPerDay;
    TIME_ZONE_ID_STANDARD:
      Result := Result + (TZ.Bias + TZ.StandardBias) / MinsPerDay;
    TIME_ZONE_ID_UNKNOWN:
      Result := Result + TZ.Bias / MinsPerDay;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function UTCToTzDateTime(Value: TDateTime): TDateTime;

var
  TZ: TTimeZoneInformation;

begin
  Result := Value;

  case GetTimeZoneInformation(TZ) of
    TIME_ZONE_ID_DAYLIGHT:
      Result := Result - (TZ.Bias + TZ.DaylightBias) / MinsPerDay;
    TIME_ZONE_ID_STANDARD:
      Result := Result - (TZ.Bias + TZ.StandardBias) / MinsPerDay;
    TIME_ZONE_ID_UNKNOWN:
      Result := Result - TZ.Bias / MinsPerDay;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function AprTimeToDateTime(AprTime: TAprTime): TDateTime;

begin
  if AprTime = 0 then
    Result := 0
  else
    Result := UTCToTzDateTime(UnixDateDelta + AprTime / SecsPerDay / 1000000);
end;

//----------------------------------------------------------------------------------------------------------------------

function DateTimeToAprTime(Value: TDateTime): TAprTime;

begin
  if Value = 0 then
    Result := 0
  else
    Result := Round(TzToUTCDateTime(Value - UnixDateDelta) * SecsPerDay * 1000000);
end;

//----------------------------------------------------------------------------------------------------------------------

function CompareSvnPathNames(Item1, Item2: Pointer): Integer;

begin
  Result := AnsiCompareText(TSvnItem(Item1).SvnPathName, TSvnItem(Item2).SvnPathName);
end;

//----------------------------------------------------------------------------------------------------------------------

function CompareNativePathNames(P1, P2: Pointer): Integer;

var
  Item1: TSvnItem absolute P1;
  Item2: TSvnItem absolute P2;

begin
  Result := 0;
  if Assigned(P1) and Assigned(P2) then
  begin
    Result := Ord(Item2.IsDirectory) - Ord(Item1.IsDirectory);
    if Result = 0 then
      Result := AnsiCompareText(Item1.PathName, Item2.PathName);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetAppDataDir: string;

var
  Malloc: IMalloc;
  P: PItemIDList;

begin
  Result := '';

  OleCheck(SHGetMalloc(Malloc));

  if Succeeded(SHGetSpecialFolderLocation(0, CSIDL_APPDATA, P)) then
  begin
    SetLength(Result, MAX_PATH);
    if SHGetPathFromIDList(P, PChar(Result)) then
      SetLength(Result, StrLen(PChar(Result)));
    Malloc.Free(P);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnStrToDateTime(const S: string; Pool: PAprPool): TDateTime;

var
  AprTime: TAprTime;
  Error: PSvnError;

begin
  Result := 0;
  AprTime := 0;
  Error := svn_time_from_cstring(AprTime, PAnsiChar(AnsiString(S)), Pool);
  if Assigned(Error) then
    svn_error_clear(Error)
  else
    Result := AprTimeToDateTime(AprTime);
end;

//----------------------------------------------------------------------------------------------------------------------

function BlameReceiver(baton: Pointer; line_no: Int64; revision: TSvnRevNum; author, date, line: PAnsiChar;
  pool: PAprPool): PSvnError; cdecl;

begin
  Result := nil;
  if revision <> SVN_INVALID_REVNUM then
    TSvnClient(baton).DoBlame(line_no, revision, string(author), string(date), string(line));
end;

//----------------------------------------------------------------------------------------------------------------------

function ListReceiver(baton: Pointer; path: PAnsiChar; dirent: PSvnDirEnt; lock: PSvnLock; abs_path: PAnsiChar;
  pool: PAprPool): PSvnError; cdecl;

var
  DirEntry: TSvnDirEnt;
  Locked: Boolean;
  LockData: TSvnLock;

begin
  Result := nil;

  if Assigned(dirent) then
    DirEntry := Dirent^
  else
  begin
    DirEntry.kind := svnNodeUnknown;
    DirEntry.size := 0;
    DirEntry.has_props := False;
    DirEntry.created_rev := 0;
    DirEntry.time := DateTimeToAprTime(0);
    DirEntry.last_author := nil;
  end;
  Locked := Assigned(lock);
  if Locked then
    LockData := lock^
  else
  begin
    LockData.path := nil;
    LockData.token := nil;
    LockData.owner := nil;
    LockData.comment := nil;
    LockData.is_dav_comment := False;
    LockData.creation_date := DateTimeToAprTime(0);
    LockData.expiration_date := DateTimeToAprTime(0);
  end;

  TSvnClient(baton).DoList(string(Path), DirEntry, Locked, LockData, string(abs_path));
end;

//----------------------------------------------------------------------------------------------------------------------

function LogMessage(baton: Pointer; changed_paths: PAprHash; revision: TSvnRevNum; author, date, message: PAnsiChar;
  pool: PAprPool): PSvnError; cdecl;

var
  Item: TSvnHistoryItem;
  Time: TAprTime;

begin
  Result := nil;

  Item := TSvnHistoryItem.Create;
  try
    Item.FOwner := baton;
    Item.FRevision := revision;
    Item.FAuthor := string(author);
    Item.FLogMessage := string(message);
    if Assigned(date) and (date^ <> #0) then
    begin
      SvnCheck(svn_time_from_cstring(Time, date, pool));
      Item.FTime := AprTimeToDateTime(Time);
    end
    else
      Item.FTime := 0;
      
    TSvnItem(baton).FHistory.Add(Item);
    apr_pool_clear(pool);
  except
    Item.Free;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function NotifyActionStr(Action: TSvnWcNotifyAction): string;

const
  NotifyActionStrings: array[TSvnWcNotifyAction] of string = (SWcNotifyAdd, SWcNotifyCopy, SWcNotifyDelete,
    SWcNotifyRestore, SWcNotifyRevert, SWcNotifyFailedRevert, SWcNotifyResolved, SWcNotifySkip, SWcNotifyUpdateDelete,
    SWcNotifyUpdateAdd, SWcNotifyUpdateUpdate, SWcNotifyUpdateCompleted, SWcNotifyUpdateExternal,
    SWcNotifyStatusCompleted, SWcNotifyStatusExternal, SWcNotifyCommitModified, SWcNotifyCommitAdded,
    SWcNotifyCommitDeleted, SWcNotifyCommitReplaced, SWcNotifyCommitPostfixTxdelta, SWcNotifyBlameRevision,
    SWcNotifyLocked, SWcNotifyUnlocked, SWcNotifyFailedLock, SWcNotifyFailedUnlock, SWcNotifyExists,
    SWcNotifyChangelistSet, SWcNotifyChangelistClear, SWcNotifyChangelistMoved, SWcNotifyMergeBegin,
    SWcNotifyForeignMergeBegin, SWcNotifyUpdateReplace);

begin
  Result := NotifyActionStrings[Action];
end;

//----------------------------------------------------------------------------------------------------------------------

function SimplePrompt(out cred: PSvnAuthCredSimple; baton: Pointer; realm, username: PAnsiChar; may_save: TSvnBoolean;
  pool: PAprPool): PSvnError; cdecl;

var
  SRealm, SUserName, SPassword: string;
  Save: Boolean;

begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';
  if Assigned(username) then
    SetString(SUserName, username, StrLen(username))
  else
    SUserName := '';
  SPassword := '';
  Save := may_save;

  if not TSvnClient(baton).DoLoginPrompt(SRealm, SUserName, SPassword, Save) then // not cancelled
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredSimple));
    // leaving username or password nil would cause A/V
    cred^.username := apr_pstrdup(pool, PAnsiChar(AnsiString(SUserName)));
    cred^.password := apr_pstrdup(pool, PAnsiChar(AnsiString(SPassword)));
    cred^.may_save := Save;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function SSLClientCertPrompt(out cred: PSvnAuthCredSSLClientCert; baton: Pointer; realm: PAnsiChar; may_save: TSvnBoolean;
  pool: PAprPool): PSvnError; cdecl;

var
  SRealm, SCertFileName: string;
  Save: Boolean;

begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';
  SCertFileName := '';
  Save := may_save;
  if not TSvnClient(baton).DoSSLClientCertPrompt(SRealm, SCertFileName, Save) then
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredSSLClientCert));
    cred^.cert_file := apr_pstrdup(pool, PAnsiChar(AnsiString(SCertFileName)));
    cred^.may_save := Save;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function SSLClientPasswordPrompt(out cred: PSvnAuthCredSSLClientCertPw; baton: Pointer; realm: PAnsiChar;
  may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;

var
  SRealm, SPassword: string;
  Save: Boolean;

begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';
  SPassword := '';
  Save := may_save;
  if not TSvnClient(baton).DoSSLClientPasswordPrompt(SRealm, SPassword, Save) then
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredSSLClientCertPw));
    cred^.password := apr_pstrdup(pool, PAnsiChar(AnsiString(SPassword)));
    cred^.may_save := Save;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function SSLServerTrustPrompt(out cred: PSvnAuthCredSSLServerTrust; baton: Pointer; realm: PAnsiChar; failures: Cardinal;
  cert_info: PSvnAuthSSLServerCertInfo; may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;

var
  SRealm: string;
  Failed: TSSLServerTrustFailures;
  Save: Boolean;

begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';

  Failed := [];
  if failures and SVN_AUTH_SSL_NOTYETVALID <> 0 then
    Include(Failed, sslCertNotYetValid);
  if failures and SVN_AUTH_SSL_EXPIRED <> 0 then
    Include(Failed, sslCertExpired);
  if failures and SVN_AUTH_SSL_CNMISMATCH <> 0 then
    Include(Failed, sslCertHostNameMismatch);
  if failures and SVN_AUTH_SSL_UNKNOWNCA <> 0 then
    Include(Failed, sslCertAuthorityUnknown);
  if failures and SVN_AUTH_SSL_OTHER <> 0 then
    Include(Failed, sslCertOther);
  Save := may_save;

  if not TSvnClient(baton).DoSSLServerTrustPrompt(SRealm, cert_info^, Failed, Save) then // not cancelled
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredSSLServerTrust));
    cred^.may_save := Save;
    cred^.accepted_failures := failures;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function FileAttrStr(Attr: Cardinal): string;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  Result := '';
  if Attr and faReadOnly <> 0 then
    Result := Result + 'R';
  if Attr and faHidden <> 0 then
    Result := Result + 'H';
  if Attr and faSysFile <> 0 then
    Result := Result + 'S';
  if Attr and faArchive <> 0 then
    Result := Result + 'A';
  {$WARN SYMBOL_PLATFORM ON}
end;

//----------------------------------------------------------------------------------------------------------------------

function StatusKindStr(Status: TSvnWCStatusKind): string;

begin
  case Status of
    svnWcStatusNone:
      Result := SWcStatusNone;
    svnWcStatusUnversioned:
      Result := SWcStatusUnversioned;
    svnWcStatusNormal:
      Result := SWcStatusNormal;
    svnWcStatusAdded:
      Result := SWcStatusAdded;
    svnWcStatusMissing:
      Result := SWcStatusMissing;
    svnWcStatusDeleted:
      Result := SWcStatusDeleted;
    svnWcStatusReplaced:
      Result := SWcStatusReplaced;
    svnWcStatusModified:
      Result := SWcStatusModified;
    svnWcStatusMerged:
      Result := SWcStatusMerged;
    svnWcStatusConflicted:
      Result := SWcStatusConflicted;
    svnWcStatusIgnored:
      Result := SWcStatusIgnored;
    svnWcStatusObstructed:
      Result := SWcStatusObstructed;
    svnWcStatusExternal:
      Result := SWcStatusExternal;
    svnWcStatusIncomplete:
      Result := SWcStatusIncomplete;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnContextCancel(cancel_baton: Pointer): PSvnError; cdecl;

begin
  if TSvnClient(cancel_baton).DoCancel then
    Result := svn_error_create(SVN_ERR_CANCELLED, nil, 'Cancelled by user')
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnContextLogMessage(out log_msg, tmp_file: PAnsiChar; commit_items: PAprArrayHeader; baton: Pointer;
  pool: PAprPool): PSvnError; cdecl;

begin
  Result := nil;
  log_msg := apr_pstrdup(pool, PAnsiChar(AnsiString(TSvnClient(baton).FCommitLogMessage)));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SvnContextNotify(baton: Pointer; path: PAnsiChar; action: TSvnWcNotifyAction; kind: TSvnNodeKind;
  mime_type: PAnsiChar; content_state, prop_state: TSvnWCNotifyState; revision: TSvnRevNum); cdecl;

begin
  TSvnClient(baton).DoNotify(string(path), string(mime_type), action, kind, content_state, prop_state, revision);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SvnContextProgress(progress, total: TAprOff; baton: Pointer; pool: PAprPool); cdecl;

begin
  if total = 0 then
    OutputDebugString(PChar(Format('SvnContextProgress(%d, %d)', [progress, total])))
  else
    OutputDebugString(PChar(Format('SvnContextProgress(%d, %d) %.2f%%', [progress, total, 100 * progress / total])));
  TSvnClient(baton).DoProgress(progress, total);
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnExcludeTrailingPathDelimiter(const S: string): string;

var
  L: Integer;

begin
  Result := S;
  if Result = '' then
    Exit;

  L := Length(Result);
  if IsDelimiter(SvnPathDelim, Result, L) then
    SetLength(Result, L - 1);
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnExtractFileDrive(const SvnFileName: string): string;

var
  I, J: Integer;

begin
  if (Length(SvnFileName) >= 2) and (SvnFileName[2] = DriveDelim) then
    Result := Copy(SvnFileName, 1, 2)
  else if (Length(SvnFileName) >= 2) and (SvnFileName[1] = SvnPathDelim) and
    (SvnFileName[2] = SvnPathDelim) then
  begin
    J := 0;
    I := 3;
    while (I < Length(SvnFileName)) and (J < 2) do
    begin
      if SvnFileName[I] = SvnPathDelim then Inc(J);
      if J < 2 then Inc(I);
    end;
    if SvnFileName[I] = SvnPathDelim then Dec(I);
    Result := Copy(SvnFileName, 1, I);
  end else Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnExtractFileName(const SvnFileName: string): string;

var
  I: Integer;

begin
  I := LastDelimiter(DriveDelim + SvnPathDelim, SvnFileName);
  Result := Copy(SvnFileName, I + 1, MaxInt);
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnExtractFilePath(const SvnFileName: string): string;

var
  I: Integer;

begin
  I := LastDelimiter(SvnPathDelim + DriveDelim, SvnFileName);
  Result := Copy(SvnFileName, 1, I);
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnIncludeTrailingPathDelimiter(const S: string): string;

var
  L: Integer;

begin
  Result := S;
  if Result = '' then
    Exit;

  L := Length(Result);
  if not IsDelimiter(SvnPathDelim, Result, L) then
    Result := Result + SvnPathDelim;
end;

//----------------------------------------------------------------------------------------------------------------------

function UserNamePrompt(out cred: PSvnAuthCredUsername; baton: Pointer; realm: PAnsiChar; may_save: TSvnBoolean;
  pool: PAprPool): PSvnError; cdecl;

var
  SRealm, SUserName: string;
  Save: Boolean;

begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';
  SUserName := '';
  Save := may_save;

  if not TSvnClient(baton).DoUserNamePrompt(SRealm, SUserName, Save) then // not cancelled
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredUserName));
    cred^.username := apr_pstrdup(pool, PAnsiChar(AnsiString(SUserName)));
    cred^.may_save := Save;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure WCStatus(baton: Pointer; path: PAnsiChar; status: PSvnWCStatus2); cdecl;

begin
  if Assigned(status) then
    TSvnItem(baton).DoWCStatus(path, status^);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure WCStatus2(baton: Pointer; path: PAnsiChar; status: PSvnWCStatus2); cdecl;

begin
  if Assigned(status) then
    TSvnClient(baton).DoWCStatus(path, status^);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure WCStatus3(baton: Pointer; path: PAnsiChar; status: PSvnWCStatus2); cdecl;

begin
  if Assigned(status) then
    TSvnItem(baton).LoadStatus(status^);
end;

//----------------------------------------------------------------------------------------------------------------------

function DummyInfoReceiver(baton: Pointer; path: PAnsiChar; const info: TSvnInfo; pool: PAprPool): PSvnError; cdecl;

begin
  Result := nil;
end;

type
  TBlameThread = class(TThread)
  private
    FItem: TSvnHistoryItem;
  protected
    procedure Execute; override;
  public
    constructor Create(AItem: TSvnHistoryItem; AOnTerminate: TNotifyEvent);
  end;

//----------------------------------------------------------------------------------------------------------------------

{ TBlameThread protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TBlameThread.Execute;

var
  SvnClient: TSvnClient;

begin
  SvnClient := TSvnClient.Create;
  try
    SvnClient.Initialize('', FItem.Owner.SvnClient.Ctx^.auth_baton);
    SvnClient.OnLoginPrompt := FItem.Owner.SvnClient.OnLoginPrompt;
    SvnClient.OnUserNamePrompt := FItem.Owner.SvnClient.OnUserNamePrompt;
    SvnClient.OnSSLServerTrustPrompt := FItem.Owner.SvnClient.OnSSLServerTrustPrompt;
    SvnClient.OnSSLClientPasswordPrompt := FItem.Owner.SvnClient.OnSSLClientPasswordPrompt;
    SvnClient.OnSSLClientPasswordPrompt := FItem.Owner.SvnClient.OnSSLClientPasswordPrompt;
    FItem.ReloadBlame(SvnClient, FItem.Owner.PathName, FItem.Owner.BaseRevision);
  finally
    SvnClient.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TBlameThread public }

//----------------------------------------------------------------------------------------------------------------------

constructor TBlameThread.Create(AItem: TSvnHistoryItem; AOnTerminate: TNotifyEvent);

begin
  FItem := AItem;
  OnTerminate := AOnTerminate;
  FreeOnTerminate := True;
  inherited Create(False);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnHistoryItem private }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnHistoryItem.BlameCallback(Sender: TObject; LineNo: Int64; Revision: TSvnRevNum; const Author: string;
  Time: TDateTime; const Line: string; var Cancel: Boolean);

var
  Item: TSvnBlameItem;

begin
  Cancel := FCancelBlame or (Assigned(FBlameThread) and TBlameThread(FBlameThread).Terminated);
  if Cancel then
    Exit;
    
  Item := TSvnBlameItem.Create;
  try
    Item.FLineNo := LineNo;
    Item.FRevision := Revision;
    Item.FAuthor := Author;
    Item.FTime := Time;
    Item.FLine := Line;
    
    FBlame.Add(Item);
  except
    Item.Free;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnHistoryItem.BlameThreadTerminate(Sender: TObject);

begin
  try
    if Assigned(TThread(Sender).FatalException) then
    begin
      ClearBlame;
      if TThread(Sender).FatalException is Exception then
        FBlameError := Exception(TThread(Sender).FatalException).Message;
    end;

    if Assigned(FBlameNotify) and not FCancelBlame and not FDestroying then
      FBlameNotify(Self);
  finally
    FBlameThread := nil;
    FBlameNotify := nil;
    FCancelBlame := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnHistoryItem.ClearBlame;

var
  I: Integer;

begin
  if Assigned(FBlame) then
  begin
    for I := 0 to FBlame.Count - 1 do
      TObject(FBlame[I]).Free;
    FreeAndNil(FBlame);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnHistoryItem.ReloadBlame;

begin
  ReloadBlame(FOwner.SvnClient, FOwner.PathName, FOwner.BaseRevision);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnHistoryItem.ReloadBlame(ASvnClient: TSvnClient; const APathName: string; ABaseRevision: Integer);

begin
  ClearBlame;
  FBlameError := '';

  FBlame := TList.Create;
  try
    ASvnClient.Blame(APathName, BlameCallback, 1, FRevision, ABaseRevision);
  except
    ClearBlame;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnHistoryItem public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnHistoryItem.Create;

begin
  inherited Create;
  FBlame := nil;
  FBlameThread := nil;
  FBlameNotify := nil;
  FCancelBlame := False;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnHistoryItem.Destroy;

begin
  FDestroying := True;
  CancelBlame;
  FBlameThread := nil;
  ClearBlame;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnHistoryItem.GetBlameCount: Integer;

begin
  Result := 0;
  if IsLoadingBlame then
    Exit;
    
  if not Assigned(FBlame) then
    ReloadBlame;
  Result := FBlame.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnHistoryItem.GetBlameItems(Index: Integer): TSvnBlameItem;

begin
  Result := nil;
  if IsLoadingBlame then
    Exit;
    
  if not Assigned(FBlame) then
    ReloadBlame;
  Result := FBlame[Index];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnHistoryItem.CancelBlame;

begin
  FCancelBlame := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnHistoryItem.GetFile: string;

var
  SubPool: PAprPool;
  PegRevision, Revision: TSvnOptRevision;
  Buffer: PSvnStringBuf;
  Stream: PSvnStream;

begin
  Result := '';

  if FFile = '' then
  begin
    AprCheck(apr_pool_create_ex(SubPool, FOwner.SvnClient.Pool, nil, FOwner.SvnClient.Allocator));
    try
      FillChar(PegRevision, SizeOf(TSvnOptRevision), 0);
      PegRevision.Kind := svnOptRevisionUnspecified;;
      FillChar(Revision, SizeOf(TSvnOptRevision), 0);
      Revision.Kind := svnOptRevisionNumber;
      Revision.Value.number := FRevision;
      Buffer := svn_stringbuf_create('', SubPool);
      Stream := svn_stream_from_stringbuf(Buffer, SubPool);
      FOwner.FSvnClient.FCancelled := False;
      SvnCheck(svn_client_cat2(Stream, PAnsiChar(AnsiString(FOwner.SvnPathName)), @PegRevision, @Revision, FOwner.SvnClient.Ctx,
        SubPool));
      SetString(FFile, Buffer.data, Buffer.len);
    finally
      apr_pool_destroy(SubPool);
    end;
  end;

  Result := FFile;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnHistoryItem.HasBlameLoaded: Boolean;

begin
  Result := Assigned(FBlame) and not IsLoadingBlame;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnHistoryItem.IsLoadingBlame: Boolean;

begin
  Result := Assigned(FBlameThread);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnHistoryItem.StartLoadingBlame(ANotify: TNotifyEvent = nil);

begin
  if IsLoadingBlame then
    Exit;
  FCancelBlame := False;
  FBlameNotify := ANotify;
  FBlameError := '';
  FBlameThread := TBlameThread.Create(Self, BlameThreadTerminate);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnItem private }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.ClearHistory;

var
  I: Integer;

begin
  if Assigned(FHistory) then
  begin
    for I := 0 to FHistory.Count - 1 do
      TObject(FHistory[I]).Free;
    FreeAndNil(FHistory);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.ClearItems;

var
  I: Integer;

begin
  if Assigned(FItems) then
  begin
    for I := FItems.Count - 1 downto 0 do
      TObject(FItems[I]).Free;
    FreeAndNil(FItems);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetCount: Integer;

begin
  if not Assigned(FItems) then
    Reload;
  Result := FItems.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetExternal: Boolean;

var
  Externals: TStringList;
  I: Integer;
  S: string;

begin
  Result := False;

  if Assigned(Parent) then
  begin
    Externals := TStringList.Create;
    try
      Externals.Delimiter := Parent.PropValDelimiter;
      {$IFDEF COMPILER_10_UP}
      Externals.StrictDelimiter := True;
      {$ENDIF}
      Externals.DelimitedText := Parent.PropValues['svn:externals'];
      S := ExtractFileName(FPathName) + ' ';
      for I := 0 to Externals.Count - 1 do
        if StrLIComp(PChar(Externals[I]), PChar(S), Length(S)) = 0 then
        begin
          Result := True;
          Break;
        end;
    finally
      Externals.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetFileAttr: Cardinal;

begin
  if FFileAttr = 0 then
    FFileAttr := GetFileAttributes(PChar(FPathName));
  Result := FFileAttr;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetHistoryCount: Integer;

begin
  if not Assigned(FHistory) then
    ReloadHistory;
  Result := FHistory.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetHistoryItems(Index: Integer): TSvnHistoryItem;

begin
  if not Assigned(FHistory) then
    ReloadHistory;
  Result := FHistory[Index];
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetIsDirectory: Boolean;

begin
  case FKind of
    svnNodeNone:
      Result := GetFileAttr and FILE_ATTRIBUTE_DIRECTORY <> 0;
    svnNodeDir:
      Result := True;
    else
      Result := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetItems(Index: Integer): TSvnItem;

begin
  if not Assigned(FItems) then
    Reload;
  Result := FItems[Index];
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetPropCount: Integer;

begin
  if not Assigned(FProps) then
    ReloadProps;
  Result := FProps.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetPropNames(Index: Integer): string;

begin
  if not Assigned(FProps) then
    ReloadProps;
  Result := FProps.Names[Index];
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetPropValueFromIndex(Index: Integer): string;

begin
  if not Assigned(FProps) then
    ReloadProps;
  Result := FProps.ValueFromIndex[Index];
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetPropValues(const Name: string): string;

begin
  if not Assigned(FProps) then
    ReloadProps;
  Result := FProps.Values[Name];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.LoadStatus(const Status: TSvnWcStatus2);

begin
  if Assigned(Status.entry) then
  begin
    FBaseRevision := Status.entry^.revision;
    FURL := string(Status.entry^.url);
    FRepository := string(Status.entry^.repos);
    FUUID := string(Status.entry^.uuid);
    FKind := Status.entry^.kind;
    FSchedule := Status.entry^.schedule;
    FCopied := Status.entry^.copied;
    FDeleted := Status.entry^.deleted;
    FAbsent := Status.entry^.absent;
    FIncomplete := Status.entry^.incomplete;
    FCopiedFromURL := string(Status.entry^.copyfrom_url);
    FCopiedFromRevision := Status.entry^.copyfrom_rev;
    FConflictOldFile := string(Status.entry^.conflict_old);
    FConflictNewFile := string(Status.entry^.conflict_new);
    FConflictWorkingFile := string(Status.entry^.conflict_wrk);
    FPropRejectFile := string(Status.entry^.prejfile);
    FTextTime := AprTimeToDateTime(Status.entry^.text_time);
    FPropTime := AprTimeToDateTime(Status.entry^.prop_time);
    FCheckSum := string(Status.entry^.checksum);
    FCommittedRevision := Status.entry^.cmt_rev;
    FCommitAuthor := string(Status.entry^.cmt_author);
    FCommitTime := AprTimeToDateTime(Status.entry^.cmt_date);
    FLockToken := string(Status.entry^.lock_token);
    FLockOwner := string(Status.entry^.lock_owner);
    FLockComment := string(Status.entry^.lock_comment);
    FLockTime := AprTimeToDateTime(Status.entry^.lock_creation_date);
  end
  else
  begin
    FBaseRevision := -1;
    FURL := '';
    FRepository := '';
    FUUID := '';
    FKind := svnNodeNone;
    FSchedule := svnWcScheduleNormal;
    FCopied := False;
    FDeleted := False;
    FAbsent := False;
    FIncomplete := False;
    FCopiedFromURL := '';
    FCopiedFromRevision := -1;
    FConflictOldFile := '';
    FConflictNewFile := '';
    FConflictWorkingFile := '';
    FPropRejectFile := '';
    FTextTime := 0;
    FPropTime := 0;
    FCheckSum := '';
    FCommittedRevision := -1;
    FCommitAuthor := '';
    FCommitTime := 0;
    FLockToken := '';
    FLockOwner := '';
    FLockComment := '';
    FLockTime := 0;
  end;
  FTextStatus := Status.text_status;
  FPropStatus := Status.prop_status;
  FLocked := Status.locked;
  FCopied := Status.copied;
  FSwitched := Status.switched;
  FRemoteTextStatus := Status.repos_text_status;
  FRemotePropStatus := Status.repos_prop_status;
  if Assigned(Status.repos_lock) then
  begin
    FLockPath := string(Status.repos_lock^.path);
    FLockToken := string(Status.repos_lock^.token);
    FLockOwner := string(Status.repos_lock^.owner);
    FLockComment := string(Status.repos_lock^.comment);
    FLockDAVComment := Status.repos_lock^.is_dav_comment;
    FLockTime := AprTimeToDateTime(Status.repos_lock^.creation_date);
    FLockExpirationTime := AprTimeToDateTime(Status.repos_lock^.expiration_date);
  end
  else
  begin
    FLockPath := '';
    FLockToken := '';
    FLockOwner := '';
    FLockComment := '';
    FLockDAVComment := False;
    FLockTime := 0;
    FLockExpirationTime := 0;
  end;
  FURL := string(Status.url);

  FLastCommitRevision := Status.ood_last_cmt_rev;
  FLastCommitAuthor := string(Status.ood_last_cmt_author);
  FLastCommitTime := AprTimeToDateTime(Status.ood_last_cmt_date);

  if IsDirectory and (FTextStatus = svnWcStatusUnversioned) and FSvnClient.RecurseUnversioned then
    LoadUnversionedPaths;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.LoadUnversionedPaths;

var
  R: Integer;
  F: TSearchRec;
  Status: TSvnWcStatus2;
  Item: TSvnItem;
  Error: PSvnError;

begin
  R := FindFirst(IncludeTrailingPathDelimiter(FPathName) + '*.*', faAnyFile, F);
  if R <> 0 then
    Exit;

  try
    while R = 0 do
    begin
      if (F.Name <> '.') and (F.Name <> '..') and not FSvnClient.MatchGlobalIgnores(F.Name) then
      begin
        if (F.Attr and faDirectory <> 0) and
          FSvnClient.IsPathVersioned(IncludeTrailingPathDelimiter(FPathName) + F.Name) then
        begin
          OutputDebugString(PChar(Format('versioned subdirectory ''%s'' in unversioned directory ''%s''',
            [F.Name, FPathName])));
        end
        else
        begin
          FillChar(Status, SizeOf(TSvnWcStatus2), 0);
          Status.entry := nil;
          Status.text_status := svnWcStatusUnversioned;
          Item := TSvnItem.Create(FSvnClient, Self,
            FSvnClient.NativePathToSvnPath(IncludeTrailingPathDelimiter(FPathName) + F.Name), Status);

          if Assigned(FSvnClient.FStatusCallback) then
            FSvnClient.FStatusCallback(FSvnClient, Item, FSvnClient.FCancelled);
          Error := SvnContextCancel(FSvnClient);
          if Assigned(Error) then
            RaiseSvnError(Error);
        end;
      end;

      R := FindNext(F);
    end;
  finally
    FindClose(F);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.ReloadHistory;

var
  SubPool: PAprPool;
  StartRevision, EndRevision: TSvnOptRevision;
  Targets: PAprArrayHeader;
  Error: PSvnError;

begin
  ClearHistory;

  FHistory := TList.Create;
  try
    if FTextStatus in [svnWcStatusNone, svnWcStatusUnversioned] then
      Exit;

    AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
    try
      FillChar(StartRevision, SizeOf(TSvnOptRevision), 0);
      StartRevision.Kind := svnOptRevisionHead;
      FillChar(EndRevision, SizeOf(TSvnOptRevision), 0);
      EndRevision.Kind := svnOptRevisionNumber;
      EndRevision.Value.number := 0;

      Targets := FSvnClient.PathNamesToAprArray([FSvnPathName], SubPool);
      FSvnClient.FCancelled := False;
      Error := svn_client_log2(Targets, @StartRevision, @EndRevision, 0, False, False, LogMessage, Self, FSvnClient.Ctx,
        SubPool);
      if Assigned(Error) then
      begin
        if Error^.apr_err = APR_OS_START_SYSERR + WSAHOST_NOT_FOUND then
          svn_error_clear(Error)
        else
          RaiseSvnError(Error);
      end;
    finally
      apr_pool_destroy(SubPool);
    end;
  except
    ClearHistory;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.ReloadProps;

var
  SubPool: PAprPool;
  PegRevision, Revision: TSvnOptRevision;
  TruePath: PAnsiChar;
  Props: PAprArrayHeader;

begin
  FreeAndNil(FProps);

  FProps := TStringList.Create;
  try
    AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
    try
      FillChar(PegRevision, SizeOf(TSvnOptRevision), 0);
      PegRevision.Kind := svnOptRevisionUnspecified;
      FillChar(Revision, SizeOf(TSvnOptRevision), 0);
      Revision.Kind := svnOptRevisionUnspecified;
      AprCheck(apr_filepath_merge(TruePath, '', PAnsiChar(AnsiString(FPathName)), APR_FILEPATH_TRUENAME, SubPool));
      FSvnClient.FCancelled := False;
      SvnCheck(svn_client_proplist2(Props, TruePath, @PegRevision, @Revision, False, FSvnClient.Ctx,
        SubPool));

      FSvnClient.GetProps(Props, FProps, SubPool, FPropValDelimiter);
    finally
      apr_pool_destroy(SubPool);
    end;
  except
    FreeAndNil(FProps);
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.SetPropValues(const Name, Value: string);

var
  SubPool: PAprPool;
  TruePath: PAnsiChar;
  SValue: string;
  SvnValue: PSvnString;

begin
  AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
  try
    AprCheck(apr_filepath_merge(TruePath, '', PAnsiChar(AnsiString(FPathName)), APR_FILEPATH_TRUENAME, SubPool));
    SValue := Value;
    if Pos(';', SValue) <> 0 then
    begin
      if SValue[Length(SValue)] <> ';' then
        SValue := SValue + ';';
      SValue := StringReplace(SValue, ';', SvnLineBreak, [rfReplaceAll, rfIgnoreCase]);
    end;

    SvnValue := svn_string_create(PAnsiChar(AnsiString(Value)), SubPool);
    if svn_prop_needs_translation(PAnsiChar(AnsiString(Name))) then
      SvnCheck(svn_subst_translate_string(SvnValue, SvnValue, nil, SubPool));

    FSvnClient.FCancelled := False;
    SvnCheck(svn_client_propset2(PAnsiChar(AnsiString(Name)), SvnValue, TruePath, False, False, FSvnClient.Ctx, SubPool));
  finally
    apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.SortItems(Recurse: Boolean);

var
  I: Integer;

begin
  if not Assigned(FItems) then
    Exit;

  for I := 0 to FItems.Count - 1 do
    if TSvnItem(FItems[I]).Kind = svnNodeDir then
      TSvnItem(FItems[I]).SortItems(Recurse);
  FItems.Sort(CompareNativePathNames);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnItem protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.DoDestroyNotifications;

var
  I: Integer;

begin
  if Assigned(FDestroyNotifications) then
    for I := 0 to FDestroyNotifications.Count - 1 do
      TNotifyEvent(PMethod(FDestroyNotifications[I])^)(Self);
end;

//----------------------------------------------------------------------------------------------------------------------
// svn recursive status lists items in the following order:
// 1.  unversioned items within current directory, if any
// 2.  current directory itself
// 3.  versioned items within current directory, however:
//     if the item is a directory, descend and continue with step 1 (unversioned items first).
//
// for example:
//   V:/unversioned1.txt
//   V:/unversioned2.txt
//   V:
//   V:/Subdir/unversioned1.txt
//   V:/Subdir/unversioned2.txt
//   V:/Subdir
//   V:/Subdir/versioned1.txt
//   V:/Subdir/versioned2.txt
//   V:/versioned1.txt
//   V:/versioned2.txt

procedure TSvnItem.DoWCStatus(Path: PAnsiChar; const Status: TSvnWcStatus2);

var
  Parent, Child: TSvnItem;
  ParentPath, ChildsParentPath: string;
  I: Integer;

  procedure AddExternals(Parent: TSvnItem);
  var
    I: Integer;
    Child: TSvnItem;
  begin
    for I := FReloadExternals.Count - 1 downto 0 do
    begin
      Child := FReloadExternals[I];
      FReloadExternals.Delete(I);
      Child.FParent := Parent;
      Parent.Add(Child);
    end;
  end;

  procedure AddUnversionedItems(Parent: TSvnItem);
  var
    I: Integer;
    Child: TSvnItem;
  begin
    for I := FReloadUnversioned.Count - 1 downto 0 do
    begin
      Child := FReloadUnversioned[I];
      FReloadUnversioned.Delete(I);
      Child.FParent := Parent;
      Parent.Add(Child);
    end;
  end;

begin
  case Status.text_status of
    svnWcStatusUnversioned:
      FReloadUnversioned.Add(TSvnItem.Create(FSvnClient, nil, string(Path), Status));
    svnWcStatusExternal:
      begin
        Child := TSvnItem.Create(FSvnClient, nil, string(Path), Status);
        FReloadExternals.Add(Child);
        if FReloadRecursive then
          FReloadGlobalExternals.Add(Child);
      end;
    else
    begin
      if FReloadRecursive then
      begin
        if FReloadStack.Count > 0 then
          Parent := FReloadStack.Peek
        else
          Parent := nil;
      end
      else
        Parent := Self;

      if Assigned(Parent) then
      begin
        if StrIComp(Path, PAnsiChar(AnsiString(Parent.SvnPathName))) = 0 then
        begin
          LoadStatus(Status);
          if FKind = svnNodeDir then
          begin
            AddUnversionedItems(Self);
            AddExternals(Self);
          end;
          Exit;
        end
        else if FReloadRecursive then
        begin
          ChildsParentPath := SvnExtractFilePath(string(Path));
          ParentPath := SvnIncludeTrailingPathDelimiter(Parent.SvnPathName);
          while not AnsiSameText(ChildsParentPath, ParentPath) do
          begin
            FReloadStack.Pop;
            if FReloadStack.Count > 0 then
            begin
              Parent := FReloadStack.Peek;
              ParentPath := SvnIncludeTrailingPathDelimiter(Parent.SvnPathName);
            end
            else
            begin
              Parent := nil;
              Break;
            end;
          end;
        end;
      end;

      if Assigned(Parent) then
      begin
        Child := TSvnItem.Create(FSvnClient, Parent, string(Path), Status);
        if Child.Kind = svnNodeDir then
        begin
          AddUnversionedItems(Child);
          AddExternals(Child);
          FReloadStack.Push(Child);
        end;
      end
      else if FReloadRecursive then // not found in current stack, try externals
      begin
        for I := 0 to FReloadGlobalExternals.Count - 1 do
          if AnsiSameText(string(Path), TSvnItem(FReloadGlobalExternals[I]).SvnPathName) then
          begin
            Parent := FReloadGlobalExternals[I];
            FReloadGlobalExternals.Delete(I);
            FReloadStack.Push(Parent);
            Break;
          end;
        if Assigned(Parent) then
          Parent.LoadStatus(Status);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnItem public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnItem.Create(ASvnClient: TSvnClient; AParent: TSvnItem; const APathName: string;
  Recurse: Boolean = False; Update: Boolean = False);

begin
  inherited Create;
  FSvnClient := ASvnClient;
  FParent := AParent;
  if Assigned(FParent) then
    FParent.Add(Self);
  FItems := nil;
  FHistory := nil;
  FProps := nil;
  FPathName := APathName;
  FFileAttr := 0;
  FSmallImageIndex := -1;
  FLargeImageIndex := -1;
  FPropValDelimiter := ';';
  Reload(Recurse, Update);
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnItem.Create(ASvnClient: TSvnClient; AParent: TSvnItem; const ASvnPathName: string;
  const Status: TSvnWcStatus2);

begin
  inherited Create;
  FSvnClient := ASvnClient;
  FSvnPathName := ASvnPathName;
  FParent := AParent;
  FItems := nil;
  FHistory := nil;
  FProps := nil;
  FFileAttr := 0;
  FPathName := FSvnClient.SvnPathToNativePath(FSvnPathName);
  FSmallImageIndex := -1;
  FLargeImageIndex := -1;
  FPropValDelimiter := ';';
  LoadStatus(Status);
  if Assigned(FParent) then
    FParent.Add(Self);
  FDestroyNotifications := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnItem.Destroy;

var
  I: Integer;

begin
  DoDestroyNotifications;
  if Assigned(FDestroyNotifications) then
    for I := 0 to FDestroyNotifications.Count - 1 do
      Dispose(FDestroyNotifications[I]);
  FDestroyNotifications.Free;
  if Assigned(FParent) then
    FParent.Remove(Self);
  Clear;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.Add(Item: TSvnItem): Integer;

begin
  if not Assigned(FItems) then
    FItems := TList.Create;
  Result := FItems.Add(Item);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.AddDestroyNotification(Notification: TNotifyEvent);

var
  P: PMethod;

begin
  if not Assigned(FDestroyNotifications) then
    FDestroyNotifications := TList.Create;
    
  New(P);
  try
    P^ := TMethod(Notification);
    FDestroyNotifications.Add(P);
  except
    Dispose(P);
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.Clear;

begin
  FBaseRevision := -1;
  FURL := '';
  FRepository := '';
  FUUID := '';
  FKind := svnNodeNone;
  FSchedule := svnWcScheduleNormal;
  FCopied := False;
  FDeleted := False;
  FAbsent := False;
  FIncomplete := False;
  FCopiedFromURL := '';
  FCopiedFromRevision := -1;
  FConflictOldFile := '';
  FConflictNewFile := '';
  FConflictWorkingFile := '';
  FPropRejectFile := '';
  FTextTime := 0;
  FPropTime := 0;
  FCheckSum := '';
  FCommittedRevision := -1;
  FCommitAuthor := '';
  FCommitTime := 0;
  FLockToken := '';
  FLockOwner := '';
  FLockComment := '';
  FLockTime := 0;
  FTextStatus := svnWcStatusNone;
  FPropStatus := svnWcStatusNone;
  FLocked := False;
  FCopied := False;
  FSwitched := False;
  FRemoteTextStatus := svnWcStatusNone;
  FRemotePropStatus := svnWcStatusNone;
  FLockPath := '';
  FLockToken := '';
  FLockOwner := '';
  FLockComment := '';
  FLockDAVComment := False;
  FLockTime := 0;
  FLockExpirationTime := 0;
  FURL := '';
  FLastCommitRevision := -1;
  FLastCommitAuthor := '';
  FLastCommitTime := 0;

  ClearItems;
  ClearHistory;
  FreeAndNil(FProps);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetBaseFile: string;

var
  SubPool: PAprPool;
  PegRevision, Revision: TSvnOptRevision;
  Buffer: PSvnStringBuf;
  Stream: PSvnStream;

begin
  Result := '';
  if FKind <> svnNodeFile then
    Exit;

  AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
  try
    FillChar(PegRevision, SizeOf(TSvnOptRevision), 0);
    PegRevision.Kind := svnOptRevisionUnspecified;;
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionBase;
    Buffer := svn_stringbuf_create('', SubPool);
    Stream := svn_stream_from_stringbuf(Buffer, SubPool);
    FSvnClient.FCancelled := False;
    SvnCheck(svn_client_cat2(Stream, PAnsiChar(AnsiString(FSvnPathName)), @PegRevision, @Revision, FSvnClient.Ctx, SubPool));
    SetString(Result, Buffer.data, Buffer.len);
  finally
    apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.GetCommittedFile: string;

var
  SubPool: PAprPool;
  PegRevision, Revision: TSvnOptRevision;
  Buffer: PSvnStringBuf;
  Stream: PSvnStream;

begin
  Result := '';
  if FKind <> svnNodeFile then
    Exit;

  AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
  try
    FillChar(PegRevision, SizeOf(TSvnOptRevision), 0);
    PegRevision.Kind := svnOptRevisionUnspecified;;
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionCommitted;
    Buffer := svn_stringbuf_create('', SubPool);
    Stream := svn_stream_from_stringbuf(Buffer, SubPool);
    FSvnClient.FCancelled := False;
    SvnCheck(svn_client_cat2(Stream, PAnsiChar(AnsiString(FSvnPathName)), @PegRevision, @Revision, FSvnClient.Ctx, SubPool));
    SetString(Result, Buffer.data, Buffer.len);
  finally
    apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.IndexOf(Item: TSvnItem): Integer;

begin
  if Assigned(FItems) then
    Result := FItems.IndexOf(Item)
  else
    Result := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnItem.IndexOf(const PathName: string; SvnPath: Boolean = False): Integer;

var
  I: Integer;
  SPathName1, SPathName2: string;

begin
  Result := -1;

  if not Assigned(FItems) then
    Exit;

  for I := 0 to FItems.Count - 1 do
  begin
    if SvnPath then
    begin
      SPathName1 := SvnExcludeTrailingPathDelimiter(PathName);
      SPathName2 := SvnExcludeTrailingPathDelimiter(TSvnItem(FItems[I]).SvnPathName);
    end
    else
    begin
      SPathName1 := ExcludeTrailingPathDelimiter(PathName);
      SPathName2 := ExcludeTrailingPathDelimiter(TSvnItem(FItems[I]).PathName);
    end;
    
    if AnsiSameText(SPathName1, SPathName2) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.Reload(Recurse: Boolean = False; Update: Boolean = False);

var
  SubPool: PAprPool;
  Revision: TSvnOptRevision;
  I: Integer;

begin
  Clear;
  FItems := TList.Create;

  FReloadRecursive := Recurse;
  FReloadExternals := nil;
  FReloadGlobalExternals := nil;
  FReloadUnversioned := nil;
  FReloadStack := TStack.Create;
  try
    FReloadExternals := TList.Create;
    if FReloadRecursive then
      FReloadGlobalExternals := TList.Create;
    FReloadUnversioned := TList.Create;
    AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
    try
      FSvnPathName := FSvnClient.NativePathToSvnPath(FPathName, SubPool);
      FillChar(Revision, SizeOf(TSvnOptRevision), 0);
      Revision.Kind := svnOptRevisionHead;
      FSvnClient.FCancelled := False;
      if FReloadRecursive then
        FReloadStack.Push(Self);
      SvnCheck(svn_client_status2(nil, PAnsiChar(AnsiString(FSvnPathName)), @Revision, WCStatus, Self, Recurse, True, Update, False,
        False, FSvnClient.Ctx, SubPool));
    finally
      apr_pool_destroy(SubPool);
    end;

    if FKind = svnNodeDir then
      SortItems(Recurse);
  finally
    FreeAndNil(FReloadStack);
    for I := 0 to FReloadUnversioned.Count - 1 do
      TSvnItem(FReloadUnversioned[I]).Free;
    FreeAndNil(FReloadUnversioned);
    for I := 0 to FReloadExternals.Count - 1 do
      TSvnItem(FReloadExternals[I]).Free;
    FreeAndNil(FReloadExternals);
    if FReloadRecursive then
      for I := 0 to FReloadGlobalExternals.Count - 1 do
        TSvnItem(FReloadGlobalExternals[I]).Free;
    FreeAndNil(FReloadGlobalExternals);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.ReloadStatus;

var
  SubPool: PAprPool;
  Revision: TSvnOptRevision;

begin
  AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
  try
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionHead;
    FSvnClient.FCancelled := False;
    SvnCheck(svn_client_status2(nil, PAnsiChar(AnsiString(FSvnPathName)), @Revision, WCStatus3, Self, False, True, True, False, False,
      FSvnClient.Ctx, SubPool));
  finally
    apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.Remove(Item: TSvnItem);

begin
  if Assigned(FItems) then
    FItems.Remove(Item);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.RemoveDestroyNotification(Notification: TNotifyEvent);

var
  P: PMethod;
  I, Index: Integer;

begin
  if Assigned(FDestroyNotifications) then
  begin
    P := nil;
    Index := -1;
    for I := 0 to FDestroyNotifications.Count - 1 do
     if (PMethod(FDestroyNotifications[I])^.Data = TMethod(Notification).Data) and
       (PMethod(FDestroyNotifications[I])^.Code = TMethod(Notification).Code) then
     begin
       P := FDestroyNotifications[I];
       Index := I;
       Break;
     end;

     if Index <> -1 then
     begin
       FDestroyNotifications.Delete(Index);
       Dispose(P);
     end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnItem.Resolved(Recurse: Boolean = False);

begin
  FSvnClient.Resolved(FSvnPathName, Recurse);
  ReloadStatus;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnList.Create(ASvnClient: TSvnClient);

begin
  inherited Create;
  FItems := TObjectList.Create;
  FSvnClient := ASvnClient;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnList.Destroy;

begin
  FItems.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnList.Clear;

begin
  FItems.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnList.GetCount: Integer;

begin
  Result := FItems.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnList.GetItems(Index: Integer): TSvnListItem;

begin
  Result := TSvnListItem(FItems[Index]);
end;

procedure TSvnList.ListCallback(Sender: TObject; Path: string; DirEntry: TSvnDirEnt; Locked: Boolean; LockData: TSvnLock;
  AbsPath: string; var Cancel: Boolean);

var
  SvnListItem: TSvnListItem;

begin
  Cancel := False;
  FItems.Add(TSvnListItem.Create);
  SvnListItem := TSvnListItem(FItems.Last);
  SvnListItem.AbsolutePath := AbsPath;
  SvnListItem.Path := Path;
  SvnListItem.Kind := DirEntry.kind;
  SvnListItem.Size := DirEntry.size;
  SvnListItem.HasProps := DirEntry.has_props;
  SvnListItem.CreatedRevision := DirEntry.created_rev;
  SvnListItem.Time := AprTimeToDateTime(DirEntry.time);
  SvnListItem.LastAuthor := string(DirEntry.last_author);
  SvnListItem.Locked := Locked;
  SvnListItem.LockPath := string(LockData.path);
  SvnListItem.LockToken := string(LockData.token);
  SvnListItem.LockOwner := string(LockData.owner);
  SvnListItem.LockComment := string(LockData.comment);
  SvnListItem.LockDAVComment := LockData.is_dav_comment;
  SvnListItem.LockTime := AprTimeToDateTime(LockData.creation_date);
  SvnListItem.LockExpirationTime := AprTimeToDateTime(LockData.expiration_date);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnList.LoadList(const APathName: string; ADepth: TSvnDepth; AFetchLocks: Boolean; ADirEntryFields: DWORD = SVN_DIRENT_ALL);

begin
  Clear;
  FPathName := APathName;
  FDepth := ADepth;
  FFetchLocks := AFetchLocks;
  FDirEntryFields := ADirEntryFields;
  FSvnClient.List(FPathName, FDepth, FFetchLocks, FDirEntryFields, ListCallback);
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TSvnClientManager = class(TObject)
  private
    FAprLoadCounter: Integer;
    FLock: TCriticalSection;
    FSvnLoadCounter: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FreeAprLib;
    procedure FreeSvnLibs;
    function LoadAprLib: Boolean;
    function LoadSvnLibs: Boolean;
  end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnClientManager }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnClientManager.Create;

begin
  inherited Create;
  FAprLoadCounter := 0;
  FLock := TCriticalSection.Create;
  FSvnLoadCounter := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnClientManager.Destroy;

begin
  FLock.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClientManager.FreeAprLib;

begin
  FLock.Enter;
  try
    if FAprLoadCounter > 0 then
    begin
      Dec(FAprLoadCounter);
      if FAprLoadCounter = 0 then
        apr.FreeAprLib;
    end;
  finally
    FLock.Leave;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClientManager.FreeSvnLibs;

begin
  FLock.Enter;
  try
    if FSvnLoadCounter > 0 then
    begin
      Dec(FSvnLoadCounter);
      if FSvnLoadCounter = 0 then
      begin
        FreeSvnClientLib;
        FreeSvnDeltaLib;
        FreeSvnDiffLib;
        FreeSvnFsLib;
        FreeSvnRaLib;
        FreeSvnReposLib;
        FreeSvnSubrLib;
        FreeSvnWcLib;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClientManager.LoadAprLib: Boolean;

begin
  FLock.Enter;
  try
    if FAprLoadCounter = 0 then
      Result := apr.LoadAprLib
    else
      Result := True;
    if Result then
      Inc(FAprLoadCounter);
  finally
    FLock.Leave;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClientManager.LoadSvnLibs: Boolean;

begin
  FLock.Enter;
  try
    if FSvnLoadCounter = 0 then
      Result := LoadSvnClientLib and LoadSvnDeltaLib and LoadSvnDiffLib and LoadSvnFsLib and LoadSvnRaLib and
        LoadSvnReposLib and LoadSvnSubrLib and LoadSvnWcLib
    else
      Result := True;
    if Result then
      Inc(FSvnLoadCounter);
  finally
    FLock.Leave;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

var
  GSvnClientManager: TSvnClientManager = nil;

function SvnClientManager: TSvnClientManager;

begin
  if not Assigned(GSvnClientManager) then
    GSvnClientManager := TSvnClientManager.Create;
  Result := GSvnClientManager;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FreeSvnClientManager;

begin
  FreeAndNil(GSvnClientManager);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnClient private }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.GetExternalsCallback(Sender: TObject; Item: TSvnItem; var Cancel: Boolean);

begin
  Cancel := False;
  if Assigned(FExternals) and (Item.TextStatus = svnWcStatusExternal) then
    FExternals.Add(Item.PathName);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.GetInitialized: Boolean;

begin
  Result := Assigned(FAllocator);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.GetListCallback(Sender: TObject; Path: string; DirEntry: TSvnDirEnt; Locked: Boolean;
  LockData: TSvnLock; AbsPath: string; var Cancel: Boolean);

begin
  Cancel := False;
  if Assigned(FListStrings) then
  begin
    if Length(Path) = 0 then
      Path := '.'
    else
      AbsPath := AbsPath + '/' + Path;
    FListStrings.Add(Path + '=' + AbsPath);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnClient protected }

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoBlame(LineNo: Int64; Revision: TSvnRevNum; const Author, Date, Line: string): Boolean;

var
  D: TDateTime;
  L: Integer;
  SLine: string;

begin
  Result := False;
  if Assigned(FBlameCallback) then
  begin
    D := SvnStrToDateTime(Date, FBlameSubPool);
    SLine := Line;
    L := Length(SLine);
    if (L > 0) and (SLine[L] = #13) then
      System.Delete(SLine, L, 1);
    FBlameCallback(Self, LineNo, Revision, Author, D, SLine, Result);
    FCancelled := Result;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoCancel: Boolean;

var
  Cancel: Boolean;

begin
  if Assigned(FOnCancel) then
  begin
    Cancel := False;
    FOnCancel(Self, Cancel);
    if Cancel then
      FCancelled := True;
  end;
  Result := Cancelled;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoList(Path: string; DirEntry: TSvnDirEnt; Locked: Boolean; LockData: TSvnLock; AbsPath: string): Boolean;

begin
  Result := False;
  if Assigned(FListCallback) then
  begin
    FListCallback(Self, Path, DirEntry, Locked, LockData, AbsPath, Result);
    FCancelled := Result;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoLoginPrompt(const Realm: string; var UserName, Password: string; var Save: Boolean): Boolean;

begin
  Result := not Assigned(FOnLoginPrompt);
  if not Result then
    FOnLoginPrompt(Self, Realm, UserName, Password, Result, Save);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoNotify(const Path, MimeType: string; Action: TSvnWcNotifyAction; Kind: TSvnNodeKind;
  ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum): Boolean;

var
  SPath: string;

begin
  Result := False;
  if Assigned(FNotifyCallback) then
  begin
    SPath := Path;
    if SPath = SvnPathDelim then
      Insert(ExtractFileDrive(GetCurrentDir), SPath, 1)
    else if SPath = SvnExtractFileDrive(SPath) then
      SPath := SvnIncludeTrailingPathDelimiter(SPath);
         
    FNotifyCallback(Self, SPath, MimeType, Action, Kind, ContentState, PropState, Revision, Result);
    FCancelled := Result;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.DoProgress(Progress, Total: TAprOff);

begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Progress, Total);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoSSLClientCertPrompt(const Realm: string; var CertFileName: string; var Save: Boolean): Boolean;

begin
  Result := not Assigned(FOnSSLClientCertPrompt);
  if not Result then
    FOnSSLClientCertPrompt(Self, Realm, CertFileName, Result, Save);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoSSLClientPasswordPrompt(const Realm: string; var Password: string; var Save: Boolean): Boolean;

begin
  Result := not Assigned(FOnSSLClientPasswordPrompt);
  if not Result then
     FOnSSLClientPasswordPrompt(Self, Realm, Password, Result, Save);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoSSLServerTrustPrompt(const Realm: string; const CertInfo: TSvnAuthSSLServerCertInfo;
  Failures: TSSLServerTrustFailures; var Save: Boolean): Boolean;

begin
  Result := not Assigned(FOnSSLServerTrustPrompt);
  if not Result then
    FOnSSLServerTrustPrompt(Self, Realm, CertInfo, Failures, Result, Save);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoUserNamePrompt(const Realm: string; var UserName: string; var Save: Boolean): Boolean;

begin
  Result := not Assigned(FOnUserNamePrompt);
  if not Result then
    FOnUserNamePrompt(Self, Realm, UserName, Result, Save);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.DoWCStatus(Path: PAnsiChar; const Status: TSvnWcStatus2): Boolean;

var
  Item: TSvnItem;

begin
  Result := False;
  if Assigned(FStatusCallback) then
  begin
    Item := TSvnItem.Create(Self, nil, string(Path), Status);
    try
      FStatusCallback(Self, Item, Result);
    except
      Item.Free;
      raise;
    end;
    FCancelled := Result;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnClient public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnClient.Create;

begin
  inherited Create;
  FAllocator := nil;
  FPool := nil;
  FCtx := nil;
  FUserName := '';
  FPassword := '';
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnClient.Destroy;

begin
  Finalize;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Add(const PathName: string; Recurse, Force, NoIgnore: Boolean; SubPool: PAprPool);

var
  NewPool: Boolean;

begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FCancelled := False;
    SvnCheck(svn_client_add3(PAnsiChar(AnsiString(NativePathToSvnPath(PathName))), Recurse, Force, NoIgnore, FCtx, SubPool));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Blame(const PathName: string; Callback: TSvnBlameCallback; StartRevision: TSvnRevNum = 1;
  EndRevision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil);

var
  NewPool: Boolean;
  PegRev, StartRev, EndRev: TSvnOptRevision;

begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(StartRev, SizeOf(TSvnOptRevision), 0);
    StartRev.Kind := svnOptRevisionNumber;
    if StartRevision <= 0 then
      StartRev.Value.number := 1
    else
      StartRev.Value.number := StartRevision;
    FillChar(EndRev, SizeOf(TSvnOptRevision), 0);
    if EndRevision <= 0 then
      EndRev.Kind := svnOptRevisionBase
    else
    begin
      EndRev.Kind := svnOptRevisionNumber;
      EndRev.Value.number := EndRevision;
    end;
    FillChar(PegRev, SizeOf(TSvnOptRevision), 0);
    if PegRevision <= 0 then
      PegRev.Kind := svnOptRevisionUnspecified
    else
    begin
      PegRev.Kind := svnOptRevisionNumber;
      PegRev.Value.number := PegRevision;
    end;

    FCancelled := False;
    FBlameCallback := Callback;
    FBlameSubPool := SubPool;
    SvnCheck(svn_client_blame2(PAnsiChar(AnsiString(NativePathToSvnPath(PathName))), @PegRev, @StartRev, @EndRev, BlameReceiver, Self,
      FCtx, SubPool));
  finally
    FBlameSubPool := nil;
    FBlameCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Checkout(const PathName, TargetDir: string; Callback: TSvnNotifyCallback;
  Recurse, IgnoreExternals: Boolean; Revision, PegRevision: TSvnRevNum; SubPool: PAprPool);

var
  NewPool: Boolean;
  PegRev, Rev: TSvnOptRevision;

begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(PegRev, SizeOf(TSvnOptRevision), 0);
    if PegRevision <= 0 then
      PegRev.Kind := svnOptRevisionHead
    else
    begin
      PegRev.Kind := svnOptRevisionNumber;
      PegRev.Value.number := PegRevision;
    end;
    FillChar(Rev, SizeOf(TSvnOptRevision), 0);
    if Revision <= 0 then
      Rev.Kind := svnOptRevisionHead
    else
    begin
      Rev.Kind := svnOptRevisionNumber;
      Rev.Value.number := Revision;
    end;
    FCancelled := False;
    FNotifyCallback := Callback;
    SvnCheck(svn_client_checkout2(Revision, PAnsiChar(AnsiString(PathName)),
      PAnsiChar(AnsiString(TargetDir)), @PegRev, @Rev, Recurse, IgnoreExternals,
      FCtx, SubPool));
  finally
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Cleanup(const PathName: string; SubPool: PAprPool = nil);

var
  NewPool: Boolean;

begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FCancelled := False;
    SvnCheck(svn_client_cleanup(PAnsiChar(AnsiString(NativePathToSvnPath(PathName))), FCtx, SubPool));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Copy(SourcesNames: TStrings; const DstPath: string; SubPool: PAprPool = nil);
var
  NewPool: Boolean;
  {Sources: PAprArrayHeader;}
  CommitInfo: PSvnCommitInfo;
  SrcRevision: TSvnOptRevision;

begin
  if not Assigned(SourcesNames) or (SourcesNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    {Sources := PathNamesToAprArray(SourcesNames, SubPool); }
    CommitInfo := nil;
    FCancelled := False;
    FillChar(SrcRevision, SizeOf(TSvnOptRevision), 0);
    SrcRevision.Kind := svnOptRevisionHead;
    SvnCheck(svn_client_copy3(CommitInfo, PAnsiChar(AnsiString(NativePathToSvnPath(SourcesNames[0]))), @SrcRevision,
      PAnsiChar(AnsiString(NativePathToSvnPath(DstPath))), FCtx, SubPool));
    { TODO -ousc :- put some into method declaration
- use svn_client_copy4 (crash currently with working copy paths and returns an error with repository paths)}
    {
    SvnCheck(svn_client_copy4(CommitInfo, Sources, PAnsiChar(AnsiString(NativePathToSvnPath(DstPath))),
      False, False, nil, FCtx, SubPool));
    }
    if Assigned(CommitInfo) and Assigned(CommitInfo^.post_commit_err) and (CommitInfo^.post_commit_err^ <> #0) then
      raise Exception.Create(string(CommitInfo^.post_commit_err));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.Commit(PathNames: TStrings; const LogMessage: string; Callback: TSvnNotifyCallback = nil;
  Recurse: Boolean = True; KeepLocks: Boolean = False; SubPool: PAprPool = nil): Boolean;

var
  NewPool: Boolean;
  Targets: PAprArrayHeader;
  CommitInfo: PSvnCommitInfo;

begin
  Result := False;

  if not Assigned(PathNames) or (PathNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Targets := PathNamesToAprArray(PathNames, SubPool);
    CommitInfo := nil;
    FCommitLogMessage := LogMessage;
    FNotifyCallback := Callback;
    FCancelled := False;
    SvnCheck(svn_client_commit3(CommitInfo, Targets, Recurse, KeepLocks, FCtx, SubPool));
    if Assigned(CommitInfo) and Assigned(CommitInfo^.post_commit_err) and (CommitInfo^.post_commit_err^ <> #0) then
      raise Exception.Create(string(CommitInfo^.post_commit_err));

    Result := Assigned(CommitInfo) and (CommitInfo^.revision <> SVN_INVALID_REVNUM);
  finally
    FCommitLogMessage := '';
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Delete(PathNames: TStrings; Force: Boolean = False; KeepLocal: Boolean = False; SubPool: PAprPool = nil);

var
  NewPool: Boolean;
  Paths: PAprArrayHeader;
  CommitInfo: PSvnCommitInfo;

begin
  if not Assigned(PathNames) or (PathNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Paths := PathNamesToAprArray(PathNames, SubPool);
    CommitInfo := nil;
    FCancelled := False;
    SvnCheck(svn_client_delete3(CommitInfo, Paths, Force, KeepLocal, nil, FCtx, SubPool));
    if Assigned(CommitInfo) and Assigned(CommitInfo^.post_commit_err) and (CommitInfo^.post_commit_err^ <> #0) then
      raise Exception.Create(string(CommitInfo^.post_commit_err));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Export(const PathName, TargetDir: string; Callback: TSvnNotifyCallback;
  Overwrite, Recurse, IgnoreExternals: Boolean; Revision, PegRevision: TSvnRevNum; SubPool: PAprPool);

var
  NewPool: Boolean;
  PegRev, Rev: TSvnOptRevision;

begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(PegRev, SizeOf(TSvnOptRevision), 0);
    if PegRevision <= 0 then
      PegRev.Kind := svnOptRevisionHead
    else
    begin
      PegRev.Kind := svnOptRevisionNumber;
      PegRev.Value.number := PegRevision;
    end;
    FillChar(Rev, SizeOf(TSvnOptRevision), 0);
    if Revision <= 0 then
      Rev.Kind := svnOptRevisionHead
    else
    begin
      Rev.Kind := svnOptRevisionNumber;
      Rev.Value.number := Revision;
    end;

    FCancelled := False;
    FNotifyCallback := Callback;
    SvnCheck(svn_client_export3(Revision, PAnsiChar(AnsiString(PathName)),
      PAnsiChar(AnsiString(TargetDir)), @PegRev, @Rev, Overwrite, IgnoreExternals,
      Recurse, sLineBreak, FCtx, SubPool));
  finally
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Finalize;

begin
  if Assigned(FPool) then
  begin
    apr_pool_destroy(FPool);
    FPool := nil;
  end;
  if Assigned(FPoolUtf8) then
  begin
    apr_pool_destroy(FPoolUtf8);
    FPoolUtf8 := nil;
  end;
  if Assigned(FAllocator) then
  begin
    apr_allocator_destroy(FAllocator);
    FAllocator := nil;
  end;
  if FAprLibLoaded then
    apr_terminate2;

  { TODO -cCleanup -ousc : remove if SvnClientManager solution is okay }
  {
  if FSvnClientLibLoaded then
    FreeSvnClientLib;
  if FAprLibLoaded then
    FreeAprLib;
  }
  if FSvnClientLibLoaded then
  begin
    SvnClientManager.FreeSvnLibs;
    FSvnClientLibLoaded := False;
  end;
  if FAprLibLoaded then
  begin
    SvnClientManager.FreeAprLib;
    FAprLibLoaded := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.GetExternals(const PathName: string; Externals: TStrings; Recurse: Boolean);

begin
  Externals.BeginUpdate;
  try
    FExternals := Externals;
    try
      GetModifications(PathName, GetExternalsCallback, Recurse);
    finally
      FExternals := nil;
    end;
  finally
    Externals.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.GetModifications(const PathName: string; Callback: TSvnStatusCallback;
  Recurse, Update, IgnoreExternals, RecurseUnversioned: Boolean; SubPool: PAprPool): TSvnRevNum;

var
  NewPool: Boolean;
  Revision: TSvnOptRevision;

begin
  Result := -1;
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionHead;
    FCancelled := False;
    FRecurseUnversioned := RecurseUnversioned; 
    FStatusCallback := Callback;
    SvnCheck(svn_client_status2(@Result, PAnsiChar(AnsiString(NativePathToSvnPath(PathName))), @Revision, WCStatus2, Self, Recurse,
      False, Update, False, IgnoreExternals, FCtx, SubPool));
  finally
    FStatusCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.GetPathRevision(const PathName: string): TSvnOptRevision;
var
  SubPool: PAprPool;
  TruePath: PAnsiChar;
  Revision, PegRevision: TSvnOptRevision;
  SvnError: PSvnError;

begin
  if (PathName = '') or (GetFileAttributes(PChar(PathName)) = Cardinal(-1)) then // path does not exist
    Exit;

  if not Initialized then
    Initialize;

  AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(PegRevision, SizeOf(TSvnOptRevision), 0);
    PegRevision.Kind := svnOptRevisionUnspecified;
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionUnspecified;
    AprCheck(apr_filepath_merge(TruePath, '', PAnsiChar(AnsiString(PathName)), APR_FILEPATH_TRUENAME, SubPool));
    FCancelled := False;
    SvnError := svn_client_info(TruePath, @PegRevision, @Revision, DummyInfoReceiver, nil, False, Ctx, SubPool);

    if not Assigned(SvnError) then
      Result := Revision

  finally
    apr_pool_destroy(SubPool);
  end;

end;

procedure TSvnClient.GetProps(Props: PAprArrayHeader; Strings: TStrings; SubPool: PAprPool = nil;
  Delimiter: Char = DefaultPropValDelimiter);

const
  CRLF = #13#10;

var
  NewPool: Boolean;
  P: PSvnClientPropListItem;
  I, J: Integer;
  H: PAprHashIndex;
  PName: PAnsiChar;
  PValue: PSvnString;
  Name, Value: string;

begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    if not Assigned(Props) then
      Exit;

    NewPool := not Assigned(SubPool);
    if NewPool then
      AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
    try
      P := PPSvnClientPropListItem(Props^.elts)^;
      for I := 0 to Props^.nelts - 1 do
      begin
        H := apr_hash_first(SubPool, P^.prop_hash);
        while Assigned(H) do
        begin
          apr_hash_this(H, @PName, 0, @PValue);
          // special Subversion properties, stored as UTF8
          if svn_prop_needs_translation(PName) or // svn:
            (StrLComp(PName, 'bugtraq:', 8) = 0) or (StrLComp(PName, 'tsvn:', 5) = 0) then // TortoiseSVN
            SvnCheck(svn_subst_detranslate_string(PValue, PValue, False, SubPool));

          SetString(Name, PName, StrLen(PName));
          SetString(Value, PValue^.data, StrLen(PValue^.data));

          Value := StringReplace(Value, CRLF, Delimiter, [rfReplaceAll, rfIgnoreCase]);
          if (Value <> '') and (Value[Length(Value)] = Delimiter) then
            System.Delete(Value, Length(Value), 1);

          J := Strings.IndexOfName(Name);
          if J = -1 then
            Strings.Add(Format('%s=%s', [Name, Value]))
          else
            Strings.ValueFromIndex[J] := Strings.ValueFromIndex[J] + Delimiter + Value;

          H := apr_hash_next(H);
        end;

        Inc(P);
      end;
    finally
      if NewPool then
        apr_pool_destroy(SubPool);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Initialize(const AConfigDir: string = ''; Auth: PSvnAuthBaton = nil);

var
  Providers: PAprArrayHeader;
  Provider: PSvnAuthProviderObject;
  S: string;
  P: PAnsiChar;

begin
  { TODO -cCleanup -ousc : remove if SvnClientManager solution is okay }
  {
  if not AprLibLoaded then
  begin
    if not LoadAprLib then
      RaiseLastOSError;
    FAprLibLoaded := True;
  end;
  }
  if not FAprLibLoaded then
  begin
    if SvnClientManager.LoadAprLib then
      FAprLibLoaded := True
    else
      RaiseLastOSError;
  end;

  { TODO -cCleanup -ousc : remove if SvnClientManager solution is okay }
  {
  if not SvnClientLibLoaded then
  begin
    if not LoadSvnClientLib or not LoadSvnDeltaLib or not LoadSvnDiffLib or not LoadSvnFsLib or not LoadSvnRaLib or
      not LoadSvnReposLib or not LoadSvnSubrLib or not LoadSvnWcLib then
      RaiseLastOSError;
    FSvnClientLibLoaded := True;
  end;
  }
  if not FSvnClientLibLoaded then
  begin
    if SvnClientManager.LoadSvnLibs then
      FSvnClientLibLoaded := True
    else
      RaiseLastOSError;
  end;

  AprCheck(apr_initialize);
  FPoolUtf8 := svn_pool_create_ex(nil, nil);
  svn_utf_initialize(FPoolUtf8);
  SvnCheck(svn_nls_init);

  AprCheck(apr_allocator_create(FAllocator));
  try
    apr_allocator_max_free_set(Allocator, SVN_ALLOCATOR_RECOMMENDED_MAX_FREE);
    AprCheck(apr_pool_create_ex(FPool, nil, nil, FAllocator));
    try
      SvnCheck(svn_ra_initialize(FPool));
      SvnCheck(svn_client_create_context(FCtx, FPool));

      FCtx^.notify_func := SvnContextNotify;
      FCtx^.notify_baton := Self;
      FCtx^.progress_func := SvnContextProgress;
      FCtx^.progress_baton := Self;
      FCtx^.cancel_func := SvnContextCancel;
      FCtx^.cancel_baton := Self;
      FCtx^.log_msg_func := SvnContextLogMessage;
      FCtx^.log_msg_baton := Self;

      if AConfigDir = '' then
        S := IncludeTrailingPathDelimiter(GetAppDataDir) + 'Subversion'
      else
        S := ExcludeTrailingPathDelimiter(AConfigDir);
      SvnCheck(svn_utf_cstring_to_utf8(@P, PAnsiChar(AnsiString(S)), FPool));
      P := svn_path_canonicalize(P, FPool);
      SvnCheck(svn_config_ensure(P, FPool));
      SetString(FConfigDir, P, StrLen(P));
      SvnCheck(svn_config_get_config(FCtx^.config, PAnsiChar(AnsiString(FConfigDir)), FPool));

      if not Assigned(Auth) then
      begin
        Provider := nil;
        Providers := apr_array_make(FPool, 11, SizeOf(PSvnAuthProviderObject));

        svn_client_get_windows_simple_provider(Provider, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_simple_provider(Provider, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_username_provider(Provider, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_ssl_server_trust_file_provider(Provider, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_ssl_client_cert_file_provider(Provider, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_ssl_client_cert_pw_file_provider(Provider, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_simple_prompt_provider(Provider, SimplePrompt, Self, 1, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_username_prompt_provider(Provider, UserNamePrompt, Self, 1, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_ssl_server_trust_prompt_provider(Provider, SSLServerTrustPrompt, Self, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_ssl_client_cert_prompt_provider(Provider, SSLClientCertPrompt, Self, 0, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_ssl_client_cert_pw_prompt_provider(Provider, SSLClientPasswordPrompt, Self, 0, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;

        svn_auth_open(Auth, Providers, FPool);
      end;
      Ctx^.auth_baton := Auth;
      svn_auth_set_parameter(Auth, SVN_AUTH_PARAM_CONFIG_DIR, PAnsiChar(AnsiString(FConfigDir)));
      if FUserName <> '' then
        svn_auth_set_parameter(Auth, SVN_AUTH_PARAM_DEFAULT_USERNAME, PAnsiChar(AnsiString(FUserName)));
      if FPassword <> '' then
        svn_auth_set_parameter(Auth, SVN_AUTH_PARAM_DEFAULT_PASSWORD, PAnsiChar(AnsiString(FPassword)));
      svn_auth_set_parameter(Auth, SVN_AUTH_PARAM_DONT_STORE_PASSWORDS, PAnsiChar(''));
      svn_auth_set_parameter(Auth, SVN_AUTH_PARAM_NO_AUTH_CACHE, PAnsiChar(''));
    except
      apr_pool_destroy(FPool);
      FPool := nil;
      raise;
    end;
  except
    apr_pool_destroy(FPoolUtf8);
    apr_allocator_destroy(FAllocator);
    FAllocator := nil;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.IsPathVersioned(const PathName: string): Boolean;

var
  SubPool: PAprPool;
  TruePath: PAnsiChar;
  Revision, PegRevision: TSvnOptRevision;
  SvnError: PSvnError;

begin
  Result := False;
  if (PathName = '') or (GetFileAttributes(PChar(PathName)) = Cardinal(-1)) then // path does not exist
    Exit;

  if not Initialized then
    Initialize;

  AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(PegRevision, SizeOf(TSvnOptRevision), 0);
    PegRevision.Kind := svnOptRevisionUnspecified;
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionUnspecified;
    AprCheck(apr_filepath_merge(TruePath, '', PAnsiChar(AnsiString(PathName)), APR_FILEPATH_TRUENAME, SubPool));
    FCancelled := False;
    SvnError := svn_client_info(TruePath, @PegRevision, @Revision, DummyInfoReceiver, nil, False, Ctx, SubPool);
    Result := not Assigned(SvnError);
    if not Result then
    begin
      case SvnError^.apr_err of
        SVN_ERR_WC_NOT_DIRECTORY, SVN_ERR_UNVERSIONED_RESOURCE:
          svn_error_clear(SvnError);
        else
          RaiseSvnError(SvnError);
      end;
    end;
  finally
    apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.List(const PathName: string; Depth: TSvnDepth; FetchLocks: Boolean; DirEntryFields: DWORD = SVN_DIRENT_ALL;
  Callback: TSvnListCallback = nil; Revision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil);

var
  NewPool: Boolean;
  PegRev, Rev: TSvnOptRevision;

begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(PegRev, SizeOf(TSvnOptRevision), 0);
    if PegRevision <= 0 then
      PegRev.Kind := svnOptRevisionUnspecified
    else
    begin
      PegRev.Kind := svnOptRevisionNumber;
      PegRev.Value.number := PegRevision;
    end;
    FillChar(Rev, SizeOf(TSvnOptRevision), 0);
    if Revision <= 0 then
      Rev.Kind := svnOptRevisionHead
    else
    begin
      Rev.Kind := svnOptRevisionNumber;
      Rev.Value.number := Revision;
    end;

    FCancelled := False;
    FListCallback := Callback;
    SvnCheck(svn_client_list2(PAnsiChar(AnsiString(PathName)), @PegRev, @Rev, Depth, DirEntryFields, FetchLocks, ListReceiver, Self,
      FCtx, SubPool));
  finally
    FListCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.List(const PathName: string; Depth: TSvnDepth; FetchLocks: Boolean; ListStrings: TStrings;
  DirEntryFields: DWORD = SVN_DIRENT_ALL; Revision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil);

begin
  ListStrings.BeginUpdate;
  try
    FListStrings := ListStrings;
    try
      List(PathName, Depth, FetchLocks, DirEntryFields, GetListCallback, Revision, PegRevision, SubPool);
    finally
      FListStrings := nil;
    end;
  finally
    ListStrings.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.MatchGlobalIgnores(const PathName: string; SubPool: PAprPool = nil): Boolean;

var
  NewPool: Boolean;
  GlobalIgnores: PAprArrayHeader;

begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    SvnCheck(svn_wc_get_default_ignores(GlobalIgnores, FCtx^.config, SubPool));
    Result := svn_cstring_match_glob_list(PAnsiChar(AnsiString(PathName)), GlobalIgnores);
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Move(SrcPathNames: TStrings; const DstPath: string; SubPool: PAprPool = nil);

var
  NewPool: Boolean;
  SrcPaths: PAprArrayHeader;
  CommitInfo: PSvnCommitInfo;

begin
  if not Assigned(SrcPathNames) or (SrcPathNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    SrcPaths := PathNamesToAprArray(SrcPathNames, SubPool);
    CommitInfo := nil;
    FCancelled := False;
    { TODO -ousc : put params force, move_as_child, make_parents into method declaration }
    SvnCheck(svn_client_move5(CommitInfo, SrcPaths, PAnsiChar(AnsiString(NativePathToSvnPath(DstPath))),
      True, False, False, nil, FCtx, SubPool));
    if Assigned(CommitInfo) and Assigned(CommitInfo^.post_commit_err) and (CommitInfo^.post_commit_err^ <> #0) then
      raise Exception.Create(string(CommitInfo^.post_commit_err));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.NativePathToSvnPath(const NativePath: string; SubPool: PAprPool = nil): string;

var
  NewPool: Boolean;
  SvnPath: PAnsiChar;

begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    AprCheck(apr_filepath_merge(SvnPath, '', PAnsiChar(AnsiString(NativePath)), APR_FILEPATH_TRUENAME, SubPool));
    Result := string(SvnPath);
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.PathNamesToAprArray(PathNames: TStrings; SubPool: PAprPool): PAprArrayHeader;

var
  NewPool: Boolean;
  I: Integer;
  CurrentDrive, S: string;
  P: PAnsiChar;

begin
  Result := nil;
  
  if not Assigned(PathNames) or (PathNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  CurrentDrive := ExtractFileDrive(GetCurrentDir);

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Result := apr_array_make(SubPool, PathNames.Count, SizeOf(PAnsiChar));

    for I := 0 to PathNames.Count - 1 do
    begin
      // Work around an apparent Subversion glitch:
      // Svn update reproducibly fails with error SVN_ERR_WC_LOCKED "Attempted to lock an already-locked dir"
      // in two cases:
      // 1. A directory with trailing path delimiter, e.g. D:\Temp\Test\ (D:/Temp/Test/ in SVN notation)
      //    Workaround: always exclude the trailing path delimiter

      S := ExcludeTrailingPathDelimiter(PathNames[I]);

      // 2. Root directory which is the same as the current directory drive, e.g. 'D:\' or 'D:'
      //    when current directory is anywhere on drive D:
      //    Workaround: use '/'

      if S = CurrentDrive then
        P := SvnPathDelim
      else
        AprCheck(apr_filepath_merge(P, '', PAnsiChar(AnsiString(S)), APR_FILEPATH_TRUENAME, SubPool));
        
      PPAnsiChar(apr_array_push(Result))^ := P;
    end;
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.PathNamesToAprArray(const PathNames: array of string; SubPool: PAprPool): PAprArrayHeader;

var
  NewPool: Boolean;
  I: Integer;
  CurrentDrive, S: string;
  P: PAnsiChar;

begin
  Result := nil;

  if Length(PathNames) = 0 then
    Exit;

  if not Initialized then
    Initialize;

  CurrentDrive := ExtractFileDrive(GetCurrentDir);

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Result := apr_array_make(SubPool, Length(PathNames), SizeOf(PAnsiChar));

    for I := Low(PathNames) to High(PathNames) do
    begin
      // Work around an apparent Subversion glitch:
      // Svn update reproducibly fails with error SVN_ERR_WC_LOCKED "Attempted to lock an already-locked dir"
      // in two cases:
      // 1. A directory with trailing path delimiter, e.g. D:\Temp\Test\ (D:/Temp/Test/ in SVN notation)
      //    Workaround: always exclude the trailing path delimiter

      S := ExcludeTrailingPathDelimiter(PathNames[I]);

      // 2. Root directory which is the same as the current directory drive, e.g. 'D:\' or 'D:'
      //    when current directory is anywhere on drive D:
      //    Workaround: use '/'

      if S = CurrentDrive then
        P := SvnPathDelim
      else
        AprCheck(apr_filepath_merge(P, '', PAnsiChar(AnsiString(S)), APR_FILEPATH_TRUENAME, SubPool));

      PPAnsiChar(apr_array_push(Result))^ := P;
    end;
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Revert(PathNames: TStrings; Callback: TSvnNotifyCallback = nil; Recurse: Boolean = True;
  SubPool: PAprPool = nil);

var
  NewPool: Boolean;
  Paths: PAprArrayHeader;

begin
  if not Assigned(PathNames) or (PathNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Paths := PathNamesToAprArray(PathNames, SubPool);
    FCancelled := False;
    FNotifyCallback := Callback;
    SvnCheck(svn_client_revert(Paths, Recurse, FCtx, SubPool));
  finally
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Resolved(const SvnPath: string; Recurse: Boolean = False; SubPool: PAprPool = nil);

var
  NewPool: Boolean;

begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    SvnCheck(svn_client_resolved(PAnsiChar(AnsiString(SvnPath)), Recurse, FCtx, SubPool));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnClient.SvnPathToNativePath(const SvnPath: string; SubPool: PAprPool = nil): string;

var
  NewPool: Boolean;
  NativePath: PAnsiChar;

begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    AprCheck(apr_filepath_merge(NativePath, '', PAnsiChar(AnsiString(SvnPath)), APR_FILEPATH_NATIVE, SubPool));
    Result := string(NativePath);
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnClient.Update(PathNames: TStrings; Callback: TSvnNotifyCallback = nil; Recurse: Boolean = True;
  IgnoreExternals: Boolean = False; SubPool: PAprPool = nil);

var
  NewPool: Boolean;
  Targets: PAprArrayHeader;
  Revision: TSvnOptRevision;

begin
  if not Assigned(PathNames) or (PathNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Targets := PathNamesToAprArray(PathNames, SubPool);
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionHead;
    FCancelled := False;
    FNotifyCallback := Callback;
    SvnCheck(svn_client_update2(nil, Targets, @Revision, Recurse, IgnoreExternals, FCtx, SubPool));
  finally
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization

finalization
  FreeSvnClientManager;

end.