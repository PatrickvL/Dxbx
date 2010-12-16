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

unit uEmuXOnline;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  Winsock,
  SysUtils,
  // Jedi Win32API
  JwaWinType,
  // Dxbx
  uEmuKrnl,
  uTypes,
  uLog,
  uEmuFS;

implementation

const
  lfUnit = lfCxbx or lfXOnline;
  // Todo: Get real ammount of XONLINE_MAX_LOGON_USERS
  XONLINE_MAX_LOGON_USERS = 256; // 256 is made up...
  // Todo: Get real ammound of XONLINE_STAT_MAX_MEMBERS_IN_UNIT
  XONLINE_STAT_MAX_MEMBERS_IN_UNIT = 16; // 16 is made up
  // Todo: Get real ammound of XONLINE_GAMERTAG_SIZE
  XONLINE_GAMERTAG_SIZE = 12; // 12 is made up
  // Todo: Get real ammound of XONLINE_PASSCODE_LENGTH
  XONLINE_PASSCODE_LENGTH = 12; // 12 is made up
  // Todo: Get real ammound of XONLINE_USER_RESERVED_SIZE
  XONLINE_USER_RESERVED_SIZE = 12; // 12 is made up
  // Todo: Get real ammound of MAX_STATEDATA_SIZE
  MAX_STATEDATA_SIZE = 12; // 12 is made up
  // Todo: Get real ammound of XONLINE_LOGON_STATE_SIZE
  XONLINE_LOGON_STATE_SIZE = 12; // 12 is made up
  // Todo: Get real ammound of XONLINE_MAX_TEAM_NAME_SIZE
  XONLINE_MAX_TEAM_NAME_SIZE = 265; // 256 is made up
  // Todo: Get real ammound of XONLINE_MAX_TEAM_DESCRIPTION_SIZE
  XONLINE_MAX_TEAM_DESCRIPTION_SIZE = 256; // 256 is made up
  // Todo: Get real ammound of XONLINE_MAX_TEAM_MOTTO_SIZE
  XONLINE_MAX_TEAM_MOTTO_SIZE = 256; // 256 is made up
  // Todo: Get real ammound of XONLINE_MAX_TEAM_URL_SIZE
  XONLINE_MAX_TEAM_URL_SIZE = 256; // 256 is made up
  // Todo: Get real ammound of XONLINE_MAX_TEAM_DATA_SIZE
  XONLINE_MAX_TEAM_DATA_SIZE = 256; // 256 is made up
  // Todo: Get real ammound of XONLINE_MAX_TEAM_MEMBER_DATA_SIZE
  XONLINE_MAX_TEAM_MEMBER_DATA_SIZE = 256; // 256 is made up


type
   PHRESULT = ^HRESULT;

   _XNKID = record
      ab: Array[0..8-1] of BYTE;
   end;
   XNKID = _XNKID;
   PXNKID = ^XNKID;

   _XUID = record
    qwUserID: ULONGLONG;
    dwUserFlags: DWORD;
   end;
   XUID = _XUID;
   PXUID = ^XUID;

  _XONLINE_ARB_ID = record
    SessionID: XNKID;
    qwRoundID: ULONGLONG;
  end;
  XONLINE_ARB_ID = _XONLINE_ARB_ID;
  PXONLINE_ARB_ID = ^XONLINE_ARB_ID;

  _XONLINE_ARB_REGISTRANT = record
    qwMachineID: ULONGLONG;
    xuidUsers: Array [0..XONLINE_MAX_LOGON_USERS-1] of XUID;
    bReliabilityValue: BYTE;
  end;
  XONLINE_ARB_REGISTRANT = _XONLINE_ARB_REGISTRANT;
  PXONLINE_ARB_REGISTRANT = ^XONLINE_ARB_REGISTRANT;

  XONLINE_STAT_TYPE = (
    XONLINE_STAT_NONE,
    XONLINE_STAT_LONG,
    XONLINE_STAT_LONGLONG,
    XONLINE_STAT_DOUBLE,
    XONLINE_STAT_LPCWSTR );

  _XONLINE_STAT = record
    wID: WORD;
    atype: XONLINE_STAT_TYPE;
    case Integer of
      0: (lValue: LONG);
      1: (llValue: LONGLONG);
      2: (dValue: double);
      3: (lpString: LPCWSTR);
  end;
  XONLINE_STAT = _XONLINE_STAT;
  PXONLINE_STAT = ^XONLINE_STAT;

  _XONLINE_STAT_UPDATE = record
    xuid: XUID;
    dwLeaderBoardID: DWORD;
    dwConditionalIndex: DWORD;
    dwNumStats: DWORD;
    pStats: PXONLINE_STAT;
  end;
  XONLINE_STAT_UPDATE = _XONLINE_STAT_UPDATE;
  PXONLINE_STAT_UPDATE = ^XONLINE_STAT_UPDATE;

  _XONLINE_STAT_UPDATE_UNIT = record
    xuidUnitMembers: Array[0..XONLINE_STAT_MAX_MEMBERS_IN_UNIT-1] of XUID;
    dwLeaderBoardID: DWORD;
    dwConditionalUnitIndex: DWORD;
    dwNumStats: DWORD;
    pStats: PXONLINE_STAT;
  end;
  XONLINE_STAT_UPDATE_UNIT = _XONLINE_STAT_UPDATE_UNIT;
  PXONLINE_STAT_UPDATE_UNIT = ^XONLINE_STAT_UPDATE_UNIT;

  _XONLINE_STAT_CONDITIONAL = record
    xuid: XUID;
    dwLeaderBoardID: DWORD;
    bComparisonType: BYTE;
    StatToCompare: XONLINE_STAT;
  end;
  XONLINE_STAT_CONDITIONAL = _XONLINE_STAT_CONDITIONAL;
  PXONLINE_STAT_CONDITIONAL = ^XONLINE_STAT_CONDITIONAL;

  _XONLINE_STAT_CONDITIONAL_UNIT = record
    xuidUnitMembers: Array [0..XONLINE_STAT_MAX_MEMBERS_IN_UNIT-1] of XUID;
    dwLeaderBoardID: DWORD;
    bComparisonType: BYTE;
    StatToCompare: XONLINE_STAT;
  end;
  XONLINE_STAT_CONDITIONAL_UNIT = _XONLINE_STAT_CONDITIONAL_UNIT;
  PXONLINE_STAT_CONDITIONAL_UNIT = ^XONLINE_STAT_CONDITIONAL_UNIT;

  _XONLINE_STAT_ELO = record
    xuid1: XUID;
    xuid2: XUID;
    dwLeaderBoardID: DWORD;
    dwConditionalIndex: DWORD;
    W: double;
    C1: double;
    C2: double;
  end;
  XONLINE_STAT_ELO = _XONLINE_STAT_ELO;
  PXONLINE_STAT_ELO = ^XONLINE_STAT_ELO;

  _XONLINE_STAT_ELO_UNIT = record
    xuidUnit1Members: Array [0..XONLINE_STAT_MAX_MEMBERS_IN_UNIT -1] of XUID;
    xuidUnit2Members: Array [0..XONLINE_STAT_MAX_MEMBERS_IN_UNIT -1] of XUID;
    dwLeaderBoardID: DWORD;
    dwConditionalUnitIndex: DWORD;
    W: double;
    C1: double;
    C2: double;
  end;
  XONLINE_STAT_ELO_UNIT = _XONLINE_STAT_ELO_UNIT;
  PXONLINE_STAT_ELO_UNIT = ^XONLINE_STAT_ELO_UNIT;

  XONLINE_STAT_PARAM_TYPE = (
    XONLINE_STAT_PARAM_NONE,
    XONLINE_STAT_PARAM_BYTE,
    XONLINE_STAT_PARAM_WORD,
    XONLINE_STAT_PARAM_LONG,
    XONLINE_STAT_PARAM_LONGLONG,
    XONLINE_STAT_PARAM_DOUBLE,
    XONLINE_STAT_PARAM_LPCWSTR,
    XONLINE_STAT_PARAM_XUID);

  _XONLINE_STAT_CUSTOM_PARAM = record
    atype: XONLINE_STAT_PARAM_TYPE;
    case Integer of
      0: (bValue: BYTE);
      1: (wValue: WORD);
      2: (w1Value: LONG);
      3: (w2Value: LONGLONG);
      4: (dValue: double);
      5: (lpString: LPCWSTR);
      6: (xuidValue: XUID);
  end;
  XONLINE_STAT_CUSTOM_PARAM = _XONLINE_STAT_CUSTOM_PARAM;
  PXONLINE_STAT_CUSTOM_PARAM = ^XONLINE_STAT_CUSTOM_PARAM;

  _XONLINE_STAT_CUSTOM = record
    dwNumParams: DWORD;
    pParams: PXONLINE_STAT_CUSTOM_PARAM;
  end;
  XONLINE_STAT_CUSTOM = _XONLINE_STAT_CUSTOM;
  PXONLINE_STAT_CUSTOM = ^XONLINE_STAT_CUSTOM;

  _XONLINE_STAT_PROC = record
    wProcedureID: WORD;
    case Integer of
      0: (Update: XONLINE_STAT_UPDATE);
      1: (UpdateUnit: XONLINE_STAT_UPDATE_UNIT);
      2: (Conditional: XONLINE_STAT_CONDITIONAL);
      3: (ConditionalUnit: XONLINE_STAT_CONDITIONAL_UNIT);
      4: (Elo: XONLINE_STAT_ELO);
      5: (EloUnit: XONLINE_STAT_ELO_UNIT);
      6: (Custom: XONLINE_STAT_CUSTOM);
  end;
  XONLINE_STAT_PROC = _XONLINE_STAT_PROC;
  PXONLINE_STAT_PROC = ^XONLINE_STAT_PROC;

  _XNADDR = record
    ina: IN_ADDR;
    inaOnline: IN_ADDR;
    wPortOnline: WORD;
    abEnet: Array[0..6-1] of BYTE;
    abOnline: Array[0..20-1] of BYTE;
  end;
  XNADDR = _XNADDR;
  PXNADDR = ^XNADDR;

  _XONLINE_ARB_SUSPICIOUS_INFO = record
    pszMessage: PCHAR;
    bNumRelatedAddresses: BYTE;
    pxnaddrRelatedAddresses: PXNADDR;
    bNumRelatedUsers: BYTE;
    pxuidRelatedUsers: PXUID;
  end;
  XONLINE_ARB_SUSPICIOUS_INFO = _XONLINE_ARB_SUSPICIOUS_INFO;
  PXONLINE_ARB_SUSPICIOUS_INFO = ^XONLINE_ARB_SUSPICIOUS_INFO;

  _XONLINE_ARB_REPORT_DATA = record
    bNumLostConnectivityAddresses: BYTE;
    pxnaddrLostConnectivityAddresses: PXNADDR;
    pSuspiciousInfoType1: PXONLINE_ARB_SUSPICIOUS_INFO;
    pSuspiciousInfoType2: PXONLINE_ARB_SUSPICIOUS_INFO;
    pSuspiciousInfoType3: PXONLINE_ARB_SUSPICIOUS_INFO;
  end;
  XONLINE_ARB_REPORT_DATA = _XONLINE_ARB_REPORT_DATA;
  PXONLINE_ARB_REPORT_DATA = ^XONLINE_ARB_REPORT_DATA;

  _XONLINE_USER = record
    xuid: XUID;
    szGamertag: Array [0..XONLINE_GAMERTAG_SIZE -1] of CHAR;
    dwUserOptions: DWORD;
    passcode: Array [0..XONLINE_PASSCODE_LENGTH -1] of BYTE;
    reserved: Array [0..XONLINE_USER_RESERVED_SIZE -1] of BYTE;
    hr: HRESULT;
  end;
  XONLINE_USER = _XONLINE_USER;
  PXONLINE_USER = ^XONLINE_USER;

  _XONLINE_ATTRIBUTE = record
    dwAttributeID: DWORD;
    fChanged: BOOL_;
    {union} case info: Integer of
    0: (integer_: record
          qwValue: ULONGLONG;
        end);
    1: (string_: record
          lpValue: LPWSTR;
        end);
    2: (blob: record
          pvValue: PVOID;
          dwLength: DWORD;
        end);
  end;
  XONLINE_ATTRIBUTE = _XONLINE_ATTRIBUTE;
  PXONLINE_ATTRIBUTE = ^XONLINE_ATTRIBUTE;

  _XONLINE_COMP_CREATE_RESULTS = record
    qwCompetitionID: ULONGLONG;
  end;
  XONLINE_COMP_CREATE_RESULTS = _XONLINE_COMP_CREATE_RESULTS;
  PXONLINE_COMP_CREATE_RESULTS = ^XONLINE_COMP_CREATE_RESULTS;

  _XONLINE_COMP_INTERVAL_UNIT = (
    XONLINE_COMP_INTERVAL_MINUTE = 2,
    XONLINE_COMP_INTERVAL_HOUR = 3,
    XONLINE_COMP_INTERVAL_DAILY = 4,
    XONLINE_COMP_INTERVAL_WEEKLY = 5);
  XONLINE_COMP_INTERVAL_UNIT = _XONLINE_COMP_INTERVAL_UNIT;
  PXONLINE_COMP_INTERVAL_UNIT = ^XONLINE_COMP_INTERVAL_UNIT;

  _XONLINE_COMP_DAY_MASK = (
    XONLINE_COMP_DAY_MASK_ALL = $007F,
    XONLINE_COMP_DAY_MASK_SUNDAY = $0001,
    XONLINE_COMP_DAY_MASK_MONDAY = $0002,
    XONLINE_COMP_DAY_MASK_TUESDAY = $0004,
    XONLINE_COMP_DAY_MASK_WEDNESDAY = $0008,
    XONLINE_COMP_DAY_MASK_THURSDAY = $0010,
    XONLINE_COMP_DAY_MASK_FRIDAY = $0020,
    XONLINE_COMP_DAY_MASK_SATURDAY = $0040);
  XONLINE_COMP_DAY_MASK = _XONLINE_COMP_DAY_MASK;

  _XONLINE_COMP_UNITS_OR_MASK = record
    dwUnitsOfTime: DWORD;
    DayMask: XONLINE_COMP_DAY_MASK;
  end;
  XONLINE_COMP_UNITS_OR_MASK = _XONLINE_COMP_UNITS_OR_MASK;
  PXONLINE_COMP_UNITS_OR_MASK = ^XONLINE_COMP_UNITS_OR_MASK;

  _XONLINE_COMP_SINGLE_ELIMINATION_ATTRIBUTES = record
    dwPrivateSlots: DWORD;
    dwPublicSlots: DWORD;
    dwMinimumPlayers: DWORD;
    ftRegistrationOpen: FILETIME;
    ftRegistrationClose: FILETIME;
    ftCompetitionStart: FILETIME;
    ftRoundOneStart: FILETIME;
    ftRoundOneEnd: FILETIME;
    dwMatchReminderAdvanceMinutes: DWORD;
    Interval: XONLINE_COMP_INTERVAL_UNIT;
    UnitOrMask: XONLINE_COMP_UNITS_OR_MASK;
    fTeamCompetition: BOOL;
    dwTeamSize: DWORD;
  end;
  XONLINE_COMP_SINGLE_ELIMINATION_ATTRIBUTES = _XONLINE_COMP_SINGLE_ELIMINATION_ATTRIBUTES;
  PXONLINE_COMP_SINGLE_ELIMINATION_ATTRIBUTES = ^XONLINE_COMP_SINGLE_ELIMINATION_ATTRIBUTES;

  _XONLINE_ATTRIBUTE_SPEC = record
    dwType: DWORD;
    dwLength: DWORD;
  end;
  XONLINE_ATTRIBUTE_SPEC = _XONLINE_ATTRIBUTE_SPEC;
  PXONLINE_ATTRIBUTE_SPEC = ^XONLINE_ATTRIBUTE_SPEC;

  _XONLINE_COMP_TOPOLOGY_SE_RESULTS = record
    dwBaseWidth: DWORD;
    dwRoundsReturned: DWORD;
    dwTotalResultEntries: DWORD;
    dwResultsSize: DWORD;
    pbResults: PBYTE;
    dwNumResultAttributeSpecs: DWORD;
    pResultAttributeSpecs: PXONLINE_ATTRIBUTE_SPEC;
  end;
  XONLINE_COMP_TOPOLOGY_SE_RESULTS = _XONLINE_COMP_TOPOLOGY_SE_RESULTS;
  PXONLINE_COMP_TOPOLOGY_SE_RESULTS = ^XONLINE_COMP_TOPOLOGY_SE_RESULTS;
  PPXONLINE_COMP_TOPOLOGY_SE_RESULTS = ^PXONLINE_COMP_TOPOLOGY_SE_RESULTS;

  _XONLINE_FEEDBACK_TYPE = (
    XONLINE_FEEDBACK_NEG_NICKNAME,
    XONLINE_FEEDBACK_NEG_GAMEPLAY,
    XONLINE_FEEDBACK_NEG_SCREAMING,
    XONLINE_FEEDBACK_NEG_HARASSMENT,
    XONLINE_FEEDBACK_NEG_LEWDNESS,
    XONLINE_FEEDBACK_POS_ATTITUDE,
    XONLINE_FEEDBACK_POS_SESSION,
    XONLINE_FEEDBACK_POS_STATS_ATTACHMENT,
    XONLINE_FEEDBACK_NEG_STATS_ATTACHMENT,
    XONLINE_FEEDBACK_NEG_STATS_ATTACHMENT_CHEATING,
    NUM_XONLINE_FEEDBACK_TYPES);
  XONLINE_FEEDBACK_TYPE = _XONLINE_FEEDBACK_TYPE;

  _XONLINE_FRIEND = record
    xuid: XUID;
    szGamertag: Array [0..XONLINE_GAMERTAG_SIZE -1] of CHAR;
    dwFriendState: DWORD;
    gameinviteTime: FILETIME;
    sessionID: XNKID;
    dwTitleID: DWORD;
    StateDataSize: BYTE;
    StateData: Array [0..MAX_STATEDATA_SIZE-1] of BYTE;
    bReserved: BYTE;
  end;
  XONLINE_FRIEND = _XONLINE_FRIEND;
  PXONLINE_FRIEND = ^XONLINE_FRIEND;

  _XONLINE_GAMEINVITE_ANSWER_TYPE = (
    XONLINE_GAMEINVITE_NO = 0,
    XONLINE_GAMEINVITE_YES = 1,
    XONLINE_GAMEINVITE_REMOVE = 2);
  XONLINE_GAMEINVITE_ANSWER_TYPE = _XONLINE_GAMEINVITE_ANSWER_TYPE;

  _XONLINE_REQUEST_ANSWER_TYPE = (
    XONLINE_REQUEST_NO = 0,
    XONLINE_REQUEST_YES = 1,
    XONLINE_REQUEST_BLOCK = 2);
  XONLINE_REQUEST_ANSWER_TYPE = _XONLINE_REQUEST_ANSWER_TYPE;

  _XONLINE_ACCEPTED_GAMEINVITE = record
    InvitingFriend: XONLINE_FRIEND;
    xuidAcceptedFriend: XUID;
    InviteAcceptTime: FILETIME;
    xuidLogonUsers: Array [0..XONLINE_MAX_LOGON_USERS -1] of XUID
  end;
  XONLINE_ACCEPTED_GAMEINVITE = _XONLINE_ACCEPTED_GAMEINVITE;
  PXONLINE_ACCEPTED_GAMEINVITE = ^XONLINE_ACCEPTED_GAMEINVITE;


  _XONLINE_GAMEINVITE_ANSWER_INFO = record
    xuidInvitingUser: XUID;
    szInvitingUserGamertag: Array[0..XONLINE_GAMERTAG_SIZE -1] of CHAR;
    dwTitleID: DWORD;
    SessionID: XNKID;
    GameInviteTime: FILETIME;
  end;
  XONLINE_GAMEINVITE_ANSWER_INFO = _XONLINE_GAMEINVITE_ANSWER_INFO;
  PXONLINE_GAMEINVITE_ANSWER_INFO = ^XONLINE_GAMEINVITE_ANSWER_INFO;

  _XONLINE_PEER_ANSWER_TYPE = (
    XONLINE_PEER_ANSWER_NO,
    XONLINE_PEER_ANSWER_YES,
    XONLINE_PEER_ANSWER_NEVER );
  XONLINE_PEER_ANSWER_TYPE = _XONLINE_PEER_ANSWER_TYPE;

  _XONLINE_LATEST_ACCEPTED_GAMEINVITE = record
    xuidAcceptedUser: XUID;
    xuidInvitingUser: XUID;
    szInvitingUserGamertag: Array[0..XONLINE_GAMERTAG_SIZE -1] of CHAR;
    SessionID: XNKID;
    InviteAcceptTime: FILETIME;
    xuidLogonUsers: Array [0..XONLINE_MAX_LOGON_USERS -1] of XUID;
  end;
  XONLINE_LATEST_ACCEPTED_GAMEINVITE = _XONLINE_LATEST_ACCEPTED_GAMEINVITE;
  PXONLINE_LATEST_ACCEPTED_GAMEINVITE = ^XONLINE_LATEST_ACCEPTED_GAMEINVITE;

  _XONLINE_GAME_JOIN_INFO = record
    xuidJoinedUser: XUID;
    szJoinedUserGamertag: Array [0..XONLINE_GAMERTAG_SIZE -1] of CHAR;
    dwTitleID: DWORD;
    SessionID: XNKID;
  end;
  XONLINE_GAME_JOIN_INFO = _XONLINE_GAME_JOIN_INFO;
  PXONLINE_GAME_JOIN_INFO = ^XONLINE_GAME_JOIN_INFO;

  _XONLINE_NAT_TYPE = (
    XONLINE_NAT_OPEN = 1,
    XONLINE_NAT_MODERATE,
    XONLINE_NAT_STRICT);
  XONLINE_NAT_TYPE = _XONLINE_NAT_TYPE;

  _XONLINE_NOTIFICATION_TYPE = (
    XONLINE_NOTIFICATION_FRIEND_REQUEST = 0,
    XONLINE_NOTIFICATION_GAME_INVITE = 1,
    XONLINE_NOTIFICATION_NEW_GAME_INVITE = 2,
    XONLINE_NOTIFICATION_GAME_INVITE_ANSWER = 3,
    XONLINE_NOTIFICATION_NUM = 4);
  XONLINE_NOTIFICATION_TYPE = _XONLINE_NOTIFICATION_TYPE;

  _XONLINE_NOTIFICATION_EX_INFO = record
    bMessageType: BYTE;
    dwMessageID: DWORD;
    dwNotifyFlags: DWORD
  end;
  XONLINE_NOTIFICATION_EX_INFO = _XONLINE_NOTIFICATION_EX_INFO;
  PXONLINE_NOTIFICATION_EX_INFO = ^XONLINE_NOTIFICATION_EX_INFO;

  _XONLINE_SERVICE_INFO = record
    dwServiceID: DWORD;
    serviceIP: IN_ADDR;
    wServicePort: WORD;
    wReserved: WORD;
  end;
  XONLINE_SERVICE_INFO = _XONLINE_SERVICE_INFO;
  PXONLINE_SERVICE_INFO = ^XONLINE_SERVICE_INFO;

  _XNKEY = record
    ab: Array[0..16 -1] of BYTE
  end;
  XNKEY = _XNKEY;
  PXNKEY = ^XNKEY;

  _XONLINE_MATCH_SEARCHRESULT = record
    dwReserved: DWORD;
    SessionID: XNKID;
    HostAddress: XNADDR;
    KeyExchangeKey: XNKEY;
    dwPublicOpen: DWORD;
    dwPrivateOpen: DWORD;
    dwPublicFilled: DWORD;
    dwPrivateFilled: DWORD;
    dwNumAttributes: DWORD;
  end;
  XONLINE_MATCH_SEARCHRESULT = _XONLINE_MATCH_SEARCHRESULT;
  PXONLINE_MATCH_SEARCHRESULT = ^XONLINE_MATCH_SEARCHRESULT;
  PPXONLINE_MATCH_SEARCHRESULT = ^PXONLINE_MATCH_SEARCHRESULT;

  _XONLINE_MSG_SUMMARY = record
    xuidSender: XUID;
    bMsgType: BYTE;
    qwMessageContext: ULONGLONG;
    ftSentTime: FILETIME;
    dwMessageID: DWORD;
    dwMessageFlags: DWORD;
    dwSenderTitleID: DWORD;
    wExpireMinutes: WORD;
    cbDetails: WORD;
    szSenderName: Array [0..XONLINE_GAMERTAG_SIZE -1] of char;
  end;
  XONLINE_MSG_SUMMARY = _XONLINE_MSG_SUMMARY;
  PXONLINE_MSG_SUMMARY = ^XONLINE_MSG_SUMMARY;

  _XONLINE_MSG_SEND_RESULT = record
    xuidRecipient: XUID;
    hr: HRESULT;
    dwMessageID: DWORD;
  end;
  XONLINE_MSG_SEND_RESULT = _XONLINE_MSG_SEND_RESULT;
  PXONLINE_MSG_SEND_RESULT = ^XONLINE_MSG_SEND_RESULT;

  _XONLINE_MUTELISTUSER = record
    xuid: XUID;
    dwReserved: DWORD
  end;
  XONLINE_MUTELISTUSER = _XONLINE_MUTELISTUSER;
  PXONLINE_MUTELISTUSER = ^XONLINE_MUTELISTUSER;

  _XONLINE_TAX_TYPE = (
    NO_TAX = 0,
    DEFAULT,
    GST,
    VAT,
    TAX_NOT_APPLICABLE);
  XONLINE_TAX_TYPE = _XONLINE_TAX_TYPE;

  _XONLINE_PRICE = record
    dwWholePart: DWORD;
    dwFractionalPart: DWORD;
    rgwchISOCurrencyCode: Array [0..3 -1] of WCHAR;
    fOfferingIsFree: BOOL;
    Tax: XONLINE_TAX_TYPE;
    bCurrencyFormat: BYTE;
  end;
  XONLINE_PRICE = _XONLINE_PRICE;
  PXONLINE_PRICE = ^XONLINE_PRICE;


  _XONLINE_OFFERING_FREQUENCY = (
    ONE_TIME_CHARGE = 0,
    MONTHLY,
    QUARTERLY,
    BIANNUALLY,
    ANNUALLY);
  XONLINE_OFFERING_FREQUENCY = _XONLINE_OFFERING_FREQUENCY;

  _XONLINEOFFERING_DETAILS = record
    pbDetailsBuffer: PBYTE;  // Pointer to buffer of details blob
    dwDetailsBuffer: DWORD;  // Length of details blob
    dwInstances: DWORD;  // Count of currently-owned instances
    Price: XONLINE_PRICE;  // price structure
    dwFreeMonthsBeforeCharge: DWORD;  // free months before charge begins
    dwDuration: DWORD;  // duration of the recurring charge (months)
    Frequency: XONLINE_OFFERING_FREQUENCY;  // how often charges are made
  end;
  XONLINEOFFERING_DETAILS = _XONLINEOFFERING_DETAILS;
  PXONLINEOFFERING_DETAILS = ^XONLINEOFFERING_DETAILS;

  _XONLINEOFFERING_ENUM_PARAMS = record
    dwOfferingType: DWORD;
    dwBitFilter: DWORD;
    dwDescriptionIndex: DWORD;
    wStartingIndex: WORD;
    wMaxResults: WORD;
  end;
  XONLINEOFFERING_ENUM_PARAMS = _XONLINEOFFERING_ENUM_PARAMS;
  PXONLINEOFFERING_ENUM_PARAMS = ^XONLINEOFFERING_ENUM_PARAMS;

  _XONLINE_PRESENCE = record
    xuid: XUID;
    dwUserState: DWORD;
    SessionID: XNKID;
    dwTitleID: DWORD;
    StateDataSize: BYTE;
    StateData: Array [0..MAX_STATEDATA_SIZE -1] of BYTE;
  end;
  XONLINE_PRESENCE = _XONLINE_PRESENCE;
  PXONLINE_PRESENCE = ^XONLINE_PRESENCE;

  _XONLINE_SIGNATURE_TO_VERIFY = record
    cbDigest: DWORD;
    pbDigest: PBYTE;
    cbOnlineSignature: DWORD;
    pbOnlineSignature: PBYTE
  end;
  XONLINE_SIGNATURE_TO_VERIFY = _XONLINE_SIGNATURE_TO_VERIFY;
  PXONLINE_SIGNATURE_TO_VERIFY = ^XONLINE_SIGNATURE_TO_VERIFY;

  _XONLINE_STAT_SORTORDER = (
      XONLINE_STAT_SORTORDER_LASTACTIVITY,
      XONLINE_STAT_SORTORDER_RATING );
  XONLINE_STAT_SORTORDER = _XONLINE_STAT_SORTORDER;

  _XNQOSINFO = record
    bFlags: BYTE;
    bReserved: BYTE;
    cProbesXmit: WORD;
    cProbesRecv: BYTE;
    cbData: WORD;
    pbData: PBYTE;
    wRttMinInMsecs: WORD;
    wRttMedInMsecs: WORD;
    dwUpBitsPerSec: DWORD;
    dwDnBitsPerSec: DWORD
  end;
  XNQOSINFO = _XNQOSINFO;


  _XNQOS = record
    cxnqos: UINT;
    cxnqosPending: UINT;
    axnqosinfo: Array [0..1 -1] of XNQOSINFO;
  end;
  XNQOS = _XNQOS;
  PXNQOS = ^XNQOS;

  PIN_ADDR = ^IN_ADDR;

  _XONLINE_FEEDBACK_PARAMS = record
      lpStringParam: LPCWSTR;
  end;
  XONLINE_FEEDBACK_PARAMS = _XONLINE_FEEDBACK_PARAMS;
  PXONLINE_FEEDBACK_PARAMS = ^XONLINE_FEEDBACK_PARAMS;

  // Implement XOFFERING_ID
(*  _XONLINEOFFERING_INFO = record
    XOFFERING_ID OfferingId;  // Offering ID
    DWORD dwOfferingType;  // Offering type
    DWORD dwBitFlags;  // Package-specific flags
    DWORD dwPackageSize;  // Package wire size (bytes)
    DWORD dwInstallSize;  // Installed size (blocks)
    FILETIME ftActivationDate;  // Activation date of package
    DWORD dwRating;  // Package rating
    WORD fOfferingFlags;  // Per-offering flags
    DWORD dwTitleSpecificData;  // Size of data blob (bytes)
    PBYTE pbTitleSpecificData;  // Pointer to data blob
} XONLINEOFFERING_INFO, *PXONLINEOFFERING_INFO; *)

  _XONLINE_LOGON_STATE = record
    bType: BYTE;
    bVersion: BYTE;
    cbSize: WORD;
    Data: Array [0..XONLINE_LOGON_STATE_SIZE -1] of BYTE;
  end;
  XONLINE_LOGON_STATE = _XONLINE_LOGON_STATE;
  PXONLINE_LOGON_STATE = ^XONLINE_LOGON_STATE;

  _XONLINE_STARTUP_PARAMS = record
    dwMaxPrivatePool: DWORD;
  end;
  XONLINE_STARTUP_PARAMS = _XONLINE_STARTUP_PARAMS;
  PXONLINE_STARTUP_PARAMS = ^XONLINE_STARTUP_PARAMS;

  _XONLINE_STAT_USER = record
    xuidUser: XUID;
    case Integer of
      0:  (szGamertag: Array [0..XONLINE_GAMERTAG_SIZE -1] of CHAR);
      1:  (wszTeamName: Array [0..XONLINE_MAX_TEAM_NAME_SIZE -1] of WCHAR);
  end;
  XONLINE_STAT_USER = _XONLINE_STAT_USER;
  PXONLINE_STAT_USER = ^XONLINE_STAT_USER;

  _XONLINE_STAT_SPEC = record
    xuidUser: XUID;
    dwLeaderBoardID: DWORD;
    dwNumStats: DWORD;
    pStats: PXONLINE_STAT;
  end;
  XONLINE_STAT_SPEC = _XONLINE_STAT_SPEC;
  PXONLINE_STAT_SPEC = ^XONLINE_STAT_SPEC;

  _XONLINE_STAT_NAME = record
    case Integer of
      0: (szGamertag: Array [0..XONLINE_GAMERTAG_SIZE -1] of CHAR);
      1: (wszTeamName: Array [0..XONLINE_MAX_TEAM_NAME_SIZE -1] of WCHAR);
  end;
  XONLINE_STAT_NAME = _XONLINE_STAT_NAME;
  PXONLINE_STAT_NAME = ^XONLINE_STAT_NAME;


  _XONLINE_STAT_UNIT = record
    xuidUnitMembers: Array [0..XONLINE_STAT_MAX_MEMBERS_IN_UNIT -1] of XUID;
    UnitMemberNames: Array [0..XONLINE_STAT_MAX_MEMBERS_IN_UNIT -1] of XONLINE_STAT_NAME;
  end;
  XONLINE_STAT_UNIT = _XONLINE_STAT_UNIT;
  PXONLINE_STAT_UNIT = ^XONLINE_STAT_UNIT;

  _XONLINE_STAT_SPEC_UNIT = record
    dwLeaderBoardID: DWORD;
    dwNumStats: DWORD;
    pStats: PXONLINE_STAT;
  end;
  XONLINE_STAT_SPEC_UNIT = _XONLINE_STAT_SPEC_UNIT;
  PXONLINE_STAT_SPEC_UNIT = ^XONLINE_STAT_SPEC_UNIT;

  _XONLINE_STAT_ATTACHMENT_REFERENCE = record
    dwLeaderboardIndex: DWORD;
    qwUserPuid: ULONGLONG;
  end;
  XONLINE_STAT_ATTACHMENT_REFERENCE = _XONLINE_STAT_ATTACHMENT_REFERENCE;
  PXONLINE_STAT_ATTACHMENT_REFERENCE = ^XONLINE_STAT_ATTACHMENT_REFERENCE;

  _XONLINESTORAGE_FILE_INFO = record
    dwTitleID: DWORD;
    dwTitleVersion: DWORD;
    qwOwnerPUID: ULONGLONG;
    bCountryID: BYTE;
    qwReserved: ULONGLONG;
    dwContentType: DWORD;
    dwStorageSize: DWORD;
    dwInstalledSize: DWORD;
    ftCreated: FILETIME;
    ftLastModified: FILETIME;
    wAttributesSize: WORD;
    cchPathName: WORD;
    wszPathName: LPCWSTR;
    pbAttributes: PBYTE;
  end;
  XONLINESTORAGE_FILE_INFO = _XONLINESTORAGE_FILE_INFO;
  PXONLINESTORAGE_FILE_INFO = ^XONLINESTORAGE_FILE_INFO;

  PLPCWSTR = ^LPCWSTR;

  _XONLINE_TEAM_PROPERTIES = record
    wszTeamName: Array [0..XONLINE_MAX_TEAM_NAME_SIZE -1] of WCHAR;
    wszDescription: Array [0..XONLINE_MAX_TEAM_DESCRIPTION_SIZE -1] of WCHAR;
    wszMotto: Array [0..XONLINE_MAX_TEAM_MOTTO_SIZE -1] of WCHAR;
    wszURL: Array [0..XONLINE_MAX_TEAM_URL_SIZE -1] of WCHAR;
    TeamDataSize: WORD;
    TeamData: Array [0..XONLINE_MAX_TEAM_DATA_SIZE -1] of BYTE;
  end;
  XONLINE_TEAM_PROPERTIES = _XONLINE_TEAM_PROPERTIES;
  PXONLINE_TEAM_PROPERTIES = ^XONLINE_TEAM_PROPERTIES;

  _XONLINE_TEAM_MEMBER_PROPERTIES = record
    dwPrivileges: DWORD;
    TeamMemberDataSize: WORD;
    TeamMemberData: Array [0..XONLINE_MAX_TEAM_MEMBER_DATA_SIZE -1] of BYTE;
  end;
  XONLINE_TEAM_MEMBER_PROPERTIES = _XONLINE_TEAM_MEMBER_PROPERTIES;
  PXONLINE_TEAM_MEMBER_PROPERTIES = ^XONLINE_TEAM_MEMBER_PROPERTIES;

  _XONLINE_TEAM = record
    xuidTeam: XUID;
    TeamProperties: XONLINE_TEAM_PROPERTIES;
    dwFlags: DWORD;
    CreationTime: FILETIME;
    dwMemberCount: DWORD;
  end;
  XONLINE_TEAM = _XONLINE_TEAM;
  PXONLINE_TEAM = ^XONLINE_TEAM;

  _XONLINE_TEAM_MEMBER = record
    xuidTeamMember: XUID;
    szGamertag: Array [0..XONLINE_GAMERTAG_SIZE -1] of CHAR;
    TeamMemberProperties: XONLINE_TEAM_MEMBER_PROPERTIES;
    dwFlags: DWORD;
    JoinDate: FILETIME;
  end;
  XONLINE_TEAM_MEMBER = _XONLINE_TEAM_MEMBER;
  PXONLINE_TEAM_MEMBER = ^XONLINE_TEAM_MEMBER;


function LogBegin(const aSymbolName: string): PLogStack;
begin
  Result := uLog.LogBegin(aSymbolName, {Category=}'XOnline');
end;

function XTL_EmuWSAStartup
(
  wVersionRequested: WORD;
  lpWSAData: PWSADATA
): int; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  ret: int;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuWSAStartup').
      _(wVersionRequested, 'wVersionRequested').
      _(lpWSAData, 'lpWSAData').
    LogEnd();

  ret := WSAStartup(wVersionRequested, {var}lpWSAData^);

  EmuSwapFS(fsXbox);

  Result := ret;
end;

function XTL_EmuXNetCleanup(): Integer; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetStartup').
    LogEnd();

  Unimplemented('EmuXNetCleanup');
  Result := 0;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXNetConnect
(
    ina: IN_ADDR
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    // Todo: Log ina
    LogBegin('EmuXNetStartup').
    //      _(ina, 'ina').
    LogEnd();

  Unimplemented('EmuXNetConnect');
  Result := 0;

  EmuSwapFS(fsXbox);
end;

// Todo: PXNKID and PXNKEY not availible
function XTL_EmuXNetCreateKey
(
    pxnkid: PXNKID;
    pxnKey: PXNKEY
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetCreateKey').
      _(pxnkid, 'pxnkid').
      _(pxnKey, 'pxnKey').
    LogEnd();

  Unimplemented('EmuXNetCreateKey');
  Result := 0;

  EmuSwapFS(fsXbox);
end;

// Todo: pszHost, hEvent, ppxndns not availible
(*function XTL_EmuXNetDnsLookup
(
    pszHost: PChar;
    hEvent: WSAEVENT;
    ppxndns: PPXNDNS;
): INT: stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetDnsLookup').
      _(pszHost, 'pszHost').
      _(hEvent, 'hEvent').
      _(ppxndns, 'ppxndns').
    LogEnd();

  Unimplemented('EmuXNetDnsLookup');
  Result := 0;

  EmuSwapFS(fsXbox);
end; *)

// Todo: PXNDNS not availible
(*function XTL_EmuXNetDnsRelease
(
    pxndns: PXNDNS
): INT: stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetDnsRelease').
      _(pxndns, 'pxndns').
    LogEnd();

  Unimplemented('EmuXNetDnsRelease');
  Result := 0;

  EmuSwapFS(fsXbox);
end; *)

function XTL_EmuXNetGetConnectStatus
(
    ina: IN_ADDR
): DWORD; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    // Todo: Log ina
    LogBegin('XNetGetConnectStatus').
//      _(ina, 'ina').
    LogEnd();

  Unimplemented('XNetGetConnectStatus');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXNetGetDebugXnAddr
(
    pxna: PXNADDR
): DWORD; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetGetDebugXnAddr').
      _(pxna, 'pxna').
    LogEnd();

  Unimplemented('EmuXNetGetDebugXnAddr');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXNetGetTitleXnAddr
(
    pxna: PXNADDR
): DWORD; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetGetTitleXnAddr').
      _(pxna, 'pxna').
    LogEnd();

  Unimplemented('EmuXNetGetTitleXnAddr');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuXNetInAddrToString
(
    ina: IN_ADDR;
    pchBuf: Pchar;
    cchBuf: INT
); stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    // Todo: Log ina
    LogBegin('EmuXNetInAddrToString').
//      _(ina, 'ina').
      _(pchBuf, 'pchBuf').
      _(cchBuf, 'cchBuf').
    LogEnd();

  Unimplemented('EmuXNetInAddrToString');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXNetInAddrToXnAddr
(
    ina: IN_ADDR;
    pxna: PXNADDR;
    pxnkid: PXNKID
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetInAddrToXnAddr').
      // Todo: Implement
//      _(ina, 'ina').
      _(pxna, 'pxna').
      _(pxnkid, 'pxnkid').
    LogEnd();

  Unimplemented('EmuXNetInAddrToXnAddr');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXNetQosListen
(
    pxnkid: PXNKID;
    pb: PBYTE;
    cb: UINT;
    dwBitsPerSec: DWORD;
    dwFlags: DWORD
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetQosListen').
      _(pxnkid, 'pxnkid').
      _(pb, 'pb').
      _(cb, 'cb').
      _(dwBitsPerSec, 'dwBitsPerSec').
      _(dwFlags, 'dwFlags').
    LogEnd();

  Unimplemented('EmuXNetQosListen');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

// Todo: Implement
(*function XTL_EmuXNetQosLookup(
    UINT cxna,
    XNADDR * apxna[],
    XNKID * apxnkid[],
    XNKEY * apxnkey[],
    UINT cina,
    IN_ADDR aina[],
    DWORD adwServiceId[],
    UINT cProbes,
    DWORD dwBitsPerSec,
    DWORD dwFlags,
    WSAEVENT hEvent,
    XNQOS * * ppxnqos
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin

end; *)

function XTL_EmuXNetQosRelease
(
    pxnqos: PXNQOS
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetQosRelease').
      _(pxnqos, 'pxnqos').
    LogEnd();

  Unimplemented('EmuXNetQosRelease');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXNetServerToInAddr
(
    ina: IN_ADDR;
    dwServiceId: DWORD;
    pina: PIN_ADDR
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetServerToInAddr').
       // Todo: Implement logging
//      _(ina, 'ina').
      _(dwServiceId, 'dwServiceId').
      _(pina, 'pina').
    LogEnd();

  Unimplemented('EmuXNetServerToInAddr');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXNetUnregisterInAddr
(
    ina: IN_ADDR
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetUnregisterInAddr').
    // Todo: Log ina
   // _(ina, 'ina').
    LogEnd();

  Unimplemented('EmuXNetUnregisterInAddr');

  EmuSwapFS(fsXbox);

  Result := 0;
end;

function XTL_EmuXNetXnAddrToInAddr
(
    pxna: PXNADDR;
    pxnkid: PXNKID;
    pina: PIN_ADDR
): INT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetXnAddrToInAddr').
      _(pxna, 'pxna').
      _(pxnkid, 'pxnkid').
      _(pina, 'pina').
    LogEnd();

  Unimplemented('EmuXNetXnAddrToInAddr');

  EmuSwapFS(fsXbox);

  Result := 0;
end;

function XTL_EmuXNetStartup
(
    {const} pDummy: PVOID
): INT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetStartup').
      _(pDummy, 'pDummy').
    LogEnd();

  EmuSwapFS(fsXbox);

  // Cxbx : Fake Successfull...hehehe...sucker...hehehehehe

  Result := 0;
end;

function XTL_EmuXNetGetEthernetLinkStatus(): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXNetGetEthernetLinkStatus').
    LogEnd();

  EmuSwapFS(fsXbox);

  // Cxbx : for now, no ethernet connection is available
  Result := 0;
end;

(*
SOCKET XTL.EmuThis.Emusocket
(
    af: int;
    ctype: int;
    protocol: int
)
--DXBX:UNUSED_CODE Branch:martin  UNUSED_Revision:39  Translator:PatrickvL  Done:0
begin
    EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuThis.Emusocket').
      _(this, 'this').
      _(af, 'af').
      _(ctype, 'ctype').
      _(protocol, 'protocol').
    LogEnd();

    SOCKET ret = socket(af, ctype, protocol);

    EmuSwapFS(fsXbox);

    Result := ret;
end;

function XTL.EmuThis.Emubind(s: SOCKET; var sockaddrFARname: struct; namelen: Integer): Integer;
--DXBX:UNUSED_CODE Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
    EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuThis.Emubind').
      _(this, 'this').
      _(s, 's').
      _(name, 'name').
      _(namelen, 'namelen').
    LogEnd();

    // TODO -oCXBX:: Host-To-Network order if necessary (probably not?)

    Integer ret := bind(s, name, namelen);

    EmuSwapFS(fsXbox);

    Result := ret;
end;

function XTL.EmuThis.Emulisten(s: SOCKET; backlog: Integer): Integer;
--DXBX:UNUSED_CODE Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
    EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuThis.Emulisten').
      _(this, 'this').
      _(s, 's').
      _(listen, 'listen').
    LogEnd();

    // TODO -oCXBX: Host-To-Network order if necessary (probably not?)

    Integer ret := listen(s, backlog);

    EmuSwapFS(fsXbox);

    Result := ret;
end;

function XTL.EmuThis.Emuioctlsocket(s: SOCKET; cmd: LongInt; var FARargp: u_long): Integer;
--DXBX:UNUSED_CODE Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
    EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuThis.Emuioctlsocket').
      _(this, 'this').
      _(s, 's').
      _(cmd, 'cmd').
      _(argp, 'argp').
    LogEnd();

    Integer ret := ioctlsocket(s, cmd, argp);

    EmuSwapFS(fsXbox);

    Result := ret;
end;
*)

function XTL_EmuXOnlineLaunchNewImage
(
    lpImagePath: LPCSTR;
    pLaunchData: LPVOID
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineLaunchNewImage').
      _(UIntPtr(lpImagePath), 'lpImagePath').
      _(pLaunchData, 'pLaunchData').
    LogEnd();

  // TODO -oCXBX: Launch another .xbe from Cxbx someday?

  EmuSwapFS(fsXbox);

  Result := E_FAIL;
end;

function XTL_EmuXOnlineArbitrationCreateRoundID
(
    pqwRoundID: PULONGLONG
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineArbitrationCreateRoundID').
      _(pqwRoundID, 'pqwRoundID').
    LogEnd();

  Unimplemented('EmuXOnlineArbitrationCreateRoundID');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

// Todo: Implement
function XTL_EmuXOnlineArbitrationExtendRound
(
    pArbID: PXONLINE_ARB_ID;
    wMaxSecondsFromNow: WORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineArbitrationExtendRound').
      _(pArbID, 'pArbID').
      _(wMaxSecondsFromNow, 'wMaxSecondsFromNow').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  UnImplemented('EmuXOnlineArbitrationExtendRound');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineArbitrationRegister
(
    pArbID: PXONLINE_ARB_ID;
    wMaxRoundSeconds: WORD;
    dwFlags: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineArbitrationRegister').
      _(pArbID, 'pArbID').
      _(wMaxRoundSeconds, 'wMaxRoundSeconds').
      _(dwFlags, 'dwFlags').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  UnImplemented('EmuXOnlineArbitrationRegister');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineArbitrationRegisterGetResults
(
    hTask: HANDLE; // XONLINETASK_HANDLE
    dwRegistrantsBufferCount: DWORD;
    pRegistrants: PXONLINE_ARB_REGISTRANT;
    pdwNumRegistrants: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineArbitrationRegisterGetResults').
      _(hTask, 'hTask').
      _(dwRegistrantsBufferCount, 'dwRegistrantsBufferCount').
      _(pRegistrants, 'pRegistrants').
      _(pdwNumRegistrants, 'pdwNumRegistrants').
    LogEnd();

  UnImplemented('EmuXOnlineArbitrationRegisterGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineArbitrationReport
(
    pArbID: PXONLINE_ARB_ID;
    dwNumStatProcs: DWORD;
    pStatProcs: PXONLINE_STAT_PROC;
    pReportData: PXONLINE_ARB_REPORT_DATA;
    dwFlags: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineArbitrationReport').
      _(pArbID, 'pArbID').
      _(dwNumStatProcs, 'dwNumStatProcs').
      _(pStatProcs, 'pStatProcs').
      _(pReportData, 'pReportData').
      _(dwFlags, 'dwFlags').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  UnImplemented('EmuXOnlineArbitrationReport');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineChangeLogonUsers
(
    pUsers: PXONLINE_USER;
    hWorkEvent: HANDLE;
    pHandle: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineChangeLogonUsers').
      _(pUsers, 'pUsers').
      _(hWorkEvent, 'hWorkEvent').
      _(pHandle, 'pHandle').
    LogEnd();

  UnImplemented('EmuXOnlineChangeLogonUsers');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineChangeLogonUsersTaskGetResults
(
    hLogonTask: HANDLE; //XONLINETASK_HANDLE;
    phr: PHRESULT
): HRESULT; stdcall
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineChangeLogonUsersTaskGetResults').
      _(hLogonTask, 'hLogonTask').
      _(phr, 'phr').
    LogEnd();

  UnImplemented('EmuXOnlineChangeLogonUsersTaskGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCleanup(): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCleanup').
    LogEnd();

  Unimplemented('EmuXOnlineCleanup');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionCancel
(
    dwUserIndex: DWORD;
    dwTemplate: DWORD;
    qwTeamID: ULONGLONG;
    qwCompetitionID: ULONGLONG;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE *
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionCancel').
      _(dwUserIndex, 'dwUserIndex').
      _(dwTemplate, 'dwTemplate').
      _(qwTeamID, 'qwTeamID').
      _(qwCompetitionID, 'qwCompetitionID').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionCancel');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionCheckin
(
    dwUserIndex: DWORD;
    dwTemplate: DWORD;
    qwTeamID: ULONGLONG;
    qwCompetitionID: ULONGLONG;
    qwEventID: ULONGLONG;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionCheckin').
      _(dwUserIndex, 'dwUserIndex').
      _(dwTemplate, 'dwTemplate').
      _(qwTeamID, 'qwTeamID').
      _(qwCompetitionID, 'qwCompetitionID').
      _(qwEventID, 'qwEventID').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionCheckin');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionCreate
(
    dwUserIndex: DWORD;
    dwTemplate: DWORD;
    qwTeamID: ULONGLONG;
    dwNumCompetitionAttributes: DWORD;
    pCompetitionAttributes: PXONLINE_ATTRIBUTE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionCreate').
      _(dwUserIndex, 'dwUserIndex').
      _(dwTemplate, 'dwTemplate').
      _(qwTeamID, 'qwTeamID').
      _(dwNumCompetitionAttributes, 'dwNumCompetitionAttributes').
      _(pCompetitionAttributes, 'pCompetitionAttributes').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionCreate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionCreateGetResults
(
    hTask: HANDLE; //XONLINETASK_HANDLE
    pCompResults: PXONLINE_COMP_CREATE_RESULTS
): HRESULT;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionCreateGetResults').
      _(hTask, 'hTask').
      _(pCompResults, 'pCompResults').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionCreateGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionCreateSingleElimination
(
    dwUserIndex: DWORD;
    dwTemplate: DWORD;
    qwTeamID: ULONGLONG;
    pDefaultAttributes: PXONLINE_COMP_SINGLE_ELIMINATION_ATTRIBUTES;
    dwNumAdditionalAttributes: DWORD;
    pAdditionalAttributes: PXONLINE_ATTRIBUTE;
    hWorkEvent: HANDLE;
    phTas: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionCreateSingleElimination').
      _(dwUserIndex, 'dwUserIndex').
      _(dwTemplate, 'dwTemplate').
      _(qwTeamID, 'qwTeamID').
      _(pDefaultAttributes, 'pDefaultAttributes').
      _(dwNumAdditionalAttributes, 'dwNumAdditionalAttributes').
      _(pAdditionalAttributes, 'pAdditionalAttributes').
      _(hWorkEvent, 'hWorkEvent').
      _(phTas, 'phTas').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionCreateSingleElimination');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionGetResultsBufferSize
(
    dwResultsPerPage: DWORD;
    dwNumSpecs: DWORD;
    pSpecs: PXONLINE_ATTRIBUTE_SPEC
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionGetResultsBufferSize').
      _(dwResultsPerPage, 'dwResultsPerPage').
      _(dwNumSpecs, 'dwNumSpecs').
      _(pSpecs, 'pSpecs').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionGetResultsBufferSize');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionManageEntrant
(
    dwAction: DWORD;
    dwUserIndex: DWORD;
    dwTemplate: DWORD;
    qwTeamID: ULONGLONG;
    qwCompetitionID: ULONGLONG;
    dwNumAttributes: DWORD;
    pAttributes: PXONLINE_ATTRIBUTE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionManageEntrant').
      _(dwAction, 'dwAction').
      _(dwUserIndex, 'dwUserIndex').
      _(dwTemplate, 'dwTemplate').
      _(qwTeamID, 'qwTeamID').
      _(qwCompetitionID, 'qwCompetitionID').
      _(dwNumAttributes, 'dwNumAttributes').
      _(pAttributes, 'pAttributes').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionManageEntrant');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionSearch
(
    dwQueryID: DWORD;
    dwTarget: DWORD;
    dwPage: DWORD;
    dwResultsPerPage: DWORD;
    dwNumSearchAttributes: DWORD;
    pSearchAttributes: PXONLINE_ATTRIBUTE;
    dwNumResultAttributeSpecs: DWORD;
    pResultAttributeSpecs: PXONLINE_ATTRIBUTE_SPEC;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionSearch').
      _(dwQueryID, 'dwQueryID').
      _(dwTarget, 'dwTarget').
      _(dwPage, 'dwPage').
      _(dwResultsPerPage, 'dwResultsPerPage').
      _(dwNumSearchAttributes, 'dwNumSearchAttributes').
      _(pSearchAttributes, 'pSearchAttributes').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionSearch');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionSearchGetResults
(
    hTask: HANDLE; //XONLINETASK_HANDLE
    pdwTotalItemsInSearchResult: PDWORD;
    pdwItemsReturned: PDWORD;
    pdwResultBufferSize: PDWORD;
    pbResultBuffer: PBYTE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionSearchGetResults').
      _(hTask, 'hTask').
      _(pdwTotalItemsInSearchResult, 'pdwTotalItemsInSearchResult').
      _(pdwItemsReturned, 'pdwItemsReturned').
      _(pdwResultBufferSize, 'pdwResultBufferSize').
      _(pbResultBuffer, 'pbResultBuffer').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionSearchGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionSessionRegister
(
    pArbitrationID: PXONLINE_ARB_ID;
    wMaxRoundSeconds: WORD;
    dwArbitrationRegisterFlags: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionSessionRegister').
      _(pArbitrationID, 'pArbitrationID').
      _(wMaxRoundSeconds, 'wMaxRoundSeconds').
      _(dwArbitrationRegisterFlags, 'dwArbitrationRegisterFlags').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionSessionRegister');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionSessionRegisterGetResults
(
    hTask: HANDLE; // XONLINETASK_HANDLE
    dwRegistrantsBufferCount: DWORD;
    pRegistrants: PXONLINE_ARB_REGISTRANT;
    pdwNumRegistrants: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionSessionRegisterGetResults').
      _(hTask, 'hTask').
      _(dwRegistrantsBufferCount, 'dwRegistrantsBufferCount').
      _(pRegistrants, 'pRegistrants').
      _(pdwNumRegistrants, 'pdwNumRegistrants').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionSessionRegisterGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionSubmitResults
(
    dwTemplate: DWORD;
    qwCompetitionID: ULONGLONG;
    pArbitrationID: PXONLINE_ARB_ID;
    dwArbitrationReportFlags: DWORD;
    pArbitrationReportData: PXONLINE_ARB_REPORT_DATA;
    dwNumStatProcs: DWORD;
    pStatProcs: PXONLINE_STAT_PROC;
    dwNumAttributes: DWORD;
    pAttributes: PXONLINE_ATTRIBUTE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionSubmitResults').
      _(dwTemplate, 'dwTemplate').
      _(qwCompetitionID, 'qwCompetitionID').
      _(pArbitrationID, 'pArbitrationID').
      _(dwArbitrationReportFlags, 'dwArbitrationReportFlags').
      _(dwNumStatProcs, 'dwNumStatProcs').
      _(pStatProcs, 'pStatProcs').
      _(dwNumAttributes, 'dwNumAttributes').
      _(pAttributes, 'pAttributes').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionSubmitResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionTopology
(
    dwTemplate: DWORD;
    qwCompetitionID: ULONGLONG;
    dwPage: DWORD;
    dwResultsPerPage: DWORD;
    dwStartingEventTopologyID: DWORD;
    dwEndingEventTopologyID: DWORD;
    dwNumResultAttributeSpecs: DWORD;
    pResultAttributeSpecs: PXONLINE_ATTRIBUTE_SPEC;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionTopology').
      _(dwTemplate, 'dwTemplate').
      _(qwCompetitionID, 'qwCompetitionID').
      _(dwPage, 'dwPage').
      _(dwResultsPerPage, 'dwResultsPerPage').
      _(dwStartingEventTopologyID, 'dwStartingEventTopologyID').
      _(dwEndingEventTopologyID, 'dwEndingEventTopologyID').
      _(dwNumResultAttributeSpecs, 'dwNumResultAttributeSpecs').
      _(pResultAttributeSpecs, 'pResultAttributeSpecs').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionTopology');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionTopologyGetResults
(
    hTask: HANDLE; //XONLINETASK_HANDLE;
    pdwTotalItemsInSearchResult: PDWORD;
    pdwItemsReturned: PDWORD;
    pdwResultBufferSize: PDWORD;
    pbResultBuffer: PBYTE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionTopologyGetResults').
      _(hTask, 'hTask').
      _(pdwTotalItemsInSearchResult, 'pdwTotalItemsInSearchResult').
      _(pdwItemsReturned, 'pdwItemsReturned').
      _(pdwResultBufferSize, 'pdwResultBufferSize').
      _(pbResultBuffer, 'pbResultBuffer').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionTopologyGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionTopologySingleElimination
(
    dwTemplate: DWORD;
    qwCompetitionID: ULONGLONG;
    dwOriginEventTopologyID: DWORD;
    dwRoundsForward: DWORD;
    dwRoundsBackward: DWORD;
    dwTopWidth: DWORD;
    dwNumResultAttributeSpecs: DWORD;
    pResultAttributeSpecs: PXONLINE_ATTRIBUTE_SPEC;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionTopologySingleElimination').
      _(dwTemplate, 'dwTemplate').
      _(qwCompetitionID, 'qwCompetitionID').
      _(dwOriginEventTopologyID, 'dwOriginEventTopologyID').
      _(dwRoundsForward, 'dwRoundsForward').
      _(dwRoundsBackward, 'dwRoundsBackward').
      _(dwTopWidth, 'dwTopWidth').
      _(dwNumResultAttributeSpecs, 'dwNumResultAttributeSpecs').
      _(pResultAttributeSpecs, 'pResultAttributeSpecs').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionTopologySingleElimination');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineCompetitionTopologySingleEliminationGetResults
(
    hTask: HANDLE; //XONLINETASK_HANDLE
    ppTopologyResults: PPXONLINE_COMP_TOPOLOGY_SE_RESULTS
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineCompetitionTopologySingleEliminationGetResults').
      _(hTask, 'hTask').
      _(ppTopologyResults, 'ppTopologyResults').
    LogEnd();

  UnImplemented('EmuXOnlineCompetitionTopologySingleEliminationGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

// Todo: Implement XOFFERING_ID
(*function XTL_EmuXOnlineContentInstall
(
    OfferingId: XOFFERING_ID;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineContentInstall').
      _(OfferingId, 'OfferingId').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  UnImplemented('EmuXOnlineContentInstall');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;  *)

function XTL_EmuXOnlineContentInstallGetProgress
(
    hTask: HANDLE; //XONLINETASK_HANDLE
    pdwPercentDone: PDWORD;
    pqwNumerator: PULONGLONG;
    pqwDenominator: PULONGLONG
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineContentInstallGetProgress').
      _(hTask, 'hTask').
      _(pdwPercentDone, 'pdwPercentDone').
      _(pqwNumerator, 'pqwNumerator').
      _(pqwDenominator, 'pqwDenominator').
    LogEnd();

  UnImplemented('EmuXOnlineContentInstallGetProgress');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineContentInstallGetSize
(
    hTask: HANDLE; // XONLINETASK_HANDLE
    pdwTotalInstalledSizeInBlocks: PDWORD;
    pdwAdditionalBlocksRequired: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineContentInstallGetSize').
      _(hTask, 'hTask').
      _(pdwTotalInstalledSizeInBlocks, 'pdwTotalInstalledSizeInBlocks').
      _(pdwAdditionalBlocksRequired, 'pdwAdditionalBlocksRequired').
    LogEnd();

  UnImplemented('EmuXOnlineContentInstallGetSize');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineContentSetSecurityKey
(
    pbSecretKey: PByte
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineContentSetSecurityKey').
      _(pbSecretKey, 'pbSecretKey').
    LogEnd();

  Unimplemented('EmuXOnlineContentSetSecurityKey');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTl_EmuXOnlineFeedbackSend
(
    dwUserIndex: DWORD;
    xTargetUser: XUID;
    FeedbackType: XONLINE_FEEDBACK_TYPE;
    pParams: PXONLINE_FEEDBACK_PARAMS;
    hWorkEvent: HANDLE;
    phTask: HANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFeedbackSend').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: Implement logging
//      _(xTargetUser, 'xTargetUser').
//      _(FeedbackType, 'FeedbackType').
      _(pParams, 'pParams').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineFeedbackSend');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsAnswerGameInvite
(
    dwUserIndex: DWORD;
    pToFriend: PXONLINE_FRIEND;
    Answer: XONLINE_GAMEINVITE_ANSWER_TYPE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsAnswerGameInvite').
      _(dwUserIndex, 'dwUserIndex').
      _(pToFriend, 'pToFriend').
      // Todo: needs to be converted
//      _(Answer, 'Answer').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsAnswerGameInvite');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsAnswerRequest
(
    dwUserIndex: DWORD;
    pToFriend: PXONLINE_FRIEND;
    Answer: XONLINE_REQUEST_ANSWER_TYPE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsAnswerRequest').
      _(dwUserIndex, 'dwUserIndex').
      _(pToFriend, 'pToFriend').
      // Todo: needs to be converted
 //     _(Answer, 'Answer').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsAnswerRequest');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsEnumerate
(
    dwUserIndex: DWORD;
    hWorkEvent: HANDLE;
    phTask: HANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsEnumerate').
      _(dwUserIndex, 'dwUserIndex').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsEnumerate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsEnumerateFinish
(
    hTask: HANDLE // XONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsEnumerateFinish').
      _(hTask, 'hTask').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsEnumerateFinish');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsGameInvite
(
    dwUserIndex: DWORD;
    SessionID: XNKID;
    dwFriendListCount: DWORD;
    pToFriendList: PXONLINE_FRIEND
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsGameInvite').
      _(dwUserIndex, 'dwUserIndex').
       // Todo: needs to be converted
//      _(SessionID, 'SessionID').
      _(dwFriendListCount, 'dwFriendListCount').
      _(pToFriendList, 'pToFriendList').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsGameInvite');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsGetAcceptedGameInvite
(
    pAcceptedGameInvite: PXONLINE_ACCEPTED_GAMEINVITE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsGetAcceptedGameInvite').
      _(pAcceptedGameInvite, 'pAcceptedGameInvite').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsGetAcceptedGameInvite');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsGetLatest
(
    dwUserIndex: DWORD;
    dwFriendBufferCount: DWORD;
    pFriendBuffer: PXONLINE_FRIEND
): DWORD; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsGetLatest').
      _(dwUserIndex, 'dwUserIndex').
      _(dwFriendBufferCount, 'dwFriendBufferCount').
      _(pFriendBuffer, 'pFriendBuffer').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsGetLatest');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsGetLatestByFocus
(
    dwUserIndex: DWORD;
    xuidFriendFocus: XUID;
    dwBeforeFocus: DWORD;
    pdwFriendBuffer: PDWORD;
    pFriendBuffer: PXONLINE_FRIEND;
    pdwFriendsBefore: PDWORD;
    pdwFriendsAfter: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsGetLatestByFocus').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: needs to be converted
//      _(xuidFriendFocus, 'xuidFriendFocus').
      _(dwBeforeFocus, 'dwBeforeFocus').
      _(pdwFriendBuffer, 'pdwFriendBuffer').
      _(pFriendBuffer, 'pFriendBuffer').
      _(pdwFriendsBefore, 'pdwFriendsBefore').
      _(pdwFriendsAfter, 'pdwFriendsAfter').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsGetLatestByFocus');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsGetLatestByRange
(
    dwUserIndex: DWORD;
    dwRangeStart: DWORD;
    pdwFriendBuffer: PDWORD;
    pFriendBuffer: PXONLINE_FRIEND;
    pdwFriendsBefore: PDWORD;
    pdwFriendsAfter: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsGetLatestByRange').
      _(dwUserIndex, 'dwUserIndex').
      _(dwRangeStart, 'dwRangeStart').
      _(pdwFriendBuffer, 'pdwFriendBuffer').
      _(pFriendBuffer, 'pFriendBuffer').
      _(pdwFriendsBefore, 'pdwFriendsBefore').
      _(pdwFriendsAfter, 'pdwFriendsAfter').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsGetLatestByRange');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsGetTitleName
(
    dwTitleId: DWORD;
    dwLanguage: DWORD;
    dwMaxTitleNameChars: DWORD;
    lpTitleName: LPWSTR
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsGetTitleName').
      _(dwTitleId, 'dwTitleId').
      _(dwLanguage, 'dwLanguage').
      _(dwMaxTitleNameChars, 'dwMaxTitleNameChars').
      _(lpTitleName, 'lpTitleName').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsGetTitleName');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsJoinGame
(
    dwUserIndex: DWORD;
    pToFriend: PXONLINE_FRIEND
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsJoinGame').
      _(dwUserIndex, 'dwUserIndex').
      _(pToFriend, 'pToFriend').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsJoinGame');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsRemove
(
    dwUserIndex: DWORD;
    pFriend: PXONLINE_FRIEND
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsRemove').
      _(dwUserIndex, 'dwUserIndex').
      _(pFriend, 'pFriend').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsRemove');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsRequest
(
    dwUserIndex: DWORD;
    xuidToUser: XUID
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsRequest').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: Needs to be converted
//      _(xuidToUser, 'xuidToUser').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsRequest');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsRequestByName
(
    dwUserIndex: DWORD;
    lpUserName: LPCSTR;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsRequestByName').
      _(dwUserIndex, 'dwUserIndex').
      _(lpUserName, 'lpUserName').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsRequestByName');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsRequestByNameEx
(
    dwUserIndex: DWORD;
    lpUserName: LPCSTR;
    hMsg: HANDLE; //XONLINE_MSG_HANDLE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsRequestByNameEx').
      _(dwUserIndex, 'dwUserIndex').
      _(lpUserName, 'lpUserName').
      _(hMsg, 'hMsg').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsRequestByNameEx');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsRequestEx
(
    dwUserIndex: DWORD;
    xuidToUser: XUID;
    hMsg: HANDLE; //XONLINE_MSG_HANDLE
    hWorkEvent: HANDLE;
    phTask: PHANDLE// PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsRequestEx').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: implement
//      _(xuidToUser, 'xuidToUser').
      _(hMsg, 'hMsg').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsRequestEx');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsRevokeGameInvite
(
    dwUserIndex: DWORD;
    SessionID: XNKID;
    dwFriendListCount: DWORD;
    pToFriendList: PXONLINE_FRIEND
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsRevokeGameInvite').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: Implement
//      _(SessionID, 'SessionID').
      _(dwFriendListCount, 'dwFriendListCount').
      _(pToFriendList, 'pToFriendList').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsRevokeGameInvite');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineFriendsStartup
(
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineFriendsStartup').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineFriendsStartup');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineGameDataUpload
(
    szRelativeURL: LPCSTR;
    dwFlags: DWORD;
    dwTimeout: DWORD;
    pbUsageData: PBYTE;
    dwUsageDataSize: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineGameDataUpload').
      _(szRelativeURL, 'szRelativeURL').
      _(dwFlags, 'dwFlags').
      _(dwTimeout, 'dwTimeout').
      _(pbUsageData, 'pbUsageData').
      _(dwUsageDataSize, 'dwUsageDataSize').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineGameDataUpload');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineGameInviteAnswer
(
    dwUserIndex: DWORD;
    pGameInviteAnswerInfo: PXONLINE_GAMEINVITE_ANSWER_INFO;
    GameInviteAnswer: XONLINE_PEER_ANSWER_TYPE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineGameInviteAnswer').
      _(dwUserIndex, 'dwUserIndex').
      _(pGameInviteAnswerInfo, 'pGameInviteAnswerInfo').
      // Todo: Implement
//      _(GameInviteAnswer, 'GameInviteAnswer').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineGameInviteAnswer');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineGameInviteGetLatestAccepted
(
    pLatestAcceptedGameInvite: PXONLINE_LATEST_ACCEPTED_GAMEINVITE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineGameInviteGetLatestAccepted').
      _(pLatestAcceptedGameInvite, 'pLatestAcceptedGameInvite').
    LogEnd();

  Unimplemented('EmuXOnlineGameInviteGetLatestAccepted');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineGameInviteRevoke
(
    dwUserIndex: DWORD;
    dwPeerCount: DWORD;
    pxuidPeersToRevoke: PXUID;
    SessionID: XNKID;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineGameInviteRevoke').
      _(dwUserIndex, 'dwUserIndex').
      _(dwPeerCount, 'dwPeerCount').
      _(pxuidPeersToRevoke, 'pxuidPeersToRevoke').
      // Todo: Implement
//      _(SessionID, 'SessionID').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineGameInviteRevoke');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineGameInviteSend
(
    dwUserIndex: DWORD;
    dwPeerCount: DWORD;
    pxuidPeersToInvite: PXUID;
    SessionID: XNKID;
    dwFlags: DWORD;
    hMsg: HANDLE; //XONLINE_MSG_HANDLE
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineGameInviteSend').
      _(dwUserIndex, 'dwUserIndex').
      _(dwPeerCount, 'dwPeerCount').
      _(pxuidPeersToInvite, 'pxuidPeersToInvite').
      // Todo: implement
//      _(SessionID, 'SessionID').
      _(dwFlags, 'dwFlags').
      _(hMsg, 'hMsg').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineGameInviteSend');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineGameJoin
(
    dwUserIndex: DWORD;
    pGameJoinInfo: PXONLINE_GAME_JOIN_INFO
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineGameJoin').
      _(dwUserIndex, 'dwUserIndex').
      _(pGameJoinInfo, 'pGameJoinInfo').
    LogEnd();

  Unimplemented('EmuXOnlineGameJoin');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineGetLogonUsers(): PXONLINE_USER; stdcall
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineGetLogonUsers').
    LogEnd();

  Unimplemented('EmuXOnlineGetLogonUsers');
  Result := Nil;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineGetNatType(): XONLINE_NAT_TYPE; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineGetNatType').
    LogEnd();

  Unimplemented('EmuXOnlineGetNatType');
  // Todo: get real XONLINE_NAT_TYPE type.
  Result := XONLINE_NAT_STRICT;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineGetNotification
(
    dwUserIndex: DWORD;
    NotificationType: XONLINE_NOTIFICATION_TYPE
): BOOL; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineGetNotification').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: Implement
//      _(NotificationType, 'NotificationType').
    LogEnd();

  Unimplemented('EmuXOnlineGetNotification');
  Result := BOOL_FALSE;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineGetNotificationEx
(
    dwUserIndex: DWORD;
    pNotificationInfo: PXONLINE_NOTIFICATION_EX_INFO;
    pdwStateFlags: PDWORD
): BOOL; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineGetNotificationEx').
      _(dwUserIndex, 'dwUserIndex').
      _(pNotificationInfo, 'pNotificationInfo').
      _(pdwStateFlags, 'pdwStateFlags').
    LogEnd();

  Unimplemented('EmuXOnlineGetNotificationEx');
  Result := BOOL_FALSE;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineGetServiceInfo
(
    dwServiceID: DWORD;
    pServiceInfo: PXONLINE_SERVICE_INFO
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineGetServiceInfo').
      _(dwServiceID, 'dwServiceID').
      _(pServiceInfo, 'pServiceInfo').
    LogEnd();

  Unimplemented('EmuXOnlineGetServiceInfo');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineGetUsers
(
    pUsers: PXONLINE_USER;
    pdwUsers: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineGetUsers').
      _(pUsers, 'pUsers').
      _(pdwUsers, 'pdwUsers').
    LogEnd();

  Unimplemented('EmuXOnlineGetUsers');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineLogon
(
    pUsers: PVOID;
    pdwServiceIDs: PDWORD;
    dwServices: DWORD;
    hEvent: HANDLE;
    pHandle: HANDLE
): HRESULT; stdcall;
begin
	EmuSwapFS(fsWindows);	// Win2k/XP FS
  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineLogon').
      _(pUsers, 'pUsers').
      _(pdwServiceIDs, 'pdwServiceIDs').
      _(dwServices, 'dwServices').
      _(hEvent, 'hEvent').
      _(pHandle, 'pHandle').
    LogEnd();

	// TODO: What will it take to log on to Xbox Live?

	EmuSwapFS(fsXbox);	// Xbox FS

	Result := HResult($80151000);	// XONLINE_E_LOGON_NO_NETWORK_CONNECTION
end;

function XTL_EmuXOnlineLogonTaskGetResults
(
    hLogonTask: HANDLE //XONLINETASK_HANDLE
): HRESULT; stdcall
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineLogonTaskGetResults').
      _(hLogonTask, 'hLogonTask').
    LogEnd();

  Unimplemented('EmuXOnlineLogonTaskGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMatchSearch
(
    dwProcedureIndex: DWORD;
    dwNumResults: DWORD;
    dwNumAttributes: DWORD;
    pAttributes: PXONLINE_ATTRIBUTE;
    dwResultsLen: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMatchSearch').
      _(dwProcedureIndex, 'dwProcedureIndex').
      _(dwNumResults, 'dwNumResults').
      _(dwNumAttributes, 'dwNumAttributes').
      _(pAttributes, 'pAttributes').
      _(dwResultsLen, 'dwResultsLen').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineMatchSearch');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMatchSearchGetResults
(
    hTask: HANDLE; //XONLINETASK_HANDLE
    prgpSearchResults: PPXONLINE_MATCH_SEARCHRESULT;
    pdwReturnedResults: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMatchSearchGetResults').
      _(hTask, 'hTask').
      _(prgpSearchResults, 'prgpSearchResults').
      _(pdwReturnedResults, 'pdwReturnedResults').
    LogEnd();

  Unimplemented('EmuXOnlineMatchSearchGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMatchSearchParse
(
    pSearchResult: PXONLINE_MATCH_SEARCHRESULT;
    dwNumSessionAttributes: DWORD;
    pSessionAttributeSpec: PXONLINE_ATTRIBUTE_SPEC;
    pQuerySession: PVOID
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMatchSearchParse').
      _(pSearchResult, 'pSearchResult').
      _(dwNumSessionAttributes, 'dwNumSessionAttributes').
      _(pSessionAttributeSpec, 'pSessionAttributeSpec').
      _(pQuerySession, 'pQuerySession').
    LogEnd();

  Unimplemented('EmuXOnlineMatchSearchParse');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMatchSearchResultsLen
(
    dwNumResults: DWORD;
    dwNumSessionAttributes: DWORD;
    pSessionAttributeSpec: PXONLINE_ATTRIBUTE_SPEC
): DWORD; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMatchSearchResultsLen').
      _(dwNumResults, 'dwNumResults').
      _(dwNumSessionAttributes, 'dwNumSessionAttributes').
      _(pSessionAttributeSpec, 'pSessionAttributeSpec').
    LogEnd();

  Unimplemented('EmuXOnlineMatchSearchResultsLen');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMatchSessionCreate
(
    dwPublicFilled: DWORD;
    dwPublicOpen: DWORD;
    dwPrivateFilled: DWORD;
    dwPrivateOpen: DWORD;
    dwNumAttributes: DWORD;
    pAttributes: PXONLINE_ATTRIBUTE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMatchSessionCreate').
      _(dwPublicFilled, 'dwPublicFilled').
      _(dwPublicOpen, 'dwPublicOpen').
      _(dwPrivateFilled, 'dwPrivateFilled').
      _(dwPrivateOpen, 'dwPrivateOpen').
      _(dwNumAttributes, 'dwNumAttributes').
      _(pAttributes, 'pAttributes').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineMatchSessionCreate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMatchSessionDelete
(
    SessionID: XNKID;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMatchSessionDelete').
      // Todo: Implement
//      _(SessionID, 'SessionID').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineMatchSessionDelete');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMatchSessionFindFromID
(
    SessionID: XNKID;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMatchSessionFindFromID').
      // Todo: Implement
//      _(SessionID, 'SessionID').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineMatchSessionFindFromID');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMatchSessionGetInfo
(
    hTask: HANDLE; //XONLINETASK_HANDLE;
    pSessionID: PXNKID;
    pKeyExchangeKey: PXNKEY
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMatchSessionGetInfo').
      _(hTask, 'hTask').
      _(pSessionID, 'pSessionID').
      _(pKeyExchangeKey, 'pKeyExchangeKey').
    LogEnd();

  Unimplemented('EmuXOnlineMatchSessionGetInfo');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMatchSessionUpdate
(
    SessionID: XNKID;
    dwPublicFilled: DWORD;
    dwPublicOpen: DWORD;
    dwPrivateFilled: DWORD;
    dwPrivateOpen: DWORD;
    dwNumAttributes: DWORD;
    pAttributes: PXONLINE_ATTRIBUTE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE  // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMatchSessionUpdate').
      // Todo: Implement
//      _(SessionID, 'SessionID').
      _(dwPublicFilled, 'dwPublicFilled').
      _(dwPublicOpen, 'dwPublicOpen').
      _(dwPrivateFilled, 'dwPrivateFilled').
      _(dwPrivateOpen, 'dwPrivateOpen').
      _(dwNumAttributes, 'dwNumAttributes').
      _(pAttributes, 'pAttributes').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineMatchSessionUpdate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageCreate
(
    bMsgType: BYTE;
    wNumProperties: WORD;
    wExpectedValuesSize: WORD;
    qwMessageContext: ULONGLONG;
    dwMessageFlags: DWORD;
    wExpireMinutes: WORD;
    phMsg: PHANDLE // XONLINE_MSG_HANDLE *
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageCreate').
      _(bMsgType, 'bMsgType').
      _(wNumProperties, 'wNumProperties').
      _(wExpectedValuesSize, 'wExpectedValuesSize').
      _(qwMessageContext, 'qwMessageContext').
      _(dwMessageFlags, 'dwMessageFlags').
      _(wExpireMinutes, 'wExpireMinutes').
      _(phMsg, 'phMsg').
    LogEnd();

  Unimplemented('EmuXOnlineMessageCreate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageDelete
(
    dwUserIndex: DWORD;
    dwMessageID: DWORD;
    fBlockSender: BOOL
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageDelete').
      _(dwUserIndex, 'dwUserIndex').
      _(dwMessageID, 'dwMessageID').
      _(fBlockSender, 'fBlockSender').
    LogEnd();

  Unimplemented('EmuXOnlineMessageDelete');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageDestroy
(
    hMsg: HANDLE //XONLINE_MSG_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageDestroy').
      _(hMsg, 'hMsg').
    LogEnd();

  Unimplemented('EmuXOnlineMessageDestroy');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageDetails
(
    dwUserIndex: DWORD;
    dwMessageID: DWORD;
    dwMessageFlagsToSet: DWORD;
    dwMessageFlagsToClear: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageDetails').
      _(dwUserIndex, 'dwUserIndex').
      _(dwMessageID, 'dwMessageID').
      _(dwMessageFlagsToSet, 'dwMessageFlagsToSet').
      _(dwMessageFlagsToClear, 'dwMessageFlagsToClear').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineMessageDetails');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageDetailsGetResultsProperty
(
    hTask: HANDLE; //XONLINETASK_HANDLE
    wPropTag: WORD;
    dwPropValueBufferSize: DWORD;
    pPropValue: PVOID;
    pdwPropValueSize: PDWORD;
    pdwAttachmentFlags: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageDetailsGetResultsProperty').
      _(hTask, 'hTask').
      _(wPropTag, 'wPropTag').
      _(dwPropValueBufferSize, 'dwPropValueBufferSize').
      _(pPropValue, 'pPropValue').
      _(pdwPropValueSize, 'pdwPropValueSize').
      _(pdwAttachmentFlags, 'pdwAttachmentFlags').
    LogEnd();

  Unimplemented('EmuXOnlineMessageDetailsGetResultsProperty');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageDetailsGetResultsSummary
(
    hTask: HANDLE;//XONLINETASK_HANDLE;
    pMsgSummary: PXONLINE_MSG_SUMMARY;
    pdwNumProperties: PDWORD;
    pqwAttachmentsSize: PULONGLONG
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageDetailsGetResultsSummary').
      _(hTask, 'hTask').
      _(pMsgSummary, 'pMsgSummary').
      _(pdwNumProperties, 'pdwNumProperties').
      _(pqwAttachmentsSize, 'pqwAttachmentsSize').
    LogEnd();

  Unimplemented('EmuXOnlineMessageDetailsGetResultsSummary');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageDownloadAttachmentGetProgress
(
    hDownloadTask: HANDLE;//XONLINETASK_HANDLE;
    pdwPercentDone: PDWORD;
    pqwNumerator: PULONGLONG;
    pqwDenominator: PLONGLONG
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageDownloadAttachmentGetProgress').
      _(hDownloadTask, 'hDownloadTask').
      _(pdwPercentDone, 'pdwPercentDone').
      _(pqwNumerator, 'pqwNumerator').
      _(pqwDenominator, 'pqwDenominator').
    LogEnd();

  Unimplemented('EmuXOnlineMessageDownloadAttachmentGetProgress');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageDownloadAttachmentToDirectory
(
    hDetailsTask: HANDLE; //XONLINETASK_HANDLE
    wPropTag: WORD;
    lpLocalPath: LPCSTR;
    hWorkEvent: HANDLE;
    phDownloadTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageDownloadAttachmentToDirectory').
      _(hDetailsTask, 'hDetailsTask').
      _(wPropTag, 'wPropTag').
      _(lpLocalPath, 'lpLocalPath').
      _(hWorkEvent, 'hWorkEvent').
      _(phDownloadTask, 'phDownloadTask').
    LogEnd();

  Unimplemented('EmuXOnlineMessageDownloadAttachmentToDirectory');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageDownloadAttachmentToMemory
(
    hDetailsTask: HANDLE; //XONLINETASK_HANDLE
    wPropTag: WORD;
    pbBuffer: PBYTE;
    dwBufferSize: DWORD;
    hWorkEvent: HANDLE;
    phDownloadTask: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageDownloadAttachmentToMemory').
      _(hDetailsTask, 'hDetailsTask').
      _(pbBuffer, 'pbBuffer').
      _(dwBufferSize, 'dwBufferSize').
      _(hWorkEvent, 'hWorkEvent').
      _(phDownloadTask, 'phDownloadTask').
    LogEnd();

  Unimplemented('EmuXOnlineMessageDownloadAttachmentToMemory');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageDownloadAttachmentToMemoryGetResults
(
    hDownloadTask: HANDLE; //XONLINETASK_HANDLE
    ppbReceivedData: PPBYTE;
    pdwReceivedDataSize: PDWORD;
    pdwTotalDataSize: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageDownloadAttachmentToMemoryGetResults').
      _(hDownloadTask, 'hDownloadTask').
      _(ppbReceivedData, 'ppbReceivedData').
      _(pdwReceivedDataSize, 'pdwReceivedDataSize').
      _(pdwTotalDataSize, 'pdwTotalDataSize').
    LogEnd();

  Unimplemented('EmuXOnlineMessageDownloadAttachmentToMemoryGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageEnableReceivingFamilyTitleIDs
(
    dwNumTitleIDs: DWORD;
    pdwTitleIDs: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageEnableReceivingFamilyTitleIDs').
      _(dwNumTitleIDs, 'dwNumTitleIDs').
      _(pdwTitleIDs, 'pdwTitleIDs').
    LogEnd();

  Unimplemented('EmuXOnlineMessageEnableReceivingFamilyTitleIDs');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageEnumerate
(
    dwUserIndex: DWORD;
    pMsgSummaries: PXONLINE_MSG_SUMMARY;
    pdwNumMsgSummaries: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageEnumerate').
      _(dwUserIndex, 'dwUserIndex').
      _(pMsgSummaries, 'pMsgSummaries').
      _(pdwNumMsgSummaries, 'pdwNumMsgSummaries').
    LogEnd();

  Unimplemented('EmuXOnlineMessageEnumerate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageRevoke
(
    dwUserIndex: DWORD;
    dwNumMsgSendResults: DWORD;
    pMsgSendResults: PXONLINE_MSG_SEND_RESULT;
    hWorkEvent: HANDLE;
    phTask: HANDLE // ONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageRevoke').
      _(dwUserIndex, 'dwUserIndex').
      _(dwNumMsgSendResults, 'dwNumMsgSendResults').
      _(pMsgSendResults, 'pMsgSendResults').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineMessageRevoke');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageSend
(
    dwUserIndexSender: DWORD;
    hMsg: HANDLE; // XONLINE_MSG_HANDLE
    dwRecipientCount: DWORD;
    pxuidRecipients: PXUID;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageSend').
      _(dwUserIndexSender, 'dwUserIndexSender').
      _(hMsg, 'hMsg').
      _(dwRecipientCount, 'dwRecipientCount').
      _(pxuidRecipients, 'pxuidRecipients').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineMessageSend');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageSendGetProgress
(
    hTask: HANDLE; //XONLINETASK_HANDLE
    pdwPercentDone: PDWORD;
    pqwNumerator: PULONGLONG;
    pqwDenominator: PULONGLONG
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageSendGetProgress').
      _(hTask, 'hTask').
      _(pdwPercentDone, 'pdwPercentDone').
      _(pqwNumerator, 'pqwNumerator').
      _(pqwDenominator, 'pqwDenominator').
    LogEnd();

  Unimplemented('EmuXOnlineMessageSendGetProgress');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageSendGetResults
(
    hTask: HANDLE; //XONLINETASK_HANDLE
    pMsgSendResults: PXONLINE_MSG_SEND_RESULT
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageSendGetResults').
      _(hTask, 'hTask').
      _(pMsgSendResults, 'pMsgSendResults').
    LogEnd();

  Unimplemented('EmuXOnlineMessageSendGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageSetFlags
(
    dwUserIndex: DWORD;
    dwMessageID: DWORD;
    dwMessageFlagsToSet: DWORD;
    dwMessageFlagsToClear: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageSetFlags').
      _(dwUserIndex, 'dwUserIndex').
      _(dwMessageID, 'dwMessageID').
      _(dwMessageFlagsToSet, 'dwMessageFlagsToSet').
      _(dwMessageFlagsToClear, 'dwMessageFlagsToClear').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineMessageSetFlags');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageSetProperty
(
    hMsg: HANDLE;//XONLINE_MSG_HANDLE;
    wPropTag: WORD;
    dwPropValueSize: DWORD;
    pPropValue: PVOID;
    dwAttachmentFlags: DWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageSetProperty').
      _(hMsg, 'hMsg').
      _(wPropTag, 'wPropTag').
      _(dwPropValueSize, 'dwPropValueSize').
      _(pPropValue, 'pPropValue').
      _(dwAttachmentFlags, 'dwAttachmentFlags').
    LogEnd();

  Unimplemented('EmuXOnlineMessageSetProperty');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageSetSendingFamilyTitleID
(
    dwTitleID: DWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageSetSendingFamilyTitleID').
      _(dwTitleID, 'dwTitleID').
    LogEnd();

  Unimplemented('EmuXOnlineMessageSetSendingFamilyTitleID');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageSetSummaryRefresh
(
    fEnable: BOOL
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageSetSummaryRefresh').
      _(fEnable, 'fEnable').
    LogEnd();

  Unimplemented('EmuXOnlineMessageSetSummaryRefresh');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMessageSummary
(
    dwUserIndex: DWORD;
    dwMessageID: DWORD;
    pMsgSummary: PXONLINE_MSG_SUMMARY
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMessageSummary').
      _(dwUserIndex, 'dwUserIndex').
      _(dwMessageID, 'dwMessageID').
      _(pMsgSummary, 'pMsgSummary').
    LogEnd();

  Unimplemented('EmuXOnlineMessageSummary');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMutelistAdd
(
    dwUserIndex: DWORD;
    xUserID: XUID
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMutelistAdd').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: implement
//      _(xUserID, 'xUserID').
    LogEnd();

  Unimplemented('EmuXOnlineMutelistAdd');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMutelistGet
(
    dwUserIndex: DWORD;
    dwMutelistUserBufferCount: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE;//PXONLINETASK_HANDLE;
    pMutelistUsersBuffer: PXONLINE_MUTELISTUSER;
    pdwNumMustlistUsers: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMutelistGet').
      _(dwUserIndex, 'dwUserIndex').
      _(dwMutelistUserBufferCount, 'dwMutelistUserBufferCount').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
      _(pMutelistUsersBuffer, 'pMutelistUsersBuffer').
      _(pdwNumMustlistUsers, 'pdwNumMustlistUsers').
    LogEnd();

  Unimplemented('EmuXOnlineMutelistGet');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMutelistRemove
(
    dwUserIndex: DWORD;
    xUserID: XUID
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMutelistRemove').
      _(dwUserIndex, 'dwUserIndex').
      // todo: implement
//      _(xUserID, 'xUserID').
    LogEnd();

  Unimplemented('EmuXOnlineMutelistRemove');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineMutelistStartup
(
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineMutelistStartup').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineMutelistStartup');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineNotificationSetState
(
    dwUserIndex: DWORD;
    dwStateFlags: DWORD;
    sessionID: XNKID;
    dwStateData: DWORD;
    pbStateData: PBYTE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineNotificationSetState').
      _(dwUserIndex, 'dwUserIndex').
      _(dwStateFlags, 'dwStateFlags').
      // Todo: implement
//      _(sessionID, 'sessionID').
      _(dwStateData, 'dwStateData').
      _(pbStateData, 'pbStateData').
    LogEnd();

  Unimplemented('EmuXOnlineNotificationSetState');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

// Todo: Implement XOFFERING_ID
(*function XTL_EmuXOnlineOfferingCancel
(
    dwUserIndex: DWORD;
    OfferingId: XOFFERING_ID;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineOfferingCancel').
      _(dwUserIndex, 'dwUserIndex').
      _(OfferingId, 'OfferingId').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineOfferingCancel');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end; *)

// TODO: Implement XOFFERING_ID
(*function XTL_EmuXOnlineOfferingDetails
(
    dwUserIndex: DWORD;
    OfferingId: XOFFERING_ID;
    dwLanguage: DWORD;
    dwDescriptionIndex: DWORD;
    pbBuffer: PBYTE;
    dwBufferSize: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineOfferingDetails').
      _(dwUserIndex, 'dwUserIndex').
      _(OfferingId, 'OfferingId').
      _(dwLanguage, 'dwLanguage').
      _(dwDescriptionIndex, 'dwDescriptionIndex').
      _(pbBuffer, 'pbBuffer').
      _(dwBufferSize, 'dwBufferSize').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineOfferingDetails');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;   *)

function XTL_EmuXOnlineOfferingDetailsGetResults
(
    hTask: HANDLE;//XONLINETASK_HANDLE;
    pDetails: PXONLINEOFFERING_DETAILS
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineOfferingDetailsGetResults').
      _(hTask, 'hTask').
      _(pDetails, 'pDetails').
    LogEnd();

  Unimplemented('EmuXOnlineOfferingDetailsGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineOfferingDetailsMaxSize
(
    dwTitleSpecificDataMaxSize: DWORD
): DWORD; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineOfferingDetailsMaxSize').
      _(dwTitleSpecificDataMaxSize, 'dwTitleSpecificDataMaxSize').
    LogEnd();

  Unimplemented('EmuXOnlineOfferingDetailsMaxSize');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTl_EmuXOnlineOfferingEnumerate
(
    dwUserIndex: DWORD;
    pEnumParams: PXONLINEOFFERING_ENUM_PARAMS;
    pbBuffer: PBYTE;
    dwBufferSize: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineOfferingEnumerate').
      _(dwUserIndex, 'dwUserIndex').
      _(pEnumParams, 'pEnumParams').
      _(pbBuffer, 'pbBuffer').
      _(dwBufferSize, 'dwBufferSize').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineOfferingEnumerate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

// Todo: Implement PPPXONLINEOFFERING_INFO
(*function XTL_EmuXOnlineOfferingEnumerateGetResults
(
    hTask: HANDLE; // XONLINETASK_HANDLE;
    prgpOfferingInfo: PPPXONLINEOFFERING_INFO;
    pdwReturnedResults: PDWORD;
    pfMoreResults: PBOOL
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineOfferingEnumerateGetResults').
      _(hTask, 'hTask').
      _(prgpOfferingInfo, 'prgpOfferingInfo').
      _(pdwReturnedResults, 'pdwReturnedResults').
      _(pfMoreResults, 'pfMoreResults').
    LogEnd();

  Unimplemented('EmuXOnlineOfferingEnumerateGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end; *)

function XTL_EmuXOnlineOfferingEnumerateMaxSize
(
    pEnumParams: PXONLINEOFFERING_ENUM_PARAMS;
    dwTitleSpecificDataMaxSize: DWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineOfferingEnumerateMaxSize').
      _(pEnumParams, 'pEnumParams').
      _(dwTitleSpecificDataMaxSize, 'dwTitleSpecificDataMaxSize').
    LogEnd();

  Unimplemented('EmuXOnlineOfferingEnumerateMaxSize');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineOfferingIsNewContentAvailable
(
    dwBitFilter: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineOfferingIsNewContentAvailable').
      _(dwBitFilter, 'dwBitFilter').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineOfferingIsNewContentAvailable');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineOfferingPriceFormat
(
    pPrice: PXONLINE_PRICE;
    lpFormattedPrice: LPWSTR;
    pdwLength: PDWORD;
    dwExtendedCharsFilter: DWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineOfferingPriceFormat').
      _(pPrice, 'pPrice').
      _(lpFormattedPrice, 'lpFormattedPrice').
      _(pdwLength, 'pdwLength').
      _(dwExtendedCharsFilter, 'dwExtendedCharsFilter').
    LogEnd();

  Unimplemented('EmuXOnlineOfferingPriceFormat');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

//Todo: Implement XOFFERING_ID
(*function XTL_EmuXOnlineOfferingPurchase
(
    dwUserIndex: DWORD;
    OfferingId: XOFFERING_ID;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineOfferingPurchase').
      _(dwUserIndex, 'dwUserIndex').
      _(OfferingId, 'OfferingId').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineOfferingPurchase');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end; *)

function XTL_EmuXOnlinePresenceAdd
(
    hTask: HANDLE; // XONLINETASK_HANDLE;
    dwGroupID: DWORD;
    dwUserCount: DWORD;
    pxuidUsers: PXUID
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlinePresenceAdd').
      _(hTask, 'hTask').
      _(dwGroupID, 'dwGroupID').
      _(dwUserCount, 'dwUserCount').
      _(pxuidUsers, 'pxuidUsers').
    LogEnd();

  Unimplemented('EmuXOnlinePresenceAdd');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlinePresenceClear
(
    hTask: HANDLE // XONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlinePresenceClear').
      _(hTask, 'hTask').
    LogEnd();

  Unimplemented('EmuXOnlinePresenceClear');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlinePresenceGetLatest
(
    hTask: HANDLE; // XONLINETASK_HANDLE;
    dwGroupID: DWORD;
    dwUserPresenceBufferCount: DWORD;
    pUserPresence: PXONLINE_PRESENCE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlinePresenceGetLatest').
      _(hTask, 'hTask').
      _(dwGroupID, 'dwGroupID').
      _(dwUserPresenceBufferCount, 'dwUserPresenceBufferCount').
      _(pUserPresence, 'pUserPresence').
    LogEnd();

  Unimplemented('EmuXOnlinePresenceGetLatest');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlinePresenceGetTitleName
(
    hTask: HANDLE; // XONLINETASK_HANDLE;
    dwTitleID: DWORD;
    dwLanguage: DWORD;
    dwTitleNameSize: DWORD;
    wszTitleName: LPWSTR
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlinePresenceGetTitleName').
      _(hTask, 'hTask').
      _(dwTitleID, 'dwTitleID').
      _(dwLanguage, 'dwLanguage').
      _(dwTitleNameSize, 'dwTitleNameSize').
      _(wszTitleName, 'wszTitleName').
    LogEnd();

  Unimplemented('EmuXOnlinePresenceGetTitleName');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlinePresenceInit
(
    dwUserIndex: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlinePresenceInit').
      _(dwUserIndex, 'dwUserIndex').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlinePresenceInit');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlinePresenceSubmit
(
    hTask: HANDLE // XONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlinePresenceSubmit').
      _(hTask, 'hTask').
    LogEnd();

  Unimplemented('EmuXOnlinePresenceSubmit');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineQueryAdd
(
    dwUserIndex: DWORD;
    qwTeamId: ULONGLONG;
    dwDatasetId: DWORD;
    dwNumAttributes: DWORD;
    pAttributes: PXONLINE_ATTRIBUTE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineQueryAdd').
      _(dwUserIndex, 'dwUserIndex').
      _(qwTeamId, 'qwTeamId').
      _(dwDatasetId, 'dwDatasetId').
      _(dwNumAttributes, 'dwNumAttributes').
      _(pAttributes, 'pAttributes').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineQueryAdd');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

// Todo: Implement PXENTITY_ID
(*function XTL_EmuXOnlineQueryAddGetResults
(
    hTask: HANDLE; // XONLINETASK_HANDLE;
    pEntityId: PXENTITY_ID;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineQueryAddGetResults').
      _(hTask, 'hTask').
      _(pEntityId, 'pEntityId').
    LogEnd();

  Unimplemented('EmuXOnlineQueryAddGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end; *)

// Todo: Implement PXENTITY_ID
(*function XTL_EmuXOnlineQueryFindFromIds
(
    dwDatasetId: DWORD;
    dwProcIndex: DWORD;
    dwNumResultSpecs: DWORD;
    pAttributeSpecs: PXONLINE_ATTRIBUTE_SPEC;
    dwNumEntityIds: DWORD;
    pEntityIds: PXENTITY_ID;
    hWorkEvent: HANDLE;
    phTask: PXONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineQueryFindFromIds').
      _(dwDatasetId, 'dwDatasetId').
      _(dwProcIndex, 'dwProcIndex').
      _(dwNumResultSpecs, 'dwNumResultSpecs').
      _(pAttributeSpecs, 'pAttributeSpecs').
      _(dwNumEntityIds, 'dwNumEntityIds').
      _(pEntityIds, 'pEntityIds').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineQueryFindFromIds');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end; *)

function XTL_EmuXOnlineQueryFindFromIdsGetResults
(
    hTask: HANDLE; //XONLINETASK_HANDLE;
    pdwReturnedResults: PDWORD;
    pResults: PVOID
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineQueryFindFromIdsGetResults').
      _(hTask, 'hTask').
      _(pdwReturnedResults, 'pdwReturnedResults').
      _(pResults, 'pResults').
    LogEnd();

  Unimplemented('EmuXOnlineQueryFindFromIdsGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineQueryGetResultsBufferSize
(
    dwNumResults: DWORD;
    dwNumSpecs: DWORD;
    pSpecs: PXONLINE_ATTRIBUTE_SPEC
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineQueryGetResultsBufferSize').
      _(dwNumResults, 'dwNumResults').
      _(dwNumSpecs, 'dwNumSpecs').
      _(pSpecs, 'pSpecs').
    LogEnd();

  Unimplemented('EmuXOnlineQueryGetResultsBufferSize');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineQueryRemove
(
    dwUserIndex: DWORD;
    qwTeamId: ULONGLONG;
    dwDatasetId: DWORD;
    dwProcIndex: DWORD;
    dwNumAttributes: DWORD;
    pAttributes: PXONLINE_ATTRIBUTE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PPXONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineQueryRemove').
      _(dwUserIndex, 'dwUserIndex').
      _(qwTeamId, 'qwTeamId').
      _(dwDatasetId, 'dwDatasetId').
      _(dwProcIndex, 'dwProcIndex').
      _(dwNumAttributes, 'dwNumAttributes').
      _(pAttributes, 'pAttributes').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineQueryRemove');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

// Todo: Implement XENTITY_ID
(*function XTL_EmuXOnlineQueryRemoveId
(
    dwUserIndex: DWORD;
    qwTeamId: ULONGLONG;
    dwDatasetId: DWORD;
    entityId: XENTITY_ID;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineQueryRemoveId').
      _(dwUserIndex, 'dwUserIndex').
      _(qwTeamId, 'qwTeamId').
      _(dwDatasetId, 'dwDatasetId').
      _(entityId, 'entityId').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineQueryRemoveId');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end; *)

function XTL_EmuXOnlineQuerySearch
(
    dwDatasetId: DWORD;
    dwProcIndex: DWORD;
    dwPage: DWORD;
    dwResultsPerPage: DWORD;
    dwNumResultSpecs: DWORD;
    pAttributeSpecs: PXONLINE_ATTRIBUTE_SPEC;
    dwNumAttributes: DWORD;
    pAttributes: PXONLINE_ATTRIBUTE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PPXONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineQuerySearch').
      _(dwDatasetId, 'dwDatasetId').
      _(dwProcIndex, 'dwProcIndex').
      _(dwPage, 'dwPage').
      _(dwResultsPerPage, 'dwResultsPerPage').
      _(pAttributeSpecs, 'pAttributeSpecs').
      _(dwNumAttributes, 'dwNumAttributes').
      _(pAttributes, 'pAttributes').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineQuerySearch');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineQuerySearchGetResults
(
    hTask: HANDLE; //ONLINETASK_HANDLE;
    pdwTotalResults: PDWORD;
    pdwReturnedResults: PDWORD;
    pdwResultsSize: PDWORD;
    pbResults: PBYTE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineQuerySearchGetResults').
      _(hTask, 'hTask').
      _(pdwTotalResults, 'pdwTotalResults').
      _(pdwReturnedResults, 'pdwReturnedResults').
      _(pbResults, 'pbResults').
    LogEnd();

  Unimplemented('EmuXOnlineQuerySearchGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

// Todo: Implement XENTITY_ID
(*function XTL_EmuXOnlineQuerySelect
(
    dwUserIndex: DWORD;
    qwTeamId: ULONGLONG;
    dwDatasetId: DWORD;
    entityId: XENTITY_ID;
    dwAction: DWORD;
    dwNumAttributes: DWORD;
    pAttributes: PXONLINE_ATTRIBUTE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineQuerySelect').
      _(dwUserIndex, 'dwUserIndex').
      _(qwTeamId, 'qwTeamId').
      _(dwDatasetId, 'dwDatasetId').
      _(entityId, 'entityId').
      _(dwAction, 'dwAction').
      _(dwNumAttributes, 'dwNumAttributes').
      _(pAttributes, 'pAttributes').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineQuerySelect');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;  *)

function XTL_EmuXOnlineQueryUpdate
(
    dwUserIndex: DWORD;
    qwTeamId: ULONGLONG;
    dwDatasetId: DWORD;
    dwProcIndex: DWORD;
    dwNumAttributes: DWORD;
    pAttributes: PXONLINE_ATTRIBUTE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PPXONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineQueryUpdate').
      _(dwUserIndex, 'dwUserIndex').
      _(qwTeamId, 'qwTeamId').
      _(dwDatasetId, 'dwDatasetId').
      _(dwNumAttributes, 'dwNumAttributes').
      _(pAttributes, 'pAttributes').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineQueryUpdate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

// Todo: Implement XENTITY_ID
(*function XTL_EmuXOnlineQueryUpdateId
(
    dwUserIndex: DWORD;
    qwTeamId: ULONGLONG;
    dwDatasetId: DWORD;
    dwProcIndex: DWORD;
    entityId: XENTITY_ID;
    dwNumAttributes: DWORD;
    pAttributes: PXONLINE_ATTRIBUTE;
    hWorkEvent: HANDLE;
    phTask: PXONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineQueryUpdateId').
      _(dwUserIndex, 'dwUserIndex').
      _(qwTeamId, 'qwTeamId').
      _(dwDatasetId, 'dwDatasetId').
      _(dwProcIndex, 'dwProcIndex').
      _(entityId, 'entityId').
      _(dwNumAttributes, 'dwNumAttributes').
      _(pAttributes, 'pAttributes').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineQueryUpdateId');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;        *)

function XTL_EmuXOnlineRetrieveLogonState
(
    pLogonState: PXONLINE_LOGON_STATE;
    pUsers: PXONLINE_USER;
    pdwServiceIDs: PDWORD;
    pdwServices: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineRetrieveLogonState').
      _(pLogonState, 'pLogonState').
      _(pUsers, 'pUsers').
      _(pdwServiceIDs, 'pdwServiceIDs').
      _(pdwServices, 'pdwServices').
    LogEnd();

  Unimplemented('EmuXOnlineRetrieveLogonState');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineSaveLogonState
(
    pLogonState: PXONLINE_LOGON_STATE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineSaveLogonState').
      _(pLogonState, 'pLogonState').
    LogEnd();

  Unimplemented('EmuXOnlineSaveLogonState');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineSignatureVerify
(
    rgSignaturesToVerify: PXONLINE_SIGNATURE_TO_VERIFY;
    dwSignaturesToVerify: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineSignatureVerify').
      _(rgSignaturesToVerify, 'rgSignaturesToVerify').
      _(dwSignaturesToVerify, 'dwSignaturesToVerify').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineSignatureVerify');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineSignatureVerifyGetResults
(
    hTask: HANDLE; //XONLINETASK_HANDLE;
    prgHresults: PHRESULT;
    pdwHresults: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineSignatureVerifyGetResults').
      _(hTask, 'hTask').
      _(prgHresults, 'prgHresults').
      _(pdwHresults, 'pdwHresults').
    LogEnd();

  Unimplemented('EmuXOnlineSignatureVerifyGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineSilentLogon
(
    pdwServiceIDs: PDWORD;
    dwServices: DWORD;
    hWorkEvent: HANDLE;
    pHandle: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineSilentLogon').
      _(pdwServiceIDs, 'pdwServiceIDs').
      _(dwServices, 'dwServices').
      _(hWorkEvent, 'hWorkEvent').
      _(pHandle, 'pHandle').
    LogEnd();

  Unimplemented('EmuXOnlineSilentLogon');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTl_EmuXOnlineStartup
(
    pxosp: PXONLINE_STARTUP_PARAMS
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStartup').
      _(pxosp, 'pxosp').
    LogEnd();

  Unimplemented('EmuXOnlineStartup');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStatLeaderEnumerate
(
    pxuidPagePivot: PXUID;
    dwPageStart: DWORD;
    dwPageSize: DWORD;
    dwLeaderboardID: DWORD;
    dwNumStatsPerUser: DWORD;
    pStatsPerUser: PWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStatLeaderEnumerate').
      _(pxuidPagePivot, 'pxuidPagePivot').
      _(dwPageStart, 'dwPageStart').
      _(dwPageSize, 'dwPageSize').
      _(dwLeaderboardID, 'dwLeaderboardID').
      _(dwNumStatsPerUser, 'dwNumStatsPerUser').
      _(pStatsPerUser, 'pStatsPerUser').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStatLeaderEnumerate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStatLeaderEnumerateGetResults
(
    hTask: HANDLE; // XONLINETASK_HANDLE;
    dwUserCount: DWORD;
    pUsers: PXONLINE_STAT_USER;
    dwStatCount: DWORD;
    pStats: PXONLINE_STAT;
    pdwLeaderboardSize: PDWORD;
    pdwReturnedResults: PDWORD;
    dwExtraBufferSize: DWORD;
    pExtraBuffer: PBYTE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStatLeaderEnumerateGetResults').
      _(hTask, 'hTask').
      _(dwUserCount, 'dwUserCount').
      _(pUsers, 'pUsers').
      _(pStats, 'pStats').
      _(pdwLeaderboardSize, 'pdwLeaderboardSize').
      _(pdwReturnedResults, 'pdwReturnedResults').
      _(dwExtraBufferSize, 'dwExtraBufferSize').
      _(pExtraBuffer, 'pExtraBuffer').
    LogEnd();

  Unimplemented('EmuXOnlineStatLeaderEnumerateGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStatRead
(
    dwNumStatSpecs: DWORD;
    pStatSpecs: PXONLINE_STAT_SPEC;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStatRead').
      _(dwNumStatSpecs, 'dwNumStatSpecs').
      _(pStatSpecs, 'pStatSpecs').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStatRead');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStatReadGetResult
(
    hTask: HANDLE; //XONLINETASK_HANDLE;
    dwNumStatSpecs: DWORD;
    pStatSpecs: PXONLINE_STAT_SPEC;
    dwExtraBufferSize: DWORD;
    pExtraBuffer: PBYTE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStatReadGetResult').
      _(hTask, 'hTask').
      _(dwNumStatSpecs, 'dwNumStatSpecs').
      _(pStatSpecs, 'pStatSpecs').
      _(dwExtraBufferSize, 'dwExtraBufferSize').
      _(pExtraBuffer, 'pExtraBuffer').
    LogEnd();

  Unimplemented('EmuXOnlineStatReadGetResult');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStatReset
(
    xuid: XUID;
    dwLeaderBoardId: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStatReset').
      // Todo: implement
//      _(xuid, 'xuid').
      _(dwLeaderBoardId, 'dwLeaderBoardId').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStatReset');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStatUnitEnumerate
(
    xuidUnitMember: XUID;
    dwLeaderboardID: DWORD;
    SortOrder: XONLINE_STAT_SORTORDER;
    dwMaxUnitsToReturn: DWORD;
    dwNumStatsPerUnit: DWORD;
    pStatsPerUnit: PWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStatUnitEnumerate').
       // Todo: Implement
//      _(xuidUnitMember, 'xuidUnitMember').
      _(dwLeaderboardID, 'dwLeaderboardID').
      // Todo: implement
//       _(SortOrder, 'SortOrder').
      _(dwMaxUnitsToReturn, 'dwMaxUnitsToReturn').
      _(dwNumStatsPerUnit, 'dwNumStatsPerUnit').
      _(pStatsPerUnit, 'pStatsPerUnit').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStatUnitEnumerate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStatUnitEnumerateGetResults
(
    hTask: HANDLE; // XONLINETASK_HANDLE;
    pUnits: PXONLINE_STAT_UNIT;
    dwStatCount: DWORD;
    pStats: PXONLINE_STAT;
    pdwReturnedResults: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStatUnitEnumerateGetResults').
      _(hTask, 'hTask').
      _(pUnits, 'pUnits').
      _(dwStatCount, 'dwStatCount').
      _(pStats, 'pStats').
      _(pdwReturnedResults, 'pdwReturnedResults').
    LogEnd();

  Unimplemented('EmuXOnlineStatUnitEnumerateGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStatUnitRead
(
    pxuidUnitMembers: PXUID;
    dwNumStatSpecUnits: DWORD;
    pStatSpecUnits: PXONLINE_STAT_SPEC_UNIT;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStatUnitRead').
      _(pxuidUnitMembers, 'pxuidUnitMembers').
      _(dwNumStatSpecUnits, 'dwNumStatSpecUnits').
      _(pStatSpecUnits, 'pStatSpecUnits').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStatUnitRead');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStatUnitReadGetResult
(
    hTask: HANDLE; // XONLINETASK_HANDLE;
    dwNumStatSpecUnits: DWORD;
    pStatSpecUnits: PXONLINE_STAT_SPEC_UNIT
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStatUnitReadGetResult').
      _(hTask, 'hTask').
      _(dwNumStatSpecUnits, 'dwNumStatSpecUnits').
      _(pStatSpecUnits, 'pStatSpecUnits').
    LogEnd();

  Unimplemented('EmuXOnlineStatUnitReadGetResult');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStatWrite
(
    dwNumStatSpecs: DWORD;
    pStatSpecs: PXONLINE_STAT_SPEC;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStatWrite').
      _(dwNumStatSpecs, 'dwNumStatSpecs').
      _(pStatSpecs, 'pStatSpecs').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStatWrite');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStatWriteEx
(
    dwNumStatProcs: DWORD;
    pStatProcs: PXONLINE_STAT_PROC;
    hWorkEvent: HANDLE;
    phTask: PHANDLE// PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStatWriteEx').
      _(dwNumStatProcs, 'dwNumStatProcs').
      _(pStatProcs, 'pStatProcs').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStatWriteEx');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStatWriteGetResult
(
    hTask: HANDLE; // XONLINETASK_HANDLE;
    phServerFileReference: PHANDLE;
    prgReferences: PXONLINE_STAT_ATTACHMENT_REFERENCE;
    pdwReferences: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStatWriteGetResult').
      _(hTask, 'hTask').
      _(phServerFileReference, 'phServerFileReference').
      _(prgReferences, 'prgReferences').
      _(pdwReferences, 'pdwReferences').
    LogEnd();

  Unimplemented('EmuXOnlineStatWriteGetResult');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStorageCreateServerPath
(
    dwFacility: DWORD;
    qwUserID: ULONGLONG;
    qwTeamID: ULONGLONG;
    wszStorageFileName: LPCWSTR;
    wszStorageServerPath: LPWSTR;
    pcchStorageServerPath: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStorageCreateServerPath').
      _(dwFacility, 'dwFacility').
      _(qwUserID, 'qwUserID').
      _(qwTeamID, 'qwTeamID').
      _(wszStorageFileName, 'wszStorageFileName').
      _(pcchStorageServerPath, 'pcchStorageServerPath').
    LogEnd();

  Unimplemented('EmuXOnlineStorageCreateServerPath');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStorageDeleteFile
(
    dwFacility: DWORD;
    dwUserIndex: DWORD;
    wszStorageFileName: LPCWSTR;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE;
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStorageDeleteFile').
      _(dwFacility, 'dwFacility').
      _(dwUserIndex, 'dwUserIndex').
      _(wszStorageFileName, 'wszStorageFileName').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStorageDeleteFile');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStorageDownload
(
    dwFacility: DWORD;
    dwUserIndex: DWORD;
    wszStoragePath: LPCWSTR;
    szInstallDirectory: LPCSTR;
    dwDownloadFlags: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStorageDownload').
      _(dwFacility, 'dwFacility').
      _(dwUserIndex, 'dwUserIndex').
      _(wszStoragePath, 'wszStoragePath').
      _(dwDownloadFlags, 'dwDownloadFlags').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStorageDownload');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStorageDownloadToMemory
(
    dwFacility: DWORD;
    dwUserIndex: DWORD;
    wszStoragePath: LPCWSTR;
    pbReceiveBuffer: PBYTE;
    cbReceiveBuffer: DWORD;
    dwDownloadFlags: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE// PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStorageDownloadToMemory').
      _(dwFacility, 'dwFacility').
      _(dwUserIndex, 'dwUserIndex').
      _(wszStoragePath, 'wszStoragePath').
      _(pbReceiveBuffer, 'pbReceiveBuffer').
      _(cbReceiveBuffer, 'cbReceiveBuffer').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStorageDownloadToMemory');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStorageDownloadToMemoryGetResults
(
    hTask: HANDLE; //XONLINETASK_HANDLE;
    ppbReceivedData: PPBYTE;
    pcbReceivedData: PDWORD;
    pcbDataTotal: PDWORD;
    pqwOwnerPuid: PULONGLONG;
    pftCreationDate: PFILETIME
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStorageDownloadToMemoryGetResults').
      _(hTask, 'hTask').
      _(ppbReceivedData, 'ppbReceivedData').
      _(pcbReceivedData, 'pcbReceivedData').
      _(pcbDataTotal, 'pcbDataTotal').
      _(pqwOwnerPuid, 'pqwOwnerPuid').
      _(pftCreationDate, 'pftCreationDate').
    LogEnd();

  Unimplemented('EmuXOnlineStorageDownloadToMemoryGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStorageEnumerate
(
    dwFacility: DWORD;
    dwUserIndex: DWORD;
    wszStorageEnumerationPath: LPCWSTR;
    dwStartingIndex: DWORD;
    cMaxResultsToReturn: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE// PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStorageEnumerate').
      _(dwFacility, 'dwFacility').
      _(dwUserIndex, 'dwUserIndex').
      _(wszStorageEnumerationPath, 'wszStorageEnumerationPath').
      _(dwStartingIndex, 'dwStartingIndex').
      _(cMaxResultsToReturn, 'cMaxResultsToReturn').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStorageEnumerate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStorageEnumerateGetResults
(
    hTask: HANDLE; //XONLINETASK_HANDLE;
    pdwTotalResults: PDWORD;
    pdwResultsReturned: PDWORD;
    prgpStorageFileInfo: PXONLINESTORAGE_FILE_INFO
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStorageEnumerateGetResults').
      _(hTask, 'hTask').
      _(pdwTotalResults, 'pdwTotalResults').
      _(pdwResultsReturned, 'pdwResultsReturned').
      _(prgpStorageFileInfo, 'prgpStorageFileInfo').
    LogEnd();

  Unimplemented('EmuXOnlineStorageEnumerateGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStorageGetInstallLocation
(
    dwFacility: DWORD;
    wszStoragePath: LPCWSTR;
    szLocation: LPSTR;
    pdwLocationSize: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStorageGetInstallLocation').
      _(dwFacility, 'dwFacility').
      _(wszStoragePath, 'wszStoragePath').
      _(szLocation, 'szLocation').
      _(pdwLocationSize, 'pdwLocationSize').
    LogEnd();

  Unimplemented('EmuXOnlineStorageGetInstallLocation');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStorageGetProgress
(
    hTask: HANDLE; //XONLINETASK_HANDLE;
    pdwPercentDone: PDWORD;
    pqwNumerator: PULONGLONG;
    pqwDenominator: PULONGLONG
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStorageGetProgress').
      _(hTask, 'hTask').
      _(pdwPercentDone, 'pdwPercentDone').
      _(pqwNumerator, 'pqwNumerator').
      _(pqwDenominator, 'pqwDenominator').
    LogEnd();

  Unimplemented('EmuXOnlineStorageGetProgress');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStorageSetFamilyTitleID
(
    dwTitleID: DWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStorageSetFamilyTitleID').
      _(dwTitleID, 'dwTitleID').
    LogEnd();

  Unimplemented('EmuXOnlineStorageSetFamilyTitleID');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStorageUpload
(
    hServerFileReference: HANDLE;
    szDirectory: LPCSTR;
    dwUploadFlags: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE//PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStorageUpload').
      _(hServerFileReference, 'hServerFileReference').
      _(szDirectory, 'szDirectory').
      _(dwUploadFlags, 'dwUploadFlags').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStorageUpload');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStorageUploadByServerPath
(
    dwFacility: DWORD;
    dwUserIndex: DWORD;
    wszStorageFileName: LPCWSTR;
    ftServerExpirationDate: FILETIME;
    szDirectory: LPCSTR;
    dwUploadFlags: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE// PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStorageUploadByServerPath').
      _(dwFacility, 'dwFacility').
      _(dwUserIndex, 'dwUserIndex').
      _(wszStorageFileName, 'wszStorageFileName').
      // Todo: Implement logging
//      _(ftServerExpirationDate, 'ftServerExpirationDate').
      _(szDirectory, 'szDirectory').
      _(dwUploadFlags, 'dwUploadFlags').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStorageUploadByServerPath');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStorageUploadFromMemory
(
    dwFacility: DWORD;
    dwUserIndex: DWORD;
    wszStorageFileName: LPCWSTR;
    ftServerExpirationDate: FILETIME;
    pbDataToUpload: PBYTE;
    cbDataToUpload: DWORD;
    dwUploadFlags: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStorageUploadFromMemory').
      _(dwFacility, 'dwFacility').
      _(dwUserIndex, 'dwUserIndex').
      _(wszStorageFileName, 'wszStorageFileName').
      // Todo: Implement logging
//      _(ftServerExpirationDate, 'ftServerExpirationDate').
      _(pbDataToUpload, 'pbDataToUpload').
      _(cbDataToUpload, 'cbDataToUpload').
      _(dwUploadFlags, 'dwUploadFlags').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStorageUploadFromMemory');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStringLookup
(
    wNumStringIDs: WORD;
    pdwStringIDs: PDWORD;
    dwLanguage: DWORD;
    hWorkEvent: HANDLE;
    phTask: HANDLE // ONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStringLookup').
      _(wNumStringIDs, 'wNumStringIDs').
      _(pdwStringIDs, 'pdwStringIDs').
      _(dwLanguage, 'dwLanguage').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStringLookup');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStringLookupEx
(
    dwTitleID: DWORD;
    wNumStringIDs: WORD;
    pdwStringIDs: PDWORD;
    dwLanguage: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStringLookupEx').
      _(dwTitleID, 'dwTitleID').
      _(wNumStringIDs, 'wNumStringIDs').
      _(pdwStringIDs, 'pdwStringIDs').
      _(dwLanguage, 'dwLanguage').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStringLookupEx');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStringLookupGetResults
(
    hTask: HANDLE;// XONLINETASK_HANDLE;
    pbBuffer: PBYTE;
    pdwBufferSize: PDWORD;
    ppwszStrings: PPWCHAR;
    pwNumStrings: PWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStringLookupGetResults').
      _(hTask, 'hTask').
      _(pbBuffer, 'pbBuffer').
      _(pdwBufferSize, 'pdwBufferSize').
      _(ppwszStrings, 'ppwszStrings').
      _(pwNumStrings, 'pwNumStrings').
    LogEnd();

  Unimplemented('EmuXOnlineStringLookupGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTl_EmuXOnlineStringVerify
(
    wNumStrings: WORD;
    ppwStrings: PLPCWSTR;
    dwLanguage: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStringVerify').
      _(wNumStrings, 'wNumStrings').
      _(ppwStrings, 'ppwStrings').
      _(dwLanguage, 'dwLanguage').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineStringVerify');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineStringVerifyGetResults
(
    hTask: HANDLE; //XONLINETASK_HANDLE;
    wNumResults: WORD;
    pResults: PHRESULT
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineStringVerifyGetResults').
      _(hTask, 'hTask').
      _(wNumResults, 'wNumResults').
      _(pResults, 'pResults').
    LogEnd();

  Unimplemented('EmuXOnlineStringVerifyGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTaskClose
(
    hTask: HANDLE // XONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTaskClose').
      _(hTask, 'hTask').
    LogEnd();

  Unimplemented('EmuXOnlineTaskClose');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTaskContinue
(
    hTask: HANDLE //XONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTaskContinue').
      _(hTask, 'hTask').
    LogEnd();

  Unimplemented('EmuXOnlineTaskContinue');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamCreate
(
    dwUserIndex: DWORD;
    pTeamProperties: PXONLINE_TEAM_PROPERTIES;
    pFirstTeamMemberProperties: PXONLINE_TEAM_MEMBER_PROPERTIES;
    dwMaxTeamMemberCount: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE// PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamCreate').
      _(dwUserIndex, 'dwUserIndex').
      _(pTeamProperties, 'pTeamProperties').
      _(pFirstTeamMemberProperties, 'pFirstTeamMemberProperties').
      _(dwMaxTeamMemberCount, 'dwMaxTeamMemberCount').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineTeamCreate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamCreateGetResults
(
    hTask: HANDLE; // XONLINETASK_HANDLE;
    pTeam: PXONLINE_TEAM
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamCreateGetResults').
      _(hTask, 'hTask').
      _(pTeam, 'pTeam').
    LogEnd();

  Unimplemented('EmuXOnlineTeamCreateGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamDelete
(
    dwUserIndex: DWORD;
    xuidTeam: XUID;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamDelete').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: implement logging
//      _(xuidTeam, 'xuidTeam').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineTeamDelete');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamEnumerate
(
    dwUserIndex: DWORD;
    dwTeamCount: DWORD;
    pxuidTeams: PXUID;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamEnumerate').
      _(dwUserIndex, 'dwUserIndex').
      _(dwTeamCount, 'dwTeamCount').
      _(pxuidTeams, 'pxuidTeams').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineTeamEnumerate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamEnumerateByUserXUID
(
    dwUserIndex: DWORD;
    xuidUser: XUID;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamEnumerateByUserXUID').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: Implement logging
//      _(xuidUser, 'xuidUser').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineTeamEnumerateByUserXUID');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamEnumerateGetResults
(
    hTask: HANDLE; //XONLINETASK_HANDLE;
    pdwTeamCount: PDWORD;
    pxuidTeams: PXUID
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamEnumerateGetResults').
      _(hTask, 'hTask').
      _(pdwTeamCount, 'pdwTeamCount').
      _(pxuidTeams, 'pxuidTeams').
    LogEnd();

  Unimplemented('EmuXOnlineTeamEnumerateGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamGetDetails
(
    hTask: HANDLE; //XONLINETASK_HANDLE;
    xuidTeam: XUID;
    pTeamInfo: PXONLINE_TEAM
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamGetDetails').
      _(hTask, 'hTask').
      // Todo: Implement logging
//      _(xuidTeam, 'xuidTeam').
      _(pTeamInfo, 'pTeamInfo').
    LogEnd();

  Unimplemented('EmuXOnlineTeamGetDetails');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamMemberAnswerRecruit
(
    dwUserIndex: DWORD;
    xuidTeam: XUID;
    RecruitAnswer: XONLINE_PEER_ANSWER_TYPE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE //PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamMemberAnswerRecruit').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: Implement logging
//      _(xuidTeam, 'xuidTeam').
//      _(RecruitAnswer, 'RecruitAnswer').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineTeamMemberAnswerRecruit');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamMemberGetDetails
(
    hTask: HANDLE; //XONLINETASK_HANDLE;
    xuidTeamMember: XUID;
    pTeamMemberInfo: PXONLINE_TEAM_MEMBER
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamMemberGetDetails').
      _(hTask, 'hTask').
      // Todo: Implement logging
//      _(xuidTeamMember, 'xuidTeamMember').
      _(pTeamMemberInfo, 'pTeamMemberInfo').
    LogEnd();

  Unimplemented('EmuXOnlineTeamMemberGetDetails');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamMemberRecruit
(
    dwUserIndex: DWORD;
    xuidTeam: XUID;
    xuidPeer: XUID;
    pPeerTeamMemberInfo: PXONLINE_TEAM_MEMBER_PROPERTIES;
    hMsg: HANDLE; // XONLINE_MSG_HANDLE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamMemberRecruit').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: Implement logging
//      _(xuidTeam, 'xuidTeam').
//      _(xuidPeer, 'xuidPeer').
      _(pPeerTeamMemberInfo, 'pPeerTeamMemberInfo').
      _(hMsg, 'hMsg').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineTeamMemberRecruit');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamMemberRecruitByName
(
    dwUserIndex: DWORD;
    xuidTeam: XUID;
    lpPeerName: LPCSTR;
    pPeerTeamMemberInfo: PXONLINE_TEAM_MEMBER_PROPERTIES;
    hMsg: HANDLE; //XONLINE_MSG_HANDLE;
    hWorkEvent: HANDLE;
    phTask: PHANDLE// PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamMemberRecruitByName').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: Implement logging
//      _(xuidTeam, 'xuidTeam').
      _(lpPeerName, 'lpPeerName').
      _(pPeerTeamMemberInfo, 'pPeerTeamMemberInfo').
      _(hMsg, 'hMsg').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineTeamMemberRecruitByName');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamMemberRemove
(
    dwUserIndex: DWORD;
    xuidTeam: XUID;
    xuidTeamMember: XUID;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamMemberRemove').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: Implement logging
//      _(xuidTeam, 'xuidTeam').
//      _(xuidTeamMember, 'xuidTeamMember').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineTeamMemberRemove');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamMembersEnumerate
(
    dwUserIndex: DWORD;
    xuidTeam: XUID;
    dwFlags: DWORD;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamMembersEnumerate').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: Implement logging
//      _(xuidTeam, 'xuidTeam').
      _(dwFlags, 'dwFlags').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineTeamMembersEnumerate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamMembersEnumerateGetResults
(
    hTask: PHANDLE; // XONLINETASK_HANDLE;
    pdwTeamMemberCount: PDWORD;
    pxuidTeamMembers: PXUID
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamMembersEnumerateGetResults').
      _(hTask, 'hTask').
      _(pdwTeamMemberCount, 'pdwTeamMemberCount').
      _(pxuidTeamMembers, 'pxuidTeamMembers').
    LogEnd();

  Unimplemented('EmuXOnlineTeamMembersEnumerateGetResults');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamMemberSetProperties
(
    dwUserIndex: DWORD;
    xuidTeam: XUID;
    xuidTeamMember: XUID;
    pTeamMemberProperties: PXONLINE_TEAM_MEMBER_PROPERTIES;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamMemberSetProperties').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: Implement logging
//      _(xuidTeam, 'xuidTeam').
//      _(xuidTeamMember, 'xuidTeamMember').
      _(pTeamMemberProperties, 'pTeamMemberProperties').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineTeamMemberSetProperties');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamSetFamilyTitleID
(
    dwTitleID: DWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamSetFamilyTitleID').
      _(dwTitleID, 'dwTitleID').
    LogEnd();

  Unimplemented('EmuXOnlineTeamSetFamilyTitleID');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTeamSetProperties
(
    dwUserIndex: DWORD;
    xuidTeam: XUID;
    pTeamProperties: PXONLINE_TEAM_PROPERTIES;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTeamSetProperties').
      _(dwUserIndex, 'dwUserIndex').
      // Todo: Implement logging
//      _(xuidTeam, 'xuidTeam').
      _(pTeamProperties, 'pTeamProperties').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineTeamSetProperties');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineThrottleGet
(
    dwServiceID: DWORD;
    szThrottleTag: LPCSTR;
    pdwThrottleFlags: PDWORD;
    pdwDelay: PDWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineThrottleGet').
      _(dwServiceID, 'dwServiceID').
      _(szThrottleTag, 'szThrottleTag').
      _(pdwThrottleFlags, 'pdwThrottleFlags').
      _(pdwDelay, 'pdwDelay').
    LogEnd();

  Unimplemented('EmuXOnlineThrottleGet');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineThrottleSet
(
    dwServiceID: DWORD;
    szThrottleTag: LPCSTR;
    dwThrottleFlags: DWORD;
    dwDelay: DWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineThrottleSet').
      _(dwServiceID, 'dwServiceID').
      _(szThrottleTag, 'szThrottleTag').
      _(dwThrottleFlags, 'dwThrottleFlags').
      _(dwDelay, 'dwDelay').
    LogEnd();

  Unimplemented('EmuXOnlineThrottleSet');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTitleIdIsSamePublisher
(
    dwTitleID: DWORD
): BOOL; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTitleIdIsSamePublisher').
      _(dwTitleID, 'dwTitleID').
    LogEnd();

  Unimplemented('EmuXOnlineTitleIdIsSamePublisher');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTitleIdIsSameTitle
(
    dwTitleID: DWORD
): BOOL; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTitleIdIsSameTitle').
      _(dwTitleID, 'dwTitleID').
    LogEnd();

  Unimplemented('EmuXOnlineTitleIdIsSameTitle');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXOnlineTitleUpdate
(
    dwContext: DWORD
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTitleUpdate').
      _(dwContext, 'dwContext').
    LogEnd();

  Unimplemented('EmuXOnlineTitleUpdate');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

// Todo: Implement PLD_UPDATE
(*function XTL_EmuXOnlineTitleUpdateEx
(
    pldUpdate: PLD_UPDATE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineTitleUpdateEx').
      _(pldUpdate, 'pldUpdate').
    LogEnd();

  Unimplemented('EmuXOnlineTitleUpdateEx');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end; *)

function XTL_EmuXOnlineVerifyNickname
(
    lpNickname: LPCWSTR;
    hWorkEvent: HANDLE;
    phTask: PHANDLE // PXONLINETASK_HANDLE
): HRESULT; stdcall;
// Branch:Dxbx  Translator:Shadow_tj  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuXOnlineVerifyNickname').
      _(lpNickname, 'lpNickname').
      _(hWorkEvent, 'hWorkEvent').
      _(phTask, 'phTask').
    LogEnd();

  Unimplemented('EmuXOnlineVerifyNickname');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;


exports
  XTL_EmuWSAStartup,

  XTL_EmuXNetGetEthernetLinkStatus,
  XTL_EmuXNetStartup, //: DXBX marked out for better logging

  XTL_EmuXNetCleanup,
  XTL_EmuXNetConnect,
  XTL_EmuXNetCreateKey,
//  XTL_EmuXNetDnsLookup, // not implemented
//  XTL_EmuXNetDnsRelease, // not implemented
  XTL_EmuXNetGetConnectStatus,
  XTL_EmuXNetGetDebugXnAddr,
  XTL_EmuXNetGetEthernetLinkStatus,
  XTL_EmuXNetGetTitleXnAddr,
  XTL_EmuXNetInAddrToString,
  XTL_EmuXNetInAddrToXnAddr,
  XTL_EmuXNetQosListen,
//  XTL_EmuXNetQosLookup, // not implemented
  XTL_EmuXNetQosRelease,
  XTL_EmuXNetServerToInAddr,
  XTL_EmuXNetStartup,
  XTL_EmuXNetUnregisterInAddr,
  XTL_EmuXNetXnAddrToInAddr,

  XTL_EmuXOnlineLaunchNewImage,
  XTL_EmuXOnlineLogon,

  XTL_EmuXOnlineLaunchNewImage,
  XTL_EmuXOnlineArbitrationCreateRoundID,
  XTL_EmuXOnlineArbitrationExtendRound,
  XTL_EmuXOnlineArbitrationRegister,
  XTL_EmuXOnlineArbitrationRegisterGetResults,
  XTL_EmuXOnlineArbitrationReport,
  XTL_EmuXOnlineChangeLogonUsers,
  XTL_EmuXOnlineChangeLogonUsersTaskGetResults,
  XTL_EmuXOnlineCleanup,
  XTL_EmuXOnlineCompetitionCancel,
  XTL_EmuXOnlineCompetitionCheckin,
  XTL_EmuXOnlineCompetitionCreate,
  XTL_EmuXOnlineCompetitionCreateGetResults,
  XTL_EmuXOnlineCompetitionCreateSingleElimination,
  XTL_EmuXOnlineCompetitionGetResultsBufferSize,
  XTL_EmuXOnlineCompetitionManageEntrant,
  XTL_EmuXOnlineCompetitionSearch,
  XTL_EmuXOnlineCompetitionSearchGetResults,
  XTL_EmuXOnlineCompetitionSessionRegister,
  XTL_EmuXOnlineCompetitionSessionRegisterGetResults,
  XTL_EmuXOnlineCompetitionSubmitResults,
  XTL_EmuXOnlineCompetitionTopology,
  XTL_EmuXOnlineCompetitionTopologyGetResults,
  XTL_EmuXOnlineCompetitionTopologySingleElimination,
  XTL_EmuXOnlineCompetitionTopologySingleEliminationGetResults,
//  XTL_EmuXOnlineContentInstall, // Not yet implemented
  XTL_EmuXOnlineContentInstallGetProgress,
  XTL_EmuXOnlineContentInstallGetSize,
  XTL_EmuXOnlineContentSetSecurityKey,
  XTl_EmuXOnlineFeedbackSend,
  XTL_EmuXOnlineFriendsAnswerGameInvite,
  XTL_EmuXOnlineFriendsAnswerRequest,
  XTL_EmuXOnlineFriendsEnumerate,
  XTL_EmuXOnlineFriendsEnumerateFinish,
  XTL_EmuXOnlineFriendsGameInvite,
  XTL_EmuXOnlineFriendsGetAcceptedGameInvite,
  XTL_EmuXOnlineFriendsGetLatest,
  XTL_EmuXOnlineFriendsGetLatestByFocus,
  XTL_EmuXOnlineFriendsGetLatestByRange,
  XTL_EmuXOnlineFriendsGetTitleName,
  XTL_EmuXOnlineFriendsJoinGame,
  XTL_EmuXOnlineFriendsRemove,
  XTL_EmuXOnlineFriendsRequest,
  XTL_EmuXOnlineFriendsRequestByName,
  XTL_EmuXOnlineFriendsRequestByNameEx,
  XTL_EmuXOnlineFriendsRequestEx,
  XTL_EmuXOnlineFriendsRevokeGameInvite,
  XTL_EmuXOnlineFriendsStartup,
  XTL_EmuXOnlineGameDataUpload,
  XTL_EmuXOnlineGameInviteAnswer,
  XTL_EmuXOnlineGameInviteGetLatestAccepted,
  XTL_EmuXOnlineGameInviteRevoke,
  XTL_EmuXOnlineGameInviteSend,
  XTL_EmuXOnlineGameJoin,
  XTL_EmuXOnlineGetLogonUsers,
  XTL_EmuXOnlineGetNotification,
  XTL_EmuXOnlineGetNotificationEx,
  XTL_EmuXOnlineGetServiceInfo,
  XTL_EmuXOnlineGetUsers,
  XTL_EmuXOnlineLogon,
  XTL_EmuXOnlineLogonTaskGetResults,
  XTL_EmuXOnlineMatchSearch,
  XTL_EmuXOnlineMatchSearchGetResults,
  XTL_EmuXOnlineMatchSearchParse,
  XTL_EmuXOnlineMatchSearchResultsLen,
  XTL_EmuXOnlineMatchSessionCreate,
  XTL_EmuXOnlineMatchSessionDelete,
  XTL_EmuXOnlineMatchSessionFindFromID,
  XTL_EmuXOnlineMatchSessionGetInfo,
  XTL_EmuXOnlineMatchSessionUpdate,
  XTL_EmuXOnlineMessageCreate,
  XTL_EmuXOnlineMessageDelete,
  XTL_EmuXOnlineMessageDestroy,
  XTL_EmuXOnlineMessageDetails,
  XTL_EmuXOnlineMessageDetailsGetResultsProperty,
  XTL_EmuXOnlineMessageDetailsGetResultsSummary,
  XTL_EmuXOnlineMessageDownloadAttachmentGetProgress,
  XTL_EmuXOnlineMessageDownloadAttachmentToDirectory,
  XTL_EmuXOnlineMessageDownloadAttachmentToMemory,
  XTL_EmuXOnlineMessageDownloadAttachmentToMemoryGetResults,
  XTL_EmuXOnlineMessageEnableReceivingFamilyTitleIDs,
  XTL_EmuXOnlineMessageEnumerate,
  XTL_EmuXOnlineMessageEnumerate,
  XTL_EmuXOnlineMessageRevoke,
  XTL_EmuXOnlineMessageSend,
  XTL_EmuXOnlineMessageSendGetProgress,
  XTL_EmuXOnlineMessageSendGetResults,
  XTL_EmuXOnlineMessageSetFlags,
  XTL_EmuXOnlineMessageSetProperty,
  XTL_EmuXOnlineMessageSetSendingFamilyTitleID,
  XTL_EmuXOnlineMessageSetSummaryRefresh,
  XTL_EmuXOnlineMessageSummary,
  XTL_EmuXOnlineMutelistAdd,
  XTL_EmuXOnlineMutelistGet,
  XTL_EmuXOnlineMutelistRemove,
  XTL_EmuXOnlineMutelistStartup,
  XTL_EmuXOnlineNotificationSetState,
//  XTL_EmuXOnlineOfferingCancel, // not yet implemented
// XTL_EmuXOnlineOfferingDetails, // not yet implemented
  XTL_EmuXOnlineOfferingDetailsGetResults,
  XTL_EmuXOnlineOfferingDetailsMaxSize,
  XTl_EmuXOnlineOfferingEnumerate,
//  XTL_EmuXOnlineOfferingEnumerateGetResults, // not yet implemented
  XTL_EmuXOnlineOfferingEnumerateMaxSize,
  XTL_EmuXOnlineOfferingIsNewContentAvailable,
  XTL_EmuXOnlineOfferingPriceFormat,
//  XTL_EmuXOnlineOfferingPurchase, // not yet implemented
  XTL_EmuXOnlinePresenceAdd,
  XTL_EmuXOnlinePresenceClear,
  XTL_EmuXOnlinePresenceGetLatest,
  XTL_EmuXOnlinePresenceGetTitleName,
  XTL_EmuXOnlinePresenceInit,
  XTL_EmuXOnlinePresenceSubmit,
  XTL_EmuXOnlineQueryAdd,
//  XTL_EmuXOnlineQueryAddGetResults, // not yet implemented
//  XTL_EmuXOnlineQueryFindFromIds, // not yet implemented
  XTL_EmuXOnlineQueryFindFromIdsGetResults,
  XTL_EmuXOnlineQueryGetResultsBufferSize,
  XTL_EmuXOnlineQueryRemove,
//   XTL_EmuXOnlineQueryRemoveId, // not yet implemented
  XTL_EmuXOnlineQuerySearch,
  XTL_EmuXOnlineQuerySearchGetResults,
//  XTL_EmuXOnlineQuerySelect, // not yet implemented
  XTL_EmuXOnlineQueryUpdate,
//  XTL_EmuXOnlineQueryUpdateId, // not yet implemented
  XTL_EmuXOnlineRetrieveLogonState,
  XTL_EmuXOnlineSaveLogonState,
  XTL_EmuXOnlineSignatureVerify,
  XTL_EmuXOnlineSignatureVerifyGetResults,
  XTL_EmuXOnlineSilentLogon,
  XTl_EmuXOnlineStartup,
  XTL_EmuXOnlineStatLeaderEnumerate,
  XTL_EmuXOnlineStatLeaderEnumerateGetResults,
  XTL_EmuXOnlineStatRead,
  XTL_EmuXOnlineStatReadGetResult,
  XTL_EmuXOnlineStatReset,
  XTL_EmuXOnlineStatUnitEnumerate,
  XTL_EmuXOnlineStatUnitEnumerateGetResults,
  XTL_EmuXOnlineStatUnitRead,
  XTL_EmuXOnlineStatUnitReadGetResult,
  XTL_EmuXOnlineStatWrite,
  XTL_EmuXOnlineStatWriteEx,
  XTL_EmuXOnlineStatWriteGetResult,
  XTL_EmuXOnlineStorageCreateServerPath,
  XTL_EmuXOnlineStorageDeleteFile,
  XTL_EmuXOnlineStorageDownload,
  XTL_EmuXOnlineStorageDownloadToMemory,
  XTL_EmuXOnlineStorageDownloadToMemoryGetResults,
  XTL_EmuXOnlineStorageEnumerate,
  XTL_EmuXOnlineStorageEnumerateGetResults,
  XTL_EmuXOnlineStorageGetInstallLocation,
  XTL_EmuXOnlineStorageGetProgress,
  XTL_EmuXOnlineStorageSetFamilyTitleID,
  XTL_EmuXOnlineStorageUpload,
  XTL_EmuXOnlineStorageUploadByServerPath,
  XTL_EmuXOnlineStorageUploadFromMemory,
  XTL_EmuXOnlineStringLookup,
  XTL_EmuXOnlineStringLookupEx,
  XTL_EmuXOnlineStringLookupGetResults,
  XTl_EmuXOnlineStringVerify,
  XTL_EmuXOnlineStringVerifyGetResults,
  XTL_EmuXOnlineTaskClose,
  XTL_EmuXOnlineTaskContinue,
  XTL_EmuXOnlineTeamCreate,
  XTL_EmuXOnlineTeamCreateGetResults,
  XTL_EmuXOnlineTeamDelete,
  XTL_EmuXOnlineTeamEnumerate,
  XTL_EmuXOnlineTeamEnumerateByUserXUID,
  XTL_EmuXOnlineTeamEnumerateGetResults,
  XTL_EmuXOnlineTeamGetDetails,
  XTL_EmuXOnlineTeamMemberAnswerRecruit,
  XTL_EmuXOnlineTeamMemberGetDetails,
  XTL_EmuXOnlineTeamMemberRecruit,
  XTL_EmuXOnlineTeamMemberRecruitByName,
  XTL_EmuXOnlineTeamMemberRemove,
  XTL_EmuXOnlineTeamMembersEnumerate,
  XTL_EmuXOnlineTeamMembersEnumerateGetResults,
  XTL_EmuXOnlineTeamMemberSetProperties,
  XTL_EmuXOnlineTeamSetFamilyTitleID,
  XTL_EmuXOnlineTeamSetProperties,
  XTL_EmuXOnlineThrottleGet,
  XTL_EmuXOnlineThrottleSet,
  XTL_EmuXOnlineTitleIdIsSamePublisher,
  XTL_EmuXOnlineTitleIdIsSameTitle,
  XTL_EmuXOnlineTitleUpdate,
//  XTL_EmuXOnlineTitleUpdateEx, // not yet implemented
  XTL_EmuXOnlineVerifyNickname;


end.

