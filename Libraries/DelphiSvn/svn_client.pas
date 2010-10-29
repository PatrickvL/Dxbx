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
{ The Original Code is svn_client.pas.                                                                                 }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{   Uwe Schuster (uschuster)                                                                                           }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains import declarations for libsvn_client.dll, Subversion 1.5.0 DLL.                                  }
{                                                                                                                      }
{**********************************************************************************************************************}

unit svn_client;

interface

{$MINENUMSIZE 4}

uses
  Windows, SysUtils,
  apr;

//----- svn_ctype.h ----------------------------------------------------------------------------------------------------

const
  SVN_CTYPE_ASCII_MINUS            =  45; // ASCII value of '-'
  SVN_CTYPE_ASCII_DOT              =  46; // ASCII value of '.'
  SVN_CTYPE_ASCII_COLON            =  58; // ASCII value of ':'
  SVN_CTYPE_ASCII_UNDERSCORE       =  95; // ASCII value of '_'
  SVN_CTYPE_ASCII_TAB              =   9; // ASCII value of a tab
  SVN_CTYPE_ASCII_LINEFEED         =  10; // ASCII value of a line feed
  SVN_CTYPE_ASCII_CARRIAGERETURN   =  13;
  SVN_CTYPE_ASCII_DELETE           = 127;

function svn_ctype_iscntrl(C: Char): Boolean;
function svn_ctype_isspace(C: Char): Boolean;
function svn_ctype_isdigit(C: Char): Boolean;
function svn_ctype_isupper(C: Char): Boolean;
function svn_ctype_islower(C: Char): Boolean;
function svn_ctype_ispunct(C: Char): Boolean;
function svn_ctype_isascii(C: Char): Boolean;
function svn_ctype_isalpha(C: Char): Boolean;
function svn_ctype_isalnum(C: Char): Boolean;
function svn_ctype_isxdigit(C: Char): Boolean;
function svn_ctype_isgraph(C: Char): Boolean;
function svn_ctype_isprint(C: Char): Boolean;
function svn_ctype_isutf8lead(C: Char): Boolean;
function svn_ctype_isutf8cont(C: Char): Boolean;
function svn_ctype_isutf8mbc(C: Char): Boolean;
function svn_ctype_isutf8(C: Char): Boolean;

var
  svn_ctype_table: function: PCardinal; cdecl;
  svn_ctype_casecmp: function(a, b: Integer): Integer; cdecl;

//----- svn_ctype.h ----------------------------------------------------------------------------------------------------

//----- svn_dav.h ------------------------------------------------------------------------------------------------------

const
  SVN_SVNDIFF_MIME_TYPE = 'application/vnd.svn-svndiff';
  SVN_DAV_DELTA_BASE_HEADER = 'X-SVN-VR-Base';
  SVN_DAV_OPTIONS_HEADER = 'X-SVN-Options';
  SVN_DAV_OPTION_NO_MERGE_RESPONSE = 'no-merge-response';
  SVN_DAV_OPTION_LOCK_BREAK = 'lock-break';
  SVN_DAV_OPTION_LOCK_STEAL = 'lock-steal';
  SVN_DAV_OPTION_RELEASE_LOCKS = 'release-locks';
  SVN_DAV_OPTION_KEEP_LOCKS = 'keep-locks';
  SVN_DAV_VERSION_NAME_HEADER = 'X-SVN-Version-Name';
  SVN_DAV_CREATIONDATE_HEADER = 'X-SVN-Creation-Date';
  SVN_DAV_LOCK_OWNER_HEADER = 'X-SVN-Lock-Owner';
  SVN_DAV_BASE_FULLTEXT_MD5_HEADER = 'X-SVN-Base-Fulltext-MD5';
  SVN_DAV_RESULT_FULLTEXT_MD5_HEADER = 'X-SVN-Result-Fulltext-MD5';
  SVN_DAV_ERROR_NAMESPACE = 'svn:';
  SVN_DAV_ERROR_TAG = 'error';
  SVN_DAV_PROP_NS_SVN = 'http://subversion.tigris.org/xmlns/svn/';
  SVN_DAV_PROP_NS_CUSTOM = 'http://subversion.tigris.org/xmlns/custom/';
  SVN_DAV_PROP_NS_DAV = 'http://subversion.tigris.org/xmlns/dav/';
  SVN_DAV_NS_DAV_SVN_DEPTH = SVN_DAV_PROP_NS_DAV + 'svn/depth';
  SVN_DAV_NS_DAV_SVN_MERGEINFO = SVN_DAV_PROP_NS_DAV + 'svn/mergeinfo';
  SVN_DAV_NS_DAV_SVN_LOG_REVPROPS = SVN_DAV_PROP_NS_DAV + 'svn/log-revprops';
  SVN_DAV_NS_DAV_SVN_PARTIAL_REPLAY = SVN_DAV_PROP_NS_DAV + 'svn/partial-replay';

//----- svn_dav.h ------------------------------------------------------------------------------------------------------

//----- svn_types.h ----------------------------------------------------------------------------------------------------

const
  SVN_DIRENT_KIND = $00001;
  SVN_DIRENT_SIZE = $00002;
  SVN_DIRENT_HAS_PROPS = $00004;
  SVN_DIRENT_CREATED_REV = $00008;
  SVN_DIRENT_TIME = $00010;
  SVN_DIRENT_LAST_AUTHOR = $00020;
  SVN_DIRENT_ALL = High(Word);

type
  PSvnError = ^TSvnError;
  TSvnError = record
    apr_err: TAprStatus;
    message: PAnsiChar;
    child: PSvnError;
    pool: PAprPool;
    afile: PAnsiChar;
    line: Longint;
  end;
  TSvnNodeKind = (
    svnNodeNone,
    svnNodeFile,
    svnNodeDir,
    svnNodeUnknown
  );
  PSvnRevNum = ^TSvnRevNum;
  TSvnRevNum = Longint;
  TSvnFileSize = Int64;
  TSvnBoolean = LongBool;
  TSvnRecurseKind = (
    svnRecursive,
    svnNonrecursive
  );

  TSvnDepth = (
    svnDepthUnknown = -2,
    svnDepthExclude = -1,
    svnDepthEmpty = 0,
    svnDepthFiles = 1,
    svnDepthImmediates = 2,
    svnDepthInfinity = 3
  );

  PPSvnDirEnt = ^PSvnDirEnt;
  PSvnDirEnt = ^TSvnDirEnt;
  TSvnDirEnt = record
    kind: TSvnNodeKind;
    size: TSvnFileSize;
    has_props: TSvnBoolean;
    created_rev: TSvnRevNum;
    time: TAprTime;
    last_author: PAnsiChar;
  end;

  PSvnCommitInfo = ^TSvnCommitInfo;
  TSvnCommitInfo = record
    revision: TSvnRevNum;
    date: PAnsiChar;
    author: PAnsiChar;
    post_commit_err: PAnsiChar;
  end;

  PSvnLogChangedPath = ^TSvnLogChangedPath;
  TSvnLogChangedPath = record
    action: Char;
    copyfrom_path: PAnsiChar;
    copyfrom_rev: TSvnRevNum;
  end;

  PSvnLogEntry = ^TSvnLogEntry;
  TSvnLogEntry = record
    changed_paths: PAprHash;
    revision: TSvnRevNum;
    revprops: PAprHash;
    has_children: TSvnBoolean;
  end;

  TSvnLogEntryReceiver = function(baton: Pointer; log_entry: PSvnLogEntry; pool: PAprPool): PSvnError; cdecl;
  TSvnLogMessageReceiver = function(baton: Pointer; changed_paths: PAprHash; revision: TSvnRevNum;
    author, date, message: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  TSvnCommitCallback2 = function(commit_info: PSvnCommitInfo; baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  TSvnCommitCallback = function(new_revision: TSvnRevNum; date, author: PAnsiChar; baton: Pointer): PSvnError; cdecl;
  TSvnCancelFunc = function(cancel_baton: Pointer): PSvnError; cdecl;

  PSvnLock = ^TSvnLock;
  TSvnLock = record
    path: PAnsiChar;
    token: PAnsiChar;
    owner: PAnsiChar;
    comment: PAnsiChar;
    is_dav_comment: TSvnBoolean;
    creation_date: TAprTime;
    expiration_date: TAprTime;
  end;

  PSvnMergeRange = ^TSvnMergeRange;
  TSvnMergeRange = record
    rev_start: TSvnRevNum;
    rev_end: TSvnRevNum;
    inheritable: TSvnBoolean;
  end;

  PSvnLocationSegment = ^TSvnLocationSegment;
  TSvnLocationSegment = record
    range_start: TSvnRevNum;
    range_end: TSvnRevNum;
    path: PAnsiChar;
  end;

  TSvnLocationSegmentReceiver = function(segment: PSvnLocationSegment; baton: Pointer; pool: PAprPool): PSvnError;
    cdecl;

const
  SVN_INVALID_REVNUM: TSvnRevNum = -1;
  SVN_INVALID_FILESIZE: TSvnFileSize = -1;

  SVN_KEYWORD_MAX_LEN = 255;
  SVN_KEYWORD_REVISION_LONG = 'LastChangedRevision';
  SVN_KEYWORD_REVISION_SHORT = 'Rev';
  SVN_KEYWORD_REVISION_MEDIUM = 'Revision';
  SVN_KEYWORD_DATE_LONG = 'LastChangedDate';
  SVN_KEYWORD_DATE_SHORT = 'Date';
  SVN_KEYWORD_AUTHOR_LONG = 'LastChangedBy';
  SVN_KEYWORD_AUTHOR_SHORT = 'Author';
  SVN_KEYWORD_URL_LONG = 'HeadURL';
  SVN_KEYWORD_URL_SHORT = 'URL';
  SVN_KEYWORD_ID = 'Id';

  SVN_STREAM_CHUNK_SIZE = 102400;
  SVN__STREAM_CHUNK_SIZE = 16384;

var
  svn_revnum_parse: function(rev: PSvnRevNum; str: PAnsiChar; endptr: PPAnsiChar): PSvnError; cdecl;
  svn_depth_to_word: function(depth: TSvnDepth): PAnsiChar; cdecl;
  svn_depth_from_word: function(word: PAnsiChar): TSvnDepth; cdecl;
  svn_dirent_dup: function(dirent: PSvnDirent; pool: PAprPool): PSvnDirent; cdecl;
  svn_log_entry_create: function(pool: PAprPool): PSvnLogEntry; cdecl;
  svn_create_commit_info: function(pool: PAprPool): PSvnCommitInfo; cdecl;
  svn_commit_info_dup: function(src_commit_info: PSvnCommitInfo; pool: PAprPool): PSvnCommitInfo; cdecl;
  svn_log_changed_path_dup: function(changed_path: PSvnLogChangedPath; pool: PAprPool): PSvnLogChangedPath; cdecl;
  svn_mime_type_validate: function(mime_type: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_mime_type_is_binary: function(mime_type: PAnsiChar): TSvnBoolean; cdecl;
  svn_lock_create: function(pool: PAprPool): PSvnLock; cdecl;
  svn_lock_dup: function(lock: PSvnLock; pool: PAprPool): PSvnLock; cdecl;
  svn_uuid_generate: function(pool: PAprPool): PAnsiChar; cdecl;
  svn_merge_range_dup: function(range: PSvnMergeRange; pool: PAprPool): PSvnMergeRange; cdecl;
  svn_merge_range_contains_rev: function(range: PSvnMergeRange; rev: TSvnRevNum): TSvnBoolean; cdecl;
  svn_location_segment_dup: function(segment: PSvnLocationSegment; pool: PAprPool): PSvnLocationSegment; cdecl;

//----- svn_types.h ----------------------------------------------------------------------------------------------------

//----- svn_compat.h ---------------------------------------------------------------------------------------------------

var
  svn_compat_wrap_commit_callback: procedure(out callback2: TSvnCommitCallback2; out callback2_baton: Pointer;
    callback: TSvnCommitCallback; callback_baton: Pointer; pool: PAprPool); cdecl;
  svn_compat_log_revprops_clear: procedure(revprops: PAprHash); cdecl;
  svn_compat_log_revprops_in: function(pool: PAprPool): PAprArrayHeader; cdecl;
  svn_compat_log_revprops_out: procedure(out author, date, message: PAnsiChar; revprops: PAprHash); cdecl;
  svn_compat_wrap_log_receiver: procedure(out receiver2: TSvnLogEntryReceiver; out receiver2_baton: Pointer;
    receiver: TSvnLogEntryReceiver; receiver_baton: Pointer; pool: PAprPool); cdecl;

//----- svn_compat.h ---------------------------------------------------------------------------------------------------

//----- svn_nls.h ------------------------------------------------------------------------------------------------------

var
  svn_nls_init: function: PSvnError; cdecl;

//----- svn_nls.h ------------------------------------------------------------------------------------------------------

//----- svn_version.h --------------------------------------------------------------------------------------------------

type
  PSvnVersion = ^TSvnVersion;
  TSvnVersion = record
    major: Integer;
    minor: Integer;
    patch: Integer;
    tag: PAnsiChar;
  end;
  TVersionQueryFunc = function: PSvnVersion; cdecl;
  PSvnVersionChecklist = ^TSvnVersionChecklist;
  TSvnVersionChecklist = record
    alabel: PAnsiChar;
    version_query: TVersionQueryFunc;
  end;

const
  SVN_VER_MAJOR = 1;
  SVN_VER_MINOR = 5;
  SVN_VER_PATCH = 0;
  SVN_VER_TAG = ' (r31699)';
  SVN_VER_NUMTAG = '';
  SVN_VER_REVISION = 31699;

  SVN_VER_NUM = '1.5.0';
  SVN_VER_NUMBER = SVN_VER_NUM + SVN_VER_NUMTAG;
  SVN_VERSION = SVN_VER_NUM + SVN_VER_TAG;

var
  svn_ver_compatible: function(my_version, lib_version: PSvnVersion): TSvnBoolean; cdecl;
  svn_ver_equal: function(my_version, lib_version: PSvnVersion): TSvnBoolean; cdecl;
  svn_ver_check_list: function(my_version: PSvnVersion; checklist: PSvnVersionChecklist): PSvnError; cdecl;
  svn_subr_version: function: PSvnVersion; cdecl;

//----- svn_version.h --------------------------------------------------------------------------------------------------

//----- svn_time.h -----------------------------------------------------------------------------------------------------

var
  svn_time_to_cstring: function(when: TAprTime; pool: PAprPool): PAnsiChar; cdecl;
  svn_time_from_cstring: function(out when: TAprTime; data: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_time_to_human_cstring: function(when: TAprTime; pool: PAprPool): PAnsiChar; cdecl;
  svn_parse_date: function(out matched: TSvnBoolean; out result: TAprTime; text: PAnsiChar; now: TAprTime;
    pool: PAprPool): PSvnError; cdecl;
  svn_sleep_for_timestamps: procedure; cdecl;

//----- svn_time.h -----------------------------------------------------------------------------------------------------

//----- svn_user.h -----------------------------------------------------------------------------------------------------

var
  svn_user_get_name: function(pool: PAprPool): PAnsiChar; cdecl;
  svn_user_get_homedir: function(pool: PAprPool): PAnsiChar; cdecl;

//----- svn_user.h -----------------------------------------------------------------------------------------------------

//----- svn_sorts.h ----------------------------------------------------------------------------------------------------

type
  PSvnSortItem = ^TSvnSortItem;
  TSvnSortItem = record
    key: Pointer;
    klen: TAprSize;
    value: Pointer;
  end;
  TSvnSortHashCompare = function(a, b: PSvnSortItem): Integer; cdecl;

var
  svn_sort_compare_items_as_paths: function(a, b: PSvnSortItem): Integer; cdecl;
  svn_sort_compare_items_lexically: function(a, b: PSvnSortItem): Integer; cdecl;
  svn_sort_compare_revisions: function(a, b: Pointer): Integer; cdecl;
  svn_sort_compare_paths: function(a, b: Pointer): Integer; cdecl;
  svn_sort_compare_ranges: function(a, b: Pointer): Integer; cdecl;
  svn_sort__hash: function(ht: PAprHash; comparison_func: TSvnSortHashCompare; pool: PAprPool): PAprArrayHeader; cdecl;

//----- svn_sorts.h ----------------------------------------------------------------------------------------------------

//----- svn_pools.h ----------------------------------------------------------------------------------------------------

const
  SVN_ALLOCATOR_RECOMMENDED_MAX_FREE = 4096 * 1024;

var
  svn_pool_create_ex: function(parent_pool: PAprPool; allocator: PAprAllocator): PAprPool; cdecl;
  svn_pool_create_ex_debug: function(parent_pool: PAprPool; allocator: PAprAllocator; file_line: PAnsiChar): PAprPool; cdecl;

//----- svn_pools.h ----------------------------------------------------------------------------------------------------

//----- svn_md5.h ------------------------------------------------------------------------------------------------------

var
  svn_md5_empty_string_digest: function: PByte; cdecl;
  svn_md5_digest_to_cstring_display: function(digest: PByte; pool: PAprPool): PAnsiChar; cdecl;
  svn_md5_digest_to_cstring: function(digest: PByte; pool: PAprPool): PAnsiChar; cdecl;
  svn_md5_digests_match: function(d1, d2: PByte): TSvnBoolean; cdecl;

//----- svn_md5.h ------------------------------------------------------------------------------------------------------

//----- svn_string.h ---------------------------------------------------------------------------------------------------

type
  PPSvnString = ^PSvnString;
  PSvnString = ^TSvnString;
  TSvnString = record
    data: PAnsiChar;
    len: TAprSize;
  end;
  PSvnStringBuf = ^TSvnStringBuf;
  TSvnStringBuf = record
    pool: PAprPool;
    data: PAnsiChar;
    len: TAprSize;
    blocksize: TAprSize;
  end;

var
  svn_string_create: function(cstring: PAnsiChar; pool: PAprPool): PSvnString; cdecl;
  svn_string_ncreate: function(bytes: PAnsiChar; size: TAprSize; pool: PAprPool): PSvnString; cdecl;
  svn_string_create_from_buf: function(strbuf: PSvnStringBuf; pool: PAprPool): PSvnString; cdecl;
  svn_string_createf: function(pool: PAprPool; fmt: PAnsiChar; const args: array of const): PSvnString; cdecl;
  svn_string_createv: function(pool: PAprPool; fmt: PAnsiChar; const ap: array of const): PSvnString; cdecl;
  svn_string_isempty: function(str: PSvnString): TSvnBoolean; cdecl;
  svn_string_dup: function(original_string: PSvnString; pool: PAprPool): PSvnString; cdecl;
  svn_string_compare: function(str1, str2: PSvnString): TSvnBoolean; cdecl;
  svn_string_first_non_whitespace: function(str: PSvnString): TAprSize; cdecl;
  svn_string_find_char_backward: function(str: PSvnString; ch: Char): TAprSize; cdecl;
  
  svn_stringbuf_create: function(cstring: PAnsiChar; pool: PAprPool): PSvnStringBuf; cdecl;
  svn_stringbuf_ncreate: function(bytes: PAnsiChar; size: TAprSize; pool: PAprPool): PSvnStringBuf; cdecl;
  svn_stringbuf_create_from_string: function(str: PSvnString; pool: PAprPool): PSvnStringBuf; cdecl;
  svn_stringbuf_createf: function(pool: PAprPool; fmt: PAnsiChar; const args: array of const): PSvnStringBuf; cdecl;
  svn_stringbuf_createv: function(pool: PAprPool; fmt: PAnsiChar; const ap: array of const): PSvnStringBuf; cdecl;
  svn_stringbuf_ensure: procedure(str: PSvnStringBuf; minimum_size: TAprSize); cdecl;
  svn_stringbuf_set: procedure(str: PSvnStringBuf; value: PAnsiChar); cdecl;
  svn_stringbuf_setempty: procedure(str: PSvnStringBuf); cdecl;
  svn_stringbuf_isempty: function(str: PSvnStringBuf): TSvnBoolean; cdecl;
  svn_stringbuf_chop: procedure(str: PSvnStringBuf; bytes: TAprSize); cdecl;
  svn_stringbuf_fillchar: procedure(str: PSvnStringBuf; c: Byte); cdecl;
  svn_stringbuf_appendbytes: procedure(targetstr: PSvnStringBuf; bytes: PAnsiChar; count: TAprSize); cdecl;
  svn_stringbuf_appendstr: procedure(targetstr, appendstr: PSvnStringBuf); cdecl;
  svn_stringbuf_appendcstr: procedure(targetstr: PSvnStringBuf; cstr: PAnsiChar); cdecl;
  svn_stringbuf_dup: function(original_string: PSvnStringBuf; pool: PAprPool): PSvnStringBuf; cdecl;
  svn_stringbuf_compare: function(str1, str2: PSvnStringBuf): TSvnBoolean; cdecl;
  svn_stringbuf_first_non_whitespace: function(str: PSvnStringBuf): TAprSize; cdecl;
  svn_stringbuf_strip_whitespace: procedure(str: PSvnStringBuf); cdecl;
  svn_stringbuf_find_char_backward: function(str: PSvnStringBuf; ch: Char): TAprSize; cdecl;
  svn_string_compare_stringbuf: function(str1, str2: PSvnStringBuf): TSvnBoolean; cdecl;

  svn_cstring_split: function(input, sep_chars: PAnsiChar; chop_whitespace: TSvnBoolean; pool: PAprPool): PAprArrayHeader;
    cdecl;
  svn_cstring_split_append: procedure(arr: PAprArrayHeader; input, sep_chars: PAnsiChar; chop_whitespace: TSvnBoolean;
    pool: PAprPool); cdecl;
  svn_cstring_match_glob_list: function(str: PAnsiChar; list: PAprArrayHeader): TSvnBoolean; cdecl;
  svn_cstring_count_newlines: function(msg: PAnsiChar): Integer; cdecl;
  svn_cstring_join: function(strings: PAprArrayHeader; separator: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_cstring_casecmp: function(str1, str2: PAnsiChar): Integer; cdecl;

//----- svn_string.h ---------------------------------------------------------------------------------------------------

//----- svn_xml.h ------------------------------------------------------------------------------------------------------

const
  SVN_XML_NAMESPACE = 'svn:';

type
  TSvnXMLOpenTagStyle = (
    svnXMLNormal = 1,
    svnXMLProtectPCData,
    svnXMLSelfClosing
  );
  PSvnXMLParser = ^TSvnXMLParser;
  TSvnXMLParser = THandle;
  TSvnXMLStartElem = procedure(baton: Pointer; name: PAnsiChar; atts: PPAnsiChar); cdecl;
  TSvnXMLEndElem = procedure(baton: Pointer; name: PAnsiChar); cdecl;
  TSvnXMLCharData = procedure(baton: Pointer; data: PAnsiChar; len: TAprSize); cdecl;

var
  svn_xml_is_xml_safe: function(data: PAnsiChar; len: TAprSize): TSvnBoolean; cdecl;
  svn_xml_escape_cdata_stringbuf: procedure(outstr: PSvnStringBuf; str: PSvnStringBuf; pool: PAprPool); cdecl;
  svn_xml_escape_cdata_string: procedure(out outstr: PSvnStringBuf; str: PSvnString; pool: PAprPool); cdecl;
  svn_xml_escape_cdata_cstring: procedure(out outstr: PSvnStringBuf; str: PAnsiChar; pool: PAprPool); cdecl;
  svn_xml_escape_attr_stringbuf: procedure(out outstr: PSvnStringBuf; str: PSvnStringBuf; pool: PAprPool); cdecl;
  svn_xml_escape_attr_string: procedure(out outstr: PSvnStringBuf; str: PSvnString; pool: PAprPool); cdecl;
  svn_xml_escape_attr_cstring: procedure(out outstr: PSvnStringBuf; str: PAnsiChar; pool: PAprPool); cdecl;
  svn_xml_fuzzy_escape: function(str: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_xml_make_parser: function(baton: Pointer; start_handler: TSvnXMLStartElem; end_handler: TSvnXMLEndElem;
    data_handler: TSvnXMLCharData; pool: PAprPool): PSvnXMLParser; cdecl;
  svn_xml_free_parser: procedure(svn_parser: PSvnXMLParser); cdecl;
  svn_xml_parse: function(svn_parser: PSvnXMLParser; buf: PAnsiChar; len: TAprSize; is_final: TSvnBoolean): PSvnError; cdecl;
  svn_xml_signal_bailout: procedure(error: PSvnError; svn_parser: PSvnXMLParser); cdecl;
  svn_xml_get_attr_value: function(name: PAnsiChar; atts: PPAnsiChar): PAnsiChar; cdecl;
  svn_xml_ap_to_hash: function(ap: array of const; pool: PAprPool): PAprHash; cdecl;
  svn_xml_make_att_hash: function(atts: PPAnsiChar; pool: PAprPool): PAprHash; cdecl;
  svn_xml_hash_atts_preserving: procedure(atts: PPAnsiChar; ht: PAprHash; pool: PAprPool); cdecl;
  svn_xml_hash_atts_overlaying: procedure(atts: PPAnsiChar; ht: PAprHash; pool: PAprPool); cdecl;
  svn_xml_make_header: procedure(out str: PSvnStringBuf; pool: PAprPool); cdecl;
  svn_xml_make_open_tag: procedure(out str: PSvnStringBuf; pool: PAprPool; style: TSvnXMLOpenTagStyle;
    tagname: PAnsiChar; const args: array of const); cdecl;
  svn_xml_make_open_tag_v: procedure(out str: PSvnStringBuf; pool: PAprPool; style: TSvnXMLOpenTagStyle; tagname: PAnsiChar;
    const ap: array of const); cdecl;
  svn_xml_make_open_tag_hash: procedure(out str: PSvnStringBuf; pool: PAprPool; style: TSvnXMLOpenTagStyle;
    tagname: PAnsiChar; attributes: PAprHash); cdecl;
  svn_xml_make_close_tag: procedure(out str: PSvnStringBuf; pool: PAprPool; tagname: PAnsiChar); cdecl;

//----- svn_xml.h ------------------------------------------------------------------------------------------------------

//----- svn_utf.h ------------------------------------------------------------------------------------------------------

var
  svn_utf_initialize: procedure(pool: PAprPool); cdecl;
  svn_utf_stringbuf_to_utf8: function(out dest: PSvnStringBuf; src: PSvnStringBuf; pool: PAprPool): PSvnError; cdecl;
  svn_utf_string_to_utf8: function(out dest: PSvnString; src: PSvnString; pool: PAprPool): PSvnError; cdecl;
  svn_utf_cstring_to_utf8: function(dest: PPAnsiChar; src: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_utf_cstring_to_utf8_ex2: function(dest: PPAnsiChar; src, frompage: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_utf_cstring_to_utf8_ex: function(dest: PPAnsiChar; src, frompage, convset_key: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_utf_stringbuf_from_utf8: function(out dest: PSvnStringBuf; src: PSvnStringBuf; pool: PAprPool): PSvnError;
    cdecl;
  svn_utf_string_from_utf8: function(out dest: PSvnString; src: PSvnString; pool: PAprPool): PSvnError; cdecl;
  svn_utf_cstring_from_utf8: function(out dest: PAnsiChar; src: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_utf_cstring_from_utf8_ex2: function(out dest: PAnsiChar; src, topage: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_utf_cstring_from_utf8_ex: function(out dest: PAnsiChar; src, topage, convset_key: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_utf_cstring_from_utf8_fuzzy: function(src: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_utf_cstring_from_utf8_stringbuf: function(out dest: PAnsiChar; src: PSvnStringBuf; pool: PAprPool): PSvnError;
    cdecl;
  svn_utf_cstring_from_utf8_string: function(out dest: PAnsiChar; src: PAnsiChar; pool: PAprPool): PSvnError; cdecl;

//----- svn_utf.h ------------------------------------------------------------------------------------------------------

//----- svn_props.h ----------------------------------------------------------------------------------------------------

type
  PSvnProp = ^TSvnProp;
  TSvnProp = record
    name: PAnsiChar;
    value: PSvnString;
  end;
  TSvnPropKind = (
    svnPropEntryKind,
    svnPropWCKind,
    svnPropRegularKind
  );

var
  svn_prop_dup: function(prop: PSvnProp; pool: PAprPool): PSvnProp; cdecl;
  svn_prop_array_dup: function(arr: PAprArrayHeader; pool: PAprPool): PAprArrayHeader; cdecl;
  svn_prop_hash_to_array: function(hash: PAprHash; pool: PAprPool): PAprArrayHeader; cdecl;
  svn_property_kind: function(prefix_len: PInteger; prop_name: PAnsiChar): TSvnPropKind; cdecl;
  svn_prop_is_svn_prop: function(prop_name: PAnsiChar): TSvnBoolean; cdecl;
  svn_prop_has_svn_prop: function(prop_name: PAnsiChar): TSvnBoolean; cdecl;
  svn_prop_is_boolean: function(prop_name: PAnsiChar): TSvnBoolean; cdecl;
  svn_prop_needs_translation: function(propname: PAnsiChar): TSvnBoolean; cdecl;
  svn_categorize_props: function(proplist: PAprArrayHeader; out entry_props, wc_props, regular_props: PAprArrayHeader;
    pool: PAprPool): PSvnError; cdecl;
  svn_prop_diffs: function(out propdiffs: PAprArrayHeader; target_props, source_props: PAprHash;
    pool: PAprPool): PSvnError; cdecl;
  svn_prop_name_is_valid: function(prop_name: PAnsiChar): TSvnBoolean; cdecl;

const
  SVN_PROP_PREFIX = 'svn:';
  SVN_PROP_BOOLEAN_TRUE = '*';
  SVN_PROP_MIME_TYPE = SVN_PROP_PREFIX + 'mime-type';
  SVN_PROP_IGNORE = SVN_PROP_PREFIX = 'ignore';
  SVN_PROP_EOL_STYLE = SVN_PROP_PREFIX + 'eol-style';
  SVN_PROP_KEYWORDS = SVN_PROP_PREFIX + 'keywords';
  SVN_PROP_EXECUTABLE = SVN_PROP_PREFIX + 'executable';
  SVN_PROP_EXECUTABLE_VALUE = SVN_PROP_BOOLEAN_TRUE;
  SVN_PROP_NEEDS_LOCK = SVN_PROP_PREFIX = 'needs-lock';
  SVN_PROP_NEEDS_LOCK_VALUE = SVN_PROP_BOOLEAN_TRUE;
  SVN_PROP_SPECIAL = SVN_PROP_PREFIX + 'special';
  SVN_PROP_SPECIAL_VALUE = SVN_PROP_BOOLEAN_TRUE;
  SVN_PROP_EXTERNALS = SVN_PROP_PREFIX + 'externals';
  SVN_PROP_MERGEINFO = SVN_PROP_PREFIX + 'mergeinfo';

  SVN_PROP_WC_PREFIX = SVN_PROP_PREFIX + 'wc:';
  SVN_PROP_ENTRY_PREFIX = SVN_PROP_PREFIX + 'entry:';
  SVN_PROP_ENTRY_COMMITTED_REV = SVN_PROP_ENTRY_PREFIX + 'committed-rev';
  SVN_PROP_ENTRY_COMMITTED_DATE = SVN_PROP_ENTRY_PREFIX + 'committed-date';
  SVN_PROP_ENTRY_LAST_AUTHOR = SVN_PROP_ENTRY_PREFIX + 'last-author';
  SVN_PROP_ENTRY_UUID = SVN_PROP_ENTRY_PREFIX + 'uuid';
  SVN_PROP_ENTRY_LOCK_TOKEN = SVN_PROP_ENTRY_PREFIX + 'lock-token';
  SVN_PROP_CUSTOM_PREFIX = SVN_PROP_PREFIX + 'custom:';

  SVN_PROP_REVISION_AUTHOR = SVN_PROP_PREFIX + 'author';
  SVN_PROP_REVISION_LOG = SVN_PROP_PREFIX + 'log';
  SVN_PROP_REVISION_DATE = SVN_PROP_PREFIX + 'date';
  SVN_PROP_REVISION_ORIG_DATE = SVN_PROP_PREFIX + 'original-date';
  SVN_PROP_REVISION_AUTOVERSIONED = SVN_PROP_PREFIX + 'autoversioned';
  SVNSYNC_PROP_PREFIX = SVN_PROP_PREFIX + 'sync-';
  SVNSYNC_PROP_LOCK = SVNSYNC_PROP_PREFIX + 'lock';
  SVNSYNC_PROP_FROM_URL = SVNSYNC_PROP_PREFIX + 'from-url';
  SVNSYNC_PROP_FROM_UUID = SVNSYNC_PROP_PREFIX + 'from-uuid';
  SVNSYNC_PROP_LAST_MERGED_REV = SVNSYNC_PROP_PREFIX + 'last-merged-rev';
  SVNSYNC_PROP_CURRENTLY_COPYING = SVNSYNC_PROP_PREFIX + 'currently-copying';
  SVN_PROP_REVISION_ALL_PROPS: array[0..9] of string = (
    SVN_PROP_REVISION_AUTHOR,
    SVN_PROP_REVISION_LOG,
    SVN_PROP_REVISION_DATE,
    SVN_PROP_REVISION_ORIG_DATE,
    SVN_PROP_REVISION_AUTOVERSIONED,
    SVNSYNC_PROP_LOCK,
    SVNSYNC_PROP_FROM_URL,
    SVNSYNC_PROP_FROM_UUID,
    SVNSYNC_PROP_LAST_MERGED_REV,
    SVNSYNC_PROP_CURRENTLY_COPYING
  );

//----- svn_props.h ----------------------------------------------------------------------------------------------------

//----- svn_iter.h -----------------------------------------------------------------------------------------------------

type
  TSvnIterAprHashCb = function(baton, key: Pointer; klen: TAprSize; val: Pointer; pool: PAprPool): PSvnError; cdecl;
  TSvnIterAprArrayCb = function(baton, item: Pointer; pool: PAprPool): PSvnError; cdecl;

var
  svn_iter_apr_hash: function(out completed: TSvnBoolean; hash: PAprHash; func: TSvnIterAprHashCb; baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_iter_apr_array: function(out completed: TSvnBoolean; _array: PAprArrayHeader; func: TSvnIterAprArrayCb;
    baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_iter__break: function: PSvnError; cdecl;

//----- svn_iter.h -----------------------------------------------------------------------------------------------------

//----- svn_error_codes.h ----------------------------------------------------------------------------------------------

const
  SVN_ERR_CATEGORY_SIZE                           = 5000;
  SVN_ERR_BAD_CATEGORY_START                      = APR_OS_START_USERERR + 1 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_XML_CATEGORY_START                      = APR_OS_START_USERERR + 2 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_IO_CATEGORY_START                       = APR_OS_START_USERERR + 3 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_STREAM_CATEGORY_START                   = APR_OS_START_USERERR + 4 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_NODE_CATEGORY_START                     = APR_OS_START_USERERR + 5 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_ENTRY_CATEGORY_START                    = APR_OS_START_USERERR + 6 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_WC_CATEGORY_START                       = APR_OS_START_USERERR + 7 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_FS_CATEGORY_START                       = APR_OS_START_USERERR + 8 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_REPOS_CATEGORY_START                    = APR_OS_START_USERERR + 9 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_RA_CATEGORY_START                       = APR_OS_START_USERERR + 10 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_RA_DAV_CATEGORY_START                   = APR_OS_START_USERERR + 11 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_RA_LOCAL_CATEGORY_START                 = APR_OS_START_USERERR + 12 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_SVNDIFF_CATEGORY_START                  = APR_OS_START_USERERR + 13 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_APMOD_CATEGORY_START                    = APR_OS_START_USERERR + 14 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_CLIENT_CATEGORY_START                   = APR_OS_START_USERERR + 15 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_MISC_CATEGORY_START                     = APR_OS_START_USERERR + 16 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_CL_CATEGORY_START                       = APR_OS_START_USERERR + 17 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_RA_SVN_CATEGORY_START                   = APR_OS_START_USERERR + 18 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_AUTHN_CATEGORY_START                    = APR_OS_START_USERERR + 19 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_AUTHZ_CATEGORY_START                    = APR_OS_START_USERERR + 20 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_DIFF_CATEGORY_START                     = APR_OS_START_USERERR + 21 * SVN_ERR_CATEGORY_SIZE;
  SVN_ERR_RA_SERF_CATEGORY_START                  = APR_OS_START_USERERR + 22 * SVN_ERR_CATEGORY_SIZE;

  SVN_ERR_BAD_CONTAINING_POOL                     = SVN_ERR_BAD_CATEGORY_START + 0;
  SVN_ERR_BAD_FILENAME                            = SVN_ERR_BAD_CATEGORY_START + 1;
  SVN_ERR_BAD_URL                                 = SVN_ERR_BAD_CATEGORY_START + 2;
  SVN_ERR_BAD_DATE                                = SVN_ERR_BAD_CATEGORY_START + 3;
  SVN_ERR_BAD_MIME_TYPE                           = SVN_ERR_BAD_CATEGORY_START + 4;
  SVN_ERR_BAD_PROPERTY_VALUE                      = SVN_ERR_BAD_CATEGORY_START + 5;
  SVN_ERR_BAD_VERSION_FILE_FORMAT                 = SVN_ERR_BAD_CATEGORY_START + 6;
  SVN_ERR_BAD_RELATIVE_PATH                       = SVN_ERR_BAD_CATEGORY_START + 7;
  SVN_ERR_BAD_UUID                                = SVN_ERR_BAD_CATEGORY_START + 8;

  SVN_ERR_XML_ATTRIB_NOT_FOUND                    = SVN_ERR_XML_CATEGORY_START + 0;
  SVN_ERR_XML_MISSING_ANCESTRY                    = SVN_ERR_XML_CATEGORY_START + 1;
  SVN_ERR_XML_UNKNOWN_ENCODING                    = SVN_ERR_XML_CATEGORY_START + 2;
  SVN_ERR_XML_MALFORMED                           = SVN_ERR_XML_CATEGORY_START + 3;
  SVN_ERR_XML_UNESCAPABLE_DATA                    = SVN_ERR_XML_CATEGORY_START + 4;

  SVN_ERR_IO_INCONSISTENT_EOL                     = SVN_ERR_IO_CATEGORY_START + 0;
  SVN_ERR_IO_UNKNOWN_EOL                          = SVN_ERR_IO_CATEGORY_START + 1;
  SVN_ERR_IO_CORRUPT_EOL                          = SVN_ERR_IO_CATEGORY_START + 2;
  SVN_ERR_IO_UNIQUE_NAMES_EXHAUSTED               = SVN_ERR_IO_CATEGORY_START + 3;
  SVN_ERR_IO_PIPE_FRAME_ERROR                     = SVN_ERR_IO_CATEGORY_START + 4;
  SVN_ERR_IO_PIPE_READ_ERROR                      = SVN_ERR_IO_CATEGORY_START + 5;
  SVN_ERR_IO_WRITE_ERROR                          = SVN_ERR_IO_CATEGORY_START + 6;

  SVN_ERR_STREAM_UNEXPECTED_EOF                   = SVN_ERR_STREAM_CATEGORY_START + 0;
  SVN_ERR_STREAM_MALFORMED_DATA                   = SVN_ERR_STREAM_CATEGORY_START + 1;
  SVN_ERR_STREAM_UNRECOGNIZED_DATA                = SVN_ERR_STREAM_CATEGORY_START + 2;

  SVN_ERR_NODE_UNKNOWN_KIND                       = SVN_ERR_NODE_CATEGORY_START + 0;
  SVN_ERR_NODE_UNEXPECTED_KIND                    = SVN_ERR_NODE_CATEGORY_START + 1;

  SVN_ERR_ENTRY_NOT_FOUND                         = SVN_ERR_ENTRY_CATEGORY_START + 0;
  SVN_ERR_ENTRY_EXISTS                            = SVN_ERR_ENTRY_CATEGORY_START + 2;
  SVN_ERR_ENTRY_MISSING_REVISION                  = SVN_ERR_ENTRY_CATEGORY_START + 3;
  SVN_ERR_ENTRY_MISSING_URL                       = SVN_ERR_ENTRY_CATEGORY_START + 4;
  SVN_ERR_ENTRY_ATTRIBUTE_INVALID                 = SVN_ERR_ENTRY_CATEGORY_START + 5;

  SVN_ERR_WC_OBSTRUCTED_UPDATE                    = SVN_ERR_WC_CATEGORY_START + 0;
  SVN_ERR_WC_UNWIND_MISMATCH                      = SVN_ERR_WC_CATEGORY_START + 1;
  SVN_ERR_WC_UNWIND_EMPTY                         = SVN_ERR_WC_CATEGORY_START + 2;
  SVN_ERR_WC_UNWIND_NOT_EMPTY                     = SVN_ERR_WC_CATEGORY_START + 3;
  SVN_ERR_WC_LOCKED                               = SVN_ERR_WC_CATEGORY_START + 4;
  SVN_ERR_WC_NOT_LOCKED                           = SVN_ERR_WC_CATEGORY_START + 5;
  SVN_ERR_WC_INVALID_LOCK                         = SVN_ERR_WC_CATEGORY_START + 6;
  SVN_ERR_WC_NOT_DIRECTORY                        = SVN_ERR_WC_CATEGORY_START + 7;
  SVN_ERR_WC_NOT_FILE                             = SVN_ERR_WC_CATEGORY_START + 8;
  SVN_ERR_WC_BAD_ADM_LOG                          = SVN_ERR_WC_CATEGORY_START + 9;
  SVN_ERR_WC_PATH_NOT_FOUND                       = SVN_ERR_WC_CATEGORY_START + 10;
  SVN_ERR_WC_NOT_UP_TO_DATE                       = SVN_ERR_WC_CATEGORY_START + 11;
  SVN_ERR_WC_LEFT_LOCAL_MOD                       = SVN_ERR_WC_CATEGORY_START + 12;
  SVN_ERR_WC_SCHEDULE_CONFLICT                    = SVN_ERR_WC_CATEGORY_START + 13;
  SVN_ERR_WC_PATH_FOUND                           = SVN_ERR_WC_CATEGORY_START + 14;
  SVN_ERR_WC_FOUND_CONFLICT                       = SVN_ERR_WC_CATEGORY_START + 15;
  SVN_ERR_WC_CORRUPT                              = SVN_ERR_WC_CATEGORY_START + 16;
  SVN_ERR_WC_CORRUPT_TEXT_BASE                    = SVN_ERR_WC_CATEGORY_START + 17;
  SVN_ERR_WC_NODE_KIND_CHANGE                     = SVN_ERR_WC_CATEGORY_START + 18;
  SVN_ERR_WC_INVALID_OP_ON_CWD                    = SVN_ERR_WC_CATEGORY_START + 19;
  SVN_ERR_WC_BAD_ADM_LOG_START                    = SVN_ERR_WC_CATEGORY_START + 20;
  SVN_ERR_WC_UNSUPPORTED_FORMAT                   = SVN_ERR_WC_CATEGORY_START + 21;
  SVN_ERR_WC_BAD_PATH                             = SVN_ERR_WC_CATEGORY_START + 22;
  SVN_ERR_WC_INVALID_SCHEDULE                     = SVN_ERR_WC_CATEGORY_START + 23;
  SVN_ERR_WC_INVALID_RELOCATION                   = SVN_ERR_WC_CATEGORY_START + 24;
  SVN_ERR_WC_INVALID_SWITCH                       = SVN_ERR_WC_CATEGORY_START + 25;
  SVN_ERR_WC_MISMATCHED_CHANGELIST                = SVN_ERR_WC_CATEGORY_START + 26;
  SVN_ERR_WC_CONFLICT_RESOLVER_FAILURE            = SVN_ERR_WC_CATEGORY_START + 27;
  SVN_ERR_WC_COPYFROM_PATH_NOT_FOUND              = SVN_ERR_WC_CATEGORY_START + 28;
  SVN_ERR_WC_CHANGELIST_MOVE                      = SVN_ERR_WC_CATEGORY_START + 29;

  SVN_ERR_FS_GENERAL                              = SVN_ERR_FS_CATEGORY_START + 0;
  SVN_ERR_FS_CLEANUP                              = SVN_ERR_FS_CATEGORY_START + 1;
  SVN_ERR_FS_ALREADY_OPEN                         = SVN_ERR_FS_CATEGORY_START + 2;
  SVN_ERR_FS_NOT_OPEN                             = SVN_ERR_FS_CATEGORY_START + 3;
  SVN_ERR_FS_CORRUPT                              = SVN_ERR_FS_CATEGORY_START + 4;
  SVN_ERR_FS_PATH_SYNTAX                          = SVN_ERR_FS_CATEGORY_START + 5;
  SVN_ERR_FS_NO_SUCH_REVISION                     = SVN_ERR_FS_CATEGORY_START + 6;
  SVN_ERR_FS_NO_SUCH_TRANSACTION                  = SVN_ERR_FS_CATEGORY_START + 7;
  SVN_ERR_FS_NO_SUCH_ENTRY                        = SVN_ERR_FS_CATEGORY_START + 8;
  SVN_ERR_FS_NO_SUCH_REPRESENTATION               = SVN_ERR_FS_CATEGORY_START + 9;
  SVN_ERR_FS_NO_SUCH_STRING                       = SVN_ERR_FS_CATEGORY_START + 10;
  SVN_ERR_FS_NO_SUCH_COPY                         = SVN_ERR_FS_CATEGORY_START + 11;
  SVN_ERR_FS_TRANSACTION_NOT_MUTABLE              = SVN_ERR_FS_CATEGORY_START + 12;
  SVN_ERR_FS_NOT_FOUND                            = SVN_ERR_FS_CATEGORY_START + 13;
  SVN_ERR_FS_ID_NOT_FOUND                         = SVN_ERR_FS_CATEGORY_START + 14;
  SVN_ERR_FS_NOT_ID                               = SVN_ERR_FS_CATEGORY_START + 15;
  SVN_ERR_FS_NOT_DIRECTORY                        = SVN_ERR_FS_CATEGORY_START + 16;
  SVN_ERR_FS_NOT_FILE                             = SVN_ERR_FS_CATEGORY_START + 17;
  SVN_ERR_FS_NOT_SINGLE_PATH_COMPONENT            = SVN_ERR_FS_CATEGORY_START + 18;
  SVN_ERR_FS_NOT_MUTABLE                          = SVN_ERR_FS_CATEGORY_START + 19;
  SVN_ERR_FS_ALREADY_EXISTS                       = SVN_ERR_FS_CATEGORY_START + 20;
  SVN_ERR_FS_ROOT_DIR                             = SVN_ERR_FS_CATEGORY_START + 21;
  SVN_ERR_FS_NOT_TXN_ROOT                         = SVN_ERR_FS_CATEGORY_START + 22;
  SVN_ERR_FS_NOT_REVISION_ROOT                    = SVN_ERR_FS_CATEGORY_START + 23;
  SVN_ERR_FS_CONFLICT                             = SVN_ERR_FS_CATEGORY_START + 24;
  SVN_ERR_FS_REP_CHANGED                          = SVN_ERR_FS_CATEGORY_START + 25;
  SVN_ERR_FS_REP_NOT_MUTABLE                      = SVN_ERR_FS_CATEGORY_START + 26;
  SVN_ERR_FS_MALFORMED_SKEL                       = SVN_ERR_FS_CATEGORY_START + 27;
  SVN_ERR_FS_TXN_OUT_OF_DATE                      = SVN_ERR_FS_CATEGORY_START + 28;
  SVN_ERR_FS_BERKELEY_DB                          = SVN_ERR_FS_CATEGORY_START + 29;
  SVN_ERR_FS_BERKELEY_DB_DEADLOCK                 = SVN_ERR_FS_CATEGORY_START + 30;
  SVN_ERR_FS_TRANSACTION_DEAD                     = SVN_ERR_FS_CATEGORY_START + 31;
  SVN_ERR_FS_TRANSACTION_NOT_DEAD                 = SVN_ERR_FS_CATEGORY_START + 32;
  SVN_ERR_FS_UNKNOWN_FS_TYPE                      = SVN_ERR_FS_CATEGORY_START + 33;
  SVN_ERR_FS_NO_USER                              = SVN_ERR_FS_CATEGORY_START + 34;
  SVN_ERR_FS_PATH_ALREADY_LOCKED                  = SVN_ERR_FS_CATEGORY_START + 35;
  SVN_ERR_FS_PATH_NOT_LOCKED                      = SVN_ERR_FS_CATEGORY_START + 36;
  SVN_ERR_FS_BAD_LOCK_TOKEN                       = SVN_ERR_FS_CATEGORY_START + 37;
  SVN_ERR_FS_NO_LOCK_TOKEN                        = SVN_ERR_FS_CATEGORY_START + 38;
  SVN_ERR_FS_LOCK_OWNER_MISMATCH                  = SVN_ERR_FS_CATEGORY_START + 39;
  SVN_ERR_FS_NO_SUCH_LOCK                         = SVN_ERR_FS_CATEGORY_START + 40;
  SVN_ERR_FS_LOCK_EXPIRED                         = SVN_ERR_FS_CATEGORY_START + 41;
  SVN_ERR_FS_OUT_OF_DATE                          = SVN_ERR_FS_CATEGORY_START + 42;
  SVN_ERR_FS_UNSUPPORTED_FORMAT                   = SVN_ERR_FS_CATEGORY_START + 43;
  SVN_ERR_FS_REP_BEING_WRITTEN                    = SVN_ERR_FS_CATEGORY_START + 44;
  SVN_ERR_FS_TXN_NAME_TOO_LONG                    = SVN_ERR_FS_CATEGORY_START + 45;
  SVN_ERR_FS_NO_SUCH_NODE_ORIGIN                  = SVN_ERR_FS_CATEGORY_START + 46;
  SVN_ERR_FS_UNSUPPORTED_UPGRADE                  = SVN_ERR_FS_CATEGORY_START + 47;

  SVN_ERR_REPOS_LOCKED                            = SVN_ERR_REPOS_CATEGORY_START + 0;
  SVN_ERR_REPOS_HOOK_FAILURE                      = SVN_ERR_REPOS_CATEGORY_START + 1;
  SVN_ERR_REPOS_BAD_ARGS                          = SVN_ERR_REPOS_CATEGORY_START + 2;
  SVN_ERR_REPOS_NO_DATA_FOR_REPORT                = SVN_ERR_REPOS_CATEGORY_START + 3;
  SVN_ERR_REPOS_BAD_REVISION_REPORT               = SVN_ERR_REPOS_CATEGORY_START + 4;
  SVN_ERR_REPOS_UNSUPPORTED_VERSION               = SVN_ERR_REPOS_CATEGORY_START + 5;
  SVN_ERR_REPOS_DISABLED_FEATURE                  = SVN_ERR_REPOS_CATEGORY_START + 6;
  SVN_ERR_REPOS_POST_COMMIT_HOOK_FAILED           = SVN_ERR_REPOS_CATEGORY_START + 7;
  SVN_ERR_REPOS_POST_LOCK_HOOK_FAILED             = SVN_ERR_REPOS_CATEGORY_START + 8;
  SVN_ERR_REPOS_POST_UNLOCK_HOOK_FAILED           = SVN_ERR_REPOS_CATEGORY_START + 9;
  SVN_ERR_REPOS_UNSUPPORTED_UPGRADE               = SVN_ERR_REPOS_CATEGORY_START + 10;

  SVN_ERR_RA_ILLEGAL_URL                          = SVN_ERR_RA_CATEGORY_START + 0;
  SVN_ERR_RA_NOT_AUTHORIZED                       = SVN_ERR_RA_CATEGORY_START + 1;
  SVN_ERR_RA_UNKNOWN_AUTH                         = SVN_ERR_RA_CATEGORY_START + 2;
  SVN_ERR_RA_NOT_IMPLEMENTED                      = SVN_ERR_RA_CATEGORY_START + 3;
  SVN_ERR_RA_OUT_OF_DATE                          = SVN_ERR_RA_CATEGORY_START + 4;
  SVN_ERR_RA_NO_REPOS_UUID                        = SVN_ERR_RA_CATEGORY_START + 5;
  SVN_ERR_RA_UNSUPPORTED_ABI_VERSION              = SVN_ERR_RA_CATEGORY_START + 6;
  SVN_ERR_RA_NOT_LOCKED                           = SVN_ERR_RA_CATEGORY_START + 7;
  SVN_ERR_RA_PARTIAL_REPLAY_NOT_SUPPORTED         = SVN_ERR_RA_CATEGORY_START + 8;
  SVN_ERR_RA_UUID_MISMATCH                        = SVN_ERR_RA_CATEGORY_START + 9;

  SVN_ERR_RA_DAV_SOCK_INIT                        = SVN_ERR_RA_DAV_CATEGORY_START + 0;
  SVN_ERR_RA_DAV_CREATING_REQUEST                 = SVN_ERR_RA_DAV_CATEGORY_START + 1;
  SVN_ERR_RA_DAV_REQUEST_FAILED                   = SVN_ERR_RA_DAV_CATEGORY_START + 2;
  SVN_ERR_RA_DAV_OPTIONS_REQ_FAILED               = SVN_ERR_RA_DAV_CATEGORY_START + 3;
  SVN_ERR_RA_DAV_PROPS_NOT_FOUND                  = SVN_ERR_RA_DAV_CATEGORY_START + 4;
  SVN_ERR_RA_DAV_ALREADY_EXISTS                   = SVN_ERR_RA_DAV_CATEGORY_START + 5;
  SVN_ERR_RA_DAV_INVALID_CONFIG_VALUE             = SVN_ERR_RA_DAV_CATEGORY_START + 6;
  SVN_ERR_RA_DAV_PATH_NOT_FOUND                   = SVN_ERR_RA_DAV_CATEGORY_START + 7;
  SVN_ERR_RA_DAV_PROPPATCH_FAILED                 = SVN_ERR_RA_DAV_CATEGORY_START + 8;
  SVN_ERR_RA_DAV_MALFORMED_DATA                   = SVN_ERR_RA_DAV_CATEGORY_START + 9;
  SVN_ERR_RA_DAV_RESPONSE_HEADER_BADNESS          = SVN_ERR_RA_DAV_CATEGORY_START + 10;
  SVN_ERR_RA_DAV_RELOCATED                        = SVN_ERR_RA_DAV_CATEGORY_START + 11;

  SVN_ERR_RA_LOCAL_REPOS_NOT_FOUND                = SVN_ERR_RA_LOCAL_CATEGORY_START + 0;
  SVN_ERR_RA_LOCAL_REPOS_OPEN_FAILED              = SVN_ERR_RA_LOCAL_CATEGORY_START + 1;

  SVN_ERR_RA_SVN_CMD_ERR                          = SVN_ERR_RA_SVN_CATEGORY_START + 0;
  SVN_ERR_RA_SVN_UNKNOWN_CMD                      = SVN_ERR_RA_SVN_CATEGORY_START + 1;
  SVN_ERR_RA_SVN_CONNECTION_CLOSED                = SVN_ERR_RA_SVN_CATEGORY_START + 2;
  SVN_ERR_RA_SVN_IO_ERROR                         = SVN_ERR_RA_SVN_CATEGORY_START + 3;
  SVN_ERR_RA_SVN_MALFORMED_DATA                   = SVN_ERR_RA_SVN_CATEGORY_START + 4;
  SVN_ERR_RA_SVN_REPOS_NOT_FOUND                  = SVN_ERR_RA_SVN_CATEGORY_START + 5;
  SVN_ERR_RA_SVN_BAD_VERSION                      = SVN_ERR_RA_SVN_CATEGORY_START + 6;
  SVN_ERR_RA_SVN_NO_MECHANISMS                    = SVN_ERR_RA_SVN_CATEGORY_START + 7;

  SVN_ERR_RA_SERF_SSPI_INITIALIZATION_FAILED      = SVN_ERR_RA_SERF_CATEGORY_START + 0;
  SVN_ERR_RA_SERF_SSL_CERT_UNTRUSTED              = SVN_ERR_RA_SERF_CATEGORY_START + 1;

  SVN_ERR_AUTHN_CREDS_UNAVAILABLE                 = SVN_ERR_AUTHN_CATEGORY_START + 0;
  SVN_ERR_AUTHN_NO_PROVIDER                       = SVN_ERR_AUTHN_CATEGORY_START + 1;
  SVN_ERR_AUTHN_PROVIDERS_EXHAUSTED               = SVN_ERR_AUTHN_CATEGORY_START + 2;
  SVN_ERR_AUTHN_CREDS_NOT_SAVED                   = SVN_ERR_AUTHN_CATEGORY_START + 3;
  SVN_ERR_AUTHN_FAILED                            = SVN_ERR_AUTHN_CATEGORY_START + 4;

  SVN_ERR_AUTHZ_ROOT_UNREADABLE                   = SVN_ERR_AUTHZ_CATEGORY_START + 0;
  SVN_ERR_AUTHZ_UNREADABLE                        = SVN_ERR_AUTHZ_CATEGORY_START + 1;
  SVN_ERR_AUTHZ_PARTIALLY_READABLE                = SVN_ERR_AUTHZ_CATEGORY_START + 2;
  SVN_ERR_AUTHZ_INVALID_CONFIG                    = SVN_ERR_AUTHZ_CATEGORY_START + 3;
  SVN_ERR_AUTHZ_UNWRITABLE                        = SVN_ERR_AUTHZ_CATEGORY_START + 4;

  SVN_ERR_SVNDIFF_INVALID_HEADER                  = SVN_ERR_SVNDIFF_CATEGORY_START + 0;
  SVN_ERR_SVNDIFF_CORRUPT_WINDOW                  = SVN_ERR_SVNDIFF_CATEGORY_START + 1;
  SVN_ERR_SVNDIFF_BACKWARD_VIEW                   = SVN_ERR_SVNDIFF_CATEGORY_START + 2;
  SVN_ERR_SVNDIFF_INVALID_OPS                     = SVN_ERR_SVNDIFF_CATEGORY_START + 3;
  SVN_ERR_SVNDIFF_UNEXPECTED_END                  = SVN_ERR_SVNDIFF_CATEGORY_START + 4;
  SVN_ERR_SVNDIFF_INVALID_COMPRESSED_DATA         = SVN_ERR_SVNDIFF_CATEGORY_START + 5;

  SVN_ERR_DIFF_DATASOURCE_MODIFIED                = SVN_ERR_DIFF_CATEGORY_START + 0;

  SVN_ERR_APMOD_MISSING_PATH_TO_FS                = SVN_ERR_APMOD_CATEGORY_START + 0;
  SVN_ERR_APMOD_MALFORMED_URI                     = SVN_ERR_APMOD_CATEGORY_START + 1;
  SVN_ERR_APMOD_ACTIVITY_NOT_FOUND                = SVN_ERR_APMOD_CATEGORY_START + 2;
  SVN_ERR_APMOD_BAD_BASELINE                      = SVN_ERR_APMOD_CATEGORY_START + 3;
  SVN_ERR_APMOD_CONNECTION_ABORTED                = SVN_ERR_APMOD_CATEGORY_START + 4;

  SVN_ERR_CLIENT_VERSIONED_PATH_REQUIRED          = SVN_ERR_CLIENT_CATEGORY_START + 0;
  SVN_ERR_CLIENT_RA_ACCESS_REQUIRED               = SVN_ERR_CLIENT_CATEGORY_START + 1;
  SVN_ERR_CLIENT_BAD_REVISION                     = SVN_ERR_CLIENT_CATEGORY_START + 2;
  SVN_ERR_CLIENT_DUPLICATE_COMMIT_URL             = SVN_ERR_CLIENT_CATEGORY_START + 3;
  SVN_ERR_CLIENT_IS_BINARY_FILE                   = SVN_ERR_CLIENT_CATEGORY_START + 4;
  SVN_ERR_CLIENT_INVALID_EXTERNALS_DESCRIPTION    = SVN_ERR_CLIENT_CATEGORY_START + 5;
  SVN_ERR_CLIENT_MODIFIED                         = SVN_ERR_CLIENT_CATEGORY_START + 6;
  SVN_ERR_CLIENT_IS_DIRECTORY                     = SVN_ERR_CLIENT_CATEGORY_START + 7;
  SVN_ERR_CLIENT_REVISION_RANGE                   = SVN_ERR_CLIENT_CATEGORY_START + 8;
  SVN_ERR_CLIENT_INVALID_RELOCATION               = SVN_ERR_CLIENT_CATEGORY_START + 9;
  SVN_ERR_CLIENT_REVISION_AUTHOR_CONTAINS_NEWLINE = SVN_ERR_CLIENT_CATEGORY_START + 10;
  SVN_ERR_CLIENT_PROPERTY_NAME                    = SVN_ERR_CLIENT_CATEGORY_START + 11;
  SVN_ERR_CLIENT_UNRELATED_RESOURCES              = SVN_ERR_CLIENT_CATEGORY_START + 12;
  SVN_ERR_CLIENT_MISSING_LOCK_TOKEN               = SVN_ERR_CLIENT_CATEGORY_START + 13;
  SVN_ERR_CLIENT_MULTIPLE_SOURCES_DISALLOWED      = SVN_ERR_CLIENT_CATEGORY_START + 14;
  SVN_ERR_CLIENT_NO_VERSIONED_PARENT              = SVN_ERR_CLIENT_CATEGORY_START + 15;
  SVN_ERR_CLIENT_NOT_READY_TO_MERGE               = SVN_ERR_CLIENT_CATEGORY_START + 16;

  SVN_ERR_BASE                                    = SVN_ERR_MISC_CATEGORY_START + 0;
  SVN_ERR_PLUGIN_LOAD_FAILURE                     = SVN_ERR_MISC_CATEGORY_START + 1;
  SVN_ERR_MALFORMED_FILE                          = SVN_ERR_MISC_CATEGORY_START + 2;
  SVN_ERR_INCOMPLETE_DATA                         = SVN_ERR_MISC_CATEGORY_START + 3;
  SVN_ERR_INCORRECT_PARAMS                        = SVN_ERR_MISC_CATEGORY_START + 4;
  SVN_ERR_UNVERSIONED_RESOURCE                    = SVN_ERR_MISC_CATEGORY_START + 5;
  SVN_ERR_TEST_FAILED                             = SVN_ERR_MISC_CATEGORY_START + 6;
  SVN_ERR_UNSUPPORTED_FEATURE                     = SVN_ERR_MISC_CATEGORY_START + 7;
  SVN_ERR_BAD_PROP_KIND                           = SVN_ERR_MISC_CATEGORY_START + 8;
  SVN_ERR_ILLEGAL_TARGET                          = SVN_ERR_MISC_CATEGORY_START + 9;
  SVN_ERR_DELTA_MD5_CHECKSUM_ABSENT               = SVN_ERR_MISC_CATEGORY_START + 10;
  SVN_ERR_DIR_NOT_EMPTY                           = SVN_ERR_MISC_CATEGORY_START + 11;
  SVN_ERR_EXTERNAL_PROGRAM                        = SVN_ERR_MISC_CATEGORY_START + 12;
  SVN_ERR_SWIG_PY_EXCEPTION_SET                   = SVN_ERR_MISC_CATEGORY_START + 13;
  SVN_ERR_CHECKSUM_MISMATCH                       = SVN_ERR_MISC_CATEGORY_START + 14;
  SVN_ERR_CANCELLED                               = SVN_ERR_MISC_CATEGORY_START + 15;
  SVN_ERR_INVALID_DIFF_OPTION                     = SVN_ERR_MISC_CATEGORY_START + 16;
  SVN_ERR_PROPERTY_NOT_FOUND                      = SVN_ERR_MISC_CATEGORY_START + 17;
  SVN_ERR_NO_AUTH_FILE_PATH                       = SVN_ERR_MISC_CATEGORY_START + 18;
  SVN_ERR_VERSION_MISMATCH                        = SVN_ERR_MISC_CATEGORY_START + 19;
  SVN_ERR_MERGEINFO_PARSE_ERROR                   = SVN_ERR_MISC_CATEGORY_START + 20;
  SVN_ERR_CEASE_INVOCATION                        = SVN_ERR_MISC_CATEGORY_START + 21;
  SVN_ERR_REVNUM_PARSE_FAILURE                    = SVN_ERR_MISC_CATEGORY_START + 22;
  SVN_ERR_ITER_BREAK                              = SVN_ERR_MISC_CATEGORY_START + 23;
  SVN_ERR_UNKNOWN_CHANGELIST                      = SVN_ERR_MISC_CATEGORY_START + 24;
  SVN_ERR_RESERVED_FILENAME_SPECIFIED             = SVN_ERR_MISC_CATEGORY_START + 25;
  SVN_ERR_UNKNOWN_CAPABILITY                      = SVN_ERR_MISC_CATEGORY_START + 26;

  SVN_ERR_CL_ARG_PARSING_ERROR                    = SVN_ERR_CL_CATEGORY_START + 0;
  SVN_ERR_CL_INSUFFICIENT_ARGS                    = SVN_ERR_CL_CATEGORY_START + 1;
  SVN_ERR_CL_MUTUALLY_EXCLUSIVE_ARGS              = SVN_ERR_CL_CATEGORY_START + 2;
  SVN_ERR_CL_ADM_DIR_RESERVED                     = SVN_ERR_CL_CATEGORY_START + 3;
  SVN_ERR_CL_LOG_MESSAGE_IS_VERSIONED_FILE        = SVN_ERR_CL_CATEGORY_START + 4;
  SVN_ERR_CL_LOG_MESSAGE_IS_PATHNAME              = SVN_ERR_CL_CATEGORY_START + 5;
  SVN_ERR_CL_COMMIT_IN_ADDED_DIR                  = SVN_ERR_CL_CATEGORY_START + 6;
  SVN_ERR_CL_NO_EXTERNAL_EDITOR                   = SVN_ERR_CL_CATEGORY_START + 7;
  SVN_ERR_CL_BAD_LOG_MESSAGE                      = SVN_ERR_CL_CATEGORY_START + 8;
  SVN_ERR_CL_UNNECESSARY_LOG_MESSAGE              = SVN_ERR_CL_CATEGORY_START + 9;
  SVN_ERR_CL_NO_EXTERNAL_MERGE_TOOL               = SVN_ERR_CL_CATEGORY_START + 10;
  
//----- svn_error_codes.h ----------------------------------------------------------------------------------------------

//----- svn_error.h ----------------------------------------------------------------------------------------------------

const
  SVN_NO_ERROR   = 0;

var
  svn_error__locate: procedure(afile: PAnsiChar; line: Longint); cdecl;
  svn_strerror: function(statcode: TAprStatus; buf: PAnsiChar; bufsize: TAprSize): PAnsiChar; cdecl;
  svn_err_best_message: function(err: PSvnError; buf: PAnsiChar; bufsize: TAprSize): PAnsiChar; cdecl;
  svn_error_create: function(apr_err: TAprStatus; child: PSvnError; message: PAnsiChar): PSvnError; cdecl;
  svn_error_createf: function(apr_err: TAprStatus; child: PSvnError; fmt: PAnsiChar;
    const args: array of const): PSvnError; cdecl;
  svn_error_wrap_apr: function(status: TAprStatus; fmt: PAnsiChar; const args: array of const): PSvnError; cdecl;
  svn_error_quick_wrap: function(child: PSvnError; new_msg: PAnsiChar): PSvnError; cdecl;
  svn_error_compose: procedure(chain: PSvnError; new_err: pSvnError); cdecl;
  svn_error_root_cause: function(err: PSvnError): PSvnError; cdecl;
  svn_error_dup: function(err: PSvnError): PSvnError; cdecl;
  svn_error_clear: procedure(error: PSvnError); cdecl;
  svn_handle_error2: procedure(error: PSvnError; stream: THandle; fatal: TSvnBoolean; prefix: PAnsiChar); cdecl;
  svn_handle_error: procedure(error: PSvnError; stream: THandle; fatal: TSvnBoolean); cdecl;
  svn_handle_warning2: procedure(stream: THandle; error: PSvnError; prefix: PAnsiChar); cdecl;
  svn_handle_warning: procedure(stream: THandle; error: PSvnError); cdecl;

function SvnIsLockError(err: TSvnError): Boolean;
function SvnIsUnlockError(err: TSvnError): Boolean;

//----- svn_error.h ----------------------------------------------------------------------------------------------------

//----- svn_path.h -----------------------------------------------------------------------------------------------------

var
  svn_path_internal_style: function(path: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_path_local_style: function(path: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_path_join: function(base, component: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_path_join_many: function(pool: PAprPool; const base: array of PAnsiChar): PAnsiChar; cdecl;
  svn_path_basename: function(path: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_path_dirname: function(path: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_path_splitext: procedure(out path_root, path_ext: PAnsiChar; path: PAnsiChar; pool: PAprPool); cdecl;
  svn_path_component_count: function(path: PAnsiChar): TAprSize; cdecl;
  svn_path_add_component: procedure(path: PSvnStringBuf; component: PAnsiChar); cdecl;
  svn_path_remove_component: procedure(path: PSvnStringBuf); cdecl;
  svn_path_remove_components: procedure(path: PSvnStringBuf; n: TAprSize); cdecl;
  svn_path_split: procedure(path: PAnsiChar; out dirpath, base_name: PAnsiChar; pool: PAprPool); cdecl;
  svn_path_is_empty: function(path: PAnsiChar): LongBool; cdecl;
  svn_dirent_is_root: function(dirent: PAnsiChar; len: TAprSize): TSvnBoolean; cdecl;
  svn_path_canonicalize: function(path: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_path_is_canonical: function(path: PAnsiChar; pool: PAprPool): TSvnBoolean; cdecl;
  svn_path_compare_paths: function(path1, path2: PAnsiChar): Integer; cdecl;
  svn_path_get_longest_ancestor: function(path1, path2: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_path_get_absolute: function(out pabsolute: PAnsiChar; relative: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_path_split_if_file: function(path: PAnsiChar; out pdirectory, pfile: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_path_condense_targets: function(out pcommon: PAnsiChar; out pcondensed_targets: PAprArrayHeader;
    targets: PAprArrayHeader; remove_redundancies: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_path_remove_redundancies: function(out pcondensed_targets: PAprArrayHeader; targets: PAprArrayHeader;
    pool: PAprPool): PSvnError; cdecl;
  svn_path_decompose: function(path: PAnsiChar; pool: PAprPool): PAprArrayHeader; cdecl;
  svn_path_compose: function(components: PAprArrayHeader; pool: PAprPool): PAnsiChar; cdecl;
  svn_path_is_single_path_component: function(name: PAnsiChar): TSvnBoolean; cdecl;
  svn_path_is_backpath_present: function(path: PAnsiChar): TSvnBoolean; cdecl;
  svn_path_is_child: function(path1, path2: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_path_is_ancestor: function(path1, path2: PAnsiChar): TSvnBoolean; cdecl;
  svn_path_check_valid: function(path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_path_is_url: function(path: PAnsiChar): TSvnBoolean; cdecl;
  svn_path_is_uri_safe: function(path: PAnsiChar): TSvnBoolean; cdecl;
  svn_path_uri_encode: function(path: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_path_uri_decode: function(path: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_path_url_add_component: function(url, component: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_path_uri_from_iri: function(iri: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_path_uri_autoescape: function(uri: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_path_cstring_from_utf8: function(out path_apr: PAnsiChar; path_utf8: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_path_cstring_to_utf8: function(out path_utf8: PAnsiChar; path_apr: PAnsiChar; pool: PAprPool): PSvnError; cdecl;

//----- svn_path.h -----------------------------------------------------------------------------------------------------

//----- svn_config.h ---------------------------------------------------------------------------------------------------

const
  SVN_CONFIG_CATEGORY_SERVERS = 'servers';
  SVN_CONFIG_SECTION_GROUPS = 'groups';
  SVN_CONFIG_SECTION_GLOBAL = 'global';
  SVN_CONFIG_OPTION_HTTP_PROXY_HOST = 'http-proxy-host';
  SVN_CONFIG_OPTION_HTTP_PROXY_PORT = 'http-proxy-port';
  SVN_CONFIG_OPTION_HTTP_PROXY_USERNAME = 'http-proxy-username';
  SVN_CONFIG_OPTION_HTTP_PROXY_PASSWORD = 'http-proxy-password';
  SVN_CONFIG_OPTION_HTTP_PROXY_EXCEPTIONS = 'http-proxy-exceptions';
  SVN_CONFIG_OPTION_HTTP_TIMEOUT = 'http-timeout';
  SVN_CONFIG_OPTION_HTTP_COMPRESSION = 'http-compression';
  SVN_CONFIG_OPTION_NEON_DEBUG_MASK = 'neon-debug-mask';
  SVN_CONFIG_OPTION_HTTP_AUTH_TYPES = 'http-auth-types';
  SVN_CONFIG_OPTION_SSL_AUTHORITY_FILES = 'ssl-authority-files';
  SVN_CONFIG_OPTION_SSL_TRUST_DEFAULT_CA = 'ssl-trust-default-ca';
  SVN_CONFIG_OPTION_SSL_CLIENT_CERT_FILE = 'ssl-client-cert-file';
  SVN_CONFIG_OPTION_SSL_CLIENT_CERT_PASSWORD = 'ssl-client-cert-password';
  SVN_CONFIG_OPTION_SSL_PKCS11_PROVIDER = 'ssl-pkcs11-provider';
  SVN_CONFIG_OPTION_HTTP_LIBRARY = 'http-library';

  SVN_CONFIG_CATEGORY_CONFIG = 'config';
  SVN_CONFIG_SECTION_AUTH = 'auth';
  SVN_CONFIG_OPTION_STORE_PASSWORDS = 'store-passwords';
  SVN_CONFIG_OPTION_STORE_AUTH_CREDS = 'store-auth-creds';
  SVN_CONFIG_SECTION_HELPERS = 'helpers';
  SVN_CONFIG_OPTION_EDITOR_CMD = 'editor-cmd';
  SVN_CONFIG_OPTION_DIFF_CMD = 'diff-cmd';
  SVN_CONFIG_OPTION_DIFF3_CMD = 'diff3-cmd';
  SVN_CONFIG_OPTION_DIFF3_HAS_PROGRAM_ARG = 'diff3-has-program-arg';
  SVN_CONFIG_OPTION_MERGE_TOOL_CMD = 'merge-tool-cmd';
  SVN_CONFIG_SECTION_MISCELLANY = 'miscellany';
  SVN_CONFIG_OPTION_GLOBAL_IGNORES = 'global-ignores';
  SVN_CONFIG_OPTION_LOG_ENCODING = 'log-encoding';
  SVN_CONFIG_OPTION_USE_COMMIT_TIMES = 'use-commit-times';
  SVN_CONFIG_OPTION_TEMPLATE_ROOT = 'template-root';
  SVN_CONFIG_OPTION_ENABLE_AUTO_PROPS = 'enable-auto-props';
  SVN_CONFIG_OPTION_NO_UNLOCK = 'no-unlock';
  SVN_CONFIG_OPTION_MIMETYPES_FILE = 'mime-types-file';
  SVN_CONFIG_OPTION_PRESERVED_CF_EXTS = 'preserved-conflict-file-exts';
  SVN_CONFIG_OPTION_INTERACTIVE_CONFLICTS = 'interactive-conflicts';
  SVN_CONFIG_SECTION_TUNNELS = 'tunnels';
  SVN_CONFIG_SECTION_AUTO_PROPS = 'auto-props';

  SVN_CONFIG_SECTION_GENERAL = 'general';
  SVN_CONFIG_OPTION_ANON_ACCESS = 'anon-access';
  SVN_CONFIG_OPTION_AUTH_ACCESS = 'auth-access';
  SVN_CONFIG_OPTION_PASSWORD_DB = 'password-db';
  SVN_CONFIG_OPTION_REALM = 'realm';
  SVN_CONFIG_OPTION_AUTHZ_DB = 'authz-db';
  SVN_CONFIG_SECTION_SASL = 'sasl';
  SVN_CONFIG_OPTION_USE_SASL = 'use-sasl';
  SVN_CONFIG_OPTION_MIN_SSF = 'min-encryption';
  SVN_CONFIG_OPTION_MAX_SSF = 'max-encryption';

  SVN_CONFIG_SECTION_USERS = 'users';

  SVN_CONFIG_DEFAULT_GLOBAL_IGNORES = '*.o *.lo *.la #*# .*.rej *.rej .*~ *~ .#* .DS_Store';

  SVN_CONFIG_TRUE = 'true';
  SVN_CONFIG_FALSE = 'false';

  SVN_CONFIG_REALMSTRING_KEY = 'svn:realmstring';

type
  PSvnConfig = ^TSvnConfig;
  TSvnConfig = THandle;
  TSvnConfigSectionEnumerator = function(name: PAnsiChar; baton: Pointer): TSvnBoolean; cdecl;
  TSvnConfigSectionEnumerator2 = function(name: PAnsiChar; baton: Pointer; pool: PAprPool): TSvnBoolean; cdecl;
  TSvnConfigEnumerator = function(name, value: PAnsiChar; baton: Pointer): TSvnBoolean; cdecl;
  TSvnConfigEnumerator2 = function(name, value: PAnsiChar; baton: Pointer; pool: PAprPool): TSvnBoolean; cdecl;

var
  svn_config_get_config: function(out cfg_hash: PAprHash; config_dir: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_config_read: function(out cfgp: PSvnConfig; afile: PAnsiChar; must_exist: TSvnBoolean; pool: PAprPool): PSvnError;
    cdecl;
  svn_config_merge: function(cfg: PSvnConfig; afile: PAnsiChar; must_exist: TSvnBoolean): PSvnError; cdecl;
  svn_config_get: procedure(cfg: PSvnConfig; out valuep: PAnsiChar; section, option, default_value: PAnsiChar); cdecl;
  svn_config_set: procedure(cfg: PSvnConfig; section, option, value: PAnsiChar); cdecl;
  svn_config_get_bool: function(cfg: PSvnConfig; out valuep: TSvnBoolean; section, option: PAnsiChar;
    default_value: TSvnBoolean): PSvnError; cdecl;
  svn_config_set_bool: procedure(cfg: PSvnConfig; section, option: PAnsiChar; value: TSvnBoolean); cdecl;
  svn_config_enumerate_sections: function(cfg: PSvnConfig; callback: TSvnConfigSectionEnumerator;
    baton: Pointer): Integer; cdecl;
  svn_config_enumerate_sections2: function(cfg: PSvnConfig; callback: TSvnConfigSectionEnumerator2; baton: Pointer;
    pool: PAprPool): Integer; cdecl;
  svn_config_enumerate: function(cfg: PSvnConfig; section: PAnsiChar; callback: TSvnConfigEnumerator;
    baton: Pointer): Integer; cdecl;
  svn_config_enumerate2: function(cfg: PSvnConfig; section: PAnsiChar; callback: TSvnConfigEnumerator2; baton: Pointer;
    pool: PAprPool): Integer; cdecl;
  svn_config_has_section: function(cfg: PSvnConfig; section: PAnsiChar): TSvnBoolean; cdecl;
  svn_config_find_group: function(cfg: PSvnConfig; key, master_section: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_config_get_server_setting: function(cfg: PSvnConfig; server_group, option_name, default_value: PAnsiChar): PAnsiChar;
    cdecl;
  svn_config_get_server_setting_int: function(cfg: PSvnConfig; server_group, option_name: PAnsiChar; default_value: Int64;
    out result_value: Int64; pool: PAprPool): PSvnError; cdecl;
  svn_config_ensure: function(config_dir: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_config_read_auth_data: function(out hash: PAprHash; cred_kind, realmstring, config_dir: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_config_write_auth_data: function(hash: PAprHash; cred_kind, realmstring, config_dir: PAnsiChar;
    pool: PAprPool): PSvnError; stdcall;

//----- svn_config.h ---------------------------------------------------------------------------------------------------

//----- svn_mergeinfo.h ------------------------------------------------------------------------------------------------

const
  SVN_MERGEINFO_NONINHERITABLE_STR = '*';

type
  TSvnMergeInfo = PAprHash;
  TSvnMergeInfoCatalog = PAprHash;
  TSvnMergeInfoInheritance = (
    svnMergeInfoExplicit = 0,
    svnMergeInfoInherited = 1,
    svnMergeInfoNearest_ancestor = 2
  );

var
  svn_mergeinfo_parse: function(out mergeinfo: TSvnMergeInfo; input: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_mergeinfo_diff: function(out deleted, added: TSvnMergeInfo; mergefrom, mergeto: TSvnMergeInfo;
    consider_inheritance: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_mergeinfo_merge: function(mergeinfo, changes: TSvnMergeInfo; pool: PAprPool): PSvnError; cdecl;
  svn_mergeinfo_remove: function(out mergeinfo: TSvnMergeInfo; eraser, whiteboard: TSvnMergeInfo;
    pool: PAprPool): PSvnError; cdecl;
  svn_rangelist_diff: function(out deleted, added: PAprArrayHeader; range_from, range_to: PAprArrayHeader;
    consider_inheritance: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_rangelist_merge: function(out rangelist: PAprArrayHeader; changes: PAprArrayHeader; pool: PAprPool): PSvnError;
    cdecl;
  svn_rangelist_remove: function(out output: PAprArrayHeader; eraser, whiteboard: PAprArrayHeader;
    consider_inheritance: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_mergeinfo_intersect: function(out mergeinfo: TSvnMergeInfo; mergeinfo1, mergeinfo2: TSvnMergeInfo;
    pool: PAprPool): PSvnError; cdecl;
  svn_rangelist_intersect: function(out rangelist: PAprArrayHeader; rangelist1, rangelist2: PAprArrayHeader;
    consider_inheritance: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_rangelist_reverse: function(var rangelist: PAprArrayHeader; pool: PAprPool): PSvnError; cdecl;
  svn_rangelist_to_string: function(out output: PSvnString; rangelist: PAprArrayHeader; pool: PAprPool): PSvnError;
    cdecl;
  svn_rangelist_inheritable: function(out inheritable_rangelist: PAprArrayHeader; rangelist: PAprArrayHeader;
    rev_start, rev_end: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_mergeinfo_inheritable: function(out inheritable_mergeinfo: TSvnMergeInfo; mergeinfo: TSvnMergeInfo;
    path: PAnsiChar; rev_start, rev_end: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_mergeinfo_to_string: function(out output: PSvnString; mergeinput: TSvnMergeInfo; pool: PAprPool): PSvnError;
    cdecl;
  svn_mergeinfo_sort: function(mergeinfo: TSvnMergeInfo; pool: PAprPool): PSvnError; cdecl;
  svn_mergeinfo_dup: function(mergeinfo: TSvnMergeInfo; pool: PAprPool): TSvnMergeInfo; cdecl;
  svn_rangelist_dup: function(rangelist: PAprArrayHeader; pool: PAprPool): PAprArrayHeader; cdecl;
  svn_inheritance_to_word: function(inherit: TSvnMergeInfoInheritance): PAnsiChar; cdecl;
  svn_inheritance_from_word: function(word: PAnsiChar): TSvnMergeInfoInheritance; cdecl;

//----- svn_mergeinfo.h ------------------------------------------------------------------------------------------------

//----- svn_dso.h ------------------------------------------------------------------------------------------------------

var
  svn_dso_initialize: procedure; cdecl;
  svn_dso_load: function(out dso: PAprDSOHandle; libname: PAnsiChar): PSvnError; cdecl;

//----- svn_dso.h ------------------------------------------------------------------------------------------------------

//----- svn_io.h -------------------------------------------------------------------------------------------------------

type
  TSvnIOFileDel = (
    svnFileDelNone,
    svnFileDelOnClose,
    svnFileDelOnPoolCleanup
  );
  PSvnIODirEnt = ^TSvnIODirEnt;
  TSvnIODirEnt = record
    kind: TSvnNodeKind;
    special: TSvnBoolean;
  end;
  PPSvnStream = ^PSvnStream;
  PSvnStream = ^TSvnStream;
  TSvnStream = THandle;
  TSvnReadFunc = function(baton: Pointer; buffer: PAnsiChar; var len: TAprSize): PSvnError; cdecl;
  TSvnWriteFunc = function(baton: Pointer; data: PAnsiChar; var len: TAprSize): PSvnError; cdecl;
  TSvnCloseFunc = function(baton: Pointer): PSvnError; cdecl;
  TSvnIOWalkFunc = function(baton: Pointer; path: PAnsiChar; finfo: PAprFInfo; pool: PAprPool): PSvnError; cdecl;

var
  svn_io_check_path: function(path: PAnsiChar; out kind: TSvnNodeKind; pool: PAprPool): PSvnError; cdecl;
  svn_io_check_special_path: function(path: PAnsiChar; out kind: TSvnNodeKind; out is_special: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_io_check_resolved_path: function(path: PAnsiChar; out kind: TSvnNodeKind; pool: PAprPool): PSvnError; cdecl;
  svn_io_open_unique_file2: function(out f: PAprFile; out unique_name_p: PAnsiChar; path, suffix: PAnsiChar;
    delete_when: TSvnIOFileDel; pool: PAprPool): PSvnError; cdecl;
  svn_io_open_unique_file: function(out f: PAprFile; out unique_name_p: PAnsiChar; path, suffix: PAnsiChar;
    delete_on_close: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_io_create_unique_link: function(out unique_name_p: PAnsiChar; path, dest, suffix: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_read_link: function(out dest: PSvnString; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_temp_dir: function(out dir: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_copy_file: function(src, dst: PAnsiChar; copy_perms: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_io_copy_link: function(src, dst: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_copy_dir_recursively: function(src, dst_parent, dst_basename: PAnsiChar; copy_perms: TSvnBoolean;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_io_make_dir_recursively: function(path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_empty: function(out is_empty_p: TSvnBoolean; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_append_file: function(src, dst: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_set_file_read_only: function(path: PAnsiChar; ignore_enoent: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_io_set_file_read_write: function(path: PAnsiChar; ignore_enoent: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_io_set_file_read_write_carefully: function(path: PAnsiChar; enable_write, ignore_enoent: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_io_set_file_executable: function(path: PAnsiChar; executable, ignore_enoent: TSvnBoolean; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_is_file_executable: function(out executable: TSvnBoolean; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_read_length_line: function(afile: PAprFile; buf: PAnsiChar; out limit: TAprSize; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_file_affected_time: function(out apr_time: TAprTime; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_set_file_affected_time: function(apr_time: TAprTime; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_filesizes_different_p: function(out different_p: TSvnBoolean; file1, file2: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_file_checksum: function(digest: PByte; afile: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_files_contents_same_p: function(out same: TSvnBoolean; file1, file2: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_file_create: function(afile, contents: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_lock: function(lock_file: PAnsiChar; exclusive: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_lock2: function(lock_file: PAnsiChar; exclusive, nonblocking: TSvnBoolean; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_file_flush_to_disk: function(afile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_file_copy: function(src_path, dest_path, afile: PAnsiChar; pool: PAprPool): PSvnError; cdecl;

  svn_stream_create: function(baton: Pointer; pool: PAprPool): PSvnStream;
  svn_stream_set_baton: procedure(stream: PSvnStream; baton: Pointer); cdecl;
  svn_stream_set_read: procedure(stream: PSvnStream; read_fn: TSvnReadFunc); cdecl;
  svn_stream_set_write: procedure(stream: PSvnStream; write_fn: TSvnWriteFunc); cdecl;
  svn_stream_set_close: procedure(stream: PSvnStream; close_fn: TSvnCloseFunc); cdecl;
  svn_stream_empty: function(pool: PAprPool): PSvnStream; cdecl;
  svn_stream_disown: function(stream: PSvnStream; pool: PAprPool): PSvnStream; cdecl;
  svn_stream_from_aprfile2: function(afile: PAprFile; disown: TSvnBoolean; pool: PAprPool): PSvnStream; cdecl;
  svn_stream_from_aprfile: function(afile: PAprFile; pool: PAprPool): PSvnStream; cdecl;
  svn_stream_for_stdout: function(out stdout: PSvnStream; pool: PAprPool): PSvnError; cdecl;
  svn_stream_from_stringbuf: function(str: PSvnStringBuf; pool: PAprPool): PSvnStream; cdecl;
  svn_stream_compressed: function(stream: PSvnStream; pool: PAprPool): PSvnStream; cdecl;
  svn_stream_checksummed: function(stream: PSvnStream; read_digest, write_digest: Pointer; read_all: TSvnBoolean;
    pool: PAprPool): PSvnStream; cdecl;
  svn_stream_read: function(stream: PSvnStream; buffer: PAnsiChar; var len: TAprSize): PSvnError; cdecl;
  svn_stream_write: function(stream: PSvnStream; data: PAnsiChar; var len: TAprSize): PSvnError; cdecl;
  svn_stream_close: function(stream: PSvnStream): PSvnError; cdecl;
  svn_stream_printf: function(stream: PSvnStream; pool: PAprPool; fmt: PAnsiChar; const args: array of const): PSvnError;
    cdecl;
  svn_stream_printf_from_utf8: function(stream: PSvnStream; encoding: PAnsiChar; pool: PAprPool; fmt: PAnsiChar;
    const args: array of const): PSvnError; cdecl;
  svn_stream_readline: function(stream: PSvnStream; out stringbuf: PSvnStringBuf; eol: PAnsiChar; out eof: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_stream_copy2: function(sfrom, sto: PSvnStream; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_stream_copy: function(sfrom, sto: PSvnStream; pool: PAprPool): PSvnError; cdecl;
  svn_stream_contents_same: function(out same: TSvnBoolean; stream1, stream2: PSvnStream; pool: PAprPool): PSvnError;
    cdecl;
  svn_stringbuf_from_file2: function(out result: PSvnStringBuf; filename: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_stringbuf_from_file: function(out result: PSvnStringBuf; filename: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_stringbuf_from_aprfile: function(out result: PSvnStringBuf; afile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_io_remove_file: function(path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_remove_dir2: function(path: PAnsiChar; ignore_enoent: TSvnBoolean; cancel_func: TSvnCancelFunc;
    cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_io_remove_dir: function(path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_get_dir_filenames: function(out dirents: PAprHash; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_get_dirents2: function(out dirents: PAprHash; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_get_dirents: function(out dirents: PAprHash; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_walk: function(dirname: PAnsiChar; wanted: Integer; walk_func: TSvnIOWalkFunc; walk_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_io_start_cmd: function(out cmd_proc: TAprProc; path, cmd: PAnsiChar; args: PPAnsiChar; inherit: TSvnBoolean;
    infile, outfile, errfile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_io_wait_for_cmd: function(cmd_proc: PAprProc; cmd: PAnsiChar; exitcode, exitwhy: PInteger; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_run_cmd: function(path, cmd: PAnsiChar; args: PPAnsiChar; exitcode, exitwhy: PInteger; inherit: TSvnBoolean;
    infile, outfile, errfile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_io_run_diff: function(dir: PAnsiChar; user_args: PPAnsiChar; num_user_args: Integer; label1, label2, ffrom, fto: PAnsiChar;
    exitcode: PInteger; outfile, errfile: PAprFile; diff_cmd: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_run_diff3_2: function(out exitcode: Integer; dir, mine, older, yours, mine_label, older_label,
    yours_label: PAnsiChar; merged: PAprFile; diff3_cmd: PAnsiChar; user_args: PAprArrayHeader; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_run_diff3: function(dir, mine, older, yours, mine_label, older_label, yours_label: PAnsiChar; merged: PAprFile;
    exitcode: PInteger; diff3_cmd: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_parse_mimetypes_file: function(out type_map: PAprHash; mimetypes_file: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_detect_mimetype2: function(out mimetype: PAnsiChar; afile: PAnsiChar; mimetype_map: PAprHash;
    pool: PAprPool): PSvnError; cdecl;
  svn_io_detect_mimetype: function(out mimetype: PAnsiChar; afile: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_open: function(out new_file: PAprFile; fname: PAnsiChar; flag: Integer; perm: TAprFilePerms;
    pool: PAprPool): PSvnError; cdecl;
  svn_io_file_close: function(afile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_getc: function(out ch: Char; afile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_info_get: function(out finfo: TAprFInfo; wanted: Integer; afile: PAprFile; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_file_read: function(afile: PAprFile; buf: Pointer; var nbytes: TAprSize; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_read_full: function(afile: PAprFile; buf: Pointer; nbytes: TAprSize; out bytes_read: TAprSize;
    pool: PAprPool): PSvnError; cdecl;
  svn_io_file_seek: function(afile: PAprFile; where: TAprSeekWhere; offset: PAprOff; pool: PAprPool): PSvnError;
    cdecl;
  svn_io_file_write: function(afile: PAprFile; buf: Pointer; var nbytes: TAprSize; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_write_full: function(afile: PAprFile; buf: Pointer; nbytes: TAprSize; bytes_written: PAprSize;
    pool: PAprPool): PSvnError; cdecl;
  svn_io_stat: function(out finfo: TAprFInfo; fname: PAnsiChar; wanted: Integer; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_rename: function(from_path, to_path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_file_move: function(from_path, to_path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_make: function(path: PAnsiChar; perm: TAprFilePerms; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_make_hidden: function(path: PAnsiChar; perm: TAprFilePerms; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_make_sgid: function(path: PAnsiChar; perm: TAprFilePerms; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_open: function(out new_dir: PAprDir; dirname: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_remove_nonrecursive: function(dirname: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_dir_read: function(finfo: PAprFInfo; wanted: Integer; thedir: PAprDir; pool: PAprPool): PSvnError; cdecl;
  svn_io_read_version_file: function(out version: Integer; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_io_write_version_file: function(path: PAnsiChar; version: Integer; pool: PAprPool): PSvnError; cdecl;

//----- svn_io.h -------------------------------------------------------------------------------------------------------

//----- svn_quoprint.h -------------------------------------------------------------------------------------------------

var
  svn_quoprint_encode: function(output: PSvnStream; pool: PAprPool): PSvnStream; cdecl;
  svn_quoprint_decode: function(output: PSvnStream; pool: PAprPool): PSvnStream; cdecl;
  svn_quoprint_encode_string: function(str: PSvnStringBuf; pool: PAprPool): PSvnStringBuf; cdecl;
  svn_quoprint_decode_string: function(str: PSvnStringBuf; pool: PAprPool): PSvnStringBuf; cdecl;

//----- svn_quoprint.h -------------------------------------------------------------------------------------------------

//----- svn_hash.h -----------------------------------------------------------------------------------------------------

const
  SVN_KEYLINE_MAXLEN = 100;
  SVN_HASH_TERMINATOR = 'END';

type
  TSvnHashDiffKeyStatus = (
    svnHashDiffKeyBoth,
    svnHashDiffKeyA,
    svnHashDiffKeyB
  );
  TSvnHashDiffFunc = function(key: Pointer; klen: TAprSize; status: TSvnHashDiffKeyStatus; baton: Pointer): PSvnError;
    cdecl;

var
  svn_hash_read2: function(hash: PAprHash; stream: PSvnStream; terminator: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_hash_write2: function(hash: PAprHash; stream: PSvnStream; terminator: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_hash_read_incremental: function(hash: PAprHash; stream: PSvnStream; terminator: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_hash_write_incremental: function(hash, oldhash: PAprHash; stream: PSvnStream; terminator: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_hash_read: function(hash: PAprHash; srcfile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_hash_write: function(hash: PAprHash; destfile: PAprFile; pool: PAprPool): PSvnError; cdecl;
  svn_hash_diff: function(hash_a, hash_b: PAprHash; diff_func: TSvnHashDiffFunc; diff_func_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_hash_keys: function(out keys_array: PAprArrayHeader; hash: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_hash_from_cstring_keys: function(out hash: PAprHash; keys: PAprArrayHeader; pool: PAprPool): PSvnError; cdecl;
  svn_hash__clear: function(hash: PAprHash): PSvnError; cdecl;

//----- svn_hash.h -----------------------------------------------------------------------------------------------------

//----- svn_subst.h ----------------------------------------------------------------------------------------------------

type
  TSvnSubstEolStyle = (
    svnSubstEolStyleUnknown,
    svnSubstEolStyleNone,
    svnSubstEolStyleNative,
    svnSubstEolStyleFixed
  );
  PSvnSubstKeywords = ^TSvnSubstKeywords;
  TSvnSubstKeywords = record
    revision: PSvnString;
    date: PSvnString;
    author: PSvnString;
    url: PSvnString;
    id: PSvnString;
  end;

var
  svn_subst_eol_style_from_value: procedure(out style: TSvnSubstEolStyle; out eol: PAnsiChar; value: PAnsiChar); cdecl;
  svn_subst_translation_required: function(style: TSvnSubstEolStyle; eol: PAnsiChar; keywords: PAprHash;
    special, force_eol_check: TSvnBoolean): TSvnBoolean; cdecl;
  svn_subst_build_keywords2: function(out kw: PAprHash; keywords_string, rev, url: PAnsiChar; date: TAprTime;
    author: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_subst_build_keywords: function(out kw: TSvnSubstKeywords; keywords_string, rev, url: PAnsiChar; date: TAprTime;
    author: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_subst_keywords_differ2: function(a, b: PAprHash; compare_values: TSvnBoolean; pool: PAprPool): TSvnBoolean; cdecl;
  svn_subst_keywords_differ: function(a, b: PSvnSubstKeywords; compare_values: TSvnBoolean): TSvnBoolean; cdecl;
  svn_subst_stream_translated: function(stream: PSvnStream; eol_str: PAnsiChar; repair: TSvnBoolean; keywords: PAprHash;
    expand: TSvnBoolean; pool: PAprPool): PSvnStream; cdecl;
  svn_subst_stream_translated_to_normal_form: function(out stream: PSvnStream; source: PSvnStream;
    eol_style: TSvnSubstEolStyle; eol_str: PAnsiChar; always_repair_eols: TSvnBoolean; keywords: PAprHash;
    pool: PAprPool): PSvnError; cdecl;
  svn_subst_stream_from_specialfile: function(out stream: PSvnStream; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_subst_translate_stream3: function(src, dst: PSvnStream; eol_str: PAnsiChar; repair: TSvnBoolean; keywords: PAprHash;
    expand: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_translate_stream2: function(src, dst: PSvnStream; eol_str: PAnsiChar; repair: TSvnBoolean;
    keywords: PSvnSubstKeywords; expand: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_translate_stream: function(src, dst: PSvnStream; eol_str: PAnsiChar; repair: TSvnBoolean;
    keywords: PSvnSubstKeywords; expand: TSvnBoolean): PSvnError; cdecl;
  svn_subst_copy_and_translate3: function(src, dst, eol_str: PAnsiChar; repair: TSvnBoolean; keywords: PAprHash;
    expand, special: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_copy_and_translate2: function(src, dst, eol_str: PAnsiChar; repair: TSvnBoolean; keywords: PSvnSubstKeywords;
    expand, special: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_copy_and_translate: function(src, dst, eol_str: PAnsiChar; repair: TSvnBoolean; keywords: PSvnSubstKeywords;
    expand: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_translate_cstring2: function(src: PAnsiChar; out dst: PAnsiChar; eol_str: PAnsiChar; repair: TSvnBoolean;
    keywords: PAprHash; expand: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_translate_cstring: function(src: PAnsiChar; out dst: PAnsiChar; eol_str: PAnsiChar; repair: TSvnBoolean;
    keywords: PSvnSubstKeywords; expand: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_translate_to_normal_form: function(src, dst: PAnsiChar; eol_style: TSvnSubstEolStyle; eol_str: PAnsiChar;
    always_repair_eols: TSvnBoolean; keywords: PAprHash; special: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_subst_stream_detranslated: function(out stream_p: PSvnStream; src: PAnsiChar; eol_style: TSvnSubstEolStyle;
    eol_str: PAnsiChar; always_repair_eols: TSvnBoolean; keywords: PAprHash; special: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_subst_translate_string: function(out new_value: PSvnString; value: PSvnString; encoding: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_subst_detranslate_string: function(out new_value: PSvnString; value: PSvnString; for_output: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;

//----- svn_subst.h ----------------------------------------------------------------------------------------------------

//----- svn_diff.h -----------------------------------------------------------------------------------------------------

type
  TSvnDiffType = (
    svnDiffTypeCommon,
    svnDiffTypeDiffModified,
    svnDiffTypeDiffLatest,
    svnDiffTypeDiffCommon,
    svnDiffTypeConflict
  );
  PSvnDiff = ^TSvnDiff;
  TSvnDiff = record
    next: PSvnDiff;
    _type: TSvnDiffType;
    original_start: TAprOff;
    original_length: TAprOff;
    modified_start: TAprOff;
    modified_length: TAprOff;
    latest_start: TAprOff;
    latest_length: TAprOff;
    resolved_diff: PSvnDiff;
  end;
  TSvnDiffDataSource = (
    svnDiffDatasourceOriginal,
    svnDiffDatasourceModified,
    svnDiffDatasourceLatest,
    svnDiffDatasourceAncestor
  );
  PSvnDiffFns = ^TSvnDiffFns;
  TSvnDiffFns = record
    datasource_open: function(diff_baton: Pointer; datasource: TSvnDiffDataSource): PSvnError; cdecl;
    datasource_close: function(diff_baton: Pointer; datasource: TSvnDiffDataSource): PSvnError; cdecl;
    datasource_get_next_token: function(hash: PCardinal; out token: Pointer; diff_baton: Pointer;
      datasource: TSvnDiffDataSource): PSvnError; cdecl;
    token_compare: function(diff_baton, ltoken, rtoken: Pointer; out compare: Integer): PSvnError; cdecl;
    token_discard: procedure(diff_baton, token: Pointer); cdecl;
    token_discard_all: procedure(diff_baton: Pointer); cdecl;
  end;
  PSvnDiffOutputFns = ^TSvnDiffOutputFns;
  TSvnDiffOutputFns = record
    output_common: function(output_baton: Pointer; original_start, original_length, modified_start, modified_length,
      latest_start, latest_length: TAprOff): PSvnError; cdecl;
    output_diff_modified: function(output_baton: Pointer; original_start, original_length, modified_start,
      modified_length, latest_start, latest_length: TAprOff): PSvnError; cdecl;
    output_diff_latest: function(output_baton: Pointer; original_start, original_length, modified_start,
      modified_length, latest_start, latest_length: TAprOff): PSvnError; cdecl;
    output_diff_common: function(output_baton: Pointer; original_start, original_length, modified_start,
      modified_length, latest_start, latest_length: TAprOff): PSvnError; cdecl;
    output_conflict: function(output_baton: Pointer; original_start, original_length, modified_start, modified_length,
      latest_start, latest_length: TAprOff; resolved_diff: PSvnDiff): PSvnError; cdecl;
  end;
  TSvnDiffFileIgnoreSpace = (
    svnIgnoreSpaceNone,
    svnIgnoreSpaceChange,
    svnIgnoreSpaceAll
  );
  PSvnDiffFileOptions = ^TSvnDiffFileOptions;
  TSvnDiffFileOptions = record
    ignore_space: TSvnDiffFileIgnoreSpace;
    ignore_eol_style: TSvnBoolean;
    show_c_function: TSvnBoolean;
  end;

var
  svn_diff_version: function: PSvnVersion; cdecl;
  svn_diff_diff: function(out diff: PSvnDiff; diff_baton: Pointer; diff_fns: PSvnDiffFns; pool: PAprPool): PSvnError;
    cdecl;
  svn_diff_diff3: function(out diff: PSvnDiff; diff_baton: Pointer; diff_fns: PSvnDiffFns; pool: PAprPool): PSvnError;
    cdecl;
  svn_diff_diff4: function(out diff: PSvnDiff; diff_baton: Pointer; diff_fns: PSvnDiffFns; pool: PAprPool): PSvnError;
    cdecl;
  svn_diff_contains_conflicts: function(diff: PSvnDiff): TSvnBoolean; cdecl;
  svn_diff_contains_diffs: function(diff: PSvnDiff): TSvnBoolean; cdecl;
  svn_diff_output: function(diff: PSvnDiff; output_baton: Pointer; output_fns: PSvnDiffOutputFns): PSvnError; cdecl;
  svn_diff_file_diff_2: function(out diff: PSvnDiff; original, modified: PAnsiChar; options: PSvnDiffFileOptions;
    pool: PAprPool): PSvnError; cdecl;
  svn_diff_file_diff: function(out diff: PSvnDiff; original, modified: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_diff_file_diff3_2: function(out diff: PSvnDiff; original, modified, latest: PAnsiChar; options: PSvnDiffFileOptions;
    pool: PAprPool): PSvnError; cdecl;
  svn_diff_file_diff3: function(out diff: PSvnDiff; original, modified, latest: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_diff_file_diff4_2: function(out diff: PSvnDiff; original, modified, latest, ancestor: PAnsiChar;
    options: PSvnDiffFileOptions; pool: PAprPool): PSvnError; cdecl;
  svn_diff_file_diff4: function(out diff: PSvnDiff; original, modified, latest, ancestor: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_diff_file_output_unified3: function(output_stream: PSvnStream; diff: PSvnDiff; original_path, modified_path,
    original_header, modified_header, header_encoding, relative_to_dir: PAnsiChar; show_c_function: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_diff_file_output_unified2: function(output_stream: PSvnStream; diff: PSvnDiff; original_path, modified_path,
    original_header, modified_header, header_encoding: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_diff_file_output_unified: function(output_stream: PSvnStream; diff: PSvnDiff; original_path, modified_path,
    original_header, modified_header: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_diff_file_output_merge: function(output_stream: PSvnStream; diff: PSvnDiff; original_path, modified_path,
    latest_path, conflict_original, conflict_modified, conflict_latest, conflict_separator: PAnsiChar;
    display_original_in_conflict, display_resolved_conflicts: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_diff_file_options_create: function(pool: PAprPool): PSvnDiffFileOptions; cdecl;
  svn_diff_file_options_parse: function(options: PSvnDiffFileOptions; const args: PAprArrayHeader;
    pool: PAprPool): PSvnError; cdecl;
  svn_diff_mem_string_diff: function(out diff: PSvnDiff; original, modified: PSvnString; options: PSvnDiffFileOptions;
    pool: PAprPool): PSvnError; cdecl;
  svn_diff_mem_string_diff3: function(out diff: PSvnDiff; original, modified, latest: PSvnString;
    options: PSvnDiffFileOptions; pool: PAprPool): PSvnError; cdecl;
  svn_diff_mem_string_diff4: function(out diff: PSvnDiff; original, modified, latest, ancestor: PSvnString;
    options: PSvnDiffFileOptions; pool: PAprPool): PSvnError; cdecl;
  svn_diff_mem_string_output_unified: function(output_stream: PSvnStream; diff: PSvnDiff;
    original_header, modified_header, header_encoding: PAnsiChar; original, modified: PSvnString;
    pool: PAprPool): PSvnError; cdecl;
  svn_diff_mem_string_output_merge: function(output_stream: PSvnStream; diff: PSvnDiff;
    original, modified, latest: PSvnString;
    conflict_original, conflict_modified, conflict_latest, conflict_separator: PAnsiChar;
    display_original_in_conflict, display_resolved_conflicts: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;

//----- svn_diff.h -----------------------------------------------------------------------------------------------------

//----- svn_base64.h ---------------------------------------------------------------------------------------------------

var
  svn_base64_encode: function(output: PSvnStream; pool: PAprPool): PSvnStream; cdecl;
  svn_base64_decode: function(output: PSvnStream; pool: PAprPool): PSvnStream; cdecl;
  svn_base64_encode_string: function(str: PSvnString; pool: PAprPool): PSvnString; cdecl;
  svn_base64_decode_string: function(str: PSvnString; pool: PAprPool): PSvnString; cdecl;
  svn_base64_from_md5: function(digest: PByte; pool: PAprPool): PSvnStringBuf; cdecl;

//----- svn_base64.h ---------------------------------------------------------------------------------------------------

//----- svn_delta.h ----------------------------------------------------------------------------------------------------

type
  TSvnDeltaAction = (
    svnTxDeltaSource,
    svnTxDeltaTarget,
    svnTxDeltaNew
  );
  PSvnTxDeltaOp = ^TSvnTxDeltaOp;
  TSvnTxDeltaOp = record
    action_code: TSvnDeltaAction;
    offset: TAprSize;
    length: TAprSize;
  end;
  PSvnTxDeltaWindow = ^TSvnTxDeltaWindow;
  TSvnTxDeltaWindow = record
    sview_offset: TSvnFileSize;
    sview_len: TAprSize;
    tview_len: TAprSize;
    num_ops: Integer;
    src_ops: Integer;
    ops: PSvnTxDeltaOp;
    new_data: PSvnString;
  end;
  TSvnTxDeltaWindowHandler = function(window: PSvnTxDeltaWindow; baton: Pointer): PSvnError; cdecl;
  TSvnTxDeltaNextWindowFunc = function(out window: PSvnTxDeltaWindow; baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  TSvnTxDeltaMd5DigestFunc = function(baton: Pointer): Pointer; cdecl;
  PSvnTxDeltaStream = ^TSvnTxDeltaStream;
  TSvnTxDeltaStream = THandle;

  PSvnDeltaEditor = ^TSvnDeltaEditor;
  TSvnDeltaEditor = record
    set_target_revision: function(edit_baton: Pointer; target_revision: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
    open_root: function(edit_baton: Pointer; base_revision: TSvnRevNum; dir_pool: PAprPool;
      out root_baton: Pointer): PSvnError; cdecl;
    delete_entry: function(path: PAnsiChar; revision: TSvnRevNum; parent_baton: Pointer; pool: PAprPool): PSvnError;
      cdecl;
    add_directory: function(path: PAnsiChar; parent_baton: Pointer; copyfrom_path: PAnsiChar; copyfrom_revision: TSvnRevNum;
      dir_pool: PAprPool; out child_baton: Pointer): PSvnError; cdecl;
    open_directory: function(path: PAnsiChar; parent_baton: Pointer; base_revision: TSvnRevNum; dir_pool: PAprPool;
      out child_baton: Pointer): PSvnError; cdecl;
    change_dir_prop: function(dir_baton: Pointer; name: PAnsiChar; value: PSvnString; pool: PAprPool): PSvnError; cdecl;
    close_directory: function(dir_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    absent_directory: function(path: PAnsiChar; parent_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    add_file: function(path: PAnsiChar; parent_baton: Pointer; copyfrom_path: PAnsiChar; copyfrom_revision: TSvnRevNum;
      file_pool: PAprPool; out file_baton: Pointer): PSvnError; cdecl;
    open_file: function(path: PAnsiChar; parent_baton: Pointer; base_revision: TSvnRevNum; file_pool: PAprPool;
      out file_baton: Pointer): PSvnError; cdecl;
    apply_text_delta: function(file_baton: Pointer; base_checksum: PAnsiChar; pool: PAprPool;
      out handler: TSvnTxDeltaWindowHandler; out handler_baton: Pointer): PSvnError; cdecl;
    change_file_prop: function(file_baton: Pointer; name: PAnsiChar; value: PSvnString; pool: PAprPool): PSvnError; cdecl;
    close_file: function(file_baton: Pointer; text_checksum: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
    absent_file: function(path: PAnsiChar; parent_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    close_edit: function(edit_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    abort_edit: function(edit_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  end;

  TSvnDeltaPathDriverCbFunc = function(out dir_baton: Pointer; parent_baton, callback_baton: Pointer;
    path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  TSvnFileRevHandler = function(baton: Pointer; path: PAnsiChar; rev: TSvnRevNum; rev_props: PAprHash;
    result_of_merge: TSvnBoolean; out delta_handler: TSvnTxDeltaWindowHandler; out delta_baton: Pointer;
    prop_diffs: PAprArrayHeader; pool: PAprPool): PSvnError; cdecl;
  TSvnFileRevHandlerOld = function(baton: Pointer; path: PAnsiChar; rev: TSvnRevNum; rev_props: PAprHash;
    out delta_handler: TSvnTxDeltaWindowHandler; out delta_baton: Pointer; prop_diffs: PAprArrayHeader;
    pool: PAprPool): PSvnError; cdecl;

var
  svn_delta_version: function: PSvnVersion; cdecl;
  svn_txdelta_window_dup: function(window: PSvnTxDeltaWindow; pool: PAprPool): PSvnTxDeltaWindow; cdecl;
  svn_txdelta_compose_windows: function(window_A, window_B: PSvnTxDeltaWindow; pool: PAprPool): PSvnTxDeltaWindow;
    cdecl;
  svn_txdelta_stream_create: function(baton: Pointer; next_window: TSvnTxDeltaNextWindowFunc;
    md5_digest: TSvnTxDeltaMd5DigestFunc; pool: PAprPool): PSvnTxDeltaStream; cdecl;
  svn_txdelta_next_window: function(out window: PSvnTxDeltaWindow; stream: PSvnTxDeltaStream;
    pool: PAprPool): PSvnError; cdecl;
  svn_txdelta_apply_instructions: procedure(window: PSvnTxDeltaWindow; sbuf, tbuf: PAnsiChar; tlen: PAprSize); cdecl;
  svn_txdelta_md5_digest: function(stream: PSvnTxDeltaStream): PByte; cdecl;
  svn_txdelta: procedure(out stream: PSvnTxDeltaStream; source, target: PSvnStream; pool: PAprPool); cdecl;
  svn_txdelta_target_push: function(handler: TSvnTxDeltaWindowHandler; handler_baton: Pointer; source: PSvnStream;
    pool: PAprPool): PSvnStream; cdecl;
  svn_txdelta_send_string: function(str: PSvnString; handler: TSvnTxDeltaWindowHandler; handler_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_txdelta_send_stream: function(stream: PSvnStream; handler: TSvnTxDeltaWindowHandler; handler_baton: Pointer;
    digest: PByte; pool: PAprPool): PSvnError; cdecl;
  svn_txdelta_send_txstream: function(txstream: PSvnTxDeltaStream; handler: TSvnTxDeltaWindowHandler;
    handler_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_txdelta_apply: procedure(source, target: PSvnStream; result_digest: PByte; error_info: PAnsiChar; pool: PAprPool;
    out handler: TSvnTxDeltaWindowHandler; out handler_baton: Pointer); cdecl;
  svn_txdelta_to_svndiff2: procedure(out handler: TSvnTxDeltaWindowHandler; out handler_baton: Pointer;
    output: PSvnStream; svndiff_version: Integer; pool: PAprPool); cdecl;
  svn_txdelta_to_svndiff: procedure(output: PSvnStream; pool: PAprPool; out handler: TSvnTxDeltaWindowHandler;
    out handler_baton: Pointer); cdecl;
  svn_txdelta_parse_svndiff: function(handler: TSvnTxDeltaWindowHandler; handler_baton: Pointer;
    error_on_early_close: TSvnBoolean; pool: PAprPool): PSvnStream; cdecl;
  svn_txdelta_read_svndiff_window: function(out window: PSvnTxDeltaWindow; stream: PSvnStream; svndiff_version: Integer;
    pool: PAprPool): PSvnError; cdecl;
  svn_txdelta_skip_svndiff_window: function(afile: PAprFile; svndiff_version: Integer; pool: PAprPool): PSvnError;
    cdecl;

  svn_delta_default_editor: function(pool: PAprPool): PSvnDeltaEditor; cdecl;
  svn_delta_noop_window_handler: function(window: PSvnTxDeltaWindow; baton: Pointer): PSvnError; cdecl;
  svn_delta_get_cancellation_editor: function(cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    wrapped_editor: PSvnDeltaEditor; wrapped_baton: Pointer; out editor: PSvnDeltaEditor; out edit_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_delta_depth_filter_editor: function(out editor: PSvnDeltaEditor; out edit_baton: Pointer;
    wrapped_editor: PSvnDeltaEditor; wrapped_edit_baton: Pointer; requested_depth: TSvnDepth; has_target: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_delta_path_driver: function(editor: PSvnDeltaEditor; edit_baton: Pointer; revision: TSvnRevNum;
    paths: PAprArrayHeader; callback_func: TSvnDeltaPathDriverCbFunc; callback_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_compat_wrap_file_rev_handler: procedure(out handler2: TSvnFileRevHandler; out handler2_baton: Pointer;
    handler: TSvnFileRevHandlerOld; handler_baton: Pointer; pool: PAprPool); cdecl;

//----- svn_delta.h ----------------------------------------------------------------------------------------------------

//----- svn_fs.h -------------------------------------------------------------------------------------------------------

const
  SVN_FS_CONFIG_BDB_TXN_NOSYNC = 'bdb-txn-nosync';
  SVN_FS_CONFIG_BDB_LOG_AUTOREMOVE = 'bdb-log-autoremove';
  SVN_FS_CONFIG_FS_TYPE = 'fs-type';
  SVN_FS_TYPE_BDB = 'bdb';
  SVN_FS_TYPE_FSFS = 'fsfs';
  SVN_FS_CONFIG_PRE_1_4_COMPATIBLE = 'pre-1.4-compatible';
  SVN_FS_CONFIG_PRE_1_5_COMPATIBLE = 'pre-1.5-compatible';
  SVN_FS_TXN_CHECK_OOD = $00001;
  SVN_FS_TXN_CHECK_LOCKS = $00002;

type
  PSvnFS = ^TSvnFS;
  TSvnFS = THandle;
  TSvnFSWarningCallback = procedure(baton: Pointer; err: PSvnError); cdecl;
  TSvnBerkeleyErrorCallback = procedure(errpfs, msg: PAnsiChar); cdecl;
  PSvnFSAccess = ^TSvnFSAccess;
  TSvnFSAccess = THandle;
  PSvnFSID = ^TSvnFSID;
  TSvnFSID = THandle;
  PSvnFSTxn = ^TSvnFSTxn;
  TSvnFSTxn = THandle;
  PSvnFSRoot = ^TSvnFSRoot;
  TSvnFSRoot = THandle;
  TSvnFSPathChangeKind = (
    svnFSPathChangeModify,
    svnFSPathChangeAdd,
    svnFSPathChangeDelete,
    svnFSPathChangeReplace,
    svnFSPathChangeReset
  );
  PSvnFSPathChange = ^TSvnFSPathChange;
  TSvnFSPathChange = record
    node_rev_id: PSvnFSID;
    change_kind: TSvnFSPathChangeKind;
    text_mod: TSvnBoolean;
    prop_mod: TSvnBoolean;
  end;
  PSvnFSHistory = ^TSvnFSHistory;
  TSvnFSHistory = THandle;
  PSvnFSDirent = ^TSvnFSDirent;
  TSvnFSDirent = record
    name: PAnsiChar;
    id: PSvnFSID;
    kind: TSvnNodeKind;
  end;
  TSvnFSGetLocksCallback = function(baton: Pointer; lock: PSvnLock; pool: PAprPool): PSvnError; cdecl;

var
  svn_fs_version: function: PSvnVersion; cdecl;
  svn_fs_initialize: function(pool: PAprPool): PSvnError; cdecl;
  svn_fs_set_warning_func: procedure(fs: PSvnFS; warning: TSvnFSWarningCallback; warning_baton: Pointer); cdecl;
  svn_fs_create: function(out fs_p: PSvnFS; path: PAnsiChar; fs_config: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_fs_open: function(out fs_p: PSvnFS; path: PAnsiChar; config: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_fs_upgrade: function(path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_type: function(out fs_type: PAnsiChar; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_path: function(fs: PSvnFS; pool: PAprPool): PAnsiChar; cdecl;
  svn_fs_delete_fs: function(path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_hotcopy: function(src_path, dest_path: PAnsiChar; clean: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_fs_recover: function(path: PAnsiChar; cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_set_berkeley_errcall: function(fs: PSvnFS; handler: TSvnBerkeleyErrorCallback): PSvnError; cdecl;
  svn_fs_berkeley_recover: function(path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_berkeley_logfiles: function(out logfiles: PAprArrayHeader; path: PAnsiChar; only_unused: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_new: function(fs_config: PAprHash; pool: PAprPool): PSvnFS; cdecl;
  svn_fs_create_berkeley: function(fs: PSvnFS; path: PAnsiChar): PSvnError; cdecl;
  svn_fs_open_berkeley: function(fs: PSvnFS; path: PAnsiChar): PSvnError; cdecl;
  svn_fs_berkeley_path: function(fs: PSvnFS; pool: PAprPool): PAnsiChar; cdecl;
  svn_fs_delete_berkeley: function(path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_hotcopy_berkeley: function(src_path, dest_path: PAnsiChar; clean_logs: TSvnBoolean; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_create_access: function(out access_ctx: PSvnFSAccess; username: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_set_access: function(fs: PSvnFS; access_ctx: PSvnFSAccess): PSvnError; cdecl;
  svn_fs_get_access: function(out access_ctx: PSvnFSAccess; fs: PSvnFS): PSvnError; cdecl;
  svn_fs_access_get_username: function(out username: PAnsiChar; access_ctx: PSvnFSAccess): PSvnError; cdecl;
  svn_fs_access_add_lock_token: function(access_ctx: PSvnFSAccess; token: PAnsiChar): PSvnError; cdecl;
  svn_fs_compare_ids: function(a, b: PSvnFSID): Integer; cdecl;
  svn_fs_check_related: function(id1, id2: PSvnFSID): TSvnBoolean; cdecl;
  svn_fs_parse_id: function(data: PAnsiChar; len: TAprSize; pool: PAprPool): PSvnFSID; cdecl;
  svn_fs_unparse_id: function(id: PSvnFSID; pool: PAprPool): PSvnString; cdecl;
  svn_fs_begin_txn2: function(out txn_p: PSvnFSTxn; fs: PSvnFS; rev: TSvnRevNum; flags: Cardinal;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_begin_txn: function(out txn_p: PSvnFSTxn; fs: PSvnFS; rev: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_fs_commit_txn: function(conflict_p: PPAnsiChar; out new_rev: TSvnRevNum; txn: PSvnFSTxn; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_abort_txn: function(txn: PSvnFSTxn; pool: PAprPool): PSvnError; cdecl;
  svn_fs_purge_txn: function(fs: PSvnFS; txn_id: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_txn_name: function(out name_p: PAnsiChar; txn: PSvnFSTxn; pool: PAprPool): PSvnError; cdecl;
  svn_fs_txn_base_revision: function(txn: PSvnFSTxn): TSvnRevNum; cdecl;
  svn_fs_open_txn: function(out txn: PSvnFSTxn; fs: PSvnFS; name: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_list_transactions: function(out names_p: PAprArrayHeader; fs: PSvnFS; pool: PAprPool): PSvnError; cdecl;
  svn_fs_txn_prop: function(out value_p: PSvnString; txn: PSvnFSTxn; propname: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_txn_proplist: function(out table_p: PAprHash; txn: PSvnFSTxn; pool: PAprPool): PSvnError; cdecl;
  svn_fs_change_txn_prop: function(txn: PSvnFSTxn; name: PAnsiChar; value: PSvnString; pool: PAprPool): PSvnError; cdecl;
  svn_fs_change_txn_props: function(txn: PSvnFSTxn; props: PAprArrayHeader; pool: PAprPool): PSvnError; cdecl;
  svn_fs_revision_root: function(out root_p: PSvnFSRoot; fs: PSvnFS; rev: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_fs_txn_root: function(out root_p: PSvnFSRoot; txn: PSvnFSTxn; pool: PAprPool): PSvnError; cdecl;
  svn_fs_close_root: procedure(root: PSvnFSRoot); cdecl;
  svn_fs_root_fs: function(root: PSvnFSRoot): PSvnFS; cdecl;
  svn_fs_is_txn_root: function(root: PSvnFSRoot): TSvnBoolean; cdecl;
  svn_fs_is_revision_root: function(root: PSvnFSRoot): TSvnBoolean; cdecl;
  svn_fs_txn_root_name: function(root: PSvnFSRoot; pool: PAprPool): PAnsiChar; cdecl;
  svn_fs_txn_root_base_revision: function(root: PSvnFSRoot): TSvnRevNum; cdecl;
  svn_fs_revision_root_revision: function(root: PSvnFSRoot): TSvnRevNum; cdecl;
  svn_fs_paths_changed: function(out changed_paths_p: PAprHash; root: PSvnFSRoot; pool: PAprPool): PSvnError; cdecl;
  svn_fs_check_path: function(out kind_p: TSvnNodeKind; root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_node_history: function(out history_p: PSvnFSHistory; root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_history_prev: function(out prev_history_p: PSvnFSHistory; history: PSvnFSHistory; cross_copies: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_history_location: function(out path: PAnsiChar; out revision: TSvnRevNum; history: PSvnFSHistory;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_is_dir: function(out is_dir: TSvnBoolean; root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_is_file: function(out is_file: TSvnBoolean; root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_node_id: function(out id_p: PSvnFSID; root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_node_created_rev: function(out revision: TSvnRevNum; root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_node_origin_rev: function(out revision: TSvnRevNum; root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_node_created_path: function(out created_path: PAnsiChar; root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_node_prop: function(out value_p: PSvnString; root: PSvnFSRoot; path, propname: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_node_proplist: function(out table_p: PAprHash; root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_change_node_prop: function(root: PSvnFSRoot; path, name: PAnsiChar; value: PSvnString; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_props_changed: function(out changed_p: TSvnBoolean; root1: PSvnFSRoot; path1: PAnsiChar; root2: PSvnFSRoot;
    path2: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_copied_from: function(out rev_p: TSvnRevNum; out path_p: PAnsiChar; root: PSvnFSRoot; path: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_closest_copy: function(out root_p: PSvnFSRoot; out path_p: PAnsiChar; root: PSvnFSRoot; path: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_get_mergeinfo: function(out catalog: TSvnMergeInfoCatalog; root: PSvnFSRoot; paths: PAprArrayHeader;
    inherit: TSvnMergeInfoInheritance; include_descendants: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_fs_merge: function(conflict_p: PPAnsiChar; source_root: PSvnFSRoot; source_path: PAnsiChar; target_root: PSvnFSRoot;
    target_path: PAnsiChar; ancestor_root: PSvnFSRoot; ancestor_path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_dir_entries: function(out entries_p: PAprHash; root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_make_dir: function(root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_delete: function(root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_copy: function(from_root: PSvnFSRoot; from_path: PAnsiChar; to_root: PSvnFSRoot; to_path: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_revision_link: function(from_root, to_root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_file_length: function(out length_p: TSvnFileSize; root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_file_md5_checksum: function(digest: PByte; root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_file_contents: function(out contents: PSvnStream; root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_make_file: function(root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_apply_textdelta: function(out contents_p: TSvnTxDeltaWindowHandler; out contents_baton_p: Pointer;
    root: PSvnFSRoot; path, base_checksum, result_checksum: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_apply_text: function(out contents_p: PSvnStream; root: PSvnFSRoot; path, result_checksum: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_contents_changed: function(out changed_p: TSvnBoolean; root1: PSvnFSRoot; path1: PAnsiChar; root2: PSvnFSRoot;
    path2: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_youngest_rev: function(out youngest_p: TSvnRevNum; fs: PSvnFS; pool: PAprPool): PSvnError; cdecl;
  svn_fs_deltify_revision: function(fs: PSvnFS; revision: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_fs_revision_prop: function(out value_p: PSvnString; fs: PSvnFS; rev: TSvnRevNum; propname: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_revision_proplist: function(out table_p: PAprHash; fs: PSvnFS; rev: TSvnRevNum; pool: PAprPool): PSvnError;
    cdecl;
  svn_fs_change_rev_prop: function(fs: PSvnFS; rev: TSvnRevNum; name: PAnsiChar; value: PSvnString;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_get_file_delta_stream: function(out stream_p: PSvnTxDeltaStream; source_root: PSvnFSRoot; source_path: PAnsiChar;
    target_root: PSvnFSRoot; target_path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_get_uuid: function(fs: PSvnFS; out uuid: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_set_uuid: function(fs: PSvnFS; uuid: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_lock: function(out lock: PSvnLock; fs: PSvnFS; path, token, comment: PAnsiChar; is_dav_comment: TSvnBoolean;
    expiration_date: TAprTime; current_rev: TSvnRevNum; steal_lock: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_fs_generate_lock_token: function(out token: PAnsiChar; fs: PSvnFS; pool: PAprPool): PSvnError; cdecl;
  svn_fs_unlock: function(fs: PSvnFS; path, token: PAnsiChar; break_lock: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_fs_get_lock: function(out lock: PSvnLock; fs: PSvnFS; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_fs_get_locks: function(fs: PSvnFS; path: PAnsiChar; get_locks_func: TSvnFSGetLocksCallback; get_locks_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_fs_print_modules: function(output: PSvnStringBuf; pool: PAprPool): PSvnError; cdecl;

//----- svn_fs.h -------------------------------------------------------------------------------------------------------

//----- svn_repos.h ----------------------------------------------------------------------------------------------------

const
  SVN_REPOS_DUMPFILE_MAGIC_HEADER = 'SVN-fs-dump-format-version';
  SVN_REPOS_DUMPFILE_FORMAT_VERSION = 3;
  SVN_REPOS_DUMPFILE_UUID = 'UUID';
  SVN_REPOS_DUMPFILE_CONTENT_LENGTH = 'Content-length';
  SVN_REPOS_DUMPFILE_REVISION_NUMBER = 'Revision-number';
  SVN_REPOS_DUMPFILE_NODE_PATH = 'Node-path';
  SVN_REPOS_DUMPFILE_NODE_KIND = 'Node-kind';
  SVN_REPOS_DUMPFILE_NODE_ACTION = 'Node-action';
  SVN_REPOS_DUMPFILE_NODE_COPYFROM_PATH = 'Node-copyfrom-path';
  SVN_REPOS_DUMPFILE_NODE_COPYFROM_REV = 'Node-copyfrom-rev';
  SVN_REPOS_DUMPFILE_TEXT_COPY_SOURCE_CHECKSUM = 'Text-copy-source-md5';
  SVN_REPOS_DUMPFILE_TEXT_CONTENT_CHECKSUM = 'Text-content-md5';
  SVN_REPOS_DUMPFILE_PROP_CONTENT_LENGTH = 'Prop-content-length';
  SVN_REPOS_DUMPFILE_TEXT_CONTENT_LENGTH = 'Text-content-length';
  SVN_REPOS_DUMPFILE_PROP_DELTA = 'Prop-delta';
  SVN_REPOS_DUMPFILE_TEXT_DELTA = 'Text-delta';
  SVN_REPOS_DUMPFILE_TEXT_DELTA_BASE_CHECKSUM = 'Text-delta-base-md5';

  SVN_REPOS_CAPABILITY_MERGEINFO = 'mergeinfo';

type
  TSvnReposAuthzFunc = function(out allowed: TSvnBoolean; root: PSvnFSRoot; path: PAnsiChar; baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  TSvnReposAuthzAccess = (
    svnAuthzNone = 0,
    svnAuthzRead = 1,
    svnAuthzWrite = 2,
    svnAuthzRecursive = 4
  );
  TSvnReposAuthzCallback = function(required: TSvnReposAuthzAccess; out allowed: TSvnBoolean; root: PSvnFSRoot;
    path: PAnsiChar; baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  TSvnReposFileRevHandler = function(baton: Pointer; path: PAnsiChar; rev: TSvnRevNum; rev_props: PAprHash;
    out delta_handler: TSvnTxDeltaWindowHandler; out delta_baton: Pointer; prop_diffs: PAprArrayHeader;
    pool: PAprPool): PSvnError; cdecl;
  PSvnRepos = ^TSvnRepos;
  TSvnRepos = THandle;
  TSvnReposRecoverCallback = function(baton: Pointer): PSvnError; cdecl;
  TSvnReposHistoryFunc = function(baton: Pointer; path: PAnsiChar; revision: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  PSvnReposNode = ^TSvnReposNode;
  TSvnReposNode = record
    kind: TSvnNodeKind;
    action: Char;
    text_mod: TSvnBoolean;
    prop_mod: TSvnBoolean;
    name: PAnsiChar;
    copyfrom_rev: TSvnRevNum;
    copyfrom_path: PAnsiChar;
    sibling: PSvnReposNode;
    child: PSvnReposNode;
    parent: PSvnReposNode;
  end;
  TSvnNodeAction = (
    svnNodeActionChange,
    svnNodeActionAdd,
    svnNodeActionDelete,
    svnNodeActionReplace
  );
  TSvnReposLoadUuid = (
    svnReposLoadUuidDefault,
    svnReposLoadUuidIgnore,
    svnReposLoadUuidForce
  );
  PSvnReposParseFns2 = ^TSvnReposParseFns2;
  TSvnReposParseFns2 = record
    new_revision_record: function(out revision_baton: Pointer; headers: PAprHash; parse_baton: Pointer;
      pool: PAprPool): PSvnError; cdecl;
    uuid_record: function(uuid: PAnsiChar; parse_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    new_node_record: function(out node_baton: Pointer; headers: PAprHash; revision_baton: Pointer;
      pool: PAprPool): PSvnError; cdecl;
    set_revision_property: function(revision_baton: Pointer; name: PAnsiChar; value: PSvnString): PSvnError; cdecl;
    set_node_property: function(node_baton: Pointer; name: PAnsiChar; value: PSvnString): PSvnError; cdecl;
    delete_node_property: function(node_baton: Pointer;  name: PAnsiChar): PSvnError; cdecl;
    remove_node_props: function(node_baton: Pointer): PSvnError; cdecl;
    set_fulltext: function(stream: PPSvnStream): PSvnError; cdecl;
    apply_textdelta: function(out handler: TSvnTxDeltaWindowHandler; out handler_baton: Pointer;
      node_baton: Pointer): PSvnError; cdecl;
    close_node: function(node_baton: Pointer): PSvnError; cdecl;
    close_revision: function(revision_baton: Pointer): PSvnError; cdecl;
  end;
  PSvnReposParseFns = ^TSvnReposParseFns;
  TSvnReposParseFns = record
    new_revision_record: function(out revision_baton: Pointer; headers: PAprHash; parse_baton: Pointer;
      pool: PAprPool): PSvnError; cdecl;
    uuid_record: function(uuid: PAnsiChar; parse_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    new_node_record: function(out node_baton: Pointer; headers: PAprHash; revision_baton: Pointer;
      pool: PAprPool): PSvnError; cdecl;
    set_revision_property: function(revision_baton: Pointer; name: PAnsiChar; value: PSvnString): PSvnError; cdecl;
    set_node_property: function(node_baton: Pointer; name: PAnsiChar; value: PSvnString): PSvnError; cdecl;
    remove_node_props: function(node_baton: Pointer): PSvnError; cdecl;
    set_fulltext: function(out stream: PSvnStream; node_baton: Pointer): PSvnError; cdecl;
    close_node: function(node_baton: Pointer): PSvnError; cdecl;
    close_revision: function(revision_baton: Pointer): PSvnError; cdecl;
  end;
  PSvnAuthz = ^TSvnAuthz;
  TSvnAuthz = THandle;
  TSvnReposUpgradeCallback = function(baton: Pointer): PSvnError; cdecl;

type
  TSvnReposRevisionAccessLevel = (
    svnReposRevisionAccessNone,
    svnReposRevisionAccessPartial,
    svnReposRevisionAccessFull
  );

var
  svn_repos_version: function: PSvnVersion; cdecl;
  svn_repos_find_root_path: function(path: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_open: function(out repos_p: PSvnRepos; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_create: function(out repos_p: PSvnRepos; path, unused_1, unused_2: PAnsiChar; config, fs_config: PAprHash;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_upgrade: function(path: PAnsiChar; nonblocking: TSvnBoolean; start_callback: TSvnReposUpgradeCallback;
    start_callback_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_delete: function(path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_has_capability: function(repos: PSvnRepos; out has: TSvnBoolean; capability: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs: function(repos: PSvnRepos): PSvnFS; cdecl;
  svn_repos_hotcopy: function(src_path, dst_path: PAnsiChar; clean_logs: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_repos_recover3: function(path: PAnsiChar; nonblocking: TSvnBoolean; start_callback: TSvnReposRecoverCallback;
    start_callback_baton: Pointer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_recover2: function(path: PAnsiChar; nonblocking: TSvnBoolean; start_callback: TSvnReposRecoverCallback;
    start_callback_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_recover: function(path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_db_logfiles: function(out logfiles: PAprArrayHeader; path: PAnsiChar; only_unused: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_path: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_db_env: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_conf_dir: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_svnserve_conf: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_lock_dir: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_db_lockfile: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_db_logs_lockfile: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_hook_dir: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_start_commit_hook: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_pre_commit_hook: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_post_commit_hook: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_pre_revprop_change_hook: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_post_revprop_change_hook: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_pre_lock_hook: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_post_lock_hook: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_pre_unlock_hook: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_post_unlock_hook: function(repos: PSvnRepos; pool: PAprPool): PAnsiChar; cdecl;
  svn_repos_begin_report2: function(out report_baton: Pointer; revnum: TSvnRevNum; repos: PSvnRepos;
    fs_base, target, tgt_path: PAnsiChar; text_deltas: TSvnBoolean; depth: TSvnDepth;
    ignore_ancestry, send_copy_from_args: TSvnBoolean; editor: PSvnDeltaEditor; edit_baton: Pointer;
    authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_begin_report: function(out report_baton: Pointer; revnum: TSvnRevNum; username: PAnsiChar; repos: PSvnRepos;
    fs_base, target, tgt_path: PAnsiChar; text_deltas, recurse, ignore_ancestry: TSvnBoolean; editor: PSvnDeltaEditor;
    edit_baton: Pointer; authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; pool: PAprPool): PSvnError;
    cdecl;
  svn_repos_set_path3: function(report_baton: Pointer; path: PAnsiChar; revision: TSvnRevNum; depth: TSvnDepth;
    start_empty: TSvnBoolean; lock_token: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_set_path2: function(report_baton: Pointer; path: PAnsiChar; revision: TSvnRevNum; start_empty: TSvnBoolean;
    lock_token: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_set_path: function(report_baton: Pointer; path: PAnsiChar; revision: TSvnRevNum; start_empty: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_link_path3: function(report_baton: Pointer; path, link_path: PAnsiChar; revision: TSvnRevNum; depth: TSvnDepth;
    start_empty: TSvnBoolean; lock_token: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_link_path2: function(report_baton: Pointer; path, link_path: PAnsiChar; revision: TSvnRevNum;
    start_empty: TSvnBoolean; lock_token: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_link_path: function(report_baton: Pointer; path, link_path: PAnsiChar; revision: TSvnRevNum;
    start_empty: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_repos_delete_path: function(report_baton: Pointer; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_finish_report: function(report_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_abort_report: function(report_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_dir_delta2: function(src_root: PSvnFSRoot; src_parent_dir, src_entry: PAnsiChar; tgt_root: PSvnFSRoot;
    tgt_path: PAnsiChar; editor: PSvnDeltaEditor; edit_baton: Pointer; authz_read_func: TSvnReposAuthzFunc;
    authz_read_baton: Pointer; text_deltas: TSvnBoolean; depth: TSvnDepth; entry_props, ignore_ancestry: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_dir_delta: function(src_root: PSvnFSRoot; src_parent_dir, src_entry: PAnsiChar; tgt_root: PSvnFSRoot;
    tgt_path: PAnsiChar; editor: PSvnDeltaEditor; edit_baton: Pointer; authz_read_func: TSvnReposAuthzFunc;
    authz_read_baton: Pointer; text_deltas, recurse, entry_props, ignore_ancestry: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_replay2: function(root: PSvnFSRoot; base_dir: PAnsiChar; low_water_mark: TSvnRevNum; send_deltas: TSvnBoolean;
    editor: PSvnDeltaEditor; edit_baton: Pointer; authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_replay: function(root: PSvnFSRoot; editor: PSvnDeltaEditor; edit_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_commit_editor5: function(out editor: PSvnDeltaEditor; out edit_baton: Pointer; repos: PSvnRepos;
    txn: PSvnFSTxn; repos_url, base_path: PAnsiChar; revprop_table: PAprHash; callback: TSvnCommitCallback2;
    callback_baton: Pointer; authz_callback: TSvnReposAuthzCallback; authz_baton: Pointer; pool: PAprPool): PSvnError;
    cdecl;
  svn_repos_get_commit_editor4: function(out editor: PSvnDeltaEditor; out edit_baton: Pointer; repos: PSvnRepos;
    txn: PSvnFSTxn; repos_url, base_path, user, log_msg: PAnsiChar; callback: TSvnCommitCallback; callback_baton: Pointer;
    authz_callback: TSvnReposAuthzCallback; authz_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_commit_editor3: function(out editor: PSvnDeltaEditor; out edit_baton: Pointer; repos: PSvnRepos;
    txn: PSvnFSTxn; repos_url, base_path, user, log_msg: PAnsiChar; callback: TSvnCommitCallback; callback_baton: Pointer;
    authz_callback: TSvnReposAuthzCallback; authz_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_commit_editor2: function(out editor: PSvnDeltaEditor; out edit_baton: Pointer; repos: PSvnRepos;
    txn: PSvnFSTxn; repos_url, base_path, user, log_msg: PAnsiChar; callback: TSvnCommitCallback; callback_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_commit_editor: function(out editor: PSvnDeltaEditor; out edit_baton: Pointer; repos: PSvnRepos;
    repos_url, base_path, user, log_msg: PAnsiChar; callback: TSvnCommitCallback; callback_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_dated_revision: function(out revision: TSvnRevNum; repos: PSvnRepos; tm: TAprTime;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_committed_info: function(out committed_rev: TSvnRevNum; out committed_date, last_author: PAnsiChar;
    root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_stat: function(out dirent: PSvnDirEnt; root: PSvnFSRoot; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_deleted_rev: function(fs: PSvnFS; path: PAnsiChar; rev_start, rev_end: TSvnRevNum; out deleted: TSvnRevNum;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_history2: function(fs: PSvnFS; path: PAnsiChar; history_func: TSvnReposHistoryFunc; history_baton: Pointer;
    authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; rev_start, rev_end: TSvnRevNum;
    cross_copies: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_repos_history: function(fs: PSvnFS; path: PAnsiChar; history_func: TSvnReposHistoryFunc; history_baton: Pointer;
    rev_start, rev_end: TSvnRevNum; cross_copies: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_repos_trace_node_locations: function(fs: PSvnFS; out locations: PAprHash; fs_path: PAnsiChar;
    peg_revision: TSvnRevNum; location_revisions: PAprArrayHeader; authz_read_func: TSvnReposAuthzFunc;
    authz_read_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_node_location_segments: function(repos: PSvnRepos; path: PAnsiChar; peg_revision, start_rev, end_rev: TSvnRevNum;
    receiver: TSvnLocationSegmentReceiver; receiver_baton: Pointer; authz_read_func: TSvnReposAuthzFunc;
    authz_read_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_logs4: function(repos: PSvnRepos; paths: PAprArrayHeader; rev_start, rev_end: TSvnRevNum;
    limit: Integer; discover_changed_paths, strict_node_history, include_merged_revisions: TSvnBoolean;
    revprops: PAprArrayHeader; authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer;
    receiver: TSvnLogEntryReceiver; receiver_baton: Pointer; pool: PAprPool): PSvnError; stdcall;
  svn_repos_get_logs3: function(repos: PSvnRepos; paths: PAprArrayHeader; rev_start, rev_end: TSvnRevNum;
    limit: Integer; discover_changed_paths, strict_node_history: TSvnBoolean; authz_read_func: TSvnReposAuthzFunc;
    authz_read_baton: Pointer; receiver: TSvnLogMessageReceiver; receiver_baton: Pointer; pool: PAprPool): PSvnError;
    cdecl;
  svn_repos_get_logs2: function(repos: PSvnRepos; paths: PAprArrayHeader; rev_start, rev_end: TSvnRevNum;
    discover_changed_paths, strict_node_history: TSvnBoolean; authz_read_func: TSvnReposAuthzFunc;
    authz_read_baton: Pointer; receiver: TSvnLogMessageReceiver; receiver_baton: Pointer; pool: PAprPool): PSvnError;
    cdecl;
  svn_repos_get_logs: function(repos: PSvnRepos; paths: PAprArrayHeader; rev_start, rev_end: TSvnRevNum;
    discover_changed_paths, strict_node_history: TSvnBoolean; receiver: TSvnLogMessageReceiver; receiver_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_get_mergeinfo: function(out catalog: TSvnMergeInfoCatalog; repos: PSvnRepos; paths: PAprArrayHeader;
    revision: TSvnRevNum; inherit: TSvnMergeInfoInheritance; include_descendants: TSvnBoolean;
    authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_file_revs2: function(repos: PSvnRepos; path: PAnsiChar; rev_start, rev_end: TSvnRevNum;
    include_merged_revisions: TSvnBoolean; authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer;
    handler: TSvnFileRevHandler; handler_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_file_revs: function(repos: PSvnRepos; path: PAnsiChar; rev_start, rev_end: TSvnRevNum;
    authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; handler: TSvnReposFileRevHandler;
    handler_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_commit_txn: function(out conflict_p: PAnsiChar; repos: PSvnRepos; out new_rev: TSvnRevNum; txn: PSvnFSTxn;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_begin_txn_for_commit2: function(out txn_p: PSvnFSTxn; repos: PSvnRepos; rev: TSvnRevNum;
    revprop_table: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_begin_txn_for_commit: function(out txn_p: PSvnFSTxn; repos: PSvnRepos; rev: TSvnRevNum;
    author, log_msg: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_begin_txn_for_update: function(out txn_p: PSvnFSTxn; repos: PSvnRepos; rev: TSvnRevNum; author: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_lock: function(out lock: PSvnLock; repos: PSvnRepos; path, token, comment: PAnsiChar;
    is_dav_comment: TSvnBoolean; expiration_date: TAprTime; current_rev: TSvnRevNum; steal_lock: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_unlock: function(repos: PSvnRepos; path, token: PAnsiChar; break_lock: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_get_locks: function(out locks: PAprHash; repos: PSvnRepos; path: PAnsiChar;
    authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_change_rev_prop3: function(repos: PSvnRepos; rev: TSvnRevNum; author, name: PAnsiChar; new_value: PSvnString;
    use_pre_revprop_change_hook, use_post_revprop_change_hook: TSvnBoolean; authz_read_func: TSvnReposAuthzFunc;
    authz_read_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_change_rev_prop2: function(repos: PSvnRepos; rev: TSvnRevNum; author, name: PAnsiChar; new_value: PSvnString;
    authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_change_rev_prop: function(repos: PSvnRepos; rev: TSvnRevNum; author, name: PAnsiChar; new_value: PSvnString;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_revision_prop: function(out value_p: PSvnString; repos: PSvnRepos; rev: TSvnRevNum; propname: PAnsiChar;
    authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_revision_proplist: function(out table_p: PAprHash; repos: PSvnRepos; rev: TSvnRevNum;
    authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_change_node_prop: function(root: PSvnFSRoot; path, name: PAnsiChar; value: PSvnString;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_fs_change_txn_prop: function(txn: PSvnFSTxn; name: PAnsiChar; value: PSvnString; pool: PAprPool): PSvnError;
    cdecl;
  svn_repos_fs_change_txn_props: function(txn: PSvnFSTxn; props: PAprArrayHeader; pool: PAprPool): PSvnError; cdecl;
  svn_repos_node_editor: function(out editor: PSvnDeltaEditor; out edit_baton: Pointer; repos: PSvnRepos;
    base_root, root: PSvnFSRoot; node_pool, pool: PAprPool): PSvnError; cdecl;
  svn_repos_node_from_baton: function(edit_baton: Pointer): PSvnReposNode; cdecl;
  svn_repos_verify_fs: function(repos: PSvnRepos; feedback_stream: PSvnStream; start_rev, end_rev: TSvnRevNum;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_dump_fs2: function(repos: PSvnRepos; dumpstream, feedback_stream: PSvnStream;
    start_rev, end_rev: TSvnRevNum; incremental, use_deltas: TSvnBoolean; cancel_func: TSvnCancelFunc;
    cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_dump_fs: function(repos: PSvnRepos; dumpstream, feedback_stream: PSvnStream; start_rev, end_rev: TSvnRevNum;
    incremental: TSvnBoolean; cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_load_fs2: function(repos: PSvnRepos; dumpstream, feedback_stream: PSvnStream;
    uuid_action: TSvnReposLoadUuid; parent_dir: PAnsiChar; use_pre_commit_hook, use_post_commit_hook: TSvnBoolean;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_load_fs: function(repos: PSvnRepos; dumpstream, feedback_stream: PSvnStream; uuid_action: TSvnReposLoadUuid;
    parent_dir: PAnsiChar; cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_parse_dumpstream2: function(stream: PSvnStream; parse_fns: PSvnReposParseFns2; parse_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_fs_build_parser2: function(out parser: PSvnReposParseFns2; out parse_baton: Pointer; repos: PSvnRepos;
    use_history: TSvnBoolean; uuid_action: TSvnReposLoadUuid; outstream: PSvnStream; parent_dir: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_parse_dumpstream: function(stream: PSvnStream; parse_fns: PSvnReposParseFns; parse_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_repos_get_fs_build_parser: function(out parser: PSvnReposParseFns; out parse_baton: Pointer; repos: PSvnRepos;
    use_history: TSvnBoolean; uuid_action: TSvnReposLoadUuid; outstream: PSvnStream; parent_dir: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_authz_read: function(out authz_p: PSvnAuthz; afile: PAnsiChar; must_exist: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_repos_authz_check_access: function(authz: PSvnAuthz; repos_name, path, user: PAnsiChar;
    required_access: TSvnReposAuthzAccess; out access_granted: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_repos_check_revision_access: function(out access_level: TSvnReposRevisionAccessLevel; repos: PSvnRepos;
    revision: TSvnRevNum; authz_read_func: TSvnReposAuthzFunc; authz_read_baton: Pointer; pool: PAprPool): PSvnError;
    cdecl;
  svn_repos_remember_client_capabilities: function(repos: PSvnRepos; capabilities: PAprArrayHeader): PSvnError; cdecl;

//----- svn_repos.h ----------------------------------------------------------------------------------------------------

//----- svn_opt.h ------------------------------------------------------------------------------------------------------

const
  SVN_OPT_MAX_ALIASES = 3;
  SVN_OPT_MAX_OPTIONS = 50;
  SVN_OPT_FIRST_LONGOPT_ID = 256;

type
  TSvnOptSubcommand = function(os: PAprGetOpt; baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  TSvnOptSubcommandDescOverride = record
    optch: Integer;
    desc: PAnsiChar;
  end;
  PSvnOptSubcommandDesc2 = ^TSvnOptSubcommandDesc2;
  TSvnOptSubcommandDesc2 = record
    name: PAnsiChar;
    cmd_func: TSvnOptSubcommand;
    aliases: array[0..SVN_OPT_MAX_ALIASES - 1] of PAnsiChar;
    help: PAnsiChar;
    valid_options: array[0..SVN_OPT_MAX_OPTIONS - 1] of Integer;
    desc_overrides: array[0..SVN_OPT_MAX_OPTIONS - 1] of TSvnOptSubcommandDescOverride;
  end;
  PSvnOptSubcommandDesc = ^TSvnOptSubcommandDesc;
  TSvnOptSubcommandDesc = record
    name: PAnsiChar;
    cmd_func: TSvnOptSubcommand;
    aliases: array[0..SVN_OPT_MAX_ALIASES - 1] of PAnsiChar;
    help: PAnsiChar;
    valid_options: array[0..SVN_OPT_MAX_OPTIONS - 1] of Integer;
  end;
  TSvnOptRevisionKind = (
    svnOptRevisionUnspecified,
    svnOptRevisionNumber,
    svnOptRevisionDate,
    svnOptRevisionCommitted,
    svnOptRevisionPrevious,
    svnOptRevisionBase,
    svnOptRevisionWorking,
    svnOptRevisionHead
  );
  PSvnOptRevisionValue = ^TSvnOptRevisionValue;
  TSvnOptRevisionValue = record
    case Boolean of
      False: (number: TSvnRevNum);
      True: (date: TAprTime);
  end;
  PSvnOptRevision = ^TSvnOptRevision;
  TSvnOptRevision = record
    Kind: TSvnOptRevisionKind;
    Value: TSvnOptRevisionValue;
  end;
  PSvnOptRevisionRange = ^TSvnOptRevisionRange;
  TSvnOptRevisionRange = record
    rev_start: TSvnOptRevision;
    rev_end: TSvnOptRevision;
  end;

var
  svn_opt_get_canonical_subcommand2: function(table: PSvnOptSubcommandDesc2; cmd_name: PAnsiChar): PSvnOptSubcommandDesc2; cdecl;
  svn_opt_get_canonical_subcommand: function(table: PSvnOptSubcommandDesc; cmd_name: PAnsiChar): PSvnOptSubcommandDesc;
    cdecl;
  svn_opt_get_option_from_code2: function(code: Integer; option_table: PAprGetOptOption;
    command: PSvnOptSubcommandDesc2; pool: PAprPool): PAprGetOptOption; cdecl;
  svn_opt_get_option_from_code: function(code: Integer; option_table: PAprGetOptOption): PAprGetOptOption; cdecl;
  svn_opt_subcommand_takes_option3: function(command: PSvnOptSubcommandDesc2; option_code: Integer;
    global_options: PInteger): TSvnBoolean; cdecl;
  svn_opt_subcommand_takes_option2: function(command: PSvnOptSubcommandDesc2; option_code: Integer): TSvnBoolean; cdecl;
  svn_opt_subcommand_takes_option: function(command: PSvnOptSubcommandDesc; option_code: Integer): TSvnBoolean; cdecl;
  svn_opt_print_generic_help2: procedure(header: PAnsiChar; cmd_table: PSvnOptSubcommandDesc2; opt_table: PAprGetOptOption;
    footer: PAnsiChar; pool: PAprPool; stream: THandle); cdecl;
  svn_opt_print_generic_help: procedure(header: PAnsiChar; cmd_table: PSvnOptSubcommandDesc; opt_table: PAprGetOptOption;
    footer: PAnsiChar; pool: PAprPool; stream: THandle); cdecl;
  svn_opt_format_option: procedure(str: PPAnsiChar; opt: PAprGetOptOption; doc: TSvnBoolean; pool: PAprPool); cdecl;
  svn_opt_subcommand_help3: procedure(subcommand: PAnsiChar; table: PSvnOptSubcommandDesc2; options_table: PAprGetOptOption;
    global_options: PInteger; pool: PAprPool); cdecl;
  svn_opt_subcommand_help2: procedure(subcommand: PAnsiChar; table: PSvnOptSubcommandDesc2; options_table: PAprGetOptOption;
    pool: PAprPool); cdecl;
  svn_opt_subcommand_help: procedure(subcommand: PAnsiChar; table: PSvnOptSubcommandDesc; options_table: PAprGetOptOption;
    pool: PAprPool); cdecl;
  svn_opt_parse_revision: function(start_revision, end_revision: PSvnOptRevision; arg: PAnsiChar; pool: PAprPool): Integer;
    cdecl;
  svn_opt_parse_revision_to_range: function(opt_ranges: PAprArrayHeader; arg: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_opt_resolve_revisions: function(peg_rev, op_rev: PSvnOptRevision; is_url, notice_local_mods: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_opt_args_to_target_array3: function(out targets_p: PAprArrayHeader; os: PAprGetOpt;
    known_targets: PAprArrayHeader; pool: PAprPool): PSvnError; cdecl;
  svn_opt_args_to_target_array2: function(out targets_p: PAprArrayHeader; os: PAprGetOpt;
    known_targets: PAprArrayHeader; pool: PAprPool): PSvnError; cdecl;
  svn_opt_args_to_target_array: function(out targets_p: PAprArrayHeader; os: PAprGetOpt; known_targets: PAprArrayHeader;
    start_revision, end_revision: PSvnOptRevision; extract_revisions: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_opt_push_implicit_dot_target: procedure(targets: PAprArrayHeader; pool: PAprPool); cdecl;
  svn_opt_parse_num_args: function(out args_p: PAprArrayHeader; os: PAprGetOpt; num_args: Integer;
    pool: PAprPool): PSvnError; cdecl;
  svn_opt_parse_all_args: function(out args_p: PAprArrayHeader; os: PAprGetOpt; pool: PAprPool): PSvnError; cdecl;
  svn_opt_parse_path: function(out rev: TSvnOptRevision; out truepath: PAnsiChar; path: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_opt_print_help3: function(os: PAprGetOpt; pgm_name: PAnsiChar; print_version, quiet: TSvnBoolean;
    version_footer, header: PAnsiChar; cmd_table: PSvnOptSubcommandDesc2; option_table: PAprGetOptOption;
    global_options: PInteger; footer: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_opt_print_help2: function(os: PAprGetOpt; pgm_name: PAnsiChar; print_version, quiet: TSvnBoolean;
    version_footer, header: PAnsiChar; cmd_table: PSvnOptSubcommandDesc2; option_table: PAprGetOptOption; footer: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_opt_print_help: function(os: PAprGetOpt; pgm_name: PAnsiChar; print_version, quiet: TSvnBoolean;
    version_footer, header: PAnsiChar; cmd_table: PSvnOptSubcommandDesc; option_table: PAprGetOptOption; footer: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;

//----- svn_opt.h ------------------------------------------------------------------------------------------------------

//----- svn_auth.h -----------------------------------------------------------------------------------------------------

const
  SVN_AUTH_CRED_SIMPLE = 'svn.simple';
  SVN_AUTH_CRED_USERNAME = 'svn.username';
  SVN_AUTH_CRED_SSL_CLIENT_CERT = 'svn.ssl.client-cert';
  SVN_AUTH_CRED_SSL_CLIENT_CERT_PW = 'svn.ssl.client-passphrase';
  SVN_AUTH_CRED_SSL_SERVER_TRUST = 'svn.ssl.server';

  SVN_AUTH_SSL_NOTYETVALID = $00000001;
  SVN_AUTH_SSL_EXPIRED     = $00000002;
  SVN_AUTH_SSL_CNMISMATCH  = $00000004;
  SVN_AUTH_SSL_UNKNOWNCA   = $00000008;
  SVN_AUTH_SSL_OTHER       = $40000000;

  SVN_AUTH_PARAM_PREFIX = 'svn:auth:';
  SVN_AUTH_PARAM_DEFAULT_USERNAME                 = SVN_AUTH_PARAM_PREFIX + 'username';
  SVN_AUTH_PARAM_DEFAULT_PASSWORD                 = SVN_AUTH_PARAM_PREFIX + 'password';
  SVN_AUTH_PARAM_NON_INTERACTIVE                  = SVN_AUTH_PARAM_PREFIX + 'non-interactive';
  SVN_AUTH_PARAM_DONT_STORE_PASSWORDS             = SVN_AUTH_PARAM_PREFIX + 'dont-store-passwords';
  SVN_AUTH_PARAM_NO_AUTH_CACHE                    = SVN_AUTH_PARAM_PREFIX + 'no-auth-cache';
  SVN_AUTH_PARAM_SSL_SERVER_FAILURES              = SVN_AUTH_PARAM_PREFIX + 'ssl:failures';
  SVN_AUTH_PARAM_SSL_SERVER_CERT_INFO             = SVN_AUTH_PARAM_PREFIX + 'ssl:cert-info';
  SVN_AUTH_PARAM_CONFIG                           = SVN_AUTH_PARAM_PREFIX + 'config';
  SVN_AUTH_PARAM_SERVER_GROUP                     = SVN_AUTH_PARAM_PREFIX + 'server-group';
  SVN_AUTH_PARAM_CONFIG_DIR                       = SVN_AUTH_PARAM_PREFIX + 'config-dir';

  SVN_RA_ABI_VERSION = 2;

type
  PSvnAuthBaton = ^TSvnAuthBaton;
  TSvnAuthBaton = THandle;
  PSvnAuthIterState = ^TSvnAuthIterState;
  TSvnAuthIterState = THandle;

  PSvnAuthProvider = ^TSvnAuthProvider;
  TSvnAuthProvider = record
    cred_kind: PAnsiChar;
    first_credentials: function(out credentials, iter_baton: Pointer; provider_baton: Pointer; parameters: PAprHash;
      realmstring: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
    next_credentials: function(out credentials: Pointer; iter_baton, provider_baton: Pointer; parameters: PAprHash;
      realmstring: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
    save_credentials: function(out saved: TSvnBoolean; credentials, provider_baton: Pointer; parameters: PAprHash;
      realmstring: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  end;
  PPSvnAuthProviderObject = ^PSvnAuthProviderObject;
  PSvnAuthProviderObject = ^TSvnAuthProviderObject;
  TSvnAuthProviderObject = record
    vtable: PSvnAuthProvider;
    provider_baton: Pointer;
  end;
  PSvnAuthCredSimple = ^TSvnAuthCredSimple;
  TSvnAuthCredSimple = record
    username: PAnsiChar;
    password: PAnsiChar;
    may_save: TSvnBoolean;
  end;
  PSvnAuthCredUsername = ^TSvnAuthCredUsername;
  TSvnAuthCredUsername = record
    username: PAnsiChar;
    may_save: TSvnBoolean;
  end;
  PSvnAuthCredSSLClientCert = ^TSvnAuthCredSSLClientCert;
  TSvnAuthCredSSLClientCert = record
    cert_file: PAnsiChar;
    may_save: TSvnBoolean;
  end;
  PSvnAuthCredSSLClientCertPw = ^TSvnAuthCredSSLClientCertPw;
  TSvnAuthCredSSLClientCertPw = record
    password: PAnsiChar;
    may_save: TSvnBoolean;
  end;
  PSvnAuthSSLServerCertInfo = ^TSvnAuthSSLServerCertInfo;
  TSvnAuthSSLServerCertInfo = record
    hostname: PAnsiChar;
    fingerprint: PAnsiChar;
    valid_from: PAnsiChar;
    valid_until: PAnsiChar;
    issuer_dname: PAnsiChar;
    ascii_cert: PAnsiChar;
  end;
  PSvnAuthCredSSLServerTrust = ^TSvnAuthCredSSLServerTrust;
  TSvnAuthCredSSLServerTrust = record
    may_save: TSvnBoolean;
    accepted_failures: Cardinal;
  end;
  
  TSvnAuthSimplePromptFunc = function(out cred: PSvnAuthCredSimple; baton: Pointer; realm, username: PAnsiChar;
    may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  TSvnAuthUsernamePromptFunc = function(out cred: PSvnAuthCredUsername; baton: Pointer; realm: PAnsiChar;
    may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  TSvnAuthSSLServerTrustPromptFunc = function(out cred: PSvnAuthCredSSLServerTrust; baton: Pointer; realm: PAnsiChar;
    failures: Cardinal; cert_info: PSvnAuthSSLServerCertInfo; may_save: TSvnBoolean; pool: PAprPool): PSvnError;
    cdecl;
  TSvnAuthSSLClientCertPromptFunc = function(out cred: PSvnAuthCredSSLClientCert; baton: Pointer; realm: PAnsiChar;
    may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  TSvnAuthSSLClientCertPwPromptFunc = function(out cred: PSvnAuthCredSSLClientCertPw; baton: Pointer; realm: PAnsiChar;
    may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;

var
  svn_auth_ssl_server_cert_info_dup: function(info: PSvnAuthSSLServerCertInfo;
    pool: PAprPool): PSvnAuthSSLServerCertInfo; cdecl;
  svn_auth_open: procedure(out auth_baton: PSvnAuthBaton; providers: PAprArrayHeader; pool: PAprPool); cdecl;
  svn_auth_set_parameter: procedure(auth_baton: PSvnAuthBaton; name: PAnsiChar; value: Pointer); cdecl;
  svn_auth_get_parameter: function(auth_baton: PSvnAuthBaton; name: PAnsiChar): Pointer; cdecl;
  svn_auth_first_credentials: function(out credentials: Pointer; out state: PSvnAuthIterState;
    cred_kind, realmstring: PAnsiChar; auth_baton: PSvnAuthBaton; pool: PAprPool): PSvnError; cdecl;
  svn_auth_next_credentials: function(out credentials: Pointer; state: PSvnAuthIterState; pool: PAprPool): PSvnError;
    cdecl;
  svn_auth_save_credentials: function(state: PSvnAuthIterState; pool: PAprPool): PSvnError; cdecl;
  svn_auth_get_simple_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthSimplePromptFunc; prompt_baton: Pointer; retry_limit: Integer; pool: PAprPool); cdecl;
  svn_auth_get_username_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthUsernamePromptFunc; prompt_baton: Pointer; retry_limit: Integer; pool: PAprPool); cdecl;
  svn_auth_get_simple_provider: procedure (out provider: PSvnAuthProviderObject; pool: PAprPool); cdecl;
  svn_auth_get_windows_simple_provider: procedure (out provider: PSvnAuthProviderObject; pool: PAprPool); cdecl;
  svn_auth_get_username_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool); cdecl;
  svn_auth_get_ssl_server_trust_file_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool); cdecl;
  svn_auth_get_windows_ssl_server_trust_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool); cdecl;
  svn_auth_get_ssl_client_cert_file_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool); cdecl;
  svn_auth_get_ssl_client_cert_pw_file_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool); cdecl;
  svn_auth_get_ssl_server_trust_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthSSLServerTrustPromptFunc; prompt_baton: Pointer; pool: PAprPool); cdecl;
  svn_auth_get_ssl_client_cert_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthSSLClientCertPromptFunc; prompt_baton: Pointer; retry_limit: Integer; pool: PAprPool); cdecl;
  svn_auth_get_ssl_client_cert_pw_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthSSLClientCertPwPromptFunc; prompt_baton: Pointer; retry_limit: Integer; pool: PAprPool); cdecl;

//----- svn_auth.h -----------------------------------------------------------------------------------------------------

//----- svn_cmdline.h --------------------------------------------------------------------------------------------------

type
  PSvnCmdlinePromptBaton = ^TSvnCmdlinePromptBaton;
  TSvnCmdlinePromptBaton = record
    cancel_func: TSvnCancelFunc;
    cancel_baton: Pointer;
  end;

var
  svn_cmdline_init: function(progname: PAnsiChar; error_stream: THandle): Integer; cdecl;
  svn_cmdline_cstring_from_utf8: function(out dest: PAnsiChar; src: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_cstring_from_utf8_fuzzy: function(src: PAnsiChar; pool: PAprPool): PAnsiChar; cdecl;
  svn_cmdline_cstring_to_utf8: function(out dest: PAnsiChar; src: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_path_local_style_from_utf8: function(out dest: PAnsiChar; src: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_printf: function(pool: PAprPool; fmt: PAnsiChar; const args: array of const): PSvnError; cdecl;
  svn_cmdline_fprintf: function(stream: THandle; pool: PAprPool; fmt: PAnsiChar; const args: array of const): PSvnError;
    cdecl;
  svn_cmdline_fputs: function(str: PAnsiChar; stream: THandle; pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_fflush: function(stream: THandle): PSvnError; cdecl;
  svn_cmdline_output_encoding: function(pool: PAprPool): PAnsiChar; cdecl;
  svn_cmdline_handle_exit_error: function(error: PSvnError; pool: PAprPool; prefix: PAnsiChar): Integer; cdecl;
  svn_cmdline_prompt_user2: function(out result: PAnsiChar; prompt_str: PAnsiChar; baton: PSvnCmdLinePromptBaton;
    pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_prompt_user: function(out result: PAnsiChar; prompt_str: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_auth_simple_prompt: function(out cred_p: PSvnAuthCredSimple; baton: Pointer; realm, username: PAnsiChar;
    may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_auth_username_prompt: function(out cred_p: PSvnAuthCredUsername; baton: Pointer; realm: PAnsiChar;
    may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_auth_ssl_server_trust_prompt: function(out cred_p: PSvnAuthCredSSLServerTrust; baton: Pointer;
    realm: PAnsiChar; failures: Cardinal; cert_info: PSvnAuthSSLServerCertInfo; may_save: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_auth_ssl_client_cert_prompt: function(out cred_p: PSvnAuthCredSSLClientCert; baton: Pointer; realm: PAnsiChar;
    may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_auth_ssl_client_cert_pw_prompt: function(out cred_p: PSvnAuthCredSSLClientCertPw; baton: Pointer;
    realm: PAnsiChar; may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_cmdline_setup_auth_baton: function(out ab: PSvnAuthBaton; non_interactive: TSvnBoolean;
    username, password, config_dir: PAnsiChar; no_auth_cache: TSvnBoolean; cfg: PSvnConfig; cancel_func: TSvnCancelFunc;
    cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_cmdline__getopt_init: function(out os: PAprGetOpt; argc: Integer; argv: array of const;
    pool: PAprPool): PSvnError; cdecl;

//----- svn_cmdline.h --------------------------------------------------------------------------------------------------

//----- svn_ra.h -------------------------------------------------------------------------------------------------------

const
  SVN_RA_CAPABILITY_DEPTH = 'depth';
  SVN_RA_CAPABILITY_MERGEINFO = 'mergeinfo';
  SVN_RA_CAPABILITY_LOG_REVPROPS = 'log-revprops';
  SVN_RA_CAPABILITY_PARTIAL_REPLAY = 'partial-replay';
  SVN_RA_CAPABILITY_COMMIT_REVPROPS = 'commit-revprops';

type
  TSvnRaGetWCPropFunc = function(baton: Pointer; relpath, name: PAnsiChar; value: PPSvnString; pool: PAprPool): PSvnError;
    cdecl;
  TSvnRaSetWCPropFunc = function(baton: Pointer; path, name: PAnsiChar; value: PSvnString; pool: PAprPool): PSvnError;
    cdecl;
  TSvnRaPushWCPropFunc = function(baton: Pointer; path, name: PAnsiChar; value: PSvnString; pool: PAprPool): PSvnError;
    cdecl;
  TSvnRaInvalidateWCPropsFunc = function(baton: Pointer; path, name: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  TSvnRaGetLatestRevNumFunc = function(session_baton: Pointer; out latest_revnum: TSvnRevNum): PSvnError; cdecl;
  TSvnRaGetClientStringFunc = function(baton: Pointer; var name: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  TSvnRaFileRevHandler = function(baton: Pointer; path: PAnsiChar; rev: TSvnRevNum; rev_props: PAprHash;
    out delta_handler: TSvnTxDeltaWindowHandler; out delta_baton: Pointer; prop_diffs: PAprArrayHeader;
    pool: PAprPool): PSvnError; cdecl;
  TSvnRaLockCallback = function(baton: Pointer; path: PAnsiChar; do_lock: TSvnBoolean; lock: PSvnLock; ra_err: PSvnError;
    pool: PAprPool): PSvnError; cdecl;
  TSvnRaProgressNotifyFunc = procedure(progress, total: TAprOff; baton: Pointer; pool: PAprPool); cdecl;
  TSvnRaReplayRevStartCallback = function(revision: TSvnRevNum; replay_baton: Pointer; out editor: PSvnDeltaEditor;
    out edit_baton: Pointer; rev_props: PAprHash; pool: PAprPool): PSvnError; cdecl;
  TSvnRaReplayRevFinishCallback = function(revision: TSvnRevNum; replay_baton: Pointer; out editor: TSvnDeltaEditor;
    edit_baton: Pointer; rev_props: PAprHash; pool: PAprPool): PSvnError; cdecl;

  PSvnRaReporter3 = ^TSvnRaReporter3;
  TSvnRaReporter3 = record
    set_path: function(report_baton: Pointer; path: PAnsiChar; revision: TSvnRevNum; start_empty: TSvnBoolean;
      lock_token: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
    delete_path: function(report_baton: Pointer; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
    link_path: function(report_baton: Pointer; path, url: PAnsiChar; revision: TSvnRevNum; depth: TSvnDepth;
      start_empty: TSvnBoolean; lock_token: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
    finish_report: function(report_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    abort_report: function(report_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  end;
  PSvnRaReporter2 = ^TSvnRaReporter2;
  TSvnRaReporter2 = record
    set_path: function(report_baton: Pointer; path: PAnsiChar; revision: TSvnRevNum; start_empty: TSvnBoolean;
      lock_token: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
    delete_path: function(report_baton: Pointer; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
    link_path: function(report_baton: Pointer; path, url: PAnsiChar; revision: TSvnRevNum; start_empty: TSvnBoolean;
      lock_token: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
    finish_report: function(report_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    abort_report: function(report_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  end;

  PSvnRaReporter = ^TSvnRaReporter;
  TSvnRaReporter = record
    set_path: function(report_baton: Pointer; path: PAnsiChar; revision: TSvnRevNum; start_empty: TSvnBoolean;
      pool: PAprPool): PSvnError; cdecl;
    delete_path: function(report_baton: Pointer; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
    link_path: function(report_baton: Pointer; path, url: PAnsiChar; revision: TSvnRevNum; start_empty: TSvnBoolean;
      pool: PAprPool): PSvnError; cdecl;
    finish_report: function(report_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    abort_report: function(report_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  end;

  PSvnRaCallbacks2 = ^TSvnRaCallbacks2;
  TSvnRaCallbacks2 = record
    open_tmp_file: function(out fp: PAprFile; callback_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    auth_baton: PSvnAuthBaton;
    get_wc_prop: TSvnRaGetWCPropFunc;
    set_wc_prop: TSvnRaSetWCPropFunc;
    push_wc_prop: TSvnRaPushWCPropFunc;
    invalidate_wc_props: TSvnRaInvalidateWCPropsFunc;
    progress_func: TSvnRaProgressNotifyFunc;
    progress_baton: Pointer;
    cancel_func: TSvnCancelFunc;
    get_client_string: TSvnRaGetClientStringFunc;
  end;

  PSvnRaCallbacks = ^TSvnRaCallbacks;
  TSvnRaCallbacks = record
    open_tmp_file: function(out fp: PAprFile; callback_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    auth_baton: PSvnAuthBaton;
    get_wc_prop: TSvnRaGetWCPropFunc;
    set_wc_prop: TSvnRaSetWCPropFunc;
    push_wc_prop: TSvnRaPushWCPropFunc;
    invalidate_wc_props: TSvnRaInvalidateWCPropsFunc;
  end;

  PSvnRaSession = ^TSvnRaSession;
  TSvnRaSession = THandle;

  PSvnRaPlugin = ^TSvnRaPlugin;
  TSvnRaPlugin = record
    name: PAnsiChar;
    description: PAnsiChar;
    open: function(out session_baton: Pointer; repos_URL: PAnsiChar; callbacks: PSvnRaCallbacks; callback_baton: Pointer;
      config: PAprHash; pool: PAprPool): PSvnError; cdecl;
    get_latest_revnum: function(session_baton: Pointer; out latest_revnum: TSvnRevNum; pool: PAprPool): PSvnError;
      cdecl;
    get_dated_revision: function(session_baton: Pointer; out revision: TSvnRevNum; tm: TAprTime;
      pool: PAprPool): PSvnError; cdecl;
    change_rev_prop: function(session_baton: Pointer; rev: TSvnRevNum; name: PAnsiChar; value: PSvnString;
      pool: PAprPool): PSvnError; cdecl;
    rev_proplist: function(session_baton: Pointer; rev: TSvnRevNum; out props: PAprHash; pool: PAprPool): PSvnError;
      cdecl;
    rev_prop: function(session_baton: Pointer; rev: TSvnRevNum; name: PAnsiChar; out value: PSvnString;
      pool: PAprPool): PSvnError; cdecl;
    get_commit_editor: function(session_baton: Pointer; out editor: PSvnDeltaEditor; out edit_baton: Pointer;
      log_msg: PAnsiChar; callback: TSvnCommitCallback; callback_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    get_file: function(session_baton: Pointer; path: PAnsiChar; revision: TSvnRevNum; stream: PSvnStream;
      fetched_rev: TSvnRevNum; out props: PAprHash; pool: PAprPool): PSvnError; cdecl;
    get_dir: function(session_baton: Pointer; path: PAnsiChar; revision: TSvnRevNum; out dirents: PAprHash;
      out fetched_rev: TSvnRevNum; out props: PAprHash; pool: PAprPool): PSvnError; cdecl;
    do_update: function(session_baton: Pointer; out reporter: PSvnRaReporter; out report_baton: Pointer;
      revision_to_update_to: TSvnRevNum; update_target: PAnsiChar; recurse: TSvnBoolean; update_editor: TSvnDeltaEditor;
      update_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    do_switch: function(session_baton: Pointer; out reporter: PSvnRaReporter; out report_baton: Pointer;
      revision_to_switch_to: TSvnRevNum; switch_target: PAnsiChar; recurse: TSvnBoolean; switch_url: PAnsiChar;
      switch_editor: PSvnDeltaEditor; switch_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    do_status: function(session_baton: Pointer; out reporter: PSvnRaReporter; out report_baton: Pointer;
      status_target: PAnsiChar; revision: TSvnRevNum; recurse: TSvnBoolean; status_editor: PSvnDeltaEditor;
      status_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    do_diff: function(session_baton: Pointer; out reporter: PSvnRaReporter; out report_baton: Pointer;
      revision: TSvnRevNum; diff_target: PAnsiChar; recurse, ignore_ancestry: TSvnBoolean; versus_url: PAnsiChar;
      diff_editor: PSvnDeltaEditor; diff_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    get_log: function(session_baton: Pointer; paths: PAprArrayHeader; revstart, revend: TSvnRevNum;
      discover_changed_paths, strict_node_history: TSvnBoolean; receiver: TSvnLogMessageReceiver;
      receiver_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    check_path: function(session_baton: Pointer; path: PAnsiChar; revision: TSvnRevNum; out kind: TSvnNodeKind;
      pool: PAprPool): PSvnError; cdecl;
    get_uuid: function(session_baton: Pointer; out uuid: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
    get_repos_root: function(session_baton: Pointer; out url: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
    get_locations: function(session_baton: Pointer; out locations: PAprHash; path: PAnsiChar; peg_revision: TSvnRevNum;
      location_revisions: PAprArrayHeader; pool: PAprPool): PSvnError; cdecl;
    get_file_revs: function(session_baton: Pointer; path: PAnsiChar; revstart, revend: TSvnRevNum;
      handler: TSvnRaFileRevHandler; handler_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    get_version: function: PSvnVersion; cdecl;
  end;

  TSvnRaInitFunc = function(abi_version: Integer; pool: PAprPool; hash: PAprHash): PSvnError; cdecl;

var
  svn_ra_version: function: PSvnVersion; cdecl;
  svn_ra_initialize: function(pool: PAprPool): PSvnError; cdecl;
  svn_ra_create_callbacks: function(out callbacks: PSvnRaCallbacks2; pool: PAprPool): PSvnError; cdecl;
  svn_ra_open3: function(out session_p: PSvnRaSession; repos_URL, uuid: PAnsiChar; callbacks: PSvnRaCallbacks2;
    callback_baton: Pointer; config: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_ra_open2: function(out session_p: PSvnRaSession; repos_URL: PAnsiChar; callbacks: PSvnRaCallbacks2;
    callback_baton: Pointer; config: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_ra_open: function(out session_p: PSvnRaSession; repos_URL: PAnsiChar; callbacks: PSvnRaCallbacks;
    callback_baton: Pointer; config: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_ra_reparent: function(ra_session: PSvnRaSession; url: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_session_url: function(ra_session: PSvnRaSession; out url: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_latest_revnum: function(session: PSvnRaSession; out latest_revnum: TSvnRevNum; pool: PAprPool): PSvnError;
    cdecl;
  svn_ra_get_dated_revision: function(session: PSvnRaSession; out revision: TSvnRevNum; tm: TAprTime;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_change_rev_prop: function(session: PSvnRaSession; rev: TSvnRevNum; name: PAnsiChar; value: PSvnString;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_rev_proplist: function(session: PSvnRaSession; rev: TSvnRevNum; out props: PAprHash;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_rev_prop: function(session: PSvnRaSession; rev: TSvnRevNum; name: PAnsiChar; out value: PSvnString;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_commit_editor3: function(session: PSvnRaSession; out editor: PSvnDeltaEditor; out edit_baton: Pointer;
    revprop_table: PAprHash; callback: TSvnCommitCallback2; callback_baton: Pointer; lock_tokens: PAprHash;
    keep_locks: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_commit_editor2: function(session: PSvnRaSession; out editor: PSvnDeltaEditor; out edit_baton: Pointer;
    log_msg: PAnsiChar; callback: TSvnCommitCallback2; callback_baton: Pointer; lock_tokens: PAprHash;
    keep_locks: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_commit_editor: function(session: PSvnRaSession; out editor: PSvnDeltaEditor; out edit_baton: Pointer;
    log_msg: PAnsiChar; callback: TSvnCommitCallback; callback_baton: Pointer; lock_tokens: PAprHash;
    keep_locks: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_file: function(session: PSvnRaSession; path: PAnsiChar; revision: TSvnRevNum; stream: PSvnStream;
    out fetched_rev: TSvnRevNum; out props: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_dir2: function(session: PSvnRaSession; out dirents: PAprHash; out fetched_rev: TSvnRevNum;
    out props: PAprHash; path: PAnsiChar; revision: TSvnRevNum; dirent_fields: Cardinal; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_dir: function(session: PSvnRaSession; path: PAnsiChar; revision: TSvnRevNum; out dirents: PAprHash;
    out fetched_rev: TSvnRevNum; out props: PAprHash; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_mergeinfo: function(session: PSvnRaSession; out catalog: TSvnMergeInfoCatalog; paths: PAprArrayHeader;
    revision: TSvnRevNum; inherit: TSvnMergeInfoInheritance; include_descendants: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_do_update2: function(session: PSvnRaSession; out reporter: PSvnRaReporter3; out report_baton: Pointer;
    revision_to_update_to: TSvnRevNum; update_target: PAnsiChar; depth: TSvnDepth; send_copyfrom_args: TSvnBoolean;
    update_editor: PSvnDeltaEditor; update_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_do_update: function(session: PSvnRaSession; out reporter: PSvnRaReporter2; out report_baton: Pointer;
    revision_to_update_to: TSvnRevNum; update_target: PAnsiChar; recurse: TSvnBoolean; update_editor: PSvnDeltaEditor;
    update_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_do_switch2: function(session: PSvnRaSession; out reporter: PSvnRaReporter3; out report_baton: Pointer;
    revision_to_switch_to: TSvnRevNum; switch_target: PAnsiChar; depth: TSvnDepth; switch_url: PAnsiChar;
    switch_editor: PSvnDeltaEditor; switch_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_do_switch: function(session: PSvnRaSession; out reporter: PSvnRaReporter2; out report_baton: Pointer;
    revision_to_switch_to: TSvnRevNum; switch_target: PAnsiChar; recurse: TSvnBoolean; switch_url: PAnsiChar;
    switch_editor: PSvnDeltaEditor; switch_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_do_status2: function(session: PSvnRaSession; out reporter: PSvnRaReporter3; out report_baton: Pointer;
    status_target: PAnsiChar; revision: TSvnRevNum; depth: TSvnDepth; status_editor: PSvnDeltaEditor;
    status_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_do_status: function(session: PSvnRaSession; out reporter: PSvnRaReporter2; out report_baton: Pointer;
    status_target: PAnsiChar; revision: TSvnRevNum; recurse: TSvnBoolean; status_editor: PSvnDeltaEditor;
    status_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_do_diff3: function(session: PSvnRaSession; out reporter: PSvnRaReporter3; out report_baton: Pointer;
    revision: TSvnRevNum; diff_target: PAnsiChar; depth: TSvnDepth; ignore_ancestry, text_deltas: TSvnBoolean;
    versus_url: PAnsiChar; diff_editor: PSvnDeltaEditor; diff_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_do_diff2: function(session: PSvnRaSession; out reporter: PSvnRaReporter2; out report_baton: Pointer;
    revision: TSvnRevNum; diff_target: PAnsiChar; recurse, ignore_ancestry, text_deltas: TSvnBoolean; versus_url: PAnsiChar;
    diff_editor: PSvnDeltaEditor; diff_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_do_diff: function(session: PSvnRaSession; out reporter: PSvnRaReporter2; out report_baton: Pointer;
    revision: TSvnRevNum; diff_target: PAnsiChar; recurse, ignore_ancestry: TSvnBoolean; versus_url: PAnsiChar;
    diff_editor: PSvnDeltaEditor; diff_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_log2: function(session: PSvnRaSession; paths: PAprArrayHeader; revstart, revend: TSvnRevNum; limit: Integer;
    discover_changed_paths, strict_node_history, include_merged_revisions: TSvnBoolean; revprops: PAprArrayHeader;
    receiver: TSvnLogEntryReceiver; receiver_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_log: function(session: PSvnRaSession; paths: PAprArrayHeader; revstart, revend: TSvnRevNum; limit: Integer;
    discover_changed_paths, strict_node_history: TSvnBoolean; receiver: TSvnLogMessageReceiver; receiver_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_check_path: function(session: PSvnRaSession; path: PAnsiChar; revision: TSvnRevNum; out kind: TSvnNodeKind;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_stat: function(session: PSvnRaSession; path: PAnsiChar; revision: TSvnRevNum; dirent: PPSvnDirEnt;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_uuid2: function(session: PSvnRaSession; out uuid: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_uuid: function(session: PSvnRaSession; out uuid: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_repos_root2: function(session: PSvnRaSession; out url: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_repos_root: function(session: PSvnRaSession; out url: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_location_segments: function(session: PSvnRaSession; path: PAnsiChar;
    peg_revision, start_rev, end_rev: TSvnRevNum; receiver: TSvnLocationSegmentReceiver; receiver_baton: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_locations: function(session: PSvnRaSession; out locations: PAprHash; path: PAnsiChar; peg_revision: TSvnRevNum;
    location_revisions: PAprArrayHeader; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_file_revs2: function(session: PSvnRaSession; path: PAnsiChar; revstart, revend: TSvnRevNum;
    include_merged_revisions: TSvnBoolean; handler: TSvnRaFileRevHandler; handler_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_file_revs: function(session: PSvnRaSession; path: PAnsiChar; revstart, revend: TSvnRevNum;
    handler: TSvnRaFileRevHandler; handler_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_lock: function(session: PSvnRaSession; path_revs: PAprHash; comment: PAnsiChar; steal_lock: TSvnBoolean;
    lock_func: TSvnRaLockCallback; lock_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_unlock: function(session: PSvnRaSession; path_tokens: PAprHash; break_lock: TSvnBoolean;
    lock_func: TSvnRaLockCallback; lock_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_lock: function(session: PSvnRaSession; out lock: PSvnLock; path: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_ra_get_locks: function(session: PSvnRaSession; out locks: PAprHash; path: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_ra_replay_range: function(session: PSvnRaSession; start_revision, end_revision, low_water_mark: TSvnRevNum;
    send_deltas: TSvnBoolean; revstart_func: TSvnRaReplayRevStartCallback; revfinish_func: TSvnRaReplayRevFinishCallback;
    replay_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_replay: function(session: PSvnRaSession; revision, low_water_mark: TSvnRevNum; send_deltas: TSvnBoolean;
    editor: PSvnDeltaEditor; edit_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_has_capability: function(session: PSvnRaSession; out has: TSvnBoolean; capability: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_ra_print_modules: function(out output: TSvnStringBuf; pool: PAprPool): PSvnError; cdecl;
  svn_ra_print_ra_libraries: function(out descriptions: PSvnStringBuf; ra_baton: Pointer; pool: PAprPool): PSvnError;
    cdecl;

  svn_ra_dav_init: function(abi_version: Integer; pool: PAprPool; hash: PAprHash): PSvnError; cdecl;
  svn_ra_local_init: function(abi_version: Integer; pool: PAprPool; hash: PAprHash): PSvnError; cdecl;
  svn_ra_svn_init: function(abi_version: Integer; pool: PAprPool; hash: PAprHash): PSvnError; cdecl;
  svn_ra_serf_init: function(abi_version: Integer; pool: PAprPool; hash: PAprHash): PSvnError; cdecl;
  svn_ra_init_ra_libs: function(out ra_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_ra_get_ra_library: function(out lib: PSvnRaPlugin; ra_baton: Pointer; url: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;

//----- svn_ra.h -------------------------------------------------------------------------------------------------------

//----- svn_ra_svn.h ---------------------------------------------------------------------------------------------------

const
  SVN_RA_SVN_PORT = 3690;

  SVN_RA_SVN_CAP_EDIT_PIPELINE = 'edit-pipeline';
  SVN_RA_SVN_CAP_SVNDIFF1 = 'svndiff1';
  SVN_RA_SVN_CAP_ABSENT_ENTRIES = 'absent-entries';
  SVN_RA_SVN_CAP_COMMIT_REVPROPS = 'commit-revprops';
  SVN_RA_SVN_CAP_MERGEINFO = 'mergeinfo';
  SVN_RA_SVN_CAP_DEPTH = 'depth';
  SVN_RA_SVN_CAP_LOG_REVPROPS = 'log-revprops';
  SVN_RA_SVN_CAP_PARTIAL_REPLAY = 'partial-replay';

  SVN_RA_SVN_DIRENT_KIND = 'kind';
  SVN_RA_SVN_DIRENT_SIZE = 'size';
  SVN_RA_SVN_DIRENT_HAS_PROPS = 'has-props';
  SVN_RA_SVN_DIRENT_CREATED_REV = 'created-rev';
  SVN_RA_SVN_DIRENT_TIME = 'time';
  SVN_RA_SVN_DIRENT_LAST_AUTHOR = 'last-author';

  SVN_RA_SVN_UNSPECIFIED_NUMBER = High(Int64);

//----- svn_ra_svn.h ---------------------------------------------------------------------------------------------------

//----- svn_wc.h -------------------------------------------------------------------------------------------------------

const
  SVN_WC_TRANSLATE_FROM_NF = $00000000;
  SVN_WC_TRANSLATE_TO_NF = $00000001;
  SVN_WC_TRANSLATE_FORCE_EOL_REPAIR = $00000002;
  SVN_WC_TRANSLATE_NO_OUTPUT_CLEANUP = $00000004;
  SVN_WC_TRANSLATE_FORCE_COPY = $00000008;
  SVN_WC_TRANSLATE_USE_GLOBAL_TMP = $00000010;
  
  SVN_WC_ADM_DIR_NAME = '.svn';
  SVN_WC_ENTRY_THIS_DIR = '';

  SVN_WC_ENTRY_WORKING_SIZE_UNKNOWN = -1;

type
  PSvnWcAdmAccess = ^TSvnWcAdmAccess;
  TSvnWcAdmAccess = THandle;
  PSvnWcTraversalInfo = ^TSvnWcTraversalInfo;
  TSvnWcTraversalInfo = THandle;
  PSvnWcExternalItem = ^TSvnWcExternalItem;
  TSvnWcExternalItem = record
    target_dir: PAnsiChar;
    url: PAnsiChar;
    revision: TSvnOptRevision;
  end;
  PSvnWcExternalItem2 = ^TSvnWcExternalItem2;
  TSvnWcExternalItem2 = record
    target_dir: PAnsiChar;
    url: PAnsiChar;
    revision: TSvnOptRevision;
    peg_revision: TSvnOptRevision;
  end;
  TSvnWcNotifyAction = (
    svnWcNotifyAdd,
    svnWcNotifyCopy,
    svnWcNotifyDelete,
    svnWcNotifyRestore,
    svnWcNotifyRevert,
    svnWcNotifyFailedRevert,
    svnWcNotifyResolved,
    svnWcNotifySkip,
    svnWcNotifyUpdateDelete,
    svnWcNotifyUpdateAdd,
    svnWcNotifyUpdateUpdate,
    svnWcNotifyUpdateCompleted,
    svnWcNotifyUpdateExternal,
    svnWcNotifyStatusCompleted,
    svnWcNotifyStatusExternal,
    svnWcNotifyCommitModified,
    svnWcNotifyCommitAdded,
    svnWcNotifyCommitDeleted,
    svnWcNotifyCommitReplaced,
    svnWcNotifyCommitPostfixTxdelta,
    svnWcNotifyBlameRevision,
    svnWcNotifyLocked,
    svnWcNotifyUnlocked,
    svnWcNotifyFailedLock,
    svnWcNotifyFailedUnlock,
    svnWcNotifyExists,
    svnWcNotifyChangelistSet,
    svnWcNotifyChangelistClear,
    svnWcNotifyChangelistMoved,
    svnWcNotifyMergeBegin,
    svnWcNotifyForeignMergeBegin,
    svnWcNotifyUpdateReplace
  );
  PSvnWcNotifyState = ^TSvnWcNotifyState;
  TSvnWcNotifyState = (
    svnWcNotifyStateInapplicable,
    svnWcNotifyStateUnknown,
    svnWcNotifyStateUnchanged,
    svnWcNotifyStateMissing,
    svnWcNotifyStateObstructed,
    svnWcNotifyStateChanged,
    svnWcNotifyStateMerged,
    svnWcNotifyStateConflicted
  );
  TSvnWcNotifyLockState = (
    svnWcNotifyLockStateInapplicable,
    svnWcNotifyLockStateUnknown,
    svnWcNotifyLockStateUnchanged,
    svnWcNotifyLockStateLocked,
    svnWcNotifyLockStateUnlocked
  );
  PSvnWcNotify = ^TSvnWcNotify;
  TSvnWcNotify = record
    path: PAnsiChar;
    action: TSvnWcNotifyAction;
    kind: TSvnNodeKind;
    mime_type: PAnsiChar;
    lock: PSvnLock;
    err: PSvnError;
    content_state: TSvnWcNotifyState;
    prop_state: TSvnWcNotifyState;
    lock_state: TSvnWcNotifyLockState;
    revision: TSvnRevNum;
    changelist_name: PAnsiChar;
    merge_range: PSvnMergeRange;
  end;
  TSvnWcNotifyFunc2 = procedure(baton: Pointer; notify: PSvnWcNotify; pool: PAprPool); cdecl;
  TSvnWcNotifyFunc = procedure(baton: Pointer; path: PAnsiChar; action: TSvnWcNotifyAction; kind: TSvnNodeKind;
    mime_type: PAnsiChar; content_state, prop_state: TSvnWcNotifyState; revision: TSvnRevNum); cdecl;
  PSvnWcDiffCallbacks2 = ^TSvnWcDiffCallbacks2;
  TSvnWcDiffCallbacks2 = record
    file_changed: function(adm_access: PSvnWcAdmAccess; contentstate, propstate: PSvnWcNotifyState;
      path, tmpfile1, tmpfile2: PAnsiChar; rev1, rev2: TSvnRevNum; mimetype1, mimetype2: PAnsiChar;
      propchanges: PAprArrayHeader; originalprops: PAprHash; diff_baton: Pointer): PSvnError; cdecl;
    file_added: function(adm_access: PSvnWcAdmAccess; contentstate, propstate: PSvnWcNotifyState;
      path, tmpfile1, tmpfile2: PAnsiChar; rev1, rev2: TSvnRevNum; mimetype1, mimetype2: PAnsiChar;
      propchanges: PAprArrayHeader; originalprops: PAprHash; diff_baton: Pointer): PSvnError; cdecl;
    file_deleted: function(adm_access: PSvnWcAdmAccess; state: PSvnWcNotifyState;
      path, tmpfile1, tmpfile2, mimetype1, mimetype2: PAnsiChar; originalprops: PAprHash; diff_baton: Pointer): PSvnError;
      cdecl;
    dir_added: function(adm_access: PSvnWcAdmAccess; state: PSvnWcNotifyState; path: PAnsiChar; rev: TSvnRevNum;
      diff_baton: Pointer): PSvnError; cdecl;
    dir_deleted: function(adm_access: PSvnWcAdmAccess; state: PSvnWcNotifyState; path: PAnsiChar;
      diff_baton: Pointer): PSvnError; cdecl;
    dir_props_changed: function(adm_access: PSvnWcAdmAccess; state: PSvnWcNotifyState; path: PAnsiChar;
      propchanges: PAprArrayHeader; original_props: PAprHash; diff_baton: Pointer): PSvnError; cdecl;
  end;
  PSvnWcDiffCallbacks = ^TSvnWcDiffCallbacks;
  TSvnWcDiffCallbacks = record
    file_changed: function(adm_access: PSvnWcAdmAccess; state: PSvnWcNotifyState; path, tmpfile1, tmpfile2: PAnsiChar;
      rev1, rev2: TSvnRevNum; mimetype1, mimetype2: PAnsiChar; diff_baton: Pointer): PSvnError; cdecl;
    file_added: function(adm_access: PSvnWcAdmAccess; state: PSvnWcNotifyState; path, tmpfile1, tmpfile2: PAnsiChar;
      rev1, rev2: TSvnRevNum; mimetype1, mimetype2: PAnsiChar; diff_baton: Pointer): PSvnError; cdecl;
    file_deleted: function(adm_access: PSvnWcAdmAccess; state: PSvnWcNotifyState;
      path, tmpfile1, tmpfile2, mimetype1, mimetype2: PAnsiChar; diff_baton: Pointer): PSvnError; cdecl;
    dir_added: function(adm_access: PSvnWcAdmAccess; state: PSvnWcNotifyState; path: PAnsiChar; rev: TSvnRevNum;
      diff_baton: Pointer): PSvnError; cdecl;
    dir_deleted: function(adm_access: PSvnWcAdmAccess; state: PSvnWcNotifyState; path: PAnsiChar;
      diff_baton: Pointer): PSvnError; cdecl;
    props_changed: function(adm_access: PSvnWcAdmAccess; state: PSvnWcNotifyState; path: PAnsiChar;
      propchanges: PAprArrayHeader; original_props: PAprHash; diff_baton: Pointer): PSvnError; cdecl;
  end;
  TSvnWcSchedule = (
    svnWcScheduleNormal,
    svnWcScheduleAdd,
    svnWcScheduleDelete,
    svnWcScheduleReplace
  );
  PSvnWcEntry = ^TSvnWcEntry;
  TSvnWcEntry = record
    name: PAnsiChar;
    revision: TSvnRevNum;
    url: PAnsiChar;
    repos: PAnsiChar;
    uuid: PAnsiChar;
    kind: TSvnNodeKind;
    schedule: TSvnWcSchedule;
    copied: TSvnBoolean;
    deleted: TSvnBoolean;
    absent: TSvnBoolean;
    incomplete: TSvnBoolean;
    copyfrom_url: PAnsiChar;
    copyfrom_rev: TSvnRevNum;
    conflict_old: PAnsiChar;
    conflict_new: PAnsiChar;
    conflict_wrk: PAnsiChar;
    prejfile: PAnsiChar;
    text_time: TAprTime;
    prop_time: TAprTime;
    checksum: PAnsiChar;
    cmt_rev: TSvnRevNum;
    cmt_date: TAprTime;
    cmt_author: PAnsiChar;
    lock_token: PAnsiChar;
    lock_owner: PAnsiChar;
    lock_comment: PAnsiChar;
    lock_creation_date: TAprTime;
    has_props: TSvnBoolean;
    has_prop_mods: TSvnBoolean;
    cachable_props: PAnsiChar;
    present_props: PAnsiChar;
    changelist: PAnsiChar;
    working_size: TAprOff;
    keep_local: TSvnBoolean;
    depth: TSvnDepth;
  end;
  PSvnWcEntryCallbacks2 = ^TSvnWcEntryCallbacks2;
  TSvnWcEntryCallbacks2 = record
    found_entry: function(path: PAnsiChar; entry: PSvnWcEntry; walk_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
    handle_error: function(path: PAnsiChar; err: PSvnError; walk_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  end;
  PSvnWcEntryCallbacks = ^TSvnWcEntryCallbacks;
  TSvnWcEntryCallbacks = record
    found_entry: function(path: PAnsiChar; entry: PSvnWcEntry; walk_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  end;
  TSvnWcStatusKind = (
    svnWcStatusNone = 1,
    svnWcStatusUnversioned,
    svnWcStatusNormal,
    svnWcStatusAdded,
    svnWcStatusMissing,
    svnWcStatusDeleted,
    svnWcStatusReplaced,
    svnWcStatusModified,
    svnWcStatusMerged,
    svnWcStatusConflicted,
    svnWcStatusIgnored,
    svnWcStatusObstructed,
    svnWcStatusExternal,
    svnWcStatusIncomplete
  );
  PSvnWcStatus2 = ^TSvnWcStatus2;
  TSvnWcStatus2 = record
    entry: PSvnWcEntry;
    text_status: TSvnWcStatusKind;
    prop_status: TSvnWcStatusKind;
    locked: TSvnBoolean;
    copied: TSvnBoolean;
    switched: TSvnBoolean;
    repos_text_status: TSvnWcStatusKind;
    repos_prop_status: TSvnWcStatusKind;
    repos_lock: PSvnLock;
    url: PAnsiChar;
    ood_last_cmt_rev: TSvnRevNum;
    ood_last_cmt_date: TAprTime;
    ood_kind: TSvnNodeKind;
    ood_last_cmt_author: PAnsiChar;
  end;
  PSvnWcStatus = ^TSvnWcStatus;
  TSvnWcStatus = record
    entry: PSvnWcEntry;
    text_status: TSvnWcStatusKind;
    prop_status: TSvnWcStatusKind;
    locked: TSvnBoolean;
    copied: TSvnBoolean;
    switched: TSvnBoolean;
    repos_text_status: TSvnWcStatusKind;
    repos_prop_status: TSvnWcStatusKind;
  end;
  TSvnWcStatusFunc2 = procedure(baton: Pointer; path: PAnsiChar; status: PSvnWcStatus2); cdecl;
  TSvnWcStatusFunc = procedure(baton: Pointer; path: PAnsiChar; status: PSvnWcStatus); cdecl;
  TSvnWcMergeOutcome = (
    svnWcMergeUnchanged,
    svnWcMergeMerged,
    svnWcMergeConflict,
    svnWcMergeNoMerge
  );
  TSvnWcRelocationValidator3 = function(baton: Pointer; uuid, url, root_url: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  TSvnWcRelocationValidator2 = function(baton: Pointer; uuid, url: PAnsiChar; root: TSvnBoolean; pool: PAprPool): PSvnError;
    cdecl;
  TSvnWcRelocationValidator = function(baton: Pointer; uuid, url: PAnsiChar): PSvnError; cdecl;
  PSvnWcRevisionStatus = ^TSvnWcRevisionStatus;
  TSvnWcRevisionStatus = record
    min_rev: TSvnRevNum;
    max_rev: TSvnRevNum;
    switched: TSvnBoolean;
    modified: TSvnBoolean;
    sparse_checkout: TSvnBoolean;
  end;
  TSvnWcGetFile = function(baton: Pointer; path: PAnsiChar; revision: TSvnRevNum; stream: PSvnStream;
    out fetched_rev: TSvnRevNum; out props: PAprHash; pool: PAprPool): PSvnError; cdecl;
  TSvnWcConflictAction = (
    svnWcConflictActionEdit,
    svnWcConflictActionAdd,
    svnWcConflictActionDelete
  );
  TSvnWcConflictReason = (
    svnWcConflictReasonEdited,
    svnWcConflictReasonObstructed,
    svnWcConflictReasonDeleted,
    svnWcConflictReasonMissing,
    svnWcConflictReasonUnversioned
  );
  TSvnWcConflictKind = (
    svnWcConflictKindText,
    svnWcConflictKindProperty
  );
  PSvnWcConflictDescription = ^TSvnWcConflictDescription;
  TSvnWcConflictDescription = record
    path: PAnsiChar;
    node_kind: TSvnNodeKind;
    kind: TSvnWcConflictKind;
    property_name: PAnsiChar;
    is_binary: TSvnBoolean;
    mime_type: PAnsiChar;
    access: PSvnWcAdmAccess;
    action: TSvnWcConflictAction;
    reason: TSvnWcConflictReason;
    base_file: PAnsiChar;
    their_file: PAnsiChar;
    my_file: PAnsiChar;
    merged_file: PAnsiChar;
  end;
  TSvnWcConflictChoice = (
    SvnWcConflictChoosePostpone,
    SvnWcConflictChooseBase,
    SvnWcConflictChooseTheirsFull,
    SvnWcConflictChooseMineFull,
    SvnWcConflictChooseTheirsConflict,
    SvnWcConflictChooseMineConflict,
    SvnWcConflictChooseMerged
  );
  PSvnWcConflictResult = ^TSvnWcConflictResult;
  TSvnWcConflictResult = record
    choice: TSvnWcConflictChoice;
    merged_file: PAnsiChar;
  end;
  TSvnWcConflictResolverFunc = function(out result: PSvnWcConflictResult; description: PSvnWcConflictDescription;
    baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  PSvnWcCommittedQueue = ^TSvnWcCommittedQueue;
  TSvnWcCommittedQueue = THandle;
  TSvnWcCanonicalizeSvnPropGetFile = function(mime_type: PPAnsiChar; stream: PSvnStream; baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;

var
  svn_wc_version: function: PSvnVersion; cdecl;
  svn_wc_adm_open3: function(out adm_access: PSvnWcAdmAccess; associated: PSvnWcAdmAccess; path: PAnsiChar;
    write_lock: TSvnBoolean; levels_to_lock: Integer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_open2: function(out adm_access: PSvnWcAdmAccess; associated: PSvnWcAdmAccess; path: PAnsiChar;
    write_lock: TSvnBoolean; levels_to_lock: Integer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_open: function(out adm_access: PSvnWcAdmAccess; associated: PSvnWcAdmAccess; path: PAnsiChar;
    write_lock, tree_lock: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_probe_open3: function(out adm_access: PSvnWcAdmAccess; associated: PSvnWcAdmAccess; path: PAnsiChar;
    write_lock: TSvnBoolean; levels_to_lock: Integer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_probe_open2: function(out adm_access: PSvnWcAdmAccess; associated: PSvnWcAdmAccess; path: PAnsiChar;
    write_lock: TSvnBoolean; levels_to_lock: Integer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_probe_open: function(out adm_access: PSvnWcAdmAccess; associated: PSvnWcAdmAccess; path: PAnsiChar;
    write_lock, tree_lock: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_open_anchor: function(out anchor_access, target_access: PSvnWcAdmAccess; out target: PAnsiChar;
    path: PAnsiChar; write_lock: TSvnBoolean; levels_to_lock: Integer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_retrieve: function(out adm_access: PSvnWcAdmAccess; associated: PSvnWcAdmAccess; path: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_probe_retrieve: function(adm_access: PSvnWcAdmAccess; associated: PSvnWcAdmAccess; path: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_probe_try3: function(adm_access: PSvnWcAdmAccess; associated: PSvnWcAdmAccess; path: PAnsiChar;
    write_lock: TSvnBoolean; levels_to_lock: Integer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_probe_try2: function(out adm_access: PSvnWcAdmAccess; associated: PSvnWcAdmAccess; path: PAnsiChar;
    write_lock: TSvnBoolean; levels_to_lock: Integer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_probe_try: function(out adm_access: PSvnWcAdmAccess; associated: PSvnWcAdmAccess; path: PAnsiChar;
    write_lock, tree_lock: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_adm_close: function(adm_access: PSvnWcAdmAccess): PSvnError; cdecl;
  svn_wc_adm_access_path: function(adm_access: PSvnWcAdmAccess): PAnsiChar; cdecl;
  svn_wc_adm_access_pool: function(adm_access: PSvnWcAdmAccess): PAprPool; cdecl;
  svn_wc_adm_locked: function(adm_access: PSvnWcAdmAccess): TSvnBoolean; cdecl;
  svn_wc_locked: function(out locked: TSvnBoolean; path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_is_adm_dir: function(name: PAnsiChar; pool: PAprPool): TSvnBoolean; cdecl;
  svn_wc_get_adm_dir: function(pool: PAprPool): PAnsiChar; cdecl;
  svn_wc_set_adm_dir: function(name: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_init_traversal_info: function(pool: PAprPool): PSvnWcTraversalInfo; cdecl;
  svn_wc_edited_externals: procedure(out externals_old, externals_new: PAprHash; traversal_info: PSvnWcTraversalInfo);
    cdecl;
  svn_wc_traversed_depths: procedure(out depths: PAprHash; var traversal_info: TSvnWcTraversalInfo); cdecl;
  svn_wc_external_item_create: function(out item: PSvnWcExternalItem2; pool: PAprPool): PSvnError; cdecl;
  svn_wc_external_item2_dup: function(item: PSvnWcExternalItem2; pool: PAprPool): PSvnWcExternalItem2; cdecl;
  svn_wc_external_item_dup: function(item: PSvnWcExternalItem; pool: PAprPool): PSvnWcExternalItem; cdecl;
  svn_wc_parse_externals_description3: function(out externals_p: PAprArrayHeader; parent_directory, desc: PAnsiChar;
    canonicalize_url: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_parse_externals_description2: function(out externals_p: PAprArrayHeader; parent_directory, desc: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_parse_externals_description: function(out externals_p: PAprHash; parent_directory, desc: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_create_notify: function(path: PAnsiChar; action: TSvnWcNotifyAction; pool: PAprPool): PSvnWcNotify; cdecl;
  svn_wc_dup_notify: function(notify: PSvnWcNotify; pool: PAprPool): PSvnWcNotify; cdecl;
  svn_wc_create_conflict_result: function(choice: TSvnWcConflictChoice; merged_file: PAnsiChar;
    pool: PAprPool): PSvnWcConflictResult; cdecl;
  svn_wc_check_wc: function(path: PAnsiChar; out wc_format: Integer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_has_binary_prop: function(out has_binary_prop: TSvnBoolean; path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_text_modified_p: function(out modified_p: TSvnBoolean; filename: PAnsiChar; force_comparison: TSvnBoolean;
    adm_access: PSvnWcAdmAccess; pool: PAprPool): PSvnError; cdecl;
  svn_wc_props_modified_p: function(out modified_p: TSvnBoolean; path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_entry: function(out entry: PSvnWcEntry; path: PAnsiChar; adm_access: PSvnWcAdmAccess; show_hidden: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_entries_read: function(out entries: PAprHash; adm_access: PSvnWcAdmAccess; show_hidden: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_entry_dup: function(entry: PSvnWcEntry; pool: PAprPool): PSvnWcEntry; cdecl;
  svn_wc_conflicted_p: function(out text_conflicted_p, prop_conflicted_p: TSvnBoolean; dir_path: PAnsiChar;
    entry: PSvnWcEntry; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_ancestry: function(url: PPAnsiChar; rev: PSvnRevNum; path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_walk_entries3: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; walk_callbacks: PSvnWcEntryCallbacks2;
    walk_baton: Pointer; depth: TSvnDepth; show_hidden: TSvnBoolean; cancel_func: TSvnCancelFunc;
    cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_walk_entries2: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; walk_callbacks: PSvnWcEntryCallbacks;
    walk_baton: Pointer; show_hidden: TSvnBoolean; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_walk_entries: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; walk_callbacks: PSvnWcEntryCallbacks;
    walk_baton: Pointer; show_hidden: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_mark_missing_deleted: function(path: PAnsiChar; parent: PSvnWcAdmAccess; pool: PAprPool): PSvnError; cdecl;
  svn_wc_ensure_adm3: function(path, uuid, url, repos: PAnsiChar; revision: TSvnRevNum; depth: TSvnDepth;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_ensure_adm2: function(path, uuid, url, repos: PAnsiChar; revision: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_wc_ensure_adm: function(path, uuid, url: PAnsiChar; revision: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_wc_maybe_set_repos_root: function(adm_access: PSvnWcAdmAccess; path, repos: PAnsiChar; pool: PAprPool): PSvnError;
    cdecl;
  svn_wc_dup_status2: function(orig_stat: PSvnWcStatus2; pool: PAprPool): PSvnWcStatus2; cdecl;
  svn_wc_dup_status: function(orig_stat: PSvnWcStatus; pool: PAprPool): PSvnWcStatus; cdecl;
  svn_wc_status2: function(out status: PSvnWcStatus2; path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_status: function(out status: PSvnWcStatus; path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_status_editor3: function(out editor: PSvnDeltaEditor; out edit_baton, set_locks_baton: Pointer;
    out edit_revision: TSvnRevNum; anchor: PSvnWcAdmAccess; target: PAnsiChar; depth: TSvnDepth;
    get_all, no_ignore: TSvnBoolean; ignore_patterns: PAprArrayHeader; status_func: TSvnWcStatusFunc2;
    status_baton: Pointer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer; traversal_info: PSvnWcTraversalInfo;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_status_editor2: function(out editor: PSvnDeltaEditor; out edit_baton, set_locks_baton: Pointer;
    out edit_revision: TSvnRevNum; anchor: PSvnWcAdmAccess; target: PAnsiChar; config: PAprHash;
    recurse, get_all, no_ignore: TSvnBoolean; status_func: TSvnWcStatusFunc2; status_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; traversal_info: PSvnWcTraversalInfo;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_status_editor: function(out editor: PSvnDeltaEditor; out edit_baton: Pointer;
    out edit_revision: TSvnRevNum; anchor: PSvnWcAdmAccess; target: PAnsiChar; config: PAprHash;
    recurse, get_all, no_ignore: TSvnBoolean; status_func: TSvnWcStatusFunc; status_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; traversal_info: PSvnWcTraversalInfo; pool: PAprPool): PSvnError;
    cdecl;
  svn_wc_status_set_repos_locks: function(set_locks_baton: Pointer; locks: PAprHash; repos_root: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_copy2: function(src: PAnsiChar; dst_parent: PSvnWcAdmAccess; dst_basename: PAnsiChar; cancel_func: TSvnCancelFunc;
    cancel_baton: Pointer; notify_func: TSvnWcNotifyFunc2; notify_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_copy: function(src: PAnsiChar; dst_parent: PSvnWcAdmAccess; dst_basename: PAnsiChar; cancel_func: TSvnCancelFunc;
    cancel_baton: Pointer; notify_func: TSvnWcNotifyFunc; notify_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_delete3: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    notify_func: TSvnWcNotifyFunc2; notify_baton: Pointer; keep_local: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_delete2: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    notify_func: TSvnWcNotifyFunc2; notify_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_delete: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    notify_func: TSvnWcNotifyFunc; notify_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_add2: function(path: PAnsiChar; parent_access: PSvnWcAdmAccess; copyfrom_url: PAnsiChar; copyfrom_rev: TSvnRevNum;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; notify_func: TSvnWcNotifyFunc2; notify_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_add: function(path: PAnsiChar; parent_access: PSvnWcAdmAccess; copyfrom_url: PAnsiChar; copyfrom_rev: TSvnRevNum;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; notify_func: TSvnWcNotifyFunc; notify_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_add_repos_file2: function(dst_path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    new_text_base_path, new_text_path: PAnsiChar; new_base_props, new_props: PAprHash; copyfrom_url: PAnsiChar;
    copyfrom_rev: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_wc_add_repos_file: function(dst_path: PAnsiChar; adm_access: PSvnWcAdmAccess; new_text_path: PAnsiChar;
    new_props: PAprHash; copyfrom_url: PAnsiChar; copyfrom_rev: TSvnRevNum; pool: PAprPool): PSvnError; cdecl;
  svn_wc_remove_from_revision_control: function(adm_access: PSvnWcAdmAccess; name: PAnsiChar;
    destroy_wf, instant_error: TSvnBoolean; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_resolved_conflict3: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    resolve_text, resolve_props: TSvnBoolean; depth: TSvnDepth; conflict_choice: TSvnWcConflictChoice;
    notify_func: TSvnWcNotifyFunc2; notify_baton: Pointer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_resolved_conflict2: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    resolve_text, resolve_props, recurse: TSvnBoolean; notify_func: TSvnWcNotifyFunc2; notify_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_resolved_conflict: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    resolve_text, resolve_props, recurse: TSvnBoolean; notify_func: TSvnWcNotifyFunc; notify_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_committed_queue_create: function(pool: PAprPool): PSvnWcCommittedQueue; cdecl;
  svn_wc_queue_committed: function(out queue: PSvnWcCommittedQueue; path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    recurse: TSvnBoolean; wcprop_changes: PAprArrayHeader; remove_lock, remove_changelist: TSvnBoolean; digest: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_process_committed_queue: function(queue: PSvnWcCommittedQueue; adm_access: PSvnWcAdmAccess;
    new_revnum: TSvnRevNum; rev_date, rev_author: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_process_committed4: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; recurse: TSvnBoolean;
    new_revnum: TSvnRevNum; rev_date, rev_author: PAnsiChar; wcprop_changes: PAprArrayHeader;
    remove_lock, remove_changelist: TSvnBoolean; digest: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_process_committed3: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; recurse: TSvnBoolean;
    new_revnum: TSvnRevNum; rev_date, rev_author: PAnsiChar; wcprop_changes: PAprArrayHeader; remove_lock: TSvnBoolean;
    digest: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_process_committed2: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; recurse: TSvnBoolean;
    new_revnum: TSvnRevNum; rev_date, rev_author: PAnsiChar; wcprop_changes: PAprArrayHeader; remove_lock: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_process_committed: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; recurse: TSvnBoolean;
    new_revnum: TSvnRevNum; rev_date, rev_author: PAnsiChar; wcprop_changes: PAprArrayHeader; pool: PAprPool): PSvnError;
    cdecl;
  svn_wc_crawl_revisions3: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; reporter: PSvnRaReporter3;
    report_baton: Pointer; restore_files: TSvnBoolean; depth: TSvnDepth;
    depth_compatibility_trick, use_commit_times: TSvnBoolean; notify_func: TSvnWcNotifyFunc2; notify_baton: Pointer;
    traversal_info: PSvnWcTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_crawl_revisions2: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; reporter: PSvnRaReporter2;
    report_baton: Pointer; restore_files, recurse, use_commit_times: TSvnBoolean; notify_func: TSvnWcNotifyFunc2;
    notify_baton: Pointer; traversal_info: PSvnWcTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_crawl_revisions: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; reporter: PSvnRaReporter;
    report_baton: Pointer; restore_files, recurse, use_commit_times: TSvnBoolean; notify_func: TSvnWcNotifyFunc;
    notify_baton: Pointer; traversal_info: PSvnWcTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_is_wc_root: function(out wc_root: TSvnBoolean; path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_actual_target: function(path: PAnsiChar; out anchor, target: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_update_editor3: function(out target_revision: TSvnRevNum; anchor: PSvnWcAdmAccess; target: PAnsiChar;
    use_commit_times: TSvnBoolean; depth: TSvnDepth; depth_is_sticky, allow_unver_obstructions: TSvnBoolean;
    notify_func: TSvnWcNotifyFunc2; notify_baton: Pointer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    conflict_func: TSvnWcConflictResolverFunc; conflict_baton: Pointer; fetch_func: TSvnWcGetFile; fetch_baton: Pointer;
    diff3_cmd: PAnsiChar; preserved_ext: PAprArrayHeader; out editor: PSvnDeltaEditor; out edit_baton: Pointer;
    ti: PSvnWcTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_update_editor2: function(out target_revision: TSvnRevNum; anchor: PSvnWcAdmAccess; target: PAnsiChar;
    use_commit_times, recurse: TSvnBoolean; notify_func: TSvnWcNotifyFunc2; notify_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; diff3_cmd: PAnsiChar; out editor: PSvnDeltaEditor;
    out edit_baton: Pointer; ti: PSvnWcTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_update_editor: function(out target_revision: TSvnRevNum; anchor: PSvnWcAdmAccess; target: PAnsiChar;
    use_commit_times, recurse: TSvnBoolean; notify_func: TSvnWcNotifyFunc; notify_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; diff3_cmd: PAnsiChar; out editor: PSvnDeltaEditor;
    out edit_baton: Pointer; ti: PSvnWcTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_switch_editor3: function(out target_revision: TSvnRevNum; anchor: PSvnWcAdmAccess;
    target, switch_url: PAnsiChar; use_commit_times: TSvnBoolean; depth: TSvnDepth;
    depth_is_sticky, allow_unver_obstructions: TSvnBoolean; notify_func: TSvnWcNotifyFunc2; notify_baton: Pointer;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; conflict_func: TSvnWcConflictResolverFunc;
    conflict_baton: Pointer; diff3_cmd: PAnsiChar; preserved_exts: PAprArrayHeader; out editor: PSvnDeltaEditor;
    out edit_baton: Pointer; ti: PSvnWcTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_switch_editor2: function(out target_revision: TSvnRevNum; anchor: PSvnWcAdmAccess;
    target, switch_url: PAnsiChar; use_commit_times, recurse: TSvnBoolean; notify_func: TSvnWcNotifyFunc2;
    notify_baton: Pointer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer; diff3_cmd: PAnsiChar;
    out editor: PSvnDeltaEditor; out edit_baton: Pointer; ti: PSvnWcTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_switch_editor: function(out target_revision: TSvnRevNum; anchor: PSvnWcAdmAccess;
    target, switch_url: PAnsiChar; use_commit_times, recurse: TSvnBoolean; notify_func: TSvnWcNotifyFunc;
    notify_baton: Pointer; cancel_func: TSvnCancelFunc; cancel_baton: Pointer; diff3_cmd: PAnsiChar;
    out editor: PSvnDeltaEditor; out edit_baton: Pointer; ti: PSvnWcTraversalInfo; pool: PAprPool): PSvnError; cdecl;
  svn_wc_prop_list: function(out props: PAprHash; path: PAnsiChar; adm_access: PSvnWcAdmAccess; pool: PAprPool): PSvnError;
    cdecl;
  svn_wc_prop_get: function(out value: PSvnString; name, path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_prop_set2: function(name: PAnsiChar; value: PSvnString; path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    skip_checks: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_prop_set: function(name: PAnsiChar; value: PSvnString; path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_is_normal_prop: function(name: PAnsiChar): TSvnBoolean; cdecl;
  svn_wc_is_wc_prop: function(name: PAnsiChar): TSvnBoolean; cdecl;
  svn_wc_is_entry_prop: function(name: PAnsiChar): TSvnBoolean; cdecl;
  svn_wc_canonicalize_svn_prop: function(out propval_p: PSvnString; porpname: PAnsiChar; propval: PSvnString; path: PAnsiChar;
    kind: TSvnNodeKind; skip_some_checks: TSvnBoolean; prop_getter: TSvnWcCanonicalizeSvnPropGetFile;
    getter_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_diff_editor4: function(anchor: PSvnWcAdmAccess; target: PAnsiChar; callbacks: PSvnWcDiffCallbacks2;
    callback_baton: Pointer; depth: TSvnDepth; ignore_ancestry, use_text_base, reverse_order: TSvnBoolean;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; changelists: PAprArrayHeader; out editor: PSvnDeltaEditor;
    out edit_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_diff_editor3: function(anchor: PSvnWcAdmAccess; target: PAnsiChar; callbacks: PSvnWcDiffCallbacks;
    callback_baton: Pointer; recurse, ignore_ancestry, use_text_base, reverse_order: TSvnBoolean;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; out editor: PSvnDeltaEditor; out edit_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_diff_editor2: function(anchor: PSvnWcAdmAccess; target: PAnsiChar; callbacks: PSvnWcDiffCallbacks;
    callback_baton: Pointer; recurse, ignore_ancestry, use_text_base, reverse_order: TSvnBoolean;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; out editor: PSvnDeltaEditor; edit_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_diff_editor: function(anchor: PSvnWcAdmAccess; target: PAnsiChar; callbacks: PSvnWcDiffCallbacks;
    callback_baton: Pointer; recurse, use_text_base, reverse_order: TSvnBoolean; cancel_func: TSvnCancelFunc;
    cancel_baton: Pointer; out editor: PSvnDeltaEditor; out edit_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_diff4: function(anchor: PSvnWcAdmAccess; target: PAnsiChar; callbacks: PSvnWcDiffCallbacks2;
    callback_baton: Pointer; depth: TSvnDepth; ignore_ancestry: TSvnBoolean; changelists: PAprArrayHeader;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_diff3: function(anchor: PSvnWcAdmAccess; target: PAnsiChar; callbacks: PSvnWcDiffCallbacks2;
    callback_baton: Pointer; recurse, ignore_ancestry: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_diff2: function(anchor: PSvnWcAdmAccess; target: PAnsiChar; callbacks: PSvnWcDiffCallbacks;
    callback_baton: Pointer; recurse, ignore_ancestry: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_diff: function(anchor: PSvnWcAdmAccess; target: PAnsiChar; callbacks: PSvnWcDiffCallbacks; callback_baton: Pointer;
    recurse: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_prop_diffs: function(out propchanges: PAprArrayHeader; out original_props: PAprHash; path: PAnsiChar;
    adm_access: PSvnWcAdmAccess; pool: PAprPool): PSvnError; cdecl;
  svn_wc_merge3: function(out merge_outcome: TSvnWcMergeOutcome; left, right, merge_target: PAnsiChar;
    adm_access: PSvnWcAdmAccess; left_label, right_label, target_label: PAnsiChar; dry_run: TSvnBoolean; diff3_cmd: PAnsiChar;
    merge_options: PAprArrayHeader; prop_diff: PAprArrayHeader; conflict_func: TSvnWcConflictResolverFunc;
    conflict_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_merge2: function(out merge_outcome: TSvnWcMergeOutcome; left, right, merge_target: PAnsiChar;
    adm_access: PSvnWcAdmAccess; left_label, right_label, target_label: PAnsiChar; dry_run: TSvnBoolean; diff3_cmd: PAnsiChar;
    merge_options: PAprArrayHeader; pool: PAprPool): PSvnError; cdecl;
  svn_wc_merge: function(left, right, merge_target: PAnsiChar; adm_access: PSvnWcAdmAccess;
    left_label, right_label, target_label: PAnsiChar; dry_run: TSvnBoolean; out merge_outcome: TSvnWcMergeOutcome;
    diff3_cmd: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_merge_props2: function(state: PSvnWcNotifyState; path: PAnsiChar; adm_access: PSvnWcAdmAccess; baseprops: PAprHash;
    propchanges: PAprArrayHeader; base_merge, dry_run: TSvnBoolean; conflict_func: TSvnWcConflictResolverFunc;
    conflict_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_merge_props: function(state: PSvnWcNotifyState; path: PAnsiChar; adm_access: PSvnWcAdmAccess; baseprops: PAprHash;
    propchanges: PAprArrayHeader; base_merge, dry_run: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_merge_prop_diffs: function(state: PSvnWcNotifyState; path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    propchanges: PAprArrayHeader; base_merge, dry_run: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_pristine_copy_path: function(path: PAnsiChar; out pristine_path: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_cleanup2: function(path, diff3_cmd: PAnsiChar; cancel_func: TSvnCancelFunc; cancel_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_cleanup: function(path: PAnsiChar; optional_adm_access: PSvnWcAdmAccess; diff3_cmd: PAnsiChar;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_relocate3: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; path_from, path_to: PAnsiChar; recurse: TSvnBoolean;
    validator: TSvnWcRelocationValidator3; validator_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_relocate2: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; path_from, path_to: PAnsiChar; recurse: TSvnBoolean;
    validator: TSvnWcRelocationValidator2; validator_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_relocate: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; loc_from, loc_to: PAnsiChar; recurse: TSvnBoolean;
    validator: TSvnWcRelocationValidator; validator_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_revert3: function(path: PAnsiChar; parent_access: PSvnWcAdmAccess; depth: TSvnDepth; use_commit_times: TSvnBoolean;
    changelists: PAprArrayHeader; cancel_func: TSvnCancelFunc; cancel_baton: Pointer; notify_func: TSvnWcNotifyFunc2;
    notify_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_revert2: function(path: PAnsiChar; parent_access: PSvnWcAdmAccess; recursive, use_commit_times: TSvnBoolean;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; notify_func: TSvnWcNotifyFunc2; notify_baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_revert: function(path: PAnsiChar; parent_access: PSvnWcAdmAccess; recursive, use_commit_times: TSvnBoolean;
    cancel_func: TSvnCancelFunc; cancel_baton: Pointer; notify_func: TSvnWcNotifyFunc; notify_baton: Pointer;
    pool: Pointer): PSvnError; cdecl;
  svn_wc_create_tmp_file2: function(out fp: PAprFile; out new_name: PAnsiChar; path: PAnsiChar; delete_when: TSvnIOFileDel;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_create_tmp_file: function(out fp: PAprFile; path: PAnsiChar; delete_on_close: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_translated_file2: function(out xlated_path: PAnsiChar; src, versioned_file: PAnsiChar; adm_access: PSvnWcAdmAccess;
    flags: Cardinal; pool: PAprPool): PSvnError; cdecl;
  svn_wc_translated_file: function(out xlated_p: PAnsiChar; vfile: PAnsiChar; adm_access: PSvnWcAdmAccess;
    force_repair: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
  svn_wc_translated_stream: function(out stream: PSvnStream; path, versioned_file: PAnsiChar; adm_access: PSvnWcAdmAccess;
    flags: Cardinal; pool: PAprPool): PSvnError; cdecl;
  svn_wc_transmit_text_deltas2: function(out tempfile: PAnsiChar; digest: Pointer; path: PAnsiChar; adm_access: PSvnWcAdmAccess;
    fulltext: TSvnBoolean; editor: PSvnDeltaEditor; file_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_transmit_text_deltas: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; fulltext: TSvnBoolean;
    editor: PSvnDeltaEditor; file_baton: Pointer; tempfile: PPAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_transmit_prop_deltas: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; entry: PSvnWcEntry;
    editor: PSvnDeltaEditor; baton: Pointer; tempfile: PPAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_wc_get_default_ignores: function(out patterns: PAprArrayHeader; config: PAprHash; pool: PAprPool): PSvnError;
    cdecl;
  svn_wc_get_ignores: function(out patterns: PAprArrayHeader; config: PAprHash; adm_access: PSvnWcAdmAccess;
    pool: PAprPool): PSvnError; cdecl;
  svn_wc_match_ignore_list: function(str: PAnsiChar; list: PAprArrayHeader; pool: PAprPool): PSvnError; cdecl;
  svn_wc_add_lock: function(path: PAnsiChar; lock: PSvnLock; adm_access: PSvnWcAdmAccess; pool: PAprPool): PSvnError;
    cdecl;
  svn_wc_remove_lock: function(path: PAnsiChar; adm_access: PSvnWcAdmAccess; pool: PAprPool): PSvnError; cdecl;
  svn_wc_revision_status: function(out result_p: PSvnWcRevisionStatus; wc_path, trail_url: PAnsiChar;
    committed: TSvnBoolean; cancel_func: TSvnCancelFunc; cancel_baton: Pointer; pool: PAprPool): PSvnError; cdecl;
  svn_wc_set_changelist: function(path, changelist: PAnsiChar; adm_access: PSvnWcAdmAccess;
    cancel_func: TSvnWcConflictResolverFunc; cancel_baton: Pointer; notify_func: TSvnWcNotifyFunc2;
    notify_baton: Pointer; pool: PAprPool): PSvnError; cdecl;

//----- svn_wc.h -------------------------------------------------------------------------------------------------------

//----- svn_client.h ---------------------------------------------------------------------------------------------------

const
  SVN_CLIENT_COMMIT_ITEM_ADD         = $01;
  SVN_CLIENT_COMMIT_ITEM_DELETE      = $02;
  SVN_CLIENT_COMMIT_ITEM_TEXT_MODS   = $04;
  SVN_CLIENT_COMMIT_ITEM_PROP_MODS   = $08;
  SVN_CLIENT_COMMIT_ITEM_IS_COPY     = $10;
  SVN_CLIENT_COMMIT_ITEM_LOCK_TOKEN  = $20;

  SVN_CLIENT_AUTH_USERNAME = 'username';
  SVN_CLIENT_AUTH_PASSWORD = 'password';

  SVN_INFO_SIZE_UNKNOWN: TAprSize = -1;

type
  PPSvnClientPropListItem = ^PSvnClientPropListItem;
  PSvnClientPropListItem = ^TSvnClientPropListItem;
  TSvnClientPropListItem = record
    node_name: PSvnStringBuf;
    prop_hash: PAprHash;
  end;
  TSvnPropListReceiver = function(baton: Pointer; path: PAnsiChar; prop_hash: PAprHash; pool: PAprPool): PSvnError; cdecl;
  PSvnClientCommitInfo = ^TSvnClientCommitInfo;
  TSvnClientCommitInfo = record
    revision: TSvnRevNum;
    date: PAnsiChar;
    author: PAnsiChar;
  end;
  PSvnClientCommitItem3 = ^TSvnClientCommitItem3;
  TSvnClientCommitItem3 = record
    path: PAnsiChar;
    kind: TSvnNodeKind;
    url: PAnsiChar;
    revision: TSvnRevNum;
    copyfrom_url: PAnsiChar;
    copyfrom_rev: TSvnRevNum;
    state_flags: Byte;
    incoming_prop_changes: PAprArrayHeader;
    outgoing_prop_changes: PAprArrayHeader;
  end;
  PSvnClientCommitItem2 = ^TSvnClientCommitItem2;
  TSvnClientCommitItem2 = record
    path: PAnsiChar;
    kind: TSvnNodeKind;
    url: PAnsiChar;
    revision: TSvnRevNum;
    copyfrom_url: PAnsiChar;
    copyfrom_rev: TSvnRevNum;
    state_flags: Byte;
    wcprop_changes: PAprArrayHeader;
  end;
  PSvnClientCommitItem = ^TSvnClientCommitItem;
  TSvnClientCommitItem = record
    path: PAnsiChar;
    kind: TSvnNodeKind;
    url: PAnsiChar;
    revision: TSvnRevNum;
    copyfrom_url: PAnsiChar;
    state_flags: Byte;
    wcprop_changes: PAprArrayHeader;
  end;
  TSvnClientGetCommitLog3 = function(out log_msg, tmpfile: PAnsiChar; commit_items: PAprArrayHeader; baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  TSvnClientGetCommitLog2 = function(out log_msg, tmp_file: PAnsiChar; commit_items: PAprArrayHeader; baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  TSvnClientGetCommitLog = function(out log_msg, tmp_file: PAnsiChar; commit_items: PAprArrayHeader; baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
  TSvnClientBlameReceiver2 = function(baton: Pointer; line_no: Int64; revision: TSvnRevNum;
    author, date: PAnsiChar; merged_revision: TSvnRevNum; merged_author, merged_date, merged_path, line: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  TSvnClientBlameReceiver = function(baton: Pointer; line_no: Int64; revision: TSvnRevNum;
    author, date, line: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  TSvnClientDiffSummarizeKind = (
    svnDiffKindNormal,
    svnDiffKindAdded,
    svnDiffKindModified,
    svnDiffKindDeleted
  );
  PSvnClientDiffSummarize = ^TSvnClientDiffSummarize;
  TSvnClientDiffSummarize = record
    path: PAnsiChar;
    summarize_kind: TSvnClientDiffSummarizeKind;
    prop_changed: TSvnBoolean;
    node_kind: TSvnNodeKind;
  end;
  TSvnClientDiffSummarizeFunc = function(diff: PSvnClientDiffSummarize; baton: Pointer; pool: PAprPool): PSvnError;
    cdecl;
  TSvnClientListFunc = function(baton: Pointer; path: PAnsiChar; dirent: PSvnDirEnt; lock: PSvnLock; abs_path: PAnsiChar;
    pool: PAprPool): PSvnError; cdecl;
  PSvnClientCtx = ^TSvnClientCtx;
  TSvnClientCtx = record
    auth_baton: PSvnAuthBaton;
    notify_func: TSvnWcNotifyFunc;
    notify_baton: Pointer;
    log_msg_func: TSvnClientGetCommitLog;
    log_msg_baton: Pointer;
    config: PAprHash;
    cancel_func: TSvnCancelFunc;
    cancel_baton: Pointer;
    notify_func2: TSvnWcNotifyFunc2;
    notify_baton2: Pointer;
    log_msg_func2: TSvnClientGetCommitLog2;
    log_msg_baton2: Pointer;
    progress_func: TSvnRaProgressNotifyFunc;
    progress_baton: Pointer;
    log_msg_func3: TSvnClientGetCommitLog3;
    log_msg_baton3: Pointer;
    mimetypes_map: PAprHash;
    conflict_func: TSvnWcConflictResolverFunc;
    conflict_baton: Pointer;
    client_name: PAnsiChar;
  end;
  PSvnInfo = ^TSvnInfo;
  TSvnInfo = record
    URL: PAnsiChar;
    rev: TSvnRevNum;
    kind: TSvnNodeKind;
    repos_root_URL: PAnsiChar;
    repos_UUID: PAnsiChar;
    last_changed_rev: TSvnRevNum;
    last_changed_date: TAprTime;
    last_changed_author: PAnsiChar;
    lock: PSvnLock;
    has_wc_info: TSvnBoolean;
    schedule: TSvnWcSchedule;
    copyfrom_url: PAnsiChar;
    copyfrom_rev: TSvnRevNum;
    text_time: TAprTime;
    prop_time: TAprTime;
    checksum: PAnsiChar;
    conflict_old: PAnsiChar;
    conflict_new: PAnsiChar;
    conflict_wrk: PAnsiChar;
    prejfile: PAnsiChar;
    changelist: PAnsiChar;
    depth: TSvnDepth;
    working_size: TAprSize;
    size: TAprSize;
  end;
  TSvnInfoReceiver = function(baton: Pointer; path: PAnsiChar; const info: TSvnInfo; pool: PAprPool): PSvnError; cdecl;
  PSvnClientCopySource = ^TSvnClientCopySource;
  TSvnClientCopySource = record
    path: PAnsiChar;
    revision: PSvnOptRevision;
    peg_revision: PSvnOptRevision;
  end;
  TSvnChangeListReceiver = function(baton: Pointer; path, changelist: PAnsiChar; pool: PAprPool): PSvnError; cdecl;

var
  svn_client_version: function: PSvnVersion; cdecl;
  svn_client_get_simple_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthSimplePromptFunc; prompt_baton: Pointer; retry_limit: Integer; pool: PAprPool); cdecl;
  svn_client_get_username_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthUsernamePromptFunc; prompt_baton: Pointer; retry_limit: Integer; pool: PAprPool); cdecl;
  svn_client_get_simple_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool); cdecl;
  svn_client_get_windows_simple_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool); cdecl;
  svn_client_get_username_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool); cdecl;
  svn_client_get_ssl_server_trust_file_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool);
    cdecl;
  svn_client_get_ssl_client_cert_file_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool);
    cdecl;
  svn_client_get_ssl_client_cert_pw_file_provider: procedure(out provider: PSvnAuthProviderObject; pool: PAprPool);
    cdecl;
  svn_client_get_ssl_server_trust_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthSSLServerTrustPromptFunc; prompt_baton: Pointer; pool: PAprPool); cdecl;
  svn_client_get_ssl_client_cert_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthSSLClientCertPromptFunc; prompt_baton: Pointer; retry_limit: Integer; pool: PAprPool); cdecl;
  svn_client_get_ssl_client_cert_pw_prompt_provider: procedure(out provider: PSvnAuthProviderObject;
    prompt_func: TSvnAuthSSLClientCertPwPromptFunc; prompt_baton: Pointer; retry_limit: Integer; pool: PAprPool);
    cdecl;
  svn_client_proplist_item_dup: function(item: PSvnClientPropListItem; pool: PAprPool): PSvnClientPropListItem; cdecl;
  svn_client_commit_item2_dup: function(item: PSvnClientCommitItem2; pool: PAprPool): PSvnClientCommitItem2; cdecl;
  svn_client_commit_item_create: function(out item: PSvnClientCommitItem3; pool: PAprPool): PSvnError; cdecl;
  svn_client_commit_item3_dup: function(item: PSvnClientCommitItem3; pool: PAprPool): PSvnClientCommitItem3; cdecl;
  svn_client_create_context: function(out ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_checkout3: function(out result_rev: TSvnRevNum; URL, path: PAnsiChar; peg_revision, revision: PSvnOptRevision;
    depth: TSvnDepth; ignore_externals, allow_unver_obstructions: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_checkout2: function(out result_rev: TSvnRevNum; URL, path: PAnsiChar; peg_revision, revision: PSvnOptRevision;
    recurse, ignore_externals: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_checkout: function(out result_rev: TSvnRevNum; URL, path: PAnsiChar; revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_update3: function(result_revs: PPAprArrayHeader; paths: PAprArrayHeader; revision: PSvnOptRevision;
    depth: TSvnDepth; depth_is_sticky, ignore_externals, allow_unver_obstructions: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_update2: function(result_revs: PPAprArrayHeader; paths: PAprArrayHeader; revision: PSvnOptRevision;
    recurse, ignore_externals: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_update: function(out result_rev: TSvnRevNum; path: PAnsiChar; revision: PSvnOptRevision; recurse: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_switch2: function(out result_rev: TSvnRevNum; path, url: PAnsiChar; peg_revision, revision: PSvnOptRevision;
    depth: TSvnDepth; depth_is_sticky, ignore_externals, allow_unver_obstructions: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_switch: function(out result_rev: TSvnRevNum; path, url: PAnsiChar; revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_add4: function(path: PAnsiChar; depth: TSvnDepth; force, no_ignore, add_parents: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_add3: function(path: PAnsiChar; recursive, force, no_ignore: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_add2: function(path: PAnsiChar; recursive, force: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError;
    cdecl;
  svn_client_add: function(path: PAnsiChar; recursive: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_mkdir3: function(out commit_info_p: PSvnCommitInfo; paths: PAprArrayHeader; make_parents: TSvnBoolean;
    revprop_table: PAprHash; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_mkdir2: function(out commit_info_p: PSvnCommitInfo; paths: PAprArrayHeader; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_mkdir: function(out commit_info_p: PSvnCommitInfo; paths: PAprArrayHeader; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_delete3: function(out commit_info_p: PSvnCommitInfo; paths: PAprArrayHeader; force, keep_local: TSvnBoolean;
    revprop_table: PAprHash; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_delete2: function(out commit_info_p: PSvnCommitInfo; paths: PAprArrayHeader; force: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_delete: function(out commit_info_p: PSvnCommitInfo; paths: PAprArrayHeader; force: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_import3: function(out commit_info_p: PSvnCommitInfo; path, url: PAnsiChar; depth: TSvnDepth;
    no_ignore, ignore_unknown_node_types: TSvnBoolean; revprop_table: PAprHash; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_import2: function(out commit_info_p: PSvnCommitInfo; path, url: PAnsiChar;
    nonrecursive, no_ignore: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_import: function(out commit_info_p: PSvnCommitInfo; path, url: PAnsiChar; nonrecursive: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_commit4: function(out commit_info_p: PSvnCommitInfo; targets: PAprArrayHeader; depth: TSvnDepth;
    keep_locks, keep_changelists: TSvnBoolean; changelists: PAprArrayHeader; revprop_table: PAprHash;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_commit3: function(out commit_info_p: PSvnCommitInfo; targets: PAprArrayHeader;
    recurse, keep_locks: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_commit2: function(out commit_info_p: PSvnCommitInfo; targets: PAprArrayHeader;
    recurse, keep_locks: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_commit: function(out commit_info_p: PSvnCommitInfo; targets: PAprArrayHeader;
    nonrecursive: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_status3: function(result_rev: PSvnRevNum; path: PAnsiChar; revision: PSvnOptRevision;
    status_func: TSvnWcStatusFunc2; status_baton: Pointer; depth: TSvnDepth;
    get_all, update, no_ignore, ignore_externals: TSvnBoolean; changelists: PAprArrayHeader; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_status2: function(result_rev: PSvnRevNum; path: PAnsiChar; revision: PSvnOptRevision;
    status_func: TSvnWcStatusFunc2; status_baton: Pointer;
    recurse, get_all, update, no_ignore, ignore_externals: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_status: function(out result_rev: TSvnRevNum; path: PAnsiChar; revision: PSvnOptRevision;
    status_func: TSvnWcStatusFunc; status_baton: Pointer; recurse, get_all, update, no_ignore: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_log4: function(targets: PAprArrayHeader; peg_revision, rev_start, rev_end: PSvnOptRevision; limit: Integer;
    discover_changed_paths, strict_node_history, include_merged_revisions: TSvnBoolean; revprops: PAprArrayHeader;
    receiver: TSvnLogEntryReceiver; receiver_baton: Pointer; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_log3: function(targets: PAprArrayHeader; peg_revision, rev_start, rev_end: PSvnOptRevision; limit: Integer;
    discover_changed_paths, strict_node_history: TSvnBoolean; receiver: TSvnLogMessageReceiver; receiver_baton: Pointer;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_log2: function(targets: PAprArrayHeader; rev_start, rev_end: PSvnOptRevision; limit: Integer;
    discover_changed_paths, strict_node_history: TSvnBoolean; receiver: TSvnLogMessageReceiver; receiver_baton: Pointer;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_log: function(targets: PAprArrayHeader; rev_start, rev_end: PSvnOptRevision;
    discover_changed_paths, strict_node_history: TSvnBoolean; receiver: TSvnLogMessageReceiver; receiver_baton: Pointer;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_blame4: function(path_or_url: PAnsiChar; peg_revision, rev_start, rev_end: PSvnOptRevision;
    diff_options: PSvnDiffFileOptions; ignore_mime_type, include_merged_revisions: TSvnBoolean;
    receiver: TSvnClientBlameReceiver2; receiver_baton: Pointer; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_blame3: function(path_or_url: PAnsiChar; peg_revision, rev_start, rev_end: PSvnOptRevision;
    diff_options: PSvnDiffFileOptions; ignore_mime_type: TSvnBoolean; receiver: TSvnClientBlameReceiver;
    receiver_baton: Pointer; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_blame2: function(path_or_url: PAnsiChar; peg_revision, rev_start, rev_end: PSvnOptRevision;
    receiver: TSvnClientBlameReceiver; receiver_baton: Pointer; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_blame: function(path_or_url: PAnsiChar; rev_start, rev_end: PSvnOptRevision; receiver: TSvnClientBlameReceiver;
    receiver_baton: Pointer; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff4: function(diff_options: PAprArrayHeader; path1: PAnsiChar; revision1: PSvnOptRevision; path2: PAnsiChar;
    revision2: PSvnOptRevision; relative_to_dir: PAnsiChar; depth: TSvnDepth;
    ignore_ancestry, no_diff_deleted, ignore_content_type: TSvnBoolean; header_encoding: PAnsiChar;
    outfile, errfile: PAprFile; changelists: PAprArrayHeader; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff3: function(diff_options: PAprArrayHeader; path1: PAnsiChar; revision1: PSvnOptRevision; path2: PAnsiChar;
    revision2: PSvnOptRevision; recurse, ignore_ancestry, no_diff_deleted, ignore_content_type: TSvnBoolean;
    header_encoding: PAnsiChar; outfile, errfile: PAprFile; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff2: function(diff_options: PAprArrayHeader; path1: PAnsiChar; revision1: PSvnOptRevision; path2: PAnsiChar;
    revision2: PSvnOptRevision; recurse, ignore_ancestry, no_diff_deleted, ignore_content_type: TSvnBoolean;
    outfile, errfile: PAprFile; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff: function(diff_options: PAprArrayHeader; path1: PAnsiChar; revision1: PSvnOptRevision; path2: PAnsiChar;
    revision2: PSvnOptRevision; recurse, ignore_ancestry, no_diff_deleted: TSvnBoolean; outfile, errfile: PAprFile;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff_peg4: function(diff_options: PAprArrayHeader; path: PAnsiChar;
    peg_revision, start_revision, end_revision: PSvnOptRevision; relative_to_dir: PAnsiChar; depth: TSvnDepth;
    ignore_ancestry, no_diff_deleted, ignore_content_type: TSvnBoolean; header_encoding: PAnsiChar;
    outfile, errfile: PAprFile; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff_peg3: function(diff_options: PAprArrayHeader; path: PAnsiChar;
    peg_revision, start_revision, end_revision: PSvnOptRevision;
    recurse, ignore_ancestry, no_diff_deleted, ignore_content_type: TSvnBoolean; header_encoding: PAnsiChar;
    outfile, errfile: PAprFile; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff_peg2: function(diff_options: PAprArrayHeader; path: PAnsiChar;
    peg_revision, start_revision, end_revision: PSvnOptRevision;
    recurse, ignore_ancestry, no_diff_deleted, ignore_content_type: TSvnBoolean; outfile, errfile: PAprFile;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff_peg: function(diff_options: PAprArrayHeader; path: PAnsiChar;
    peg_revision, start_revision, end_revision: PSvnOptRevision; recurse, ignore_ancestry, no_diff_deleted: TSvnBoolean;
    outfile, errfile: PAprFile; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff_summarize_dup: function(diff: PSvnClientDiffSummarize; pool: PAprPool): PSvnClientDiffSummarize;
    cdecl;
  svn_client_diff_summarize2: function(path1: PAnsiChar; revision1: PSvnOptRevision; path2: PAnsiChar;
    revision2: PSvnOptRevision; depth: TSvnDepth; ignore_ancestry: TSvnBoolean; changelists: PAprArrayHeader;
    summarize_func: TSvnClientDiffSummarizeFunc; summarize_baton: Pointer; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_diff_summarize: function(path1: PAnsiChar; revision1: PSvnOptRevision; path2: PAnsiChar;
    revision2: PSvnOptRevision; recurse, ignore_ancestry: TSvnBoolean; summarize_func: TSvnClientDiffSummarizeFunc;
    summarize_baton: Pointer; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_diff_summarize_peg2: function(path: PAnsiChar; peg_revision, start_revision, end_revision: PSvnOptRevision;
    depth: TSvnDepth; ignore_ancestry: TSvnBoolean; changelists: PAprArrayHeader;
    summarize_func: TSvnClientDiffSummarizeFunc; summarize_baton: Pointer; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_diff_summarize_peg: function(path: PAnsiChar; peg_revision, start_revision, end_revision: PSvnOptRevision;
    recurse, ignore_ancestry: TSvnBoolean; summarize_func: TSvnClientDiffSummarizeFunc; summarize_baton: Pointer;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_merge3: function(source1: PAnsiChar; revision1: PSvnOptRevision; source2: PAnsiChar; revision2: PSvnOptRevision;
    target_wcpath: PAnsiChar; depth: TSvnDepth; ignore_ancestry, force, record_only, dry_run: TSvnBoolean;
    merge_options: PAprArrayHeader; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_merge2: function(source1: PAnsiChar; revision1: PSvnOptRevision; source2: PAnsiChar; revision2: PSvnOptRevision;
    target_wcpath: PAnsiChar; recurse, ignore_ancestry, force, dry_run: TSvnBoolean; merge_options: PAprArrayHeader;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_merge: function(source1: PAnsiChar; revision1: PSvnOptRevision; source2: PAnsiChar; revision2: PSvnOptRevision;
    target_wcpath: PAnsiChar; recurse, ignore_ancestry, force, dry_run: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_merge_reintegrate: function(source: PAnsiChar; peg_revision: PSvnOptRevision; target_wcpath: PAnsiChar;
    dry_run: TSvnBoolean; merge_options: PAprArrayHeader; ctx: PSvnClientCtx; pool: PAprpool): PSvnError; cdecl;
  svn_client_merge_peg3: function(source: PAnsiChar; peg_revision: PSvnOptRevision; target_wcpath: PAnsiChar;
    depth: TSvnDepth; ignore_ancestry, force, record_only, dry_run: TSvnBoolean; merge_options: PAprArrayHeader;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_merge_peg2: function(source: PAnsiChar; revision1, revision2, peg_revision: PSvnOptRevision;
    target_wcpath: PAnsiChar; recurse, ignore_ancestry, force, dry_run: TSvnBoolean; merge_options: PAprArrayHeader;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_merge_peg: function(source: PAnsiChar; revision1, revision2, peg_revision: PSvnOptRevision;
    target_wcpath: PAnsiChar; recurse, ignore_ancestry, force, dry_run: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_suggest_merge_sources: function(out suggestions: PAprArrayHeader; path_or_url: PAnsiChar;
    peg_revision: PSvnOptRevision; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_mergeinfo_get_merged: function(out mergeinfo: PAprHash; path_or_url: PAnsiChar; peg_revision: PSvnOptRevision;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_mergeinfo_log_merged: function(path_or_url: PAnsiChar; peg_revision: PSvnOptRevision; merge_source_url: PAnsiChar;
    src_peg_revision: PSvnOptRevision; receiver: TSvnLogEntryReceiver; receiver_baton: Pointer;
    discover_changed_paths: TSvnBoolean; revprops: PAprArrayHeader; ctx: PSvnClientCtx; pool: PAprPool): PSvnError;
    cdecl;
  svn_client_mergeinfo_log_eligible: function(path_or_url: PAnsiChar; peg_revision: PSvnOptRevision;
    merge_source_url: PAnsiChar; src_peg_revision: PSvnOptRevision; receiver: TSvnLogEntryReceiver; receiver_baton: Pointer;
    discover_changed_paths: TSvnBoolean; revprops: PAprArrayHeader; ctx: PSvnClientCtx; pool: PAprPool): PSvnError;
    cdecl;
  svn_client_cleanup: function(dir: PAnsiChar; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_relocate: function(dir, url_from, url_to: PAnsiChar; recurse: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_revert2: function(paths: PAprArrayHeader; depth: TSvnDepth; changelists: PAprArrayHeader;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_revert: function(paths: PAprArrayHeader; recursive: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_resolved: function(path: PAnsiChar; recursive: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError;
    cdecl;
  svn_client_resolve: function(path: PAnsiChar; depth: TSvnDepth; conflict_choice: TSvnWcConflictChoice; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_copy4: function(out commit_info_p: PSvnCommitInfo; sources: PAprArrayHeader; dst_path: PAnsiChar;
    copy_as_child, make_parents: TSvnBoolean; revprop_table: PAprHash; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_copy3: function(out commit_info_p: PSvnCommitInfo; src_path: PAnsiChar; src_revision: PSvnOptRevision;
    dst_path: PAnsiChar; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_copy2: function(out commit_info_p: PSvnCommitInfo; src_path: PAnsiChar; src_revision: PSvnOptRevision;
    dst_path: PAnsiChar; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_copy: function(out commit_info_p: PSvnCommitInfo; src_path: PAnsiChar; src_revision: PSvnOptRevision;
    dst_path: PAnsiChar; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_move5: function(out commit_info_p: PSvnCommitInfo; src_paths: PAprArrayHeader; dst_path: PAnsiChar;
    force, move_as_child, make_parents: TSvnBoolean; revprop_table: PAprHash; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_move4: function(out commit_info_p: PSvnCommitInfo; src_path, dst_path: PAnsiChar; force: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_move3: function(out commit_info_p: PSvnCommitInfo; src_path, dst_path: PAnsiChar; force: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_move2: function(out commit_info_p: PSvnClientCommitInfo; src_path, dst_path: PAnsiChar; force: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_move: function(out commit_info_p: PSvnClientCommitInfo; src_path: PAnsiChar; src_revision: PSvnOptRevision;
    dst_path: PAnsiChar; force: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_propset3: function(out commit_info_p: PSvnCommitInfo; propname: PAnsiChar; propval: PSvnString; target: PAnsiChar;
    depth: TSvnDepth; skip_checks: TSvnBoolean; base_revision_for_url: TSvnRevNum; changelists: PAprArrayHeader;
    revprop_table: PAprHash; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_propset2: function(propname: PAnsiChar; propval: PSvnString; target: PAnsiChar; recurse, skip_checks: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_propset: function(propname: PAnsiChar; propval: PSvnString; target: PAnsiChar; recurse: TSvnBoolean;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_revprop_set: function(propname: PAnsiChar; propval: PSvnString; URL: PAnsiChar; revision: PSvnOptRevision;
    out set_rev: TSvnRevNum; force: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPOol): PSvnError; cdecl;
  svn_client_propget3: function(out props: PAprHash; propname, target: PAnsiChar; peg_revision, revision: PSvnOptRevision;
    actual_revnum: PSvnRevNum; depth: TSvnDepth; changelists: PAprArrayHeader; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_propget2: function(out props: PAprHash; propname, target: PAnsiChar; peg_revision, revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_propget: function(out props: PAprHash; propname, target: PAnsiChar; revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_revprop_get: function(propname: PAnsiChar; out propval: PSvnString; URL: PAnsiChar; revision: PSvnOptRevision;
    out set_rev: TSvnRevNum; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_proplist3: function(target: PAnsiChar; peg_revision, revision: PSvnOptRevision; depth: TSvnDepth;
    changelists: PAprArrayHeader; receiver: TSvnPropListReceiver; receiver_baton: Pointer; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_proplist2: function(out props: PAprArrayHeader; target: PAnsiChar; peg_revision, revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_proplist: function(out props: PAprArrayHeader; target: PAnsiChar; revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_revprop_list: function(out props: PAprHash; URL: PAnsiChar; revision: PSvnOptRevision; out set_rev: TSvnRevNum;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_export4: function(out result_rev: TSvnRevNum; dir_from, dir_to: PAnsiChar;
    peg_revision, revision: PSvnOptRevision; overwrite, ignore_externals: TSvnBoolean; depth: TSvnDepth;
    native_eol: PAnsiChar; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_export3: function(out result_rev: TSvnRevNum; dir_from, dir_to: PAnsiChar;
    peg_revision, revision: PSvnOptRevision; overwrite, ignore_externals, recurse: TSvnBoolean; native_eol: PAnsiChar;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_export2: function(out result_rev: TSvnRevNum; dir_from, dir_to: PAnsiChar; revision: PSvnOptRevision;
    force: TSvnBoolean; native_eol: PAnsiChar; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_export: function(out result_rev: TSvnRevNum; dir_from, dir_to: PAnsiChar; revision: PSvnOptRevision;
    force: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_list2: function(path_or_url: PAnsiChar; peg_revision, revision: PSvnOptRevision; depth: TSvnDepth;
    dirent_fields: Cardinal; fetch_locks: TSvnBoolean; list_func: TSvnClientListFunc; baton: Pointer;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_list: function(path_or_url: PAnsiChar; peg_revision, revision: PSvnOptRevision; recurse: TSvnBoolean;
    dirent_fields: Cardinal; fetch_locks: TSvnBoolean; list_func: TSvnClientListFunc; baton: Pointer;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_ls3: function(out dirents, locks: PAprHash; path_or_url: PAnsiChar; peg_revision, revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_ls2: function(out dirents: PAprHash; path_or_url: PAnsiChar; peg_revision, revision: PSvnOptRevision;
    recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_ls: function(out dirents: PAprHash; path_or_url: PAnsiChar; revision: PSvnOptRevision; recurse: TSvnBoolean;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_cat2: function(strm_out: PSvnStream; path_or_url: PAnsiChar; peg_revision, revision: PSvnOptRevision;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_cat: function(strm_out: PSvnStream; path_or_url: PAnsiChar; revision: PSvnOptRevision; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_add_to_changelist: function(paths: PAprArrayHeader; changelist: PAnsiChar; depth: TSvnDepth;
    changelists: PAprArrayHeader; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_remove_from_changelists: function(paths: PAprArrayHeader; depth: TSvnDepth; changelists: PAprArrayHeader;
    ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_get_changelists: function(path: PAnsiChar; changelists: PAprArrayHeader; depth: TSvnDepth;
    callback_func: TSvnChangeListReceiver; callback_baton: Pointer; ctx: PSvnClientCtx; pool: PAprPool): PSvnError;
    cdecl;
  svn_client_lock: function(targets: PAprArrayHeader; comment: PAnsiChar; steal_lock: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_unlock: function(targets: PAprArrayHeader; break_lock: TSvnBoolean; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_info_dup: function(info: PSvnInfo; pool: PAprPool): PSvnInfo; cdecl;
  svn_client_info2: function(path_or_url: PAnsiChar; peg_revision, revision: PSvnOptRevision; receiver: TSvnInfoReceiver;
    receiver_baton: Pointer; depth: TSvnDepth; changelists: PAprArrayHeader; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_info: function(path_or_url: PAnsiChar; peg_revision, revision: PSvnOptRevision; receiver: TSvnInfoReceiver;
    receiver_baton: Pointer; recurse: TSvnBoolean; ctx: PSvnClientCtx; pool: PAprPool): PSvnError; cdecl;
  svn_client_url_from_path: function(out url: PAnsiChar; path_or_url: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
  svn_client_root_url_from_path: function(out url: PAnsiChar; path_or_url: PAnsiChar; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_uuid_from_url: function(out uuid: PAnsiChar; url: PAnsiChar; ctx: PSvnClientCtx; pool: PAprPool): PSvnError;
    cdecl;
  svn_client_uuid_from_path: function(out uuid: PAnsiChar; path: PAnsiChar; adm_access: PSvnWcAdmAccess; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;
  svn_client_open_ra_session: function(out session: PSvnRaSession; url: PAnsiChar; ctx: PSvnClientCtx;
    pool: PAprPool): PSvnError; cdecl;

//----- svn_client.h ---------------------------------------------------------------------------------------------------

type
  TErrorData = record
    Code: Integer; // error code
    SPos: Integer; // substring start pos within Message
    SLen: Integer; // substring length
  end;
  ESvnError = class(EAprError)
  private
    FErrors: array of TErrorData;

    function GetCount: Integer;
    function GetErrorCodes(Index: Integer): Integer;
    function GetMessages(Index: Integer): string;
  public
    constructor Create(Error: PSvnError);
    destructor Destroy; override;

    property Count: Integer read GetCount;
    property ErrorCodes[Index: Integer]: Integer read GetErrorCodes;
    property Messages[Index: Integer]: string read GetMessages;
  end;

function SvnClientLibLoaded: Boolean;
function SvnDeltaLibLoaded: Boolean;
function SvnDiffLibLoaded: Boolean;
function SvnFsLibLoaded: Boolean;
function SvnRaLibLoaded: Boolean;
function SvnReposLibLoaded: Boolean;
function SvnSubrLibLoaded: Boolean;
function SvnWcLibLoaded: Boolean;

function LoadSvnClientLib(const FileName: string = ''): Boolean;
function LoadSvnDeltaLib(const FileName: string = ''): Boolean;
function LoadSvnDiffLib(const FileName: string = ''): Boolean;
function LoadSvnFsLib(const FileName: string = ''): Boolean;
function LoadSvnRaLib(const FileName: string = ''): Boolean;
function LoadSvnReposLib(const FileName: string = ''): Boolean;
function LoadSvnSubrLib(const FileName: string = ''): Boolean;
function LoadSvnWcLib(const FileName: string = ''): Boolean;

procedure FreeSvnClientLib;
procedure FreeSvnDeltaLib;
procedure FreeSvnDiffLib;
procedure FreeSvnFsLib;
procedure FreeSvnRaLib;
procedure FreeSvnReposLib;
procedure FreeSvnSubrLib;
procedure FreeSvnWcLib;

function GetSvnErrorMessage(Status: TAprStatus): string;
procedure SvnCheck(Error: PSvnError);
procedure RaiseSvnError(Error: PSvnError);

implementation

// svn_ctype.h

const
  SVN_CTYPE_CNTRL    = $0001; // Control character
  SVN_CTYPE_SPACE    = $0002; // Whitespace
  SVN_CTYPE_DIGIT    = $0004; // Decimal digit
  SVN_CTYPE_UPPER    = $0008; // Uppercase letter
  SVN_CTYPE_LOWER    = $0010; // Lowercase letter
  SVN_CTYPE_PUNCT    = $0020; // Punctuation mark
  SVN_CTYPE_XALPHA   = $0040; // Hexadecimal digits A to F
  SVN_CTYPE_ASCII    = $0080; // ASCII subset
  SVN_CTYPE_UTF8LEAD = $0100; // UTF-8 multibyte lead byte
  SVN_CTYPE_UTF8CONT = $0200; // UTF-8 multibyte non-lead byte

  SVN_CTYPE_UTF8MBC  = SVN_CTYPE_UTF8LEAD or SVN_CTYPE_UTF8CONT;
  SVN_CTYPE_UTF8     = SVN_CTYPE_ASCII or SVN_CTYPE_UTF8MBC;

  SVN_CTYPE_ALPHA    = SVN_CTYPE_LOWER or SVN_CTYPE_UPPER;
  SVN_CTYPE_ALNUM    = SVN_CTYPE_ALPHA or SVN_CTYPE_DIGIT;
  SVN_CTYPE_XDIGIT   = SVN_CTYPE_DIGIT or SVN_CTYPE_XALPHA;
  SVN_CTYPE_GRAPH    = SVN_CTYPE_PUNCT or SVN_CTYPE_ALNUM;
  SVN_CTYPE_PRINT    = SVN_CTYPE_GRAPH or SVN_CTYPE_SPACE;

function CTypeTable(X: Byte): PCardinal;
begin
  Result := svn_ctype_table;
  Inc(Result, X);
end;

function svn_ctype_iscntrl(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_CNTRL <> 0;
end;

function svn_ctype_isspace(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_SPACE <> 0;
end;

function svn_ctype_isdigit(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_DIGIT <> 0;
end;

function svn_ctype_isupper(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_UPPER <> 0;
end;

function svn_ctype_islower(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_LOWER <> 0;
end;

function svn_ctype_ispunct(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_PUNCT <> 0;
end;

function svn_ctype_isascii(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_ASCII <> 0;
end;

function svn_ctype_isalpha(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_ALPHA <> 0;
end;

function svn_ctype_isalnum(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_ALNUM <> 0;
end;

function svn_ctype_isxdigit(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_XDIGIT <> 0;
end;

function svn_ctype_isgraph(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_GRAPH <> 0;
end;

function svn_ctype_isprint(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_PRINT <> 0;
end;

function svn_ctype_isutf8lead(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_UTF8LEAD <> 0;
end;

function svn_ctype_isutf8cont(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_UTF8CONT <> 0;
end;

function svn_ctype_isutf8mbc(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_UTF8MBC <> 0;
end;

function svn_ctype_isutf8(C: Char): Boolean;
begin
  Result := CTypeTable(Ord(C))^ and SVN_CTYPE_UTF8 <> 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnIsLockError(err: TSvnError): Boolean;

begin
  Result := (err.apr_err = SVN_ERR_FS_PATH_ALREADY_LOCKED) or (err.apr_err = SVN_ERR_FS_OUT_OF_DATE);
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnIsUnlockError(err: TSvnError): Boolean;

begin
  Result := (err.apr_err = SVN_ERR_FS_PATH_NOT_LOCKED) or
    (err.apr_err = SVN_ERR_FS_BAD_LOCK_TOKEN) or
    (err.apr_err = SVN_ERR_FS_LOCK_OWNER_MISMATCH) or
    (err.apr_err = SVN_ERR_FS_NO_SUCH_LOCK) or
    (err.apr_err = SVN_ERR_RA_NOT_LOCKED) or
    (err.apr_err = SVN_ERR_FS_LOCK_EXPIRED);
end;

//----------------------------------------------------------------------------------------------------------------------

var
  SvnClientLib: THandle = INVALID_HANDLE_VALUE;
  SvnDeltaLib: THandle = INVALID_HANDLE_VALUE;
  SvnDiffLib: THandle = INVALID_HANDLE_VALUE;
  SvnFsLib: THandle = INVALID_HANDLE_VALUE;
  SvnRaLib: THandle = INVALID_HANDLE_VALUE;
  SvnReposLib: THandle = INVALID_HANDLE_VALUE;
  SvnSubrLib: THandle = INVALID_HANDLE_VALUE;
  SvnWcLib: THandle = INVALID_HANDLE_VALUE;

//----------------------------------------------------------------------------------------------------------------------

function GetProcAddress(hModule: HMODULE; lpProcName: LPCSTR): FARPROC;

begin
  Result := Windows.GetProcAddress(hModule, lpProcName);
  if not Assigned(Result) then
    OutputDebugString(PChar(Format('%s = nil!', [lpProcName])));
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnClientLibLoaded: Boolean;

begin
  Result := SvnClientLib <> INVALID_HANDLE_VALUE;
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnDeltaLibLoaded: Boolean;

begin
  Result := SvnDeltaLib <> INVALID_HANDLE_VALUE;
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnDiffLibLoaded: Boolean;

begin
  Result := SvnDiffLib <> INVALID_HANDLE_VALUE;
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnFsLibLoaded: Boolean;

begin
  Result := SvnFsLib <> INVALID_HANDLE_VALUE;
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnRaLibLoaded: Boolean;

begin
  Result := SvnRaLib <> INVALID_HANDLE_VALUE;
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnReposLibLoaded: Boolean;

begin
  Result := SvnReposLib <> INVALID_HANDLE_VALUE;
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnSubrLibLoaded: Boolean;

begin
  Result := SvnSubrLib <> INVALID_HANDLE_VALUE;
end;

//----------------------------------------------------------------------------------------------------------------------

function SvnWcLibLoaded: Boolean;

begin
  Result := SvnWcLib <> INVALID_HANDLE_VALUE;
end;

//----------------------------------------------------------------------------------------------------------------------

function LoadSvnClientLib(const FileName: string = ''): Boolean;

var
  LibFileName: string;

begin
  Result := not SvnClientLibLoaded;

  if Result then
  begin
    if FileName = '' then
      LibFileName := 'libsvn_client-1.dll'
    else
      LibFileName := FileName;

    SvnClientLib := LoadLibrary(PChar(LibFileName));
    Result := SvnClientLib <> 0;
    if not Result then
      SvnClientLib := INVALID_HANDLE_VALUE
    else
    begin
      @svn_client_add := GetProcAddress(SvnClientLib, 'svn_client_add');
      @svn_client_add2 := GetProcAddress(SvnClientLib, 'svn_client_add2');
      @svn_client_add3 := GetProcAddress(SvnClientLib, 'svn_client_add3');
      @svn_client_add4 := GetProcAddress(SvnClientLib, 'svn_client_add4');
      @svn_client_add_to_changelist := GetProcAddress(SvnClientLib, 'svn_client_add_to_changelist');
      @svn_client_blame := GetProcAddress(SvnClientLib, 'svn_client_blame');
      @svn_client_blame2 := GetProcAddress(SvnClientLib, 'svn_client_blame2');
      @svn_client_blame3 := GetProcAddress(SvnClientLib, 'svn_client_blame3');
      @svn_client_blame4 := GetProcAddress(SvnClientLib, 'svn_client_blame4');
      @svn_client_cat := GetProcAddress(SvnClientLib, 'svn_client_cat');
      @svn_client_cat2 := GetProcAddress(SvnClientLib, 'svn_client_cat2');
      @svn_client_checkout := GetProcAddress(SvnClientLib, 'svn_client_checkout');
      @svn_client_checkout2 := GetProcAddress(SvnClientLib, 'svn_client_checkout2');
      @svn_client_checkout3 := GetProcAddress(SvnClientLib, 'svn_client_checkout3');
      @svn_client_cleanup := GetProcAddress(SvnClientLib, 'svn_client_cleanup');
      @svn_client_commit := GetProcAddress(SvnClientLib, 'svn_client_commit');
      @svn_client_commit2 := GetProcAddress(SvnClientLib, 'svn_client_commit2');
      @svn_client_commit3 := GetProcAddress(SvnClientLib, 'svn_client_commit3');
      @svn_client_commit4 := GetProcAddress(SvnClientLib, 'svn_client_commit4');
      @svn_client_commit_item2_dup := GetProcAddress(SvnClientLib, 'svn_client_commit_item2_dup');
      @svn_client_commit_item3_dup := GetProcAddress(SvnClientLib, 'svn_client_commit_item3_dup');
      @svn_client_commit_item_create := GetProcAddress(SvnClientLib, 'svn_client_commit_item_create');
      @svn_client_copy := GetProcAddress(SvnClientLib, 'svn_client_copy');
      @svn_client_copy2 := GetProcAddress(SvnClientLib, 'svn_client_copy2');
      @svn_client_copy3 := GetProcAddress(SvnClientLib, 'svn_client_copy3');
      @svn_client_copy4 := GetProcAddress(SvnClientLib, 'svn_client_copy4');
      @svn_client_create_context := GetProcAddress(SvnClientLib, 'svn_client_create_context');
      @svn_client_delete := GetProcAddress(SvnClientLib, 'svn_client_delete');
      @svn_client_delete2 := GetProcAddress(SvnClientLib, 'svn_client_delete2');
      @svn_client_delete3 := GetProcAddress(SvnClientLib, 'svn_client_delete3');
      @svn_client_diff := GetProcAddress(SvnClientLib, 'svn_client_diff');
      @svn_client_diff2 := GetProcAddress(SvnClientLib, 'svn_client_diff2');
      @svn_client_diff3 := GetProcAddress(SvnClientLib, 'svn_client_diff3');
      @svn_client_diff4 := GetProcAddress(SvnClientLib, 'svn_client_diff4');
      @svn_client_diff_peg := GetProcAddress(SvnClientLib, 'svn_client_diff_peg');
      @svn_client_diff_peg2 := GetProcAddress(SvnClientLib, 'svn_client_diff_peg2');
      @svn_client_diff_peg3 := GetProcAddress(SvnClientLib, 'svn_client_diff_peg3');
      @svn_client_diff_peg4 := GetProcAddress(SvnClientLib, 'svn_client_diff_peg4');
      @svn_client_diff_summarize := GetProcAddress(SvnClientLib, 'svn_client_diff_summarize');
      @svn_client_diff_summarize2 := GetProcAddress(SvnClientLib, 'svn_client_diff_summarize2');
      @svn_client_diff_summarize_dup := GetProcAddress(SvnClientLib, 'svn_client_diff_summarize_dup');
      @svn_client_diff_summarize_peg := GetProcAddress(SvnClientLib, 'svn_client_diff_summarize_peg');
      @svn_client_diff_summarize_peg2 := GetProcAddress(SvnClientLib, 'svn_client_diff_summarize_peg2');
      @svn_client_export := GetProcAddress(SvnClientLib, 'svn_client_export');
      @svn_client_export2 := GetProcAddress(SvnClientLib, 'svn_client_export2');
      @svn_client_export3 := GetProcAddress(SvnClientLib, 'svn_client_export3');
      @svn_client_export4 := GetProcAddress(SvnClientLib, 'svn_client_export4');
      @svn_client_get_changelists := GetProcAddress(SvnClientLib, 'svn_client_get_changelists');
      @svn_client_get_simple_prompt_provider := GetProcAddress(SvnClientLib, 'svn_client_get_simple_prompt_provider');
      @svn_client_get_simple_provider := GetProcAddress(SvnClientLib, 'svn_client_get_simple_provider');
      @svn_client_get_ssl_client_cert_file_provider := GetProcAddress(SvnClientLib, 'svn_client_get_ssl_client_cert_file_provider');
      @svn_client_get_ssl_client_cert_prompt_provider := GetProcAddress(SvnClientLib, 'svn_client_get_ssl_client_cert_prompt_provider');
      @svn_client_get_ssl_client_cert_pw_file_provider := GetProcAddress(SvnClientLib, 'svn_client_get_ssl_client_cert_pw_file_provider');
      @svn_client_get_ssl_client_cert_pw_prompt_provider := GetProcAddress(SvnClientLib, 'svn_client_get_ssl_client_cert_pw_prompt_provider');
      @svn_client_get_ssl_server_trust_file_provider := GetProcAddress(SvnClientLib, 'svn_client_get_ssl_server_trust_file_provider');
      @svn_client_get_ssl_server_trust_prompt_provider := GetProcAddress(SvnClientLib, 'svn_client_get_ssl_server_trust_prompt_provider');
      @svn_client_get_username_prompt_provider := GetProcAddress(SvnClientLib, 'svn_client_get_username_prompt_provider');
      @svn_client_get_username_provider := GetProcAddress(SvnClientLib, 'svn_client_get_username_provider');
      @svn_client_get_windows_simple_provider := GetProcAddress(SvnClientLib, 'svn_client_get_windows_simple_provider');
      @svn_client_import := GetProcAddress(SvnClientLib, 'svn_client_import');
      @svn_client_import2 := GetProcAddress(SvnClientLib, 'svn_client_import2');
      @svn_client_import3 := GetProcAddress(SvnClientLib, 'svn_client_import3');
      @svn_client_info := GetProcAddress(SvnClientLib, 'svn_client_info');
      @svn_client_info2 := GetProcAddress(SvnClientLib, 'svn_client_info2');
      @svn_client_list := GetProcAddress(SvnClientLib, 'svn_client_list');
      @svn_client_list2 := GetProcAddress(SvnClientLib, 'svn_client_list2');
      @svn_client_lock := GetProcAddress(SvnClientLib, 'svn_client_lock');
      @svn_client_log := GetProcAddress(SvnClientLib, 'svn_client_log');
      @svn_client_log2 := GetProcAddress(SvnClientLib, 'svn_client_log2');
      @svn_client_log3 := GetProcAddress(SvnClientLib, 'svn_client_log3');
      @svn_client_log4 := GetProcAddress(SvnClientLib, 'svn_client_log4');
      @svn_client_ls := GetProcAddress(SvnClientLib, 'svn_client_ls');
      @svn_client_ls2 := GetProcAddress(SvnClientLib, 'svn_client_ls2');
      @svn_client_ls3 := GetProcAddress(SvnClientLib, 'svn_client_ls3');
      @svn_client_merge := GetProcAddress(SvnClientLib, 'svn_client_merge');
      @svn_client_merge2 := GetProcAddress(SvnClientLib, 'svn_client_merge2');
      @svn_client_merge3 := GetProcAddress(SvnClientLib, 'svn_client_merge3');
      @svn_client_merge_peg := GetProcAddress(SvnClientLib, 'svn_client_merge_peg');
      @svn_client_merge_peg2 := GetProcAddress(SvnClientLib, 'svn_client_merge_peg2');
      @svn_client_merge_peg3 := GetProcAddress(SvnClientLib, 'svn_client_merge_peg3');
      @svn_client_merge_reintegrate := GetProcAddress(SvnClientLib, 'svn_client_merge_reintegrate');
      @svn_client_mergeinfo_get_merged := GetProcAddress(SvnClientLib, 'svn_client_mergeinfo_get_merged');
      @svn_client_mergeinfo_log_eligible := GetProcAddress(SvnClientLib, 'svn_client_mergeinfo_log_eligible');
      @svn_client_mergeinfo_log_merged := GetProcAddress(SvnClientLib, 'svn_client_mergeinfo_log_merged');
      @svn_client_mkdir := GetProcAddress(SvnClientLib, 'svn_client_mkdir');
      @svn_client_mkdir2 := GetProcAddress(SvnClientLib, 'svn_client_mkdir2');
      @svn_client_mkdir3 := GetProcAddress(SvnClientLib, 'svn_client_mkdir3');
      @svn_client_move := GetProcAddress(SvnClientLib, 'svn_client_move');
      @svn_client_move2 := GetProcAddress(SvnClientLib, 'svn_client_move2');
      @svn_client_move3 := GetProcAddress(SvnClientLib, 'svn_client_move3');
      @svn_client_move4 := GetProcAddress(SvnClientLib, 'svn_client_move4');
      @svn_client_move5 := GetProcAddress(SvnClientLib, 'svn_client_move5');
      @svn_client_open_ra_session := GetProcAddress(SvnClientLib, 'svn_client_open_ra_session');
      @svn_client_propget := GetProcAddress(SvnClientLib, 'svn_client_propget');
      @svn_client_propget2 := GetProcAddress(SvnClientLib, 'svn_client_propget2');
      @svn_client_propget3 := GetProcAddress(SvnClientLib, 'svn_client_propget3');
      @svn_client_proplist := GetProcAddress(SvnClientLib, 'svn_client_proplist');
      @svn_client_proplist2 := GetProcAddress(SvnClientLib, 'svn_client_proplist2');
      @svn_client_proplist3 := GetProcAddress(SvnClientLib, 'svn_client_proplist3');
      @svn_client_proplist_item_dup := GetProcAddress(SvnClientLib, 'svn_client_proplist_item_dup');
      @svn_client_propset := GetProcAddress(SvnClientLib, 'svn_client_propset');
      @svn_client_propset2 := GetProcAddress(SvnClientLib, 'svn_client_propset2');
      @svn_client_propset3 := GetProcAddress(SvnClientLib, 'svn_client_propset3');
      @svn_client_relocate := GetProcAddress(SvnClientLib, 'svn_client_relocate');
      @svn_client_remove_from_changelists := GetProcAddress(SvnClientLib, 'svn_client_remove_from_changelists');
      @svn_client_resolve := GetProcAddress(SvnClientLib, 'svn_client_resolve');
      @svn_client_resolved := GetProcAddress(SvnClientLib, 'svn_client_resolved');
      @svn_client_revert := GetProcAddress(SvnClientLib, 'svn_client_revert');
      @svn_client_revert2 := GetProcAddress(SvnClientLib, 'svn_client_revert2');
      @svn_client_revprop_get := GetProcAddress(SvnClientLib, 'svn_client_revprop_get');
      @svn_client_revprop_list := GetProcAddress(SvnClientLib, 'svn_client_revprop_list');
      @svn_client_revprop_set := GetProcAddress(SvnClientLib, 'svn_client_revprop_set');
      @svn_client_root_url_from_path := GetProcAddress(SvnClientLib, 'svn_client_root_url_from_path');
      @svn_client_status := GetProcAddress(SvnClientLib, 'svn_client_status');
      @svn_client_status2 := GetProcAddress(SvnClientLib, 'svn_client_status2');
      @svn_client_status3 := GetProcAddress(SvnClientLib, 'svn_client_status3');
      @svn_client_suggest_merge_sources := GetProcAddress(SvnClientLib, 'svn_client_suggest_merge_sources');
      @svn_client_switch := GetProcAddress(SvnClientLib, 'svn_client_switch');
      @svn_client_switch2 := GetProcAddress(SvnClientLib, 'svn_client_switch2');
      @svn_client_unlock := GetProcAddress(SvnClientLib, 'svn_client_unlock');
      @svn_client_update := GetProcAddress(SvnClientLib, 'svn_client_update');
      @svn_client_update2 := GetProcAddress(SvnClientLib, 'svn_client_update2');
      @svn_client_update3 := GetProcAddress(SvnClientLib, 'svn_client_update3');
      @svn_client_url_from_path := GetProcAddress(SvnClientLib, 'svn_client_url_from_path');
      @svn_client_uuid_from_path := GetProcAddress(SvnClientLib, 'svn_client_uuid_from_path');
      @svn_client_uuid_from_url := GetProcAddress(SvnClientLib, 'svn_client_uuid_from_url');
      @svn_client_version := GetProcAddress(SvnClientLib, 'svn_client_version');
      @svn_info_dup := GetProcAddress(SvnClientLib, 'svn_info_dup');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function LoadSvnDeltaLib(const FileName: string): Boolean;

var
  LibFileName: string;

begin
  Result := not SvnDeltaLibLoaded;

  if Result then
  begin
    if FileName = '' then
      LibFileName := 'libsvn_delta-1.dll'
    else
      LibFileName := FileName;

    SvnDeltaLib := LoadLibrary(PChar(LibFileName));
    Result := SvnDeltaLib <> 0;
    if not Result then
      SvnDeltaLib := INVALID_HANDLE_VALUE
    else
    begin
      @svn_compat_wrap_file_rev_handler := GetProcAddress(SvnDeltaLib, 'svn_compat_wrap_file_rev_handler');
      @svn_delta_default_editor := GetProcAddress(SvnDeltaLib, 'svn_delta_default_editor');
      @svn_delta_depth_filter_editor := GetProcAddress(SvnDeltaLib, 'svn_delta_depth_filter_editor');
      @svn_delta_get_cancellation_editor := GetProcAddress(SvnDeltaLib, 'svn_delta_get_cancellation_editor');
      @svn_delta_noop_window_handler := GetProcAddress(SvnDeltaLib, 'svn_delta_noop_window_handler');
      @svn_delta_path_driver := GetProcAddress(SvnDeltaLib, 'svn_delta_path_driver');
      @svn_delta_version := GetProcAddress(SvnDeltaLib, 'svn_delta_version');
      @svn_txdelta := GetProcAddress(SvnDeltaLib, 'svn_txdelta');
      @svn_txdelta_apply := GetProcAddress(SvnDeltaLib, 'svn_txdelta_apply');
      @svn_txdelta_apply_instructions := GetProcAddress(SvnDeltaLib, 'svn_txdelta_apply_instructions');
      @svn_txdelta_compose_windows := GetProcAddress(SvnDeltaLib, 'svn_txdelta_compose_windows');
      @svn_txdelta_md5_digest := GetProcAddress(SvnDeltaLib, 'svn_txdelta_md5_digest');
      @svn_txdelta_next_window := GetProcAddress(SvnDeltaLib, 'svn_txdelta_next_window');
      @svn_txdelta_parse_svndiff := GetProcAddress(SvnDeltaLib, 'svn_txdelta_parse_svndiff');
      @svn_txdelta_read_svndiff_window := GetProcAddress(SvnDeltaLib, 'svn_txdelta_read_svndiff_window');
      @svn_txdelta_send_stream := GetProcAddress(SvnDeltaLib, 'svn_txdelta_send_stream');
      @svn_txdelta_send_string := GetProcAddress(SvnDeltaLib, 'svn_txdelta_send_string');
      @svn_txdelta_send_txstream := GetProcAddress(SvnDeltaLib, 'svn_txdelta_send_txstream');
      @svn_txdelta_skip_svndiff_window := GetProcAddress(SvnDeltaLib, 'svn_txdelta_skip_svndiff_window');
      @svn_txdelta_stream_create := GetProcAddress(SvnDeltaLib, 'svn_txdelta_stream_create');
      @svn_txdelta_target_push := GetProcAddress(SvnDeltaLib, 'svn_txdelta_target_push');
      @svn_txdelta_to_svndiff := GetProcAddress(SvnDeltaLib, 'svn_txdelta_to_svndiff');
      @svn_txdelta_to_svndiff2 := GetProcAddress(SvnDeltaLib, 'svn_txdelta_to_svndiff2');
      @svn_txdelta_window_dup := GetProcAddress(SvnDeltaLib, 'svn_txdelta_window_dup');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function LoadSvnDiffLib(const FileName: string): Boolean;

var
  LibFileName: string;

begin
  Result := not SvnDiffLibLoaded;

  if Result then
  begin
    if FileName = '' then
      LibFileName := 'libsvn_diff-1.dll'
    else
      LibFileName := FileName;

    SvnDiffLib := LoadLibrary(PChar(LibFileName));
    Result := SvnDiffLib <> 0;
    if not Result then
      SvnDiffLib := INVALID_HANDLE_VALUE
    else
    begin
      @svn_diff_contains_conflicts := GetProcAddress(SvnDiffLib, 'svn_diff_contains_conflicts');
      @svn_diff_contains_diffs := GetProcAddress(SvnDiffLib, 'svn_diff_contains_diffs');
      @svn_diff_diff := GetProcAddress(SvnDiffLib, 'svn_diff_diff');
      @svn_diff_diff3 := GetProcAddress(SvnDiffLib, 'svn_diff_diff3');
      @svn_diff_diff4 := GetProcAddress(SvnDiffLib, 'svn_diff_diff4');
      @svn_diff_file_diff := GetProcAddress(SvnDiffLib, 'svn_diff_file_diff');
      @svn_diff_file_diff3 := GetProcAddress(SvnDiffLib, 'svn_diff_file_diff3');
      @svn_diff_file_diff3_2 := GetProcAddress(SvnDiffLib, 'svn_diff_file_diff3_2');
      @svn_diff_file_diff4 := GetProcAddress(SvnDiffLib, 'svn_diff_file_diff4');
      @svn_diff_file_diff4_2 := GetProcAddress(SvnDiffLib, 'svn_diff_file_diff4_2');
      @svn_diff_file_diff_2 := GetProcAddress(SvnDiffLib, 'svn_diff_file_diff_2');
      @svn_diff_file_options_create := GetProcAddress(SvnDiffLib, 'svn_diff_file_options_create');
      @svn_diff_file_options_parse := GetProcAddress(SvnDiffLib, 'svn_diff_file_options_parse');
      @svn_diff_file_output_merge := GetProcAddress(SvnDiffLib, 'svn_diff_file_output_merge');
      @svn_diff_file_output_unified := GetProcAddress(SvnDiffLib, 'svn_diff_file_output_unified');
      @svn_diff_file_output_unified2 := GetProcAddress(SvnDiffLib, 'svn_diff_file_output_unified2');
      @svn_diff_file_output_unified3 := GetProcAddress(SvnDiffLib, 'svn_diff_file_output_unified3');
      @svn_diff_mem_string_diff := GetProcAddress(SvnDiffLib, 'svn_diff_mem_string_diff');
      @svn_diff_mem_string_diff3 := GetProcAddress(SvnDiffLib, 'svn_diff_mem_string_diff3');
      @svn_diff_mem_string_diff4 := GetProcAddress(SvnDiffLib, 'svn_diff_mem_string_diff4');
      @svn_diff_mem_string_output_merge := GetProcAddress(SvnDiffLib, 'svn_diff_mem_string_output_merge');
      @svn_diff_mem_string_output_unified := GetProcAddress(SvnDiffLib, 'svn_diff_mem_string_output_unified');
      @svn_diff_output := GetProcAddress(SvnDiffLib, 'svn_diff_output');
      @svn_diff_version := GetProcAddress(SvnDiffLib, 'svn_diff_version');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function LoadSvnFsLib(const FileName: string): Boolean;

var
  LibFileName: string;

begin
  Result := not SvnFsLibLoaded;

  if Result then
  begin
    if FileName = '' then
      LibFileName := 'libsvn_fs-1.dll'
    else
      LibFileName := FileName;

    SvnFsLib := LoadLibrary(PChar(LibFileName));
    Result := SvnFsLib <> 0;
    if not Result then
      SvnFsLib := INVALID_HANDLE_VALUE
    else
    begin
      @svn_fs_abort_txn := GetProcAddress(SvnFsLib, 'svn_fs_abort_txn');
      @svn_fs_access_add_lock_token := GetProcAddress(SvnFsLib, 'svn_fs_access_add_lock_token');
      @svn_fs_access_get_username := GetProcAddress(SvnFsLib, 'svn_fs_access_get_username');
      @svn_fs_apply_text := GetProcAddress(SvnFsLib, 'svn_fs_apply_text');
      @svn_fs_apply_textdelta := GetProcAddress(SvnFsLib, 'svn_fs_apply_textdelta');
      @svn_fs_begin_txn := GetProcAddress(SvnFsLib, 'svn_fs_begin_txn');
      @svn_fs_begin_txn2 := GetProcAddress(SvnFsLib, 'svn_fs_begin_txn2');
      @svn_fs_berkeley_logfiles := GetProcAddress(SvnFsLib, 'svn_fs_berkeley_logfiles');
      @svn_fs_berkeley_path := GetProcAddress(SvnFsLib, 'svn_fs_berkeley_path');
      @svn_fs_berkeley_recover := GetProcAddress(SvnFsLib, 'svn_fs_berkeley_recover');
      @svn_fs_change_node_prop := GetProcAddress(SvnFsLib, 'svn_fs_change_node_prop');
      @svn_fs_change_rev_prop := GetProcAddress(SvnFsLib, 'svn_fs_change_rev_prop');
      @svn_fs_change_txn_prop := GetProcAddress(SvnFsLib, 'svn_fs_change_txn_prop');
      @svn_fs_change_txn_props := GetProcAddress(SvnFsLib, 'svn_fs_change_txn_props');
      @svn_fs_check_path := GetProcAddress(SvnFsLib, 'svn_fs_check_path');
      @svn_fs_check_related := GetProcAddress(SvnFsLib, 'svn_fs_check_related');
      @svn_fs_close_root := GetProcAddress(SvnFsLib, 'svn_fs_close_root');
      @svn_fs_closest_copy := GetProcAddress(SvnFsLib, 'svn_fs_closest_copy');
      @svn_fs_commit_txn := GetProcAddress(SvnFsLib, 'svn_fs_commit_txn');
      @svn_fs_compare_ids := GetProcAddress(SvnFsLib, 'svn_fs_compare_ids');
      @svn_fs_contents_changed := GetProcAddress(SvnFsLib, 'svn_fs_contents_changed');
      @svn_fs_copied_from := GetProcAddress(SvnFsLib, 'svn_fs_copied_from');
      @svn_fs_copy := GetProcAddress(SvnFsLib, 'svn_fs_copy');
      @svn_fs_create := GetProcAddress(SvnFsLib, 'svn_fs_create');
      @svn_fs_create_access := GetProcAddress(SvnFsLib, 'svn_fs_create_access');
      @svn_fs_create_berkeley := GetProcAddress(SvnFsLib, 'svn_fs_create_berkeley');
      @svn_fs_delete := GetProcAddress(SvnFsLib, 'svn_fs_delete');
      @svn_fs_delete_berkeley := GetProcAddress(SvnFsLib, 'svn_fs_delete_berkeley');
      @svn_fs_delete_fs := GetProcAddress(SvnFsLib, 'svn_fs_delete_fs');
      @svn_fs_deltify_revision := GetProcAddress(SvnFsLib, 'svn_fs_deltify_revision');
      @svn_fs_dir_entries := GetProcAddress(SvnFsLib, 'svn_fs_dir_entries');
      @svn_fs_file_contents := GetProcAddress(SvnFsLib, 'svn_fs_file_contents');
      @svn_fs_file_length := GetProcAddress(SvnFsLib, 'svn_fs_file_length');
      @svn_fs_file_md5_checksum := GetProcAddress(SvnFsLib, 'svn_fs_file_md5_checksum');
      @svn_fs_generate_lock_token := GetProcAddress(SvnFsLib, 'svn_fs_generate_lock_token');
      @svn_fs_get_access := GetProcAddress(SvnFsLib, 'svn_fs_get_access');
      @svn_fs_get_file_delta_stream := GetProcAddress(SvnFsLib, 'svn_fs_get_file_delta_stream');
      @svn_fs_get_lock := GetProcAddress(SvnFsLib, 'svn_fs_get_lock');
      @svn_fs_get_locks := GetProcAddress(SvnFsLib, 'svn_fs_get_locks');
      @svn_fs_get_mergeinfo := GetProcAddress(SvnFsLib, 'svn_fs_get_mergeinfo');
      @svn_fs_get_uuid := GetProcAddress(SvnFsLib, 'svn_fs_get_uuid');
      @svn_fs_history_location := GetProcAddress(SvnFsLib, 'svn_fs_history_location');
      @svn_fs_history_prev := GetProcAddress(SvnFsLib, 'svn_fs_history_prev');
      @svn_fs_hotcopy := GetProcAddress(SvnFsLib, 'svn_fs_hotcopy');
      @svn_fs_hotcopy_berkeley := GetProcAddress(SvnFsLib, 'svn_fs_hotcopy_berkeley');
      @svn_fs_initialize := GetProcAddress(SvnFsLib, 'svn_fs_initialize');
      @svn_fs_is_dir := GetProcAddress(SvnFsLib, 'svn_fs_is_dir');
      @svn_fs_is_file := GetProcAddress(SvnFsLib, 'svn_fs_is_file');
      @svn_fs_is_revision_root := GetProcAddress(SvnFsLib, 'svn_fs_is_revision_root');
      @svn_fs_is_txn_root := GetProcAddress(SvnFsLib, 'svn_fs_is_txn_root');
      @svn_fs_list_transactions := GetProcAddress(SvnFsLib, 'svn_fs_list_transactions');
      @svn_fs_lock := GetProcAddress(SvnFsLib, 'svn_fs_lock');
      @svn_fs_make_dir := GetProcAddress(SvnFsLib, 'svn_fs_make_dir');
      @svn_fs_make_file := GetProcAddress(SvnFsLib, 'svn_fs_make_file');
      @svn_fs_merge := GetProcAddress(SvnFsLib, 'svn_fs_merge');
      @svn_fs_new := GetProcAddress(SvnFsLib, 'svn_fs_new');
      @svn_fs_node_created_path := GetProcAddress(SvnFsLib, 'svn_fs_node_created_path');
      @svn_fs_node_created_rev := GetProcAddress(SvnFsLib, 'svn_fs_node_created_rev');
      @svn_fs_node_history := GetProcAddress(SvnFsLib, 'svn_fs_node_history');
      @svn_fs_node_id := GetProcAddress(SvnFsLib, 'svn_fs_node_id');
      @svn_fs_node_origin_rev := GetProcAddress(SvnFsLib, 'svn_fs_node_origin_rev');
      @svn_fs_node_prop := GetProcAddress(SvnFsLib, 'svn_fs_node_prop');
      @svn_fs_node_proplist := GetProcAddress(SvnFsLib, 'svn_fs_node_proplist');
      @svn_fs_open := GetProcAddress(SvnFsLib, 'svn_fs_open');
      @svn_fs_open_berkeley := GetProcAddress(SvnFsLib, 'svn_fs_open_berkeley');
      @svn_fs_open_txn := GetProcAddress(SvnFsLib, 'svn_fs_open_txn');
      @svn_fs_parse_id := GetProcAddress(SvnFsLib, 'svn_fs_parse_id');
      @svn_fs_path := GetProcAddress(SvnFsLib, 'svn_fs_path');
      @svn_fs_paths_changed := GetProcAddress(SvnFsLib, 'svn_fs_paths_changed');
      @svn_fs_print_modules := GetProcAddress(SvnFsLib, 'svn_fs_print_modules');
      @svn_fs_props_changed := GetProcAddress(SvnFsLib, 'svn_fs_props_changed');
      @svn_fs_purge_txn := GetProcAddress(SvnFsLib, 'svn_fs_purge_txn');
      @svn_fs_recover := GetProcAddress(SvnFsLib, 'svn_fs_recover');
      @svn_fs_revision_link := GetProcAddress(SvnFsLib, 'svn_fs_revision_link');
      @svn_fs_revision_prop := GetProcAddress(SvnFsLib, 'svn_fs_revision_prop');
      @svn_fs_revision_proplist := GetProcAddress(SvnFsLib, 'svn_fs_revision_proplist');
      @svn_fs_revision_root := GetProcAddress(SvnFsLib, 'svn_fs_revision_root');
      @svn_fs_revision_root_revision := GetProcAddress(SvnFsLib, 'svn_fs_revision_root_revision');
      @svn_fs_root_fs := GetProcAddress(SvnFsLib, 'svn_fs_root_fs');
      @svn_fs_set_access := GetProcAddress(SvnFsLib, 'svn_fs_set_access');
      @svn_fs_set_berkeley_errcall := GetProcAddress(SvnFsLib, 'svn_fs_set_berkeley_errcall');
      @svn_fs_set_uuid := GetProcAddress(SvnFsLib, 'svn_fs_set_uuid');
      @svn_fs_set_warning_func := GetProcAddress(SvnFsLib, 'svn_fs_set_warning_func');
      @svn_fs_txn_base_revision := GetProcAddress(SvnFsLib, 'svn_fs_txn_base_revision');
      @svn_fs_txn_name := GetProcAddress(SvnFsLib, 'svn_fs_txn_name');
      @svn_fs_txn_prop := GetProcAddress(SvnFsLib, 'svn_fs_txn_prop');
      @svn_fs_txn_proplist := GetProcAddress(SvnFsLib, 'svn_fs_txn_proplist');
      @svn_fs_txn_root := GetProcAddress(SvnFsLib, 'svn_fs_txn_root');
      @svn_fs_txn_root_base_revision := GetProcAddress(SvnFsLib, 'svn_fs_txn_root_base_revision');
      @svn_fs_txn_root_name := GetProcAddress(SvnFsLib, 'svn_fs_txn_root_name');
      @svn_fs_type := GetProcAddress(SvnFsLib, 'svn_fs_type');
      @svn_fs_unlock := GetProcAddress(SvnFsLib, 'svn_fs_unlock');
      @svn_fs_unparse_id := GetProcAddress(SvnFsLib, 'svn_fs_unparse_id');
      @svn_fs_upgrade := GetProcAddress(SvnFsLib, 'svn_fs_upgrade');
      @svn_fs_version := GetProcAddress(SvnFsLib, 'svn_fs_version');
      @svn_fs_youngest_rev := GetProcAddress(SvnFsLib, 'svn_fs_youngest_rev');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function LoadSvnRaLib(const FileName: string): Boolean;

var
  LibFileName: string;

begin
  Result := not SvnRaLibLoaded;

  if Result then
  begin
    if FileName = '' then
      LibFileName := 'libsvn_ra-1.dll'
    else
      LibFileName := FileName;

    SvnRaLib := LoadLibrary(PChar(LibFileName));
    Result := SvnRaLib <> 0;
    if not Result then
      SvnRaLib := INVALID_HANDLE_VALUE
    else
    begin
      @svn_ra_change_rev_prop := GetProcAddress(SvnRaLib, 'svn_ra_change_rev_prop');
      @svn_ra_check_path := GetProcAddress(SvnRaLib, 'svn_ra_check_path');
      @svn_ra_create_callbacks := GetProcAddress(SvnRaLib, 'svn_ra_create_callbacks');
      @svn_ra_dav_init := GetProcAddress(SvnRaLib, 'svn_ra_dav_init');
      @svn_ra_do_diff := GetProcAddress(SvnRaLib, 'svn_ra_do_diff');
      @svn_ra_do_diff2 := GetProcAddress(SvnRaLib, 'svn_ra_do_diff2');
      @svn_ra_do_diff3 := GetProcAddress(SvnRaLib, 'svn_ra_do_diff3');
      @svn_ra_do_status := GetProcAddress(SvnRaLib, 'svn_ra_do_status');
      @svn_ra_do_status2 := GetProcAddress(SvnRaLib, 'svn_ra_do_status2');
      @svn_ra_do_switch := GetProcAddress(SvnRaLib, 'svn_ra_do_switch');
      @svn_ra_do_switch2 := GetProcAddress(SvnRaLib, 'svn_ra_do_switch2');
      @svn_ra_do_update := GetProcAddress(SvnRaLib, 'svn_ra_do_update');
      @svn_ra_do_update2 := GetProcAddress(SvnRaLib, 'svn_ra_do_update2');
      @svn_ra_get_commit_editor := GetProcAddress(SvnRaLib, 'svn_ra_get_commit_editor');
      @svn_ra_get_commit_editor2 := GetProcAddress(SvnRaLib, 'svn_ra_get_commit_editor2');
      @svn_ra_get_commit_editor3 := GetProcAddress(SvnRaLib, 'svn_ra_get_commit_editor3');
      @svn_ra_get_dated_revision := GetProcAddress(SvnRaLib, 'svn_ra_get_dated_revision');
      @svn_ra_get_dir := GetProcAddress(SvnRaLib, 'svn_ra_get_dir');
      @svn_ra_get_dir2 := GetProcAddress(SvnRaLib, 'svn_ra_get_dir2');
      @svn_ra_get_file := GetProcAddress(SvnRaLib, 'svn_ra_get_file');
      @svn_ra_get_file_revs := GetProcAddress(SvnRaLib, 'svn_ra_get_file_revs');
      @svn_ra_get_file_revs2 := GetProcAddress(SvnRaLib, 'svn_ra_get_file_revs2');
      @svn_ra_get_latest_revnum := GetProcAddress(SvnRaLib, 'svn_ra_get_latest_revnum');
      @svn_ra_get_location_segments := GetProcAddress(SvnRaLib, 'svn_ra_get_location_segments');
      @svn_ra_get_locations := GetProcAddress(SvnRaLib, 'svn_ra_get_locations');
      @svn_ra_get_lock := GetProcAddress(SvnRaLib, 'svn_ra_get_lock');
      @svn_ra_get_locks := GetProcAddress(SvnRaLib, 'svn_ra_get_locks');
      @svn_ra_get_log := GetProcAddress(SvnRaLib, 'svn_ra_get_log');
      @svn_ra_get_log2 := GetProcAddress(SvnRaLib, 'svn_ra_get_log2');
      @svn_ra_get_mergeinfo := GetProcAddress(SvnRaLib, 'svn_ra_get_mergeinfo');
      @svn_ra_get_ra_library := GetProcAddress(SvnRaLib, 'svn_ra_get_ra_library');
      @svn_ra_get_repos_root := GetProcAddress(SvnRaLib, 'svn_ra_get_repos_root');
      @svn_ra_get_repos_root2 := GetProcAddress(SvnRaLib, 'svn_ra_get_repos_root2');
      @svn_ra_get_session_url := GetProcAddress(SvnRaLib, 'svn_ra_get_session_url');
      @svn_ra_get_uuid := GetProcAddress(SvnRaLib, 'svn_ra_get_uuid');
      @svn_ra_get_uuid2 := GetProcAddress(SvnRaLib, 'svn_ra_get_uuid2');
      @svn_ra_has_capability := GetProcAddress(SvnRaLib, 'svn_ra_has_capability');
      @svn_ra_init_ra_libs := GetProcAddress(SvnRaLib, 'svn_ra_init_ra_libs');
      @svn_ra_initialize := GetProcAddress(SvnRaLib, 'svn_ra_initialize');
      @svn_ra_local_init := GetProcAddress(SvnRaLib, 'svn_ra_local_init');
      @svn_ra_lock := GetProcAddress(SvnRaLib, 'svn_ra_lock');
      @svn_ra_open := GetProcAddress(SvnRaLib, 'svn_ra_open');
      @svn_ra_open2 := GetProcAddress(SvnRaLib, 'svn_ra_open2');
      @svn_ra_open3 := GetProcAddress(SvnRaLib, 'svn_ra_open3');
      @svn_ra_print_modules := GetProcAddress(SvnRaLib, 'svn_ra_print_modules');
      @svn_ra_print_ra_libraries := GetProcAddress(SvnRaLib, 'svn_ra_print_ra_libraries');
      @svn_ra_reparent := GetProcAddress(SvnRaLib, 'svn_ra_reparent');
      @svn_ra_replay := GetProcAddress(SvnRaLib, 'svn_ra_replay');
      @svn_ra_replay_range := GetProcAddress(SvnRaLib, 'svn_ra_replay_range');
      @svn_ra_rev_prop := GetProcAddress(SvnRaLib, 'svn_ra_rev_prop');
      @svn_ra_rev_proplist := GetProcAddress(SvnRaLib, 'svn_ra_rev_proplist');
      @svn_ra_serf_init := GetProcAddress(SvnRaLib, 'svn_ra_serf_init');
      @svn_ra_stat := GetProcAddress(SvnRaLib, 'svn_ra_stat');
      @svn_ra_svn_init := GetProcAddress(SvnRaLib, 'svn_ra_svn_init');
      @svn_ra_unlock := GetProcAddress(SvnRaLib, 'svn_ra_unlock');
      @svn_ra_version := GetProcAddress(SvnRaLib, 'svn_ra_version');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function LoadSvnReposLib(const FileName: string = ''): Boolean;

var
  LibFileName: string;

begin
  Result := not SvnReposLibLoaded;

  if Result then
  begin
    if FileName = '' then
      LibFileName := 'libsvn_repos-1.dll'
    else
      LibFileName := FileName;

    SvnReposLib := LoadLibrary(PChar(LibFileName));
    Result := SvnReposLib <> 0;
    if not Result then
      SvnReposLib := INVALID_HANDLE_VALUE
    else
    begin
      @svn_repos_abort_report := GetProcAddress(SvnReposLib, 'svn_repos_abort_report');
      @svn_repos_authz_check_access := GetProcAddress(SvnReposLib, 'svn_repos_authz_check_access');
      @svn_repos_authz_read := GetProcAddress(SvnReposLib, 'svn_repos_authz_read');
      @svn_repos_begin_report := GetProcAddress(SvnReposLib, 'svn_repos_begin_report');
      @svn_repos_begin_report2 := GetProcAddress(SvnReposLib, 'svn_repos_begin_report2');
      @svn_repos_check_revision_access := GetProcAddress(SvnReposLib, 'svn_repos_check_revision_access');
      @svn_repos_conf_dir := GetProcAddress(SvnReposLib, 'svn_repos_conf_dir');
      @svn_repos_create := GetProcAddress(SvnReposLib, 'svn_repos_create');
      @svn_repos_dated_revision := GetProcAddress(SvnReposLib, 'svn_repos_dated_revision');
      @svn_repos_db_env := GetProcAddress(SvnReposLib, 'svn_repos_db_env');
      @svn_repos_db_lockfile := GetProcAddress(SvnReposLib, 'svn_repos_db_lockfile');
      @svn_repos_db_logfiles := GetProcAddress(SvnReposLib, 'svn_repos_db_logfiles');
      @svn_repos_db_logs_lockfile := GetProcAddress(SvnReposLib, 'svn_repos_db_logs_lockfile');
      @svn_repos_delete := GetProcAddress(SvnReposLib, 'svn_repos_delete');
      @svn_repos_delete_path := GetProcAddress(SvnReposLib, 'svn_repos_delete_path');
      @svn_repos_deleted_rev := GetProcAddress(SvnReposLib, 'svn_repos_deleted_rev');
      @svn_repos_dir_delta := GetProcAddress(SvnReposLib, 'svn_repos_dir_delta');
      @svn_repos_dir_delta2 := GetProcAddress(SvnReposLib, 'svn_repos_dir_delta2');
      @svn_repos_dump_fs := GetProcAddress(SvnReposLib, 'svn_repos_dump_fs');
      @svn_repos_dump_fs2 := GetProcAddress(SvnReposLib, 'svn_repos_dump_fs2');
      @svn_repos_find_root_path := GetProcAddress(SvnReposLib, 'svn_repos_find_root_path');
      @svn_repos_finish_report := GetProcAddress(SvnReposLib, 'svn_repos_finish_report');
      @svn_repos_fs := GetProcAddress(SvnReposLib, 'svn_repos_fs');
      @svn_repos_fs_begin_txn_for_commit := GetProcAddress(SvnReposLib, 'svn_repos_fs_begin_txn_for_commit');
      @svn_repos_fs_begin_txn_for_commit2 := GetProcAddress(SvnReposLib, 'svn_repos_fs_begin_txn_for_commit2');
      @svn_repos_fs_begin_txn_for_update := GetProcAddress(SvnReposLib, 'svn_repos_fs_begin_txn_for_update');
      @svn_repos_fs_change_node_prop := GetProcAddress(SvnReposLib, 'svn_repos_fs_change_node_prop');
      @svn_repos_fs_change_rev_prop := GetProcAddress(SvnReposLib, 'svn_repos_fs_change_rev_prop');
      @svn_repos_fs_change_rev_prop2 := GetProcAddress(SvnReposLib, 'svn_repos_fs_change_rev_prop2');
      @svn_repos_fs_change_rev_prop3 := GetProcAddress(SvnReposLib, 'svn_repos_fs_change_rev_prop3');
      @svn_repos_fs_change_txn_prop := GetProcAddress(SvnReposLib, 'svn_repos_fs_change_txn_prop');
      @svn_repos_fs_change_txn_props := GetProcAddress(SvnReposLib, 'svn_repos_fs_change_txn_props');
      @svn_repos_fs_commit_txn := GetProcAddress(SvnReposLib, 'svn_repos_fs_commit_txn');
      @svn_repos_fs_get_locks := GetProcAddress(SvnReposLib, 'svn_repos_fs_get_locks');
      @svn_repos_fs_get_mergeinfo := GetProcAddress(SvnReposLib, 'svn_repos_fs_get_mergeinfo');
      @svn_repos_fs_lock := GetProcAddress(SvnReposLib, 'svn_repos_fs_lock');
      @svn_repos_fs_revision_prop := GetProcAddress(SvnReposLib, 'svn_repos_fs_revision_prop');
      @svn_repos_fs_revision_proplist := GetProcAddress(SvnReposLib, 'svn_repos_fs_revision_proplist');
      @svn_repos_fs_unlock := GetProcAddress(SvnReposLib, 'svn_repos_fs_unlock');
      @svn_repos_get_commit_editor := GetProcAddress(SvnReposLib, 'svn_repos_get_commit_editor');
      @svn_repos_get_commit_editor2 := GetProcAddress(SvnReposLib, 'svn_repos_get_commit_editor2');
      @svn_repos_get_commit_editor3 := GetProcAddress(SvnReposLib, 'svn_repos_get_commit_editor3');
      @svn_repos_get_commit_editor4 := GetProcAddress(SvnReposLib, 'svn_repos_get_commit_editor4');
      @svn_repos_get_commit_editor5 := GetProcAddress(SvnReposLib, 'svn_repos_get_commit_editor5');
      @svn_repos_get_committed_info := GetProcAddress(SvnReposLib, 'svn_repos_get_committed_info');
      @svn_repos_get_file_revs := GetProcAddress(SvnReposLib, 'svn_repos_get_file_revs');
      @svn_repos_get_file_revs2 := GetProcAddress(SvnReposLib, 'svn_repos_get_file_revs2');
      @svn_repos_get_fs_build_parser := GetProcAddress(SvnReposLib, 'svn_repos_get_fs_build_parser');
      @svn_repos_get_fs_build_parser2 := GetProcAddress(SvnReposLib, 'svn_repos_get_fs_build_parser2');
      @svn_repos_get_logs := GetProcAddress(SvnReposLib, 'svn_repos_get_logs');
      @svn_repos_get_logs2 := GetProcAddress(SvnReposLib, 'svn_repos_get_logs2');
      @svn_repos_get_logs3 := GetProcAddress(SvnReposLib, 'svn_repos_get_logs3');
      @svn_repos_get_logs4 := GetProcAddress(SvnReposLib, 'svn_repos_get_logs4');
      @svn_repos_has_capability := GetProcAddress(SvnReposLib, 'svn_repos_has_capability');
      @svn_repos_history := GetProcAddress(SvnReposLib, 'svn_repos_history');
      @svn_repos_history2 := GetProcAddress(SvnReposLib, 'svn_repos_history2');
      @svn_repos_hook_dir := GetProcAddress(SvnReposLib, 'svn_repos_hook_dir');
      @svn_repos_hotcopy := GetProcAddress(SvnReposLib, 'svn_repos_hotcopy');
      @svn_repos_link_path := GetProcAddress(SvnReposLib, 'svn_repos_link_path');
      @svn_repos_link_path2 := GetProcAddress(SvnReposLib, 'svn_repos_link_path2');
      @svn_repos_link_path3 := GetProcAddress(SvnReposLib, 'svn_repos_link_path3');
      @svn_repos_load_fs := GetProcAddress(SvnReposLib, 'svn_repos_load_fs');
      @svn_repos_load_fs2 := GetProcAddress(SvnReposLib, 'svn_repos_load_fs2');
      @svn_repos_lock_dir := GetProcAddress(SvnReposLib, 'svn_repos_lock_dir');
      @svn_repos_node_editor := GetProcAddress(SvnReposLib, 'svn_repos_node_editor');
      @svn_repos_node_from_baton := GetProcAddress(SvnReposLib, 'svn_repos_node_from_baton');
      @svn_repos_node_location_segments := GetProcAddress(SvnReposLib, 'svn_repos_node_location_segments');
      @svn_repos_open := GetProcAddress(SvnReposLib, 'svn_repos_open');
      @svn_repos_parse_dumpstream := GetProcAddress(SvnReposLib, 'svn_repos_parse_dumpstream');
      @svn_repos_parse_dumpstream2 := GetProcAddress(SvnReposLib, 'svn_repos_parse_dumpstream2');
      @svn_repos_path := GetProcAddress(SvnReposLib, 'svn_repos_path');
      @svn_repos_post_commit_hook := GetProcAddress(SvnReposLib, 'svn_repos_post_commit_hook');
      @svn_repos_post_lock_hook := GetProcAddress(SvnReposLib, 'svn_repos_post_lock_hook');
      @svn_repos_post_revprop_change_hook := GetProcAddress(SvnReposLib, 'svn_repos_post_revprop_change_hook');
      @svn_repos_post_unlock_hook := GetProcAddress(SvnReposLib, 'svn_repos_post_unlock_hook');
      @svn_repos_pre_commit_hook := GetProcAddress(SvnReposLib, 'svn_repos_pre_commit_hook');
      @svn_repos_pre_lock_hook := GetProcAddress(SvnReposLib, 'svn_repos_pre_lock_hook');
      @svn_repos_pre_revprop_change_hook := GetProcAddress(SvnReposLib, 'svn_repos_pre_revprop_change_hook');
      @svn_repos_pre_unlock_hook := GetProcAddress(SvnReposLib, 'svn_repos_pre_unlock_hook');
      @svn_repos_recover := GetProcAddress(SvnReposLib, 'svn_repos_recover');
      @svn_repos_recover2 := GetProcAddress(SvnReposLib, 'svn_repos_recover2');
      @svn_repos_recover3 := GetProcAddress(SvnReposLib, 'svn_repos_recover3');
      @svn_repos_remember_client_capabilities := GetProcAddress(SvnReposLib, 'svn_repos_remember_client_capabilities');
      @svn_repos_replay := GetProcAddress(SvnReposLib, 'svn_repos_replay');
      @svn_repos_replay2 := GetProcAddress(SvnReposLib, 'svn_repos_replay2');
      @svn_repos_set_path := GetProcAddress(SvnReposLib, 'svn_repos_set_path');
      @svn_repos_set_path2 := GetProcAddress(SvnReposLib, 'svn_repos_set_path2');
      @svn_repos_set_path3 := GetProcAddress(SvnReposLib, 'svn_repos_set_path3');
      @svn_repos_start_commit_hook := GetProcAddress(SvnReposLib, 'svn_repos_start_commit_hook');
      @svn_repos_stat := GetProcAddress(SvnReposLib, 'svn_repos_stat');
      @svn_repos_svnserve_conf := GetProcAddress(SvnReposLib, 'svn_repos_svnserve_conf');
      @svn_repos_trace_node_locations := GetProcAddress(SvnReposLib, 'svn_repos_trace_node_locations');
      @svn_repos_upgrade := GetProcAddress(SvnReposLib, 'svn_repos_upgrade');
      @svn_repos_verify_fs := GetProcAddress(SvnReposLib, 'svn_repos_verify_fs');
      @svn_repos_version := GetProcAddress(SvnReposLib, 'svn_repos_version');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function LoadSvnSubrLib(const FileName: string = ''): Boolean;

var
  LibFileName: string;

begin
  Result := not SvnSubrLibLoaded;

  if Result then
  begin
    if FileName = '' then
      LibFileName := 'libsvn_subr-1.dll'
    else
      LibFileName := FileName;

    SvnSubrLib := LoadLibrary(PChar(LibFileName));
    Result := SvnSubrLib <> 0;
    if not Result then
      SvnSubrLib := INVALID_HANDLE_VALUE
    else
    begin
      @svn_auth_first_credentials := GetProcAddress(SvnSubrLib, 'svn_auth_first_credentials');
      @svn_auth_get_parameter := GetProcAddress(SvnSubrLib, 'svn_auth_get_parameter');
      @svn_auth_get_simple_prompt_provider := GetProcAddress(SvnSubrLib, 'svn_auth_get_simple_prompt_provider');
      @svn_auth_get_simple_provider := GetProcAddress(SvnSubrLib, 'svn_auth_get_simple_provider');
      @svn_auth_get_ssl_client_cert_file_provider := GetProcAddress(SvnSubrLib, 'svn_auth_get_ssl_client_cert_file_provider');
      @svn_auth_get_ssl_client_cert_prompt_provider := GetProcAddress(SvnSubrLib, 'svn_auth_get_ssl_client_cert_prompt_provider');
      @svn_auth_get_ssl_client_cert_pw_file_provider := GetProcAddress(SvnSubrLib, 'svn_auth_get_ssl_client_cert_pw_file_provider');
      @svn_auth_get_ssl_client_cert_pw_prompt_provider := GetProcAddress(SvnSubrLib, 'svn_auth_get_ssl_client_cert_pw_prompt_provider');
      @svn_auth_get_ssl_server_trust_file_provider := GetProcAddress(SvnSubrLib, 'svn_auth_get_ssl_server_trust_file_provider');
      @svn_auth_get_ssl_server_trust_prompt_provider := GetProcAddress(SvnSubrLib, 'svn_auth_get_ssl_server_trust_prompt_provider');
      @svn_auth_get_username_prompt_provider := GetProcAddress(SvnSubrLib, 'svn_auth_get_username_prompt_provider');
      @svn_auth_get_username_provider := GetProcAddress(SvnSubrLib, 'svn_auth_get_username_provider');
      @svn_auth_get_windows_simple_provider := GetProcAddress(SvnSubrLib, 'svn_auth_get_windows_simple_provider');
      @svn_auth_get_windows_ssl_server_trust_provider := GetProcAddress(SvnSubrLib, 'svn_auth_get_windows_ssl_server_trust_provider');
      @svn_auth_next_credentials := GetProcAddress(SvnSubrLib, 'svn_auth_next_credentials');
      @svn_auth_open := GetProcAddress(SvnSubrLib, 'svn_auth_open');
      @svn_auth_save_credentials := GetProcAddress(SvnSubrLib, 'svn_auth_save_credentials');
      @svn_auth_set_parameter := GetProcAddress(SvnSubrLib, 'svn_auth_set_parameter');
      @svn_auth_ssl_server_cert_info_dup := GetProcAddress(SvnSubrLib, 'svn_auth_ssl_server_cert_info_dup');
      @svn_base64_decode := GetProcAddress(SvnSubrLib, 'svn_base64_decode');
      @svn_base64_decode_string := GetProcAddress(SvnSubrLib, 'svn_base64_decode_string');
      @svn_base64_encode := GetProcAddress(SvnSubrLib, 'svn_base64_encode');
      @svn_base64_encode_string := GetProcAddress(SvnSubrLib, 'svn_base64_encode_string');
      @svn_base64_from_md5 := GetProcAddress(SvnSubrLib, 'svn_base64_from_md5');
      @svn_categorize_props := GetProcAddress(SvnSubrLib, 'svn_categorize_props');
      @svn_cmdline__getopt_init := GetProcAddress(SvnSubrLib, 'svn_cmdline__getopt_init');
      @svn_cmdline_auth_simple_prompt := GetProcAddress(SvnSubrLib, 'svn_cmdline_auth_simple_prompt');
      @svn_cmdline_auth_ssl_client_cert_prompt := GetProcAddress(SvnSubrLib, 'svn_cmdline_auth_ssl_client_cert_prompt');
      @svn_cmdline_auth_ssl_client_cert_pw_prompt := GetProcAddress(SvnSubrLib, 'svn_cmdline_auth_ssl_client_cert_pw_prompt');
      @svn_cmdline_auth_ssl_server_trust_prompt := GetProcAddress(SvnSubrLib, 'svn_cmdline_auth_ssl_server_trust_prompt');
      @svn_cmdline_auth_username_prompt := GetProcAddress(SvnSubrLib, 'svn_cmdline_auth_username_prompt');
      @svn_cmdline_cstring_from_utf8 := GetProcAddress(SvnSubrLib, 'svn_cmdline_cstring_from_utf8');
      @svn_cmdline_cstring_from_utf8_fuzzy := GetProcAddress(SvnSubrLib, 'svn_cmdline_cstring_from_utf8_fuzzy');
      @svn_cmdline_cstring_to_utf8 := GetProcAddress(SvnSubrLib, 'svn_cmdline_cstring_to_utf8');
      @svn_cmdline_fflush := GetProcAddress(SvnSubrLib, 'svn_cmdline_fflush');
      @svn_cmdline_fprintf := GetProcAddress(SvnSubrLib, 'svn_cmdline_fprintf');
      @svn_cmdline_fputs := GetProcAddress(SvnSubrLib, 'svn_cmdline_fputs');
      @svn_cmdline_handle_exit_error := GetProcAddress(SvnSubrLib, 'svn_cmdline_handle_exit_error');
      @svn_cmdline_init := GetProcAddress(SvnSubrLib, 'svn_cmdline_init');
      @svn_cmdline_output_encoding := GetProcAddress(SvnSubrLib, 'svn_cmdline_output_encoding');
      @svn_cmdline_path_local_style_from_utf8 := GetProcAddress(SvnSubrLib, 'svn_cmdline_path_local_style_from_utf8');
      @svn_cmdline_printf := GetProcAddress(SvnSubrLib, 'svn_cmdline_printf');
      @svn_cmdline_prompt_user := GetProcAddress(SvnSubrLib, 'svn_cmdline_prompt_user');
      @svn_cmdline_prompt_user2 := GetProcAddress(SvnSubrLib, 'svn_cmdline_prompt_user2');
      @svn_cmdline_setup_auth_baton := GetProcAddress(SvnSubrLib, 'svn_cmdline_setup_auth_baton');
      @svn_commit_info_dup := GetProcAddress(SvnSubrLib, 'svn_commit_info_dup');
      @svn_compat_log_revprops_clear := GetProcAddress(SvnSubrLib, 'svn_compat_log_revprops_clear');
      @svn_compat_log_revprops_in := GetProcAddress(SvnSubrLib, 'svn_compat_log_revprops_in');
      @svn_compat_log_revprops_out := GetProcAddress(SvnSubrLib, 'svn_compat_log_revprops_out');
      @svn_compat_wrap_commit_callback := GetProcAddress(SvnSubrLib, 'svn_compat_wrap_commit_callback');
      @svn_compat_wrap_log_receiver := GetProcAddress(SvnSubrLib, 'svn_compat_wrap_log_receiver');
      @svn_config_ensure := GetProcAddress(SvnSubrLib, 'svn_config_ensure');
      @svn_config_enumerate := GetProcAddress(SvnSubrLib, 'svn_config_enumerate');
      @svn_config_enumerate2 := GetProcAddress(SvnSubrLib, 'svn_config_enumerate2');
      @svn_config_enumerate_sections := GetProcAddress(SvnSubrLib, 'svn_config_enumerate_sections');
      @svn_config_enumerate_sections2 := GetProcAddress(SvnSubrLib, 'svn_config_enumerate_sections2');
      @svn_config_find_group := GetProcAddress(SvnSubrLib, 'svn_config_find_group');
      @svn_config_get := GetProcAddress(SvnSubrLib, 'svn_config_get');
      @svn_config_get_bool := GetProcAddress(SvnSubrLib, 'svn_config_get_bool');
      @svn_config_get_config := GetProcAddress(SvnSubrLib, 'svn_config_get_config');
      @svn_config_get_server_setting := GetProcAddress(SvnSubrLib, 'svn_config_get_server_setting');
      @svn_config_get_server_setting_int := GetProcAddress(SvnSubrLib, 'svn_config_get_server_setting_int');
      @svn_config_has_section := GetProcAddress(SvnSubrLib, 'svn_config_has_section');
      @svn_config_merge := GetProcAddress(SvnSubrLib, 'svn_config_merge');
      @svn_config_read := GetProcAddress(SvnSubrLib, 'svn_config_read');
      @svn_config_read_auth_data := GetProcAddress(SvnSubrLib, 'svn_config_read_auth_data');
      @svn_config_set := GetProcAddress(SvnSubrLib, 'svn_config_set');
      @svn_config_set_bool := GetProcAddress(SvnSubrLib, 'svn_config_set_bool');
      @svn_config_write_auth_data := GetProcAddress(SvnSubrLib, 'svn_config_write_auth_data');
      @svn_create_commit_info := GetProcAddress(SvnSubrLib, 'svn_create_commit_info');
      @svn_cstring_casecmp := GetProcAddress(SvnSubrLib, 'svn_cstring_casecmp');
      @svn_cstring_count_newlines := GetProcAddress(SvnSubrLib, 'svn_cstring_count_newlines');
      @svn_cstring_join := GetProcAddress(SvnSubrLib, 'svn_cstring_join');
      @svn_cstring_match_glob_list := GetProcAddress(SvnSubrLib, 'svn_cstring_match_glob_list');
      @svn_cstring_split := GetProcAddress(SvnSubrLib, 'svn_cstring_split');
      @svn_cstring_split_append := GetProcAddress(SvnSubrLib, 'svn_cstring_split_append');
      @svn_ctype_casecmp := GetProcAddress(SvnSubrLib, 'svn_ctype_casecmp');
      @svn_ctype_table := GetProcAddress(SvnSubrLib, 'svn_ctype_table');
      @svn_depth_from_word := GetProcAddress(SvnSubrLib, 'svn_depth_from_word');
      @svn_depth_to_word := GetProcAddress(SvnSubrLib, 'svn_depth_to_word');
      @svn_dirent_dup := GetProcAddress(SvnSubrLib, 'svn_dirent_dup');
      @svn_dirent_is_root := GetProcAddress(SvnSubrLib, 'svn_dirent_is_root');
      @svn_dso_initialize := GetProcAddress(SvnSubrLib, 'svn_dso_initialize');
      @svn_dso_load := GetProcAddress(SvnSubrLib, 'svn_dso_load');
      @svn_err_best_message := GetProcAddress(SvnSubrLib, 'svn_err_best_message');
      @svn_error__locate := GetProcAddress(SvnSubrLib, 'svn_error__locate');
      @svn_error_clear := GetProcAddress(SvnSubrLib, 'svn_error_clear');
      @svn_error_compose := GetProcAddress(SvnSubrLib, 'svn_error_compose');
      @svn_error_create := GetProcAddress(SvnSubrLib, 'svn_error_create');
      @svn_error_createf := GetProcAddress(SvnSubrLib, 'svn_error_createf');
      @svn_error_dup := GetProcAddress(SvnSubrLib, 'svn_error_dup');
      @svn_error_quick_wrap := GetProcAddress(SvnSubrLib, 'svn_error_quick_wrap');
      @svn_error_root_cause := GetProcAddress(SvnSubrLib, 'svn_error_root_cause');
      @svn_error_wrap_apr := GetProcAddress(SvnSubrLib, 'svn_error_wrap_apr');
      @svn_handle_error := GetProcAddress(SvnSubrLib, 'svn_handle_error');
      @svn_handle_error2 := GetProcAddress(SvnSubrLib, 'svn_handle_error2');
      @svn_handle_warning := GetProcAddress(SvnSubrLib, 'svn_handle_warning');
      @svn_handle_warning2 := GetProcAddress(SvnSubrLib, 'svn_handle_warning2');
      @svn_hash__clear := GetProcAddress(SvnSubrLib, 'svn_hash__clear');
      @svn_hash_diff := GetProcAddress(SvnSubrLib, 'svn_hash_diff');
      @svn_hash_from_cstring_keys := GetProcAddress(SvnSubrLib, 'svn_hash_from_cstring_keys');
      @svn_hash_keys := GetProcAddress(SvnSubrLib, 'svn_hash_keys');
      @svn_hash_read := GetProcAddress(SvnSubrLib, 'svn_hash_read');
      @svn_hash_read2 := GetProcAddress(SvnSubrLib, 'svn_hash_read2');
      @svn_hash_read_incremental := GetProcAddress(SvnSubrLib, 'svn_hash_read_incremental');
      @svn_hash_write := GetProcAddress(SvnSubrLib, 'svn_hash_write');
      @svn_hash_write2 := GetProcAddress(SvnSubrLib, 'svn_hash_write2');
      @svn_hash_write_incremental := GetProcAddress(SvnSubrLib, 'svn_hash_write_incremental');
      @svn_inheritance_from_word := GetProcAddress(SvnSubrLib, 'svn_inheritance_from_word');
      @svn_inheritance_to_word := GetProcAddress(SvnSubrLib, 'svn_inheritance_to_word');
      @svn_io_append_file := GetProcAddress(SvnSubrLib, 'svn_io_append_file');
      @svn_io_check_path := GetProcAddress(SvnSubrLib, 'svn_io_check_path');
      @svn_io_check_resolved_path := GetProcAddress(SvnSubrLib, 'svn_io_check_resolved_path');
      @svn_io_check_special_path := GetProcAddress(SvnSubrLib, 'svn_io_check_special_path');
      @svn_io_copy_dir_recursively := GetProcAddress(SvnSubrLib, 'svn_io_copy_dir_recursively');
      @svn_io_copy_file := GetProcAddress(SvnSubrLib, 'svn_io_copy_file');
      @svn_io_copy_link := GetProcAddress(SvnSubrLib, 'svn_io_copy_link');
      @svn_io_create_unique_link := GetProcAddress(SvnSubrLib, 'svn_io_create_unique_link');
      @svn_io_detect_mimetype := GetProcAddress(SvnSubrLib, 'svn_io_detect_mimetype');
      @svn_io_detect_mimetype2 := GetProcAddress(SvnSubrLib, 'svn_io_detect_mimetype2');
      @svn_io_dir_empty := GetProcAddress(SvnSubrLib, 'svn_io_dir_empty');
      @svn_io_dir_file_copy := GetProcAddress(SvnSubrLib, 'svn_io_dir_file_copy');
      @svn_io_dir_make := GetProcAddress(SvnSubrLib, 'svn_io_dir_make');
      @svn_io_dir_make_hidden := GetProcAddress(SvnSubrLib, 'svn_io_dir_make_hidden');
      @svn_io_dir_make_sgid := GetProcAddress(SvnSubrLib, 'svn_io_dir_make_sgid');
      @svn_io_dir_open := GetProcAddress(SvnSubrLib, 'svn_io_dir_open');
      @svn_io_dir_read := GetProcAddress(SvnSubrLib, 'svn_io_dir_read');
      @svn_io_dir_remove_nonrecursive := GetProcAddress(SvnSubrLib, 'svn_io_dir_remove_nonrecursive');
      @svn_io_dir_walk := GetProcAddress(SvnSubrLib, 'svn_io_dir_walk');
      @svn_io_file_affected_time := GetProcAddress(SvnSubrLib, 'svn_io_file_affected_time');
      @svn_io_file_checksum := GetProcAddress(SvnSubrLib, 'svn_io_file_checksum');
      @svn_io_file_close := GetProcAddress(SvnSubrLib, 'svn_io_file_close');
      @svn_io_file_create := GetProcAddress(SvnSubrLib, 'svn_io_file_create');
      @svn_io_file_flush_to_disk := GetProcAddress(SvnSubrLib, 'svn_io_file_flush_to_disk');
      @svn_io_file_getc := GetProcAddress(SvnSubrLib, 'svn_io_file_getc');
      @svn_io_file_info_get := GetProcAddress(SvnSubrLib, 'svn_io_file_info_get');
      @svn_io_file_lock := GetProcAddress(SvnSubrLib, 'svn_io_file_lock');
      @svn_io_file_lock2 := GetProcAddress(SvnSubrLib, 'svn_io_file_lock2');
      @svn_io_file_move := GetProcAddress(SvnSubrLib, 'svn_io_file_move');
      @svn_io_file_open := GetProcAddress(SvnSubrLib, 'svn_io_file_open');
      @svn_io_file_read := GetProcAddress(SvnSubrLib, 'svn_io_file_read');
      @svn_io_file_read_full := GetProcAddress(SvnSubrLib, 'svn_io_file_read_full');
      @svn_io_file_rename := GetProcAddress(SvnSubrLib, 'svn_io_file_rename');
      @svn_io_file_seek := GetProcAddress(SvnSubrLib, 'svn_io_file_seek');
      @svn_io_file_write := GetProcAddress(SvnSubrLib, 'svn_io_file_write');
      @svn_io_file_write_full := GetProcAddress(SvnSubrLib, 'svn_io_file_write_full');
      @svn_io_files_contents_same_p := GetProcAddress(SvnSubrLib, 'svn_io_files_contents_same_p');
      @svn_io_filesizes_different_p := GetProcAddress(SvnSubrLib, 'svn_io_filesizes_different_p');
      @svn_io_get_dir_filenames := GetProcAddress(SvnSubrLib, 'svn_io_get_dir_filenames');
      @svn_io_get_dirents := GetProcAddress(SvnSubrLib, 'svn_io_get_dirents');
      @svn_io_get_dirents2 := GetProcAddress(SvnSubrLib, 'svn_io_get_dirents2');
      @svn_io_is_file_executable := GetProcAddress(SvnSubrLib, 'svn_io_is_file_executable');
      @svn_io_make_dir_recursively := GetProcAddress(SvnSubrLib, 'svn_io_make_dir_recursively');
      @svn_io_open_unique_file := GetProcAddress(SvnSubrLib, 'svn_io_open_unique_file');
      @svn_io_open_unique_file2 := GetProcAddress(SvnSubrLib, 'svn_io_open_unique_file2');
      @svn_io_parse_mimetypes_file := GetProcAddress(SvnSubrLib, 'svn_io_parse_mimetypes_file');
      @svn_io_read_length_line := GetProcAddress(SvnSubrLib, 'svn_io_read_length_line');
      @svn_io_read_link := GetProcAddress(SvnSubrLib, 'svn_io_read_link');
      @svn_io_read_version_file := GetProcAddress(SvnSubrLib, 'svn_io_read_version_file');
      @svn_io_remove_dir := GetProcAddress(SvnSubrLib, 'svn_io_remove_dir');
      @svn_io_remove_dir2 := GetProcAddress(SvnSubrLib, 'svn_io_remove_dir2');
      @svn_io_remove_file := GetProcAddress(SvnSubrLib, 'svn_io_remove_file');
      @svn_io_run_cmd := GetProcAddress(SvnSubrLib, 'svn_io_run_cmd');
      @svn_io_run_diff := GetProcAddress(SvnSubrLib, 'svn_io_run_diff');
      @svn_io_run_diff3 := GetProcAddress(SvnSubrLib, 'svn_io_run_diff3');
      @svn_io_run_diff3_2 := GetProcAddress(SvnSubrLib, 'svn_io_run_diff3_2');
      @svn_io_set_file_affected_time := GetProcAddress(SvnSubrLib, 'svn_io_set_file_affected_time');
      @svn_io_set_file_executable := GetProcAddress(SvnSubrLib, 'svn_io_set_file_executable');
      @svn_io_set_file_read_only := GetProcAddress(SvnSubrLib, 'svn_io_set_file_read_only');
      @svn_io_set_file_read_write := GetProcAddress(SvnSubrLib, 'svn_io_set_file_read_write');
      @svn_io_set_file_read_write_carefully := GetProcAddress(SvnSubrLib, 'svn_io_set_file_read_write_carefully');
      @svn_io_start_cmd := GetProcAddress(SvnSubrLib, 'svn_io_start_cmd');
      @svn_io_stat := GetProcAddress(SvnSubrLib, 'svn_io_stat');
      @svn_io_temp_dir := GetProcAddress(SvnSubrLib, 'svn_io_temp_dir');
      @svn_io_wait_for_cmd := GetProcAddress(SvnSubrLib, 'svn_io_wait_for_cmd');
      @svn_io_write_version_file := GetProcAddress(SvnSubrLib, 'svn_io_write_version_file');
      @svn_iter__break := GetProcAddress(SvnSubrLib, 'svn_iter__break');
      @svn_iter_apr_array := GetProcAddress(SvnSubrLib, 'svn_iter_apr_array');
      @svn_iter_apr_hash := GetProcAddress(SvnSubrLib, 'svn_iter_apr_hash');
      @svn_location_segment_dup := GetProcAddress(SvnSubrLib, 'svn_location_segment_dup');
      @svn_lock_create := GetProcAddress(SvnSubrLib, 'svn_lock_create');
      @svn_lock_dup := GetProcAddress(SvnSubrLib, 'svn_lock_dup');
      @svn_log_changed_path_dup := GetProcAddress(SvnSubrLib, 'svn_log_changed_path_dup');
      @svn_log_entry_create := GetProcAddress(SvnSubrLib, 'svn_log_entry_create');
      @svn_md5_digest_to_cstring := GetProcAddress(SvnSubrLib, 'svn_md5_digest_to_cstring');
      @svn_md5_digest_to_cstring_display := GetProcAddress(SvnSubrLib, 'svn_md5_digest_to_cstring_display');
      @svn_md5_digests_match := GetProcAddress(SvnSubrLib, 'svn_md5_digests_match');
      @svn_md5_empty_string_digest := GetProcAddress(SvnSubrLib, 'svn_md5_empty_string_digest');
      @svn_merge_range_contains_rev := GetProcAddress(SvnSubrLib, 'svn_merge_range_contains_rev');
      @svn_merge_range_dup := GetProcAddress(SvnSubrLib, 'svn_merge_range_dup');
      @svn_mergeinfo_diff := GetProcAddress(SvnSubrLib, 'svn_mergeinfo_diff');
      @svn_mergeinfo_dup := GetProcAddress(SvnSubrLib, 'svn_mergeinfo_dup');
      @svn_mergeinfo_inheritable := GetProcAddress(SvnSubrLib, 'svn_mergeinfo_inheritable');
      @svn_mergeinfo_intersect := GetProcAddress(SvnSubrLib, 'svn_mergeinfo_intersect');
      @svn_mergeinfo_merge := GetProcAddress(SvnSubrLib, 'svn_mergeinfo_merge');
      @svn_mergeinfo_parse := GetProcAddress(SvnSubrLib, 'svn_mergeinfo_parse');
      @svn_mergeinfo_remove := GetProcAddress(SvnSubrLib, 'svn_mergeinfo_remove');
      @svn_mergeinfo_sort := GetProcAddress(SvnSubrLib, 'svn_mergeinfo_sort');
      @svn_mergeinfo_to_string := GetProcAddress(SvnSubrLib, 'svn_mergeinfo_to_string');
      @svn_mime_type_is_binary := GetProcAddress(SvnSubrLib, 'svn_mime_type_is_binary');
      @svn_mime_type_validate := GetProcAddress(SvnSubrLib, 'svn_mime_type_validate');
      @svn_nls_init := GetProcAddress(SvnSubrLib, 'svn_nls_init');
      @svn_opt_args_to_target_array := GetProcAddress(SvnSubrLib, 'svn_opt_args_to_target_array');
      @svn_opt_args_to_target_array2 := GetProcAddress(SvnSubrLib, 'svn_opt_args_to_target_array2');
      @svn_opt_args_to_target_array3 := GetProcAddress(SvnSubrLib, 'svn_opt_args_to_target_array3');
      @svn_opt_format_option := GetProcAddress(SvnSubrLib, 'svn_opt_format_option');
      @svn_opt_get_canonical_subcommand := GetProcAddress(SvnSubrLib, 'svn_opt_get_canonical_subcommand');
      @svn_opt_get_canonical_subcommand2 := GetProcAddress(SvnSubrLib, 'svn_opt_get_canonical_subcommand2');
      @svn_opt_get_option_from_code := GetProcAddress(SvnSubrLib, 'svn_opt_get_option_from_code');
      @svn_opt_get_option_from_code2 := GetProcAddress(SvnSubrLib, 'svn_opt_get_option_from_code2');
      @svn_opt_parse_all_args := GetProcAddress(SvnSubrLib, 'svn_opt_parse_all_args');
      @svn_opt_parse_num_args := GetProcAddress(SvnSubrLib, 'svn_opt_parse_num_args');
      @svn_opt_parse_path := GetProcAddress(SvnSubrLib, 'svn_opt_parse_path');
      @svn_opt_parse_revision := GetProcAddress(SvnSubrLib, 'svn_opt_parse_revision');
      @svn_opt_parse_revision_to_range := GetProcAddress(SvnSubrLib, 'svn_opt_parse_revision_to_range');
      @svn_opt_print_generic_help := GetProcAddress(SvnSubrLib, 'svn_opt_print_generic_help');
      @svn_opt_print_generic_help2 := GetProcAddress(SvnSubrLib, 'svn_opt_print_generic_help2');
      @svn_opt_print_help := GetProcAddress(SvnSubrLib, 'svn_opt_print_help');
      @svn_opt_print_help2 := GetProcAddress(SvnSubrLib, 'svn_opt_print_help2');
      @svn_opt_print_help3 := GetProcAddress(SvnSubrLib, 'svn_opt_print_help3');
      @svn_opt_push_implicit_dot_target := GetProcAddress(SvnSubrLib, 'svn_opt_push_implicit_dot_target');
      @svn_opt_resolve_revisions := GetProcAddress(SvnSubrLib, 'svn_opt_resolve_revisions');
      @svn_opt_subcommand_help := GetProcAddress(SvnSubrLib, 'svn_opt_subcommand_help');
      @svn_opt_subcommand_help2 := GetProcAddress(SvnSubrLib, 'svn_opt_subcommand_help2');
      @svn_opt_subcommand_help3 := GetProcAddress(SvnSubrLib, 'svn_opt_subcommand_help3');
      @svn_opt_subcommand_takes_option := GetProcAddress(SvnSubrLib, 'svn_opt_subcommand_takes_option');
      @svn_opt_subcommand_takes_option2 := GetProcAddress(SvnSubrLib, 'svn_opt_subcommand_takes_option2');
      @svn_opt_subcommand_takes_option3 := GetProcAddress(SvnSubrLib, 'svn_opt_subcommand_takes_option3');
      @svn_parse_date := GetProcAddress(SvnSubrLib, 'svn_parse_date');
      @svn_path_add_component := GetProcAddress(SvnSubrLib, 'svn_path_add_component');
      @svn_path_basename := GetProcAddress(SvnSubrLib, 'svn_path_basename');
      @svn_path_canonicalize := GetProcAddress(SvnSubrLib, 'svn_path_canonicalize');
      @svn_path_check_valid := GetProcAddress(SvnSubrLib, 'svn_path_check_valid');
      @svn_path_compare_paths := GetProcAddress(SvnSubrLib, 'svn_path_compare_paths');
      @svn_path_component_count := GetProcAddress(SvnSubrLib, 'svn_path_component_count');
      @svn_path_compose := GetProcAddress(SvnSubrLib, 'svn_path_compose');
      @svn_path_condense_targets := GetProcAddress(SvnSubrLib, 'svn_path_condense_targets');
      @svn_path_cstring_from_utf8 := GetProcAddress(SvnSubrLib, 'svn_path_cstring_from_utf8');
      @svn_path_cstring_to_utf8 := GetProcAddress(SvnSubrLib, 'svn_path_cstring_to_utf8');
      @svn_path_decompose := GetProcAddress(SvnSubrLib, 'svn_path_decompose');
      @svn_path_dirname := GetProcAddress(SvnSubrLib, 'svn_path_dirname');
      @svn_path_get_absolute := GetProcAddress(SvnSubrLib, 'svn_path_get_absolute');
      @svn_path_get_longest_ancestor := GetProcAddress(SvnSubrLib, 'svn_path_get_longest_ancestor');
      @svn_path_internal_style := GetProcAddress(SvnSubrLib, 'svn_path_internal_style');
      @svn_path_is_ancestor := GetProcAddress(SvnSubrLib, 'svn_path_is_ancestor');
      @svn_path_is_backpath_present := GetProcAddress(SvnSubrLib, 'svn_path_is_backpath_present');
      @svn_path_is_canonical := GetProcAddress(SvnSubrLib, 'svn_path_is_canonical');
      @svn_path_is_child := GetProcAddress(SvnSubrLib, 'svn_path_is_child');
      @svn_path_is_empty := GetProcAddress(SvnSubrLib, 'svn_path_is_empty');
      @svn_path_is_single_path_component := GetProcAddress(SvnSubrLib, 'svn_path_is_single_path_component');
      @svn_path_is_uri_safe := GetProcAddress(SvnSubrLib, 'svn_path_is_uri_safe');
      @svn_path_is_url := GetProcAddress(SvnSubrLib, 'svn_path_is_url');
      @svn_path_join := GetProcAddress(SvnSubrLib, 'svn_path_join');
      @svn_path_join_many := GetProcAddress(SvnSubrLib, 'svn_path_join_many');
      @svn_path_local_style := GetProcAddress(SvnSubrLib, 'svn_path_local_style');
      @svn_path_remove_component := GetProcAddress(SvnSubrLib, 'svn_path_remove_component');
      @svn_path_remove_components := GetProcAddress(SvnSubrLib, 'svn_path_remove_components');
      @svn_path_remove_redundancies := GetProcAddress(SvnSubrLib, 'svn_path_remove_redundancies');
      @svn_path_split := GetProcAddress(SvnSubrLib, 'svn_path_split');
      @svn_path_split_if_file := GetProcAddress(SvnSubrLib, 'svn_path_split_if_file');
      @svn_path_splitext := GetProcAddress(SvnSubrLib, 'svn_path_splitext');
      @svn_path_uri_autoescape := GetProcAddress(SvnSubrLib, 'svn_path_uri_autoescape');
      @svn_path_uri_decode := GetProcAddress(SvnSubrLib, 'svn_path_uri_decode');
      @svn_path_uri_encode := GetProcAddress(SvnSubrLib, 'svn_path_uri_encode');
      @svn_path_uri_from_iri := GetProcAddress(SvnSubrLib, 'svn_path_uri_from_iri');
      @svn_path_url_add_component := GetProcAddress(SvnSubrLib, 'svn_path_url_add_component');
      @svn_pool_create_ex := GetProcAddress(SvnSubrLib, 'svn_pool_create_ex');
      @svn_pool_create_ex_debug := GetProcAddress(SvnSubrLib, 'svn_pool_create_ex_debug');
      @svn_prop_array_dup := GetProcAddress(SvnSubrLib, 'svn_prop_array_dup');
      @svn_prop_diffs := GetProcAddress(SvnSubrLib, 'svn_prop_diffs');
      @svn_prop_dup := GetProcAddress(SvnSubrLib, 'svn_prop_dup');
      @svn_prop_has_svn_prop := GetProcAddress(SvnSubrLib, 'svn_prop_has_svn_prop');
      @svn_prop_hash_to_array := GetProcAddress(SvnSubrLib, 'svn_prop_hash_to_array');
      @svn_prop_is_boolean := GetProcAddress(SvnSubrLib, 'svn_prop_is_boolean');
      @svn_prop_is_svn_prop := GetProcAddress(SvnSubrLib, 'svn_prop_is_svn_prop');
      @svn_prop_name_is_valid := GetProcAddress(SvnSubrLib, 'svn_prop_name_is_valid');
      @svn_prop_needs_translation := GetProcAddress(SvnSubrLib, 'svn_prop_needs_translation');
      @svn_property_kind := GetProcAddress(SvnSubrLib, 'svn_property_kind');
      @svn_quoprint_decode := GetProcAddress(SvnSubrLib, 'svn_quoprint_decode');
      @svn_quoprint_decode_string := GetProcAddress(SvnSubrLib, 'svn_quoprint_decode_string');
      @svn_quoprint_encode := GetProcAddress(SvnSubrLib, 'svn_quoprint_encode');
      @svn_quoprint_encode_string := GetProcAddress(SvnSubrLib, 'svn_quoprint_encode_string');
      @svn_rangelist_diff := GetProcAddress(SvnSubrLib, 'svn_rangelist_diff');
      @svn_rangelist_dup := GetProcAddress(SvnSubrLib, 'svn_rangelist_dup');
      @svn_rangelist_inheritable := GetProcAddress(SvnSubrLib, 'svn_rangelist_inheritable');
      @svn_rangelist_intersect := GetProcAddress(SvnSubrLib, 'svn_rangelist_intersect');
      @svn_rangelist_merge := GetProcAddress(SvnSubrLib, 'svn_rangelist_merge');
      @svn_rangelist_remove := GetProcAddress(SvnSubrLib, 'svn_rangelist_remove');
      @svn_rangelist_reverse := GetProcAddress(SvnSubrLib, 'svn_rangelist_reverse');
      @svn_rangelist_to_string := GetProcAddress(SvnSubrLib, 'svn_rangelist_to_string');
      @svn_revnum_parse := GetProcAddress(SvnSubrLib, 'svn_revnum_parse');
      @svn_sleep_for_timestamps := GetProcAddress(SvnSubrLib, 'svn_sleep_for_timestamps');
      @svn_sort__hash := GetProcAddress(SvnSubrLib, 'svn_sort__hash');
      @svn_sort_compare_items_as_paths := GetProcAddress(SvnSubrLib, 'svn_sort_compare_items_as_paths');
      @svn_sort_compare_items_lexically := GetProcAddress(SvnSubrLib, 'svn_sort_compare_items_lexically');
      @svn_sort_compare_paths := GetProcAddress(SvnSubrLib, 'svn_sort_compare_paths');
      @svn_sort_compare_ranges := GetProcAddress(SvnSubrLib, 'svn_sort_compare_ranges');
      @svn_sort_compare_revisions := GetProcAddress(SvnSubrLib, 'svn_sort_compare_revisions');
      @svn_stream_checksummed := GetProcAddress(SvnSubrLib, 'svn_stream_checksummed');
      @svn_stream_close := GetProcAddress(SvnSubrLib, 'svn_stream_close');
      @svn_stream_compressed := GetProcAddress(SvnSubrLib, 'svn_stream_compressed');
      @svn_stream_contents_same := GetProcAddress(SvnSubrLib, 'svn_stream_contents_same');
      @svn_stream_copy := GetProcAddress(SvnSubrLib, 'svn_stream_copy');
      @svn_stream_copy2 := GetProcAddress(SvnSubrLib, 'svn_stream_copy2');
      @svn_stream_create := GetProcAddress(SvnSubrLib, 'svn_stream_create');
      @svn_stream_disown := GetProcAddress(SvnSubrLib, 'svn_stream_disown');
      @svn_stream_empty := GetProcAddress(SvnSubrLib, 'svn_stream_empty');
      @svn_stream_for_stdout := GetProcAddress(SvnSubrLib, 'svn_stream_for_stdout');
      @svn_stream_from_aprfile := GetProcAddress(SvnSubrLib, 'svn_stream_from_aprfile');
      @svn_stream_from_aprfile2 := GetProcAddress(SvnSubrLib, 'svn_stream_from_aprfile2');
      @svn_stream_from_stringbuf := GetProcAddress(SvnSubrLib, 'svn_stream_from_stringbuf');
      @svn_stream_printf := GetProcAddress(SvnSubrLib, 'svn_stream_printf');
      @svn_stream_printf_from_utf8 := GetProcAddress(SvnSubrLib, 'svn_stream_printf_from_utf8');
      @svn_stream_read := GetProcAddress(SvnSubrLib, 'svn_stream_read');
      @svn_stream_readline := GetProcAddress(SvnSubrLib, 'svn_stream_readline');
      @svn_stream_set_baton := GetProcAddress(SvnSubrLib, 'svn_stream_set_baton');
      @svn_stream_set_close := GetProcAddress(SvnSubrLib, 'svn_stream_set_close');
      @svn_stream_set_read := GetProcAddress(SvnSubrLib, 'svn_stream_set_read');
      @svn_stream_set_write := GetProcAddress(SvnSubrLib, 'svn_stream_set_write');
      @svn_stream_write := GetProcAddress(SvnSubrLib, 'svn_stream_write');
      @svn_strerror := GetProcAddress(SvnSubrLib, 'svn_strerror');
      @svn_string_compare := GetProcAddress(SvnSubrLib, 'svn_string_compare');
      @svn_string_compare_stringbuf := GetProcAddress(SvnSubrLib, 'svn_string_compare_stringbuf');
      @svn_string_create := GetProcAddress(SvnSubrLib, 'svn_string_create');
      @svn_string_create_from_buf := GetProcAddress(SvnSubrLib, 'svn_string_create_from_buf');
      @svn_string_createf := GetProcAddress(SvnSubrLib, 'svn_string_createf');
      @svn_string_createv := GetProcAddress(SvnSubrLib, 'svn_string_createv');
      @svn_string_dup := GetProcAddress(SvnSubrLib, 'svn_string_dup');
      @svn_string_find_char_backward := GetProcAddress(SvnSubrLib, 'svn_string_find_char_backward');
      @svn_string_first_non_whitespace := GetProcAddress(SvnSubrLib, 'svn_string_first_non_whitespace');
      @svn_string_isempty := GetProcAddress(SvnSubrLib, 'svn_string_isempty');
      @svn_string_ncreate := GetProcAddress(SvnSubrLib, 'svn_string_ncreate');
      @svn_stringbuf_appendbytes := GetProcAddress(SvnSubrLib, 'svn_stringbuf_appendbytes');
      @svn_stringbuf_appendcstr := GetProcAddress(SvnSubrLib, 'svn_stringbuf_appendcstr');
      @svn_stringbuf_appendstr := GetProcAddress(SvnSubrLib, 'svn_stringbuf_appendstr');
      @svn_stringbuf_chop := GetProcAddress(SvnSubrLib, 'svn_stringbuf_chop');
      @svn_stringbuf_compare := GetProcAddress(SvnSubrLib, 'svn_stringbuf_compare');
      @svn_stringbuf_create := GetProcAddress(SvnSubrLib, 'svn_stringbuf_create');
      @svn_stringbuf_create_from_string := GetProcAddress(SvnSubrLib, 'svn_stringbuf_create_from_string');
      @svn_stringbuf_createf := GetProcAddress(SvnSubrLib, 'svn_stringbuf_createf');
      @svn_stringbuf_createv := GetProcAddress(SvnSubrLib, 'svn_stringbuf_createv');
      @svn_stringbuf_dup := GetProcAddress(SvnSubrLib, 'svn_stringbuf_dup');
      @svn_stringbuf_ensure := GetProcAddress(SvnSubrLib, 'svn_stringbuf_ensure');
      @svn_stringbuf_fillchar := GetProcAddress(SvnSubrLib, 'svn_stringbuf_fillchar');
      @svn_stringbuf_find_char_backward := GetProcAddress(SvnSubrLib, 'svn_stringbuf_find_char_backward');
      @svn_stringbuf_first_non_whitespace := GetProcAddress(SvnSubrLib, 'svn_stringbuf_first_non_whitespace');
      @svn_stringbuf_from_aprfile := GetProcAddress(SvnSubrLib, 'svn_stringbuf_from_aprfile');
      @svn_stringbuf_from_file := GetProcAddress(SvnSubrLib, 'svn_stringbuf_from_file');
      @svn_stringbuf_from_file2 := GetProcAddress(SvnSubrLib, 'svn_stringbuf_from_file2');
      @svn_stringbuf_isempty := GetProcAddress(SvnSubrLib, 'svn_stringbuf_isempty');
      @svn_stringbuf_ncreate := GetProcAddress(SvnSubrLib, 'svn_stringbuf_ncreate');
      @svn_stringbuf_set := GetProcAddress(SvnSubrLib, 'svn_stringbuf_set');
      @svn_stringbuf_setempty := GetProcAddress(SvnSubrLib, 'svn_stringbuf_setempty');
      @svn_stringbuf_strip_whitespace := GetProcAddress(SvnSubrLib, 'svn_stringbuf_strip_whitespace');
      @svn_subr_version := GetProcAddress(SvnSubrLib, 'svn_subr_version');
      @svn_subst_build_keywords := GetProcAddress(SvnSubrLib, 'svn_subst_build_keywords');
      @svn_subst_build_keywords2 := GetProcAddress(SvnSubrLib, 'svn_subst_build_keywords2');
      @svn_subst_copy_and_translate := GetProcAddress(SvnSubrLib, 'svn_subst_copy_and_translate');
      @svn_subst_copy_and_translate2 := GetProcAddress(SvnSubrLib, 'svn_subst_copy_and_translate2');
      @svn_subst_copy_and_translate3 := GetProcAddress(SvnSubrLib, 'svn_subst_copy_and_translate3');
      @svn_subst_detranslate_string := GetProcAddress(SvnSubrLib, 'svn_subst_detranslate_string');
      @svn_subst_eol_style_from_value := GetProcAddress(SvnSubrLib, 'svn_subst_eol_style_from_value');
      @svn_subst_keywords_differ := GetProcAddress(SvnSubrLib, 'svn_subst_keywords_differ');
      @svn_subst_keywords_differ2 := GetProcAddress(SvnSubrLib, 'svn_subst_keywords_differ2');
      @svn_subst_stream_detranslated := GetProcAddress(SvnSubrLib, 'svn_subst_stream_detranslated');
      @svn_subst_stream_from_specialfile := GetProcAddress(SvnSubrLib, 'svn_subst_stream_from_specialfile');
      @svn_subst_stream_translated := GetProcAddress(SvnSubrLib, 'svn_subst_stream_translated');
      @svn_subst_stream_translated_to_normal_form := GetProcAddress(SvnSubrLib, 'svn_subst_stream_translated_to_normal_form');
      @svn_subst_translate_cstring := GetProcAddress(SvnSubrLib, 'svn_subst_translate_cstring');
      @svn_subst_translate_cstring2 := GetProcAddress(SvnSubrLib, 'svn_subst_translate_cstring2');
      @svn_subst_translate_stream := GetProcAddress(SvnSubrLib, 'svn_subst_translate_stream');
      @svn_subst_translate_stream2 := GetProcAddress(SvnSubrLib, 'svn_subst_translate_stream2');
      @svn_subst_translate_stream3 := GetProcAddress(SvnSubrLib, 'svn_subst_translate_stream3');
      @svn_subst_translate_string := GetProcAddress(SvnSubrLib, 'svn_subst_translate_string');
      @svn_subst_translate_to_normal_form := GetProcAddress(SvnSubrLib, 'svn_subst_translate_to_normal_form');
      @svn_subst_translation_required := GetProcAddress(SvnSubrLib, 'svn_subst_translation_required');
      @svn_time_from_cstring := GetProcAddress(SvnSubrLib, 'svn_time_from_cstring');
      @svn_time_to_cstring := GetProcAddress(SvnSubrLib, 'svn_time_to_cstring');
      @svn_time_to_human_cstring := GetProcAddress(SvnSubrLib, 'svn_time_to_human_cstring');
      @svn_user_get_homedir := GetProcAddress(SvnSubrLib, 'svn_user_get_homedir');
      @svn_user_get_name := GetProcAddress(SvnSubrLib, 'svn_user_get_name');
      @svn_utf_cstring_from_utf8 := GetProcAddress(SvnSubrLib, 'svn_utf_cstring_from_utf8');
      @svn_utf_cstring_from_utf8_ex := GetProcAddress(SvnSubrLib, 'svn_utf_cstring_from_utf8_ex');
      @svn_utf_cstring_from_utf8_ex2 := GetProcAddress(SvnSubrLib, 'svn_utf_cstring_from_utf8_ex2');
      @svn_utf_cstring_from_utf8_fuzzy := GetProcAddress(SvnSubrLib, 'svn_utf_cstring_from_utf8_fuzzy');
      @svn_utf_cstring_from_utf8_string := GetProcAddress(SvnSubrLib, 'svn_utf_cstring_from_utf8_string');
      @svn_utf_cstring_from_utf8_stringbuf := GetProcAddress(SvnSubrLib, 'svn_utf_cstring_from_utf8_stringbuf');
      @svn_utf_cstring_to_utf8 := GetProcAddress(SvnSubrLib, 'svn_utf_cstring_to_utf8');
      @svn_utf_cstring_to_utf8_ex := GetProcAddress(SvnSubrLib, 'svn_utf_cstring_to_utf8_ex');
      @svn_utf_cstring_to_utf8_ex2 := GetProcAddress(SvnSubrLib, 'svn_utf_cstring_to_utf8_ex2');
      @svn_utf_initialize := GetProcAddress(SvnSubrLib, 'svn_utf_initialize');
      @svn_utf_string_from_utf8 := GetProcAddress(SvnSubrLib, 'svn_utf_string_from_utf8');
      @svn_utf_string_to_utf8 := GetProcAddress(SvnSubrLib, 'svn_utf_string_to_utf8');
      @svn_utf_stringbuf_from_utf8 := GetProcAddress(SvnSubrLib, 'svn_utf_stringbuf_from_utf8');
      @svn_utf_stringbuf_to_utf8 := GetProcAddress(SvnSubrLib, 'svn_utf_stringbuf_to_utf8');
      @svn_uuid_generate := GetProcAddress(SvnSubrLib, 'svn_uuid_generate');
      @svn_ver_check_list := GetProcAddress(SvnSubrLib, 'svn_ver_check_list');
      @svn_ver_compatible := GetProcAddress(SvnSubrLib, 'svn_ver_compatible');
      @svn_ver_equal := GetProcAddress(SvnSubrLib, 'svn_ver_equal');
      @svn_xml_ap_to_hash := GetProcAddress(SvnSubrLib, 'svn_xml_ap_to_hash');
      @svn_xml_escape_attr_cstring := GetProcAddress(SvnSubrLib, 'svn_xml_escape_attr_cstring');
      @svn_xml_escape_attr_string := GetProcAddress(SvnSubrLib, 'svn_xml_escape_attr_string');
      @svn_xml_escape_attr_stringbuf := GetProcAddress(SvnSubrLib, 'svn_xml_escape_attr_stringbuf');
      @svn_xml_escape_cdata_cstring := GetProcAddress(SvnSubrLib, 'svn_xml_escape_cdata_cstring');
      @svn_xml_escape_cdata_string := GetProcAddress(SvnSubrLib, 'svn_xml_escape_cdata_string');
      @svn_xml_escape_cdata_stringbuf := GetProcAddress(SvnSubrLib, 'svn_xml_escape_cdata_stringbuf');
      @svn_xml_free_parser := GetProcAddress(SvnSubrLib, 'svn_xml_free_parser');
      @svn_xml_fuzzy_escape := GetProcAddress(SvnSubrLib, 'svn_xml_fuzzy_escape');
      @svn_xml_get_attr_value := GetProcAddress(SvnSubrLib, 'svn_xml_get_attr_value');
      @svn_xml_hash_atts_overlaying := GetProcAddress(SvnSubrLib, 'svn_xml_hash_atts_overlaying');
      @svn_xml_hash_atts_preserving := GetProcAddress(SvnSubrLib, 'svn_xml_hash_atts_preserving');
      @svn_xml_is_xml_safe := GetProcAddress(SvnSubrLib, 'svn_xml_is_xml_safe');
      @svn_xml_make_att_hash := GetProcAddress(SvnSubrLib, 'svn_xml_make_att_hash');
      @svn_xml_make_close_tag := GetProcAddress(SvnSubrLib, 'svn_xml_make_close_tag');
      @svn_xml_make_header := GetProcAddress(SvnSubrLib, 'svn_xml_make_header');
      @svn_xml_make_open_tag := GetProcAddress(SvnSubrLib, 'svn_xml_make_open_tag');
      @svn_xml_make_open_tag_hash := GetProcAddress(SvnSubrLib, 'svn_xml_make_open_tag_hash');
      @svn_xml_make_open_tag_v := GetProcAddress(SvnSubrLib, 'svn_xml_make_open_tag_v');
      @svn_xml_make_parser := GetProcAddress(SvnSubrLib, 'svn_xml_make_parser');
      @svn_xml_parse := GetProcAddress(SvnSubrLib, 'svn_xml_parse');
      @svn_xml_signal_bailout := GetProcAddress(SvnSubrLib, 'svn_xml_signal_bailout');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function LoadSvnWcLib(const FileName: string = ''): Boolean;

var
  LibFileName: string;

begin
  Result := not SvnWcLibLoaded;

  if Result then
  begin
    if FileName = '' then
      LibFileName := 'libsvn_wc-1.dll'
    else
      LibFileName := FileName;

    SvnWcLib := LoadLibrary(PChar(LibFileName));
    Result := SvnWcLib <> 0;
    if not Result then
      SvnWcLib := INVALID_HANDLE_VALUE
    else
    begin
      @svn_wc_add := GetProcAddress(SvnWcLib, 'svn_wc_add');
      @svn_wc_add2 := GetProcAddress(SvnWcLib, 'svn_wc_add2');
      @svn_wc_add_lock := GetProcAddress(SvnWcLib, 'svn_wc_add_lock');
      @svn_wc_add_repos_file := GetProcAddress(SvnWcLib, 'svn_wc_add_repos_file');
      @svn_wc_add_repos_file2 := GetProcAddress(SvnWcLib, 'svn_wc_add_repos_file2');
      @svn_wc_adm_access_path := GetProcAddress(SvnWcLib, 'svn_wc_adm_access_path');
      @svn_wc_adm_access_pool := GetProcAddress(SvnWcLib, 'svn_wc_adm_access_pool');
      @svn_wc_adm_close := GetProcAddress(SvnWcLib, 'svn_wc_adm_close');
      @svn_wc_adm_locked := GetProcAddress(SvnWcLib, 'svn_wc_adm_locked');
      @svn_wc_adm_open := GetProcAddress(SvnWcLib, 'svn_wc_adm_open');
      @svn_wc_adm_open2 := GetProcAddress(SvnWcLib, 'svn_wc_adm_open2');
      @svn_wc_adm_open3 := GetProcAddress(SvnWcLib, 'svn_wc_adm_open3');
      @svn_wc_adm_open_anchor := GetProcAddress(SvnWcLib, 'svn_wc_adm_open_anchor');
      @svn_wc_adm_probe_open := GetProcAddress(SvnWcLib, 'svn_wc_adm_probe_open');
      @svn_wc_adm_probe_open2 := GetProcAddress(SvnWcLib, 'svn_wc_adm_probe_open2');
      @svn_wc_adm_probe_open3 := GetProcAddress(SvnWcLib, 'svn_wc_adm_probe_open3');
      @svn_wc_adm_probe_retrieve := GetProcAddress(SvnWcLib, 'svn_wc_adm_probe_retrieve');
      @svn_wc_adm_probe_try := GetProcAddress(SvnWcLib, 'svn_wc_adm_probe_try');
      @svn_wc_adm_probe_try2 := GetProcAddress(SvnWcLib, 'svn_wc_adm_probe_try2');
      @svn_wc_adm_probe_try3 := GetProcAddress(SvnWcLib, 'svn_wc_adm_probe_try3');
      @svn_wc_adm_retrieve := GetProcAddress(SvnWcLib, 'svn_wc_adm_retrieve');
      @svn_wc_canonicalize_svn_prop := GetProcAddress(SvnWcLib, 'svn_wc_canonicalize_svn_prop');
      @svn_wc_check_wc := GetProcAddress(SvnWcLib, 'svn_wc_check_wc');
      @svn_wc_cleanup := GetProcAddress(SvnWcLib, 'svn_wc_cleanup');
      @svn_wc_cleanup2 := GetProcAddress(SvnWcLib, 'svn_wc_cleanup2');
      @svn_wc_committed_queue_create := GetProcAddress(SvnWcLib, 'svn_wc_committed_queue_create');
      @svn_wc_conflicted_p := GetProcAddress(SvnWcLib, 'svn_wc_conflicted_p');
      @svn_wc_copy := GetProcAddress(SvnWcLib, 'svn_wc_copy');
      @svn_wc_copy2 := GetProcAddress(SvnWcLib, 'svn_wc_copy2');
      @svn_wc_crawl_revisions := GetProcAddress(SvnWcLib, 'svn_wc_crawl_revisions');
      @svn_wc_crawl_revisions2 := GetProcAddress(SvnWcLib, 'svn_wc_crawl_revisions2');
      @svn_wc_crawl_revisions3 := GetProcAddress(SvnWcLib, 'svn_wc_crawl_revisions3');
      @svn_wc_create_conflict_result := GetProcAddress(SvnWcLib, 'svn_wc_create_conflict_result');
      @svn_wc_create_notify := GetProcAddress(SvnWcLib, 'svn_wc_create_notify');
      @svn_wc_create_tmp_file := GetProcAddress(SvnWcLib, 'svn_wc_create_tmp_file');
      @svn_wc_create_tmp_file2 := GetProcAddress(SvnWcLib, 'svn_wc_create_tmp_file2');
      @svn_wc_delete := GetProcAddress(SvnWcLib, 'svn_wc_delete');
      @svn_wc_delete2 := GetProcAddress(SvnWcLib, 'svn_wc_delete2');
      @svn_wc_delete3 := GetProcAddress(SvnWcLib, 'svn_wc_delete3');
      @svn_wc_diff := GetProcAddress(SvnWcLib, 'svn_wc_diff');
      @svn_wc_diff2 := GetProcAddress(SvnWcLib, 'svn_wc_diff2');
      @svn_wc_diff3 := GetProcAddress(SvnWcLib, 'svn_wc_diff3');
      @svn_wc_diff4 := GetProcAddress(SvnWcLib, 'svn_wc_diff4');
      @svn_wc_dup_notify := GetProcAddress(SvnWcLib, 'svn_wc_dup_notify');
      @svn_wc_dup_status := GetProcAddress(SvnWcLib, 'svn_wc_dup_status');
      @svn_wc_dup_status2 := GetProcAddress(SvnWcLib, 'svn_wc_dup_status2');
      @svn_wc_edited_externals := GetProcAddress(SvnWcLib, 'svn_wc_edited_externals');
      @svn_wc_ensure_adm := GetProcAddress(SvnWcLib, 'svn_wc_ensure_adm');
      @svn_wc_ensure_adm2 := GetProcAddress(SvnWcLib, 'svn_wc_ensure_adm2');
      @svn_wc_ensure_adm3 := GetProcAddress(SvnWcLib, 'svn_wc_ensure_adm3');
      @svn_wc_entries_read := GetProcAddress(SvnWcLib, 'svn_wc_entries_read');
      @svn_wc_entry := GetProcAddress(SvnWcLib, 'svn_wc_entry');
      @svn_wc_entry_dup := GetProcAddress(SvnWcLib, 'svn_wc_entry_dup');
      @svn_wc_external_item2_dup := GetProcAddress(SvnWcLib, 'svn_wc_external_item2_dup');
      @svn_wc_external_item_create := GetProcAddress(SvnWcLib, 'svn_wc_external_item_create');
      @svn_wc_external_item_dup := GetProcAddress(SvnWcLib, 'svn_wc_external_item_dup');
      @svn_wc_get_actual_target := GetProcAddress(SvnWcLib, 'svn_wc_get_actual_target');
      @svn_wc_get_adm_dir := GetProcAddress(SvnWcLib, 'svn_wc_get_adm_dir');
      @svn_wc_get_ancestry := GetProcAddress(SvnWcLib, 'svn_wc_get_ancestry');
      @svn_wc_get_default_ignores := GetProcAddress(SvnWcLib, 'svn_wc_get_default_ignores');
      @svn_wc_get_diff_editor := GetProcAddress(SvnWcLib, 'svn_wc_get_diff_editor');
      @svn_wc_get_diff_editor2 := GetProcAddress(SvnWcLib, 'svn_wc_get_diff_editor2');
      @svn_wc_get_diff_editor3 := GetProcAddress(SvnWcLib, 'svn_wc_get_diff_editor3');
      @svn_wc_get_diff_editor4 := GetProcAddress(SvnWcLib, 'svn_wc_get_diff_editor4');
      @svn_wc_get_ignores := GetProcAddress(SvnWcLib, 'svn_wc_get_ignores');
      @svn_wc_get_pristine_copy_path := GetProcAddress(SvnWcLib, 'svn_wc_get_pristine_copy_path');
      @svn_wc_get_prop_diffs := GetProcAddress(SvnWcLib, 'svn_wc_get_prop_diffs');
      @svn_wc_get_status_editor := GetProcAddress(SvnWcLib, 'svn_wc_get_status_editor');
      @svn_wc_get_status_editor2 := GetProcAddress(SvnWcLib, 'svn_wc_get_status_editor2');
      @svn_wc_get_status_editor3 := GetProcAddress(SvnWcLib, 'svn_wc_get_status_editor3');
      @svn_wc_get_switch_editor := GetProcAddress(SvnWcLib, 'svn_wc_get_switch_editor');
      @svn_wc_get_switch_editor2 := GetProcAddress(SvnWcLib, 'svn_wc_get_switch_editor2');
      @svn_wc_get_switch_editor3 := GetProcAddress(SvnWcLib, 'svn_wc_get_switch_editor3');
      @svn_wc_get_update_editor := GetProcAddress(SvnWcLib, 'svn_wc_get_update_editor');
      @svn_wc_get_update_editor2 := GetProcAddress(SvnWcLib, 'svn_wc_get_update_editor2');
      @svn_wc_get_update_editor3 := GetProcAddress(SvnWcLib, 'svn_wc_get_update_editor3');
      @svn_wc_has_binary_prop := GetProcAddress(SvnWcLib, 'svn_wc_has_binary_prop');
      @svn_wc_init_traversal_info := GetProcAddress(SvnWcLib, 'svn_wc_init_traversal_info');
      @svn_wc_is_adm_dir := GetProcAddress(SvnWcLib, 'svn_wc_is_adm_dir');
      @svn_wc_is_entry_prop := GetProcAddress(SvnWcLib, 'svn_wc_is_entry_prop');
      @svn_wc_is_normal_prop := GetProcAddress(SvnWcLib, 'svn_wc_is_normal_prop');
      @svn_wc_is_wc_prop := GetProcAddress(SvnWcLib, 'svn_wc_is_wc_prop');
      @svn_wc_is_wc_root := GetProcAddress(SvnWcLib, 'svn_wc_is_wc_root');
      @svn_wc_locked := GetProcAddress(SvnWcLib, 'svn_wc_locked');
      @svn_wc_mark_missing_deleted := GetProcAddress(SvnWcLib, 'svn_wc_mark_missing_deleted');
      @svn_wc_match_ignore_list := GetProcAddress(SvnWcLib, 'svn_wc_match_ignore_list');
      @svn_wc_maybe_set_repos_root := GetProcAddress(SvnWcLib, 'svn_wc_maybe_set_repos_root');
      @svn_wc_merge := GetProcAddress(SvnWcLib, 'svn_wc_merge');
      @svn_wc_merge2 := GetProcAddress(SvnWcLib, 'svn_wc_merge2');
      @svn_wc_merge3 := GetProcAddress(SvnWcLib, 'svn_wc_merge3');
      @svn_wc_merge_prop_diffs := GetProcAddress(SvnWcLib, 'svn_wc_merge_prop_diffs');
      @svn_wc_merge_props := GetProcAddress(SvnWcLib, 'svn_wc_merge_props');
      @svn_wc_merge_props2 := GetProcAddress(SvnWcLib, 'svn_wc_merge_props2');
      @svn_wc_parse_externals_description := GetProcAddress(SvnWcLib, 'svn_wc_parse_externals_description');
      @svn_wc_parse_externals_description2 := GetProcAddress(SvnWcLib, 'svn_wc_parse_externals_description2');
      @svn_wc_parse_externals_description3 := GetProcAddress(SvnWcLib, 'svn_wc_parse_externals_description3');
      @svn_wc_process_committed := GetProcAddress(SvnWcLib, 'svn_wc_process_committed');
      @svn_wc_process_committed2 := GetProcAddress(SvnWcLib, 'svn_wc_process_committed2');
      @svn_wc_process_committed3 := GetProcAddress(SvnWcLib, 'svn_wc_process_committed3');
      @svn_wc_process_committed4 := GetProcAddress(SvnWcLib, 'svn_wc_process_committed4');
      @svn_wc_process_committed_queue := GetProcAddress(SvnWcLib, 'svn_wc_process_committed_queue');
      @svn_wc_prop_get := GetProcAddress(SvnWcLib, 'svn_wc_prop_get');
      @svn_wc_prop_list := GetProcAddress(SvnWcLib, 'svn_wc_prop_list');
      @svn_wc_prop_set := GetProcAddress(SvnWcLib, 'svn_wc_prop_set');
      @svn_wc_prop_set2 := GetProcAddress(SvnWcLib, 'svn_wc_prop_set2');
      @svn_wc_props_modified_p := GetProcAddress(SvnWcLib, 'svn_wc_props_modified_p');
      @svn_wc_queue_committed := GetProcAddress(SvnWcLib, 'svn_wc_queue_committed');
      @svn_wc_relocate := GetProcAddress(SvnWcLib, 'svn_wc_relocate');
      @svn_wc_relocate2 := GetProcAddress(SvnWcLib, 'svn_wc_relocate2');
      @svn_wc_relocate3 := GetProcAddress(SvnWcLib, 'svn_wc_relocate3');
      @svn_wc_remove_from_revision_control := GetProcAddress(SvnWcLib, 'svn_wc_remove_from_revision_control');
      @svn_wc_remove_lock := GetProcAddress(SvnWcLib, 'svn_wc_remove_lock');
      @svn_wc_resolved_conflict := GetProcAddress(SvnWcLib, 'svn_wc_resolved_conflict');
      @svn_wc_resolved_conflict2 := GetProcAddress(SvnWcLib, 'svn_wc_resolved_conflict2');
      @svn_wc_resolved_conflict3 := GetProcAddress(SvnWcLib, 'svn_wc_resolved_conflict3');
      @svn_wc_revert := GetProcAddress(SvnWcLib, 'svn_wc_revert');
      @svn_wc_revert2 := GetProcAddress(SvnWcLib, 'svn_wc_revert2');
      @svn_wc_revert3 := GetProcAddress(SvnWcLib, 'svn_wc_revert3');
      @svn_wc_revision_status := GetProcAddress(SvnWcLib, 'svn_wc_revision_status');
      @svn_wc_set_adm_dir := GetProcAddress(SvnWcLib, 'svn_wc_set_adm_dir');
      @svn_wc_set_changelist := GetProcAddress(SvnWcLib, 'svn_wc_set_changelist');
      @svn_wc_status := GetProcAddress(SvnWcLib, 'svn_wc_status');
      @svn_wc_status2 := GetProcAddress(SvnWcLib, 'svn_wc_status2');
      @svn_wc_status_set_repos_locks := GetProcAddress(SvnWcLib, 'svn_wc_status_set_repos_locks');
      @svn_wc_text_modified_p := GetProcAddress(SvnWcLib, 'svn_wc_text_modified_p');
      @svn_wc_translated_file := GetProcAddress(SvnWcLib, 'svn_wc_translated_file');
      @svn_wc_translated_file2 := GetProcAddress(SvnWcLib, 'svn_wc_translated_file2');
      @svn_wc_translated_stream := GetProcAddress(SvnWcLib, 'svn_wc_translated_stream');
      @svn_wc_transmit_prop_deltas := GetProcAddress(SvnWcLib, 'svn_wc_transmit_prop_deltas');
      @svn_wc_transmit_text_deltas := GetProcAddress(SvnWcLib, 'svn_wc_transmit_text_deltas');
      @svn_wc_transmit_text_deltas2 := GetProcAddress(SvnWcLib, 'svn_wc_transmit_text_deltas2');
      @svn_wc_traversed_depths := GetProcAddress(SvnWcLib, 'svn_wc_traversed_depths');
      @svn_wc_version := GetProcAddress(SvnWcLib, 'svn_wc_version');
      @svn_wc_walk_entries := GetProcAddress(SvnWcLib, 'svn_wc_walk_entries');
      @svn_wc_walk_entries2 := GetProcAddress(SvnWcLib, 'svn_wc_walk_entries2');
      @svn_wc_walk_entries3 := GetProcAddress(SvnWcLib, 'svn_wc_walk_entries3');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FreeSvnClientLib;

begin
  if SvnClientLibLoaded then
    FreeLibrary(SvnClientLib);
  SvnClientLib := INVALID_HANDLE_VALUE;
  @svn_client_add := nil;
  @svn_client_add2 := nil;
  @svn_client_add3 := nil;
  @svn_client_add4 := nil;
  @svn_client_add_to_changelist := nil;
  @svn_client_blame := nil;
  @svn_client_blame2 := nil;
  @svn_client_blame3 := nil;
  @svn_client_blame4 := nil;
  @svn_client_cat := nil;
  @svn_client_cat2 := nil;
  @svn_client_checkout := nil;
  @svn_client_checkout2 := nil;
  @svn_client_checkout3 := nil;
  @svn_client_cleanup := nil;
  @svn_client_commit := nil;
  @svn_client_commit2 := nil;
  @svn_client_commit3 := nil;
  @svn_client_commit4 := nil;
  @svn_client_commit_item2_dup := nil;
  @svn_client_commit_item3_dup := nil;
  @svn_client_commit_item_create := nil;
  @svn_client_copy := nil;
  @svn_client_copy2 := nil;
  @svn_client_copy3 := nil;
  @svn_client_copy4 := nil;
  @svn_client_create_context := nil;
  @svn_client_delete := nil;
  @svn_client_delete2 := nil;
  @svn_client_delete3 := nil;
  @svn_client_diff := nil;
  @svn_client_diff2 := nil;
  @svn_client_diff3 := nil;
  @svn_client_diff4 := nil;
  @svn_client_diff_peg := nil;
  @svn_client_diff_peg2 := nil;
  @svn_client_diff_peg3 := nil;
  @svn_client_diff_peg4 := nil;
  @svn_client_diff_summarize := nil;
  @svn_client_diff_summarize2 := nil;
  @svn_client_diff_summarize_dup := nil;
  @svn_client_diff_summarize_peg := nil;
  @svn_client_diff_summarize_peg2 := nil;
  @svn_client_export := nil;
  @svn_client_export2 := nil;
  @svn_client_export3 := nil;
  @svn_client_export4 := nil;
  @svn_client_get_changelists := nil;
  @svn_client_get_simple_prompt_provider := nil;
  @svn_client_get_simple_provider := nil;
  @svn_client_get_ssl_client_cert_file_provider := nil;
  @svn_client_get_ssl_client_cert_prompt_provider := nil;
  @svn_client_get_ssl_client_cert_pw_file_provider := nil;
  @svn_client_get_ssl_client_cert_pw_prompt_provider := nil;
  @svn_client_get_ssl_server_trust_file_provider := nil;
  @svn_client_get_ssl_server_trust_prompt_provider := nil;
  @svn_client_get_username_prompt_provider := nil;
  @svn_client_get_username_provider := nil;
  @svn_client_get_windows_simple_provider := nil;
  @svn_client_import := nil;
  @svn_client_import2 := nil;
  @svn_client_import3 := nil;
  @svn_client_info := nil;
  @svn_client_info2 := nil;
  @svn_client_list := nil;
  @svn_client_list2 := nil;
  @svn_client_lock := nil;
  @svn_client_log := nil;
  @svn_client_log2 := nil;
  @svn_client_log3 := nil;
  @svn_client_log4 := nil;
  @svn_client_ls := nil;
  @svn_client_ls2 := nil;
  @svn_client_ls3 := nil;
  @svn_client_merge := nil;
  @svn_client_merge2 := nil;
  @svn_client_merge3 := nil;
  @svn_client_merge_peg := nil;
  @svn_client_merge_peg2 := nil;
  @svn_client_merge_peg3 := nil;
  @svn_client_merge_reintegrate := nil;
  @svn_client_mergeinfo_get_merged := nil;
  @svn_client_mergeinfo_log_eligible := nil;
  @svn_client_mergeinfo_log_merged := nil;
  @svn_client_mkdir := nil;
  @svn_client_mkdir2 := nil;
  @svn_client_mkdir3 := nil;
  @svn_client_move := nil;
  @svn_client_move2 := nil;
  @svn_client_move3 := nil;
  @svn_client_move4 := nil;
  @svn_client_move5 := nil;
  @svn_client_open_ra_session := nil;
  @svn_client_propget := nil;
  @svn_client_propget2 := nil;
  @svn_client_propget3 := nil;
  @svn_client_proplist := nil;
  @svn_client_proplist2 := nil;
  @svn_client_proplist3 := nil;
  @svn_client_proplist_item_dup := nil;
  @svn_client_propset := nil;
  @svn_client_propset2 := nil;
  @svn_client_propset3 := nil;
  @svn_client_relocate := nil;
  @svn_client_remove_from_changelists := nil;
  @svn_client_resolve := nil;
  @svn_client_resolved := nil;
  @svn_client_revert := nil;
  @svn_client_revert2 := nil;
  @svn_client_revprop_get := nil;
  @svn_client_revprop_list := nil;
  @svn_client_revprop_set := nil;
  @svn_client_root_url_from_path := nil;
  @svn_client_status := nil;
  @svn_client_status2 := nil;
  @svn_client_status3 := nil;
  @svn_client_suggest_merge_sources := nil;
  @svn_client_switch := nil;
  @svn_client_switch2 := nil;
  @svn_client_unlock := nil;
  @svn_client_update := nil;
  @svn_client_update2 := nil;
  @svn_client_update3 := nil;
  @svn_client_url_from_path := nil;
  @svn_client_uuid_from_path := nil;
  @svn_client_uuid_from_url := nil;
  @svn_client_version := nil;
  @svn_info_dup := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FreeSvnDeltaLib;

begin
  if SvnDeltaLibLoaded then
    FreeLibrary(SvnDeltaLib);
  SvnDeltaLib := INVALID_HANDLE_VALUE;
  @svn_compat_wrap_file_rev_handler := nil;
  @svn_delta_default_editor := nil;
  @svn_delta_depth_filter_editor := nil;
  @svn_delta_get_cancellation_editor := nil;
  @svn_delta_noop_window_handler := nil;
  @svn_delta_path_driver := nil;
  @svn_delta_version := nil;
  @svn_txdelta := nil;
  @svn_txdelta_apply := nil;
  @svn_txdelta_apply_instructions := nil;
  @svn_txdelta_compose_windows := nil;
  @svn_txdelta_md5_digest := nil;
  @svn_txdelta_next_window := nil;
  @svn_txdelta_parse_svndiff := nil;
  @svn_txdelta_read_svndiff_window := nil;
  @svn_txdelta_send_stream := nil;
  @svn_txdelta_send_string := nil;
  @svn_txdelta_send_txstream := nil;
  @svn_txdelta_skip_svndiff_window := nil;
  @svn_txdelta_stream_create := nil;
  @svn_txdelta_target_push := nil;
  @svn_txdelta_to_svndiff := nil;
  @svn_txdelta_to_svndiff2 := nil;
  @svn_txdelta_window_dup := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FreeSvnDiffLib;

begin
  if SvnDiffLibLoaded then
    FreeLibrary(SvnDiffLib);
  SvnDiffLib := INVALID_HANDLE_VALUE;
  @svn_diff_contains_conflicts := nil;
  @svn_diff_contains_diffs := nil;
  @svn_diff_diff := nil;
  @svn_diff_diff3 := nil;
  @svn_diff_diff4 := nil;
  @svn_diff_file_diff := nil;
  @svn_diff_file_diff3 := nil;
  @svn_diff_file_diff3_2 := nil;
  @svn_diff_file_diff4 := nil;
  @svn_diff_file_diff4_2 := nil;
  @svn_diff_file_diff_2 := nil;
  @svn_diff_file_options_create := nil;
  @svn_diff_file_options_parse := nil;
  @svn_diff_file_output_merge := nil;
  @svn_diff_file_output_unified := nil;
  @svn_diff_file_output_unified2 := nil;
  @svn_diff_file_output_unified3 := nil;
  @svn_diff_mem_string_diff := nil;
  @svn_diff_mem_string_diff3 := nil;
  @svn_diff_mem_string_diff4 := nil;
  @svn_diff_mem_string_output_merge := nil;
  @svn_diff_mem_string_output_unified := nil;
  @svn_diff_output := nil;
  @svn_diff_version := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FreeSvnFsLib;

begin
  if SvnFsLibLoaded then
    FreeLibrary(SvnFsLib);
  SvnFsLib := INVALID_HANDLE_VALUE;
  @svn_fs_abort_txn := nil;
  @svn_fs_access_add_lock_token := nil;
  @svn_fs_access_get_username := nil;
  @svn_fs_apply_text := nil;
  @svn_fs_apply_textdelta := nil;
  @svn_fs_begin_txn := nil;
  @svn_fs_begin_txn2 := nil;
  @svn_fs_berkeley_logfiles := nil;
  @svn_fs_berkeley_path := nil;
  @svn_fs_berkeley_recover := nil;
  @svn_fs_change_node_prop := nil;
  @svn_fs_change_rev_prop := nil;
  @svn_fs_change_txn_prop := nil;
  @svn_fs_change_txn_props := nil;
  @svn_fs_check_path := nil;
  @svn_fs_check_related := nil;
  @svn_fs_close_root := nil;
  @svn_fs_closest_copy := nil;
  @svn_fs_commit_txn := nil;
  @svn_fs_compare_ids := nil;
  @svn_fs_contents_changed := nil;
  @svn_fs_copied_from := nil;
  @svn_fs_copy := nil;
  @svn_fs_create := nil;
  @svn_fs_create_access := nil;
  @svn_fs_create_berkeley := nil;
  @svn_fs_delete := nil;
  @svn_fs_delete_berkeley := nil;
  @svn_fs_delete_fs := nil;
  @svn_fs_deltify_revision := nil;
  @svn_fs_dir_entries := nil;
  @svn_fs_file_contents := nil;
  @svn_fs_file_length := nil;
  @svn_fs_file_md5_checksum := nil;
  @svn_fs_generate_lock_token := nil;
  @svn_fs_get_access := nil;
  @svn_fs_get_file_delta_stream := nil;
  @svn_fs_get_lock := nil;
  @svn_fs_get_locks := nil;
  @svn_fs_get_mergeinfo := nil;
  @svn_fs_get_uuid := nil;
  @svn_fs_history_location := nil;
  @svn_fs_history_prev := nil;
  @svn_fs_hotcopy := nil;
  @svn_fs_hotcopy_berkeley := nil;
  @svn_fs_initialize := nil;
  @svn_fs_is_dir := nil;
  @svn_fs_is_file := nil;
  @svn_fs_is_revision_root := nil;
  @svn_fs_is_txn_root := nil;
  @svn_fs_list_transactions := nil;
  @svn_fs_lock := nil;
  @svn_fs_make_dir := nil;
  @svn_fs_make_file := nil;
  @svn_fs_merge := nil;
  @svn_fs_new := nil;
  @svn_fs_node_created_path := nil;
  @svn_fs_node_created_rev := nil;
  @svn_fs_node_history := nil;
  @svn_fs_node_id := nil;
  @svn_fs_node_origin_rev := nil;
  @svn_fs_node_prop := nil;
  @svn_fs_node_proplist := nil;
  @svn_fs_open := nil;
  @svn_fs_open_berkeley := nil;
  @svn_fs_open_txn := nil;
  @svn_fs_parse_id := nil;
  @svn_fs_path := nil;
  @svn_fs_paths_changed := nil;
  @svn_fs_print_modules := nil;
  @svn_fs_props_changed := nil;
  @svn_fs_purge_txn := nil;
  @svn_fs_recover := nil;
  @svn_fs_revision_link := nil;
  @svn_fs_revision_prop := nil;
  @svn_fs_revision_proplist := nil;
  @svn_fs_revision_root := nil;
  @svn_fs_revision_root_revision := nil;
  @svn_fs_root_fs := nil;
  @svn_fs_set_access := nil;
  @svn_fs_set_berkeley_errcall := nil;
  @svn_fs_set_uuid := nil;
  @svn_fs_set_warning_func := nil;
  @svn_fs_txn_base_revision := nil;
  @svn_fs_txn_name := nil;
  @svn_fs_txn_prop := nil;
  @svn_fs_txn_proplist := nil;
  @svn_fs_txn_root := nil;
  @svn_fs_txn_root_base_revision := nil;
  @svn_fs_txn_root_name := nil;
  @svn_fs_type := nil;
  @svn_fs_unlock := nil;
  @svn_fs_unparse_id := nil;
  @svn_fs_upgrade := nil;
  @svn_fs_version := nil;
  @svn_fs_youngest_rev := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FreeSvnRaLib;

begin
  if SvnRaLibLoaded then
    FreeLibrary(SvnRaLib);
  SvnRaLib := INVALID_HANDLE_VALUE;
  @svn_ra_change_rev_prop := nil;
  @svn_ra_check_path := nil;
  @svn_ra_create_callbacks := nil;
  @svn_ra_dav_init := nil;
  @svn_ra_do_diff := nil;
  @svn_ra_do_diff2 := nil;
  @svn_ra_do_diff3 := nil;
  @svn_ra_do_status := nil;
  @svn_ra_do_status2 := nil;
  @svn_ra_do_switch := nil;
  @svn_ra_do_switch2 := nil;
  @svn_ra_do_update := nil;
  @svn_ra_do_update2 := nil;
  @svn_ra_get_commit_editor := nil;
  @svn_ra_get_commit_editor2 := nil;
  @svn_ra_get_commit_editor3 := nil;
  @svn_ra_get_dated_revision := nil;
  @svn_ra_get_dir := nil;
  @svn_ra_get_dir2 := nil;
  @svn_ra_get_file := nil;
  @svn_ra_get_file_revs := nil;
  @svn_ra_get_file_revs2 := nil;
  @svn_ra_get_latest_revnum := nil;
  @svn_ra_get_location_segments := nil;
  @svn_ra_get_locations := nil;
  @svn_ra_get_lock := nil;
  @svn_ra_get_locks := nil;
  @svn_ra_get_log := nil;
  @svn_ra_get_log2 := nil;
  @svn_ra_get_mergeinfo := nil;
  @svn_ra_get_ra_library := nil;
  @svn_ra_get_repos_root := nil;
  @svn_ra_get_repos_root2 := nil;
  @svn_ra_get_session_url := nil;
  @svn_ra_get_uuid := nil;
  @svn_ra_get_uuid2 := nil;
  @svn_ra_has_capability := nil;
  @svn_ra_init_ra_libs := nil;
  @svn_ra_initialize := nil;
  @svn_ra_local_init := nil;
  @svn_ra_lock := nil;
  @svn_ra_open := nil;
  @svn_ra_open2 := nil;
  @svn_ra_open3 := nil;
  @svn_ra_print_modules := nil;
  @svn_ra_print_ra_libraries := nil;
  @svn_ra_reparent := nil;
  @svn_ra_replay := nil;
  @svn_ra_replay_range := nil;
  @svn_ra_rev_prop := nil;
  @svn_ra_rev_proplist := nil;
  @svn_ra_serf_init := nil;
  @svn_ra_stat := nil;
  @svn_ra_svn_init := nil;
  @svn_ra_unlock := nil;
  @svn_ra_version := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FreeSvnReposLib;

begin
  if SvnReposLibLoaded then
    FreeLibrary(SvnReposLib);
  SvnReposLib := INVALID_HANDLE_VALUE;
  @svn_repos_abort_report := nil;
  @svn_repos_authz_check_access := nil;
  @svn_repos_authz_read := nil;
  @svn_repos_begin_report := nil;
  @svn_repos_begin_report2 := nil;
  @svn_repos_check_revision_access := nil;
  @svn_repos_conf_dir := nil;
  @svn_repos_create := nil;
  @svn_repos_dated_revision := nil;
  @svn_repos_db_env := nil;
  @svn_repos_db_lockfile := nil;
  @svn_repos_db_logfiles := nil;
  @svn_repos_db_logs_lockfile := nil;
  @svn_repos_delete := nil;
  @svn_repos_delete_path := nil;
  @svn_repos_deleted_rev := nil;
  @svn_repos_dir_delta := nil;
  @svn_repos_dir_delta2 := nil;
  @svn_repos_dump_fs := nil;
  @svn_repos_dump_fs2 := nil;
  @svn_repos_find_root_path := nil;
  @svn_repos_finish_report := nil;
  @svn_repos_fs := nil;
  @svn_repos_fs_begin_txn_for_commit := nil;
  @svn_repos_fs_begin_txn_for_commit2 := nil;
  @svn_repos_fs_begin_txn_for_update := nil;
  @svn_repos_fs_change_node_prop := nil;
  @svn_repos_fs_change_rev_prop := nil;
  @svn_repos_fs_change_rev_prop2 := nil;
  @svn_repos_fs_change_rev_prop3 := nil;
  @svn_repos_fs_change_txn_prop := nil;
  @svn_repos_fs_change_txn_props := nil;
  @svn_repos_fs_commit_txn := nil;
  @svn_repos_fs_get_locks := nil;
  @svn_repos_fs_get_mergeinfo := nil;
  @svn_repos_fs_lock := nil;
  @svn_repos_fs_revision_prop := nil;
  @svn_repos_fs_revision_proplist := nil;
  @svn_repos_fs_unlock := nil;
  @svn_repos_get_commit_editor := nil;
  @svn_repos_get_commit_editor2 := nil;
  @svn_repos_get_commit_editor3 := nil;
  @svn_repos_get_commit_editor4 := nil;
  @svn_repos_get_commit_editor5 := nil;
  @svn_repos_get_committed_info := nil;
  @svn_repos_get_file_revs := nil;
  @svn_repos_get_file_revs2 := nil;
  @svn_repos_get_fs_build_parser := nil;
  @svn_repos_get_fs_build_parser2 := nil;
  @svn_repos_get_logs := nil;
  @svn_repos_get_logs2 := nil;
  @svn_repos_get_logs3 := nil;
  @svn_repos_get_logs4 := nil;
  @svn_repos_has_capability := nil;
  @svn_repos_history := nil;
  @svn_repos_history2 := nil;
  @svn_repos_hook_dir := nil;
  @svn_repos_hotcopy := nil;
  @svn_repos_link_path := nil;
  @svn_repos_link_path2 := nil;
  @svn_repos_link_path3 := nil;
  @svn_repos_load_fs := nil;
  @svn_repos_load_fs2 := nil;
  @svn_repos_lock_dir := nil;
  @svn_repos_node_editor := nil;
  @svn_repos_node_from_baton := nil;
  @svn_repos_node_location_segments := nil;
  @svn_repos_open := nil;
  @svn_repos_parse_dumpstream := nil;
  @svn_repos_parse_dumpstream2 := nil;
  @svn_repos_path := nil;
  @svn_repos_post_commit_hook := nil;
  @svn_repos_post_lock_hook := nil;
  @svn_repos_post_revprop_change_hook := nil;
  @svn_repos_post_unlock_hook := nil;
  @svn_repos_pre_commit_hook := nil;
  @svn_repos_pre_lock_hook := nil;
  @svn_repos_pre_revprop_change_hook := nil;
  @svn_repos_pre_unlock_hook := nil;
  @svn_repos_recover := nil;
  @svn_repos_recover2 := nil;
  @svn_repos_recover3 := nil;
  @svn_repos_remember_client_capabilities := nil;
  @svn_repos_replay := nil;
  @svn_repos_replay2 := nil;
  @svn_repos_set_path := nil;
  @svn_repos_set_path2 := nil;
  @svn_repos_set_path3 := nil;
  @svn_repos_start_commit_hook := nil;
  @svn_repos_stat := nil;
  @svn_repos_svnserve_conf := nil;
  @svn_repos_trace_node_locations := nil;
  @svn_repos_upgrade := nil;
  @svn_repos_verify_fs := nil;
  @svn_repos_version := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FreeSvnSubrLib;

begin
  if SvnSubrLibLoaded then
    FreeLibrary(SvnSubrLib);
  SvnSubrLib := INVALID_HANDLE_VALUE;
  @svn_auth_first_credentials := nil;
  @svn_auth_get_parameter := nil;
  @svn_auth_get_simple_prompt_provider := nil;
  @svn_auth_get_simple_provider := nil;
  @svn_auth_get_ssl_client_cert_file_provider := nil;
  @svn_auth_get_ssl_client_cert_prompt_provider := nil;
  @svn_auth_get_ssl_client_cert_pw_file_provider := nil;
  @svn_auth_get_ssl_client_cert_pw_prompt_provider := nil;
  @svn_auth_get_ssl_server_trust_file_provider := nil;
  @svn_auth_get_ssl_server_trust_prompt_provider := nil;
  @svn_auth_get_username_prompt_provider := nil;
  @svn_auth_get_username_provider := nil;
  @svn_auth_get_windows_simple_provider := nil;
  @svn_auth_get_windows_ssl_server_trust_provider := nil;
  @svn_auth_next_credentials := nil;
  @svn_auth_open := nil;
  @svn_auth_save_credentials := nil;
  @svn_auth_set_parameter := nil;
  @svn_auth_ssl_server_cert_info_dup := nil;
  @svn_base64_decode := nil;
  @svn_base64_decode_string := nil;
  @svn_base64_encode := nil;
  @svn_base64_encode_string := nil;
  @svn_base64_from_md5 := nil;
  @svn_categorize_props := nil;
  @svn_cmdline__getopt_init := nil;
  @svn_cmdline_auth_simple_prompt := nil;
  @svn_cmdline_auth_ssl_client_cert_prompt := nil;
  @svn_cmdline_auth_ssl_client_cert_pw_prompt := nil;
  @svn_cmdline_auth_ssl_server_trust_prompt := nil;
  @svn_cmdline_auth_username_prompt := nil;
  @svn_cmdline_cstring_from_utf8 := nil;
  @svn_cmdline_cstring_from_utf8_fuzzy := nil;
  @svn_cmdline_cstring_to_utf8 := nil;
  @svn_cmdline_fflush := nil;
  @svn_cmdline_fprintf := nil;
  @svn_cmdline_fputs := nil;
  @svn_cmdline_handle_exit_error := nil;
  @svn_cmdline_init := nil;
  @svn_cmdline_output_encoding := nil;
  @svn_cmdline_path_local_style_from_utf8 := nil;
  @svn_cmdline_printf := nil;
  @svn_cmdline_prompt_user := nil;
  @svn_cmdline_prompt_user2 := nil;
  @svn_cmdline_setup_auth_baton := nil;
  @svn_commit_info_dup := nil;
  @svn_compat_log_revprops_clear := nil;
  @svn_compat_log_revprops_in := nil;
  @svn_compat_log_revprops_out := nil;
  @svn_compat_wrap_commit_callback := nil;
  @svn_compat_wrap_log_receiver := nil;
  @svn_config_ensure := nil;
  @svn_config_enumerate := nil;
  @svn_config_enumerate2 := nil;
  @svn_config_enumerate_sections := nil;
  @svn_config_enumerate_sections2 := nil;
  @svn_config_find_group := nil;
  @svn_config_get := nil;
  @svn_config_get_bool := nil;
  @svn_config_get_config := nil;
  @svn_config_get_server_setting := nil;
  @svn_config_get_server_setting_int := nil;
  @svn_config_has_section := nil;
  @svn_config_merge := nil;
  @svn_config_read := nil;
  @svn_config_read_auth_data := nil;
  @svn_config_set := nil;
  @svn_config_set_bool := nil;
  @svn_config_write_auth_data := nil;
  @svn_create_commit_info := nil;
  @svn_cstring_casecmp := nil;
  @svn_cstring_count_newlines := nil;
  @svn_cstring_join := nil;
  @svn_cstring_match_glob_list := nil;
  @svn_cstring_split := nil;
  @svn_cstring_split_append := nil;
  @svn_ctype_casecmp := nil;
  @svn_ctype_table := nil;
  @svn_depth_from_word := nil;
  @svn_depth_to_word := nil;
  @svn_dirent_dup := nil;
  @svn_dirent_is_root := nil;
  @svn_dso_initialize := nil;
  @svn_dso_load := nil;
  @svn_err_best_message := nil;
  @svn_error__locate := nil;
  @svn_error_clear := nil;
  @svn_error_compose := nil;
  @svn_error_create := nil;
  @svn_error_createf := nil;
  @svn_error_dup := nil;
  @svn_error_quick_wrap := nil;
  @svn_error_root_cause := nil;
  @svn_error_wrap_apr := nil;
  @svn_handle_error := nil;
  @svn_handle_error2 := nil;
  @svn_handle_warning := nil;
  @svn_handle_warning2 := nil;
  @svn_hash__clear := nil;
  @svn_hash_diff := nil;
  @svn_hash_from_cstring_keys := nil;
  @svn_hash_keys := nil;
  @svn_hash_read := nil;
  @svn_hash_read2 := nil;
  @svn_hash_read_incremental := nil;
  @svn_hash_write := nil;
  @svn_hash_write2 := nil;
  @svn_hash_write_incremental := nil;
  @svn_inheritance_from_word := nil;
  @svn_inheritance_to_word := nil;
  @svn_io_append_file := nil;
  @svn_io_check_path := nil;
  @svn_io_check_resolved_path := nil;
  @svn_io_check_special_path := nil;
  @svn_io_copy_dir_recursively := nil;
  @svn_io_copy_file := nil;
  @svn_io_copy_link := nil;
  @svn_io_create_unique_link := nil;
  @svn_io_detect_mimetype := nil;
  @svn_io_detect_mimetype2 := nil;
  @svn_io_dir_empty := nil;
  @svn_io_dir_file_copy := nil;
  @svn_io_dir_make := nil;
  @svn_io_dir_make_hidden := nil;
  @svn_io_dir_make_sgid := nil;
  @svn_io_dir_open := nil;
  @svn_io_dir_read := nil;
  @svn_io_dir_remove_nonrecursive := nil;
  @svn_io_dir_walk := nil;
  @svn_io_file_affected_time := nil;
  @svn_io_file_checksum := nil;
  @svn_io_file_close := nil;
  @svn_io_file_create := nil;
  @svn_io_file_flush_to_disk := nil;
  @svn_io_file_getc := nil;
  @svn_io_file_info_get := nil;
  @svn_io_file_lock := nil;
  @svn_io_file_lock2 := nil;
  @svn_io_file_move := nil;
  @svn_io_file_open := nil;
  @svn_io_file_read := nil;
  @svn_io_file_read_full := nil;
  @svn_io_file_rename := nil;
  @svn_io_file_seek := nil;
  @svn_io_file_write := nil;
  @svn_io_file_write_full := nil;
  @svn_io_files_contents_same_p := nil;
  @svn_io_filesizes_different_p := nil;
  @svn_io_get_dir_filenames := nil;
  @svn_io_get_dirents := nil;
  @svn_io_get_dirents2 := nil;
  @svn_io_is_file_executable := nil;
  @svn_io_make_dir_recursively := nil;
  @svn_io_open_unique_file := nil;
  @svn_io_open_unique_file2 := nil;
  @svn_io_parse_mimetypes_file := nil;
  @svn_io_read_length_line := nil;
  @svn_io_read_link := nil;
  @svn_io_read_version_file := nil;
  @svn_io_remove_dir := nil;
  @svn_io_remove_dir2 := nil;
  @svn_io_remove_file := nil;
  @svn_io_run_cmd := nil;
  @svn_io_run_diff := nil;
  @svn_io_run_diff3 := nil;
  @svn_io_run_diff3_2 := nil;
  @svn_io_set_file_affected_time := nil;
  @svn_io_set_file_executable := nil;
  @svn_io_set_file_read_only := nil;
  @svn_io_set_file_read_write := nil;
  @svn_io_set_file_read_write_carefully := nil;
  @svn_io_start_cmd := nil;
  @svn_io_stat := nil;
  @svn_io_temp_dir := nil;
  @svn_io_wait_for_cmd := nil;
  @svn_io_write_version_file := nil;
  @svn_iter__break := nil;
  @svn_iter_apr_array := nil;
  @svn_iter_apr_hash := nil;
  @svn_location_segment_dup := nil;
  @svn_lock_create := nil;
  @svn_lock_dup := nil;
  @svn_log_changed_path_dup := nil;
  @svn_log_entry_create := nil;
  @svn_md5_digest_to_cstring := nil;
  @svn_md5_digest_to_cstring_display := nil;
  @svn_md5_digests_match := nil;
  @svn_md5_empty_string_digest := nil;
  @svn_merge_range_contains_rev := nil;
  @svn_merge_range_dup := nil;
  @svn_mergeinfo_diff := nil;
  @svn_mergeinfo_dup := nil;
  @svn_mergeinfo_inheritable := nil;
  @svn_mergeinfo_intersect := nil;
  @svn_mergeinfo_merge := nil;
  @svn_mergeinfo_parse := nil;
  @svn_mergeinfo_remove := nil;
  @svn_mergeinfo_sort := nil;
  @svn_mergeinfo_to_string := nil;
  @svn_mime_type_is_binary := nil;
  @svn_mime_type_validate := nil;
  @svn_nls_init := nil;
  @svn_opt_args_to_target_array := nil;
  @svn_opt_args_to_target_array2 := nil;
  @svn_opt_args_to_target_array3 := nil;
  @svn_opt_format_option := nil;
  @svn_opt_get_canonical_subcommand := nil;
  @svn_opt_get_canonical_subcommand2 := nil;
  @svn_opt_get_option_from_code := nil;
  @svn_opt_get_option_from_code2 := nil;
  @svn_opt_parse_all_args := nil;
  @svn_opt_parse_num_args := nil;
  @svn_opt_parse_path := nil;
  @svn_opt_parse_revision := nil;
  @svn_opt_parse_revision_to_range := nil;
  @svn_opt_print_generic_help := nil;
  @svn_opt_print_generic_help2 := nil;
  @svn_opt_print_help := nil;
  @svn_opt_print_help2 := nil;
  @svn_opt_print_help3 := nil;
  @svn_opt_push_implicit_dot_target := nil;
  @svn_opt_resolve_revisions := nil;
  @svn_opt_subcommand_help := nil;
  @svn_opt_subcommand_help2 := nil;
  @svn_opt_subcommand_help3 := nil;
  @svn_opt_subcommand_takes_option := nil;
  @svn_opt_subcommand_takes_option2 := nil;
  @svn_opt_subcommand_takes_option3 := nil;
  @svn_parse_date := nil;
  @svn_path_add_component := nil;
  @svn_path_basename := nil;
  @svn_path_canonicalize := nil;
  @svn_path_check_valid := nil;
  @svn_path_compare_paths := nil;
  @svn_path_component_count := nil;
  @svn_path_compose := nil;
  @svn_path_condense_targets := nil;
  @svn_path_cstring_from_utf8 := nil;
  @svn_path_cstring_to_utf8 := nil;
  @svn_path_decompose := nil;
  @svn_path_dirname := nil;
  @svn_path_get_absolute := nil;
  @svn_path_get_longest_ancestor := nil;
  @svn_path_internal_style := nil;
  @svn_path_is_ancestor := nil;
  @svn_path_is_backpath_present := nil;
  @svn_path_is_canonical := nil;
  @svn_path_is_child := nil;
  @svn_path_is_empty := nil;
  @svn_path_is_single_path_component := nil;
  @svn_path_is_uri_safe := nil;
  @svn_path_is_url := nil;
  @svn_path_join := nil;
  @svn_path_join_many := nil;
  @svn_path_local_style := nil;
  @svn_path_remove_component := nil;
  @svn_path_remove_components := nil;
  @svn_path_remove_redundancies := nil;
  @svn_path_split := nil;
  @svn_path_split_if_file := nil;
  @svn_path_splitext := nil;
  @svn_path_uri_autoescape := nil;
  @svn_path_uri_decode := nil;
  @svn_path_uri_encode := nil;
  @svn_path_uri_from_iri := nil;
  @svn_path_url_add_component := nil;
  @svn_pool_create_ex := nil;
  @svn_pool_create_ex_debug := nil;
  @svn_prop_array_dup := nil;
  @svn_prop_diffs := nil;
  @svn_prop_dup := nil;
  @svn_prop_has_svn_prop := nil;
  @svn_prop_hash_to_array := nil;
  @svn_prop_is_boolean := nil;
  @svn_prop_is_svn_prop := nil;
  @svn_prop_name_is_valid := nil;
  @svn_prop_needs_translation := nil;
  @svn_property_kind := nil;
  @svn_quoprint_decode := nil;
  @svn_quoprint_decode_string := nil;
  @svn_quoprint_encode := nil;
  @svn_quoprint_encode_string := nil;
  @svn_rangelist_diff := nil;
  @svn_rangelist_dup := nil;
  @svn_rangelist_inheritable := nil;
  @svn_rangelist_intersect := nil;
  @svn_rangelist_merge := nil;
  @svn_rangelist_remove := nil;
  @svn_rangelist_reverse := nil;
  @svn_rangelist_to_string := nil;
  @svn_revnum_parse := nil;
  @svn_sleep_for_timestamps := nil;
  @svn_sort__hash := nil;
  @svn_sort_compare_items_as_paths := nil;
  @svn_sort_compare_items_lexically := nil;
  @svn_sort_compare_paths := nil;
  @svn_sort_compare_ranges := nil;
  @svn_sort_compare_revisions := nil;
  @svn_stream_checksummed := nil;
  @svn_stream_close := nil;
  @svn_stream_compressed := nil;
  @svn_stream_contents_same := nil;
  @svn_stream_copy := nil;
  @svn_stream_copy2 := nil;
  @svn_stream_create := nil;
  @svn_stream_disown := nil;
  @svn_stream_empty := nil;
  @svn_stream_for_stdout := nil;
  @svn_stream_from_aprfile := nil;
  @svn_stream_from_aprfile2 := nil;
  @svn_stream_from_stringbuf := nil;
  @svn_stream_printf := nil;
  @svn_stream_printf_from_utf8 := nil;
  @svn_stream_read := nil;
  @svn_stream_readline := nil;
  @svn_stream_set_baton := nil;
  @svn_stream_set_close := nil;
  @svn_stream_set_read := nil;
  @svn_stream_set_write := nil;
  @svn_stream_write := nil;
  @svn_strerror := nil;
  @svn_string_compare := nil;
  @svn_string_compare_stringbuf := nil;
  @svn_string_create := nil;
  @svn_string_create_from_buf := nil;
  @svn_string_createf := nil;
  @svn_string_createv := nil;
  @svn_string_dup := nil;
  @svn_string_find_char_backward := nil;
  @svn_string_first_non_whitespace := nil;
  @svn_string_isempty := nil;
  @svn_string_ncreate := nil;
  @svn_stringbuf_appendbytes := nil;
  @svn_stringbuf_appendcstr := nil;
  @svn_stringbuf_appendstr := nil;
  @svn_stringbuf_chop := nil;
  @svn_stringbuf_compare := nil;
  @svn_stringbuf_create := nil;
  @svn_stringbuf_create_from_string := nil;
  @svn_stringbuf_createf := nil;
  @svn_stringbuf_createv := nil;
  @svn_stringbuf_dup := nil;
  @svn_stringbuf_ensure := nil;
  @svn_stringbuf_fillchar := nil;
  @svn_stringbuf_find_char_backward := nil;
  @svn_stringbuf_first_non_whitespace := nil;
  @svn_stringbuf_from_aprfile := nil;
  @svn_stringbuf_from_file := nil;
  @svn_stringbuf_from_file2 := nil;
  @svn_stringbuf_isempty := nil;
  @svn_stringbuf_ncreate := nil;
  @svn_stringbuf_set := nil;
  @svn_stringbuf_setempty := nil;
  @svn_stringbuf_strip_whitespace := nil;
  @svn_subr_version := nil;
  @svn_subst_build_keywords := nil;
  @svn_subst_build_keywords2 := nil;
  @svn_subst_copy_and_translate := nil;
  @svn_subst_copy_and_translate2 := nil;
  @svn_subst_copy_and_translate3 := nil;
  @svn_subst_detranslate_string := nil;
  @svn_subst_eol_style_from_value := nil;
  @svn_subst_keywords_differ := nil;
  @svn_subst_keywords_differ2 := nil;
  @svn_subst_stream_detranslated := nil;
  @svn_subst_stream_from_specialfile := nil;
  @svn_subst_stream_translated := nil;
  @svn_subst_stream_translated_to_normal_form := nil;
  @svn_subst_translate_cstring := nil;
  @svn_subst_translate_cstring2 := nil;
  @svn_subst_translate_stream := nil;
  @svn_subst_translate_stream2 := nil;
  @svn_subst_translate_stream3 := nil;
  @svn_subst_translate_string := nil;
  @svn_subst_translate_to_normal_form := nil;
  @svn_subst_translation_required := nil;
  @svn_time_from_cstring := nil;
  @svn_time_to_cstring := nil;
  @svn_time_to_human_cstring := nil;
  @svn_user_get_homedir := nil;
  @svn_user_get_name := nil;
  @svn_utf_cstring_from_utf8 := nil;
  @svn_utf_cstring_from_utf8_ex := nil;
  @svn_utf_cstring_from_utf8_ex2 := nil;
  @svn_utf_cstring_from_utf8_fuzzy := nil;
  @svn_utf_cstring_from_utf8_string := nil;
  @svn_utf_cstring_from_utf8_stringbuf := nil;
  @svn_utf_cstring_to_utf8 := nil;
  @svn_utf_cstring_to_utf8_ex := nil;
  @svn_utf_cstring_to_utf8_ex2 := nil;
  @svn_utf_initialize := nil;
  @svn_utf_string_from_utf8 := nil;
  @svn_utf_string_to_utf8 := nil;
  @svn_utf_stringbuf_from_utf8 := nil;
  @svn_utf_stringbuf_to_utf8 := nil;
  @svn_uuid_generate := nil;
  @svn_ver_check_list := nil;
  @svn_ver_compatible := nil;
  @svn_ver_equal := nil;
  @svn_xml_ap_to_hash := nil;
  @svn_xml_escape_attr_cstring := nil;
  @svn_xml_escape_attr_string := nil;
  @svn_xml_escape_attr_stringbuf := nil;
  @svn_xml_escape_cdata_cstring := nil;
  @svn_xml_escape_cdata_string := nil;
  @svn_xml_escape_cdata_stringbuf := nil;
  @svn_xml_free_parser := nil;
  @svn_xml_fuzzy_escape := nil;
  @svn_xml_get_attr_value := nil;
  @svn_xml_hash_atts_overlaying := nil;
  @svn_xml_hash_atts_preserving := nil;
  @svn_xml_is_xml_safe := nil;
  @svn_xml_make_att_hash := nil;
  @svn_xml_make_close_tag := nil;
  @svn_xml_make_header := nil;
  @svn_xml_make_open_tag := nil;
  @svn_xml_make_open_tag_hash := nil;
  @svn_xml_make_open_tag_v := nil;
  @svn_xml_make_parser := nil;
  @svn_xml_parse := nil;
  @svn_xml_signal_bailout := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FreeSvnWcLib;

begin
  if SvnWcLibLoaded then
    FreeLibrary(SvnWcLib);
  SvnWcLib := INVALID_HANDLE_VALUE;
  @svn_wc_add := nil;
  @svn_wc_add2 := nil;
  @svn_wc_add_lock := nil;
  @svn_wc_add_repos_file := nil;
  @svn_wc_add_repos_file2 := nil;
  @svn_wc_adm_access_path := nil;
  @svn_wc_adm_access_pool := nil;
  @svn_wc_adm_close := nil;
  @svn_wc_adm_locked := nil;
  @svn_wc_adm_open := nil;
  @svn_wc_adm_open2 := nil;
  @svn_wc_adm_open3 := nil;
  @svn_wc_adm_open_anchor := nil;
  @svn_wc_adm_probe_open := nil;
  @svn_wc_adm_probe_open2 := nil;
  @svn_wc_adm_probe_open3 := nil;
  @svn_wc_adm_probe_retrieve := nil;
  @svn_wc_adm_probe_try := nil;
  @svn_wc_adm_probe_try2 := nil;
  @svn_wc_adm_probe_try3 := nil;
  @svn_wc_adm_retrieve := nil;
  @svn_wc_canonicalize_svn_prop := nil;
  @svn_wc_check_wc := nil;
  @svn_wc_cleanup := nil;
  @svn_wc_cleanup2 := nil;
  @svn_wc_committed_queue_create := nil;
  @svn_wc_conflicted_p := nil;
  @svn_wc_copy := nil;
  @svn_wc_copy2 := nil;
  @svn_wc_crawl_revisions := nil;
  @svn_wc_crawl_revisions2 := nil;
  @svn_wc_crawl_revisions3 := nil;
  @svn_wc_create_conflict_result := nil;
  @svn_wc_create_notify := nil;
  @svn_wc_create_tmp_file := nil;
  @svn_wc_create_tmp_file2 := nil;
  @svn_wc_delete := nil;
  @svn_wc_delete2 := nil;
  @svn_wc_delete3 := nil;
  @svn_wc_diff := nil;
  @svn_wc_diff2 := nil;
  @svn_wc_diff3 := nil;
  @svn_wc_diff4 := nil;
  @svn_wc_dup_notify := nil;
  @svn_wc_dup_status := nil;
  @svn_wc_dup_status2 := nil;
  @svn_wc_edited_externals := nil;
  @svn_wc_ensure_adm := nil;
  @svn_wc_ensure_adm2 := nil;
  @svn_wc_ensure_adm3 := nil;
  @svn_wc_entries_read := nil;
  @svn_wc_entry := nil;
  @svn_wc_entry_dup := nil;
  @svn_wc_external_item2_dup := nil;
  @svn_wc_external_item_create := nil;
  @svn_wc_external_item_dup := nil;
  @svn_wc_get_actual_target := nil;
  @svn_wc_get_adm_dir := nil;
  @svn_wc_get_ancestry := nil;
  @svn_wc_get_default_ignores := nil;
  @svn_wc_get_diff_editor := nil;
  @svn_wc_get_diff_editor2 := nil;
  @svn_wc_get_diff_editor3 := nil;
  @svn_wc_get_diff_editor4 := nil;
  @svn_wc_get_ignores := nil;
  @svn_wc_get_pristine_copy_path := nil;
  @svn_wc_get_prop_diffs := nil;
  @svn_wc_get_status_editor := nil;
  @svn_wc_get_status_editor2 := nil;
  @svn_wc_get_status_editor3 := nil;
  @svn_wc_get_switch_editor := nil;
  @svn_wc_get_switch_editor2 := nil;
  @svn_wc_get_switch_editor3 := nil;
  @svn_wc_get_update_editor := nil;
  @svn_wc_get_update_editor2 := nil;
  @svn_wc_get_update_editor3 := nil;
  @svn_wc_has_binary_prop := nil;
  @svn_wc_init_traversal_info := nil;
  @svn_wc_is_adm_dir := nil;
  @svn_wc_is_entry_prop := nil;
  @svn_wc_is_normal_prop := nil;
  @svn_wc_is_wc_prop := nil;
  @svn_wc_is_wc_root := nil;
  @svn_wc_locked := nil;
  @svn_wc_mark_missing_deleted := nil;
  @svn_wc_match_ignore_list := nil;
  @svn_wc_maybe_set_repos_root := nil;
  @svn_wc_merge := nil;
  @svn_wc_merge2 := nil;
  @svn_wc_merge3 := nil;
  @svn_wc_merge_prop_diffs := nil;
  @svn_wc_merge_props := nil;
  @svn_wc_merge_props2 := nil;
  @svn_wc_parse_externals_description := nil;
  @svn_wc_parse_externals_description2 := nil;
  @svn_wc_parse_externals_description3 := nil;
  @svn_wc_process_committed := nil;
  @svn_wc_process_committed2 := nil;
  @svn_wc_process_committed3 := nil;
  @svn_wc_process_committed4 := nil;
  @svn_wc_process_committed_queue := nil;
  @svn_wc_prop_get := nil;
  @svn_wc_prop_list := nil;
  @svn_wc_prop_set := nil;
  @svn_wc_prop_set2 := nil;
  @svn_wc_props_modified_p := nil;
  @svn_wc_queue_committed := nil;
  @svn_wc_relocate := nil;
  @svn_wc_relocate2 := nil;
  @svn_wc_relocate3 := nil;
  @svn_wc_remove_from_revision_control := nil;
  @svn_wc_remove_lock := nil;
  @svn_wc_resolved_conflict := nil;
  @svn_wc_resolved_conflict2 := nil;
  @svn_wc_resolved_conflict3 := nil;
  @svn_wc_revert := nil;
  @svn_wc_revert2 := nil;
  @svn_wc_revert3 := nil;
  @svn_wc_revision_status := nil;
  @svn_wc_set_adm_dir := nil;
  @svn_wc_set_changelist := nil;
  @svn_wc_status := nil;
  @svn_wc_status2 := nil;
  @svn_wc_status_set_repos_locks := nil;
  @svn_wc_text_modified_p := nil;
  @svn_wc_translated_file := nil;
  @svn_wc_translated_file2 := nil;
  @svn_wc_translated_stream := nil;
  @svn_wc_transmit_prop_deltas := nil;
  @svn_wc_transmit_text_deltas := nil;
  @svn_wc_transmit_text_deltas2 := nil;
  @svn_wc_traversed_depths := nil;
  @svn_wc_version := nil;
  @svn_wc_walk_entries := nil;
  @svn_wc_walk_entries2 := nil;
  @svn_wc_walk_entries3 := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

{ ESvnError private }

//----------------------------------------------------------------------------------------------------------------------

function ESvnError.GetCount: Integer;

begin
  Result := Length(FErrors);
end;

//----------------------------------------------------------------------------------------------------------------------

function ESvnError.GetErrorCodes(Index: Integer): Integer;

begin
  if (Index >= Low(FErrors)) and (Index <= High(FErrors)) then
    Result := FErrors[Index].Code
  else
    Result := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function ESvnError.GetMessages(Index: Integer): string;

begin
  if (Index >= Low(FErrors)) and (Index <= High(FErrors)) then
    Result := Copy(Message, FErrors[Index].SPos, FErrors[Index].SLen)
  else
    Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

{ ESvnError public }

//----------------------------------------------------------------------------------------------------------------------

constructor ESvnError.Create(Error: PSvnError);

var
  E: PSvnError;
  CurPos: Integer;
  S: string;

begin
  if Assigned(Error) then
    inherited Create(Error^.apr_err)
  else
  begin
    inherited Create(APR_SUCCESS);
    Exit;
  end;

  Message := '';
  CurPos := 1;
  E := Error;
  while Assigned(E) do
  begin
    SetLength(FErrors, Length(FErrors) + 1);

    if Assigned(E^.message) then
      S := E^.message
    else
      S := GetSvnErrorMessage(E^.apr_err);

    with FErrors[High(FErrors)] do
    begin
      Code := E^.apr_err;
      SPos := CurPos;
      SLen := Length(S);
    end;

    if Message <> '' then
    begin
      Message := Message + #13#10;
      Inc(CurPos, 2);
    end;

    Message := Message + S;
    Inc(CurPos, Length(S));

    E := E^.child;
  end;

  svn_error_clear(Error);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor ESvnError.Destroy;

begin
  FErrors := nil;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetSvnErrorMessage(Status: TAprStatus): string;

var
  ErrorStr: AnsiString;

begin
  Result := '';
  if Status = SVN_NO_ERROR then
    Exit;
  SetLength(ErrorStr, 255);
  svn_strerror(Status, PAnsiChar(ErrorStr), Length(ErrorStr));
  SetLength(ErrorStr, StrLen(PAnsiChar(ErrorStr)));
  Result := string(ErrorStr);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SvnCheck(Error: PSvnError);

begin
  if Assigned(Error) then
    RaiseSvnError(Error);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure RaiseSvnError(Error: PSvnError);

begin
  raise ESvnError.Create(Error);
end;

//----------------------------------------------------------------------------------------------------------------------

initialization

finalization
  FreeSvnClientLib;

//----------------------------------------------------------------------------------------------------------------------

end.