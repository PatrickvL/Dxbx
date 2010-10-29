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
{ The Original Code is apr.pas.                                                                                        }
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
{ This unit contains import declarations for libapr.dll, Apache Portable Runtime 0.9.12 DLL.                           }
{                                                                                                                      }
{**********************************************************************************************************************}

unit apr;

interface

uses
  Windows, SysUtils, WinSock;

//----- apr.h ----------------------------------------------------------------------------------------------------------

type
  PIOVec = ^TIOVec;
  TIOVec = packed record
    iov_base: PAnsiChar;
    iov_len: Integer;
  end;
  PAprSize = ^TAprSize;
  TAprSize = Integer;
  PAprOff = ^TAprOff;
  TAprOff = Int64;
  TAprSockLen = Integer;

//----- apr.h ----------------------------------------------------------------------------------------------------------

//----- apr_errno.h ----------------------------------------------------------------------------------------------------

type
  TAprStatus = Integer;

var
  apr_strerror: function(statcode: TAprStatus; buf: PAnsiChar; bufsize: TAprSize): PAnsiChar; stdcall;

const
  APR_OS_START_ERROR     = 20000;
  APR_OS_ERRSPACE_SIZE   = 50000;
  APR_OS_START_STATUS    = APR_OS_START_ERROR + APR_OS_ERRSPACE_SIZE;
  APR_OS_START_USERERR   = APR_OS_START_STATUS + APR_OS_ERRSPACE_SIZE;
  APR_OS_START_USEERR    = APR_OS_START_USERERR;
  APR_OS_START_CANONERR  = APR_OS_START_USERERR + (APR_OS_ERRSPACE_SIZE * 10);
  APR_OS_START_EAIERR    = APR_OS_START_CANONERR + APR_OS_ERRSPACE_SIZE;
  APR_OS_START_SYSERR    = APR_OS_START_EAIERR + APR_OS_ERRSPACE_SIZE;

  APR_SUCCESS = 0;

  APR_ENOSTAT            = APR_OS_START_ERROR + 1;
  APR_ENOPOOL            = APR_OS_START_ERROR + 2;
  APR_EBADDATE           = APR_OS_START_ERROR + 4;
  APR_EINVALSOCK         = APR_OS_START_ERROR + 5;
  APR_ENOPROC            = APR_OS_START_ERROR + 6;
  APR_ENOTIME            = APR_OS_START_ERROR + 7;
  APR_ENODIR             = APR_OS_START_ERROR + 8;
  APR_ENOLOCK            = APR_OS_START_ERROR + 9;
  APR_ENOPOLL            = APR_OS_START_ERROR + 10;
  APR_ENOSOCKET          = APR_OS_START_ERROR + 11;
  APR_ENOTHREAD          = APR_OS_START_ERROR + 12;
  APR_ENOTHDKEY          = APR_OS_START_ERROR + 13;
  APR_EGENERAL           = APR_OS_START_ERROR + 14;
  APR_ENOSHMAVAIL        = APR_OS_START_ERROR + 15;
  APR_EBADIP             = APR_OS_START_ERROR + 16;
  APR_EBADMASK           = APR_OS_START_ERROR + 17;
  APR_EDSOOPEN           = APR_OS_START_ERROR + 19;
  APR_EABSOLUTE          = APR_OS_START_ERROR + 20;
  APR_ERELATIVE          = APR_OS_START_ERROR + 21;
  APR_EINCOMPLETE        = APR_OS_START_ERROR + 22;
  APR_EABOVEROOT         = APR_OS_START_ERROR + 23;
  APR_EBADPATH           = APR_OS_START_ERROR + 24;
  APR_EPATHWILD          = APR_OS_START_ERROR + 25;
  APR_ESYMNOTFOUND       = APR_OS_START_ERROR + 26;
  APR_EPROC_UNKNOWN      = APR_OS_START_ERROR + 27;
  APR_ENOTENOUGHENTROPY  = APR_OS_START_ERROR + 28;

  APR_INCHILD            = APR_OS_START_STATUS + 1;
  APR_INPARENT           = APR_OS_START_STATUS + 2;
  APR_DETACH             = APR_OS_START_STATUS + 3;
  APR_NOTDETACH          = APR_OS_START_STATUS + 4;
  APR_CHILD_DONE         = APR_OS_START_STATUS + 5;
  APR_CHILD_NOTDONE      = APR_OS_START_STATUS + 6;
  APR_TIMEUP             = APR_OS_START_STATUS + 7;
  APR_INCOMPLETE         = APR_OS_START_STATUS + 8;
  APR_BADCH              = APR_OS_START_STATUS + 12;
  APR_BADARG             = APR_OS_START_STATUS + 13;
  APR_EOF                = APR_OS_START_STATUS + 14;
  APR_NOTFOUND           = APR_OS_START_STATUS + 15;
  APR_ANONYMOUS          = APR_OS_START_STATUS + 19;
  APR_FILEBASED          = APR_OS_START_STATUS + 20;
  APR_KEYBASED           = APR_OS_START_STATUS + 21;
  APR_EINIT              = APR_OS_START_STATUS + 22;
  APR_ENOTIMPL           = APR_OS_START_STATUS + 23;
  APR_EMISMATCH          = APR_OS_START_STATUS + 24;
  APR_EBUSY              = APR_OS_START_STATUS + 25;

  APR_EACCES             = APR_OS_START_CANONERR + 1;
  APR_EEXIST             = APR_OS_START_CANONERR + 2;
  APR_ENAMETOOLONG       = APR_OS_START_CANONERR + 3;
  APR_ENOENT             = APR_OS_START_CANONERR + 4;
  APR_ENOTDIR            = APR_OS_START_CANONERR + 5;
  APR_ENOSPC             = APR_OS_START_CANONERR + 6;
  APR_ENOMEM             = APR_OS_START_CANONERR + 7;
  APR_EMFILE             = APR_OS_START_CANONERR + 8;
  APR_ENFILE             = APR_OS_START_CANONERR + 9;
  APR_EBADF              = APR_OS_START_CANONERR + 10;
  APR_EINVAL             = APR_OS_START_CANONERR + 11;
  APR_ESPIPE             = APR_OS_START_CANONERR + 12;
  APR_EAGAIN             = APR_OS_START_CANONERR + 13;
  APR_EINTR              = APR_OS_START_CANONERR + 14;
  APR_ENOTSOCK           = APR_OS_START_CANONERR + 15;
  APR_ECONNREFUSED       = APR_OS_START_CANONERR + 16;
  APR_EINPROGRESS        = APR_OS_START_CANONERR + 17;
  APR_ECONNABORTED       = APR_OS_START_CANONERR + 18;
  APR_ECONNRESET         = APR_OS_START_CANONERR + 19;
  APR_ETIMEDOUT          = APR_OS_START_CANONERR + 20;
  APR_EHOSTUNREACH       = APR_OS_START_CANONERR + 21;
  APR_ENETUNREACH        = APR_OS_START_CANONERR + 22;
  APR_EFTYPE             = APR_OS_START_CANONERR + 23;
  APR_EPIPE              = APR_OS_START_CANONERR + 24;
  APR_EXDEV              = APR_OS_START_CANONERR + 25;
  APR_ENOTEMPTY          = APR_OS_START_CANONERR + 26;

//----- apr_errno.h ----------------------------------------------------------------------------------------------------

//----- apr_version.h --------------------------------------------------------------------------------------------------

const
  AprMajorVersion = 1;
  AprMinorVersion = 2;
  AprPatchVersion = 12;

type
  TAprVersion = packed record
    Major: Integer;
    Minor: Integer;
    Patch: Integer;
    IsDev: LongBool;
  end;

var
  apr_version: procedure(out pvsn: TAprVersion); stdcall;
  apr_version_string: function: PAnsiChar; stdcall;

//----- apr_version.h --------------------------------------------------------------------------------------------------

//----- apr_arch_utf8.h ------------------------------------------------------------------------------------------------

type
  PAprWChar = ^TAprWChar;
  TAprWChar = WideChar;

var
  apr_conv_utf8_to_ucs2: function(_in: PAnsiChar; var inbytes: TAprSize; _out: PAprWChar; var outwords: TAprSize): TAprStatus;
    stdcall;
  apr_conv_ucs2_to_utf8: function(_in: PAprWChar; var inwords: TAprSize; _out: PAnsiChar; var outbytes: TAprSize): TAprStatus;
    stdcall;

//----- apr_arch_utf8.h ------------------------------------------------------------------------------------------------

//----- apr_lib.h ------------------------------------------------------------------------------------------------------

const
  HUGE_STRING_LEN = 8192;

type
  PAprVFormatterBuf = ^TAprVFormatterBuf;
  TAprVFormatterBuf = packed record
    curpos: PAnsiChar;
    endpos: PAnsiChar;
  end;
  TFlushFunc = function(b: PAprVFormatterBuf): Integer; stdcall;

var
  apr_filepath_name_get: function(pathname: PAnsiChar): PAnsiChar; stdcall;
  apr_vformatter: function(flush_func: TFlushFunc; c: PAprVFormatterBuf; fmt: PAnsiChar; ap: array of const): Integer;
    stdcall;
  apr_password_get: function(prompt, pwbuf: PAnsiChar; var bufsize: TAprSize): TAprStatus; stdcall;

//----- apr_lib.h ------------------------------------------------------------------------------------------------------

//----- apr_general.h --------------------------------------------------------------------------------------------------

var
  apr_initialize: function: TAprStatus; stdcall;
  apr_app_initialize: function(argc: Integer; argv, env: array of PAnsiChar): TAprStatus; stdcall;
  apr_terminate: procedure; stdcall;
  apr_terminate2: procedure; stdcall;
  apr_generate_random_bytes: function(buf: PByte; length: TAprSize): TAprStatus; stdcall;

//----- apr_general.h --------------------------------------------------------------------------------------------------

//----- apr_pools.h ----------------------------------------------------------------------------------------------------

type
  PAprPool = ^TAprPool;
  TAprPool = THandle;
  PAprAllocator = ^TAprAllocator;
  TAprAllocator = THandle;
  TAprAbortFunc = function(retcode: Integer): Integer; stdcall;
  TUserDataCleanup = function(p: Pointer): TAprStatus; stdcall;

var
  apr_pool_initialize: function: TAprStatus; stdcall;
  apr_pool_terminate: procedure; stdcall;
  apr_pool_create_ex: function(out newpool: PAprPool; parent: PAprPool; abort_fn: TAprAbortFunc;
    allocator: PAprAllocator): TAprStatus; stdcall;
  apr_pool_create_ex_debug: function(out newpool: PAprPool; parent: PAprPool; abort_fn: TAprAbortFunc;
    allocator: PAprAllocator; FileLine: PAnsiChar): TAprStatus; stdcall;
  apr_pool_allocator_get: function(pool: PAprPool): PAprAllocator; stdcall;
  apr_pool_clear: procedure(p: PAprPool); stdcall;
  apr_pool_clear_debug: procedure(p: PAprPool; file_line: PAnsiChar); stdcall;
  apr_pool_destroy: procedure(p: PAprPool); stdcall;
  apr_pool_destroy_debug: procedure(p: PAprPool; file_line: PAnsiChar); stdcall;
  apr_palloc: function(p: PAprPool; size: TAprSize): Pointer; stdcall;
  apr_palloc_debug: function(p: PAprPool; size: TAprSize; file_line: PAnsiChar): Pointer; stdcall;
  apr_pcalloc: function(p: PAprPool; size: TAprSize): Pointer; stdcall;
  apr_pcalloc_debug: function(p: PAprPool; size: TAprSize; file_line: PAnsiChar): Pointer;
  apr_pool_abort_set: procedure(abortfunc: TAprAbortFunc; pool: PAprPool); stdcall;
  apr_pool_abort_get: function(pool: PAprPool): TAprAbortFunc; stdcall;
  apr_pool_parent_get: function(pool: PAprPool): PAprPool; stdcall;
  apr_pool_is_ancestor: function(a, b: PAprPool): LongBool; stdcall;
  apr_pool_tag: procedure(pool: PAprPool; tag: PAnsiChar); stdcall;
  apr_pool_userdata_set: function(data: Pointer; key: PAnsiChar; cleanup: TUserDataCleanup;
    pool: PAprPool): TAprStatus; stdcall;
  apr_pool_userdata_setn: function(data: Pointer; key: PAnsiChar; cleanup: TUserDataCleanup;
    pool: PAprPool): TAprStatus; stdcall;
  apr_pool_userdata_get: function(out data: Pointer; key: PAnsiChar; pool: PAprPool): TAprStatus; stdcall;
  apr_pool_cleanup_register: procedure(p: PAprPool; data: Pointer;
    plain_cleanup, child_cleanup: TUserDataCleanup); stdcall;
  apr_pool_cleanup_kill: procedure(p: PAprPool; data: Pointer; cleanup: TUserDataCleanup); stdcall;
  apr_pool_child_cleanup_set: procedure(p: PAprPool; data: Pointer;
    plain_cleanup, child_cleanup: TUserDataCleanup); stdcall;
  apr_pool_cleanup_run: function(p: PAprPool; data: Pointer; cleanup: TUserDataCleanup): TAprStatus; stdcall;
  apr_pool_cleanup_null: function(data: Pointer): TAprStatus; stdcall;
  apr_pool_cleanup_for_exec: procedure; stdcall;

//----- apr_pools.h ----------------------------------------------------------------------------------------------------

//----- apr_atomic.h ---------------------------------------------------------------------------------------------------

var
  apr_atomic_init: function(p: PAprPool): TAprStatus; stdcall;
  apr_atomic_read32: function(mem: PCardinal): Cardinal; stdcall;
  apr_atomic_set32: procedure(mem: PCardinal; val: Cardinal); stdcall;
  apr_atomic_add32: function(mem: PCardinal; val: Cardinal): Cardinal; stdcall;
  apr_atomic_sub32: function(mem: PCardinal; val: Cardinal): Cardinal; stdcall;
  apr_atomic_inc32: function(mem: PCardinal): Cardinal; stdcall;
  apr_atomic_dec32: function(mem: PCardinal): Integer; stdcall;
  apr_atomic_cas32: function(mem: PCardinal; _with, cmp: Cardinal): Cardinal; stdcall;
  apr_atomic_xchg32: function(mem: PCardinal; val: Cardinal): Cardinal; stdcall;
  apr_atomic_casptr: function(mem, _with, cmp: Pointer): Pointer; stdcall;

//----- apr_atomic.h ---------------------------------------------------------------------------------------------------

//----- apr_random.h ---------------------------------------------------------------------------------------------------

type
  TAprCryptoHashInit = procedure(hash: Pointer); cdecl;
  TAprCryptoHashAdd = procedure(hash: Pointer; data: Pointer; bytes: TAprSize); cdecl;
  TAprCryptoHashFinish = procedure(hash: Pointer; result: Pointer); cdecl;

  PAprCryptoHash = ^TAprCryptoHash;
  TAprCryptoHash = packed record
    init: TAprCryptoHashInit;
    add: TAprCryptoHashAdd;
    finish: TAprCryptoHashFinish;
    size: TAprSize;
    data: Pointer;
  end;

var
  apr_crypto_sha256_new: function(p: PAprPool): PAprCryptoHash; stdcall;

type
  PAprRandom = ^TAprRandom;
  TAprRandom = THandle;

var
  apr_random_init: procedure(g: PAprRandom; p: PAprPool; pool_hash, key_hash, prng_hash: PAprCryptoHash); stdcall;
  apr_random_standard_new: function(p: PAprPool): PAprRandom; stdcall;
  apr_random_add_entropy: procedure(g: PAprRandom; entropy_: Pointer; bytes: TAprSize); stdcall;
  apr_random_insecure_bytes: function(g: PAprRandom; random: Pointer; bytes: TAprSize): TAprStatus; stdcall;
  apr_random_secure_bytes: function(g: PAprRandom; random: Pointer; bytes: TAprSize): TAprStatus; stdcall;
  apr_random_barrier: procedure(g: PAprRandom); stdcall;
  apr_random_secure_ready: function(r: PAprRandom): TAprStatus; stdcall;
  apr_random_insecure_ready: function(r: PAprRandom): TAprStatus; stdcall;
  apr_random_after_fork: procedure(proc: Pointer); stdcall;

//----- apr_random.h ---------------------------------------------------------------------------------------------------

//----- apr_signal.h ---------------------------------------------------------------------------------------------------

var
  apr_signal_block: function(signum: Integer): TAprStatus; stdcall;
  apr_signal_unblock: function(signum: Integer): TAprStatus; stdcall;

//----- apr_signal.h ---------------------------------------------------------------------------------------------------

//----- apr_strings.h --------------------------------------------------------------------------------------------------

var
  apr_strnatcmp: function(a, b: PAnsiChar): Integer; stdcall;
  apr_strnatcasecmp: function(a, b: PAnsiChar): Integer; stdcall;
  apr_pstrdup: function(p: PAprPool; s: PAnsiChar): PAnsiChar; stdcall;
  apr_pstrmemdup: function(p: PAprPool; s: PAnsiChar; n: TAprSize): PAnsiChar; stdcall;
  apr_pstrndup: function(p: PAprPool; s: PAnsiChar; n: TAprSize): PAnsiChar; stdcall;
  apr_pmemdup: function(p: PAprPool; m: Pointer; n: TAprSize): Pointer; stdcall;
  apr_pstrcat: function(p: PAprPool; args: array of const): PAnsiChar; stdcall;
  apr_pstrcatv: function(p: PAprPool; vec: PIOVec; nvec: TAprSize; out nbytes: TAprSize): PAnsiChar; stdcall;
  apr_pvsprintf: function(p: PAprPool; fmt: PAnsiChar; ap: array of const): PAnsiChar; stdcall;
  apr_psprintf: function(p: PAprPool; fmt: PAnsiChar; args: array of const): PAnsiChar; stdcall;
  apr_cpystrn: function (dst, src: PAnsiChar; dst_size: TAprSize): PAnsiChar; stdcall;
  apr_collapse_spaces: function(dest, src: PAnsiChar): PAnsiChar; stdcall;
  apr_tokenize_to_argv: function(arg_str: PAnsiChar; out argv_out: Pointer; token_context: PAprPool): TAprStatus; stdcall;
  apr_strtok: function(str, sep: PAnsiChar; var last: PAnsiChar): PAnsiChar; stdcall;
  apr_snprintf: function(buf: PAnsiChar; len: TAprSize; format: PAnsiChar; args: array of const): Integer; stdcall;
  apr_vsnprintf: function(buf: PAnsiChar; len: TAprSize; format: PAnsiChar; ap: array of const): Integer; stdcall;
  apr_itoa: function(p: PAprPOol; n: Integer): PAnsiChar; stdcall;
  apr_ltoa: function(p: PAprPool; n: Longint): PAnsiChar; stdcall;
  apr_off_t_toa: function(p: PAprPool; n: TAprOff): PAnsiChar; stdcall;
  apr_strtoff: function(var offset: TAprOff; buf: PAnsiChar; cend: PPAnsiChar; base: Integer): Integer; stdcall;
  apr_strtoi64: function(buf: PAnsiChar; pend: PPAnsiChar; base: Integer): Int64; stdcall;
  apr_atoi64: function(buf: PAnsiChar): Int64; stdcall;
  apr_strfsize: function(size: TAprOff; buf: PAnsiChar): PAnsiChar; stdcall;

//----- apr_strings.h --------------------------------------------------------------------------------------------------

//----- apr_shm.h ------------------------------------------------------------------------------------------------------

type
  PAprShm = ^TAprShm;
  TAprShm = THandle;

var
  apr_shm_create: function(out m: PAprShm; reqsize: TAprSize; filename: PAnsiChar; pool: PAprPool): TAprStatus; stdcall;
  apr_shm_remove: function(filename: PAnsiChar; pool: PAprPool): TAprStatus; stdcall;
  apr_shm_destroy: function(m: PAprShm): TAprStatus; stdcall;
  apr_shm_attach: function(out m: PAprShm; filename: PAnsiChar; pool: PAprPool): TAprStatus; stdcall;
  apr_shm_detach: function(m: PAprShm): TAprStatus; stdcall;
  apr_shm_baseaddr_get: function(m: PAprShm): Pointer; stdcall;
  apr_shm_size_get: function(m: PAprShm): TAprSize; stdcall;
  apr_shm_pool_get: function(theshm: PAprShm): PAprPool; stdcall;

//----- apr_shm.h ------------------------------------------------------------------------------------------------------

//----- apr_hash.h -----------------------------------------------------------------------------------------------------

const
  APR_HASH_KEY_STRING     = -1;

type
  PAprHash = ^TAprHash;
  TAprHash = THandle;
  PAprHashIndex = ^TAprHashIndex;
  TAprHashIndex = THandle;
  TAprHashFunc = function(key: PAnsiChar; var klen: TAprSize): Cardinal; cdecl;
  THashMergeFunc = function(p: PAprPool; key: Pointer; klen: TAprSize; h1_val, h2_val, data: Pointer): Pointer; stdcall;

var
  apr_hashfunc_default: TAprHashFunc;
  apr_hash_make: function(pool: PAprPool): PAprHash; stdcall;
  apr_hash_make_custom: function(pool: PAprPool; hash_func: TAprHashFunc): PAprHash; stdcall;
  apr_hash_copy: function(pool: PAprPool; h: PAprHash): PAprHash; stdcall;
  apr_hash_set: procedure(ht: PAprHash; key: Pointer; klen: TAprSize; val: Pointer); stdcall;
  apr_hash_get: function(ht: PAprHash; key: Pointer; klen: TAprSize): Pointer; stdcall;
  apr_hash_first: function(p: PAprPool; ht: PAprHash): PAprHashIndex; stdcall;
  apr_hash_next: function(hi: PAprHashIndex): PAprHashIndex; stdcall;
  apr_hash_this: procedure(hi: PAprHashIndex; key: Pointer; klen: TAprSize; val: Pointer); stdcall;
  apr_hash_count: function(ht: PAprHash): Cardinal; stdcall;
  apr_hash_overlay: function(p: PAprPool; overlay, base: PAprHash): PAprHash; stdcall;
  apr_hash_merge: function(p: PAprPool; h1, h2: PAprHash; merger: THashMergeFunc; data: Pointer): PAprHash; stdcall;
  apr_hash_pool_get: function(thehash: PAprHash): PAprPool; stdcall;

//----- apr_hash.h -----------------------------------------------------------------------------------------------------

//----- apr_tables.h ---------------------------------------------------------------------------------------------------

const
  APR_OVERLAP_TABLES_SET = 0;
  APR_OVERLAP_TABLES_MERGE = 1;

type
  PAprTable = ^TAprTable;
  TAprTable = THandle;
  PPAprArrayHeader = ^PAprArrayHeader;
  PAprArrayHeader = ^TAprArrayHeader;
  TAprArrayHeader = packed record
    pool: PAprPool;
    elt_size: Integer;
    nelts: Integer;
    nalloc: Integer;
    elts: PAnsiChar;
  end;
  TAprTableEntry = packed record
    key: PAnsiChar;
    val: PAnsiChar;
    key_checksum: Cardinal;
  end;
  TTableCallback = function(rec: Pointer; key, value: PAnsiChar): Integer; stdcall;

var
  apr_table_elts: function(t: PAprTable): PAprArrayHeader; stdcall;
  apr_is_empty_table: function(t: PAprTable): LongBool; stdcall;
  apr_is_empty_array: function(a: PAprArrayHeader): LongBool; stdcall;
  apr_array_make: function(p: PAprPool; nelts, elt_size: Integer): PAprArrayHeader; stdcall;
  apr_array_push: function(arr: PAprArrayHeader): Pointer; stdcall;
  apr_array_pop: function(arr: PAprArrayHeader): Pointer; stdcall;
  apr_array_cat: procedure(dst, src: PAprArrayHeader); stdcall;
  apr_array_copy: function(p: PAprPool; arr: PAprArrayHeader): PAprArrayHeader; stdcall;
  apr_array_copy_hdr: function(p: PAprPool; arr: PAprArrayHeader):PAprArrayHeader; stdcall;
  apr_array_append: function(p: PAprPool; first, second: PAprArrayHeader): PAprArrayHeader; stdcall;
  apr_array_pstrcat: function(p: PAprPool; arr: PAprArrayHeader; sep: Char): PAnsiChar; stdcall;
  apr_table_make: function(p: PAprPool; nelts: Integer): PAprTable; stdcall;
  apr_table_copy: function(p: PAprPool; t: PAprTable): PAprTable; stdcall;
  apr_table_clear: procedure(t: PAprTable); stdcall;
  apr_table_get: function(t: PAprTable; key: PAnsiChar): PAnsiChar; stdcall;
  apr_table_set: procedure(t: PAprTable; key, val: PAnsiChar); stdcall;
  apr_table_setn: procedure(t: PAprTable; key, val: PAnsiChar); stdcall;
  apr_table_unset: procedure(t: PAprTable; key: PAnsiChar); stdcall;
  apr_table_merge: procedure(t: PAprTable; key, val: PAnsiChar); stdcall;
  apr_table_mergen: procedure(t: PAprTable; key, val: PAnsiChar); stdcall;
  apr_table_add: procedure(t: PAprTable; key, val: PAnsiChar); stdcall;
  apr_table_addn: procedure(t: PAprTable; key, val: PAnsiChar); stdcall;
  apr_table_overlay: function(p: PAprPool; overlay, base: PAprTable): PAprTable; stdcall;
  apr_table_do: function(comp: TTableCallback; rec: Pointer; t: PAprTable; args: array of const): Integer; stdcall;
  apr_table_vdo: function(comp: TTableCallback; rec: Pointer; t: PAprTable; vp: array of const): Integer; stdcall;
  apr_table_overlap: procedure(a, b: PAprTable; flags: Cardinal); stdcall;
  apr_table_compress: procedure(t: PaprTable; flags: Cardinal); stdcall;

//----- apr_tables.h ---------------------------------------------------------------------------------------------------

//----- apr_env.h ------------------------------------------------------------------------------------------------------

var
  apr_env_get: function(out value: PAnsiChar; envvar: PAnsiChar; pool: PAprPool): TAprStatus; stdcall;
  apr_env_set: function(envvar, value: PAnsiChar; pool: PAprPool): TAprStatus; stdcall;
  apr_env_delete: function(envvar: PAnsiChar; pool: PAprPool): TAprStatus; stdcall;

//----- apr_env.h ------------------------------------------------------------------------------------------------------

//----- apr_getopt.h ---------------------------------------------------------------------------------------------------

type
  TGetOptErrorProc = procedure(arg: Pointer; err: PAnsiChar; args: array of const); stdcall;
  PAprGetOpt = ^TAprGetOpt;
  TAprGetOpt = packed record
    cont: PAprPool;
    errfn: TGetOptErrorProc;
    errarg: Pointer;
    ind: Integer;
    opt: Integer;
    reset: Integer;
    argc: Integer;
    argv: PPAnsiChar;
    place: PAnsiChar;
    interleave: Integer;
    skip_start: Integer;
    skip_end: Integer;
  end;
  PAprGetOptOption = ^TAprGetOptOption;
  TAprGetOptOption = packed record
    name: PAnsiChar;
    optch: Integer;
    has_arg: LongBool;
    description: PAnsiChar;
  end;

var
  apr_getopt_init: function(out os: PAprGetOpt; cont: PAprPool; argc: Integer; argv: PPAnsiChar): TAprStatus; stdcall;
  apr_getopt: function(os: PAprGetOpt; opts, option_ch: PAnsiChar; option_arg: PPAnsiChar): TAprStatus; stdcall;
  apr_getopt_long: function(os: PAprGetOpt; opts: PAprGetOptOption; out option_ch: Integer;
    option_arg: PPAnsiChar): TAprStatus; stdcall;

//----- apr_getopt.h ---------------------------------------------------------------------------------------------------

//----- apr_dso.h ------------------------------------------------------------------------------------------------------

type
  PAprDSOHandle = ^TAprDSOHandle;
  TAprDSOHandle = THandle;
  TAprDSOHandleSym = Pointer;

var
  apr_dso_load: function(out res_handle: PAprDSOHandle; path: PAnsiChar; ctx: PAprPool): TAprStatus; stdcall;
  apr_dso_unload: function(handle: PAprDSOHandle): TAprStatus; stdcall;
  apr_dso_sym: function(out ressym: TAprDSOHandleSym; handle: PAprDSOHandle; symname: PAnsiChar): TAprStatus; stdcall;
  apr_dso_error: function(dso: PAprDSOHandle; buf: PAnsiChar; bufsize: TAprSize): PAnsiChar; stdcall;

//----- apr_dso.h ------------------------------------------------------------------------------------------------------

//----- apr_user.h -----------------------------------------------------------------------------------------------------

type
  TAprUID = PSID;
  TAprGID = PSID;

var
  apr_uid_current: function(out userid: TAprUID; var groupid: TAprGID; p: PAprPool): TAprStatus; stdcall;
  apr_uid_name_get: function(out username: PAnsiChar; userid: TAprUID; p: PAprPool): TAprStatus; stdcall;
  apr_uid_get: function(out userid: TAprUID; out groupid: TAprGID; username: PAnsiChar; p: PAprPool): TAprStatus; stdcall;
  apr_uid_homepath_get: function(out dirname: PAnsiChar; username: PAnsiChar; p: PAprPool): TAprStatus; stdcall;
  apr_uid_compare: function(left, right: TAprUID): TAprStatus; stdcall;
  apr_gid_name_get: function(out groupname: PAnsiChar; groupid: TAprGID; p: PAprPool): TAprStatus; stdcall;
  apr_gid_get: function(out groupid: TAprGID; groupname: PAnsiChar; p: PAprPool): TAprStatus; stdcall;
  apr_gid_compare: function(left, right: TAprGID): TAprStatus; stdcall;

//----- apr_user.h -----------------------------------------------------------------------------------------------------

//----- apr_time.h -----------------------------------------------------------------------------------------------------

const
  APR_RFC822_DATE_LEN = 30;
  APR_CTIME_LEN = 25;

type
  TMonthSNames = array[0..11,0..3] of Char;
  TDaySNames = array[0..6,0..3] of Char;
  PAprTime = ^TAprTime;
  TAprTime = Int64;
  PAprIntervalTime = ^TAprIntervalTime;
  TAprIntervalTime = Int64;
  PAprShortIntervalTime = ^TAprShortIntervalTime;
  TAprShortIntervalTime = Int64;
  PAprTimeExp = ^TAprTimeExp;
  TAprTimeExp = packed record
    tm_usec: Integer;
    tm_sec: Integer;
    tm_min: Integer;
    tm_hour: Integer;
    tm_mday: Integer;
    tm_mon: Integer;
    tm_year: Integer;
    tm_wday: Integer;
    tm_yday: Integer;
    tm_isdst: Integer;
    tm_gmtoff: Integer;
  end;

const
  APR_USEC_PER_SEC: TAprTime = 1000000;

function apr_time_sec(time: TAprTime): TAprTime;
function apr_time_usec(time: TAprTime): TAprTime;
function apr_time_msec(time: TAprTime): TAprTime;
function apr_time_as_msec(time: TAprTime): TAprTime;
function apr_time_from_sec(sec: TAprTime): TAprTime;
function apr_time_make(sec, usec: TAprTime): TAprTime;

var
  apr_month_snames: function: TMonthSNames; stdcall;
  apr_day_snames: function: TDaySNames; stdcall;
  apr_time_now: function: TAprTime; stdcall;
  apr_time_ansi_put: function(out result: TAprTime; input: Longint): TAprStatus; stdcall;
  apr_time_exp_tz: function(out result: TAprTimeExp; input: TAprTime; offs: Integer): TAprStatus; stdcall;
  apr_time_exp_gmt: function(out result: TAprTimeExp; input: TAprTime): TAprStatus; stdcall;
  apr_time_exp_lt: function(out result: TAprTimeExp; input: TAprTime): TAprStatus; stdcall;
  apr_time_exp_get: function(out result: TAprTimeExp; input: PAprTimeExp): TAprStatus; stdcall;
  apr_time_exp_gmt_get: function(out result: TAprTime; input: PAprTimeExp): TAprStatus; stdcall;
  apr_sleep: procedure(t: TAprIntervalTime); stdcall;
  apr_rfc822_date: function(date_str: PAnsiChar; t: TAprTime): TAprStatus; stdcall;
  apr_ctime: function(date_str: PAnsiChar; t: TAprTime): TAprStatus; stdcall;
  apr_strftime: function(s: PAnsiChar; out retsize: TAprSize; max: TAprSize; format: PAnsiChar; tm: PAprTimeExp): TAprStatus;
    stdcall;
  apr_time_clock_hires: procedure(p: PAprPool); stdcall;

//----- apr_time.h -----------------------------------------------------------------------------------------------------

//----- apr_arch_time.h ------------------------------------------------------------------------------------------------

type
  TATime = packed record
    cntxt: PAprPool;
    currtime: TAprTime;
    explodedtime: PSystemTime;
  end;

const
  // Number of micro-seconds between the beginning of the Windows epoch (Jan. 1, 1601)
  // and the Unix epoch (Jan. 1, 1970)
  APR_DELTA_EPOCH_IN_USEC: TAprTime = 11644473600000000;

function FileTimeToAprTime(t: TFileTime): TAprTime; inline;
function AprTimeToFileTime(t: TAprTime): TFileTime; inline;

//----- apr_arch_time.h ------------------------------------------------------------------------------------------------

//----- apr_file_info.h ------------------------------------------------------------------------------------------------

const
  APR_FPROT_USETID      = $8000;
  APR_FPROT_UREAD       = $0400;
  APR_FPROT_UWRITE      = $0200;
  APR_FPROT_UEXECUTE    = $0100;

  APR_FPROT_GSETID      = $4000;
  APR_FPROT_GREAD       = $0040;
  APR_FPROT_GWRITE      = $0020;
  APR_FPROT_GEXECUTE    = $0010;

  APR_FPROT_WSTICKY     = $2000;
  APR_FPROT_WREAD       = $0004;
  APR_FPROT_WWRITE      = $0002;
  APR_FPROT_WEXECUTE    = $0001;

  APR_FPROT_OS_DEFAULT  = $0FFF;

  APR_FILE_SOURCE_PERMS = $1000;

  APR_FINFO_LINK   = $00000001;
  APR_FINFO_MTIME  = $00000010;
  APR_FINFO_CTIME  = $00000020;
  APR_FINFO_ATIME  = $00000040;
  APR_FINFO_SIZE   = $00000100;
  APR_FINFO_CSIZE  = $00000200;
  APR_FINFO_DEV    = $00001000;
  APR_FINFO_INODE  = $00002000;
  APR_FINFO_NLINK  = $00004000;
  APR_FINFO_TYPE   = $00008000;
  APR_FINFO_USER   = $00010000;
  APR_FINFO_GROUP  = $00020000;
  APR_FINFO_UPROT  = $00100000;
  APR_FINFO_GPROT  = $00200000;
  APR_FINFO_WPROT  = $00400000;
  APR_FINFO_ICASE  = $01000000;
  APR_FINFO_NAME   = $02000000;

  APR_FINFO_MIN    = $00008170;
  APR_FINFO_IDENT  = $00003000;
  APR_FINFO_OWNER  = $00030000;
  APR_FINFO_PROT   = $00700000;
  APR_FINFO_NORM   = $0073B170;
  APR_FINFO_DIRENT = $02000000;

  APR_FILEPATH_NOTABOVEROOT   = $01;
  APR_FILEPATH_SECUREROOTTEST = $02;
  APR_FILEPATH_SECUREROOT     = $03;
  APR_FILEPATH_NOTRELATIVE    = $04;
  APR_FILEPATH_NOTABSOLUTE    = $08;
  APR_FILEPATH_NATIVE         = $10;
  APR_FILEPATH_TRUENAME       = $20;

  APR_FILEPATH_ENCODING_UNKNOWN  = 0;
  APR_FILEPATH_ENCODING_LOCALE   = 1;
  APR_FILEPATH_ENCODING_UTF8     = 2;

type
  TAprFileType = (APR_NOFILE = 0, APR_REG, APR_DIR, APR_CHR, APR_BLK, APR_PIPE, APR_LNK, APR_SOCK, APR_UNKFILE = 127);
  PPAprFile = ^PAprFile;
  PAprFile = ^TAprFile;
  TAprFile = THandle;
  PAprDir = ^TAprDir;
  TAprDir = THandle;
  TAprFilePerms = Integer;
  TAprIno = Int64;
  TAprDev = THandle;
  PAprFInfo = ^TAprFInfo;
  TAprFInfo = packed record
    pool: PAprPool;
    valid: Integer;
    protection: TAprFilePerms;
    filetype: TAprFileType;
    user: TAprUID;
    group: TAprGID;
    inode: TAprIno;
    device: TAprDev;
    nlink: Integer;
    size: TAprOff;
    csize: TAprOff;
    atime: TAprTime;
    mtime: TAprTime;
    ctime: TAprTime;
    fname: PAnsiChar;
    name: PAnsiChar;
    filehand: PAprFile;
  end;

var
  apr_stat: function(out finfo: TAprFInfo; fname: PAnsiChar; wanted: Integer; pool: PAprPool): TAprStatus; stdcall;
  apr_dir_open: function(out new_dir: PAprDir; dirname: PAnsiChar; pool: PAprPool): TAprStatus; stdcall;
  apr_dir_close: function(thedir: PAprDir): TAprStatus; stdcall;
  apr_dir_read: function(finfo: PAprFInfo; wanted: Integer; thedir: PAprDir): TAprStatus; stdcall;
  apr_dir_rewind: function(thedir: PAprDir): TAprStatus; stdcall;
  apr_filepath_root: function(rootpath, filepath: PPAnsiChar; flags: Integer; p: PAprPool): TAprStatus; stdcall;
  apr_filepath_merge: function(out newpath: PAnsiChar; rootpath, addpath: PAnsiChar; flags: Integer;
    p: PAprPool): TAprStatus; stdcall;
  apr_filepath_list_split: function(out pathelts: PAprArrayHeader; liststr: PAnsiChar; p: PAprPool): TAprStatus; stdcall;
  apr_filepath_list_merge: function(out liststr: PAnsiChar; pathelts: PAprArrayHeader; p: PAprPool): TAprStatus; stdcall;
  apr_filepath_get: function(out path: PAnsiChar; flags: Integer; p: PAprPool): TAprStatus; stdcall;
  apr_filepath_set: function(path: PAnsiChar; p: PAprPool): TAprStatus; stdcall;
  apr_filepath_encoding: function(out style: Integer; p: PAprPool): TAprStatus; stdcall;

//----- apr_file_info.h ------------------------------------------------------------------------------------------------

//----- apr_file_io.h --------------------------------------------------------------------------------------------------

const
  APR_FOPEN_READ             = $00001;
  APR_FOPEN_WRITE            = $00002;
  APR_FOPEN_CREATE           = $00004;
  APR_FOPEN_APPEND           = $00008;
  APR_FOPEN_TRUNCATE         = $00010;
  APR_FOPEN_BINARY           = $00020;
  APR_FOPEN_EXCL             = $00040;
  APR_FOPEN_BUFFERED         = $00080;
  APR_FOPEN_DELONCLOSE       = $00100;
  APR_FOPEN_XTHREAD          = $00200;
  APR_FOPEN_SHARELOCK        = $00400;
  APR_FOPEN_FILE_NOCLEANUP   = $00800;
  APR_FOPEN_SENDFILE_ENABLED = $01000;
  APR_FOPEN_LARGEFILE        = $04000;

  APR_FILE_ATTR_READONLY   = $01;
  APR_FILE_ATTR_EXECUTABLE = $02;
  APR_FILE_ATTR_HIDDEN     = $04;

  APR_MAX_IOVEC_SIZE       = 1024;

  APR_FLOCK_SHARED        = 1;
  APR_FLOCK_EXCLUSIVE     = 2;

  APR_FLOCK_TYPEMASK      = $000F;
  APR_FLOCK_NONBLOCK      = $0010;

type
  TAprFileAttrs = Cardinal;
  TAprSeekWhere = Integer;
  TAprFileDataCleanup = function(p: Pointer): TAprStatus;

var
  apr_file_open: function(out newf: PAprFile; fname: PAnsiChar; flag: Integer; perm: TAprFilePerms;
    pool: PAprPool): TAprStatus; stdcall;
  apr_file_close: function(afile: PAprFile): TAprStatus; stdcall;
  apr_file_remove: function(path: PAnsiChar; pool: PAprPool): TAprStatus; stdcall;
  apr_file_rename: function(from_path, to_path: PAnsiChar; pool: PAprPool): TAprStatus; stdcall;
  apr_file_copy: function(from_path, to_path: PAnsiChar; perms: TAprFilePerms; pool: PAprPool): TAprStatus; stdcall;
  apr_file_append: function(from_path, to_path: PAnsiChar; perms: TAprFilePerms; pool: PAprPool): TAprStatus; stdcall;
  apr_file_eof: function(fptr: PAprFile): TAprStatus; stdcall;
  apr_file_open_stderr: function(out thefile: PAprFile; pool: PAprPool): TAprStatus; stdcall;
  apr_file_open_stdout: function(out thefile: PAprFile; pool: PAprPool): TAprStatus; stdcall;
  apr_file_open_stdin: function(out thefile: PAprFile; pool: PAprPool): TAprStatus; stdcall;
  apr_file_read: function(thefile: PAprFile; buf: Pointer; var nbytes: TAprSize): TAprStatus; stdcall;
  apr_file_write: function(thefile: PAprFile; buf: Pointer; var nbytes: TAprSize): TAprStatus; stdcall;
  apr_file_writev: function(thefile: PAprFile; vec: PIOVec; nvec: TAprSize; var nbytes: TAprSize): TAprStatus; stdcall;
  apr_file_read_full: function(thefile: PAprFile; buf: Pointer; nbytes: TAprSize;
    bytes_read: PAprSize): TAprStatus; stdcall;
  apr_file_write_full: function(thefile: PAprFile; buf: Pointer; nbytes: TAprSize;
    bytes_written: PAprSize): TAprStatus; stdcall;
  apr_file_writev_full: function(thefile: PAprFile; vec: PIOVec; nvec: TAprSize; nbytes: PAprSize): TAprStatus; stdcall;
  apr_file_putc: function(ch: Char; thefile: PAprFile): TAprStatus; stdcall;
  apr_file_getc: function(out ch: Char; thefile: PAprFile): TAprStatus; stdcall;
  apr_file_ungetc: function(ch: Char; thefile: PAprFile): TAprStatus; stdcall;
  apr_file_gets: function(str: PAnsiChar; len: Integer; thefile: PAprFile): TAprStatus; stdcall;
  apr_file_puts: function(str: PAnsiChar; thefile: PAprFile): TAprStatus; stdcall;
  apr_file_flush: function(thefile: PAprFile): TAprStatus; stdcall;
  apr_file_dup: function(out new_file: PAprFile; old_file: PAprFile; p: PAprPool): TAprStatus; stdcall;
  apr_file_dup2: function(new_file, old_file: PAprFile; p: PAprPool): TAprStatus; stdcall;
  apr_file_setaside: function(out new_file: PAprFile; old_file: PAprFile; p: PAprPool): TAprStatus; stdcall;
  apr_file_seek: function(thefile: PAprFile; where: TAprSeekWhere; var offset: TAprOff): TAprStatus; stdcall;
  apr_file_pipe_create: function(var ain, aout: PPAprFile; pool: PAprPool): TAprStatus; stdcall;
  apr_file_namedpipe_create: function(filename: PAnsiChar; perm: TAprFilePerms; pool: PAprPool): TAprStatus; stdcall;
  apr_file_pipe_timeout_get: function(thepipe: PAprFile; out timeout: TAprIntervalTime): TAprStatus; stdcall;
  apr_file_pipe_timeout_set: function(thepipe: PAprFile; timeout: TAprIntervalTime): TAprStatus; stdcall;
  apr_file_lock: function(thefile: PAprFile; atype: Integer): TAprStatus; stdcall;
  apr_file_unlock: function(thefile: PAprFile): TAprStatus; stdcall;
  apr_file_name_get: function(out new_path: PAnsiChar; thefile: PAprFile): TAprStatus; stdcall;
  apr_file_data_get: function(out data: Pointer; key: PAnsiChar; afile: PAprFile): TAprStatus; stdcall;
  apr_file_data_set: function(afile: PAprFile; data: Pointer; key: PAnsiChar;
    cleanup: TAprFileDataCleanup): TAprStatus; stdcall;
  apr_file_printf: function(fptr: PAprFile; format: PAnsiChar; args: array of const): Integer; stdcall;
  apr_file_perms_set: function(fname: PAnsiChar; perms: TAprFilePerms): TAprStatus; stdcall;
  apr_file_attrs_set: function(fname: PAnsiChar; attributes, attr_mask: TAprFileAttrs; cont: PAprPool): TAprStatus; stdcall;
  apr_file_mtime_set: function(fname: PAnsiChar; mtime: TAprTime; pool: PAprPool): TAprStatus; stdcall;
  apr_dir_make: function(path: PAnsiChar; perm: TAprTime; cont: PAprPool): TAprStatus; stdcall;
  apr_dir_make_recursive: function(path: PAnsiChar; perm: TAprFilePerms; pool: PAprPool): TAprStatus; stdcall;
  apr_dir_remove: function(path: PAnsiChar; pool: PAprPool): TAprStatus; stdcall;
  apr_file_info_get: function(out finfo: TAprFInfo; wanted: Integer; thefile: PAprFile): TAprStatus; stdcall;
  apr_file_trunc: function(fp: PAprFile; offset: TAprOff): TAprStatus; stdcall;
  apr_file_flags_get: function(f: PAprFile): Integer; stdcall;
  apr_file_pool_get: function(f: PAprFile): PAprPool; stdcall;
  apr_file_inherit_set: function(thefile: PAprFile): TAprStatus; stdcall;
  apr_file_inherit_unset: function(thefile: PAprFile): TAprStatus; stdcall;
  apr_file_mktemp: function(out fp: PAprFile; templ: PAnsiChar; flags: Integer; p: PAprPool): TAprStatus; stdcall;
  apr_temp_dir_get: function(temp_dir: PPAnsiChar; p: PAprPool): TAprStatus; stdcall;
  
//----- apr_file_io.h --------------------------------------------------------------------------------------------------

//----- apr_mmap.h -----------------------------------------------------------------------------------------------------

const
  APR_MMAP_READ    = 1;
  APR_MMAP_WRITE   = 2;

type
  PAprMMap = ^TAprMMap;
  TAprMMapRingEntry = packed record
    next: PAprMMap;
    prev: PAprMMap;
  end;
  TAprMMap = packed record
    cntxt: PAprPool;
    mhandle: THandle;
    mv: Pointer;
    pstart: TAprOff;
    psize: TAprSize;
    poffset: TAprOff;
    mm: Pointer;
    size: TAprSize;
    link: TAprMMapRingEntry;
  end;

const
  APR_MMAP_THRESHOLD = 1;
  APR_MMAP_LIMIT = 4 * 1024 * 1024;

function AprMMapCandidate(filelength: Int64): Boolean;

var
  apr_mmap_create: function(out newmmap: PAprMMap; afile: PAprFile; offset: TAprOff; size: TAprSize; flag: Integer;
    cntxt: PAprPool): TAprStatus; stdcall;
  apr_mmap_dup: function(out new_mmap: PAprMMap; old_mmap: PAprMMap; p: PAprPool): TAprStatus; stdcall;
  apr_mmap_delete: function(mm: PAprMMap): TAprStatus; stdcall;
  apr_mmap_offset: function(out addr: Pointer; mm: PAprMMap; offset: TAprOff): TAprStatus; stdcall;

//----- apr_mmap.h -----------------------------------------------------------------------------------------------------

//----- apr_network_io.h -----------------------------------------------------------------------------------------------

const
  APR_MAX_SECS_TO_LINGER = 30;
  APRMAXHOSTLEN = 256;
  APR_ANYADDR = '0.0.0.0';

  APR_SO_LINGER         = 1;
  APR_SO_KEEPALIVE      = 2;
  APR_SO_DEBUG          = 4;
  APR_SO_NONBLOCK       = 8;
  APR_SO_REUSEADDR      = 16;
  APR_SO_SNDBUF         = 64;
  APR_SO_RCVBUF         = 128;
  APR_SO_DISCONNECTED   = 256;
  APR_TCP_NODELAY       = 512;
  APR_TCP_NOPUSH        = 1024;
  APR_RESET_NODELAY     = 2048;
  APR_INCOMPLETE_READ   = 4096;
  APR_INCOMPLETE_WRITE  = 8192;
  APR_IPV6_V6ONLY       = 16384;

  APR_TCP_DEFER_ACCEPT  = 32768;

  APR_IPV4_ADDR_OK  = $01;
  APR_IPV6_ADDR_OK  = $02;

  APR_INADDR_NONE = $FFFFFFFF;

  APR_PROTO_TCP       = 6;
  APR_PROTO_UDP       = 17;
  APR_PROTO_SCTP      = 132;

  APR_SENDFILE_DISCONNECT_SOCKET = 1;

type
  TAprShutdownHow = (APR_SHUTDOWN_READ, APR_SHUTDOWN_WRITE, APR_SHUTDOWN_READWRITE);
  TAprInterface = (APR_LOCAL, APR_REMOTE);

  PAprSocket = ^TAprSocket;
  TAprSocket = THandle;
  TAprPort = Word;

  PAprSockAddr = ^TAprSockAddr;
  TAprSockAddr = packed record
    pool: PAprPool;
    hostname: PAnsiChar;
    servname: PAnsiChar;
    port: TAprPort;
    family: Integer;
    salen: TAprSockLen;
    ipaddr_len: Integer;
    addr_str_len: Integer;
    ipaddr_ptr: Pointer;
    next: PAprSockAddr;
    sin: TSockAddrIn;
  end;
  PAprHdtr = ^TAprHdtr;
  TAprHdtr = packed record
    headers: PIOVec;
    numheaders: Integer;
    trailers: PIOVec;
    numtrailers: Integer;
  end;
  PAprIPSubnet = ^TAprIPSubnet;
  TAprIPSubnet = THandle;
  TSocketDataCleanup = function(p: Pointer): TAprStatus; stdcall;

var
  apr_socket_create: function(out new_sock: PAprSocket; family, atype: Integer; cont: PAprPool): TAprStatus; stdcall;
  apr_socket_shutdown: function(thesocket: PAprSocket; how: TAprShutdownHow): TAprStatus; stdcall;
  apr_socket_close: function(thesocket: PAprSocket): TAprStatus; stdcall;
  apr_socket_bind: function(sock: PAprSocket; sa: PAprSockAddr): TAprStatus; stdcall;
  apr_socket_listen: function(sock: PAprSocket; backlog: Integer): TAprStatus; stdcall;
  apr_socket_accept: function(out new_sock: PAprSocket; sock: PAprSocket;
    connection_pool: PAprPool): TAprStatus; stdcall;
  apr_socket_connect: function(sock: PAprSocket; sa: PAprSockAddr): TAprStatus; stdcall;
  apr_sockaddr_info_get: function(out sa: PAprSockAddr; hostname: PAnsiChar; family: Integer; port: TAprPort;
    flags: Integer; p: PAprPool): TAprStatus; stdcall;
  apr_getnameinfo: function(out hostname: PAnsiChar; sa: PAprSockAddr; flags: Integer): TAprPort; stdcall;
  apr_parse_addr_port: function(var addr, scope_id: PAnsiChar; var port: TAprPort; str: PAnsiChar;
    p: PAprPool): TAprStatus; stdcall;
  apr_gethostname: function(buf: PAnsiChar; len: Integer; cont: PAprPool): TAprStatus; stdcall;
  apr_socket_data_get: function(out data: Pointer; key: PAnsiChar; sock: PAprSocket): TAprStatus; stdcall;
  apr_socket_data_set: function(sock: PAprSocket; data: Pointer; key: PAnsiChar; cleanup: TSocketDataCleanup): TAprStatus;
    stdcall;
  apr_socket_send: function(sock: PAprSocket; buf: PChar; var len: TAprSize): TAprStatus; stdcall;
  apr_socket_sendv: function(sock: PAprSocket; vec: PIOVec; nvec: Integer; var len: TAprSize): TAprStatus; stdcall;
  apr_socket_sendto: function(sock: PAprSocket; where: PAprSockAddr; flags: Integer; buf: PAnsiChar;
    var len: TAprSize): TAprStatus; stdcall;
  apr_socket_recvfrom: function(out from: TAprSockAddr; sock: PAprSocket; flags: Integer; buf: PAnsiChar;
    var len: TAprSize): TAprStatus; stdcall;
  apr_socket_sendfile: function(sock: PAprSocket; afile: PAprFile; hdtr: PAprHdtr; offset: PAprOff; var len: TAprSize;
    flags: Integer): TAprStatus; stdcall;
  apr_socket_recv: function(sock: PAprSocket; buf: PAnsiChar; var len: TAprSize): TAprStatus; stdcall;
  apr_socket_opt_set: function(sock: PAprSocket; opt, aon: Integer): TAprStatus; stdcall;
  apr_socket_timeout_set: function(sock: PAprSocket; t: TAprIntervalTime): TAprStatus; stdcall;
  apr_socket_opt_get: function(sock: PAprSocket; opt: Integer; out aon: Integer): TAprStatus; stdcall;
  apr_socket_timeout_get: function(sock: PAprSocket; out t: TAprIntervalTime): TAprStatus; stdcall;
  apr_socket_atmark: function(sock: PAprSocket; out atmark: LongBool): TAprStatus; stdcall;
  apr_socket_addr_get: function(out sa: PAprSockAddr; which: TAprInterface; sock: PAprSocket): TAprStatus; stdcall;
  apr_sockaddr_ip_get: function(out addr: PAnsiChar; sockaddr: PAprSockAddr): TAprStatus; stdcall;
  apr_sockaddr_equal: function(addr1, addr2: PAprSockAddr): LongBool; stdcall;
  apr_socket_type_get: function(sock: PAprSocket; out _type: Integer): TAprStatus; stdcall;
  apr_getservbyname: function(sockaddr: PAprSockAddr; servname: PAnsiChar): TAprStatus; stdcall;
  apr_ipsubnet_create: function(out ipsub: PAprIPSubnet; ipstr, mask_or_numbits: PAnsiChar; p: PAprPool): TAprStatus;
    stdcall;
  apr_ipsubnet_test: function(ipsub: PAprIPSubnet; sa: PAprSockAddr): LongBool; stdcall;
  apr_socket_protocol_get: function(sock: PAprSocket; out protocol: Integer): TAprStatus; stdcall;
  apr_socket_inherit_set: function(thesocket: PAprSocket): TAprStatus; stdcall;
  apr_socket_set_inherit: procedure(skt: PAprSocket); stdcall;
  apr_socket_inherit_unset: function(thesocket: PAprSocket): TAprStatus; stdcall;
  apr_socket_unset_inherit: procedure(skt: PAprSocket); stdcall;
  apr_mcast_join: function(sock, join, iface, source: PAprSockAddr): TAprStatus; stdcall;
  apr_mcast_leave: function(sock, join, iface, source: PAprSockAddr): TAprStatus; stdcall;
  apr_mcast_hops: function(sock: PAprSocket; ttl: Byte): TAprStatus; stdcall;
  apr_mcast_loopback: function(sock: PAprSocket; opt: Byte): TAprStatus; stdcall;
  apr_mcast_interface: function(sock: PAprSocket; iface: PAprSockAddr): TAprStatus; stdcall;

//----- apr_network_io.h -----------------------------------------------------------------------------------------------

//----- apr_poll.h -----------------------------------------------------------------------------------------------------

const
  APR_POLLIN    = $001;
  APR_POLLPRI   = $002;
  APR_POLLOUT   = $004;
  APR_POLLERR   = $010;
  APR_POLLHUP   = $020;
  APR_POLLNVAL  = $040;

  APR_POLLSET_THREADSAFE = $001;

type
  TAprDataType = (APR_NO_DESC, APR_POLL_SOCKET, APR_POLL_FILE, APR_POLL_LASTDESC);
  TAprDescriptor = packed record
    case Boolean of
      False: (f: PAprFile);
      True: (s: PAprSocket);
  end;
  PPAprPollFD = ^PAprPollFD;
  PAprPollFD = ^TAprPollFD;
  TAprPollFD = packed record
    p: PAprPool;
    desc_type: TAprDataType;
    reqevents: Word;
    rtnevents: Word;
    desc: TAprDescriptor;
    client_data: Pointer;
  end;
  PAprPollSet = ^TAprPollSet;
  TAprPollSet = THandle;

var
  apr_pollset_create: function(out pollset: PAprPollSet; size: Integer; p: PAprPool; flags: Cardinal): TAprStatus;
    stdcall;
  apr_pollset_destroy: function(pollset: PAprPollSet): TAprStatus; stdcall;
  apr_pollset_add: function(pollset: PAprPollSet; descriptor: PAprPollFD): TAprStatus; stdcall;
  apr_pollset_remove: function(pollset: PAprPollSet; descriptor: PAprPollFD): TAprStatus; stdcall;
  apr_pollset_poll: function(pollset: PAprPollSet; timeout: TAprIntervalTime; out num: Integer;
    descriptors: PPAprPollFD): TAprStatus; stdcall;
  apr_poll: function(aprset: PAprPollFD; numsock: Longint; nsds: PLongint; timeout: TAprIntervalTime): TAprStatus;
    stdcall;

//----- apr_poll.h -----------------------------------------------------------------------------------------------------

//----- apr_thread_mutex.h ---------------------------------------------------------------------------------------------

const
  APR_THREAD_MUTEX_DEFAULT  = 0;   // platform-optimal lock behavior
  APR_THREAD_MUTEX_NESTED   = 1;   // enable nested (recursive) locks
  APR_THREAD_MUTEX_UNNESTED = 2;   // disable nested locks

type
  PAprThreadMutex = ^TAprThreadMutex;
  TAprThreadMutex = THandle;

var
  apr_thread_mutex_create: function(out mutex: PAprThreadMutex; flags: Cardinal; pool: PAprPool): TAprStatus; stdcall;
  apr_thread_mutex_lock: function(mutex: PAprThreadMutex): TAprStatus; stdcall;
  apr_thread_mutex_trylock: function(mutex: PAprThreadMutex): TAprStatus; stdcall;
  apr_thread_mutex_unlock: function(mutex: PAprThreadMutex): TAprStatus; stdcall;
  apr_thread_mutex_destroy: function(mutex: PAprThreadMutex): TAprStatus; stdcall;
  apr_thread_mutex_pool_get: function(mutex: PAprThreadMutex): PAprPool; stdcall;

//----- apr_thread_mutex.h ---------------------------------------------------------------------------------------------

//----- apr_thread_rwlock.h --------------------------------------------------------------------------------------------

type
  PAprThreadRWLock = ^TAprThreadRWLock;
  TAprThreadRWLock = THandle;

var
  apr_thread_rwlock_create: function(out rwlock: PAprThreadRWLock; pool: PAprPool): TAprStatus; stdcall;
  apr_thread_rwlock_rdlock: function(rwlock: PAprThreadRWLock): TAprStatus; stdcall;
  apr_thread_rwlock_tryrdlock: function(rwlock: PAprThreadRWLock): TAprStatus; stdcall;
  apr_thread_rwlock_wrlock: function(rwlock: PAprThreadRWLock): TAprStatus; stdcall;
  apr_thread_rwlock_trywrlock: function(rwlock: PAprThreadRWLock): TAprStatus; stdcall;
  apr_thread_rwlock_unlock: function(rwlock: PAprThreadRWLock): TAprStatus; stdcall;
  apr_thread_rwlock_destroy: function(rwlock: PAprThreadRWLock): TAprStatus; stdcall;
  apr_thread_rwlock_pool_get: function(rwlock: PAprThreadRWLock): PAprPool; stdcall;

//----- apr_thread_rwlock.h --------------------------------------------------------------------------------------------

//----- apr_thread_cond.h ----------------------------------------------------------------------------------------------

type
  PAprThreadCond = ^TAprThreadCond;
  TAprThreadCond = THandle;

var
  apr_thread_cond_create: function(out cond: PAprThreadCond; pool: PAprPool): TAprStatus; stdcall;
  apr_thread_cond_wait: function(cond: PAprThreadCond; mutex: PAprThreadMutex): TAprStatus; stdcall;
  apr_thread_cond_timedwait: function(cond: PAprThreadCond; mutex: PAprThreadMutex;
    timeout: TAprIntervalTime): TAprStatus; stdcall;
  apr_thread_cond_signal: function(cond: PAprThreadCond): TAprStatus; stdcall;
  apr_thread_cond_broadcast: function(cond: PAprThreadCond): TAprStatus; stdcall;
  apr_thread_cond_destroy: function(cond: PAprThreadCond): TAprStatus; stdcall;
  apr_thread_cond_pool_get: function(thread_cond: PAprThreadCond): PAprPool; stdcall;

//----- apr_thread_cond.h ----------------------------------------------------------------------------------------------

//----- apr_thread_proc.h ----------------------------------------------------------------------------------------------

const
  APR_NO_PIPE          = 0;

  APR_FULL_BLOCK       = 1;
  APR_FULL_NONBLOCK    = 2;
  APR_PARENT_BLOCK     = 3;
  APR_CHILD_BLOCK      = 4;

  APR_LIMIT_CPU        = 0;
  APR_LIMIT_MEM        = 1;
  APR_LIMIT_NPROC      = 2;
  APR_LIMIT_NOFILE     = 3;

  APR_OC_REASON_DEATH         = 0;
  APR_OC_REASON_UNWRITABLE    = 1;
  APR_OC_REASON_RESTART       = 2;
  APR_OC_REASON_UNREGISTER    = 3;
  APR_OC_REASON_LOST          = 4;
  APR_OC_REASON_RUNNING       = 5;

type
  TAprCmdType = (APR_SHELLCMD, APR_PROGRAM, APR_PROGRAM_ENV, APR_PROGRAM_PATH, APR_SHELLCMD_ENV);
  TAprWaitHow = (APR_WAIT, APR_NOWAIT);
  TAprExitWhy = (APR_PROC_EXIT = 1, APR_PROC_SIGNAL = 2, APR_PROC_SIGNAL_CORE = 4);
  PAprThread = ^TAprThread;
  TAprThread = THandle;
  PAprThreadAttr = ^TAprThreadAttr;
  TAprThreadAttr = THandle;
  PAprThreadOnce = ^TAprThreadOnce;
  TAprThreadOnce = THandle;
  PAprThreadKey = ^TAprThreadKey;
  TAprThreadKey = THandle;
  PAprProc = ^TAprProc;
  TAprProc = packed record
    pid: Cardinal;
    stdin: PAprFile;
    stdout: PAprFile;
    stderr: PAprFile;
    hproc: THandle;
  end;
  PAprProcAttr = ^TAprProcAttr;
  TAprProcAttr = THandle;
  PAprOtherChild = ^TAprOtherChild;
  TAprOtherChild = THandle;
  TAprChildErrFn = procedure(pool: PAprPool; err: TAprStatus; description: PAnsiChar); stdcall;
  TAprThreadStart = function(thread: PAprThread; data: Pointer): Pointer; stdcall;
  TAprKillConditions = (APR_KILL_NEVER, APR_KILL_ALWAYS, APR_KILL_AFTER_TIMEOUT, APR_JUST_WAIT, APR_KILL_ONLY_ONCE);
  TAprThreadOnceProc = procedure; stdcall;
  TAprThreadDataCleanup = function(p: Pointer): TAprStatus; stdcall;
  TAprThreadPrivateDest = procedure(p: Pointer); stdcall;
  TChildRegProc = procedure(reason: Integer; p: Pointer; status: Integer); stdcall;

var
  apr_threadattr_create: function(out new_attr: PAprThreadAttr; cont: PAprPool): TAprStatus; stdcall;
  apr_threadattr_detach_set: function(attr: PAprThreadAttr; aon: LongBool): TAprStatus; stdcall;
  apr_threadattr_detach_get: function(attr: PAprThreadAttr): TAprStatus; stdcall;
  apr_threadattr_stacksize_set: function(attr: PAprThreadAttr; stacksize: TAprSize): TAprStatus; stdcall;
  apr_threadattr_guardsize_set: function(attr: PAprThreadAttr; guardsize: TAprSize): TAprStatus; stdcall;
  apr_thread_create: function(out new_thread: PAprThread; attr: PAprThreadAttr; func: TAprThreadStart; data: Pointer;
    cont: PAprPool): TAprStatus; stdcall;
  apr_thread_exit: function(thd: PAprThread; retval: TAprStatus): TAprStatus; stdcall;
  apr_thread_join: function(out retval: TAprStatus; thd: PAprThread): TAprStatus; stdcall;
  apr_thread_yield: procedure; stdcall;
  apr_thread_once_init: function(out control: PAprThreadOnce; p: PAprPool): TAprStatus; stdcall;
  apr_thread_once: function(control: PAprThreadOnce; func: TAprThreadOnceProc): TAprStatus; stdcall;
  apr_thread_detach: function(thd: PAprThread): TAprStatus; stdcall;
  apr_thread_data_get: function(out data: Pointer; key: PAnsiChar; thread: PAprThread): TAprStatus; stdcall;
  apr_thread_data_set: function(data: Pointer; key: PAnsiChar; cleanup: TAprThreadDataCleanup;
    thread: PAprThread): TAprStatus; stdcall;
  apr_threadkey_private_create: function(out key: PAprThreadKey; dest: TAprThreadPrivateDest;
    cont: PAprPool): TAprStatus; stdcall;
  apr_threadkey_private_get: function(out new_mem: Pointer; key: PAprThreadKey): TAprStatus; stdcall;
  apr_threadkey_private_set: function(priv: Pointer; key: PAprThreadKey): TAprStatus; stdcall;
  apr_threadkey_private_delete: function(key: PAprThreadKey): TAprStatus; stdcall;
  apr_threadkey_data_get: function (out data: Pointer; key: PAnsiChar; threadkey: PAprThreadKey): TAprStatus; stdcall;
  apr_threadkey_data_set: function(data: Pointer; key: PAnsiChar; cleanup: TAprThreadDataCleanup;
    threadkey: PAprThreadKey): TAprStatus; stdcall;
  apr_procattr_create: function(out new_attr: PAprProcAttr; cont: PAprPool): TAprStatus; stdcall;
  apr_procattr_io_set: function(attr: PAprProcAttr; stdin, stdout, stderr: Integer): TAprStatus; stdcall;
  apr_procattr_child_in_set: function(attr: PAprProcAttr; child_in, parent_in: PAprFile): TAprStatus; stdcall;
  apr_procattr_child_out_set: function(attr: PAprProcAttr; child_out, parent_out: PAprFile): TAprStatus; stdcall;
  apr_procattr_child_err_set: function(attr: PAprProcAttr; child_err, parent_err: PAprFile): TAprStatus; stdcall;
  apr_procattr_dir_set: function(attr: PAprProcAttr; dir: PAnsiChar): TAprStatus; stdcall;
  apr_procattr_cmdtype_set: function(attr: PAprProcAttr; cmd: TAprCmdType): TAprStatus; stdcall;
  apr_procattr_detach_set: function(attr: PAprProcAttr; detach: LongBool): TAprStatus; stdcall;
  apr_procattr_child_errfn_set: function(attr: PAprProcAttr; errfn: TAprChildErrFn): TAprStatus; stdcall;
  apr_procattr_error_check_set: function(attr: PAprProcAttr; chk: LongBool): TAprStatus; stdcall;
  apr_procattr_addrspace_set: function(attr: PAprProcAttr; addrspace: LongBool): TAprStatus; stdcall;
  apr_procattr_user_set: function(attr: PAprProcAttr; username, password: PAnsiChar): TAprStatus; stdcall;
  apr_procattr_group_set: function(attr: PAprProcAttr; groupname: PAnsiChar): TAprStatus; stdcall;
  apr_proc_create: function(out new_proc: TAprProc; progname, args, env: PAnsiChar; attr: PAprProcAttr;
    pool: PAprPool): TAprStatus; stdcall;
  apr_proc_wait: function(proc: PAprProc; out exitcode, exitwhy: Integer; waithow: TAprWaitHow): TAprStatus; stdcall;
  apr_proc_wait_all_procs: function(proc: PAprProc; out exitcode, exitwhy: Integer; waithow: TAprWaitHow;
    p: PAprPool): TAprStatus; stdcall;
  apr_proc_detach: function(daemonize: Integer): TAprStatus; stdcall;
  apr_proc_other_child_register: procedure(proc: PAprProc; maintenance: TChildRegProc; data: Pointer;
    write_fd: PAprFile; p: PAprPool); stdcall;
  apr_proc_other_child_unregister: procedure(data: Pointer); stdcall;
  apr_proc_other_child_alert: function(proc: PAprProc; reason, status: Integer): TAprStatus; stdcall;
  apr_proc_other_child_refresh: procedure(ocr: PAprOtherChild; reason: Integer); stdcall;
  apr_proc_other_child_refresh_all: procedure(reason: Integer); stdcall;
  apr_proc_kill: function(proc: PAprProc; sig: Integer): TAprStatus; stdcall;
  apr_pool_note_subprocess: procedure(a: PAprPool; proc: PAprProc; how: TAprKillConditions); stdcall;
  apr_thread_pool_get: function(thread: PAprThread): PAprPool; stdcall;

//----- apr_thread_proc.h ----------------------------------------------------------------------------------------------

//----- apr_proc_mutex.h -----------------------------------------------------------------------------------------------

type
  TAprLockMech = (APR_LOCK_FCNTL, APR_LOCK_FLOCK, APR_LOCK_SYSVSEM, APR_LOCK_PROC_PTHREAD, APR_LOCK_POSIXSEM,
    APR_LOCK_DEFAULT);
  PAprProcMutex = ^TAprProcMutex;
  TAprProcMutex = THandle;

var
  apr_proc_mutex_create: function(out mutex: PAprProcMutex; fname: PAnsiChar; mech: TAprLockMech;
    pool: PAprPool): TAprStatus; stdcall;
  apr_proc_mutex_child_init: function(out mutex: PAprProcMutex; fname: PAnsiChar; pool: PAprPool): TAprStatus; stdcall;
  apr_proc_mutex_lock: function(mutex: PAprProcMutex): TAprStatus; stdcall;
  apr_proc_mutex_trylock: function(mutex: PAprProcMutex): TAprStatus; stdcall;
  apr_proc_mutex_unlock: function(mutex: PAprProcMutex): TAprStatus; stdcall;
  apr_proc_mutex_destroy: function(mutex: PAprProcMutex): TAprStatus; stdcall;
  apr_proc_mutex_cleanup: function(mutex: Pointer): TAprStatus; stdcall;
  apr_proc_mutex_lockfile: function(mutex: PAprProcMutex): PAnsiChar; stdcall;
  apr_proc_mutex_name: function(mutex: PAprProcMutex): PAnsiChar; stdcall;
  apr_proc_mutex_defname: function: PAnsiChar; stdcall;
  apr_proc_mutex_pool_get: function(mutex: PAprProcMutex): PAprPool; stdcall;

//----- apr_proc_mutex.h -----------------------------------------------------------------------------------------------

//----- apr_allocator.h ------------------------------------------------------------------------------------------------

const
  APR_ALLOCATOR_MAX_FREE_UNLIMITED = 0;

type
  PPAprMemNode = ^PAprMemNode;
  PAprMemNode = ^TAprMemNode;
  TAprMemNode = packed record
    Next: PAprMemNode;   // next memnode
    Ref: PPAprMemNode;   // reference to self
    Index: Cardinal;     // size
    FreeIndex: Cardinal; // how much free
    FirstAvail: PAnsiChar;   // pointer to first free memory
    EndP: PAnsiChar;         // pointer to end of free memory
  end;

var
  apr_allocator_create: function(out allocator: PAprAllocator): TAprStatus; stdcall;
  apr_allocator_destroy: procedure(allocator: PAprAllocator); stdcall;
  apr_allocator_alloc: function(allocator: PAprAllocator; size: TAprSize): PAprMemNode; stdcall;
  apr_allocator_free: procedure(allocator: PAprAllocator; memnode: PAprMemNode); stdcall;
  apr_allocator_owner_set: procedure(allocator: PAprAllocator; pool: PAprPool); stdcall;
  apr_allocator_owner_get: function(allocator: PAprAllocator): PAprPool; stdcall;
  apr_allocator_max_free_set: procedure(allocator: PAprAllocator; size: TAprSize); stdcall;
  apr_allocator_mutex_set: procedure(allocator: PAprAllocator; mutex: PAprThreadMutex); stdcall;
  apr_allocator_mutex_get: function(allocator: PAprAllocator):PAprThreadMutex; stdcall;

//----- apr_allocator.h ------------------------------------------------------------------------------------------------

//----- apr_fnmatch.h --------------------------------------------------------------------------------------------------

var
  apr_fnmatch: function(pattern, strings: PAnsiChar; flags: Integer): TAprStatus; stdcall;
  apr_fnmatch_test: function(pattern: PAnsiChar): Integer; stdcall;
  apr_is_fnmatch: function(pattern: PAnsiChar): Integer; stdcall;
  apr_match_glob: function(pattern: PAnsiChar; out result: PAprArrayHeader; p: PAprPool): TAprStatus; stdcall;

//----- apr_fnmatch.h --------------------------------------------------------------------------------------------------

//----- apr_portable.h -------------------------------------------------------------------------------------------------

type
  PAprOSFile = ^TAprOSFile;
  TAprOSFile = THandle;
  PAprOSDir = ^TAprOSDir;
  TAprOSDir = THandle;
  PAprOSSock = ^TAprOSSock;
  TAprOSSock = TSocket;
  PAprOSProcMutex = ^TAprOSProcMutex;
  TAprOSProcMutex = THandle;
  PAprOSThread = ^TAprOSThread;
  TAprOSThread = THandle;
  PAprOSProc = ^TAprOSProc;
  TAprOSProc = THandle;
  PAprOSThreadKey = ^TAprOSThreadKey;
  TAprOSThreadKey = Cardinal;
  PPAprOSImpTime = ^PAprOSImpTime;
  PAprOSImpTime = ^TAprOSImpTime;
  TAprOSImpTime = TFileTime;
  PPAprOSExpTime = ^PAprOSExpTime;
  PAprOSExpTime = ^TAprOSExpTime;
  TAprOSExpTime = TSystemTime;
  PAprOSDSOHandle = ^TAprOSDSOHandle;
  TAprOSDSOHandle = THandle;
  PAprOSShm = ^TAprOSShm;
  TAprOSShm = THandle;

  PAprOSSockInfo = ^TAprOSSockInfo;
  TAprOSSockInfo = packed record
    os_sock: TAprOSSock;
    local: PSockAddr;
    remote: PSockAddr;
    family: Integer;
    stype: Integer;
    protocol: Integer;
  end;

  PAprOSGlobalMutex = ^TAprOSGlobalMutex;
  TAprOSGlobalMutex = TAprOSProcMutex;

var
  apr_os_file_get: function(out thefile: TAprOSFile; afile: PAprFile): TAprStatus; stdcall;
  apr_os_dir_get: function(out thedir: PAprOSDir; dir: PAprDir): TAprStatus; stdcall;
  apr_os_sock_get: function(out thesock: TAprOSSock; sock: PAprSocket): TAprStatus; stdcall;
  apr_os_proc_mutex_get: function(out ospmutex: TAprOSProcMutex; pmutex: PAprProcMutex): TAprStatus; stdcall;
  apr_os_exp_time_get: function(out ostime: PAprOSExpTime; aprtime: PAprTimeExp): TAprStatus; stdcall;
  apr_os_imp_time_get: function(out ostime: PAprOSImpTime; aprtime: PAprTime): TAprStatus; stdcall;
  apr_os_shm_get: function(out osshm: TAprOSShm; shm: PAprShm): TAprStatus; stdcall;
  apr_os_thread_get: function(out thethd: PAprOSThread; thd: PAprThread): TAprStatus; stdcall;
  apr_os_threadkey_get: function(out thekey: TAprOSThreadKey; key: PAprThreadKey): TAprStatus; stdcall;
  apr_os_thread_put: function(out thd: PAprThread; thethd: PAprOSThread; cont: PAprPool): TAprStatus; stdcall;
  apr_os_threadkey_put: function(out key: PAprThreadKey; thekey: PAprOSThreadKey; cont: PAprPool): TAprStatus; stdcall;
  apr_os_thread_current: function: TAprOSThread; stdcall;
  apr_os_thread_equal: function(tid1, tid2: TAprOSThread): LongBool; stdcall;
  apr_os_file_put: function(out f: PAprFile; thefile: PAprOSFile; flags: Integer; cont: PAprPool): TAprStatus; stdcall;
  apr_os_pipe_put: function(out f: PAprFile; thefile: PAprOSFile; cont: PAprPool): TAprStatus; stdcall;
  apr_os_pipe_put_ex: function(out f: PAprFile; thefile: PAprOSFile; register_cleanup: Integer;
    cont: PAprPool): TAprStatus; stdcall;
  apr_os_dir_put: function(out dir: PAprDir; thedir: PAprOSDir; cont: PAprPool): TAprStatus; stdcall;
  apr_os_sock_put: function(out sock: PAprSocket; thesock: PAprOSSock; cont: PAprPool): TAprStatus; stdcall;
  apr_os_sock_make: function(out apr_sock: PAprSocket; os_sock_info: PAprOSSockInfo; cont: PAprPool): TAprStatus;
    stdcall;
  apr_os_proc_mutex_put: function(out pmutex: PAprProcMutex; ospmutex: PAprOSProcMutex; cont: PAprPool): TAprStatus;
    stdcall;
  apr_os_imp_time_put: function(out aprtime: TAprTime; ostime: PPAprOSImpTime; cont: PAprPool): TAprStatus; stdcall;
  apr_os_exp_time_put: function(out aprtime: TAprTimeExp; ostime: PPAprOSExpTime; cont: PAprPool): TAprStatus; stdcall;
  apr_os_shm_put: function(out shm: PAprShm; osshm: PAprOSShm; cont: PAprPool): TAprStatus; stdcall;
  apr_os_dso_handle_put: function(out dso: PAprDSOHandle; thedso: TAprOSDSOHandle; pool: PAprPool): TAprStatus; stdcall;
  apr_os_dso_handle_get: function(out dso: PAprOSDSOHandle; aprdso: PAprDSOHandle): TAprStatus; stdcall;
  apr_os_uuid_get: function(uuid_data: Pointer): TAprStatus; stdcall;
  apr_os_default_encoding: function(pool: PAprPool): PAnsiChar; stdcall;
  apr_os_locale_encoding: function(pool: PAprPool): PAnsiChar; stdcall;

//----- apr_portable.h -------------------------------------------------------------------------------------------------

type
  EAprError = class(Exception)
  private
    FErrorCode: TAprStatus;
  public
    constructor Create(AErrorCode: TAprStatus);

    property ErrorCode: TAprStatus read FErrorCode;
  end;

function AprLibLoaded: Boolean;
function LoadAprLib(const FileName: string = ''): Boolean;
procedure FreeAprLib;

procedure AprCheck(Status: TAprStatus);
function GetAprErrorMessage(Status: TAprStatus): string;
procedure RaiseAprError(Status: TAprStatus);

implementation

var
  AprLib: THandle = INVALID_HANDLE_VALUE;

//----------------------------------------------------------------------------------------------------------------------

function apr_time_sec(time: TAprTime): TAprTime;

begin
  Result := time div APR_USEC_PER_SEC;
end;

//----------------------------------------------------------------------------------------------------------------------

function apr_time_usec(time: TAprTime): TAprTime;

begin
  Result := time mod APR_USEC_PER_SEC;
end;

//----------------------------------------------------------------------------------------------------------------------

function apr_time_msec(time: TAprTime): TAprTime;

begin
  Result := (time div 1000) mod 1000;
end;

//----------------------------------------------------------------------------------------------------------------------

function apr_time_as_msec(time: TAprTime): TAprTime;

begin
  Result := time div 1000;
end;

//----------------------------------------------------------------------------------------------------------------------

function apr_time_from_sec(sec: TAprTime): TAprTime;

begin
  Result := sec * APR_USEC_PER_SEC;
end;

//----------------------------------------------------------------------------------------------------------------------

function apr_time_make(sec, usec: TAprTime): TAprTime;

begin
  Result := (sec * APR_USEC_PER_SEC) div usec;
end;

//----------------------------------------------------------------------------------------------------------------------

function AprMMapCandidate(filelength: Int64): Boolean;

begin
  Result := (filelength >= APR_MMAP_THRESHOLD) and (filelength < APR_MMAP_LIMIT);
end;

//----------------------------------------------------------------------------------------------------------------------

function FileTimeToAprTime(t: TFileTime): TAprTime;

var
  L: LARGE_INTEGER;

begin
  L.LowPart := t.dwLowDateTime;
  L.HighPart := t.dwHighDateTime;
  Result := TAprTime(L.QuadPart div 10) - APR_DELTA_EPOCH_IN_USEC;
end;

//----------------------------------------------------------------------------------------------------------------------

function AprTimeToFileTime(t: TAprTime): TFileTime;

var
  L: LARGE_INTEGER;

begin
  L.QuadPart := (t + APR_DELTA_EPOCH_IN_USEC) * 10;
  Result.dwLowDateTime := L.LowPart;
  Result.dwHighDateTime := L.HighPart;
end;

//----------------------------------------------------------------------------------------------------------------------

function AprLibLoaded: Boolean;

begin
  Result := AprLib <> INVALID_HANDLE_VALUE;
end;

//----------------------------------------------------------------------------------------------------------------------

const
  sapr_allocator_alloc = '_apr_allocator_alloc@8';
  sapr_allocator_create = '_apr_allocator_create@4';
  sapr_allocator_destroy = '_apr_allocator_destroy@4';
  sapr_allocator_free = '_apr_allocator_free@8';
  sapr_allocator_max_free_set = '_apr_allocator_max_free_set@8';
  sapr_allocator_mutex_get = '_apr_allocator_mutex_get@4';
  sapr_allocator_mutex_set = '_apr_allocator_mutex_set@8';
  sapr_allocator_owner_get = '_apr_allocator_owner_get@4';
  sapr_allocator_owner_set = '_apr_allocator_owner_set@8';
  sapr_app_init_complete = 'apr_app_init_complete';
  sapr_app_initialize = '_apr_app_initialize@12';
  sapr_array_append = '_apr_array_append@12';
  sapr_array_cat = '_apr_array_cat@8';
  sapr_array_copy = '_apr_array_copy@8';
  sapr_array_copy_hdr = '_apr_array_copy_hdr@8';
  sapr_array_make = '_apr_array_make@12';
  sapr_array_pop = '_apr_array_pop@4';
  sapr_array_pstrcat = '_apr_array_pstrcat@12';
  sapr_array_push = '_apr_array_push@4';
  sapr_atoi64 = '_apr_atoi64@4';
  sapr_atomic_add32 = '_apr_atomic_add32@8';
  sapr_atomic_cas32 = '_apr_atomic_cas32@12';
  sapr_atomic_casptr = '_apr_atomic_casptr@12';
  sapr_atomic_dec32 = '_apr_atomic_dec32@4';
  sapr_atomic_inc32 = '_apr_atomic_inc32@4';
  sapr_atomic_init  = '_apr_atomic_init@4';
  sapr_atomic_read32 = '_apr_atomic_read32@4';
  sapr_atomic_set32 = '_apr_atomic_set32@8';
  sapr_atomic_sub32 = '_apr_atomic_sub32@8';
  sapr_atomic_xchg32 = '_apr_atomic_xchg32@8';
  sapr_collapse_spaces = '_apr_collapse_spaces@8';
  sapr_conv_ucs2_to_utf8 = '_apr_conv_ucs2_to_utf8@16';
  sapr_conv_utf8_to_ucs2 = '_apr_conv_utf8_to_ucs2@16';
  sapr_cpystrn = '_apr_cpystrn@12';
  sapr_crypto_sha256_new = '_apr_crypto_sha256_new@4';
  sapr_ctime = '_apr_ctime@12';
  sapr_day_snames = 'apr_day_snames';
  sapr_dbg_log = 'apr_dbg_log';
  sapr_dir_close = '_apr_dir_close@4';
  sapr_dir_make = '_apr_dir_make@12';
  sapr_dir_make_recursive = '_apr_dir_make_recursive@12';
  sapr_dir_open = '_apr_dir_open@12';
  sapr_dir_read = '_apr_dir_read@12';
  sapr_dir_remove = '_apr_dir_remove@8';
  sapr_dir_rewind = '_apr_dir_rewind@4';
  sapr_dso_error = '_apr_dso_error@12';
  sapr_dso_load = '_apr_dso_load@12';
  sapr_dso_sym = '_apr_dso_sym@12';
  sapr_dso_unload = '_apr_dso_unload@4';
  sapr_env_delete = '_apr_env_delete@8';
  sapr_env_get = '_apr_env_get@12';
  sapr_env_set = '_apr_env_set@12';
  sapr_file_append = '_apr_file_append@16';
  sapr_file_attrs_set = '_apr_file_attrs_set@16';
  sapr_file_close = '_apr_file_close@4';
  sapr_file_copy = '_apr_file_copy@16';
  sapr_file_data_get = '_apr_file_data_get@12';
  sapr_file_data_set = '_apr_file_data_set@16';
  sapr_file_dup = '_apr_file_dup@12';
  sapr_file_dup2 = '_apr_file_dup2@12';
  sapr_file_eof = '_apr_file_eof@4';
  sapr_file_flags_get = '_apr_file_flags_get@4';
  sapr_file_flush = '_apr_file_flush@4';
  sapr_file_getc = '_apr_file_getc@8';
  sapr_file_gets = '_apr_file_gets@12';
  sapr_file_info_get = '_apr_file_info_get@12';
  sapr_file_inherit_set = '_apr_file_inherit_set@4';
  sapr_file_inherit_unset = '_apr_file_inherit_unset@4';
  sapr_file_lock = '_apr_file_lock@8';
  sapr_file_mktemp = '_apr_file_mktemp@16';
  sapr_file_mtime_set = '_apr_file_mtime_set@16';
  sapr_file_name_get = '_apr_file_name_get@8';
  sapr_file_namedpipe_create = '_apr_file_namedpipe_create@12';
  sapr_file_open = '_apr_file_open@20';
  sapr_file_open_stderr = '_apr_file_open_stderr@8';
  sapr_file_open_stdin = '_apr_file_open_stdin@8';
  sapr_file_open_stdout = '_apr_file_open_stdout@8';
  sapr_file_perms_set = '_apr_file_perms_set@8';
  sapr_file_pipe_create = '_apr_file_pipe_create@12';
  sapr_file_pipe_timeout_get = '_apr_file_pipe_timeout_get@8';
  sapr_file_pipe_timeout_set = '_apr_file_pipe_timeout_set@12';
  sapr_file_pool_get = '_apr_file_pool_get@4';
  sapr_file_printf = 'apr_file_printf';
  sapr_file_putc = '_apr_file_putc@8';
  sapr_file_puts = '_apr_file_puts@8';
  sapr_file_read = '_apr_file_read@12';
  sapr_file_read_full = '_apr_file_read_full@16';
  sapr_file_remove = '_apr_file_remove@8';
  sapr_file_rename = '_apr_file_rename@12';
  sapr_file_seek = '_apr_file_seek@12';
  sapr_file_setaside = '_apr_file_setaside@12';
  sapr_file_trunc = '_apr_file_trunc@12';
  sapr_file_ungetc = '_apr_file_ungetc@8';
  sapr_file_unlock = '_apr_file_unlock@4';
  sapr_file_write = '_apr_file_write@12';
  sapr_file_write_full = '_apr_file_write_full@16';
  sapr_file_writev = '_apr_file_writev@16';
  sapr_file_writev_full = '_apr_file_writev_full@16';
  sapr_filepath_encoding = '_apr_filepath_encoding@8';
  sapr_filepath_get = '_apr_filepath_get@12';
  sapr_filepath_list_merge = '_apr_filepath_list_merge@12';
  sapr_filepath_list_split = '_apr_filepath_list_split@12';
  sapr_filepath_merge = '_apr_filepath_merge@20';
  sapr_filepath_name_get = '_apr_filepath_name_get@4';
  sapr_filepath_root = '_apr_filepath_root@16';
  sapr_filepath_set = '_apr_filepath_set@8';
  sapr_fnmatch = '_apr_fnmatch@12';
  sapr_fnmatch_test = '_apr_fnmatch_test@4';
  sapr_generate_random_bytes = '_apr_generate_random_bytes@8';
  sapr_gethostname = '_apr_gethostname@12';
  sapr_getnameinfo = '_apr_getnameinfo@12';
  sapr_getopt = '_apr_getopt@16';
  sapr_getopt_init = '_apr_getopt_init@16';
  sapr_getopt_long = '_apr_getopt_long@16';
  sapr_getservbyname = '_apr_getservbyname@8';
  sapr_gid_compare = '_apr_gid_compare@8';
  sapr_gid_get = '_apr_gid_get@12';
  sapr_gid_name_get = '_apr_gid_name_get@12';
  sapr_hash_copy = '_apr_hash_copy@8';
  sapr_hash_count = '_apr_hash_count@4';
  sapr_hash_first = '_apr_hash_first@8';
  sapr_hash_get = '_apr_hash_get@12';
  sapr_hash_make = '_apr_hash_make@4';
  sapr_hash_make_custom = '_apr_hash_make_custom@8';
  sapr_hash_merge = '_apr_hash_merge@20';
  sapr_hash_next = '_apr_hash_next@4';
  sapr_hash_overlay = '_apr_hash_overlay@12';
  sapr_hash_pool_get = '_apr_hash_pool_get@4';
  sapr_hash_set = '_apr_hash_set@16';
  sapr_hash_this = '_apr_hash_this@16';
  sapr_hashfunc_default = 'apr_hashfunc_default';
  sapr_initialize = '_apr_initialize@0';
  sapr_ipsubnet_create = '_apr_ipsubnet_create@16';
  sapr_ipsubnet_test = '_apr_ipsubnet_test@8';
  sapr_is_empty_array = '_apr_is_empty_array@4';
  sapr_is_empty_table = '_apr_is_empty_table@4';
  sapr_is_fnmatch = '_apr_is_fnmatch@4';
  sapr_itoa = '_apr_itoa@8';
  sapr_ltoa = '_apr_ltoa@8';
  sapr_match_glob = '_apr_match_glob@12';
  sapr_mcast_hops = '_apr_mcast_hops@8';
  sapr_mcast_interface = '_apr_mcast_interface@8';
  sapr_mcast_join = '_apr_mcast_join@16';
  sapr_mcast_leave = '_apr_mcast_leave@16';
  sapr_mcast_loopback = '_apr_mcast_loopback@8';
  sapr_mmap_create = '_apr_mmap_create@28';
  sapr_mmap_delete = '_apr_mmap_delete@4';
  sapr_mmap_dup = '_apr_mmap_dup@12';
  sapr_mmap_offset = '_apr_mmap_offset@16';
  sapr_month_snames = 'apr_month_snames';
  sapr_off_t_toa = '_apr_off_t_toa@12';
  sapr_os_default_encoding = '_apr_os_default_encoding@4';
  sapr_os_dir_get = '_apr_os_dir_get@8';
  sapr_os_dir_put = '_apr_os_dir_put@12';
  sapr_os_dso_handle_get = '_apr_os_dso_handle_get@8';
  sapr_os_dso_handle_put = '_apr_os_dso_handle_put@12';
  sapr_os_exp_time_get = '_apr_os_exp_time_get@8';
  sapr_os_exp_time_put = '_apr_os_exp_time_put@12';
  sapr_os_file_get = '_apr_os_file_get@8';
  sapr_os_file_put = '_apr_os_file_put@16';
  sapr_os_imp_time_get = '_apr_os_imp_time_get@8';
  sapr_os_imp_time_put = '_apr_os_imp_time_put@12';
  sapr_os_level = 'apr_os_level';
  sapr_os_locale_encoding = '_apr_os_locale_encoding@4';
  sapr_os_pipe_put = '_apr_os_pipe_put@12';
  sapr_os_pipe_put_ex = '_apr_os_pipe_put_ex@16';
  sapr_os_proc_mutex_get = '_apr_os_proc_mutex_get@8';
  sapr_os_proc_mutex_put = '_apr_os_proc_mutex_put@12';
  sapr_os_shm_get = '_apr_os_shm_get@8';
  sapr_os_shm_put = '_apr_os_shm_put@12';
  sapr_os_sock_get = '_apr_os_sock_get@8';
  sapr_os_sock_make = '_apr_os_sock_make@12';
  sapr_os_sock_put = '_apr_os_sock_put@12';
  sapr_os_thread_current = '_apr_os_thread_current@0';
  sapr_os_thread_equal = '_apr_os_thread_equal@8';
  sapr_os_thread_get = '_apr_os_thread_get@8';
  sapr_os_thread_put = '_apr_os_thread_put@12';
  sapr_os_threadkey_get = '_apr_os_threadkey_get@8';
  sapr_os_threadkey_put = '_apr_os_threadkey_put@12';
  sapr_os_uuid_get = '_apr_os_uuid_get@4';
  sapr_palloc = '_apr_palloc@8';
  sapr_palloc_debug = '_apr_palloc_debug@12';
  sapr_parse_addr_port = '_apr_parse_addr_port@20';
  sapr_password_get = '_apr_password_get@12';
  sapr_pcalloc = '_apr_pcalloc@8';
  sapr_pcalloc_debug = '_apr_pcalloc_debug@12';
  sapr_pmemdup = '_apr_pmemdup@12';
  sapr_poll = '_apr_poll@20';
  sapr_pollset_add = '_apr_pollset_add@8';
  sapr_pollset_create = '_apr_pollset_create@16';
  sapr_pollset_destroy = '_apr_pollset_destroy@4';
  sapr_pollset_poll = '_apr_pollset_poll@20';
  sapr_pollset_remove = '_apr_pollset_remove@8';
  sapr_pool_abort_get = '_apr_pool_abort_get@4';
  sapr_pool_abort_set = '_apr_pool_abort_set@8';
  sapr_pool_allocator_get = '_apr_pool_allocator_get@4';
  sapr_pool_child_cleanup_set = '_apr_pool_child_cleanup_set@16';
  sapr_pool_cleanup_for_exec = '_apr_pool_cleanup_for_exec@0';
  sapr_pool_cleanup_kill = '_apr_pool_cleanup_kill@12';
  sapr_pool_cleanup_null = 'apr_pool_cleanup_null';
  sapr_pool_cleanup_register = '_apr_pool_cleanup_register@16';
  sapr_pool_cleanup_run = '_apr_pool_cleanup_run@12';
  sapr_pool_clear = '_apr_pool_clear@4';
  sapr_pool_clear_debug = '_apr_pool_clear_debug@8';
  sapr_pool_create_ex = '_apr_pool_create_ex@16';
  sapr_pool_create_ex_debug = '_apr_pool_create_ex_debug@20';
  sapr_pool_destroy = '_apr_pool_destroy@4';
  sapr_pool_destroy_debug = '_apr_pool_destroy_debug@8';
  sapr_pool_initialize = '_apr_pool_initialize@0';
  sapr_pool_is_ancestor = '_apr_pool_is_ancestor@8';
  sapr_pool_note_subprocess = '_apr_pool_note_subprocess@12';
  sapr_pool_parent_get = '_apr_pool_parent_get@4';
  sapr_pool_tag = '_apr_pool_tag@8';
  sapr_pool_terminate = '_apr_pool_terminate@0';
  sapr_pool_userdata_get = '_apr_pool_userdata_get@12';
  sapr_pool_userdata_set = '_apr_pool_userdata_set@16';
  sapr_pool_userdata_setn = '_apr_pool_userdata_setn@16';
  sapr_proc_create = '_apr_proc_create@24';
  sapr_proc_detach = '_apr_proc_detach@4';
  sapr_proc_kill = '_apr_proc_kill@8';
  sapr_proc_mutex_child_init = '_apr_proc_mutex_child_init@12';
  sapr_proc_mutex_cleanup = '_apr_proc_mutex_cleanup@4';
  sapr_proc_mutex_create = '_apr_proc_mutex_create@16';
  sapr_proc_mutex_defname = '_apr_proc_mutex_defname@0';
  sapr_proc_mutex_destroy = '_apr_proc_mutex_destroy@4';
  sapr_proc_mutex_lock = '_apr_proc_mutex_lock@4';
  sapr_proc_mutex_lockfile = '_apr_proc_mutex_lockfile@4';
  sapr_proc_mutex_name = '_apr_proc_mutex_name@4';
  sapr_proc_mutex_pool_get = '_apr_proc_mutex_pool_get@4';
  sapr_proc_mutex_trylock = '_apr_proc_mutex_trylock@4';
  sapr_proc_mutex_unlock = '_apr_proc_mutex_unlock@4';
  sapr_proc_other_child_alert = '_apr_proc_other_child_alert@12';
  sapr_proc_other_child_refresh = '_apr_proc_other_child_refresh@8';
  sapr_proc_other_child_refresh_all = '_apr_proc_other_child_refresh_all@4';
  sapr_proc_other_child_register = '_apr_proc_other_child_register@20';
  sapr_proc_other_child_unregister = '_apr_proc_other_child_unregister@4';
  sapr_proc_wait = '_apr_proc_wait@16';
  sapr_proc_wait_all_procs = '_apr_proc_wait_all_procs@20';
  sapr_procattr_addrspace_set = '_apr_procattr_addrspace_set@8';
  sapr_procattr_child_err_set = '_apr_procattr_child_err_set@12';
  sapr_procattr_child_errfn_set = '_apr_procattr_child_errfn_set@8';
  sapr_procattr_child_in_set = '_apr_procattr_child_in_set@12';
  sapr_procattr_child_out_set = '_apr_procattr_child_out_set@12';
  sapr_procattr_cmdtype_set = '_apr_procattr_cmdtype_set@8';
  sapr_procattr_create = '_apr_procattr_create@8';
  sapr_procattr_detach_set = '_apr_procattr_detach_set@8';
  sapr_procattr_dir_set = '_apr_procattr_dir_set@8';
  sapr_procattr_error_check_set = '_apr_procattr_error_check_set@8';
  sapr_procattr_group_set = '_apr_procattr_group_set@8';
  sapr_procattr_io_set = '_apr_procattr_io_set@16';
  sapr_procattr_user_set = '_apr_procattr_user_set@12';
  sapr_psprintf = 'apr_psprintf';
  sapr_pstrcat = 'apr_pstrcat';
  sapr_pstrcatv = '_apr_pstrcatv@16';
  sapr_pstrdup = '_apr_pstrdup@8';
  sapr_pstrmemdup = '_apr_pstrmemdup@12';
  sapr_pstrndup = '_apr_pstrndup@12';
  sapr_pvsprintf = '_apr_pvsprintf@12';
  sapr_random_add_entropy = '_apr_random_add_entropy@12';
  sapr_random_after_fork = '_apr_random_after_fork@4';
  sapr_random_barrier = '_apr_random_barrier@4';
  sapr_random_init = '_apr_random_init@20';
  sapr_random_insecure_bytes = '_apr_random_insecure_bytes@12';
  sapr_random_insecure_ready = '_apr_random_insecure_ready@4';
  sapr_random_secure_bytes = '_apr_random_secure_bytes@12';
  sapr_random_secure_ready = '_apr_random_secure_ready@4';
  sapr_random_standard_new = '_apr_random_standard_new@4';
  sapr_rfc822_date = '_apr_rfc822_date@12';
  sapr_shm_attach = '_apr_shm_attach@12';
  sapr_shm_baseaddr_get = '_apr_shm_baseaddr_get@4';
  sapr_shm_create = '_apr_shm_create@16';
  sapr_shm_destroy = '_apr_shm_destroy@4';
  sapr_shm_detach = '_apr_shm_detach@4';
  sapr_shm_pool_get = '_apr_shm_pool_get@4';
  sapr_shm_remove = '_apr_shm_remove@8';
  sapr_shm_size_get = '_apr_shm_size_get@4';
  sapr_signal_block = '_apr_signal_block@4';
  sapr_signal_unblock = '_apr_signal_unblock@4';
  sapr_sleep = '_apr_sleep@8';
  sapr_snprintf = 'apr_snprintf';
  sapr_sockaddr_equal = '_apr_sockaddr_equal@8';
  sapr_sockaddr_info_get = '_apr_sockaddr_info_get@24';
  sapr_sockaddr_ip_get = '_apr_sockaddr_ip_get@8';
  sapr_socket_accept = '_apr_socket_accept@12';
  sapr_socket_addr_get = '_apr_socket_addr_get@12';
  sapr_socket_atmark = '_apr_socket_atmark@8';
  sapr_socket_bind = '_apr_socket_bind@8';
  sapr_socket_close = '_apr_socket_close@4';
  sapr_socket_connect = '_apr_socket_connect@8';
  sapr_socket_create = '_apr_socket_create@20';
  sapr_socket_data_get = '_apr_socket_data_get@12';
  sapr_socket_data_set = '_apr_socket_data_set@16';
  sapr_socket_inherit_set = '_apr_socket_inherit_set@4';
  sapr_socket_inherit_unset = '_apr_socket_inherit_unset@4';
  sapr_socket_listen = '_apr_socket_listen@8';
  sapr_socket_opt_get = '_apr_socket_opt_get@12';
  sapr_socket_opt_set = '_apr_socket_opt_set@12';
  sapr_socket_protocol_get = '_apr_socket_protocol_get@8';
  sapr_socket_recv = '_apr_socket_recv@12';
  sapr_socket_recvfrom = '_apr_recvfrom@20';
  sapr_socket_send = '_apr_socket_send@12';
  sapr_socket_sendfile = '_apr_socket_sendfile@24';
  sapr_socket_sendto = '_apr_socket_sendto@20';
  sapr_socket_sendv = '_apr_socket_sendv@16';
  sapr_socket_set_inherit = '_apr_socket_set_inherit@4';
  sapr_socket_shutdown = '_apr_socket_shutdown@8';
  sapr_socket_timeout_get = '_apr_socket_timeout_get@8';
  sapr_socket_timeout_set = '_apr_socket_timeout_set@12';
  sapr_socket_type_get = '_apr_socket_type_get@8';
  sapr_socket_unset_inherit = '_apr_socket_unset_inherit@4';
  sapr_stat = '_apr_stat@16';
  sapr_strerror = '_apr_strerror@12';
  sapr_strfsize = '_apr_strfsize@12';
  sapr_strftime = '_apr_strftime@20';
  sapr_strnatcasecmp = '_apr_strnatcasecmp@8';
  sapr_strnatcmp = '_apr_strnatcmp@8';
  sapr_strtoff = '_apr_strtoff@16';
  sapr_strtoi64 = '_apr_strtoi64@12';
  sapr_strtok = '_apr_strtok@12';
  sapr_table_add = '_apr_table_add@12';
  sapr_table_addn = '_apr_table_addn@12';
  sapr_table_clear = '_apr_table_clear@4';
  sapr_table_compress = '_apr_table_compress@8';
  sapr_table_copy = '_apr_table_copy@8';
  sapr_table_do = 'apr_table_do';
  sapr_table_elts = '_apr_table_elts@4';
  sapr_table_get = '_apr_table_get@8';
  sapr_table_make = '_apr_table_make@8';
  sapr_table_merge = '_apr_table_merge@12';
  sapr_table_mergen = '_apr_table_mergen@12';
  sapr_table_overlap = '_apr_table_overlap@12';
  sapr_table_overlay = '_apr_table_overlay@12';
  sapr_table_set = '_apr_table_set@12';
  sapr_table_setn = '_apr_table_setn@12';
  sapr_table_unset = '_apr_table_unset@8';
  sapr_table_vdo = '_apr_table_vdo@16';
  sapr_temp_dir_get = '_apr_temp_dir_get@8';
  sapr_terminate = 'apr_terminate';
  sapr_terminate2 = '_apr_terminate2@0';
  sapr_thread_cond_broadcast = '_apr_thread_cond_broadcast@4';
  sapr_thread_cond_create = '_apr_thread_cond_create@8';
  sapr_thread_cond_destroy = '_apr_thread_cond_destroy@4';
  sapr_thread_cond_pool_get = '_apr_thread_cond_pool_get@4';
  sapr_thread_cond_signal = '_apr_thread_cond_signal@4';
  sapr_thread_cond_timedwait = '_apr_thread_cond_timedwait@16';
  sapr_thread_cond_wait = '_apr_thread_cond_wait@8';
  sapr_thread_create = '_apr_thread_create@20';
  sapr_thread_data_get = '_apr_thread_data_get@12';
  sapr_thread_data_set = '_apr_thread_data_set@16';
  sapr_thread_detach = '_apr_thread_detach@4';
  sapr_thread_exit = '_apr_thread_exit@8';
  sapr_thread_join = '_apr_thread_join@8';
  sapr_thread_mutex_create = '_apr_thread_mutex_create@12';
  sapr_thread_mutex_destroy = '_apr_thread_mutex_destroy@4';
  sapr_thread_mutex_lock = '_apr_thread_mutex_lock@4';
  sapr_thread_mutex_pool_get = '_apr_thread_mutex_pool_get@4';
  sapr_thread_mutex_trylock = '_apr_thread_mutex_trylock@4';
  sapr_thread_mutex_unlock = '_apr_thread_mutex_unlock@4';
  sapr_thread_once = '_apr_thread_once@8';
  sapr_thread_once_init = '_apr_thread_once_init@8';
  sapr_thread_pool_get = '_apr_thread_pool_get@4';
  sapr_thread_rwlock_create = '_apr_thread_rwlock_create@8';
  sapr_thread_rwlock_destroy = '_apr_thread_rwlock_destroy@4';
  sapr_thread_rwlock_pool_get = '_apr_thread_rwlock_pool_get@4';
  sapr_thread_rwlock_rdlock = '_apr_thread_rwlock_rdlock@4';
  sapr_thread_rwlock_tryrdlock = '_apr_thread_rwlock_tryrdlock@4';
  sapr_thread_rwlock_trywrlock = '_apr_thread_rwlock_trywrlock@4';
  sapr_thread_rwlock_unlock = '_apr_thread_rwlock_unlock@4';
  sapr_thread_rwlock_wrlock = '_apr_thread_rwlock_wrlock@4';
  sapr_thread_yield = '_apr_thread_yield@0';
  sapr_threadattr_create = '_apr_threadattr_create@8';
  sapr_threadattr_detach_get = '_apr_threadattr_detach_get@4';
  sapr_threadattr_detach_set = '_apr_threadattr_detach_set@8';
  sapr_threadattr_guardsize_set = '_apr_threadattr_guardsize_set@8';
  sapr_threadattr_stacksize_set = '_apr_threadattr_stacksize_set@8';
  sapr_threadkey_data_get = '_apr_threadkey_data_get@12';
  sapr_threadkey_data_set = '_apr_threadkey_data_set@16';
  sapr_threadkey_private_create = '_apr_threadkey_private_create@12';
  sapr_threadkey_private_delete = '_apr_threadkey_private_delete@4';
  sapr_threadkey_private_get = '_apr_threadkey_private_get@8';
  sapr_threadkey_private_set = '_apr_threadkey_private_set@8';
  sapr_time_ansi_put = '_apr_time_ansi_put@8';
  sapr_time_clock_hires = '_apr_time_clock_hires@4';
  sapr_time_exp_get = '_apr_time_exp_get@8';
  sapr_time_exp_gmt = '_apr_time_exp_gmt@12';
  sapr_time_exp_gmt_get = '_apr_time_exp_gmt_get@8';
  sapr_time_exp_lt = '_apr_time_exp_lt@12';
  sapr_time_exp_tz = '_apr_time_exp_tz@16';
  sapr_time_now = '_apr_time_now@0';
  sapr_tokenize_to_argv = '_apr_tokenize_to_argv@12';
  sapr_uid_compare = '_apr_uid_compare@8';
  sapr_uid_current = '_apr_uid_current@12';
  sapr_uid_get = '_apr_uid_get@16';
  sapr_uid_homepath_get = '_apr_uid_homepath_get@12';
  sapr_uid_name_get = '_apr_uid_name_get@12';
  sapr_version = '_apr_version@4';
  sapr_version_string = '_apr_version_string@0';
  sapr_vformatter = '_apr_vformatter@16';
  sapr_vsnprintf = '_apr_vsnprintf@16';

function LoadAprLib(const FileName: string = ''): Boolean;

var
  LibFileName: string;

begin
  Result := not AprLibLoaded;

  if Result then
  begin
    if FileName = '' then
      LibFileName := 'libapr-1.dll'
    else
      LibFileName := FileName;

    AprLib := LoadLibrary(PChar(LibFileName));
    Result := AprLib <> 0;
    if not Result then
      AprLib := INVALID_HANDLE_VALUE
    else
    begin
      // apr_errno.h
      @apr_strerror := GetProcAddress(AprLib, sapr_strerror);
      // apr_version.h
      @apr_version := GetProcAddress(AprLib, sapr_version);
      @apr_version_string := GetProcAddress(AprLib, sapr_version_string);
      // apr_arch_utf8.h
      @apr_conv_utf8_to_ucs2 := GetProcAddress(AprLib, sapr_conv_utf8_to_ucs2);
      @apr_conv_ucs2_to_utf8 := GetProcAddress(AprLib, sapr_conv_ucs2_to_utf8);
      // apr_lib.h
      @apr_filepath_name_get := GetProcAddress(AprLib, sapr_filepath_name_get);
      @apr_vformatter := GetProcAddress(AprLib, sapr_vformatter);
      @apr_password_get := GetProcAddress(AprLib, sapr_password_get);
      // apr_general.h
      @apr_initialize := GetProcAddress(AprLib, sapr_initialize);
      @apr_app_initialize := GetProcAddress(AprLib, sapr_app_initialize);
      @apr_terminate := GetProcAddress(AprLib, sapr_terminate);
      @apr_terminate2 := GetProcAddress(AprLib, sapr_terminate2);
      @apr_generate_random_bytes := GetProcAddress(AprLib, sapr_generate_random_bytes);
      // apr_pools.h
      @apr_pool_initialize := GetProcAddress(AprLib, sapr_pool_initialize);
      @apr_pool_terminate := GetProcAddress(AprLib, sapr_pool_terminate);
      @apr_pool_create_ex := GetProcAddress(AprLib, sapr_pool_create_ex);
      @apr_pool_create_ex_debug := GetProcAddress(AprLib, sapr_pool_create_ex_debug);
      @apr_pool_allocator_get := GetProcAddress(AprLib, sapr_pool_allocator_get);
      @apr_pool_clear := GetProcAddress(AprLib, sapr_pool_clear);
      @apr_pool_clear_debug := GetProcAddress(AprLib, sapr_pool_clear_debug);
      @apr_pool_destroy := GetProcAddress(AprLib, sapr_pool_destroy);
      @apr_pool_destroy_debug := GetProcAddress(AprLib, sapr_pool_destroy_debug);
      @apr_palloc := GetProcAddress(AprLib, sapr_palloc);
      @apr_palloc_debug := GetProcAddress(AprLib, sapr_palloc_debug);
      @apr_pcalloc := GetProcAddress(AprLib, sapr_pcalloc);
      @apr_pcalloc_debug := GetProcAddress(AprLib, sapr_pcalloc_debug);
      @apr_pool_abort_set := GetProcAddress(AprLib, sapr_pool_abort_set);
      @apr_pool_abort_get := GetProcAddress(AprLib, sapr_pool_abort_get);
      @apr_pool_parent_get := GetProcAddress(AprLib, sapr_pool_parent_get);
      @apr_pool_is_ancestor := GetProcAddress(AprLib, sapr_pool_is_ancestor);
      @apr_pool_tag := GetProcAddress(AprLib, sapr_pool_tag);
      @apr_pool_userdata_set := GetProcAddress(AprLib, sapr_pool_userdata_set);
      @apr_pool_userdata_setn := GetProcAddress(AprLib, sapr_pool_userdata_setn);
      @apr_pool_userdata_get := GetProcAddress(AprLib, sapr_pool_userdata_get);
      @apr_pool_cleanup_register := GetProcAddress(AprLib, sapr_pool_cleanup_register);
      @apr_pool_cleanup_kill := GetProcAddress(AprLib, sapr_pool_cleanup_kill);
      @apr_pool_child_cleanup_set := GetProcAddress(AprLib, sapr_pool_child_cleanup_set);
      @apr_pool_cleanup_run := GetProcAddress(AprLib, sapr_pool_cleanup_run);
      @apr_pool_cleanup_null := GetProcAddress(AprLib, sapr_pool_cleanup_null);
      @apr_pool_cleanup_for_exec := GetProcAddress(AprLib, sapr_pool_cleanup_for_exec);
      // apr_atomic.h
      @apr_atomic_init := GetProcAddress(AprLib, sapr_atomic_init);
      @apr_atomic_read32 := GetProcAddress(AprLib, sapr_atomic_read32);
      @apr_atomic_set32 := GetProcAddress(AprLib, sapr_atomic_set32);
      @apr_atomic_add32 := GetProcAddress(AprLib, sapr_atomic_add32);
      @apr_atomic_sub32 := GetProcAddress(AprLib, sapr_atomic_sub32);
      @apr_atomic_inc32 := GetProcAddress(AprLib, sapr_atomic_inc32);
      @apr_atomic_dec32 := GetProcAddress(AprLib, sapr_atomic_dec32);
      @apr_atomic_cas32 := GetProcAddress(AprLib, sapr_atomic_cas32);
      @apr_atomic_xchg32 := GetProcAddress(AprLib, sapr_atomic_xchg32);
      @apr_atomic_casptr := GetProcAddress(AprLib, sapr_atomic_casptr);
      // apr_random.h
      @apr_crypto_sha256_new := GetProcAddress(AprLib, sapr_crypto_sha256_new);
      @apr_random_init := GetProcAddress(AprLib, sapr_random_init);
      @apr_random_standard_new := GetProcAddress(AprLib, sapr_random_standard_new);
      @apr_random_add_entropy := GetProcAddress(AprLib, sapr_random_add_entropy);
      @apr_random_insecure_bytes := GetProcAddress(AprLib, sapr_random_insecure_bytes);
      @apr_random_secure_bytes := GetProcAddress(AprLib, sapr_random_secure_bytes);
      @apr_random_barrier := GetProcAddress(AprLib, sapr_random_barrier);
      @apr_random_secure_ready := GetProcAddress(AprLib, sapr_random_secure_ready);
      @apr_random_insecure_ready := GetProcAddress(AprLib, sapr_random_insecure_ready);
      @apr_random_after_fork := GetProcAddress(AprLib, sapr_random_after_fork);
      // apr_signal.h
      @apr_signal_block := GetProcAddress(AprLib, sapr_signal_block);
      @apr_signal_unblock := GetProcAddress(AprLib, sapr_signal_unblock);
      // apr_strings.h
      @apr_strnatcmp := GetProcAddress(AprLib, sapr_strnatcmp);
      @apr_strnatcasecmp := GetProcAddress(AprLib, sapr_strnatcasecmp);
      @apr_pstrdup := GetProcAddress(AprLib, sapr_pstrdup);
      @apr_pstrmemdup := GetProcAddress(AprLib, sapr_pstrmemdup);
      @apr_pstrndup := GetProcAddress(AprLib, sapr_pstrndup);
      @apr_pmemdup := GetProcAddress(AprLib, sapr_pmemdup);
      @apr_pstrcat := GetProcAddress(AprLib, sapr_pstrcat);
      @apr_pstrcatv := GetProcAddress(AprLib, sapr_pstrcatv);
      @apr_pvsprintf := GetProcAddress(AprLib, sapr_pvsprintf);
      @apr_psprintf := GetProcAddress(AprLib, sapr_psprintf);
      @apr_cpystrn := GetProcAddress(AprLib, sapr_cpystrn);
      @apr_collapse_spaces := GetProcAddress(AprLib, sapr_collapse_spaces);
      @apr_tokenize_to_argv := GetProcAddress(AprLib, sapr_tokenize_to_argv);
      @apr_strtok := GetProcAddress(AprLib, sapr_strtok);
      @apr_snprintf := GetProcAddress(AprLib, sapr_snprintf);
      @apr_vsnprintf := GetProcAddress(AprLib, sapr_vsnprintf);
      @apr_itoa := GetProcAddress(AprLib, sapr_itoa);
      @apr_ltoa := GetProcAddress(AprLib, sapr_ltoa);
      @apr_off_t_toa := GetProcAddress(AprLib, sapr_off_t_toa);
      @apr_strtoff := GetProcAddress(AprLib, sapr_strtoff);
      @apr_strtoi64 := GetProcAddress(AprLib, sapr_strtoi64);
      @apr_atoi64 := GetProcAddress(AprLib, sapr_atoi64);
      @apr_strfsize := GetProcAddress(AprLib, sapr_strfsize);
      // apr_shm.h
      @apr_shm_create := GetProcAddress(AprLib, sapr_shm_create);
      @apr_shm_remove := GetProcAddress(AprLib, sapr_shm_remove);
      @apr_shm_destroy := GetProcAddress(AprLib, sapr_shm_destroy);
      @apr_shm_attach := GetProcAddress(AprLib, sapr_shm_attach);
      @apr_shm_detach := GetProcAddress(AprLib, sapr_shm_detach);
      @apr_shm_baseaddr_get := GetProcAddress(AprLib, sapr_shm_baseaddr_get);
      @apr_shm_size_get := GetProcAddress(AprLib, sapr_shm_size_get);
      @apr_shm_pool_get := GetProcAddress(AprLib, sapr_shm_pool_get);
      // apr_hash.h
      @apr_hashfunc_default := GetProcAddress(AprLib, sapr_hashfunc_default); 
      @apr_hash_make := GetProcAddress(AprLib, sapr_hash_make);
      @apr_hash_make_custom := GetProcAddress(AprLib, sapr_hash_make_custom);
      @apr_hash_copy := GetProcAddress(AprLib, sapr_hash_copy);
      @apr_hash_set := GetProcAddress(AprLib, sapr_hash_set);
      @apr_hash_get := GetProcAddress(AprLib, sapr_hash_get);
      @apr_hash_first := GetProcAddress(AprLib, sapr_hash_first);
      @apr_hash_next := GetProcAddress(AprLib, sapr_hash_next);
      @apr_hash_this := GetProcAddress(AprLib, sapr_hash_this);
      @apr_hash_count := GetProcAddress(AprLib, sapr_hash_count);
      @apr_hash_overlay := GetProcAddress(AprLib, sapr_hash_overlay);
      @apr_hash_merge := GetProcAddress(AprLib, sapr_hash_merge);
      @apr_hash_pool_get := GetProcAddress(AprLib, sapr_hash_pool_get);
      // apr_tables.h
      @apr_table_elts := GetProcAddress(AprLib, sapr_table_elts);
      @apr_is_empty_table := GetProcAddress(AprLib, sapr_is_empty_table);
      @apr_is_empty_array := GetProcAddress(AprLib, sapr_is_empty_array);
      @apr_array_make := GetProcAddress(AprLib, sapr_array_make);
      @apr_array_push := GetProcAddress(AprLib, sapr_array_push);
      @apr_array_pop := GetProcAddress(AprLib, sapr_array_pop);
      @apr_array_cat := GetProcAddress(AprLib, sapr_array_cat);
      @apr_array_copy := GetProcAddress(AprLib, sapr_array_copy);
      @apr_array_copy_hdr := GetProcAddress(AprLib, sapr_array_copy_hdr);
      @apr_array_append := GetProcAddress(AprLib, sapr_array_append);
      @apr_array_pstrcat := GetProcAddress(AprLib, sapr_array_pstrcat);
      @apr_table_make := GetProcAddress(AprLib, sapr_table_make);
      @apr_table_copy := GetProcAddress(AprLib, sapr_table_copy);
      @apr_table_clear := GetProcAddress(AprLib, sapr_table_clear);
      @apr_table_get := GetProcAddress(AprLib, sapr_table_get);
      @apr_table_set := GetProcAddress(AprLib, sapr_table_set);
      @apr_table_setn := GetProcAddress(AprLib, sapr_table_setn);
      @apr_table_unset := GetProcAddress(AprLib, sapr_table_unset);
      @apr_table_merge := GetProcAddress(AprLib, sapr_table_merge);
      @apr_table_mergen := GetProcAddress(AprLib, sapr_table_mergen);
      @apr_table_add := GetProcAddress(AprLib, sapr_table_add);
      @apr_table_addn := GetProcAddress(AprLib, sapr_table_addn);
      @apr_table_overlay := GetProcAddress(AprLib, sapr_table_overlay);
      @apr_table_do := GetProcAddress(AprLib, sapr_table_do);
      @apr_table_vdo := GetProcAddress(AprLib, sapr_table_vdo);
      @apr_table_overlap := GetProcAddress(AprLib, sapr_table_overlap);
      @apr_table_compress := GetProcAddress(AprLib, sapr_table_compress);
      // apr_env.h
      @apr_env_get := GetProcAddress(AprLib, sapr_env_get);
      @apr_env_set := GetProcAddress(AprLib, sapr_env_set);
      @apr_env_delete := GetProcAddress(AprLib, sapr_env_delete);
      // apr_getopt.h
      @apr_getopt_init := GetProcAddress(AprLib, sapr_getopt_init);
      @apr_getopt := GetProcAddress(AprLib, sapr_getopt);
      @apr_getopt_long := GetProcAddress(AprLib, sapr_getopt_long);
      // apr_dso.h
      @apr_dso_load := GetProcAddress(AprLib, sapr_dso_load);
      @apr_dso_unload := GetProcAddress(AprLib, sapr_dso_unload);
      @apr_dso_sym := GetProcAddress(AprLib, sapr_dso_sym);
      @apr_dso_error := GetProcAddress(AprLib, sapr_dso_error);
      // apr_user.h
      @apr_uid_current := GetProcAddress(AprLib, sapr_uid_current);
      @apr_uid_name_get := GetProcAddress(AprLib, sapr_uid_name_get);
      @apr_uid_get := GetProcAddress(AprLib, sapr_uid_get);
      @apr_uid_homepath_get := GetProcAddress(AprLib, sapr_uid_homepath_get);
      @apr_uid_compare := GetProcAddress(AprLib, sapr_uid_compare);
      @apr_gid_name_get := GetProcAddress(AprLib, sapr_gid_name_get);
      @apr_gid_get := GetProcAddress(AprLib, sapr_gid_get);
      @apr_gid_compare := GetProcAddress(AprLib, sapr_gid_compare);
      // apr_time.h
      @apr_month_snames := GetProcAddress(AprLib, sapr_month_snames);
      @apr_day_snames := GetProcAddress(AprLib, sapr_day_snames);
      @apr_time_now := GetProcAddress(AprLib, sapr_time_now);
      @apr_time_ansi_put := GetProcAddress(AprLib, sapr_time_ansi_put);
      @apr_time_exp_tz := GetProcAddress(AprLib, sapr_time_exp_tz);
      @apr_time_exp_gmt := GetProcAddress(AprLib, sapr_time_exp_gmt);
      @apr_time_exp_lt := GetProcAddress(AprLib, sapr_time_exp_lt);
      @apr_time_exp_get := GetProcAddress(AprLib, sapr_time_exp_get);
      @apr_time_exp_gmt_get := GetProcAddress(AprLib, sapr_time_exp_gmt_get);
      @apr_sleep := GetProcAddress(AprLib, sapr_sleep);
      @apr_rfc822_date := GetProcAddress(AprLib, sapr_rfc822_date);
      @apr_ctime := GetProcAddress(AprLib, sapr_ctime);
      @apr_strftime := GetProcAddress(AprLib, sapr_strftime);
      @apr_time_clock_hires := GetProcAddress(AprLib, sapr_time_clock_hires);
      // apr_file_info.h
      @apr_stat := GetProcAddress(AprLib, sapr_stat);
      @apr_dir_open := GetProcAddress(AprLib, sapr_dir_open);
      @apr_dir_close := GetProcAddress(AprLib, sapr_dir_close);
      @apr_dir_read := GetProcAddress(AprLib, sapr_dir_read);
      @apr_dir_rewind := GetProcAddress(AprLib, sapr_dir_rewind);
      @apr_filepath_root := GetProcAddress(AprLib, sapr_filepath_root);
      @apr_filepath_merge := GetProcAddress(AprLib, sapr_filepath_merge);
      @apr_filepath_list_split := GetProcAddress(AprLib, sapr_filepath_list_split);
      @apr_filepath_list_merge := GetProcAddress(AprLib, sapr_filepath_list_merge);
      @apr_filepath_get := GetProcAddress(AprLib, sapr_filepath_get);
      @apr_filepath_set := GetProcAddress(AprLib, sapr_filepath_set);
      @apr_filepath_encoding := GetProcAddress(AprLib, sapr_filepath_encoding);
      // apr_file_io.h
      @apr_file_open := GetProcAddress(AprLib, sapr_filepath_encoding);
      @apr_file_close := GetProcAddress(AprLib, sapr_file_close);
      @apr_file_remove := GetProcAddress(AprLib, sapr_file_remove);
      @apr_file_rename := GetProcAddress(AprLib, sapr_file_rename);
      @apr_file_copy := GetProcAddress(AprLib, sapr_file_copy);
      @apr_file_append := GetProcAddress(AprLib, sapr_file_append);
      @apr_file_eof := GetProcAddress(AprLib, sapr_file_eof);
      @apr_file_open_stderr := GetProcAddress(AprLib, sapr_file_open_stderr);
      @apr_file_open_stdout := GetProcAddress(AprLib, sapr_file_open_stdout);
      @apr_file_open_stdin := GetProcAddress(AprLib, sapr_file_open_stdin);
      @apr_file_read := GetProcAddress(AprLib, sapr_file_read);
      @apr_file_write := GetProcAddress(AprLib, sapr_file_write);
      @apr_file_writev := GetProcAddress(AprLib, sapr_file_writev);
      @apr_file_read_full := GetProcAddress(AprLib, sapr_file_read_full);
      @apr_file_write_full := GetProcAddress(AprLib, sapr_file_write_full);
      @apr_file_writev_full := GetProcAddress(AprLib, sapr_file_writev_full);
      @apr_file_putc := GetProcAddress(AprLib, sapr_file_putc);
      @apr_file_getc := GetProcAddress(AprLib, sapr_file_getc);
      @apr_file_ungetc := GetProcAddress(AprLib, sapr_file_ungetc);
      @apr_file_gets := GetProcAddress(AprLib, sapr_file_gets);
      @apr_file_puts := GetProcAddress(AprLib, sapr_file_puts);
      @apr_file_flush := GetProcAddress(AprLib, sapr_file_flush);
      @apr_file_dup := GetProcAddress(AprLib, sapr_file_dup);
      @apr_file_dup2 := GetProcAddress(AprLib, sapr_file_dup2);
      @apr_file_setaside := GetProcAddress(AprLib, sapr_file_setaside);
      @apr_file_seek := GetProcAddress(AprLib, sapr_file_seek);
      @apr_file_pipe_create := GetProcAddress(AprLib, sapr_file_pipe_create);
      @apr_file_namedpipe_create := GetProcAddress(AprLib, sapr_file_namedpipe_create);
      @apr_file_pipe_timeout_get := GetProcAddress(AprLib, sapr_file_pipe_timeout_get);
      @apr_file_pipe_timeout_set := GetProcAddress(AprLib, sapr_file_pipe_timeout_set);
      @apr_file_lock := GetProcAddress(AprLib, sapr_file_lock);
      @apr_file_unlock := GetProcAddress(AprLib, sapr_file_unlock);
      @apr_file_name_get := GetProcAddress(AprLib, sapr_file_name_get);
      @apr_file_data_get := GetProcAddress(AprLib, sapr_file_data_get);
      @apr_file_data_set := GetProcAddress(AprLib, sapr_file_data_set);
      @apr_file_printf := GetProcAddress(AprLib, sapr_file_printf);
      @apr_file_perms_set := GetProcAddress(AprLib, sapr_file_perms_set);
      @apr_file_attrs_set := GetProcAddress(AprLib, sapr_file_attrs_set);
      @apr_file_mtime_set := GetProcAddress(AprLib, sapr_file_mtime_set);
      @apr_dir_make := GetProcAddress(AprLib, sapr_dir_make);
      @apr_dir_make_recursive := GetProcAddress(AprLib, sapr_dir_make_recursive);
      @apr_dir_remove := GetProcAddress(AprLib, sapr_dir_remove);
      @apr_file_info_get := GetProcAddress(AprLib, sapr_file_info_get);
      @apr_file_trunc := GetProcAddress(AprLib, sapr_file_trunc);
      @apr_file_flags_get := GetProcAddress(AprLib, sapr_file_flags_get);
      @apr_file_pool_get := GetProcAddress(AprLib, sapr_file_pool_get);
      @apr_file_inherit_set := GetProcAddress(AprLib, sapr_file_inherit_set);
      @apr_file_inherit_unset := GetProcAddress(AprLib, sapr_file_inherit_unset);
      @apr_file_mktemp := GetProcAddress(AprLib, sapr_file_mktemp);
      @apr_temp_dir_get := GetProcAddress(AprLib, sapr_temp_dir_get);
      // apr_mmap.h
      @apr_mmap_create := GetProcAddress(AprLib, sapr_mmap_create);
      @apr_mmap_dup := GetProcAddress(AprLib, sapr_mmap_dup);
      @apr_mmap_delete := GetProcAddress(AprLib, sapr_mmap_delete);
      @apr_mmap_offset := GetProcAddress(AprLib, sapr_mmap_offset);
      // apr_network_io.h
      @apr_socket_create := GetProcAddress(AprLib, sapr_socket_create);
      @apr_socket_shutdown := GetProcAddress(AprLib, sapr_socket_shutdown);
      @apr_socket_close := GetProcAddress(AprLib, sapr_socket_close);
      @apr_socket_bind := GetProcAddress(AprLib, sapr_socket_bind);
      @apr_socket_listen := GetProcAddress(AprLib, sapr_socket_listen);
      @apr_socket_accept := GetProcAddress(AprLib, sapr_socket_accept);
      @apr_socket_connect := GetProcAddress(AprLib, sapr_socket_connect);
      @apr_sockaddr_info_get := GetProcAddress(AprLib, sapr_sockaddr_info_get);
      @apr_getnameinfo := GetProcAddress(AprLib, sapr_getnameinfo);
      @apr_parse_addr_port := GetProcAddress(AprLib, sapr_parse_addr_port);
      @apr_gethostname := GetProcAddress(AprLib, sapr_gethostname);
      @apr_socket_data_get := GetProcAddress(AprLib, sapr_socket_data_get);
      @apr_socket_send := GetProcAddress(AprLib, sapr_socket_send);
      @apr_socket_sendv := GetProcAddress(AprLib, sapr_socket_sendv);
      @apr_socket_sendto := GetProcAddress(AprLib, sapr_socket_sendto);
      @apr_socket_recvfrom := GetProcAddress(AprLib, sapr_socket_recvfrom);
      @apr_socket_sendfile := GetProcAddress(AprLib, sapr_socket_sendfile);
      @apr_socket_recv := GetProcAddress(AprLib, sapr_socket_recv);
      @apr_socket_opt_set := GetProcAddress(AprLib, sapr_socket_opt_set);
      @apr_socket_timeout_set := GetProcAddress(AprLib, sapr_socket_timeout_set);
      @apr_socket_opt_get := GetProcAddress(AprLib, sapr_socket_opt_get);
      @apr_socket_timeout_get := GetProcAddress(AprLib, sapr_socket_timeout_get);
      @apr_socket_atmark := GetProcAddress(AprLib, sapr_socket_atmark);
      @apr_socket_addr_get := GetProcAddress(AprLib, sapr_socket_addr_get);
      @apr_sockaddr_ip_get := GetProcAddress(AprLib, sapr_sockaddr_ip_get);
      @apr_sockaddr_equal := GetProcAddress(AprLib, sapr_sockaddr_equal);
      @apr_socket_type_get := GetProcAddress(AprLib, sapr_socket_type_get);
      @apr_getservbyname := GetProcAddress(AprLib, sapr_getservbyname);
      @apr_ipsubnet_create := GetProcAddress(AprLib, sapr_ipsubnet_create);
      @apr_ipsubnet_test := GetProcAddress(AprLib, sapr_ipsubnet_test);
      @apr_socket_protocol_get := GetProcAddress(AprLib, sapr_socket_protocol_get);
      @apr_socket_inherit_set := GetProcAddress(AprLib, sapr_socket_inherit_set);
      @apr_socket_set_inherit := GetProcAddress(AprLib, sapr_socket_set_inherit);
      @apr_socket_inherit_unset := GetProcAddress(AprLib, sapr_socket_inherit_unset);
      @apr_socket_unset_inherit := GetProcAddress(AprLib, sapr_socket_unset_inherit);
      @apr_mcast_join := GetProcAddress(AprLib, sapr_mcast_join);
      @apr_mcast_leave := GetProcAddress(AprLib, sapr_mcast_leave);
      @apr_mcast_hops := GetProcAddress(AprLib, sapr_mcast_hops);
      @apr_mcast_loopback := GetProcAddress(AprLib, sapr_mcast_loopback);
      @apr_mcast_interface := GetProcAddress(AprLib, sapr_mcast_interface);
      // apr_poll.h
      @apr_pollset_create := GetProcAddress(AprLib, sapr_pollset_create);
      @apr_pollset_destroy := GetProcAddress(AprLib, sapr_pollset_destroy);
      @apr_pollset_add := GetProcAddress(AprLib, sapr_pollset_add);
      @apr_pollset_remove := GetProcAddress(AprLib, sapr_pollset_remove);
      @apr_pollset_poll := GetProcAddress(AprLib, sapr_pollset_poll);
      @apr_poll := GetProcAddress(AprLib, sapr_poll);
      // apr_thread_mutex.h
      @apr_thread_mutex_create := GetProcAddress(AprLib, sapr_thread_mutex_create);
      @apr_thread_mutex_lock := GetProcAddress(AprLib, sapr_thread_mutex_lock);
      @apr_thread_mutex_trylock := GetProcAddress(AprLib, sapr_thread_mutex_trylock);
      @apr_thread_mutex_unlock := GetProcAddress(AprLib, sapr_thread_mutex_unlock);
      @apr_thread_mutex_destroy := GetProcAddress(AprLib, sapr_thread_mutex_destroy);
      @apr_thread_mutex_pool_get := GetProcAddress(AprLib, sapr_thread_mutex_pool_get);
      // apr_thread_rwlock.h
      @apr_thread_rwlock_create := GetProcAddress(AprLib, sapr_thread_rwlock_create);
      @apr_thread_rwlock_rdlock := GetProcAddress(AprLib, sapr_thread_rwlock_rdlock);
      @apr_thread_rwlock_tryrdlock := GetProcAddress(AprLib, sapr_thread_rwlock_tryrdlock);
      @apr_thread_rwlock_wrlock := GetProcAddress(AprLib, sapr_thread_rwlock_wrlock);
      @apr_thread_rwlock_trywrlock := GetProcAddress(AprLib, sapr_thread_rwlock_trywrlock);
      @apr_thread_rwlock_unlock := GetProcAddress(AprLib, sapr_thread_rwlock_unlock);
      @apr_thread_rwlock_destroy := GetProcAddress(AprLib, sapr_thread_rwlock_destroy);
      @apr_thread_rwlock_pool_get := GetProcAddress(AprLib, sapr_thread_rwlock_pool_get);
      // apr_thread_cond.h
      @apr_thread_cond_create := GetProcAddress(AprLib, sapr_thread_cond_create);
      @apr_thread_cond_wait := GetProcAddress(AprLib, sapr_thread_cond_wait);
      @apr_thread_cond_timedwait := GetProcAddress(AprLib, sapr_thread_cond_timedwait);
      @apr_thread_cond_signal := GetProcAddress(AprLib, sapr_thread_cond_signal);
      @apr_thread_cond_broadcast := GetProcAddress(AprLib, sapr_thread_cond_broadcast);
      @apr_thread_cond_destroy := GetProcAddress(AprLib, sapr_thread_cond_destroy);
      @apr_thread_cond_pool_get := GetProcAddress(AprLib, sapr_thread_cond_pool_get);
      // apr_thread_proc.h
      @apr_threadattr_create := GetProcAddress(AprLib, sapr_threadattr_create);
      @apr_threadattr_detach_set := GetProcAddress(AprLib, sapr_threadattr_detach_set);
      @apr_threadattr_detach_get := GetProcAddress(AprLib, sapr_threadattr_detach_get);
      @apr_threadattr_stacksize_set := GetProcAddress(AprLib, sapr_threadattr_stacksize_set);
      @apr_threadattr_guardsize_set := GetProcAddress(AprLib, sapr_threadattr_guardsize_set);
      @apr_thread_create := GetProcAddress(AprLib, sapr_thread_create);
      @apr_thread_exit := GetProcAddress(AprLib, sapr_thread_exit);
      @apr_thread_join := GetProcAddress(AprLib, sapr_thread_join);
      @apr_thread_yield := GetProcAddress(AprLib, sapr_thread_yield);
      @apr_thread_once_init := GetProcAddress(AprLib, sapr_thread_once_init);
      @apr_thread_once := GetProcAddress(AprLib, sapr_thread_once);
      @apr_thread_detach := GetProcAddress(AprLib, sapr_thread_detach);
      @apr_thread_data_get := GetProcAddress(AprLib, sapr_thread_data_get);
      @apr_thread_data_set := GetProcAddress(AprLib, sapr_thread_data_set);
      @apr_threadkey_private_create := GetProcAddress(AprLib, sapr_threadkey_private_create);
      @apr_threadkey_private_get := GetProcAddress(AprLib, sapr_threadkey_private_get);
      @apr_threadkey_private_set := GetProcAddress(AprLib, sapr_threadkey_private_set);
      @apr_threadkey_private_delete := GetProcAddress(AprLib, sapr_threadkey_private_delete);
      @apr_threadkey_data_get := GetProcAddress(AprLib, sapr_threadkey_data_get);
      @apr_threadkey_data_set := GetProcAddress(AprLib, sapr_threadkey_data_set);
      @apr_procattr_create := GetProcAddress(AprLib, sapr_procattr_create);
      @apr_procattr_io_set := GetProcAddress(AprLib, sapr_procattr_io_set);
      @apr_procattr_child_in_set := GetProcAddress(AprLib, sapr_procattr_child_in_set);
      @apr_procattr_child_out_set := GetProcAddress(AprLib, sapr_procattr_child_out_set);
      @apr_procattr_child_err_set := GetProcAddress(AprLib, sapr_procattr_child_err_set);
      @apr_procattr_dir_set := GetProcAddress(AprLib, sapr_procattr_dir_set);
      @apr_procattr_cmdtype_set := GetProcAddress(AprLib, sapr_procattr_cmdtype_set);
      @apr_procattr_detach_set := GetProcAddress(AprLib, sapr_procattr_detach_set);
      @apr_procattr_child_errfn_set := GetProcAddress(AprLib, sapr_procattr_child_errfn_set);
      @apr_procattr_error_check_set := GetProcAddress(AprLib, sapr_procattr_error_check_set);
      @apr_procattr_addrspace_set := GetProcAddress(AprLib, sapr_procattr_addrspace_set);
      @apr_procattr_user_set := GetProcAddress(AprLib, sapr_procattr_user_set);
      @apr_procattr_group_set := GetProcAddress(AprLib, sapr_procattr_group_set);
      @apr_proc_create := GetProcAddress(AprLib, sapr_proc_create);
      @apr_proc_wait := GetProcAddress(AprLib, sapr_proc_wait);
      @apr_proc_wait_all_procs := GetProcAddress(AprLib, sapr_proc_wait_all_procs);
      @apr_proc_detach := GetProcAddress(AprLib, sapr_proc_detach);
      @apr_proc_other_child_register := GetProcAddress(AprLib, sapr_proc_other_child_register);
      @apr_proc_other_child_unregister := GetProcAddress(AprLib, sapr_proc_other_child_unregister);
      @apr_proc_other_child_alert := GetProcAddress(AprLib, sapr_proc_other_child_alert);
      @apr_proc_other_child_refresh := GetProcAddress(AprLib, sapr_proc_other_child_refresh);
      @apr_proc_other_child_refresh_all := GetProcAddress(AprLib, sapr_proc_other_child_refresh_all);
      @apr_proc_kill := GetProcAddress(AprLib, sapr_proc_kill);
      @apr_pool_note_subprocess := GetProcAddress(AprLib, sapr_pool_note_subprocess);
      @apr_thread_pool_get := GetProcAddress(AprLib, sapr_thread_pool_get);
      // apr_proc_mutex.h
      @apr_proc_mutex_create := GetProcAddress(AprLib, sapr_proc_mutex_create);
      @apr_proc_mutex_child_init := GetProcAddress(AprLib, sapr_proc_mutex_child_init);
      @apr_proc_mutex_lock := GetProcAddress(AprLib, sapr_proc_mutex_lock);
      @apr_proc_mutex_trylock := GetProcAddress(AprLib, sapr_proc_mutex_trylock);
      @apr_proc_mutex_unlock := GetProcAddress(AprLib, sapr_proc_mutex_unlock);
      @apr_proc_mutex_destroy := GetProcAddress(AprLib, sapr_proc_mutex_destroy);
      @apr_proc_mutex_cleanup := GetProcAddress(AprLib, sapr_proc_mutex_cleanup);
      @apr_proc_mutex_lockfile := GetProcAddress(AprLib, sapr_proc_mutex_lockfile);
      @apr_proc_mutex_name := GetProcAddress(AprLib, sapr_proc_mutex_name);
      @apr_proc_mutex_defname := GetProcAddress(AprLib, sapr_proc_mutex_defname);
      @apr_proc_mutex_pool_get := GetProcAddress(AprLib, sapr_proc_mutex_pool_get);
      // apr_allocator.h
      @apr_allocator_create := GetProcAddress(AprLib, sapr_allocator_create);
      @apr_allocator_destroy := GetProcAddress(AprLib, sapr_allocator_destroy);
      @apr_allocator_alloc := GetProcAddress(AprLib, sapr_allocator_alloc);
      @apr_allocator_free := GetProcAddress(AprLib, sapr_allocator_free);
      @apr_allocator_owner_set := GetProcAddress(AprLib, sapr_allocator_owner_set);
      @apr_allocator_owner_get := GetProcAddress(AprLib, sapr_allocator_owner_get);
      @apr_allocator_max_free_set := GetProcAddress(AprLib, sapr_allocator_max_free_set);
      @apr_allocator_mutex_set := GetProcAddress(AprLib, sapr_allocator_mutex_set);
      @apr_allocator_mutex_get := GetProcAddress(AprLib, sapr_allocator_mutex_get);
      // apr_fn_match.h
      @apr_fnmatch := GetProcAddress(AprLib, sapr_fnmatch);
      @apr_fnmatch_test := GetProcAddress(AprLib, sapr_fnmatch_test);
      @apr_is_fnmatch := GetProcAddress(AprLib, sapr_is_fnmatch);
      @apr_match_glob := GetProcAddress(AprLib, sapr_match_glob);
      // apr_portable.h
      @apr_os_file_get := GetProcAddress(AprLib, sapr_os_file_get);
      @apr_os_dir_get := GetProcAddress(AprLib, sapr_os_dir_get);
      @apr_os_sock_get := GetProcAddress(AprLib, sapr_os_sock_get);
      @apr_os_proc_mutex_get := GetProcAddress(AprLib, sapr_os_proc_mutex_get);
      @apr_os_exp_time_get := GetProcAddress(AprLib, sapr_os_exp_time_get);
      @apr_os_imp_time_get := GetProcAddress(AprLib, sapr_os_imp_time_get);
      @apr_os_shm_get := GetProcAddress(AprLib, sapr_os_shm_get);
      @apr_os_thread_get := GetProcAddress(AprLib, sapr_os_thread_get);
      @apr_os_threadkey_get := GetProcAddress(AprLib, sapr_os_threadkey_get);
      @apr_os_thread_put := GetProcAddress(AprLib, sapr_os_thread_put);
      @apr_os_threadkey_put := GetProcAddress(AprLib, sapr_os_threadkey_put);
      @apr_os_thread_current := GetProcAddress(AprLib, sapr_os_thread_current);
      @apr_os_thread_equal := GetProcAddress(AprLib, sapr_os_thread_equal);
      @apr_os_file_put := GetProcAddress(AprLib, sapr_os_file_put);
      @apr_os_pipe_put := GetProcAddress(AprLib, sapr_os_pipe_put);
      @apr_os_pipe_put_ex := GetProcAddress(AprLib, sapr_os_pipe_put_ex);
      @apr_os_dir_put := GetProcAddress(AprLib, sapr_os_dir_put);
      @apr_os_sock_put := GetProcAddress(AprLib, sapr_os_sock_put);
      @apr_os_sock_make := GetProcAddress(AprLib, sapr_os_sock_make);
      @apr_os_proc_mutex_put := GetProcAddress(AprLib, sapr_os_proc_mutex_put);
      @apr_os_imp_time_put := GetProcAddress(AprLib, sapr_os_imp_time_put);
      @apr_os_exp_time_put := GetProcAddress(AprLib, sapr_os_exp_time_put);
      @apr_os_shm_put := GetProcAddress(AprLib, sapr_os_shm_put);
      @apr_os_dso_handle_put := GetProcAddress(AprLib, sapr_os_dso_handle_put);
      @apr_os_dso_handle_get := GetProcAddress(AprLib, sapr_os_dso_handle_get);
      @apr_os_uuid_get := GetProcAddress(AprLib, sapr_os_uuid_get);
      @apr_os_default_encoding := GetProcAddress(AprLib, sapr_os_default_encoding);
      @apr_os_locale_encoding := GetProcAddress(AprLib, sapr_os_locale_encoding);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FreeAprLib;

begin
  if AprLibLoaded then
    FreeLibrary(AprLib);
  AprLib := INVALID_HANDLE_VALUE;
  @apr_strerror := nil;
  @apr_version := nil;
  @apr_version_string := nil;
  @apr_conv_utf8_to_ucs2 := nil;
  @apr_conv_ucs2_to_utf8 := nil;
  @apr_filepath_name_get := nil;
  @apr_vformatter := nil;
  @apr_password_get := nil;
  @apr_initialize := nil;
  @apr_app_initialize := nil;
  @apr_terminate := nil;
  @apr_terminate2 := nil;
  @apr_generate_random_bytes := nil;
  @apr_pool_initialize := nil;
  @apr_pool_terminate := nil;
  @apr_pool_create_ex := nil;
  @apr_pool_create_ex_debug := nil;
  @apr_pool_allocator_get := nil;
  @apr_pool_clear := nil;
  @apr_pool_clear_debug := nil;
  @apr_pool_destroy := nil;
  @apr_pool_destroy_debug := nil;
  @apr_palloc := nil;
  @apr_palloc_debug := nil;
  @apr_pcalloc := nil;
  @apr_pcalloc_debug := nil;
  @apr_pool_abort_set := nil;
  @apr_pool_abort_get := nil;
  @apr_pool_parent_get := nil;
  @apr_pool_is_ancestor := nil;
  @apr_pool_tag := nil;
  @apr_pool_userdata_set := nil;
  @apr_pool_userdata_setn := nil;
  @apr_pool_userdata_get := nil;
  @apr_pool_cleanup_register := nil;
  @apr_pool_cleanup_kill := nil;
  @apr_pool_child_cleanup_set := nil;
  @apr_pool_cleanup_run := nil;
  @apr_pool_cleanup_null := nil;
  @apr_pool_cleanup_for_exec := nil;
  @apr_atomic_init := nil;
  @apr_atomic_read32 := nil;
  @apr_atomic_set32 := nil;
  @apr_atomic_add32 := nil;
  @apr_atomic_sub32 := nil;
  @apr_atomic_inc32 := nil;
  @apr_atomic_dec32 := nil;
  @apr_atomic_cas32 := nil;
  @apr_atomic_xchg32 := nil;
  @apr_atomic_casptr := nil;
  @apr_crypto_sha256_new := nil;
  @apr_random_init := nil;
  @apr_random_standard_new := nil;
  @apr_random_add_entropy := nil;
  @apr_random_insecure_bytes := nil;
  @apr_random_secure_bytes := nil;
  @apr_random_barrier := nil;
  @apr_random_secure_ready := nil;
  @apr_random_insecure_ready := nil;
  @apr_random_after_fork := nil;
  @apr_signal_block := nil;
  @apr_signal_unblock := nil;
  @apr_strnatcmp := nil;
  @apr_strnatcasecmp := nil;
  @apr_pstrdup := nil;
  @apr_pstrmemdup := nil;
  @apr_pstrndup := nil;
  @apr_pmemdup := nil;
  @apr_pstrcat := nil;
  @apr_pstrcatv := nil;
  @apr_pvsprintf := nil;
  @apr_psprintf := nil;
  @apr_cpystrn := nil;
  @apr_collapse_spaces := nil;
  @apr_tokenize_to_argv := nil;
  @apr_strtok := nil;
  @apr_snprintf := nil;
  @apr_vsnprintf := nil;
  @apr_itoa := nil;
  @apr_ltoa := nil;
  @apr_off_t_toa := nil;
  @apr_strtoff := nil;
  @apr_strtoi64 := nil;
  @apr_atoi64 := nil;
  @apr_strfsize := nil;
  @apr_shm_create := nil;
  @apr_shm_remove := nil;
  @apr_shm_destroy := nil;
  @apr_shm_attach := nil;
  @apr_shm_detach := nil;
  @apr_shm_baseaddr_get := nil;
  @apr_shm_size_get := nil;
  @apr_shm_pool_get := nil;
  @apr_hash_make := nil;
  @apr_hash_make_custom := nil;
  @apr_hash_copy := nil;
  @apr_hash_set := nil;
  @apr_hash_get := nil;
  @apr_hash_first := nil;
  @apr_hash_next := nil;
  @apr_hash_this := nil;
  @apr_hash_count := nil;
  @apr_hash_overlay := nil;
  @apr_hash_merge := nil;
  @apr_hash_pool_get := nil;
  @apr_table_elts := nil;
  @apr_is_empty_table := nil;
  @apr_is_empty_array := nil;
  @apr_array_make := nil;
  @apr_array_push := nil;
  @apr_array_pop := nil;
  @apr_array_cat := nil;
  @apr_array_copy := nil;
  @apr_array_copy_hdr := nil;
  @apr_array_append := nil;
  @apr_array_pstrcat := nil;
  @apr_table_make := nil;
  @apr_table_copy := nil;
  @apr_table_clear := nil;
  @apr_table_get := nil;
  @apr_table_set := nil;
  @apr_table_setn := nil;
  @apr_table_unset := nil;
  @apr_table_merge := nil;
  @apr_table_mergen := nil;
  @apr_table_add := nil;
  @apr_table_addn := nil;
  @apr_table_overlay := nil;
  @apr_table_do := nil;
  @apr_table_vdo := nil;
  @apr_table_overlap := nil;
  @apr_table_compress := nil;
  @apr_env_get := nil;
  @apr_env_set := nil;
  @apr_env_delete := nil;
  @apr_getopt_init := nil;
  @apr_getopt := nil;
  @apr_getopt_long := nil;
  @apr_dso_load := nil;
  @apr_dso_unload := nil;
  @apr_dso_sym := nil;
  @apr_dso_error := nil;
  @apr_uid_current := nil;
  @apr_uid_name_get := nil;
  @apr_uid_get := nil;
  @apr_uid_homepath_get := nil;
  @apr_uid_compare := nil;
  @apr_gid_name_get := nil;
  @apr_gid_get := nil;
  @apr_gid_compare := nil;
  @apr_month_snames := nil;
  @apr_day_snames := nil;
  @apr_time_now := nil;
  @apr_time_ansi_put := nil;
  @apr_time_exp_tz := nil;
  @apr_time_exp_gmt := nil;
  @apr_time_exp_lt := nil;
  @apr_time_exp_get := nil;
  @apr_time_exp_gmt_get := nil;
  @apr_sleep := nil;
  @apr_rfc822_date := nil;
  @apr_ctime := nil;
  @apr_strftime := nil;
  @apr_time_clock_hires := nil;
  @apr_stat := nil;
  @apr_dir_open := nil;
  @apr_dir_close := nil;
  @apr_dir_read := nil;
  @apr_dir_rewind := nil;
  @apr_filepath_root := nil;
  @apr_filepath_merge := nil;
  @apr_filepath_list_split := nil;
  @apr_filepath_list_merge := nil;
  @apr_filepath_get := nil;
  @apr_filepath_set := nil;
  @apr_filepath_encoding := nil;
  @apr_file_open := nil;
  @apr_file_close := nil;
  @apr_file_remove := nil;
  @apr_file_rename := nil;
  @apr_file_copy := nil;
  @apr_file_append := nil;
  @apr_file_eof := nil;
  @apr_file_open_stderr := nil;
  @apr_file_open_stdout := nil;
  @apr_file_open_stdin := nil;
  @apr_file_read := nil;
  @apr_file_write := nil;
  @apr_file_writev := nil;
  @apr_file_read_full := nil;
  @apr_file_write_full := nil;
  @apr_file_writev_full := nil;
  @apr_file_putc := nil;
  @apr_file_getc := nil;
  @apr_file_ungetc := nil;
  @apr_file_gets := nil;
  @apr_file_puts := nil;
  @apr_file_flush := nil;
  @apr_file_dup := nil;
  @apr_file_dup2 := nil;
  @apr_file_setaside := nil;
  @apr_file_seek := nil;
  @apr_file_pipe_create := nil;
  @apr_file_namedpipe_create := nil;
  @apr_file_pipe_timeout_get := nil;
  @apr_file_pipe_timeout_set := nil;
  @apr_file_lock := nil;
  @apr_file_unlock := nil;
  @apr_file_name_get := nil;
  @apr_file_data_get := nil;
  @apr_file_data_set := nil;
  @apr_file_printf := nil;
  @apr_file_perms_set := nil;
  @apr_file_attrs_set := nil;
  @apr_file_mtime_set := nil;
  @apr_dir_make := nil;
  @apr_dir_make_recursive := nil;
  @apr_dir_remove := nil;
  @apr_file_info_get := nil;
  @apr_file_trunc := nil;
  @apr_file_flags_get := nil;
  @apr_file_pool_get := nil;
  @apr_file_inherit_set := nil;
  @apr_file_inherit_unset := nil;
  @apr_file_mktemp := nil;
  @apr_temp_dir_get := nil;
  @apr_mmap_create := nil;
  @apr_mmap_dup := nil;
  @apr_mmap_delete := nil;
  @apr_mmap_offset := nil;
  @apr_socket_create := nil;
  @apr_socket_shutdown := nil;
  @apr_socket_close := nil;
  @apr_socket_bind := nil;
  @apr_socket_listen := nil;
  @apr_socket_accept := nil;
  @apr_socket_connect := nil;
  @apr_sockaddr_info_get := nil;
  @apr_getnameinfo := nil;
  @apr_parse_addr_port := nil;
  @apr_gethostname := nil;
  @apr_socket_data_get := nil;
  @apr_socket_send := nil;
  @apr_socket_sendv := nil;
  @apr_socket_sendto := nil;
  @apr_socket_recvfrom := nil;
  @apr_socket_sendfile := nil;
  @apr_socket_recv := nil;
  @apr_socket_opt_set := nil;
  @apr_socket_timeout_set := nil;
  @apr_socket_opt_get := nil;
  @apr_socket_timeout_get := nil;
  @apr_socket_atmark := nil;
  @apr_socket_addr_get := nil;
  @apr_sockaddr_ip_get := nil;
  @apr_sockaddr_equal := nil;
  @apr_socket_type_get := nil;
  @apr_getservbyname := nil;
  @apr_ipsubnet_create := nil;
  @apr_ipsubnet_test := nil;
  @apr_socket_protocol_get := nil;
  @apr_socket_inherit_set := nil;
  @apr_socket_set_inherit := nil;
  @apr_socket_inherit_unset := nil;
  @apr_socket_unset_inherit := nil;
  @apr_mcast_join := nil;
  @apr_mcast_leave := nil;
  @apr_mcast_hops := nil;
  @apr_mcast_loopback := nil;
  @apr_mcast_interface := nil;
  @apr_pollset_create := nil;
  @apr_pollset_destroy := nil;
  @apr_pollset_add := nil;
  @apr_pollset_remove := nil;
  @apr_pollset_poll := nil;
  @apr_poll := nil;
  @apr_thread_mutex_create := nil;
  @apr_thread_mutex_lock := nil;
  @apr_thread_mutex_trylock := nil;
  @apr_thread_mutex_unlock := nil;
  @apr_thread_mutex_destroy := nil;
  @apr_thread_mutex_pool_get := nil;
  @apr_thread_rwlock_create := nil;
  @apr_thread_rwlock_rdlock := nil;
  @apr_thread_rwlock_tryrdlock := nil;
  @apr_thread_rwlock_wrlock := nil;
  @apr_thread_rwlock_trywrlock := nil;
  @apr_thread_rwlock_unlock := nil;
  @apr_thread_rwlock_destroy := nil;
  @apr_thread_rwlock_pool_get := nil;
  @apr_thread_cond_create := nil;
  @apr_thread_cond_wait := nil;
  @apr_thread_cond_timedwait := nil;
  @apr_thread_cond_signal := nil;
  @apr_thread_cond_broadcast := nil;
  @apr_thread_cond_destroy := nil;
  @apr_thread_cond_pool_get := nil;
  @apr_threadattr_create := nil;
  @apr_threadattr_detach_set := nil;
  @apr_threadattr_detach_get := nil;
  @apr_threadattr_stacksize_set := nil;
  @apr_threadattr_guardsize_set := nil;
  @apr_thread_create := nil;
  @apr_thread_exit := nil;
  @apr_thread_join := nil;
  @apr_thread_yield := nil;
  @apr_thread_once_init := nil;
  @apr_thread_once := nil;
  @apr_thread_detach := nil;
  @apr_thread_data_get := nil;
  @apr_thread_data_set := nil;
  @apr_threadkey_private_create := nil;
  @apr_threadkey_private_get := nil;
  @apr_threadkey_private_set := nil;
  @apr_threadkey_private_delete := nil;
  @apr_threadkey_data_get := nil;
  @apr_threadkey_data_set := nil;
  @apr_procattr_create := nil;
  @apr_procattr_io_set := nil;
  @apr_procattr_child_in_set := nil;
  @apr_procattr_child_out_set := nil;
  @apr_procattr_child_err_set := nil;
  @apr_procattr_dir_set := nil;
  @apr_procattr_cmdtype_set := nil;
  @apr_procattr_detach_set := nil;
  @apr_procattr_child_errfn_set := nil;
  @apr_procattr_error_check_set := nil;
  @apr_procattr_addrspace_set := nil;
  @apr_procattr_user_set := nil;
  @apr_procattr_group_set := nil;
  @apr_proc_create := nil;
  @apr_proc_wait := nil;
  @apr_proc_wait_all_procs := nil;
  @apr_proc_detach := nil;
  @apr_proc_other_child_register := nil;
  @apr_proc_other_child_unregister := nil;
  @apr_proc_other_child_alert := nil;
  @apr_proc_other_child_refresh := nil;
  @apr_proc_other_child_refresh_all := nil;
  @apr_proc_kill := nil;
  @apr_pool_note_subprocess := nil;
  @apr_thread_pool_get := nil;
  @apr_proc_mutex_create := nil;
  @apr_proc_mutex_child_init := nil;
  @apr_proc_mutex_lock := nil;
  @apr_proc_mutex_trylock := nil;
  @apr_proc_mutex_unlock := nil;
  @apr_proc_mutex_destroy := nil;
  @apr_proc_mutex_cleanup := nil;
  @apr_proc_mutex_lockfile := nil;
  @apr_proc_mutex_name := nil;
  @apr_proc_mutex_defname := nil;
  @apr_proc_mutex_pool_get := nil;
  @apr_allocator_create := nil;
  @apr_allocator_destroy := nil;
  @apr_allocator_alloc := nil;
  @apr_allocator_free := nil;
  @apr_allocator_owner_set := nil;
  @apr_allocator_owner_get := nil;
  @apr_allocator_max_free_set := nil;
  @apr_allocator_mutex_set := nil;
  @apr_allocator_mutex_get := nil;
  @apr_fnmatch := nil;
  @apr_fnmatch_test := nil;
  @apr_is_fnmatch := nil;
  @apr_match_glob := nil;
  @apr_os_file_get := nil;
  @apr_os_dir_get := nil;
  @apr_os_sock_get := nil;
  @apr_os_proc_mutex_get := nil;
  @apr_os_exp_time_get := nil;
  @apr_os_imp_time_get := nil;
  @apr_os_shm_get := nil;
  @apr_os_thread_get := nil;
  @apr_os_threadkey_get := nil;
  @apr_os_thread_put := nil;
  @apr_os_threadkey_put := nil;
  @apr_os_thread_current := nil;
  @apr_os_thread_equal := nil;
  @apr_os_file_put := nil;
  @apr_os_pipe_put := nil;
  @apr_os_pipe_put_ex := nil;
  @apr_os_dir_put := nil;
  @apr_os_sock_put := nil;
  @apr_os_sock_make := nil;
  @apr_os_proc_mutex_put := nil;
  @apr_os_imp_time_put := nil;
  @apr_os_exp_time_put := nil;
  @apr_os_shm_put := nil;
  @apr_os_dso_handle_put := nil;
  @apr_os_dso_handle_get := nil;
  @apr_os_uuid_get := nil;
  @apr_os_default_encoding := nil;
  @apr_os_locale_encoding := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure AprCheck(Status: TAprStatus);

begin
  if Status <> APR_SUCCESS then
    RaiseAprError(Status);
end;

//----------------------------------------------------------------------------------------------------------------------

function GetAprErrorMessage(Status: TAprStatus): string;

var
  AnsiResult: AnsiString;

begin
  if Status = APR_SUCCESS then
    Result := ''
  else
  begin
    SetLength(AnsiResult, 256);
    apr_strerror(Status, PAnsiChar(AnsiResult), Length(AnsiResult));
    SetLength(Result, StrLen(PAnsiChar(AnsiResult)));
    Result := string(AnsiResult);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure RaiseAprError(Status: TAprStatus);

begin
  if Status <> APR_SUCCESS then
    raise EAprError.Create(Status);
end;

//----------------------------------------------------------------------------------------------------------------------

{ EAprError public }

//----------------------------------------------------------------------------------------------------------------------

constructor EAprError.Create(AErrorCode: TAprStatus);

begin
  inherited Create(GetAprErrorMessage(AErrorCode));
  FErrorCode := AErrorCode;
end;

initialization

finalization
  FreeAprLib;

//----------------------------------------------------------------------------------------------------------------------

end.