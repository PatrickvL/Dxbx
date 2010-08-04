{$IFNDEF DXBX_INC}
{$DEFINE DXBX_INC}

(*
    This file is part of Dxbx - a XBox emulator written in Delphi (ported over from cxbx)
    Copyright (C) 2010 Shadow_tj and other members of the development team.

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

// Dxbx specific compiler directives :

// DEBUG = Build setting. Enables DbgPrintf calls, just like in Cxbx
{$DEFINE _DEBUG_TRACE} // Enable DbgPrintf tracing calls, just like in Cxbx
{.$DEFINE ONLY_CXBX_LOGGING} // Disables the additional Dxbx logging, for logfile-comparisons

{$IFDEF ONLY_CXBX_LOGGING}
  // Don't set any Dxbx logging define, we do only Cxbx-compatible logging in this mode.
{$ELSE}
  // Here, we enable additional Dxbx logging :

  {$IFDEF DEBUG}
    {$DEFINE DXBX_DEBUG} // Additional Dxbx DbgPrintf calls
  {$ENDIF}

  {$IFDEF _DEBUG_TRACE}
    {$DEFINE DXBX_DEBUG_TRACE} // Additional Dxbx DbgPrintf traces
    {.$DEFINE DXBX_EXTREME_LOGGING}
    {.$DEFINE DXBX_EXTENSIVE_CALLSTACK_LOGGING}
  {$ENDIF}
{$ENDIF}

{$IFDEF DUNIT}
  // In Unit-test projects, do as much debugging as possible (see below):
  {$DEFINE _DEBUG}
{$ENDIF}

{$IFDEF DXBX_DLL}
  {$DEFINE DXBX_USE_JCLDEBUG}
{$ENDIF}

{.$DEFINE DXBX_RECTYPE} // Include RecType info in PatternTrie
{.$DEFINE DXBX_TRIE_COMPRESS_STRINGS} // Apply prefix reduction to string stored in the Trie
{.$DEFINE DXBX_TRACE_MEMLEAKS} // Enable memleak-tracing (FastMM4 needed)


// Apply our default compiler options before jedi.inc goes ahead :

{$ALIGN 4}                // Alignment of fields (align on 4 bytes, just like Cxbx)
{$ASSERTIONS ON}
{$BOOLEVAL OFF}           // Boolean short-circuit evaluation (don't evaluate more boolean expressions than nessecary)
{$DEBUGINFO ON}
{$DEFINITIONINFO ON}      // Compile information about where each identifier is defined (this is everything except REFERENCEINFO)
{$EXTENDEDSYNTAX ON}      // Extended syntax (turns on 'Function statements', 'The Result variable' and 'Null-terminated strings')
{$HINTS ON}               // Generate compiler hint messages
{$IMPORTEDDATA OFF}       // Disables creation of imported data references (1 opcode less in each use of globale variabeles)
{$IOCHECKS ON}            // I/O checking (generates EInOutError exceptions)
{$LOCALSYMBOLS ON}        // Enable the generation of local symbol information
{$LONGSTRINGS ON}         // Long strings (string is presumed AnsiString)
{$MINENUMSIZE 1}          // Minimum enumeration size (standard 1 byte)
{$REALCOMPATIBILITY OFF}  // Make Real equal to Double (instead of Real48)
{$SAFEDIVIDE OFF}         // Dont guards against the flawed FDIV instruction exhibited by certain early Pentium processors
{$STACKFRAMES OFF}        // Don't generate stack frames when that's not nessecary
{$TYPEDADDRESS OFF}       // Type-checked pointers (pointers aren't typed)
{$TYPEINFO OFF}           // Do not generate RTTI
{$WARNINGS ON}            // Generate compiler warning messages
{$WRITEABLECONST OFF}     // Writeable typed constants (constants may not be changed)

{$IF DECLARED(UnicodeString)}
  {$STRINGCHECKS OFF}     // Turn off slow (and very unneeded!) UnicodeString type checking
{$IFEND}

// In _DEBUG mode we want as much debugging features as possible :

{$IFDEF _DEBUG}
  {$DEFINE DEBUG} // Carry on like this is a DEBUG build

  {$FINITEFLOAT ON} // Raise an exception when there is an overflow, underflow, or invalid operation
  {$OPTIMIZATION OFF}
  {$OVERFLOWCHECKS ON}
  {$RANGECHECKS ON}
  {$REFERENCEINFO ON} // Compile information about where every identifier is used as well as where it is defined
  {$STACKFRAMES ON}

  {$UNDEF _OPTIMIZE_UNIT} // In _DEBUG mode, disable _OPTIMIZE_UNIT
{$ENDIF}

// In RELEASE mode we want to optimize code (while still keeping debugging features) :
{$IFDEF RELEASE}
  {$OPTIMIZATION ON}
  {$OVERFLOWCHECKS OFF}
{$ENDIF}

// When _OPTIMIZE_UNIT is defined (by the using unit), apply as much optimizations as possible :

{$IFDEF _OPTIMIZE_UNIT}
  {$ASSERTIONS OFF}
  {$FINITEFLOAT OFF} // Raise no exception when there is an overflow, underflow, or invalid operation
  {$IOCHECKS OFF}
  {$OPTIMIZATION ON}
  {$OVERFLOWCHECKS OFF}
  {$RANGECHECKS OFF}
{$ENDIF ~_OPTIMIZE_UNIT}

// Let all other compiler options depend on the project options.

{$INCLUDE jedi.inc} // Pull in the JEDI shared directives

// Additional feature directive checks :

{$IFDEF COMPILER6_UP}
  {$DEFINE SUPPORTS_SETPEFLAGS}             // Compiler supports the SetPEFlags directives (D6+)
  {$DEFINE SUPPORTS_EXPERIMENTAL_WARNINGS}  // Compiler supports the WARN SYMBOL_EXPERIMENTAL and WARN UNIT_EXPERIMENTAL directives (D6+)
{$ENDIF}

{$IFDEF COMPILER9_UP}
  {$DEFINE SUPPORTS_REGION}     // Compiler supports the REGION and ENDREGION directives (D9+)
  {$DEFINE HAS_UNIT_HTTPPROD}   // Unit HTTPProd exists (BDS3+)
{$ENDIF}

{$IFDEF COMPILER10_UP}
  {$DEFINE SUPPORTS_METHODINFO}  // Compiler supports the METHODINFO directives (D10+)
{$ENDIF}

{$IFDEF COMPILER12_UP}
  {$DEFINE SUPPORTS_POINTERMATH}  // Compiler supports pointer-math (D12+)
{$ENDIF}

{$UNDEF SUPPORTS_INLINE_ASM} // Alas, as-of-yet Delphi can't do asm inlining...

// Enable and disable a few compiler warnings :

{$IFDEF SUPPORTS_PLATFORM_WARNINGS}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

{$IFDEF SUPPORTS_LIBRARY_WARNINGS}
  {$WARN SYMBOL_LIBRARY ON}
  {$WARN UNIT_LIBRARY OFF}
{$ENDIF}

{$IFDEF SUPPORTS_DEPRECATED_WARNINGS}
  {$IFDEF DUNIT}
    {$WARN SYMBOL_DEPRECATED OFF}
  {$ELSE}
    {$WARN SYMBOL_DEPRECATED ON}
  {$ENDIF}
  {$WARN UNIT_DEPRECATED ON}
{$ENDIF}

{$IFDEF SUPPORTS_EXPERIMENTAL_WARNINGS}
  {$WARN SYMBOL_EXPERIMENTAL ON}
  {$WARN UNIT_EXPERIMENTAL ON}
{$ENDIF}

{$IFDEF SUPPORTS_UNSAFE_WARNINGS}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

// End of Dxbx specific compiler settings

{$ENDIF ~DXBX_INC}


