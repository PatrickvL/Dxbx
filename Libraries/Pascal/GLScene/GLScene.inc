//
// Heavily based on Delphi Free Stuff Include File
//                  by Brad Stowers: bstowers@pobox.com
//                  Delphi Free Stuff: http://www.delphifreestuff.com/
//
// History:
// 02/04/10 - Yar - Added GLS_NO_INLINE option
// 13/12/09 - DaStr - Added GLS_MULTITHREAD option (thanks Controller)
// 10/11/09 - DaStr - Added Delphi 2010 support
// 07/11/09 - DaStr - Improved FPC compatibility (BugtrackerID = 2893580)
//                    Added GLS_NO_ASM option
// 12/09/07 - DaStr - Added GLS_DELPHI_OR_CPPB
// 30/03/07 - DaStr - Added GLS_ENABLE_UNSAFE_WARNINGS
// 25/03/07 - DaStr - Added UNIX and KYLIX defines
// 14/01/07 - DaStr - Added GLS_DELPHI/COMPILER_XX_DOWN defines
// 20/12/06 - DaStr - Added GLS_OPTIMIZATIONS option
// 08/03/06 - ur - Added Delphi 2006 detection
// 08/07/04 - LR - BCB5 and BCB6 detection
// 30/08/02 - EG - Delphi 7 detection
// 24/01/02 - EG - Linux/Kylix changes (Puthoon)
// 28/12/01 - EG - Added GLS_Graphics32_SUPPORT
// 27/07/01 - EG - Renamed and made GLScene-specific
// 26/06/01 - EG - Added Delphi6 stuff
//

{$DEFINE GLSCENE}

//
// Additionnal support & dependencies
//

// Activate support for Alex Denissov's Graphics32 library (http://www.g32.org)
{.$DEFINE GLS_Graphics32_SUPPORT}

// Activate support for png images (using pngimage.pas library)
// GLS_PngImage_SUPPORT is automatically defined if Delphi>=2009 (see below)
// Also, Define "Auto" Resize To Nearest 8 Multiple Size
{.$DEFINE GLS_PngImage_SUPPORT}
{.$DEFINE GLS_PngImage_RESIZENEAREST}

// Activate support for some optimizations by disabling checks that occur very often
// Warning: it will disable some options (like ShowAxis and OptSaveGLStack)
{.$DEFINE GLS_OPTIMIZATIONS}

// Enable all "Unsafe Type" / "Unsafe Code" compiler warnings
// May be useful when debugging.
{.$DEFINE GLS_ENABLE_UNSAFE_WARNINGS}

// De-Activate assembly routines in VectorGeometry.pas
{.$DEFINE GEOMETRY_NO_ASM}

// De-Activate assembly routines in other GLS units.
{.$DEFINE GLS_NO_ASM}

// De-Activate inline directive in GLS units.
{.$DEFINE GLS_NO_INLINE}

// De-Activate regions in GLS units.
{.$DEFINE GLS_NO_REGIONS}

// Activate Logging
{$DEFINE GLS_LOGGING}

// Activate OpenGL debug mode
{.$DEFINE GLS_OPENGL_DEBUG}

// Activate Multithread support
{.$DEFINE GLS_MULTITHREAD}

//
// Platform Detection
//

{$IFNDEF MSWINDOWS}
  {$IFDEF WIN32}
    // MSWINDOWS wasn't defined in Delphi 5, but WIN32 was
    // so need to define MSWINDOWS here to provide Delphi 5 support
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

{$IFDEF GLS_MULTITHREAD}
  {$DEFINE MULTITHREADOPENGL}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE GEOMETRY_NO_ASM}
  {$DEFINE UNIX}
{$ENDIF}

// FPC
{$IFDEF FPC}
  {$DEFINE GEOMETRY_NO_ASM}
  {$DEFINE GLS_NO_ASM}
  {$TYPEINFO ON}
  {$INLINE ON}
  {$H+} // Enable long strings.
{$ENDIF}

// If it's not FPC, determine if C++ builder, or Delphi
{$IFNDEF FPC}
  {$DEFINE GLS_DELPHI_OR_CPPB}
  {$IFDEF BCB}
    {$DEFINE GLS_CPPB}
    // C++Builder requires this if you use Delphi components in run-time packages.
    {$ObjExportAll On}
  {$ELSE}
    {$DEFINE GLS_DELPHI}
  {$ENDIF}
{$ENDIF}

// .NET
{$IFDEF CLR}
  {$DEFINE GEOMETRY_NO_ASM}
  {$UNSAFECODE ON}
{$ENDIF}

// by default, GLX is not supported on windows, although windows implementations
// do exist
{$IFDEF MSWINDOWS}
  {$DEFINE SUPPORT_WGL}
  {$UNDEF SUPPORT_GLX}
{$ENDIF}

{$IFDEF UNIX}
  {$UNDEF SUPPORT_WGL}
  {$DEFINE SUPPORT_GLX}
{$ENDIF}

{$UNDEF GLS_UNSUPPORTED}

// If it's not FPC, we assume it's some version of Delphi or CPPB so check
// them all, basically this checking goes to end of file + throws an error
// if unsupported
{$IFNDEF FPC}
// Conditional expressions were introduced in Delphi 6
{$ifdef CONDITIONALEXPRESSIONS}

  // Delphi XE & C++ Builder XE
  {$if CompilerVersion>22}
    {$DEFINE GLS_COMPILER_XE_UP}
  {$elseif CompilerVersion=22}
    {$DEFINE GLS_COMPILER_2011}
    {$DEFINE GLS_COMPILER_XE_UP}
    {$DEFINE GLS_COMPILER_XE_DOWN}
  {$elseif CompilerVersion<23}
    {$DEFINE GLS_COMPILER_XE_DOWN}
  {$ifend}

  // Delphi 2010 & C++ Builder 2010
  {$if CompilerVersion>21}
    {$DEFINE GLS_COMPILER_2010_UP}
  {$elseif CompilerVersion=21}
    {$DEFINE GLS_COMPILER_2010}
    {$DEFINE GLS_COMPILER_2010_UP}
    {$DEFINE GLS_COMPILER_2010_DOWN}
  {$elseif CompilerVersion<21}
    {$DEFINE GLS_COMPILER_2010_DOWN}
  {$ifend}

  // Delphi 2009 & C++ Builder 2009
  {$if CompilerVersion>20}
    {$DEFINE GLS_COMPILER_2009_UP}
  {$elseif CompilerVersion=20}
    {$DEFINE GLS_COMPILER_2009}
    {$DEFINE GLS_COMPILER_2009_UP}
    {$DEFINE GLS_COMPILER_2009_DOWN}
  {$elseif CompilerVersion<20}
    {$DEFINE GLS_COMPILER_2009_DOWN}
  {$ifend}

  // Delphi 2007 & C++ Builder 2007
  {$if CompilerVersion>18.5}
    {$DEFINE GLS_COMPILER_2007_UP}
  {$elseif CompilerVersion=18.5}
    {$DEFINE GLS_COMPILER_2007}
    {$DEFINE GLS_COMPILER_2007_UP}
    {$DEFINE GLS_COMPILER_2007_DOWN}
  {$elseif CompilerVersion<18.5}
    {$DEFINE GLS_COMPILER_2007_DOWN}
  {$ifend}

  // Delphi 2006 & C++ Builder 2006
  {$if CompilerVersion>18.0}
    {$DEFINE GLS_COMPILER_2006_UP}
  {$elseif CompilerVersion=18.0}
    {$DEFINE GLS_COMPILER_2006}
    {$DEFINE GLS_COMPILER_2006_UP}
    {$DEFINE GLS_COMPILER_2006_DOWN}
  {$elseif CompilerVersion<18.0}
    {$DEFINE GLS_COMPILER_2006_DOWN}
  {$ifend}

  // Delphi 2005
  {$if CompilerVersion>17.0}
    {$DEFINE GLS_COMPILER_2005_UP}
  {$elseif CompilerVersion=17.0}
    {$DEFINE GLS_COMPILER_2005}
    {$DEFINE GLS_COMPILER_2005_UP}
    {$DEFINE GLS_COMPILER_2005_DOWN}
  {$elseif CompilerVersion<17.0}
    {$DEFINE GLS_COMPILER_2005_DOWN}
  {$ifend}

  // Delphi 8
  {$if CompilerVersion>16.0}
    {$DEFINE GLS_COMPILER_8_UP}
  {$elseif CompilerVersion=16.0}
    {$DEFINE GLS_COMPILER_8}
    {$DEFINE GLS_COMPILER_8_UP}
    {$DEFINE GLS_COMPILER_8_DOWN}
  {$elseif CompilerVersion<16.0}
    {$DEFINE GLS_COMPILER_8_DOWN}
  {$ifend}

  // Delphi 7
  {$if CompilerVersion>15.0}
    {$DEFINE GLS_COMPILER_7_UP}
  {$elseif CompilerVersion=15.0}
    {$DEFINE GLS_COMPILER_7}
    {$DEFINE GLS_COMPILER_7_UP}
    {$DEFINE GLS_COMPILER_7_DOWN}
  {$elseif CompilerVersion<15.0}
    {$DEFINE GLS_COMPILER_7_DOWN}
  {$ifend}

  // Delphi 6 & C++ Builder 6
  {$if CompilerVersion>14.0}
    {$DEFINE GLS_COMPILER_6_UP}
  {$elseif CompilerVersion=14.0}
    {$DEFINE GLS_COMPILER_6}
    {$DEFINE GLS_COMPILER_6_UP}
    {$DEFINE GLS_COMPILER_6_DOWN}
  {$elseif CompilerVersion<14.0}
    {$DEFINE GLS_COMPILER_6_DOWN}
  {$ifend}


  {$if CompilerVersion>13.0}
    {$DEFINE GLS_COMPILER_5_UP}
  {$elseif CompilerVersion=13.0}
    {$DEFINE GLS_COMPILER_5}
    {$DEFINE GLS_COMPILER_5_UP}
    {$DEFINE GLS_COMPILER_5_DOWN}
  {$elseif CompilerVersion<13.0}
    {$DEFINE GLS_COMPILER_5_DOWN}
  {$ifend}

   // We've already determined it's Delphi 5+
  {$DEFINE GLS_COMPILER_5_UP}

  // Friendly names
  {$IFDEF BCB}
    {$IFDEF GLS_COMPILER_5}
      {$DEFINE GLS_CPPB_5}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_6}
      {$DEFINE GLS_CPPB_6}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2006}
      {$DEFINE GLS_CPPB_2006}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2007}
      {$DEFINE GLS_CPPB_2007}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2009}
      {$DEFINE GLS_CPPB_2009}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2010}
      {$DEFINE GLS_CPPB_2010}
    {$ENDIF}
  {$ELSE}
    {$IFDEF GLS_COMPILER_5}
      {$DEFINE GLS_DELPHI_5}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_6}
      {$DEFINE GLS_DELPHI_6}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_7}
      {$DEFINE GLS_DELPHI_7}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_8}
      {$DEFINE GLS_DELPHI_8}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2005}
      {$DEFINE GLS_DELPHI_2005}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2006}
      {$DEFINE GLS_DELPHI_2006}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2007}
      {$DEFINE GLS_DELPHI_2007}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2009}
      {$DEFINE GLS_DELPHI_2009}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2010}
      {$DEFINE GLS_DELPHI_2010}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_XE}
      {$DEFINE GLS_DELPHI_XE}
    {$ENDIF}
  {$ENDIF}

  // UP Declarations
  {$IFDEF BCB}
    {$IFDEF GLS_COMPILER_5_UP}
      {$DEFINE GLS_CPPB_5_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_6_UP}
      {$DEFINE GLS_CPPB_6_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2006_UP}
      {$DEFINE GLS_CPPB_2006_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2007_UP}
      {$DEFINE GLS_CPPB_2007_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2009_UP}
      {$DEFINE GLS_CPPB_2009_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2010_UP}
      {$DEFINE GLS_CPPB_2010_UP}
    {$ENDIF}
  {$ELSE}
    {$IFDEF GLS_COMPILER_5_UP}
      {$DEFINE GLS_DELPHI_5_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_6_UP}
      {$DEFINE GLS_DELPHI_6_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_7_UP}
      {$DEFINE GLS_DELPHI_7_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_8_UP}
      {$DEFINE GLS_DELPHI_8_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2005_UP}
      {$DEFINE GLS_DELPHI_2005_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2006_UP}
      {$DEFINE GLS_DELPHI_2006_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2007_UP}
      {$DEFINE GLS_DELPHI_2007_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2009_UP}
      {$DEFINE GLS_DELPHI_2009_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2010_UP}
      {$DEFINE GLS_DELPHI_2010_UP}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_XE_UP}
      {$DEFINE GLS_DELPHI_XE_UP}
    {$ENDIF}
  {$ENDIF}

  // Down Declarations
  {$IFDEF BCB}
    {$IFDEF GLS_COMPILER_6_DOWN}
      {$DEFINE GLS_CPPB_6_DOWN}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2006_DOWN}
      {$DEFINE GLS_CPPB_2006_DOWN}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2007_DOWN}
      {$DEFINE GLS_CPPB_2007_DOWN}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2009_DOWN}
      {$DEFINE GLS_CPPB_2009_DOWN}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2010_DOWN}
      {$DEFINE GLS_CPPB_2010_DOWN}
    {$ENDIF}
  {$ELSE}
    {$IFDEF GLS_COMPILER_6_DOWN}
      {$DEFINE GLS_DELPHI_6_DOWN}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_7_DOWN}
      {$DEFINE GLS_DELPHI_7_DOWN}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_8_DOWN}
      {$DEFINE GLS_DELPHI_8_DOWN}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2005_DOWN}
      {$DEFINE GLS_DELPHI_2005_DOWN}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2006_DOWN}
      {$DEFINE GLS_DELPHI_2006_DOWN}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2007_DOWN}
      {$DEFINE GLS_DELPHI_2007_DOWN}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2009_DOWN}
      {$DEFINE GLS_DELPHI_2009_DOWN}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_2010_DOWN}
      {$DEFINE GLS_DELPHI_2010_DOWN}
    {$ENDIF}
    {$IFDEF GLS_COMPILER_XE_DOWN}
      {$DEFINE GLS_DELPHI_XE_DOWN}
    {$ENDIF}
  {$ENDIF}


 {$endif}

// need to check VERXXX prior to Delphi 6, when conditional expressions
// were introduced

// Delphi 5 & C++ Builder 5
{$IFDEF VER130}
  {$DEFINE GLS_COMPILER_5}
  {$DEFINE GLS_COMPILER_5_UP}
  {$IFDEF BCB}
    {$DEFINE GLS_CPPB_5}
    {$DEFINE GLS_CPPB_5_UP}
  {$ELSE}
    {$DEFINE GLS_DELPHI_5}
    {$DEFINE GLS_DELPHI_5_UP}
  {$ENDIF}
{$ENDIF}

// C++ Builder 4
{$IFDEF VER125}
  {$DEFINE GLS_UNSUPPORTED}
{$ENDIF}

// Delphi 4
{$IFDEF VER120}
  {$DEFINE GLS_UNSUPPORTED}
{$ENDIF}

// C++ Builder 3
{$IFDEF VER110}
  {$DEFINE GLS_UNSUPPORTED}
{$ENDIF}

// Delphi 3
{$IFDEF VER100}
  {$DEFINE GLS_UNSUPPORTED}
{$ENDIF}

// C++ Builder (1)
{$IFDEF VER93}
  {$DEFINE GLS_UNSUPPORTED}
{$ENDIF}

// Delphi 2
{$IFDEF VER90}
  {$DEFINE GLS_UNSUPPORTED}
{$ENDIF}

// Delphi (1)
{$IFDEF VER80}
  {$DEFINE GLS_UNSUPPORTED}
{$ENDIF}

{$IFNDEF GLS_CPPB}
  {$IFNDEF GLS_DELPHI}
    { Don't know what the hell it is.  Could be new version, or could be old BP. }
    {$DEFINE GLS_UNKNOWN_COMPILER}
  {$ENDIF}
{$ENDIF}

//_DOWN Defines

{$IFDEF GLS_COMPILER_5}
  {$DEFINE GLS_COMPILER_XE_DOWN}
  {$DEFINE GLS_COMPILER_2010_DOWN}
  {$DEFINE GLS_COMPILER_2009_DOWN}
  {$DEFINE GLS_COMPILER_2007_DOWN}
  {$DEFINE GLS_COMPILER_2006_DOWN}
  {$DEFINE GLS_COMPILER_2005_DOWN}
  {$DEFINE GLS_COMPILER_8_DOWN}
  {$DEFINE GLS_COMPILER_7_DOWN}
  {$DEFINE GLS_COMPILER_6_DOWN}
  {$DEFINE GLS_COMPILER_5_DOWN}
{$ENDIF}

{$IFDEF GLS_DELPHI_5}
  {$DEFINE GLS_DELPHI_XE_DOWN}
  {$DEFINE GLS_DELPHI_2010_DOWN}
  {$DEFINE GLS_DELPHI_2009_DOWN}
  {$DEFINE GLS_DELPHI_2007_DOWN}
  {$DEFINE GLS_DELPHI_2006_DOWN}
  {$DEFINE GLS_DELPHI_2005_DOWN}
  {$DEFINE GLS_DELPHI_8_DOWN}
  {$DEFINE GLS_DELPHI_7_DOWN}
  {$DEFINE GLS_DELPHI_6_DOWN}
  {$DEFINE GLS_DELPHI_5_DOWN}
{$ENDIF}

//CPP Defines

{$IFDEF GLS_CPPB_6}
  {$DEFINE GLS_CPPB_5_UP}
  {$DEFINE GLS_CPPB_6_UP}
{$ENDIF}

// GLS_ENABLE_UNSAFE_WARNINGS.
{$IFDEF GLS_COMPILER_7_UP}
  {$IFDEF GLS_ENABLE_UNSAFE_WARNINGS}
    {$WARN UNSAFE_TYPE ON}
    {$WARN UNSAFE_CODE ON}
    {$WARN UNSAFE_CAST ON}
  {$ELSE}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$ENDIF}
{$ENDIF}

{$ENDIF FPC}

{$IFDEF GLS_UNSUPPORTED}
  Error: unsupported version, requires at least Delphi 5
{$ENDIF}

{$IFNDEF GLS_NO_INLINE}
  {$IFDEF GLS_COMPILER_2005_UP}
  {$DEFINE GLS_INLINE}
  {$ENDIF}
  {$IFDEF FPC}
  {$DEFINE GLS_INLINE}
  {$ENDIF}
{$ENDIF}

{$IFNDEF GLS_NO_REGIONS}
  {$IFDEF GLS_COMPILER_2005_UP}
  {$DEFINE GLS_REGIONS}
  {$ENDIF}
  {$IFDEF FPC}
  {$DEFINE GLS_REGIONS}
  {$ENDIF}
{$ENDIF}

// PNG is supported by Delphi since Dephi 2009
{$IFDEF GLS_DELPHI_2009_UP}
  {$DEFINE GLS_PngImage_SUPPORT}
{$ENDIF}





