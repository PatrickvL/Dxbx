//
// This unit is part of the GLScene Project, http://glscene.org
//
{: OpenGLTokens<p>

 <b>History : </b><font size=-1><ul>
      <li>23/01/11 - DanB - Added OpenGL procedural types, OpenGL 4.1 + ARB extensions
      <li>14/10/10 - Yar - Moved GLU types and constants from OpenGL1x
      <li>03/08/10 - Yar - Moved from OpenGL1x types and constants
   </ul></font>
}
unit OpenGLTokens;

interface

{$I GLScene.inc}

uses
  VectorTypes,
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows
{$ENDIF }

{$IFDEF UNIX}
  Types,
  LCLType,
  X,
  Xlib,
  XUtil,
  dynlibs
{$ENDIF}

{$IFDEF DARWIN}
  MacOsAll
{$ENDIF}
  ;

const
{$IFDEF MSWINDOWS}
  opengl32 = 'OpenGL32.dll';
  glu32 = 'GLU32.dll';
{$ENDIF}

{$IFDEF UNIX}
  opengl32 = 'libGL.so';
  glu32 = 'libGLU.so';
{$ENDIF}

{$IFDEF DARWIN}
  opengl32  = '/System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib';
  glu32 = '/System/Library/Frameworks/OpenGL.framework/Libraries/libGLU.dylib';
  libAGL = '/System/Library/Frameworks/AGL.framework/AGL';
{$ENDIF}

type

  PGLChar = PAnsiChar;
  TGLString = AnsiString;

  GLenum = UINT;
  TGLenum = UINT;
  PGLenum = ^TGLenum;

  GLboolean = BYTEBOOL;
  TGLboolean = BYTEBOOL;
  PGLboolean = ^TGLboolean;

  GLbitfield = UINT;
  TGLbitfield = UINT;
  PGLbitfield = ^TGLbitfield;

  GLbyte = ShortInt;
  TGLbyte = ShortInt;
  PGLbyte = ^TGLbyte;

  GLshort = SmallInt;
  TGLshort = SmallInt;
  PGLshort = ^TGLshort;

  GLint = Integer;
  TGLint = Integer;
  PGLint = ^Integer;

  GLsizei = Integer;
  TGLsizei = Integer;
  PGLsizei = ^TGLsizei;

  GLint64 = Int64;
  TGLint64 = Int64;
  PGLint64 = ^TGLInt64;

  GLint64EXT = Int64;
  TGLint64EXT = Int64;
  PGLint64EXT = ^TGLint64EXT;

{$IFNDEF GLS_DELPHI_7_DOWN}
  GLuint64 = UInt64;
  TGLuint64 = UInt64;
  PGLuint64 = ^TGLuint64;

  GLuint64EXT = UInt64;
  TGLuint64EXT = UInt64;
  PGLuint64EXT = ^TGLuint64EXT;
{$ELSE}
  // fake UInt64 by using Int64 for Delphi5 + 6
  GLuint64 = Int64;
  TGLuint64 = Int64;
  PGLuint64 = ^TGLuint64;

  GLuint64EXT = Int64;
  TGLuint64EXT = Int64;
  PGLuint64EXT = ^TGLuint64EXT;
{$ENDIF}

  GLubyte = Byte;
  TGLubyte = Byte;
  PGLubyte = {$IFDEF GLS_DELPHI_5}Windows.PByte{$ELSE}System.PByte{$ENDIF};

  GLushort = Word;
  TGLushort = Word;
  PGLushort = {$IFDEF GLS_DELPHI_5}Windows.PWord{$ELSE}System.PWord{$ENDIF};

  GLuint = UINT;
  TGLuint = UINT;
  PGLuint = ^TGLuint;

  GLfloat = Single;
  TGLfloat = Single;
  PGLfloat = {$IFDEF GLS_DELPHI_5}Windows.PSingle{$ELSE}System.PSingle{$ENDIF};

  GLclampf = Single;
  TGLclampf = Single;
  PGLclampf = ^TGLclampf;

  GLdouble = Double;
  TGLdouble = Double;
  PGLdouble = {$IFDEF GLS_DELPHI_5}Windows.PDouble{$ELSE}System.PDouble{$ENDIF};

  GLclampd = Double;
  TGLclampd = Double;
  PGLclampd = ^TGLclampd;

  GLhandleARB = Cardinal;
  PGLhandleARB = ^GLhandleARB;

  PGLPCharArray = ^PGLChar;

  PGLvoid = Pointer;

  PGLPointer = ^Pointer;

  // the size of these depend on platform (32bit or 64bit)
{$IFDEF FPC}
  GLintptr = PtrInt;
  TGLintptr = PtrInt;
  GLsizeiptr = SizeInt;
  TGLsizeiptr = SizeInt;
  GLsync = PtrInt;
  TGLsync = PtrInt;
{$ELSE}
{$IFDEF GLS_DELPHI_2009_UP}
  GLintptr = NativeInt;
  TGLintptr = NativeInt;
  GLsizeiptr = NativeInt;
  TGLsizeiptr = NativeInt;
  GLsync = NativeInt;
  TGLsync = NativeInt;
{$ELSE}
  GLintptr = Integer;
  TGLintptr = Integer;
  GLsizeiptr = Integer;
  TGLsizeiptr = Integer;
  GLsync = Integer;
  TGLsync = Integer;
{$ENDIF}
{$ENDIF}

  // Windows types
{$IFDEF MSWINDOWS}
  PWGLSwap = ^TWGLSwap;
  _WGLSWAP = packed record
    hdc: HDC;
    uiFlags: UINT;
  end;
  TWGLSwap = _WGLSWAP;
  WGLSWAP = _WGLSWAP;
  HPBUFFERARB = Integer;

{$ENDIF}

  // Unix types
{$IFDEF UNIX}
  XPixmap = TXID;
  XFont = TXID;
  XColormap = TXID;

  GLXContext = Pointer;
  GLXPixmap = TXID;
  GLXDrawable = TXID;

  // GLX 1.3 and later
  GLXFBConfig = Pointer;
  GLXFBConfigID = TXID;
  GLXContextID = TXID;
  GLXWindow = TXID;
  GLXPbuffer = TXID;
  //GLX 1.4
  GLXVideoSourceSGIX = TXID;
  GLXFBConfigSGIX = Pointer;
  GLXFBConfigIDSGIX = TXID;
  GLXPbufferSGIX = TXID;
  TGLXBufferClobberEventSGIX = record
    count: GLint;
    display: PDisplay;
    draw_type: GLint;
    drawable: GLXDrawable;
    event_type: GLint;
    mask: GLuint; //need test
    send_event: GLboolean;
    serial: GLint; //need test
    _type: GLuint;
    width, height: GLint;
    x, y: GLint;
  end;
  GLXBufferClobberEventSGIX = ^TGLXBufferClobberEventSGIX;
  GLXVideoDeviceNV = PGLuint;
  GLXVideoCaptureDeviceNV = TXID;
{$ENDIF}

{$IFDEF DARWIN}
const
   AGL_VERSION_2_0 = 1;

type
   TGDHandle = ptrint;
   TCGrafPtr = Pointer;

   PAGLDevice = ^TAGLDevice;
   TAGLDevice = TGDHandle;

   PAGLDrawable = ^TAGLDrawable;
   TAGLDrawable = TCGrafPtr;

   TAGLRendererInfo = Pointer;

   TAGLPixelFormat = Pointer;

   TAGLContext = Pointer;

   TAGLPbuffer = Pointer;
   PAGLPbuffer = ^TAGLPbuffer;
{$ENDIF}

{$IFDEF SUPPORT_WGL}
type
  PHGPUNV = ^HGPUNV;
  HGPUNV = THandle;

  PGPUDevice = ^TGPUDevice;
  TGPUDevice = record
    cb: Cardinal;
    DeviceName: array[0..31] of AnsiChar;
    DeviceString: array[0..127] of AnsiChar;
    Flags: Cardinal;
    rcVirtualScreen: TRect;
  end;
{$ENDIF}

  TDebugProc = procedure(
    source: GLenum;
    type_: GLenum;
    id: GLuint;
    severity: GLenum;
    length: GLsizei;
    const message: PGLchar;
    userParam: Pointer);
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}
  TGLDEBUGPROCARB = TDebugProc;

  TDebugProcAMD = procedure(
    id: GLuint;
    category: GLenum;
    severity: GLenum;
    length: GLsizei;
    message: PGLchar;
    userParam: Pointer);
{$IFDEF MSWINDOWS}stdcall;
{$ENDIF}{$IFDEF UNIX}cdecl;
{$ENDIF}

  TGLvdpauSurfaceNV = GLintptr;
  PGLvdpauSurfaceNV = ^TGLvdpauSurfaceNV;

{$IFDEF GLS_REGIONS} {$REGION 'OpenGL Utility (GLU) types'} {$ENDIF}
type
   // GLU types
   TGLUNurbs = record end;
   TGLUQuadric = record end;
   TGLUTesselator = record end;

   PGLUNurbs = ^TGLUNurbs;
   PGLUQuadric = ^TGLUQuadric;
   PGLUTesselator=  ^TGLUTesselator;

   // backwards compatibility
   TGLUNurbsObj = TGLUNurbs;
   TGLUQuadricObj = TGLUQuadric;
   TGLUTesselatorObj = TGLUTesselator;
   TGLUTriangulatorObj = TGLUTesselator;

   PGLUNurbsObj = PGLUNurbs;
   PGLUQuadricObj = PGLUQuadric;
   PGLUTesselatorObj = PGLUTesselator;
   PGLUTriangulatorObj = PGLUTesselator;

   // Callback function prototypes
   // GLUQuadricCallback
   TGLUQuadricErrorProc = procedure(errorCode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GLUTessCallback
   TGLUTessBeginProc = procedure(AType: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessEdgeFlagProc = procedure(Flag: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessVertexProc = procedure(VertexData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessEndProc = procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessErrorProc = procedure(ErrNo: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessCombineProc = procedure(const Coords: TVector3d; const VertexData: TVector4p; const Weight: TVector4f; OutData: PGLPointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessBeginDataProc = procedure(AType: TGLEnum; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessEdgeFlagDataProc = procedure(Flag: TGLboolean; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessVertexDataProc = procedure(VertexData: Pointer; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessEndDataProc = procedure(UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessErrorDataProc = procedure(ErrNo: TGLEnum; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
   TGLUTessCombineDataProc = procedure(const Coords: TVector3d; const VertexData: TVector4p; const Weight: TVector4f; OutData: PGLPointer; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

   // GLUNurbsCallback
   TGLUNurbsErrorProc = procedure(ErrorCode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

{$IFDEF GLS_REGIONS} {$ENDREGION} {$ENDIF}

const

{$IFDEF GLS_REGIONS}{$REGION 'OpenGL v1.1 generic constants'}{$ENDIF}
  // ********** GL generic constants **********

  // attribute bits
  GL_DEPTH_BUFFER_BIT = $00000100;
  GL_STENCIL_BUFFER_BIT = $00000400;
  GL_COLOR_BUFFER_BIT = $00004000;

  // boolean values
  GL_FALSE = 0;
  GL_TRUE = 1;

  // primitives
  GL_POINTS = $0000;
  GL_LINES = $0001;
  GL_LINE_LOOP = $0002;
  GL_LINE_STRIP = $0003;
  GL_TRIANGLES = $0004;
  GL_TRIANGLE_STRIP = $0005;
  GL_TRIANGLE_FAN = $0006;

  // AlphaFunction
  GL_NEVER = $0200;
  GL_LESS = $0201;
  GL_EQUAL = $0202;
  GL_LEQUAL = $0203;
  GL_GREATER = $0204;
  GL_NOTEQUAL = $0205;
  GL_GEQUAL = $0206;
  GL_ALWAYS = $0207;

  // blending
  GL_ZERO = 0;
  GL_ONE = 1;
  GL_SRC_COLOR = $0300;
  GL_ONE_MINUS_SRC_COLOR = $0301;
  GL_SRC_ALPHA = $0302;
  GL_ONE_MINUS_SRC_ALPHA = $0303;
  GL_DST_ALPHA = $0304;
  GL_ONE_MINUS_DST_ALPHA = $0305;
  GL_DST_COLOR = $0306;
  GL_ONE_MINUS_DST_COLOR = $0307;
  GL_SRC_ALPHA_SATURATE = $0308;

  // buffers
  GL_NONE = 0;
  GL_FRONT_LEFT = $0400;
  GL_FRONT_RIGHT = $0401;
  GL_BACK_LEFT = $0402;
  GL_BACK_RIGHT = $0403;
  GL_FRONT = $0404;
  GL_BACK = $0405;
  GL_LEFT = $0406;
  GL_RIGHT = $0407;
  GL_FRONT_AND_BACK = $0408;

  // errors
  GL_NO_ERROR = 0;
  GL_INVALID_ENUM = $0500;
  GL_INVALID_VALUE = $0501;
  GL_INVALID_OPERATION = $0502;
  GL_OUT_OF_MEMORY = $0505;

  // FrontFaceDirection
  GL_CW = $0900;
  GL_CCW = $0901;

  // points
  GL_POINT_SIZE = $0B11;
  GL_POINT_SIZE_RANGE = $0B12;
  GL_POINT_SIZE_GRANULARITY = $0B13;

  // lines
  GL_LINE_SMOOTH = $0B20;
  GL_LINE_WIDTH = $0B21;
  GL_LINE_WIDTH_RANGE = $0B22;
  GL_LINE_WIDTH_GRANULARITY = $0B23;

  // polygons
  GL_POLYGON_SMOOTH = $0B41;
  GL_CULL_FACE = $0B44;
  GL_CULL_FACE_MODE = $0B45;
  GL_FRONT_FACE = $0B46;

  // depth buffer
  GL_DEPTH_RANGE = $0B70;
  GL_DEPTH_TEST = $0B71;
  GL_DEPTH_WRITEMASK = $0B72;
  GL_DEPTH_CLEAR_VALUE = $0B73;
  GL_DEPTH_FUNC = $0B74;

  // stenciling
  GL_STENCIL_TEST = $0B90;
  GL_STENCIL_CLEAR_VALUE = $0B91;
  GL_STENCIL_FUNC = $0B92;
  GL_STENCIL_VALUE_MASK = $0B93;
  GL_STENCIL_FAIL = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS = $0B96;
  GL_STENCIL_REF = $0B97;
  GL_STENCIL_WRITEMASK = $0B98;

  GL_MATRIX_MODE = $0BA0;

  GL_VIEWPORT = $0BA2;

  // miscellaneous
  GL_DITHER = $0BD0;

  GL_BLEND_DST = $0BE0;
  GL_BLEND_SRC = $0BE1;
  GL_BLEND = $0BE2;

  GL_LOGIC_OP_MODE = $0BF0;
  GL_COLOR_LOGIC_OP = $0BF2;

  GL_DRAW_BUFFER = $0C01;
  GL_READ_BUFFER = $0C02;

  GL_SCISSOR_BOX = $0C10;
  GL_SCISSOR_TEST = $0C11;
  GL_COLOR_CLEAR_VALUE = $0C22;
  GL_COLOR_WRITEMASK = $0C23;

  GL_DOUBLEBUFFER = $0C32;
  GL_STEREO = $0C33;

  GL_LINE_SMOOTH_HINT = $0C52;
  GL_POLYGON_SMOOTH_HINT = $0C53;

  // pixel mode, transfer
  GL_UNPACK_SWAP_BYTES = $0CF0;
  GL_UNPACK_LSB_FIRST = $0CF1;
  GL_UNPACK_ROW_LENGTH = $0CF2;
  GL_UNPACK_SKIP_ROWS = $0CF3;
  GL_UNPACK_SKIP_PIXELS = $0CF4;
  GL_UNPACK_ALIGNMENT = $0CF5;
  GL_PACK_SWAP_BYTES = $0D00;
  GL_PACK_LSB_FIRST = $0D01;
  GL_PACK_ROW_LENGTH = $0D02;
  GL_PACK_SKIP_ROWS = $0D03;
  GL_PACK_SKIP_PIXELS = $0D04;
  GL_PACK_ALIGNMENT = $0D05;

  GL_MAX_TEXTURE_SIZE = $0D33;
  GL_MAX_VIEWPORT_DIMS = $0D3A;
  GL_SUBPIXEL_BITS = $0D50;

  GL_TEXTURE_1D = $0DE0;
  GL_TEXTURE_2D = $0DE1;

  GL_POLYGON_OFFSET_UNITS = $2A00;
  GL_POLYGON_OFFSET_POINT = $2A01;
  GL_POLYGON_OFFSET_LINE = $2A02;
  GL_POLYGON_OFFSET_FILL = $8037;
  GL_POLYGON_OFFSET_FACTOR = $8038;
  GL_TEXTURE_BINDING_1D = $8068;
  GL_TEXTURE_BINDING_2D = $8069;

  // texture mapping
  GL_TEXTURE_WIDTH = $1000;
  GL_TEXTURE_HEIGHT = $1001;
  GL_TEXTURE_INTERNAL_FORMAT = $1003;
  GL_TEXTURE_BORDER_COLOR = $1004;
  GL_TEXTURE_BORDER = $1005;
  GL_TEXTURE_RED_SIZE = $805C;
  GL_TEXTURE_GREEN_SIZE = $805D;
  GL_TEXTURE_BLUE_SIZE = $805E;
  GL_TEXTURE_ALPHA_SIZE = $805F;

  // hints
  GL_DONT_CARE = $1100;
  GL_FASTEST = $1101;
  GL_NICEST = $1102;

  // data types
  GL_BYTE = $1400;
  GL_UNSIGNED_BYTE = $1401;
  GL_SHORT = $1402;
  GL_UNSIGNED_SHORT = $1403;
  GL_INT = $1404;
  GL_UNSIGNED_INT = $1405;
  GL_FLOAT = $1406;
  GL_DOUBLE = $140A;

  // logic operations
  GL_CLEAR = $1500;
  GL_AND = $1501;
  GL_AND_REVERSE = $1502;
  GL_COPY = $1503;
  GL_AND_INVERTED = $1504;
  GL_NOOP = $1505;
  GL_XOR = $1506;
  GL_OR = $1507;
  GL_NOR = $1508;
  GL_EQUIV = $1509;
  GL_INVERT = $150A;
  GL_OR_REVERSE = $150B;
  GL_COPY_INVERTED = $150C;
  GL_OR_INVERTED = $150D;
  GL_NAND = $150E;
  GL_SET = $150F;

  GL_TEXTURE = $1702; // (for gl3.h, FBO attachment type)

  // PixelCopyType
  GL_COLOR = $1800;
  GL_DEPTH = $1801;
  GL_STENCIL = $1802;

  // pixel formats
  GL_STENCIL_INDEX = $1901;
  GL_DEPTH_COMPONENT = $1902;
  GL_RED = $1903;
  GL_GREEN = $1904;
  GL_BLUE = $1905;
  GL_ALPHA = $1906;
  GL_RGB = $1907;
  GL_RGBA = $1908;

  // PolygonMode
  GL_POINT = $1B00;
  GL_LINE = $1B01;
  GL_FILL = $1B02;

  // StencilOp
  GL_KEEP = $1E00;
  GL_REPLACE = $1E01;
  GL_INCR = $1E02;
  GL_DECR = $1E03;

  // implementation strings
  GL_VENDOR = $1F00;
  GL_RENDERER = $1F01;
  GL_VERSION = $1F02;
  GL_EXTENSIONS = $1F03;

  GL_NEAREST = $2600;
  GL_LINEAR = $2601;
  GL_NEAREST_MIPMAP_NEAREST = $2700;
  GL_LINEAR_MIPMAP_NEAREST = $2701;
  GL_NEAREST_MIPMAP_LINEAR = $2702;
  GL_LINEAR_MIPMAP_LINEAR = $2703;
  GL_TEXTURE_MAG_FILTER = $2800;
  GL_TEXTURE_MIN_FILTER = $2801;
  GL_TEXTURE_WRAP_S = $2802;
  GL_TEXTURE_WRAP_T = $2803;
  GL_PROXY_TEXTURE_1D = $8063;
  GL_PROXY_TEXTURE_2D = $8064;
  GL_REPEAT = $2901;

  // pixel formats
  GL_R3_G3_B2 = $2A10;
  GL_RGB4 = $804F;
  GL_RGB5 = $8050;
  GL_RGB8 = $8051;
  GL_RGB10 = $8052;
  GL_RGB12 = $8053;
  GL_RGB16 = $8054;
  GL_RGBA2 = $8055;
  GL_RGBA4 = $8056;
  GL_RGB5_A1 = $8057;
  GL_RGBA8 = $8058;
  GL_RGB10_A2 = $8059;
  GL_RGBA12 = $805A;
  GL_RGBA16 = $805B;

{$IFDEF GLS_REGIONS}{$REGION 'OpenGL 1.1 deprecated'}{$ENDIF}
  // attribute bits
  GL_CURRENT_BIT = $00000001 {deprecated};
  GL_POINT_BIT = $00000002 {deprecated};
  GL_LINE_BIT = $00000004 {deprecated};
  GL_POLYGON_BIT = $00000008 {deprecated};
  GL_POLYGON_STIPPLE_BIT = $00000010 {deprecated};
  GL_PIXEL_MODE_BIT = $00000020 {deprecated};
  GL_LIGHTING_BIT = $00000040 {deprecated};
  GL_FOG_BIT = $00000080 {deprecated};
  GL_ACCUM_BUFFER_BIT = $00000200 {deprecated};
  GL_VIEWPORT_BIT = $00000800 {deprecated};
  GL_TRANSFORM_BIT = $00001000 {deprecated};
  GL_ENABLE_BIT = $00002000 {deprecated};
  GL_HINT_BIT = $00008000 {deprecated};
  GL_EVAL_BIT = $00010000 {deprecated};
  GL_LIST_BIT = $00020000 {deprecated};
  GL_TEXTURE_BIT = $00040000 {deprecated};
  GL_SCISSOR_BIT = $00080000 {deprecated};
  // changed from $000FFFFF to $FFFFFFFF in OpenGL 1.3
  GL_ALL_ATTRIB_BITS = $FFFFFFFF {deprecated};

  // client attribute bits
  GL_CLIENT_PIXEL_STORE_BIT = $00000001 {deprecated};
  GL_CLIENT_VERTEX_ARRAY_BIT = $00000002 {deprecated};
  GL_CLIENT_ALL_ATTRIB_BITS = $FFFFFFFF {deprecated};

  // primitives
  GL_QUADS = $0007 {deprecated};
  GL_QUAD_STRIP = $0008 {deprecated};
  GL_POLYGON = $0009 {deprecated};

  // accumulation buffer
  GL_ACCUM = $0100 {deprecated};
  GL_LOAD = $0101 {deprecated};
  GL_RETURN = $0102 {deprecated};
  GL_MULT = $0103 {deprecated};
  GL_ADD = $0104 {deprecated};

  // buffers
  GL_AUX0 = $0409 {deprecated};
  GL_AUX1 = $040A {deprecated};
  GL_AUX2 = $040B {deprecated};
  GL_AUX3 = $040C {deprecated};

  // errors
  GL_STACK_OVERFLOW = $0503 {deprecated};
  GL_STACK_UNDERFLOW = $0504 {deprecated};

  // feedback types
  GL_2D = $0600 {deprecated};
  GL_3D = $0601 {deprecated};
  GL_3D_COLOR = $0602 {deprecated};
  GL_3D_COLOR_TEXTURE = $0603 {deprecated};
  GL_4D_COLOR_TEXTURE = $0604 {deprecated};

  // feedback tokens
  GL_PASS_THROUGH_TOKEN = $0700 {deprecated};
  GL_POINT_TOKEN = $0701 {deprecated};
  GL_LINE_TOKEN = $0702 {deprecated};
  GL_POLYGON_TOKEN = $0703 {deprecated};
  GL_BITMAP_TOKEN = $0704 {deprecated};
  GL_DRAW_PIXEL_TOKEN = $0705 {deprecated};
  GL_COPY_PIXEL_TOKEN = $0706 {deprecated};
  GL_LINE_RESET_TOKEN = $0707 {deprecated};

  // fog
  GL_EXP = $0800 {deprecated};
  GL_EXP2 = $0801 {deprecated};

  // evaluators
  GL_COEFF = $0A00 {deprecated};
  GL_ORDER = $0A01 {deprecated};
  GL_DOMAIN = $0A02 {deprecated};

  // gets
  GL_CURRENT_COLOR = $0B00 {deprecated};
  GL_CURRENT_INDEX = $0B01 {deprecated};
  GL_CURRENT_NORMAL = $0B02 {deprecated};
  GL_CURRENT_TEXTURE_COORDS = $0B03 {deprecated};
  GL_CURRENT_RASTER_COLOR = $0B04 {deprecated};
  GL_CURRENT_RASTER_INDEX = $0B05 {deprecated};
  GL_CURRENT_RASTER_TEXTURE_COORDS = $0B06 {deprecated};
  GL_CURRENT_RASTER_POSITION = $0B07 {deprecated};
  GL_CURRENT_RASTER_POSITION_VALID = $0B08 {deprecated};
  GL_CURRENT_RASTER_DISTANCE = $0B09 {deprecated};

  // points
  GL_POINT_SMOOTH = $0B10 {deprecated};

  // lines
  GL_LINE_STIPPLE = $0B24 {deprecated};
  GL_LINE_STIPPLE_PATTERN = $0B25 {deprecated};
  GL_LINE_STIPPLE_REPEAT = $0B26 {deprecated};

  // display lists
  GL_LIST_MODE = $0B30 {deprecated};
  GL_MAX_LIST_NESTING = $0B31 {deprecated};
  GL_LIST_BASE = $0B32 {deprecated};
  GL_LIST_INDEX = $0B33 {deprecated};

  // polygons
  // DanB - not sure "GL_POLYGON_MODE" should be deprecated, but it is marked
  // deprecated in OpenGL spec, so will put it here for now
  GL_POLYGON_MODE = $0B40 {deprecated};
  GL_POLYGON_STIPPLE = $0B42 {deprecated};
  GL_EDGE_FLAG = $0B43 {deprecated};

  // lighting
  GL_LIGHTING = $0B50 {deprecated};
  GL_LIGHT_MODEL_LOCAL_VIEWER = $0B51 {deprecated};
  GL_LIGHT_MODEL_TWO_SIDE = $0B52 {deprecated};
  GL_LIGHT_MODEL_AMBIENT = $0B53 {deprecated};
  GL_SHADE_MODEL = $0B54 {deprecated};

  // color material
  GL_COLOR_MATERIAL_FACE = $0B55 {deprecated};
  GL_COLOR_MATERIAL_PARAMETER = $0B56 {deprecated};
  GL_COLOR_MATERIAL = $0B57 {deprecated};

  // fog
  GL_FOG = $0B60 {deprecated};
  GL_FOG_INDEX = $0B61 {deprecated};
  GL_FOG_DENSITY = $0B62 {deprecated};
  GL_FOG_START = $0B63 {deprecated};
  GL_FOG_END = $0B64 {deprecated};
  GL_FOG_MODE = $0B65 {deprecated};
  GL_FOG_COLOR = $0B66 {deprecated};

  GL_ACCUM_CLEAR_VALUE = $0B80 {deprecated};

  GL_NORMALIZE = $0BA1 {deprecated};
  GL_MODELVIEW_STACK_DEPTH = $0BA3 {deprecated};
  GL_PROJECTION_STACK_DEPTH = $0BA4 {deprecated};
  GL_TEXTURE_STACK_DEPTH = $0BA5 {deprecated};
  GL_MODELVIEW_MATRIX = $0BA6 {deprecated};
  GL_PROJECTION_MATRIX = $0BA7 {deprecated};
  GL_TEXTURE_MATRIX = $0BA8 {deprecated};
  GL_ATTRIB_STACK_DEPTH = $0BB0 {deprecated};
  GL_CLIENT_ATTRIB_STACK_DEPTH = $0BB1 {deprecated};

  // alpha testing
  GL_ALPHA_TEST = $0BC0 {deprecated};
  GL_ALPHA_TEST_FUNC = $0BC1 {deprecated};
  GL_ALPHA_TEST_REF = $0BC2 {deprecated};

  GL_INDEX_LOGIC_OP = $0BF1 {deprecated};
  GL_LOGIC_OP = $0BF1 {deprecated};

  GL_AUX_BUFFERS = $0C00 {deprecated};

  GL_INDEX_CLEAR_VALUE = $0C20 {deprecated};
  GL_INDEX_WRITEMASK = $0C21 {deprecated};

  GL_INDEX_MODE = $0C30 {deprecated};
  GL_RGBA_MODE = $0C31 {deprecated};

  GL_RENDER_MODE = $0C40 {deprecated};
  GL_PERSPECTIVE_CORRECTION_HINT = $0C50 {deprecated};
  GL_POINT_SMOOTH_HINT = $0C51 {deprecated};

  GL_FOG_HINT = $0C54 {deprecated};
  GL_TEXTURE_GEN_S = $0C60 {deprecated};
  GL_TEXTURE_GEN_T = $0C61 {deprecated};
  GL_TEXTURE_GEN_R = $0C62 {deprecated};
  GL_TEXTURE_GEN_Q = $0C63 {deprecated};

  // pixel mode, transfer
  GL_PIXEL_MAP_I_TO_I = $0C70 {deprecated};
  GL_PIXEL_MAP_S_TO_S = $0C71 {deprecated};
  GL_PIXEL_MAP_I_TO_R = $0C72 {deprecated};
  GL_PIXEL_MAP_I_TO_G = $0C73 {deprecated};
  GL_PIXEL_MAP_I_TO_B = $0C74 {deprecated};
  GL_PIXEL_MAP_I_TO_A = $0C75 {deprecated};
  GL_PIXEL_MAP_R_TO_R = $0C76 {deprecated};
  GL_PIXEL_MAP_G_TO_G = $0C77 {deprecated};
  GL_PIXEL_MAP_B_TO_B = $0C78 {deprecated};
  GL_PIXEL_MAP_A_TO_A = $0C79 {deprecated};
  GL_PIXEL_MAP_I_TO_I_SIZE = $0CB0 {deprecated};
  GL_PIXEL_MAP_S_TO_S_SIZE = $0CB1 {deprecated};
  GL_PIXEL_MAP_I_TO_R_SIZE = $0CB2 {deprecated};
  GL_PIXEL_MAP_I_TO_G_SIZE = $0CB3 {deprecated};
  GL_PIXEL_MAP_I_TO_B_SIZE = $0CB4 {deprecated};
  GL_PIXEL_MAP_I_TO_A_SIZE = $0CB5 {deprecated};
  GL_PIXEL_MAP_R_TO_R_SIZE = $0CB6 {deprecated};
  GL_PIXEL_MAP_G_TO_G_SIZE = $0CB7 {deprecated};
  GL_PIXEL_MAP_B_TO_B_SIZE = $0CB8 {deprecated};
  GL_PIXEL_MAP_A_TO_A_SIZE = $0CB9 {deprecated};

  GL_MAP_COLOR = $0D10 {deprecated};
  GL_MAP_STENCIL = $0D11 {deprecated};
  GL_INDEX_SHIFT = $0D12 {deprecated};
  GL_INDEX_OFFSET = $0D13 {deprecated};
  GL_RED_SCALE = $0D14 {deprecated};
  GL_RED_BIAS = $0D15 {deprecated};
  GL_ZOOM_X = $0D16 {deprecated};
  GL_ZOOM_Y = $0D17 {deprecated};
  GL_GREEN_SCALE = $0D18 {deprecated};
  GL_GREEN_BIAS = $0D19 {deprecated};
  GL_BLUE_SCALE = $0D1A {deprecated};
  GL_BLUE_BIAS = $0D1B {deprecated};
  GL_ALPHA_SCALE = $0D1C {deprecated};
  GL_ALPHA_BIAS = $0D1D {deprecated};
  GL_DEPTH_SCALE = $0D1E {deprecated};
  GL_DEPTH_BIAS = $0D1F {deprecated};
  GL_MAX_EVAL_ORDER = $0D30 {deprecated};
  GL_MAX_LIGHTS = $0D31 {deprecated};
  GL_MAX_CLIP_PLANES = $0D32 {deprecated};

  GL_MAX_PIXEL_MAP_TABLE = $0D34 {deprecated};
  GL_MAX_ATTRIB_STACK_DEPTH = $0D35 {deprecated};
  GL_MAX_MODELVIEW_STACK_DEPTH = $0D36 {deprecated};
  GL_MAX_NAME_STACK_DEPTH = $0D37 {deprecated};
  GL_MAX_PROJECTION_STACK_DEPTH = $0D38 {deprecated};
  GL_MAX_TEXTURE_STACK_DEPTH = $0D39 {deprecated};

  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH = $0D3B {deprecated};
  GL_INDEX_BITS = $0D51 {deprecated};
  GL_RED_BITS = $0D52 {deprecated};
  GL_GREEN_BITS = $0D53 {deprecated};
  GL_BLUE_BITS = $0D54 {deprecated};
  GL_ALPHA_BITS = $0D55 {deprecated};
  GL_DEPTH_BITS = $0D56 {deprecated};
  GL_STENCIL_BITS = $0D57 {deprecated};
  GL_ACCUM_RED_BITS = $0D58 {deprecated};
  GL_ACCUM_GREEN_BITS = $0D59 {deprecated};
  GL_ACCUM_BLUE_BITS = $0D5A {deprecated};
  GL_ACCUM_ALPHA_BITS = $0D5B {deprecated};
  GL_NAME_STACK_DEPTH = $0D70 {deprecated};
  GL_AUTO_NORMAL = $0D80 {deprecated};
  GL_MAP1_COLOR_4 = $0D90 {deprecated};
  GL_MAP1_INDEX = $0D91 {deprecated};
  GL_MAP1_NORMAL = $0D92 {deprecated};
  GL_MAP1_TEXTURE_COORD_1 = $0D93 {deprecated};
  GL_MAP1_TEXTURE_COORD_2 = $0D94 {deprecated};
  GL_MAP1_TEXTURE_COORD_3 = $0D95 {deprecated};
  GL_MAP1_TEXTURE_COORD_4 = $0D96 {deprecated};
  GL_MAP1_VERTEX_3 = $0D97 {deprecated};
  GL_MAP1_VERTEX_4 = $0D98 {deprecated};
  GL_MAP2_COLOR_4 = $0DB0 {deprecated};
  GL_MAP2_INDEX = $0DB1 {deprecated};
  GL_MAP2_NORMAL = $0DB2 {deprecated};
  GL_MAP2_TEXTURE_COORD_1 = $0DB3 {deprecated};
  GL_MAP2_TEXTURE_COORD_2 = $0DB4 {deprecated};
  GL_MAP2_TEXTURE_COORD_3 = $0DB5 {deprecated};
  GL_MAP2_TEXTURE_COORD_4 = $0DB6 {deprecated};
  GL_MAP2_VERTEX_3 = $0DB7 {deprecated};
  GL_MAP2_VERTEX_4 = $0DB8 {deprecated};
  GL_MAP1_GRID_DOMAIN = $0DD0 {deprecated};
  GL_MAP1_GRID_SEGMENTS = $0DD1 {deprecated};
  GL_MAP2_GRID_DOMAIN = $0DD2 {deprecated};
  GL_MAP2_GRID_SEGMENTS = $0DD3 {deprecated};

  // feedback buffer
  GL_FEEDBACK_BUFFER_POINTER = $0DF0 {deprecated};
  GL_FEEDBACK_BUFFER_SIZE = $0DF1 {deprecated};
  GL_FEEDBACK_BUFFER_TYPE = $0DF2 {deprecated};

  GL_SELECTION_BUFFER_POINTER = $0DF3 {deprecated};
  GL_SELECTION_BUFFER_SIZE = $0DF4 {deprecated};

  GL_TEXTURE_COMPONENTS = $1003 {deprecated};
  GL_TEXTURE_LUMINANCE_SIZE = $8060 {deprecated};
  GL_TEXTURE_INTENSITY_SIZE = $8061 {deprecated};
  GL_TEXTURE_PRIORITY = $8066 {deprecated};
  GL_TEXTURE_RESIDENT = $8067 {deprecated};

  // lighting
  GL_AMBIENT = $1200 {deprecated};
  GL_DIFFUSE = $1201 {deprecated};
  GL_SPECULAR = $1202 {deprecated};
  GL_POSITION = $1203 {deprecated};
  GL_SPOT_DIRECTION = $1204 {deprecated};
  GL_SPOT_EXPONENT = $1205 {deprecated};
  GL_SPOT_CUTOFF = $1206 {deprecated};
  GL_CONSTANT_ATTENUATION = $1207 {deprecated};
  GL_LINEAR_ATTENUATION = $1208 {deprecated};
  GL_QUADRATIC_ATTENUATION = $1209 {deprecated};

  // display lists
  GL_COMPILE = $1300 {deprecated};
  GL_COMPILE_AND_EXECUTE = $1301 {deprecated};

  // data types
  GL_2_BYTES = $1407 {deprecated};
  GL_3_BYTES = $1408 {deprecated};
  GL_4_BYTES = $1409 {deprecated};
  GL_DOUBLE_EXT = $140A {deprecated};

  GL_EMISSION = $1600 {deprecated};
  GL_SHININESS = $1601 {deprecated};
  GL_AMBIENT_AND_DIFFUSE = $1602 {deprecated};
  GL_COLOR_INDEXES = $1603 {deprecated};

  // matrix modes
  GL_MODELVIEW = $1700 {deprecated};
  GL_PROJECTION = $1701 {deprecated};

  // pixel formats
  GL_COLOR_INDEX = $1900 {deprecated};
  GL_LUMINANCE = $1909 {deprecated};
  GL_LUMINANCE_ALPHA = $190A {deprecated};

  // pixel type
  GL_BITMAP = $1A00 {deprecated};

  // rendering modes
  GL_RENDER = $1C00 {deprecated};
  GL_FEEDBACK = $1C01 {deprecated};
  GL_SELECT = $1C02 {deprecated};

  GL_FLAT = $1D00 {deprecated};
  GL_SMOOTH = $1D01 {deprecated};

  GL_S = $2000 {deprecated};
  GL_T = $2001 {deprecated};
  GL_R = $2002 {deprecated};
  GL_Q = $2003 {deprecated};
  GL_MODULATE = $2100 {deprecated};
  GL_DECAL = $2101 {deprecated};
  GL_TEXTURE_ENV_MODE = $2200 {deprecated};
  GL_TEXTURE_ENV_COLOR = $2201 {deprecated};
  GL_TEXTURE_ENV = $2300 {deprecated};
  GL_EYE_LINEAR = $2400 {deprecated};
  GL_OBJECT_LINEAR = $2401 {deprecated};
  GL_SPHERE_MAP = $2402 {deprecated};
  GL_TEXTURE_GEN_MODE = $2500 {deprecated};
  GL_OBJECT_PLANE = $2501 {deprecated};
  GL_EYE_PLANE = $2502 {deprecated};

  GL_CLAMP = $2900 {deprecated};

  // pixel formats
  GL_ALPHA4 = $803B {deprecated};
  GL_ALPHA8 = $803C {deprecated};
  GL_ALPHA12 = $803D {deprecated};
  GL_ALPHA16 = $803E {deprecated};
  GL_LUMINANCE4 = $803F {deprecated};
  GL_LUMINANCE8 = $8040 {deprecated};
  GL_LUMINANCE12 = $8041 {deprecated};
  GL_LUMINANCE16 = $8042 {deprecated};
  GL_LUMINANCE4_ALPHA4 = $8043 {deprecated};
  GL_LUMINANCE6_ALPHA2 = $8044 {deprecated};
  GL_LUMINANCE8_ALPHA8 = $8045 {deprecated};
  GL_LUMINANCE12_ALPHA4 = $8046 {deprecated};
  GL_LUMINANCE12_ALPHA12 = $8047 {deprecated};
  GL_LUMINANCE16_ALPHA16 = $8048 {deprecated};
  GL_INTENSITY = $8049 {deprecated};
  GL_INTENSITY4 = $804A {deprecated};
  GL_INTENSITY8 = $804B {deprecated};
  GL_INTENSITY12 = $804C {deprecated};
  GL_INTENSITY16 = $804D {deprecated};

  GL_VERTEX_ARRAY = $8074 {deprecated};
  GL_NORMAL_ARRAY = $8075 {deprecated};
  GL_COLOR_ARRAY = $8076 {deprecated};
  GL_INDEX_ARRAY = $8077 {deprecated};
  GL_TEXTURE_COORD_ARRAY = $8078 {deprecated};
  GL_EDGE_FLAG_ARRAY = $8079 {deprecated};
  GL_VERTEX_ARRAY_SIZE = $807A {deprecated};
  GL_VERTEX_ARRAY_TYPE = $807B {deprecated};
  GL_VERTEX_ARRAY_STRIDE = $807C {deprecated};
  GL_NORMAL_ARRAY_TYPE = $807E {deprecated};
  GL_NORMAL_ARRAY_STRIDE = $807F {deprecated};
  GL_COLOR_ARRAY_SIZE = $8081 {deprecated};
  GL_COLOR_ARRAY_TYPE = $8082 {deprecated};
  GL_COLOR_ARRAY_STRIDE = $8083 {deprecated};
  GL_INDEX_ARRAY_TYPE = $8085 {deprecated};
  GL_INDEX_ARRAY_STRIDE = $8086 {deprecated};
  GL_TEXTURE_COORD_ARRAY_SIZE = $8088 {deprecated};
  GL_TEXTURE_COORD_ARRAY_TYPE = $8089 {deprecated};
  GL_TEXTURE_COORD_ARRAY_STRIDE = $808A {deprecated};
  GL_EDGE_FLAG_ARRAY_STRIDE = $808C {deprecated};

  // vertex arrays
  GL_VERTEX_ARRAY_POINTER = $808E {deprecated};
  GL_NORMAL_ARRAY_POINTER = $808F {deprecated};
  GL_COLOR_ARRAY_POINTER = $8090 {deprecated};
  GL_INDEX_ARRAY_POINTER = $8091 {deprecated};
  GL_TEXTURE_COORD_ARRAY_POINTER = $8092 {deprecated};
  GL_EDGE_FLAG_ARRAY_POINTER = $8093 {deprecated};

  // interleaved arrays formats
  GL_V2F = $2A20 {deprecated};
  GL_V3F = $2A21 {deprecated};
  GL_C4UB_V2F = $2A22 {deprecated};
  GL_C4UB_V3F = $2A23 {deprecated};
  GL_C3F_V3F = $2A24 {deprecated};
  GL_N3F_V3F = $2A25 {deprecated};
  GL_C4F_N3F_V3F = $2A26 {deprecated};
  GL_T2F_V3F = $2A27 {deprecated};
  GL_T4F_V4F = $2A28 {deprecated};
  GL_T2F_C4UB_V3F = $2A29 {deprecated};
  GL_T2F_C3F_V3F = $2A2A {deprecated};
  GL_T2F_N3F_V3F = $2A2B {deprecated};
  GL_T2F_C4F_N3F_V3F = $2A2C {deprecated};
  GL_T4F_C4F_N3F_V4F = $2A2D {deprecated};

  // clip planes
  GL_CLIP_PLANE0 = $3000 {deprecated};
  GL_CLIP_PLANE1 = $3001 {deprecated};
  GL_CLIP_PLANE2 = $3002 {deprecated};
  GL_CLIP_PLANE3 = $3003 {deprecated};
  GL_CLIP_PLANE4 = $3004 {deprecated};
  GL_CLIP_PLANE5 = $3005 {deprecated};

  // lights
  GL_LIGHT0 = $4000 {deprecated};
  GL_LIGHT1 = $4001 {deprecated};
  GL_LIGHT2 = $4002 {deprecated};
  GL_LIGHT3 = $4003 {deprecated};
  GL_LIGHT4 = $4004 {deprecated};
  GL_LIGHT5 = $4005 {deprecated};
  GL_LIGHT6 = $4006 {deprecated};
  GL_LIGHT7 = $4007 {deprecated};

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'New core constants in OpenGL v1.2'}{$ENDIF}

  // promoted to core v1.2 from GL_EXT_packed_pixels (EXT #23)
  GL_UNSIGNED_BYTE_3_3_2 = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
  GL_UNSIGNED_INT_8_8_8_8 = $8035;
  GL_UNSIGNED_INT_10_10_10_2 = $8036;

  // promoted to core v1.2 from GL_EXT_texture3D (EXT #6)
  GL_PACK_SKIP_IMAGES = $806B;
  GL_PACK_IMAGE_HEIGHT = $806C;
  GL_UNPACK_SKIP_IMAGES = $806D;
  GL_UNPACK_IMAGE_HEIGHT = $806E;
  GL_TEXTURE_3D = $806F;
  GL_TEXTURE_BINDING_3D = $806A;
  GL_PROXY_TEXTURE_3D = $8070;
  GL_TEXTURE_DEPTH = $8071;
  GL_TEXTURE_WRAP_R = $8072;
  GL_MAX_3D_TEXTURE_SIZE = $8073;

  // new for OpenGL 1.2
  GL_UNSIGNED_BYTE_2_3_3_REV = $8362;
  GL_UNSIGNED_SHORT_5_6_5 = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4_REV = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV = $8366;
  GL_UNSIGNED_INT_8_8_8_8_REV = $8367;
  GL_UNSIGNED_INT_2_10_10_10_REV = $8368;

  // promoted to core v1.2 from GL_EXT_bgra (EXT #129)
  GL_BGR = $80E0;
  GL_BGRA = $80E1;

  // promoted to core v1.2 from GL_EXT_draw_range_elements (EXT #112)
  GL_MAX_ELEMENTS_VERTICES = $80E8;
  GL_MAX_ELEMENTS_INDICES = $80E9;

  // promoted to core v1.2 from GL_SGIS_texture_edge_clamp (EXT #35)
  GL_CLAMP_TO_EDGE = $812F;

  // promoted to core v1.2 from GL_SGIS_texture_lod (EXT #24)
  GL_TEXTURE_MIN_LOD = $813A;
  GL_TEXTURE_MAX_LOD = $813B;
  GL_TEXTURE_BASE_LEVEL = $813C;
  GL_TEXTURE_MAX_LEVEL = $813D;

  // new 1.2 naming scheme (POINT => SMOOTH_POINT)
  GL_SMOOTH_POINT_SIZE_RANGE = $0B12;
  GL_SMOOTH_POINT_SIZE_GRANULARITY = $0B13;
  GL_SMOOTH_LINE_WIDTH_RANGE = $0B22;
  GL_SMOOTH_LINE_WIDTH_GRANULARITY = $0B23;
  GL_ALIASED_LINE_WIDTH_RANGE = $846E;

  // Blending ( 1.2 ARB imaging)
  // promoted to core v1.2 from GL_EXT_blend_color (EXT #2)
  GL_CONSTANT_COLOR = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR = $8002;
  GL_CONSTANT_ALPHA = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA = $8004;
  GL_BLEND_COLOR = $8005;

  // promoted to core v1.2 from GL_EXT_blend_minmax (EXT #37)
  GL_FUNC_ADD = $8006;
  GL_MIN = $8007;
  GL_MAX = $8008;
  GL_BLEND_EQUATION = $8009;

  // promoted to core v1.2 from GL_EXT_blend_subtract (EXT #38)
  GL_FUNC_SUBTRACT = $800A;
  GL_FUNC_REVERSE_SUBTRACT = $800B;

{$IFDEF GLS_REGIONS}{$REGION 'OpenGL 1.2 deprecated'}{$ENDIF}
  // {deprecated}
  // promoted to core v1.2 from GL_EXT_rescale_normal (EXT #27)
  GL_RESCALE_NORMAL = $803A {deprecated};

  // promoted to core v1.2 from EXT_separate_specular_color (EXT #144)
  GL_LIGHT_MODEL_COLOR_CONTROL = $81F8 {deprecated};
  GL_SINGLE_COLOR = $81F9 {deprecated};
  GL_SEPARATE_SPECULAR_COLOR = $81FA {deprecated};

  // new 1.2 naming scheme (POINT => SMOOTH_POINT)
  GL_ALIASED_POINT_SIZE_RANGE = $846D {deprecated};

  // Convolutions (GL 1.2 ARB imaging)
  // promoted to core v1.2 from GL_EXT_convolution (EXT #12)
  GL_CONVOLUTION_1D = $8010 {deprecated};
  GL_CONVOLUTION_2D = $8011 {deprecated};
  GL_SEPARABLE_2D = $8012 {deprecated};
  GL_CONVOLUTION_BORDER_MODE = $8013 {deprecated};
  GL_CONVOLUTION_FILTER_SCALE = $8014 {deprecated};
  GL_CONVOLUTION_FILTER_BIAS = $8015 {deprecated};
  GL_REDUCE = $8016 {deprecated};
  GL_CONVOLUTION_FORMAT = $8017 {deprecated};
  GL_CONVOLUTION_WIDTH = $8018 {deprecated};
  GL_CONVOLUTION_HEIGHT = $8019 {deprecated};
  GL_MAX_CONVOLUTION_WIDTH = $801A {deprecated};
  GL_MAX_CONVOLUTION_HEIGHT = $801B {deprecated};
  GL_POST_CONVOLUTION_RED_SCALE = $801C {deprecated};
  GL_POST_CONVOLUTION_GREEN_SCALE = $801D {deprecated};
  GL_POST_CONVOLUTION_BLUE_SCALE = $801E {deprecated};
  GL_POST_CONVOLUTION_ALPHA_SCALE = $801F {deprecated};
  GL_POST_CONVOLUTION_RED_BIAS = $8020 {deprecated};
  GL_POST_CONVOLUTION_GREEN_BIAS = $8021 {deprecated};
  GL_POST_CONVOLUTION_BLUE_BIAS = $8022 {deprecated};
  GL_POST_CONVOLUTION_ALPHA_BIAS = $8023 {deprecated};

  // Histogram (GL 1.2 ARB imaging)
  // promoted to core v1.2 from GL_EXT_histogram (EXT #11)
  GL_HISTOGRAM = $8024 {deprecated};
  GL_PROXY_HISTOGRAM = $8025 {deprecated};
  GL_HISTOGRAM_WIDTH = $8026 {deprecated};
  GL_HISTOGRAM_FORMAT = $8027 {deprecated};
  GL_HISTOGRAM_RED_SIZE = $8028 {deprecated};
  GL_HISTOGRAM_GREEN_SIZE = $8029 {deprecated};
  GL_HISTOGRAM_BLUE_SIZE = $802A {deprecated};
  GL_HISTOGRAM_ALPHA_SIZE = $802B {deprecated};
  GL_HISTOGRAM_LUMINANCE_SIZE = $802C {deprecated};
  GL_HISTOGRAM_SINK = $802D {deprecated};
  GL_MINMAX = $802E {deprecated};
  GL_MINMAX_FORMAT = $802F {deprecated};
  GL_MINMAX_SINK = $8030 {deprecated};
  GL_TABLE_TOO_LARGE = $8031 {deprecated};

  // Color Matrix (GL 1.2 ARB imaging)
  // promoted to core v1.2 from SGI_color_matrix (EXT #13)
  GL_COLOR_MATRIX = $80B1 {deprecated};
  GL_COLOR_MATRIX_STACK_DEPTH = $80B2 {deprecated};
  GL_MAX_COLOR_MATRIX_STACK_DEPTH = $80B3 {deprecated};
  GL_POST_COLOR_MATRIX_RED_SCALE = $80B4 {deprecated};
  GL_POST_COLOR_MATRIX_GREEN_SCALE = $80B5 {deprecated};
  GL_POST_COLOR_MATRIX_BLUE_SCALE = $80B6 {deprecated};
  GL_POST_COLOR_MATRIX_ALPHA_SCALE = $80B7 {deprecated};
  GL_POST_COLOR_MATRIX_RED_BIAS = $80B8 {deprecated};
  GL_POST_COLOR_MATRIX_GREEN_BIAS = $80B9 {deprecated};
  GL_POST_COLOR_MATRIX_BLUE_BIAS = $80BA {deprecated};
  GL_POST_COLOR_MATRIX_ALPHA_BIAS = $80BB {deprecated};

  // Color Table (GL 1.2 ARB imaging)
  // promoted to core v1.2 from GL_SGI_color_table (EXT #14)
  GL_COLOR_TABLE = $80D0 {deprecated};
  GL_POST_CONVOLUTION_COLOR_TABLE = $80D1 {deprecated};
  GL_POST_COLOR_MATRIX_COLOR_TABLE = $80D2 {deprecated};
  GL_PROXY_COLOR_TABLE = $80D3 {deprecated};
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE = $80D4 {deprecated};
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE = $80D5 {deprecated};
  GL_COLOR_TABLE_SCALE = $80D6 {deprecated};
  GL_COLOR_TABLE_BIAS = $80D7 {deprecated};
  GL_COLOR_TABLE_FORMAT = $80D8 {deprecated};
  GL_COLOR_TABLE_WIDTH = $80D9 {deprecated};
  GL_COLOR_TABLE_RED_SIZE = $80DA {deprecated};
  GL_COLOR_TABLE_GREEN_SIZE = $80DB {deprecated};
  GL_COLOR_TABLE_BLUE_SIZE = $80DC {deprecated};
  GL_COLOR_TABLE_ALPHA_SIZE = $80DD {deprecated};
  GL_COLOR_TABLE_LUMINANCE_SIZE = $80DE {deprecated};
  GL_COLOR_TABLE_INTENSITY_SIZE = $80DF {deprecated};

  // Convolution Border Modes (GL 1.2 ARB imaging)
  // promoted to core v1.2 from GL_HP_convolution_border_modes (EXT #67)
  GL_CONSTANT_BORDER = $8151 {deprecated};
  GL_REPLICATE_BORDER = $8153 {deprecated};
  GL_CONVOLUTION_BORDER_COLOR = $8154 {deprecated};
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'New core constants in OpenGL v1.3'}{$ENDIF}
  // Multitexturing
  // promoted to core OpenGL v1.3 from GL_ARB_multitexture (ARB #1)
  GL_TEXTURE0 = $84C0;
  GL_TEXTURE1 = $84C1;
  GL_TEXTURE2 = $84C2;
  GL_TEXTURE3 = $84C3;
  GL_TEXTURE4 = $84C4;
  GL_TEXTURE5 = $84C5;
  GL_TEXTURE6 = $84C6;
  GL_TEXTURE7 = $84C7;
  GL_TEXTURE8 = $84C8;
  GL_TEXTURE9 = $84C9;
  GL_TEXTURE10 = $84CA;
  GL_TEXTURE11 = $84CB;
  GL_TEXTURE12 = $84CC;
  GL_TEXTURE13 = $84CD;
  GL_TEXTURE14 = $84CE;
  GL_TEXTURE15 = $84CF;
  GL_TEXTURE16 = $84D0;
  GL_TEXTURE17 = $84D1;
  GL_TEXTURE18 = $84D2;
  GL_TEXTURE19 = $84D3;
  GL_TEXTURE20 = $84D4;
  GL_TEXTURE21 = $84D5;
  GL_TEXTURE22 = $84D6;
  GL_TEXTURE23 = $84D7;
  GL_TEXTURE24 = $84D8;
  GL_TEXTURE25 = $84D9;
  GL_TEXTURE26 = $84DA;
  GL_TEXTURE27 = $84DB;
  GL_TEXTURE28 = $84DC;
  GL_TEXTURE29 = $84DD;
  GL_TEXTURE30 = $84DE;
  GL_TEXTURE31 = $84DF;
  GL_ACTIVE_TEXTURE = $84E0;

  // Multisampling
  // promoted to core OpenGL v1.3 from GL_ARB_multisample (ARB #5)
  GL_MULTISAMPLE = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
  GL_SAMPLE_ALPHA_TO_ONE = $809F;
  GL_SAMPLE_COVERAGE = $80A0;
  GL_SAMPLE_BUFFERS = $80A8;
  GL_SAMPLES = $80A9;
  GL_SAMPLE_COVERAGE_VALUE = $80AA;
  GL_SAMPLE_COVERAGE_INVERT = $80AB;

  // Cube Mapping
  // promoted to core OpenGL v1.3 from GL_ARB_texture_cube_map (ARB #7)
  GL_TEXTURE_CUBE_MAP = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE = $851C;

  // Texture Compression
  // promoted to core OpenGL v1.3 from GL_ARB_texture_compression (ARB #12)
  GL_COMPRESSED_RGB = $84ED;
  GL_COMPRESSED_RGBA = $84EE;
  GL_TEXTURE_COMPRESSION_HINT = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE = $86A0;
  GL_TEXTURE_COMPRESSED = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS = $86A3;

  // Texture Border Clamping
  // promoted to core OpenGL v1.3 from GL_ARB_texture_border_clamp (ARB #13)
  GL_CLAMP_TO_BORDER = $812D;

{$IFDEF GLS_REGIONS}{$REGION 'OpenGL 1.3 deprecated'}{$ENDIF}
  // promoted to core OpenGL v1.3 from GL_ARB_multitexture (ARB #1)
  GL_CLIENT_ACTIVE_TEXTURE = $84E1 {deprecated};
  GL_MAX_TEXTURE_UNITS = $84E2 {deprecated};

  // Transpose Matrices
  // promoted to core OpenGL v1.3 from GL_ARB_transpose_matrix (ARB #3)
  GL_TRANSPOSE_MODELVIEW_MATRIX = $84E3 {deprecated};
  GL_TRANSPOSE_PROJECTION_MATRIX = $84E4 {deprecated};
  GL_TRANSPOSE_TEXTURE_MATRIX = $84E5 {deprecated};
  GL_TRANSPOSE_COLOR_MATRIX = $84E6 {deprecated};

  // promoted to core OpenGL v1.3 from GL_ARB_multisample (ARB #5)
  GL_MULTISAMPLE_BIT = $20000000 {deprecated};

  // promoted to core OpenGL v1.3 from GL_ARB_texture_cube_map (ARB #7)
  GL_NORMAL_MAP = $8511 {deprecated};
  GL_REFLECTION_MAP = $8512 {deprecated};

  // promoted to core OpenGL v1.3 from GL_ARB_texture_compression (ARB #12)
  GL_COMPRESSED_ALPHA = $84E9 {deprecated};
  GL_COMPRESSED_LUMINANCE = $84EA {deprecated};
  GL_COMPRESSED_LUMINANCE_ALPHA = $84EB {deprecated};
  GL_COMPRESSED_INTENSITY = $84EC {deprecated};

  // Texture Combine Environment Mode
  // promoted to core OpenGL v1.3 from GL_ARB_texture_env_combine (ARB #17)
  GL_COMBINE = $8570 {deprecated};
  GL_COMBINE_RGB = $8571 {deprecated};
  GL_COMBINE_ALPHA = $8572 {deprecated};
  GL_SOURCE0_RGB = $8580 {deprecated};
  GL_SOURCE1_RGB = $8581 {deprecated};
  GL_SOURCE2_RGB = $8582 {deprecated};
  GL_SOURCE0_ALPHA = $8588 {deprecated};
  GL_SOURCE1_ALPHA = $8589 {deprecated};
  GL_SOURCE2_ALPHA = $858A {deprecated};
  GL_OPERAND0_RGB = $8590 {deprecated};
  GL_OPERAND1_RGB = $8591 {deprecated};
  GL_OPERAND2_RGB = $8592 {deprecated};
  GL_OPERAND0_ALPHA = $8598 {deprecated};
  GL_OPERAND1_ALPHA = $8599 {deprecated};
  GL_OPERAND2_ALPHA = $859A {deprecated};
  GL_RGB_SCALE = $8573 {deprecated};
  GL_ADD_SIGNED = $8574 {deprecated};
  GL_INTERPOLATE = $8575 {deprecated};
  GL_SUBTRACT = $84E7 {deprecated};
  GL_CONSTANT = $8576 {deprecated};
  GL_PRIMARY_COLOR = $8577 {deprecated};
  GL_PREVIOUS = $8578 {deprecated};

  // Texture Dot3 Environment Mode
  // promoted to OpenGL v1.3 from GL_ARB_texture_env_dot3 (ARB #19)
  GL_DOT3_RGB = $86AE {deprecated};
  GL_DOT3_RGBA = $86AF {deprecated};
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'New core constants in OpenGL v1.4'}{$ENDIF}

  // Separate Blend Functions
  // promoted to core OpenGL v1.4 from GL_EXT_blend_func_separate (EXT #173)
  GL_BLEND_DST_RGB = $80C8;
  GL_BLEND_SRC_RGB = $80C9;
  GL_BLEND_DST_ALPHA = $80CA;
  GL_BLEND_SRC_ALPHA = $80CB;

  // Point Parameters
  // promoted to core OpenGL v1.4 from GL_ARB_point_parameters (ARB #14)
  GL_POINT_FADE_THRESHOLD_SIZE = $8128;

  // Depth Texture
  // promoted to core OpenGL v1.4 from GL_ARB_depth_texture (ARB #22)
  GL_DEPTH_COMPONENT16 = $81A5;
  GL_DEPTH_COMPONENT24 = $81A6;
  GL_DEPTH_COMPONENT32 = $81A7;

  // Texture Mirrored Repeat
  // promoted to Core OpenGL v1.4 from GL_ARB_texture_mirrored_repeat (ARB #21)
  GL_MIRRORED_REPEAT = $8370;

  // Texture LOD Bias
  // (promoted to core OpenGL v1.4 from GL_EXT_texture_lod_bias (EXT #186)
  GL_MAX_TEXTURE_LOD_BIAS = $84FD;
  GL_TEXTURE_LOD_BIAS = $8501;

  // Stencil Wrap
  // promoted to core OpenGL v1.4 from GL_EXT_stencil_wrap (EXT #176)
  GL_INCR_WRAP = $8507;
  GL_DECR_WRAP = $8508;

  // Depth Textures
  // promoted to core OpenGL v1.4 from GL_ARB_depth_texture (ARB #22)
  GL_TEXTURE_DEPTH_SIZE = $884A;

  // Shadows
  // promoted to core OpenGL v1.4 from GL_ARB_shadow (ARB #23)
  GL_TEXTURE_COMPARE_MODE = $884C;
  GL_TEXTURE_COMPARE_FUNC = $884D;

{$IFDEF GLS_REGIONS}{$REGION 'OpenGL 1.4 deprecated'}{$ENDIF}
  // from GL_ARB_point_parameters (ARB #14)
  GL_POINT_SIZE_MIN = $8126 {deprecated};
  GL_POINT_SIZE_MAX = $8127 {deprecated};
  GL_POINT_DISTANCE_ATTENUATION = $8129 {deprecated};

  // Automatic Mipmap Generation
  // promoted to core OpenGL v1.4 from GL_SGIS_generate_mipmap (EXT #32)
  GL_GENERATE_MIPMAP = $8191 {deprecated};
  GL_GENERATE_MIPMAP_HINT = $8192 {deprecated};

  // Fog Coordinate
  // promoted to core OpenGL v1.4 from GL_EXT_fog_coord (EXT #149)
  GL_FOG_COORDINATE_SOURCE = $8450 {deprecated};
  GL_FOG_COORDINATE = $8451 {deprecated};
  GL_FRAGMENT_DEPTH = $8452 {deprecated};
  GL_CURRENT_FOG_COORDINATE = $8453 {deprecated};
  GL_FOG_COORDINATE_ARRAY_TYPE = $8454 {deprecated};
  GL_FOG_COORDINATE_ARRAY_STRIDE = $8455 {deprecated};
  GL_FOG_COORDINATE_ARRAY_POINTER = $8456 {deprecated};
  GL_FOG_COORDINATE_ARRAY = $8457 {deprecated};

  // Secondary Color
  // promoted to core OpenGL v1.4 from GL_EXT_secondary_color (EXT #145)
  GL_COLOR_SUM = $8458 {deprecated};
  GL_CURRENT_SECONDARY_COLOR = $8459 {deprecated};
  GL_SECONDARY_COLOR_ARRAY_SIZE = $845A {deprecated};
  GL_SECONDARY_COLOR_ARRAY_TYPE = $845B {deprecated};
  GL_SECONDARY_COLOR_ARRAY_STRIDE = $845C {deprecated};
  GL_SECONDARY_COLOR_ARRAY_POINTER = $845D {deprecated};
  GL_SECONDARY_COLOR_ARRAY = $845E {deprecated};

  // (promoted to core OpenGL v1.4 from GL_EXT_texture_lod_bias (EXT #186)
  GL_TEXTURE_FILTER_CONTROL = $8500 {deprecated};

  // promoted to core OpenGL v1.4 from GL_ARB_depth_texture (ARB #22)
  GL_DEPTH_TEXTURE_MODE = $884B {deprecated};

  // promoted to core OpenGL v1.4 from GL_ARB_shadow (ARB #23)
  GL_COMPARE_R_TO_TEXTURE = $884E {deprecated};
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'New core constants in OpenGL v1.5'}{$ENDIF}
  // Buffer Objects
  // promoted to core OpenGL v1.5 from GL_ARB_vertex_buffer_object (ARB #28)
  GL_BUFFER_SIZE = $8764;
  GL_BUFFER_USAGE = $8765;

  // Occlusion Queries
  // promoted to core OpenGL v1.5 from GL_ARB_occulsion_query (ARB #29)
  GL_QUERY_COUNTER_BITS = $8864;
  GL_CURRENT_QUERY = $8865;
  GL_QUERY_RESULT = $8866;
  GL_QUERY_RESULT_AVAILABLE = $8867;

  // Buffer Objects
  // promoted to core OpenGL v1.5 from GL_ARB_vertex_buffer_object (ARB #28)
  GL_ARRAY_BUFFER = $8892;
  GL_ELEMENT_ARRAY_BUFFER = $8893;
  GL_ARRAY_BUFFER_BINDING = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING = $8895;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
  GL_READ_ONLY = $88B8;
  GL_WRITE_ONLY = $88B9;
  GL_READ_WRITE = $88BA;
  GL_BUFFER_ACCESS = $88BB;
  GL_BUFFER_MAPPED = $88BC;
  GL_BUFFER_MAP_POINTER = $88BD;
  GL_STREAM_DRAW = $88E0;
  GL_STREAM_READ = $88E1;
  GL_STREAM_COPY = $88E2;
  GL_STATIC_DRAW = $88E4;
  GL_STATIC_READ = $88E5;
  GL_STATIC_COPY = $88E6;
  GL_DYNAMIC_DRAW = $88E8;
  GL_DYNAMIC_READ = $88E9;
  GL_DYNAMIC_COPY = $88EA;

  // Occlusion Queries
  // promoted to core OpenGL v1.5 from GL_ARB_occulsion_query (ARB #29)
  GL_SAMPLES_PASSED = $8914;

  // Changed Tokens
  GL_SRC1_ALPHA = GL_SOURCE1_ALPHA; // required for 3.3+

{$IFDEF GLS_REGIONS}{$REGION 'OpenGL 1.5 deprecated'}{$ENDIF}
  // from GL_ARB_vertex_buffer_object (ARB #28)
  GL_VERTEX_ARRAY_BUFFER_BINDING = $8896 {deprecated};
  GL_NORMAL_ARRAY_BUFFER_BINDING = $8897 {deprecated};
  GL_COLOR_ARRAY_BUFFER_BINDING = $8898 {deprecated};
  GL_INDEX_ARRAY_BUFFER_BINDING = $8899 {deprecated};
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING = $889A {deprecated};
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING = $889B {deprecated};
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING = $889C {deprecated};
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING = $889D {deprecated};
  GL_WEIGHT_ARRAY_BUFFER_BINDING = $889E {deprecated};

  GL_FOG_COORD_SRC = GL_FOG_COORDINATE_SOURCE {deprecated};
  GL_FOG_COORD = GL_FOG_COORDINATE {deprecated};
  GL_CURRENT_FOG_COORD = GL_CURRENT_FOG_COORDINATE {deprecated};
  GL_FOG_COORD_ARRAY_TYPE = GL_FOG_COORDINATE_ARRAY_TYPE {deprecated};
  GL_FOG_COORD_ARRAY_STRIDE = GL_FOG_COORDINATE_ARRAY_STRIDE {deprecated};
  GL_FOG_COORD_ARRAY_POINTER = GL_FOG_COORDINATE_ARRAY_POINTER {deprecated};
  GL_FOG_COORD_ARRAY = GL_FOG_COORDINATE_ARRAY {deprecated};
  GL_FOG_COORD_ARRAY_BUFFER_BINDING = GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING {deprecated};

  // Changed Tokens
  // new naming scheme in OpenGL v1.5, old tokens kept for backwards compatibility
  GL_SRC0_RGB = GL_SOURCE0_RGB {deprecated};
  GL_SRC1_RGB = GL_SOURCE1_RGB {deprecated};
  GL_SRC2_RGB = GL_SOURCE2_RGB {deprecated};
  GL_SRC0_ALPHA = GL_SOURCE0_ALPHA {deprecated};
  GL_SRC2_ALPHA = GL_SOURCE2_ALPHA {deprecated};
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'New core constants in OpenGL v2.0'}{$ENDIF}
  // OpenGL 2.0

  // Changed Tokens
  // new name in OpenGL v2.0
  GL_BLEND_EQUATION_RGB = GL_BLEND_EQUATION;

  // promoted to core OpenGL v2.0 from GL_ARB_vertex_shader (ARB #31)
  GL_VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE = $8625;
  GL_CURRENT_VERTEX_ATTRIB = $8626;
  GL_VERTEX_PROGRAM_POINT_SIZE = $8642;
  GL_VERTEX_ATTRIB_ARRAY_POINTER = $8645;

  // Separate Stencil
  // promoted to core OpenGL v2.0 from GL_ARB_stencil_two_side (ARB #unknown)
  GL_STENCIL_BACK_FUNC = $8800;
  GL_STENCIL_BACK_FAIL = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS = $8803;

  // promoted to core OpenGL v2.0 from GL_ARB_draw_buffers (ARB #37) / GL_ATI_draw_buffers (EXT #277)
  GL_MAX_DRAW_BUFFERS = $8824;
  GL_DRAW_BUFFER0 = $8825;
  GL_DRAW_BUFFER1 = $8826;
  GL_DRAW_BUFFER2 = $8827;
  GL_DRAW_BUFFER3 = $8828;
  GL_DRAW_BUFFER4 = $8829;
  GL_DRAW_BUFFER5 = $882A;
  GL_DRAW_BUFFER6 = $882B;
  GL_DRAW_BUFFER7 = $882C;
  GL_DRAW_BUFFER8 = $882D;
  GL_DRAW_BUFFER9 = $882E;
  GL_DRAW_BUFFER10 = $882F;
  GL_DRAW_BUFFER11 = $8830;
  GL_DRAW_BUFFER12 = $8831;
  GL_DRAW_BUFFER13 = $8832;
  GL_DRAW_BUFFER14 = $8833;
  GL_DRAW_BUFFER15 = $8834;

  // Separate Blend Equation
  // promoted to core OpenGL v2.0 from GL_EXT_blend_equation_separate (EXT #299)
  GL_BLEND_EQUATION_ALPHA = $883D;

  // Shader Programs
  // promoted to core OpenGL v2.0 from GL_ARB_vertex_shader (ARB #31)
  GL_MAX_VERTEX_ATTRIBS = $8869;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;

  // promoted to core OpenGL v2.0 from GL_ARB_vertex_shader (ARB #31) /GL_ARB_fragment_shader (ARB #32)
  GL_MAX_TEXTURE_IMAGE_UNITS = $8872;

  // promoted to core OpenGL v2.0 from GL_ARB_fragment_shader (ARB #32)
  GL_FRAGMENT_SHADER = $8B30;

  // promoted to core OpenGL v2.0 from GL_ARB_vertex_shader (ARB #31)
  GL_VERTEX_SHADER = $8B31;

  // promoted to core OpenGL v2.0 from GL_ARB_fragment_shader (ARB #32)
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS = $8B49;

  // promoted to core OpenGL v2.0 from GL_ARB_vertex_shader (ARB #31)
  GL_MAX_VERTEX_UNIFORM_COMPONENTS = $8B4A;
  GL_MAX_VARYING_FLOATS = $8B4B {deprecated}; // not yet removed
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;

  // Shader Objects
  // promoted to core OpenGL v2.0 from GL_ARB_shader_objects (ARB #30)
  GL_SHADER_TYPE = $8B4F;
  GL_FLOAT_VEC2 = $8B50;
  GL_FLOAT_VEC3 = $8B51;
  GL_FLOAT_VEC4 = $8B52;
  GL_INT_VEC2 = $8B53;
  GL_INT_VEC3 = $8B54;
  GL_INT_VEC4 = $8B55;
  GL_BOOL = $8B56;
  GL_BOOL_VEC2 = $8B57;
  GL_BOOL_VEC3 = $8B58;
  GL_BOOL_VEC4 = $8B59;
  GL_FLOAT_MAT2 = $8B5A;
  GL_FLOAT_MAT3 = $8B5B;
  GL_FLOAT_MAT4 = $8B5C;
  GL_SAMPLER_1D = $8B5D;
  GL_SAMPLER_2D = $8B5E;
  GL_SAMPLER_3D = $8B5F;
  GL_SAMPLER_CUBE = $8B60;
  GL_SAMPLER_1D_SHADOW = $8B61;
  GL_SAMPLER_2D_SHADOW = $8B62;
  GL_DELETE_STATUS = $8B80;
  GL_COMPILE_STATUS = $8B81;
  GL_LINK_STATUS = $8B82;
  GL_VALIDATE_STATUS = $8B83;
  GL_INFO_LOG_LENGTH = $8B84;
  GL_ATTACHED_SHADERS = $8B85;
  GL_ACTIVE_UNIFORMS = $8B86;
  GL_ACTIVE_UNIFORM_MAX_LENGTH = $8B87;
  GL_SHADER_SOURCE_LENGTH = $8B88;

  // Shader Programs
  // promoted to core OpenGL v2.0 from GL_ARB_vertex_shader (ARB #31)
  GL_ACTIVE_ATTRIBUTES = $8B89;
  GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = $8B8A;

  // promoted to core OpenGL v2.0 from GL_ARB_fragment_shader (ARB #32)
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT = $8B8B;

  // OpenGL Shading Language
  // promoted to core OpenGL v2.0 from GL_ARB_shading_language_100 (ARB #33)
  GL_SHADING_LANGUAGE_VERSION = $8B8C;

  // Shader Objects
  // promoted to core OpenGL v2.0 from GL_ARB_shader_objects (ARB #30) (added for 2.0)
  GL_CURRENT_PROGRAM = $8B8D;

  // Point Sprites
  // promoted to core OpenGL v2.0 from GL_ARB_point_sprite (ARB #35) (added for 2.0)
  GL_POINT_SPRITE_COORD_ORIGIN = $8CA0;
  GL_LOWER_LEFT = $8CA1;
  GL_UPPER_LEFT = $8CA2;

  // Separate Stencil
  // promoted to core OpenGL v2.0 from GL_ARB_stencil_two_side (ARB #unknown)
  GL_STENCIL_BACK_REF = $8CA3;
  GL_STENCIL_BACK_VALUE_MASK = $8CA4;
  GL_STENCIL_BACK_WRITEMASK = $8CA5;

{$IFDEF GLS_REGIONS}{$REGION 'OpenGL 2.0 deprecated'}{$ENDIF}
  // from GL_ARB_vertex_shader (ARB #31)
  GL_VERTEX_PROGRAM_TWO_SIDE = $8643 {deprecated};

  // from GL_ARB_point_sprite (ARB #35)
  GL_POINT_SPRITE = $8861 {deprecated};
  GL_COORD_REPLACE = $8862 {deprecated};

  // from GL_ARB_vertex_shader (ARB #31) /GL_ARB_fragment_shader (ARB #32)
  GL_MAX_TEXTURE_COORDS = $8871 {deprecated};
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'New core constants in OpenGL v2.1'}{$ENDIF}

  // OpenGL 2.1

  // Pixel Buffer Objects
  // from GL_ARB_pixel_buffer_object (ARB #42)
  GL_PIXEL_PACK_BUFFER = $88EB;
  GL_PIXEL_UNPACK_BUFFER = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING = $88EF;

  // Non-Square Matrices
  // new for OpenGL 2.1
  GL_FLOAT_MAT2x3 = $8B65;
  GL_FLOAT_MAT2x4 = $8B66;
  GL_FLOAT_MAT3x2 = $8B67;
  GL_FLOAT_MAT3x4 = $8B68;
  GL_FLOAT_MAT4x2 = $8B69;
  GL_FLOAT_MAT4x3 = $8B6A;

  // sRGB Textures
  // from GL_EXT_texture_sRGB (EXT #315)
  GL_SRGB = $8C40;
  GL_SRGB8 = $8C41;
  GL_SRGB_ALPHA = $8C42;
  GL_SRGB8_ALPHA8 = $8C43;
  GL_COMPRESSED_SRGB = $8C48;
  GL_COMPRESSED_SRGB_ALPHA = $8C49;

{$IFDEF GLS_REGIONS}{$REGION 'OpenGL 2.1 deprecated'}{$ENDIF}
  // new
  GL_CURRENT_RASTER_SECONDARY_COLOR = $845F {deprecated};
  // from GL_EXT_texture_sRGB (EXT #315)
  GL_SLUMINANCE_ALPHA = $8C44 {deprecated};
  GL_SLUMINANCE8_ALPHA8 = $8C45 {deprecated};
  GL_SLUMINANCE = $8C46 {deprecated};
  GL_SLUMINANCE8 = $8C47 {deprecated};
  GL_COMPRESSED_SLUMINANCE = $8C4A {deprecated};
  GL_COMPRESSED_SLUMINANCE_ALPHA = $8C4B {deprecated};
{$IFDEF GLS_REGIONS}{$ENDREGION'}{$ENDIF}

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'New core constants in OpenGL v3.0'}{$ENDIF}
  // TODO: arrange these better, find where they came from
  GL_COMPARE_REF_TO_TEXTURE = $884E; //GL_COMPARE_R_TO_TEXTURE;
  GL_CLIP_DISTANCE0 = $3000; //GL_CLIP_PLANE0;
  GL_CLIP_DISTANCE1 = $3001; //GL_CLIP_PLANE1;
  GL_CLIP_DISTANCE2 = $3002; //GL_CLIP_PLANE2;
  GL_CLIP_DISTANCE3 = $3003; //GL_CLIP_PLANE3;
  GL_CLIP_DISTANCE4 = $3004; //GL_CLIP_PLANE4;
  GL_CLIP_DISTANCE5 = $3005; //GL_CLIP_PLANE5;
  GL_CLIP_DISTANCE6 = $3006;
  GL_CLIP_DISTANCE7 = $3007;
  GL_MAX_CLIP_DISTANCES = $0D32; //GL_MAX_CLIP_PLANES;
  GL_MAJOR_VERSION = $821B;
  GL_MINOR_VERSION = $821C;
  GL_NUM_EXTENSIONS = $821D;
  GL_CONTEXT_FLAGS = $821E;
  //# Removed - replaced by per-attachment framebuffer queries
  //##	  COLOR_COMPONENT_TYPE				  = 0x821F
  //##	  COLOR_ENCODING_TYPE				  = 0x8220
  //##	  DEPTH_COMPONENT_TYPE				  = 0x8221
  //##	  TEXTURE_SHARED_TYPE				  = 0x8222
  GL_DEPTH_BUFFER = $8223;
  GL_STENCIL_BUFFER = $8224;
  GL_COMPRESSED_RED = $8225;
  GL_COMPRESSED_RG = $8226;
  GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT = $0001;
  GL_RGBA32F = $8814;
  GL_RGB32F = $8815;
  GL_RGBA16F = $881A;
  GL_RGB16F = $881B;
  GL_VERTEX_ATTRIB_ARRAY_INTEGER = $88FD;
  GL_MAX_ARRAY_TEXTURE_LAYERS = $88FF;
  GL_MIN_PROGRAM_TEXEL_OFFSET = $8904;
  GL_MAX_PROGRAM_TEXEL_OFFSET = $8905;
  GL_CLAMP_VERTEX_COLOR = $891A;
  GL_CLAMP_FRAGMENT_COLOR = $891B;
  GL_CLAMP_READ_COLOR = $891C;
  GL_FIXED_ONLY = $891D;
  GL_MAX_VARYING_COMPONENTS = GL_MAX_VARYING_FLOATS {deprecated}; // not yet removed
  //	 GL_TEXTURE_RED_TYPE				=$8C10;
  //	 GL_TEXTURE_GREEN_TYPE				=$8C11;
  //	 GL_TEXTURE_BLUE_TYPE				=$8C12;
  //	 GL_TEXTURE_ALPHA_TYPE				=$8C13;
  //	 GL_TEXTURE_LUMINANCE_TYPE				=$8C14;
  //	 GL_TEXTURE_INTENSITY_TYPE				=$8C15;
  //	 GL_TEXTURE_DEPTH_TYPE				= $8C16;
  //	 GL_UNSIGNED_NORMALIZED				= $8C17;
  GL_TEXTURE_1D_ARRAY = $8C18;
  GL_PROXY_TEXTURE_1D_ARRAY = $8C19;
  GL_TEXTURE_2D_ARRAY = $8C1A;
  GL_PROXY_TEXTURE_2D_ARRAY = $8C1B;
  GL_TEXTURE_BINDING_1D_ARRAY = $8C1C;
  GL_TEXTURE_BINDING_2D_ARRAY = $8C1D;
  GL_R11F_G11F_B10F = $8C3A;
  GL_UNSIGNED_INT_10F_11F_11F_REV = $8C3B;
  GL_RGB9_E5 = $8C3D;
  GL_UNSIGNED_INT_5_9_9_9_REV = $8C3E;
  GL_TEXTURE_SHARED_SIZE = $8C3F;
  GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH = $8C76;
  GL_TRANSFORM_FEEDBACK_BUFFER_MODE = $8C7F;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS = $8C80;
  GL_TRANSFORM_FEEDBACK_VARYINGS = $8C83;
  GL_TRANSFORM_FEEDBACK_BUFFER_START = $8C84;
  GL_TRANSFORM_FEEDBACK_BUFFER_SIZE = $8C85;
  GL_PRIMITIVES_GENERATED = $8C87;
  GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN = $8C88;
  GL_RASTERIZER_DISCARD = $8C89;
  GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS = $8C8A;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS = $8C8B;
  GL_INTERLEAVED_ATTRIBS = $8C8C;
  GL_SEPARATE_ATTRIBS = $8C8D;
  GL_TRANSFORM_FEEDBACK_BUFFER = $8C8E;
  GL_TRANSFORM_FEEDBACK_BUFFER_BINDING = $8C8F;
  GL_RGBA32UI = $8D70;
  GL_RGB32UI = $8D71;
  GL_RGBA16UI = $8D76;
  GL_RGB16UI = $8D77;
  GL_RGBA8UI = $8D7C;
  GL_RGB8UI = $8D7D;
  GL_RGBA32I = $8D82;
  GL_RGB32I = $8D83;
  GL_RGBA16I = $8D88;
  GL_RGB16I = $8D89;
  GL_RGBA8I = $8D8E;
  GL_RGB8I = $8D8F;
  GL_RED_INTEGER = $8D94;
  GL_GREEN_INTEGER = $8D95;
  GL_BLUE_INTEGER = $8D96;
  GL_ALPHA_INTEGER = $8D97;
  GL_RGB_INTEGER = $8D98;
  GL_RGBA_INTEGER = $8D99;
  GL_BGR_INTEGER = $8D9A;
  GL_BGRA_INTEGER = $8D9B;
  // these 2 never made it to core, only _EXT?
//	 GL_LUMINANCE_INTEGER       = $8D9C;
//	 GL_LUMINANCE_ALPHA_INTEGER = $8D9D;
  GL_SAMPLER_1D_ARRAY = $8DC0;
  GL_SAMPLER_2D_ARRAY = $8DC1;
  GL_SAMPLER_1D_ARRAY_SHADOW = $8DC3;
  GL_SAMPLER_2D_ARRAY_SHADOW = $8DC4;
  GL_SAMPLER_CUBE_SHADOW = $8DC5;
  GL_UNSIGNED_INT_VEC2 = $8DC6;
  GL_UNSIGNED_INT_VEC3 = $8DC7;
  GL_UNSIGNED_INT_VEC4 = $8DC8;
  GL_INT_SAMPLER_1D = $8DC9;
  GL_INT_SAMPLER_2D = $8DCA;
  GL_INT_SAMPLER_3D = $8DCB;
  GL_INT_SAMPLER_CUBE = $8DCC;
  GL_INT_SAMPLER_1D_ARRAY = $8DCE;
  GL_INT_SAMPLER_2D_ARRAY = $8DCF;
  GL_UNSIGNED_INT_SAMPLER_1D = $8DD1;
  GL_UNSIGNED_INT_SAMPLER_2D = $8DD2;
  GL_UNSIGNED_INT_SAMPLER_3D = $8DD3;
  GL_UNSIGNED_INT_SAMPLER_CUBE = $8DD4;
  GL_UNSIGNED_INT_SAMPLER_1D_ARRAY = $8DD6;
  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY = $8DD7;
  GL_QUERY_WAIT = $8E13;
  GL_QUERY_NO_WAIT = $8E14;
  GL_QUERY_BY_REGION_WAIT = $8E15;
  GL_QUERY_BY_REGION_NO_WAIT = $8E16;
  GL_BUFFER_ACCESS_FLAGS = $911F;
  GL_BUFFER_MAP_LENGTH = $9120;
  GL_BUFFER_MAP_OFFSET = $9121;

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'New core constants in OpenGL v3.1'}{$ENDIF}
  GL_SAMPLER_2D_RECT = $8B63;
  GL_SAMPLER_2D_RECT_SHADOW = $8B64;
  GL_SAMPLER_BUFFER = $8DC2;
  GL_INT_SAMPLER_2D_RECT = $8DCD;
  GL_INT_SAMPLER_BUFFER = $8DD0;
  GL_UNSIGNED_INT_SAMPLER_2D_RECT = $8DD5;
  GL_UNSIGNED_INT_SAMPLER_BUFFER = $8DD8;
  GL_TEXTURE_BUFFER = $8C2A;
  GL_MAX_TEXTURE_BUFFER_SIZE = $8C2B;
  GL_TEXTURE_BINDING_BUFFER = $8C2C;
  GL_TEXTURE_BUFFER_DATA_STORE_BINDING = $8C2D;
  GL_TEXTURE_BUFFER_FORMAT = $8C2E;
  GL_TEXTURE_RECTANGLE = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE = $84F8;
  GL_RED_SNORM = $8F90;
  GL_RG_SNORM = $8F91;
  GL_RGB_SNORM = $8F92;
  GL_RGBA_SNORM = $8F93;
  GL_R8_SNORM = $8F94;
  GL_RG8_SNORM = $8F95;
  GL_RGB8_SNORM = $8F96;
  GL_RGBA8_SNORM = $8F97;
  GL_R16_SNORM = $8F98;
  GL_RG16_SNORM = $8F99;
  GL_RGB16_SNORM = $8F9A;
  GL_RGBA16_SNORM = $8F9B;
  GL_SIGNED_NORMALIZED = $8F9C;
  GL_PRIMITIVE_RESTART = $8F9D;
  GL_PRIMITIVE_RESTART_INDEX = $8F9E;
  // GL_ARB_texture_compression_bptc (ARB #77)
  GL_COMPRESSED_RGBA_BPTC_UNORM_ARB = $8E8C;
  GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM_ARB = $8E8D;
  GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT_ARB = $8E8E;
  GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_ARB = $8E8F;
  // re-use tokens from:
  // ARB_copy_buffer (ARB #59)
  // ARB_draw_instanced (ARB #44)
  // ARB_uniform_buffer_object (ARB #57)

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'New core constants in OpenGL v3.2'}{$ENDIF}
  GL_CONTEXT_CORE_PROFILE_BIT = $00000001;
  GL_CONTEXT_COMPATIBILITY_PROFILE_BIT = $00000002;
  GL_LINES_ADJACENCY = $000A;
  GL_LINE_STRIP_ADJACENCY = $000B;
  GL_TRIANGLES_ADJACENCY = $000C;
  GL_TRIANGLE_STRIP_ADJACENCY = $000D;
  GL_PROGRAM_POINT_SIZE = $8642;
  GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS = $8C29;
  GL_FRAMEBUFFER_ATTACHMENT_LAYERED = $8DA7;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS = $8DA8;
  GL_GEOMETRY_SHADER = $8DD9;
  GL_GEOMETRY_VERTICES_OUT = $8916;
  GL_GEOMETRY_INPUT_TYPE = $8917;
  GL_GEOMETRY_OUTPUT_TYPE = $8918;
  GL_MAX_GEOMETRY_UNIFORM_COMPONENTS = $8DDF;
  GL_MAX_GEOMETRY_OUTPUT_VERTICES = $8DE0;
  GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS = $8DE1;
  GL_MAX_VERTEX_OUTPUT_COMPONENTS = $9122;
  GL_MAX_GEOMETRY_INPUT_COMPONENTS = $9123;
  GL_MAX_GEOMETRY_OUTPUT_COMPONENTS = $9124;
  GL_MAX_FRAGMENT_INPUT_COMPONENTS = $9125;
  GL_CONTEXT_PROFILE_MASK = $9126;
  // re-use tokens from:
  // VERSION_3_0
  // ARB_framebuffer_object (ARB #45)
  // ARB_depth_clamp (ARB #61)
  // ARB_draw_elements_base_vertex (ARB #62)
  // ARB_fragment_coord_conventions (ARB #63)
  // ARB_provoking_vertex (ARB #64)
  // ARB_seamless_cube_map (ARB #65)
  // ARB_sync (ARB #66)
  // ARB_texture_multisample (ARB #67)
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'New core constants in OpenGL v3.3'}{$ENDIF}
  GL_VERTEX_ATTRIB_ARRAY_DIVISOR = $88FE;
  // re-use tokens from:
  // GL_ARB_blend_func_extended (ARB #78)
  // GL_ARB_explicit_attrib_location (ARB #79) (none)
  // GL_ARB_occlusion_query2 (ARB #80)
  // GL_ARB_sampler_objects (ARB #81)
  // GL_ARB_shader_bit_encoding (ARB #82) (none)
  // GL_ARB_texture_rgb10_a2ui (ARB #83)
  // GL_ARB_texture_swizzle (ARB #84)
  // GL_ARB_timer_query (ARB #85)
  // GL_ARB_vertex_type_2_10_10_10_rev (ARB #86)
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'New core constants in OpenGL v4.0'}{$ENDIF}
  GL_SAMPLE_SHADING = $8C36;
  GL_MIN_SAMPLE_SHADING_VALUE = $8C37;
  GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET = $8E5E;
  GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET = $8E5F;
  GL_TEXTURE_CUBE_MAP_ARRAY = $9009;
  GL_TEXTURE_BINDING_CUBE_MAP_ARRAY = $900A;
  GL_PROXY_TEXTURE_CUBE_MAP_ARRAY = $900B;
  GL_SAMPLER_CUBE_MAP_ARRAY = $900C;
  GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW = $900D;
  GL_INT_SAMPLER_CUBE_MAP_ARRAY = $900E;
  GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY = $900F;
  // re-use tokens from:
  // GL_ARB_draw_indirect (ARB #87)
  // GL_ARB_gpu_shader5 (ARB #88)
  // GL_ARB_gpu_shader_fp64 (ARB #89)
  // GL_ARB_shader_subroutine (ARB #90)
  // GL_ARB_tessellation_shader (ARB #91)
  // GL_ARB_texture_buffer_object_rgb32 (ARB #92) (none)
  // GL_ARB_transform_feedback2 (ARB #93)
  // GL_ARB_transform_feedback3 (ARB # 94)
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'New core constants in OpenGL v4.1'}{$ENDIF}
  // re-use tokens from:
  // GL_ARB_ES2_compatibility (ARB #95)
  // GL_ARB_get_program_binary (ARB #96)
  // GL_ARB_separate_shader_objects (ARB #97)
  // GL_ARB_shader_precision (none) (ARB #98)
  // GL_ARB_vertex_attrib_64bit (ARB #99)
  // GL_ARB_viewport_array (ARB #100)

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'ARB approved extensions constants, in extension number order'}{$ENDIF}
  // ARB approved extensions enumerants, in number order

  // ARB Extension #1 - GL_ARB_multitexture
  GL_ACTIVE_TEXTURE_ARB = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE_ARB = $84E1;
  GL_MAX_TEXTURE_UNITS_ARB = $84E2;
  GL_TEXTURE0_ARB = $84C0;
  GL_TEXTURE1_ARB = $84C1;
  GL_TEXTURE2_ARB = $84C2;
  GL_TEXTURE3_ARB = $84C3;
  GL_TEXTURE4_ARB = $84C4;
  GL_TEXTURE5_ARB = $84C5;
  GL_TEXTURE6_ARB = $84C6;
  GL_TEXTURE7_ARB = $84C7;
  GL_TEXTURE8_ARB = $84C8;
  GL_TEXTURE9_ARB = $84C9;
  GL_TEXTURE10_ARB = $84CA;
  GL_TEXTURE11_ARB = $84CB;
  GL_TEXTURE12_ARB = $84CC;
  GL_TEXTURE13_ARB = $84CD;
  GL_TEXTURE14_ARB = $84CE;
  GL_TEXTURE15_ARB = $84CF;
  GL_TEXTURE16_ARB = $84D0;
  GL_TEXTURE17_ARB = $84D1;
  GL_TEXTURE18_ARB = $84D2;
  GL_TEXTURE19_ARB = $84D3;
  GL_TEXTURE20_ARB = $84D4;
  GL_TEXTURE21_ARB = $84D5;
  GL_TEXTURE22_ARB = $84D6;
  GL_TEXTURE23_ARB = $84D7;
  GL_TEXTURE24_ARB = $84D8;
  GL_TEXTURE25_ARB = $84D9;
  GL_TEXTURE26_ARB = $84DA;
  GL_TEXTURE27_ARB = $84DB;
  GL_TEXTURE28_ARB = $84DC;
  GL_TEXTURE29_ARB = $84DD;
  GL_TEXTURE30_ARB = $84DE;
  GL_TEXTURE31_ARB = $84DF;

  // ARB Extension #2 - GLX_ARB_get_proc_address
  // (no new tokens)

  // ARB Extension #3 - GL_ARB_transpose_matrix
  GL_TRANSPOSE_MODELVIEW_MATRIX_ARB = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX_ARB = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX_ARB = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX_ARB = $84E6;

  // ARB Extension #4 - WGL_ARB_buffer_region
  WGL_FRONT_COLOR_BUFFER_BIT_ARB = $00000001;
  WGL_BACK_COLOR_BUFFER_BIT_ARB = $00000002;
  WGL_DEPTH_BUFFER_BIT_ARB = $00000004;
  WGL_STENCIL_BUFFER_BIT_ARB = $00000008;

  // ARB Extension #5 - GL_ARB_multisample
  //                  - GLX_ARB_multisample
  //                  - WGL_ARB_multisample
  GL_MULTISAMPLE_ARB = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE_ARB = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_ARB = $809F;
  GL_SAMPLE_COVERAGE_ARB = $80A0;
  GL_SAMPLE_BUFFERS_ARB = $80A8;
  GL_SAMPLES_ARB = $80A9;
  GL_SAMPLE_COVERAGE_VALUE_ARB = $80AA;
  GL_SAMPLE_COVERAGE_INVERT_ARB = $80AB;
  GL_MULTISAMPLE_BIT_ARB = $20000000;
  GLX_SAMPLE_BUFFERS_ARB = 100000;
  GLX_SAMPLES_ARB = 100001;
  WGL_SAMPLE_BUFFERS_ARB = $2041;
  WGL_SAMPLES_ARB = $2042;
  //GLX 1.4
  GLX_SAMPLE_BUFFERS_SGIS = $100000; //Visual attribute (SGIS_multisample)
  GLX_SAMPLES_SGIS = $100001;
  GLX_SAMPLE_BUFFERS = $100000; //Visual attribute (GLX 1.4 core - alias of SGIS_multisample)
  GLX_SAMPLES = $100001;

  // ARB Extension #6 - GL_ARB_texture_env_add
  // (no new tokens)

  // ARB Extension #7 - GL_ARB_texture_cube_map
  GL_NORMAL_MAP_ARB = $8511;
  GL_REFLECTION_MAP_ARB = $8512;
  GL_TEXTURE_CUBE_MAP_ARB = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP_ARB = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP_ARB = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB = $851C;

  // ARB Extension #8 - WGL_ARB_extensions_string
  // (no new tokens)

  // ARB Extension #9 - WGL_ARB_pixel_format
  // (no new tokens)
  WGL_NUMBER_PIXEL_FORMATS_ARB = $2000;
  WGL_DRAW_TO_WINDOW_ARB = $2001;
  WGL_DRAW_TO_BITMAP_ARB = $2002;
  WGL_ACCELERATION_ARB = $2003;
  WGL_NEED_PALETTE_ARB = $2004;
  WGL_NEED_SYSTEM_PALETTE_ARB = $2005;
  WGL_SWAP_LAYER_BUFFERS_ARB = $2006;
  WGL_SWAP_METHOD_ARB = $2007;
  WGL_NUMBER_OVERLAYS_ARB = $2008;
  WGL_NUMBER_UNDERLAYS_ARB = $2009;
  WGL_TRANSPARENT_ARB = $200A;
  WGL_TRANSPARENT_RED_VALUE_ARB = $2037;
  WGL_TRANSPARENT_GREEN_VALUE_ARB = $2038;
  WGL_TRANSPARENT_BLUE_VALUE_ARB = $2039;
  WGL_TRANSPARENT_ALPHA_VALUE_ARB = $203A;
  WGL_TRANSPARENT_INDEX_VALUE_ARB = $203B;
  WGL_SHARE_DEPTH_ARB = $200C;
  WGL_SHARE_STENCIL_ARB = $200D;
  WGL_SHARE_ACCUM_ARB = $200E;
  WGL_SUPPORT_GDI_ARB = $200F;
  WGL_SUPPORT_OPENGL_ARB = $2010;
  WGL_DOUBLE_BUFFER_ARB = $2011;
  WGL_STEREO_ARB = $2012;
  WGL_PIXEL_TYPE_ARB = $2013;
  WGL_COLOR_BITS_ARB = $2014;
  WGL_RED_BITS_ARB = $2015;
  WGL_RED_SHIFT_ARB = $2016;
  WGL_GREEN_BITS_ARB = $2017;
  WGL_GREEN_SHIFT_ARB = $2018;
  WGL_BLUE_BITS_ARB = $2019;
  WGL_BLUE_SHIFT_ARB = $201A;
  WGL_ALPHA_BITS_ARB = $201B;
  WGL_ALPHA_SHIFT_ARB = $201C;
  WGL_ACCUM_BITS_ARB = $201D;
  WGL_ACCUM_RED_BITS_ARB = $201E;
  WGL_ACCUM_GREEN_BITS_ARB = $201F;
  WGL_ACCUM_BLUE_BITS_ARB = $2020;
  WGL_ACCUM_ALPHA_BITS_ARB = $2021;
  WGL_DEPTH_BITS_ARB = $2022;
  WGL_STENCIL_BITS_ARB = $2023;
  WGL_AUX_BUFFERS_ARB = $2024;
  WGL_NO_ACCELERATION_ARB = $2025;
  WGL_GENERIC_ACCELERATION_ARB = $2026;
  WGL_FULL_ACCELERATION_ARB = $2027;
  WGL_SWAP_EXCHANGE_ARB = $2028;
  WGL_SWAP_COPY_ARB = $2029;
  WGL_SWAP_UNDEFINED_ARB = $202A;
  WGL_TYPE_RGBA_ARB = $202B;
  WGL_TYPE_COLORINDEX_ARB = $202C;

  // ARB Extension #10 - WGL_ARB_make_current_read
  ERROR_INVALID_PIXEL_TYPE_ARB = $2043;
  ERROR_INCOMPATIBLE_DEVICE_CONTEXTS_ARB = $2054;

  // ARB Extension #11 - WGL_ARB_pbuffer
  WGL_DRAW_TO_PBUFFER_ARB = $202D;
  WGL_MAX_PBUFFER_PIXELS_ARB = $202E;
  WGL_MAX_PBUFFER_WIDTH_ARB = $202F;
  WGL_MAX_PBUFFER_HEIGHT_ARB = $2030;
  WGL_PBUFFER_LARGEST_ARB = $2033;
  WGL_PBUFFER_WIDTH_ARB = $2034;
  WGL_PBUFFER_HEIGHT_ARB = $2035;
  WGL_PBUFFER_LOST_ARB = $2036;

  // ARB Extension #12 - GL_ARB_texture_compression
  GL_COMPRESSED_ALPHA_ARB = $84E9;
  GL_COMPRESSED_LUMINANCE_ARB = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA_ARB = $84EB;
  GL_COMPRESSED_INTENSITY_ARB = $84EC;
  GL_COMPRESSED_RGB_ARB = $84ED;
  GL_COMPRESSED_RGBA_ARB = $84EE;
  GL_TEXTURE_COMPRESSION_HINT_ARB = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB = $86A0;
  GL_TEXTURE_COMPRESSED_ARB = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS_ARB = $86A3;

  // ARB Extension #13 - GL_ARB_texture_border_clamp
  // (promoted from #36 GL_SGIS_texture_border_clamp)
  GL_CLAMP_TO_BORDER_ARB = $812D;

  // ARB Extension #14 - GL_ARB_point_parameters
  // (promoted from #54 GL_{SGIS,EXT}_point_parameters)
  GL_POINT_SIZE_MIN_ARB = $8126;
  GL_POINT_SIZE_MAX_ARB = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_ARB = $8128;
  GL_DISTANCE_ATTENUATION_ARB = $8129;

  // ARB Extension #15 - GL_ARB_vertex_blend
  GL_MAX_VERTEX_UNITS_ARB = $86A4;
  GL_ACTIVE_VERTEX_UNITS_ARB = $86A5;
  GL_WEIGHT_SUM_UNITY_ARB = $86A6;
  GL_VERTEX_BLEND_ARB = $86A7;
  GL_CURRENT_WEIGHT_ARB = $86A8;
  GL_WEIGHT_ARRAY_TYPE_ARB = $86A9;
  GL_WEIGHT_ARRAY_STRIDE_ARB = $86AA;
  GL_WEIGHT_ARRAY_SIZE_ARB = $86AB;
  GL_WEIGHT_ARRAY_POINTER_ARB = $86AC;
  GL_WEIGHT_ARRAY_ARB = $86AD;
  GL_MODELVIEW0_ARB = $1700;
  GL_MODELVIEW1_ARB = $850A;
  GL_MODELVIEW2_ARB = $8722;
  GL_MODELVIEW3_ARB = $8723;
  GL_MODELVIEW4_ARB = $8724;
  GL_MODELVIEW5_ARB = $8725;
  GL_MODELVIEW6_ARB = $8726;
  GL_MODELVIEW7_ARB = $8727;
  GL_MODELVIEW8_ARB = $8728;
  GL_MODELVIEW9_ARB = $8729;
  GL_MODELVIEW10_ARB = $872A;
  GL_MODELVIEW11_ARB = $872B;
  GL_MODELVIEW12_ARB = $872C;
  GL_MODELVIEW13_ARB = $872D;
  GL_MODELVIEW14_ARB = $872E;
  GL_MODELVIEW15_ARB = $872F;
  GL_MODELVIEW16_ARB = $8730;
  GL_MODELVIEW17_ARB = $8731;
  GL_MODELVIEW18_ARB = $8732;
  GL_MODELVIEW19_ARB = $8733;
  GL_MODELVIEW20_ARB = $8734;
  GL_MODELVIEW21_ARB = $8735;
  GL_MODELVIEW22_ARB = $8736;
  GL_MODELVIEW23_ARB = $8737;
  GL_MODELVIEW24_ARB = $8738;
  GL_MODELVIEW25_ARB = $8739;
  GL_MODELVIEW26_ARB = $873A;
  GL_MODELVIEW27_ARB = $873B;
  GL_MODELVIEW28_ARB = $873C;
  GL_MODELVIEW29_ARB = $873D;
  GL_MODELVIEW30_ARB = $873E;
  GL_MODELVIEW31_ARB = $873F;

  // ARB Extension #16 - GL_ARB_matrix_palette
  GL_MATRIX_PALETTE_ARB = $8840;
  GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB = $8841;
  GL_MAX_PALETTE_MATRICES_ARB = $8842;
  GL_CURRENT_PALETTE_MATRIX_ARB = $8843;
  GL_MATRIX_INDEX_ARRAY_ARB = $8844;
  GL_CURRENT_MATRIX_INDEX_ARB = $8845;
  GL_MATRIX_INDEX_ARRAY_SIZE_ARB = $8846;
  GL_MATRIX_INDEX_ARRAY_TYPE_ARB = $8847;
  GL_MATRIX_INDEX_ARRAY_STRIDE_ARB = $8848;
  GL_MATRIX_INDEX_ARRAY_POINTER_ARB = $8849;

  // ARB Extension #17 - GL_ARB_texture_env_combine
  // (Shares enum values with #158 GL_EXT_texture_env_combine)
  GL_COMBINE_ARB = $8570;
  GL_COMBINE_RGB_ARB = $8571;
  GL_COMBINE_ALPHA_ARB = $8572;
  GL_RGB_SCALE_ARB = $8573;
  GL_ADD_SIGNED_ARB = $8574;
  GL_INTERPOLATE_ARB = $8575;
  GL_CONSTANT_ARB = $8576;
  GL_CONSTANT_COLOR_ARB = $8576;
  GL_PRIMARY_COLOR_ARB = $8577;
  GL_PREVIOUS_ARB = $8578;
  GL_SOURCE0_RGB_ARB = $8580;
  GL_SOURCE1_RGB_ARB = $8581;
  GL_SOURCE2_RGB_ARB = $8582;
  GL_SOURCE0_ALPHA_ARB = $8588;
  GL_SOURCE1_ALPHA_ARB = $8589;
  GL_SOURCE2_ALPHA_ARB = $858A;
  GL_OPERAND0_RGB_ARB = $8590;
  GL_OPERAND1_RGB_ARB = $8591;
  GL_OPERAND2_RGB_ARB = $8592;
  GL_OPERAND0_ALPHA_ARB = $8598;
  GL_OPERAND1_ALPHA_ARB = $8599;
  GL_OPERAND2_ALPHA_ARB = $859A;
  GL_SUBTRACT_ARB = $84E7;

  // ARB Extension #18 - GL_ARB_texture_env_crossbar
  // (no new tokens)

  // ARB Extension #19 - GL_ARB_texture_env_dot3
  // (promoted from #220 GL_EXT_texture_env_dot3; enum values changed)
  GL_DOT3_RGB_ARB = $86AE;
  GL_DOT3_RGBA_ARB = $86AF;

  // ARB Extension #20 - WGL_ARB_render_texture
  WGL_BIND_TO_TEXTURE_RGB_ARB = $2070;
  WGL_BIND_TO_TEXTURE_RGBA_ARB = $2071;
  WGL_TEXTURE_FORMAT_ARB = $2072;
  WGL_TEXTURE_TARGET_ARB = $2073;
  WGL_MIPMAP_TEXTURE_ARB = $2074;
  WGL_TEXTURE_RGB_ARB = $2075;
  WGL_TEXTURE_RGBA_ARB = $2076;
  WGL_NO_TEXTURE_ARB = $2077;
  WGL_TEXTURE_CUBE_MAP_ARB = $2078;
  WGL_TEXTURE_1D_ARB = $2079;
  WGL_TEXTURE_2D_ARB = $207A;
  WGL_MIPMAP_LEVEL_ARB = $207B;
  WGL_CUBE_MAP_FACE_ARB = $207C;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = $207D;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = $207E;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = $207F;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = $2080;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = $2081;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = $2082;
  WGL_FRONT_LEFT_ARB = $2083;
  WGL_FRONT_RIGHT_ARB = $2084;
  WGL_BACK_LEFT_ARB = $2085;
  WGL_BACK_RIGHT_ARB = $2086;
  WGL_AUX0_ARB = $2087;
  WGL_AUX1_ARB = $2088;
  WGL_AUX2_ARB = $2089;
  WGL_AUX3_ARB = $208A;
  WGL_AUX4_ARB = $208B;
  WGL_AUX5_ARB = $208C;
  WGL_AUX6_ARB = $208D;
  WGL_AUX7_ARB = $208E;
  WGL_AUX8_ARB = $208F;
  WGL_AUX9_ARB = $2090;

  // ARB Extension #21 - GL_ARB_texture_mirrored_repeat
  GL_MIRRORED_REPEAT_ARB = $8370;

  // ARB Extension #22 - GL_ARB_depth_texture
  GL_DEPTH_COMPONENT16_ARB = $81A5;
  GL_DEPTH_COMPONENT24_ARB = $81A6;
  GL_DEPTH_COMPONENT32_ARB = $81A7;
  GL_TEXTURE_DEPTH_SIZE_ARB = $884A;
  GL_DEPTH_TEXTURE_MODE_ARB = $884B;

  // ARB Extension #23 - GL_ARB_shadow
  GL_TEXTURE_COMPARE_MODE_ARB = $884C;
  GL_TEXTURE_COMPARE_FUNC_ARB = $884D;
  GL_COMPARE_R_TO_TEXTURE_ARB = $884E;

  // ARB Extension #24 - GL_ARB_shadow_ambient
  // (same as #90 GL_SGIX_shadow_ambient)
  GL_TEXTURE_COMPARE_FAIL_VALUE_ARB = $80BF;

  // ARB Extension #25 - GL_ARB_window_pos
  // (no new tokens)

  // ARB Extension #26 - GL_ARB_vertex_program
  // GL_ARB_vertex_program enums are shared by GL_ARB_fragment_program are so marked.
  // Unfortunately, PROGRAM_BINDING_ARB does accidentally reuse 0x8677 -
  //   this was a spec editing typo that's now uncorrectable.
  GL_COLOR_SUM_ARB = $8458;
  GL_VERTEX_PROGRAM_ARB = $8620;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB = $8625;
  GL_CURRENT_VERTEX_ATTRIB_ARB = $8626;
  GL_PROGRAM_LENGTH_ARB = $8627; //shared
  GL_PROGRAM_STRING_ARB = $8628; //shared
  GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB = $862E; //shared
  GL_MAX_PROGRAM_MATRICES_ARB = $862F; //shared
  GL_CURRENT_MATRIX_STACK_DEPTH_ARB = $8640; //shared
  GL_CURRENT_MATRIX_ARB = $8641; //shared
  GL_VERTEX_PROGRAM_POINT_SIZE_ARB = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_ARB = $8643;
  GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB = $8645;
  GL_PROGRAM_ERROR_POSITION_ARB = $864B; //shared
  GL_PROGRAM_BINDING_ARB = $8677; //shared
  GL_MAX_VERTEX_ATTRIBS_ARB = $8869;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB = $886A;

  GL_PROGRAM_ERROR_STRING_ARB = $8874; //shared
  GL_PROGRAM_FORMAT_ASCII_ARB = $8875; //shared
  GL_PROGRAM_FORMAT_ARB = $8876; //shared

  GL_PROGRAM_INSTRUCTIONS_ARB = $88A0; //shared
  GL_MAX_PROGRAM_INSTRUCTIONS_ARB = $88A1; //shared
  GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB = $88A2; //shared
  GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB = $88A3; //shared
  GL_PROGRAM_TEMPORARIES_ARB = $88A4; //shared
  GL_MAX_PROGRAM_TEMPORARIES_ARB = $88A5; //shared
  GL_PROGRAM_NATIVE_TEMPORARIES_ARB = $88A6; //shared
  GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB = $88A7; //shared
  GL_PROGRAM_PARAMETERS_ARB = $88A8; //shared
  GL_MAX_PROGRAM_PARAMETERS_ARB = $88A9; //shared
  GL_PROGRAM_NATIVE_PARAMETERS_ARB = $88AA; //shared
  GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB = $88AB; //shared
  GL_PROGRAM_ATTRIBS_ARB = $88AC; //shared
  GL_MAX_PROGRAM_ATTRIBS_ARB = $88AD; //shared
  GL_PROGRAM_NATIVE_ATTRIBS_ARB = $88AE; //shared
  GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB = $88AF; //shared
  GL_PROGRAM_ADDRESS_REGISTERS_ARB = $88B0; //shared
  GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB = $88B1; //shared
  GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB = $88B2; //shared
  GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB = $88B3; //shared
  GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB = $88B4; //shared
  GL_MAX_PROGRAM_ENV_PARAMETERS_ARB = $88B5; //shared
  GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB = $88B6; //shared
  GL_TRANSPOSE_CURRENT_MATRIX_ARB = $88B7; //shared

  GL_MATRIX0_ARB = $88C0; //shared
  GL_MATRIX1_ARB = $88C1; //shared
  GL_MATRIX2_ARB = $88C2; //shared
  GL_MATRIX3_ARB = $88C3; //shared
  GL_MATRIX4_ARB = $88C4; //shared
  GL_MATRIX5_ARB = $88C5; //shared
  GL_MATRIX6_ARB = $88C6; //shared
  GL_MATRIX7_ARB = $88C7; //shared
  GL_MATRIX8_ARB = $88C8; //shared
  GL_MATRIX9_ARB = $88C9; //shared
  GL_MATRIX10_ARB = $88CA; //shared
  GL_MATRIX11_ARB = $88CB; //shared
  GL_MATRIX12_ARB = $88CC; //shared
  GL_MATRIX13_ARB = $88CD; //shared
  GL_MATRIX14_ARB = $88CE; //shared
  GL_MATRIX15_ARB = $88CF; //shared
  GL_MATRIX16_ARB = $88D0; //shared
  GL_MATRIX17_ARB = $88D1; //shared
  GL_MATRIX18_ARB = $88D2; //shared
  GL_MATRIX19_ARB = $88D3; //shared
  GL_MATRIX20_ARB = $88D4; //shared
  GL_MATRIX21_ARB = $88D5; //shared
  GL_MATRIX22_ARB = $88D6; //shared
  GL_MATRIX23_ARB = $88D7; //shared
  GL_MATRIX24_ARB = $88D8; //shared
  GL_MATRIX25_ARB = $88D9; //shared
  GL_MATRIX26_ARB = $88DA; //shared
  GL_MATRIX27_ARB = $88DB; //shared
  GL_MATRIX28_ARB = $88DC; //shared
  GL_MATRIX29_ARB = $88DD; //shared
  GL_MATRIX30_ARB = $88DE; //shared
  GL_MATRIX31_ARB = $88DF; //shared

  // ARB Extension #27 - GL_ARB_fragment_program
  // Some GL_ARB_fragment_program enums are shared with #26 GL_ARB_vertex_program,
  //  and are included in there for now.
  GL_FRAGMENT_PROGRAM_ARB = $8804;
  GL_PROGRAM_ALU_INSTRUCTIONS_ARB = $8805;
  GL_PROGRAM_TEX_INSTRUCTIONS_ARB = $8806;
  GL_PROGRAM_TEX_INDIRECTIONS_ARB = $8807;
  GL_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB = $8808;
  GL_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB = $8809;
  GL_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB = $880A;
  GL_MAX_PROGRAM_ALU_INSTRUCTIONS_ARB = $880B;
  GL_MAX_PROGRAM_TEX_INSTRUCTIONS_ARB = $880C;
  GL_MAX_PROGRAM_TEX_INDIRECTIONS_ARB = $880D;
  GL_MAX_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB = $880E;
  GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB = $880F;
  GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB = $8810;
  GL_MAX_TEXTURE_COORDS_ARB = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS_ARB = $8872;

  // ARB Extension #28 - GL_ARB_vertex_buffer_object
  GL_BUFFER_SIZE_ARB = $8764;
  GL_BUFFER_USAGE_ARB = $8765;
  GL_ARRAY_BUFFER_ARB = $8892;
  GL_ELEMENT_ARRAY_BUFFER_ARB = $8893;
  GL_ARRAY_BUFFER_BINDING_ARB = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB = $8895;
  GL_VERTEX_ARRAY_BUFFER_BINDING_ARB = $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING_ARB = $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING_ARB = $8898;
  GL_INDEX_ARRAY_BUFFER_BINDING_ARB = $8899;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB = $889A;
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB = $889B;
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB = $889C;
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB = $889D;
  GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB = $889E;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB = $889F;
  GL_READ_ONLY_ARB = $88B8;
  GL_WRITE_ONLY_ARB = $88B9;
  GL_READ_WRITE_ARB = $88BA;
  GL_BUFFER_ACCESS_ARB = $88BB;
  GL_BUFFER_MAPPED_ARB = $88BC;
  GL_BUFFER_MAP_POINTER_ARB = $88BD;
  GL_STREAM_DRAW_ARB = $88E0;
  GL_STREAM_READ_ARB = $88E1;
  GL_STREAM_COPY_ARB = $88E2;
  GL_STATIC_DRAW_ARB = $88E4;
  GL_STATIC_READ_ARB = $88E5;
  GL_STATIC_COPY_ARB = $88E6;
  GL_DYNAMIC_DRAW_ARB = $88E8;
  GL_DYNAMIC_READ_ARB = $88E9;
  GL_DYNAMIC_COPY_ARB = $88EA;

  // ARB Extension #29 - GL_ARB_occlusion_query
  // (promoted from GL_HP_occulsion_query / GL_NV_occlusion_query)
  GL_QUERY_COUNTER_BITS_ARB = $8864;
  GL_CURRENT_QUERY_ARB = $8865;
  GL_QUERY_RESULT_ARB = $8866;
  GL_QUERY_RESULT_AVAILABLE_ARB = $8867;
  GL_SAMPLES_PASSED_ARB = $8914;

  // ARB Extension #30 - GL_ARB_shader_objects
  GL_PROGRAM_OBJECT_ARB = $8B40;
  GL_SHADER_OBJECT_ARB = $8B48;
  GL_OBJECT_TYPE_ARB = $8B4E;
  GL_OBJECT_SUBTYPE_ARB = $8B4F;
  GL_FLOAT_VEC2_ARB = $8B50;
  GL_FLOAT_VEC3_ARB = $8B51;
  GL_FLOAT_VEC4_ARB = $8B52;
  GL_INT_VEC2_ARB = $8B53;
  GL_INT_VEC3_ARB = $8B54;
  GL_INT_VEC4_ARB = $8B55;
  GL_BOOL_ARB = $8B56;
  GL_BOOL_VEC2_ARB = $8B57;
  GL_BOOL_VEC3_ARB = $8B58;
  GL_BOOL_VEC4_ARB = $8B59;
  GL_FLOAT_MAT2_ARB = $8B5A;
  GL_FLOAT_MAT3_ARB = $8B5B;
  GL_FLOAT_MAT4_ARB = $8B5C;
  GL_SAMPLER_1D_ARB = $8B5D;
  GL_SAMPLER_2D_ARB = $8B5E;
  GL_SAMPLER_3D_ARB = $8B5F;
  GL_SAMPLER_CUBE_ARB = $8B60;
  GL_SAMPLER_1D_SHADOW_ARB = $8B61;
  GL_SAMPLER_2D_SHADOW_ARB = $8B62;
  GL_SAMPLER_2D_RECT_ARB = $8B63;
  GL_SAMPLER_2D_RECT_SHADOW_ARB = $8B64;
  GL_OBJECT_DELETE_STATUS_ARB = $8B80;
  GL_OBJECT_COMPILE_STATUS_ARB = $8B81;
  GL_OBJECT_LINK_STATUS_ARB = $8B82;
  GL_OBJECT_VALIDATE_STATUS_ARB = $8B83;
  GL_OBJECT_INFO_LOG_LENGTH_ARB = $8B84;
  GL_OBJECT_ATTACHED_OBJECTS_ARB = $8B85;
  GL_OBJECT_ACTIVE_UNIFORMS_ARB = $8B86;
  GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB = $8B87;
  GL_OBJECT_SHADER_SOURCE_LENGTH_ARB = $8B88;

  // ARB Extension #31 - GL_ARB_vertex_shader
  // (additional enums are reused from:
  //  #26 GL_ARB_vertex_program
  //  #27 GL_ARB_fragment_program
  //  #30 GL_ARB_shader_objects)
  GL_VERTEX_SHADER_ARB = $8B31;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB = $8B4A;
  GL_MAX_VARYING_FLOATS_ARB = $8B4B;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB = $8B4D;
  GL_OBJECT_ACTIVE_ATTRIBUTES_ARB = $8B89;
  GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB = $8B8A;

  // ARB Extension #32 - GL_ARB_fragment_shader
  // (additional enums are reused from #27 GL_ARB_fragment_program and #30 GL_ARB_shader_objects)
  GL_FRAGMENT_SHADER_ARB = $8B30;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB = $8B49;
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB = $8B8B;

  // ARB Extension #33 - GL_ARB_shading_language_100
  GL_SHADING_LANGUAGE_VERSION_ARB = $8B8C;

  // ARB Extension #34 - GL_ARB_texture_non_power_of_two
  // (no new tokens)

  // ARB Extension #35 - GL_ARB_point_sprite
  GL_POINT_SPRITE_ARB = $8861;
  GL_COORD_REPLACE_ARB = $8862;

  // ARB Extension #36 - GL_ARB_fragment_program_shadow
  // (no new tokens)

  // ARB Extension #37 - GL_ARB_draw_buffers
  GL_MAX_DRAW_BUFFERS_ARB = $8824;
  GL_DRAW_BUFFER0_ARB = $8825;
  GL_DRAW_BUFFER1_ARB = $8826;
  GL_DRAW_BUFFER2_ARB = $8827;
  GL_DRAW_BUFFER3_ARB = $8828;
  GL_DRAW_BUFFER4_ARB = $8829;
  GL_DRAW_BUFFER5_ARB = $882A;
  GL_DRAW_BUFFER6_ARB = $882B;
  GL_DRAW_BUFFER7_ARB = $882C;
  GL_DRAW_BUFFER8_ARB = $882D;
  GL_DRAW_BUFFER9_ARB = $882E;
  GL_DRAW_BUFFER10_ARB = $882F;
  GL_DRAW_BUFFER11_ARB = $8830;
  GL_DRAW_BUFFER12_ARB = $8831;
  GL_DRAW_BUFFER13_ARB = $8832;
  GL_DRAW_BUFFER14_ARB = $8833;
  GL_DRAW_BUFFER15_ARB = $8834;

  // ARB Extension #38 - GL_ARB_texture_rectangle
  GL_TEXTURE_RECTANGLE_ARB = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE_ARB = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE_ARB = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB = $84F8;

  // ARB Extension #39 - GL_ARB_color_buffer_float
  //                   - WGL_ARB_pixel_format_float
  //                   - GLX_ARB_fbconfig_float
  GL_RGBA_FLOAT_MODE_ARB = $8820;
  GL_CLAMP_VERTEX_COLOR_ARB = $891A;
  GL_CLAMP_FRAGMENT_COLOR_ARB = $891B;
  GL_CLAMP_READ_COLOR_ARB = $891C;
  GL_FIXED_ONLY_ARB = $891D;

  WGL_TYPE_RGBA_FLOAT_ARB = $21A0;
  GLX_RGBA_FLOAT_TYPE_ARB = $20B9;
  GLX_RGBA_FLOAT_BIT_ARB = $00000004;

  // ARB Extension #40 - GL_ARB_half_float_pixel
  GL_HALF_FLOAT_ARB = $140B;

  // ARB Extension #41 - GL_ARB_texture_float
  GL_TEXTURE_RED_TYPE_ARB = $8C10;
  GL_TEXTURE_GREEN_TYPE_ARB = $8C11;
  GL_TEXTURE_BLUE_TYPE_ARB = $8C12;
  GL_TEXTURE_ALPHA_TYPE_ARB = $8C13;
  GL_TEXTURE_LUMINANCE_TYPE_ARB = $8C14;
  GL_TEXTURE_INTENSITY_TYPE_ARB = $8C15;
  GL_TEXTURE_DEPTH_TYPE_ARB = $8C16;
  GL_UNSIGNED_NORMALIZED_ARB = $8C17;
  GL_RGBA32F_ARB = $8814;
  GL_RGB32F_ARB = $8815;
  GL_ALPHA32F_ARB = $8816;
  GL_INTENSITY32F_ARB = $8817;
  GL_LUMINANCE32F_ARB = $8818;
  GL_LUMINANCE_ALPHA32F_ARB = $8819;
  GL_RGBA16F_ARB = $881A;
  GL_RGB16F_ARB = $881B;
  GL_ALPHA16F_ARB = $881C;
  GL_INTENSITY16F_ARB = $881D;
  GL_LUMINANCE16F_ARB = $881E;
  GL_LUMINANCE_ALPHA16F_ARB = $881F;

  // ARB Extension #42 - GL_ARB_pixel_buffer_object
  GL_PIXEL_PACK_BUFFER_ARB = $88EB;
  GL_PIXEL_UNPACK_BUFFER_ARB = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING_ARB = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING_ARB = $88EF;

  // ARB Extension #43 - GL_ARB_depth_buffer_float
  GL_DEPTH_COMPONENT32F = $8CAC;
  GL_DEPTH32F_STENCIL8 = $8CAD;
  GL_FLOAT_32_UNSIGNED_INT_24_8_REV = $8DAD;

  // ARB Extension #44 - GL_ARB_draw_instanced
  // (no new tokens)

  // ARB Extension #45 - GL_ARB_framebuffer_object
  // (Also went simultaneously to core 3.0, so no ARB prefix on names)
  GL_INVALID_FRAMEBUFFER_OPERATION = $0506;
  GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING = $8210;
  GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE = $8211;
  GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE = $8212;
  GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE = $8213;
  GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE = $8214;
  GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE = $8215;
  GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE = $8216;
  GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE = $8217;
  GL_FRAMEBUFFER_DEFAULT = $8218;
  GL_FRAMEBUFFER_UNDEFINED = $8219;
  GL_DEPTH_STENCIL_ATTACHMENT = $821A;
  GL_INDEX = $8222;
  GL_MAX_RENDERBUFFER_SIZE = $84E8;
  GL_DEPTH_STENCIL = $84F9;
  GL_UNSIGNED_INT_24_8 = $84FA;
  GL_DEPTH24_STENCIL8 = $88F0;
  GL_TEXTURE_STENCIL_SIZE = $88F1;
  GL_TEXTURE_RED_TYPE = $8C10;
  GL_TEXTURE_GREEN_TYPE = $8C11;
  GL_TEXTURE_BLUE_TYPE = $8C12;
  GL_TEXTURE_ALPHA_TYPE = $8C13;
  GL_TEXTURE_LUMINANCE_TYPE = $8C14;
  GL_TEXTURE_INTENSITY_TYPE = $8C15;
  GL_TEXTURE_DEPTH_TYPE = $8C16;
  GL_UNSIGNED_NORMALIZED = $8C17;
  GL_FRAMEBUFFER_BINDING = $8CA6;
  GL_DRAW_FRAMEBUFFER_BINDING = GL_FRAMEBUFFER_BINDING;
  GL_RENDERBUFFER_BINDING = $8CA7;
  GL_READ_FRAMEBUFFER = $8CA8;
  GL_DRAW_FRAMEBUFFER = $8CA9;
  GL_READ_FRAMEBUFFER_BINDING = $8CAA;
  GL_RENDERBUFFER_SAMPLES = $8CAB;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER = $8CD4;
  GL_FRAMEBUFFER_COMPLETE = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = $8CDC;
  GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;
  GL_MAX_COLOR_ATTACHMENTS = $8CDF;
  GL_COLOR_ATTACHMENT0 = $8CE0;
  GL_COLOR_ATTACHMENT1 = $8CE1;
  GL_COLOR_ATTACHMENT2 = $8CE2;
  GL_COLOR_ATTACHMENT3 = $8CE3;
  GL_COLOR_ATTACHMENT4 = $8CE4;
  GL_COLOR_ATTACHMENT5 = $8CE5;
  GL_COLOR_ATTACHMENT6 = $8CE6;
  GL_COLOR_ATTACHMENT7 = $8CE7;
  GL_COLOR_ATTACHMENT8 = $8CE8;
  GL_COLOR_ATTACHMENT9 = $8CE9;
  GL_COLOR_ATTACHMENT10 = $8CEA;
  GL_COLOR_ATTACHMENT11 = $8CEB;
  GL_COLOR_ATTACHMENT12 = $8CEC;
  GL_COLOR_ATTACHMENT13 = $8CED;
  GL_COLOR_ATTACHMENT14 = $8CEE;
  GL_COLOR_ATTACHMENT15 = $8CEF;
  GL_DEPTH_ATTACHMENT = $8D00;
  GL_STENCIL_ATTACHMENT = $8D20;
  GL_FRAMEBUFFER = $8D40;
  GL_RENDERBUFFER = $8D41;
  GL_RENDERBUFFER_WIDTH = $8D42;
  GL_RENDERBUFFER_HEIGHT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT = $8D44;
  GL_STENCIL_INDEX1 = $8D46;
  GL_STENCIL_INDEX4 = $8D47;
  GL_STENCIL_INDEX8 = $8D48;
  GL_STENCIL_INDEX16 = $8D49;
  GL_RENDERBUFFER_RED_SIZE = $8D50;
  GL_RENDERBUFFER_GREEN_SIZE = $8D51;
  GL_RENDERBUFFER_BLUE_SIZE = $8D52;
  GL_RENDERBUFFER_ALPHA_SIZE = $8D53;
  GL_RENDERBUFFER_DEPTH_SIZE = $8D54;
  GL_RENDERBUFFER_STENCIL_SIZE = $8D55;
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = $8D56;
  GL_MAX_SAMPLES = $8D57;

  // ARB Extension #46 -  GL_ARB_framebuffer_sRGB
  //                      GLX_ARB_framebuffer_sRGB
  //                      WGL_ARB_framebuffer_sRGB
  GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB = $20B2;
  WGL_FRAMEBUFFER_SRGB_CAPABLE_ARB = $20A9;
  GL_FRAMEBUFFER_SRGB = $8DB9;
  //GL_FRAMEBUFFER_SRGB_CAPABLE                       = $8DBA;

  // ARB Extension #47 - GL_ARB_geometry_shader4
  GL_GEOMETRY_SHADER_ARB = $8DD9;
  GL_GEOMETRY_VERTICES_OUT_ARB = $8DDA;
  GL_GEOMETRY_INPUT_TYPE_ARB = $8DDB;
  GL_GEOMETRY_OUTPUT_TYPE_ARB = $8DDC;
  GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_ARB = $8C29;
  GL_MAX_GEOMETRY_VARYING_COMPONENTS_ARB = $8DDD;
  GL_MAX_VERTEX_VARYING_COMPONENTS_ARB = $8DDE;
  GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_ARB = $8DDF;
  GL_MAX_GEOMETRY_OUTPUT_VERTICES_ARB = $8DE0;
  GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_ARB = $8DE1;
  GL_LINES_ADJACENCY_ARB = $A;
  GL_LINE_STRIP_ADJACENCY_ARB = $B;
  GL_TRIANGLES_ADJACENCY_ARB = $C;
  GL_TRIANGLE_STRIP_ADJACENCY_ARB = $D;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_ARB = $8DA8;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_ARB = $8DA9;
  GL_FRAMEBUFFER_ATTACHMENT_LAYERED_ARB = $8DA7;
  //GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER             =$8CD4;
  GL_PROGRAM_POINT_SIZE_ARB = $8642;

  // ARB Extension #48 - GL_ARB_half_float_vertex
  GL_HALF_FLOAT = $140B;

  // ARB Extension #49 - GL_ARB_instanced_arrays
  GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ARB = $88FE;

  // ARB Extension #50 - GL_ARB_map_buffer_range
  GL_MAP_READ_BIT = $0001;
  GL_MAP_WRITE_BIT = $0002;
  GL_MAP_INVALIDATE_RANGE_BIT = $0004;
  GL_MAP_INVALIDATE_BUFFER_BIT = $0008;
  GL_MAP_FLUSH_EXPLICIT_BIT = $0010;
  GL_MAP_UNSYNCHRONIZED_BIT = $0020;

  // ARB Extension #51 - GL_ARB_texture_buffer_object
  GL_TEXTURE_BUFFER_ARB = $8C2A;
  GL_MAX_TEXTURE_BUFFER_SIZE_ARB = $8C2B;
  GL_TEXTURE_BINDING_BUFFER_ARB = $8C2C;
  GL_TEXTURE_BUFFER_DATA_STORE_BINDING_ARB = $8C2D;
  GL_TEXTURE_BUFFER_FORMAT_ARB = $8C2E;

  // ARB Extension #52 - GL_ARB_texture_compression_rgtc
  GL_COMPRESSED_RED_RGTC1 = $8DBB;
  GL_COMPRESSED_SIGNED_RED_RGTC1 = $8DBC;
  GL_COMPRESSED_RG_RGTC2 = $8DBD;
  GL_COMPRESSED_SIGNED_RG_RGTC2 = $8DBE;

  // ARB Extension #53 - GL_ARB_texture_rg
  GL_R8 = $8229;
  GL_R16 = $822A;
  GL_RG8 = $822B;
  GL_RG16 = $822C;
  GL_R16F = $822D;
  GL_R32F = $822E;
  GL_RG16F = $822F;
  GL_RG32F = $8230;
  GL_R8I = $8231;
  GL_R8UI = $8232;
  GL_R16I = $8233;
  GL_R16UI = $8234;
  GL_R32I = $8235;
  GL_R32UI = $8236;
  GL_RG8I = $8237;
  GL_RG8UI = $8238;
  GL_RG16I = $8239;
  GL_RG16UI = $823A;
  GL_RG32I = $823B;
  GL_RG32UI = $823C;
  GL_RG = $8227;
  GL_RG_INTEGER = $8228;

  // ARB Extension #54 - GL_ARB_vertex_array_object
  GL_VERTEX_ARRAY_BINDING = $85B5;

  // ARB Extension #55 - WGL_ARB_create_context
  // see also WGL_ARB_create_context_profile (ARB #74)
  WGL_CONTEXT_MAJOR_VERSION_ARB = $2091;
  WGL_CONTEXT_MINOR_VERSION_ARB = $2092;
  WGL_CONTEXT_LAYER_PLANE_ARB = $2093;
  WGL_CONTEXT_FLAGS_ARB = $2094;
  WGL_CONTEXT_DEBUG_BIT_ARB = $0001;
  WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = $0002;
  ERROR_INVALID_VERSION_ARB = $2095;

  // ARB Extension #56 - GLX_ARB_create_context
  // see also GLX_ARB_create_context_profile (ARB #75)
  GLX_CONTEXT_MAJOR_VERSION_ARB = $2091;
  GLX_CONTEXT_MINOR_VERSION_ARB = $2092;
  GLX_CONTEXT_FLAGS_ARB = $2094;
  //GLX_CONTEXT_DEBUG_BIT_ARB                           = $0001;
  //GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB              = $0002;
  GLX_CONTEXT_DEBUG_BIT_ARB = $00000001;
  GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = $00000002;

  // ARB Extension #57 - GL_ARB_uniform_buffer_object
  GL_UNIFORM_BUFFER = $8A11;
  GL_UNIFORM_BUFFER_BINDING = $8A28;
  GL_UNIFORM_BUFFER_START = $8A29;
  GL_UNIFORM_BUFFER_SIZE = $8A2A;
  GL_MAX_VERTEX_UNIFORM_BLOCKS = $8A2B;
  GL_MAX_GEOMETRY_UNIFORM_BLOCKS = $8A2C;
  GL_MAX_FRAGMENT_UNIFORM_BLOCKS = $8A2D;
  GL_MAX_COMBINED_UNIFORM_BLOCKS = $8A2E;
  GL_MAX_UNIFORM_BUFFER_BINDINGS = $8A2F;
  GL_MAX_UNIFORM_BLOCK_SIZE = $8A30;
  GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS = $8A31;
  GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS = $8A32;
  GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS = $8A33;
  GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT = $8A34;
  GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH = $8A35;
  GL_ACTIVE_UNIFORM_BLOCKS = $8A36;
  GL_UNIFORM_TYPE = $8A37;
  GL_UNIFORM_SIZE = $8A38;
  GL_UNIFORM_NAME_LENGTH = $8A39;
  GL_UNIFORM_BLOCK_INDEX = $8A3A;
  GL_UNIFORM_OFFSET = $8A3B;
  GL_UNIFORM_ARRAY_STRIDE = $8A3C;
  GL_UNIFORM_MATRIX_STRIDE = $8A3D;
  GL_UNIFORM_IS_ROW_MAJOR = $8A3E;
  GL_UNIFORM_BLOCK_BINDING = $8A3F;
  GL_UNIFORM_BLOCK_DATA_SIZE = $8A40;
  GL_UNIFORM_BLOCK_NAME_LENGTH = $8A41;
  GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS = $8A42;
  GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES = $8A43;
  GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER = $8A44;
  GL_UNIFORM_BLOCK_REFERENCED_BY_GEOMETRY_SHADER = $8A45;
  GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER = $8A46;
  GL_INVALID_INDEX = $FFFFFFFF;

  // ARB Extension #58 - GL_ARB_compatibility
  // (no new tokens)

  // ARB Extension #59 - GL_ARB_copy_buffer
  GL_COPY_READ_BUFFER = $8F36;
  GL_COPY_WRITE_BUFFER = $8F37;

  // ARB Extension #60 - GL_ARB_shader_texture_lod
  // (no new tokens)

  // ARB Extension #61 - GL_ARB_depth_clamp
  GL_DEPTH_CLAMP = $864F;

  // ARB Extension #62 - GL_ARB_draw_elements_base_vertex
  // (no new tokens)

  // ARB Extension #63 - GL_ARB_fragment_coord_conventions
  // (no new tokens)

  // ARB Extension #64 - GL_ARB_provoking_vertex
  GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION = $8E4C;
  GL_FIRST_VERTEX_CONVENTION = $8E4D;
  GL_LAST_VERTEX_CONVENTION = $8E4E;
  GL_PROVOKING_VERTEX = $8E4F;

  // ARB Extension #65 - GL_ARB_seamless_cube_map
  GL_TEXTURE_CUBE_MAP_SEAMLESS = $884F;

  // ARB Extension #66 - GL_ARB_sync
  GL_MAX_SERVER_WAIT_TIMEOUT = $9111;
  GL_OBJECT_TYPE = $9112;
  GL_SYNC_CONDITION = $9113;
  GL_SYNC_STATUS = $9114;
  GL_SYNC_FLAGS = $9115;
  GL_SYNC_FENCE = $9116;
  GL_SYNC_GPU_COMMANDS_COMPLETE = $9117;
  GL_UNSIGNALED = $9118;
  GL_SIGNALED = $9119;
  GL_ALREADY_SIGNALED = $911A;
  GL_TIMEOUT_EXPIRED = $911B;
  GL_CONDITION_SATISFIED = $911C;
  GL_WAIT_FAILED = $911D;
  GL_SYNC_FLUSH_COMMANDS_BIT = $00000001;
  GL_TIMEOUT_IGNORED = $FFFFFFFFFFFFFFFF;

  // ARB Extension #67 - GL_ARB_texture_multisample
  GL_SAMPLE_POSITION = $8E50;
  GL_SAMPLE_MASK = $8E51;
  GL_SAMPLE_MASK_VALUE = $8E52;
  GL_MAX_SAMPLE_MASK_WORDS = $8E59;
  GL_TEXTURE_2D_MULTISAMPLE = $9100;
  GL_PROXY_TEXTURE_2D_MULTISAMPLE = $9101;
  GL_TEXTURE_2D_MULTISAMPLE_ARRAY = $9102;
  GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY = $9103;
  GL_TEXTURE_BINDING_2D_MULTISAMPLE = $9104;
  GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY = $9105;
  GL_TEXTURE_SAMPLES = $9106;
  GL_TEXTURE_FIXED_SAMPLE_LOCATIONS = $9107;
  GL_SAMPLER_2D_MULTISAMPLE = $9108;
  GL_INT_SAMPLER_2D_MULTISAMPLE = $9109;
  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE = $910A;
  GL_SAMPLER_2D_MULTISAMPLE_ARRAY = $910B;
  GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = $910C;
  GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY = $910D;
  GL_MAX_COLOR_TEXTURE_SAMPLES = $910E;
  GL_MAX_DEPTH_TEXTURE_SAMPLES = $910F;
  GL_MAX_INTEGER_SAMPLES = $9110;

  // ARB Extension #68 - GL_ARB_vertex_array_bgra
  // (no new tokens)

  // ARB Extension #69 - GL_ARB_draw_buffers_blend
  // (no new tokens)

  // ARB Extension #70 - GL_ARB_sample_shading
  GL_SAMPLE_SHADING_ARB = $8C36;
  GL_MIN_SAMPLE_SHADING_VALUE_ARB = $8C37;

  // ARB Extension #71 - GL_ARB_texture_cube_map_array
  GL_TEXTURE_CUBE_MAP_ARRAY_ARB = $9009;
  GL_TEXTURE_BINDING_CUBE_MAP_ARRAY_ARB = $900A;
  GL_PROXY_TEXTURE_CUBE_MAP_ARRAY_ARB = $900B;
  GL_SAMPLER_CUBE_MAP_ARRAY_ARB = $900C;
  GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW_ARB = $900D;
  GL_INT_SAMPLER_CUBE_MAP_ARRAY_ARB = $900E;
  GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY_ARB = $900F;

  // ARB Extension #72 - GL_ARB_texture_gather
  GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET_ARB = $8E5E;
  GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET_ARB = $8E5F;
  GL_MAX_PROGRAM_TEXTURE_GATHER_COMPONENTS_ARB = $8F9F;

  // ARB Extension #73 - GL_ARB_texture_query_lod
  // (no new tokens)

  // ARB Extension #74 - WGL_ARB_create_context_profile
  // see also WGL_ARB_create_context (ARB #55)
  WGL_CONTEXT_PROFILE_MASK_ARB = $9126;
  WGL_CONTEXT_CORE_PROFILE_BIT_ARB = $00000001;
  WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = $00000002;
  ERROR_INVALID_PROFILE_ARB = $2096;

  // ARB Extension #75 - GLX_ARB_create_context_profile
  // see also GLX_ARB_create_context (ARB #56)
  GLX_CONTEXT_PROFILE_MASK_ARB = $9126;
  GLX_CONTEXT_CORE_PROFILE_BIT_ARB = $00000001;
  GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = $00000002;

  // ARB Extension #78 - GL_ARB_blend_func_extended
  GL_SRC1_COLOR = $88F9;
  //use GL_SRC1_ALPHA
  GL_ONE_MINUS_SRC1_COLOR = $88FA;
  GL_ONE_MINUS_SRC1_ALPHA = $88FB;
  GL_MAX_DUAL_SOURCE_DRAW_BUFFERS = $88FC;

  // ARB Extension #79 - GL_ARB_explicit_attrib_location
  // (no new tokens)

  // ARB Extension #80 - GL_ARB_occlusion_query2
  GL_ANY_SAMPLES_PASSED = $8C2F;

  // ARB Extension #81 - GL_ARB_sampler_objects
  GL_SAMPLER_BINDING = $8919;

  // ARB Extension #82 - GL_ARB_shader_bit_encoding
  // (no new tokens)

  // ARB Extension #83 - GL_ARB_texture_rgb10_a2ui
  GL_RGB10_A2UI = $906F;

  // ARB Extension #84 - GL_ARB_texture_swizzle
  GL_TEXTURE_SWIZZLE_R = $8E42;
  GL_TEXTURE_SWIZZLE_G = $8E43;
  GL_TEXTURE_SWIZZLE_B = $8E44;
  GL_TEXTURE_SWIZZLE_A = $8E45;
  GL_TEXTURE_SWIZZLE_RGBA = $8E46;

  // ARB Extension #85 - GL_ARB_timer_query
  GL_TIME_ELAPSED = $88BF;
  GL_TIMESTAMP = $8E28;

  // ARB Extension #86 - GL_ARB_vertex_type_2_10_10_10_rev
  // reuse GL_UNSIGNED_INT_2_10_10_10_REV
  GL_INT_2_10_10_10_REV = $8D9F;

  // ARB Extension #87 - GL_ARB_draw_indirect
  GL_DRAW_INDIRECT_BUFFER = $8F3F;
  GL_DRAW_INDIRECT_BUFFER_BINDING = $8F43;

  // ARB Extension #88 - GL_ARB_gpu_shader5
  GL_GEOMETRY_SHADER_INVOCATIONS = $887F;
  GL_MAX_GEOMETRY_SHADER_INVOCATIONS = $8E5A;
  GL_MIN_FRAGMENT_INTERPOLATION_OFFSET = $8E5B;
  GL_MAX_FRAGMENT_INTERPOLATION_OFFSET = $8E5C;
  GL_FRAGMENT_INTERPOLATION_OFFSET_BITS = $8E5D;
  GL_MAX_VERTEX_STREAMS = $8E71;

  // ARB Extension #89 - GL_ARB_gpu_shader_fp64
  // reuse GL_DOUBLE
  GL_DOUBLE_VEC2 = $8FFC;
  GL_DOUBLE_VEC3 = $8FFD;
  GL_DOUBLE_VEC4 = $8FFE;
  GL_DOUBLE_MAT2 = $8F46;
  GL_DOUBLE_MAT3 = $8F47;
  GL_DOUBLE_MAT4 = $8F48;
  GL_DOUBLE_MAT2x3 = $8F49;
  GL_DOUBLE_MAT2x4 = $8F4A;
  GL_DOUBLE_MAT3x2 = $8F4B;
  GL_DOUBLE_MAT3x4 = $8F4C;
  GL_DOUBLE_MAT4x2 = $8F4D;
  GL_DOUBLE_MAT4x3 = $8F4E;

  // ARB Extension #90 - GL_ARB_shader_subroutine
  GL_ACTIVE_SUBROUTINES = $8DE5;
  GL_ACTIVE_SUBROUTINE_UNIFORMS = $8DE6;
  GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS = $8E47;
  GL_ACTIVE_SUBROUTINE_MAX_LENGTH = $8E48;
  GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH = $8E49;
  GL_MAX_SUBROUTINES = $8DE7;
  GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS = $8DE8;
  GL_NUM_COMPATIBLE_SUBROUTINES = $8E4A;
  GL_COMPATIBLE_SUBROUTINES = $8E4B;
  // reuse GL_UNIFORM_SIZE
  // reuse GL_UNIFORM_NAME_LENGTH

  // ARB Extension #91 - GL_ARB_tessellation_shader
  GL_PATCHES = $000E;
  GL_PATCH_VERTICES = $8E72;
  GL_PATCH_DEFAULT_INNER_LEVEL = $8E73;
  GL_PATCH_DEFAULT_OUTER_LEVEL = $8E74;
  GL_TESS_CONTROL_OUTPUT_VERTICES = $8E75;
  GL_TESS_GEN_MODE = $8E76;
  GL_TESS_GEN_SPACING = $8E77;
  GL_TESS_GEN_VERTEX_ORDER = $8E78;
  GL_TESS_GEN_POINT_MODE = $8E79;
  // reuse GL_TRIANGLES
  // reuse GL_QUADS
  GL_ISOLINES = $8E7A;
  // reuse GL_EQUAL
  GL_FRACTIONAL_ODD = $8E7B;
  GL_FRACTIONAL_EVEN = $8E7C;
  // reuse GL_CCW
  // reuse GL_CW
  GL_MAX_PATCH_VERTICES = $8E7D;
  GL_MAX_TESS_GEN_LEVEL = $8E7E;
  GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS = $8E7F;
  GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS = $8E80;
  GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS = $8E81;
  GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS = $8E82;
  GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS = $8E83;
  GL_MAX_TESS_PATCH_COMPONENTS = $8E84;
  GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS = $8E85;
  GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS = $8E86;
  GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS = $8E89;
  GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS = $8E8A;
  GL_MAX_TESS_CONTROL_INPUT_COMPONENTS = $886C;
  GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS = $886D;
  GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS = $8E1E;
  GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS = $8E1F;
  GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_CONTROL_SHADER = $84F0;
  GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_EVALUATION_SHADER = $84F1;
  GL_TESS_EVALUATION_SHADER = $8E87;
  GL_TESS_CONTROL_SHADER = $8E88;

  // ARB Extension #92 - GL_ARB_texture_buffer_object_rgb32
  // reuse GL_RGB32F
  // reuse GL_RGB32UI
  // reuse GL_RGB32I

  // ARB Extension #93 - GL_ARB_transform_feedback2
  GL_TRANSFORM_FEEDBACK = $8E22;
  GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED = $8E23;
  GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE = $8E24;
  GL_TRANSFORM_FEEDBACK_BINDING = $8E25;

  // ARB Extension #94 - GL_ARB_transform_feedback3
  GL_MAX_TRANSFORM_FEEDBACK_BUFFERS = $8E70;

  // ARB Extension #95 - GL_ARB_ES2_compatibility
  GL_FIXED = $140C;
  GL_IMPLEMENTATION_COLOR_READ_TYPE = $8B9A;
  GL_IMPLEMENTATION_COLOR_READ_FORMAT = $8B9B;
  GL_LOW_FLOAT = $8DF0;
  GL_MEDIUM_FLOAT = $8DF1;
  GL_HIGH_FLOAT = $8DF2;
  GL_LOW_INT = $8DF3;
  GL_MEDIUM_INT = $8DF4;
  GL_HIGH_INT = $8DF5;
  GL_SHADER_COMPILER = $8DFA;
  GL_NUM_SHADER_BINARY_FORMATS = $8DF9;
  GL_MAX_VERTEX_UNIFORM_VECTORS = $8DFB;
  GL_MAX_VARYING_VECTORS = $8DFC;
  GL_MAX_FRAGMENT_UNIFORM_VECTORS = $8DFD;

  // ARB Extension #96 - GL_ARB_get_program_binary
  GL_PROGRAM_BINARY_RETRIEVABLE_HINT = $8257;
  GL_PROGRAM_BINARY_LENGTH = $8741;
  GL_NUM_PROGRAM_BINARY_FORMATS = $87FE;
  GL_PROGRAM_BINARY_FORMATS = $87FF;

  // ARB Extension #97 - GL_ARB_separate_shader_objects
  GL_VERTEX_SHADER_BIT = $00000001;
  GL_FRAGMENT_SHADER_BIT = $00000002;
  GL_GEOMETRY_SHADER_BIT = $00000004;
  GL_TESS_CONTROL_SHADER_BIT = $00000008;
  GL_TESS_EVALUATION_SHADER_BIT = $00000010;
  GL_ALL_SHADER_BITS = $FFFFFFFF;
  GL_PROGRAM_SEPARABLE = $8258;
  GL_ACTIVE_PROGRAM = $8259;
  GL_PROGRAM_PIPELINE_BINDING = $825A;

  // ARB Extension #98 - GL_ARB_shader_precision (none)

  // ARB Extension #99 - GL_ARB_vertex_attrib_64bit
  // reuses tokens from 3.0 + GL_ARB_gpu_shader_fp64

  // ARB Extension #100 - GL_ARB_viewport_array
  GL_MAX_VIEWPORTS = $825B;
  GL_VIEWPORT_SUBPIXEL_BITS = $825C;
  GL_VIEWPORT_BOUNDS_RANGE = $825D;
  GL_LAYER_PROVOKING_VERTEX = $825E;
  GL_VIEWPORT_INDEX_PROVOKING_VERTEX = $825F;
  GL_UNDEFINED_VERTEX = $8260;

  // ARB Extension #104 - GL_ARB_debug_output
  GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB = $8242;
  GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH_ARB = $8243;
  GL_DEBUG_CALLBACK_FUNCTION_ARB = $8244;
  GL_DEBUG_CALLBACK_USER_PARAM_ARB = $8245;
  GL_DEBUG_SOURCE_API_ARB = $8246;
  GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB = $8247;
  GL_DEBUG_SOURCE_SHADER_COMPILER_ARB = $8248;
  GL_DEBUG_SOURCE_THIRD_PARTY_ARB = $8249;
  GL_DEBUG_SOURCE_APPLICATION_ARB = $824A;
  GL_DEBUG_SOURCE_OTHER_ARB = $824B;
  GL_DEBUG_TYPE_ERROR_ARB = $824C;
  GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB = $824D;
  GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB = $824E;
  GL_DEBUG_TYPE_PORTABILITY_ARB = $824F;
  GL_DEBUG_TYPE_PERFORMANCE_ARB = $8250;
  GL_DEBUG_TYPE_OTHER_ARB = $8251;
  GL_MAX_DEBUG_MESSAGE_LENGTH_ARB = $9143;
  GL_MAX_DEBUG_LOGGED_MESSAGES_ARB = $9144;
  GL_DEBUG_LOGGED_MESSAGES_ARB = $9145;
  GL_DEBUG_SEVERITY_HIGH_ARB = $9146;
  GL_DEBUG_SEVERITY_MEDIUM_ARB = $9147;
  GL_DEBUG_SEVERITY_LOW_ARB = $9148;

  // ARB Extension #105 - GL_ARB_robustness
  GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB = $00000004;
  GL_LOSE_CONTEXT_ON_RESET_ARB = $8252;
  GL_GUILTY_CONTEXT_RESET_ARB = $8253;
  GL_INNOCENT_CONTEXT_RESET_ARB = $8254;
  GL_UNKNOWN_CONTEXT_RESET_ARB = $8255;
  GL_RESET_NOTIFICATION_STRATEGY_ARB = $8256;
  GL_NO_RESET_NOTIFICATION_ARB = $8261;

  // ARB Extension #106 - GL_ARB_shader_stencil_export
  // (no new tokens)

{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'Vendor/EXT extensions constants, in extension number order'}{$ENDIF}

  // ----- extensions enumerants -----

  // EXT_texture_rectangle (can't find this extension in OpenGL registry)

  GL_TEXTURE_RECTANGLE_EXT = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE_EXT = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE_EXT = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE_EXT = $84F8;

  // ARB Extension #20 - GLX_ARB_render_texture
  GLX_BIND_TO_TEXTURE_RGB_EXT = $20D0;
  GLX_BIND_TO_TEXTURE_RGBA_EXT = $20D1;
  GLX_BIND_TO_MIPMAP_TEXTURE_EXT = $20D2;
  GLX_BIND_TO_TEXTURE_TARGETS_EXT = $20D3;
  GLX_Y_INVERTED_EXT = $20D4;
  GLX_TEXTURE_FORMAT_EXT = $20D5;
  GLX_TEXTURE_TARGET_EXT = $20D6;
  GLX_MIPMAP_TEXTURE_EXT = $20D7;
  GLX_TEXTURE_FORMAT_NONE_EXT = $20D8;
  GLX_TEXTURE_FORMAT_RGB_EXT = $20D9;
  GLX_TEXTURE_FORMAT_RGBA_EXT = $20DA;
  GLX_TEXTURE_1D_EXT = $20DB;
  GLX_TEXTURE_2D_EXT = $20DC;
  GLX_TEXTURE_RECTANGLE_EXT = $20DD;
  GLX_FRONT_LEFT_EXT = $20DE;
  GLX_FRONT_RIGHT_EXT = $20DF;
  GLX_BACK_LEFT_EXT = $20E0;
  GLX_BACK_RIGHT_EXT = $20E1;
  GLX_FRONT_EXT = GLX_FRONT_LEFT_EXT;
  GLX_BACK_EXT = GLX_BACK_LEFT_EXT;
  GLX_AUX0_EXT = $20E2;
  GLX_AUX1_EXT = $20E3;
  GLX_AUX2_EXT = $20E4;
  GLX_AUX3_EXT = 420E5;
  GLX_AUX4_EXT = $20E6;
  GLX_AUX5_EXT = $20E7;
  GLX_AUX6_EXT = $20E8;
  GLX_AUX7_EXT = $20E9;
  GLX_AUX8_EXT = $20EA;
  GLX_AUX9_EXT = $20EB;

  // EXT_abgr (#1)
  GL_ABGR_EXT = $8000;

  // EXT_blend_color (#2)
  GL_CONSTANT_COLOR_EXT = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR_EXT = $8002;
  GL_CONSTANT_ALPHA_EXT = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA_EXT = $8004;
  GL_BLEND_COLOR_EXT = $8005;

  // EXT_polygon_offset (#3)
  GL_POLYGON_OFFSET_EXT = $8037;
  GL_POLYGON_OFFSET_FACTOR_EXT = $8038;
  GL_POLYGON_OFFSET_BIAS_EXT = $8039;

  // EXT_texture (#4)
  GL_ALPHA4_EXT = $803B;
  GL_ALPHA8_EXT = $803C;
  GL_ALPHA12_EXT = $803D;
  GL_ALPHA16_EXT = $803E;
  GL_LUMINANCE4_EXT = $803F;
  GL_LUMINANCE8_EXT = $8040;
  GL_LUMINANCE12_EXT = $8041;
  GL_LUMINANCE16_EXT = $8042;
  GL_LUMINANCE4_ALPHA4_EXT = $8043;
  GL_LUMINANCE6_ALPHA2_EXT = $8044;
  GL_LUMINANCE8_ALPHA8_EXT = $8045;
  GL_LUMINANCE12_ALPHA4_EXT = $8046;
  GL_LUMINANCE12_ALPHA12_EXT = $8047;
  GL_LUMINANCE16_ALPHA16_EXT = $8048;
  GL_INTENSITY_EXT = $8049;
  GL_INTENSITY4_EXT = $804A;
  GL_INTENSITY8_EXT = $804B;
  GL_INTENSITY12_EXT = $804C;
  GL_INTENSITY16_EXT = $804D;
  GL_RGB2_EXT = $804E;
  GL_RGB4_EXT = $804F;
  GL_RGB5_EXT = $8050;
  GL_RGB8_EXT = $8051;
  GL_RGB10_EXT = $8052;
  GL_RGB12_EXT = $8053;
  GL_RGB16_EXT = $8054;
  GL_RGBA2_EXT = $8055;
  GL_RGBA4_EXT = $8056;
  GL_RGB5_A1_EXT = $8057;
  GL_RGBA8_EXT = $8058;
  GL_RGB10_A2_EXT = $8059;
  GL_RGBA12_EXT = $805A;
  GL_RGBA16_EXT = $805B;
  GL_TEXTURE_RED_SIZE_EXT = $805C;
  GL_TEXTURE_GREEN_SIZE_EXT = $805D;
  GL_TEXTURE_BLUE_SIZE_EXT = $805E;
  GL_TEXTURE_ALPHA_SIZE_EXT = $805F;
  GL_TEXTURE_LUMINANCE_SIZE_EXT = $8060;
  GL_TEXTURE_INTENSITY_SIZE_EXT = $8061;
  GL_REPLACE_EXT = $8062;
  GL_PROXY_TEXTURE_1D_EXT = $8063;
  GL_PROXY_TEXTURE_2D_EXT = $8064;
  GL_TEXTURE_TOO_LARGE_EXT = $8065;

  GL_RGB_S3TC = $83A0;
  GL_RGB4_S3TC = $83A1;
  GL_RGBA_S3TC = $83A2;
  GL_RGBA4_S3TC = $83A3;
  GL_RGBA_DXT5_S3TC = $83A4;
  GL_RGBA4_DXT5_S3TC = $83A5;

  // EXT_texture3D (#6)
  GL_PACK_SKIP_IMAGES_EXT = $806B;
  GL_PACK_IMAGE_HEIGHT_EXT = $806C;
  GL_UNPACK_SKIP_IMAGES_EXT = $806D;
  GL_UNPACK_IMAGE_HEIGHT_EXT = $806E;
  GL_TEXTURE_3D_EXT = $806F;
  GL_PROXY_TEXTURE_3D_EXT = $8070;
  GL_TEXTURE_DEPTH_EXT = $8071;
  GL_TEXTURE_WRAP_R_EXT = $8072;
  GL_MAX_3D_TEXTURE_SIZE_EXT = $8073;

  // EXT_histogram (#11)
  GL_HISTOGRAM_EXT = $8024;
  GL_PROXY_HISTOGRAM_EXT = $8025;
  GL_HISTOGRAM_WIDTH_EXT = $8026;
  GL_HISTOGRAM_FORMAT_EXT = $8027;
  GL_HISTOGRAM_RED_SIZE_EXT = $8028;
  GL_HISTOGRAM_GREEN_SIZE_EXT = $8029;
  GL_HISTOGRAM_BLUE_SIZE_EXT = $802A;
  GL_HISTOGRAM_ALPHA_SIZE_EXT = $802B;
  GL_HISTOGRAM_LUMINANCE_SIZE_EXT = $802C;
  GL_HISTOGRAM_SINK_EXT = $802D;
  GL_MINMAX_EXT = $802E;
  GL_MINMAX_FORMAT_EXT = $802F;
  GL_MINMAX_SINK_EXT = $8030;

  // EXT_convolution (#12)
  GL_CONVOLUTION_1D_EXT = $8010;
  GL_CONVOLUTION_2D_EXT = $8011;
  GL_SEPARABLE_2D_EXT = $8012;
  GL_CONVOLUTION_BORDER_MODE_EXT = $8013;
  GL_CONVOLUTION_FILTER_SCALE_EXT = $8014;
  GL_CONVOLUTION_FILTER_BIAS_EXT = $8015;
  GL_REDUCE_EXT = $8016;
  GL_CONVOLUTION_FORMAT_EXT = $8017;
  GL_CONVOLUTION_WIDTH_EXT = $8018;
  GL_CONVOLUTION_HEIGHT_EXT = $8019;
  GL_MAX_CONVOLUTION_WIDTH_EXT = $801A;
  GL_MAX_CONVOLUTION_HEIGHT_EXT = $801B;
  GL_POST_CONVOLUTION_RED_SCALE_EXT = $801C;
  GL_POST_CONVOLUTION_GREEN_SCALE_EXT = $801D;
  GL_POST_CONVOLUTION_BLUE_SCALE_EXT = $801E;
  GL_POST_CONVOLUTION_ALPHA_SCALE_EXT = $801F;
  GL_POST_CONVOLUTION_RED_BIAS_EXT = $8020;
  GL_POST_CONVOLUTION_GREEN_BIAS_EXT = $8021;
  GL_POST_CONVOLUTION_BLUE_BIAS_EXT = $8022;
  GL_POST_CONVOLUTION_ALPHA_BIAS_EXT = $8023;

  // SGI_color_matrix (#13)
  GL_COLOR_MATRIX_SGI = $80B1;
  GL_COLOR_MATRIX_STACK_DEPTH_SGI = $80B2;
  GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI = $80B3;
  GL_POST_COLOR_MATRIX_RED_SCALE_SGI = $80B4;
  GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI = $80B5;
  GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI = $80B6;
  GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI = $80B7;
  GL_POST_COLOR_MATRIX_RED_BIAS_SGI = $80B8;
  GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI = $80B9;
  GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI = $80BA;
  GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI = $80BB;

  // EXT_texture_object (#20)
  GL_TEXTURE_PRIORITY_EXT = $8066;
  GL_TEXTURE_RESIDENT_EXT = $8067;
  GL_TEXTURE_1D_BINDING_EXT = $8068;
  GL_TEXTURE_2D_BINDING_EXT = $8069;
  GL_TEXTURE_3D_BINDING_EXT = $806A;

  // EXT_packed_pixels (#23)
  GL_UNSIGNED_BYTE_3_3_2_EXT = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4_EXT = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1_EXT = $8034;
  GL_UNSIGNED_INT_8_8_8_8_EXT = $8035;
  GL_UNSIGNED_INT_10_10_10_2_EXT = $8036;

  // GL_SGIS_texture_lod (#24)
  GL_TEXTURE_MIN_LOD_SGIS = $813A;
  GL_TEXTURE_MAX_LOD_SGIS = $813B;
  GL_TEXTURE_BASE_LEVEL_SGIS = $813C;
  GL_TEXTURE_MAX_LEVEL_SGIS = $813D;

  // GL_SGIS_multisample (#25)
  GL_MULTISAMPLE_SGIS = $809D;
  GL_SAMPLE_ALPHA_TO_MASK_SGIS = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_SGIS = $809F;
  GL_SAMPLE_MASK_SGIS = $80A0;
  GL_1PASS_SGIS = $80A1;
  GL_2PASS_0_SGIS = $80A2;
  GL_2PASS_1_SGIS = $80A3;
  GL_4PASS_0_SGIS = $80A4;
  GL_4PASS_1_SGIS = $80A5;
  GL_4PASS_2_SGIS = $80A6;
  GL_4PASS_3_SGIS = $80A7;
  GL_SAMPLE_BUFFERS_SGIS = $80A8;
  GL_SAMPLES_SGIS = $80A9;
  GL_SAMPLE_MASK_VALUE_SGIS = $80AA;
  GL_SAMPLE_MASK_INVERT_SGIS = $80AB;
  GL_SAMPLE_PATTERN_SGIS = $80AC;

  // GL_EXT_rescale_normal (#27)
  GL_RESCALE_NORMAL_EXT = $803A;

  // GL_SGIS_generate_mipmap (#32)
  GL_GENERATE_MIPMAP_SGIS = $8191;
  GL_GENERATE_MIPMAP_HINT_SGIS = $8192;

  // GL_SGIX_shadow (#34)
  GL_TEXTURE_COMPARE_SGIX = $819A;
  GL_TEXTURE_COMPARE_OPERATOR_SGIX = $819B;
  GL_TEXTURE_LEQUAL_R_SGIX = $819C;
  GL_TEXTURE_GEQUAL_R_SGIX = $819D;

  // GL_SGIS_texture_edge_clamp (#35)
  GL_CLAMP_TO_EDGE_SGIS = $812F;

  // GL_SGIS_texture_border_clamp (#36)
  GL_CLAMP_TO_BORDER_SGIS = $812D;

  // EXT_blend_minmax (#37)
  GL_FUNC_ADD_EXT = $8006;
  GL_MIN_EXT = $8007;
  GL_MAX_EXT = $8008;
  GL_BLEND_EQUATION_EXT = $8009;

  // EXT_blend_subtract (#38)
  GL_FUNC_SUBTRACT_EXT = $800A;
  GL_FUNC_REVERSE_SUBTRACT_EXT = $800B;

  // GL_EXT_object_space_tess (#75)
  GLU_OBJECT_PARAMETRIC_ERROR_EXT = 100208;
  GLU_OBJECT_PATH_LENGTH_EXT = 100209;

  // GL_EXT_paletted_texture (#78)
  GL_COLOR_INDEX1_EXT = $80E2;
  GL_COLOR_INDEX2_EXT = $80E3;
  GL_COLOR_INDEX4_EXT = $80E4;
  GL_COLOR_INDEX8_EXT = $80E5;
  GL_COLOR_INDEX12_EXT = $80E6;
  GL_COLOR_INDEX16_EXT = $80E7;

  // GL_EXT_paletted_texture (#78)
  GL_TEXTURE_INDEX_SIZE_EXT = $80ED;

  // GL_EXT_clip_volume_hint (#79)
  GL_CLIP_VOLUME_CLIPPING_HINT_EXT = $80F0;

  // GL_SGIX_shadow_ambient (#90)
  GL_SHADOW_AMBIENT_SGIX = $80BF;

  // EXT_compiled_vertex_array (#97)
  GL_ARRAY_ELEMENT_LOCK_FIRST_EXT = $81A8;
  GL_ARRAY_ELEMENT_LOCK_COUNT_EXT = $81A9;

  // EXT_nurbs_tessellator (#100)
  GLU_NURBS_MODE_EXT = 100160;
  GLU_NURBS_TESSELLATOR_EXT = 100161;
  GLU_NURBS_RENDERER_EXT = 100162;
  GLU_NURBS_BEGIN_EXT = 100164;
  GLU_NURBS_VERTEX_EXT = 100165;
  GLU_NURBS_NORMAL_EXT = 100166;
  GLU_NURBS_COLOR_EXT = 100167;
  GLU_NURBS_TEX_COORD_EXT = 100168;
  GLU_NURBS_END_EXT = 100169;
  GLU_NURBS_BEGIN_DATA_EXT = 100170;
  GLU_NURBS_VERTEX_DATA_EXT = 100171;
  GLU_NURBS_NORMAL_DATA_EXT = 100172;
  GLU_NURBS_COLOR_DATA_EXT = 100173;
  GLU_NURBS_TEX_COORD_DATA_EXT = 100174;
  GLU_NURBS_END_DATA_EXT = 100175;

  // GL_IBM_rasterpos_clip (#110)
  GL_RASTER_POSITION_UNCLIPPED_IBM = $19262;

  // GL_EXT_draw_range_elements (#112)
  GL_MAX_ELEMENTS_VERTICES_EXT = $80E8;
  GL_MAX_ELEMENTS_INDICES_EXT = $80E9;

  // EXT_bgra (#129)
  GL_BGR_EXT = $80E0;
  GL_BGRA_EXT = $80E1;

  // GL_HP_occlusion_test (#137)
  GL_OCCLUSION_TEST_HP = $8165;
  GL_OCCLUSION_TEST_RESULT_HP = $8166;

  // GL_EXT_shared_texture_palette (#141)
  GL_SHARED_TEXTURE_PALETTE_EXT = $81FB;

  // GL_EXT_separate_specular_color (#144)
  GL_LIGHT_MODEL_COLOR_CONTROL_EXT = $81F8;
  GL_SINGLE_COLOR_EXT = $81F9;
  GL_SEPARATE_SPECULAR_COLOR_EXT = $81FA;

  // GL_EXT_secondary_color (#145)
  GL_COLOR_SUM_EXT = $8458;
  GL_CURRENT_SECONDARY_COLOR_EXT = $8459;
  GL_SECONDARY_COLOR_ARRAY_SIZE_EXT = $845A;
  GL_SECONDARY_COLOR_ARRAY_TYPE_EXT = $845B;
  GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT = $845C;
  GL_SECONDARY_COLOR_ARRAY_POINTER_EXT = $845D;
  GL_SECONDARY_COLOR_ARRAY_EXT = $845E;

  // GL_EXT_fog_coord (#149)
  GL_FOG_COORDINATE_SOURCE_EXT = $8450;
  GL_FOG_COORDINATE_EXT = $8451;
  GL_FRAGMENT_DEPTH_EXT = $8452;
  GL_CURRENT_FOG_COORDINATE_EXT = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE_EXT = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE_EXT = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER_EXT = $8456;
  GL_FOG_COORDINATE_ARRAY_EXT = $8457;

  // GL_EXT_texture_env_combine (#158)
  GL_COMBINE_EXT = $8570;
  GL_COMBINE_RGB_EXT = $8571;
  GL_COMBINE_ALPHA_EXT = $8572;
  GL_RGB_SCALE_EXT = $8573;
  GL_ADD_SIGNED_EXT = $8574;
  GL_INTERPOLATE_EXT = $8575;
  GL_CONSTANT_EXT = $8576;
  GL_PRIMARY_COLOR_EXT = $8577;
  GL_PREVIOUS_EXT = $8578;
  GL_SOURCE0_RGB_EXT = $8580;
  GL_SOURCE1_RGB_EXT = $8581;
  GL_SOURCE2_RGB_EXT = $8582;
  GL_SOURCE0_ALPHA_EXT = $8588;
  GL_SOURCE1_ALPHA_EXT = $8589;
  GL_SOURCE2_ALPHA_EXT = $858A;
  GL_OPERAND0_RGB_EXT = $8590;
  GL_OPERAND1_RGB_EXT = $8591;
  GL_OPERAND2_RGB_EXT = $8592;
  GL_OPERAND0_ALPHA_EXT = $8598;
  GL_OPERAND1_ALPHA_EXT = $8599;
  GL_OPERAND2_ALPHA_EXT = $859A;

  // GL_EXT_texture_env_combine (#158)
  GL_SOURCE3_RGB_EXT = $8583;
  GL_SOURCE4_RGB_EXT = $8584;
  GL_SOURCE5_RGB_EXT = $8585;
  GL_SOURCE6_RGB_EXT = $8586;
  GL_SOURCE7_RGB_EXT = $8587;
  GL_SOURCE3_ALPHA_EXT = $858B;
  GL_SOURCE4_ALPHA_EXT = $858C;
  GL_SOURCE5_ALPHA_EXT = $858D;
  GL_SOURCE6_ALPHA_EXT = $858E;
  GL_SOURCE7_ALPHA_EXT = $858F;
  GL_OPERAND3_RGB_EXT = $8593;
  GL_OPERAND4_RGB_EXT = $8594;
  GL_OPERAND5_RGB_EXT = $8595;
  GL_OPERAND6_RGB_EXT = $8596;
  GL_OPERAND7_RGB_EXT = $8597;
  GL_OPERAND3_ALPHA_EXT = $859B;
  GL_OPERAND4_ALPHA_EXT = $859C;
  GL_OPERAND5_ALPHA_EXT = $859D;
  GL_OPERAND6_ALPHA_EXT = $859E;
  GL_OPERAND7_ALPHA_EXT = $859F;

  // GL_EXT_blend_func_separate (#173)
  GL_BLEND_DST_RGB_EXT = $80C8;
  GL_BLEND_SRC_RGB_EXT = $80C9;
  GL_BLEND_DST_ALPHA_EXT = $80CA;
  GL_BLEND_SRC_ALPHA_EXT = $80CB;

  // DanB : "GL_EXT_texture_cube_map (can't find this extension in OpenGL registry so removed)"
  // Mrqzzz : The following block was commented by DanB
  // But the constants are currently used in dws2openGL1x.pas, so i re-add them. If they
  // result harmful, we will remove them again.
  GL_NORMAL_MAP_EXT = $8511;
  GL_REFLECTION_MAP_EXT = $8512;
  GL_TEXTURE_CUBE_MAP_EXT = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP_EXT = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP_EXT = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT = $851C;

  // GL_EXT_stencil_wrap (#176)
  GL_INCR_WRAP_EXT = $8507;
  GL_DECR_WRAP_EXT = $8508;

  // GL_NV_texgen_reflection (#179)
  GL_NORMAL_MAP_NV = $8511;
  GL_REFLECTION_MAP_NV = $8512;

  // GL_EXT_texture_lod_bias (#186)
  GL_MAX_TEXTURE_LOD_BIAS_EXT = $84FD;
  GL_TEXTURE_FILTER_CONTROL_EXT = $8500;
  GL_TEXTURE_LOD_BIAS_EXT = $8501;

  // GL_EXT_texture_filter_anisotropic (#187)
  GL_TEXTURE_MAX_ANISOTROPY_EXT = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;

  // GL_NV_light_max_exponent (#189)
  GL_MAX_SHININESS_NV = $8504;
  GL_MAX_SPOT_EXPONENT_NV = $8505;

  // GL_NV_vertex_array_range (#190)
  GL_VERTEX_ARRAY_RANGE_NV = $851D;
  GL_VERTEX_ARRAY_RANGE_LENGTH_NV = $851E;
  GL_VERTEX_ARRAY_RANGE_VALID_NV = $851F;
  GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV = $8520;
  GL_VERTEX_ARRAY_RANGE_POINTER_NV = $8521;

  // GL_NV_register_combiners (#191)
  GL_REGISTER_COMBINERS_NV = $8522;
  GL_VARIABLE_A_NV = $8523;
  GL_VARIABLE_B_NV = $8524;
  GL_VARIABLE_C_NV = $8525;
  GL_VARIABLE_D_NV = $8526;
  GL_VARIABLE_E_NV = $8527;
  GL_VARIABLE_F_NV = $8528;
  GL_VARIABLE_G_NV = $8529;
  GL_CONSTANT_COLOR0_NV = $852A;
  GL_CONSTANT_COLOR1_NV = $852B;
  GL_PRIMARY_COLOR_NV = $852C;
  GL_SECONDARY_COLOR_NV = $852D;
  GL_SPARE0_NV = $852E;
  GL_SPARE1_NV = $852F;
  GL_DISCARD_NV = $8530;
  GL_E_TIMES_F_NV = $8531;
  GL_SPARE0_PLUS_SECONDARY_COLOR_NV = $8532;
  GL_UNSIGNED_IDENTITY_NV = $8536;
  GL_UNSIGNED_INVERT_NV = $8537;
  GL_EXPAND_NORMAL_NV = $8538;
  GL_EXPAND_NEGATE_NV = $8539;
  GL_HALF_BIAS_NORMAL_NV = $853A;
  GL_HALF_BIAS_NEGATE_NV = $853B;
  GL_SIGNED_IDENTITY_NV = $853C;
  GL_SIGNED_NEGATE_NV = $853D;
  GL_SCALE_BY_TWO_NV = $853E;
  GL_SCALE_BY_FOUR_NV = $853F;
  GL_SCALE_BY_ONE_HALF_NV = $8540;
  GL_BIAS_BY_NEGATIVE_ONE_HALF_NV = $8541;
  GL_COMBINER_INPUT_NV = $8542;
  GL_COMBINER_MAPPING_NV = $8543;
  GL_COMBINER_COMPONENT_USAGE_NV = $8544;
  GL_COMBINER_AB_DOT_PRODUCT_NV = $8545;
  GL_COMBINER_CD_DOT_PRODUCT_NV = $8546;
  GL_COMBINER_MUX_SUM_NV = $8547;
  GL_COMBINER_SCALE_NV = $8548;
  GL_COMBINER_BIAS_NV = $8549;
  GL_COMBINER_AB_OUTPUT_NV = $854A;
  GL_COMBINER_CD_OUTPUT_NV = $854B;
  GL_COMBINER_SUM_OUTPUT_NV = $854C;
  GL_MAX_GENERAL_COMBINERS_NV = $854D;
  GL_NUM_GENERAL_COMBINERS_NV = $854E;
  GL_COLOR_SUM_CLAMP_NV = $854F;
  GL_COMBINER0_NV = $8550;
  GL_COMBINER1_NV = $8551;
  GL_COMBINER2_NV = $8552;
  GL_COMBINER3_NV = $8553;
  GL_COMBINER4_NV = $8554;
  GL_COMBINER5_NV = $8555;
  GL_COMBINER6_NV = $8556;
  GL_COMBINER7_NV = $8557;

  //NV_video_out
  GLX_VIDEO_OUT_COLOR_NV = $20C3;
  GLX_VIDEO_OUT_ALPHA_NV = $20C4;
  GLX_VIDEO_OUT_DEPTH_NV = $20C5;
  GLX_VIDEO_OUT_COLOR_AND_ALPHA_NV = $20C6;
  GLX_VIDEO_OUT_COLOR_AND_DEPTH_NV = $20C7;
  GLX_VIDEO_OUT_FRAME_NV = $20C8;
  GLX_VIDEO_OUT_FIELD_1_NV = $20C9;
  GLX_VIDEO_OUT_FIELD_2_NV = $20CA;
  GLX_VIDEO_OUT_STACKED_FIELDS_1_2_NV = $20CB;
  GLX_VIDEO_OUT_STACKED_FIELDS_2_1_NV = $20CC;

  //NV_present_video enum:
  GLX_NUM_VIDEO_SLOTS_NV = $20F0;

  //EXT_swap_control enum:
  GLX_SWAP_INTERVAL_EXT = $20F1;
  GLX_MAX_SWAP_INTERVAL_EXT = $20F2;

  //NV_video_capture
  GLX_DEVICE_ID_NV = $20CD;
  GLX_UNIQUE_ID_NV = $20CE;
  GLX_NUM_VIDEO_CAPTURE_SLOTS_NV = $20CF;

  // GL_NV_fog_distance (#192)
  GL_FOG_DISTANCE_MODE_NV = $855A;
  GL_EYE_RADIAL_NV = $855B;
  GL_EYE_PLANE_ABSOLUTE_NV = $855C;

  // GL_NV_texture_env_combine4 (#195)
  GL_COMBINE4_NV = $8503;
  GL_SOURCE3_RGB_NV = $8583;
  GL_SOURCE3_ALPHA_NV = $858B;
  GL_OPERAND3_RGB_NV = $8593;
  GL_OPERAND3_ALPHA_NV = $859B;

  // GL_EXT_texture_compression_s3tc (#198)
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = $83F3;

  // GL_3DFX_texture_compression_FXT1 (#206)
  GL_COMPRESSED_RGB_FXT1_3DFX = $86B0;
  GL_COMPRESSED_RGBA_FXT1_3DFX = $86B1;

  // GL_3DFX_multisample (#207)
  GL_MULTISAMPLE_3DFX = $86B2;
  GL_SAMPLE_BUFFERS_3DFX = $86B3;
  GL_SAMPLES_3DFX = $86B4;
  GL_MULTISAMPLE_BIT_3DFX = $20000000;

  // GL_EXT_multisample / WGL_EXT_multisample (#209)
  GL_MULTISAMPLE_EXT = $809D;
  GL_SAMPLE_ALPHA_TO_MASK_EXT = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_EXT = $809F;
  GL_SAMPLE_MASK_EXT = $80A0;
  GL_1PASS_EXT = $80A1;
  GL_2PASS_0_EXT = $80A2;
  GL_2PASS_1_EXT = $80A3;
  GL_4PASS_0_EXT = $80A4;
  GL_4PASS_1_EXT = $80A5;
  GL_4PASS_2_EXT = $80A6;
  GL_4PASS_3_EXT = $80A7;
  GL_SAMPLE_BUFFERS_EXT = $80A8;
  GL_SAMPLES_EXT = $80A9;
  GL_SAMPLE_MASK_VALUE_EXT = $80AA;
  GL_SAMPLE_MASK_INVERT_EXT = $80AB;
  GL_SAMPLE_PATTERN_EXT = $80AC;
  WGL_SAMPLE_BUFFERS_EXT = $2041;
  WGL_SAMPLES_EXT = $2042;

  // GL_SGIS_texture_color_mask (#214)
  GL_TEXTURE_COLOR_WRITEMASK_SGIS = $81EF;

  // GL_EXT_texture_env_dot3 (#220)
  GL_DOT3_RGB_EXT = $8740;
  GL_DOT3_RGBA_EXT = $8741;

  // GL_ATI_texture_mirror_once (#221)
  GL_MIRROR_CLAMP_ATI = $8742;
  GL_MIRROR_CLAMP_TO_EDGE_ATI = $8743;

  // GL_NV_fence (#222)
  GL_ALL_COMPLETED_NV = $84F2;
  GL_FENCE_STATUS_NV = $84F3;
  GL_FENCE_CONDITION_NV = $84F4;

  // GL_NV_texture_rectangle (#229)
  GL_TEXTURE_RECTANGLE_NV = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE_NV = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE_NV = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE_NV = $84F8;

  // GL_NV_texture_shader (#230)
  GL_OFFSET_TEXTURE_RECTANGLE_NV = $864C;
  GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV = $864D;
  GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV = $864E;
  GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV = $86D9;
  GL_UNSIGNED_INT_S8_S8_8_8_NV = $86DA;
  GL_UNSIGNED_INT_8_8_S8_S8_REV_NV = $86DB;
  GL_DSDT_MAG_INTENSITY_NV = $86DC;
  GL_SHADER_CONSISTENT_NV = $86DD;
  GL_TEXTURE_SHADER_NV = $86DE;
  GL_SHADER_OPERATION_NV = $86DF;
  GL_CULL_MODES_NV = $86E0;
  GL_OFFSET_TEXTURE_MATRIX_NV = $86E1;
  GL_OFFSET_TEXTURE_SCALE_NV = $86E2;
  GL_OFFSET_TEXTURE_BIAS_NV = $86E3;
  GL_OFFSET_TEXTURE_2D_MATRIX_NV = GL_OFFSET_TEXTURE_MATRIX_NV;
  GL_OFFSET_TEXTURE_2D_SCALE_NV = GL_OFFSET_TEXTURE_SCALE_NV;
  GL_OFFSET_TEXTURE_2D_BIAS_NV = GL_OFFSET_TEXTURE_BIAS_NV;
  GL_PREVIOUS_TEXTURE_INPUT_NV = $86E4;
  GL_CONST_EYE_NV = $86E5;
  GL_PASS_THROUGH_NV = $86E6;
  GL_CULL_FRAGMENT_NV = $86E7;
  GL_OFFSET_TEXTURE_2D_NV = $86E8;
  GL_DEPENDENT_AR_TEXTURE_2D_NV = $86E9;
  GL_DEPENDENT_GB_TEXTURE_2D_NV = $86EA;
  GL_DOT_PRODUCT_NV = $86EC;
  GL_DOT_PRODUCT_DEPTH_REPLACE_NV = $86ED;
  GL_DOT_PRODUCT_TEXTURE_2D_NV = $86EE;
  GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV = $86F0;
  GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV = $86F1;
  GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV = $86F2;
  GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV = $86F3;
  GL_HILO_NV = $86F4;
  GL_DSDT_NV = $86F5;
  GL_DSDT_MAG_NV = $86F6;
  GL_DSDT_MAG_VIB_NV = $86F7;
  GL_HILO16_NV = $86F8;
  GL_SIGNED_HILO_NV = $86F9;
  GL_SIGNED_HILO16_NV = $86FA;
  GL_SIGNED_RGBA_NV = $86FB;
  GL_SIGNED_RGBA8_NV = $86FC;
  GL_SIGNED_RGB_NV = $86FE;
  GL_SIGNED_RGB8_NV = $86FF;
  GL_SIGNED_LUMINANCE_NV = $8701;
  GL_SIGNED_LUMINANCE8_NV = $8702;
  GL_SIGNED_LUMINANCE_ALPHA_NV = $8703;
  GL_SIGNED_LUMINANCE8_ALPHA8_NV = $8704;
  GL_SIGNED_ALPHA_NV = $8705;
  GL_SIGNED_ALPHA8_NV = $8706;
  GL_SIGNED_INTENSITY_NV = $8707;
  GL_SIGNED_INTENSITY8_NV = $8708;
  GL_DSDT8_NV = $8709;
  GL_DSDT8_MAG8_NV = $870A;
  GL_DSDT8_MAG8_INTENSITY8_NV = $870B;
  GL_SIGNED_RGB_UNSIGNED_ALPHA_NV = $870C;
  GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV = $870D;
  GL_HI_SCALE_NV = $870E;
  GL_LO_SCALE_NV = $870F;
  GL_DS_SCALE_NV = $8710;
  GL_DT_SCALE_NV = $8711;
  GL_MAGNITUDE_SCALE_NV = $8712;
  GL_VIBRANCE_SCALE_NV = $8713;
  GL_HI_BIAS_NV = $8714;
  GL_LO_BIAS_NV = $8715;
  GL_DS_BIAS_NV = $8716;
  GL_DT_BIAS_NV = $8717;
  GL_MAGNITUDE_BIAS_NV = $8718;
  GL_VIBRANCE_BIAS_NV = $8719;
  GL_TEXTURE_BORDER_VALUES_NV = $871A;
  GL_TEXTURE_HI_SIZE_NV = $871B;
  GL_TEXTURE_LO_SIZE_NV = $871C;
  GL_TEXTURE_DS_SIZE_NV = $871D;
  GL_TEXTURE_DT_SIZE_NV = $871E;
  GL_TEXTURE_MAG_SIZE_NV = $871F;

  // GL_NV_texture_shader2 (#231)
  GL_DOT_PRODUCT_TEXTURE_3D_NV = $86EF;

  // GL_NV_vertex_array_range2 (#232)
  GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV = $8533;

  // GL_NV_vertex_program (#233)
  GL_VERTEX_PROGRAM_NV = $8620;
  GL_VERTEX_STATE_PROGRAM_NV = $8621;
  GL_ATTRIB_ARRAY_SIZE_NV = $8623;
  GL_ATTRIB_ARRAY_STRIDE_NV = $8624;
  GL_ATTRIB_ARRAY_TYPE_NV = $8625;
  GL_CURRENT_ATTRIB_NV = $8626;
  GL_PROGRAM_LENGTH_NV = $8627;
  GL_PROGRAM_STRING_NV = $8628;
  GL_MODELVIEW_PROJECTION_NV = $8629;
  GL_IDENTITY_NV = $862A;
  GL_INVERSE_NV = $862B;
  GL_TRANSPOSE_NV = $862C;
  GL_INVERSE_TRANSPOSE_NV = $862D;
  GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV = $862E;
  GL_MAX_TRACK_MATRICES_NV = $862F;
  GL_MATRIX0_NV = $8630;
  GL_MATRIX1_NV = $8631;
  GL_MATRIX2_NV = $8632;
  GL_MATRIX3_NV = $8633;
  GL_MATRIX4_NV = $8634;
  GL_MATRIX5_NV = $8635;
  GL_MATRIX6_NV = $8636;
  GL_MATRIX7_NV = $8637;
  GL_CURRENT_MATRIX_STACK_DEPTH_NV = $8640;
  GL_CURRENT_MATRIX_NV = $8641;
  GL_VERTEX_PROGRAM_POINT_SIZE_NV = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_NV = $8643;
  GL_PROGRAM_PARAMETER_NV = $8644;
  GL_ATTRIB_ARRAY_POINTER_NV = $8645;
  GL_PROGRAM_TARGET_NV = $8646;
  GL_PROGRAM_RESIDENT_NV = $8647;
  GL_TRACK_MATRIX_NV = $8648;
  GL_TRACK_MATRIX_TRANSFORM_NV = $8649;
  GL_VERTEX_PROGRAM_BINDING_NV = $864A;
  GL_PROGRAM_ERROR_POSITION_NV = $864B;
  GL_VERTEX_ATTRIB_ARRAY0_NV = $8650;
  GL_VERTEX_ATTRIB_ARRAY1_NV = $8651;
  GL_VERTEX_ATTRIB_ARRAY2_NV = $8652;
  GL_VERTEX_ATTRIB_ARRAY3_NV = $8653;
  GL_VERTEX_ATTRIB_ARRAY4_NV = $8654;
  GL_VERTEX_ATTRIB_ARRAY5_NV = $8655;
  GL_VERTEX_ATTRIB_ARRAY6_NV = $8656;
  GL_VERTEX_ATTRIB_ARRAY7_NV = $8657;
  GL_VERTEX_ATTRIB_ARRAY8_NV = $8658;
  GL_VERTEX_ATTRIB_ARRAY9_NV = $8659;
  GL_VERTEX_ATTRIB_ARRAY10_NV = $865A;
  GL_VERTEX_ATTRIB_ARRAY11_NV = $865B;
  GL_VERTEX_ATTRIB_ARRAY12_NV = $865C;
  GL_VERTEX_ATTRIB_ARRAY13_NV = $865D;
  GL_VERTEX_ATTRIB_ARRAY14_NV = $865E;
  GL_VERTEX_ATTRIB_ARRAY15_NV = $865F;
  GL_MAP1_VERTEX_ATTRIB0_4_NV = $8660;
  GL_MAP1_VERTEX_ATTRIB1_4_NV = $8661;
  GL_MAP1_VERTEX_ATTRIB2_4_NV = $8662;
  GL_MAP1_VERTEX_ATTRIB3_4_NV = $8663;
  GL_MAP1_VERTEX_ATTRIB4_4_NV = $8664;
  GL_MAP1_VERTEX_ATTRIB5_4_NV = $8665;
  GL_MAP1_VERTEX_ATTRIB6_4_NV = $8666;
  GL_MAP1_VERTEX_ATTRIB7_4_NV = $8667;
  GL_MAP1_VERTEX_ATTRIB8_4_NV = $8668;
  GL_MAP1_VERTEX_ATTRIB9_4_NV = $8669;
  GL_MAP1_VERTEX_ATTRIB10_4_NV = $866A;
  GL_MAP1_VERTEX_ATTRIB11_4_NV = $866B;
  GL_MAP1_VERTEX_ATTRIB12_4_NV = $866C;
  GL_MAP1_VERTEX_ATTRIB13_4_NV = $866D;
  GL_MAP1_VERTEX_ATTRIB14_4_NV = $866E;
  GL_MAP1_VERTEX_ATTRIB15_4_NV = $866F;
  GL_MAP2_VERTEX_ATTRIB0_4_NV = $8670;
  GL_MAP2_VERTEX_ATTRIB1_4_NV = $8671;
  GL_MAP2_VERTEX_ATTRIB2_4_NV = $8672;
  GL_MAP2_VERTEX_ATTRIB3_4_NV = $8673;
  GL_MAP2_VERTEX_ATTRIB4_4_NV = $8674;
  GL_MAP2_VERTEX_ATTRIB5_4_NV = $8675;
  GL_MAP2_VERTEX_ATTRIB6_4_NV = $8676;
  GL_MAP2_VERTEX_ATTRIB7_4_NV = $8677;
  GL_MAP2_VERTEX_ATTRIB8_4_NV = $8678;
  GL_MAP2_VERTEX_ATTRIB9_4_NV = $8679;
  GL_MAP2_VERTEX_ATTRIB10_4_NV = $867A;
  GL_MAP2_VERTEX_ATTRIB11_4_NV = $867B;
  GL_MAP2_VERTEX_ATTRIB12_4_NV = $867C;
  GL_MAP2_VERTEX_ATTRIB13_4_NV = $867D;
  GL_MAP2_VERTEX_ATTRIB14_4_NV = $867E;
  GL_MAP2_VERTEX_ATTRIB15_4_NV = $867F;

  // GL_NV_multisample_filter_hint (#259)
  GL_MULTISAMPLE_FILTER_HINT_NV = $8534;

  // GL_NV_occlusion_query (#261)
  GL_PIXEL_COUNTER_BITS_NV = $8864;
  GL_CURRENT_OCCLUSION_QUERY_ID_NV = $8865;
  GL_PIXEL_COUNT_NV = $8866;
  GL_PIXEL_COUNT_AVAILABLE_NV = $8867;

  // GL_NV_point_sprite (#262)
  GL_POINT_SPRITE_NV = $8861;
  GL_COORD_REPLACE_NV = $8862;
  GL_POINT_SPRITE_R_MODE_NV = $8863;

  // GL_NV_texture_shader3 (#265)
  GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV = $8850;
  GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV = $8851;
  GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV = $8852;
  GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV = $8853;
  GL_OFFSET_HILO_TEXTURE_2D_NV = $8854;
  GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV = $8855;
  GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV = $8856;
  GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV = $8857;
  GL_DEPENDENT_HILO_TEXTURE_2D_NV = $8858;
  GL_DEPENDENT_RGB_TEXTURE_3D_NV = $8859;
  GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV = $885A;
  GL_DOT_PRODUCT_PASS_THROUGH_NV = $885B;
  GL_DOT_PRODUCT_TEXTURE_1D_NV = $885C;
  GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV = $885D;
  GL_HILO8_NV = $885E;
  GL_SIGNED_HILO8_NV = $885F;
  GL_FORCE_BLUE_TO_ONE_NV = $8860;

  // GL_EXT_stencil_two_side (#268)
  GL_STENCIL_TEST_TWO_SIDE_EXT = $8910;
  GL_ACTIVE_STENCIL_FACE_EXT = $8911;

  // GL_ATI_draw_buffers (#277)
  GL_MAX_DRAW_BUFFERS_ATI = $8824;
  GL_DRAW_BUFFER0_ATI = $8825;
  GL_DRAW_BUFFER1_ATI = $8826;
  GL_DRAW_BUFFER2_ATI = $8827;
  GL_DRAW_BUFFER3_ATI = $8828;
  GL_DRAW_BUFFER4_ATI = $8829;
  GL_DRAW_BUFFER5_ATI = $882A;
  GL_DRAW_BUFFER6_ATI = $882B;
  GL_DRAW_BUFFER7_ATI = $882C;
  GL_DRAW_BUFFER8_ATI = $882D;
  GL_DRAW_BUFFER9_ATI = $882E;
  GL_DRAW_BUFFER10_ATI = $882F;
  GL_DRAW_BUFFER11_ATI = $8830;
  GL_DRAW_BUFFER12_ATI = $8831;
  GL_DRAW_BUFFER13_ATI = $8832;
  GL_DRAW_BUFFER14_ATI = $8833;
  GL_DRAW_BUFFER15_ATI = $8834;

  // WGL_ATI_pixel_format_float (#278)
  WGL_TYPE_RGBA_FLOAT_ATI = $21A0;
  GL_TYPE_RGBA_FLOAT_ATI = $8820;
  GL_COLOR_CLEAR_UNCLAMPED_VALUE_ATI = $8835;

  // GL_ATI_texture_float (#280)
  GL_RGBA_FLOAT32_ATI = $8814;
  GL_RGB_FLOAT32_ATI = $8815;
  GL_ALPHA_FLOAT32_ATI = $8816;
  GL_INTENSITY_FLOAT32_ATI = $8817;
  GL_LUMINANCE_FLOAT32_ATI = $8818;
  GL_LUMINANCE_ALPHA_FLOAT32_ATI = $8819;
  GL_RGBA_FLOAT16_ATI = $881A;
  GL_RGB_FLOAT16_ATI = $881B;
  GL_ALPHA_FLOAT16_ATI = $881C;
  GL_INTENSITY_FLOAT16_ATI = $881D;
  GL_LUMINANCE_FLOAT16_ATI = $881E;
  GL_LUMINANCE_ALPHA_FLOAT16_ATI = $881F;

  // GL_NV_float_buffer (#281)
  // WGL_NV_float_buffer
  // GLX_NV_float_buffer
  GL_FLOAT_R_NV = $8880;
  GL_FLOAT_RG_NV = $8881;
  GL_FLOAT_RGB_NV = $8882;
  GL_FLOAT_RGBA_NV = $8883;
  GL_FLOAT_R16_NV = $8884;
  GL_FLOAT_R32_NV = $8885;
  GL_FLOAT_RG16_NV = $8886;
  GL_FLOAT_RG32_NV = $8887;
  GL_FLOAT_RGB16_NV = $8888;
  GL_FLOAT_RGB32_NV = $8889;
  GL_FLOAT_RGBA16_NV = $888A;
  GL_FLOAT_RGBA32_NV = $888B;
  GL_TEXTURE_FLOAT_COMPONENTS_NV = $888C;
  GL_FLOAT_CLEAR_COLOR_VALUE_NV = $888D;
  GL_FLOAT_RGBA_MODE_NV = $888E;
  WGL_FLOAT_COMPONENTS_NV = $20B0;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_R_NV = $20B1;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RG_NV = $20B2;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGB_NV = $20B3;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGBA_NV = $20B4;
  WGL_TEXTURE_FLOAT_R_NV = $20B5;
  WGL_TEXTURE_FLOAT_RG_NV = $20B6;
  WGL_TEXTURE_FLOAT_RGB_NV = $20B7;
  WGL_TEXTURE_FLOAT_RGBA_NV = $20B8;
  GLX_FLOAT_COMPONENTS_NV = $20B0;

  // GL_NV_primitive_restart (#285)
  GL_PRIMITIVE_RESTART_NV = $8558;
  GL_PRIMITIVE_RESTART_INDEX_NV = $8559;

  // GL_EXT_depth_bounds_test (#297)
  GL_DEPTH_BOUNDS_TEST_EXT = $8890;
  GL_DEPTH_BOUNDS_EXT = $8891;

  // GL_EXT_texture_mirror_clamp (#298)
  GL_MIRROR_CLAMP_EXT = $8742;
  GL_MIRROR_CLAMP_TO_EDGE_EXT = $8743;
  GL_MIRROR_CLAMP_TO_BORDER_EXT = $8912;

  // GL_EXT_blend_equation_separate (EXT #299)
  GL_BLEND_EQUATION_RGB_EXT = $8009;
  GL_BLEND_EQUATION_ALPHA_EXT = $883D;

  // GL_EXT_pixel_buffer_object (EXT #302)
  GL_PIXEL_PACK_BUFFER_EXT = $88EB;
  GL_PIXEL_UNPACK_BUFFER_EXT = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING_EXT = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING_EXT = $88EF;

  // GL_EXT_framebuffer_object (#310)
  GL_FRAMEBUFFER_EXT = $8D40;
  GL_RENDERBUFFER_EXT = $8D41;
  GL_STENCIL_INDEX1_EXT = $8D46;
  GL_STENCIL_INDEX4_EXT = $8D47;
  GL_STENCIL_INDEX8_EXT = $8D48;
  GL_STENCIL_INDEX16_EXT = $8D49;
  GL_DEPTH24_STENCIL8_EXT = $88F0;
  GL_RENDERBUFFER_WIDTH_EXT = $8D42;
  GL_RENDERBUFFER_HEIGHT_EXT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT_EXT = $8D44;
  GL_RENDERBUFFER_RED_SIZE_EXT = $8D50;
  GL_RENDERBUFFER_GREEN_SIZE_EXT = $8D51;
  GL_RENDERBUFFER_BLUE_SIZE_EXT = $8D52;
  GL_RENDERBUFFER_ALPHA_SIZE_EXT = $8D53;
  GL_RENDERBUFFER_DEPTH_SIZE_EXT = $8D54;
  GL_RENDERBUFFER_STENCIL_SIZE_EXT = $8D55;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT = $8CD3;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT = $8CD4;
  GL_COLOR_ATTACHMENT0_EXT = $8CE0;
  GL_COLOR_ATTACHMENT1_EXT = $8CE1;
  GL_COLOR_ATTACHMENT2_EXT = $8CE2;
  GL_COLOR_ATTACHMENT3_EXT = $8CE3;
  GL_COLOR_ATTACHMENT4_EXT = $8CE4;
  GL_COLOR_ATTACHMENT5_EXT = $8CE5;
  GL_COLOR_ATTACHMENT6_EXT = $8CE6;
  GL_COLOR_ATTACHMENT7_EXT = $8CE7;
  GL_COLOR_ATTACHMENT8_EXT = $8CE8;
  GL_COLOR_ATTACHMENT9_EXT = $8CE9;
  GL_COLOR_ATTACHMENT10_EXT = $8CEA;
  GL_COLOR_ATTACHMENT11_EXT = $8CEB;
  GL_COLOR_ATTACHMENT12_EXT = $8CEC;
  GL_COLOR_ATTACHMENT13_EXT = $8CED;
  GL_COLOR_ATTACHMENT14_EXT = $8CEE;
  GL_COLOR_ATTACHMENT15_EXT = $8CEF;
  GL_DEPTH_ATTACHMENT_EXT = $8D00;
  GL_STENCIL_ATTACHMENT_EXT = $8D20;
  GL_FRAMEBUFFER_COMPLETE_EXT = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT = $8CD8;
  GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT = $8CD9;
  GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT = $8CDA;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT = $8CDC;
  GL_FRAMEBUFFER_UNSUPPORTED_EXT = $8CDD;
  GL_FRAMEBUFFER_BINDING_EXT = $8CA6;
  GL_RENDERBUFFER_BINDING_EXT = $8CA7;
  GL_MAX_COLOR_ATTACHMENTS_EXT = $8CDF;
  GL_MAX_RENDERBUFFER_SIZE_EXT = $84E8;
  GL_INVALID_FRAMEBUFFER_OPERATION_EXT = $0506;

  // GL_EXT_packed_depth_stencil (#312)
  GL_DEPTH_STENCIL_EXT = $84F9;
  GL_UNSIGNED_INT_24_8_EXT = $84FA;
  //GL_DEPTH24_STENCIL8_EXT                          = $88F0;
  GL_TEXTURE_STENCIL_SIZE_EXT = $88F1;

  // GL_EXT_stencil_clear_tag (#314)
  GL_STENCIL_TAG_BITS_EXT = $88F2;
  GL_STENCIL_CLEAR_TAG_VALUE_EXT = $88F3;

  // GL_EXT_texture_sRGB (#315)
  GL_SRGB_EXT = $8C40;
  GL_SRGB8_EXT = $8C41;
  GL_SRGB_ALPHA_EXT = $8C42;
  GL_SRGB8_ALPHA8_EXT = $8C43;
  GL_SLUMINANCE_ALPHA_EXT = $8C44;
  GL_SLUMINANCE8_ALPHA8_EXT = $8C45;
  GL_SLUMINANCE_EXT = $8C46;
  GL_SLUMINANCE8_EXT = $8C47;
  GL_COMPRESSED_SRGB_EXT = $8C48;
  GL_COMPRESSED_SRGB_ALPHA_EXT = $8C49;
  GL_COMPRESSED_SLUMINANCE_EXT = $8C4A;
  GL_COMPRESSED_SLUMINANCE_ALPHA_EXT = $8C4B;
  GL_COMPRESSED_SRGB_S3TC_DXT1_EXT = $8C4C;
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT = $8C4D;
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT = $8C4E;
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT = $8C4F;

  // GL_EXT_framebuffer_blit (#316)
  GL_READ_FRAMEBUFFER_EXT = $8CA8;
  GL_DRAW_FRAMEBUFFER_EXT = $8CA9;
  GL_DRAW_FRAMEBUFFER_BINDING_EXT = $8CA6; // alias FRAMEBUFFER_BINDING_EXT
  GL_READ_FRAMEBUFFER_BINDING_EXT = $8CAA;

  // GL_EXT_framebuffer_multisample (#317)
  GL_RENDERBUFFER_SAMPLES_EXT = $8CAB;
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_EXT = $8D56;
  GL_MAX_SAMPLES_EXT = $8D57;

  // GL_EXT_timer_query (#319)
  GL_TIME_ELAPSED_EXT = $88BF;

  // GL_EXT_gpu_program_parameters (#320)
  // (no new tokens)

  // GL_NV_geometry_program4 (#323) - this seems to be supported on NO hardware
  GL_GEOMETRY_PROGRAM_NV = $8C26;
  GL_MAX_PROGRAM_OUTPUT_VERTICES_NV = $8C27;
  GL_MAX_PROGRAM_TOTAL_OUTPUT_COMPONENTS_NV = $8C28;

  // GL_EXT_geometry_shader4 (#324)
  GL_GEOMETRY_SHADER_EXT = $8DD9;
  GL_GEOMETRY_VERTICES_OUT_EXT = $8DDA;
  GL_GEOMETRY_INPUT_TYPE_EXT = $8DDB;
  GL_GEOMETRY_OUTPUT_TYPE_EXT = $8DDC;
  GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_EXT = $8C29;
  GL_MAX_GEOMETRY_VARYING_COMPONENTS_EXT = $8DDD;
  GL_MAX_VERTEX_VARYING_COMPONENTS_EXT = $8DDE;
  GL_MAX_VARYING_COMPONENTS_EXT = $8B4B;
  GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_EXT = $8DDF;
  GL_MAX_GEOMETRY_OUTPUT_VERTICES_EXT = $8DE0;
  GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_EXT = $8DE1;
  GL_LINES_ADJACENCY_EXT = $A;
  GL_LINE_STRIP_ADJACENCY_EXT = $B;
  GL_TRIANGLES_ADJACENCY_EXT = $C;
  GL_TRIANGLE_STRIP_ADJACENCY_EXT = $D;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_EXT = $8DA8;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_EXT = $8DA9;
  GL_FRAMEBUFFER_ATTACHMENT_LAYERED_EXT = $8DA7;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT = $8CD4;
  GL_PROGRAM_POINT_SIZE_EXT = $8642;

  // GL_EXT_gpu_shader4 (#326)
  GL_VERTEX_ATTRIB_ARRAY_INTEGER_EXT = $88FD;
  GL_SAMPLER_1D_ARRAY_EXT = $8DC0;
  GL_SAMPLER_2D_ARRAY_EXT = $8DC1;
  GL_SAMPLER_BUFFER_EXT = $8DC2;
  GL_SAMPLER_1D_ARRAY_SHADOW_EXT = $8DC3;
  GL_SAMPLER_2D_ARRAY_SHADOW_EXT = $8DC4;
  GL_SAMPLER_CUBE_SHADOW_EXT = $8DC5;
  //GL_UNSIGNED_INT                                     = $1405;
  GL_UNSIGNED_INT_VEC2_EXT = $8DC6;
  GL_UNSIGNED_INT_VEC3_EXT = $8DC7;
  GL_UNSIGNED_INT_VEC4_EXT = $8DC8;
  GL_INT_SAMPLER_1D_EXT = $8DC9;
  GL_INT_SAMPLER_2D_EXT = $8DCA;
  GL_INT_SAMPLER_3D_EXT = $8DCB;
  GL_INT_SAMPLER_CUBE_EXT = $8DCC;
  GL_INT_SAMPLER_2D_RECT_EXT = $8DCD;
  GL_INT_SAMPLER_1D_ARRAY_EXT = $8DCE;
  GL_INT_SAMPLER_2D_ARRAY_EXT = $8DCF;
  GL_INT_SAMPLER_BUFFER_EXT = $8DD0;
  GL_UNSIGNED_INT_SAMPLER_1D_EXT = $8DD1;
  GL_UNSIGNED_INT_SAMPLER_2D_EXT = $8DD2;
  GL_UNSIGNED_INT_SAMPLER_3D_EXT = $8DD3;
  GL_UNSIGNED_INT_SAMPLER_CUBE_EXT = $8DD4;
  GL_UNSIGNED_INT_SAMPLER_2D_RECT_EXT = $8DD5;
  GL_UNSIGNED_INT_SAMPLER_1D_ARRAY_EXT = $8DD6;
  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY_EXT = $8DD7;
  GL_UNSIGNED_INT_SAMPLER_BUFFER_EXT = $8DD8;
  GL_MIN_PROGRAM_TEXEL_OFFSET_EXT = $8904;
  GL_MAX_PROGRAM_TEXEL_OFFSET_EXT = $8905;

  // GL_EXT_packed_float (#328)
  // WGL_EXT_pixel_format_packed_float
  // GLX_EXT_fbconfig_packed_float
  GL_R11F_G11F_B10F_EXT = $8C3A;
  GL_UNSIGNED_INT_10F_11F_11F_REV_EXT = $8C3B;
  GL_RGBA_SIGNED_COMPONENTS_EXT = $8C3C;
  WGL_TYPE_RGBA_UNSIGNED_FLOAT_EXT = $20A8;
  GLX_RGBA_UNSIGNED_FLOAT_TYPE_EXT = $20B1;
  GLX_RGBA_UNSIGNED_FLOAT_BIT_EXT = $00000008;

  // GL_EXT_texture_array (#329)
  GL_TEXTURE_1D_ARRAY_EXT = $8C18;
  GL_TEXTURE_2D_ARRAY_EXT = $8C1A;
  GL_PROXY_TEXTURE_2D_ARRAY_EXT = $8C1B;
  GL_PROXY_TEXTURE_1D_ARRAY_EXT = $8C19;
  GL_TEXTURE_BINDING_1D_ARRAY_EXT = $8C1C;
  GL_TEXTURE_BINDING_2D_ARRAY_EXT = $8C1D;
  GL_MAX_ARRAY_TEXTURE_LAYERS_EXT = $88FF;
  GL_COMPARE_REF_DEPTH_TO_TEXTURE_EXT = $884E;
  //GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT      = $8CD4;
  //GL_SAMPLER_1D_ARRAY_EXT                          = $8DC0;
  //GL_SAMPLER_2D_ARRAY_EXT                          = $8DC1;
  //GL_SAMPLER_1D_ARRAY_SHADOW_EXT                   = $8DC3;
  //GL_SAMPLER_2D_ARRAY_SHADOW_EXT                   = $8DC4;

  // GL_EXT_texture_buffer_object (#330)
  GL_TEXTURE_BUFFER_EXT = $8C2A;
  GL_MAX_TEXTURE_BUFFER_SIZE_EXT = $8C2B;
  GL_TEXTURE_BINDING_BUFFER_EXT = $8C2C;
  GL_TEXTURE_BUFFER_DATA_STORE_BINDING_EXT = $8C2D;
  GL_TEXTURE_BUFFER_FORMAT_EXT = $8C2E;

  // GL_EXT_texture_compression_latc (#331)
  GL_COMPRESSED_LUMINANCE_LATC1_EXT = $8C70;
  GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT = $8C71;
  GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT = $8C72;
  GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT = $8C73;

  // // GL_ATI_texture_compression_3dc
  GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI = $8837;

  // GL_EXT_texture_compression_rgtc (#332)
  GL_COMPRESSED_RED_RGTC1_EXT = $8DBB;
  GL_COMPRESSED_SIGNED_RED_RGTC1_EXT = $8DBC;
  GL_COMPRESSED_RED_GREEN_RGTC2_EXT = $8DBD;
  GL_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT = $8DBE;

  // GL_EXT_texture_shared_exponent (#333)
  GL_RGB9_E5_EXT = $8C3D;
  GL_UNSIGNED_INT_5_9_9_9_REV_EXT = $8C3E;
  GL_TEXTURE_SHARED_SIZE_EXT = $8C3F;

  // GL_EXT_framebuffer_sRGB (#337)
  // GLX_EXT_framebuffer_sRGB
  // WGL_EXT_framebuffer_sRGB
  GLX_FRAMEBUFFER_SRGB_CAPABLE_EXT = $20B2;
  WGL_FRAMEBUFFER_SRGB_CAPABLE_EXT = $20A9;
  GL_FRAMEBUFFER_SRGB_EXT = $8DB9;
  GL_FRAMEBUFFER_SRGB_CAPABLE_EXT = $8DBA;

  // GL_NV_transform_feedback (#341)
  GL_TRANSFORM_FEEDBACK_BUFFER_NV = $8C8E;
  GL_TRANSFORM_FEEDBACK_BUFFER_START_NV = $8C84;
  GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_NV = $8C85;
  GL_TRANSFORM_FEEDBACK_RECORD_NV = $8C86;
  GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_NV = $8C8F;
  GL_INTERLEAVED_ATTRIBS_NV = $8C8C;
  GL_SEPARATE_ATTRIBS_NV = $8C8D;
  GL_PRIMITIVES_GENERATED_NV = $8C87;
  GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_NV = $8C88;
  GL_RASTERIZER_DISCARD_NV = $8C89;
  GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_NV = $8C8A;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_NV = $8C8B;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_NV = $8C80;
  GL_TRANSFORM_FEEDBACK_ATTRIBS_NV = $8C7E;
  GL_ACTIVE_VARYINGS_NV = $8C81;
  GL_ACTIVE_VARYING_MAX_LENGTH_NV = $8C82;
  GL_TRANSFORM_FEEDBACK_VARYINGS_NV = $8C83;
  GL_TRANSFORM_FEEDBACK_BUFFER_MODE_NV = $8C7F;
  GL_BACK_PRIMARY_COLOR_NV = $8C77;
  GL_BACK_SECONDARY_COLOR_NV = $8C78;
  GL_TEXTURE_COORD_NV = $8C79;
  GL_CLIP_DISTANCE_NV = $8C7A;
  GL_VERTEX_ID_NV = $8C7B;
  GL_PRIMITIVE_ID_NV = $8C7C;
  GL_GENERIC_ATTRIB_NV = $8C7D;
  //GL_POINT_SIZE                                        =$0B11;
  //GL_FOG_COORDINATE                                    =$8451;
  //GL_SECONDARY_COLOR_NV                                =$852D;
  //GL_PRIMARY_COLOR                                     =$8577;
  //GL_POSITION                                          =$1203;
  GL_LAYER_NV = $8DAA;
  //GL_UNSIGNED_INT_VEC2_EXT                             =$8DC6;
  //GL_UNSIGNED_INT_VEC3_EXT                             =$8DC7;
  //GL_UNSIGNED_INT_VEC4_EXT                             =$8DC8;

  // GL_EXT_bindable_uniform (#342)
  GL_MAX_VERTEX_BINDABLE_UNIFORMS_EXT = $8DE2;
  GL_MAX_FRAGMENT_BINDABLE_UNIFORMS_EXT = $8DE3;
  GL_MAX_GEOMETRY_BINDABLE_UNIFORMS_EXT = $8DE4;
  GL_MAX_BINDABLE_UNIFORM_SIZE_EXT = $8DED;
  GL_UNIFORM_BUFFER_BINDING_EXT = $8DEF;
  GL_UNIFORM_BUFFER_EXT = $8DEE;

  // GL_EXT_texture_integer (#343)
  GL_RGBA_INTEGER_MODE_EXT = $8D9E;
  GL_RGBA32UI_EXT = $8D70;
  GL_RGB32UI_EXT = $8D71;
  GL_ALPHA32UI_EXT = $8D72;
  GL_INTENSITY32UI_EXT = $8D73;
  GL_LUMINANCE32UI_EXT = $8D74;
  GL_LUMINANCE_ALPHA32UI_EXT = $8D75;

  GL_RGBA16UI_EXT = $8D76;
  GL_RGB16UI_EXT = $8D77;
  GL_ALPHA16UI_EXT = $8D78;
  GL_INTENSITY16UI_EXT = $8D79;
  GL_LUMINANCE16UI_EXT = $8D7A;
  GL_LUMINANCE_ALPHA16UI_EXT = $8D7B;

  GL_RGBA8UI_EXT = $8D7C;
  GL_RGB8UI_EXT = $8D7D;
  GL_ALPHA8UI_EXT = $8D7E;
  GL_INTENSITY8UI_EXT = $8D7F;
  GL_LUMINANCE8UI_EXT = $8D80;
  GL_LUMINANCE_ALPHA8UI_EXT = $8D81;

  GL_RGBA32I_EXT = $8D82;
  GL_RGB32I_EXT = $8D83;
  GL_ALPHA32I_EXT = $8D84;
  GL_INTENSITY32I_EXT = $8D85;
  GL_LUMINANCE32I_EXT = $8D86;
  GL_LUMINANCE_ALPHA32I_EXT = $8D87;

  GL_RGBA16I_EXT = $8D88;
  GL_RGB16I_EXT = $8D89;
  GL_ALPHA16I_EXT = $8D8A;
  GL_INTENSITY16I_EXT = $8D8B;
  GL_LUMINANCE16I_EXT = $8D8C;
  GL_LUMINANCE_ALPHA16I_EXT = $8D8D;

  GL_RGBA8I_EXT = $8D8E;
  GL_RGB8I_EXT = $8D8F;
  GL_ALPHA8I_EXT = $8D90;
  GL_INTENSITY8I_EXT = $8D91;
  GL_LUMINANCE8I_EXT = $8D92;
  GL_LUMINANCE_ALPHA8I_EXT = $8D93;

  GL_RED_INTEGER_EXT = $8D94;
  GL_GREEN_INTEGER_EXT = $8D95;
  GL_BLUE_INTEGER_EXT = $8D96;
  GL_ALPHA_INTEGER_EXT = $8D97;
  GL_RGB_INTEGER_EXT = $8D98;
  GL_RGBA_INTEGER_EXT = $8D99;
  GL_BGR_INTEGER_EXT = $8D9A;
  GL_BGRA_INTEGER_EXT = $8D9B;
  GL_LUMINANCE_INTEGER_EXT = $8D9C;
  GL_LUMINANCE_ALPHA_INTEGER_EXT = $8D9D;

  // GL_NV_conditional_render (#346)
  GL_QUERY_WAIT_NV = $8E13;
  GL_QUERY_NO_WAIT_NV = $8E14;
  GL_QUERY_BY_REGION_WAIT_NV = $8E15;
  GL_QUERY_BY_REGION_NO_WAIT_NV = $8E16;

  // GL_EXT_transform_feedback (#352)
  GL_TRANSFORM_FEEDBACK_BUFFER_EXT = $8C8E;
  GL_TRANSFORM_FEEDBACK_BUFFER_START_EXT = $8C84;
  GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_EXT = $8C85;
  GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_EXT = $8C8F;
  GL_INTERLEAVED_ATTRIBS_EXT = $8C8C;
  GL_SEPARATE_ATTRIBS_EXT = $8C8D;
  GL_PRIMITIVES_GENERATED_EXT = $8C87;
  GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_EXT = $8C88;
  GL_RASTERIZER_DISCARD_EXT = $8C89;
  GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_EXT = $8C8A;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_EXT = $8C8B;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_EXT = $8C80;
  GL_TRANSFORM_FEEDBACK_VARYINGS_EXT = $8C83;
  GL_TRANSFORM_FEEDBACK_BUFFER_MODE_EXT = $8C7F;
  GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH_EXT = $8C76;

  // GL_ATI_meminfo (#359)
  GL_VBO_FREE_MEMORY_ATI                     = $87FB;
  GL_TEXTURE_FREE_MEMORY_ATI                 = $87FC;
  GL_RENDERBUFFER_FREE_MEMORY_ATI            = $87FD;

  // GL_AMD_vertex_shader_tessellator (#363)
  GL_SAMPLER_BUFFER_AMD = $9001;
  GL_INT_SAMPLER_BUFFER_AMD = $9002;
  GL_UNSIGNED_INT_SAMPLER_BUFFER_AMD = $9003;
  GL_DISCRETE_AMD = $9006;
  GL_CONTINUOUS_AMD = $9007;
  GL_TESSELLATION_MODE_AMD = $9004;
  GL_TESSELLATION_FACTOR_AMD = $9005;

  // GL_NV_shader_buffer_load (#379)
  GL_BUFFER_GPU_ADDRESS_NV = $8F1D;
  GL_GPU_ADDRESS_NV = $8F34;
  GL_MAX_SHADER_BUFFER_ADDRESS_NV = $8F35;

  // GL_NV_vertex_buffer_unified_memory (#380)
  GL_VERTEX_ATTRIB_ARRAY_UNIFIED_NV = $8F1E;
  GL_ELEMENT_ARRAY_UNIFIED_NV = $8F1F;
  GL_VERTEX_ATTRIB_ARRAY_ADDRESS_NV = $8F20;
  GL_VERTEX_ARRAY_ADDRESS_NV = $8F21;
  GL_NORMAL_ARRAY_ADDRESS_NV = $8F22;
  GL_COLOR_ARRAY_ADDRESS_NV = $8F23;
  GL_INDEX_ARRAY_ADDRESS_NV = $8F24;
  GL_TEXTURE_COORD_ARRAY_ADDRESS_NV = $8F25;
  GL_EDGE_FLAG_ARRAY_ADDRESS_NV = $8F26;
  GL_SECONDARY_COLOR_ARRAY_ADDRESS_NV = $8F27;
  GL_FOG_COORD_ARRAY_ADDRESS_NV = $8F28;
  GL_ELEMENT_ARRAY_ADDRESS_NV = $8F29;
  GL_VERTEX_ATTRIB_ARRAY_LENGTH_NV = $8F2A;
  GL_VERTEX_ARRAY_LENGTH_NV = $8F2B;
  GL_NORMAL_ARRAY_LENGTH_NV = $8F2C;
  GL_COLOR_ARRAY_LENGTH_NV = $8F2D;
  GL_INDEX_ARRAY_LENGTH_NV = $8F2E;
  GL_TEXTURE_COORD_ARRAY_LENGTH_NV = $8F2F;
  GL_EDGE_FLAG_ARRAY_LENGTH_NV = $8F30;
  GL_SECONDARY_COLOR_ARRAY_LENGTH_NV = $8F31;
  GL_FOG_COORD_ARRAY_LENGTH_NV = $8F32;
  GL_ELEMENT_ARRAY_LENGTH_NV = $8F33;

  // GL_NV_vdpau_interop (#396)
  GL_SURFACE_STATE_NV                              = $86EB;
  GL_SURFACE_REGISTERED_NV                         = $86FD;
  GL_SURFACE_MAPPED_NV                             = $8700;
  GL_WRITE_DISCARD_NV                              = $88BE;

  // unknown extension, where does it come from?
  WGL_COLOR_SAMPLES_NV = $20B9;

  // GL_NVX_gpu_memory_info (experimental NV extension)
  GL_GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX          = $9047;
  GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX    = $9048;
  GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX  = $9049;
  GL_GPU_MEMORY_INFO_EVICTION_COUNT_NVX            = $904A;
  GL_GPU_MEMORY_INFO_EVICTED_MEMORY_NVX            = $904B;


{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS} {$REGION 'OpenGL Extension to the X Window System (GLX) generic constants'} {$ENDIF}

   GLX_EXTENSION_NAME                               = 'GLX';
   GLX_USE_GL                                       = 1;
   GLX_BUFFER_SIZE                                  = 2;
   GLX_LEVEL                                        = 3;
   GLX_RGBA                                         = 4;
   GLX_DOUBLEBUFFER                                 = 5;
   GLX_STEREO                                       = 6;
   GLX_AUX_BUFFERS                                  = 7;
   GLX_RED_SIZE                                     = 8;
   GLX_GREEN_SIZE                                   = 9;
   GLX_BLUE_SIZE                                    = 10;
   GLX_ALPHA_SIZE                                   = 11;
   GLX_DEPTH_SIZE                                   = 12;
   GLX_STENCIL_SIZE                                 = 13;
   GLX_ACCUM_RED_SIZE                               = 14;
   GLX_ACCUM_GREEN_SIZE                             = 15;
   GLX_ACCUM_BLUE_SIZE                              = 16;
   GLX_ACCUM_ALPHA_SIZE                             = 17;

   // Error codes returned by glXGetConfig:
   GLX_BAD_SCREEN                                   = 1;
   GLX_BAD_ATTRIBUTE                                = 2;
   GLX_NO_EXTENSION                                 = 3;
   GLX_BAD_VISUAL                                   = 4;
   GLX_BAD_CONTEXT                                  = 5;
   GLX_BAD_VALUE                                    = 6;
   GLX_BAD_ENUM                                     = 7;
   // SGIX_hyperpipe
   GLX_BAD_HYPERPIPE_CONFIG_SGIX		    = 91;
   GLX_BAD_HYPERPIPE_SGIX			    = 92;

   // GLX 1.1 and later:
   GLX_VENDOR                                       = 1;
   GLX_VERSION                                      = 2;
   GLX_EXTENSIONS                                   = 3;

   // GLX 1.3 and later:
   GLX_CONFIG_CAVEAT                                = $20;
   //CONFIG_CAVEAT attribute value
   GLX_NONE                                         = $8000;
   GLX_DONT_CARE                                    = $FFFFFFFF;
   GLX_SLOW_CONFIG                                  = $8001;
   GLX_NON_CONFORMANT_CONFIG                        = $800D;

   GLX_X_VISUAL_TYPE                                = $22;
   GLX_TRANSPARENT_TYPE                             = $23;
   GLX_TRANSPARENT_INDEX_VALUE                      = $24;
   GLX_TRANSPARENT_RED_VALUE                        = $25;
   GLX_TRANSPARENT_GREEN_VALUE                      = $26;
   GLX_TRANSPARENT_BLUE_VALUE                       = $27;
   GLX_TRANSPARENT_ALPHA_VALUE                      = $28;
   GLX_TRUE_COLOR                                   = $8002;
   GLX_DIRECT_COLOR                                 = $8003;
   GLX_PSEUDO_COLOR                                 = $8004;
   GLX_STATIC_COLOR                                 = $8005;
   GLX_GRAY_SCALE                                   = $8006;
   GLX_STATIC_GRAY                                  = $8007;
   GLX_TRANSPARENT_RGB				    = $8008;
   GLX_TRANSPARENT_INDEX                            = $8009;

   GLX_MAX_PBUFFER_WIDTH                            = $8016;
   GLX_MAX_PBUFFER_HEIGHT                           = $8017;
   GLX_MAX_PBUFFER_PIXELS                           = $8018;
   GLX_PRESERVED_CONTENTS                           = $801B;
   GLX_LARGEST_BUFFER                               = $801C;
   GLX_WIDTH					    = $801D;
   GLX_HEIGHT					    = $801E;
   GLX_EVENT_MASK				    = $801F;
   GLX_DRAWABLE_TYPE                                = $8010;
   GLX_FBCONFIG_ID                                  = $8013;
   GLX_VISUAL_ID                                    = $800B;
   //GLXDrawableTypeMask
   GLX_WINDOW_BIT                                   = $00000001;
   GLX_PIXMAP_BIT                                   = $00000002;
   GLX_PBUFFER_BIT                                  = $00000004;
   GLX_WINDOW_BIT_SGIX				    = $00000001;//DRAWABLE_TYPE_SGIX value
   GLX_PIXMAP_BIT_SGIX				    = $00000002;
   GLX_PBUFFER_BIT_SGIX			 	    = $00000004;

   //GLXPbufferClobberMask
   GLX_AUX_BUFFERS_BIT                              = $00000010;
   GLX_FRONT_LEFT_BUFFER_BIT                        = $00000001;
   GLX_FRONT_RIGHT_BUFFER_BIT                       = $00000002;
   GLX_BACK_LEFT_BUFFER_BIT                         = $00000004;
   GLX_BACK_RIGHT_BUFFER_BIT                        = $00000008;
   GLX_DEPTH_BUFFER_BIT                             = $00000020;
   GLX_STENCIL_BUFFER_BIT                           = $00000040;
   GLX_ACCUM_BUFFER_BIT                             = $00000080;
   GLX_FRONT_LEFT_BUFFER_BIT_SGIX	  	    = $00000001;//BufferClobberEventSGIX mask
   GLX_FRONT_RIGHT_BUFFER_BIT_SGIX		    = $00000002;
   GLX_BACK_LEFT_BUFFER_BIT_SGIX		    = $00000004;
   GLX_BACK_RIGHT_BUFFER_BIT_SGIX		    = $00000008;
   GLX_AUX_BUFFERS_BIT_SGIX			    = $00000010;
   GLX_DEPTH_BUFFER_BIT_SGIX			    = $00000020;
   GLX_STENCIL_BUFFER_BIT_SGIX			    = $00000040;
   GLX_ACCUM_BUFFER_BIT_SGIX			    = $00000080;
   GLX_SAMPLE_BUFFERS_BIT_SGIX			    = $00000100;


   GLX_RENDER_TYPE                                  = $8011;
   GLX_X_RENDERABLE                                 = $8012;

   GLX_RGBA_TYPE                                    = $8014;
   //GLXRenderTypeMask
   GLX_RGBA_BIT                                     = $00000001;
   GLX_COLOR_INDEX_TYPE                             = $8015;
   GLX_COLOR_INDEX_BIT                              = $00000002;
   GLX_RGBA_BIT_SGIX				    = $00000001	;
   GLX_COLOR_INDEX_BIT_SGIX			    = $00000002	;

   GLX_SCREEN                                       = $800C;
   GLX_PBUFFER_CLOBBER_MASK                         = $08000000;
   GLX_DAMAGED                                      = $8020;
   GLX_SAVED                                        = $8021;
   GLX_WINDOW                                       = $8022;
   //CreateGLXPbuffer attribute
   GLX_PBUFFER                                      = $8023;
   GLX_PBUFFER_HEIGHT				    = $8040;
   GLX_PBUFFER_WIDTH				    = $8041;
   GLX_X_VISUAL_TYPE_EXT                            = $22;
   GLX_TRANSPARENT_TYPE_EXT                         = $23;
   GLX_TRANSPARENT_INDEX_VALUE_EXT                  = $24;
   GLX_TRANSPARENT_RED_VALUE_EXT                    = $25;
   GLX_TRANSPARENT_GREEN_VALUE_EXT                  = $26;
   GLX_TRANSPARENT_BLUE_VALUE_EXT                   = $27;
   GLX_TRANSPARENT_ALPHA_VALUE_EXT                  = $28;
   GLX_TRUE_COLOR_EXT                               = $8002;
   GLX_DIRECT_COLOR_EXT                             = $8003;
   GLX_PSEUDO_COLOR_EXT                             = $8004;
   GLX_STATIC_COLOR_EXT                             = $8005;
   GLX_GRAY_SCALE_EXT                               = $8006;
   GLX_STATIC_GRAY_EXT                              = $8007;
   GLX_TRANSPARENT_RGB_EXT                          = $8008;
   GLX_TRANSPARENT_INDEX_EXT                        = $8009;
   GLX_VISUAL_CAVEAT_EXT                            = $20;
   GLX_NONE_EXT                                     = $8000;
   GLX_SLOW_VISUAL_EXT                              = $8001;
   GLX_NON_CONFORMANT_VISUAL_EXT                    = $800D;
   GLX_SHARE_CONTEXT_EXT                            = $800A;
   GLX_VISUAL_ID_EXT                                = $800B;
   GLX_SCREEN_EXT                                   = $800C;
   GLX_3DFX_WINDOW_MODE_MESA                        = $1;
   GLX_3DFX_FULLSCREEN_MODE_MESA                    = $2;

   //SGIX
   GLX_DRAWABLE_TYPE_SGIX			    = $8010;//FBConfigSGIX attribute
   GLX_RENDER_TYPE_SGIX			   	    = $8011;
   GLX_X_RENDERABLE_SGIX			 	    = $8012;
   GLX_FBCONFIG_ID_SGIX			 	    = 48013;
   GLX_RGBA_TYPE_SGIX			  	    = $8014; //CreateContextWithConfigSGIX render_type value
   GLX_COLOR_INDEX_TYPE_SGIX		    	    = $8015;
   GLX_MAX_PBUFFER_WIDTH_SGIX		    	    = $8016; //FBConfigSGIX attribute
   GLX_MAX_PBUFFER_HEIGHT_SGIX		   	    = $8017;
   GLX_MAX_PBUFFER_PIXELS_SGIX		   	    = $8018;
   GLX_OPTIMAL_PBUFFER_WIDTH_SGIX		    = $8019;
   GLX_OPTIMAL_PBUFFER_HEIGHT_SGIX		    = $801A;
   GLX_PRESERVED_CONTENTS_SGIX			    = $801B; //PbufferSGIX attribute
   GLX_LARGEST_PBUFFER_SGIX			    = $801C;
   GLX_WIDTH_SGIX				    = $801D;
   GLX_HEIGHT_SGIX			            = $801E;
   GLX_EVENT_MASK_SGIX				    = $801F;
   GLX_DAMAGED_SGIX				    = $8020;//BufferClobberSGIX event_type value
   GLX_SAVED_SGIX				    = $8021;
   GLX_WINDOW_SGIX			            = $8022;//BufferClobberSGIX draw_type value
   GLX_PBUFFER_SGIX				    = $8023;
   GLX_DIGITAL_MEDIA_PBUFFER_SGIX		    = $8024; //PbufferSGIX attribute
   GLX_BLENDED_RGBA_SGIS			    = $8025; //TRANSPARENT_TYPE_EXT attribute value
   GLX_MULTISAMPLE_SUB_RECT_WIDTH_SGIS		    = $8026;// Visual attribute (shared_multisample)
   GLX_MULTISAMPLE_SUB_RECT_HEIGHT_SGIS		    = $8027;
   GLX_VISUAL_SELECT_GROUP_SGIX			    = $8028; //Visual attribute (visual_select_group)
   GLX_HYPERPIPE_ID_SGIX			    = $8030;//Associated hyperpipe ID (SGIX_hyperpipe)
   //GLXSyncType enum:
   GLX_SYNC_FRAME_SGIX				    = $00000000	;//ChannelRectSyncSGIX synctype
   GLX_SYNC_SWAP_SGIX				    = $00000001;
   //GLXEventMask enum:
   GLX_BUFFER_CLOBBER_MASK_SGIX			    = $08000000;// SelectEventSGIX mask
   GLX_BUFFER_SWAP_COMPLETE_INTEL_MASK		    = $04000000;
   //GLXHyperpipeTypeMask enum:
   GLX_HYPERPIPE_DISPLAY_PIPE_SGIX		    = $00000001;//SGIX_hyperpipe
   GLX_HYPERPIPE_RENDER_PIPE_SGIX		    = $00000002;
   //GLXHyperpipeAttrib enum:
   GLX_PIPE_RECT_SGIX				    = $00000001;//SGIX_hyperpipe
   GLX_PIPE_RECT_LIMITS_SGIX		  	    = $00000002;
   GLX_HYPERPIPE_STEREO_SGIX			    = $00000003;
   GLX_HYPERPIPE_PIXEL_AVERAGE_SGIX	            = $00000004;
   GLX_HYPERPIPE_PIPE_NAME_LENGTH_SGIX		    = $80      ;// SGIX_hyperpipe
   //GLXBindToTextureTargetMask enum:
   GLX_TEXTURE_1D_BIT_EXT			    = $00000001;// EXT_texture_from_pixmap
   GLX_TEXTURE_2D_BIT_EXT			    = $00000002;
   GLX_TEXTURE_RECTANGLE_BIT_EXT		    = $00000004;
  // OML_swap_method enum:
   GLX_SWAP_METHOD_OML				    = $8060;
   GLX_SWAP_EXCHANGE_OML			    = $8061;
   GLX_SWAP_COPY_OML				    = $8062;
   GLX_SWAP_UNDEFINED_OML			    = $8063;
   //INTEL_swap_event enum:
   GLX_EXCHANGE_COMPLETE_INTEL			    = $8180;
   GLX_COPY_COMPLETE_INTEL			    = $8181;
   GLX_FLIP_COMPLETE_INTEL			    = $8182;
   GLX_COLOR_SAMPLES_NV                             = $20B3;

{$IFDEF SUPPORT_GLX}
type
   TGLXHyperpipeNetworkSGIX = record
      pipeName: array[0..GLX_HYPERPIPE_PIPE_NAME_LENGTH_SGIX-1] of AnsiChar;
      networkId: TGLint;
   end;
   PGLXHyperpipeNetworkSGIX = ^TGLXHyperpipeNetworkSGIX;
   TGLXHyperpipeConfigSGIX = record
      pipeName: array[0..GLX_HYPERPIPE_PIPE_NAME_LENGTH_SGIX-1] of AnsiChar;
      channel: TGLInt;
      participationType: TGLuInt;
      timeSlice: TGLInt;
   end;
   PGLXHyperpipeConfigSGIX = ^TGLXHyperpipeConfigSGIX;
   TGLXPipeRect = record
      pipeName: array[0..GLX_HYPERPIPE_PIPE_NAME_LENGTH_SGIX-1] of AnsiChar;
      srcXOrigin, srcYOrigin, srcWidth, srcHeight: TGLInt;
      destXOrigin, destYOrigin, destWidth, destHeight: TGLInt;
   end;
   PGLXPipeRect = ^TGLXPipeRect;
   TGLXPipeRectLimits = record
      pipeName: array[0..GLX_HYPERPIPE_PIPE_NAME_LENGTH_SGIX-1] of AnsiChar;
      XOrigin, YOrigin, maxHeight, maxWidth: TGLInt;
   end;
   PGLXPipeRectLimits = ^TGLXPipeRectLimits;
{$ENDIF}

{$IFDEF GLS_REGIONS} {$ENDREGION} {$ENDIF}

{$IFDEF GLS_REGIONS} {$REGION 'OpenGL Extension OSX generic constants'} {$ENDIF}
const
   AGL_NONE = 0;
{ choose from all available renderers           }
   AGL_ALL_RENDERERS = 1;
{ depth of the index buffer                     }
   AGL_BUFFER_SIZE = 2;
{ level in plane stacking                       }
   AGL_LEVEL = 3;
{ choose an RGBA format                         }
   AGL_RGBA = 4;
{ double buffering supported                    }
   AGL_DOUBLEBUFFER = 5;
{ stereo buffering supported                    }
   AGL_STEREO = 6;
{ number of aux buffers                         }
   AGL_AUX_BUFFERS = 7;
{ number of red component bits                  }
   AGL_RED_SIZE = 8;
{ number of green component bits                }
   AGL_GREEN_SIZE = 9;
{ number of blue component bits                 }
   AGL_BLUE_SIZE = 10;
{ number of alpha component bits                }
   AGL_ALPHA_SIZE = 11;
{ number of depth bits                          }
   AGL_DEPTH_SIZE = 12;
{ number of stencil bits                        }
   AGL_STENCIL_SIZE = 13;
{ number of red accum bits                      }
   AGL_ACCUM_RED_SIZE = 14;
{ number of green accum bits                    }
   AGL_ACCUM_GREEN_SIZE = 15;
{ number of blue accum bits                     }
   AGL_ACCUM_BLUE_SIZE = 16;
{ number of alpha accum bits                    }
   AGL_ACCUM_ALPHA_SIZE = 17;
{
** Extended attributes
 }
{ frame buffer bits per pixel                   }
   AGL_PIXEL_SIZE = 50;
{ never choose smaller buffers than requested   }
   AGL_MINIMUM_POLICY = 51;
{ choose largest buffers of type requested      }
   AGL_MAXIMUM_POLICY = 52;
{ choose an off-screen capable renderer         }
   AGL_OFFSCREEN = 53;
{ choose a full-screen capable renderer         }
   AGL_FULLSCREEN = 54;
{ number of multi sample buffers                }
   AGL_SAMPLE_BUFFERS_ARB = 55;
{ number of samples per multi sample buffer     }
   AGL_SAMPLES_ARB = 56;
{ independent depth and/or stencil buffers for the aux buffer  }
   AGL_AUX_DEPTH_STENCIL = 57;
{ color buffers store floating point pixels     }
   AGL_COLOR_FLOAT = 58;
{ choose multisample                            }
   AGL_MULTISAMPLE = 59;
{ choose supersample                            }
   AGL_SUPERSAMPLE = 60;
{ request alpha filtering                       }
   AGL_SAMPLE_ALPHA = 61;
{
** Renderer management
 }
{ request renderer by ID                        }
   AGL_RENDERER_ID = 70;
{ choose a single renderer for all screens      }
   AGL_SINGLE_RENDERER = 71;
{ disable all failure recovery systems          }
   AGL_NO_RECOVERY = 72;
{ choose a hardware accelerated renderer        }
   AGL_ACCELERATED = 73;
{ choose the closest color buffer to request    }
   AGL_CLOSEST_POLICY = 74;
{ renderer does not need failure recovery       }
   AGL_ROBUST = 75;
{ back buffer contents are valid after swap     }
   AGL_BACKING_STORE = 76;
{ renderer is multi-processor safe              }
   AGL_MP_SAFE = 78;
{ can be used to render to a window             }
   AGL_WINDOW = 80;
{ single window can span multiple screens       }
   AGL_MULTISCREEN = 81;
{ virtual screen number                         }
   AGL_VIRTUAL_SCREEN = 82;
{ renderer is opengl compliant                  }
   AGL_COMPLIANT = 83;
{ can be used to render to a pbuffer            }
   AGL_PBUFFER = 90;
{ can be used to render offline to a pbuffer	   }
   AGL_REMOTE_PBUFFER = 91;
{
** Property names for aglDescribeRenderer
 }
   AGL_BUFFER_MODES = 100;
   AGL_MIN_LEVEL = 101;
   AGL_MAX_LEVEL = 102;
   AGL_COLOR_MODES = 103;
   AGL_ACCUM_MODES = 104;
   AGL_DEPTH_MODES = 105;
   AGL_STENCIL_MODES = 106;
   AGL_MAX_AUX_BUFFERS = 107;
   AGL_VIDEO_MEMORY = 120;
   AGL_TEXTURE_MEMORY = 121;
   AGL_RENDERER_COUNT = 128;
{
** Integer parameter names
 }
{ Enable or set the swap rectangle               }
   AGL_SWAP_RECT = 200;
{ Enable or set the buffer rectangle             }
   AGL_BUFFER_RECT = 202;
{ Enable or disable the swap async limit         }
   AGL_SWAP_LIMIT = 203;
{ Enable or disable colormap tracking            }
   AGL_COLORMAP_TRACKING = 210;
{ Set a colormap entry to index, r, g, b       }
   AGL_COLORMAP_ENTRY = 212;
{ Enable or disable all rasterization            }
   AGL_RASTERIZATION = 220;
{ 0 -> Don't sync, n -> Sync every n retrace     }
   AGL_SWAP_INTERVAL = 222;
{ Validate state for multi-screen functionality  }
   AGL_STATE_VALIDATION = 230;
{ Set the buffer name. Allows for multi ctx to share a buffer  }
   AGL_BUFFER_NAME = 231;
{ Order the current context in front of all the other contexts.  }
   AGL_ORDER_CONTEXT_TO_FRONT = 232;
{ aglGetInteger only - returns the ID of the drawable surface for the context  }
   AGL_CONTEXT_SURFACE_ID = 233;
{ aglGetInteger only - returns the display ID(s) of all displays touched by the context, up to a maximum of 32 displays  }
   AGL_CONTEXT_DISPLAY_ID = 234;
{ Position of OpenGL surface relative to window: 1 -> Above window, -1 -> Below Window  }
   AGL_SURFACE_ORDER = 235;
{ Opacity of OpenGL surface: 1 -> Surface is opaque (default), 0 -> non-opaque  }
   AGL_SURFACE_OPACITY = 236;
{ Enable or set the drawable clipping region  }
   AGL_CLIP_REGION = 254;
{ Enable the capture of only a single display for aglFullScreen, normally disabled  }
   AGL_FS_CAPTURE_SINGLE = 255;
{ 2 params.   Width/height of surface backing size      }
   AGL_SURFACE_BACKING_SIZE = 304;
{ Enable or disable surface backing size override  }
   AGL_ENABLE_SURFACE_BACKING_SIZE = 305;
{ Flag surface to candidate for deletion  }
   AGL_SURFACE_VOLATILE = 306;
{
** Option names for aglConfigure.
 }
{ Set the size of the pixel format cache         }
   AGL_FORMAT_CACHE_SIZE = 501;
{ Reset the pixel format cache                   }
   AGL_CLEAR_FORMAT_CACHE = 502;
{ Whether to retain loaded renderers in memory   }
   AGL_RETAIN_RENDERERS = 503;
{ buffer_modes  }
   AGL_MONOSCOPIC_BIT = $00000001;
   AGL_STEREOSCOPIC_BIT = $00000002;
   AGL_SINGLEBUFFER_BIT = $00000004;
   AGL_DOUBLEBUFFER_BIT = $00000008;
{ bit depths  }
   AGL_0_BIT = $00000001;
   AGL_1_BIT = $00000002;
   AGL_2_BIT = $00000004;
   AGL_3_BIT = $00000008;
   AGL_4_BIT = $00000010;
   AGL_5_BIT = $00000020;
   AGL_6_BIT = $00000040;
   AGL_8_BIT = $00000080;
   AGL_10_BIT = $00000100;
   AGL_12_BIT = $00000200;
   AGL_16_BIT = $00000400;
   AGL_24_BIT = $00000800;
   AGL_32_BIT = $00001000;
   AGL_48_BIT = $00002000;
   AGL_64_BIT = $00004000;
   AGL_96_BIT = $00008000;
   AGL_128_BIT = $00010000;
{ color modes  }
{ 8 rgb bit/pixel,     RGB=7:0, inverse colormap          }
   AGL_RGB8_BIT = $00000001;
{ 8-8 argb bit/pixel,  A=7:0, RGB=7:0, inverse colormap   }
   AGL_RGB8_A8_BIT = $00000002;
{ 8 rgb bit/pixel,     B=7:6, G=5:3, R=2:0                }
   AGL_BGR233_BIT = $00000004;
{ 8-8 argb bit/pixel,  A=7:0, B=7:6, G=5:3, R=2:0         }
   AGL_BGR233_A8_BIT = $00000008;
{ 8 rgb bit/pixel,     R=7:5, G=4:2, B=1:0                }
   AGL_RGB332_BIT = $00000010;
{ 8-8 argb bit/pixel,  A=7:0, R=7:5, G=4:2, B=1:0         }
   AGL_RGB332_A8_BIT = $00000020;
{ 16 rgb bit/pixel,    R=11:8, G=7:4, B=3:0               }
   AGL_RGB444_BIT = $00000040;
{ 16 argb bit/pixel,   A=15:12, R=11:8, G=7:4, B=3:0      }
   AGL_ARGB4444_BIT = $00000080;
{ 8-16 argb bit/pixel, A=7:0, R=11:8, G=7:4, B=3:0        }
   AGL_RGB444_A8_BIT = $00000100;
{ 16 rgb bit/pixel,    R=14:10, G=9:5, B=4:0              }
   AGL_RGB555_BIT = $00000200;
{ 16 argb bit/pixel,   A=15, R=14:10, G=9:5, B=4:0        }
   AGL_ARGB1555_BIT = $00000400;
{ 8-16 argb bit/pixel, A=7:0, R=14:10, G=9:5, B=4:0       }
   AGL_RGB555_A8_BIT = $00000800;
{ 16 rgb bit/pixel,    R=15:11, G=10:5, B=4:0             }
   AGL_RGB565_BIT = $00001000;
{ 8-16 argb bit/pixel, A=7:0, R=15:11, G=10:5, B=4:0      }
   AGL_RGB565_A8_BIT = $00002000;
{ 32 rgb bit/pixel,    R=23:16, G=15:8, B=7:0             }
   AGL_RGB888_BIT = $00004000;
{ 32 argb bit/pixel,   A=31:24, R=23:16, G=15:8, B=7:0    }
   AGL_ARGB8888_BIT = $00008000;
{ 8-32 argb bit/pixel, A=7:0, R=23:16, G=15:8, B=7:0      }
   AGL_RGB888_A8_BIT = $00010000;
{ 32 rgb bit/pixel,    R=29:20, G=19:10, B=9:0            }
   AGL_RGB101010_BIT = $00020000;
{ 32 argb bit/pixel,   A=31:30  R=29:20, G=19:10, B=9:0   }
   AGL_ARGB2101010_BIT = $00040000;
{ 8-32 argb bit/pixel, A=7:0  R=29:20, G=19:10, B=9:0     }
   AGL_RGB101010_A8_BIT = $00080000;
{ 48 rgb bit/pixel,    R=35:24, G=23:12, B=11:0           }
   AGL_RGB121212_BIT = $00100000;
{ 48 argb bit/pixel,   A=47:36, R=35:24, G=23:12, B=11:0  }
   AGL_ARGB12121212_BIT = $00200000;
{ 64 rgb bit/pixel,    R=47:32, G=31:16, B=15:0           }
   AGL_RGB161616_BIT = $00400000;
{ 64 argb bit/pixel,   A=63:48, R=47:32, G=31:16, B=15:0  }
   AGL_ARGB16161616_BIT = $00800000;
{ 8 bit color look up table (deprecated)                  }
   AGL_INDEX8_BIT = $20000000;
{ 16 bit color look up table (deprecated)				    }
   AGL_INDEX16_BIT = $40000000;
{ 64 rgb bit/pixel,    half float                         }
   AGL_RGBFLOAT64_BIT = $01000000;
{ 64 argb bit/pixel,   half float                         }
   AGL_RGBAFLOAT64_BIT = $02000000;
{ 128 rgb bit/pixel,   ieee float                         }
   AGL_RGBFLOAT128_BIT = $04000000;
{ 128 argb bit/pixel,  ieee float                         }
   AGL_RGBAFLOAT128_BIT = $08000000;
{ 256 rgb bit/pixel,   ieee double                        }
   AGL_RGBFLOAT256_BIT = $10000000;
{ 256 argb bit/pixel,  ieee double                        }
   AGL_RGBAFLOAT256_BIT = $20000000;
{
** Error return values from aglGetError.
 }
{ no error                         }
   AGL_NO_ERROR = 0;
{ invalid pixel format attribute   }
   AGL_BAD_ATTRIBUTE = 10000;
{ invalid renderer property        }
   AGL_BAD_PROPERTY = 10001;
{ invalid pixel format             }
   AGL_BAD_PIXELFMT = 10002;
{ invalid renderer info            }
   AGL_BAD_RENDINFO = 10003;
{ invalid context                  }
   AGL_BAD_CONTEXT = 10004;
{ invalid drawable                 }
   AGL_BAD_DRAWABLE = 10005;
{ invalid graphics device          }
   AGL_BAD_GDEV = 10006;
{ invalid context state            }
   AGL_BAD_STATE = 10007;
{ invalid numerical value          }
   AGL_BAD_VALUE = 10008;
{ invalid share context            }
   AGL_BAD_MATCH = 10009;
{ invalid enumerant                }
   AGL_BAD_ENUM = 10010;
{ invalid offscreen drawable       }
   AGL_BAD_OFFSCREEN = 10011;
{ invalid offscreen drawable       }
   AGL_BAD_FULLSCREEN = 10012;
{ invalid window                   }
   AGL_BAD_WINDOW = 10013;
{ invalid pointer                  }
   AGL_BAD_POINTER = 10014;
{ invalid code module              }
   AGL_BAD_MODULE = 10015;
{ memory allocation failure        }
   AGL_BAD_ALLOC = 10016;
{ invalid CoreGraphics connection  }
   AGL_BAD_CONNECTION = 10017;
{$IFDEF GLS_REGIONS} {$ENDREGION} {$ENDIF}

{$IFDEF GLS_REGIONS} {$REGION 'OpenGL Utility (GLU) generic constants'} {$ENDIF}
   // ********** GLU generic constants **********
const
   // Errors: (return value 0= no error)
   GLU_INVALID_ENUM                                 = 100900;
   GLU_INVALID_VALUE                                = 100901;
   GLU_OUT_OF_MEMORY                                = 100902;
   GLU_INCOMPATIBLE_GL_VERSION                      = 100903;

   // StringName
   GLU_VERSION                                      = 100800;
   GLU_EXTENSIONS                                   = 100801;

   // Boolean
   GLU_TRUE                                         = GL_TRUE;
   GLU_FALSE                                        = GL_FALSE;

   // Quadric constants
   // QuadricNormal
   GLU_SMOOTH                                       = 100000;
   GLU_FLAT                                         = 100001;
   GLU_NONE                                         = 100002;

   // QuadricDrawStyle
   GLU_POINT                                        = 100010;
   GLU_LINE                                         = 100011;
   GLU_FILL                                         = 100012;
   GLU_SILHOUETTE                                   = 100013;

   // QuadricOrientation
   GLU_OUTSIDE                                      = 100020;
   GLU_INSIDE                                       = 100021;

   // Tesselation constants
   GLU_TESS_MAX_COORD                               = 1.0e150;

   // TessProperty
   GLU_TESS_WINDING_RULE                            = 100140;
   GLU_TESS_BOUNDARY_ONLY                           = 100141;
   GLU_TESS_TOLERANCE                               = 100142;

   // TessWinding
   GLU_TESS_WINDING_ODD                             = 100130;
   GLU_TESS_WINDING_NONZERO                         = 100131;
   GLU_TESS_WINDING_POSITIVE                        = 100132;
   GLU_TESS_WINDING_NEGATIVE                        = 100133;
   GLU_TESS_WINDING_ABS_GEQ_TWO                     = 100134;

   // TessCallback
   GLU_TESS_BEGIN                                   = 100100; // TGLUTessBeginProc
   GLU_TESS_VERTEX                                  = 100101; // TGLUTessVertexProc
   GLU_TESS_END                                     = 100102; // TGLUTessEndProc
   GLU_TESS_ERROR                                   = 100103; // TGLUTessErrorProc
   GLU_TESS_EDGE_FLAG                               = 100104; // TGLUTessEdgeFlagProc
   GLU_TESS_COMBINE                                 = 100105; // TGLUTessCombineProc
   GLU_TESS_BEGIN_DATA                              = 100106; // TGLUTessBeginDataProc
   GLU_TESS_VERTEX_DATA                             = 100107; // TGLUTessVertexDataProc
   GLU_TESS_END_DATA                                = 100108; // TGLUTessEndDataProc
   GLU_TESS_ERROR_DATA                              = 100109; // TGLUTessErrorDataProc
   GLU_TESS_EDGE_FLAG_DATA                          = 100110; // TGLUTessEdgeFlagDataProc
   GLU_TESS_COMBINE_DATA                            = 100111; // TGLUTessCombineDataProc

   // TessError
   GLU_TESS_ERROR1                                  = 100151;
   GLU_TESS_ERROR2                                  = 100152;
   GLU_TESS_ERROR3                                  = 100153;
   GLU_TESS_ERROR4                                  = 100154;
   GLU_TESS_ERROR5                                  = 100155;
   GLU_TESS_ERROR6                                  = 100156;
   GLU_TESS_ERROR7                                  = 100157;
   GLU_TESS_ERROR8                                  = 100158;

   GLU_TESS_MISSING_BEGIN_POLYGON                   = GLU_TESS_ERROR1;
   GLU_TESS_MISSING_BEGIN_CONTOUR                   = GLU_TESS_ERROR2;
   GLU_TESS_MISSING_END_POLYGON                     = GLU_TESS_ERROR3;
   GLU_TESS_MISSING_END_CONTOUR                     = GLU_TESS_ERROR4;
   GLU_TESS_COORD_TOO_LARGE                         = GLU_TESS_ERROR5;
   GLU_TESS_NEED_COMBINE_CALLBACK                   = GLU_TESS_ERROR6;

   // NURBS constants

   // NurbsProperty
   GLU_AUTO_LOAD_MATRIX                             = 100200;
   GLU_CULLING                                      = 100201;
   GLU_SAMPLING_TOLERANCE                           = 100203;
   GLU_DISPLAY_MODE                                 = 100204;
   GLU_PARAMETRIC_TOLERANCE                         = 100202;
   GLU_SAMPLING_METHOD                              = 100205;
   GLU_U_STEP                                       = 100206;
   GLU_V_STEP                                       = 100207;

   // NurbsSampling
   GLU_PATH_LENGTH                                  = 100215;
   GLU_PARAMETRIC_ERROR                             = 100216;
   GLU_DOMAIN_DISTANCE                              = 100217;

   // NurbsTrim
   GLU_MAP1_TRIM_2                                  = 100210;
   GLU_MAP1_TRIM_3                                  = 100211;

   // NurbsDisplay
   GLU_OUTLINE_POLYGON                              = 100240;
   GLU_OUTLINE_PATCH                                = 100241;

   // NurbsErrors
   GLU_NURBS_ERROR1                                 = 100251;
   GLU_NURBS_ERROR2                                 = 100252;
   GLU_NURBS_ERROR3                                 = 100253;
   GLU_NURBS_ERROR4                                 = 100254;
   GLU_NURBS_ERROR5                                 = 100255;
   GLU_NURBS_ERROR6                                 = 100256;
   GLU_NURBS_ERROR7                                 = 100257;
   GLU_NURBS_ERROR8                                 = 100258;
   GLU_NURBS_ERROR9                                 = 100259;
   GLU_NURBS_ERROR10                                = 100260;
   GLU_NURBS_ERROR11                                = 100261;
   GLU_NURBS_ERROR12                                = 100262;
   GLU_NURBS_ERROR13                                = 100263;
   GLU_NURBS_ERROR14                                = 100264;
   GLU_NURBS_ERROR15                                = 100265;
   GLU_NURBS_ERROR16                                = 100266;
   GLU_NURBS_ERROR17                                = 100267;
   GLU_NURBS_ERROR18                                = 100268;
   GLU_NURBS_ERROR19                                = 100269;
   GLU_NURBS_ERROR20                                = 100270;
   GLU_NURBS_ERROR21                                = 100271;
   GLU_NURBS_ERROR22                                = 100272;
   GLU_NURBS_ERROR23                                = 100273;
   GLU_NURBS_ERROR24                                = 100274;
   GLU_NURBS_ERROR25                                = 100275;
   GLU_NURBS_ERROR26                                = 100276;
   GLU_NURBS_ERROR27                                = 100277;
   GLU_NURBS_ERROR28                                = 100278;
   GLU_NURBS_ERROR29                                = 100279;
   GLU_NURBS_ERROR30                                = 100280;
   GLU_NURBS_ERROR31                                = 100281;
   GLU_NURBS_ERROR32                                = 100282;
   GLU_NURBS_ERROR33                                = 100283;
   GLU_NURBS_ERROR34                                = 100284;
   GLU_NURBS_ERROR35                                = 100285;
   GLU_NURBS_ERROR36                                = 100286;
   GLU_NURBS_ERROR37                                = 100287;

   // Contours types -- obsolete!
   GLU_CW                                           = 100120;
   GLU_CCW                                          = 100121;
   GLU_INTERIOR                                     = 100122;
   GLU_EXTERIOR                                     = 100123;
   GLU_UNKNOWN                                      = 100124;

   // Names without "TESS_" prefix
   GLU_BEGIN                                        = GLU_TESS_BEGIN;
   GLU_VERTEX                                       = GLU_TESS_VERTEX;
   GLU_END                                          = GLU_TESS_END;
   GLU_ERROR                                        = GLU_TESS_ERROR;
   GLU_EDGE_FLAG                                    = GLU_TESS_EDGE_FLAG;

{$IFDEF GLS_REGIONS} {$ENDREGION} {$ENDIF}

  type
  // core 1.2
  // promoted to core v1.2 from GL_EXT_blend_color (#2)
  PFNGLBLENDCOLORPROC = procedure(red, green, blue, alpha: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v1.2 from GL_EXT_blend_minmax (#37)
  PFNGLBLENDEQUATIONPROC = procedure(mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v1.2 from GL_EXT_draw_range_elements (#112)
  PFNGLDRAWRANGEELEMENTSPROC = procedure(mode: TGLEnum; Astart, Aend: TGLuint; count: TGLsizei; Atype: TGLEnum;
                                indices: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v1.2 from GL_EXT_texture3D (#6)
  PFNGLTEXIMAGE3DPROC = procedure(target: TGLEnum; level: TGLint; internalformat: TGLEnum; width, height, depth: TGLsizei;
                         border: TGLint; format: TGLEnum; Atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXSUBIMAGE3DPROC = procedure(target: TGLEnum; level, xoffset, yoffset, zoffset: TGLint;  width, height, depth: TGLsizei;
                            format: TGLEnum; Atype: TGLEnum; pixels: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v1.2 from GL_EXT_copy_texture
  PFNGLCOPYTEXSUBIMAGE3DPROC = procedure(target: TGLEnum; level, xoffset, yoffset, zoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // core 1.2 deprecated
  // promoted to core v1.2 from GL_SGI_color_table (#14)
  PFNGLCOLORTABLEPROC = procedure(target, internalformat: TGLEnum; width: TGLsizei; format, Atype: TGLEnum;
                         table: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLCOLORTABLEPARAMETERFVPROC = procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLCOLORTABLEPARAMETERIVPROC = procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLCOPYCOLORTABLEPROC = procedure(target, internalformat: TGLEnum; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLGETCOLORTABLEPROC = procedure(target, format, Atype: TGLEnum; table: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLGETCOLORTABLEPARAMETERFVPROC = procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLGETCOLORTABLEPARAMETERIVPROC = procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

  // promoted to core v1.2 from GL_EXT_color_subtable (#74)
  PFNGLCOLORSUBTABLEPROC = procedure(target: TGLEnum; start, count: TGLsizei; format, Atype: TGLEnum; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLCOPYCOLORSUBTABLEPROC = procedure(target: TGLEnum; start: TGLsizei; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

  // promoted to core v1.2 from GL_EXT_convolution (#12)
  PFNGLCONVOLUTIONFILTER1DPROC = procedure(target, internalformat: TGLEnum; width: TGLsizei; format, Atype: TGLEnum;
   image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLCONVOLUTIONFILTER2DPROC = procedure(target, internalformat: TGLEnum; width, height: TGLsizei; format, Atype: TGLEnum;
   image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLCONVOLUTIONPARAMETERFPROC = procedure(target, pname: TGLEnum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLCONVOLUTIONPARAMETERFVPROC = procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLCONVOLUTIONPARAMETERIPROC = procedure(target, pname: TGLEnum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLCONVOLUTIONPARAMETERIVPROC = procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLCOPYCONVOLUTIONFILTER1DPROC = procedure(target, internalformat: TGLEnum; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLCOPYCONVOLUTIONFILTER2DPROC = procedure(target, internalformat: TGLEnum; x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLGETCONVOLUTIONFILTERPROC = procedure(target, internalformat, Atype: TGLEnum; image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLGETCONVOLUTIONPARAMETERFVPROC = procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLGETCONVOLUTIONPARAMETERIVPROC = procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLGETSEPARABLEFILTERPROC = procedure(target, format, Atype: TGLEnum; row, column, span: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSEPARABLEFILTER2DPROC = procedure(target, internalformat: TGLEnum; width, height: TGLsizei; format, Atype: TGLEnum; row,
   column: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

  // promoted to core v1.2 from GL_EXT_histogram (#11)
  PFNGLGETHISTOGRAMPROC = procedure(target: TGLEnum; reset: TGLboolean; format, Atype: TGLEnum; values: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLGETHISTOGRAMPARAMETERFVPROC = procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLGETHISTOGRAMPARAMETERIVPROC = procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLGETMINMAXPROC = procedure(target: TGLEnum; reset: TGLboolean; format, Atype: TGLEnum; values: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLGETMINMAXPARAMETERFVPROC = procedure(target, pname: TGLEnum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLGETMINMAXPARAMETERIVPROC = procedure(target, pname: TGLEnum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLHISTOGRAMPROC = procedure(target: TGLEnum; width: TGLsizei; internalformat: TGLEnum; sink: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMINMAXPROC = procedure(target, internalformat: TGLEnum; sink: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLRESETHISTOGRAMPROC = procedure(target: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLRESETMINMAXPROC = procedure(target: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

  // core 1.3
  // promoted to core v1.3 from GL_ARB_multitexture (#1)
  PFNGLACTIVETEXTUREPROC = procedure(texture: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v1.3 from GL_ARB_multisample (#5)
  PFNGLSAMPLECOVERAGEPROC = procedure(Value: TGLclampf; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v1.3 from GL_ARB_texture_compression (#12)
  PFNGLCOMPRESSEDTEXIMAGE3DPROC = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height, depth: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXIMAGE2DPROC = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXIMAGE1DPROC = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXSUBIMAGE3DPROC = procedure(target: TGLenum; level: TGLint; xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXSUBIMAGE2DPROC = procedure(target: TGLenum; level: TGLint; xoffset, yoffset: TGLint; width, height: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXSUBIMAGE1DPROC = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETCOMPRESSEDTEXIMAGEPROC = procedure(target: TGLenum; level: TGLint; img: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // core 1.3 deprecated
  // promoted to core v1.3 from GL_ARB_multitexture (#1)
  PFNGLCLIENTACTIVETEXTUREPROC = procedure(texture: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1DPROC = procedure(target: TGLenum; s: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1DVPROC = procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1FPROC = procedure(target: TGLenum; s: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1FVPROC = procedure(target: TGLenum; v: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1IPROC = procedure(target: TGLenum; s: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1IVPROC = procedure(target: TGLenum; v: PGLInt); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1SPROC = procedure(target: TGLenum; s: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1SVPROC = procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2DPROC = procedure(target: TGLenum; s, t: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2DVPROC = procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2FPROC = procedure(target: TGLenum; s, t: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2FVPROC = procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2IPROC = procedure(target: TGLenum; s, t: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2IVPROC = procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2SPROC = procedure(target: TGLenum; s, t: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2SVPROC = procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3DPROC = procedure(target: TGLenum; s, t, r: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3DVPROC = procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3FPROC = procedure(target: TGLenum; s, t, r: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3FVPROC = procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3IPROC = procedure(target: TGLenum; s, t, r: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3IVPROC = procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3SPROC = procedure(target: TGLenum; s, t, r: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3SVPROC = procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4DPROC = procedure(target: TGLenum; s, t, r, q: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4DVPROC = procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4FPROC = procedure(target: TGLenum; s, t, r, q: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4FVPROC = procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4IPROC = procedure(target: TGLenum; s, t, r, q: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4IVPROC = procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4SPROC = procedure(target: TGLenum; s, t, r, q: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4SVPROC = procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

  // promoted to core v1.3 from GL_ARB_transpose_matrix
  PFNGLLOADTRANSPOSEMATRIXFPROC = procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLLOADTRANSPOSEMATRIXDPROC = procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTTRANSPOSEMATRIXFPROC = procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTTRANSPOSEMATRIXDPROC = procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

  // core 1.4
  // promoted to core v1.4 from GL_EXT_blend_func_separate (#173)
  PFNGLBLENDFUNCSEPARATEPROC = procedure(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v1.4 from GL_EXT_multi_draw_arrays (#148)
  PFNGLMULTIDRAWARRAYSPROC = procedure(mode: TGLenum; First: PGLint; Count: PGLsizei; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTIDRAWELEMENTSPROC = procedure(mode: TGLenum; Count: PGLsizei; AType: TGLenum; var indices; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v1.4 from GL_ARB_point_parameters (#14), GL_NV_point_sprite (#262)
  PFNGLPOINTPARAMETERFPROC = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPOINTPARAMETERFVPROC = procedure(pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPOINTPARAMETERIPROC = procedure(pname: TGLenum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPOINTPARAMETERIVPROC = procedure(pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // core 1.4 deprecated
  // promoted to core v1.4 from GL_EXT_fog_coord (#149)
  PFNGLFOGCOORDFPROC = procedure(coord: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLFOGCOORDFVPROC = procedure(coord: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLFOGCOORDDPROC = procedure(coord: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLFOGCOORDDVPROC = procedure(coord: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLFOGCOORDPOINTERPROC = procedure(AType: TGLenum; stride: TGLsizei; p: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

  // promoted to core v1.4 from GL_EXT_secondary_color (#145)
  PFNGLSECONDARYCOLOR3BPROC = procedure(red, green, blue: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3BVPROC = procedure(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3DPROC = procedure(red, green, blue: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3DVPROC = procedure(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3FPROC = procedure(red, green, blue: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3FVPROC = procedure(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3IPROC = procedure(red, green, blue: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3IVPROC = procedure(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3SPROC = procedure(red, green, blue: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3SVPROC = procedure(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3UBPROC = procedure(red, green, blue: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3UBVPROC = procedure(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3UIPROC = procedure(red, green, blue: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3UIVPROC = procedure(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3USPROC = procedure(red, green, blue: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3USVPROC = procedure(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLORPOINTERPROC = procedure(Size: TGLint; Atype: TGLenum; stride: TGLsizei; p: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

  // promoted to core v1.4 from GL_ARB_window_pos (#25)
  PFNGLWINDOWPOS2DPROC = procedure(x,y : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS2DVPROC = procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS2FPROC = procedure(x,y : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS2FVPROC = procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS2IPROC = procedure(x,y : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS2IVPROC = procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS2SPROC = procedure(x,y : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS2SVPROC = procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3DPROC = procedure(x,y,z : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3DVPROC = procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3FPROC = procedure(x,y,z : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3FVPROC = procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3IPROC = procedure(x,y,z : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3IVPROC = procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3SPROC = procedure(x,y,z : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3SVPROC = procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} //deprecated;

  // core 1.5
  // promoted to core v1.5 from GL_ARB_occlusion_query (#29)
  PFNGLGENQUERIESPROC = procedure(n: TGLsizei; ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETEQUERIESPROC = procedure(n: TGLsizei; const ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISQUERYPROC = function(id: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBEGINQUERYPROC = procedure(target: TGLenum; id: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLENDQUERYPROC = procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETQUERYIVPROC = procedure(target: TGLEnum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETQUERYOBJECTIVPROC = procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETQUERYOBJECTUIVPROC = procedure(id: TGLuint; pname: TGLenum; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v1.5 from GL_ARB_vertex_buffer_object (#28)
  PFNGLBINDBUFFERPROC = procedure(target: GLenum; buffer: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETEBUFFERSPROC = procedure(n: GLsizei; const buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGENBUFFERSPROC = procedure(n: GLsizei; buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISBUFFERPROC = function(buffer: GLuint): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBUFFERDATAPROC = procedure(target: GLenum; size: GLsizei; const data: Pointer; usage: GLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBUFFERSUBDATAPROC = procedure(target: GLenum; offset: GLuint; size: GLsizei; const data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETBUFFERSUBDATAPROC = procedure(target: GLenum; offset: GLuint; size: GLsizei; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMAPBUFFERPROC = function(target: GLenum; access: GLenum): Pointer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNMAPBUFFERPROC = function(target: GLenum): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETBUFFERPARAMETERIVPROC = procedure(target: GLenum; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETBUFFERPOINTERVPROC = procedure(target: GLenum; pname: GLenum; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // core 2.0
  // promoted to core v2.0 from GL_EXT_blend_equation_separate (#299)
  PFNGLBLENDEQUATIONSEPARATEPROC = procedure(modeRGB: TGLenum; modeAlpha: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v2.0 from GL_ARB_draw_buffers (#37)
  PFNGLDRAWBUFFERSPROC = procedure(n: GLSizei; const bufs: PGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v2.0 from GL_ARB_stencil_two_side (no # found)
  PFNGLSTENCILOPSEPARATEPROC = procedure(face, sfail, dpfail, dppass: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSTENCILFUNCSEPARATEPROC = procedure(face, func: TGLenum; ref: TGLint; mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSTENCILMASKSEPARATEPROC = procedure(face: TGLenum; mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v2.0 from GL_ARB_shader_objects (#30) / GL_ARB_vertex_shader (#31) / GL_ARB_fragment_shader (#32)
  PFNGLATTACHSHADERPROC = procedure(_program: TGLuint; shader: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDATTRIBLOCATIONPROC = procedure(_program: TGLuint; index: TGLuint; const name: PGLChar);
  PFNGLCOMPILESHADERPROC = procedure(shader: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCREATEPROGRAMPROC = function(): TGLuint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCREATESHADERPROC = function(_type: TGLenum): TGLuint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETEPROGRAMPROC = procedure(_program: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETESHADERPROC = procedure(shader: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDETACHSHADERPROC = procedure(_program: TGLuint; shader: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDISABLEVERTEXATTRIBARRAYPROC = procedure(index: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLENABLEVERTEXATTRIBARRAYPROC = procedure(index: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETACTIVEATTRIBPROC = procedure(_program: TGLuint; index: TGLuint; bufSize: TGLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETACTIVEUNIFORMPROC = procedure(_program: TGLuint; index: TGLuint; bufSize: TGLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETATTACHEDSHADERSPROC = procedure(_program: TGLuint; maxCount: TGLsizei; count: PGLSizei; obj: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETATTRIBLOCATIONPROC = function(_program: TGLuint; const name: PGLChar): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMIVPROC = procedure(_program: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMINFOLOGPROC = procedure(_program: TGLuint; bufSize: TGLsizei; length: PGLsizei; infoLog: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETSHADERIVPROC = procedure(shader: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETSHADERINFOLOGPROC = procedure(shader: TGLuint; bufSize: TGLsizei; length: PGLsizei; infoLog: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETSHADERSOURCEPROC = procedure(shader:TGLuint; bufSize: TGLsizei; length: PGLsizei; source: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETUNIFORMLOCATIONPROC = function(_program: TGLuint; const name: PGLChar): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETUNIFORMFVPROC = procedure(_program: TGLuint; location: TGLint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETUNIFORMIVPROC = procedure(_program: TGLuint; location: TGLint; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBDVPROC = procedure(index:TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBFVPROC = procedure(index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBIVPROC = procedure(index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBPOINTERVPROC = procedure(index: TGLuint; pname: TGLenum; _pointer:pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISPROGRAMPROC = function(_program: TGLuint):TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISSHADERPROC = function(shader: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLLINKPROGRAMPROC = procedure(_program: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSHADERSOURCEPROC = procedure(shader: TGLuint; count: TGLsizei; const _string: PGLPCharArray; const length: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUSEPROGRAMPROC = procedure(_program: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM1FPROC = procedure(location: GLint; v0: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM2FPROC = procedure(location: GLint; v0: GLfloat; v1: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM3FPROC = procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM4FPROC = procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM1IPROC = procedure(location: GLint; v0: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM2IPROC = procedure(location: GLint; v0: GLint; v1: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM3IPROC = procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM4IPROC = procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM1FVPROC = procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM2FVPROC = procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM3FVPROC = procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM4FVPROC = procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM1IVPROC = procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM2IVPROC = procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM3IVPROC = procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM4IVPROC = procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX2FVPROC = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX3FVPROC = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX4FVPROC = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVALIDATEPROGRAMPROC = procedure(_program: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1DPROC = procedure(index:TGLuint; x: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1DVPROC = procedure(index:TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1FPROC = procedure(index:TGLuint; x: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1FVPROC = procedure(index:TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1SPROC = procedure(index:TGLuint; x: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1SVPROC = procedure(index:TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2DPROC = procedure(index:TGLuint; x,y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2DVPROC = procedure(index:TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2FPROC = procedure(index:TGLuint; x,y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2FVPROC = procedure(index:TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2SPROC = procedure(index:TGLuint; x,y: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2SVPROC = procedure(index:TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3DPROC = procedure(index:TGLuint; x,y,z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3DVPROC = procedure(index:TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3FPROC = procedure(index:TGLuint; x,y,z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3FVPROC = procedure(index:TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3SPROC = procedure(index:TGLuint; x,y,z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3SVPROC = procedure(index:TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NBVPROC = procedure(index:TGLuint; v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NIVPROC = procedure(index:TGLuint; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NSVPROC = procedure(index:TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUBPROC = procedure(index:TGLuint; x,y,z,w: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUBVPROC = procedure(index:TGLuint; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUIVPROC = procedure(index:TGLuint; v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUSVPROC = procedure(index:TGLuint; v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4BVPROC = procedure(index:TGLuint; v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4DPROC = procedure(index:TGLuint; x,y,z,w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4DVPROC = procedure(index:TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4FPROC = procedure(index:TGLuint; x,y,z,w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4FVPROC = procedure(index:TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4IVPROC = procedure(index:TGLuint; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4SPROC = procedure(index:TGLuint; x,y,z,w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4SVPROC = procedure(index:TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4UBVPROC = procedure(index:TGLuint; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4UIVPROC = procedure(index:TGLuint; v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4USVPROC = procedure(index:TGLuint; v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBPOINTERPROC = procedure(index:TGLuint; size: TGLint; _type: TGLenum; normalized: TGLboolean; stride:TGLsizei; _pointer:pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // core 2.1
  // new commands in OpenGL 2.1
  PFNGLUNIFORMMATRIX2X3FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX3X2FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX2X4FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX4X2FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX3X4FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX4X3FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // Core 3.0
  // promoted to core v3.0 from GL_EXT_gpu_shader4
  PFNGLVERTEXATTRIBI1IPROC = procedure(index: TGLuint; x: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2IPROC = procedure(index: TGLuint; x: TGLint; y: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3IPROC = procedure(index: TGLuint; x: TGLint; y: TGLint; z: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4IPROC = procedure(index: TGLuint; x: TGLint; y: TGLint; z: TGLint; w: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI1UIPROC = procedure(index: TGLuint; x: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2UIPROC = procedure(index: TGLuint; x: TGLuint; y: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3UIPROC = procedure(index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4UIPROC = procedure(index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint; w: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI1IVPROC = procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2IVPROC = procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3IVPROC = procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4IVPROC = procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI1UIVPROC = procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2UIVPROC = procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3UIVPROC = procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4UIVPROC = procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4BVPROC = procedure(index: TGLuint; v:PGLbyte);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4SVPROC = procedure(index: TGLuint; v:PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4UBVPROC = procedure(index: TGLuint; v: PGLUbyte);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4USVPROC = procedure(index: TGLuint; v: PGLushort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBIPOINTERPROC = procedure(index: TGLuint; size: TGLint; _type: TGLenum;
                              stride: TGLsizei; _pointer: pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBIIVPROC = procedure(index: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBIUIVPROC = procedure(index: TGLuint; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM1UIPROC = procedure(location: TGLInt; v0: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM2UIPROC = procedure(location: TGLInt; v0: TGLuint; v1: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM3UIPROC = procedure(location: TGLInt; v0: TGLuint; v1: TGLuint; v2: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM4UIPROC = procedure(location: TGLInt; v0: TGLuint; v1: TGLuint; v2: TGLuint; v3: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM1UIVPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM2UIVPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM3UIVPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM4UIVPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETUNIFORMUIVPROC = procedure(_program: TGLuint; location: TGLint; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDFRAGDATALOCATIONPROC = procedure(_program: TGLuint; colorNumber: TGLuint; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETFRAGDATALOCATIONPROC = function(_program: TGLuint; name: PGLChar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v3.0 from GL_NV_conditional_render
  PFNGLBEGINCONDITIONALRENDERPROC = procedure(id: TGLuint; mode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLENDCONDITIONALRENDERPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v3.0 from GL_ARB_color_buffer_float
  PFNGLCLAMPCOLORPROC = procedure (target: TGLenum; clamp: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v3.0 from GL_EXT_texture_integer
  PFNGLTEXPARAMETERIIVPROC = procedure(target: TGLenum; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXPARAMETERIUIVPROC = procedure(target: TGLenum; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETTEXPARAMETERIIVPROC = procedure(target: TGLenum; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETTEXPARAMETERIUIVPROC = procedure(target: TGLenum; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v3.0 from GL_EXT_draw_buffers2
  PFNGLCOLORMASKIPROC = procedure(index: TGLuint; r: TGLboolean; g: TGLboolean;
                          b: TGLboolean; a: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETBOOLEANI_VPROC = procedure(target: TGLenum; index: TGLuint; data: PGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETINTEGERI_VPROC = procedure(target: TGLenum; index: TGLuint; data: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLENABLEIPROC = procedure(target: TGLenum; index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDISABLEIPROC = procedure(target: TGLenum; index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISENABLEDIPROC = function(target: TGLenum; index: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  //promoted to core v3.0 from GL_EXT_transform_feedback
  PFNGLBINDBUFFERRANGEPROC = procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                          offset:TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDBUFFERBASEPROC = procedure(target: TGLenum; index: TGLuint; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBEGINTRANSFORMFEEDBACKPROC = procedure(primitiveMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLENDTRANSFORMFEEDBACKPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTRANSFORMFEEDBACKVARYINGSPROC = procedure(_program: TGLuint; count: TGLsizei;
                                    const varyings: PGLPCharArray; bufferMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETTRANSFORMFEEDBACKVARYINGPROC = procedure(_program: TGLuint; index: TGLuint;
   bufSize: TGLsizei; length: PGLsizei; size: PGLsizei; _type: PGLenum; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // New commands in OpenGL 3.0
  PFNGLCLEARBUFFERIVPROC = procedure(buffer: TGLenum; drawbuffer: TGLint; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCLEARBUFFERUIVPROC = procedure(buffer: TGLenum; drawbuffer: TGLint; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCLEARBUFFERFVPROC = procedure(buffer: TGLenum; drawbuffer: TGLint; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCLEARBUFFERFIPROC = procedure(buffer: TGLenum; drawbuffer: TGLint; depth: TGLfloat; stencil: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETSTRINGIPROC = function(name: TGLenum; index: TGLuint): PGLChar;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // Core 3.1
  // New commands in OpenGL 3.1
  PFNGLDRAWARRAYSINSTANCEDPROC = procedure(mode: TGLenum; first: TGLint; count: TGLsizei; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDRAWELEMENTSINSTANCEDPROC = procedure(mode: TGLenum; count: TGLsizei; _type: TGLenum; indices: PGLvoid; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXBUFFERPROC = procedure(target: TGLenum; internalformat: TGLenum; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPRIMITIVERESTARTINDEXPROC = procedure(index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // Core 3.2
  PFNGLGETINTEGER64I_VPROC = procedure(target: TGLenum; index: TGLuint; data: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETBUFFERPARAMETERI64VPROC = procedure(target: TGLenum; pname: TGLenum; params: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTUREPROC = procedure(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // Core 3.3
  PFNGLVERTEXATTRIBDIVISORPROC = procedure(index: TGLuint; divisor: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // Core 4.0
  // promoted to core v4.0 from GL_ARB_draw_buffers_blend (ARB #69)
  PFNGLBLENDEQUATIONIPROC = procedure(buf: TGLuint; mode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBLENDEQUATIONSEPARATEIPROC = procedure(buf: TGLuint; modeRGB: TGLenum; modeAlpha: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBLENDFUNCIPROC = procedure(buf: TGLuint; src: TGLenum; dst: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBLENDFUNCSEPARATEIPROC = procedure(buf: TGLuint; srcRGB: TGLenum; dstRGB: TGLenum;
                             srcAlpha: TGLenum; dstAlpha: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // promoted to core v4.0 from GL_ARB_sample_shading (ARB #70)
  PFNGLMINSAMPLESHADINGPROC = procedure(value: TGLclampf);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GLU extensions (might not be same naming as c versions?)
  PFNGLUNURBSCALLBACKDATAEXTPROC = procedure(nurb: PGLUnurbs; userData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNEWNURBSTESSELLATOREXTPROC = function: PGLUnurbs; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUDELETENURBSTESSELLATOREXTPROC = procedure(nurb: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  {$IFDEF SUPPORT_WGL}
  // WGL
  // WGL - ARB
  // WGL_buffer_region (ARB #4)
  PFNWGLCREATEBUFFERREGIONARBPROC = function(DC: HDC; iLayerPlane: Integer; uType: TGLenum) : Integer; stdcall;
  PFNWGLDELETEBUFFERREGIONARBPROC = procedure(hRegion: Integer); stdcall;
  PFNWGLSAVEBUFFERREGIONARBPROC = function(hRegion: Integer; x, y, width, height: Integer): BOOL; stdcall;
  PFNWGLRESTOREBUFFERREGIONARBPROC = function(hRegion: Integer; x, y, width, height: Integer;
   xSrc, ySrc: Integer): BOOL; stdcall;

  // WGL_ARB_extensions_string (ARB #8)
  PFNWGLGETEXTENSIONSSTRINGARBPROC = function(DC: HDC): PGLChar; stdcall;

  // WGL_ARB_pixel_format (ARB #9)
  PFNWGLGETPIXELFORMATATTRIBIVARBPROC = function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: TGLenum;
   const piAttributes: PGLint; piValues : PGLint) : BOOL; stdcall;
  PFNWGLGETPIXELFORMATATTRIBFVARBPROC = function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: TGLenum;
   const piAttributes: PGLint; piValues: PGLFloat) : BOOL; stdcall;
  PFNWGLCHOOSEPIXELFORMATARBPROC = function(DC: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLFloat;
   nMaxFormats: GLuint; piFormats: PGLint; nNumFormats: PGLenum) : BOOL; stdcall;

  // WGL_make_current_read (ARB #10)
  PFNWGLMAKECONTEXTCURRENTARBPROC = function(hDrawDC: HDC; hReadDC: HDC; _hglrc: HGLRC): BOOL; stdcall;
  PFNWGLGETCURRENTREADDCARBPROC = function(): HDC; stdcall;

  // WGL_ARB_pbuffer (ARB #11)
  PFNWGLCREATEPBUFFERARBPROC = function(DC: HDC; iPixelFormat: GLInt; iWidth, iHeight : GLInt;
   const piAttribList: PGLint) : HPBUFFERARB; stdcall;
  PFNWGLGETPBUFFERDCARBPROC = function(hPbuffer: HPBUFFERARB) : HDC; stdcall;
  PFNWGLRELEASEPBUFFERDCARBPROC = function(hPbuffer: HPBUFFERARB; DC: HDC) : Integer; stdcall;
  PFNWGLDESTROYPBUFFERARBPROC = function(hPbuffer: HPBUFFERARB): BOOL; stdcall;
  PFNWGLQUERYPBUFFERARBPROC = function(hPbuffer: HPBUFFERARB; iAttribute : Integer;
   piValue: PGLint) : BOOL; stdcall;

  // WGL_ARB_render_texture (ARB #20)
  PFNWGLBINDTEXIMAGEARBPROC = function(hPbuffer: HPBUFFERARB; iBuffer: Integer): BOOL; stdcall;
  PFNWGLRELEASETEXIMAGEARBPROC = function(hpBuffer: HPBUFFERARB; iBuffer: Integer): BOOL; stdcall;
  PFNWGLSETPBUFFERATTRIBARBPROC = function(hpBuffer: HPBUFFERARB; const piAttribList: PGLint): BOOL; stdcall;

  // WGL_ARB_create_context (ARB #55)
  PFNWGLCREATECONTEXTATTRIBSARBPROC = function(DC: HDC; hShareContext: HGLRC;
           attribList: PGLint):HGLRC; stdcall;

  // WGL - EXT
  // WGL_EXT_swap_control (EXT #172)
  PFNWGLSWAPINTERVALEXTPROC = function(interval : Integer) : BOOL; stdcall;
  PFNWGLGETSWAPINTERVALEXTPROC = function : Integer; stdcall;

  // WGL_NV_gpu_affinity (EXT #355)
  PFNWGLENUMGPUSNVPROC = function(iGpuIndex: Cardinal; var hGpu: HGPUNV): Boolean; stdcall;
  PFNWGLENUMGPUDEVICESNVPROC = function(hGpu: HGPUNV; iDeviceIndex: Cardinal; lpGpuDevice: PGPUDevice): Boolean; stdcall;
  PFNWGLCREATEAFFINITYDCNVPROC = function(hGpuList: PHGPUNV): HDC; stdcall;
  PFNWGLENUMGPUSFROMAFFINITYDCNVPROC = function(hAffinityDC: HDC; iGpuIndex: Cardinal; var hGpu: HGPUNV): Boolean; stdcall;
  PFNWGLDELETEDCNVPROC = function(hdc: HDC): Boolean; stdcall;

  {$ENDIF}

  {$IFDEF SUPPORT_GLX}
  // GLX
  // GLX 1.3+
  PFNGLXCHOOSEFBCONFIGPROC = function(dpy: PDisplay; screen: TGLInt; attribList: PGLInt; nitems: PGLInt): GLXFBConfig; cdecl;
  PFNGLXGETFBCONFIGATTRIBPROC = function(dpy: PDisplay; config: GLXFBConfig; attribute: TGLInt; value: PGLInt): TGLInt; cdecl;
  PFNGLXGETFBCONFIGSPROC = function(dpy: PDisplay; screen: TGLInt; nelements: PGLInt): GLXFBConfig; cdecl;
  PFNGLXGETVISUALFROMFBCONFIGPROC = function(dpy: PDisplay; config: GLXFBConfig): PXVisualInfo; cdecl;
  PFNGLXCREATEWINDOWPROC = function(dpy: PDisplay; config: GLXFBConfig; win: GLXWindow; const attribList: PGLInt): GLXWindow; cdecl;
  PFNGLXDESTROYWINDOWPROC = procedure(dpy: PDisplay; window: GLXWindow); cdecl;
  PFNGLXCREATEPIXMAPPROC = function(dpy: PDisplay; config: GLXFBConfig; pixmap: GLXPixmap; attribList: PGLInt): GLXPixmap; cdecl;
  PFNGLXDESTROYPIXMAPPROC = procedure(dpy: PDisplay; pixmap: GLXPixmap); cdecl;
  PFNGLXCREATEPBUFFERPROC = function(dpy: PDisplay; config: GLXFBConfig; attribList: PGLInt): GLXPBuffer; cdecl;
  PFNGLXDESTROYPBUFFERPROC = procedure(dpy: PDisplay; pbuf: GLXPBuffer); cdecl;
  PFNGLXQUERYDRAWABLEPROC = procedure(dpy: PDisplay; draw: GLXDrawable; attribute: TGLInt; value: PGLuint); cdecl;
  PFNGLXCREATENEWCONTEXTPROC = function(dpy: PDisplay; config: GLXFBConfig; renderType: TGLInt; shareList: GLXContext; direct: TGLboolean): GLXContext; cdecl;
  PFNGLXMAKECONTEXTCURRENTPROC = function(dpy: PDisplay; draw: GLXDrawable; read: GLXDrawable; ctx: GLXContext): TGLboolean; cdecl;
  PFNGLXGETCURRENTREADDRAWABLEPROC = function: GLXDrawable; cdecl;
  PFNGLXQUERYCONTEXTPROC = function(dpy: PDisplay; ctx: GLXContext; attribute: TGLInt; value: PGLInt): TGLInt; cdecl;
  PFNGLXSELECTEVENTPROC = procedure(dpy: PDisplay; drawable: GLXDrawable; mask: TGLsizei); cdecl;
  PFNGLXGETSELECTEDEVENTPROC = procedure(dpy: PDisplay; drawable: GLXDrawable; mask: TGLsizei); cdecl;
  PFNGLXBINDTEXIMAGEARBPROC = function(dpy: PDisplay; pbuffer: GLXPbuffer; buffer: TGLInt): TGLboolean; cdecl;
  PFNGLXRELEASETEXIMAGEARBPROC = function(dpy: PDisplay; pbuffer: GLXPbuffer; buffer: TGLint): TGLboolean; cdecl;
  PFNGLXDRAWABLEATTRIBARBPROC = function(dpy: PDisplay; draw: GLXDrawable; const attribList:PGLInt): TGLboolean; cdecl;

  // 1.4
  // GLX_ARB_create_context (EXT #56)
  PFNGLXCREATECONTEXTATTRIBSARBPROC = function(dpy: PDisplay; config: GLXFBConfig;
      share_context: GLXContext; direct: TGLBoolean;
      attrib_list: PGLint): GLXContext; cdecl;
  PFNGLXGETPROCADDRESSPROC = function(const name: PAnsiChar): pointer; cdecl;
  PFNGLXGETPROCADDRESSARBPROC = function (const name: PAnsiChar): pointer; cdecl;

  // GLX_SGI_swap_control (EXT #40)
  PFNGLXSWAPINTERVALSGIPROC = function(interval: TGLint): TGLint; cdecl;
  PFNGLXGETVIDEOSYNCSGIPROC = function(count: PGLuint): TGLInt; cdecl;
  PFNGLXWAITVIDEOSYNCSGIPROC = function(divisor: TGLInt; remainder: TGLInt; count: PGLuint): TGLInt; cdecl;
  PFNGLXFREECONTEXTEXTPROC = procedure(dpy: PDisplay; context: GLXContext); cdecl;
  PFNGLXGETCONTEXTIDEXTPROC = function(const context: GLXContext): GLXContextID; cdecl;
  PFNGLXGETCURRENTDISPLAYEXTPROC = function: PDisplay; cdecl;
  PFNGLXIMPORTCONTEXTEXTPROC = function(dpy: PDisplay; contextID: GLXContextID): GLXContext; cdecl;
  PFNGLXQUERYCONTEXTINFOEXTPROC = function(dpy: PDisplay; context: GLXContext; attribute: TGLInt; value: PGLInt): TGLInt; cdecl;
  PFNGLXCOPYSUBBUFFERMESAPROC = procedure(dpy: PDisplay; drawable: GLXDrawable; x: TGLInt; y: TGLInt; width: TGLInt; height: TGLInt); cdecl;
  PFNGLXCREATEGLXPIXMAPMESAPROC = function(dpy: PDisplay; visual: PXVisualInfo; pixmap: XPixmap; cmap: XColormap): GLXPixmap; cdecl;
  PFNGLXRELEASEBUFFERSMESAPROC = function(dpy: PDisplay; d: GLXDrawable): TGLboolean; cdecl;
  PFNGLXSET3DFXMODEMESAPROC = function(mode: TGLint): TGLboolean; cdecl;

  PFNGLXBINDTEXIMAGEEXTPROC = procedure(dpy: PDisplay; drawable: GLXDrawable; buffer: GLint; const attrib_list: PGLint); cdecl;
  PFNGLXRELEASETEXIMAGEEXTPROC = procedure(dpy: PDisplay; drawable: GLXDrawable; buffer: GLint); cdecl;

  //GLX 1.4
  PFNGLXMAKECURRENTREADSGIPROC = function(dpy: PDisplay; draw: GLXDrawable; read: GLXDrawable; ctx: GLXContext): TGLboolean; cdecl;
  PFNGLXGETCURRENTREADDRAWABLESGIPROC = function: GLXDrawable; cdecl;
  PFNGLXGETFBCONFIGATTRIBSGIXPROC = function(dpy: PDisplay; config: GLXFBConfigSGIX; attribute: TGLInt; value: PGLInt):TGLInt; cdecl;
  PFNGLXCHOOSEFBCONFIGSGIXPROC = function(dpy: PDisplay; screen: TGLInt; attribList: PGLInt; nitems: PGLInt): GLXFBConfigSGIX; cdecl;
  PFNGLXCREATEGLXPIXMAPWITHCONFIGSGIXPROC = function(dpy: PDisplay; config:GLXFBConfigSGIX;  pixmap: GLXPixmap): GLXPixmap; cdecl;
  PFNGLXCREATECONTEXTWITHCONFIGSGIXPROC = function(dpy: PDisplay; config: GLXFBConfigSGIX; renderType: TGLInt; shareList: GLXContext; direct: TGLboolean): GLXContext; cdecl;
  PFNGLXGETVISUALFROMFBCONFIGSGIXPROC = function(dpy: PDisplay; config: GLXFBConfigSGIX): PXVisualInfo; cdecl;
  PFNGLXGETFBCONFIGFROMVISUALSGIXPROC = function(dpy: PDisplay; vis: PXVisualInfo): GLXFBConfigSGIX; cdecl;
  PFNGLXCREATEGLXPBUFFERSGIXPROC = function(dpy: PDisplay; config: GLXFBConfigSGIX; width:PGLuint;  height: PGLuint; attribList: PGLInt): GLXPBufferSGIX; cdecl;
  PFNGLXDESTROYGLXPBUFFERSGIXPROC = procedure(dpy: PDisplay; pbuf: GLXFBConfigSGIX); cdecl;
  PFNGLXQUERYGLXPBUFFERSGIXPROC = function(dpy: PDisplay; pbuf: GLXFBConfigSGIX; attribute: PGLInt; value: PGLuint): TGLInt; cdecl;
  PFNGLXSELECTEVENTSGIXPROC = procedure(dpy: PDisplay; drawable: GLXDrawable; mask: PGLuint64); cdecl;
  PFNGLXGETSELECTEDEVENTSGIXPROC = procedure(dpy: PDisplay; drawable: GLXDrawable; mask: PGLuint64); cdecl;
  PFNGLXCUSHIONSGIPROC = procedure(dpy: PDisplay; window: TWindow; cushion: TGLfloat); cdecl;
  PFNGLXBINDCHANNELTOWINDOWSGIXPROC = function(dpy: PDisplay; screen: TGLInt; channel: TGLInt; window: TWindow): TGLInt; cdecl;
  PFNGLXCHANNELRECTSGIXPROC = function (dpy: PDisplay; screen: TGLInt; channel:TGLInt; x, y, w, h: TGLInt): TGLInt; cdecl;
  PFNGLXQUERYCHANNELRECTSGIXPROC = function (dpy: PDisplay; screen: TGLInt; channel:TGLInt; dx, dy, dw, dh: TGLInt): TGLInt; cdecl;
  PFNGLXQUERYCHANNELDELTASSGIXPROC = function (dpy: PDisplay; screen: TGLInt; channel:TGLInt; x, y, w, h: TGLInt): TGLInt; cdecl;
  PFNGLXCHANNELRECTSYNCSGIXPROC = function (dpy: PDisplay; screen: TGLInt; channel: TGLInt; synctype: TGLEnum): TGLInt; cdecl;
  PFNGLXJOINSWAPGROUPSGIXPROC = procedure (dpy: PDisplay; drawable: GLXDrawable; member: GLXDrawable); cdecl;
  PFNGLXBINDSWAPBARRIERSGIXPROC = procedure (dpy: PDisplay; drawable: GLXDrawable; barrier: TGLInt); cdecl;
  PFNGLXQUERYMAXSWAPBARRIERSSGIXPROC = procedure (dpy: PDisplay; screen: TGLInt; max: TGLInt); cdecl;
  PFNGLXQUERYHYPERPIPENETWORKSGIXPROC = function (dpy: PDisplay; npipes:PGLint): PGLXHyperpipeNetworkSGIX; cdecl;
  PFNGLXHYPERPIPECONFIGSGIXPROC = function(dpy: PDisplay; networkId, npipes: PGLint; cfg: PGLXHyperpipeConfigSGIX; hpId: PGLInt): TGLint; cdecl;
  PFNGLXQUERYHYPERPIPECONFIGSGIXPROC = function(dpy: PDisplay; hpId: TGLInt; npipes: PGLInt): PGLXHyperpipeConfigSGIX; cdecl;
  PFNGLXDESTROYHYPERPIPECONFIGSGIXPROC = function(dpy: PDisplay; hpId:TGLint): PGLInt; cdecl;
  PFNGLXBINDHYPERPIPESGIXPROC = function(dpy: PDisplay; hpId: PGLint): PGLInt; cdecl;
  PFNGLXQUERYHYPERPIPEBESTATTRIBSGIXPROC = function(dpy: PDisplay; timeSlice: TGLint; attrib: TGLint; size: TGLint; attribList: TGLint; returnAttribList: TGLint): TGLint; cdecl;
  PFNGLXHYPERPIPEATTRIBSGIXPROC = function(dpy: PDisplay; timeSlice: TGLint; attrib: TGLint; size: TGLint; attribList: TGLint): TGLint; cdecl;
  PFNGLXQUERYHYPERPIPEATTRIBSGIXPROC = function(dpy: PDisplay; timeSlice: TGLint; attrib: TGLint; size: TGLint; returnAttribList: TGLint): TGLint; cdecl;
  PFNGLXGETAGPOFFSETMESAPROC = function(param: Pointer): PGLInt;cdecl;
  PFNGLXENUMERATEVIDEODEVICESNVPROC = function(dpy: PDisplay; screen: TGLInt; nelements: PGLint): PGLuint; cdecl;
  PFNGLXBINDVIDEODEVICENVPROC = function(dpy: PDisplay; video_slot: TGLInt; video_device: TGLInt; attrib_list: PGLint): TGLint; cdecl;
  PFNGLXGETVIDEODEVICENVPROC = function(dpy: PDisplay; screen: TGLInt; numVideoDevices: TGLInt; pVideoDevice: GLXVideoDeviceNV): TGLInt; cdecl;

  PFNGLXALLOCATEMEMORYNVPROC = function( size: TGLsizei; readFrequency: TGLfloat; writeFrequency: TGLfloat; priority: TGLfloat ): Pointer; cdecl;
  PFNGLXFREEMEMORYNVPROC = procedure ( GLvoid: pointer ); cdecl;

  PFNGLXRELEASEVIDEODEVICENVPROC = function(dpy: PDisplay; screen: TGLInt; VideoDevice: GLXVideoDeviceNV): TGLuint; cdecl;
  PFNGLXBINDVIDEOIMAGENVPROC = function(dpy: PDisplay; VideoDevice: GLXVideoDeviceNV; pbuf: GLXPbuffer; iVideoBuffer: TGLInt): TGLuint; cdecl;
  PFNGLXRELEASEVIDEOIMAGENVPROC = function(dpy: PDisplay; pbuf: GLXPbuffer): TGLInt; cdecl;
  PFNGLXSENDPBUFFERTOVIDEONVPROC = function(dpy: PDisplay; pbuf: GLXPbuffer; iBufferType: TGLInt; pulCounterPbuffer: TGLuint64; bBlock: TGLboolean): TGLInt; cdecl;
  PFNGLXGETVIDEOINFONVPROC = function(dpy: PDisplay; screen: TGLInt; VideoDevice: GLXVideoDeviceNV; pulCounterOutputPbuffer: TGLuInt64; pulCounterOutputVideo: TGLuInt64): TGLInt; cdecl;
  PFNGLXJOINSWAPGROUPNVPROC = function(dpy: PDisplay; drawable: GLXDrawable; group: TGLuint): TGLBoolean; cdecl;
  PFNGLXBINDSWAPBARRIERNVPROC = function(dpy: PDisplay; group: TGLuint; barrier: TGLuint): TGLboolean; cdecl;
  PFNGLXQUERYSWAPGROUPNVPROC = function(dpy: PDisplay; drawable: GLXDrawable; group: PGLuint; barrier: PGLuint): TGLBoolean; cdecl;
  PFNGLXQUERYMAXSWAPGROUPSNVPROC = function(dpy: PDisplay; screen: TGLInt; maxGroups: TGLuInt; maxBarriers: TGLuInt): TGLBoolean; cdecl;
  PFNGLXQUERYFRAMECOUNTNVPROC = function(dpy: PDisplay; screen: TGLInt; count: TGLuint): TGLBoolean; cdecl;
  PFNGLXRESETFRAMECOUNTNVPROC = function(dpy: PDisplay; screen: TGLInt): TGLBoolean; cdecl;
  PFNGLXBINDVIDEOCAPTUREDEVICENVPROC = function(dpy: PDisplay; video_capture_slot: TGLuint; device: GLXVideoCaptureDeviceNV): TGLint; cdecl;
  PFNGLXENUMERATEVIDEOCAPTUREDEVICESNVPROC = function(dpy: PDisplay; screen: TGLInt; nelements: PGLint): GLXVideoCaptureDeviceNV; cdecl;
  PFNGLXLOCKVIDEOCAPTUREDEVICENVPROC = procedure (dpy: PDisplay; device: GLXVideoCaptureDeviceNV); cdecl;
  PFNGLXQUERYVIDEOCAPTUREDEVICENVPROC = function(dpy: PDisplay; device: GLXVideoCaptureDeviceNV; attribute:TGLint; value: PGLint): TGLint; cdecl;
  PFNGLXRELEASEVIDEOCAPTUREDEVICENVPROC = procedure(dpy: PDisplay; device: GLXVideoCaptureDeviceNV); cdecl;
  PFNGLXSWAPINTERVALEXTPROC = function(dpy: PDisplay; drawable: GLXDrawable; interval:TGLint): TGLint; cdecl;
  PFNGLXCOPYIMAGESUBDATANVPROC = procedure(dpy: PDisplay; srcCtx: GLXContext; srcName: TGLuint; srcTarget: TGLenum;
                       srcLevel: TGLuint; srcX: TGLuint;
                       srcY: TGLuint; srcZ: TGLuint;
                       dstCtx:GLXContext; dstName:TGLuint; dstTarget: TGLenum; dstLevel: TGLint;
                       dstX: TGLint; dstY: TGLint; dstZ: TGLint; width: GLsizei; height: GLsizei;
                       depth: GLsizei); cdecl;

  {$ENDIF}

  // ARB Extensions

  // unknown ARB extension
  PFNGLSAMPLEPASSARBPROC = procedure(pass: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_multitexture (ARB #1)
  PFNGLACTIVETEXTUREARBPROC = procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCLIENTACTIVETEXTUREARBPROC = procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1DARBPROC = procedure(target: TGLenum; s: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1DVARBPROC = procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1FARBPROC = procedure(target: TGLenum; s: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1FVARBPROC = procedure(target: TGLenum; v: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1IARBPROC = procedure(target: TGLenum; s: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1IVARBPROC = procedure(target: TGLenum; v: PGLInt); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1SARBPROC = procedure(target: TGLenum; s: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1SVARBPROC = procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2DARBPROC = procedure(target: TGLenum; s, t: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2DVARBPROC = procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2FARBPROC = procedure(target: TGLenum; s, t: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2FVARBPROC = procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2IARBPROC = procedure(target: TGLenum; s, t: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2IVARBPROC = procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2SARBPROC = procedure(target: TGLenum; s, t: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2SVARBPROC = procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3DARBPROC = procedure(target: TGLenum; s, t, r: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3DVARBPROC = procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3FARBPROC = procedure(target: TGLenum; s, t, r: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3FVARBPROC = procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3IARBPROC = procedure(target: TGLenum; s, t, r: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3IVARBPROC = procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3SARBPROC = procedure(target: TGLenum; s, t, r: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3SVARBPROC = procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4DARBPROC = procedure(target: TGLenum; s, t, r, q: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4DVARBPROC = procedure(target: TGLenum; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4FARBPROC = procedure(target: TGLenum; s, t, r, q: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4FVARBPROC = procedure(target: TGLenum; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4IARBPROC = procedure(target: TGLenum; s, t, r, q: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4IVARBPROC = procedure(target: TGLenum; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4SARBPROC = procedure(target: TGLenum; s, t, r, q: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4SVARBPROC = procedure(target: TGLenum; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_transpose_matrix (ARB #3)
  PFNGLLOADTRANSPOSEMATRIXFARBPROC = procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLLOADTRANSPOSEMATRIXDARBPROC = procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTTRANSPOSEMATRIXFARBPROC = procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTTRANSPOSEMATRIXDARBPROC = procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_multisample (ARB #5)
  PFNGLSAMPLECOVERAGEARBPROC = procedure(Value: TGLclampf; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_texture_compression (ARB #12)
  PFNGLCOMPRESSEDTEXIMAGE3DARBPROC = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height, depth: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXIMAGE2DARBPROC = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width, Height: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXIMAGE1DARBPROC = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; Width: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXSUBIMAGE3DARBPROC = procedure(target: TGLenum; level: TGLint; xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXSUBIMAGE2DARBPROC = procedure(target: TGLenum; level: TGLint; xoffset, yoffset: TGLint; width, height: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXSUBIMAGE1DARBPROC = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; Format: TGLenum; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETCOMPRESSEDTEXIMAGEARBPROC = procedure(target: TGLenum; level: TGLint; img: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_point_parameter (ARB #14)
  PFNGLPOINTPARAMETERFARBPROC = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPOINTPARAMETERFVARBPROC = procedure(pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_vertex_blend (ARB #15) {deprecated?}
  PFNGLWEIGHTBVARBPROC = procedure(size: TGLint; weights: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWEIGHTSVARBPROC = procedure(size: TGLint; weights: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWEIGHTIVARBPROC = procedure(size: TGLint; weights: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWEIGHTFVARBPROC = procedure(size: TGLint; weights: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWEIGHTDVARBPROC = procedure(size: TGLint; weights: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWEIGHTUBVARBPROC = procedure(size: TGLint; weights: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWEIGHTUSVARBPROC = procedure(size: TGLint; weights: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWEIGHTUIVARBPROC = procedure(size: TGLint; weights: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWEIGHTPOINTERARBPROC = procedure(size: TGLint; _type: TGLenum; stride:TGLsizei;
                               _pointer:pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXBLENDARBPROC = procedure(count: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_matrix_palette (ARB #16) {deprecated?}
  PFNGLCURRENTPALETTEMATRIXARBPROC = procedure(index: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMATRIXINDEXUBVARBPROC = procedure(size: TGLint; indices: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMATRIXINDEXUSVARBPROC = procedure(size: TGLint; indices: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMATRIXINDEXUIVARBPROC = procedure(size: TGLint; indices: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMATRIXINDEXPOINTERARBPROC = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; _pointer:pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_window_pos (ARB #25)
  PFNGLWINDOWPOS2DARBPROC = procedure(x,y : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS2DVARBPROC = procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS2FARBPROC = procedure(x,y : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS2FVARBPROC = procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS2IARBPROC = procedure(x,y : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS2IVARBPROC = procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS2SARBPROC = procedure(x,y : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS2SVARBPROC = procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3DARBPROC = procedure(x,y,z : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3DVARBPROC = procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3FARBPROC = procedure(x,y,z : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3FVARBPROC = procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3IARBPROC = procedure(x,y,z : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3IVARBPROC = procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3SARBPROC = procedure(x,y,z : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3SVARBPROC = procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_vertex_program (ARB #26)
  PFNGLVERTEXATTRIB1DARBPROC = procedure(index: GLuint; x: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1DVARBPROC = procedure(index: GLuint; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1FARBPROC = procedure(index: GLuint; x: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1FVARBPROC = procedure(index: GLuint; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1SARBPROC = procedure(index: GLuint; x: GLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1SVARBPROC = procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2DARBPROC = procedure(index: GLuint; x: GLdouble; y: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2DVARBPROC = procedure(index: GLuint; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2FARBPROC = procedure(index: GLuint; x: GLfloat; y: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2FVARBPROC = procedure(index: GLuint; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2SARBPROC = procedure(index: GLuint; x: GLshort; y: GLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2SVARBPROC = procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3DARBPROC = procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3DVARBPROC = procedure(index: GLuint; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3FARBPROC = procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3FVARBPROC = procedure(index: GLuint; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3SARBPROC = procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3SVARBPROC = procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NBVARBPROC = procedure(index: GLuint; const v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NIVARBPROC = procedure(index: GLuint; const v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NSVARBPROC = procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUBARBPROC = procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUBVARBPROC = procedure(index: GLuint; const v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUIVARBPROC = procedure(index: GLuint; const v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUSVARBPROC = procedure(index: GLuint; const v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4BVARBPROC = procedure(index: GLuint; const v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4DARBPROC = procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4DVARBPROC = procedure(index: GLuint; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4FARBPROC = procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4FVARBPROC = procedure(index: GLuint; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4IVARBPROC = procedure(index: GLuint; const v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4SARBPROC = procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4SVARBPROC = procedure(index: GLuint; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4UBVARBPROC = procedure(index: GLuint; const v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4UIVARBPROC = procedure(index: GLuint; const v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4USVARBPROC = procedure(index: GLuint; const v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBPOINTERARBPROC = procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; const _pointer: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLENABLEVERTEXATTRIBARRAYARBPROC = procedure(index: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDISABLEVERTEXATTRIBARRAYARBPROC = procedure(index: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMSTRINGARBPROC = procedure(target: GLenum; format: GLenum; len: GLsizei; const _string: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDPROGRAMARBPROC = procedure(target: GLenum; _program: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETEPROGRAMSARBPROC = procedure(n: GLsizei; const programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGENPROGRAMSARBPROC = procedure(n: GLsizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMENVPARAMETER4DARBPROC = procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMENVPARAMETER4DVARBPROC = procedure(target: GLenum; index: GLuint; const params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMENVPARAMETER4FARBPROC = procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMENVPARAMETER4FVARBPROC = procedure(target: GLenum; index: GLuint; const params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMLOCALPARAMETER4DARBPROC = procedure(target: GLenum; index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMLOCALPARAMETER4DVARBPROC = procedure(target: GLenum; index: GLuint; const params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMLOCALPARAMETER4FARBPROC = procedure(target: GLenum; index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMLOCALPARAMETER4FVARBPROC = procedure(target: GLenum; index: GLuint; const params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMENVPARAMETERDVARBPROC = procedure(target: GLenum; index: GLuint; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMENVPARAMETERFVARBPROC = procedure(target: GLenum; index: GLuint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMLOCALPARAMETERDVARBPROC = procedure(target: GLenum; index: GLuint; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMLOCALPARAMETERFVARBPROC = procedure(target: GLenum; index: GLuint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMIVARBPROC = procedure(target: GLenum; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMSTRINGARBPROC = procedure(target: GLenum; pname: GLenum; _string: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBDVARBPROC = procedure(index: GLuint; pname: GLenum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBFVARBPROC = procedure(index: GLuint; pname: GLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBIVARBPROC = procedure(index: GLuint; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBPOINTERVARBPROC = procedure(index: GLuint; pname: GLenum; _pointer: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISPROGRAMARBPROC = function(_program: GLuint): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_vertex_buffer_object (ARB #28)
  PFNGLBINDBUFFERARBPROC = procedure(target: GLenum; buffer: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETEBUFFERSARBPROC = procedure(n: GLsizei; const buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGENBUFFERSARBPROC = procedure(n: GLsizei; buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISBUFFERARBPROC = function(buffer: GLuint): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBUFFERDATAARBPROC = procedure(target: GLenum; size: GLsizei; const data: Pointer; usage: GLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBUFFERSUBDATAARBPROC = procedure(target: GLenum; offset: GLuint; size: GLsizei; const data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETBUFFERSUBDATAARBPROC = procedure(target: GLenum; offset: GLuint; size: GLsizei; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMAPBUFFERARBPROC = function(target: GLenum; access: GLenum): Pointer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNMAPBUFFERARBPROC = function(target: GLenum): GLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETBUFFERPARAMETERIVARBPROC = procedure(target: GLenum; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETBUFFERPOINTERVARBPROC = procedure(target: GLenum; pname: GLenum; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_occlusion_query (ARB #29)
  PFNGLGENQUERIESARBPROC = procedure(n: TGLsizei; ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETEQUERIESARBPROC = procedure(n: TGLsizei; const ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISQUERYARBPROC = function(id: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBEGINQUERYARBPROC = procedure(target: TGLenum; id: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLENDQUERYARBPROC = procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETQUERYIVARBPROC = procedure(target: TGLEnum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETQUERYOBJECTIVARBPROC = procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETQUERYOBJECTUIVARBPROC = procedure(id: TGLuint; pname: TGLenum; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_shader_objects (ARB #30)
  PFNGLDELETEOBJECTARBPROC = procedure(obj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETHANDLEARBPROC = function(pname: GLenum): GLhandleARB; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDETACHOBJECTARBPROC = procedure(containerObj: GLhandleARB; attachedObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCREATESHADEROBJECTARBPROC = function(shaderType: GLenum): GLhandleARB; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSHADERSOURCEARBPROC = procedure(shaderObj: GLhandleARB; count: GLsizei; const _string: PGLPCharArray; const length: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMPILESHADERARBPROC = procedure(shaderObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCREATEPROGRAMOBJECTARBPROC = function(): GLhandleARB; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLATTACHOBJECTARBPROC = procedure(containerObj: GLhandleARB; obj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLLINKPROGRAMARBPROC = procedure(programObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUSEPROGRAMOBJECTARBPROC = procedure(programObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVALIDATEPROGRAMARBPROC = procedure(programObj: GLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM1FARBPROC = procedure(location: GLint; v0: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM2FARBPROC = procedure(location: GLint; v0: GLfloat; v1: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM3FARBPROC = procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM4FARBPROC = procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM1IARBPROC = procedure(location: GLint; v0: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM2IARBPROC = procedure(location: GLint; v0: GLint; v1: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM3IARBPROC = procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM4IARBPROC = procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM1FVARBPROC = procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM2FVARBPROC = procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM3FVARBPROC = procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM4FVARBPROC = procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM1IVARBPROC = procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM2IVARBPROC = procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM3IVARBPROC = procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM4IVARBPROC = procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX2FVARBPROC = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX3FVARBPROC = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX4FVARBPROC = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETOBJECTPARAMETERFVARBPROC = procedure(obj: GLhandleARB; pname: GLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETOBJECTPARAMETERIVARBPROC = procedure(obj: GLhandleARB; pname: GLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETINFOLOGARBPROC = procedure(obj: GLhandleARB; maxLength: GLsizei; length: PGLsizei; infoLog: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETATTACHEDOBJECTSARBPROC = procedure(containerObj: GLhandleARB; maxCount: GLsizei; count: PGLsizei; obj: PGLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETUNIFORMLOCATIONARBPROC = function(programObj: GLhandleARB; const name: PGLChar): GLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETACTIVEUNIFORMARBPROC = procedure(programObj: GLhandleARB; index: GLuint; maxLength: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETUNIFORMFVARBPROC = procedure(programObj: GLhandleARB; location: GLint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETUNIFORMIVARBPROC = procedure(programObj: GLhandleARB; location: GLint; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETSHADERSOURCEARBPROC = procedure(obj: GLhandleARB; maxLength: GLsizei; length: PGLsizei; source: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_vertex_shader (ARB #31)
  PFNGLBINDATTRIBLOCATIONARBPROC = procedure(programObj: GLhandleARB; index: GLuint; const name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETACTIVEATTRIBARBPROC = procedure(programObj: GLhandleARB; index: GLuint; maxLength: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETATTRIBLOCATIONARBPROC = function(programObj: GLhandleARB; const name: PGLChar): GLint; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_DrawBuffers (ARB #37)
  PFNGLDRAWBUFFERSARBPROC = procedure (n: GLSizei; const bufs: PGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_color_buffer_float (ARB #39)
  PFNGLCLAMPCOLORARBPROC = procedure (target: TGLenum; clamp: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_draw_instanced (ARB #44)
  PFNGLDRAWARRAYSINSTANCEDARBPROC = procedure(mode: TGLenum; first: TGLint; count: TGLsizei;
          primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDRAWELEMENTSINSTANCEDARBPROC = procedure(mode: TGLenum; count: TGLSizei; _type: TGLenum;
          indices: PGLvoid; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_framebuffer_object (ARB #45)
  PFNGLISRENDERBUFFERPROC = function(renderbuffer: TGLuint): TGLBoolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDRENDERBUFFERPROC = procedure(target: TGLenum; renderbuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETERENDERBUFFERSPROC = procedure(n: TGLsizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGENRENDERBUFFERSPROC = procedure(n: TGLSizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLRENDERBUFFERSTORAGEPROC = procedure(target: TGLenum; internalformat: TGLenum;
          width: TGLsizei;  height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLRENDERBUFFERSTORAGEMULTISAMPLEPROC = procedure(target: TGLenum; samples: TGLsizei;
        internalformat: TGLenum;
        width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETRENDERBUFFERPARAMETERIVPROC = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISFRAMEBUFFERPROC = function(framebuffer: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDFRAMEBUFFERPROC = procedure(target: TGLenum; framebuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETEFRAMEBUFFERSPROC = procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGENFRAMEBUFFERSPROC = procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCHECKFRAMEBUFFERSTATUSPROC = function(target: TGLenum): TGLenum; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURE1DPROC = procedure(target: TGLenum; attachment: TGLenum;
          textarget: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURE2DPROC = procedure(target: TGLenum; attachment: TGLenum;
          textarget: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURE3DPROC = procedure(target: TGLenum; attachment: TGLenum;
          textarget: TGLenum; texture: TGLuint;
          level: TGLint; layer: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURELAYERPROC = procedure(target: TGLenum; attachment: TGLenum;
       texture: TGLuint; level: TGLint; layer: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERRENDERBUFFERPROC = procedure(target: TGLenum; attachment: TGLenum;
       renderbuffertarget: TGLenum; renderbuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVPROC = procedure(target: TGLenum; attachment: TGLenum;
             pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBLITFRAMEBUFFERPROC = procedure(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint;
     dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint;
     mask: TGLbitfield; filter: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGENERATEMIPMAPPROC = procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_geometry_shader4 (ARB #47)
  PFNGLPROGRAMPARAMETERIARBPROC = procedure ( _program:TGLuint; pname:TGLenum; value: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTUREARBPROC = procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURELAYERARBPROC = procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint; layer:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTUREFACEARBPROC = procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint; face:TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_instanced_arrays (ARB #49)
  PFNGLVERTEXATTRIBDIVISORARBPROC = procedure(index: TGLuint; divisor: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_map_buffer_range (ARB #50)
  PFNGLMAPBUFFERRANGEPROC = function(target: TGLenum; offset: TGLint{ptr}; length: TGLsizei{ptr};
            access: TGLbitfield ): Pointer;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFLUSHMAPPEDBUFFERRANGEPROC = procedure( target: TGLenum; offset: TGLint{ptr}; length: TGLsizei{ptr} );{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_texture_buffer_object (ARB #51)
  PFNGLTEXBUFFERARBPROC = procedure(target: TGLenum; internalformat: TGLEnum; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_vertex_array_object (ARB #54)
  PFNGLBINDVERTEXARRAYPROC = procedure(_array: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETEVERTEXARRAYSPROC = procedure(n: TGLsizei; arrays: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGENVERTEXARRAYSPROC = procedure(n: TGLsizei; arrays: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISVERTEXARRAYPROC = function(_array: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_uniform_buffer_object (ARB #57)
  PFNGLGETUNIFORMINDICESPROC = procedure(_program: TGLuint; uniformCount: TGLsizei; uniformNames: PGLPCharArray; uniformIndices: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETACTIVEUNIFORMSIVPROC = procedure(_program: TGLuint; uniformCount: TGLsizei; uniformIndices: PGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETACTIVEUNIFORMNAMEPROC = procedure(_program: TGLuint; uniformIndex: TGLuint; bufSize: TGLsizei; length: PGLsizei; uniformName: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETUNIFORMBLOCKINDEXPROC = function(_program: TGLuint; uniformBlockName: PGLchar): TGLuint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETACTIVEUNIFORMBLOCKIVPROC = procedure(_program: TGLuint; uniformBlockIndex: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETACTIVEUNIFORMBLOCKNAMEPROC = procedure(_program: TGLuint; uniformBlockIndex: TGLuint; bufSize: TGLsizei; length: PGLsizei; uniformBlockName: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMBLOCKBINDINGPROC = procedure(_program: TGLuint; uniformBlockIndex: TGLuint; uniformBlockBinding: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_copy_buffer (ARB #59)
  PFNGLCOPYBUFFERSUBDATAPROC = procedure(readTarget: TGLenum; writeTarget: TGLenum;
        readOffset: TGLintptr; writeOffset: TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_draw_elements_base_vertex (ARB #62)
  PFNGLDRAWELEMENTSBASEVERTEXPROC = procedure(mode: TGLenum; count: TGLsizei;
        _type: TGLenum; indices: PGLvoid; basevertex: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDRAWRANGEELEMENTSBASEVERTEXPROC = procedure(mode: TGLenum; start: TGLuint; _end: TGLuint;
        count: TGLsizei; _type: TGLenum; indices: PGLvoid; basevertex: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDRAWELEMENTSINSTANCEDBASEVERTEXPROC = procedure(mode: TGLenum; count: TGLsizei;
        _type: TGLenum; indices: PGLvoid; primcount: TGLsizei; basevertex: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTIDRAWELEMENTSBASEVERTEXPROC = procedure(mode: TGLenum; count: PGLsizei;
        _type: TGLenum; var indices; primcount: TGLsizei; basevertex: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_provoking_vertex (ARB #64)
  PFNGLPROVOKINGVERTEXPROC = procedure(mode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_sync (ARB #66)
  PFNGLFENCESYNCPROC = function(condition: TGLenum; flags: TGLbitfield): TGLsync;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISSYNCPROC = function(sync: TGLsync): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETESYNCPROC = procedure(sync: TGLsync);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCLIENTWAITSYNCPROC = function(sync: TGLsync; flags: TGLbitfield; timeout: TGLuint64): TGLenum;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLWAITSYNCPROC = procedure(sync: TGLsync; flags: TGLbitfield; timeout: TGLuint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETINTEGER64VPROC = procedure(pname: TGLenum; params: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETSYNCIVPROC = procedure(sync: TGLsync; pname: TGLenum; bufSize: TGLsizei; length: PGLsizei; values: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_texture_multisample (ARB #67)
  PFNGLTEXIMAGE2DMULTISAMPLEPROC = procedure(target: TGLenum; samples: TGLsizei; internalformat: TGLint;
                             width: TGLsizei; height: TGLsizei;
                             fixedsamplelocations: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXIMAGE3DMULTISAMPLEPROC = procedure(target: TGLenum; samples: TGLsizei; internalformat: TGLint;
                             width: TGLsizei; height: TGLsizei; depth: TGLsizei;
                             fixedsamplelocations: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETMULTISAMPLEFVPROC = procedure(pname: TGLenum; index: TGLuint; val: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSAMPLEMASKIPROC = procedure(index: TGLuint; mask: TGLbitfield);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_draw_buffers_blend (ARB #69)
  PFNGLBLENDEQUATIONIARBPROC = procedure(buf: TGLuint; mode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBLENDEQUATIONSEPARATEIARBPROC = procedure(buf: TGLuint; modeRGB: TGLenum; modeAlpha: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBLENDFUNCIARBPROC = procedure(buf: TGLuint; src: TGLenum; dst: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBLENDFUNCSEPARATEIARBPROC = procedure(buf: TGLuint; srcRGB: TGLenum; dstRGB: TGLenum;
                             srcAlpha: TGLenum; dstAlpha: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_sample_shading (ARB #70)
  PFNGLMINSAMPLESHADINGARBPROC = procedure(value: TGLclampf);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_blend_func_extended (ARB #78)
  PFNGLBINDFRAGDATALOCATIONINDEXEDPROC = procedure (_program: TGLuint; colorNumber: TGLuint; index: TGLuint; const name: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETFRAGDATAINDEXPROC = function (_program: TGLuint; const name: PGLchar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_sampler_objects (ARB #81)
  PFNGLGENSAMPLERSPROC = procedure(count: TGLsizei; samplers: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETESAMPLERSPROC = procedure(count: TGLsizei; const samplers: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISSAMPLERPROC = function(sampler: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDSAMPLERPROC = procedure(_unit: TGLuint; sampler: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSAMPLERPARAMETERIPROC = procedure(sampler: TGLuint; pname: TGLenum; param: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSAMPLERPARAMETERIVPROC = procedure(sampler: TGLuint; pname: TGLenum; const params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSAMPLERPARAMETERFPROC = procedure(sampler: TGLuint; pname: TGLenum; param: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSAMPLERPARAMETERFVPROC = procedure(sampler: TGLuint; pname: TGLenum; const params: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSAMPLERPARAMETERIIVPROC = procedure(sampler: TGLuint; pname: TGLenum; const params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSAMPLERPARAMETERIUIVPROC = procedure(sampler: TGLuint; pname: TGLenum; const params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETSAMPLERPARAMETERIVPROC = procedure(sampler: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETSAMPLERPARAMETERIIVPROC = procedure(sampler: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETSAMPLERPARAMETERFVPROC = procedure(sampler: TGLuint; pname: TGLenum; params: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETSAMPLERPARAMETERIFVPROC = procedure(sampler: TGLuint; pname: TGLenum; params: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_timer_query (ARB #85)
  PFNGLQUERYCOUNTERPROC = procedure(id: TGLuint; target: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETQUERYOBJECTI64VPROC = procedure(id: TGLuint; pname: TGLenum; params: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETQUERYOBJECTUI64VPROC = procedure(id: TGLuint; pname: TGLenum; params: PGLuint64);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_vertex_type_2_10_10_10_rev (ARB #86)
  PFNGLVERTEXP2UIPROC = procedure(_type: TGLenum; value: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXP2UIVPROC = procedure(_type: TGLenum; const value: PGLuint );{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXP3UIPROC = procedure(_type: TGLenum; value: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXP3UIVPROC = procedure(_type: TGLenum; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXP4UIPROC = procedure(_type: TGLenum; value: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXP4UIVPROC = procedure(_type: TGLenum; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXCOORDP1UIPROC = procedure(_type: TGLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXCOORDP1UIVPROC = procedure(_type: TGLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXCOORDP2UIPROC = procedure(_type: TGLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXCOORDP2UIVPROC = procedure(_type: TGLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXCOORDP3UIPROC = procedure(_type: TGLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXCOORDP3UIVPROC = procedure(_type: TGLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXCOORDP4UIPROC = procedure(_type: TGLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXCOORDP4UIVPROC = procedure(_type: TGLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP1UIPROC = procedure(texture: TGLenum; _type: GLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP1UIVPROC = procedure(texture: TGLenum; _type: GLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP2UIPROC = procedure(texture: TGLenum; _type: GLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP2UIVPROC = procedure(texture: TGLenum; _type: GLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP3UIPROC = procedure(texture: TGLenum; _type: GLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP3UIVPROC = procedure(texture: TGLenum; _type: GLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP4UIPROC = procedure(texture: TGLenum; _type: GLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP4UIVPROC = procedure(texture: TGLenum; _type: GLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLNORMALP3UIPROC = procedure(_type: TGLenum; coords: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLNORMALP3UIVPROC = procedure(_type: TGLenum; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOLORP3UIPROC = procedure(_type: TGLenum; color: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOLORP3UIVPROC = procedure(_type: TGLenum; const color: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOLORP4UIPROC = procedure(_type: TGLenum; color: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOLORP4UIVPROC = procedure(_type: TGLenum; const color: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLORP3UIPROC = procedure(_type: TGLenum; color: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLORP3UIVPROC = procedure(_type: TGLenum; const color: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP1UIPROC = procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; value: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP1UIVPROC = procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP2UIPROC = procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; value: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP2UIVPROC = procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP3UIPROC = procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; value: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP3UIVPROC = procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP4UIPROC = procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; value: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP4UIVPROC = procedure(index: TGLuint; _type: TGLenum; normalized: TGLboolean; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_draw_indirect (ARB #87)
  PFNGLDRAWARRAYSINDIRECTPROC = procedure(mode: TGLenum; const indirect: PGLvoid);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDRAWELEMENTSINDIRECTPROC = procedure(mode: TGLenum; _type: TGLenum; const indirect: PGLvoid);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_gpu_shader_fp64 (ARB #89)
  PFNGLUNIFORM1DPROC = procedure(location: TGLint; x: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM2DPROC = procedure(location: TGLint; x: TGLdouble; y: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM3DPROC = procedure(location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM4DPROC = procedure(location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM1DVPROC = procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM2DVPROC = procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM3DVPROC = procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM4DVPROC = procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX2DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX3DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX4DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX2X3DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX2X4DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX3X2DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX3X4DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX4X2DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX4X3DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETUNIFORMDVPROC = procedure(_program: TGLuint; location: TGLint; params : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  // glProgramUniformXXX only valid if EXT_direct_state_access is available
  PFNGLPROGRAMUNIFORM1DEXTPROC = procedure(_program: TGLuint; location: TGLint; x: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2DEXTPROC = procedure(_program: TGLuint; location: TGLint; x: TGLdouble; y: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3DEXTPROC = procedure(_program: TGLuint; location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4DEXTPROC = procedure(_program: TGLuint; location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1DVEXTPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2DVEXTPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3DVEXTPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4DVEXTPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2DVEXTPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3DVEXTPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4DVEXTPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2X3DVEXTPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2X4DVEXTPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3X2DVEXTPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3X4DVEXTPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4X2DVEXTPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4X3DVEXTPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_shader_subroutine (ARB #90)
  PFNGLGETSUBROUTINEUNIFORMLOCATIONPROC = function(_program: TGLuint; shadertype: TGLenum; const name: PGLchar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETSUBROUTINEINDEXPROC = function(_program: TGLuint; shadertype: TGLenum; const name: PGLchar): TGLuint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETACTIVESUBROUTINEUNIFORMIVPROC = procedure(_program: TGLuint; shadertype: TGLenum; index: TGLuint; pname: TGLenum; values: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETACTIVESUBROUTINEUNIFORMNAMEPROC = procedure(_program: TGLuint; shadertype: TGLenum; index: TGLuint; bufsize: TGLsizei; length: PGLsizei; name: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETACTIVESUBROUTINENAMEPROC = procedure(_program: TGLuint; shadertype: TGLenum; index: TGLuint; bufsize: TGLsizei; length: PGLsizei; name: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMSUBROUTINESUIVPROC = procedure(shadertype: TGLenum; count: TGLsizei; const indices: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETUNIFORMSUBROUTINEUIVPROC = procedure(shadertype: TGLenum; location: TGLint; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMSTAGEIVPROC = procedure(_program: TGLuint; shadertype: TGLenum; pname: TGLenum; values: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_tessellation_shader (ARB #91)
  PFNGLPATCHPARAMETERIPROC = procedure(pname: TGLenum; value: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPATCHPARAMETERFVPROC = procedure(pname: TGLenum; const values: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_transform_feedback2 (ARB #93)
  PFNGLBINDTRANSFORMFEEDBACKPROC = procedure(target: TGLenum; id: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETETRANSFORMFEEDBACKSPROC = procedure(n: TGLsizei; const ids: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGENTRANSFORMFEEDBACKSPROC = procedure(n: TGLsizei; ids: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISTRANSFORMFEEDBACKPROC = function(id: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPAUSETRANSFORMFEEDBACKPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLRESUMETRANSFORMFEEDBACKPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDRAWTRANSFORMFEEDBACKPROC = procedure(mode: TGLenum; id: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_transform_feedback3 (ARB #94)
  PFNGLDRAWTRANSFORMFEEDBACKSTREAMPROC = procedure(mode: TGLenum; id: TGLuint; stream: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBEGINQUERYINDEXEDPROC = procedure(target: TGLenum; index: TGLuint; id: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLENDQUERYINDEXEDPROC = procedure(target: TGLenum; index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETQUERYINDEXEDIVPROC = procedure(target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_ES2_compatibility (ARB #95)
  PFNGLRELEASESHADERCOMPILERPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSHADERBINARYPROC = procedure(count: TGLsizei; shaders: PGLuint; binaryformat: TGLenum; binary: Pointer; length: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETSHADERPRECISIONFORMATPROC = procedure(shadertype: TGLenum; precisiontype: TGLenum; range: PGLint; precision: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDEPTHRANGEFPROC = procedure(n: TGLclampf; f: TGLclampf);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCLEARDEPTHFPROC = procedure(d: TGLclampf);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_get_program_binary (ARB #96)
  PFNGLGETPROGRAMBINARYPROC = procedure(_program: TGLuint; bufSize: TGLsizei; length: PGLsizei; binaryFormat: PGLenum; binary: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMBINARYPROC = procedure(_program: TGLuint; binaryFormat: TGLEnum; binary: Pointer; length: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMPARAMETERIPROC = procedure(_program: TGLuint; pname: TGLenum; value: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_separate_shader_objects (ARB #97)
  PFNGLUSEPROGRAMSTAGESPROC = procedure(pipeline: TGLuint; stages: TGLbitfield; _program: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLACTIVESHADERPROGRAMPROC = procedure(pipeline: TGLuint; _program: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCREATESHADERPROGRAMVPROC = function(_type: TGLenum; count: TGLsizei; const strings: PGLPCharArray): TGLuint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDPROGRAMPIPELINEPROC = procedure(pipeline: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETEPROGRAMPIPELINESPROC = procedure(n: TGLsizei; pipelines: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGENPROGRAMPIPELINESPROC = procedure(n: TGLsizei; pipelines: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISPROGRAMPIPELINEPROC = function(pipeline: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMPIPELINEIVPROC = procedure(pipeline: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1IPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1IVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1FPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1FVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1DPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1DVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1UIPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1UIVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2IPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLint; v1: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2IVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2FPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLfloat; v1: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2FVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2DPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLdouble; v1: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2DVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2UIPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLuint; v1: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2UIVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3IPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLint; v1: TGLint; v2: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3IVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3FPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLfloat; v1: TGLfloat; v2: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3FVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3DPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLdouble; v1: TGLdouble; v2: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3DVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3UIPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLuint; v1: TGLuint; v2: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3UIVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4IPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLint; v1: TGLint; v2: TGLint; v3: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4IVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4FPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLfloat; v1: TGLfloat; v2: TGLfloat; v3: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4FVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4DPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLdouble; v1: TGLdouble; v2: TGLdouble; v3: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4DVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4UIPROC = procedure(_program: TGLuint; location: TGLint; v0: TGLuint; v1: TGLuint; v2: TGLuint; v3: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4UIVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2FVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3FVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4FVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2DVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3DVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4DVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2X3FVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3X2FVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2X4FVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4X2FVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3X4FVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4X3FVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2X3DVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3X2DVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2X4DVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4X2DVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3X4DVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4X3DVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVALIDATEPROGRAMPIPELINEPROC = procedure(pipeline: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMPIPELINEINFOLOGPROC = procedure(pipeline: TGLuint; bufSize: TGLsizei; length: PGLsizei; infoLog: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_shader_precision (ARB #98)
  // (no entry points)

  // GL_ARB_vertex_attrib_64bit (ARB #99)
  PFNGLVERTEXATTRIBL1DPROC = procedure(index: TGLuint; x: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBL2DPROC = procedure(index: TGLuint; x: TGLdouble; y: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBL3DPROC = procedure(index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBL4DPROC = procedure(index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBL1DVPROC = procedure(index: TGLuint; {const} v: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBL2DVPROC = procedure(index: TGLuint; {const} v: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBL3DVPROC = procedure(index: TGLuint; {const} v: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBL4DVPROC = procedure(index: TGLuint; {const} v :PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBLPOINTERPROC = procedure(index: TGLuint; size: TGLint; _type: TGLenum; stride: TGLsizei; {const} ptr: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBLDVPROC = procedure(index: TGLuint; pname: TGLenum; params: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  // VertexArrayVertexAttribLOffsetEXT only valid if EXT_direct_state_access is available
  PFNGLVERTEXARRAYVERTEXATTRIBLOFFSETEXTPROC = procedure (vaobj: TGLuint; buffer: TGLuint;
                                           index: TGLuint; size: TGLint;
                                           _type: TGLenum; stride: TGLsizei;
                                           offset: TGLintptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}


  // GL_ARB_viewport_array (ARB #100)
  PFNGLVIEWPORTARRAYVPROC = procedure(first: TGLuint; count: TGLsizei; {const} v: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVIEWPORTINDEXEDFPROC = procedure(index: TGLuint; x: TGLfloat; y: TGLfloat; w: TGLfloat; h: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVIEWPORTINDEXEDFVPROC = procedure(index: TGLuint; {const} v: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSCISSORARRAYVPROC = procedure(first: TGLuint; count: TGLsizei; {const} v: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSCISSORINDEXEDPROC = procedure(index: TGLuint; left: TGLint; bottom: TGLint; width: TGLsizei; height: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSCISSORINDEXEDVPROC = procedure(index: TGLuint; {const} v: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDEPTHRANGEARRAYVPROC = procedure(first: TGLuint; count: TGLsizei; {const} v: PGLclampd);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDEPTHRANGEINDEXEDPROC = procedure(index: TGLuint; n: TGLclampd; f: TGLclampd);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETFLOATI_VPROC = procedure(target: TGLenum; index: TGLuint; data: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETDOUBLEI_VPROC = procedure(target: TGLenum; index: TGLuint; data: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_debug_output (ARB #104)
  PFNGLDEBUGMESSAGECONTROLARBPROC = procedure(source: TGLenum; _type: TGLenum; severity: TGLenum; count: TGLsizei; {const} ids: PGLuint; enabled: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDEBUGMESSAGEINSERTARBPROC = procedure(source: TGLenum; _type: TGLenum; id: TGLuint; severity: TGLenum; length: TGLsizei; {const} buf: PGLchar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDEBUGMESSAGECALLBACKARBPROC = procedure(callback: TGLDEBUGPROCARB; {const} userParam: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETDEBUGMESSAGELOGARBPROC = function(count: TGLuint; bufsize: TGLsizei; sources: PGLenum; types: PGLenum; ids: PGLuint; severities: PGLenum; lengths: PGLsizei; messageLog: PGLchar): TGLuint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_robustness (ARB #105)
  PFNGLGETGRAPHICSRESETSTATUSARBPROC = function(): TGLenum;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNMAPDVARBPROC = procedure(target: TGLenum; query: TGLenum; bufSize: TGLsizei; v: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNMAPFVARBPROC = procedure(target: TGLenum; query: TGLenum; bufSize: TGLsizei; v: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNMAPIVARBPROC = procedure(target: TGLenum; query: TGLenum; bufSize: TGLsizei; v: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNPIXELMAPFVARBPROC = procedure(map: TGLenum; bufSize: TGLsizei; values: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNPIXELMAPUIVARBPROC = procedure(map: TGLenum; bufSize: TGLsizei; values: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNPIXELMAPUSVARBPROC = procedure(map: TGLenum; bufSize: TGLsizei; values: PGLushort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNPOLYGONSTIPPLEARBPROC = procedure(bufSize: TGLsizei; pattern: PGLubyte);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNCOLORTABLEARBPROC = procedure(target: TGLenum; format: TGLenum; _type: TGLenum; bufSize: TGLsizei; table: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNCONVOLUTIONFILTERARBPROC = procedure(target: TGLenum; format: TGLenum; _type: TGLenum; bufSize: TGLsizei; image: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNSEPARABLEFILTERARBPROC = procedure(target: TGLenum; format: TGLenum; _type: TGLenum; rowBufSize: TGLsizei; row: Pointer; columnBufSize: TGLsizei; column: Pointer; span: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNHISTOGRAMARBPROC = procedure(target: TGLenum; reset: TGLboolean; format: TGLenum; _type: TGLenum; bufSize: TGLsizei; values: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNMINMAXARBPROC = procedure(target: TGLenum; reset: TGLboolean; format: TGLenum; _type: TGLenum; bufSize: TGLsizei; values: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNTEXIMAGEARBPROC = procedure(target: TGLenum; level: TGLint; format: TGLenum; _type: TGLenum; bufSize: TGLsizei; img: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLREADNPIXELSARBPROC = procedure(x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; bufSize: TGLsizei; data: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNCOMPRESSEDTEXIMAGEARBPROC = procedure(target: TGLenum; lod: TGLint; bufSize: TGLsizei; img: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNUNIFORMFVARBPROC = procedure(_program: TGLuint; location: TGLint; bufSize: TGLsizei; params: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNUNIFORMIVARBPROC = procedure(_program: TGLuint; location: TGLint; bufSize: TGLsizei; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNUNIFORMUIVARBPROC = procedure(_program: TGLuint; location: TGLint; bufSize: TGLsizei; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNUNIFORMDVARBPROC = procedure(_program: TGLuint; location: TGLint; bufSize: TGLsizei; params: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ARB_shader_stencil_export (ARB #106)
  // (no entry points)

  // EXT/Vendor extensions

  // Unknown Vendor/EXT functions
  PFNGLARRAYELEMENTARRAYEXTPROC = procedure(mode: TGLEnum; count: TGLsizei; pi: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_WIN_swap_hint (extension # not found)
  PFNGLADDSWAPHINTRECTWINPROC = procedure(x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_blend_color (EXT #2)
  PFNGLBLENDCOLOREXTPROC = procedure(red, green, blue: TGLclampf; alpha: TGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_polygon_offset (EXT #3)
  PFNGLPOLYGONOFFSETEXTPROC = procedure(factor, bias: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_texture3D (EXT #6)
  PFNGLTEXIMAGE3DEXTPROC = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width, height, depth: TGLsizei; border: TGLint; Format, AType: TGLenum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_subtexture (EXT #9)
  PFNGLTEXSUBIMAGE1DEXTPROC = procedure(target: TGLEnum; level, xoffset: TGLint; width: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXSUBIMAGE2DEXTPROC = procedure(target: TGLEnum; level, xoffset, yoffset: TGLint; width, height: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXSUBIMAGE3DEXTPROC = procedure(target: TGLEnum; level, xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; format, Atype: TGLEnum; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_copy_texture (EXT #10)
  PFNGLCOPYTEXIMAGE1DEXTPROC = procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOPYTEXIMAGE2DEXTPROC = procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum; x, y: TGLint; width, height: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOPYTEXSUBIMAGE1DEXTPROC = procedure(target: TGLEnum; level, xoffset, x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOPYTEXSUBIMAGE2DEXTPROC = procedure(target: TGLEnum; level, xoffset, yoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOPYTEXSUBIMAGE3DEXTPROC = procedure(target: TGLEnum; level, xoffset, yoffset, zoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_texture_object (EXT #20)
  PFNGLGENTEXTURESEXTPROC = procedure(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETETEXTURESEXTPROC = procedure(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDTEXTUREEXTPROC = procedure(target: TGLEnum; texture: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPRIORITIZETEXTURESEXTPROC = procedure(n: TGLsizei; textures: PGLuint; priorities: PGLclampf); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLARETEXTURESRESIDENTEXTPROC = function(n: TGLsizei; textures: PGLuint; residences: PGLBoolean): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISTEXTUREEXTPROC = function(texture: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_SGIS_multisample (EXT #25)
  PFNGLSAMPLEMASKSGISPROC = procedure(Value: TGLclampf; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSAMPLEPATTERNSGISPROC = procedure(pattern: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_blend_minmax (EXT #37)
  PFNGLBLENDEQUATIONEXTPROC = procedure(mode: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_paletted_texture (EXT #78)
  PFNGLCOLORTABLEEXTPROC = procedure(target, internalFormat: TGLEnum; width: TGLsizei; format, atype: TGLEnum; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOLORSUBTABLEEXTPROC = procedure(target: TGLEnum; start, count: TGLsizei; format, atype: TGLEnum; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETCOLORTABLEEXTPROC = procedure(target, format, atype: TGLEnum; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETCOLORTABLEPARAMETERFVEXTPROC = procedure(target, pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETCOLORTABLEPARAMETERIVEXTPROC = procedure(target, pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  //   glGetColorTableParameterfvEXT: procedure(target, pname: TGLEnum; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  //   glGetColorTableParameterivEXT: procedure(target, pname: TGLEnum; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_index_material (EXT #94)
  PFNGLINDEXMATERIALEXTPROC = procedure(face: TGLEnum; mode: TGLEnum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_index_func (EXT #95)
  PFNGLINDEXFUNCEXTPROC = procedure(func: TGLEnum; ref: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_compiled_vertex_array (EXT #97)
  PFNGLLOCKARRAYSEXTPROC = procedure(first: TGLint; count: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNLOCKARRAYSEXTPROC = procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_draw_range_elements (EXT #112)
  PFNGLDRAWRANGEELEMENTSEXTPROC = procedure(mode: TGLenum; start, Aend: TGLuint; Count: TGLsizei; Atype: TGLenum; indices: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_scene_marker (EXT #120)
  PFNGLBEGINSCENEEXTPROC = procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLENDSCENEEXTPROC = procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_secondary_color (EXT #145)
  PFNGLSECONDARYCOLOR3BEXTPROC = procedure(red, green, blue: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3BVEXTPROC = procedure(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3DEXTPROC = procedure(red, green, blue: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3DVEXTPROC = procedure(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3FEXTPROC = procedure(red, green, blue: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3FVEXTPROC = procedure(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3IEXTPROC = procedure(red, green, blue: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3IVEXTPROC = procedure(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3SEXTPROC = procedure(red, green, blue: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3SVEXTPROC = procedure(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3UBEXTPROC = procedure(red, green, blue: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3UBVEXTPROC = procedure(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3UIEXTPROC = procedure(red, green, blue: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3UIVEXTPROC = procedure(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3USEXTPROC = procedure(red, green, blue: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3USVEXTPROC = procedure(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLORPOINTEREXTPROC = procedure(Size: TGLint; Atype: TGLenum; stride: TGLsizei; p: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_multi_draw_arrays (EXT #148)
  PFNGLMULTIDRAWARRAYSEXTPROC = procedure(mode: TGLenum; First: PGLint; Count: PGLsizei; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMULTIDRAWELEMENTSEXTPROC = procedure(mode: TGLenum; Count: PGLsizei; AType: TGLenum; var indices; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_fog_coord (EXT #149)
  PFNGLFOGCOORDFEXTPROC = procedure(coord: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFOGCOORDFVEXTPROC = procedure(coord: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFOGCOORDDEXTPROC = procedure(coord: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFOGCOORDDVEXTPROC = procedure(coord: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFOGCOORDPOINTEREXTPROC = procedure(AType: TGLenum; stride: TGLsizei; p: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_blend_func_separate (EXT #173)
  PFNGLBLENDFUNCSEPARATEEXTPROC = procedure(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_NV_vertex_array_range (EXT #190)
  PFNGLFLUSHVERTEXARRAYRANGENVPROC = procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXARRAYRANGENVPROC = procedure(Size: TGLsizei; p: pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNWGLALLOCATEMEMORYNVPROC = function(size: TGLsizei; readFrequency, writeFrequency, priority: Single): Pointer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNWGLFREEMEMORYNVPROC = procedure(ptr: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_NV_register_combiners (EXT #191)
  PFNGLCOMBINERPARAMETERFVNVPROC = procedure(pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMBINERPARAMETERFNVPROC = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMBINERPARAMETERIVNVPROC = procedure(pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMBINERPARAMETERINVPROC = procedure(pname: TGLenum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMBINERINPUTNVPROC = procedure(stage, portion, variable, input, mapping, componentUsage: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOMBINEROUTPUTNVPROC = procedure(stage, portion, abOutput, cdOutput, sumOutput, scale, bias: TGLenum; abDotProduct, cdDotProduct, muxSum: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFINALCOMBINERINPUTNVPROC = procedure(variable, input, mapping, componentUsage: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETCOMBINERINPUTPARAMETERFVNVPROC = procedure(stage, portion, variable, pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETCOMBINERINPUTPARAMETERIVNVPROC = procedure(stage, portion, variable, pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETCOMBINEROUTPUTPARAMETERFVNVPROC = procedure(stage, portion, pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETCOMBINEROUTPUTPARAMETERIVNVPROC = procedure(stage, portion, pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETFINALCOMBINERINPUTPARAMETERFVNVPROC = procedure(variable, pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETFINALCOMBINERINPUTPARAMETERIVNVPROC = procedure(variable, pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_MESA_resize_buffers (EXT #196)
  PFNGLRESIZEBUFFERSMESAPROC = procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_3DFX_tbuffer (EXT #208)
  PFNGLTBUFFERMASK3DFXPROC = procedure(mask: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_multisample (EXT #209)
  PFNGLSAMPLEMASKEXTPROC = procedure(Value: TGLclampf; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSAMPLEPATTERNEXTPROC = procedure(pattern: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_SGIS_texture_color_mask (EXT #214)
  PFNGLTEXTURECOLORMASKSGISPROC = procedure(red, green, blue, alpha: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_NV_fence (EXT #222)
  PFNGLGENFENCESNVPROC = procedure(n: TGLsizei; fences: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETEFENCESNVPROC = procedure(n: TGLsizei; fences: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSETFENCENVPROC = procedure(fence: TGLuint; condition: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTESTFENCENVPROC = function(fence: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFINISHFENCENVPROC = procedure(fence: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISFENCENVPROC = function(fence: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETFENCEIVNVPROC = procedure(fence: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_NV_vertex_program (EXT #233)
  PFNGLAREPROGRAMSRESIDENTNVPROC = procedure(n: TGLSizei; programs: PGLuint; residences: PGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDPROGRAMNVPROC = procedure(target: TGLenum; id: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETEPROGRAMSNVPROC = procedure(n: TGLSizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLEXECUTEPROGRAMNVPROC = procedure(target: TGLenum; id: TGLuint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGENPROGRAMSNVPROC = procedure(n: TGLSizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMPARAMETERDVNVPROC = procedure (target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMPARAMETERFVNVPROC = procedure (target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMIVNVPROC = procedure (id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETPROGRAMSTRINGNVPROC = procedure (id: TGLuint; pname: TGLenum; programIdx: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETTRACKMATRIXIVNVPROC = procedure (target: TGLenum; address: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBDVNVPROC = procedure (index: TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBFVNVPROC = procedure (index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBIVNVPROC = procedure (index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBPOINTERVNVPROC = procedure (index: TGLuint; pname: TGLenum; pointer: PGLPointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISPROGRAMNVPROC = function (id: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLLOADPROGRAMNVPROC = procedure (target: TGLenum; id: TGLuint; len: TGLSizei; programIdx: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMPARAMETER4DNVPROC = procedure (target: TGLenum; index: TGLuint; x, y, z, w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMPARAMETER4DVNVPROC = procedure (target: TGLenum; index: TGLuint; v: PGLdouble ); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMPARAMETER4FNVPROC = procedure (target: TGLenum; index: TGLuint; x, y, z, w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMPARAMETER4FVNVPROC = procedure (target: TGLenum; index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMPARAMETERS4DVNVPROC = procedure (target: TGLenum; index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMPARAMETERS4FVNVPROC = procedure (target: TGLenum; index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLREQUESTRESIDENTPROGRAMSNVPROC = procedure (n: TGLSizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTRACKMATRIXNVPROC = procedure (target: TGLenum; address: TGLuint; matrix: TGLenum; transform: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBPOINTERNVPROC = procedure (index: TGLuint; fsize: TGLint; vertextype: TGLenum; stride: TGLSizei; pointer: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1DNVPROC = procedure (index: TGLuint; x: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1DVNVPROC = procedure (index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1FNVPROC = procedure (index: TGLuint; x: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1FVNVPROC = procedure (index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1SNVPROC = procedure (index: TGLuint; x: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1SVNVPROC = procedure (index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2DNVPROC = procedure (index: TGLuint; x: TGLdouble; y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2DVNVPROC = procedure (index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2FNVPROC = procedure (index: TGLuint; x: TGLfloat; y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2FVNVPROC = procedure (index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2SNVPROC = procedure (index: TGLuint; x: TGLshort; y: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2SVNVPROC = procedure (index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3DNVPROC = procedure (index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3DVNVPROC = procedure (index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3FNVPROC = procedure (index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3FVNVPROC = procedure (index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3SNVPROC = procedure (index: TGLuint; x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3SVNVPROC = procedure (index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4DNVPROC = procedure (index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4DVNVPROC = procedure (index: TGLuint; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4FNVPROC = procedure(index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4FVNVPROC = procedure(index: TGLuint; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4SNVPROC = procedure (index: TGLuint; x: TGLshort; y: TGLshort; z: TGLdouble; w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4SVNVPROC = procedure (index: TGLuint; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4UBVNVPROC = procedure (index: TGLuint; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS1DVNVPROC = procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS1FVNVPROC = procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS1SVNVPROC = procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS2DVNVPROC = procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS2FVNVPROC = procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS2SVNVPROC = procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS3DVNVPROC = procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS3FVNVPROC = procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS3SVNVPROC = procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS4DVNVPROC = procedure (index: TGLuint; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS4FVNVPROC = procedure (index: TGLuint; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS4SVNVPROC = procedure (index: TGLuint; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS4UBVNVPROC = procedure (index: TGLuint; count: TGLSizei; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_NV_occlusion_query (EXT #261)
  PFNGLGENOCCLUSIONQUERIESNVPROC = procedure(n: TGLsizei; ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETEOCCLUSIONQUERIESNVPROC = procedure(n: TGLsizei; const ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISOCCLUSIONQUERYNVPROC = function(id: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBEGINOCCLUSIONQUERYNVPROC = procedure(id: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLENDOCCLUSIONQUERYNVPROC = procedure; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETOCCLUSIONQUERYIVNVPROC = procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETOCCLUSIONQUERYUIVNVPROC = procedure(id: TGLuint; pname: TGLenum; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_NV_point_sprite (EXT #262)
  PFNGLPOINTPARAMETERINVPROC = procedure(pname: TGLenum; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPOINTPARAMETERIVNVPROC = procedure(pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_stencil_two_side (EXT #268)
  PFNGLACTIVESTENCILFACEEXTPROC = procedure(face: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_ATI_draw_buffers (EXT #277)
  PFNGLDRAWBUFFERSATIPROC = procedure(n: GLsizei; const bufs: PGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_NV_primitive_restart (EXT #285)
  PFNGLPRIMITIVERESTARTNVPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPRIMITIVERESTARTINDEXNVPROC = procedure(index: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_depth_bounds_test (EXT #297)
  PFNGLDEPTHBOUNDSEXTPROC = procedure(zmin: TGLclampd; zmax: TGLclampd);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_blend_equation_separate (EXT #299)
  PFNGLBLENDEQUATIONSEPARATEEXTPROC = procedure(modeRGB: TGLenum; modeAlpha: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_framebuffer_object (EXT #310)
  PFNGLISRENDERBUFFEREXTPROC = function(renderbuffer: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDRENDERBUFFEREXTPROC = procedure(target: TGLenum; renderbuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETERENDERBUFFERSEXTPROC = procedure(n: TGLsizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGENRENDERBUFFERSEXTPROC = procedure(n: TGLsizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLRENDERBUFFERSTORAGEEXTPROC = procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETRENDERBUFFERPARAMETERIVEXTPROC = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISFRAMEBUFFEREXTPROC = function(framebuffer: TGLuint): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDFRAMEBUFFEREXTPROC = procedure(target: TGLenum; framebuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDELETEFRAMEBUFFERSEXTPROC = procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGENFRAMEBUFFERSEXTPROC = procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCHECKFRAMEBUFFERSTATUSEXTPROC = function(target: TGLenum): TGLenum; {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURE1DEXTPROC = procedure(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURE2DEXTPROC = procedure(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURE3DEXTPROC = procedure(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint; zoffset: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERRENDERBUFFEREXTPROC = procedure(target: TGLenum; attachment: TGLenum; renderbuffertarget: TGLenum; renderbuffer: TGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC = procedure(target: TGLenum; attachment: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGENERATEMIPMAPEXTPROC = procedure(target: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_GREMEDY_string_marker (EXT #311)
  PFNGLSTRINGMARKERGREMEDYPROC = procedure(len: GLsizei; str: PGLChar); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_stencil_clear_tag (EXT #314)
  PFNGLSTENCILCLEARTAGEXTPROC = procedure(stencilTagBits: TGLsizei; stencilClearTag: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_framebuffer_blit (EXT #316)
  PFNGLBLITFRAMEBUFFEREXTPROC = procedure(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint;
                          dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint;
                          mask: TGLbitfield; filter: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_framebuffer_multisample (EXT #317)
  PFNGLRENDERBUFFERSTORAGEMULTISAMPLEEXTPROC = procedure(target: TGLenum; samples: TGLsizei;
          internalformat: TGLenum; width: TGLsizei; height: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_timer_query (EXT #319)
  PFNGLGETQUERYOBJECTI64VEXTPROC = procedure(id: TGLuint; pname: TGLenum; params: PGLint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETQUERYOBJECTUI64VEXTPROC = procedure(id: TGLuint; pname: TGLenum; params: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_gpu_program_parameters (EXT #320)
  PFNGLPROGRAMENVPARAMETERS4FVEXTPROC = procedure(target:TGLenum; index:TGLuint; count:TGLsizei;
                                   const params:PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMLOCALPARAMETERS4FVEXTPROC = procedure(target:TGLenum; index:TGLuint; count:TGLsizei;
                                   const params:PGLFloat);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_NV_geometry_program4 (EXT #323)
  PFNGLPROGRAMVERTEXLIMITNVPROC = procedure (target: TGLenum; limit: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_geometry_shader4 (EXT #324)
  PFNGLPROGRAMPARAMETERIEXTPROC = procedure ( _program:TGLuint; pname:TGLenum; value: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTUREEXTPROC = procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURELAYEREXTPROC = procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint; layer:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTUREFACEEXTPROC = procedure ( target:TGLenum;  attachment:TGLenum; texture:TGLuint;  level:TGLint; face:TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_gpu_shader4 (EXT #326)
  PFNGLVERTEXATTRIBI1IEXTPROC = procedure(index: TGLuint; x: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2IEXTPROC = procedure(index: TGLuint; x: TGLint; y: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3IEXTPROC = procedure(index: TGLuint; x: TGLint; y: TGLint; z: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4IEXTPROC = procedure(index: TGLuint; x: TGLint; y: TGLint; z: TGLint; w: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI1UIEXTPROC = procedure(index: TGLuint; x: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2UIEXTPROC = procedure(index: TGLuint; x: TGLuint; y: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3UIEXTPROC = procedure(index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4UIEXTPROC = procedure(index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint; w: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI1IVEXTPROC = procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2IVEXTPROC = procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3IVEXTPROC = procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4IVEXTPROC = procedure(index: TGLuint; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI1UIVEXTPROC = procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2UIVEXTPROC = procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3UIVEXTPROC = procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4UIVEXTPROC = procedure(index: TGLuint; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4BVEXTPROC = procedure(index: TGLuint; v:PGLbyte);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4SVEXTPROC = procedure(index: TGLuint; v:PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4UBVEXTPROC = procedure(index: TGLuint; v: PGLUbyte);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4USVEXTPROC = procedure(index: TGLuint; v: PGLushort);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBIPOINTEREXTPROC = procedure(index: TGLuint; size: TGLint; _type: TGLenum;
                              stride: TGLsizei; _pointer: pointer);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBIIVEXTPROC = procedure(index: TGLuint; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBIUIVEXTPROC = procedure(index: TGLuint; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM1UIEXTPROC = procedure(location: TGLInt; v0: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM2UIEXTPROC = procedure(location: TGLInt; v0: TGLuint; v1: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM3UIEXTPROC = procedure(location: TGLInt; v0: TGLuint; v1: TGLuint; v2: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM4UIEXTPROC = procedure(location: TGLInt; v0: TGLuint; v1: TGLuint; v2: TGLuint; v3: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM1UIVEXTPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM2UIVEXTPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM3UIVEXTPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORM4UIVEXTPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETUNIFORMUIVEXTPROC = procedure(_program: TGLuint; location: TGLint; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDFRAGDATALOCATIONEXTPROC = procedure(_program: TGLuint; colorNumber: TGLuint; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETFRAGDATALOCATIONEXTPROC = function(_program: TGLuint; name: PGLChar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_draw_instanced (EXT #327)
  PFNGLDRAWARRAYSINSTANCEDEXTPROC = procedure(mode: TGLenum; first: TGLint; count: TGLsizei;
          primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDRAWELEMENTSINSTANCEDEXTPROC = procedure(mode: TGLenum; count: TGLSizei; _type: TGLenum;
          indices: PGLvoid; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_packed_float (EXT #328)
  // WGL_EXT_pixel_format_packed_float
  // GLX_EXT_fbconfig_packed_float


  // GL_EXT_texture_array (EXT #329)
  //glFramebufferTextureLayerEXT: procedure(target: TGLenum; attachment: TGLenum;
  //                                texture: TGLuint; level: TGLint; layer: TGLint);


  // GL_EXT_texture_buffer_object (EXT #330)
  PFNGLTEXBUFFEREXTPROC = procedure(target: TGLenum; internalformat: TGLenum; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_draw_buffers2 (EXT #340)
  PFNGLCOLORMASKINDEXEDEXTPROC = procedure(buf: TGLuint; r: TGLboolean; g: TGLboolean;
                          b: TGLboolean; a: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETBOOLEANINDEXEDVEXTPROC = procedure(value: TGLenum; index: TGLuint; data: PGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETINTEGERINDEXEDVEXTPROC = procedure(value: TGLenum; index: TGLuint; data: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLENABLEINDEXEDEXTPROC = procedure(target: TGLenum; index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLDISABLEINDEXEDEXTPROC = procedure(target: TGLenum; index: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISENABLEDINDEXEDEXTPROC = function(target: TGLenum; index: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_NV_transform_feedback (EXT #341)
  PFNGLBINDBUFFERRANGENVPROC = procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                                offset: TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDBUFFEROFFSETNVPROC = procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                                 offset: TGLintptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDBUFFERBASENVPROC = procedure(target: TGLenum; index: TGLuint; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTRANSFORMFEEDBACKATTRIBSNVPROC = procedure(count: TGLsizei; attribs: PGLint;
                                         bufferMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTRANSFORMFEEDBACKVARYINGSNVPROC = procedure(_program: TGLuint; count: TGLsizei;
                                          locations: PGLint;
                                          bufferMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBEGINTRANSFORMFEEDBACKNVPROC = procedure(primitiveMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLENDTRANSFORMFEEDBACKNVPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  PFNGLGETVARYINGLOCATIONNVPROC = function(_program: TGLuint; name: PGLChar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETACTIVEVARYINGNVPROC = procedure(_program: TGLuint; index: TGLuint;
                                 bufSize: TGLsizei; length: PGLsizei; size: PGLsizei;
                                 _type: TGLenum; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLACTIVEVARYINGNVPROC = procedure(_program: TGLuint; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETTRANSFORMFEEDBACKVARYINGNVPROC = procedure(_program: TGLuint; index: TGLuint;
                                            location: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}


  // GL_EXT_bindable_uniform (EXT #342)
  PFNGLUNIFORMBUFFEREXTPROC = procedure(_program: TGLUint; location: TGLint; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETUNIFORMBUFFERSIZEEXTPROC = function(_program: TGLuint; location: TGLint): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETUNIFORMOFFSETEXTPROC = function(_program: TGLuint; location: TGLint): PGLint;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_texture_integer (EXT #343)
  PFNGLCLEARCOLORIIEXTPROC = procedure(r: TGLint; g: TGLint; b: TGLint; a: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCLEARCOLORIUIEXTPROC = procedure(r: TGLuint; g: TGLuint; b: TGLuint; a: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXPARAMETERIIVEXTPROC = procedure(target: TGLenum; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXPARAMETERIUIVEXTPROC = procedure(target: TGLenum; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETTEXPARAMETERIIVEXTPROC = procedure(target: TGLenum; pname: TGLenum; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETTEXPARAMETERIUIVEXTPROC = procedure(target: TGLenum; pname: TGLenum; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_GREMEDY_frame_terminator (EXT #345)
  PFNGLFRAMETERMINATORGREMEDYPROC = procedure(); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_NV_conditional_render (EXT #346)
  PFNGLBEGINCONDITIONALRENDERNVPROC = procedure(id: TGLuint; mode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLENDCONDITIONALRENDERNVPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_EXT_transform_feedback (EXT #352)
  PFNGLBINDBUFFERRANGEEXTPROC = procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                          offset:TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDBUFFEROFFSETEXTPROC = procedure(target: TGLenum; index: TGLuint; buffer: TGLuint;
                          offset:TGLintptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBINDBUFFERBASEEXTPROC = procedure(target: TGLenum; index: TGLuint; buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLBEGINTRANSFORMFEEDBACKEXTPROC = procedure(primitiveMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLENDTRANSFORMFEEDBACKEXTPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTRANSFORMFEEDBACKVARYINGSEXTPROC = procedure(_program: TGLuint; count: TGLsizei;
                                    const varyings: PGLPCharArray; bufferMode: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETTRANSFORMFEEDBACKVARYINGEXTPROC = procedure(_program: TGLuint; index: TGLuint;
                                      bufSize: TGLsizei; length: PGLsizei;
                                      size: PGLsizei; _type: PGLenum; name: PGLChar);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_AMD_vertex_shader_tessellator (EXT #363)
  PFNGLTESSELLATIONFACTORAMDPROC = procedure(factor: GLfloat); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTESSELLATIONMODEAMDPROC = procedure(mode: TGLenum); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_NV_copy_image (EXT #376)
  PFNGLCOPYIMAGESUBDATANVPROC = procedure(
    srcName: GLuint; srcTarget: GLenum; srcLevel: GLint;
    srcX: GLint; srcY: GLint; srcZ: GLint;
    dstName: GLuint; dstTarget: GLenum; dstLevel: GLint;
    dstX: GLint; dstY: GLint; dstZ: GLint;
    width: GLsizei; height: GLsizei; depth: GLsizei);  {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_NV_shader_buffer_load (EXT #379)
  PFNGLMAKEBUFFERRESIDENTNVPROC = procedure(target: TGLenum; access: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMAKEBUFFERNONRESIDENTNVPROC = procedure(target: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISBUFFERRESIDENTNVPROC = function(target: TGLenum): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMAKENAMEDBUFFERRESIDENTNVPROC = procedure(buffer: TGLuint; access: TGLenum);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLMAKENAMEDBUFFERNONRESIDENTNVPROC = procedure(buffer: TGLuint);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLISNAMEDBUFFERRESIDENTNVPROC = function (buffer: TGLuint): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETBUFFERPARAMETERUI64VNVPROC = procedure(target: TGLenum; pname: TGLenum; params: PGLuint64EXT );{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETNAMEDBUFFERPARAMETERUI64VNVPROC = procedure(buffer: TGLuint; pname: TGLenum; params: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETINTEGERUI64VNVPROC = procedure(value: TGLenum; result: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMUI64NVPROC = procedure(location: TGLint; value: TGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLUNIFORMUI64VNVPROC = procedure(location: GLint; count: TGLsizei; const value: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETUNIFORMUI64VNVPROC = procedure(_program: TGLuint; location: TGLint; params: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMUI64NVPROC = procedure(_program: TGLuint; location: TGLint; value: TGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMUI64VNVPROC = procedure(_program: TGLuint; location: TGLint; count: TGLsizei; const value: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

  // GL_NV_vertex_buffer_unified_memory (EXT #380)
  PFNGLBUFFERADDRESSRANGENVPROC = procedure(pname: TGLenum; index: TGLuint; address: TGLuint64EXT; length: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXFORMATNVPROC = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLNORMALFORMATNVPROC = procedure(_type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLCOLORFORMATNVPROC = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLINDEXFORMATNVPROC = procedure(_type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLTEXCOORDFORMATNVPROC = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLEDGEFLAGFORMATNVPROC = procedure(stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLORFORMATNVPROC = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLFOGCOORDFORMATNVPROC = procedure(_type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBFORMATNVPROC = procedure(index: TGLuint; size: TGLint; _type: TGLenum; normalized: TGLboolean; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBIFORMATNVPROC = procedure(index: TGLuint; size: TGLint; _type: TGLenum; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}
  PFNGLGETINTEGERUI64I_VNVPROC = procedure(value: TGLenum; index: TGLuint; result: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

implementation

end.
