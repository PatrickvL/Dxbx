unit uProlog;

interface

var

  // ******************************************************************
  // *
  // * WARNING:
  // *
  // * This is hard-coded as an array to stress the point that
  // * it can not be modified (unless you really know what you're
  // * doing). EmuExe will modify the contents of this function
  // * directly, so this precise structure is necessary.
  // *
  // ******************************************************************
  Prolog: array[0..255] of Byte = (
    $BE, $C3, $C3, $C3, $C3, // mov esi, 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $68, $C3, $C3, $C3, $C3, // push 0xC3C3C3C3
    $FF, $D6,                // call esi
    $C3, $CC, $CC, $CC, $CC, // ret
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC);


  EndFilling: array[0..524] of Byte = (
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CC, $CC, $CC, $CC, $CC,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD,
    $CD, $CD, $CD, $CD, $CD);


implementation

end.
