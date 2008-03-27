unit uConsts;

interface

const
  // Application Versions
  _DXBX_VERSION = '0.0.0.7';
  _XDK_TRACKER_VERSION = '2.0.0.2';

  XOR_EP_DEBUG = $94859D4B; //Entry Point (Debug)
  XOR_EP_RETAIL = $A8FC57AB; // Entry Point (Retail)
  XOR_KT_DEBUG = $EFB1F152; // Kernel Thunk (Debug)
  XOR_KT_RETAIL = $5B6D40B6; // Kernel Thunk (Retail)

  PE_FILE_ALIGN = $00000020; // File alignment
  PE_SEGM_ALIGN = $00000020; // Segment alignment

  cXDK_TRACKER_DATA_FILE = 'GameData.dat';
  cXDk_TRACKER_XML_VERSION = '1.0';

  // Websites
  cWEBSITE_CXBX = 'http://www.caustik.com/cxbx/';
  cWEBSITE_SHADOWTJ = 'http://www.shadowtj.org';
  cWEBSITE_FORUM = 'http://forums.ngemu.com/cxbx-official-discussion/';

  cOpen = 'open';

  // Xbe File Format
  _MagicNumber = 'XBEH';       

  // Limits
  _RecentXbeLimit: Integer = 10;
  _RecentExeLimit: Integer = 10;

implementation

end.
