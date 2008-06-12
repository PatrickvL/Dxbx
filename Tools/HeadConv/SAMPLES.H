// Author: Bob Swart (aka Dr.Bob - www.drbob42.com)
//
// this sample header illustrate some of the fixes in HeadConv v3.24

#define LEN 42

// float
typedef struct
{
  BYTE ErrorByte;
  float Value;
}BlockFloatRec;

// double
typedef struct
{
  BYTE ErrorByte;
  double Value;
}BlockDoubleRec;

typedef struct
{
  int*   Number;
  char*  Field;
  char   List[LEN + 1];
} BUG, *pBUG;

__DR_BOB_HEADCONV_STOP
const
  CHARLEN = 42;
__DR_BOB_HEADCONV_START

struct share_info_0 {
  charshi0_netname[CHARLEN+1];
};  /* share_info_0 */

void * GEODLL_EXPORT GeoEngine(char *, int, short *, void * p, void*);

void DSSSetTransWAY(DWORD dwRecogWay) ;
void DSSGetTransWAY(DWORD *dwRecogWay) ;

typedef struct _USER_INFO_0 {
  LPWSTR   usri0_name;
}USER_INFO_0, *PUSER_INFO_0, *LPUSER_INFO_0;

typedef struct _USER_INFO_1 {
  LPWSTR   usri1_name;
  LPWSTR   usri1_password;
  DWORD    usri1_password_age;
  DWORD    usri1_priv;
  LPWSTR   usri1_home_dir;
  LPWSTR   usri1_comment;
  DWORD    usri1_flags;
  LPWSTR   usri1_script_path;
}USER_INFO_1, *PUSER_INFO_1, *LPUSER_INFO_1;
