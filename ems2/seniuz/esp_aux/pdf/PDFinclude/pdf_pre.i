/******************************************************************************

  Program:      pdf_pre.i
  
  Written By:   Gordon Campbell
  Written On:   July 6, 2005
  
  Description:  Preprocessor defintions for PDFinclude
  
******************************************************************************/

&GLOBAL-DEFINE PDFDIR pdfinclude

&IF OPSYS = "UNIX" &THEN
  &GLOBAL-DEFINE zlib          /lib/libz.so.1
&ELSE
  &GLOBAL-DEFINE zlib         \\ntems\ems204\liasa\PDFinclude\dll\zlib1.dll /*  PDFinclude\Dll\zlib1.dll*/
  
&ENDIF

&GLOBAL-DEFINE MD5LIB          
&GLOBAL-DEFINE pdfencryptlib   

/* end of pdf_pre.i */
