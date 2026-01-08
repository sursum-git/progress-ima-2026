/******************************************************************************
*   Programa .....: cd0708-zoom.p                                              *
*   Data .........: 31/10/2002                                                *
*   Cliente ......: Ima                                                       *
*   Programador ..: Vin¡cius                                                  *
*   Objetivo .....: Zoom do campo usu rio                                 *
*                                                                             *
******************************************************************************/

DEF NEW GLOBAL SHARED VAR wh-campo-conteudo AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR wh-window         AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pesquisa	    AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR l-implanta        AS LOGICAL INIT NO.
DEF NEW GLOBAL SHARED VAR adm-broker-hdl    AS HANDLE NO-UNDO.

{include/zoomvar.i &prog-zoom=unzoom/z01un178.r
                   &proghandle=wh-window
                   &campohandle=wh-campo-conteudo
                   &campozoom=cod_usuario}
