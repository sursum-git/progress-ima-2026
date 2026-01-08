/******************************************************************************
*   Programa .....: epc-re2001a-z.p                                             *
*   Data .........: 05/04/2016                                                *
*   Cliente ......: Ima                                                       *
*   Programador ..: Toninho                                                   *
*   Objetivo .....: Zoom Codigo de Rejei‡Æo                                   *
*                                                                             *
******************************************************************************/

DEF NEW GLOBAL SHARED VAR wh-campo-conteudo AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR wh-window         AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pesquisa	    AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR l-implanta        AS LOGICAL INIT NO.
DEF NEW GLOBAL SHARED VAR adm-broker-hdl    AS HANDLE NO-UNDO.

{include/zoomvar.i &prog-zoom=inzoom/z01in047.r
                   &proghandle=wh-window
                   &campohandle=SELF:HANDLE
                   &campozoom=codigo-rejei}
