/******************************************************************************
*   Programa .....: epc-cd0704z.p                                             *
*   Data .........: 05/06/2017                                                *
*   Cliente ......: Ima                                                       *
*   Programador ..: Toninho                                                   *
*   Objetivo .....: Zoom Coligada                                             *
*                                                                             *
******************************************************************************/
DEF NEW GLOBAL SHARED VAR wh-campo-conteudo AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR wh-window         AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-pesquisa	    AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR l-implanta        AS LOGICAL INIT NO.
DEF NEW GLOBAL SHARED VAR adm-broker-hdl    AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-coligada AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-coligada AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-coligada AS CHAR.

ASSIGN c-coligada = wh-coligada:SCREEN-VALUE.

{include/zoomvar.i &prog-zoom=adzoom/z02ad098.r
                   &proghandle=wh-window
                   &campohandle=wh-coligada
                   &campozoom=cod-emitente}
                   
