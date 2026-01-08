/******************************************************************************
*   Programa .....: imcc0300z.p                                               *
*   Data .........: 19/01/2005                                                *
*   Cliente ......: Ima Tecidos                                               *
*   Programador ..: Viviane Fonseca Moreira                                   *
*   Objetivo .....: Zoom do campo tipo pedido                                 *
******************************************************************************/




def new global shared var wh-tp-pedido as widget-handle no-undo.

def new global shared var wh-window        as handle no-undo.
def new global shared var wh-pesquisa      as widget-handle.
def new global shared var l-implanta       as logical init no.
def new global shared var adm-broker-hdl   as handle no-undo.

{include/zoomvar.i &prog-zoom=imzoom/z01imes201.w
                   &proghandle=wh-window
                   &campohandle=wh-tp-pedido
                   &campozoom=cod-tipo-ped}
