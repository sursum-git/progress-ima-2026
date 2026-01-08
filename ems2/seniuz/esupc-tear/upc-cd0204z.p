def var wh-pesquisa as widget-handle.
def new global shared var l-implanta as logical init no.
def new global shared var wh-composi    as widget-handle no-undo.
def new global shared var wh-window  as handle no-undo.
def new global shared var adm-broker-hdl as handle no-undo.

{include/zoomvar.i &prog-zoom=eszoom/z01es003.w 
                   &proghandle=wh-window
                   &campohandle=wh-composi
                   &campozoom=cod-composi}
