/*********************************************************************************
 * Programa .....: imcc0300d.p                                                   *
 * Data .........: 20/01/2005                                                    *
 * Cliente ......: Ima Tecidos                                                   *
 * Programador ..: Viviane Fonseca Moreira                                       *
 * Objetivo .....: Executada botao Salvar na manutencao do pedido de compra      *
 *********************************************************************************/



def new global shared var wh-btSave-cc0300a as widget-handle no-undo.
def new global shared var wh-tp-pedido      as widget-handle no-undo.


find first im-tipo-ped where
     im-tipo-ped.cod-tipo-ped = wh-tp-pedido:SCREEN-VALUE no-lock no-error.

if not avail im-tipo-ped then do:
   message "Tipo de Pedido nao cadastrado!" view-as alert-box error.
   apply "entry" to wh-tp-pedido.
   return "NOK".
end.
else
   apply "CHOOSE" to wh-btSave-cc0300a.
   
