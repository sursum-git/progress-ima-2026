/******************************************************************************
*   Programa .....: imcc0300.p                                                *
*   Data .........: 24/01/2005                                                *
*   Cliente ......: Ima Tecidos                                               *
*   Programador ..: Viviane Fonseca Moreira                                   *
*   Objetivo .....: Verifica Eliminacao dos Itens e do Pedido de Compra       *
******************************************************************************/


/****** Parametros ******/
DEF INPUT PARAMETER p-ind-event  AS CHARACTER.              
DEF INPUT PARAMETER p-ind-object AS CHARACTER.              
DEF INPUT PARAMETER p-wgh-object AS HANDLE.                 
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.          
DEF INPUT PARAMETER p-cod-table  AS CHARACTER.              
DEF INPUT PARAMETER p-row-table  AS ROWID.                  

DEF VAR c-char  AS CHAR.
DEF VAR h-frame AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo AS WIDGET-HANDLE NO-UNDO.

DEF BUFFER b-pedido-compr FOR pedido-compr.
DEF BUFFER b-ordem-compra FOR ordem-compra.
DEF BUFFER b-im-item-ped-compra-pi FOR im-item-ped-compra-pi.

def new global shared var l-modifica-cc0300 as logical.
def new global shared var l-elimina-cc0300   as logical.

IF VALID-HANDLE(p-wgh-object) THEN
   ASSIGN c-char = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,"~/"), p-wgh-object:FILE-NAME,"~/").


/* MESSAGE p-ind-event                          */
/*         p-ind-object skip                    */
/*         "Objeto:" string(p-wgh-object) skip  */
/*         "frame:" p-wgh-frame skip            */
/*         "frame:" string(p-wgh-frame) skip    */
/*         "Cod table:" p-cod-table skip        */
/*         "Rowid:" string(p-row-table) SKIP    */
/*         c-char  VIEW-AS ALERT-BOX.           */


if p-ind-event  = 'before-initialize' and
   p-ind-object = 'container' and
   c-char       = 'cc0300.w' then do:

   assign l-elimina-cc0300 = no.
                
   assign h-frame = p-wgh-frame:FIRST-CHILD
          h-frame = h-frame:FIRST-CHILD.

   do while valid-handle(h-frame):
      /*message h-frame:name view-as alert-box.*/

      if h-frame:name = 'btUpdate' then do:
         on "ENTRY" of h-frame PERSISTENT RUN imepc/imcc0300f.p.
      end.

      if h-frame:name = 'btCopy' then do:
         on "ENTRY" of h-frame PERSISTENT RUN imepc/imcc0300g.p.
      end.

      if h-frame:TYPE = 'field-group'
         then assign h-frame = h-frame:FIRST-CHILD.
         else assign h-frame = h-frame:NEXT-SIBLING.
   end.
end.


if p-ind-event  = 'DELETE-PEDIDO' and
   p-ind-object = 'BTDELETE' and
   c-char       = 'cc0300.w' then do:

   find first b-pedido-compr where ROWID(b-pedido-compr) = p-row-table no-lock no-error.
   if avail b-pedido-compr then do:
      
      find first b-im-item-ped-compra-pi where
           b-im-item-ped-compra-pi.num-pedido-compra = b-pedido-compr.num-pedido and
           b-im-item-ped-compra-pi.cod-emitente      = b-pedido-compr.cod-emitente and
          (b-im-item-ped-compra-pi.qtd-recebida-estoque > 0 or
           b-im-item-ped-compra-pi.qtd-reservada-venda > 0 or
           b-im-item-ped-compra-pi.qtd-solicitada <> b-im-item-ped-compra-pi.qtd-solic-disponivel) no-error.
      if avail b-im-item-ped-compra-pi then do:
         message "Pedido de Compra n∆o pode ser Eliminado!" skip
                 "Existe Quantidade Recebida no Estoque ou Quantidade Solicitada para venda deste pedido."
                 view-as alert-box error.
         return "NOK".
      end.
   end.
end.

if p-ind-event  = 'DELETE-ORDEM' and
   p-ind-object = 'BTDELETESON1' and
   c-char       = 'cc0300.w' then do:

   find first b-ordem-compra where ROWID(b-ordem-compra) = p-row-table no-lock no-error.
   if avail b-ordem-compra then do:
      find first b-im-item-ped-compra-pi where
           b-im-item-ped-compra-pi.num-pedido-compra = b-ordem-compra.num-pedido and
           b-im-item-ped-compra-pi.cod-emitente      = b-ordem-compra.cod-emitente and
           b-im-item-ped-compra-pi.it-codigo         = b-ordem-compra.it-codigo and
          (b-im-item-ped-compra-pi.qtd-recebida-estoque > 0 or
           b-im-item-ped-compra-pi.qtd-reservada-venda > 0 or
           b-im-item-ped-compra-pi.qtd-solicitada <> b-im-item-ped-compra-pi.qtd-solic-disponivel) no-error.
      if avail b-im-item-ped-compra-pi then do:
         message "Ordem de Compra n∆o pode ser Eliminada!" skip
                 "Existe Quantidade Recebida no Estoque ou Quantidade Solicitada para venda deste item."
                 view-as alert-box error.
         return "NOK".
      end.

      assign l-elimina-cc0300 = yes.
   end.
end.
