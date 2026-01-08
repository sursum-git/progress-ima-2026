/******************************************************************************
*   Programa .....: imcc0300b.p                                               *
*   Data .........: 18/01/2005                                                *
*   Cliente ......: Ima Tecidos                                               *
*   Programador ..: Viviane Fonseca Moreira                                   *
*   Objetivo .....: Itens do Pedido de Compra PI                              *
******************************************************************************/


/****** Parametros ******/
DEF INPUT PARAMETER p-ind-event  AS CHARACTER.              
DEF INPUT PARAMETER p-ind-object AS CHARACTER.              
DEF INPUT PARAMETER p-wgh-object AS HANDLE.                 
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.          
DEF INPUT PARAMETER p-cod-table  AS CHARACTER.              
DEF INPUT PARAMETER p-row-table  AS ROWID.                  

/****** Vari†veis ******/
def new global shared var wh-num-pedido-cc0300b   as widget-handle no-undo.
def new global shared var wh-cod-emitente-cc0300b as widget-handle no-undo.
def new global shared var wh-nome-fornec-cc0300b  as widget-handle no-undo.
def new global shared var wh-it-codigo-cc0300b    as widget-handle no-undo.
def new global shared var wh-cod-refer-cc0300b    as widget-handle no-undo.
def new global shared var wh-browse-cc0300b       as widget-handle no-undo.
def new global shared var wh-query-cc0300b        as widget-handle no-undo.
def new global shared var wh-buffer-cc0300b       as widget-handle no-undo.
def new global shared var wh-btOK-cc0300b         as widget-handle no-undo.
def new global shared var wh-btSave-cc0300b       as widget-handle no-undo.
def new global shared var wh-btOK-novo-b          as widget-handle no-undo.
def new global shared var wh-btSave-novo-b        as widget-handle no-undo.
def new global shared var wh-frame-item           as widget-handle no-undo.


DEF VAR c-char  AS CHAR.
DEF VAR h-frame AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo AS WIDGET-HANDLE NO-UNDO.

def var l-existe as logical no-undo.

def var wh-it-codigo    as widget-handle no-undo.
def var wh-quantidade   as widget-handle no-undo.
def var wh-situacao     as widget-handle no-undo.
def var wh-cod-refer    as widget-handle no-undo.

def new global shared temp-table tt-prazo-old no-undo
    field it-codigo  like prazo-compra.it-codigo
    field quant-old  like prazo-compra.quantidade
    field cod-refer  like prazo-compra.cod-refer.


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


if p-ind-event  = 'AFTER-INITIALIZE' and
   p-ind-object = 'CONTAINER' and
   c-char       = 'cc0300b.w' then do:
   
   assign h-frame = p-wgh-frame:FIRST-CHILD
          h-frame = h-frame:FIRST-CHILD.

   do while valid-handle(h-frame):
      /*message h-frame:name view-as alert-box.*/

      case h-frame:name:
           when 'fPage3' then do:
                assign h-campo = h-frame:FIRST-CHILD.
                do while valid-handle(h-campo):
                   /*MESSAGE h-campo:NAME VIEW-AS ALERT-BOX.*/
                  
                   case h-campo:name:
                        when 'cod-refer' then
                             assign wh-cod-refer-cc0300b = h-campo.

                        when 'BROWSE-1' then do:
                             assign wh-browse-cc0300b = h-campo.
                             assign wh-query-cc0300b  = wh-browse-cc0300b:QUERY
                                    wh-buffer-cc0300b = wh-query-cc0300b:GET-BUFFER-HANDLE(1).

                             for each tt-prazo-old: delete tt-prazo-old. end.

                             l-existe = wh-query-cc0300b:GET-FIRST().
                             if l-existe then do: /*existe registro na query*/
                                Bloco-1:
                                Repeat: 
                                   assign wh-it-codigo  = wh-buffer-cc0300b:buffer-field(3)
                                          wh-quantidade = wh-buffer-cc0300b:buffer-field(6)
                                          wh-situacao   = wh-buffer-cc0300b:buffer-field(23)
                                          wh-cod-refer  = wh-buffer-cc0300b:buffer-field(25).
                                   
                                   if wh-situacao:buffer-value <> 4 then do:
                                      find first tt-prazo-old where
                                                 tt-prazo-old.cod-refer = wh-cod-refer:buffer-value
                                                 no-lock no-error.
                                      if not avail tt-prazo-old then do:
                                         create tt-prazo-old.
                                         assign tt-prazo-old.it-codigo = wh-it-codigo:buffer-value
                                                tt-prazo-old.quant-old = wh-quantidade:buffer-value
                                                tt-prazo-old.cod-refer = wh-cod-refer:buffer-value.
                                      end.
                                      else
                                         assign tt-prazo-old.quant-old = tt-prazo-old.quant-old +
                                                                         wh-quantidade:buffer-value.
                                   end.
                                   
                                   l-existe = wh-query-cc0300b:GET-NEXT().
                                   if not l-existe then do:
                                      wh-query-cc0300b:GET-FIRST().
                                      leave Bloco-1.
                                   end.
                                end.
                             end.
                        end.
                   end case.
                  
                   if h-campo:TYPE = 'field-group'
                      then assign h-campo = h-campo:FIRST-CHILD.
                      else assign h-campo = h-campo:NEXT-SIBLING.
                end.
           end.

           when 'num-pedido' then
                assign wh-num-pedido-cc0300b = h-frame.
    
           when 'cod-emitente' then
                assign wh-cod-emitente-cc0300b = h-frame.
    
           when 'c-nome-fornec' then
                assign wh-nome-fornec-cc0300b = h-frame.
    
           when 'it-codigo' then
                assign wh-it-codigo-cc0300b = h-frame.


           when 'btOK' then do:
                assign wh-btOK-cc0300b = h-frame
                       wh-frame-item = h-frame:frame.

                IF NOT VALID-HANDLE(wh-btOK-novo-b) THEN DO:
                   CREATE BUTTON wh-btOK-novo-b
                   ASSIGN frame     = wh-frame-item /*p-wgh-frame*/
                          width     = h-frame:width
                          height    = h-frame:height
                          row       = h-frame:row
                          col       = h-frame:col
                          label     = 'OK'
                          visible   = yes
                          sensitive = yes
                          tooltip   = ''
                   TRIGGERS:
                       on choose persistent run "imepc/imcc0300e.p".
                   END TRIGGERS.
                END.                
           end.

           when 'btSave' then
                assign wh-btSave-cc0300b = h-frame.
      end case.

      if h-frame:TYPE = 'field-group'
         then assign h-frame = h-frame:FIRST-CHILD.
         else assign h-frame = h-frame:NEXT-SIBLING.
   end.
end.


if p-ind-event  = 'DELETE-PARCELA' and
   p-ind-object = 'BTDELETESON1' and
   c-char       = 'cc0300b.w' then do:

/*    message "vou eliminar a parcela" skip                                */
/*                   wh-num-pedido-cc0300b:screen-value skip               */
/*                   wh-nome-fornec-cc0300b:screen-value skip              */
/*                   wh-cod-emitente-cc0300b:screen-value skip             */
/*                   wh-it-codigo-cc0300b:screen-value skip                */
/*                   wh-cod-refer-cc0300b:screen-value view-as alert-box.  */

   find first im-item-ped-compra-pi where
               im-item-ped-compra-pi.num-pedido-compra = int(wh-num-pedido-cc0300b:screen-value) and
               im-item-ped-compra-pi.nome-abrev        = wh-nome-fornec-cc0300b:screen-value and
               im-item-ped-compra-pi.cod-emitente      = int(wh-cod-emitente-cc0300b:screen-value) and
               im-item-ped-compra-pi.it-codigo         = wh-it-codigo-cc0300b:screen-value and
               im-item-ped-compra-pi.cod-refer         = wh-cod-refer-cc0300b:screen-value
               no-lock no-error.
   if avail im-item-ped-compra-pi then do:
      if (im-item-ped-compra-pi.qtd-recebida-estoque > 0 or
          im-item-ped-compra-pi.qtd-reservada-venda > 0 or
          im-item-ped-compra-pi.qtd-solicitada <> im-item-ped-compra-pi.qtd-solic-disponivel) then do:
          message "Parcela de Compra n∆o pode ser Eliminada!" skip
                        "Existe Quantidade Recebida no Estoque ou Quantidade Solicitada para venda deste item."
                        view-as alert-box error.

          RETURN "NOK".
      end.
   end.
end.
