/***********************************************************************************
 *   Programa .....: imce0206.p                                                    *  
 *   Data .........: 24/01/2005                                                    *
 *   Cliente ......: Ima Tecidos                                                   *
 *   Programador ..: Viviane Fonseca Moreira                                       *
 *   Objetivo .....: Verifica a quantidade disponivel do item na tabela especifica *
 *                   de pedido de compra                                           *
 ***********************************************************************************/

/****** Parametros ******/
DEF INPUT PARAMETER p-ind-event  AS CHARACTER.              
DEF INPUT PARAMETER p-ind-object AS CHARACTER.              
DEF INPUT PARAMETER p-wgh-object AS HANDLE.                 
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.          
DEF INPUT PARAMETER p-cod-table  AS CHARACTER.              
DEF INPUT PARAMETER p-row-table  AS ROWID.                  

/****** Vari veis ******/
def new global shared var wh-cod-estabel-ce0206 as widget-handle no-undo.
def new global shared var wh-cod-depos-ce0206   as widget-handle no-undo.
def new global shared var wh-cod-localiz-ce0206 as widget-handle no-undo.
def new global shared var wh-quantidade-ce0206  as widget-handle no-undo.
def new global shared var wh-it-codigo-ce0206   as widget-handle no-undo.
def new global shared var wh-cod-refer-ce0206   as widget-handle no-undo.

def new global shared var c-seg-usuario as char format "x(12)" no-undo.

def var qtd-disp-total like im-item-ped-compra-pi.qtd-recebida-disponivel no-undo.

def var c-char  as char.
def var h-frame as widget-handle no-undo.
def var h-campo as widget-handle no-undo.

DEF BUFFER b-movto-estoq FOR movto-estoq.
DEF BUFFER b-im-item-ped-compra-pi FOR im-item-ped-compra-pi.

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


if p-ind-event  = 'BEFORE-INITIALIZE' and
   p-ind-object = 'VIEWER' and
   c-char       = 'v12in218.w' then do:

   assign h-campo = p-wgh-frame:FIRST-CHILD
          h-campo = h-campo:FIRST-CHILD.
   do while valid-handle(h-campo):
      /***message h-campo:name view-as alert-box.***/

      if h-campo:name = 'cod-estabel' then
         assign wh-cod-estabel-ce0206 = h-campo.

      if h-campo:name = 'cod-depos' then
         assign wh-cod-depos-ce0206 = h-campo.

      if h-campo:name = 'cod-localiz' then
         assign wh-cod-localiz-ce0206 = h-campo.

      if h-campo:name = 'quantidade' then
         assign wh-quantidade-ce0206 = h-campo.

      if h-campo:name = 'it-codigo' then
         assign wh-it-codigo-ce0206 = h-campo.

      if h-campo:name = 'cod-refer' then
         assign wh-cod-refer-ce0206 = h-campo.

      if h-campo:TYPE = 'field-group'
         then assign h-campo = h-campo:FIRST-CHILD.
         else assign h-campo = h-campo:NEXT-SIBLING.
   end.
end.


if p-ind-event  = 'VALIDATE' and
   p-ind-object = 'VIEWER' and
   c-char       = 'v12in218.w' then do:

   find im-tipo-ped where
        im-tipo-ped.cod-tipo-ped = "PI" and
        im-tipo-ped.cod-gestor   = c-seg-usuario no-lock no-error.

   if avail im-tipo-ped then do:
      if im-tipo-ped.cod-estabel <> wh-cod-estabel-ce0206:screen-value or
         im-tipo-ped.cod-depos   <> wh-cod-depos-ce0206:screen-value or
         im-tipo-ped.cod-localiz <> wh-cod-localiz-ce0206:screen-value then do:

         message "Usu rio" c-seg-usuario "nÆo pode efetuar transferˆncia para esta localiza‡Æo!"
                 view-as alert-box error.
         apply "entry" to wh-cod-estabel-ce0206.
         return "NOK".
      end.

      assign qtd-disp-total = 0.
      for each b-im-item-ped-compra-pi where
               b-im-item-ped-compra-pi.it-codigo = wh-it-codigo-ce0206:screen-value and
               b-im-item-ped-compra-pi.cod-refer = wh-cod-refer-ce0206:screen-value and
              (b-im-item-ped-compra-pi.Sit-Item-Ped-Compra = 1 or
               b-im-item-ped-compra-pi.Sit-Item-Ped-Compra = 2 or
               b-im-item-ped-compra-pi.Sit-Item-Ped-Compra = 3):

          assign qtd-disp-total = qtd-disp-total + b-im-item-ped-compra-pi.qtd-recebida-disponivel.
      end.
      
      if dec(wh-quantidade-ce0206:screen-value) > qtd-disp-total then do:
         message "Quantidade nÆo pode ser maior que a Quantidade Recebida Dispon¡vel para venda:"
                 qtd-disp-total view-as alert-box error.
         apply "entry" to wh-quantidade-ce0206.
         return "NOK". 
      end.
   end.
end.

if p-ind-event  = 'END-UPDATE' and
   p-ind-object = 'VIEWER' and
   c-char       = 'v12in218.w' then do:

   find first b-movto-estoq where rowid(b-movto-estoq) = p-row-table no-error.
   if avail b-movto-estoq then do:
      if valid-handle(wh-quantidade-ce0206) then do:
         assign qtd-disp-total = dec(wh-quantidade-ce0206:screen-value).

         for each b-im-item-ped-compra-pi where
                  b-im-item-ped-compra-pi.it-codigo = wh-it-codigo-ce0206:screen-value and
                  b-im-item-ped-compra-pi.cod-refer = wh-cod-refer-ce0206:screen-value and
                 (b-im-item-ped-compra-pi.Sit-Item-Ped-Compra = 1 or
                  b-im-item-ped-compra-pi.Sit-Item-Ped-Compra = 2 or
                  b-im-item-ped-compra-pi.Sit-Item-Ped-Compra = 3):

             if qtd-disp-total >= b-im-item-ped-compra-pi.qtd-recebida-disponivel then
                assign qtd-disp-total = qtd-disp-total - b-im-item-ped-compra-pi.qtd-recebida-disponivel
                       b-im-item-ped-compra-pi.qtd-recebida-disponivel = 0.
             else
                assign b-im-item-ped-compra-pi.qtd-recebida-disponivel =
                       b-im-item-ped-compra-pi.qtd-recebida-disponivel - qtd-disp-total
                       qtd-disp-total = 0.
         end.
      end.
   end.
end.
