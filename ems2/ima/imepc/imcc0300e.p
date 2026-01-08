/*********************************************************************************
 * Programa .....: imcc0300e.p                                                   *
 * Data .........: 20/01/2005                                                    *
 * Cliente ......: Ima Tecidos                                                   *
 * Programador ..: Viviane Fonseca Moreira                                       *
 * Objetivo .....: Executada botao OK na manutencao dos itens do pedido de compra*
 *********************************************************************************/


def new global shared var wh-num-pedido-cc0300b    as widget-handle no-undo.
def new global shared var wh-cod-emitente-cc0300b  as widget-handle no-undo.
def new global shared var wh-nome-fornec-cc0300b   as widget-handle no-undo.
def new global shared var wh-it-codigo-cc0300b     as widget-handle no-undo.
def new global shared var wh-cod-refer-cc0300b     as widget-handle no-undo.
def new global shared var wh-browse-cc0300b        as widget-handle no-undo.
def new global shared var wh-query-cc0300b         as widget-handle no-undo.
def new global shared var wh-buffer-cc0300b        as widget-handle no-undo.
def new global shared var wh-btOK-cc0300b          as widget-handle no-undo.

def var qtde-parcela-antiga as decimal format ">>>>,>>9.9999" no-undo.
def var qtd-solic-venda     like im-item-vinc.qtd-solicitada no-undo.
def var qtd-solic-compra    as decimal format ">>>>,>>9.9999" no-undo.
def var l-existe            as logical no-undo.

def var wh-it-codigo    as widget-handle no-undo.
def var wh-quantidade   as widget-handle no-undo.
def var wh-situacao     as widget-handle no-undo.
def var wh-cod-refer    as widget-handle no-undo.

def new global shared temp-table tt-prazo-old no-undo
    field it-codigo  like prazo-compra.it-codigo
    field quant-old  like prazo-compra.quantidade
    field cod-refer  like prazo-compra.cod-refer.

def temp-table tt-prazo-new no-undo
    field it-codigo  like prazo-compra.it-codigo
    field quant-new  like prazo-compra.quantidade
    field cod-refer  like prazo-compra.cod-refer.

def buffer b-im-item-ped-compra-pi for im-item-ped-compra-pi.
def buffer b-im-item-vinc for im-item-vinc.


find im-ext-pedido-compr where
     im-ext-pedido-compr.num-pedido = int(wh-num-pedido-cc0300b:screen-value) no-lock no-error.

if avail im-ext-pedido-compr and im-ext-pedido-compr.Cod-Tipo-Ped = "PI" then do:
   if wh-cod-refer-cc0300b:screen-value = "" then do:
      message "Referància do Item deve ser informada!" view-as alert-box error.
      return "NOK".
   end.
    
   l-existe = wh-query-cc0300b:GET-FIRST().
   if l-existe then do: /*existe registro na query*/
      Bloco-1:
      Repeat: 
         assign wh-it-codigo    = wh-buffer-cc0300b:buffer-field(3)
                wh-quantidade   = wh-buffer-cc0300b:buffer-field(6)
                wh-situacao     = wh-buffer-cc0300b:buffer-field(23)
                wh-cod-refer    = wh-buffer-cc0300b:buffer-field(25).
    
   /*       do i = 1 to 58:                                                    */
   /*          assign h-teste = wh-buffer-cc0300b:buffer-field(i).             */
   /*          message "i:" i skip                                             */
   /*                  h-teste:name view-as alert-box.                         */
   /*       end.                                                               */
   /*                                                                          */
   /*       message "Item" wh-it-codigo:buffer-value skip                      */
   /*               "Quantid. Orig" wh-quantid-orig:buffer-value skip          */
   /*               "Quantidade" wh-quantidade:buffer-value skip               */
   /*               "Quantid. Saldo" wh-quant-saldo:buffer-value skip          */
   /*               "Quantid. Receb" wh-quant-receb:buffer-value skip          */
   /*               "Nome abrev" wh-nome-abrev:buffer-value skip               */
   /*               "Situacao" wh-situacao:buffer-value skip                   */
   /*               "Cod. Refer." wh-cod-refer:buffer-value view-as alert-box. */
    
         if wh-situacao:buffer-value <> 4 then do:
            find first tt-prazo-new where
                       tt-prazo-new.cod-refer = wh-cod-refer:buffer-value
                       no-lock no-error.
            if not avail tt-prazo-new then do:
               create tt-prazo-new.
               assign tt-prazo-new.it-codigo = wh-it-codigo:buffer-value
                      tt-prazo-new.quant-new = wh-quantidade:buffer-value
                      tt-prazo-new.cod-refer = wh-cod-refer:buffer-value.
            end.
            else
               assign tt-prazo-new.quant-new = tt-prazo-new.quant-new +
                                               wh-quantidade:buffer-value.
         end.

         l-existe = wh-query-cc0300b:GET-NEXT().
         if not l-existe then do:
            wh-query-cc0300b:GET-FIRST().
            leave Bloco-1.
         end.
      end.
   end.


   /** Elimina os itens/referencias da Tabela especifica **/
   for each tt-prazo-old:
  /*     message "tt-prazo-old" tt-prazo-old.it-codigo skip
                     tt-prazo-old.cod-refer view-as alert-box. */

       find first tt-prazo-new where
                   tt-prazo-new.it-codigo = tt-prazo-old.it-codigo and
                   tt-prazo-new.cod-refer = tt-prazo-old.cod-refer
                   no-error.

       if not avail tt-prazo-new  then do:

 /*          message "n∆o achei tt-prazo-new" view-as alert-box. */


         find first b-im-item-ped-compra-pi where
               b-im-item-ped-compra-pi.num-pedido-compra = int(wh-num-pedido-cc0300b:screen-value) and
               b-im-item-ped-compra-pi.nome-abrev             = wh-nome-fornec-cc0300b:screen-value and
               b-im-item-ped-compra-pi.cod-emitente           = int(wh-cod-emitente-cc0300b:screen-value) and
               b-im-item-ped-compra-pi.it-codigo                 = tt-prazo-old.it-codigo and
               b-im-item-ped-compra-pi.cod-refer                 = tt-prazo-old.cod-refer
               no-error.
         if avail b-im-item-ped-compra-pi then do:
            assign b-im-item-ped-compra-pi.qtd-solicitada = b-im-item-ped-compra-pi.qtd-solicitada -
                                                            tt-prazo-old.quant-old
                      b-im-item-ped-compra-pi.qtd-solic-disponivel = b-im-item-ped-compra-pi.qtd-solicitada.
    
            if b-im-item-ped-compra-pi.qtd-solicitada <= 0 then
               delete b-im-item-ped-compra-pi.
               /*assign b-im-item-ped-compra-pi.sit-item-ped-compra = 6. /*Excluido*/*/
         end.
       end.
       else do:
    /*       message "encontrei b-im-item-ped-compra-pi" view-as alert-box.  */
           find first b-im-item-ped-compra-pi where
               b-im-item-ped-compra-pi.num-pedido-compra = int(wh-num-pedido-cc0300b:screen-value) and
               b-im-item-ped-compra-pi.nome-abrev             = wh-nome-fornec-cc0300b:screen-value and
               b-im-item-ped-compra-pi.cod-emitente           = int(wh-cod-emitente-cc0300b:screen-value) and
               b-im-item-ped-compra-pi.it-codigo                 = tt-prazo-old.it-codigo and
               b-im-item-ped-compra-pi.cod-refer                 = tt-prazo-old.cod-refer
               no-error.
         if avail b-im-item-ped-compra-pi then do:

     /*        message "entrei " b-im-item-ped-compra-pi.qtd-solicitada  tt-prazo-old.quant-old view-as alert-box.  */
             if  b-im-item-ped-compra-pi.qtd-solicitada <> tt-prazo-old.quant-old then do:
                   assign b-im-item-ped-compra-pi.qtd-solicitada = b-im-item-ped-compra-pi.qtd-solicitada -
                                                            tt-prazo-old.quant-old
                      b-im-item-ped-compra-pi.qtd-solic-disponivel = b-im-item-ped-compra-pi.qtd-solicitada.
             end.
            if b-im-item-ped-compra-pi.qtd-solicitada <= 0 then
               delete b-im-item-ped-compra-pi.
         end.
       end.
   end.


   for each tt-prazo-new:
       find first b-im-item-ped-compra-pi where
            b-im-item-ped-compra-pi.num-pedido-compra = int(wh-num-pedido-cc0300b:screen-value) and
            b-im-item-ped-compra-pi.nome-abrev        = wh-nome-fornec-cc0300b:screen-value and
            b-im-item-ped-compra-pi.cod-emitente      = int(wh-cod-emitente-cc0300b:screen-value) and
            b-im-item-ped-compra-pi.it-codigo         = tt-prazo-new.it-codigo and
            b-im-item-ped-compra-pi.cod-refer         = tt-prazo-new.cod-refer
            no-error.
    
       if avail b-im-item-ped-compra-pi then do:
          find first tt-prazo-old where tt-prazo-old.cod-refer = tt-prazo-new.cod-refer no-error.
          if avail tt-prazo-old then
             assign qtde-parcela-antiga = tt-prazo-old.quant-old.
          else
             assign qtde-parcela-antiga = 0.
    
          /* Verifica se ja existe quantidade recebida no estoque e se esta quantidade nao pode ser 
             maior que a quantidade solicitada na compra */
    
          assign qtd-solic-compra = b-im-item-ped-compra-pi.Qtd-Solicitada -
                                    qtde-parcela-antiga +
                                    tt-prazo-new.quant-new.
    
          if qtd-solic-compra < b-im-item-ped-compra-pi.Qtd-Recebida-Estoque then do:
             message "J† foram recebidos no estoque" b-im-item-ped-compra-pi.Qtd-Recebida-Estoque
                     "para a Referància" tt-prazo-new.cod-refer view-as alert-box error.
             return "NOK".
          end.
    
    
          /* Verifica se ja existem pedidos de venda vinculados a este pedido de compra. A quantidade
             solicitada para estes pedidos nao pode ser maior que a quantidade solicitada na compra */
    
          assign qtd-solic-venda = 0.
          for each b-im-item-vinc where
              b-im-item-vinc.num-pedido-compra = int(wh-num-pedido-cc0300b:screen-value) and
              b-im-item-vinc.nome-abrev        = wh-nome-fornec-cc0300b:screen-value and
              b-im-item-vinc.cod-emitente      = int(wh-cod-emitente-cc0300b:screen-value) and
              b-im-item-vinc.it-codigo         = tt-prazo-new.it-codigo and
              b-im-item-vinc.cod-refer         = tt-prazo-new.cod-refer:
    
              assign qtd-solic-venda = qtd-solic-venda + b-im-item-vinc.qtd-solicitada.
          end.
    
          if qtd-solic-compra < qtd-solic-venda then do:
             message "J† foram solicitados" qtd-solic-venda "para a Referància" tt-prazo-new.cod-refer
                     "do item" tt-prazo-new.it-codigo "no pedido de venda!"
                     view-as alert-box error.
             return "NOK".
          end.
       end.
   end.
end.


apply "CHOOSE" to wh-btOK-cc0300b.

