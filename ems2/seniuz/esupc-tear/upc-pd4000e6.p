/* Programa: upc-pd4000e6.p
** Objetivo: Trigger de 'row-display" ou "Entry' para o browse br-itens (itens do pedido)
**           Altera a Imagem do bot∆o que confirma o pedido (wh-btn-confirma)
**           conforme a situaá∆o do Pedido 
** Autor...: DBNet - Toninho  Maráo/2005
*/

DEF NEW GLOBAL SHARED VAR h-nome-abrev AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nr-pedcli AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-btn-confirma AS WIDGET-HANDLE NO-UNDO.

FIND ped-venda WHERE
     ped-venda.nome-abrev = h-nome-abrev:SCREEN-VALUE AND
     ped-venda.nr-pedcli = h-nr-pedcli:SCREEN-VALUE NO-LOCK NO-ERROR.

IF VALID-HANDLE(wh-btn-confirma) THEN DO.
   IF ped-venda.completo THEN
      wh-btn-confirma:LOAD-IMAGE("image/imt-chck1.bmp").
   ELSE
      wh-btn-confirma:LOAD-IMAGE("image/im-uchk4.bmp").
END.

