/****************************************************************************
** Programa: upc-cd0404.p 
** Objetivo: Criar um campo de digitacao Toggle-Box, VENDOR.
**
**           Este Novo campo sera gravado na tabela COND-PAGTO.log-2
**           N∆o Permite alteraá∆o do numero de parcela de uma condiá∆o de
**           pagamento com pedido em aberto.
**
**           Verifica se a Condiá∆o de Pagamento possui Pedidos de Venda em
**           aberto ou Ordem Compra, caso tenha, n∆o permite o usuario efetuar ALTERAÄ«O.
**           FµBIO COELHO LANZA - AGOSTO/2010.
**
**
** Autor   : FµBIO COELHO LANZA - JUNHO/2008 
*****************************************************************************/
/* Parameter Definitions ****************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR wh-vendor      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-num-parcelas AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-cod-cond-pag AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-prazo-med AS WIDGET-HANDLE NO-UNDO.

/* Variable Definitions *****************************************************/
DEF VAR h-objeto       AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo        AS WIDGET-HANDLE NO-UNDO.

DEF VAR c-objeto       AS CHAR NO-UNDO.
DEF VAR c-nr-pedcli    AS CHAR.

/* Variavies criadas na viewer dinamicamente*********************************/

/* Main Block ***************************************************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v04ad039.w" THEN DO:

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "bt-calculo" THEN 
            ASSIGN h-bt-prazo-med = h-objeto.

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03ad039.w" THEN DO:

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "lAtualizaIndice" THEN 
            ON 'value-changed':U OF h-objeto PERSISTENT RUN esupc/upc-cd0404a.p.

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v02ad039.w" THEN DO:

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "cod-cond-pag" THEN 
            ASSIGN h-cod-cond-pag = h-objeto.

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03ad039.w" THEN DO:
   CREATE TOGGLE-BOX wh-vendor
          ASSIGN FRAME              = p-wgh-frame
                 ROW                = 1.60
                 COL                = 50
                 VISIBLE            = YES
                 LABEL              = "Vendor"
                 SENSITIVE          = NO
                 TOOLTIP            = "A Condiá∆o de Pagamento Ç VENDOR".
END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03ad039.w" THEN
   ASSIGN wh-vendor:SENSITIVE = YES.


IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03ad039.w" THEN 
   ASSIGN wh-vendor:SENSITIVE = NO.


IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03ad039.w" THEN 
   ASSIGN wh-vendor:SCREEN-VALUE = "NO".


IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03ad039.w" THEN DO:
   FIND cond-pagto WHERE
        ROWID(cond-pagto) = p-row-table NO-LOCK NO-ERROR.

   IF AVAIL cond-pagto THEN 
      ASSIGN wh-vendor:SCREEN-VALUE = STRING(cond-pagto.log-2).
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03ad039.w" THEN DO:
   FIND cond-pagto WHERE
        ROWID(cond-pagto) = p-row-table NO-LOCK NO-ERROR.
   IF AVAIL cond-pagto THEN
      ASSIGN cond-pagto.log-2 = LOGICAL(wh-vendor:SCREEN-VALUE).
END.

IF p-ind-event = "VALIDATE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v04ad039.w" THEN DO:

   /* Comentado por Toninho em 20/09/2012, pois n∆o faz sentido n∆o conseguir
      alterar nada nas Condiá‰es de Pagamento com Pedido em Aberto,
      se precisar descomentar favor falar comigo 
      
   ASSIGN c-nr-pedcli = "".  
   FOR EACH ped-venda WHERE
            ped-venda.cod-cond-pag = INT(h-cod-cond-pag:SCREEN-VALUE) NO-LOCK.
       IF ped-venda.cod-sit-ped = 5 OR
          ped-venda.cod-sit-ped < 2 THEN DO:
          ASSIGN c-nr-pedcli = ped-venda.nr-pedcli.
          LEAVE.
       END.
   END.

   IF c-nr-pedcli <> "" THEN DO:
      MESSAGE 'Alteraá∆o n∆o permitida ! ! !' SKIP
              'Existe um pedido: ' c-nr-pedcli ', em aberto para esta Condiá∆o de Pagamento.'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN "NOK".
   END.
   
   FOR EACH ordem-compra WHERE
            ordem-compra.situacao = 2  NO-LOCK.
       IF ordem-compra.cod-cond-pag = INT(h-cod-cond-pag:SCREEN-VALUE) THEN DO:
           ASSIGN c-nr-pedcli = STRING(ordem-compra.numero-ordem).
           LEAVE.
       END.
   END.
   IF c-nr-pedcli <> "" THEN DO:
      MESSAGE 'Alteraá∆o n∆o permitida ! ! !' SKIP
              'Existe Ordem Compra: ' c-nr-pedcli ', em aberto para esta Condiá∆o de Pagamento.'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN "NOK".
   END.
   
   Fim comentario Toninho 20/09/2012 */
END.

