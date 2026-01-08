/* Programa: upc-pd4000m1.p
** Objetivo: Trigger de 'Mouse-Select-Click' para o botÆo que Completa o Pedido
**           Replicar o pre‡o do Primeiro artigo aos demais artigos iguais
**           desde que o a var¡avel l-preco-unico definida no programa de 
**           parƒmetros (pd4000b) esteja marcada (yes).
** Autor...: DbNET - Toninho  Novembro/2004
*/

DEF NEW GLOBAL SHARED VAR h-nome-abrev AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nr-pedcli AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-btCompletOrder AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR l-preco-unico AS LOG.
DEF VAR c-lote AS CHAR.

DEF BUFFER b-ped-item FOR ped-item.

DEF TEMP-TABLE tt-ped-item LIKE ped-item 
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi154 AS HANDLE.
DEF VAR h-bodi154com AS HANDLE.

IF l-preco-unico THEN DO.
   IF NOT VALID-HANDLE(h-bodi154) OR 
      h-bodi154:TYPE      <> "PROCEDURE":U OR
      h-bodi154:FILE-NAME <> "dibo/bodi154.p":U THEN
      RUN dibo/bodi154.p PERSISTENT SET h-bodi154.

   IF NOT VALID-HANDLE(h-bodi154com) OR
      h-bodi154com:TYPE      <> "PROCEDURE":U OR
      h-bodi154com:FILE-NAME <> "dibo/bodi154com.p":U THEN
      RUN dibo/bodi159com.p PERSISTENT SET h-bodi154com.

   FIND ped-venda WHERE
        ped-venda.nome-abrev = h-nome-abrev:SCREEN-VALUE AND 
        ped-venda.nr-pedcli = h-nr-pedcli:SCREEN-VALUE NO-LOCK.

   FOR EACH b-ped-item OF ped-venda NO-LOCK
            BREAK BY b-ped-item.it-codigo
                  BY b-ped-item.nr-sequencia.

       FIND ped-item-ext WHERE
            ped-item-ext.nome-abrev   = b-ped-item.nome-abrev AND
            ped-item-ext.nr-pedcli    = b-ped-item.nr-pedcli AND
            ped-item-ext.nr-sequencia = b-ped-item.nr-sequencia AND
            ped-item-ext.it-codigo    = b-ped-item.it-codigo AND
            ped-item-ext.cod-refer    = b-ped-item.cod-refer
            NO-LOCK NO-ERROR.

       IF FIRST-OF(b-ped-item.it-codigo) OR 
          SUBSTR(ped-item-ext.lote,1,2) <> c-lote THEN DO.

          ASSIGN c-lote = IF AVAIL ped-item-ext
                          THEN SUBSTR(ped-item-ext.lote,1,2)
                          ELSE "".

          FOR EACH ped-item OF ped-venda WHERE
                   ped-item.it-codigo = b-ped-item.it-codigo NO-LOCK,
              EACH ped-item-ext WHERE
                   ped-item-ext.nome-abrev   = ped-item.nome-abrev AND
                   ped-item-ext.nr-pedcli    = ped-item.nr-pedcli AND
                   ped-item-ext.nr-sequencia = ped-item.nr-sequencia AND
                   ped-item-ext.it-codigo    = ped-item.it-codigo AND
                   ped-item-ext.cod-refer    = ped-item.cod-refer AND
                   SUBSTR(ped-item-ext.lote,1,2) = c-lote NO-LOCK.

              FOR EACH tt-ped-item.
                  DELETE tt-ped-item.
              END.

              CREATE tt-ped-item.
              BUFFER-COPY ped-item TO tt-ped-item 
                     ASSIGN tt-ped-item.vl-preori = b-ped-item.vl-preori
                            tt-ped-item.vl-liq-abe = tt-ped-item.qt-pedida * tt-ped-item.vl-preori. 

              RUN setConstraintKey IN h-bodi154 (INPUT tt-ped-item.nome-abrev,
                                                 INPUT tt-ped-item.nr-pedcli,
                                                 INPUT tt-ped-item.nr-sequencia,
                                                 INPUT tt-ped-item.it-codigo,
                                                 INPUT tt-ped-item.cod-refer).
              RUN openQueryStatic in h-bodi154 (input "Key":U).        

              RUN emptyRowErrors IN h-bodi154.
              RUN setRecord IN h-bodi154(INPUT TABLE tt-ped-item).

              RUN updateRecord IN h-bodi154.

              RUN getRowErrors IN h-bodi154(OUTPUT TABLE RowErrors).
              IF CAN-FIND(FIRST RowErrors 
                      WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

                 FOR EACH rowerrors WHERE RowErrors.ErrorSubType = "ERROR":U:
                     MESSAGE rowerrors.errornumber
                             rowerrors.errordescription VIEW-AS ALERT-BOX.
                 END.
              END.
          END. 
       END.
   END.
END.
APPLY 'CHOOSE' TO h-btCompletOrder.


/* For‡a a situacao de aprova‡Æo de credito para nÆo availiada quando
** a condi‡Æo de pagamento for (a vista, antencipado e contra apresenta‡Æo) */
FIND ped-venda WHERE
     ped-venda.nome-abrev = h-nome-abrev:SCREEN-VALUE AND 
     ped-venda.nr-pedcli = h-nr-pedcli:SCREEN-VALUE NO-ERROR.

IF ped-venda.cod-cond-pag >= 1 AND 
   ped-venda.cod-cond-pag <= 3 THEN
   ASSIGN ped-venda.cod-sit-aval = 1.


