DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
     FIELD r-rowid   AS ROWID.

DEF TEMP-TABLE tt-ped-repre LIKE ped-repre
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-cond-ped LIKE cond-ped
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi159 AS HANDLE.
DEF VAR h-bodi159com AS HANDLE.
DEF VAR i-sit-ped LIKE ped-venda.cod-sit-ped.

DEF VAR i-sit-aval LIKE ped-venda.cod-sit-aval.
DEF VAR i-cod-mess LIKE ped-venda.cod-message-alert.      
DEF VAR da-dt-mess LIKE ped-venda.dt-mensagem.
DEF VAR c-desc-for LIKE ped-venda.desc-forc-cr.           
DEF VAR l-dsp-fat  LIKE ped-venda.dsp-pre-fat.

DEF VAR i-ind-cre-cli LIKE emitente.ind-cre-cli.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF INPUT PARAMETER TABLE FOR tt-ped-venda.

FIND FIRST tt-ped-venda NO-ERROR.

IF NOT VALID-HANDLE(h-bodi159) OR
   h-bodi159:TYPE      <> "PROCEDURE":U OR
   h-bodi159:FILE-NAME <> "dibo/bodi159.p":U THEN
   RUN dibo/bodi159.p PERSISTENT SET h-bodi159.

IF NOT VALID-HANDLE(h-bodi159com) OR
   h-bodi159com:TYPE      <> "PROCEDURE":U OR
   h-bodi159com:FILE-NAME <> "dibo/bodi159com.p":U THEN
   RUN dibo/bodi159com.p PERSISTENT SET h-bodi159com.

FIND FIRST para-ped NO-LOCK NO-ERROR.

/* Cria o Pedido */
FIND ped-venda WHERE
     ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND
     ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli NO-LOCK NO-ERROR.

IF AVAIL ped-venda THEN DO.
   IF ped-venda.nat-operacao <> tt-ped-venda.nat-operacao THEN DO.
      FOR EACH ped-item OF ped-venda SHARE-LOCK.
          ASSIGN ped-item.nat-operacao = tt-ped-venda.nat-operacao.
      END.
   END.

   FIND emitente WHERE
        emitente.cod-emit = ped-venda.cod-emit NO-LOCK NO-ERROR.

   ASSIGN i-ind-cre-cli = 0.
   IF emitente.ind-cre-cli >= 3 THEN DO.  // S¢ a Vista ou Suspenso
      FIND CURRENT emitente SHARE-LOCK NO-ERROR.
      ASSIGN i-ind-cre-cli = emitente.ind-cre-cli.
      ASSIGN emitente.ind-cre-cli = 1.
   END.

   ASSIGN i-sit-aval = ped-venda.cod-sit-aval
          i-cod-mess = ped-venda.cod-message-alert
          da-dt-mess = ped-venda.dt-mensagem
          c-desc-for = ped-venda.desc-forc-cr
          l-dsp-fat  = ped-venda.dsp-pre-fat. 

   ASSIGN i-sit-ped = 0.
   IF ped-venda.cod-sit-ped = 4 THEN
      ASSIGN i-sit-ped = ped-venda.cod-sit-ped
             ped-venda.cod-sit-ped = 1.

   RUN setconstraintRowid IN h-bodi159 (INPUT ROWID(ped-venda)).
   RUN openQueryStatic in h-bodi159 (input "Rowid":U).      
   RUN emptyRowErrors IN h-bodi159.
   RUN setRecord IN h-bodi159(INPUT TABLE tt-ped-venda).
   RUN UpdateRecord IN h-bodi159.
   RUN getRowErrors IN h-bodi159(OUTPUT TABLE RowErrors).
   IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
      FOR EACH rowerrors WHERE
               RowErrors.ErrorSubType = "ERROR":U:
          MESSAGE "Erro ao Gravar no Pedido" SKIP
                  rowerrors.errordescription 
                  VIEW-AS ALERT-BOX.
     END.
     RETURN 'ADM-ERROR'.
   END.

   IF ped-venda.tp-pedido = "PE" THEN DO.
      RUN emptyRowErrors IN h-bodi159com.
      RUN completeOrder IN h-bodi159com (INPUT ROWID(ped-venda),
                                         OUTPUT TABLE Rowerrors).
   END.

   FIND ped-venda WHERE
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli  NO-ERROR.
   
   IF i-sit-ped <> 0 THEN
      ASSIGN ped-venda.cod-sit-ped = i-sit-ped.

   ASSIGN ped-venda.cod-sit-aval = i-sit-aval
          ped-venda.cod-message-alert = i-cod-mess 
          ped-venda.dt-mensagem = da-dt-mess 
          ped-venda.desc-forc-cr = c-desc-for 
          ped-venda.dsp-pre-fat = l-dsp-fat.

   // Retorna a Situa‡Æo original do Cliente
   IF i-ind-cre-cli <> 0 THEN DO.
      ASSIGN emitente.ind-cre-cli = i-ind-cre-cli.
      FIND CURRENT emitente NO-LOCK NO-ERROR.
   END.
END.

IF VALID-HANDLE(h-bodi159) THEN
   DELETE PROCEDURE h-bodi159.

IF VALID-HANDLE(h-bodi159com) THEN
   DELETE PROCEDURE h-bodi159com.
