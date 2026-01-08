DEF INPUT PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEF OUTPUT PARAMETER c-erro-pedvenda AS CHAR.

DEF VAR h-bodi159com AS HANDLE.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR c-tpped-cred-aut AS CHAR INIT "· Vista,Exporta‡Æo,Amostra,Amostra Exporta‡Æo,Bonifica‡Æo,Doa‡Æo".

DEF VAR i-sit-aval LIKE ped-venda.cod-sit-aval.
DEF VAR i-cod-mess LIKE ped-venda.cod-message-alert.      
DEF VAR da-dt-mess LIKE ped-venda.dt-mensagem.
DEF VAR c-desc-for LIKE ped-venda.desc-forc-cr.           
DEF VAR l-dsp-fat  LIKE ped-venda.dsp-pre-fat.

IF NOT VALID-HANDLE(h-bodi159com) OR
   h-bodi159com:TYPE      <> "PROCEDURE":U OR
   h-bodi159com:FILE-NAME <> "dibo/bodi159com.p":U THEN
   RUN dibo/bodi159com.p PERSISTENT SET h-bodi159com.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = p-nr-pedcli NO-ERROR.

FIND ped-venda-ext WHERE
     ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

IF AVAIL ped-venda-ext THEN DO. /* Avalia‡Æo de Cr‚dito Autom tico para os tipos de pedidos abaixo */
   IF LOOKUP(ped-venda-ext.tp-pedido,c-tpped-cred-aut) > 0 THEN DO.
      IF (ped-venda-ext.tp-pedido = "Amostra" OR
          ped-venda-ext.tp-pedido = "Amostra Exporta‡Æo") THEN DO.
         FIND natur-oper WHERE
              natur-oper.nat-operacao = ped-venda.nat-oper NO-LOCK NO-ERROR.
         IF natur-oper.emite-duplic = NO THEN
            ASSIGN ped-venda.cod-sit-aval = 3.
      END.
      ELSE
         ASSIGN ped-venda.cod-sit-aval = 3.
   END.
END.

ASSIGN i-sit-aval = ped-venda.cod-sit-aval
       i-cod-mess = ped-venda.cod-message-alert
       da-dt-mess = ped-venda.dt-mensagem
       c-desc-for = ped-venda.desc-forc-cr
       l-dsp-fat  = ped-venda.dsp-pre-fat. 

IF NOT ped-venda.completo THEN DO.
   RUN emptyRowErrors IN h-bodi159com.
   RUN completeOrder IN h-bodi159com (INPUT ROWID(ped-venda),
                                       OUTPUT TABLE Rowerrors).
   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
/*       MESSAGE "Erro ao Completar o Pedido: " ped-venda.nr-pedcli SKIP
               "Erro:" rowerrors.errornumber " - "
               rowerrors.errordescription
               VIEW-AS ALERT-BOX INFO BUTTONS OK. */
       ASSIGN c-erro-pedvenda = "Erro ao Completar o Pedido: " + ped-venda.nr-pedcl + " " + rowerrors.errordescription.
   END.
END.

ASSIGN ped-venda.cod-sit-aval = i-sit-aval
       ped-venda.cod-message-alert = i-cod-mess 
       ped-venda.dt-mensagem = da-dt-mess 
       ped-venda.desc-forc-cr = c-desc-for 
       ped-venda.dsp-pre-fat = l-dsp-fat.

FIND emitente WHERE
     emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

/* Avalia‡Æo de Cr‚dito Autom tico */
IF emitente.ind-cre-cli = 2 OR
   (AVAIL ped-venda-ext AND LOOKUP(ped-venda-ext.tp-pedido,c-tpped-cred-aut) > 0) THEN
   ASSIGN ped-venda.cod-message-alert = 0
          ped-venda.dt-mensagem = ?
          ped-venda.dsp-pre-fat = YES.

/* IF AVAIL ped-venda-ext THEN DO.                                                         */
/*    IF NOT LOOKUP(ped-venda-ext.tp-pedido,c-tpped-cred-aut) > 0 AND  /* fora da lista */ */
/*      (ped-venda.cod-sit-aval = 1 OR ped-venda.cod-sit-aval = 4) THEN                    */
/*       ASSIGN ped-venda.cod-sit-ped = 4.                                                 */
/* END.                                                                                    */

IF VALID-HANDLE(h-bodi159com) THEN
   DELETE PROCEDURE h-bodi159com.

