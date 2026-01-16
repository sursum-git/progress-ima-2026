DEF INPUT PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEFINE VARIABLE hBoTranspCli  AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoEmitente   AS HANDLE      NO-UNDO.
DEFINE VARIABLE itransp       AS INT         NO-UNDO.
DEFINE VARIABLE idTranspEstab AS INTEGER     NO-UNDO.
DEFINE VARIABLE codEmitente   AS INTEGER     NO-UNDO.

RUN esbo/boTranspCli.p PERSISTENT SET hBoTranspCli.
RUN esbo/boEmitente.p PERSISTENT SET hBoEmitente.


DEF VAR h-bodi159com AS HANDLE.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR i-sit-aval    LIKE ped-venda.cod-sit-aval.
DEF VAR i-cod-mess    LIKE ped-venda.cod-message-alert.      
DEF VAR da-dt-mess    LIKE ped-venda.dt-mensagem.
DEF VAR c-desc-for    LIKE ped-venda.desc-forc-cr.           
DEF VAR l-dsp-fat     LIKE ped-venda.dsp-pre-fat.
DEF VAR i-ind-cre-cli LIKE emitente.ind-cre-cli.

IF NOT VALID-HANDLE(h-bodi159com) OR
   h-bodi159com:TYPE      <> "PROCEDURE":U OR
   h-bodi159com:FILE-NAME <> "dibo/bodi159com.p":U THEN
   RUN dibo/bodi159com.p PERSISTENT SET h-bodi159com.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = p-nr-pedcli NO-ERROR.

ASSIGN i-sit-aval = ped-venda.cod-sit-aval
       i-cod-mess = ped-venda.cod-message-alert
       da-dt-mess = ped-venda.dt-mensagem
       c-desc-for = ped-venda.desc-forc-cr
       l-dsp-fat  = ped-venda.dsp-pre-fat. 


FIND emitente WHERE
     emitente.cod-emit = ped-venda.cod-emit NO-LOCK NO-ERROR.

ASSIGN i-ind-cre-cli = 0.
IF emitente.ind-cre-cli >= 3 THEN DO.  // S½ a Vista ou Suspenso
   FIND CURRENT emitente SHARE-LOCK NO-ERROR.

   ASSIGN i-ind-cre-cli = emitente.ind-cre-cli.

   ASSIGN emitente.ind-cre-cli = 1.
END.

IF NOT ped-venda.completo THEN DO.
   RUN emptyRowErrors IN h-bodi159com.
   RUN completeOrder IN h-bodi159com (INPUT ROWID(ped-venda),
                                       OUTPUT TABLE Rowerrors).
   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Completar o Pedido" SKIP
               "Erro:" rowerrors.errornumber " - "
               rowerrors.errordescription
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
END.

ASSIGN ped-venda.cod-sit-aval = i-sit-aval
       ped-venda.cod-message-alert = i-cod-mess 
       ped-venda.dt-mensagem = da-dt-mess 
       ped-venda.desc-forc-cr = c-desc-for 
       ped-venda.dsp-pre-fat = l-dsp-fat.

// Retorna a Situa»’o original do Cliente
IF i-ind-cre-cli <> 0 THEN DO.
   ASSIGN emitente.ind-cre-cli = i-ind-cre-cli.
   FIND CURRENT emitente NO-LOCK NO-ERROR.
END.

FIND emitente WHERE
     emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

FIND ped-venda-ext WHERE
     ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
     ped-venda-ext.nr-pedido = ped-venda.nr-pedido
     SHARE-LOCK NO-ERROR.
     
IF AVAIL ped-venda-ext THEN DO.
   IF ped-venda-ext.tp-pedido = "Amostra" OR
      ped-venda-ext.tp-pedido = "Amostra Exporta‡Æo" THEN DO.
      ASSIGN ped-venda.desc-forc-cr = ""
             ped-venda.desc-bloq-cr = ""
             ped-venda.dt-apr-cred  = TODAY
             ped-venda.cod-sit-aval = 3
             ped-venda.quem-aprovou = 'Automatico'.
    
      ASSIGN ped-venda.cod-message-alert = 0
             ped-venda.dt-mensagem = ?
             ped-venda.dsp-pre-fat = YES. 
             
   END.
END.

IF ped-venda-ext.tp-frete BEGINS 'Cif' THEN 
   ASSIGN ped-venda.cidade-cif = ped-venda.cidade
          ped-venda.ind-tp-frete = 1.
ELSE
   ASSIGN ped-venda.cidade-cif = ''
          ped-venda.ind-tp-frete = 2.
 
/*IF ped-venda-ext.tp-frete = "Cif Total" THEN
DO: 
      
         ASSIGN codEmitente = ped-venda.cod-emitente.
         IF ped-venda.nome-abrev-tri <> '' THEN
         DO:           
           RUN setNomeAbrev IN hboEmitente(INPUT ped-venda.nome-abrev-tri).
           RUN getCodEmitente IN hboEmitente(OUTPUT codEmitente).           
         END.
         
           RUN iniciar IN hBoTranspCli. 
           RUN setProp IN hboTranspCli('codEstab',0,ped-venda.cod-estabel).
           RUN setProp IN hboTranspCli('codCliente',0,codEmitente).
           RUN setProp IN hboTranspCli('codTpFrete',0, ped-venda-ext.tp-frete).
           RUN exec    IN hboTranspCli.
           RUN getTransportadora IN hboTranspCli(OUTPUT iTransp, OUTPUT idTranspEstab). 
           RUN finalizar IN hBoTranspCli.
           
           FIND transporte 
           WHERE transporte.cod-transp = iTransp NO-ERROR.
           IF AVAIL transporte THEN
           DO:
              ped-venda.nome-transp = transporte.nome-abrev. 
           END.
           
END. */

IF VALID-HANDLE(h-bodi159com) THEN
   DELETE PROCEDURE h-bodi159com.

