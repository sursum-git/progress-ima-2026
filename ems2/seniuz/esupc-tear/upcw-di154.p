/* Programa: especificos/esupc/upcw-di154.p
** Objetivo: Criar Log de Altera‡Æo dos Itens do Pedido                 
** Autor...: Toninho-SeniuZ
** Data....: 19/Abr/2005
*/

DEFINE PARAMETER BUFFER p-table FOR ped-item.
DEFINE PARAMETER BUFFER p-table-old FOR ped-item.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR c-mensagem     AS CHAR.
DEF VAR l-env-e-mail   AS LOG.
DEF VAR i-pos          AS INT.
DEF VAR i-tam          AS INT.
DEF VAR c-remetente    LIKE usuar_mestre.cod_e_mail_local.
DEF VAR c-destinatario LIKE param-dis.destinatario.

/*
IF NEW p-table AND p-table.qt-pedida <> 0 THEN DO:
   RUN pi-cria-log (INPUT "Sequencia " + TRIM(STRING(p-table.nr-sequencia,">>>9")) + ": Inclu¡da").
   /*ASSIGN l-env-e-mail = YES.*/
END.
ELSE DO:
   IF p-table.qt-pedida <> p-table-old.qt-pedida AND p-table-old.qt-pedida <> 0 THEN DO:
      RUN pi-cria-log (INPUT "Sequencia " + TRIM(STRING(p-table.nr-sequencia,">>>9")) + ": Alterada a Quantidade Pedida, De: " +
                       TRIM(STRING(p-table-old.qt-pedida,">>>,>>9.99")) + " Para: " + TRIM(STRING(p-table.qt-pedida,">>>,>>9.99"))).
      ASSIGN l-env-e-mail = YES.
   END.

   IF p-table.vl-preuni <> p-table-old.vl-preuni AND p-table-old.vl-preuni <> 0 THEN DO:
      RUN pi-cria-log (INPUT "Sequencia " + TRIM(STRING(p-table.nr-sequencia,">>>9")) + ": Alterado o Pre‡o, De: " +
                       TRIM(STRING(p-table-old.vl-preuni,">>>,>>9.99")) + " Para: " + TRIM(STRING(p-table.vl-preuni,">>>,>>9.99"))).
      ASSIGN l-env-e-mail = YES.
   END.
   
   IF p-table.dt-canseq <> p-table-old.dt-canseq THEN DO:
      RUN pi-cria-log (INPUT "Sequencia " + TRIM(STRING(p-table.nr-sequencia,">>>9")) + 
                             " Item: " + TRIM(p-table.it-codigo) + " Refer: " + TRIM(p-table.cod-refer) + 
                             ": Cancelada").
      ASSIGN l-env-e-mail = YES.
   END.
END.

/*--- Envia e-mail aos usu rios do Faturamento ---*/
IF l-env-e-mail THEN DO:
   FIND FIRST espec.param-dis NO-LOCK NO-ERROR.
   ASSIGN l-env-e-mail = NO.
   FOR EACH usuar_grp_usuar WHERE usuar_grp_usuar.cod_usuario = c-seg-usuario NO-LOCK:
       IF INDEX(espec.param-dis.grp-remetente,usuar_grp_usuar.cod_grp_usuar) <> 0 THEN DO:
          ASSIGN l-env-e-mail = YES.
          LEAVE.
       END.
   END.
END.

IF l-env-e-mail THEN DO:
   FIND usuar_mestre WHERE usuar_mestre.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
   IF AVAIL usuar_mestre AND usuar_mestre.cod_e_mail_local <> "" THEN DO:
      ASSIGN c-remetente = usuar_mestre.cod_e_mail_local
             i-pos = INDEX(param-dis.destinatario,usuar_mestre.cod_e_mail_local)
             i-tam = LENGTH(usuar_mestre.cod_e_mail_local).
      IF SUBSTR(param-dis.destinatario,i-pos + i-tam,1) = ";" THEN
         ASSIGN i-tam = i-tam + 1.

      IF i-pos = 1 THEN
         ASSIGN c-destinatario = SUBSTR(param-dis.destinatario, i-tam + 1, LENGTH(param-dis.destinatario) - i-tam).
      ELSE
      IF i-pos <> 0 THEN
         ASSIGN c-destinatario = SUBSTR(PARAM-dis.destinatario,1,i-pos - 1) + SUBSTR(param-dis.destinatario, i-pos + 
                                                i-tam,LENGTH(param-dis.destinatario) - (i-pos + i-tam - 1)).
      ELSE
         ASSIGN c-destinatario = param-dis.destinatario.
   END.
   ELSE
      ASSIGN c-remetente    = "teartextil@teartextil.com.br"
             c-destinatario = param-dis.destinatario.

   FIND ITEM WHERE ITEM.it-codigo = p-table.it-codigo NO-LOCK NO-ERROR.
   ASSIGN c-mensagem = "Pedido: " + p-table.nr-pedcli + " Seq.: " + TRIM(STRING(p-table.nr-sequencia,">>>9")) + " Cliente: " + 
                       p-table.nome-abrev + CHR(13) +
                       "Item: " + TRIM(p-table.it-codigo) + " " + TRIM(ITEM.desc-item) + CHR(13) +
                       "Refer: " + p-table.cod-refer + CHR(13) +
                       "Usu rio: " + c-seg-usuario + " Data: " + STRING(TODAY,"99/99/9999") + " Hora: " + STRING(TIME,"HH:MM") + 
                       CHR(13) + CHR(13).
   IF NEW p-table THEN
      ASSIGN c-mensagem = c-mensagem + "Sequencia incluida, quantidade " + TRIM(STRING(p-table-old.qt-pedida,">>>,>>9.99")) + CHR(13).
   
   IF p-table.qt-pedida <> p-table-old.qt-pedida THEN
      ASSIGN c-mensagem = c-mensagem + "Alterada Quantidade Pedida, De: " +
                          TRIM(STRING(p-table-old.qt-pedida,">>>,>>9.99")) + " Para: " + TRIM(STRING(p-table.qt-pedida,">>>,>>9.99")) + CHR(13).
   
   IF p-table.dt-canseq <> p-table-old.dt-canseq THEN
      ASSIGN c-mensagem = c-mensagem + "Sequencia cancelada, quantidade " + TRIM(STRING(p-table-old.qt-pedida,">>>,>>9.99")).
   
   IF c-mensagem <> '' THEN
      RUN esapi/esapi002.p (INPUT c-remetente, /* e-mail remetente */
                            INPUT c-destinatario, /* e-mail destinat rio */
                            INPUT "Altera‡Æo no Pedido de Venda: " + p-table.nr-pedcli, /* Assunto */
                            INPUT c-mensagem, /* Mensagem */
                            INPUT "", /*arquivo anexo*/
                            INPUT YES). /* Mostra Erros */
END.

PROCEDURE pi-cria-log.
   DEF INPUT PARAMETER p-ocorrencia AS CHAR.
   CREATE his-ped-venda-ext.
   ASSIGN his-ped-venda-ext.nr-pedcli  = p-table.nr-pedcli
          his-ped-venda-ext.nome-abrev = p-table.nome-abrev
          his-ped-venda-ext.dt-trans   = TODAY
          his-ped-venda-ext.hr-trans   = TIME
          his-ped-venda-ext.usuario    = c-seg-usuario
          his-ped-venda-ext.ocorrencia = p-ocorrencia.
END PROCEDURE.

*/
