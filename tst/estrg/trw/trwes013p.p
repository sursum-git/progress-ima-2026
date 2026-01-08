DEFINE PARAMETER BUFFER p-ped-item-ext FOR ped-item-ext.
DEFINE PARAMETER BUFFER p-ped-item-ext-old FOR ped-item-ext.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR c-mensagem     AS CHAR.
DEF VAR c-corte-1      LIKE corte-comerc.descricao.
DEF VAR c-corte-2      LIKE corte-comerc.descricao.
DEF VAR l-env-e-mail   AS LOG.
DEF VAR i-pos          AS INT.
DEF VAR i-tam          AS INT.
DEF VAR c-remetente    LIKE usuar_mestre.cod_e_mail_local.
DEF VAR c-destinatario LIKE param-dis.destinatario.

IF NOT NEW p-ped-item-ext THEN DO:
   IF p-ped-item-ext.corte-comerc <> p-ped-item-ext-old.corte-comerc THEN DO:
      FIND corte-comerc WHERE corte-comerc.codigo = p-ped-item-ext-old.corte-comerc NO-LOCK NO-ERROR.
      ASSIGN c-corte-1 = IF AVAIL corte-comerc THEN corte-comerc.descricao ELSE "".

      FIND corte-comerc WHERE corte-comerc.codigo = p-ped-item-ext.corte-comerc NO-LOCK NO-ERROR.
      ASSIGN c-corte-2 = IF AVAIL corte-comerc THEN corte-comerc.descricao ELSE "".
      
      RUN esapi/cria-log-pedvenda.p (INPUT p-ped-item-ext.nr-pedcli,
                                     INPUT p-ped-item-ext.nome-abrev,
                                     INPUT ("Sequencia " + TRIM(STRING(p-ped-item-ext.nr-sequencia,">>>9")) + ": Alterado Corte Comercial, De: " + c-corte-1 + " Para: " + c-corte-2),
                                     INPUT NO).


   /*   RUN pi-cria-log (INPUT "Sequencia " + TRIM(STRING(p-ped-item-ext.nr-sequencia,">>>9")) + ": Alterado Corte Comercial, De: " +
                       c-corte-1 + " Para: " + c-corte-2).   */
      ASSIGN l-env-e-mail = YES.
   END.
   IF p-ped-item-ext.lote <> p-ped-item-ext-old.lote THEN DO:
      RUN esapi/cria-log-pedvenda.p (INPUT p-ped-item-ext.nr-pedcli,
                                     INPUT p-ped-item-ext.nome-abrev,
                                     INPUT ("Sequencia " + TRIM(STRING(p-ped-item-ext.nr-sequencia,">>>9")) + ": Alterado Lote, De: " + p-ped-item-ext-old.lote + " Para: " + p-ped-item-ext.lote),
                                     INPUT NO).
   
   /*   RUN pi-cria-log (INPUT "Sequencia " + TRIM(STRING(p-ped-item-ext.nr-sequencia,">>>9")) + ": Alterado Lote, De: " +
                      p-ped-item-ext-old.lote + " Para: " + p-ped-item-ext.lote).  */
   
      ASSIGN l-env-e-mail = YES.
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
            ASSIGN c-destinatario = SUBSTR(PARAM-dis.destinatario,1,i-pos - 1) + SUBSTR(param-dis.destinatario, i-pos + 
                                                   i-tam,LENGTH(param-dis.destinatario) - (i-pos + i-tam - 1)).
      END.
      ELSE
         ASSIGN c-remetente    = "teartextil@teartextil.com.br"
                c-destinatario = param-dis.destinatario.

      ASSIGN c-mensagem = "Pedido: " + p-ped-item-ext.nr-pedcli + " Seq.: " + TRIM(STRING(p-ped-item-ext.nr-sequencia,">>>9")) + " Cliente: " + 
                          p-ped-item-ext.nome-abrev + CHR(13) +
                          "Usu rio: " + c-seg-usuario + " Data: " + STRING(TODAY,"99/99/9999") + " Hora: " + STRING(TIME,"HH:MM") + 
                          CHR(13) + CHR(13).
      IF p-ped-item-ext.corte-comerc <> p-ped-item-ext-old.corte-comerc THEN
         ASSIGN c-mensagem = c-mensagem + "Alterado Corte Comercial, De: " +
                             c-corte-1 + " Para: " + c-corte-2 + CHR(13).
      IF p-ped-item-ext.lote <> p-ped-item-ext-old.lote THEN
         ASSIGN c-mensagem = c-mensagem + "Alterado Lote, De: " +
                             p-ped-item-ext-old.lote + " Para: " + p-ped-item-ext.lote.
      IF c-mensagem <> '' THEN
         RUN esapi/esapi002.p (INPUT c-remetente, /* e-mail remetente */
                               INPUT c-destinatario, /* e-mail destinat rio */
                               INPUT "Altera‡Æo no Pedido de Venda: " + p-ped-item-ext.nr-pedcli, /* Assunto */
                               INPUT c-mensagem, /* Mensagem */
                               INPUT "", /*arquivo anexo*/
                               INPUT YES). /* Mostra Erros */
   END.
END.
/*
PROCEDURE pi-cria-log.
   DEF INPUT PARAMETER p-ocorrencia AS CHAR.
   CREATE his-ped-venda-ext.
   ASSIGN his-ped-venda-ext.nr-pedcli  = p-ped-item-ext.nr-pedcli
          his-ped-venda-ext.nome-abrev = p-ped-item-ext.nome-abrev
          his-ped-venda-ext.dt-trans   = TODAY
          his-ped-venda-ext.hr-trans   = TIME
          his-ped-venda-ext.usuario    = c-seg-usuario
          his-ped-venda-ext.ocorrencia = p-ocorrencia.
END PROCEDURE.
*/
