/* Programa: especificos/esupc/upcd-di154.p
** Objetivo: Criar Log de Altera‡Æo dos Itens do Pedido                 
** Autor...: Toninho-SeniuZ/Gilvando
** Data....: 09/Jul/2007
*/

DEFINE PARAMETER BUFFER p-table FOR ped-item.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR c-mensagem     AS CHAR.
DEF VAR l-env-e-mail   AS LOG.
DEF VAR i-pos          AS INT.
DEF VAR i-tam          AS INT.
DEF VAR c-remetente    LIKE usuar_mestre.cod_e_mail_local.
DEF VAR c-destinatario LIKE param-dis.destinatario.

FIND ped-item-res WHERE 
     ped-item-res.nome-abrev   = p-table.nome-abrev AND 
     ped-item-res.nr-pedcli    = p-table.nr-pedcli AND 
     ped-item-res.it-codigo    = p-table.it-codigo AND 
     ped-item-res.cod-refer    = p-table.cod-refer AND
     ped-item-res.nr-sequencia = p-table.nr-sequencia 
     NO-ERROR.

IF AVAIL ped-item-res THEN DO.
   MESSAGE 'Item: ' ped-item-res.it-codigo '    Referencia: ' ped-item-res.cod-refer 
           '    Sequencia: ' ped-item-res.nr-sequencia SKIP 
           'est  Reservado, Imposs¡vel Eliminar...' VIEW-AS ALERT-BOX ERROR.

   RETURN 'NOK'.
END.

FIND ped-item-ext WHERE
     ped-item-ext.nome-abrev   = p-table.nome-abrev AND
     ped-item-ext.nr-pedcli    = p-table.nr-pedcli  AND
     ped-item-ext.nr-sequencia = p-table.nr-sequencia AND
     ped-item-ext.it-codigo    = p-table.it-codigo AND
     ped-item-ext.cod-refer    = p-table.cod-refer NO-ERROR.

IF AVAILABLE ped-item-ext THEN 
   DELETE ped-item-ext.

/*
RUN pi-cria-log (INPUT "Sequencia " + TRIM(STRING(p-table.nr-sequencia,">>>9")) + ": Exclu¡da").

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

   ASSIGN c-mensagem = "Pedido: " + p-table.nr-pedcli + " Seq.: " + TRIM(STRING(p-table.nr-sequencia,">>>9")) + " Cliente: " + 
                       p-table.nome-abrev + CHR(13) +
                       "Usu rio: " + c-seg-usuario + " Data: " + STRING(TODAY,"99/99/9999") + " Hora: " + STRING(TIME,"HH:MM") + 
                       CHR(13) + CHR(13).
   ASSIGN c-mensagem = c-mensagem + "Sequencia excluida, quantidade " + TRIM(STRING(p-table.qt-pedida,">>>,>>9.99")) + CHR(13).
   
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
