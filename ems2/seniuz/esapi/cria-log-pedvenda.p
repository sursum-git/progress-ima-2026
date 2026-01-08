DEF INPUT PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEF INPUT PARAMETER p-nome-abrev LIKE ped-venda.nome-abrev.
DEF INPUT PARAMETER p-ocorrencia AS CHAR.
DEF INPUT PARAMETER p-envia-e-mail AS LOG.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

FIND ped-venda WHERE
     ped-venda.nome-abrev = p-nome-abrev AND
     ped-venda.nr-pedcli = p-nr-pedcli NO-LOCK NO-ERROR.      

CREATE his-ped-venda-ext.
ASSIGN his-ped-venda-ext.cod-estabel = ped-venda.cod-estabel  
       his-ped-venda-ext.nr-pedcli   = p-nr-pedcli
       his-ped-venda-ext.nome-abrev  = p-nome-abrev
       his-ped-venda-ext.dt-trans    = TODAY
       his-ped-venda-ext.hr-trans    = TIME
       his-ped-venda-ext.usuario     = c-seg-usuario
       his-ped-venda-ext.ocorrencia  = p-ocorrencia.


DEF VAR c-mensagem     AS CHAR.
DEF VAR i-cont         AS INT.
DEF VAR c-remetente    LIKE usuar_mestre.cod_e_mail_local.
DEF VAR c-destinatario LIKE param-dis.destinatario.

IF p-envia-e-mail = YES THEN DO.
   FIND FIRST espec.param-dis NO-LOCK NO-ERROR.
   ASSIGN p-envia-e-mail = NO.
   FOR EACH usuar_grp_usuar WHERE
            usuar_grp_usuar.cod_usuario = c-seg-usuario NO-LOCK:
       IF INDEX(espec.param-dis.grp-remetente,usuar_grp_usuar.cod_grp_usuar) <> 0 THEN 
          ASSIGN p-envia-e-mail = YES.
   END.
END.

IF p-envia-e-mail THEN DO:
   FOR EACH usuar_grp_usuar WHERE
            usuar_grp_usuar.cod_usuario = c-seg-usuario NO-LOCK:
       IF INDEX(espec.param-dis.grp-remetente,usuar_grp_usuar.cod_grp_usuar) <> 0 THEN DO:
          FIND usuar_mestre WHERE
               usuar_mestre.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
          IF AVAIL usuar_mestre AND usuar_mestre.cod_e_mail_local <> "" THEN DO:
             ASSIGN c-remetente = usuar_mestre.cod_e_mail_local.

             DO i-cont = 1 TO NUM-ENTRIES(espec.param-dis.destinatario,";").                   
                IF usuar_mestre.cod_e_mail_local <> ENTRY(i-cont,espec.param-dis.destinatario,";") THEN DO:
                   IF c-destinatario = '' THEN
                      ASSIGN c-destinatario = ENTRY(i-cont,espec.param-dis.destinatario,";").
                   ELSE
                      ASSIGN c-destinatario = c-destinatario + ";" + ENTRY(i-cont,espec.param-dis.destinatario,";").
                END.
             END.                                                                         
          END.
       END.
    END.

    ASSIGN c-mensagem = "Pedido: " + p-nr-pedcli + "  Cliente: " + p-nome-abrev + CHR(13) +
                        p-ocorrencia +  CHR(13) +
                        "Usu rio: " + c-seg-usuario + " Data: " + STRING(TODAY,"99/99/9999") + " Hora: " + STRING(TIME,"HH:MM") + 
                        CHR(13) + CHR(13).

    IF c-mensagem <> '' THEN
       RUN esapi/esapi002.p (INPUT c-remetente, /* e-mail remetente */
                             INPUT c-destinatario, /* e-mail destinat rio */
                             INPUT "Manuten‡Æo em Pedidos de Venda: ", /* Assunto */
                             INPUT c-mensagem, /* Mensagem */
                             INPUT "", /*arquivo anexo*/
                             INPUT YES). /* Mostra Erros */
END.

