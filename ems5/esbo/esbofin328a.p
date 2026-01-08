/******************
 tratamento referente a tabela de usuario financeiro do APB
 com relaá∆o a permiss∆o de aprovaá∆o

********************/

DEFINE VARIABLE cArquivo AS CHARACTER   NO-UNDO INIT 'p:\LOG_Aprovacao.txt' .
{utp/ut-glob.i}
PROCEDURE verificarPermissaoAPB:
   /********************************************************************************
    verifica se o usuario corrente Ç aprovador 
   ou n∆o e se est† no grupo de usu†rios 777, utilizado para colocar os usu†rios 
   que utilizam a rotina de vinculaá∆o de antecipaá∆o
    *********************************************************************************/
   DEFINE OUTPUT PARAMETER lAprovador   AS LOGICAL     NO-UNDO INIT NO.
   DEFINE VARIABLE lGrupo AS LOGICAL     NO-UNDO INIT NO.
   FIND FIRST usuar_financ_estab_apb NO-LOCK
       WHERE usuar_financ_estab_apb.cod_usuario = c-seg-usuario
       AND   usuar_financ_estab_apb.cod_empresa = i-ep-codigo-usuario + '00' NO-ERROR.
   IF AVAIL usuar_financ_estab_apb THEN DO:
      RUN verificarGrupoUsuario(OUTPUT lGrupo).
      IF lGrupo THEN DO:
         /*MESSAGE 'buscou a situaá∆o do usuario:' c-seg-usuario SKIP
                 'aprovador:' usuar_financ_estab_apb.log_habilit_liber_tit_ap
             VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

         ASSIGN lAprovador = usuar_financ_estab_apb.log_habilit_liber_tit_ap.
      END.
   END.
END PROCEDURE.


PROCEDURE verificarPermissaoAPB02:
   /********************************************************************************
    verifica se o usuario passado  por parametro Ç aprovador 
    ou n∆o.
   *********************************************************************************/
   DEFINE INPUT  PARAMETER pUsuario         AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER lAprovador       AS LOGICAL     NO-UNDO INIT NO.
   DEFINE VARIABLE lGrupo AS LOGICAL NO-UNDO INIT NO.
   FIND FIRST usuar_financ_estab_apb NO-LOCK
       WHERE usuar_financ_estab_apb.cod_usuario = pUsuario
       AND   usuar_financ_estab_apb.cod_empresa = i-ep-codigo-usuario + '00' NO-ERROR.
   IF AVAIL usuar_financ_estab_apb THEN DO:
      ASSIGN lAprovador = usuar_financ_estab_apb.log_habilit_liber_tit_ap.
   END.
END PROCEDURE.



PROCEDURE modificarPermissaoAPB:
   DEFINE INPUT  PARAMETER lPermissao   AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE lGrupo               AS LOGICAL     NO-UNDO INIT NO.
/*    MESSAGE c-seg-usuario SKIP       */
/*            i-ep-codigo-usuario SKIP */
/* VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
   /*MESSAGE 'usuario corrente:' c-seg-usuario SKIP
           'permissao a ser aplicada:' lPermissao
       VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
   FOR EACH usuar_financ_estab_apb EXCLUSIVE-LOCK
       WHERE usuar_financ_estab_apb.cod_usuario = c-seg-usuario
       AND   usuar_financ_estab_apb.cod_empresa = i-ep-codigo-usuario + '00'.
       RUN verificarGrupoUsuario(OUTPUT lGrupo).
       IF lGrupo THEN DO:
          ASSIGN usuar_financ_estab_apb.log_habilit_liber_tit_ap = lPermissao.
          IF lPermissao THEN
            ASSIGN
            usuar_financ_estab_apb.val_lim_liber_usuar_movto = 9999999
            usuar_financ_estab_apb.val_lim_liber_usuar_mes   = 9999999.
          ELSE
            ASSIGN 
            usuar_financ_estab_apb.val_lim_liber_usuar_movto  = 0
            usuar_financ_estab_apb.val_lim_liber_usuar_mes    = 0.

       END.
   END.
       


END PROCEDURE.

PROCEDURE verificarGrupoUsuario:
    DEFINE OUTPUT  PARAMETER l AS LOGICAL     NO-UNDO INIT NO.
    FIND FIRST usuar_grp_usuar NO-LOCK
        WHERE usuar_grp_usuar.cod_usuario = c-seg-usuario 
        AND   usuar_grp_usuar.cod_grp_usuar = '777' NO-ERROR.
    IF AVAIL usuar_grp_usuar THEN
       ASSIGN l = YES.
    /*MESSAGE 'achou usuario no grupo?' l
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
END PROCEDURE.

PROCEDURE criarLog:
    
    DEFINE INPUT  PARAMETER ptexto AS CHARACTER FORMAT 'x(500)'   NO-UNDO.
    DEFINE VARIABLE cHorario AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
   /* MESSAGE 'vou criar log'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
     */
    ASSIGN cHorario = STRING(NOW,"99/99/9999 hh:mm:ss")
           ptexto   = ptexto + '-' + cHorario.
    OUTPUT TO VALUE(cArquivo) APPEND.
        PUT ptexto SKIP.
    OUTPUT CLOSE.
   

END.

PROCEDURE zerarlog:

   OUTPUT TO cArquivo.
        PUT 'Novo processo de aprovaá∆o iniciado:'  NOW SKIP.
    OUTPUT CLOSE.


END PROCEDURE.




/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_empresa                      char        m
   20 cod_usuario                      char        im
   30 cod_estab                        char        im
   40 log_habilit_impl_tit_ap          logi        m
   50 log_habilit_prepar_tit_ap        logi        m
   60 log_habilit_liber_tit_ap         logi        m
   70 log_habilit_pagto_tit_ap         logi        m
   80 log_habilit_impr_docto           logi        m
   90 log_habilit_confir_tit_ap        logi        m
  100 log_habilit_ctbz_apb             logi        m
  110 log_habilit_transf_tit_ap        logi
  120 log_habilit_correc_tit_ap        logi        m
  130 log_habilit_estorn_ap            logi        m
  140 log_habilit_alter_tit_ap         logi        m
  150 log_habilit_enctro_cta           logi        m
  160 log_habilit_cancel_tit_ap        logi        m
  170 log_habilit_mutuo_pagto          logi        m
  180 val_lim_liber_usuar_movto        deci-2      m
  190 val_lim_liber_usuar_mes          deci-2      m
  200 val_lim_pagto_usuar_movto        deci-2      m
  210 val_lim_pagto_usuar_mes          deci-2      m
  220 cod_livre_1                      char
  230 log_livre_1                      logi
  240 num_livre_1                      inte
  250 val_livre_1                      deci-4
  260 dat_livre_1                      date
  270 log_habilita_unid_organ          logi        m
  280 log_habilita_cta_ctbl            logi        m
  290 log_habilita_tip_fluxo           logi        m
  300 log_habilita_ccusto              logi        m
  310 log_habilita_estab               logi        m
  320 cod_livre_2                      char
  330 dat_livre_2                      date
  340 log_livre_2                      logi
  350 num_livre_2                      inte
  360 val_livre_2                      deci-4
  370 cdd_version                      deci-0
  380 log_habilit_alter_juros          logi
  390 log_habilit_alter_vencto         logi
  400 log_habilit_alter_sdo            logi
*/
