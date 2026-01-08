/****************************************************************************
** Programa: TWAD264 - trigger de write para a tabela tit_acr
** Data    : Nov/2014
** Objetivo: Criar Titulo Contas a Receber na Base BACKUP
** Empresa : IMA 
** Fluxo   : Cadastrado na Oficial =>  Backup
*****************************************************************************/
DEFINE PARAMETER BUFFER b-tit_acr_new FOR tit_acr.
DEFINE PARAMETER BUFFER b-tit_acr_old FOR tit_acr. 

DEFINE NEW GLOBAL SHARED VARIABLE gc-narrativa AS CHARACTER.

DEFINE VAR c-base AS CHAR.

RUN esapi/busca-base.p (OUTPUT c-base).


IF c-base = 'BASE-BKP' AND
   NEW(b-tit_acr_new) AND b-tit_acr_new.cod_espec_docto = "AD" THEN DO.
   IF gc-narrativa <> "" THEN DO.
      CREATE histor_movto_tit_acr.
      ASSIGN histor_movto_tit_acr.cod_estab = b-tit_acr_new.cod_estab
             histor_movto_tit_acr.num_id_tit_acr = b-tit_acr_new.num_id_tit_acr
             histor_movto_tit_acr.num_id_movto_tit_acr = b-tit_acr_new.num_id_movto_tit_acr
             histor_movto_tit_acr.num_seq_histor_movto_acr = 1.

      ASSIGN histor_movto_tit_acr.des_text_histor = gc-narrativa.

      ASSIGN b-tit_acr_new.des_observacao = gc-narrativa.

      ASSIGN gc-narrativa = "".
   END.
END.


IF c-base = 'BASE-PRO' AND
   NEW(b-tit_acr_new) AND b-tit_acr_new.ind_orig_tit_acr BEGINS 'FATEMS2' THEN DO.
   FIND FIRST repres_tit_acr WHERE
              repres_tit_acr.cod_estab = b-tit_acr_new.cod_estab AND
              repres_tit_acr.num_id_tit_acr = b-tit_acr_new.num_id_tit_acr AND
              repres_tit_acr.cdn_repres = 99999 NO-LOCK NO-ERROR.

    IF AVAIL repres_tit_acr THEN DO.
       SESSION:SET-WAIT-STATE("general":U).
           RUN esapi/integra_tit_acr.p (INPUT b-tit_acr_new.cod_estab,
                                        INPUT b-tit_acr_new.cod_espec_docto,
                                        INPUT b-tit_acr_new.cod_ser_docto,
                                        INPUT b-tit_acr_new.cod_tit_acr,
                                        INPUT b-tit_acr_new.cod_parcela).
       SESSION:SET-WAIT-STATE("":U).

       IF RETURN-VALUE = 'NOK' THEN DO.
          MESSAGE "ERRO na Integraá∆o do Titulo " b-tit_acr_new.num_id_tit_acr SKIP
                  "Appserver n∆o Conectado, avise CPD (IMTRG/TW-TIT-ACR.P"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN ERROR.
       END.
    END.
END.

RETURN 'OK'.

