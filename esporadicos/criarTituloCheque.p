{utp/ut-glob.i}
DEFINE VARIABLE iClienteMatriz  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRepres         AS INTEGER     NO-UNDO.
DEFINE VARIABLE cNomeAbrev      AS CHARACTER   NO-UNDO FORMAT 'x(12)'.
DEFINE VARIABLE cParcela        AS CHARACTER   NO-UNDO.

   
PROCEDURE buscarCodMatrizCliente:
    DEFINE INPUT  PARAMETER iCliente AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER iMatriz  AS INTEGER     NO-UNDO INIT 0.
    DEFINE VARIABLE nomeAbrev        AS CHAR        NO-UNDO.
    FIND FIRST emitente WHERE
               emitente.cod-emitente = iCliente NO-LOCK NO-ERROR.
    IF AVAIL emitente AND  emitente.nome-abrev = emitente.nome-matriz THEN
       ASSIGN iMatriz = emitente.cod-emitente.
    ELSE DO:
      ASSIGN nomeAbrev = emitente.nome-matriz.
      FIND FIRST emitente WHERE
          emitente.nome-abrev = nomeAbrev NO-LOCK NO-ERROR.
      IF AVAIL emitente THEN
         ASSIGN iMatriz = emitente.cod-emitente.
    END.                                        
END PROCEDURE.
PROCEDURE buscarRepresCliente:
   DEFINE INPUT  PARAMETER iCliente AS INTEGER     NO-UNDO.
   DEFINE OUTPUT PARAMETER iRepres  AS INTEGER     NO-UNDO INIT 0.
   FIND FIRST emitente WHERE
               emitente.cod-emitente = iCliente NO-LOCK NO-ERROR.
   IF AVAIL emitente THEN
      ASSIGN iRepres = emitente.cod-rep.
END PROCEDURE.



PROCEDURE buscarNomeCliente:
   DEFINE INPUT  PARAMETER iCliente     AS INTEGER     NO-UNDO.
   DEFINE OUTPUT PARAMETER nomeAbrev    AS CHAR     NO-UNDO FORMAT 'x(12)' INIT ''.
   FIND FIRST emitente WHERE
               emitente.cod-emitente = iCliente NO-LOCK NO-ERROR.
   IF AVAIL emitente THEN
      ASSIGN nomeAbrev = emitente.nome-abrev.
END PROCEDURE.

PROCEDURE buscarNovaParcela:
    DEFINE INPUT  PARAMETER cEmpresa    AS CHARACTER    NO-UNDO.
    DEFINE INPUT  PARAMETER cEstab      AS CHARACTER    NO-UNDO.
    DEFINE INPUT  PARAMETER codTitAcr   AS CHARACTER    NO-UNDO.
    DEFINE INPUT  PARAMETER iCliente    AS INTEGER      NO-UNDO.
    DEFINE OUTPUT PARAMETER cParcela    AS CHARACTER   NO-UNDO.
    FIND LAST tit_acr
        WHERE tit_acr.cod_empresa   = cEmpresa 
        AND   tit_acr.cod_estab     = cEstab
        AND   tit_acr.cod_tit_acr   = codTitAcr
        AND   tit_acr.cdn_cliente   = iCliente
        AND   tit_acr.cod_espec_docto = 'CH' NO-LOCK NO-ERROR.
    IF AVAIL tit_acr THEN DO:
       ASSIGN cParcela = STRING( int(tit_acr.cod_parcela) + 1).
    END.
    ELSE
      ASSIGN cParcela = '1'.
END.

FIND FIRST cheq_acr  EXCLUSIVE-LOCK WHERE
           cheq_acr.num_cheq = 900320.
RUN buscarCodMatrizCliente(cheq_acr.cdn_cliente, OUTPUT iClienteMatriz).
RUN buscarRepresCliente(cheq_acr.cdn_cliente,    OUTPUT iRepres ).
RUN buscarNomeCliente(cheq_acr.cdn_cliente,      OUTPUT cNomeAbrev ).
RUN buscarNovaParcela(cheq_acr.cod_empresa, cheq_acr.cod_estab, cheq_acr.num_cheque, cheq_acr.cdn_cliente, OUTPUT  cParcela).
CREATE tit_acr. 
ASSIGN 
  tit_acr.cod_parcela                  = cParcela
  tit_acr.cdn_clien_matriz             = iClienteMatriz
  tit_acr.cdn_repres                   = iRepres
  tit_acr.cdn_cliente                  = cheq_acr.cdn_cliente 
  tit_acr.cod_empresa                  = cheq_acr.cod_empresa
  tit_acr.cod_espec_docto              = cheq_acr.cod_espec_docto_cheq_acr
  tit_acr.cod_estab                    = cheq_acr.cod_estab
  tit_acr.cod_indic_econ               = cheq_acr.cod_indic_econ
  tit_acr.cod_portador                 = cheq_acr.cod_portador
  tit_acr.cod_refer                    = STRING(NOW)
  tit_acr.cod_ser_docto                = '3'
  tit_acr.cod_tit_acr                  = string(cheq_acr.num_cheque)
  tit_acr.dat_alter_portad             = cheq_acr.dat_transacao
  tit_acr.dat_emis_docto               = cheq_acr.dat_emis_cheq
  tit_acr.dat_fluxo_tit_acr            = cheq_acr.dat_prev_apres_cheq_acr
  tit_acr.dat_indcao_perda_dedut       = 12.31.9999
  tit_acr.dat_liquidac_tit_acr         = 12.31.9999
  tit_acr.dat_prev_liquidac            = cheq_acr.dat_prev_apres_cheq_acr
  tit_acr.dat_transacao                = cheq_acr.dat_transacao
  tit_acr.dat_ult_aprop_despes_financ  = 12.31.9999  
  tit_acr.dat_ult_apurac_variac_val    = cheq_acr.dat_transacao
  tit_acr.dat_ult_liquidac_tit_acr     = 12.31.9999
  tit_acr.dat_vencto_origin_tit_acr    = cheq_acr.dat_prev_apres_cheq_acr 
  tit_acr.dat_vencto_tit_acr           = cheq_acr.dat_prev_apres_cheq_acr
  tit_acr.ind_ender_cobr               = 'cliente' 
  tit_acr.ind_orig_tit_acr             = 'ACR'
  tit_acr.ind_sit_bcia_tit_acr         = 'liberado'
  tit_acr.ind_sit_tit_acr              = 'Normal'
  tit_acr.ind_tip_calc_juros           = 'Simples'
  tit_acr.ind_tip_cobr_acr             = 'Normal'
  tit_acr.ind_tip_espec_docto          = 'Cheques Recebidos'
  tit_acr.log_aviso_db_emitid          = NO
  tit_acr.log_db_autom                 = NO
  tit_acr.log_dupl_emitid              = NO
  tit_acr.log_emis_boleto              = NO
  tit_acr.log_integr_cfl_atlzdo        = NO
  tit_acr.log_livre_1                  = NO
  tit_acr.log_livre_2                  = NO
  tit_acr.log_npromis_emitid           = NO
  tit_acr.log_recibo_emitid            = NO
  tit_acr.log_retenc_impto_impl        = NO
  tit_acr.log_sdo_tit_acr              = YES
  tit_acr.log_tip_cr_perda_dedut_tit   = NO
  tit_acr.log_tit_acr_cobr_bcia        = NO
  tit_acr.log_tit_acr_destndo          = NO
  tit_acr.log_tit_acr_estordo          = NO
  tit_acr.log_tit_agrup_especial       = NO
  tit_acr.nom_abrev                    = cNomeAbrev 
  tit_acr.num_id_movto_cta_corren      = cheq_acr.num_id_movto_cta_corren
  tit_acr.num_id_tit_acr               = NEXT-VALUE(seq_tit_acr)  
  tit_acr.num_pessoa                   = cheq_acr.num_pessoa
  tit_acr.val_origin_tit_acr           = cheq_acr.val_cheque
  tit_acr.val_sdo_tit_acr              = cheq_acr.val_cheque.


CREATE movto_tit_acr.

ASSIGN movto_tit_acr.cdn_cliente                = tit_acr.cdn_cliente
       movto_tit_acr.cod_cart_bcia              = tit_acr.cod_cart_bcia
       movto_tit_acr.cod_empresa                = tit_acr.cod_empresa
       movto_tit_acr.cod_espec_docto            = tit_acr.cod_espec_docto
       movto_tit_acr.cod_estab                  = tit_acr.cod_estab
       movto_tit_acr.cod_portador               = tit_acr.cod_portador
       movto_tit_acr.cod_refer                  = tit_acr.cod_refer
       movto_tit_acr.cod_usuario                = c-seg-usuario
       movto_tit_acr.dat_apurac_variac_val_ant  = cheq_acr.dat_transacao
       movto_tit_acr.dat_contrat_cambio_export  = cheq_acr.dat_transacao
       movto_tit_acr.dat_fluxo_cx_movto         = cheq_acr.dat_prev_apres_cheq_acr
       movto_tit_acr.dat_gerac_movto            = cheq_acr.dat_transacao
       movto_tit_acr.dat_refer_contrat_cambio   = cheq_acr.dat_transacao
       movto_tit_acr.dat_transacao              = cheq_acr.dat_transacao
       movto_tit_acr.dat_vencto_ant_tit_acr     = cheq_acr.dat_prev_apres_cheq_acr
       movto_tit_acr.dat_vencto_tit_acr         = cheq_acr.dat_prev_apres_cheq_acr
       movto_tit_acr.hra_gerac_movto            = STRING(TIME)
       movto_tit_acr.ind_trans_acr              = "implanta‡Æo"
       movto_tit_acr.ind_trans_acr_abrev        = "impl"
       movto_tit_acr.num_id_movto_tit_acr       = NEXT-VALUE(seq_movto_tit_acr)
       movto_tit_acr.num_id_tit_acr             = tit_acr.num_id_tit_acr
       movto_tit_acr.val_movto_tit_acr          = cheq_acr.val_cheque.

ASSIGN tit_acr.num_id_movto_tit_acr_ult = movto_tit_acr.num_id_movto_tit_acr
       cheq_acr.num_id_tit_acr          = tit_acr.num_id_tit_acr
       cheq_acr.val_tot_vincul_cheq_acr = cheq_acr.val_cheque.                                                                            


CREATE val_tit_acr.

ASSIGN val_tit_acr.cod_empresa              = tit_acr.cod_empresa
       val_tit_acr.cod_estab                = tit_acr.cod_estab
       val_tit_acr.cod_finalid_econ         = 'corrente'
       val_tit_acr.cod_tip_fluxo_financ     = '1.01'
       val_tit_acr.cod_unid_negoc           = '001'
       val_tit_acr.dat_cotac_indic_econ     = tit_acr.dat_emis_
       val_tit_acr.num_id_tit_acr           = tit_acr.num_id_tit_acr
       val_tit_acr.num_id_val_tit_acr       = NEXT-VALUE(seq_val_tit_acr)
       val_tit_acr.val_cotac_indic_econ     = 1 
       val_tit_acr.val_origin_tit_acr       = tit_acr.val_sdo_tit_acr
       val_tit_acr.val_perc_rat             = 100
       val_tit_acr.val_sdo_tit_acr          = tit_acr.val_sdo_tit_acr  .


