/*****************************************************************************
**       Programa: ESP0005RP.p
**       Data....: 23/03/2011
**       Autor...: DATASUL S.A.
**       Objetivo: Integraá∆o de Implantaá∆o de T°tulos
**       Vers∆o..: 2.06.001 - Bruno
*******************************************************************************/
DEF TEMP-TABLE tt-siaf-acr NO-UNDO LIKE ESP_LOG_INTEGRACAO.

DEF INPUT PARAM TABLE FOR tt-siaf-acr.

DEF SHARED stream s_1.

DEFINE TEMP-TABLE tt-erros-integr NO-UNDO
    FIELDS nr-linha AS INTEGER    /*** Numero da Linha ***/
    FIELDS msg-erro AS CHARACTER  /*** Mensagem indicano o error da linha (existir erro) ***/ 
    FIELDS chave    AS CHARACTER  /*** Chave unica do registro, se chave composta separar valores por | ***/
    FIELDS cod-erro AS INTEGER.   /*** C¢digo do erro retornado pelo Datasul ***/

/****************** INCLUDE COM VARIÊVEIS GLOBAIS *********************/
def new global shared var c-seg-usuario as char format "x(12)" no-undo.
def new global shared var v_cod_usuar_corren as character format "x(12)" label "Usu†rio Corrente" column-label "Usu†rio Corrente" no-undo.
def new global shared var v_cod_empres_usuar  as character format "x(3)"  label "Empresa" column-label "Empresa" no-undo. 


/***************** Definiªao de Vari†veis de Processamento do RelatΩrio *********************/
def var h-acomp              as handle no-undo.
def var v-cont-registro      as int    no-undo.
DEF VAR c-cod-refer          AS CHAR   NO-UNDO.
DEF VAR l-erro               AS LOG NO-UNDO.

DEF VAR V_estab       LIKE MV_TITULO_ACR.cod_estab.
DEF VAR V_espec_docto LIKE MV_TITULO_ACR.cod_espec_docto.
DEF VAR V_ser_docto   LIKE MV_TITULO_ACR.cod_ser_docto.
DEF VAR V_tit_acr     LIKE MV_TITULO_ACR.cod_tit_acr.
DEF VAR V_parcela     LIKE MV_TITULO_ACR.cod_parcela.
DEF VAR v_nom_pessoa  LIKE ems5.cliente.nom_pessoa.
DEF VAR V_plano       LIKE MV_APROPR_TITULO_ACR.cod_plano_ccusto.
DEF VAR V_ccusto      LIKE MV_APROPR_TITULO_ACR.cod_ccusto.

DEF VAR c-msg-erro AS CHAR.

def var v_log_refer_uni as LOGICAL format "Sim/N∆o" initial NO no-undo.

/**** Definiá∆o de temp-tables ****************************************/
{prgfin/acr/acr900zi.i}

/**** Definiá∆o de buffer ****************************************/
def buffer b_tit_acr for tit_acr.


run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input "Acompanhamento ACR").
run pi-desabilita-cancela in h-acomp.

/* gr9020a.p */
/*Criando temp-table de titulos*/
RUN pi-principal.

IF VALID-HANDLE(h-acomp) THEN /*gr9030g*/
    RUN pi-finalizar IN h-acomp NO-ERROR.


PROCEDURE pi-principal.

    FIND FIRST tt-siaf-acr 
       WHERE tt-siaf-acr.IND_SITUACAO_INTEGRACAO = 0
             NO-ERROR.
    REPEAT:
        IF AVAILABLE tt-siaf-acr THEN DO:
            FIND FIRST esp_log_integracao EXCLUSIVE-LOCK
                 WHERE esp_log_integracao.cod_log_integracao = tt-siaf-acr.cod_log_integracao NO-ERROR.
    
            run pi-acompanhar in h-acomp(input "Integraá∆o: " + tt-siaf-acr.cod_log_integracao).
    
            ASSIGN l-erro = NO.

            RUN pi-valida-gera.

            IF NOT l-erro THEN
                RUN pi-gerar.

    
            ASSIGN tt-siaf-acr.IND_SITUACAO_INTEGRACAO = 1.

            FIND NEXT tt-siaf-acr 
               WHERE tt-siaf-acr.IND_SITUACAO_INTEGRACAO = 0 NO-ERROR.
        END.
        ELSE LEAVE.
    END.
END PROCEDURE.

PROCEDURE pi-valida-gera.

    ASSIGN l-erro       = NO
           v_nom_pessoa = "".


    ASSIGN V_estab       = IF ENTRY(1,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = ? OR ENTRY(1,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = "" THEN ? ELSE ENTRY(1,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|")
           V_espec_docto = IF ENTRY(2,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = ? OR ENTRY(2,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = "" THEN ? ELSE ENTRY(2,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|")
           V_ser_docto   = IF ENTRY(3,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = ? OR ENTRY(3,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = "" THEN ? ELSE ENTRY(3,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|")
           V_tit_acr     = IF ENTRY(4,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = ? OR ENTRY(4,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = "" THEN ? ELSE ENTRY(4,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|")
           V_parcela     = IF ENTRY(5,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = ? OR ENTRY(5,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = "" THEN ? ELSE ENTRY(5,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|").



    FIND FIRST MV_TITULO_ACR NO-LOCK
         WHERE MV_TITULO_ACR.cod_estab       = V_estab
           AND MV_TITULO_ACR.cod_espec_docto = V_espec_docto
           AND MV_TITULO_ACR.cod_ser_docto   = V_ser_docto
           AND MV_TITULO_ACR.cod_tit_acr     = V_tit_acr
           AND MV_TITULO_ACR.cod_parcela     = V_parcela NO-ERROR.
    IF NOT AVAIL MV_TITULO_ACR THEN DO:
        ASSIGN V_estab       = IF ENTRY(1,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = ? OR ENTRY(1,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = "" THEN "" ELSE ENTRY(1,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|")
               V_espec_docto = IF ENTRY(2,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = ? OR ENTRY(2,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = "" THEN "" ELSE ENTRY(2,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|")
               V_ser_docto   = IF ENTRY(3,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = ? OR ENTRY(3,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = "" THEN "" ELSE ENTRY(3,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|")
               V_tit_acr     = IF ENTRY(4,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = ? OR ENTRY(4,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = "" THEN "" ELSE ENTRY(4,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|")
               V_parcela     = IF ENTRY(5,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = ? OR ENTRY(5,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") = "" THEN "" ELSE ENTRY(5,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|").
    
        FIND FIRST MV_TITULO_ACR NO-LOCK
             WHERE MV_TITULO_ACR.cod_estab       = V_estab
               AND MV_TITULO_ACR.cod_espec_docto = V_espec_docto
               AND MV_TITULO_ACR.cod_ser_docto   = V_ser_docto
               AND MV_TITULO_ACR.cod_tit_acr     = V_tit_acr
               AND MV_TITULO_ACR.cod_parcela     = V_parcela NO-ERROR.
    END.

    IF AVAIL MV_TITULO_ACR THEN DO:

        FIND FIRST ems5.cliente NO-LOCK
             WHERE cliente.cod_empresa = MV_TITULO_ACR.COD_EMPRESA
               AND cliente.cdn_cliente = MV_TITULO_ACR.CDN_CLIENTE NO-ERROR.
        IF NOT AVAIL cliente THEN DO:
            FIND FIRST ems5.fornecedor NO-LOCK
                 WHERE fornecedor.cod_empresa    = MV_TITULO_ACR.COD_EMPRESA
                   AND fornecedor.cdn_fornecedor = MV_TITULO_ACR.CDN_CLIENTE NO-ERROR.
            IF NOT AVAIL fornecedor THEN DO:
                ASSIGN c-msg-erro = CHR(10) + "Chave  : " + tt-siaf-acr.COD_CHAVE_INTEGRACAO +
                                    CHR(10) + "Cliente: " + STRING(MV_TITULO_ACR.CDN_CLIENTE).

                RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                        INPUT "EMS5",
                                          INPUT 30,         /*Cliente/fornecedor n∆o cadastrado*/                                              /*Cliente/Fornecedor n∆o existe no EMS 5*/
                                          INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                          INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                          INPUT "",                                                          /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                          INPUT c-msg-erro). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/ 


                PUT stream s_1 UNFORMATTED 
                    esp_log_integracao.cod_log_integracao + CHR(9) + "Integraá∆o com erro. N∆o encontrado Cliente/Fornecedor no EMS!" SKIP.

                ASSIGN l-erro = YES.
            END.
            ELSE ASSIGN v_nom_pessoa = fornecedor.nom_pessoa.
        END.
        ELSE ASSIGN v_nom_pessoa = cliente.nom_pessoa.

        IF NOT l-erro THEN DO:

            IF MV_TITULO_ACR.ind_orig_tit_acr    = "SIAF"
           AND MV_TITULO_ACR.cdn_repres          = 999
           AND MV_TITULO_ACR.ind_tip_espec_docto = "Normal"
           AND MV_TITULO_ACR.cod_empresa         <> ""
           AND MV_TITULO_ACR.cod_estab           <> ""
           AND MV_TITULO_ACR.cod_espec_docto     <> ""
           AND MV_TITULO_ACR.cod_tit_acr         <> ""
           AND MV_TITULO_ACR.cod_parcela         <> ""
           AND MV_TITULO_ACR.val_tit_acr         <> 0
           AND MV_TITULO_ACR.val_liq_tit_acr     <> 0
           AND MV_TITULO_ACR.cdn_cliente         <> 0
           AND MV_TITULO_ACR.des_text_histor     <> ""
    
           AND MV_TITULO_ACR.cod_empresa         <> ? 
           AND MV_TITULO_ACR.cod_estab           <> ? 
           AND MV_TITULO_ACR.dat_transacao       <> ? 
           AND MV_TITULO_ACR.cdn_cliente         <> ? 
           AND MV_TITULO_ACR.cod_espec_docto     <> ?
           AND MV_TITULO_ACR.cod_tit_acr         <> ? 
           AND MV_TITULO_ACR.cod_parcela         <> ? 
           AND MV_TITULO_ACR.des_text_histor     <> ?
           AND MV_TITULO_ACR.dat_vencto_tit_acr  <> ? 
           AND MV_TITULO_ACR.dat_prev_liquidac   <> ? 
           AND MV_TITULO_ACR.dat_emis_docto      <> ? 
           AND MV_TITULO_ACR.val_tit_acr         <> ? 
           AND MV_TITULO_ACR.val_liq_tit_acr     <> ? THEN DO:
                IF NOT CAN-FIND (FIRST MV_APROPR_TITULO_ACR NO-LOCK
                                 WHERE MV_APROPR_TITULO_ACR.cod_estab       = MV_TITULO_ACR.cod_estab
                                   AND MV_APROPR_TITULO_ACR.cod_espec_docto = MV_TITULO_ACR.cod_espec_docto
                                   AND MV_APROPR_TITULO_ACR.cod_ser_docto   = MV_TITULO_ACR.cod_ser_docto
                                   AND MV_APROPR_TITULO_ACR.cod_tit_acr     = MV_TITULO_ACR.cod_tit_acr
                                   AND MV_APROPR_TITULO_ACR.cod_parcela     = MV_TITULO_ACR.cod_parcela) THEN DO:
    
                    ASSIGN c-msg-erro = CHR(10) + "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + " na VIEW MV_APROPR_TITULO_ACR" + 
                                        CHR(10) + "Cliente: " + v_nom_pessoa + " - " + STRING(MV_TITULO_ACR.CDN_CLIENTE).
                    
                    
                    RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                            INPUT "EMS5",
                                              INPUT 31,                                                           /*Registro que o cadastro do cliente n∆o contÇm o mesmo CPF do que o recebido para integraá∆o*/
                                              INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                              INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                              INPUT "",                                                          /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                              INPUT c-msg-erro). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/ 
    
                    PUT stream s_1 UNFORMATTED 
                        esp_log_integracao.cod_log_integracao + CHR(9) + "Integraá∆o com erro. View MV_APROPR_TITULO_ACR inexistente!" SKIP.
    
                    ASSIGN l-erro = YES.
                END.
                ELSE DO:
                    FOR EACH MV_APROPR_TITULO_ACR NO-LOCK
                         WHERE MV_APROPR_TITULO_ACR.cod_estab       = MV_TITULO_ACR.cod_estab
                           AND MV_APROPR_TITULO_ACR.cod_espec_docto = MV_TITULO_ACR.cod_espec_docto
                           AND MV_APROPR_TITULO_ACR.cod_ser_docto   = MV_TITULO_ACR.cod_ser_docto
                           AND MV_APROPR_TITULO_ACR.cod_tit_acr     = MV_TITULO_ACR.cod_tit_acr
                           AND MV_APROPR_TITULO_ACR.cod_parcela     = MV_TITULO_ACR.cod_parcela:
    
                        IF MV_APROPR_TITULO_ACR.cod_estab            <> ""
                       AND MV_APROPR_TITULO_ACR.cod_espec_docto      <> "" 
                       AND MV_APROPR_TITULO_ACR.cod_tit_acr          <> ""
                       AND MV_APROPR_TITULO_ACR.cod_parcela          <> ""
                       AND MV_APROPR_TITULO_ACR.cod_plano_cta_ctbl   <> ""
                       AND MV_APROPR_TITULO_ACR.cod_cta_ctbl         <> ""
                       AND MV_APROPR_TITULO_ACR.cod_unid_negoc       <> ""
/*                        AND MV_APROPR_TITULO_ACR.cod_plano_ccusto     <> "" */
/*                        AND MV_APROPR_TITULO_ACR.cod_ccusto           <> "" */
                       AND MV_APROPR_TITULO_ACR.cod_tip_fluxo_financ <> ""
                       AND MV_APROPR_TITULO_ACR.val_aprop_ctbl       <> 0
        
                       AND MV_APROPR_TITULO_ACR.cod_estab            <> ? 
                       AND MV_APROPR_TITULO_ACR.cod_espec_docto      <> ?
                       AND MV_APROPR_TITULO_ACR.cod_tit_acr          <> ? 
                       AND MV_APROPR_TITULO_ACR.cod_parcela          <> ? 
                       AND MV_APROPR_TITULO_ACR.cod_plano_cta_ctbl   <> ? 
                       AND MV_APROPR_TITULO_ACR.cod_cta_ctbl         <> ? 
                       AND MV_APROPR_TITULO_ACR.cod_unid_negoc       <> ? 
/*                        AND MV_APROPR_TITULO_ACR.cod_plano_ccusto     <> ? */
/*                        AND MV_APROPR_TITULO_ACR.cod_ccusto           <> ? */
                       AND MV_APROPR_TITULO_ACR.cod_tip_fluxo_financ <> ? 
                       AND MV_APROPR_TITULO_ACR.val_aprop_ctbl       <> ? THEN DO:
                            
                            ASSIGN V_ser_docto = IF MV_TITULO_ACR.cod_ser_docto = ? THEN "" ELSE MV_TITULO_ACR.cod_ser_docto. 
        
                            FIND FIRST ems5.tit_acr NO-LOCK
                                 WHERE tit_acr.cod_estab       = MV_TITULO_ACR.cod_estab
                                   AND tit_acr.cod_espec_docto = MV_TITULO_ACR.cod_espec_docto
                                   AND tit_acr.cod_ser_docto   = V_ser_docto /*IF MV_TITULO_ACR.cod_ser_docto = ? THEN "" ELSE MV_TITULO_ACR.cod_ser_docto*/
                                   AND tit_acr.cod_tit_acr     = MV_TITULO_ACR.cod_tit_acr
                                   AND tit_acr.cod_parcela     = MV_TITULO_ACR.cod_parcela  NO-ERROR.
                            IF AVAIL tit_acr THEN DO:
                                ASSIGN c-msg-erro = CHR(10) + "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + 
                                                    CHR(10) + "Cliente: " + v_nom_pessoa + " - " + STRING(MV_TITULO_ACR.CDN_CLIENTE).
    
                                RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                                        INPUT "EMS5",
                                                          INPUT 32,                                                           /*T°tulo j† implantado no EMS*/                                      /*Registro que o t°tulo j† est† implantado no contas a receber*/
                                                          INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                                          INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                                          INPUT "",                                                          /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                                          INPUT c-msg-erro). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/ 
                
                                PUT stream s_1 UNFORMATTED 
                                    esp_log_integracao.cod_log_integracao + CHR(9) + "Integraá∆o com erro. T°tulo j† est† implantado no EMS!" SKIP.
                            END.
                            ELSE DO: 
                                FIND FIRST estabelecimento NO-LOCK
                                     WHERE estabelecimento.cod_estab = MV_TITULO_ACR.cod_estab NO-ERROR.
                                IF NOT AVAIL estabelecimento THEN DO:
                                    ASSIGN c-msg-erro = CHR(10) + "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + 
                                                        CHR(10) + "Cliente: " + v_nom_pessoa + " - " + STRING(MV_TITULO_ACR.CDN_CLIENTE).
                                    RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                                            INPUT "EMS5",
                                                              INPUT 33,        /*Estabelecimento n∆o encontrado*/                                               /*Registro que o estabelecimento n∆o existe no EMS 5*/
                                                              INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                                              INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                                              INPUT "N∆o localizado estabelecimento com chave informada.",                                                          /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                                              INPUT c-msg-erro). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/ 
        
                                    PUT stream s_1 UNFORMATTED 
                                         esp_log_integracao.cod_log_integracao + CHR(9) + "Integraá∆o com erro. T°tulo j† est† implantado no EMS!" SKIP.
        
                                    ASSIGN l-erro = YES.
                                END.
        
        
                                FIND FIRST ems5.portador NO-LOCK
                                     WHERE ems5.portador.cod_portador = UPPER(MV_TITULO_ACR.cod_portador) NO-ERROR.
                                IF NOT AVAIL portador THEN DO:
                                    ASSIGN c-msg-erro = CHR(10) + "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + " - Portador informado: " + MV_TITULO_ACR.cod_portador + 
                                                        CHR(10) + "Cliente: " + v_nom_pessoa + " - " + STRING(MV_TITULO_ACR.CDN_CLIENTE).
                                    RUN espapi/espapi002.p (BUFFER esp_log_integracao,      
                                                            INPUT "EMS5",
                                                              INPUT 34,                       /*Portador n∆o cadastrado*/                            /*Portador n∆o existe no EMS 5*/
                                                              INPUT 0,                                                       /*Erro padr∆o coloca-se no 3ß campo*/
                                                              INPUT "",                                                      /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                                              INPUT "N∆o localizado Portador com a chave informada.",                                                      /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                                              INPUT c-msg-erro). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/ 
        
                                    PUT stream s_1 UNFORMATTED 
                                        esp_log_integracao.cod_log_integracao + CHR(9) + "Integraá∆o com erro. N∆o encontrado portador no EMS!" SKIP.
        
                                    ASSIGN l-erro = YES.
                                END.
                            END.
                        END.
                        ELSE DO:
                            ASSIGN c-msg-erro = "VIEW: MV_APROPR_TITULO_ACR - Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + CHR(10) + "Campos com erro: ".
                            
                            IF MV_APROPR_TITULO_ACR.cod_estab = "" OR MV_APROPR_TITULO_ACR.cod_estab                        = ? THEN
                                ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Estab".                                         
                            IF MV_APROPR_TITULO_ACR.cod_espec_docto = "" OR MV_APROPR_TITULO_ACR.cod_espec_docto            = ? THEN
                                ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "EspÇcie".                                       
                            IF MV_APROPR_TITULO_ACR.cod_tit_acr = "" OR MV_APROPR_TITULO_ACR.cod_tit_acr                    = ? THEN
                                ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "T°tulo".                                        
                            IF MV_APROPR_TITULO_ACR.cod_parcela = "" OR MV_APROPR_TITULO_ACR.cod_parcela                    = ? THEN
                                ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Parcela".                                       
                            IF MV_APROPR_TITULO_ACR.cod_plano_cta_ctbl = "" OR MV_APROPR_TITULO_ACR.cod_plano_cta_ctbl      = ? THEN
                                ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Plano de Contas".                                 
                            IF MV_APROPR_TITULO_ACR.cod_cta_ctbl = "" OR MV_APROPR_TITULO_ACR.cod_cta_ctbl                  = ? THEN
                                ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Conta Cont†bil".                                
                            IF MV_APROPR_TITULO_ACR.cod_unid_negoc = "" OR MV_APROPR_TITULO_ACR.cod_unid_negoc              = ? THEN
                                ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Unid. Neg¢cio".                                 
                            /*IF MV_APROPR_TITULO_ACR.cod_plano_ccusto = "" OR MV_APROPR_TITULO_ACR.cod_plano_ccusto          = ? THEN
                                ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Plano de Centro de Custos".                     
                            IF MV_APROPR_TITULO_ACR.cod_ccusto = "" OR MV_APROPR_TITULO_ACR.cod_ccusto                      = ? THEN
                                ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Centro de custo".
                            */    
                            IF MV_APROPR_TITULO_ACR.cod_tip_fluxo_financ  = "" OR MV_APROPR_TITULO_ACR.cod_tip_fluxo_financ = ? THEN
                                ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Fluxo Financ.".
                            IF MV_APROPR_TITULO_ACR.val_aprop_ctbl = 0 OR MV_APROPR_TITULO_ACR.val_aprop_ctbl               = ? THEN
                                ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Valor".
                                                        
                            ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Cliente: " + v_nom_pessoa + " - " + STRING(MV_TITULO_ACR.CDN_CLIENTE).
    
                            RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                                    INPUT "EMS5",
                                                      INPUT 43,                                                       /*Campos obrigat¢rios da VIEW MV_APROPR_TITULO_ACR n∆o est∆o preenchidos*/
                                                      INPUT 0,                                                       /*Erro padr∆o coloca-se no 3ß campo*/
                                                      INPUT "",                                                      /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                                      INPUT "Dados n∆o preencnidos",                                                      /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                                      INPUT c-msg-erro                                               /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/ 
                                                    ).
            
                            ASSIGN l-erro = YES.
        
                            PUT stream s_1 UNFORMATTED 
                                esp_log_integracao.cod_log_integracao + CHR(9) + "Integraá∆o com erro. Campos obrigat¢rios da VIEW MV_APROPR_TITULO_ACR n∆o preenchidos!" SKIP
                                c-msg-erro.
                        END.
                    END.
                END.
            END.
            ELSE DO:
                ASSIGN c-msg-erro = "VIEW: MV_TITULO_ACR - Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + CHR(10) + "Campos com erro: ".
    
                IF MV_TITULO_ACR.ind_orig_tit_acr    <> "SIAF" THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Origem Integraá∆o".         
                IF MV_TITULO_ACR.cod_empresa                            = ""     OR MV_TITULO_ACR.cod_empresa         = ? THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Empresa".         
                IF MV_TITULO_ACR.cod_estab                              = ""     OR MV_TITULO_ACR.cod_estab           = ? THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Estab".           
                IF MV_TITULO_ACR.cod_espec_docto                        = ""     OR MV_TITULO_ACR.cod_espec_docto     = ? THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Especie".           
                IF MV_TITULO_ACR.cod_tit_acr                            = ""     OR MV_TITULO_ACR.cod_tit_acr         = ? THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "T°tulo".          
                IF MV_TITULO_ACR.cod_parcela                            = ""     OR MV_TITULO_ACR.cod_parcela         = ? THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Parcela".         
                IF MV_TITULO_ACR.cdn_cliente                            = 0      OR MV_TITULO_ACR.cdn_cliente         = ? THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Cliente".         
                IF MV_TITULO_ACR.des_text_histor                        = ""     OR MV_TITULO_ACR.des_text_histor     = ? THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Texto Hist¢rico".
                IF MV_TITULO_ACR.cdn_repres          <> 999 THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Representante".   
                IF MV_TITULO_ACR.dat_transacao       = ? THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Dt. Transaá∆o".   
                IF MV_TITULO_ACR.dat_vencto_tit_acr  = ? THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Dt. Vencimento".  
                IF MV_TITULO_ACR.dat_prev_liquidac   = ? THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Dt. Prev. Liq.".  
                IF MV_TITULO_ACR.dat_emis_docto      = ? THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Dt. Emiss∆o".     
                IF MV_TITULO_ACR.ind_orig_tit_acr                       = ""     OR MV_TITULO_ACR.ind_orig_tit_acr    = ? THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Origem".          
                IF MV_TITULO_ACR.cod_indic_econ                         = ""     OR MV_TITULO_ACR.cod_indic_econ      = ? THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Moeda".           
                IF MV_TITULO_ACR.ind_tip_espec_docto <> "Normal" THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Tipo EspÇcie".    
                IF MV_TITULO_ACR.val_tit_acr         = ? OR MV_TITULO_ACR.val_tit_acr         = 0 THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Valor T°tulo".    
                IF MV_TITULO_ACR.val_liq_tit_acr     = ? OR MV_TITULO_ACR.val_liq_tit_acr     = 0 THEN
                    ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Valor L°q. T°tulo".
    
                ASSIGN c-msg-erro = c-msg-erro + CHR(10) + "Cliente: " + v_nom_pessoa + " - " + STRING(MV_TITULO_ACR.CDN_CLIENTE).
                    
                RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                        INPUT "EMS5",
                                          INPUT 35,                                                       /*Campos obrigat¢rios da VIEW MV_TITULO_ACR n∆o est∆o preenchidos*/
                                          INPUT 0,                                                       /*Erro padr∆o coloca-se no 3ß campo*/
                                          INPUT "",                                                      /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                          INPUT "",                                                      /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                          INPUT c-msg-erro                                               /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/ 
                                        ).
        
                 ASSIGN l-erro = YES.
    
                 PUT stream s_1 UNFORMATTED 
                            esp_log_integracao.cod_log_integracao + CHR(9) + "Integraá∆o com erro. Campos obrigat¢rios da VIEW MV_TITULO_ACR n∆o preenchidos!" SKIP
                            c-msg-erro.
            END.
    
            IF MV_TITULO_ACR.COD_BANCO <> ? AND 
               (MV_TITULO_ACR.COD_AGENC_BCIA = ? OR
                MV_TITULO_ACR.COD_CTA_CORREN = ?) THEN DO:
    
                RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                        INPUT "EMS5",
                                         INPUT 36,      /*Dados Banc†rios Inv†lidos*/
                                         INPUT 0,       /*Erro padr∆o coloca-se no 3ß campo*/
                                         INPUT "" ,     /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                         INPUT "Informaá∆o c¢digo do banco preenchida, porÇm agància e/ou conta = ?" ,     /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                         INPUT "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + " na VIEW MV_TITULO_ACR" + 
                                               CHR(10) + "Cliente: " + v_nom_pessoa + " - " + STRING(MV_TITULO_ACR.CDN_CLIENTE) ) .
        
               ASSIGN l-erro = YES.
    
               PUT stream s_1 UNFORMATTED 
                           esp_log_integracao.cod_log_integracao + CHR(9) + "Integraá∆o com erro. Informaá∆o c¢digo do banco preenchida, porÇm agància e/ou conta = ?"  SKIP.
    
            END.
    
            IF MV_TITULO_ACR.COD_AGENC_BCIA <> ? AND 
               (MV_TITULO_ACR.COD_BANCO  = ? OR
                MV_TITULO_ACR.COD_CTA_CORREN = ?) THEN DO:
    
                RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                        INPUT "EMS5",
                                         INPUT 36,       /*Dados Banc†rios Inv†lidos*/
                                         INPUT 0,       /*Erro padr∆o coloca-se no 3ß campo*/
                                         INPUT "" ,     /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                         INPUT "Informaá∆o c¢digo da agància preenchida, porÇm banco e/ou conta = ?" ,     /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                         INPUT "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + " na VIEW MV_TITULO_ACR" + 
                                               CHR(10) + "Cliente: " + v_nom_pessoa + " - " + STRING(MV_TITULO_ACR.CDN_CLIENTE)).
                ASSIGN l-erro = YES.
    
                PUT stream s_1 UNFORMATTED 
                           esp_log_integracao.cod_log_integracao + CHR(9) + "Integraá∆o com erro. Informaá∆o c¢digo da agància preenchida, porÇm banco e/ou conta = ?"  SKIP.
            END.
    
            IF MV_TITULO_ACR.COD_CTA_CORREN <> ? AND 
               (MV_TITULO_ACR.COD_BANCO  = ? OR
                MV_TITULO_ACR.COD_AGENC_BCIA = ?) THEN DO:
    
                RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                        INPUT "EMS5",
                                         INPUT 36,      /*Dados Banc†rios Inv†lidos*/
                                         INPUT 0,       /*Erro padr∆o coloca-se no 3ß campo*/
                                         INPUT "" ,     /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                         INPUT "Informaá∆o c¢digo da conta corrente preenchida, porÇm banco e/ou agància = ?" ,     /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                         INPUT "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + " na VIEW MV_TITULO_ACR" + 
                                               CHR(10) + "Cliente: " + v_nom_pessoa + " - " + STRING(MV_TITULO_ACR.CDN_CLIENTE)).
               ASSIGN l-erro = YES.
    
               PUT stream s_1 UNFORMATTED 
                          esp_log_integracao.cod_log_integracao + CHR(9) + "Integraá∆o com erro. Informaá∆o c¢digo da conta corrente preenchida, porÇm banco e/ou agància = ?"  SKIP.
            END.
    
    
            /* Validaá∆o de m†scara para agencia e conta corrente
               --------------------------------------------------*/
            IF MV_TITULO_ACR.COD_BANCO <> "" AND
               MV_TITULO_ACR.COD_BANCO <> ? THEN DO:
                FIND ems5.banco WHERE
                     banco.cod_banco = MV_TITULO_ACR.COD_BANCO NO-LOCK NO-ERROR.
                IF NOT AVAILABLE banco THEN DO:
                    RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                            INPUT "EMS5",
                                             INPUT 37,      /*Banco n∆o cadastrado*/
                                             INPUT 0,      /*Erro padr∆o coloca-se no 3ß campo*/
                                             INPUT "" ,     /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                             INPUT "Banco " + MV_TITULO_ACR.COD_BANCO + " n∆o cadastrado.",     /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                             INPUT "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + " na VIEW MV_TITULO_ACR" + 
                                                   CHR(10) + "Cliente: " + v_nom_pessoa + " - " + STRING(MV_TITULO_ACR.CDN_CLIENTE)).
    
                   ASSIGN l-erro = YES.
                   
                   PUT stream s_1 UNFORMATTED 
                               esp_log_integracao.cod_log_integracao + CHR(9) + "Integraá∆o com erro. Banco " + MV_TITULO_ACR.COD_BANCO + " n∆o cadastrado."  SKIP.
    
    
                END.
                ELSE DO:
                    IF LENGTH(MV_TITULO_ACR.COD_AGENC_BCIA) <> LENGTH(banco.cod_format_agenc_bcia) OR
                       LENGTH(MV_TITULO_ACR.COD_CTA_CORREN) <> LENGTH(banco.cod_format_cta_corren) THEN DO:
                        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                                INPUT "EMS5",
                                                 INPUT 38,     /*Formato Agància/Conta incorretos*/
                                                 INPUT 0,      /*Erro padr∆o coloca-se no 3ß campo*/
                                                 INPUT "" ,    /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                                 INPUT "",     /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                                 INPUT "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + " na VIEW MV_TITULO_ACR" + 
                                                       CHR(10) + "Cliente: " + v_nom_pessoa + " - " + STRING(MV_TITULO_ACR.CDN_CLIENTE)).
                        ASSIGN l-erro = YES.
    
                       PUT stream s_1 UNFORMATTED 
                                   esp_log_integracao.cod_log_integracao + CHR(9) + "Integraá∆o com erro. Formato de agància e/ou conta corrente inv†lido."  SKIP.
                    END.
                END.
            END.
        END.
    END.
    ELSE DO:
         RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                 INPUT "EMS5",
                                  INPUT 39,  /*Registro n∆o existe na VIEW de Integraá∆o de t°tulos*/
                                  INPUT 0,  /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "", /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                  INPUT "Registro náao localizado na VIEW MV_TITULO_ACR", /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + " na VIEW MV_TITULO_ACR").
        ASSIGN l-erro = YES.

        PUT stream s_1 UNFORMATTED 
                    esp_log_integracao.cod_log_integracao + CHR(9) + "Integraá∆o com erro. View MV_CLIENTE_INTEGRACAO inexistente!" SKIP.
    END.
END PROCEDURE.

PROCEDURE pi-gerar.
    RUN pi-zera-tables.
    RUN pi-cria-tabelas.

    IF c-cod-refer <> "QT_LIMITE" then do:

        FIND FIRST tt_integr_acr_lote_impl        NO-LOCK NO-ERROR.
        FIND FIRST tt_integr_acr_item_lote_impl_8 NO-LOCK NO-ERROR.
        FIND FIRST tt_integr_acr_aprop_ctbl_pend  NO-LOCK NO-ERROR.
    
        IF AVAIL tt_integr_acr_lote_impl       AND
           AVAIL tt_integr_acr_item_lote_impl_8 AND
           AVAIL tt_integr_acr_aprop_ctbl_pend then
            RUN pi-grava.
        RUN pi-error.
    end.

END PROCEDURE.

PROCEDURE pi-zera-tables.
    EMPTY TEMP-TABLE tt_integr_acr_lote_impl.
    EMPTY TEMP-TABLE tt_integr_acr_repres_comis_2.
    EMPTY TEMP-TABLE tt_integr_acr_item_lote_impl_8.
    EMPTY TEMP-TABLE tt_integr_acr_cheq.
    EMPTY TEMP-TABLE tt_integr_acr_aprop_relacto_2.
    EMPTY TEMP-TABLE tt_integr_acr_aprop_ctbl_pend.
    EMPTY TEMP-TABLE tt_params_generic_api.
    EMPTY TEMP-TABLE tt_integr_acr_relacto_pend_aux.
    EMPTY TEMP-TABLE tt_integr_acr_relacto_pend_cheq.
    EMPTY TEMP-TABLE tt_log_erros_atualiz.
END PROCEDURE.

PROCEDURE pi-cria-tabelas.
    
    DO WHILE v_log_refer_uni = NO:

        ASSIGN c-cod-refer = "".
        
        RUN dtsutp/dtsutp003.p (INPUT "S",
                                OUTPUT c-cod-refer).

        IF c-cod-refer = "QT_LIMITE" then do:
            RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                    INPUT "EMS5",
                                    INPUT 40, 
                                    INPUT 0,                       /*Erro padr∆o coloca-se no 3ß campo*/
                                    INPUT "",                      /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                    INPUT "",                      /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                    INPUT "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + 
                                          CHR(10) + "Cliente: " + v_nom_pessoa). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

            assign v_log_refer_uni = yes.
        end.
        else do:
            run pi_verifica_refer_unica_acr (Input MV_TITULO_ACR.cod_estab,
                                             Input c-cod-refer,
                                             Input "",
                                             Input ?,
                                             output v_log_refer_uni) /*pi_verifica_refer_unica_acr*/.
        end.
    END.

    v_log_refer_uni = NO.


    IF c-cod-refer <> "QT_LIMITE" THEN DO:
        /*Tabelas da API*/
        CREATE tt_integr_acr_lote_impl.
        ASSIGN tt_integr_acr_lote_impl.tta_cod_empresa                = MV_TITULO_ACR.cod_empresa
        /*        tt_integr_acr_lote_impl.ttv_cod_empresa_ext            = N∆o preencher */
               tt_integr_acr_lote_impl.tta_cod_estab                  = MV_TITULO_ACR.cod_estab
        /*        tt_integr_acr_lote_impl.tta_cod_estab_ext              = N∆o preencher */
               tt_integr_acr_lote_impl.tta_cod_refer                  = c-cod-refer
               tt_integr_acr_lote_impl.tta_cod_indic_econ             = MV_TITULO_ACR.cod_indic_econ
        /*        tt_integr_acr_lote_impl.tta_cod_finalid_econ_ext       = N∆o preencher */
        /*        tt_integr_acr_lote_impl.tta_cod_espec_docto            = N∆o preencher */
               tt_integr_acr_lote_impl.tta_dat_transacao              = MV_TITULO_ACR.dat_transacao
        /*        tt_integr_acr_lote_impl.tta_ind_tip_espec_docto        = N∆o preencher */
               tt_integr_acr_lote_impl.tta_ind_orig_tit_acr           = MV_TITULO_ACR.ind_orig_tit_acr
               tt_integr_acr_lote_impl.tta_val_tot_lote_impl_tit_acr  = MV_TITULO_ACR.val_tit_acr
               tt_integr_acr_lote_impl.tta_val_tot_lote_infor_tit_acr = MV_TITULO_ACR.val_tit_acr
               tt_integr_acr_lote_impl.tta_ind_tip_cobr_acr           = IF MV_TITULO_ACR.cod_cartcred <> "" AND MV_TITULO_ACR.cod_cartcred <> ? THEN "Especial" ELSE "Normal"
        /*        tt_integr_acr_lote_impl.ttv_log_lote_impl_ok           = N∆o preencher */
               tt_integr_acr_lote_impl.tta_log_liquidac_autom         = MV_TITULO_ACR.log_liquidac_autom.
    
        ASSIGN V_ser_docto = IF MV_TITULO_ACR.cod_ser_docto = ? THEN "" ELSE MV_TITULO_ACR.cod_ser_docto. 
    
        CREATE tt_integr_acr_item_lote_impl_8.
        ASSIGN tt_integr_acr_item_lote_impl_8.ttv_rec_lote_impl_tit_acr      = RECID(tt_integr_acr_lote_impl)
               tt_integr_acr_item_lote_impl_8.tta_num_seq_refer              = 1 /*Sequencia do t°tulo no lote. Como iremos gerar um lote para cada titulo, preencher sempre com "1"*/ 
               tt_integr_acr_item_lote_impl_8.tta_cdn_cliente                = MV_TITULO_ACR.cdn_cliente              
               tt_integr_acr_item_lote_impl_8.tta_cod_espec_docto            = MV_TITULO_ACR.cod_espec_docto          
               tt_integr_acr_item_lote_impl_8.tta_cod_ser_docto              = V_ser_docto
               tt_integr_acr_item_lote_impl_8.tta_cod_tit_acr                = MV_TITULO_ACR.cod_tit_acr
               tt_integr_acr_item_lote_impl_8.tta_cod_parcela                = MV_TITULO_ACR.cod_parcela              
               tt_integr_acr_item_lote_impl_8.tta_cod_indic_econ             = MV_TITULO_ACR.cod_indic_econ           
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_finalid_econ_ext       = N∆o preencher */
               tt_integr_acr_item_lote_impl_8.tta_cod_portador               = MV_TITULO_ACR.cod_portador             
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_portad_ext             = N∆o preencher */
               tt_integr_acr_item_lote_impl_8.tta_cod_cart_bcia              = MV_TITULO_ACR.cod_cart_bcia            
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_modalid_ext            = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_cond_cobr              = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_motiv_movto_tit_acr    = N∆o preencher */
               tt_integr_acr_item_lote_impl_8.tta_cod_histor_padr            = MV_TITULO_ACR.cod_histor_padr 
               tt_integr_acr_item_lote_impl_8.tta_cdn_repres                 = MV_TITULO_ACR.cdn_repres
               tt_integr_acr_item_lote_impl_8.tta_dat_vencto_tit_acr         = MV_TITULO_ACR.dat_vencto_tit_acr
               tt_integr_acr_item_lote_impl_8.tta_dat_prev_liquidac          = MV_TITULO_ACR.dat_prev_liquidac 
        /*        tt_integr_acr_item_lote_impl_8.tta_dat_desconto               = N∆o preencher */
               tt_integr_acr_item_lote_impl_8.tta_dat_emis_docto             = MV_TITULO_ACR.dat_emis_docto
               tt_integr_acr_item_lote_impl_8.tta_val_tit_acr                = MV_TITULO_ACR.val_tit_acr
        /*        tt_integr_acr_item_lote_impl_8.tta_val_desconto               = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_val_perc_desc              = N∆o preencher */
               tt_integr_acr_item_lote_impl_8.tta_val_perc_juros_dia_atraso  = MV_TITULO_ACR.val_perc_juros_dia_atraso
               tt_integr_acr_item_lote_impl_8.tta_val_perc_multa_atraso      = MV_TITULO_ACR.val_perc_multa_atraso
        /*        tt_integr_acr_item_lote_impl_8.tta_val_base_calc_comis        = N∆o preencher */
               tt_integr_acr_item_lote_impl_8.tta_des_text_histor            = MV_TITULO_ACR.des_text_histor
        /*        tt_integr_acr_item_lote_impl_8.tta_qtd_dias_carenc_multa_acr  = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_banco                  = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_agenc_bcia             = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_cta_corren_bco         = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_digito_cta_corren      = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_instruc_bcia_1_movto   = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_instruc_bcia_2_movto   = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_qtd_dias_carenc_juros_acr  = N∆o preencher */
               tt_integr_acr_item_lote_impl_8.tta_val_liq_tit_acr            = MV_TITULO_ACR.val_liq_tit_acr
               tt_integr_acr_item_lote_impl_8.tta_ind_tip_espec_docto        = MV_TITULO_ACR.ind_tip_espec_docto
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_cond_pagto             = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.ttv_cdn_agenc_fp               = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_ind_sit_tit_acr            = N∆o preencher */
               tt_integr_acr_item_lote_impl_8.tta_log_liquidac_autom         = MV_TITULO_ACR.log_liquidac_autom
        /*        tt_integr_acr_item_lote_impl_8.tta_num_id_tit_acr             = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_num_id_movto_tit_acr       = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_num_id_movto_cta_corren    = N∆o preencher */
               tt_integr_acr_item_lote_impl_8.tta_cod_admdra_cartao_cr       = MV_TITULO_ACR.cod_admdra_cartao_cr
               tt_integr_acr_item_lote_impl_8.tta_cod_cartcred               = MV_TITULO_ACR.COD_CARTCRED
               tt_integr_acr_item_lote_impl_8.tta_cod_mes_ano_valid_cartao   = MV_TITULO_ACR.cod_mes_ano_valid_cartao  
               tt_integr_acr_item_lote_impl_8.tta_cod_autoriz_cartao_cr      = MV_TITULO_ACR.cod_autoriz_cartao_cr
               tt_integr_acr_item_lote_impl_8.tta_dat_compra_cartao_cr       = MV_TITULO_ACR.dat_compra_cartao_cr 
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_conces_telef           = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_num_ddd_localid_conces     = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_num_prefix_localid_conces  = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_num_milhar_localid_conces  = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_log_tip_cr_perda_dedut_tit = N∆o preencher */
               tt_integr_acr_item_lote_impl_8.tta_cod_refer                  = tt_integr_acr_lote_impl.tta_cod_refer 
        /*        tt_integr_acr_item_lote_impl_8.tta_ind_ender_cobr             = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_nom_abrev_contat           = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_log_db_autom               = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_log_destinac_cobr          = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_ind_sit_bcia_tit_acr       = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_tit_acr_bco            = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_agenc_cobr_bcia        = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_dat_abat_tit_acr           = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_val_perc_abat_acr          = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_val_abat_tit_acr           = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_des_obs_cobr               = N∆o preencher */
        /*        tt_integr_acr_item_lote_impl_8.tta_val_cotac_indic_econ       = N∆o preencher */
               tt_integr_acr_item_lote_impl_8.ttv_rec_item_lote_impl_tit_acr = RECID(tt_integr_acr_item_lote_impl_8)
        /*        tt_integr_acr_item_lote_impl_8.tta_ind_tip_calc_juros         = N∆o preencher */
               tt_integr_acr_item_lote_impl_8.ttv_cod_comprov_vda            = MV_TITULO_ACR.cod_comprov_vda 
               tt_integr_acr_item_lote_impl_8.ttv_num_parc_cartcred          = MV_TITULO_ACR.num_parc_cartcred        
               tt_integr_acr_item_lote_impl_8.ttv_cod_autoriz_bco_emissor    = MV_TITULO_ACR.cod_comprov_vda /*MV_TITULO_ACR.cod_autoriz_bco_emissor  */
               tt_integr_acr_item_lote_impl_8.ttv_cod_lote_origin            = MV_TITULO_ACR.cod_lote_origin
        /*        tt_integr_acr_item_lote_impl_8.ttv_cod_estab_vendor           = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.ttv_num_planilha_vendor        = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.ttv_cod_cond_pagto_vendor      = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.ttv_val_cotac_tax_vendor_clien = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.ttv_dat_base_fechto_vendor     = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.ttv_qti_dias_carenc_fechto     = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.ttv_log_assume_tax_bco         = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.ttv_log_vendor                 = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.ttv_cod_estab_portad           = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_proces_export          = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.ttv_val_cr_pis                 = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.ttv_val_cr_cofins              = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.ttv_val_cr_csll                = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.tta_cod_indic_econ_desemb      = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.tta_val_base_calc_impto        = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.tta_log_retenc_impto_impl      = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
        /*        tt_integr_acr_item_lote_impl_8.ttv_cod_nota_fisc_faturam      = N∆o h† campos na VIEW nem Ç obrigat¢rio*/
            .
    
        IF MV_TITULO_ACR.num_cheque <> ? AND
           MV_TITULO_ACR.num_cheque <> 0 THEN DO:

            CREATE tt_integr_acr_cheq.	/*S∆o as informaá‰es dos cheques usados no pagamento de t°tulos ACR.	Shared*/
            ASSIGN tt_integr_acr_cheq.tta_cod_banco	              = MV_TITULO_ACR.cod_banco                         /*character	x(8)	        Banco	                    Sim	                                            View Oracle    */
                   tt_integr_acr_cheq.tta_cod_agenc_bcia	      = MV_TITULO_ACR.cod_agenc_bcia                    /*character	x(10)	        Agància Banc†ria	        Sim	                                            View Oracle    */
                   tt_integr_acr_cheq.tta_cod_cta_corren	      = MV_TITULO_ACR.cod_cta_corren                    /*character	x(10)	        Conta Corrente	            Sim	                                            View Oracle    */
                   tt_integr_acr_cheq.tta_num_cheque	          = MV_TITULO_ACR.num_cheque                        /*integer	    >>>>,>>>,>>9	Num Cheque	                Sim	                                            View Oracle    */
                   tt_integr_acr_cheq.tta_dat_emis_cheq	          = MV_TITULO_ACR.dat_emis_cheq                     /*date	    99/99/9999	    Data Emiss∆o	            Sim	                                                View Oracle    */
                   tt_integr_acr_cheq.tta_dat_depos_cheq_acr	  = MV_TITULO_ACR.dat_depos_cheq_acr                /*date	    99/99/9999	    Dep¢sito	                Sim	                                                View Oracle    */
                   tt_integr_acr_cheq.tta_dat_prev_depos_cheq_acr = MV_TITULO_ACR.dat_prev_depos_cheq_acr           /*date	    99/99/9999	    Previs∆o Dep¢sito	        Sim	                                                View Oracle    */
                   tt_integr_acr_cheq.tta_dat_desc_cheq_acr	      = MV_TITULO_ACR.dat_desc_cheq_acr                 /*date	    99/99/9999	    Data Desconto	            Sim	                                                View Oracle    */
                   tt_integr_acr_cheq.tta_dat_prev_desc_cheq_acr  = MV_TITULO_ACR.dat_prev_desc_cheq_acr            /*date	    99/99/9999	    Data Prev Desc	            Sim	                                                View Oracle    */
                   tt_integr_acr_cheq.tta_val_cheque	          = MV_TITULO_ACR.val_cheque                        /*decimal	    >>>,>>>,>>9.99	Valor Cheque	            Sim	                                            View Oracle    */
                   tt_integr_acr_cheq.tta_nom_emit	              = MV_TITULO_ACR.nom_emit                          /*character	x(40)	        Nome Emitente	            Sim	                                            View Oracle    */
                   tt_integr_acr_cheq.tta_nom_cidad_emit	      = MV_TITULO_ACR.nom_cidad_emit                    /*character	x(30)	        Cidade Emitente	            Sim	                                            View Oracle    */
                   tt_integr_acr_cheq.tta_cod_estab	              = MV_TITULO_ACR.cod_estab                         /*character	x(3)	        Estabelecimento	            Sim	                                            View Oracle    */
            /*        tt_integr_acr_cheq.tta_cod_estab_ext           = n∆o preencher       /*character x(8)            Estabelecimento Exte        N∆o                                             N∆o preencher  */ */
                   tt_integr_acr_cheq.tta_cod_id_feder	          = MV_TITULO_ACR.cod_id_feder                      /*character	x(20)	        ID Federal	                Sim	                                            View Oracle    */
            /*        tt_integr_acr_cheq.tta_cod_motiv_devol_cheq    = n∆o preencher                  /*character  x(5)            Motivo Devoluá∆o            Sim                                             View Oracle    */ */
                   tt_integr_acr_cheq.tta_cod_indic_econ	      = MV_TITULO_ACR.cod_indic_econ                    /*character	x(8)	        Moeda	Sim p/ Matriz de traduá∆o	                                        View Oracle    */
            /*        tt_integr_acr_cheq.tta_cod_finalid_econ_ext    = N∆o preencher       /*character x(8)            Finalid Econ Externa. Caso seja cheque devolvido, informar o motivo.Sim/N∆o N∆o preencher  */ */
                   tt_integr_acr_cheq.tta_cod_usuar_cheq_acr_terc = MV_TITULO_ACR.cod_usuar_cheq_acr_terc           /*character	x(12)	        Usu†rio	Sim/N∆o	                                                            View Oracle    */
                   tt_integr_acr_cheq.tta_log_pend_cheq_acr	      = NO                                           /*logical	    Sim/N∆o	        Cheque Pendente	Sim/N∆o	                                                    Fixo "N∆o"     */
                   tt_integr_acr_cheq.tta_log_cheq_terc	          = MV_TITULO_ACR.log_cheq_terc                     /*logical	    Sim/N∆o	        Cheque Terceiro	Sim/N∆o	                                                    View Oracle    */
                   tt_integr_acr_cheq.tta_log_cheq_acr_renegoc	  = MV_TITULO_ACR.log_cheq_acr_renegoc              /*logical	    Sim/N∆o	        Cheque Reneg	Sim/N∆o	                                                    View Oracle    */
                   tt_integr_acr_cheq.tta_log_cheq_acr_devolv	  = NO                                           /*logical	    Sim/N∆o	        Cheque Devolvido	Sim/N∆o	                                                Fixo "N∆o"     */
                   tt_integr_acr_cheq.tta_num_pessoa	          = MV_TITULO_ACR.num_pessoa                        /*integer	    >>>,>>>,>>9	    Pessoa	Sim/N∆o	                                                            View Oracle    */
                   tt_integr_acr_cheq.tta_cod_pais	              = MV_TITULO_ACR.cod_pais                          /*character	x(3)	        Pa°s	Sim/N∆o	                                                            View Oracle    */
            .
        
            IF tt_integr_acr_cheq.tta_num_pessoa = ? THEN
                ASSIGN tt_integr_acr_cheq.tta_num_pessoa = 0.
        END.

        FOR EACH MV_APROPR_TITULO_ACR NO-LOCK
             WHERE MV_APROPR_TITULO_ACR.cod_estab       = MV_TITULO_ACR.cod_estab
               AND MV_APROPR_TITULO_ACR.cod_espec_docto = MV_TITULO_ACR.cod_espec_docto
               AND MV_APROPR_TITULO_ACR.cod_ser_docto   = MV_TITULO_ACR.cod_ser_docto
               AND MV_APROPR_TITULO_ACR.cod_tit_acr     = MV_TITULO_ACR.cod_tit_acr
               AND MV_APROPR_TITULO_ACR.cod_parcela     = MV_TITULO_ACR.cod_parcela:
        
            ASSIGN V_plano = IF MV_APROPR_TITULO_ACR.cod_plano_ccusto = ? THEN "" ELSE MV_APROPR_TITULO_ACR.cod_plano_ccusto. 
            ASSIGN V_ccusto = IF MV_APROPR_TITULO_ACR.cod_ccusto = ? THEN "" ELSE MV_APROPR_TITULO_ACR.cod_ccusto. 

            CREATE tt_integr_acr_aprop_ctbl_pend.	/*Gerar† as apropriaá‰es cont†beis.	Shared*/
            ASSIGN tt_integr_acr_aprop_ctbl_pend.ttv_rec_item_lote_impl_tit_acr	= RECID(tt_integr_acr_item_lote_impl_8)                     /*  recid       >>>>>>9         Atributo ttv_rec_item_lote_impl_tit_acr dever† receber o valor do atributo (tt_integr_acr_item_lote_impl_2.ttv_rec_item_lote_impl_tit_acr), para que haja relacionamento entre as duas temp-tables.	Sim*/
                   tt_integr_acr_aprop_ctbl_pend.tta_cod_plano_cta_ctbl	        = MV_APROPR_TITULO_ACR.cod_plano_cta_ctbl          /* character    x(8)            Plano Contas            Sim                             View Oracle    */
                   tt_integr_acr_aprop_ctbl_pend.tta_cod_cta_ctbl	            = MV_APROPR_TITULO_ACR.cod_cta_ctbl          /* character    x(20)           Conta Cont bil          Sim                             View Oracle    */
            /*        tt_integr_acr_aprop_ctbl_pend.tta_cod_cta_ctbl_ext           = N∆o preencher        /* character    x(20)           Conta Contab Extern     Sim                             N∆o preencher  */ */
            /*        tt_integr_acr_aprop_ctbl_pend.tta_cod_sub_cta_ctbl_ext       = N∆o preencher        /* character    x(15)           Sub Conta Externa       N∆o                             N∆o preencher  */ */
                   tt_integr_acr_aprop_ctbl_pend.tta_cod_unid_negoc	            = MV_APROPR_TITULO_ACR.cod_unid_negoc          /* character    x(3)            Unid Neg¢cio            Sim p/ Matriz de traduá∆o       View Oracle    */
            /*        tt_integr_acr_aprop_ctbl_pend.tta_cod_unid_negoc_ext         = N∆o preencher        /* character    x(8)            Unid Neg¢cio Externa    Sim p/ Matriz de traduá∆o       N∆o preencher  */ */
                   tt_integr_acr_aprop_ctbl_pend.tta_cod_plano_ccusto	        = V_plano          /* character    x(8)            Plano Centros Custo                                     View Oracle    */
                   tt_integr_acr_aprop_ctbl_pend.tta_cod_ccusto	                = V_ccusto                /* character    x(11)           Centro Custo                                            View Oracle    */
            /*        tt_integr_acr_aprop_ctbl_pend.tta_cod_ccusto_ext             = N∆o preencher        /* character    x(8)            Centro Custo Externo                                    N∆o preencher  */ */
                   tt_integr_acr_aprop_ctbl_pend.tta_cod_tip_fluxo_financ	    = MV_APROPR_TITULO_ACR.cod_tip_fluxo_financ          /* character    x(12)           Tipo Fluxo Financ       Sim p/ Matriz de traduá∆o       View Oracle    */
            /*        tt_integr_acr_aprop_ctbl_pend.tta_cod_fluxo_financ_ext       = N∆o preencher        /* character    x(20)           Tipo Fluxo Externo      Sim p/ Matriz de traduá∆o       N∆o preencher  */ */
                   tt_integr_acr_aprop_ctbl_pend.tta_val_aprop_ctbl	            = MV_APROPR_TITULO_ACR.val_aprop_ctbl          /* decimal      ->>>,>>>,>>9.99 Valor Aprop Ctbl        Sim p/ Matriz de traduá∆o   View Oracle        */
            /*        tt_integr_acr_aprop_ctbl_pend.tta_cod_unid_federac           = N∆o preencher        /* character    x(3)            Unidade Federaá∆o                                       N∆o preencher  */ */
            /*        tt_integr_acr_aprop_ctbl_pend.tta_log_impto_val_agreg        = N∆o preencher        /*     logical      Sim/N∆o         Impto Val Agreg                                 N∆o preencher      */ */
            /*        tt_integr_acr_aprop_ctbl_pend.tta_cod_imposto                = N∆o preencher        /*     character    x(5)            Imposto                                             N∆o preencher  */ */
            /*        tt_integr_acr_aprop_ctbl_pend.tta_cod_classif_impto          = N∆o preencher        /* character    x(05)           Class Imposto                                           N∆o preencher  */ */
            /*        tt_integr_acr_aprop_ctbl_pend.tta_cod_pais                   = N∆o preencher        /* character    x(3)            Pa°s                                                    N∆o preencher  */ */
            /*        tt_integr_acr_aprop_ctbl_pend.tta_cod_pais_ext               = N∆o preencher        /* character    x(20)           Pa°s Externo                                            N∆o preencher  */ */
                .
        END.

        IF MV_TITULO_ACR.num_cheque <> ? AND
           MV_TITULO_ACR.num_cheque <> 0 THEN DO:

            CREATE tt_integr_acr_relacto_pend_cheq.
            ASSIGN tt_integr_acr_relacto_pend_cheq.ttv_rec_item_lote_impl_tit_acr = RECID(tt_integr_acr_item_lote_impl_8)
                   tt_integr_acr_relacto_pend_cheq.tta_cod_banco                  = tt_integr_acr_cheq.tta_cod_banco     
                   tt_integr_acr_relacto_pend_cheq.tta_cod_agenc_bcia             = tt_integr_acr_cheq.tta_cod_agenc_bcia
                   tt_integr_acr_relacto_pend_cheq.tta_cod_cta_corren             = tt_integr_acr_cheq.tta_cod_cta_corren
                   tt_integr_acr_relacto_pend_cheq.tta_num_cheque                 = tt_integr_acr_cheq.tta_num_cheque    
                   tt_integr_acr_relacto_pend_cheq.tta_val_vincul_cheq_acr        = tt_integr_acr_cheq.tta_val_cheque
                   tt_integr_acr_relacto_pend_cheq.tta_cdn_bco_cheq_salario       = 0.
        END.
    END. /*IF c-cod-refer <> "QT_LIMITE" THEN */
END PROCEDURE.


PROCEDURE pi-grava.
    def var p_cod_matriz_trad_org_ext as char initial "EMS" no-undo.
    def var p_log_atualiza_refer_acr  as log  no-undo.
    def var p_log_assume_dat_emis     as log  no-undo.

    Def var v_hdl_programa as HANDLE format ">>>>>>9":U no-undo.

    FOR EACH tt-erros-integr:
        DELETE tt-erros-integr.
    END.
    
    ASSIGN p_log_assume_dat_emis     = NO
           p_cod_matriz_trad_org_ext = "EMS".
           p_log_atualiza_refer_acr  = YES. /* Atualiza lote sim ou n∆o */


    RUN prgfin/acr/acr900zi.py persistent set v_hdl_programa.

    IF NOT VALID-HANDLE(v_hdl_programa) THEN DO:
        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                INPUT "EMS5",
                                INPUT 41, /*Programa n∆o existe no EMS 5*/
                                INPUT 0,  /*Erro padr∆o coloca-se no 3ß campo*/
                                INPUT "", /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                INPUT "", /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                INPUT "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + " - Programa inexistente: prgfin/acr/acr900zi.py. Verifique com o Adminstrador do sistema.").
    END.
    ELSE DO:
    
        /* message "Antes chamada api e for each" view-as alert-box.                                       */
        /* for each tt_integr_acr_lote_impl no-lock.                                                       */
        /*     message "tta_cod_empresa               :" tta_cod_empresa                skip               */
        /*             "ttv_cod_empresa_ext           :" ttv_cod_empresa_ext            skip               */
        /*             "tta_cod_estab                 :" tta_cod_estab                  skip               */
        /*             "tta_cod_estab_ext             :" tta_cod_estab_ext              skip               */
        /*             "tta_cod_refer                 :" tta_cod_refer                  skip               */
        /*             "tta_cod_indic_econ            :" tta_cod_indic_econ             skip               */
        /*             "tta_cod_finalid_econ_ext      :" tta_cod_finalid_econ_ext       skip               */
        /*             "tta_cod_espec_docto           :" tta_cod_espec_docto            skip               */
        /*             "tta_dat_transacao             :" tta_dat_transacao              skip               */
        /*             "tta_ind_tip_espec_docto       :" tta_ind_tip_espec_docto        skip               */
        /*             "tta_ind_orig_tit_acr          :" tta_ind_orig_tit_acr           skip               */
        /*             "tta_val_tot_lote_impl_tit_acr :" tta_val_tot_lote_impl_tit_acr  skip               */
        /*             "tta_val_tot_lote_infor_tit_acr:" tta_val_tot_lote_infor_tit_acr skip               */
        /*             "tta_ind_tip_cobr_acr          :" tta_ind_tip_cobr_acr           skip               */
        /*             "ttv_log_lote_impl_ok          :" ttv_log_lote_impl_ok           skip               */
        /*             "tta_log_liquidac_autom        :" tta_log_liquidac_autom         view-as alert-box. */
        /* end.                                                                                            */
        /* message "Antes chamada api nova" view-as alert-box.                                             */



        RUN pi_main_code_integr_acr_new_11 IN v_hdl_programa (INPUT 11,
                                                              INPUT p_cod_matriz_trad_org_ext,
                                                              INPUT p_log_atualiza_refer_acr,
                                                              INPUT p_log_assume_dat_emis,
                                                              INPUT TABLE tt_integr_acr_repres_comis_2, /*N∆o est† sendo preenchida*/
                                                              INPUT-OUTPUT TABLE tt_integr_acr_item_lote_impl_8, /*Est† sendo preenchida*/
                                                              INPUT TABLE tt_integr_acr_aprop_relacto_2,
                                                              INPUT-OUTPUT TABLE tt_params_generic_api,
                                                              INPUT TABLE tt_integr_acr_relacto_pend_aux).
        DELETE PROCEDURE v_hdl_programa.
    END.
END PROCEDURE.

PROCEDURE pi-error.
    IF CAN-FIND(FIRST tt_log_erros_atualiz) THEN DO:
        FIND FIRST b_tit_acr WHERE  /*verifica se o t°tulo foi integrado no EMS5*/
                   b_tit_acr.cod_estab       = ENTRY(1,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") and
                   b_tit_acr.cod_espec_docto = ENTRY(2,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") and
                   b_tit_acr.cod_ser_docto   = ENTRY(3,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") and
                   b_tit_acr.cod_tit_acr     = ENTRY(4,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") and
                   b_tit_acr.cod_parcela     = ENTRY(5,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") NO-LOCK NO-ERROR.
        IF AVAIL b_tit_acr THEN DO:

            RUN espapi/espapi004.p (BUFFER esp_log_integracao,
                                    INPUT 2).
            

            PUT stream s_1 UNFORMATTED esp_log_integracao.cod_log_integracao + CHR(9) +
                 "Integrado com sucesso!" SKIP.
        END.
        ELSE DO:
            FOR EACH tt_log_erros_atualiz:
            
                IF tt_log_erros_atualiz.tta_num_seq_refer = 0 /*Erro genÇrico*/ THEN DO:
                    FIND FIRST b_tit_acr WHERE  /*verifica se o t°tulo foi integrado no EMS5*/
                               b_tit_acr.cod_estab       = ENTRY(1,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") and
                               b_tit_acr.cod_espec_docto = ENTRY(2,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") and
                               b_tit_acr.cod_ser_docto   = ENTRY(3,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") and
                               b_tit_acr.cod_tit_acr     = ENTRY(4,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") and
                               b_tit_acr.cod_parcela     = ENTRY(5,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") NO-LOCK NO-ERROR.
                    IF NOT AVAIL b_tit_acr THEN DO:
                        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                                INPUT "EMS5",
                                                INPUT 0, 
                                                INPUT tt_log_erros_atualiz.ttv_num_mensagem,                       /*Erro padr∆o coloca-se no 3ß campo*/
                                                INPUT tt_log_erros_atualiz.ttv_des_msg_erro,                       /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                                INPUT tt_log_erros_atualiz.ttv_des_msg_ajuda,                          /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                                INPUT "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + 
                                                      CHR(10) + "Cliente: " + v_nom_pessoa). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
    
                        PUT stream s_1 UNFORMATTED 
                             esp_log_integracao.cod_log_integracao + CHR(9) + 
                             STRING(tt_log_erros_atualiz.ttv_num_mensagem) + CHR(9) + 
                             tt_log_erros_atualiz.ttv_des_msg_erro  + CHR(9) + 
                             tt_log_erros_atualiz.ttv_des_msg_ajuda SKIP.
                    END.
                END.
                ELSE DO:
                    RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                            INPUT "EMS5",
                                            INPUT 0, 
                                            INPUT tt_log_erros_atualiz.ttv_num_mensagem,                       /*Erro padr∆o coloca-se no 3ß campo*/
                                            INPUT tt_log_erros_atualiz.ttv_des_msg_erro,                       /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                            INPUT tt_log_erros_atualiz.ttv_des_msg_ajuda,                          /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                            INPUT "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + 
                                                  CHR(10) + "Cliente: " + v_nom_pessoa). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
    
                    PUT stream s_1 UNFORMATTED 
                    esp_log_integracao.cod_log_integracao + CHR(9) + 
                             "Integraá∆o com erro" + CHR(9) + 
                             STRING(tt_log_erros_atualiz.ttv_num_mensagem)  + CHR(9) + 
                             tt_log_erros_atualiz.ttv_des_msg_erro   + CHR(9) + 
                             tt_log_erros_atualiz.ttv_des_msg_ajuda SKIP.
                END.
            END.
        END.
    END.
    ELSE DO:
        FIND FIRST b_tit_acr WHERE  /*verifica se o t°tulo foi integrado no EMS5*/
                   b_tit_acr.cod_estab       = ENTRY(1,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") and
                   b_tit_acr.cod_espec_docto = ENTRY(2,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") and
                   b_tit_acr.cod_ser_docto   = ENTRY(3,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") and
                   b_tit_acr.cod_tit_acr     = ENTRY(4,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") and
                   b_tit_acr.cod_parcela     = ENTRY(5,tt-siaf-acr.COD_CHAVE_INTEGRACAO,"|") NO-LOCK NO-ERROR.
        IF AVAIL b_tit_acr THEN DO:

            RUN espapi/espapi004.p (BUFFER esp_log_integracao,
                                    INPUT 2).

            PUT stream s_1 UNFORMATTED esp_log_integracao.cod_log_integracao + CHR(9) +
                 "Integrado com sucesso!" SKIP.
        END.
        ELSE DO:
            RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                    INPUT "EMS5",
                                    INPUT 42, 
                                    INPUT "",                       /*Erro padr∆o coloca-se no 3ß campo*/
                                    INPUT "",                       /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                    INPUT "",                       /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                    INPUT "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + " - API n∆o retornou erros, porÇm o t°tulo n∆o foi implantado!" + 
                                          CHR(10) + "Cliente: " + v_nom_pessoa). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

            PUT stream s_1 UNFORMATTED
                esp_log_integracao.cod_log_integracao + CHR(9) + 
                "Integraá∆o com erro" + CHR(9) + 
                "Chave: " + tt-siaf-acr.COD_CHAVE_INTEGRACAO + "API n∆o retornou erros, porÇm o t°tulo n∆o foi implantado!" SKIP.
        END.
    END.
END PROCEDURE.

PROCEDURE pi_verifica_refer_unica_acr:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_estab
        as character
        format "x(3)"
        no-undo.
    def Input param p_cod_refer
        as character
        format "x(10)"
        no-undo.
    def Input param p_cod_table
        as character
        format "x(8)"
        no-undo.
    def Input param p_rec_tabela
        as recid
        format ">>>>>>9"
        no-undo.
    def output param p_log_refer_uni
        as logical
        format "Sim/N∆o"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************** Buffer Definition Begin *************************/

    /*&if "{&emsfin_version}" >= "5.02" &then*/
    def buffer b_cobr_especial_acr
        for cobr_especial_acr.
    /*&endif*/
    /*&if "{&emsfin_version}" >= "5.01" &then*/
    def buffer b_lote_impl_tit_acr
        for lote_impl_tit_acr.
    /*&endif*/
    /*&if "{&emsfin_version}" >= "5.01" &then*/
    def buffer b_lote_liquidac_acr
        for lote_liquidac_acr.
    /*&endif*/
    /*&if "{&emsfin_version}" >= "5.01" &then*/
    def buffer b_movto_tit_acr
        for movto_tit_acr.
    /*&endif*/
    /*&if "{&emsfin_version}" >= "5.01" &then*/
    def buffer b_operac_financ_acr
        for operac_financ_acr.
    /*&endif*/
    /*&if "{&emsfin_version}" >= "5.01" &then*/
    def buffer b_renegoc_acr
        for renegoc_acr.
    /*&endif*/


    /*************************** Buffer Definition End **************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_return
        as character
        format "x(40)":U
        no-undo.


    /************************** Variable Definition End *************************/

    assign p_log_refer_uni = yes.

    if  p_cod_table = "lote_impl_tit_acr" /*l_lote_impl_tit_acr*/  then do:
        find first b_lote_impl_tit_acr no-lock
             where b_lote_impl_tit_acr.cod_estab = p_cod_estab
               and b_lote_impl_tit_acr.cod_refer = p_cod_refer
               and recid( b_lote_impl_tit_acr ) <> p_rec_tabela
             use-index ltmplttc_id no-error.
        if  avail b_lote_impl_tit_acr then
            assign p_log_refer_uni = no.
    end.

    if  p_cod_table = "lote_liquidac_acr" /*l_lote_liquidac_acr*/  then do:
        find first b_lote_liquidac_acr no-lock
             where b_lote_liquidac_acr.cod_estab_refer = p_cod_estab
               and b_lote_liquidac_acr.cod_refer       = p_cod_refer
               and recid( b_lote_liquidac_acr )       <> p_rec_tabela
             use-index ltlqdccr_id no-error.
        if  avail b_lote_liquidac_acr then
            assign p_log_refer_uni = no.
    end.

    if  p_cod_table = "Operaá∆o financeira" /*l_operacao_financ*/  then do:
        find first b_operac_financ_acr no-lock
             where b_operac_financ_acr.cod_estab               = p_cod_estab
               and b_operac_financ_acr.cod_movto_operac_financ = p_cod_refer
               and recid( b_operac_financ_acr )               <> p_rec_tabela
             use-index oprcfnna_id no-error.
        if  avail b_operac_financ_acr then
            assign p_log_refer_uni = no.
    end.

    if  p_cod_table = 'cobr_especial_acr' then do:
        find first b_cobr_especial_acr no-lock
             where b_cobr_especial_acr.cod_estab = p_cod_estab
               and b_cobr_especial_acr.cod_refer = p_cod_refer
               and recid( b_cobr_especial_acr ) <> p_rec_tabela
             use-index cbrspclc_id no-error.
        if  avail b_cobr_especial_acr then
            assign p_log_refer_uni = no.
    end.

    if  p_log_refer_uni = yes then do:
        find first b_renegoc_acr no-lock
            where b_renegoc_acr.cod_estab = p_cod_estab
            and   b_renegoc_acr.cod_refer = p_cod_refer
            and   recid(b_renegoc_acr)   <> p_rec_tabela
            no-error.
        if  avail b_renegoc_acr then
            assign p_log_refer_uni = no.
        else do:
            find first b_movto_tit_acr no-lock
                 where b_movto_tit_acr.cod_estab = p_cod_estab
                   and b_movto_tit_acr.cod_refer = p_cod_refer
                   and recid(b_movto_tit_acr)   <> p_rec_tabela
                 use-index mvtttcr_refer
                 no-error.
            if  avail b_movto_tit_acr then
                assign p_log_refer_uni = no.
        end.
    end.

    &if defined (BF_FIN_BCOS_HISTORICOS) &then
        if  v_log_utiliza_mbh
        and can-find (his_movto_tit_acr_histor no-lock
                      where his_movto_tit_acr_histor.cod_estab = p_cod_estab
                      and   his_movto_tit_acr_histor.cod_refer = p_cod_refer)
        then do:

            assign p_log_refer_uni = no.
        end.
    &endif
END PROCEDURE. /* pi_verifica_refer_unica_acr */

RETURN 'OK'.
/* fim do programa */


/*tt_integr_acr_abat_antecip:
atributos que dever∆o receber valor na temp-table:

tt_integr_acr_abat_antecip.ttv_rec_item_lote_impl_tit_acr 
tt_integr_acr_abat_antecip.ttv_rec_abat_antecip_acr 
tt_integr_acr_abat_antecip.tta_cod_estab
tt_integr_acr_abat_antecip.tta_cod_espec_docto
tt_integr_acr_abat_antecip.tta_cod_ser_docto
tt_integr_acr_abat_antecip.tta_cod_tit_acr
tt_integr_acr_abat_antecip.tta_cod_parcela
tt_integr_acr_abat_antecip.tta_val_abtdo_antecip_tit_abat 

-      O atributo ttv_rec_item_lote_impl_tit_acr deve receber o  recid da tt_integr_acr_item_lote_impl_8
-      O atributo ttv_rec_abat_antecip_acr deve receber o  recid da tt_integr_acr_abat_antecip
*/


/*tt_integr_acr_abat_prev:
Atributo ttv_rec_item_lote_impl_tit_acr dever† receber o valor do atributo (tt_integr_acr_item_lote_impl_8.ttv_rec_item_lote_impl_tit_acr), para que haja relacionamento entre as duas temp-tables. 
Segue abaixo os atributos que dever∆o receber valor:

tt_integr_acr_abat_prev.tta_cod_estab
tt_integr_acr_abat_prev.tta_cod_espec_docto
tt_integr_acr_abat_prev.tta_cod_ser_docto
tt_integr_acr_abat_prev.tta_cod_tit_acr
tt_integr_acr_abat_prev.tta_cod_parcela
tt_integr_acr_abat_prev.tta_val_abtdo_prev_tit_abat
Em caso de zerar o saldo da previs∆o, deve-se informar como (yes) no atributo abaixo,
tt_integr_acr_abat_prev.tta_log_zero_sdo_prev.
*/


/*tt_integr_acr_aprop_ctbl_pend:
Atributo ttv_rec_item_lote_impl_tit_acr dever† receber o valor do atributo (tt_integr_acr_item_lote_impl_8.ttv_rec_item_lote_impl_tit_acr), para que haja relacionamento entre as duas temp-tables. 
Segue abaixo os atributos que dever∆o receber valor:

tt_integr_acr_aprop_ctbl_pend.tta_cod_plano_cta_ctbl
tt_integr_acr_aprop_ctbl_pend.tta_cod_cta_ctbl
tt_integr_acr_aprop_ctbl_pend.tta_cod_cta_ctbl_ext
para matriz de traduá∆o
tt_integr_acr_aprop_ctbl_pend.tta_cod_unid_negoc
tt_integr_acr_aprop_ctbl_pend.tta_cod_unid_negoc_ext
para matriz de traduá∆o
tt_integr_acr_aprop_ctbl_pend.tta_cod_tip_fluxo_financ
tt_integr_acr_aprop_ctbl_pend.tta_cod_fluxo_financ_ext
tt_integr_acr_aprop_ctbl_pend.tta_val_aprop_ctbl
*/

/*tt_integr_acr_aprop_desp_rec:
Atributo ttv_rec_item_lote_impl_tit_acr dever† receber o valor do atributo (tt_integr_acr_item_lote_impl_8.ttv_rec_item_lote_impl_tit_acr), para que haja relacionamento entre as duas temp-tables. 
Segue abaixo os atributos que dever∆o receber valor:

tt_integr_acr_aprop_desp_rec.tta_cod_plano_cta_ctbl
tt_integr_acr_aprop_desp_rec.tta_cod_cta_ctbl
tt_integr_acr_aprop_desp_rec.tta_cod_unid_negoc
tt_integr_acr_aprop_desp_rec.tta_cod_tip_fluxo_financ
tt_integr_acr_aprop_desp_rec.tta_ind_tip_aprop_recta_despes
tt_integr_acr_aprop_desp_rec.tta_cod_tip_abat (Este atributo s¢ se faz obrigat¢rio quando o tipo de apropriaá∆o for "ABATIMENTO")
tt_integr_acr_aprop_desp_rec.tta_val_perc_rat_ctbz 
*/

/*tt_integr_acr_aprop_liq_antec:
Atributo ttv_rec_item_lote_impl_tit_acr dever† receber o valor do atributo (tt_integr_acr_item_lote_impl_8.ttv_rec_item_lote_impl_tit_acr), para que haja relacionamento entre as duas temp-tables. 
Atributo ttv_rec_abat_antecip_acr dever† receber o valor do atributo (tt_integr_acr_abat_antecip.ttv_rec_abat_antecip_acr),  para que haja relacionamento entre as duas temp-tables.
Segue abaixo os atributos que dever∆o receber valor:

tt_integr_acr_aprop_liq_antec.tta_cod_fluxo_financ_ext
para matriz de traduá∆o
tt_integr_acr_aprop_liq_antec.ttv_cod_fluxo_financ_tit_ext
para matriz de traduá∆o
tt_integr_acr_aprop_liq_antec.tta_cod_unid_negoc
tt_integr_acr_aprop_liq_antec.tta_cod_tip_fluxo_financ
tt_integr_acr_aprop_liq_antec.tta_cod_unid_negoc_tit
tt_integr_acr_aprop_liq_antec.tta_cod_tip_fluxo_financ_tit
tt_integr_acr_aprop_liq_antec.tta_val_abtdo_antecip
*/

/*tt_integr_acr_aprop_relacto:
Atributo ttv_rec_relacto_pend_tit_acr dever† receber o valor do atributo 
(tt_integr_acr_relacto_pend. ttv_rec_relacto_pend_tit_acr), para que haja relacionamento entre as duas temp-tables.
Segue abaixo os atributos que dever∆o receber valor:

tt_integr_acr_aprop_relacto.tta_cod_cta_ctbl_ext
para matriz de traduá∆o
tt_integr_acr_aprop_relacto.tta_cod_unid_negoc_ext
para matriz de traduá∆o
tt_integr_acr_aprop_relacto.tta_cod_fluxo_financ_ext
para matriz de traduá∆o
tt_integr_acr_aprop_relacto.tta_cod_plano_cta_ctbl
tt_integr_acr_aprop_relacto.tta_cod_cta_ctbl
tt_integr_acr_aprop_relacto.tta_cod_unid_negoc
tt_integr_acr_aprop_relacto.tta_cod_tip_fluxo_financ
tt_integr_acr_aprop_relacto.tta_val_aprop_ctbl
*/

/*tt_integr_acr_cheq:
Segue abaixo os atributos que dever∆o receber valor:

tt_integr_acr_cheq.tta_cod_banco
tt_integr_acr_cheq.tta_cod_agenc_bcia
tt_integr_acr_cheq.tta_cod_cta_corren
tt_integr_acr_cheq.tta_num_cheque
tt_integr_acr_cheq.tta_dat_emis_cheq
tt_integr_acr_cheq.tta_dat_depos_cheq_acr
tt_integr_acr_cheq.tta_dat_prev_depos_cheq_acr
tt_integr_acr_cheq.tta_dat_desc_cheq_acr
tt_integr_acr_cheq.tta_dat_prev_desc_cheq_acr
tt_integr_acr_cheq.tta_val_cheque
tt_integr_acr_cheq.tta_nom_emit
tt_integr_acr_cheq.tta_nom_cidade_emit
tt_integr_acr_cheq.tta_cod_estab
tt_integr_acr_cheq.tta_cod_id_feder
tt_integr_acr_cheq.tta_cod_motiv_devol_cheq
caso seja cheque devolvido, informar o motivo
tt_integr_acr_cheq.tta_cod_finalid_econ_ext
para matriz de traduá∆o
tt_integr_acr_cheq.tta_cod_indic_econ
caso contr†rio, dever† ser informado
tt_integr_acr_cheq.tta_cod_usuar_cheq_acr_terc
tt_integr_acr_cheq.tta_log_pend_cheq_acr
quando for um cheque de terceiro, atributo recebe (yes)
tt_integr_acr_cheq.tta_log_cheq_terc
tt_integr_acr_cheq.tta_log_cheq_acr_renegoc
quando for um cheque devolvido, atributo recebe (yes)
tt_integr_acr_cheq.tta_log_cheq_acr_devolv.
tt_integr_acr_cheq.tta_num_pessoa.
tt_integr_acr_cheq.tta_cod_pais.
*/

/*tt_integr_acr_impto_impl_pend:
Atributo ttv_rec_item_lote_impl_tit_acr dever† receber o valor do atributo (tt_integr_acr_item_lote_impl_8.ttv_rec_item_lote_impl_tit_acr), para que haja relacionamento entre as duas temp-tables.
Segue abaixo os atributos que dever∆o receber valor:

tt_integr_acr_impto_impl_pend.tta_cod_pais 
tt_integr_acr_impto_impl_pend.tta_cod_pais_ext
para matriz de traduá∆o 
tt_integr_acr_impto_impl_pend.tta_cod_unid_federac
tt_integr_acr_impto_impl_pend.tta_cod_imposto
tt_integr_acr_impto_impl_pend.tta_cod_classif_impto 
tt_integr_acr_impto_impl_pend.tta_val_aliq_impto
*/

/*Tabela tt_integr_acr_item_lote_impl estar· apenas definida, por determinaÁ„o da API
*/


/*tt_integr_acr_item_lote_impl_8:
- O atributo ttv_rec_lote_impl_tit_acr deve receber o  recid da tt_integr_acr_lote_impl;
- O atributo ttv_rec_item_lote_impl_tit_acr dever† receber o seu pr¢prio recid, ou seja:
recid (tt_integr_item_lote_impl_5);
- O atributos chaves para o t°tulo devem ser informados:
tta_cod_estab; 
tta_cdn_cliente;
tta_num_seq_refer
tta_cod_espec_docto;
tta_cod_ser_docto;
tta_cod_tit_acr;
tta_cod_parcela.
	
- O atributo tta_cod_finalid_econ_ext somente ser† informado, quando a finalidade econìmica usada for de um sistema externo. 
Deve ser informado apenas um dos atributos (tta_cod_finalid_econ ou tta_cod_finalid_econ_ext).
- O atributo tta_cod_portador deve receber valor quando o portador fo do EMS 5.0;
- O atributo tta_cod_portad_ext somente ser† informado, quando o portador usado for de um sistema externo.
Deve ser informado apenas um dos atributos (tta_cod_portador ou tta_cod_portad_ext).
- O atributo tta_cod_cart_bcia deve receber o valor, quando a carteira banc†ria for a do EMS 5.0;
- O atributo tta_cod_modalid_ext somente ser† informado, quando a carteira banc†ria for de um sistema externo. Deve ser informado apenas um dos atributos (tta_cod_cart_bcia ou tta_cod_modalid_ext)
- atributo tta_val_cotac_indic_econ dever† ser informado quando se deseja que a convers∆o para a moeda corrente do usu†rio seja feita utilizando-se essa cotaá∆o e n∆o a cotaá∆o existente no Cadastro de Cotaá‰es. Caso deseje-se que o sistema utilize o valor da cotaá∆o existente no cadastro, deve-se informar 0 (zero) no valor da cotaá∆o - tta_val_cotac_indic_econ = 0.
- atributo tta_ind_tip_calc_juros n∆o Ç obrigat¢rio. Quando n∆o for informado assumir† por default "Simples".
- atributo ttv_cod_comprov_vda n∆o Ç obrigat¢rio. Quando informado, ser† gerado um lote de liquidaá∆o e comiss∆o para administradora do cart∆o de crÇdito, ou seja, Ç considerado que a venda est† autorizada pela administradora.
- atributo ttv_num_parc_cartcred n∆o Ç obrigat¢rio. Quando informado, ser∆o criados t°tulos de cobranáa especial conforme o n£mero de parcelas. Obs: estes t°tulos n∆o s∆o agrupados e o valor informado ser† dividido conforme n£mero de parcelas.
- atributo ttv_cod_autoriz_bco_emissor n∆o Ç obrigat¢rio. Quando informado ir† atualizar o c¢digo de autorizaá∆o do Banco Emissor da Venda Autorizada.
- atributo ttv_cod_lote_origin n∆o Ç obrigat¢rio. Quando informado ir† atualizar o c¢digo do Lote Original da Venda Autorizada.
- atributo ttv_log_vendor n∆o Ç obrigat¢rio. Utilizado para indicar se haver† atualizaá∆o das informaá‰es para o vendor atravÇs do documento.
- atributo ttv_cod_estab_vendor n∆o Ç obrigat¢rio. Quando for um documento vendor, e estiver implantando uma antecipaá∆o, deve ser preenchido com o estabelecimento da planilha.
- atributo ttv_num_planilha_vendor n∆o Ç obrigat¢rio. Quando for um documento vendor, e estiver implantando uma antecipaá∆o, deve ser preenchido com o n£mero da planilha.
- atributo ttv_cod_cond_pagto_vendor n∆o Ç obrigat¢rio. Quando for um documento vendor, e estiver implantando um documento normal, deve ser preenchido.
- atributo ttv_val_cotac_tax_vendor_clien n∆o Ç obrigat¢rio. Quando for um documento vendor, e estiver implantando um documento normal, deve ser preenchido, sendo seu valor inicial igual a zero.
- atributo ttv_dat_base_fechto_vendor n∆o Ç obrigat¢rio. Quando for um documento vendor, e estiver implantando um documento normal, deve ser preenchido.
- atributo ttv_qti_dias_carenc_fechto n∆o Ç obrigat¢rio. Quando for um documento vendor, e estiver implantando um documento normal, deve ser preenchido, sendo seu valor inicial igual a zero.
- atributo ttv_log_assume_tax_bco n∆o Ç obrigat¢rio. Quando for um documento vendor, e estiver implantando um documento normal, quando marcado, ignora valor informado para a taxa do cliente.
- atributo tta_val_base_calc_impto somente ser† informado quando os valores de PIS/COFINS/CSSL forem informados.
- atributo tta_log_retenc_impto_impl ser† informado para identificar se o titulo sofreu retená∆o na implantaá∆o
- atributo ttv_cod_nota_fisc_faturam ser† informado quando o t°tulo for de origem faturamento.

Segue abaixo outros atributos alÇm dos citados anteriormente que dever∆o receber valor na temp-table:

tt_integr_acr_item_lote_impl_8.ttv_rec_lote_impl_tit_acr 
tt_integr_acr_item_lote_impl_8.tta_num_seq_refer
tt_integr_acr_item_lote_impl_8.tta_cdn_cliente 
tt_integr_acr_item_lote_impl_8.tta_cod_espec_docto
tt_integr_acr_item_lote_impl_8.tta_cod_ser_docto
tt_integr_acr_item_lote_impl_8.tta_cod_tit_acr 
tt_integr_acr_item_lote_impl_8.tta_cod_parcela 
tt_integr_acr_item_lote_impl_8.tta_cod_indic_econ
tt_integr_acr_item_lote_impl_8.tta_cdn_repres
tt_integr_acr_item_lote_impl_8.tta_dat_vencto_tit_acr
tt_integr_acr_item_lote_impl_8.tta_dat_prev_liquidac
tt_integr_acr_item_lote_impl_8.tta_dat_emis_docto
tt_integr_acr_item_lote_impl_8.tta_val_tit_acr
tt_integr_acr_item_lote_impl_8.tta_val_liq_tit_acr
tt_integr_acr_item_lote_impl_8.tta_ind_tip_espec_docto 
*/



/*tt_integr_acr_lote_impl:
Segue abaixo os atributos que dever∆o receber valor na temp-table:

tt_integr_acr_lote_impl.tta_cod_empresa
tt_integr_acr_lote_impl.tta_cod_estab
tt_integr_acr_lote_impl.tta_cod_refer
tt_integr_acr_lote_impl.tta_dat_transacao
*/


/*tt_integr_acr_ped_vda_pend:
Segue abaixo os atributos que dever∆o receber valor:

tt_integr_acr_ped_vda_pend.ttv_rec_item_lote_impl_tit_acr
tt_integr_acr_ped_vda_pend.tta_cod_ped_vda
tt_integr_acr_ped_vda_pend.tta_cod_ped_vda_repres
tt_integr_acr_ped_vda_pend.tta_val_perc_particip_ped_vda

- O atributo ttv_rec_item_lote_impl_tit_acr deve receber o  recid da tt_integr_acr_item_lote_impl_8
*/


/*tt_integr_acr_relacto_pend:
Segue abaixo os atributos que dever∆o receber valor:

tt_integr_acr_relacto_pend.ttv_rec_item_lote_impl_tit_acr
tt_integr_acr_relacto_pend.ttv_cod_estab_tit_acr_pai
tt_integr_acr_relacto_pend.ttv_cod_estab_tit_acr_pai_ext
para matriz de traduá∆o
tt_integr_acr_relacto_pend.tta_num_id_tit_acr_pai 
tt_integr_acr_relacto_pend.tta_cod_espec_docto 
tt_integr_acr_relacto_pend.tta_cod_ser_docto      
tt_integr_acr_relacto_pend.tta_cod_tit_acr 
tt_integr_acr_relacto_pend.tta_cod_parcela  
tt_integr_acr_relacto_pend.tta_val_relacto_tit_acr   
tt_integr_acr_relacto_pend.tta_log_gera_alter_val 
tt_integr_acr_relacto_pend.tta_ind_motiv_acerto_val

- O atributo ttv_rec_item_lote_impl_tit_acr deve receber o  recid da tt_integr_acr_item_lote_impl_8
*/


/*tt_integr_acr_relacto_pend_cheq:
Segue abaixo os atributos que dever∆o receber valor:

tt_integr_acr_relacto_pend_cheq.ttv_rec_item_lote_impl_tit_acr
tt_integr_acr_relacto_pend_cheq.tta_cod_banco
tt_integr_acr_relacto_pend_cheq.tta_cod_agenc_bcia
tt_integr_acr_relacto_pend_cheq.tta_cod_cta_corren
tt_integr_acr_relacto_pend_cheq.tta_num_cheque

- O atributo ttv_rec_item_lote_impl_tit_acr deve receber o  recid da tt_integr_acr_item_lote_impl_8
*/

/*tt_integr_acr_repres_pend:
Atributo ttv_rec_item_lote_impl_tit_acr dever† receber o valor do atributo (tt_integr_acr_item_lote_impl_8.ttv_rec_item_lote_impl_tit_acr), para que haja relacionamento entre as duas temp-tables.
Segue abaixo os atributos que dever∆o receber valor:

tt_integr_acr_repres_pend.tta_cdn_repres
tt_integr_acr_repres_pend.tta_val_perc_comis_repres
tt_integr_acr_repres_pend.tta_val_perc_comis_repres_emis
tt_integr_acr_repres_pend.tta_val_perc_comis_abat
tt_integr_acr_repres_pend.tta_val_perc_comis_desc
tt_integr_acr_repres_pend.tta_val_perc_comis_juros
tt_integr_acr_repres_pend.tta_val_perc_comis_multa
tt_integr_acr_repres_pend.tta_val_perc_comis_acerto_val
tt_integr_acr_repres_pend.tta_ind_tip_comis

OBS:  Caso os campos:
tt_integr_acr_repres_pend.tta_val_perc_comis_abat  
tt_integr_acr_repres_pend.tta_val_perc_comis_desc
tt_integr_acr_repres_pend.tta_val_perc_comis_juros
tt_integr_acr_repres_pend.tta_val_perc_comis_multa
tt_integr_acr_repres_pend.tta_val_perc_comis_acerto_val
Sejam alimentados com o valor ? a API de implantaá∆o ira sobrepor o valore ? com o valor parametrizado no representante financeiro.
*/

/*Tabela tt_log_erros_atualiz estar· apenas definida, para utiilizaÁ„o da API*/
/*Tabela tt_params_generic_api estar· apenas definida, para utiilizaÁ„o da API*/


/*
Exemplo Criaá∆o temp-table tt_params_generic_api para campo SAFRA:
create tt_params_generic_api.
assign tt_params_generic_api.ttv_rec_id     = (Campo Recid da temp-table) tt_integr_acr_item_lote_impl_8.ttv_rec_item_lote_impl_tit_acr
       tt_params_generic_api.ttv_cod_campo  = 'Safra':U
       tt_params_generic_api.ttv_cod_tabela = ' tt_integr_acr_item_lote_impl':U
       tt_params_generic_api.ttv_cod_valor  = (Valor Da Safra).

Exemplo Criaá∆o temp-table tt_params_generic_api para campo Devolve Imposto Retido?:
    find b_tit_acr no-lock 
       where b_tit_acr.cod_estab       = tt_integr_acr_abat_antecip.tta_cod_estab    
         and b_tit_acr.cod_espec_docto = tt_integr_acr_abat_antecip.tta_cod_espec_doc
         and b_tit_acr.cod_ser_docto   = tt_integr_acr_abat_antecip.tta_cod_ser_docto
         and b_tit_acr.cod_tit_acr     = tt_integr_acr_abat_antecip.tta_cod_tit_acr  
         and b_tit_acr.cod_parcela     = tt_integr_acr_abat_antecip.tta_cod_parcela no-error.

create tt_params_generic_api. 
assign tt_params_generic_api.ttv_rec_id     = recid(b_tit_acr)
       tt_params_generic_api.ttv_cod_tabela = 'tt_integr_acr_abat_antecip':U
       tt_params_generic_api.ttv_cod_campo  = 'Devolve Imposto Retido?':U                              
       tt_params_generic_api.ttv_cod_valor  = string(yes).

create tt_params_generic_api. 
assign tt_params_generic_api.ttv_rec_id     = recid(b_tit_acr)
       tt_params_generic_api.ttv_cod_tabela = 'tt_integr_acr_abat_antecip':U
       tt_params_generic_api.ttv_cod_campo  = 'Valor Estornado':U                              
       tt_params_generic_api.ttv_cod_valor  = string(7.92).

create tt_params_generic_api. 
assign tt_params_generic_api.ttv_rec_id     = recid(b_tit_acr)
       tt_params_generic_api.ttv_cod_tabela = 'tt_integr_acr_abat_antecip':U
       tt_params_generic_api.ttv_cod_campo  = 'Gera Retená∆o Imposto Antecipaá∆o':U                              
       tt_params_generic_api.ttv_cod_valor  = string(yes).

create tt_params_generic_api. 
assign tt_params_generic_api.ttv_rec_id     = (Campo Recid da temp-table)
       tt_params_generic_api.ttv_cod_tabela = 'tt_integr_acr_item_lote_impl':U
       tt_params_generic_api.ttv_cod_campo  = '% Antecip Desconto':U                              
       tt_params_generic_api.ttv_cod_valor  = string(2.5).
*/

/*Tabela tt_integr_acr_relacto_pend_aux estar· apenas definida, para utiilizaÁ„o da API*/

/*
    View Geraá∆o de T°tulos: MV_TITULO_ACR
    - Dados T°tulo:
    
    Atributo	                Descricao	        Tipo	        Obr	Observaá∆o Dados T°tulo
    cod_empresa	                Empresa	            VARCHAR(03)	    Sim	 
    cod_estab	                Estabelecimento	    VARCHAR(03)	    Sim	 
    dat_transacao               Data transaá∆o	    DATE	        Sim	Data da contabilizaá∆o do movimento
    ind_orig_tit_acr	        Origem T°tulo	    VARCHAR(08)	    Sim	Fixo "SIAF"
    cdn_cliente	                Cliente	Number	                    Sim	 
    cod_espec_docto	            EspÇcie Documento	VARCHAR(03)	    Sim	Fixo "MC"
    cod_ser_docto	            SÇrie Documento	    VARCHAR(03)	    Sim	 
    cod_tit_acr	                T≠tulo	            VARCHAR(10	    Sim	 
    cod_parcela	                Parcela	            VARCHAR(02)	    Sim	 
    cod_indic_econ	            Moeda	            VARCHAR(08)	    Sim	 Fixo "MC"
    cod_portador	            Portador	        VARCHAR(05)	    N∆o	 
    cod_cart_bcia	            Carteira banc†ria	VARCHAR(03)	    N∆o	 
    cod_histor_padr	            Hist¢rico Padr∆o	VARCHAR(08)	    N∆o	 
    cdn_repres	                Representante	    Number	        Sim	Fixo "999"
    dat_vencto_tit_acr	        Vencimento	        DATE	        Sim	 
    dat_prev_liquidac	        Prev Liquidaá∆o	    DATE	        Sim	 
    dat_emis_docto	            Data Emiss∆o	    DATE	        Sim	 
    val_tit_acr	                Valor	            DATE	        Sim	 
    val_perc_juros_dia_atraso	Perc Jur Dia Atraso	Number	        N∆o	 
    val_perc_multa_atraso	    Perc Multa Atraso	Number	        N∆o	 
    des_text_histor	            Hist¢rico	        VARCHAR(2000)	N∆o	 
    val_liq_tit_acr	            Vl L°quido	        Number	        Sim	 
    ind_tip_espec_docto	        Tipo EspÇcie	    VARCHAR(17)	    Sim	Fixo "Normal" 
    
    - Dados Liquidaá∆o:
    
    log_liquidac_autom	        Liquidac Autom†tica	Number	Sim	0 - N∆o  1 - Sim    Se cheque enviar "1", se cart∆o enviar "0"
    cod_admdra_cartao_cr	    Administradora	VARCHAR(5)	Sim	Somente para cart‰es de crÇdito
    cod_cartcred	            C¢digo Cart∆o	VARCHAR(20)	Sim	 Somente para cart‰es de crÇdito
    cod_mes_ano_valid_cartao	Validade Cart∆o	VARCHAR(07)	Sim	Formato  XX/XXXX Somente para cart‰es de crÇdito
    cod_autoriz_cartao_cr	    C¢d PrÇ-Autorizaá∆o	VARCHAR(06)	Sim	Somente para cart‰es de crÇdito 
    dat_compra_cartao_cr	    Data Efetiva Venda	DATE	Sim	Somente para cart‰es de crÇdito 
    cod_banco	                Banco	VARCHAR(08)	Sim	 Somente para cheques
    cod_agenc_bcia	            Agància Banc†ria	VARCHAR(10)	Sim	Enviar conforme m†scara EMS Somente para cheques
    cod_cta_corren	            Conta Corrente	VARCHAR(10)	Sim	 Enviar conforme m†scara EMS Somente para cheques
    num_cheque	                Num Cheque	Number	Sim	 Somente para cheques
    dat_emis_cheq	            Data Emiss∆o	Date	Sim	 Somente para cheques
    dat_depos_cheq_acr	        Dep¢sito	Date	Sim	 Somente para cheques
    dat_prev_depos_cheq_acr	    Previs∆o Dep¢sito	Date	Sim	 Somente para cheques
    dat_desc_cheq_acr	        Data Desconto	Date	Sim	 Somente para cheques
    dat_prev_desc_cheq_acr	    Data Prev Desc	Date	Sim	 Somente para cheques
    val_cheque	                Valor Cheque	Number	Sim	 Somente para cheques
    nom_emit	                Nome Emitente	VARCHAR(40)	Sim	Somente no caso de cheque de terceiros
    nom_cidad_emit	            Cidade Emitente	VARCHAR(30)	Sim	Somente no caso de cheque de terceiros
    cod_estab	                Estabelecimento	VARCHAR(03)	Sim	Somente no caso de cheque de terceiros
    cod_id_feder	            ID Federal (CPF/CNPJ)	VARCHAR(20)	Sim	Somente no caso de cheque de terceiros
    cod_indic_econ	            Moeda	VARCHAR(08)	Sim 	Fixo "Real"
    cod_usuar_cheq_acr_terc	    Usu†rio	VARCHAR(12)	Sim	Usu†rio do EMS que tem permissao de implantaá∆o de cheques
    log_cheq_terc	            Cheque Terceiro	Number	Sim	0 - N∆o 1 - Sim
    log_cheq_acr_renegoc	    Cheque Reneg	Number	Sim	0 - N∆o 1 - Sim
    num_pessoa	                Pessoa	Number	Sim	Somente no caso de cheque de terceiros
    cod_pais	                Pa°s	VARCHAR(03)	Sim	Somente no caso de cheque de terceiros
    cod_comprov_vda	            Comprovante Venda	VARCHAR(12)	N∆o	Usar apenas no caso de cart∆o de crÇdito
    num_parc_cartcred	        Quantidade Parcelas	Number	N∆o	Usar apenas no caso de cart∆o de crÇdito
    cod_autoriz_bco_emissor	    Codigo Autorizaá∆o	VARCHAR(03)	N∆o	Usar apenas no caso de cart∆o de crÇdito
    cod_lote_origin	            Lote Orig Venda	VARCHAR(03)	N∆o	Usar apenas no caso de cart∆o de crÇdito
    
    View Apropriaá∆o Cont†bil: 
    
    cod_estab	            Estabelecimento	    VARCHAR(03)	Sim	 Mesmo dado enviado na view do t°tulo
    cod_espec_docto	        EspÇcie Documento	VARCHAR(03)	Sim	 Mesmo dado enviado na view do t°tulo
    cod_ser_docto	        SÇrie Documento	    VARCHAR(03)	Sim	 Mesmo dado enviado na view do t°tulo
    cod_tit_acr	            T≠tulo	            VARCHAR(10	Sim	 Mesmo dado enviado na view do t°tulo
    cod_parcela	            Parcela	            VARCHAR(02)	Sim	 Mesmo dado enviado na view do t°tulo
    cod_plano_cta_ctbl	    Plano Contas        VARCHAR(08)	Sim	
    cod_cta_ctbl	        Conta Cont bil      VARCHAR(20)	Sim	
    cod_unid_negoc	        Unid Neg¢cio        VARCHAR(03)	Sim	
    cod_plano_ccusto	    Plano Centros Custo	VARCHAR(08)	N∆o	
    cod_ccusto	            Centro Custo	    VARCHAR(11)	N∆o	
    cod_tip_fluxo_financ	Tipo Fluxo Financ	VARCHAR(12)	Sim	
    val_aprop_ctbl	        Valor Aprop Ctbl	Number	    Sim	
*/
