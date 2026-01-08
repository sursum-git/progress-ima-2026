/*****************************************************************************
**       Programa: ESP0025RP-a.p
**       Data....: 23/03/2011
**       Autor...: DATASUL S.A.
**       Objetivo: Integraá∆o de Clientes
**       Vers∆o..: 2.06.001 - Bruno
**       Modificaá∆o: Janeiro/2014 - Luci Aguiar
**                    - Refeito para trabalhar com appserver
*******************************************************************************/
DEFINE TEMP-TABLE tt-empresas NO-UNDO
    FIELD cod_empresa AS CHARACTER
    INDEX ix AS UNIQUE cod_empresa.

DEF TEMP-TABLE tt-siaf-cliente NO-UNDO LIKE ESP_LOG_INTEGRACAO
    FIELD cod_empresa LIKE ems5.cliente.cod_empresa. 

DEF INPUT  PARAM TABLE FOR tt-empresas.
DEF INPUT  PARAM TABLE FOR tt-siaf-cliente. 

DEFINE BUFFER bf-tt-siaf-cliente    FOR tt-siaf-cliente.  
DEFINE BUFFER bf-esp_log_integracao FOR esp_log_integracao.

DEF VAR c-log-integracao AS CHAR.
DEF VAR l-integrado           AS LOGICAL INITIAL NO.

/* Definicoes locais */
/*
{include/buffers.i}
*/
{utp/ut-glob.i}
{include/i-prgvrs.i}
{esp-aux/cdapi366b.i}
{esp-aux/espapi005.i}

/*Tabelas da API*/

{esp-aux/utb765zl.i}

DEF TEMP-TABLE tt-integra-cliente NO-UNDO
         FIELD COD_CHAVE_INTEGRACAO LIKE tt-siaf-cliente.COD_CHAVE_INTEGRACAO
         FIELD cod_empresa          LIKE EMS5.cliente.cod_empresa
         FIELD cdn_cliente          AS INTEGER FORMAT '>>>,>>>,>>9' initial 0 label 'Cliente' column-label 'Cliente'
         FIELD NOM_PESSOA           LIKE MV_CLIENTE_INTEGRACAO.NOM_PESSOA
         FIELD COD_ID_FEDER         LIKE MV_CLIENTE_INTEGRACAO.COD_ID_FEDER.

DEFINE TEMP-TABLE tt-integra-ems2
    FIELD cod_empresa           AS CHARACTER FORMAT "X(10)"
    FIELD cdn_cliente           AS INTEGER   FORMAT "999999999"
    FIELD ind_retem_iss         AS CHARACTER FORMAT "X" 
    FIELD nom_pessoa            AS CHARACTER FORMAT "X(40)"
    FIELD rw-esp-log-integracao AS ROWID
    FIELD log-integrado         AS LOGICAL INITIAL NO
    INDEX ix cod_empresa cdn_cliente.

DEFINE TEMP-TABLE tt-erros-integra-ems2
    FIELD rw-tt-integra AS ROWID
    FIELD erro-num      AS CHAR
    FIELD erro-desc     AS CHAR FORMAT "X(60)"
    FIELD erro-help     AS CHAR FORMAT "X(2000)"
    FIELD tipo-erro     AS INTEGER. /*1-especifico - 2: API EMS*/

/* Temp-table de erros da API de login no EMS2
   --------------------------------------------*/
Define Temp-table tt-erros-login
    Field cod-erro  As Integer
    Field desc-erro As Character Format "x(256)":U
    Field desc-arq  As Character.

DEFINE TEMP-TABLE tt_erro no-undo
    field ttv_num_cod_erro                 as integer format ">>>>,>>9" label "N£mero"
    field ttv_cod_desc_erro                as character format "x(8)".

DEFINE VARIABLE v_hdl_utb765zl AS HANDLE     NO-UNDO.

/****************** INCLUDE COM VARIÊVEIS GLOBAIS *********************/
def new global shared var c-seg-usuario as char format "x(12)" no-undo.
def new global shared var v_cod_usuar_corren as character format "x(12)" label "Usu†rio Corrente" column-label "Usu†rio Corrente" no-undo.
def new global shared var v_cod_empres_usuar  as character format "x(3)"  label "Empresa" column-label "Empresa" no-undo. 


/***************** Defini?ao de Vari†veis de Processamento do RelatΩrio *********************/
DEF VAR h-api-cliente        as handle no-undo.
def var h-acomp1             as handle no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-cont-registro      as int    no-undo.
DEF VAR i-cont               AS INT.
DEF VAR l-existe             AS LOG.
DEF VAR c-cod_empresa  LIKE EMS5.cliente.cod_empresa.
DEF VAR i-cdn_cliente  LIKE EMS5.cliente.cdn_cliente.

DEF VAR c-arq-valid-ems2 AS CHAR.
DEF VAR c-linha AS CHAR.
DEF VAR l-ajuda AS LOG.

DEF VAR c-erro-num  AS CHAR.
DEF VAR c-erro-desc AS CHAR.
DEF VAR c-erro-help AS CHAR.

DEF VAR c_emp_usuar_univ AS CHAR.

DEF TEMP-TABLE tt-erro-ems2 NO-UNDO
    FIELD erro-num  AS CHAR
    FIELD erro-desc AS CHAR FORMAT "X(60)"
    FIELD erro-help AS CHAR FORMAT "X(2000)"
    INDEX id IS PRIMARY UNIQUE erro-num.

DEFINE VARIABLE h-api               AS HANDLE                     NO-UNDO.
def var i-seq-contato    as int    no-undo.
DEFINE VARIABLE i-ep-codigo-empresa AS INTEGER   INITIAL 0        NO-UNDO.
DEFINE VARIABLE c-arquivo-saida     AS CHARACTER INITIAL ''       NO-UNDO.
DEF VAR lg-OK AS LOG.

DEF VAR V_DAT_IMPL_CLIEN   AS DATE.
DEF VAR V_NOM_ENDERECO     AS CHAR.
DEF VAR V_NOM_ENDER_COMPL  AS CHAR.
DEF VAR V_NOM_BAIRRO       AS CHAR.
DEF VAR V_NOM_CIDADE       AS CHAR.
DEF VAR V_COD_UNID_FEDERAC AS CHAR.
DEF VAR V_COD_CEP          AS CHAR.
DEF VAR V_COD_TELEFONE     AS CHAR.
DEF VAR V_COD_RAMAL        AS CHAR.
DEF VAR V_COD_FAX          AS CHAR.
DEF VAR V_COD_RAMAL_FAX    AS CHAR.
DEF VAR V_COD_TELEX        AS CHAR.

DEF VAR V_NOM_ENDER_COBR        AS CHAR.
DEF VAR V_NOM_ENDER_COMPL_COBR  AS CHAR.
DEF VAR V_NOM_BAIRRO_COBR       AS CHAR.
DEF VAR V_NOM_CIDAD_COBR        AS CHAR.
DEF VAR V_COD_UNID_FEDERAC_COBR AS CHAR.
DEF VAR V_COD_PAIS_NASC         AS CHAR.
DEF VAR V_COD_PAIS_COBR         AS CHAR.
DEF VAR V_COD_CEP_COBR          AS CHAR.
DEF VAR V_COD_CX_POST_COBR      AS CHAR.
DEF VAR V_cod_id_estad_fisic AS CHAR.
DEF VAR V_COD_ORGAO_EMIS_ID_ESTAD     AS CHAR.
DEF VAR V_COD_UNID_FEDERAC_EMIS_ESTAD AS CHAR.

DEF VAR V_COD_E_MAIL      AS CHAR.
DEF VAR v_id_federal      AS CHAR FORMAT "X(20)".

DEFINE VARIABLE l-erro-appserver AS LOGICAL     NO-UNDO.

DEFINE VARIABLE h-server        AS HANDLE      NO-UNDO.

DEFINE VARIABLE c-nom-abrev AS CHARACTER FORMAT "X(12)"  NO-UNDO.

run utp/ut-acomp.p persistent set h-acomp1.
run pi-inicializar in h-acomp1(input "Acompanhamento Clientes").
run pi-desabilita-cancela in h-acomp1.

RUN pi-principal.

IF VALID-HANDLE(v_hdl_utb765zl) THEN
   DELETE PROCEDURE v_hdl_utb765zl NO-ERROR.

IF VALID-HANDLE(h-acomp1) THEN
   RUN pi-finalizar IN h-acomp1 NO-ERROR.


/************* Procedur3es *******************/
PROCEDURE pi-principal.
    FIND FIRST ESP_PARAMETRO WHERE
               ESP_PARAMETRO.COD_PARAMETRO = "NOM_APPSERVER"
               NO-LOCK.

    ASSIGN i-cont = 0.

    EMPTY TEMP-TABLE tt-integra-cliente.
    FOR EACH tt-empresas:
        RUN pi-zera-tables.
        
        EMPTY TEMP-TABLE tt-integra-ems2.
        RUN pi-chama-conexao-appserver.

        IF l-erro-appserver OR NOT VALID-HANDLE(h-server) THEN NEXT.
        
        FOR EACH tt-siaf-cliente WHERE 
                 tt-siaf-cliente.cod_empresa = tt-empresas.cod_empresa:
            run pi-acompanhar in h-acomp1(input "Integraá∆o: " + tt-siaf-cliente.cod_log_integracao).
            
            FIND FIRST esp_log_integracao NO-LOCK
                 WHERE esp_log_integracao.cod_log_integracao = tt-siaf-cliente.cod_log_integracao NO-ERROR.
    
            ASSIGN lg-OK = YES.
            FIND FIRST MV_CLIENTE_INTEGRACAO NO-LOCK
                 WHERE MV_CLIENTE_INTEGRACAO.COD_EMPRESA = ENTRY(1,tt-siaf-cliente.COD_CHAVE_INTEGRACAO,"|")
                   AND MV_CLIENTE_INTEGRACAO.CDN_CLIENTE = INT(ENTRY(2,tt-siaf-cliente.COD_CHAVE_INTEGRACAO,"|")) NO-ERROR.
            IF AVAIL MV_CLIENTE_INTEGRACAO THEN DO:
                
                RUN pi-valida-info-clientes.
    
                IF lg-OK THEN 
                    RUN pi-gerar.
            END.
            ELSE DO:
                 RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                          INPUT "EMS5",
                                          INPUT 1,                                                           /*Registro n∆o existe na tabela de Integraá∆o de cliente*/
                                          INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                          INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                          INPUT "",                                                          /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                          INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
            END.
        END.

        RUN pi-grava.

        IF CAN-FIND(FIRST tt-integra-ems2) THEN DO:
            RUN pi-integra-ems2.
        END.
        IF VALID-HANDLE(h-server) THEN
           RUN pi_desconecta_appserver.
    END. 
END PROCEDURE.  



PROCEDURE pi-gerar.
    run pi-acompanhar in h-acomp1(input "Integraá∆o: " + tt-siaf-cliente.cod_log_integracao + " - PI-GERAR").

    FIND FIRST EMS5.cliente WHERE 
               EMS5.cliente.cod_id_feder = MV_CLIENTE_INTEGRACAO.cod_id_feder AND 
               EMS5.cliente.cod_pais     = MV_CLIENTE_INTEGRACAO.cod_pais_end no-lock no-error.
    IF AVAIL cliente THEN
        ASSIGN c-nom-abrev = cliente.nom_abrev.
    ELSE ASSIGN c-nom-abrev = MV_CLIENTE_INTEGRACAO.NOM_ABREV.

    CREATE tt-integra-cliente.
    ASSIGN tt-integra-cliente.COD_CHAVE_INTEGRACAO = tt-siaf-cliente.cod_chave_integracao
           tt-integra-cliente.cod_empresa          = MV_CLIENTE_INTEGRACAO.COD_EMPRESA
           tt-integra-cliente.cdn_cliente          = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE
           tt-integra-cliente.NOM_PESSOA           = MV_CLIENTE_INTEGRACAO.NOM_PESSOA
           tt-integra-cliente.COD_ID_FEDER         = MV_CLIENTE_INTEGRACAO.COD_ID_FEDER.

    if MV_CLIENTE_INTEGRACAO.ind_tip_pessoa = "Fisica" then DO:
        RUN pi-cria-tt_pessoa_fisic_integr_e.
    END.
    ELSE DO:
        RUN pi-cria-tt_pessoa_jurid_integr_j.
        RUN pi-cria-tt_contato_integr_e.
    END.

    RUN pi-cria-tt_cliente_integr_j.
    RUN pi-cria-tt_clien_financ_integr_e.

    if MV_CLIENTE_INTEGRACAO.ind_gera_fornec = "S" THEN do:
       RUN pi-cria-tt_fornecedor_integr_k.
       RUN pi-cria-tt_fornec_financ_integr_e.
    END.

    assign i-cont = i-cont + 1.
END PROCEDURE. 


PROCEDURE pi-zera-tables.
    EMPTY TEMP-TABLE tt_cliente_integr_j.
    EMPTY TEMP-TABLE tt_fornecedor_integr_k.
    EMPTY TEMP-TABLE tt_clien_financ_integr_e.
    EMPTY TEMP-TABLE tt_fornec_financ_integr_e.
    EMPTY TEMP-TABLE tt_pessoa_jurid_integr_j.
    EMPTY TEMP-TABLE tt_pessoa_fisic_integr_e.
    EMPTY TEMP-TABLE tt_contato_integr_e.
    EMPTY TEMP-TABLE tt_contat_clas_integr.
    EMPTY TEMP-TABLE tt_estrut_clien_integr.
    EMPTY TEMP-TABLE tt_estrut_fornec_integr.
    EMPTY TEMP-TABLE tt_histor_clien_integr.
    EMPTY TEMP-TABLE tt_histor_fornec_integr.
    EMPTY TEMP-TABLE tt_ender_entreg_integr_e.
    EMPTY TEMP-TABLE tt_telef_integr.
    EMPTY TEMP-TABLE tt_telef_pessoa_integr.
    EMPTY TEMP-TABLE tt_pj_ativid_integr_i.
    EMPTY TEMP-TABLE tt_pj_ramo_negoc_integr_j.
    EMPTY TEMP-TABLE tt_porte_pj_integr.
    EMPTY TEMP-TABLE tt_idiom_pf_integr.
    EMPTY TEMP-TABLE tt_idiom_contat_integr.
    EMPTY TEMP-TABLE tt_retorno_clien_fornec.
    EMPTY TEMP-TABLE tt_clien_analis_cr_integr.
    EMPTY TEMP-TABLE tt_cta_corren_fornec.
END PROCEDURE. /*pi-zera-tables*/


PROCEDURE pi-cria-tt_pessoa_fisic_integr_e.
    find first tt_pessoa_fisic_integr_e no-lock
         where tt_pessoa_fisic_integr_e.tta_cod_id_feder = MV_CLIENTE_INTEGRACAO.cod_id_feder
         and   tt_pessoa_fisic_integr_e.tta_cod_pais     = MV_CLIENTE_INTEGRACAO.cod_pais_end no-error.
    if not avail tt_pessoa_fisic_integr_e then do:

        CREATE tt_pessoa_fisic_integr_e.

        FIND FIRST EMS5.cliente NO-LOCK
             WHERE EMS5.cliente.cod_id_feder = MV_CLIENTE_INTEGRACAO.cod_id_feder
               AND EMS5.cliente.cod_pais     = MV_CLIENTE_INTEGRACAO.cod_pais_end no-error.
        IF AVAIL EMS5.cliente THEN DO:
            FIND FIRST pessoa_fisic
                 WHERE pessoa_fisic.num_pessoa_fisic = EMS5.cliente.num_pessoa NO-LOCK NO-ERROR.
            ASSIGN tt_pessoa_fisic_integr_e.tta_num_pessoa_fisic            = pessoa_fisic.num_pessoa_fisic
                   tt_pessoa_fisic_integr_e.ttv_num_tip_operac              = 1 /* Incluir / Modificar */
                   tt_pessoa_fisic_integr_e.tta_nom_pessoa                  = MV_CLIENTE_INTEGRACAO.nom_pessoa
                   tt_pessoa_fisic_integr_e.tta_cod_id_feder                = EMS5.cliente.cod_id_feder
                   /*tt_pessoa_fisic_integr_e.tta_cod_id_estad_fisic          = IF pessoa_fisic.cod_id_estad_fisic = ? OR 
                                                                                 pessoa_fisic.cod_id_estad_fisic = "" OR 
                                                                                 pessoa_fisic.cod_id_estad_fisic = "0" THEN "Isento" 
                                                                              ELSE replace(replace(replace(ems506.pessoa_fisic.cod_id_estad_fisic,".",""),"/",""),"-","")
                   tt_pessoa_fisic_integr_e.tta_cod_orgao_emis_id_estad     = V_COD_ORGAO_EMIS_ID_ESTAD
                   tt_pessoa_fisic_integr_e.tta_cod_unid_federac_emis_estad = V_COD_UNID_FEDERAC_EMIS_ESTAD */
                   tt_pessoa_fisic_integr_e.tta_cod_pais                    = MV_CLIENTE_INTEGRACAO.COD_PAIS_END /*"BRA"*/
                   tt_pessoa_fisic_integr_e.tta_nom_endereco                = V_NOM_ENDERECO     
                   tt_pessoa_fisic_integr_e.tta_nom_ender_compl             = V_NOM_ENDER_COMPL 
                   tt_pessoa_fisic_integr_e.tta_nom_bairro                  = V_NOM_BAIRRO       
                   tt_pessoa_fisic_integr_e.tta_nom_cidade                  = V_NOM_CIDADE       
                   tt_pessoa_fisic_integr_e.tta_cod_unid_federac            = V_COD_UNID_FEDERAC 
                   tt_pessoa_fisic_integr_e.tta_cod_cep                     = V_COD_CEP          
                   tt_pessoa_fisic_integr_e.tta_cod_telefone                = V_COD_TELEFONE     
                   tt_pessoa_fisic_integr_e.tta_cod_ramal                   = V_COD_RAMAL        
                   tt_pessoa_fisic_integr_e.tta_cod_fax                     = V_COD_FAX          
                   tt_pessoa_fisic_integr_e.tta_cod_ramal_fax               = V_COD_RAMAL_FAX    
                   tt_pessoa_fisic_integr_e.tta_cod_telex                   = V_COD_TELEX        
                   tt_pessoa_fisic_integr_e.tta_cod_e_mail                  = V_COD_E_MAIL
                   tt_pessoa_fisic_integr_e.tta_dat_nasc_pessoa_fisic       = MV_CLIENTE_INTEGRACAO.dat_nasc_pessoa_fisic
                   tt_pessoa_fisic_integr_e.ttv_cod_pais_nasc               = V_COD_PAIS_NASC
                   tt_pessoa_fisic_integr_e.tta_cod_unid_federac_nasc       = MV_CLIENTE_INTEGRACAO.cod_unid_federac_nasc
                   tt_pessoa_fisic_integr_e.tta_nom_mae_pessoa              = MV_CLIENTE_INTEGRACAO.nom_mae_pessoa
                   tt_pessoa_fisic_integr_e.ttv_log_altera_razao_social     = YES
                   tt_pessoa_fisic_integr_e.tta_nom_nacion_pessoa_fisic     = MV_CLIENTE_INTEGRACAO.nom_nacion_pessoa_fisic
                   tt_pessoa_fisic_integr_e.tta_nom_profis_pessoa_fisic     = MV_CLIENTE_INTEGRACAO.nom_profis_pessoa_fisic
                   tt_pessoa_fisic_integr_e.tta_ind_estado_civil_pessoa     = MV_CLIENTE_INTEGRACAO.ind_estado_civil_pessoa
                   tt_pessoa_fisic_integr_e.tta_nom_home_page               = ""
                   tt_pessoa_fisic_integr_e.tta_nom_ender_cobr              = V_NOM_ENDER_COBR 
                   tt_pessoa_fisic_integr_e.tta_nom_ender_compl_cobr        = V_NOM_ENDER_COMPL_COBR 
                   tt_pessoa_fisic_integr_e.tta_nom_bairro_cobr             = V_NOM_BAIRRO_COBR      
                   tt_pessoa_fisic_integr_e.tta_nom_cidad_cobr              = V_NOM_CIDAD_COBR       
                   tt_pessoa_fisic_integr_e.tta_cod_unid_federac_cobr       = V_COD_UNID_FEDERAC_COBR
                   tt_pessoa_fisic_integr_e.ttv_cod_pais_cobr               = V_COD_PAIS_COBR        
                   tt_pessoa_fisic_integr_e.tta_cod_cep_cobr                = V_COD_CEP_COBR         
                   tt_pessoa_fisic_integr_e.tta_cod_cx_post_cobr            = V_COD_CX_POST_COBR     
                   tt_pessoa_fisic_integr_e.tta_log_ems_20_atlzdo           = YES /*ATUALIZA EMS 2?*/
                   tt_pessoa_fisic_integr_e.tta_cod_id_munic_fisic          = "".

            ASSIGN SUBSTRING(tt_pessoa_fisic_integr_e.tta_nom_ender_text,1991,09) = string(EMS5.cliente.CDN_CLIENTE). 

        END.
        ELSE DO:
            ASSIGN v_id_federal = REPLACE(MV_CLIENTE_INTEGRACAO.cod_id_feder,".","")
                   v_id_federal = REPLACE(v_id_federal,"-","")
                   v_id_federal = REPLACE(v_id_federal,"/","").

            ASSIGN tt_pessoa_fisic_integr_e.tta_num_pessoa_fisic            = 0
                   tt_pessoa_fisic_integr_e.ttv_num_tip_operac              = 1 /* Inlcuir / Modificar */
                   tt_pessoa_fisic_integr_e.tta_nom_pessoa                  = MV_CLIENTE_INTEGRACAO.nom_pessoa
                   tt_pessoa_fisic_integr_e.tta_cod_id_feder                = v_id_federal               
                   /*tt_pessoa_fisic_integr_e.tta_cod_id_estad_fisic          = V_cod_id_estad_fisic
                   tt_pessoa_fisic_integr_e.tta_cod_orgao_emis_id_estad     = V_COD_ORGAO_EMIS_ID_ESTAD
                   tt_pessoa_fisic_integr_e.tta_cod_unid_federac_emis_estad = V_COD_UNID_FEDERAC_EMIS_ESTAD*/
                   tt_pessoa_fisic_integr_e.tta_cod_pais                    = MV_CLIENTE_INTEGRACAO.COD_PAIS_END
                   tt_pessoa_fisic_integr_e.tta_nom_endereco                = V_NOM_ENDERECO  
                   tt_pessoa_fisic_integr_e.tta_nom_ender_compl             = V_NOM_ENDER_compl
                   tt_pessoa_fisic_integr_e.tta_nom_bairro                  = V_NOM_BAIRRO      
                   tt_pessoa_fisic_integr_e.tta_nom_cidade                  = V_NOM_CIDADE      
                   tt_pessoa_fisic_integr_e.tta_cod_unid_federac            = V_COD_UNID_FEDERAC
                   tt_pessoa_fisic_integr_e.tta_cod_cep                     = V_COD_CEP         
                   tt_pessoa_fisic_integr_e.tta_cod_telefone                = V_COD_TELEFONE    
                   tt_pessoa_fisic_integr_e.tta_cod_ramal                   = V_COD_RAMAL       
                   tt_pessoa_fisic_integr_e.tta_cod_fax                     = V_COD_FAX         
                   tt_pessoa_fisic_integr_e.tta_cod_ramal_fax               = V_COD_RAMAL_FAX   
                   tt_pessoa_fisic_integr_e.tta_cod_telex                   = V_COD_TELEX
                   tt_pessoa_fisic_integr_e.tta_cod_e_mail                  = V_COD_E_MAIL
                   tt_pessoa_fisic_integr_e.tta_dat_nasc_pessoa_fisic       = MV_CLIENTE_INTEGRACAO.dat_nasc_pessoa_fisic
                   tt_pessoa_fisic_integr_e.ttv_cod_pais_nasc               = V_COD_PAIS_NASC
                   tt_pessoa_fisic_integr_e.tta_cod_unid_federac_nasc       = MV_CLIENTE_INTEGRACAO.cod_unid_federac_nasc
                   tt_pessoa_fisic_integr_e.tta_nom_mae_pessoa              = MV_CLIENTE_INTEGRACAO.nom_mae_pessoa
                   tt_pessoa_fisic_integr_e.ttv_log_altera_razao_social     = YES
                   tt_pessoa_fisic_integr_e.tta_nom_nacion_pessoa_fisic     = MV_CLIENTE_INTEGRACAO.nom_nacion_pessoa_fisic
                   tt_pessoa_fisic_integr_e.tta_nom_profis_pessoa_fisic     = MV_CLIENTE_INTEGRACAO.nom_profis_pessoa_fisic
                   tt_pessoa_fisic_integr_e.tta_ind_estado_civil_pessoa     = MV_CLIENTE_INTEGRACAO.ind_estado_civil_pessoa
                   tt_pessoa_fisic_integr_e.tta_nom_home_page               = ""
                   tt_pessoa_fisic_integr_e.tta_nom_ender_cobr              = V_NOM_ENDER_COBR  
                   tt_pessoa_fisic_integr_e.tta_nom_ender_compl_cobr        = V_NOM_ENDER_COMPL_COBR 
                   tt_pessoa_fisic_integr_e.tta_nom_bairro_cobr             = V_NOM_BAIRRO_COBR      
                   tt_pessoa_fisic_integr_e.tta_nom_cidad_cobr              = V_NOM_CIDAD_COBR       
                   tt_pessoa_fisic_integr_e.tta_cod_unid_federac_cobr       = V_COD_UNID_FEDERAC_COBR
                   tt_pessoa_fisic_integr_e.ttv_cod_pais_cobr               = V_COD_PAIS_COBR        
                   tt_pessoa_fisic_integr_e.tta_cod_cep_cobr                = V_COD_CEP_COBR         
                   tt_pessoa_fisic_integr_e.tta_cod_cx_post_cobr            = V_COD_CX_POST_COBR     
                   tt_pessoa_fisic_integr_e.tta_log_ems_20_atlzdo           = YES /*ATUALIZA EMS 2?*/
                   tt_pessoa_fisic_integr_e.tta_cod_id_munic_fisic          = "".
        END.
        /*--- N∆o retirar o c¢digo abaixo ---*/
        release tt_pessoa_fisic_integr_e.
        find first tt_pessoa_fisic_integr_e no-lock
            where tt_pessoa_fisic_integr_e.tta_cod_id_feder = MV_CLIENTE_INTEGRACAO.cod_id_feder 
            and   tt_pessoa_fisic_integr_e.tta_cod_pais     = MV_CLIENTE_INTEGRACAO.cod_pais_end no-error.
    end.
END PROCEDURE. /*pi-cria-tt_pessoa_fisic_integr_e*/


PROCEDURE pi-cria-tt_pessoa_jurid_integr_j.
    ASSIGN v_id_federal = REPLACE(MV_CLIENTE_INTEGRACAO.cod_id_feder,".","")
           v_id_federal = REPLACE(v_id_federal,"-","")
           v_id_federal = REPLACE(v_id_federal,"/","").

    if MV_CLIENTE_INTEGRACAO.cod_id_feder <> "" then
       find first tt_pessoa_jurid_integr_j no-lock
            where tt_pessoa_jurid_integr_j.tta_cod_id_feder = v_id_federal
            and   tt_pessoa_jurid_integr_j.tta_cod_pais     = MV_CLIENTE_INTEGRACAO.cod_pais_end no-error.
    else
       find first tt_pessoa_jurid_integr_j no-lock
            where tt_pessoa_jurid_integr_j.tta_cod_id_feder = v_id_federal
            and   tt_pessoa_jurid_integr_j.tta_cod_pais     = MV_CLIENTE_INTEGRACAO.cod_pais_end
            and   tt_pessoa_jurid_integr_j.tta_nom_pessoa   = MV_CLIENTE_INTEGRACAO.nom_pessoa no-error. 
                                                                                                         
    if not avail tt_pessoa_jurid_integr_j THEN
       find first tt_pessoa_jurid_integr_j no-lock
            where tt_pessoa_jurid_integr_j.tta_num_pessoa_jurid = 0 
            AND   tt_pessoa_jurid_integr_j.tta_cod_id_feder     = v_id_federal
            and   tt_pessoa_jurid_integr_j.tta_cod_pais_ext     = V_COD_PAIS_NASC no-error. 
        
   if not avail tt_pessoa_jurid_integr_j then do:         
      CREATE tt_pessoa_jurid_integr_j.                                                                 
      FIND FIRST EMS5.cliente NO-LOCK                                                                
           WHERE cliente.cod_id_feder = v_id_federal                      
               AND cliente.cod_pais     = MV_CLIENTE_INTEGRACAO.cod_pais_end no-error.                                         
      IF AVAIL cliente THEN DO:                                                                 
                                                                                                         
            FIND FIRST ems5.pessoa_jurid
                 WHERE ems5.pessoa_jurid.num_pessoa_jurid = EMS5.cliente.num_pessoa NO-LOCK NO-ERROR.

            ASSIGN tt_pessoa_jurid_integr_j.tta_num_pessoa_jurid           = ems5.pessoa_jurid.num_pessoa_jurid
                   tt_pessoa_jurid_integr_j.tta_nom_pessoa                 = MV_CLIENTE_INTEGRACAO.nom_pessoa  
                   tt_pessoa_jurid_integr_j.tta_cod_id_feder               = EMS5.cliente.cod_id_feder

                   tt_pessoa_jurid_integr_j.tta_cod_id_estad_jurid         = IF MV_CLIENTE_INTEGRACAO.NUM_INSCRICAO_ESTADUAL = ? OR
                                                                                MV_CLIENTE_INTEGRACAO.NUM_INSCRICAO_ESTADUAL = "" OR
                                                                                MV_CLIENTE_INTEGRACAO.NUM_INSCRICAO_ESTADUAL = "?" THEN "Isento" ELSE MV_CLIENTE_INTEGRACAO.NUM_INSCRICAO_ESTADUAL
                   tt_pessoa_jurid_integr_j.tta_cod_id_munic_jurid         = IF MV_CLIENTE_INTEGRACAO.NUM_INSCRICAO_MUNICIPAL = ? OR
                                                                                MV_CLIENTE_INTEGRACAO.NUM_INSCRICAO_MUNICIPAL = "" OR
                                                                                MV_CLIENTE_INTEGRACAO.NUM_INSCRICAO_MUNICIPAL = "?" THEN "Isento" ELSE MV_CLIENTE_INTEGRACAO.NUM_INSCRICAO_MUNICIPAL

                   tt_pessoa_jurid_integr_j.tta_cod_pais                   = MV_CLIENTE_INTEGRACAO.COD_PAIS_END /*"BRA"*/
                   tt_pessoa_jurid_integr_j.tta_nom_endereco               = V_NOM_ENDERECO 
                   tt_pessoa_jurid_integr_j.tta_nom_ender_compl            = V_NOM_ENDER_COMPL
                   tt_pessoa_jurid_integr_j.tta_nom_bairro                 = V_NOM_BAIRRO        
                   tt_pessoa_jurid_integr_j.tta_nom_cidade                 = V_NOM_CIDADE        
                   tt_pessoa_jurid_integr_j.tta_cod_unid_federac           = V_COD_UNID_FEDERAC  
                   tt_pessoa_jurid_integr_j.tta_cod_cep                    = V_COD_CEP           
                   tt_pessoa_jurid_integr_j.tta_cod_telefone               = V_COD_TELEFONE      
                   tt_pessoa_jurid_integr_j.tta_cod_fax                    = V_COD_FAX           
                   tt_pessoa_jurid_integr_j.tta_cod_ramal_fax              = V_COD_RAMAL_FAX     
                   tt_pessoa_jurid_integr_j.tta_cod_telex                  = V_COD_TELEX         
                   tt_pessoa_jurid_integr_j.tta_cod_e_mail                 = V_COD_E_MAIL
                   tt_pessoa_jurid_integr_j.tta_ind_tip_pessoa_jurid       = MV_CLIENTE_INTEGRACAO.ind_tip_pessoa_jurid /*Privada*/
                   tt_pessoa_jurid_integr_j.tta_ind_tip_capit_pessoa_jurid = "1" /*Nacional*/
                   tt_pessoa_jurid_integr_j.tta_log_ems_20_atlzdo          = YES /*ATUALIZA EMS2?*/
                   tt_pessoa_jurid_integr_j.ttv_num_tip_operac             = 1 /*Inclus∆o / Alteraá∆o*/
                   tt_pessoa_jurid_integr_j.tta_nom_ender_cobr             = V_NOM_ENDER_COBR    
                   tt_pessoa_jurid_integr_j.tta_nom_ender_compl_cobr       = V_NOM_ENDER_COMPL_COBR    
                   tt_pessoa_jurid_integr_j.tta_nom_bairro_cobr            = V_NOM_BAIRRO_COBR      
                   tt_pessoa_jurid_integr_j.tta_nom_cidad_cobr             = V_NOM_CIDAD_COBR       
                   tt_pessoa_jurid_integr_j.tta_cod_unid_federac_cobr      = V_COD_UNID_FEDERAC_COBR
                   tt_pessoa_jurid_integr_j.ttv_cod_pais_cobr              = V_COD_PAIS_COBR        
                   tt_pessoa_jurid_integr_j.tta_cod_cep_cobr               = V_COD_CEP_COBR         
                   tt_pessoa_jurid_integr_j.tta_cod_cx_post_cobr           = V_COD_CX_POST_COBR     
                   tt_pessoa_jurid_integr_j.ttv_log_altera_razao_social    = YES
                   tt_pessoa_jurid_integr_j.ttv_ind_tip_pessoa_ems2        = /*if*/ MV_CLIENTE_INTEGRACAO.ind_tip_pessoa /*= "Fisica" THEN "1" ELSE "2"*/ /*1 = F°sica, 2 = Jur°dica*/
               .
    
            ASSIGN SUBSTRING(tt_pessoa_jurid_integr_j.tta_nom_ender_text,1991,09) = string(EMS5.cliente.CDN_CLIENTE).

           
      END.
      ELSE DO:
            ASSIGN tt_pessoa_jurid_integr_j.tta_num_pessoa_jurid           = 0
                   tt_pessoa_jurid_integr_j.tta_nom_pessoa                 = MV_CLIENTE_INTEGRACAO.nom_pessoa  
                   tt_pessoa_jurid_integr_j.tta_cod_id_feder               = v_id_federal
                   tt_pessoa_jurid_integr_j.tta_cod_id_estad_jurid         = V_cod_id_estad_fisic
                   tt_pessoa_jurid_integr_j.tta_cod_pais                   = MV_CLIENTE_INTEGRACAO.COD_PAIS_END /*"BRA"*/
                   tt_pessoa_jurid_integr_j.tta_nom_endereco               = V_NOM_ENDERECO
                   tt_pessoa_jurid_integr_j.tta_nom_ender_compl            = V_NOM_ENDER_COMPL
                   tt_pessoa_jurid_integr_j.tta_nom_bairro                 = V_NOM_BAIRRO
                   tt_pessoa_jurid_integr_j.tta_nom_cidade                 = V_NOM_CIDADE
                   tt_pessoa_jurid_integr_j.tta_cod_unid_federac           = V_COD_UNID_FEDERAC
                   tt_pessoa_jurid_integr_j.tta_cod_cep                    = V_COD_CEP
                   tt_pessoa_jurid_integr_j.tta_cod_telefone               = V_COD_TELEFONE
                   tt_pessoa_jurid_integr_j.tta_cod_fax                    = V_COD_FAX
                   tt_pessoa_jurid_integr_j.tta_cod_ramal_fax              = V_COD_RAMAL_FAX          
                   tt_pessoa_jurid_integr_j.tta_cod_telex                  = V_COD_TELEX
                   tt_pessoa_jurid_integr_j.tta_cod_e_mail                 = V_COD_E_MAIL
                   tt_pessoa_jurid_integr_j.tta_ind_tip_pessoa_jurid       = MV_CLIENTE_INTEGRACAO.ind_tip_pessoa_jurid /*Privada*/
                   tt_pessoa_jurid_integr_j.tta_ind_tip_capit_pessoa_jurid = "1" /*Nacional*/
                   tt_pessoa_jurid_integr_j.tta_log_ems_20_atlzdo          = YES /*ATUALIZA EMS2?*/
                   tt_pessoa_jurid_integr_j.ttv_num_tip_operac             = 1 /*Inclus∆o / Alteraá∆o*/
                   tt_pessoa_jurid_integr_j.tta_nom_ender_cobr             = V_NOM_ENDER_COBR   
                   tt_pessoa_jurid_integr_j.tta_nom_ender_compl_cobr       = V_NOM_ENDER_COMPL_COBR
                   tt_pessoa_jurid_integr_j.tta_nom_bairro_cobr            = V_NOM_BAIRRO_COBR      
                   tt_pessoa_jurid_integr_j.tta_nom_cidad_cobr             = V_NOM_CIDAD_COBR       
                   tt_pessoa_jurid_integr_j.tta_cod_unid_federac_cobr      = V_COD_UNID_FEDERAC_COBR
                   tt_pessoa_jurid_integr_j.ttv_cod_pais_cobr              = V_COD_PAIS_COBR        
                   tt_pessoa_jurid_integr_j.tta_cod_cep_cobr               = V_COD_CEP_COBR         
                   tt_pessoa_jurid_integr_j.tta_cod_cx_post_cobr           = V_COD_CX_POST_COBR     
                   tt_pessoa_jurid_integr_j.ttv_log_altera_razao_social    = YES
                   tt_pessoa_jurid_integr_j.ttv_ind_tip_pessoa_ems2        = MV_CLIENTE_INTEGRACAO.ind_tip_pessoa /*if XX = "Fisica" THEN "1" ELSE "2"*/ /*1 = F°sica, 2 = Jur°dica*/
                   tt_pessoa_jurid_integr_j.tta_cod_id_munic_jurid         = "Isento"
                   tt_pessoa_jurid_integr_j.tta_cod_pais_ext               = V_COD_PAIS_NASC
               .
      END.

      /*--- N∆o retirar o c¢digo abaixo ---*/
      release tt_pessoa_jurid_integr_j.
      if MV_CLIENTE_INTEGRACAO.cod_id_feder <> "" then
          find first tt_pessoa_jurid_integr_j
               where tt_pessoa_jurid_integr_j.tta_cod_id_feder = MV_CLIENTE_INTEGRACAO.cod_id_feder
               and   tt_pessoa_jurid_integr_j.tta_cod_pais     = MV_CLIENTE_INTEGRACAO.cod_pais_end
               no-lock no-error.
      else
          find first tt_pessoa_jurid_integr_j
               where tt_pessoa_jurid_integr_j.tta_cod_id_feder = MV_CLIENTE_INTEGRACAO.cod_id_feder
               and   tt_pessoa_jurid_integr_j.tta_cod_pais     = MV_CLIENTE_INTEGRACAO.cod_pais_end
               and   tt_pessoa_jurid_integr_j.tta_nom_pessoa   = MV_CLIENTE_INTEGRACAO.nom_pessoa
               no-lock no-error.
    END.
END PROCEDURE. /*pi-cria-tt_pessoa_jurid_integr_j*/


PROCEDURE pi-cria-tt_contato_integr_e.
   find FIRST tt_contato_integr_e
        where tt_contato_integr_e.tta_num_pessoa_jurid = 0
          and tt_contato_integr_e.tta_nom_abrev_contat = c-nom-abrev
          and tt_contato_integr_e.tta_cdn_cliente      = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE no-error.
    IF NOT AVAIL tt_contato_integr_e THEN DO:
        CREATE tt_contato_integr_e.
        ASSIGN tt_contato_integr_e.tta_num_pessoa_jurid        = 0
               tt_contato_integr_e.ttv_num_tip_operac          = 1 /* Inclui / Modifica */
               tt_contato_integr_e.tta_cdn_cliente             = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE
               tt_contato_integr_e.tta_nom_abrev_contat        = c-nom-abrev
               tt_contato_integr_e.tta_nom_pessoa              = MV_CLIENTE_INTEGRACAO.nom_pessoa
               tt_contato_integr_e.tta_cod_telef_contat        = V_COD_TELEFONE                   
               tt_contato_integr_e.tta_cod_ramal_contat        = V_cod_ramal                      
               tt_contato_integr_e.tta_cod_fax_contat          = V_COD_FAX                        
               tt_contato_integr_e.tta_cod_ramal_fax_contat    = V_COD_RAMAL_FAX                  
               tt_contato_integr_e.tta_cod_e_mail_contat       = V_COD_E_MAIL
               tt_contato_integr_e.tta_log_ems_20_atlzdo       = YES /*ATUALIZA EMS 2?*/
               tt_contato_integr_e.tta_nom_endereco            = V_NOM_ENDERECO                          
	           tt_contato_integr_e.tta_nom_ender_compl         = V_NOM_ENDER_COMPL 
               tt_contato_integr_e.tta_nom_bairro              = V_NOM_BAIRRO                       
               tt_contato_integr_e.tta_nom_cidade              = V_NOM_CIDADE                       
               tt_contato_integr_e.tta_cod_pais                = MV_CLIENTE_INTEGRACAO.COD_PAIS_END
               tt_contato_integr_e.tta_cod_unid_federac        = V_COD_UNID_FEDERAC                 
               tt_contato_integr_e.tta_cod_cep_cobr            = V_cod_cep_cobr.         
    END.
END PROCEDURE. 


PROCEDURE pi-cria-tt_cliente_integr_j.
    IF NOT CAN-FIND(FIRST tt_cliente_integr_j NO-LOCK
                    WHERE tt_cliente_integr_j.tta_cod_empresa  = MV_CLIENTE_INTEGRACAO.COD_EMPRESA
                      AND tt_cliente_integr_j.tta_cdn_cliente  = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE) AND
        NOT CAN-FIND(FIRST tt_cliente_integr_j NO-LOCK
                     WHERE tt_cliente_integr_j.tta_cod_empresa  = MV_CLIENTE_INTEGRACAO.COD_EMPRESA
                       AND tt_cliente_integr_j.tta_nom_abrev    = c-nom-abrev) THEN DO:
    
        FIND FIRST tip_clien NO-LOCK NO-ERROR.

        CREATE tt_cliente_integr_j.
        ASSIGN tt_cliente_integr_j.tta_cod_empresa         = MV_CLIENTE_INTEGRACAO.COD_EMPRESA         /* as character format "x(3)" label "Empresa" column-label "Empresa"                                    */
               tt_cliente_integr_j.ttv_num_tip_operac      = 1 /*Inclus∆o/Alteraá∆o*/        /* as integer format ">9" column-label "Tipo  Operaá∆o"                                                 */
               tt_cliente_integr_j.tta_cdn_cliente         = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE         /* as Integer format ">>>,>>>,>>9" initial 0 label "Cliente" column-label "Cliente"                     */
               /* tt_cliente_integr_j.tta_num_pessoa       = ? /*Inclus∆o de cliente */        /* as integer format ">>>,>>>,>>9" initial ? label "Pessoa" column-label "Pessoa"                       */ */
               tt_cliente_integr_j.tta_nom_abrev           = c-nom-abrev         /* as character format "x(15)" label "Nome Abreviado" column-label "Nome Abreviado"                     */
               tt_cliente_integr_j.tta_cod_grp_clien       = MV_CLIENTE_INTEGRACAO.COD_GRP_CLIEN         /* as character format "x(4)" label "Grupo Cliente" column-label "Grupo Cliente"                        */
               tt_cliente_integr_j.tta_cod_tip_clien       = IF AVAIL tip_clien THEN tip_clien.cod_tip_clien ELSE ""         /* as character format "x(8)" label "Tipo Cliente" column-label "Tipo Cliente"                          */
               tt_cliente_integr_j.tta_dat_impl_clien      = V_DAT_IMPL_CLIEN         /* as date format "99/99/9999" initial ? label "Implantaá∆o Cliente" column-label "Implantaá∆o Cliente" */
               /* tt_cliente_integr_j.tta_cod_pais_ext     = ""         /* as character format "x(20)" label "Pa°s Externo" column-label "Pa°s Externo"                         */ */
               tt_cliente_integr_j.tta_cod_pais            = MV_CLIENTE_INTEGRACAO.cod_pais_end         /* as character format "x(3)" label "Pa°s" column-label "Pa°s"                                          */
               tt_cliente_integr_j.tta_cod_id_feder        = MV_CLIENTE_INTEGRACAO.COD_ID_FEDER         /* as character format "x(20)" initial ? label "ID Federal" column-label "ID Federal"                   */
               tt_cliente_integr_j.ttv_ind_pessoa          = /*if*/ MV_CLIENTE_INTEGRACAO.ind_tip_pessoa /*= "Fisica" THEN "1" ELSE "2"*/ /*1 = F°sica, 2 = Jur°dica*/         /* as character format "X(08)" initial "Jur°dica" label "Tipo Pessoa" column-label "Tipo Pessoa"        */
               tt_cliente_integr_j.tta_log_ems_20_atlzdo   = YES /*ATUALIZA EMS 2*/         /* as logical format "Sim/N∆o" initial no label "2.0 Atualizado" column-label "2.0 Atualizado"          */
               tt_cliente_integr_j.ttv_ind_tip_pessoa_ems2 = /*if*/ MV_CLIENTE_INTEGRACAO.ind_tip_pessoa. /*= "Fisica" THEN "1" ELSE "2"*/         /* as character format "X(12)" column-label "Tip Pessoa EMS2"                                           */
        release tt_cliente_integr_j.
    END.
END PROCEDURE. /*pi-cria-tt_cliente_integr_j*/


PROCEDURE pi-cria-tt_clien_financ_integr_e.
    /* PEGAR MATRIZ DE TRADUÄ«O DE CADA ABAIXO E GRAVAR NAS INFORMAÄÂES DO ASSIGN */
    DEF VAR c-tp-fluxo-financ-ems2 AS CHAR FORMAT "X(20)".
    
    ASSIGN c-tp-fluxo-financ-ems2 = MV_CLIENTE_INTEGRACAO.cod_tip_fluxo_financ.

    FIND FIRST param_integr_ems WHERE 
               param_integr_ems.ind_param_integr_ems = "clientes 2.00"
               NO-LOCK NO-ERROR.
    IF AVAILABLE param_integr_ems THEN DO:

        FIND FIRST trad_org_ext WHERE
                   trad_org_ext.cod_matriz_trad_org_ext = param_integr_ems.des_contdo_param_integr_ems AND 
                   trad_org_ext.cod_tip_unid_organ      = "998" AND
                   trad_org_ext.cod_unid_organ_ext      = STRING(INTEGER(MV_CLIENTE_INTEGRACAO.COD_EMPRESA))
                   NO-LOCK NO-ERROR.         
    
        IF AVAILABLE trad_org_ext THEN DO:
            FIND FIRST trad_fluxo_ext NO-LOCK
                 WHERE trad_fluxo_ext.cod_matriz_trad_fluxo_ext = trad_org_ext.cod_matriz_trad_fluxo_ext
                   AND trad_fluxo_ext.cod_tip_fluxo_financ      = MV_CLIENTE_INTEGRACAO.cod_tip_fluxo_financ NO-ERROR.
            IF AVAIL trad_fluxo_ext THEN
                ASSIGN c-tp-fluxo-financ-ems2 = trad_fluxo_ext.cod_fluxo_financ_ext.
        END.
    END.
    IF NOT CAN-FIND(FIRST tt_clien_financ_integr_e NO-LOCK
                    WHERE tt_clien_financ_integr_e.tta_cod_empresa         = MV_CLIENTE_INTEGRACAO.COD_EMPRESA
                      AND tt_clien_financ_integr_e.tta_cdn_cliente         = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE) THEN DO:

        CREATE tt_clien_financ_integr_e.
        ASSIGN tt_clien_financ_integr_e.tta_cod_empresa               = MV_CLIENTE_INTEGRACAO.COD_EMPRESA
               tt_clien_financ_integr_e.ttv_num_tip_operac            = 1 /*Inclus∆o/Alteraá∆o*/
               tt_clien_financ_integr_e.tta_cdn_cliente               = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE
               tt_clien_financ_integr_e.tta_cdn_repres                = MV_CLIENTE_INTEGRACAO.cdn_repres
    /*            tt_clien_financ_integr_e.ttv_cod_portad_prefer_ext     = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_cod_portad_ext            = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.ttv_cod_portad_prefer         = MV_CLIENTE_INTEGRACAO. */
               tt_clien_financ_integr_e.tta_cod_portador              = MV_CLIENTE_INTEGRACAO.cod_portador
    /*            tt_clien_financ_integr_e.tta_cod_cta_corren_bco        = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_cod_digito_cta_corren     = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_cod_agenc_bcia            = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_cod_banco                 = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_cod_classif_msg_cobr      = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_cod_instruc_bcia_1_acr    = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_cod_instruc_bcia_2_acr    = MV_CLIENTE_INTEGRACAO. */
               tt_clien_financ_integr_e.tta_log_habilit_emis_boleto   = IF MV_CLIENTE_INTEGRACAO.log_habilit_emis_boleto = 0 THEN NO ELSE YES
               tt_clien_financ_integr_e.tta_log_habilit_gera_avdeb    = IF MV_CLIENTE_INTEGRACAO.log_habilit_gera_avdeb = 0 THEN NO ELSE YES
               tt_clien_financ_integr_e.tta_log_retenc_impto          = IF MV_CLIENTE_INTEGRACAO.log_retenc_impto = 0 THEN NO ELSE YES
               tt_clien_financ_integr_e.tta_log_habilit_db_autom      = IF MV_CLIENTE_INTEGRACAO.log_habilit_db_autom = 0 THEN NO ELSE YES
    /*            tt_clien_financ_integr_e.tta_num_tit_acr_aber          = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_dat_ult_impl_tit_acr      = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_dat_ult_liquidac_tit_acr  = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_dat_maior_tit_acr         = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_dat_maior_acum_tit_acr    = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_val_ult_impl_tit_acr      = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_val_maior_tit_acr         = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_val_maior_acum_tit_acr    = MV_CLIENTE_INTEGRACAO. */
               tt_clien_financ_integr_e.tta_ind_sit_clien_perda_dedut = MV_CLIENTE_INTEGRACAO.ind_sit_clien_perda_dedut
    /*            tt_clien_financ_integr_e.tta_log_neces_acompto_spc     = MV_CLIENTE_INTEGRACAO. */
               tt_clien_financ_integr_e.tta_cod_tip_fluxo_financ      = c-tp-fluxo-financ-ems2
    /*            tt_clien_financ_integr_e.tta_log_utiliz_verba          = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_val_perc_verba            = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_val_min_avdeb             = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_log_calc_multa            = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_num_dias_atraso_avdeb     = MV_CLIENTE_INTEGRACAO. */
    /*            tt_clien_financ_integr_e.tta_cod_digito_agenc_bcia     = MV_CLIENTE_INTEGRACAO. */
               tt_clien_financ_integr_e.tta_cod_cart_bcia             = MV_CLIENTE_INTEGRACAO.cod_cart_bcia
    /*            tt_clien_financ_integr_e.tta_cod_cart_bcia_prefer      = MV_CLIENTE_INTEGRACAO. */
            .
    END.

    release tt_clien_financ_integr_e.
END PROCEDURE. /*pi-cria-tt_clien_financ_integr_e*/


PROCEDURE pi-cria-tt_fornecedor_integr_k.

    IF NOT CAN-FIND(FIRST tt_fornecedor_integr_k NO-LOCK
                    WHERE tt_fornecedor_integr_k.tta_cod_empresa     = MV_CLIENTE_INTEGRACAO.COD_EMPRESA
                      AND tt_fornecedor_integr_k.tta_cdn_fornecedor  = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE) THEN DO:
                                                                                                                                                                                                                                                                       
       CREATE tt_fornecedor_integr_k.
       ASSIGN tt_fornecedor_integr_k.tta_cod_empresa         = MV_CLIENTE_INTEGRACAO.COD_EMPRESA    /* as character format "x(3)" label "Empresa" column-label "Empresa"   */
              tt_fornecedor_integr_k.ttv_num_tip_operac      = 1 /*Inclus∆o/Alteraá∆o*/            /* as integer format ">9" column-label "Tipo  Operaá∆o"                */
              tt_fornecedor_integr_k.tta_cdn_fornecedor      = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE   /* as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"  */
  /*          tt_fornecedor_integr_k.tta_num_pessoa          = ? /*Inclus∆o de cliente */         /* as integer format ">>>,>>>,>>9" initial ? label "Pessoa" column-label "Pessoa"           */  */
              tt_fornecedor_integr_k.tta_nom_abrev           = c-nom-abrev   /* as character format "x(15)" label "Nome Abreviado" column-label "Nome Abreviado"        */
              tt_fornecedor_integr_k.tta_cod_grp_fornec      = MV_CLIENTE_INTEGRACAO.cod_grp_fornec /* as character format "x(4)" label "Grupo Fornecedor" column-label "Grupo Fornecedor" */
              tt_fornecedor_integr_k.tta_cod_tip_fornec      = "999"
              tt_fornecedor_integr_k.tta_dat_impl_fornec     = V_DAT_IMPL_CLIEN         /* as date format "99/99/9999" initial ? label "Implantaá∆o Cliente" column-label "Implantaá∆o Cliente" */
  /*          tt_fornecedor_integr_k.tta_cod_pais_ext        = ""         /* as character format "x(20)" label "Pa°s Externo" column-label "Pa°s Externo"           */  */
              tt_fornecedor_integr_k.tta_cod_pais            = MV_CLIENTE_INTEGRACAO.cod_pais_end         /* as character format "x(3)" label "Pa°s" column-label "Pa°s"          */
              tt_fornecedor_integr_k.tta_cod_id_feder        = MV_CLIENTE_INTEGRACAO.COD_ID_FEDER         /* as character format "x(20)" initial ? label "ID Federal" column-label "ID Federal"   */
              tt_fornecedor_integr_k.ttv_ind_pessoa          = /*if*/ MV_CLIENTE_INTEGRACAO.ind_tip_pessoa /*= "Fisica" THEN "1" ELSE "2"*/ /*1 = F°sica, 2 = Jur°dica*/   /* as character format "X(08)" initial "Jur°dica" label "Tipo Pessoa" column-label "Tipo Pessoa"   */
              tt_fornecedor_integr_k.tta_log_ems_20_atlzdo   = YES /*ATUALIZA EMS 2*/         /* as logical format "Sim/N∆o" initial no label "2.0 Atualizado" column-label "2.0 Atualizado"          */
              tt_fornecedor_integr_k.ttv_ind_tip_pessoa_ems2 = /*if*/ MV_CLIENTE_INTEGRACAO.ind_tip_pessoa /*= "Fisica" THEN "1" ELSE "2"*/         /* as character format "X(12)" column-label "Tip Pessoa EMS2"  */
              tt_fornecedor_integr_k.tta_log_control_inss    = no
              tt_fornecedor_integr_k.tta_log_retenc_impto_pagto = no.

       IF tt_fornecedor_integr_k.ttv_ind_pessoa = "fisica" THEN
          ASSIGN  tt_fornecedor_integr_k.tta_log_cr_pis      = NO
                  tt_fornecedor_integr_k.tta_log_cr_cofins   = NO.
       ELSE
          ASSIGN  tt_fornecedor_integr_k.tta_log_cr_pis      = YES
                  tt_fornecedor_integr_k.tta_log_cr_cofins   = yes.
         
       release tt_fornecedor_integr_k.
    END.
END PROCEDURE.


PROCEDURE pi-cria-tt_fornec_financ_integr_e.
    /* PEGAR MATRIZ DE TRADUÄ«O DE CADA ABAIXO E GRAVAR NAS INFORMAÄÂES DO ASSIGN */
    DEF VAR c-tp-fluxo-financ-ems2 AS CHAR FORMAT "X(20)".
    DEF VAR c-digito-agencia       AS CHAR FORMAT "x(02)".
    DEF VAR c-digito-cta-corrente  AS CHAR FORMAT "x(02)".

    
    
    ASSIGN c-tp-fluxo-financ-ems2 = MV_CLIENTE_INTEGRACAO.cod_tip_flx_finac_fornec
           c-digito-agencia       = MV_CLIENTE_INTEGRACAO.cod_digito_agenc
           c-digito-cta-corrente  = MV_CLIENTE_INTEGRACAO.cod_digito_cta_corren.

    IF c-digito-agencia = ? THEN
       ASSIGN c-digito-agencia = "".

    IF c-digito-cta-corrente = "?" THEN
       ASSIGN c-digito-cta-corrente = "".


    FIND FIRST param_integr_ems WHERE 
               param_integr_ems.ind_param_integr_ems = "clientes 2.00"
               NO-LOCK NO-ERROR.
    IF AVAILABLE param_integr_ems THEN DO:

        FIND FIRST trad_org_ext WHERE
                   trad_org_ext.cod_matriz_trad_org_ext = param_integr_ems.des_contdo_param_integr_ems AND 
                   trad_org_ext.cod_tip_unid_organ      = "998" AND
                   trad_org_ext.cod_unid_organ_ext      = STRING(INTEGER(MV_CLIENTE_INTEGRACAO.COD_EMPRESA))
                   NO-LOCK NO-ERROR.         
    
        IF AVAILABLE trad_org_ext THEN DO:
            FIND FIRST trad_fluxo_ext NO-LOCK
                 WHERE trad_fluxo_ext.cod_matriz_trad_fluxo_ext = trad_org_ext.cod_matriz_trad_fluxo_ext
                   AND trad_fluxo_ext.cod_tip_fluxo_financ      = MV_CLIENTE_INTEGRACAO.cod_tip_flx_finac_fornec NO-ERROR.
            IF AVAIL trad_fluxo_ext THEN
                ASSIGN c-tp-fluxo-financ-ems2 = trad_fluxo_ext.cod_fluxo_financ_ext.
        
        END.
    END.

    IF NOT CAN-FIND(FIRST tt_fornec_financ_integr_e NO-LOCK
                    WHERE tt_fornec_financ_integr_e.tta_cod_empresa    = MV_CLIENTE_INTEGRACAO.COD_EMPRESA
                      AND tt_fornec_financ_integr_e.tta_cdn_fornecedor = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE) THEN DO:

       CREATE tt_fornec_financ_integr_e.
       ASSIGN tt_fornec_financ_integr_e.tta_cod_empresa                = MV_CLIENTE_INTEGRACAO.COD_EMPRESA
              tt_fornec_financ_integr_e.ttv_num_tip_operac             = 1 /*Inclus∆o/Alteraá∆o*/
              tt_fornec_financ_integr_e.tta_cdn_fornecedor             = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE
              tt_fornec_financ_integr_e.tta_cod_portad_ext             = "" 
              tt_fornec_financ_integr_e.tta_cod_portador               = MV_CLIENTE_INTEGRACAO.cod_portador_fornec
              tt_fornec_financ_integr_e.tta_cod_cta_corren_bco         = MV_CLIENTE_INTEGRACAO.cod_cta_corren_bco
              tt_fornec_financ_integr_e.tta_cod_digito_cta_corren      = c-digito-cta-corrente
              tt_fornec_financ_integr_e.tta_cod_agenc_bcia             = MV_CLIENTE_INTEGRACAO.cod_agenc_bcia
              tt_fornec_financ_integr_e.tta_cod_digito_agenc_bcia      = c-digito-agencia
              tt_fornec_financ_integr_e.tta_cod_banco                  = MV_CLIENTE_INTEGRACAO.cod_banco 
              tt_fornec_financ_integr_e.tta_cod_forma_pagto            = "02"
              tt_fornec_financ_integr_e.tta_cod_tip_fluxo_financ       = c-tp-fluxo-financ-ems2
              tt_fornec_financ_integr_e.tta_ind_tratam_vencto_sab      = "Prorroga"
              tt_fornec_financ_integr_e.tta_ind_tratam_vencto_dom      = "Prorroga"
              tt_fornec_financ_integr_e.tta_ind_tratam_vencto_fer      = "Prorroga"
              tt_fornec_financ_integr_e.tta_ind_pagto_juros_fornec_ap  = "N∆o Paga"
              tt_fornec_financ_integr_e.tta_ind_tip_fornecto           = "Serviáos"
              tt_fornec_financ_integr_e.tta_ind_armaz_val_pagto        = "N∆o Paga"
              tt_fornec_financ_integr_e.tta_log_fornec_serv_export     = NO
              tt_fornec_financ_integr_e.tta_log_pagto_bloqdo           = NO
              tt_fornec_financ_integr_e.tta_log_retenc_impto           = NO
              tt_fornec_financ_integr_e.tta_log_vencto_dia_nao_util    = YES.
    
    END.

    /* Preencher a tabela tt_cta_corren_fornec para carregar os dados bancarios no Totvs11 feito por Eduardo Vale 06/10/2014 */

    find first cta_corren_fornec where
               cta_corren_fornec.cod_empresa            = MV_CLIENTE_INTEGRACAO.COD_EMPRESA and
               cta_corren_fornec.cdn_fornecedor         = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE and
               cta_corren_fornec.cod_banco              = MV_CLIENTE_INTEGRACAO.cod_banco   and
               cta_corren_fornec.cod_agenc_bcia         = MV_CLIENTE_INTEGRACAO.cod_agenc_bcia and
               cta_corren_fornec.cod_digito_agenc_bcia  = c-digito-agencia and
               cta_corren_fornec.cod_cta_corren_bco     = MV_CLIENTE_INTEGRACAO.cod_cta_corren_bco and
               cta_corren_fornec.cod_digito_cta_corren  = c-digito-cta-corrente no-lock no-error.
    if not avail cta_corren_fornec then do.
        create tt_cta_corren_fornec.
        assign tt_cta_corren_fornec.tta_cod_empresa             = MV_CLIENTE_INTEGRACAO.COD_EMPRESA  
               tt_cta_corren_fornec.tta_cdn_fornecedor          = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE
               tt_cta_corren_fornec.tta_cod_banco               = MV_CLIENTE_INTEGRACAO.cod_banco
               tt_cta_corren_fornec.tta_cod_agenc_bcia          = MV_CLIENTE_INTEGRACAO.cod_agenc_bcia
               tt_cta_corren_fornec.tta_cod_digito_agenc_bcia   = c-digito-agencia
               tt_cta_corren_fornec.tta_cod_cta_corren_bco      = MV_CLIENTE_INTEGRACAO.cod_cta_corren_bco
               tt_cta_corren_fornec.tta_cod_digito_cta_corren   = c-digito-cta-corrente
               tt_cta_corren_fornec.ttv_cod_desc_cta_fornec     = "SIAF"
               tt_cta_corren_fornec.ttv_log_cta_prefer          = yes
               tt_cta_corren_fornec.ttv_num_tip_operac          = 1 /* Inclus∆o */
               tt_cta_corren_fornec.ttv_rec_cta_fornec          = recid(tt_cta_corren_fornec).
    end.

    /* Fim */

    release tt_cta_corren_fornec.
    release tt_fornec_financ_integr_e.

END PROCEDURE.


PROCEDURE pi-valida-info-clientes.

    run pi-acompanhar in h-acomp1(input "Integraá∆o: " + tt-siaf-cliente.cod_log_integracao).

    IF MV_CLIENTE_INTEGRACAO.CDN_CLIENTE = 0 OR
       MV_CLIENTE_INTEGRACAO.CDN_CLIENTE = ?  THEN DO:

        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 2,                                                          /*O Cliente n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido o c¢digo do cliente na VIEW MV_CLIENTE_INTEGRACAO.", /*"O cliente n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

        ASSIGN lg-OK = NO.
    END.

    IF MV_CLIENTE_INTEGRACAO.COD_CHAVE_INTEGRACAO = "" OR
       MV_CLIENTE_INTEGRACAO.COD_CHAVE_INTEGRACAO = ?  THEN DO:


        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 3,                                                          /*A chave n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido a chave de integraá∆o na VIEW MV_CLIENTE_INTEGRACAO.", /*"A chave n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

        ASSIGN lg-OK = NO.
    END.

    IF MV_CLIENTE_INTEGRACAO.COD_EMPRESA = "" OR
       MV_CLIENTE_INTEGRACAO.COD_EMPRESA = ?  THEN DO:


        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 4,                                                           /*A empresa n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido o c¢digo da empresa na VIEW MV_CLIENTE_INTEGRACAO.", /*"A empresa n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

        ASSIGN lg-OK = NO.
    END. 

    IF MV_CLIENTE_INTEGRACAO.NOM_ABREV = "" OR
       MV_CLIENTE_INTEGRACAO.NOM_ABREV = ?  THEN DO:


        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 5,                                                           /*O nome abrev n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido o nome abreviado do cliente na VIEW MV_CLIENTE_INTEGRACAO.", /*"O nome abrev n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

        ASSIGN lg-OK = NO.
    END. 

    IF MV_CLIENTE_INTEGRACAO.IND_TIP_PESSOA = "" OR
       MV_CLIENTE_INTEGRACAO.IND_TIP_PESSOA = ?  THEN DO:


        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 6,                                                           /*O tipo pessoa n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido o tipo da pessoa na VIEW MV_CLIENTE_INTEGRACAO.", /*"O tipo pessoa n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

        ASSIGN lg-OK = NO.
    END.

    IF MV_CLIENTE_INTEGRACAO.NOM_PESSOA = "" OR
       MV_CLIENTE_INTEGRACAO.NOM_PESSOA = ?  THEN DO:


        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 7,                                                           /*O nome pessoa n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido o nome da pessoa na VIEW MV_CLIENTE_INTEGRACAO.", /*"O nome pessoa n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

        ASSIGN lg-OK = NO.
    END.   

    IF MV_CLIENTE_INTEGRACAO.cod_id_feder = "" OR
       MV_CLIENTE_INTEGRACAO.cod_id_feder = ?  THEN DO:


        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 8,                                                          /*O CPF / CNPJ n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido o ID Federal na VIEW MV_CLIENTE_INTEGRACAO.", /*"O CPF / CNPJ n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

        ASSIGN lg-OK = NO.
    END.   
    ELSE DO:
        
        FIND FIRST EMS5.cliente NO-LOCK
             WHERE EMS5.cliente.cod_id_feder = MV_CLIENTE_INTEGRACAO.cod_id_feder
               AND EMS5.cliente.cod_pais     = MV_CLIENTE_INTEGRACAO.cod_pais_end no-error.
        IF AVAIL EMS5.cliente THEN DO:
            
            IF EMS5.cliente.cdn_cliente <> MV_CLIENTE_INTEGRACAO.CDN_CLIENTE THEN DO:
    
                RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                          INPUT "EMS5",
                                          INPUT 9,                                                           /*O CPF enviado pela integraá∆o est† associdado a outro c¢digo de cliente*/
                                          INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                          INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                          INPUT "O ID Federal " + MV_CLIENTE_INTEGRACAO.cod_id_feder + " j† est† cadastrado para o cliente " + 
                                                STRING(EMS5.cliente.cdn_cliente),                                /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                          INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                                CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/ 

                ASSIGN lg-OK = NO.
            END.
        END.

        FIND FIRST tt_cliente_integr_j WHERE 
                   tt_cliente_integr_j.tta_cod_empresa  = MV_CLIENTE_INTEGRACAO.COD_EMPRESA AND 
                   tt_cliente_integr_j.tta_nom_abrev    = MV_CLIENTE_INTEGRACAO.NOM_ABREV
                   NO-ERROR.
        IF AVAILABLE tt_cliente_integr_j THEN DO:

            RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                      INPUT "EMS5",
                                      INPUT 179,                                                         
                                      INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                      INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                      INPUT "O Nome Abreviado " + MV_CLIENTE_INTEGRACAO.NOM_ABREV + " j† existe como pendància de integraá∆o para o c¢digo  " + 
                                            STRING(tt_cliente_integr_j.tta_cdn_cliente),                 /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                      INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                            CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA).      /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/ 

            ASSIGN lg-OK = NO.
            NEXT.
        END.


        IF MV_CLIENTE_INTEGRACAO.ind_gera_fornec  = "s" THEN DO: /* jac */
           FIND FIRST ems5.fornecedor NO-LOCK
           WHERE ems5.fornecedor.cod_id_feder = MV_CLIENTE_INTEGRACAO.cod_id_feder
             AND ems5.fornecedor.cod_pais     = MV_CLIENTE_INTEGRACAO.cod_pais_end no-error.
           IF AVAIL ems5.fornecedor THEN DO:
            
                IF ems5.fornecedor.cdn_fornecedor <> MV_CLIENTE_INTEGRACAO.CDN_CLIENTE THEN DO:
        
                   RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                              INPUT "EMS5",
                                              INPUT 9,                                                           /*O CPF enviado pela integraá∆o est† associdado a outro c¢digo de cliente*/
                                              INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                              INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                              INPUT "O ID Federal " + MV_CLIENTE_INTEGRACAO.cod_id_feder + " j† est† cadastrado para o cliente/fornecedor " + 
                                                    STRING(EMS5.cliente.cdn_cliente),                                /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                              INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                                    CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/ 
    
                    ASSIGN lg-OK = NO.
                END.
            END.
        END.
    END.
            
    FIND FIRST EMS5.cliente NO-LOCK
         WHERE ems5.cliente.cod_empresa = MV_CLIENTE_INTEGRACAO.COD_EMPRESA
           AND ems5.cliente.cdn_cliente = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE NO-ERROR.
    IF AVAIL cliente THEN DO:

        IF cliente.cod_id_feder <> MV_CLIENTE_INTEGRACAO.COD_ID_FEDER THEN DO:
            RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                      INPUT "EMS5",
                                      INPUT 10,                                                           /*Registro que o cadastro do cliente n∆o contÇm o mesmo CPF do que o recebido para integraá∆o*/
                                      INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                      INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                      INPUT "",                                                          /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                      INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO + 
                                            CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA).      /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/ 

            ASSIGN lg-OK = NO.
        END.
    END.

    IF MV_CLIENTE_INTEGRACAO.ind_gera_fornec = "s" THEN DO:  /* jac */
       FIND FIRST ems5.fornecedor NO-LOCK
            WHERE fornecedor.cod_empresa = MV_CLIENTE_INTEGRACAO.COD_EMPRESA
              AND fornecedor.cdn_fornecedor = MV_CLIENTE_INTEGRACAO.CDN_CLIENTE NO-ERROR.
       IF AVAIL fornecedor THEN DO:
    
          IF fornecedor.cod_id_feder <> MV_CLIENTE_INTEGRACAO.COD_ID_FEDER THEN DO:
             RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                     INPUT "EMS5",
                                     INPUT 10,                                                           /*Registro que o cadastro do cliente n∆o contÇm o mesmo CPF do que o recebido para integraá∆o*/
                                     INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                     INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/ 
                                     INPUT "",                                                          /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                     INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO + 
                                           CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA).      /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/ 
    
             ASSIGN lg-OK = NO.
          END.
       END.

       IF MV_CLIENTE_INTEGRACAO.cod_banco = "" OR
          MV_CLIENTE_INTEGRACAO.cod_banco = ?  THEN DO:


          RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS5",
                                  INPUT 150,                                                           /*O codigo do banco n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido o codigo do banco na VIEW MV_CLIENTE_INTEGRACAO.", /*"O codigo do banco n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome:  " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
    
           ASSIGN lg-OK = NO.
       END.   
    
       IF MV_CLIENTE_INTEGRACAO.cod_agenc_bcia = "" OR
          MV_CLIENTE_INTEGRACAO.cod_agenc_bcia = ?  THEN DO:


          RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS5",
                                  INPUT 151,                                                          /*O codigo da agencia bancaria n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido o codigo da agencia na VIEW MV_CLIENTE_INTEGRACAO.", /*"O codigo da agencia bancaria n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
    
          ASSIGN lg-OK = NO.
       END.  

       IF MV_CLIENTE_INTEGRACAO.cod_cta_corren_bco = "" OR
          MV_CLIENTE_INTEGRACAO.cod_cta_corren_bco = ?  THEN DO:


         RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                 INPUT "EMS5",
                                 INPUT 152,                                                          /*O codigo da conta corrente n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                 INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                 INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                 INPUT "N∆o veio preenchido o codigo da conta corrente  na VIEW MV_CLIENTE_INTEGRACAO.", /*"O codigo da conta corrente n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                 INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                       CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

         ASSIGN lg-OK = NO.
       END.   

       IF MV_CLIENTE_INTEGRACAO.cod_digito_cta_corren = "" OR
          MV_CLIENTE_INTEGRACAO.cod_cta_corren_bco = ?  THEN DO:


          RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS5",
                                  INPUT 153,                                                          /*O digito da conta corrente n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido o digito da conta corrente  na VIEW MV_CLIENTE_INTEGRACAO.", /*"O digito da conta corrente n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

          ASSIGN lg-OK = NO.
       END.   

       IF MV_CLIENTE_INTEGRACAO.cod_portador_fornec = "" OR
          MV_CLIENTE_INTEGRACAO.cod_portador_fornec = ?  THEN DO:


          RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS5",
                                  INPUT 154,                                                          /*O codigo do portador n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido o codigo do portador  na VIEW MV_CLIENTE_INTEGRACAO.", /*"O codigo do portador n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

          ASSIGN lg-OK = NO.
       END.  

       IF MV_CLIENTE_INTEGRACAO.cod_grp_fornec = "" OR
          MV_CLIENTE_INTEGRACAO.cod_grp_fornec = ?  THEN DO:


         RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                 INPUT "EMS5",
                                 INPUT 155,                                                          /*O codigo grupo fornecedor n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                 INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                 INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                 INPUT "N∆o veio preenchido o codigo do portador  na VIEW MV_CLIENTE_INTEGRACAO.", /*"O codigo do grupo fornecedor n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                 INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                       CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

         ASSIGN lg-OK = NO.
      END.  

       IF MV_CLIENTE_INTEGRACAO.cod_tip_flx_finac_fornec = "" OR
          MV_CLIENTE_INTEGRACAO.cod_tip_flx_finac_fornec = ?  THEN DO:


         RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                 INPUT "EMS5",
                                 INPUT 156,                                                          /*O codigo tipo fluxo financeiro do fornecedor n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                 INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                 INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                 INPUT "N∆o veio preenchido o codigo tipo fluxo financeiro do fornecedor na VIEW MV_CLIENTE_INTEGRACAO.", /*"O codigo tipo fluxo financeiro do fornecedor n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                 INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                       CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

         ASSIGN lg-OK = NO.
      END.  

    END.

    ASSIGN V_DAT_IMPL_CLIEN = MV_CLIENTE_INTEGRACAO.DAT_IMPL_CLIEN.
    IF MV_CLIENTE_INTEGRACAO.DAT_IMPL_CLIEN = ?  THEN 
        ASSIGN V_DAT_IMPL_CLIEN = TODAY.

    ASSIGN V_NOM_ENDERECO     = IF MV_CLIENTE_INTEGRACAO.nom_endereco     = ? OR MV_CLIENTE_INTEGRACAO.NOM_ENDERECO     = "?" THEN "" ELSE IF INDEX(MV_CLIENTE_INTEGRACAO.nom_endereco,",") = 0 THEN MV_CLIENTE_INTEGRACAO.NOM_ENDERECO + ", 999999" ELSE MV_CLIENTE_INTEGRACAO.NOM_ENDERECO
            V_NOM_ENDER_COMPL  = IF MV_CLIENTE_INTEGRACAO.nom_complemento  = ? OR MV_CLIENTE_INTEGRACAO.nom_complemento  = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.NOM_complemento 
            V_NOM_BAIRRO       = IF MV_CLIENTE_INTEGRACAO.nom_bairro       = ? OR MV_CLIENTE_INTEGRACAO.nom_bairro       = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.NOM_bairro 
            V_NOM_CIDADE       = IF MV_CLIENTE_INTEGRACAO.nom_cidade       = ? OR MV_CLIENTE_INTEGRACAO.nom_cidade       = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.NOM_cidade               
            V_COD_UNID_FEDERAC = IF MV_CLIENTE_INTEGRACAO.cod_unid_federac = ? OR MV_CLIENTE_INTEGRACAO.cod_unid_federac = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.cod_unid_federac
            V_COD_CEP          = IF MV_CLIENTE_INTEGRACAO.cod_cep          = ? OR MV_CLIENTE_INTEGRACAO.cod_cep          = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.cod_cep          
            V_COD_TELEFONE     = IF MV_CLIENTE_INTEGRACAO.cod_telefone     = ? OR MV_CLIENTE_INTEGRACAO.cod_telefone     = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.cod_telefone     
            V_COD_RAMAL        = IF MV_CLIENTE_INTEGRACAO.cod_ramal        = ? OR MV_CLIENTE_INTEGRACAO.cod_ramal        = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.cod_ramal        
            V_COD_FAX          = IF MV_CLIENTE_INTEGRACAO.cod_fax          = ? OR MV_CLIENTE_INTEGRACAO.cod_fax          = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.cod_fax          
            V_COD_RAMAL_FAX    = IF MV_CLIENTE_INTEGRACAO.cod_ramal_fax    = ? OR MV_CLIENTE_INTEGRACAO.cod_ramal_fax    = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.cod_ramal_fax    
            V_COD_TELEX        = IF MV_CLIENTE_INTEGRACAO.cod_telex        = ? OR MV_CLIENTE_INTEGRACAO.cod_telex        = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.cod_telex.

    IF MV_CLIENTE_INTEGRACAO.IND_TIP_PESSOA = "Fisica" THEN DO:

        IF MV_CLIENTE_INTEGRACAO.COD_PAIS_NASC = "" OR
           MV_CLIENTE_INTEGRACAO.COD_PAIS_NASC = ?  THEN DO:

            RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                      INPUT "EMS2",
                                      INPUT 11,                                                           /*O Pais Nasc. n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                      INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                      INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                      INPUT "N∆o veio preenchido o pa°s de nascimento na VIEW MV_CLIENTE_INTEGRACAO.", /*"O Pais Nasc. n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                      INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                            CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
    
            ASSIGN lg-OK = NO.
        END.

        IF MV_CLIENTE_INTEGRACAO.COD_UNID_FEDERAC_NASC = "" OR
           MV_CLIENTE_INTEGRACAO.COD_UNID_FEDERAC_NASC = ?  THEN DO:

            RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                      INPUT "EMS2",
                                      INPUT 12,                                                           /*O UF Nasc. n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                      INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                      INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                      INPUT "N∆o veio preenchida a UF de nascimento na VIEW MV_CLIENTE_INTEGRACAO.", /*"O UF Nasc. n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                      INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                            CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
    
            ASSIGN lg-OK = NO.
        END.
        
        IF MV_CLIENTE_INTEGRACAO.DAT_NASC_PESSOA_FISIC = ?  THEN DO:
    
            RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                      INPUT "EMS2",
                                      INPUT 13,                                                           /*O Dt Nasc. Pessoa n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                      INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                      INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                      INPUT "N∆o veio preenchida a data de nascimento do cliente na VIEW MV_CLIENTE_INTEGRACAO.", /*"O Dt Nasc. Pessoa n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                      INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                            CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
    
            ASSIGN lg-OK = NO.
        END.
    
    
        IF MV_CLIENTE_INTEGRACAO.IND_ESTADO_CIVIL_PESSOA = "" OR
           MV_CLIENTE_INTEGRACAO.IND_ESTADO_CIVIL_PESSOA = ?  THEN DO:
    
            RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                      INPUT "EMS2",
                                      INPUT 14,                                                           /*O Estado Civil n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                      INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                      INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                      INPUT "N∆o veio preenchido o estado civil do cliente na VIEW MV_CLIENTE_INTEGRACAO.", /*"O Estado Civil n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                      INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                            CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/            
    
            ASSIGN lg-OK = NO.
        END.
    END.

    IF MV_CLIENTE_INTEGRACAO.COD_ID_ESTAD_FISIC = "" OR
       MV_CLIENTE_INTEGRACAO.COD_ID_ESTAD_FISIC = ?  OR
       MV_CLIENTE_INTEGRACAO.COD_ID_ESTAD_FISIC = "?" OR
       MV_CLIENTE_INTEGRACAO.COD_ID_ESTAD_FISIC = "0" THEN DO:

       V_cod_id_estad_fisic = "Isento".
    END.
    ELSE DO:
        V_cod_id_estad_fisic = MV_CLIENTE_INTEGRACAO.COD_ID_ESTAD_FISIC.
        V_cod_id_estad_fisic = replace(replace(replace(V_cod_id_estad_fisic,".",""),"/",""),"-","").
    END.

    IF MV_CLIENTE_INTEGRACAO.COD_ORGAO_EMIS_ID_ESTAD = "" OR
       MV_CLIENTE_INTEGRACAO.COD_ORGAO_EMIS_ID_ESTAD = ?  THEN DO:

        V_COD_ORGAO_EMIS_ID_ESTAD = "".
    END.
    ELSE V_COD_ORGAO_EMIS_ID_ESTAD = MV_CLIENTE_INTEGRACAO.COD_ORGAO_EMIS_ID_ESTAD.

    IF MV_CLIENTE_INTEGRACAO.COD_UNID_FEDERAC_EMIS_ESTAD = "" OR
       MV_CLIENTE_INTEGRACAO.COD_UNID_FEDERAC_EMIS_ESTAD = ?  THEN DO:

        V_COD_UNID_FEDERAC_EMIS_ESTAD = "".
    END.
    ELSE V_COD_UNID_FEDERAC_EMIS_ESTAD = MV_CLIENTE_INTEGRACAO.COD_UNID_FEDERAC_EMIS_ESTAD.

    ASSIGN V_COD_E_MAIL = MV_CLIENTE_INTEGRACAO.cod_e_mail.

    IF MV_CLIENTE_INTEGRACAO.cod_e_mail = "" OR MV_CLIENTE_INTEGRACAO.cod_e_mail = ?  THEN 
       ASSIGN V_COD_E_MAIL = "datasul@animaeducacao.com.br".
    
    ASSIGN V_NOM_ENDER_COBR        = IF MV_CLIENTE_INTEGRACAO.NOM_ENDER_COBR        = ? OR MV_CLIENTE_INTEGRACAO.NOM_ENDER_COBR        = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.NOM_ENDER_COBR
            V_NOM_ENDER_COMPL_COBR  = IF MV_CLIENTE_INTEGRACAO.NOM_COMPL_COBR        = ? OR MV_CLIENTE_INTEGRACAO.NOM_COMPL_COBR        = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.NOM_COMPL_COBR
            V_NOM_BAIRRO_COBR       = IF MV_CLIENTE_INTEGRACAO.NOM_BAIRRO_COBR       = ? OR MV_CLIENTE_INTEGRACAO.NOM_BAIRRO_COBR       = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.NOM_BAIRRO_COBR
            V_NOM_CIDAD_COBR        = IF MV_CLIENTE_INTEGRACAO.NOM_CIDAD_COBR        = ? OR MV_CLIENTE_INTEGRACAO.NOM_CIDAD_COBR        = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.NOM_CIDAD_COBR
            V_COD_UNID_FEDERAC_COBR = IF MV_CLIENTE_INTEGRACAO.COD_UNID_FEDERAC_COBR = ? OR MV_CLIENTE_INTEGRACAO.COD_UNID_FEDERAC_COBR = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.COD_UNID_FEDERAC_COBR
            V_COD_PAIS_COBR         = IF MV_CLIENTE_INTEGRACAO.COD_PAIS_COBR         = ? OR MV_CLIENTE_INTEGRACAO.COD_PAIS_COBR         = "?" THEN "BRA" ELSE MV_CLIENTE_INTEGRACAO.COD_PAIS_COBR
            V_COD_PAIS_NASC         = IF MV_CLIENTE_INTEGRACAO.COD_PAIS_NASC         = ? OR MV_CLIENTE_INTEGRACAO.COD_PAIS_NASC         = "?" THEN "BRA" ELSE MV_CLIENTE_INTEGRACAO.COD_PAIS_NASC
            V_COD_CEP_COBR          = IF MV_CLIENTE_INTEGRACAO.COD_CEP_COBR          = ? OR MV_CLIENTE_INTEGRACAO.COD_CEP_COBR          = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.COD_CEP_COBR
            V_COD_CX_POST_COBR      = IF MV_CLIENTE_INTEGRACAO.COD_CX_POST_COBR      = ? OR MV_CLIENTE_INTEGRACAO.COD_CX_POST_COBR      = "?" THEN "" ELSE MV_CLIENTE_INTEGRACAO.COD_CX_POST_COBR.   

    IF V_NOM_ENDER_COBR <> "" AND 
       V_NOM_ENDER_COBR =  V_NOM_ENDERECO THEN DO:

        ASSIGN V_NOM_ENDER_COMPL_COBR  = V_NOM_ENDER_COMPL     
               V_NOM_BAIRRO_COBR       = V_NOM_BAIRRO  
               V_NOM_CIDAD_COBR        = V_NOM_CIDADE        
               V_COD_UNID_FEDERAC_COBR = V_COD_UNID_FEDERAC       
               V_COD_PAIS_COBR         = MV_CLIENTE_INTEGRACAO.cod_pais_end
               V_COD_CEP_COBR          = V_COD_CEP.          
    END.
    IF V_NOM_ENDER_COBR = "" THEN
        ASSIGN V_NOM_ENDER_COMPL_COBR  = ""
               V_NOM_BAIRRO_COBR       = ""
               V_NOM_CIDAD_COBR        = ""
               V_COD_UNID_FEDERAC_COBR = ""
               V_COD_PAIS_COBR         = ""
               V_COD_CEP_COBR          = "".


    IF MV_CLIENTE_INTEGRACAO.IND_TIP_PESSOA_JURID = "" OR
       MV_CLIENTE_INTEGRACAO.IND_TIP_PESSOA_JURID = ?  THEN DO:

        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 15,                                                           /*O Tipo Pessoa Jurid. n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido o tipo da pessoa jur°dica na VIEW MV_CLIENTE_INTEGRACAO.", /*"O Tipo Pessoa Jurid. n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

        ASSIGN lg-OK = NO.
    END.

    IF MV_CLIENTE_INTEGRACAO.LOG_HABILIT_EMIS_BOLETO < 0 OR
       MV_CLIENTE_INTEGRACAO.LOG_HABILIT_EMIS_BOLETO > 1  THEN DO:


        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 16,                                                           /*O Habilit. Emiss. Boleto n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido corretamente a habilitaá∆o de emiss∆o de boleto na VIEW MV_CLIENTE_INTEGRACAO.", /*"O Habilit. Emiss. Boleto n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

        

        ASSIGN lg-OK = NO.
    END.

    IF MV_CLIENTE_INTEGRACAO.LOG_HABILIT_GERA_AVDEB < 0 OR
       MV_CLIENTE_INTEGRACAO.LOG_HABILIT_GERA_AVDEB > 1  THEN DO:

        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 17,                                                           /*O Habilit. Gera AVDEB n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido corretamente a habilitaá∆o de geraá∆o AVDEB na VIEW MV_CLIENTE_INTEGRACAO.", /*"O Habilit. Gera AVDEB n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

        

        ASSIGN lg-OK = NO.
    END.

    IF MV_CLIENTE_INTEGRACAO.CDN_REPRES = 0 THEN DO:

        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 18,                                                           /*O Repres. n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido corretamente a habilitaá∆o de geraá∆o AVDEB na VIEW MV_CLIENTE_INTEGRACAO.", /*"O Repres. n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
        ASSIGN lg-OK = NO.
    END.

    IF MV_CLIENTE_INTEGRACAO.COD_GRP_CLIEN = "" OR
       MV_CLIENTE_INTEGRACAO.COD_GRP_CLIEN = ?  THEN DO:

        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 19,                                                           /*O Grupo Cliente n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido o grupo de cliente do cliente na VIEW MV_CLIENTE_INTEGRACAO.", /*"O Grupo Cliente n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
        ASSIGN lg-OK = NO.
    END.

    IF MV_CLIENTE_INTEGRACAO.IND_SIT_CLIEN_PERDA_DEDUT = "" OR
       MV_CLIENTE_INTEGRACAO.IND_SIT_CLIEN_PERDA_DEDUT = ?  THEN DO:

        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 20,                                                           /*O Sit. Cliente Perda. Dedut. n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido a situaá∆o de perda dedut°vel do cliente na VIEW MV_CLIENTE_INTEGRACAO.", /*"O Sit. Cliente Perda. Dedut. n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

        

        ASSIGN lg-OK = NO.
    END.

    IF MV_CLIENTE_INTEGRACAO.COD_PORTADOR = "" OR
       MV_CLIENTE_INTEGRACAO.COD_PORTADOR = ?  THEN DO:

        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 21,                                                           /*O Portador n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido o portador na VIEW MV_CLIENTE_INTEGRACAO.", /*"O Portador n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
        ASSIGN lg-OK = NO.
    END. 

    IF MV_CLIENTE_INTEGRACAO.COD_TIP_FLUXO_FINANC = "" OR
       MV_CLIENTE_INTEGRACAO.COD_TIP_FLUXO_FINANC = ?  THEN DO:

        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 22,                                                           /*O tipo fluxo financ. n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido o tipo de fluxo financeiro na VIEW MV_CLIENTE_INTEGRACAO.", /*"O tipo fluxo financ. n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

        ASSIGN lg-OK = NO.
    END.

    IF MV_CLIENTE_INTEGRACAO.COD_CART_BCIA = "" OR
       MV_CLIENTE_INTEGRACAO.COD_CART_BCIA = ?  THEN DO:

        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 23,                                                           /*O Carteira n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido a carteira banc†ria na VIEW MV_CLIENTE_INTEGRACAO.", /*"O Carteira n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
        ASSIGN lg-OK = NO.
    END.

    
    IF MV_CLIENTE_INTEGRACAO.IND_RETEM_ISS <> "S" AND
       MV_CLIENTE_INTEGRACAO.IND_RETEM_ISS <> "N" THEN DO:

        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                  INPUT "EMS2",
                                  INPUT 24,                                                           /*O Retem ISS n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul*/
                                  INPUT 0,                                                           /*Erro padr∆o coloca-se no 3ß campo*/
                                  INPUT "",                                                          /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                  INPUT "N∆o veio preenchido a retená∆o de ISS na VIEW MV_CLIENTE_INTEGRACAO.", /*"O Retem ISS n∆o est† preenchido no SIAF, porÇm ele Ç obrigat¢rio para o Datasul. Verifique: Cliente " + STRING(MV_CLIENTE_INTEGRACAO.CDN_CLIENTE)*/  /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                  INPUT CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO +
                                        CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

        ASSIGN lg-OK = NO.
    END.
END PROCEDURE. /*pi-valida-info-clientes*/


PROCEDURE pi-grava.

    DEF VAR v_hdl_utb765zl AS HANDLE NO-UNDO.

    ASSIGN l-existe = NO.
    
    IF NOT CAN-FIND (FIRST tt_cliente_integr_j) THEN 
        NEXT.
    

    /* Cria tabela tempor†ria para API de validaá∆o do EMS2
       ----------------------------------------------------*/
    FOR EACH tt_cliente_integr_j:

        FIND FIRST tt_clien_financ_integr_e WHERE
                   tt_clien_financ_integr_e.tta_cod_empresa  = tt_cliente_integr_j.tta_cod_empresa AND
                   tt_clien_financ_integr_e.tta_cdn_cliente  = tt_cliente_integr_j.tta_cdn_cliente
                   NO-ERROR.


        IF NOT AVAILABLE tt_clien_financ_integr_e THEN
            NEXT.
    
        EMPTY TEMP-TABLE tt_emitente_ems2.
        EMPTY TEMP-TABLE tt_retorno_validacao.

        CREATE tt_emitente_ems2.
        ASSIGN tt_emitente_ems2.num_tip_operac     = 1
               tt_emitente_ems2.cod_emitente       = tt_cliente_integr_j.tta_cdn_cliente
               tt_emitente_ems2.identific          = 1
               tt_emitente_ems2.nome_abrev         = tt_cliente_integr_j.tta_nom_abrev
               tt_emitente_ems2.nome_matriz        = tt_cliente_integr_j.tta_nom_abrev
               tt_emitente_ems2.natureza           = IF tt_cliente_integr_j.ttv_ind_pessoa = "Fisica" THEN 1 ELSE 2
               tt_emitente_ems2.cgc                = tt_cliente_integr_j.tta_cod_id_feder
               tt_emitente_ems2.tp_rec_padrao      = ?
    
               tt_emitente_ems2.cod_gr_cli         = INTEGER(tt_cliente_integr_j.tta_cod_grp_clien)
               tt_emitente_ems2.data_implant       = tt_cliente_integr_j.tta_dat_impl_clien
               tt_emitente_ems2.cod_rep            = tt_clien_financ_integr_e.tta_cdn_repres
               tt_emitente_ems2.ep_codigo          = INTEGER(tt_clien_financ_integr_e.tta_cod_empresa)
               tt_emitente_ems2.cod_banco          = INTEGER(tt_clien_financ_integr_e.tta_cod_banco).

    
        RELEASE trad_org_ext.
        RELEASE trad_portad_ext.

        FIND FIRST param_integr_ems WHERE 
                   param_integr_ems.ind_param_integr_ems = "clientes 2.00"
                   NO-LOCK NO-ERROR.

        IF AVAILABLE param_integr_ems THEN 
            FIND FIRST trad_org_ext WHERE
                       trad_org_ext.cod_matriz_trad_org_ext = param_integr_ems.des_contdo_param_integr_ems AND 
                       trad_org_ext.cod_tip_unid_organ      = "998" AND
                       trad_org_ext.cod_unid_organ_ext      = STRING(INTEGER(MV_CLIENTE_INTEGRACAO.COD_EMPRESA))
                       NO-LOCK NO-ERROR.         

        IF AVAILABLE trad_org_ext THEN 
            FIND trad_portad_ext WHERE
                 trad_portad_ext.cod_matriz_trad_portad_ext = trad_org_ext.cod_matriz_trad_portad_ext AND 
                 trad_portad_ext.cod_portador               = tt_clien_financ_integr_e.tta_cod_portador AND
                 trad_portad_ext.cod_cart_bcia              = tt_clien_financ_integr_e.tta_cod_cart_bcia
                 NO-LOCK NO-ERROR.
    
        IF AVAILABLE trad_portad_ext THEN
            ASSIGN tt_emitente_ems2.cod_portador = INTEGER(trad_portad_ext.cod_portad_ext)
                   tt_emitente_ems2.modalidade   = INTEGER(trad_portad_ext.cod_modalid_ext).
    
        RELEASE trad_portad_ext.
        IF AVAILABLE trad_org_ext THEN 
            FIND trad_portad_ext WHERE
                 trad_portad_ext.cod_matriz_trad_portad_ext = trad_org_ext.cod_matriz_trad_portad_ext AND 
                 trad_portad_ext.cod_portador               = tt_clien_financ_integr_e.ttv_cod_portad_prefer AND
                 trad_portad_ext.cod_cart_bcia              = tt_clien_financ_integr_e.tta_cod_cart_bcia_prefer
                 NO-LOCK NO-ERROR.
        IF AVAILABLE trad_portad_ext THEN
            ASSIGN tt_emitente_ems2.port_prefer = INTEGER(trad_portad_ext.cod_portad_ext)
                   tt_emitente_ems2.mod_prefer  = INTEGER(trad_portad_ext.cod_modalid_ext).
    
        RELEASE tt_pessoa_jurid_integr_j.
        RELEASE tt_pessoa_fisic_integr_e.

        FIND FIRST tt_pessoa_jurid_integr_j WHERE
                   tt_pessoa_jurid_integr_j.tta_cod_id_feder = tt_cliente_integr_j.tta_cod_id_feder AND
                   tt_pessoa_jurid_integr_j.tta_cod_pais     = tt_cliente_integr_j.tta_cod_pais    
                   NO-ERROR.
        IF AVAILABLE tt_pessoa_jurid_integr_j THEN DO:
    
            ASSIGN tt_emitente_ems2.ins_estadual       = tt_pessoa_jurid_integr_j.tta_cod_id_estad_jurid
                   tt_emitente_ems2.estado             = tt_pessoa_jurid_integr_j.tta_cod_unid_federac
                   tt_emitente_ems2.endereco           = tt_pessoa_jurid_integr_j.tta_nom_endereco
                   tt_emitente_ems2.cep                = tt_pessoa_jurid_integr_j.tta_cod_cep
                   tt_emitente_ems2.cod_pais           = tt_pessoa_jurid_integr_j.tta_cod_pais
                   tt_emitente_ems2.nom_cidade         = tt_pessoa_jurid_integr_j.tta_nom_cidade
                   tt_emitente_ems2.endereco_cob       = tt_pessoa_jurid_integr_j.tta_nom_ender_cobr.
        END.                                           
    
        FIND FIRST tt_pessoa_fisic_integr_e WHERE
                   tt_pessoa_fisic_integr_e.tta_cod_id_feder = tt_cliente_integr_j.tta_cod_id_feder AND
                   tt_pessoa_fisic_integr_e.tta_cod_pais     = tt_cliente_integr_j.tta_cod_pais
                   NO-ERROR.
        IF AVAILABLE tt_pessoa_fisic_integr_e THEN DO:
    
            ASSIGN tt_emitente_ems2.estado             = tt_pessoa_fisic_integr_e.tta_cod_unid_federac
                   tt_emitente_ems2.endereco           = tt_pessoa_fisic_integr_e.tta_nom_endereco
                   tt_emitente_ems2.cep                = tt_pessoa_fisic_integr_e.tta_cod_cep
                   tt_emitente_ems2.cod_pais           = tt_pessoa_fisic_integr_e.tta_cod_pais
                   tt_emitente_ems2.nom_cidade         = tt_pessoa_fisic_integr_e.tta_nom_cidade
                   tt_emitente_ems2.endereco_cob       = tt_pessoa_fisic_integr_e.tta_nom_ender_cobr.
        END.

        RELEASE trad_pais_ext.
        IF AVAILABLE trad_org_ext THEN
            FIND trad_pais_ext WHERE
                 trad_pais_ext.cod_matriz_trad_pais_ext = trad_org_ext.cod_matriz_trad_pais_ext AND 
                 trad_pais_ext.cod_pais                 =  tt_emitente_ems2.cod_pais
                 NO-LOCK NO-ERROR.
    
        IF AVAILABLE trad_pais_ext THEN
            ASSIGN tt_emitente_ems2.cod_pais = trad_pais_ext.cod_pais_ext.


        /* Efetua validaá‰es do EMS2 antes de integrar com EMS5
           ----------------------------------------------------*/
        RUN pi-acompanhar IN h-acomp1 ("Validaá∆o Espec EMS2: " + STRING(tt_cliente_integr_j.tta_cdn_cliente)).    

        RUN espapi/espapi005-apps.p ON h-server (INPUT tt_cliente_integr_j.tta_cod_empresa,
                                                 INPUT  TABLE tt_emitente_ems2,
                                                 OUTPUT TABLE tt_retorno_validacao).


        IF RETURN-VALUE = "NOK" OR CAN-FIND(FIRST tt_retorno_validacao) THEN DO:

            FIND FIRST bf-tt-siaf-cliente NO-LOCK
                 WHERE bf-tt-siaf-cliente.COD_CHAVE_INTEGRACAO = tt_cliente_integr_j.tta_cod_empresa + "|" + STRING(tt_cliente_integr_j.tta_cdn_cliente)
                       NO-ERROR.
                
            FIND FIRST esp_log_integracao NO-LOCK
                 WHERE esp_log_integracao.cod_log_integracao = bf-tt-siaf-cliente.cod_log_integracao NO-ERROR.
            
            IF CAN-FIND(FIRST tt_retorno_validacao) THEN DO:
                FOR EACH tt_retorno_validacao:
                    
      
                    RUN pi-acompanhar IN h-acomp1 ("Erros EMS2: " + STRING(tt_cliente_integr_j.tta_cdn_cliente)).
        
                    RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                            INPUT "EMS2",
                                            INPUT 149, 
                                            INPUT tt_retorno_validacao.ttv_num_mensagem,                    /*Erro padr∆o coloca-se no 3ß campo*/
                                            INPUT tt_retorno_validacao.ttv_des_mensagem,                    /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                            INPUT tt_retorno_validacao.ttv_des_ajuda,                       /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                            INPUT CHR(10) + "EMS2-ESP Cliente: " + STRING(tt_cliente_integr_j.tta_cdn_cliente)). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
                END.
            END.
            ELSE DO:
                RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                        INPUT "EMS2",
                                        INPUT 0, 
                                        INPUT 0,                    /*Erro padr∆o coloca-se no 3ß campo*/
                                        INPUT "Erro de conex∆o de banco EMS2",                    /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                        INPUT "A empresa conectada difere da empresa em processamento. Informe a TI para verificaá∆o do log do appserver",                       /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                        INPUT CHR(10) + "EMS2-ESP Cliente: " + STRING(tt_cliente_integr_j.tta_cdn_cliente)). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

            END.

            FIND FIRST tt-integra-cliente WHERE
                       tt-integra-cliente.COD_CHAVE_INTEGRACAO = bf-tt-siaf-cliente.COD_CHAVE_INTEGRACAO NO-ERROR.
            IF AVAILABLE tt-integra-cliente THEN
                DELETE tt-integra-cliente.

            DELETE tt_cliente_integr_j.
            DELETE tt_clien_financ_integr_e.

            IF AVAILABLE tt_pessoa_fisic_integr_e THEN
                DELETE tt_pessoa_fisic_integr_e.

            IF AVAILABLE tt_pessoa_jurid_integr_j THEN
                DELETE tt_pessoa_jurid_integr_j.

            FIND FIRST tt_fornecedor_integr_k WHERE /* jac */
                 tt_fornecedor_integr_k.tta_cod_empresa  = tt_cliente_integr_j.tta_cod_empresa AND
                 tt_fornecedor_integr_k.tta_cdn_fornec   = tt_cliente_integr_j.tta_cdn_cliente
                 NO-ERROR.
            IF AVAIL tt_fornecedor_integr_k THEN
               DELETE tt_fornecedor_integr_k.

            FIND FIRST tt_fornec_financ_integr_e WHERE /* jac */
                 tt_fornec_financ_integr_e.tta_cod_empresa  = tt_cliente_integr_j.tta_cod_empresa AND
                 tt_fornec_financ_integr_e.tta_cdn_fornec   = tt_cliente_integr_j.tta_cdn_cliente
                 NO-ERROR.
            IF AVAIL tt_fornec_financ_integr_e THEN
               DELETE tt_fornec_financ_integr_e.
        END.
        
    END.

    IF NOT CAN-FIND (FIRST tt_cliente_integr_j) THEN
        NEXT.
    

    /***
    MESSAGE "transaction " transaction
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ***/

    run prgint/utb/utb765zl.py persistent set  v_hdl_utb765zl (Input 1,
      	                                                       Input "",
                 		                                       Input "").

    if  valid-handle(v_hdl_utb765zl) then do:


     	run pi_main_block_utb765zl_10 in v_hdl_utb765zl (Input table tt_cliente_integr_j,
                                              			Input table tt_fornecedor_integr_k,
                                              			Input table tt_clien_financ_integr_e,
                                              			Input table tt_fornec_financ_integr_e,
                                              			Input table tt_pessoa_jurid_integr_j,
                                              			Input table tt_pessoa_fisic_integr_e,
                                              			Input table tt_contato_integr_e,
                                              			Input table tt_contat_clas_integr,
                                              			Input table tt_estrut_clien_integr,
                                              			Input table tt_estrut_fornec_integr,
                                              			Input table tt_histor_clien_integr,
                                              			Input table tt_histor_fornec_integr,
                                              			Input table tt_ender_entreg_integr_e,
                                              			Input table tt_telef_integr,
                                              			Input table tt_telef_pessoa_integr,
                                              			Input table tt_pj_ativid_integr_i,
                                              			Input table tt_pj_ramo_negoc_integr_j,
                                              			Input table tt_porte_pj_integr,
                                              			Input table tt_idiom_pf_integr,
                                              			Input table tt_idiom_contat_integr,
                                              			input-output table tt_retorno_clien_fornec,
                                              			Input table tt_clien_analis_cr_integr,
                                              			Input table tt_cta_corren_fornec).
         delete procedure v_hdl_utb765zl no-error.
        RUN pi-error.

    END.
END PROCEDURE. /*pi-grava*/


PROCEDURE pi-error.

    DEFINE VARIABLE c-cdn-cliente AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-id-feder    AS CHARACTER   NO-UNDO.

    FOR EACH tt-integra-cliente BREAK 
          BY tt-integra-cliente.cod_empresa
          BY tt-integra-cliente.cdn_cliente.
        BLOCO:
        DO  TRANSACTION ON ERROR UNDO BLOCO, LEAVE BLOCO:
            
            FIND FIRST bf-tt-siaf-cliente NO-LOCK
                 WHERE bf-tt-siaf-cliente.COD_CHAVE_INTEGRACAO = tt-integra-cliente.COD_CHAVE_INTEGRACAO /*tt-integra-cliente.cod_empresa + "|" + STRING(tt-integra-cliente.cdn_cliente)*/ NO-ERROR.

            FIND FIRST esp_log_integracao NO-LOCK
                 WHERE esp_log_integracao.cod_log_integracao = bf-tt-siaf-cliente.cod_log_integracao NO-ERROR.
            
            FIND FIRST MV_CLIENTE_INTEGRACAO NO-LOCK
                 WHERE MV_CLIENTE_INTEGRACAO.COD_EMPRESA = tt-integra-cliente.cod_empresa /*c-cod_empresa*/
                   AND MV_CLIENTE_INTEGRACAO.CDN_CLIENTE = tt-integra-cliente.cdn_cliente /*i-cdn_cliente*/ NO-ERROR.

            IF AVAIL bf-tt-siaf-cliente AND AVAIL MV_CLIENTE_INTEGRACAO THEN DO:

                ASSIGN c-cod_empresa = tt-integra-cliente.cod_empresa
                       i-cdn_cliente = tt-integra-cliente.cdn_cliente.

                RUN pi-acompanhar in h-acomp1(input "Integraá∆o: " + bf-tt-siaf-cliente.cod_log_integracao + " - PI-GRAVA - 2").

                FIND FIRST EMS5.cliente NO-LOCK
                    where cliente.cod_empresa = c-cod_empresa
                    and   cliente.cdn_cliente = i-cdn_cliente no-error.
                IF NOT AVAIL cliente THEN 
                    ASSIGN l-existe = NO. /*Validaá∆o se cadastro novo foi feito com sucesso*/
                ELSE ASSIGN l-existe = YES.


                IF NOT l-existe THEN DO:
                                                                                     
                    ASSIGN c-cdn-cliente = "*" + STRING(tt-integra-cliente.cdn_cliente)
                           c-id-feder    = "*" +  tt-integra-cliente.cod_id_feder.
                     
                    IF CAN-FIND(FIRST tt_retorno_clien_fornec WHERE 
                                      tt_retorno_clien_fornec.ttv_cod_parameters MATCHES c-cdn-cliente) OR
                       CAN-FIND(FIRST tt_retorno_clien_fornec WHERE 
                                      tt_retorno_clien_fornec.ttv_cod_parameters MATCHES c-id-feder) THEN DO:

                        ASSIGN lg-OK = NO.     /*BHFS - 05/09/2012*/
                        
                        FOR EACH tt_retorno_clien_fornec NO-LOCK WHERE
                                 tt_retorno_clien_fornec.ttv_cod_parameters MATCHES c-cdn-cliente OR
                                 tt_retorno_clien_fornec.ttv_cod_parameters MATCHES c-id-feder:

                            RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                                    INPUT "EMS5",
                                                    INPUT 0, 
                                                    INPUT tt_retorno_clien_fornec.ttv_num_mensagem,                    /*Erro padr∆o coloca-se no 3ß campo*/
                                                    INPUT tt_retorno_clien_fornec.ttv_des_mensagem,                    /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                                    INPUT tt_retorno_clien_fornec.ttv_des_ajuda,                       /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                                    INPUT CHR(10) + "Chave: " + bf-tt-siaf-cliente.COD_CHAVE_INTEGRACAO  +
                                                          CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/

                        END.    
                    END.
                    ELSE DO:
                        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                                INPUT "EMS5",
                                                INPUT 25, 
                                                INPUT 0,                        /*Erro padr∆o coloca-se no 3ß campo*/
                                                INPUT "",                       /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                                INPUT "",                       /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                                INPUT "N∆o criou o cliente e retornou erro genÇrico." + CHR(10) + "Chave: " + bf-tt-siaf-cliente.COD_CHAVE_INTEGRACAO  +
                                                      CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
                                                /*
                                                INPUT "N∆o criou o cliente e retornou erro genÇrico." + CHR(10) + "Chave: " + tt-siaf-cliente.COD_CHAVE_INTEGRACAO  +
                                                      CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
                                                */
                    END.
                END. /*IF NOT l-existe THEN DO:*/
                ELSE DO:
                    RUN pi-cria-imp-cli-ems5.

                    IF NOT CAN-FIND(clien_financ WHERE
                                    clien_financ.cod_empresa = c-cod_empresa AND
                                    clien_financ.cdn_cliente = i-cdn_cliente) THEN DO:

                        RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                                INPUT "EMS5",
                                                INPUT 144, 
                                                INPUT 0,                        /*Erro padr∆o coloca-se no 3ß campo*/
                                                INPUT "",                       /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                                INPUT "",                       /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                                INPUT "Chave: " + bf-tt-siaf-cliente.COD_CHAVE_INTEGRACAO  +
                                                      CHR(10) + "Nome: " + MV_CLIENTE_INTEGRACAO.NOM_PESSOA). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/                                                
                    END.
                    ELSE DO:

                        /* Cria tabela temporaria apenas com os clientes que foram integrados com sucesso no EMS5. 
                           Ser† passada como parÉmetro para a rotina de integraá∆o entre EMS5 e EM2
                           --------------------------------------------------------------------------------------*/
                        CREATE tt-integra-ems2.
                        ASSIGN tt-integra-ems2.cod_empresa           = c-cod_empresa
                               tt-integra-ems2.cdn_cliente           = i-cdn_cliente
                               tt-integra-ems2.ind_retem_iss         = MV_CLIENTE_INTEGRACAO.IND_RETEM_ISS
                               tt-integra-ems2.nom_pessoa            = MV_CLIENTE_INTEGRACAO.NOM_PESSOA
                               tt-integra-ems2.rw-esp-log-integracao = ROWID(esp_log_integracao).
                        
                    END.
                END.
            END.
        END. /* BLOCO: */
    END.
END PROCEDURE. /*pi-error*/

PROCEDURE pi-cria-imp-cli-ems5.
    
    FOR EACH intgr_param_imp_emp 
       WHERE intgr_param_imp_emp.ep_codigo = INT(EMS5.cliente.cod_empresa) NO-LOCK.

        FOR EACH classif_impto NO-LOCK
           WHERE classif_impto.cod_pais          = "BRA"
             AND classif_impto.cod_unid_federac  = ""
             AND classif_impto.cod_imposto       = STRING(intgr_param_imp_emp.cod_tax).
             
            FIND FIRST impto_vincul_clien EXCLUSIVE-LOCK
                 WHERE impto_vincul_clien.cod_empresa       = cliente.cod_empresa 
                   AND impto_vincul_clien.cdn_cliente       = cliente.cdn_cliente
                   AND impto_vincul_clien.cod_pais          = "BRA"
                   AND impto_vincul_clien.cod_unid_federac  = ""
                   AND impto_vincul_clien.cod_imposto       = STRING(intgr_param_imp_emp.cod_tax)
                   AND impto_vincul_clien.cod_classif_impto = classif_impto.cod_classif_impto NO-ERROR.
            IF NOT AVAIL impto_vincul_clien THEN DO:
                CREATE impto_vincul_clien.
                ASSIGN impto_vincul_clien.cod_empresa       = cliente.cod_empresa 
                       impto_vincul_clien.cdn_cliente       = cliente.cdn_cliente
                       impto_vincul_clien.cod_pais          = "BRA"
                       impto_vincul_clien.cod_unid_federac  = ""
                       impto_vincul_clien.cod_imposto       = STRING(intgr_param_imp_emp.cod_tax)
                       impto_vincul_clien.cod_classif_impto = classif_impto.cod_classif_impto.
            END.
        END.
    END.

END PROCEDURE.  /*pi-cria-imp-cli-ems5*/


PROCEDURE pi-integra-ems2.

    FOR EACH tt-integra-ems2:

        /* Valida cliente no appserver 
           ---------------------------*/
        RUN prgfin/acr/esp0025rp-a1.p ON h-server (INPUT tt-integra-ems2.cod_empresa,
                                                   INPUT tt-integra-ems2.cdn_cliente).


        IF RETURN-VALUE = "OK" THEN DO:
             
            ASSIGN c-arq-valid-ems2  = SESSION:TEMP-DIRECTORY + "cliente_" + tt-integra-ems2.cod_empresa + "_" + STRING(tt-integra-ems2.cdn_cliente) + "_" + REPLACE(STRING(TODAY,"99/99/9999"),"/","-")+ "_" + STRING(TIME) + ".txt".

            RUN prgint/utb/utb704za.r ON h-server (Input "Cliente", 
                                                   Input tt-integra-ems2.cdn_cliente, 
                                                   Input tt-integra-ems2.cdn_cliente, 
                                                   Input ?, 
                                                   Input 1, 
                                                   input YES, 
                                                   Input "Arquivo", 
                                                   Input c-arq-valid-ems2, 
                                                   Input "Batch" /*l_batch*/ ).
        END.
        RUN pi-verifica-integracao.
        
    END.
     
    FOR EACH tt-integra-ems2 WHERE 
             tt-integra-ems2.log-integrado:
    
        FIND esp_log_integracao WHERE
             ROWID(esp_log_integracao) = tt-integra-ems2.rw-esp-log-integracao NO-LOCK.
    
        RUN espapi/espapi004.p (BUFFER esp_log_integracao,
                                INPUT 2).
    
    END.
    
    FOR EACH tt-erros-integra-ems2:
        FIND tt-integra-ems2 WHERE
             ROWID(tt-integra-ems2) = tt-erros-integra-ems2.rw-tt-integra
             NO-ERROR.
        IF NOT AVAILABLE tt-integra-ems2 THEN
            NEXT.
    
        FIND esp_log_integracao WHERE
             ROWID(esp_log_integracao) = tt-integra-ems2.rw-esp-log-integracao NO-LOCK.
    
        IF tt-erros-integra-ems2.tipo-erro = 2 THEN DO:
            RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                    INPUT "EMS2",
                                    INPUT 0, 
                                    INPUT tt-erros-integra-ems2.erro-num,                                             /*Erro padr∆o coloca-se no 3ß campo*/
                                    INPUT tt-erros-integra-ems2.erro-desc,                                            /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                    INPUT tt-erros-integra-ems2.erro-help,                                            /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                    INPUT CHR(10) + "EMS5. Chave: " + esp_log_integracao.cod_chave_integracao). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
        END.
        ELSE DO:
            RUN espapi/espapi002.p (BUFFER esp_log_integracao,
                                    INPUT "EMS2",
                                    INPUT tt-erros-integra-ems2.erro-num, 
                                    INPUT 0,                                             /*Erro padr∆o coloca-se no 3ß campo*/
                                    INPUT tt-erros-integra-ems2.erro-desc,                                            /*T°tulo Erro padr∆o coloca-se no 4ß campo*/
                                    INPUT tt-erros-integra-ems2.erro-help,                                            /*Descriá∆o Erro padr∆o coloca-se no 5ß campo*/
                                    INPUT CHR(10) + "EMS5. Chave: " + esp_log_integracao.cod_chave_integracao). /*Descriá∆o adicional do erro ou coment†rios, escritos diretamente pelo programador ou consultor no c¢digo fonte*/
        END.
    END.
END PROCEDURE. /*pi-integra-ems2*/

PROCEDURE pi-verifica-integracao.

    IF SEARCH(c-arq-valid-ems2) <> ? THEN DO:

        INPUT FROM VALUE(c-arq-valid-ems2) NO-CONVERT.
        
        EMPTY TEMP-TABLE tt-erro-ems2.
        
        REPEAT:
    
            IMPORT UNFORMATTED c-linha.
                            
            IF c-linha MATCHES "*Mensagem:*" THEN DO:
                IF c-erro-num <> "" THEN DO:
                
                    FIND FIRST tt-erro-ems2 NO-LOCK
                         WHERE tt-erro-ems2.erro-num = c-erro-num NO-ERROR.
                    IF NOT AVAIL tt-erro-ems2 THEN DO:
                        CREATE tt-erro-ems2.
                        ASSIGN tt-erro-ems2.erro-num  = c-erro-num
                               tt-erro-ems2.erro-desc = c-erro-desc
                               tt-erro-ems2.erro-help = c-erro-help.
                    END.
                            
                    ASSIGN c-erro-num  = ""
                           c-erro-desc = ""
                           c-erro-help = "".
                END.
                
                ASSIGN c-erro-num = TRIM(REPLACE(ENTRY(2,c-linha,":"),"Descriá∆o","")).
                ASSIGN l-ajuda = NO.
            END.
            
            IF c-linha MATCHES "*Descriá∆o:*" THEN DO:
                ASSIGN c-erro-desc = TRIM(REPLACE(ENTRY(3,c-linha,":"),"Ajuda","")).
            END.
            
            IF l-ajuda THEN DO:
                IF c-linha <> "" THEN
                    ASSIGN c-erro-help = c-erro-help + " " + TRIM(c-linha).
                ELSE DO:
                    ASSIGN l-ajuda = NO.
        
                    FIND FIRST tt-erro-ems2 NO-LOCK
                         WHERE tt-erro-ems2.erro-num = c-erro-num NO-ERROR.
                    IF NOT AVAIL tt-erro-ems2 THEN DO:
                        CREATE tt-erro-ems2.
                        ASSIGN tt-erro-ems2.erro-num  = c-erro-num
                               tt-erro-ems2.erro-desc = c-erro-desc
                               tt-erro-ems2.erro-help = c-erro-help.
                    END.
        
                    ASSIGN c-erro-num  = ""
                           c-erro-desc = ""
                           c-erro-help = "".
                END.
            END.
            
            IF c-linha MATCHES "*Ajuda:*" THEN DO:
                ASSIGN c-erro-help = TRIM(ENTRY(4,c-linha,":")).
                ASSIGN l-ajuda = YES.
            END.    
        END.
    END.

    
    ASSIGN lg-OK = YES.    
    FOR EACH tt-erro-ems2 NO-LOCK.
        
        IF INTEGER(tt-erro-ems2.erro-num) = 6279 THEN /*INTEGRAÄ«O COM SUCESSO*/
            ASSIGN lg-OK = YES.
        ELSE DO:
            ASSIGN lg-OK = NO.

            CREATE tt-erros-integra-ems2.
            ASSIGN tt-erros-integra-ems2.rw-tt-integra = ROWID(tt-integra-ems2)
                   tt-erros-integra-ems2.erro-num      = tt-erro-ems2.erro-num
                   tt-erros-integra-ems2.erro-desc     = tt-erro-ems2.erro-desc
                   tt-erros-integra-ems2.erro-help     = tt-erro-ems2.erro-help
                   tt-erros-integra-ems2.tipo-erro     = 2.
        END.
    END.

    IF lg-OK THEN DO:
         /* Complementa dados do emitente
           -----------------------------*/
        ASSIGN l-integrado = NO.

        RUN prgfin/acr/esp0025rp-a2.p ON h-server (INPUT tt-integra-ems2.cod_empresa,
                                                   INPUT tt-integra-ems2.cdn_cliente,
                                                   INPUT tt-integra-ems2.ind_retem_iss,
                                                   OUTPUT l-integrado).
        IF l-integrado THEN 
            ASSIGN tt-integra-ems2.log-integrado = YES.
        ELSE DO:
            CREATE tt-erros-integra-ems2.
            ASSIGN tt-erros-integra-ems2.rw-tt-integra = ROWID(tt-integra-ems2)
                   tt-erros-integra-ems2.erro-num      = "28"
                   tt-erros-integra-ems2.erro-desc     = ""
                   tt-erros-integra-ems2.erro-help     = "N∆o criou o cliente e n∆o retornou erro"
                   tt-erros-integra-ems2.tipo-erro     = 1.
        END.
    END.
END PROCEDURE. /* pi-verifica-integracao */

PROCEDURE pi-chama-conexao-appserver.

    DEFINE VARIABLE c-erros-conexao AS CHARACTER   NO-UNDO.

    /* As validaá‰es de conex∆o com o appserver j† foram feitas no ESP0025.PY
       ----------------------------------------------------------------------*/
    l-erro-appserver = NO.

    RUN pi_conecta_appserver (OUTPUT c-erros-conexao).                            

    IF c-erros-conexao <> "" OR 
        RETURN-VALUE = "NOK" THEN DO:

        l-erro-appserver = YES.
        NEXT.
    END.

    IF NOT VALID-HANDLE(h-server) THEN DO:
        l-erro-appserver = YES.
        NEXT.
    END.

    RUN espp/grava-prog-appserver.p ON h-server (INPUT "In°cio", 
                                                 INPUT "esp0025rp-a.p").

    /* Necess†rio desconectar os bancos do Oracle para n∆o dar erro no login
       ---------------------------------------------------------------------*/
    RUN prgfin/acr/desconecta-siaf.p ON h-server.

    /* Login automatico no EMS5
       ------------------------*/
    RUN btb/btapi910za.p ON h-server(INPUT "integracao":U, 
                                     INPUT "integracao":U, 
                                     OUTPUT TABLE tt-erros-login).
    IF RETURN-VALUE = "NOK":U THEN DO:
        l-erro-appserver = YES.
    END.
    ELSE DO:
         RUN espp/desconecta_bancos_empresa.p ON h-server.
         RUN espp/conecta_bancos_empresa.p ON h-server (INPUT STRING(tt-empresas.cod_empresa,"999")).

    END.

    RUN prgfin/acr/conecta-siaf.p ON h-server.
END PROCEDURE. /*pi-chama-conexao-appserver*/


PROCEDURE pi_conecta_appserver:

    DEF OUTPUT PARAMETER c-erros AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i AS INTEGER     NO-UNDO.


    CREATE SERVER h-server.
    
    FOR FIRST servid_rpc no-lock
        WHERE servid_rpc.cod_servid_rpc = ESP_PARAMETRO.DES_VALOR_PARAMETRO
        AND   servid_rpc.log_servid_rpc_dispon:
        
        /*
        h-server:connect("-H 172.17.2.38 -S 5162 -DirectConnect") NO-ERROR.
        */
        
        h-server:connect(servid_rpc.des_carg_rpc) NO-ERROR.
    END.

    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO.

        DO i=0 TO ERROR-STATUS:NUM-MESSAGES:
            IF ERROR-STATUS:GET-MESSAGE(i) <> '' THEN
                ASSIGN c-erros = c-erros + ERROR-STATUS:GET-MESSAGE(i) + '   '. 
        END.
    END.

    IF c-erros <> "" THEN
        RETURN.
    
    IF NOT AVAIL servid_rpc then DO:
        message "Cadastro de Servidor RPC 'orcweb' nao encontrado".
        c-erros = "Cadastro de Servidor RPC 'orcweb' nao encontrado".
        return "NOK".
    END.
    
    if h-server:connected() then
        return "OK".
    else do:
        message "Erro ao conectar ao appserver orcweb".
        c-erros = "Erro ao conectar ao appserver orcweb".
        return "NOK".
    END.
END PROCEDURE. 


PROCEDURE pi_desconecta_appserver:
    RUN espp/desconecta_bancos_empresa.p ON h-server.
    RUN espp/grava-prog-appserver.p ON h-server (INPUT "TÇrmino", 
                                                 INPUT "esp0025rp-a.p").

    h-server:DISCONNECT().
    DELETE OBJECT h-server.
END.

