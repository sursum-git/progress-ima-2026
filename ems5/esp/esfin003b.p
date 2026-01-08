/*{utp\ut-glob.i} variaveis globais datasul*/
{esinc\esfin003.i}
{utp/ut-glob.i}

DEFINE INPUT        PARAMETER pDtIni    AS DATE        NO-UNDO.
DEFINE INPUT        PARAMETER pDtFim    AS DATE        NO-UNDO.
DEFINE INPUT        PARAMETER p-analise AS INTEGER.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt.

/* tabela temporaria que armazena as contas, os centros de custo por sequencia da nota fiscal de origem do titulo a pagar*/
DEFINE TEMP-TABLE ttApropItem
       FIELD rRowidnota     AS ROWID
       FIELD conta          AS CHAR FORMAT 'x(12)'
       FIELD cc             AS CHAR        
       FIELD itCodigo       LIKE ems2med.movto-estoq.it-codigo
       FIELD codRefer       LIKE ems2med.movto-estoq.cod-refer
       FIELD seq            LIKE ems2med.movto-estoq.sequen-nf
       FIELD valorMat       AS DECIMAL
       FIELD valorNf        AS DECIMAL.

/*tabela temporaria que armazena a propor‡Æo de valor por conta e centro de custo a ser apropriado no titulo pago*/   
DEFINE TEMP-TABLE ttApropRateio
       FIELD rowidnota   AS ROWID
       FIELD conta       AS CHAR FORMAT 'x(12)'
       FIELD cc          AS CHAR
       FIELD valor       AS DECIMAL
       FIELD perc        AS DECIMAL
       FIELD documento   AS CHAR
       FIELD serie       AS CHAR
       FIELD codEmitente AS INT
       FIELD natOperacao AS CHAR
       FIELD data        AS DATE .

/*buffer do movto-tit-ap para buscar a apropria‡Æo da implanta‡Æo do titulo*/
DEFINE BUFFER bf_movto_tit_ap_01 FOR ems5.movto_tit_ap.
DEFINE BUFFER bf_movto_tit_ap_02 FOR ems5.movto_tit_ap.
DEFINE BUFFER bf_movto_tit_ap_03 FOR ems5.movto_tit_ap.

DEFINE BUFFER bf_tit_ap_01  FOR ems5.tit_ap.
DEFINE BUFFER bf_tit_ap_02  FOR ems5.tit_ap.
DEFINE BUFFER bf_tit_ap_03  FOR ems5.tit_ap.
DEFINE BUFFER bf_tit_ap_04  FOR ems5.tit_ap.
DEFINE BUFFER bf_tit_ap_05  FOR ems5.tit_ap.

DEF BUFFER movto_cta_corren     FOR ems5.movto_cta_corren.
DEF BUFFER cta_corren           FOR ems5.cta_corren.
DEF BUFFER movto_tit_acr        FOR ems5.movto_tit_acr.    
DEF BUFFER tit_acr              FOR ems5.tit_acr.
DEF BUFFER aprop_ctbl_acr       FOR ems5.aprop_ctbl_acr.
DEF BUFFER movto_tit_ap         FOR ems5.movto_tit_ap.
DEF BUFFER tit_ap               FOR ems5.tit_ap.
DEF BUFFER espec_docto          FOR ems5.espec_docto.
DEF BUFFER fornecedor           FOR ems5.fornecedor.
DEF BUFFER grp_fornec           FOR ems5.grp_fornec.
DEF BUFFER antecip_pef_pend     FOR ems5.antecip_pef_pend.
DEF BUFFER aprop_ctbl_pend_ap   FOR ems5.aprop_ctbl_pend_ap.
DEF BUFFER tip_trans_cx         FOR ems5.tip_trans_cx.
DEF BUFFER aprop_ctbl_cmg       FOR ems5.aprop_ctbl_cmg.
DEF BUFFER cta_ctbl             FOR ems5.cta_ctbl.
DEF BUFFER ccusto               FOR ems5.ccusto.
DEF BUFFER aprop_ctbl_ap        FOR ems5.aprop_ctbl_ap.
DEF BUFFER val_aprop_ctbl_ap    FOR ems5.val_aprop_ctbl_ap.

DEF VAR h-acomp      AS HANDLE  NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando..").

EMPTY TEMP-TABLE tt.
EMPTY TEMP-TABLE ttApropRateio.
EMPTY TEMP-TABLE ttApropItem.

RUN pi-BuscarDados.
RUN pi-ClassificarDados.

RUN pi-finalizar in h-acomp.


//---------- Procedures
PROCEDURE pi-BuscarDados:
    DEFINE VARIABLE valor       AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE rRowidNota  AS ROWID       NO-UNDO.
    
    FOR EACH movto_cta_corren WHERE 
             movto_cta_corren.dat_transacao >= pDtIni AND   
             movto_cta_corren.dat_transacao <= pDtFim AND   
             movto_cta_corren.ind_tip_movto_cta_corren = 're' AND   
             movto_cta_corren.cod_tip_trans_cx <> '002' NO-LOCK:

        RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(movto_cta_corren.dat_transacao,"99/99/9999") ).

        FIND FIRST cta_corren OF movto_cta_corren NO-LOCK NO-ERROR.
        CASE movto_cta_corren.cod_modul_dtsul:
            WHEN 'acr'  THEN DO:
               FOR EACH movto_tit_acr OF movto_cta_corren NO-LOCK:
                   FIND FIRST tit_acr OF movto_tit_acr NO-LOCK NO-ERROR.                                                                                                               
                   FIND FIRST emitente WHERE
                              emitente.cod-emitente = tit_acr.cdn_cliente NO-LOCK NO-ERROR. 
                   FIND FIRST ext-emitente OF emitente NO-LOCK NO-ERROR.
                   IF AVAIL ext-emitente THEN
                      FIND FIRST ramo-ativ
                           WHERE ramo-ativ.cod-ramo-ativ = ext-emitente.cod-ramo-ativ NO-LOCK NO-ERROR.
    
                   FOR EACH aprop_ctbl_acr OF movto_tit_acr WHERE  
                            (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_acr.ind_natur_lancto_ctbl = 'cr') OR
                            (movto_cta_corren.ind_fluxo_movto_cta_corren = 'sai' AND aprop_ctbl_acr.ind_natur_lancto_ctbl = 'db') NO-LOCK.

                       CREATE tt.                                                                                                                                                         
                       ASSIGN tt.cod_modulo        = movto_cta_corren.cod_modul_dtsul
                              tt.cod_empresa       = aprop_ctbl_acr.cod_empresa                                                                                                                  
                              tt.cod_estab         = aprop_ctbl_acr.cod_estab                                                                                                                    
                              tt.cod_emitente      = IF AVAIL tit_acr THEN string(tit_acr.cdn_cliente) ELSE  '0'                                                                                
                              tt.desc_emitente     = IF AVAIL emitente THEN emitente.nome-emit ELSE ''                                                                                          
                              tt.data              = movto_cta_corren.dat_transacao                                                                                                               
                              tt.conta_contabil    = aprop_ctbl_acr.cod_cta_ctbl                                                                                                                
                              tt.valor             = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN  aprop_ctbl_acr.val_aprop_ctbl  ELSE  aprop_ctbl_acr.val_aprop_ctbl * -1       
                              tt.conta_corrente    = movto_cta_corren.cod_cta_corren                                                                                                            
                              tt.base              = '10'                                                                                                                                       
                              tt.origem            = 'Contas a Receber'                                                                                                                          
                              tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren                                                                                                 
                              tt.cc                = aprop_ctbl_acr.cod_ccusto
                              tt.ccusto_gerencial  = tt.cc
                              tt.sequencia         = movto_cta_corren.num_seq_movto_cta_corren
                              tt.id_movto_corren   = movto_cta_corren.num_id_movto_cta_corren
                              tt.num_id_tit        = tit_acr.num_id_tit_acr
                              tt.cod_tit           = tit_acr.cod_tit_acr
                              tt.grupo_emitente    = IF AVAIL ramo-ativ THEN string(ramo-ativ.cod-ramo-ativ) + '-' + ramo-ativ.descricao ELSE '' 
                              tt.historico         = replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
                              tt.historico         = replace(movto_cta_corren.des_histor_movto_cta_corren,chr(10),";"). 
                   END.
               END.
            END.
            WHEN 'apb'  THEN DO:
               FOR EACH movto_tit_ap OF movto_cta_corren NO-LOCK.
                   FIND FIRST tit_ap OF movto_tit_ap NO-LOCK NO-ERROR. 
                   FIND FIRST espec_docto OF tit_ap NO-LOCK NO-ERROR.
                   FIND FIRST fornecedor WHERE 
                              fornecedor.cdn_fornecedor = tit_ap.cdn_fornecedor NO-LOCK NO-ERROR. 
                   IF AVAIL fornecedor THEN
                      FIND FIRST grp_fornec WHERE 
                                 grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
                   
                   IF (AVAIL tit_ap AND tit_ap.ind_origin_tit_ap <> "REC") OR NOT AVAIL tit_Ap  THEN DO:
                      IF AVAIL tit_ap THEN DO:
                         IF SUBSTR(tit_ap.cod_tit_ap,1,2) = 'hr' OR 
                            espec_docto.ind_tip_espec_docto = 'imposto retido' THEN DO : 
                            RUN pi-BuscarApropFolhaImp(ROWID(movto_tit_ap),
                                                      movto_cta_corren.dat_transacao,
                                                      movto_cta_corren.ind_fluxo_movto_cta_corren,
                                                      movto_tit_ap.val_movto_ap,
                                                      movto_cta_corren.cod_cta_corren,
                                                      movto_cta_corren.num_seq_movto_cta_corren,                 
                                                      movto_cta_corren.num_id_movto_cta_corren ).
                         END.                                                                                  
                         ELSE DO:     
                            RUN pi-BuscarApropNormal(movto_cta_corren.dat_transacao,                            
                                                     movto_cta_corren.ind_fluxo_movto_cta_corren,               
                                                     movto_tit_ap.val_movto_ap,                                 
                                                     movto_tit_ap.val_juros,                                    
                                                     movto_tit_ap.val_desconto,                                 
                                                     movto_cta_corren.cod_cta_corren,                           
                                                     movto_cta_corren.num_seq_movto_cta_corren,                 
                                                     movto_cta_corren.num_id_movto_cta_corren,                  
                                                     ROWID(tit_ap) ).                
                         END.                                                                                  
                      END.
                      ELSE DO:
                         FOR EACH antecip_pef_pend OF movto_tit_ap NO-LOCK:
                             FIND FIRST fornecedor OF antecip_pef_pend NO-LOCK NO-ERROR.
                             IF AVAIL fornecedor THEN
                             FIND FIRST grp_fornec where
                                        grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
                             FOR EACH aprop_ctbl_pend_ap OF antecip_pef_pend NO-LOCK.

                                 /*
                                 FIND tt WHERE
                                      tt.num_id_tit = movto_tit_ap.num_id_tit_ap NO-ERROR.
                                 IF AVAIL tt THEN NEXT.
                                 */

                                 CREATE tt.                                                                                                                                                   
                                 ASSIGN tt.cod_modulo        = movto_cta_corren.cod_modul_dtsul
                                        tt.cod_empresa       = aprop_ctbl_pend_ap.cod_empresa                                                                                                             
                                        tt.cod_estab         = aprop_ctbl_pend_ap.cod_estab                                                                                                               
                                        tt.cod_emitente      = STRING(antecip_pef_pend.cdn_fornecedor)                                                                           
                                        tt.desc_emitente     = IF AVAIL fornecedor THEN fornecedor.nom_pessoa  ELSE ''                                                                                   
                                        tt.data              = movto_cta_corren.dat_transacao                                                                                                        
                                        tt.conta_contabil    = aprop_ctbl_pend_ap.cod_cta_ctbl                                                                                                           
                                        tt.valor             = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN  movto_tit_ap.val_movto_ap ELSE movto_tit_ap.val_movto_ap * -1     
                                        tt.conta_corrente    = movto_cta_corren.cod_cta_corren                                                                                                      
                                        tt.base              = '10'
                                        tt.origem            = 'Contas a Pagar - PEF'                                                                                                                     
                                        tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren                                                                                           
                                        tt.cc                = aprop_ctbl_pend_ap.cod_ccusto
                                        tt.ccusto_gerencial  = tt.cc
                                        tt.sequencia         = movto_cta_corren.num_seq_movto_cta_corren
                                        tt.id_movto_corren   = movto_cta_corren.num_id_movto_cta_corren
                                        tt.num_id_tit        = movto_tit_ap.num_id_tit_ap
                                        tt.cod_tit           = movto_tit_ap.cod_tit_ap
                                        tt.grupo_emitente    = IF AVAIL grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + grp_fornec.des_grp_fornec  ELSE ''
                                        tt.historico         = replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
                                        tt.historico         = replace(movto_cta_corren.des_histor_movto_cta_corren,chr(10),";")  .
                             END.
                         END.
                      END.
                   END.
                   ELSE DO:
                       CASE i-ep-codigo-usuario:
                           WHEN '5' THEN
                                RUN pi-BuscarNotasRateioEms2(tit_ap.cod_ser_docto,tit_ap.cod_tit_ap,tit_ap.cdn_fornecedor,tit_ap.cod_parcela, OUTPUT rRowidNota ).
                           WHEN '1' THEN
                                RUN pi-BuscarNotasRateioEms2Aux(tit_ap.cod_ser_docto,tit_ap.cod_tit_ap,tit_ap.cdn_fornecedor,tit_ap.cod_parcela, OUTPUT rRowidNota ).
                       END CASE.
    
                       FOR EACH ttApropRateio WHERE 
                                ttApropRateio.rowidNota = rRowidNota .

                           /*
                           FIND tt WHERE
                                tt.num_id_tit = tit_ap.num_id_tit_ap NO-ERROR.
                           IF AVAIL tt THEN NEXT.
                           */ 
                             
                           CREATE tt.                                                                                                                                                   
                           ASSIGN tt.cod_modulo        = movto_cta_corren.cod_modul_dtsul
                                  tt.cod_empresa       = IF AVAIL tit_ap THEN tit_ap.cod_empresa ELSE ''                                                                                                             
                                  tt.cod_estab         = IF AVAIL tit_ap THEN tit_ap.cod_estab   ELSE ''                                                                                                             
                                  tt.cod_emitente      = IF AVAIL tit_ap THEN string(tit_ap.cdn_fornecedor) ELSE '0'                                                                          
                                  tt.desc_emitente     = IF AVAIL fornecedor THEN fornecedor.nom_pessoa  ELSE ''                                                                                  
                                  tt.data              = movto_cta_corren.dat_transacao                                                                                                          
                                  tt.conta_contabil    = ttApropRateio.conta                                                                                                           
                                  tt.valor             = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' 
                                                         THEN movto_tit_ap.val_movto_ap * ttApropRateio.perc 
                                                         ELSE movto_tit_ap.val_movto_ap * -1 * ttApropRateio.perc     
                                  tt.conta_corrente    = movto_cta_corren.cod_cta_corren                                                                                                      
                                  tt.base              = '10'
                                  tt.origem            = 'Contas a Pagar - REC'                                                                                                                     
                                  tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren                                                                                           
                                  tt.cc                = ttApropRateio.cc
                                  tt.ccusto_gerencial  = tt.cc
                                  tt.sequencia         = movto_cta_corren.num_seq_movto_cta_corren
                                  tt.id_movto_corren   = movto_cta_corren.num_id_movto_cta_corren
                                  tt.num_id_tit        = tit_ap.num_id_tit_ap
                                  tt.cod_tit           = tit_ap.cod_tit_ap
                                  tt.grupo_emitente    = IF AVAIL grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + grp_fornec.des_grp_fornec  ELSE ''
                                  tt.historico         = replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
                                  tt.historico         = replace(movto_cta_corren.des_histor_movto_cta_corren,chr(10),";").
                       END.
                   END.
               END.
            END.
            WHEN 'cmg'  THEN DO:
                FIND FIRST tip_trans_cx OF movto_cta_corren NO-LOCK NO-ERROR.
                FOR EACH aprop_ctbl_cmg OF movto_cta_corren
                    WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_cmg.ind_natur_lancto_ctbl = 'cr')
                    OR  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'sai' AND aprop_ctbl_cmg.ind_natur_lancto_ctbl = 'db') NO-LOCK :
                   CREATE tt.
                   ASSIGN tt.cod_modulo        = movto_cta_corren.cod_modul_dtsul
                          tt.cod_empresa       = aprop_ctbl_cmg.cod_empresa 
                          tt.cod_estab         = aprop_ctbl_cmg.cod_estab
                          tt.cod_emitente      = movto_cta_corren.cod_tip_trans_cx
                          tt.desc_emitente     = IF AVAIL tip_trans_cx THEN tip_trans_cx.des_tip_trans_cx  ELSE ''
                          tt.data              = movto_cta_corren.dat_transacao
                          tt.conta_contabil    = aprop_ctbl_cmg.cod_cta_ctbl
                          tt.valor             = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN aprop_ctbl_cmg.val_movto_cta_corren ELSE aprop_ctbl_cmg.val_movto_cta_corren * -1
                          tt.conta_corrente    = movto_cta_corren.cod_cta_corren
                          tt.base              = '10'
                          tt.origem            = 'Caixa e Bancos'
                          tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren
                          tt.cc                = aprop_ctbl_cmg.cod_ccusto
                          tt.ccusto_gerencial  = tt.cc
                          tt.sequencia         = movto_cta_corren.num_seq_movto_cta_corren
                          tt.id_movto_corren   = movto_cta_corren.num_id_movto_cta_corren
                          tt.grupo_emitente    = ''
                          tt.historico         = REPLACE(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
                          tt.historico         = REPLACE(movto_cta_corren.des_histor_movto_cta_corren,chr(10),";") .    
                END.
            END.
        END CASE.
    END.

    FOR EACH tt NO-LOCK:
        FIND FIRST cta_ctbl WHERE 
                   cta_ctbl.cod_cta_ctbl = tt.conta_contabil NO-LOCK NO-ERROR.
        IF AVAIL cta_ctbl THEN
           ASSIGN tt.DESC_conta = cta_ctbl.DES_tit_ctbl.
    END.

    FOR EACH tt WHERE 
             tt.cc <> '' NO-LOCK.
        FIND FIRST ccusto WHERE 
                   ccusto.cod_ccusto = tt.cc NO-LOCK NO-ERROR.

        IF AVAIL ccusto THEN 
           ASSIGN tt.desc_cc = ccusto.des_tit_ctbl
                  tt.ccusto_gerencial = tt.ccusto_gerencial + "-" + ccusto.des_tit_ctbl.
    END. 

END PROCEDURE.

PROCEDURE pi-BuscarApropFolhaImp:
/******************************************************************************
 busca as contas contabeis de titulos de folha de pagamento e impostos retidos
 regra: esses titulos devido a forma de implanta‡Æo dentro do sistema precisam
 buscar a contrapartida diretamente do movimento de baixa
 ******************************************************************************/

    DEFINE VARIABLE valor AS DECIMAL     NO-UNDO.
    
    DEFINE INPUT  PARAMETER rowidMovtoTitAP                 AS ROWID                                                NO-UNDO.
    DEFINE INPUT  PARAMETER pDataContaCorrente              LIKE movto_cta_corren.dat_transacao                     NO-UNDO.
    DEFINE INPUT  PARAMETER p_ind_fluxo_movto_cta_corren    LIKE movto_cta_corren.ind_fluxo_movto_cta_corren        NO-UNDO.
    DEFINE INPUT  PARAMETER p_val_movto_ap                  LIKE bf_movto_tit_ap_03.val_movto_ap                    NO-UNDO.
    DEFINE INPUT  PARAMETER p_cod_cta_corren                LIKE movto_cta_corren.cod_cta_corren                    NO-UNDO.
    DEFINE INPUT  PARAMETER p_num_seq_movto_cta_corren      LIKE movto_cta_corren.num_seq_movto_cta_corren          NO-UNDO.
    DEFINE INPUT  PARAMETER p_num_id_movto_cta_corren       LIKE movto_cta_corren.num_id_movto_cta_corren           NO-UNDO.
    
    FIND bf_movto_tit_ap_03 WHERE 
         ROWID(bf_movto_tit_ap_03) = RowidMovtoTitAp NO-LOCK NO-ERROR.
    FIND FIRST bf_tit_ap_05 OF bf_movto_tit_ap_03 NO-LOCK NO-ERROR.
    FIND FIRST fornecedor WHERE 
               fornecedor.cdn_fornecedor = bf_tit_ap_05.cdn_fornecedor NO-LOCK NO-ERROR.
    IF AVAIL fornecedor THEN
       FIND FIRST grp_fornec where
                  grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
    
    FOR EACH aprop_ctbl_ap OF bf_movto_tit_ap_03 WHERE
             (aprop_ctbl_ap.ind_natur_lancto_ctbl = 'cr' AND p_ind_fluxo_movto_cta_corren = 'ent' ) OR
             (aprop_ctbl_ap.ind_natur_lancto_ctbl = 'db' AND p_ind_fluxo_movto_cta_corren = 'sai' ) NO-LOCK.

        ASSIGN valor = aprop_ctbl_ap.val_aprop_ctbl.
        IF aprop_ctbl_ap.cod_indic_econ = 'dolar' THEN DO:
           FIND FIRST val_aprop_ctbl_ap OF aprop_ctbl_ap WHERE 
                      val_aprop_ctbl_ap.cod_finalid_econ = 'corrente' NO-LOCK NO-ERROR.
            IF AVAIL val_aprop_ctbl_ap THEN 
               ASSIGN valor = val_aprop_ctbl_ap.val_aprop_ctbl.
            ELSE
               ASSIGN valor = aprop_ctbl_ap.val_aprop_ctbl.
         END.

         CREATE tt.                                                                                                                                                   
         ASSIGN tt.cod_empresa       = aprop_ctbl_ap.cod_empresa                                                                                                             
                tt.cod_estab         = aprop_ctbl_ap.cod_estab                                                                                                               
                tt.cod_emitente      = IF AVAIL bf_tit_ap_05 THEN string(bf_tit_ap_05.cdn_fornecedor) ELSE '0'                                                                          
                tt.desc_emitente     = IF AVAIL fornecedor THEN fornecedor.nom_pessoa  ELSE ''                                                                                   
                tt.data              = pDataContaCorrente                                                                                                          
                tt.conta_contabil    = aprop_ctbl_ap.cod_cta_ctbl                                                                                                           
                tt.valor             = IF p_ind_fluxo_movto_cta_corren = 'ent' THEN valor  ELSE  valor * -1     
                tt.conta_corrente    = p_cod_cta_corren                                                                                                      
                tt.base              = '10' 
                tt.origem            = 'Contas a Pagar - FP (Imp.Retido)' 
                tt.cod_modulo        = 'APB'
                tt.tipo              = p_ind_fluxo_movto_cta_corren                                                                                           
                tt.cc                = aprop_ctbl_ap.cod_ccusto
                tt.ccusto_gerencial  = tt.cc
                tt.sequencia         = p_num_seq_movto_cta_corren
                tt.id_movto_corren   = p_num_id_movto_cta_corren
                tt.grupo_emitente    =  IF AVAIL grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + grp_fornec.des_grp_fornec  ELSE ''
                tt.historico         = replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
                tt.historico         = replace(movto_cta_corren.des_histor_movto_cta_corren,chr(10),";").
    END.
END PROCEDURE.

PROCEDURE pi-BuscarApropNormal:
/******************************************************************************
 busca as contas contabeis de titulos digitados diretamente no contas a pagar, 
 pegando a conta de debito referente ao movimento de implanta‡Æo do titulo.
 caso o titulo seja substituido chama a procedure para tratamento de titulos
 substituidos.
 ******************************************************************************/
    
    DEFINE INPUT  PARAMETER pDataContaCorrente              LIKE movto_cta_corren.dat_transacao                     NO-UNDO.
    DEFINE INPUT  PARAMETER p_ind_fluxo_movto_cta_corren    LIKE movto_cta_corren.ind_fluxo_movto_cta_corren        NO-UNDO.
    DEFINE INPUT  PARAMETER p_val_movto_ap                  LIKE movto_tit_ap.val_movto_ap                          NO-UNDO.
    DEFINE INPUT  PARAMETER p_val_juros                     LIKE movto_tit_ap.val_juros                             NO-UNDO.
    DEFINE INPUT  PARAMETER p_val_desconto                  LIKE movto_tit_ap.val_desconto                          NO-UNDO.
    DEFINE INPUT  PARAMETER p_cod_cta_corren                LIKE movto_cta_corren.cod_cta_corren                    NO-UNDO.
    DEFINE INPUT  PARAMETER p_num_seq_movto_cta_corren      LIKE movto_cta_corren.num_seq_movto_cta_corren          NO-UNDO.
    DEFINE INPUT  PARAMETER p_num_id_movto_cta_corren       LIKE movto_cta_corren.num_id_movto_cta_corren           NO-UNDO.
    DEFINE INPUT  PARAMETER p_rowidTitAp                    AS ROWID                                                NO-UNDO.
    
    DEFINE VARIABLE dIndice     AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE valor       AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE valorInd    AS DECIMAL     NO-UNDO.
    
    
    
    FIND  bf_tit_ap_01
        WHERE ROWID(bf_tit_ap_01) = p_rowidTitAp NO-LOCK NO-ERROR.
    FIND FIRST fornecedor 
        WHERE  fornecedor.cdn_fornecedor = bf_tit_ap_01.cdn_fornecedor NO-LOCK NO-ERROR.
    IF AVAIL fornecedor THEN
       FIND FIRST grp_fornec where
                  grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
    FIND FIRST bf_movto_tit_ap_01 OF bf_tit_ap_01                                                                                                         
        WHERE bf_movto_tit_ap_01.ind_trans_ap_abrev = 'IMPL'                                                                                        
        OR    bf_movto_tit_ap_01.ind_trans_ap_abrev  = 'SBND'  NO-LOCK NO-ERROR.                                                                    
    
    /*verifica a diferenca de valor entra a baixa e a implantacao e cria indice proporcional                                                     
       para tratar valores retirados ou acrescidos ap¢s a implantacao do titulo*/        
    ASSIGN  valorInd = p_val_movto_ap + p_val_juros - p_val_desconto.
    IF bf_movto_tit_ap_01.val_movto_ap <> valorInd THEN DO:                 
       ASSIGN dIndice = bf_movto_tit_ap_01.val_movto_ap / valorInd.         
    END.                                                                                                                                         
    ELSE                                                                                                                                         
       ASSIGN dIndice = 1.  
    
    
    FIND bf_tit_ap_02 OF bf_movto_tit_ap_01 NO-LOCK NO-ERROR.
    IF  bf_movto_tit_ap_01.ind_trans_ap_abrev  = 'SBND' THEN DO:
        RUN pi-BuscarTitulosSubstituidos(rowid(bf_tit_ap_02),
                                        pDataContaCorrente,
                                        p_ind_fluxo_movto_cta_corren,                                       
                                        (p_val_movto_ap + p_val_juros - p_val_desconto), 
                                        p_cod_cta_corren,  
                                        p_num_seq_movto_cta_corren, 
                                        p_num_id_movto_cta_corren).
        NEXT.
    END.
    
    FOR EACH aprop_ctbl_ap OF bf_movto_tit_ap_01 NO-LOCK                                                                                                                   
         WHERE  (p_ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'cr')                                                  
          OR  (p_ind_fluxo_movto_cta_corren   = 'sai' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'db') :
        IF aprop_ctbl_ap.cod_indic_econ = 'dolar' THEN DO:
            FIND FIRST val_aprop_ctbl_ap OF aprop_ctbl_ap WHERE 
                       val_aprop_ctbl_ap.cod_finalid_econ = 'corrente' NO-LOCK NO-ERROR.
            IF AVAIL val_aprop_ctbl_ap THEN 
               ASSIGN valor = val_aprop_ctbl_ap.val_aprop_ctbl / dIndice.
            ELSE
               ASSIGN valor = aprop_ctbl_ap.val_aprop_ctbl / dIndice.
         END.
         ELSE DO:
             ASSIGN valor = aprop_ctbl_ap.val_aprop_ctbl / dIndice.
         END.
         CREATE tt.                                                                                                                                                   
         ASSIGN tt.cod_empresa       = aprop_ctbl_ap.cod_empresa                                                                                                             
                tt.cod_estab         = aprop_ctbl_ap.cod_estab                                                                                                               
                tt.cod_emitente      = IF AVAIL bf_tit_ap_01 THEN string(bf_tit_ap_01.cdn_fornecedor) ELSE '0'                                                                          
                tt.desc_emitente     = IF AVAIL fornecedor THEN fornecedor.nom_pessoa  ELSE ''                                                                                  
                tt.data              = pDataContaCorrente                                                                                                         
                tt.conta_contabil    = aprop_ctbl_ap.cod_cta_ctbl                                                                                                           
                tt.valor             = IF p_ind_fluxo_movto_cta_corren = 'ent' THEN valor  ELSE  valor * -1     
                tt.conta_corrente    = p_cod_cta_corren                                                                                                      
                tt.base              = '10' 
                tt.origem            = 'Contas a Pagar' 
                tt.cod_modulo        = 'APB'
                tt.tipo              = p_ind_fluxo_movto_cta_corren                                                                                           
                tt.cc                = aprop_ctbl_ap.cod_ccusto
                tt.ccusto_gerencial  = tt.cc
                tt.sequencia         = p_num_seq_movto_cta_corren
                tt.id_movto_corren   = p_num_id_movto_cta_corren
                tt.num_id_tit        = IF AVAIL bf_tit_ap_01 THEN bf_tit_ap_01.num_id_tit_ap ELSE 0
                tt.cod_tit           = IF AVAIL bf_tit_ap_01 THEN bf_tit_ap_01.cod_tit_ap ELSE ''
                tt.grupo_emitente    = IF AVAIL grp_fornec THEN STRING(fornecedor.cod_grp_fornec) + "-" + grp_fornec.des_grp_fornec  ELSE ''
                tt.historico         = REPLACE(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
                tt.historico         = REPLACE(movto_cta_corren.des_histor_movto_cta_corren,chr(10),";").
    END.
END PROCEDURE.

PROCEDURE pi-BuscarNotasRateioEms2:
/******************************************************************************
 busca as contas contabeis diretamente do estoque a partir dos parametros do 
 documento de entrada.
 Considera o tipo de controle do item para busca do lan‡amento de entrada ou 
 saida do estoque.
******************************************************************************/
    DEFINE INPUT  PARAMETER pSerie       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pDocumento   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodEmitente AS INT         NO-UNDO.
    DEFINE INPUT  PARAMETER pParcela     AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pRowid       AS ROWID       NO-UNDO.

    DEFINE VARIABLE dTotalBreak          AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dTotalGeral          AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE cConta               AS CHARACTER   NO-UNDO FORMAT 'x(12)'.
    DEFINE VARIABLE cCC                  AS CHARACTER   NO-UNDO.
    
    FIND FIRST ems2ima.dupli-apagar 
         WHERE ems2ima.dupli-apagar.serie-docto     = pSerie
         AND   ems2ima.dupli-apagar.nro-docto       = pDocumento
         AND   ems2ima.dupli-apagar.cod-emitente    = pCodEmitente
         AND   ems2ima.dupli-apagar.parcela         = pParcela
        NO-LOCK NO-ERROR.
    IF AVAIL ems2ima.dupli-apagar THEN DO:
       FIND FIRST ems2ima.docum-est OF ems2ima.dupli-apagar NO-LOCK NO-ERROR.
       
       IF AVAIL ems2ima.docum-est THEN
          ASSIGN pRowid = ROWID(ems2ima.docum-est).
       ELSE
          ASSIGN pRowid = ? .
       FIND FIRST ttApropItem
           WHERE ttApropItem.rRowidNota = pRowid NO-LOCK NO-ERROR.
       IF NOT AVAIL ttApropItem AND pRowid <> ? THEN DO:
          FOR EACH ems2ima.movto-estoq OF ems2ima.docum-est NO-LOCK.
              FIND FIRST ems2ima.ITEM OF ems2ima.movto-estoq NO-LOCK NO-ERROR.
              IF AVAIL ITEM THEN DO:
                  IF ITEM.tipo-contr = 2 /*total*/ THEN DO:
                     IF ems2ima.movto-estoq.tipo-trans <> 1 THEN 
                        NEXT.

                     ASSIGN cConta = ems2ima.movto-estoq.ct-saldo
                            cCC    = ems2ima.movto-estoq.sc-saldo.
                  END.
                  ELSE DO:
                    IF ITEM.tipo-contr <> 2  THEN DO: 
                       IF ems2ima.movto-estoq.tipo-trans = 1 THEN 
                          NEXT.

                       ASSIGN cConta = ems2ima.movto-estoq.ct-codigo
                              cCC    = ems2ima.movto-estoq.sc-codigo.
                    END.
                  END.
              END.
              ELSE DO:
                    ASSIGN cConta = ""
                           cCC    = "".
    
              END.
              CREATE ttApropItem.                                                         
              ASSIGN ttApropItem.rRowidNota = ROWID(ems2ima.docum-est)                            
                     ttApropItem.conta      = cConta                   
                     ttApropItem.cc         = cCC
                     ttApropItem.itCodigo   = ems2ima.movto-estoq.it-codigo                       
                     ttApropItem.codRefer   = ems2ima.movto-estoq.cod-refer                       
                     ttApropItem.seq        = ems2ima.movto-estoq.sequen-nf                       
                     ttApropItem.valorMat   = ems2ima.movto-estoq.valor-mat-m[1]                  
                     ttApropItem.valorNf    = ems2ima.movto-estoq.valor-nota.                     
          END. 
       END.
       FIND FIRST ttApropRateio
           WHERE ttApropRateio.rowidNota = rRowidNota NO-LOCK NO-ERROR.
       IF NOT AVAIL ttApropRateio AND pRowid <> ? THEN DO: 
          /*calcula total para futura utilizacao no percentual*/
          FOR EACH ttApropItem
              WHERE ttAPropItem.rRowidNota = pRowid:
              ASSIGN dTotalGeral = dTotalGeral + ttApropItem.valorNf.
          END.
          
          FOR EACH ttApropItem
              WHERE ttAPropItem.rRowidNota = pRowid BREAK BY ttApropItem.conta BY ttApropItem.cc:
              ASSIGN dTotalBreak = dTotalBreak + ttAPropItem.valorNf.
              IF LAST-OF(ttApropItem.conta) AND LAST-OF(ttApropItem.cc) THEN DO:
                 CREATE ttApropRateio.
                 ASSIGN ttApropRateio.rowidNota   = ROWID(ems2ima.docum-est)
                        ttApropRateio.conta       = ttApropItem.conta
                        ttApropRateio.cc          = ttAPropItem.cc
                        ttApropRateio.valor       = dTotalBreak
                        ttApropRateio.perc        = dTotalBreak / dTotalGeral /*(docum-est.tot-valor + docum-est.icm-complem)*/
                        dTotalBreak               = 0
                        ttApropRateio.documento   = ems2ima.docum-est.nro-docto 
                        ttApropRateio.serie       = ems2ima.docum-est.serie-docto
                        ttApropRateio.codEmitente = ems2ima.docum-est.cod-emitente
                        ttApropRateio.natOperacao = ems2ima.docum-est.nat-Operacao                                                       
                        ttApropRateio.data        = ems2ima.docum-est.dt-trans.
              END.
          END.
       END.
    END.

END PROCEDURE.

PROCEDURE pi-BuscarNotasRateioEms2Aux:
/******************************************************************************
 busca as contas contabeis diretamente do estoque a partir dos parametros do 
 documento de entrada.
 Considera o tipo de controle do item para busca do lan‡amento de entrada ou 
 saida do estoque.
******************************************************************************/
    DEFINE INPUT  PARAMETER pSerie       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pDocumento   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodEmitente AS INT         NO-UNDO.
    DEFINE INPUT  PARAMETER pParcela     AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pRowid       AS ROWID       NO-UNDO.

    DEFINE VARIABLE dTotalBreak          AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dTotalGeral          AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE cConta               AS CHARACTER   NO-UNDO FORMAT 'x(12)'.
    DEFINE VARIABLE cCC                  AS CHARACTER   NO-UNDO.
    
    FIND FIRST dbaux.dupli-apagar 
         WHERE dbaux.dupli-aPagar.serie-docto     = pSerie
         AND   dbaux.dupli-aPagar.nro-docto       = pDocumento
         AND   dbaux.dupli-aPagar.cod-emitente    = pCodEmitente
         AND   dbaux.dupli-aPagar.parcela         = pParcela
         NO-LOCK NO-ERROR.
    IF AVAIL dbaux.dupli-apagar THEN DO:
       FIND FIRST dbaux.docum-est OF dbaux.dupli-apagar NO-LOCK NO-ERROR.
       
       IF AVAIL dbaux.docum-est THEN
          ASSIGN pRowid = ROWID(dbaux.docum-est).
       ELSE
          ASSIGN pRowid = ? .
       FIND FIRST ttApropItem
           WHERE ttApropItem.rRowidNota = pRowid NO-LOCK NO-ERROR.
    
       IF NOT AVAIL ttApropItem AND pRowid <> ? THEN DO:
          
          FOR EACH dbaux.movto-estoq OF dbaux.docum-est NO-LOCK.
              FIND FIRST dbaux.item OF dbaux.movto-estoq NO-LOCK NO-ERROR.
              IF ITEM.tipo-contr = 2 /*total*/ THEN DO:
                 IF dbaux.movto-estoq.tipo-trans <> 1 THEN NEXT.

                 ASSIGN cConta = dbaux.movto-estoq.ct-saldo
                        cCC    = dbaux.movto-estoq.sc-saldo.
              END.
              ELSE DO:
                IF ITEM.tipo-contr <> 2  THEN DO: 
                   IF dbaux.movto-estoq.tipo-trans = 1 THEN NEXT.
                   
                   ASSIGN cConta = dbaux.movto-estoq.ct-codigo
                          cCC    = dbaux.movto-estoq.sc-codigo.
                END.
              END.
    
              CREATE ttApropItem.                                                         
              ASSIGN ttApropItem.rRowidNota = ROWID(dbaux.docum-est)                            
                     ttApropItem.conta      = cConta                   
                     ttApropItem.cc         = cCC
                     ttApropItem.itCodigo   = dbaux.movto-estoq.it-codigo                       
                     ttApropItem.codRefer   = dbaux.movto-estoq.cod-refer                       
                     ttApropItem.seq        = dbaux.movto-estoq.sequen-nf                       
                     ttApropItem.valorMat   = dbaux.movto-estoq.valor-mat-m[1]                  
                     ttApropItem.valorNf    = dbaux.movto-estoq.valor-nota.                     
          END. 
       END.
       FIND FIRST ttApropRateio
           WHERE ttApropRateio.rowidNota = rRowidNota NO-LOCK NO-ERROR.
       IF NOT AVAIL ttApropRateio AND pRowid <> ? THEN DO: 
          /*calcula total para futura utilizacao no percentual*/
          FOR EACH ttApropItem
              WHERE ttAPropItem.rRowidNota = pRowid:
              ASSIGN dTotalGeral = dTotalGeral + ttApropItem.valorNf.
          END.
          
          FOR EACH ttApropItem
              WHERE ttAPropItem.rRowidNota = pRowid BREAK BY ttApropItem.conta BY ttApropItem.cc:
              ASSIGN dTotalBreak = dTotalBreak + ttAPropItem.valorNf.
              IF LAST-OF(ttApropItem.conta) AND LAST-OF(ttApropItem.cc) THEN DO:
                 CREATE ttApropRateio.
                 ASSIGN ttApropRateio.rowidNota   = rRowidNota
                        ttApropRateio.conta       = ttApropItem.conta
                        ttApropRateio.cc          = ttAPropItem.cc
                        ttApropRateio.valor       = dTotalBreak
                        ttApropRateio.perc        = dTotalBreak / dTotalGeral /*(docum-est.tot-valor + docum-est.icm-complem)*/
                        dTotalBreak               = 0
                        ttApropRateio.documento   = dbaux.docum-est.nro-docto 
                        ttApropRateio.serie       = dbaux.docum-est.serie-docto
                        ttApropRateio.codEmitente = dbaux.docum-est.cod-emitente
                        ttApropRateio.natOperacao = dbaux.docum-est.nat-Operacao                                                       
                        ttApropRateio.data        = dbaux.docum-est.dt-trans .
              END.
          END.
       END.
    END.
END PROCEDURE.



PROCEDURE pi-BuscarTitulosSubstituidos:
/******************************************************************************
 busca os titulos que foram substituidos pelo movimento de substitui‡Æo passado
 como parametro e faz o tratamento da origem do titulo conforme foi feito na 
 procedure buscarDados
******************************************************************************/
    
    DEFINE INPUT  PARAMETER rowidTitAP                      AS ROWID                                                     NO-UNDO.
    DEFINE INPUT  PARAMETER pDataContaCorrente              LIKE ems5.movto_cta_corren.dat_transacao                     NO-UNDO.
    DEFINE INPUT  PARAMETER p_ind_fluxo_movto_cta_corren    LIKE ems5.movto_cta_corren.ind_fluxo_movto_cta_corren        NO-UNDO.
    DEFINE INPUT  PARAMETER p_val_movto_ap                  LIKE bf_movto_tit_ap_02.val_movto_ap                         NO-UNDO.
    DEFINE INPUT  PARAMETER p_cod_cta_corren                LIKE ems5.movto_cta_corren.cod_cta_corren                    NO-UNDO.
    DEFINE INPUT  PARAMETER p_num_seq_movto_cta_corren      LIKE ems5.movto_cta_corren.num_seq_movto_cta_corren          NO-UNDO.
    DEFINE INPUT  PARAMETER p_num_id_movto_cta_corren       LIKE ems5.movto_cta_corren.num_id_movto_cta_corren           NO-UNDO.
    
    DEFINE VARIABLE rRowidNota AS ROWID  NO-UNDO.
    
    FIND bf_tit_ap_03
        WHERE ROWID(bf_tit_ap_03) = rowidTitAp NO-LOCK NO-ERROR.
    
    FOR EACH bf_movto_tit_ap_02 NO-LOCK 
            WHERE ind_trans_ap_abrev        = 'BXSB'
            AND   bf_movto_tit_ap_02.num_fatur_ap = bf_tit_ap_03.num_fatur_ap.
        FIND FIRST bf_tit_ap_04 OF bf_movto_tit_ap_02 NO-LOCK NO-ERROR.
        FIND FIRST fornecedor
             WHERE fornecedor.cdn_fornecedor = bf_tit_ap_04.cdn_fornecedor  NO-LOCK NO-ERROR. 
        IF AVAIL fornecedor THEN
           FIND FIRST grp_fornec where
           grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
        IF bf_tit_ap_04.ind_origin_tit_ap <> "REC" /*origem recebimento*/  THEN DO:
           IF SUBSTR(bf_tit_ap_04.cod_tit_ap,1,2) = 'hr' THEN DO : /*caso o titulo seja da folha*/   
              RUN pi-BuscarApropFolhaImp( ROWID(bf_movto_tit_ap_02),
                                          pDataContaCorrente,
                                          p_ind_fluxo_movto_cta_corren,
                                          bf_movto_tit_ap_02.val_movto_ap,
                                          p_cod_cta_corren,
                                          p_num_seq_movto_cta_corren,                 
                                          p_num_id_movto_cta_corren ).
           END.
           ELSE DO:
              RUN pi-BuscarApropNormal(pDataContaCorrente,                 
                                       p_ind_fluxo_movto_cta_corren,    
                                       bf_movto_tit_ap_02.val_movto_ap,                      
                                       bf_movto_tit_ap_02.val_juros,                         
                                       bf_movto_tit_ap_02.val_desconto,                      
                                       p_cod_cta_corren,                
                                       p_num_seq_movto_cta_corren,      
                                       p_num_id_movto_cta_corren,       
                                       ROWID(bf_tit_ap_04)).             
           END.
        END.
        ELSE DO:
           CASE i-ep-codigo-usuario:
               WHEN '5' THEN
                    RUN pi-BuscarNotasRateioEms2(bf_tit_ap_04.cod_ser_docto,
                                                 bf_tit_ap_04.cod_tit_ap,
                                                 bf_tit_ap_04.cdn_fornecedor,
                                                 bf_tit_ap_04.cod_parcela, 
                                                 OUTPUT rRowidNota).
               WHEN '1' THEN
                    RUN pi-BuscarNotasRateioEms2Aux(bf_tit_ap_04.cod_ser_docto,
                                                    bf_tit_ap_04.cod_tit_ap,
                                                    bf_tit_ap_04.cdn_fornecedor,
                                                    bf_tit_ap_04.cod_parcela, 
                                                    OUTPUT rRowidNota).
           END CASE.
    
           FOR EACH ttApropRateio 
               WHERE ttApropRateio.rowidNota = rRowidNota .
               FIND FIRST emitente WHERE 
                          emitente.cod-emitente = ttApropRateio.codEmitente
                          NO-LOCK NO-ERROR.
               FIND FIRST fornecedor WHERE 
                          fornecedor.cdn_fornec = emitente.cod-emitente
                          NO-LOCK NO-ERROR.
               FIND FIRST grp_fornec OF fornecedor NO-LOCK NO-ERROR.
               CREATE tt.                                                                                                                                                   
               ASSIGN tt.cod_empresa      = IF AVAIL bf_tit_ap_04 
                                            THEN bf_tit_ap_04.cod_empresa
                                            ELSE ''                                                                                                             
                      tt.cod_estab        = IF AVAIL bf_tit_ap_04 
                                            THEN bf_tit_ap_04.cod_estab
                                            ELSE ''                                                                                                             
                      tt.cod_emitente     = STRING(ttApropRateio.codEmitente)
                      tt.desc_emitente    = IF AVAIL emitente 
                                            THEN emitente.nome-emit  
                                            ELSE ''                                                                                    
                      tt.data             = pDataContaCorrente                                                                                                          
                      tt.conta_contabil   = ttApropRateio.conta                                                                                                           
                      tt.valor            = IF p_ind_fluxo_movto_cta_corren = 'ent' 
                                            THEN bf_movto_tit_ap_02.val_movto_ap * ttApropRateio.perc 
                                            ELSE bf_movto_tit_ap_02.val_movto_ap * -1 * ttApropRateio.perc     
                      tt.conta_corrente   = p_cod_cta_corren                                                                                                      
                      tt.base             = '10'
                      tt.origem           = 'Contas a Pagar - REC'  
                      tt.cod_modulo       =  'APB'
                      tt.tipo             = p_ind_fluxo_movto_cta_corren                                                                                           
                      tt.cc               = ttApropRateio.cc
                      tt.ccusto_gerencial = tt.cc
                      tt.sequencia        = p_num_seq_movto_cta_corren
                      tt.id_movto_corren  = p_num_id_movto_cta_corren
                      tt.cod_tit          = bf_movto_tit_ap_02.cod_tit_ap
                      tt.grupo_emitente   = IF AVAIL fornecedor 
                                            THEN STRING(fornecedor.cod_grp_fornec) + "-" + grp_fornec.des_grp_fornec
                                            ELSE ''
                      tt.historico        = REPLACE(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
                      tt.historico        = REPLACE(movto_cta_corren.des_histor_movto_cta_corren,chr(10),";").
           END.
        END.
    END.

END PROCEDURE.



PROCEDURE pi-ClassificarDados:
    DEFINE VARIABLE cRetorno            AS CHARACTER   NO-UNDO FORMAT 'x(100)'.
    DEFINE VARIABLE cGrupoRetorno       AS CHARACTER   NO-UNDO FORMAT 'x(100)'.
    DEFINE VARIABLE iCodParamDesemb     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE LOG_desconsiderar   AS LOGICAL     NO-UNDO.
    FOR EACH tt:
        RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(tt.data,"99/99/9999") ).

        RUN pi-BuscarClassCcusto('classificacao',
                                  IF tt.cod_modulo = 'cmg' THEN YES ELSE NO,
                                  ENTRY(1,tt.grupo,"-"),
                                  tt.cod_emitente,
                                  tt.conta_contabil,
                                  tt.cc,
                                  OUTPUT cRetorno,
                                  OUTPUT cGrupoRetorno,
                                  OUTPUT icodParamDesemb,
                                  OUTPUT LOG_desconsiderar).
        
        
         ASSIGN tt.classificacao     = cRetorno
                tt.cod_param_desemb  = iCodParamDesemb
                tt.LOG_desconsiderar = LOG_desconsiderar
                tt.grupo             = cGrupoRetorno.

         RUN pi-BuscarClassCcusto('ccusto',
                                   IF tt.cod_modulo = 'cmg' THEN YES ELSE NO,
                                   ENTRY(1,tt.grupo,"-"),
                                   tt.cod_emitente,
                                   tt.conta_contabil,
                                   tt.cc,
                                   OUTPUT cRetorno,
                                   OUTPUT cGrupoRetorno,
                                   OUTPUT icodParamDesemb,
                                   OUTPUT LOG_desconsiderar).
         IF cRetorno <> '' THEN
            ASSIGN tt.ccusto_gerencial        = cRetorno
                   tt.cod_param_desemb_ccusto = iCodParamDesemb. 
    END.
END.


PROCEDURE pi-BuscarClassCcusto:
    DEFINE INPUT  PARAMETER pTipo            AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCaixaBanco      AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pGrupoEmitente   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pEmitente        AS CHAR        NO-UNDO FORMAT 'x(8)'.
    DEFINE INPUT  PARAMETER pConta           AS CHARACTER   NO-UNDO FORMAT 'x(8)'.
    DEFINE INPUT  PARAMETER pCCusto          AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER cRetorno         AS CHAR FORMAT 'x(50)'.
    DEFINE OUTPUT PARAMETER cGrupoRetorno    AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
    DEFINE OUTPUT PARAMETER icodParamDesemb  AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER LOG_Desconsiderar AS LOGICAL    NO-UNDO.

    CASE pTipo:
        WHEN 'classificacao' THEN DO:
             IF pCaixaBanco = YES THEN DO: /* modulo caixa e bancos*/
                FIND FIRST param_desembolso WHERE 
                           param_desembolso.log_caixa_banco = pCaixaBanco         
                 AND pEmitente      >= param_desembolso.tipo_trans_cx_ini        
                 AND pEmitente      <= param_desembolso.tipo_trans_cx_fim 
                 AND   pConta       >= param_desembolso.cod_cta_ctbl_ini         
                 AND   pConta       <= param_desembolso.cod_cta_ctbl_fim         
                 AND   pCCusto      >= param_desembolso.cod_cCusto_ini           
                 AND   pCCusto      <= param_desembolso.cod_cCusto_fim 
                 AND   cod_tipo_param   = 2
                 USE-INDEX ind_prioridade
                 NO-LOCK NO-ERROR.                                            
                 IF AVAIL param_desembolso THEN DO:   
                    FIND FIRST agrup_desemb
                        WHERE agrup_desemb.cod_agrup_desemb = param_desembolso.cod_agrup_desemb
                        NO-LOCK NO-ERROR.
                    FIND FIRST grupos_desemb OF agrup_desemb NO-LOCK NO-ERROR.
                    ASSIGN cRetorno = IF AVAIL agrup_desemb THEN agrup_desemb.desc_agrup_desemb ELSE string(param_desembolso.cod_agrup_desemb).
                    ASSIGN icodParamDesemb = param_desembolso.cod_param_desembolso.
                    ASSIGN LOG_desconsiderar = IF AVAIL agrup_desemb THEN agrup_desemb.LOG_desconsiderar ELSE NO.
                    ASSIGN cGrupoRetorno     = IF AVAIL grupos_desemb THEN DESC_grupo_desemb ELSE ''.
                 END.
                 ELSE DO:
                    FIND FIRST param_desembolso                                                                                                              
                     WHERE param_desembolso.LOG_caixa_banco = pCaixaBanco
                     AND   param_desembolso.tipo_trans_cx_ini = ''
                     AND   pConta       >= param_desembolso.cod_cta_ctbl_ini                                                                                 
                     AND   pConta       <= param_desembolso.cod_cta_ctbl_fim                                                                                 
                     AND   pCCusto      >= param_desembolso.cod_cCusto_ini                                                                                   
                     AND   pCCusto      <= param_desembolso.cod_cCusto_fim                                                                                   
                     AND   cod_tipo_param   = 2                                                                                                              
                     USE-INDEX ind_prioridade                                                                                                                
                     NO-LOCK NO-ERROR.                                                                                                                       
                     IF AVAIL param_desembolso THEN DO:  
                        FIND FIRST agrup_desemb WHERE 
                                   agrup_desemb.cod_agrup_desemb = param_desembolso.cod_agrup_desemb NO-LOCK NO-ERROR.   
                        FIND FIRST grupos_desemb OF agrup_desemb NO-LOCK NO-ERROR.
                        ASSIGN cRetorno = IF AVAIL agrup_desemb THEN agrup_desemb.desc_agrup_desemb ELSE string(param_desembolso.cod_agrup_desemb).
                        ASSIGN icodParamDesemb   = param_desembolso.cod_param_desembolso.
                        ASSIGN LOG_desconsiderar = IF AVAIL agrup_desemb THEN agrup_desemb.LOG_desconsiderar ELSE NO.
                        ASSIGN cGrupoRetorno     = IF AVAIL grupos_desemb THEN DESC_grupo_desemb ELSE ''.
                     END.                                                                                                                                    
                     ELSE DO:
                        ASSIGN cRetorno = "Classifica‡Æo NÆo Encontrada".                                                                                    
                     END.
                 END.
             END.
             ELSE DO: /* modulo <>  de caixa e bancos */
                 /*busca primeiramente por emitente especifico*/
                 FIND FIRST param_desembolso                                      
                 WHERE param_desembolso.LOG_caixa_banco = pCaixaBanco         
                 AND pEmitente      = string(param_desembolso.cod_emitente)
                 AND   pConta       >= param_desembolso.cod_cta_ctbl_ini         
                 AND   pConta       <= param_desembolso.cod_cta_ctbl_fim         
                 AND   pCCusto      >= param_desembolso.cod_cCusto_ini           
                 AND   pCCusto      <= param_desembolso.cod_cCusto_fim 
                 AND   cod_tipo_param   = 2
                 USE-INDEX ind_prioridade
                 NO-LOCK NO-ERROR.                                            
                 IF AVAIL param_desembolso THEN DO:
                    FIND FIRST agrup_desemb
                        WHERE agrup_desemb.cod_agrup_desemb = param_desembolso.cod_agrup_desemb
                        NO-LOCK NO-ERROR.
                    FIND FIRST grupos_desemb OF agrup_desemb
                         NO-LOCK NO-ERROR.
                    ASSIGN cRetorno = IF AVAIL agrup_desemb THEN agrup_desemb.desc_agrup_desemb ELSE string(param_desembolso.cod_agrup_desemb).
                    ASSIGN icodParamDesemb = param_desembolso.cod_param_desembolso.
                    ASSIGN LOG_desconsiderar = IF AVAIL agrup_desemb THEN agrup_desemb.LOG_desconsiderar ELSE NO.
                    ASSIGN cGrupoRetorno     = IF AVAIL grupos_desemb THEN DESC_grupo_desemb ELSE ''.
                 END.
                 ELSE DO: /*busca por grupo de fornecedor especifico*/
                    FIND FIRST param_desembolso                                                                                                    
                    WHERE param_desembolso.LOG_caixa_banco = pCaixaBanco  
                    AND param_desembolso.cod_emitente = 0
                    AND pGrupoEmitente      = param_desembolso.cod_grupo_emitente
                    AND   pConta            >= param_desembolso.cod_cta_ctbl_ini                                                                        
                    AND   pConta            <= param_desembolso.cod_cta_ctbl_fim                                                                        
                    AND   pCCusto           >= param_desembolso.cod_cCusto_ini                                                                          
                    AND   pCCusto           <= param_desembolso.cod_cCusto_fim                                                                          
                    AND   cod_tipo_param    = 2                                                                                                     
                    USE-INDEX ind_prioridade                                                                                                       
                    NO-LOCK NO-ERROR.                                                                                                              
                    IF AVAIL param_desembolso THEN DO:  
                       FIND FIRST agrup_desemb                                                                                                     
                           WHERE agrup_desemb.cod_agrup_desemb = param_desembolso.cod_agrup_desemb                                                 
                           NO-LOCK NO-ERROR.     
                        FIND FIRST grupos_desemb OF agrup_desemb
                         NO-LOCK NO-ERROR.
                       ASSIGN cRetorno = IF AVAIL agrup_desemb THEN agrup_desemb.desc_agrup_desemb ELSE string(param_desembolso.cod_agrup_desemb). 
                       ASSIGN icodParamDesemb = param_desembolso.cod_param_desembolso.
                       ASSIGN LOG_desconsiderar = IF AVAIL agrup_desemb THEN agrup_desemb.LOG_desconsiderar ELSE NO.
                       ASSIGN cGrupoRetorno     = IF AVAIL grupos_desemb THEN DESC_grupo_desemb ELSE ''.
                    END.                                                                                                                           
                    ELSE DO: /*busca por faixa de conta e centro de custo*/ 
                       FIND FIRST param_desembolso                                                                                                    
                       WHERE param_desembolso.LOG_caixa_banco = pCaixaBanco 
                       AND param_desembolso.cod_emitente = 0
                       AND param_desembolso.cod_grupo_emitente = '' 
                       AND   pConta       >= param_desembolso.cod_cta_ctbl_ini                                                                        
                       AND   pConta       <= param_desembolso.cod_cta_ctbl_fim                                                                        
                       AND   pCCusto      >= param_desembolso.cod_cCusto_ini                                                                          
                       AND   pCCusto      <= param_desembolso.cod_cCusto_fim                                                                          
                       AND   cod_tipo_param   = 2                                                                                                     
                       USE-INDEX ind_prioridade                                                                                                       
                       NO-LOCK NO-ERROR.                                                                                                              
                       IF AVAIL param_desembolso THEN DO:
                          FIND FIRST agrup_desemb                                                                                                     
                              WHERE agrup_desemb.cod_agrup_desemb = param_desembolso.cod_agrup_desemb                                                 
                              NO-LOCK NO-ERROR.  
                          FIND FIRST grupos_desemb OF agrup_desemb
                              NO-LOCK NO-ERROR.
                          ASSIGN cRetorno = IF AVAIL agrup_desemb THEN agrup_desemb.desc_agrup_desemb ELSE string(param_desembolso.cod_agrup_desemb). 
                          ASSIGN icodParamDesemb = param_desembolso.cod_param_desembolso.
                          ASSIGN LOG_desconsiderar = IF AVAIL agrup_desemb THEN agrup_desemb.LOG_desconsiderar ELSE NO.
                          ASSIGN cGrupoRetorno     = IF AVAIL grupos_desemb THEN DESC_grupo_desemb ELSE ''.

                       END.                                                                                                                           
                       ELSE DO:                                                                         
                          ASSIGN cRetorno = "Classifica‡Æo NÆo Encontrada".
                       END.                                                                                                                                                                                                                                                       
                    END.
                 END.
             END.
        END.
        WHEN 'cCusto' THEN DO:
            IF pCaixaBanco THEN DO: /* modulo caixa e bancos*/
                FIND FIRST param_desembolso                                      
                 WHERE param_desembolso.LOG_caixa_banco = pCaixaBanco         
                 AND pEmitente      >= param_desembolso.tipo_trans_cx_ini        
                 AND pEmitente      <= param_desembolso.tipo_trans_cx_fim 
                 AND   pConta       >= param_desembolso.cod_cta_ctbl_ini         
                 AND   pConta       <= param_desembolso.cod_cta_ctbl_fim         
                 AND   pCCusto      >= param_desembolso.cod_cCusto_ini           
                 AND   pCCusto      <= param_desembolso.cod_cCusto_fim 
                 AND   cod_tipo_param   = 1
                 USE-INDEX ind_prioridade
                 NO-LOCK NO-ERROR.                                            
                 IF AVAIL param_desembolso THEN DO:   
                    FIND FIRST ccusto
                        WHERE ccusto.cod_ccusto = string(param_desembolso.cod_ccusto_gerencial)
                        NO-LOCK NO-ERROR.
                    ASSIGN cRetorno = IF AVAIL ccusto THEN ccusto.des_tit_ctbl ELSE string(param_desembolso.cod_ccusto_gerencial).
                    ASSIGN icodParamDesemb = param_desembolso.cod_param_desembolso.
                 END.
                 ELSE DO:
                    ASSIGN cRetorno = "".
                 END.
            END.
            ELSE DO:
                FIND FIRST param_desembolso                                                      
                     WHERE param_desembolso.LOG_caixa_banco = pCaixaBanco                        
                     AND pEmitente            = string(param_desembolso.cod_emitente)              
                     AND   pConta            >= param_desembolso.cod_cta_ctbl_ini                
                     AND   pConta            <= param_desembolso.cod_cta_ctbl_fim                
                     AND   pCCusto           >= param_desembolso.cod_cCusto_ini                  
                     AND   pCCusto           <= param_desembolso.cod_cCusto_fim                  
                     AND   cod_tipo_param    = 1                                                 
                     USE-INDEX ind_prioridade                                                    
                     NO-LOCK NO-ERROR.                                                           
                IF AVAIL param_desembolso THEN DO:                                                                   
                   FIND FIRST ccusto
                        WHERE ccusto.cod_ccusto = string(param_desembolso.cod_ccusto_gerencial)
                        NO-LOCK NO-ERROR.
                    ASSIGN cRetorno = IF AVAIL ccusto THEN ccusto.cod_ccusto + '-' + ccusto.des_tit_ctbl ELSE string(param_desembolso.cod_ccusto_gerencial).
                    ASSIGN icodParamDesemb = param_desembolso.cod_param_desembolso.
                END.
                ELSE DO:
                    ASSIGN cRetorno = "".
                END.
            END.
        END.
    END CASE.
END PROCEDURE.







