/**************************************************************
Programa: Desembolsorp.p
Objetivo: Buscar as contas contabeis para analise de gastos a 
partir dos movimentos de caixa e banco.
Dependendo da origem do titulo Ç necess†rio buscar as contas da
nota fiscal de entrada.
Autor:Tadeu Silva Parreiras
Data:29/10/2015
Regras Implementadas:
1- Apropriaá∆o de Valores de titulos digitados diretamente no
contas a pagar.
1.1 - Apropriaá∆o de Titulos de retená∆o de impostos
1.2 - Apropriaá∆o de Titulos vindos da integraá∆o com o HCM
1.3 - Apropriaá∆o de Titulos vindos de Titulos substituidos
2- Apropriaá∆o de Valores de titutos integrados do modulo
de recebimento.
2.1- Apropriaá∆o considerando itens de debito direto
2.2- Apropriaá∆o considerando itens de controle total
3- Apropriaá∆o de valores titulos vindos do contas a receber
no caso de desembolso


----------------------------------------------------------------
Acrescimo de hist¢rico da conta corrente 11/01/2016


***************************************************************/

/********************************************************DEFINIÄ«O DE VARIAVEIS*****************************************/
/*{utp\ut-glob.i} variaveis globais datasul*/
{ems5\esp\desembolso.i}
{utp/ut-glob.i}

DEFINE INPUT  PARAMETER pDtIni   AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER pDtFim   AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER pBase    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO FORMAT 'x(80)'.
DEFINE INPUT  PARAMETER lGerarLog AS LOGICAL     NO-UNDO.
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

 /*tabela temporaria que armazena a proporá∆o de valor por conta e centro de custo a ser apropriado no titulo pago*/   
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

DEFINE VARIABLE hLog AS HANDLE      NO-UNDO.

/*buffer do movto-tit-ap para buscar a apropriaá∆o da implantaá∆o do titulo*/
DEFINE BUFFER bf_movto_tit_ap_01 FOR ems5.movto_tit_ap.
DEFINE BUFFER bf_movto_tit_ap_02 FOR ems5.movto_tit_ap.
DEFINE BUFFER bf_movto_tit_ap_03 FOR ems5.movto_tit_ap.

DEFINE BUFFER bf_tit_ap_01  FOR ems5.tit_Ap.
DEFINE BUFFER bf_tit_ap_02  FOR ems5.tit_Ap.
DEFINE BUFFER bf_tit_ap_03  FOR ems5.tit_Ap.
DEFINE BUFFER bf_tit_ap_04  FOR ems5.tit_Ap.
DEFINE BUFFER bf_tit_ap_05  FOR ems5.tit_Ap.
/*DEFINE BUFFER bf_ttApropItem FOR ttApropItem .*/


/******************* fim definiá‰es variaveis *********************/

/***********BLOCO PRINCIPAL***************************************/
EMPTY TEMP-TABLE tt.
EMPTY TEMP-TABLE ttApropRateio.
EMPTY TEMP-TABLE ttApropItem.
RUN esporadicos/LOG02.p PERSISTENT SET hLog.
RUN arquivoSaida IN hLog('logDesembolso').
RUN gerarLog IN hLog(lGerarLog).
RUN pibuscarDAdos.
/*RUN gerarLog IN hLog(YES).*/
RUN piClassificarDados.
OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + pArquivo) APPEND NO-CONVERT.
 FOR EACH tt
     WHERE tt.valor <> 0:
     EXPORT DELIMITER "|" tt EXCEPT tt.rowidnota.
 END.
OUTPUT CLOSE.
RUN imprimirtxt IN hLog.

/**************FINAL BLOCO PRINCIPAL*****************************/
PROCEDURE piBuscarDados:
    /*
    A partir das datas passadas por parametro, identifica o modulo
    de origem da baixa e posteriomente o modulo de origem de implantaá∆o
    para buscar as contas cont†beis necess†rias 
    */
    DEFINE VARIABLE valor       AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE rRowidNota  AS ROWID       NO-UNDO.
    
    
    FOR EACH ems5.movto_cta_corren 
        WHERE movto_cta_corren.dat_transacao >= pDtIni
        AND   movto_cta_corren.dat_transacao <= pDtFim 
        AND   ind_tip_movto_cta_corren = 're' /*apenas movimentos realizados*/
        AND   movto_cta_corren.cod_tip_trans_cx <> '002' /*transferencias entre contas*/
        /*AND   movto_cta_corren.ind_fluxo_movto_cta_corren = 'sai'*/ 
        NO-LOCK:

        RUN incluirLog IN hLog('agrupamento Geral','inicio movto_cta_corren ========================= data:' + STRING(movto_cta_corren.dat_transacao) + "conta:" + movto_cta_corren.cod_cta_corren) .
        RUN incluirLog IN hLog('agrupamento Geral','nivel 1 - movto_cta_corren - valor:' +  string(val_movto_cta_corren) + ' sequencia: ' + STRING(movto_cta_corren.num_seq_movto_cta_corren)).
    
        FIND FIRST ems5.cta_corren OF movto_cta_corren NO-LOCK NO-ERROR.
    /*     IF NOT AVAIL cta_corren THEN DO:                                                                                                               */
    /*        RUN incluirLog IN hLog('agrupamento Geral','fim movto_cta_corren=====EMPRESA DA CONTA CORRENTE DIFERENTE DA CORRENTE===================='). */
    /*        NEXT.                                                                                                                                       */
    /*     END.                                                                                                                                           */
        CASE movto_cta_corren.cod_modul_dtsul:
            WHEN 'acr'  THEN DO:
               FOR EACH ems5.movto_tit_acr OF movto_cta_corren NO-LOCK:
                   FIND FIRST ems5.tit_acr OF movto_tit_acr NO-LOCK NO-ERROR.                                                                                                               
                   FIND FIRST emitente WHERE                                                                                                                                           
                        tit_acr.cdn_cliente =  emitente.cod-emitente NO-LOCK NO-ERROR. 
                   FIND FIRST ext-emitente OF emitente
                       NO-LOCK NO-ERROR.
                   IF AVAIL ext-emitente THEN
                      FIND FIRST ramo-ativ
                           WHERE ramo-ativ.cod-ramo-ativ = ext-emitente.cod-ramo-ativ NO-LOCK NO-ERROR.
    
    /*                MESSAGE AVAIL tit_acr                  */
    /*                    VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                   FOR EACH ems5.aprop_ctbl_acr OF movto_tit_acr                                                                                                                               
                       WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent'   AND aprop_ctbl_acr.ind_natur_lancto_ctbl = 'cr')                                                       
                        OR  (movto_cta_corren.ind_fluxo_movto_cta_corren   = 'sai'   AND aprop_ctbl_acr.ind_natur_lancto_ctbl = 'db') NO-LOCK :        
    /*                    MESSAGE 'entrei no acr para criar tt'  */
    /*                        VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                       CREATE tt.                                                                                                                                                         
                       ASSIGN         
                       tt.cod_modulo        = movto_cta_corren.cod_modul_dtsul
                       tt.cod_empresa       = aprop_ctbl_acr.cod_empresa                                                                                                                  
                       tt.cod_estab         = aprop_ctbl_acr.cod_estab                                                                                                                    
                       tt.cod_emitente      =  IF AVAIL tit_acr THEN string(tit_acr.cdn_cliente) ELSE  '0'                                                                                
                       tt.desc_emitente     =  IF AVAIL emitente THEN emitente.nome-emit ELSE ''                                                                                          
                       tt.data              =  movto_cta_corren.dat_transacao                                                                                                               
                       tt.conta_contabil    =  aprop_ctbl_acr.cod_cta_ctbl                                                                                                                
                       tt.valor             =  IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN  aprop_ctbl_acr.val_aprop_ctbl  ELSE  aprop_ctbl_acr.val_aprop_ctbl * -1       
                       tt.conta_corrente    =  movto_cta_corren.cod_cta_corren                                                                                                            
                       tt.base              =  pBase                                                                                                                                       
                       tt.origem            = 'Contas a Receber'                                                                                                                          
                       tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren                                                                                                 
                       tt.cc                = aprop_ctbl_acr.cod_ccusto
                       tt.ccusto_gerencial  = tt.cc
                       tt.sequencia         = movto_cta_corren.num_seq_movto_cta_corren
                       tt.id_movto_corren   = movto_cta_corren.num_id_movto_cta_corren
                       tt.grupo_emitente    = IF AVAIL ramo-ativ THEN string(ramo-ativ.cod-ramo-ativ) + '-' + ramo-ativ.descricao ELSE '' 
                       tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
                       tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(10),";"). 
                   END.
               END.
            END.
            WHEN 'apb'  THEN DO:
               FOR EACH ems5.movto_tit_ap OF movto_cta_corren
                   NO-LOCK:
                   RUN incluirLog IN hLog('agrupamento Geral','==inicio movto_tit_ap========================================').
                   FIND FIRST ems5.tit_ap OF movto_tit_ap NO-LOCK NO-ERROR. 
                   FIND FIRST ems5.espec_docto OF ems5.tit_ap NO-LOCK NO-ERROR.
                   RUN incluirLog IN hLog('agrupamento Geral','nivel 2-movto_tit_ap - valor:' +  string(movto_tit_ap.val_movto_ap) ).
                   FIND FIRST ems5.fornecedor                                                                                                                                              
                       WHERE tit_ap.cdn_fornecedor = fornecedor.cdn_fornecedor NO-LOCK NO-ERROR. 
                   IF AVAIL fornecedor THEN
                      FIND FIRST ems5.grp_fornec where
                       ems5.grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
                   
                   RUN incluirLog IN hLog('agrupamento Geral','nivel 2- achou titulo?' + string(AVAIL tit_ap)).
                   RUN incluirLog IN hLog('agrupamento Geral','nivel 2- achou emitente?' + string(AVAIL ems5.fornecedor)).
    
                   IF (AVAIL tit_ap AND tit_ap.ind_origin_tit_ap <> "REC" /*origem recebimento*/ ) OR NOT AVAIL tit_Ap  THEN DO:
                      RUN incluirLog IN hLog('agrupamento Geral','nivel 3- Origem NAO EH Recebimento ou Movimento Sem Titulo').
                      IF AVAIL tit_ap THEN DO:
                         IF SUBSTR(tit_ap.cod_tit_ap,1,2) = 'hr' OR espec_docto.ind_tip_espec_docto = 'imposto retido' THEN DO : /*caso o titulo seja da folha*/     
                            RUN piBuscarApropFolhaImp( ROWID(movto_tit_ap),
                                                    movto_cta_corren.dat_transacao,
                                                    movto_cta_corren.ind_fluxo_movto_cta_corren,
                                                    movto_tit_ap.val_movto_ap,
                                                    movto_cta_corren.cod_cta_corren,
                                                    movto_cta_corren.num_seq_movto_cta_corren,                 
                                                    movto_cta_corren.num_id_movto_cta_corren ).
                         END.                                                                                  
                         ELSE DO:     
                            RUN incluirLog IN hLog('agrupamento Geral','====inicio apropNormal - achou titulo?' + string(AVAIL tit_ap) + ' p========================================').
    
                            RUN piBuscarApropNormal(movto_cta_corren.dat_transacao,                            
                                                    movto_cta_corren.ind_fluxo_movto_cta_corren,               
                                                    movto_tit_ap.val_movto_ap,                                 
                                                    movto_tit_ap.val_juros,                                    
                                                    movto_tit_ap.val_desconto,                                 
                                                    movto_cta_corren.cod_cta_corren,                           
                                                    movto_cta_corren.num_seq_movto_cta_corren,                 
                                                    movto_cta_corren.num_id_movto_cta_corren,                  
                                                    ROWID(tit_ap)
                                                    ).                
                         END.                                                                                  
                      END.
                      ELSE DO:
                         RUN incluirLog IN hLog('agrupamento Geral','nivel 3- Lancamento com ORIGEM no AP, PEF ou Adiantamento:' + string(movto_tit_ap.val_movto_ap)).
                         FOR EACH ems5.antecip_pef_pend OF movto_tit_ap  NO-LOCK:
                             FIND FIRST ems5.fornecedor OF ems5.antecip_pef_pend NO-LOCK NO-ERROR.
                             IF AVAIL fornecedor THEN
                             FIND FIRST ems5.grp_fornec where
                                   ems5.grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
                             FOR EACH ems5.aprop_ctbl_pend_ap OF antecip_pef_pend
                                 NO-LOCK:
                                 CREATE tt.                                                                                                                                                   
                                 ASSIGN  
                                 tt.cod_modulo        = movto_cta_corren.cod_modul_dtsul
                                 tt.cod_empresa       = aprop_ctbl_pend_ap.cod_empresa                                                                                                             
                                 tt.cod_estab         = aprop_ctbl_pend_ap.cod_estab                                                                                                               
                                 tt.cod_emitente      = string(ems5.antecip_pef_pend.cdn_fornecedor)                                                                           
                                 tt.desc_emitente     =  IF AVAIL fornecedor THEN fornecedor.nom_pessoa  ELSE ''                                                                                   
                                 tt.data              =  movto_cta_corren.dat_transacao                                                                                                        
                                 tt.conta_contabil    =  aprop_ctbl_pend_ap.cod_cta_ctbl                                                                                                           
                                 tt.valor             = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN  movto_tit_ap.val_movto_ap ELSE movto_tit_ap.val_movto_ap * -1     
                                 tt.conta_corrente    =  movto_cta_corren.cod_cta_corren                                                                                                      
                                 tt.base              =  pBase                                                                                                                                 
                                 tt.origem            =  'Contas a Pagar - PEF'                                                                                                                     
                                 tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren                                                                                           
                                 tt.cc                = aprop_ctbl_pend_ap.cod_ccusto
                                 tt.ccusto_gerencial  = tt.cc
                                 tt.sequencia         = movto_cta_corren.num_seq_movto_cta_corren
                                 tt.id_movto_corren   = movto_cta_corren.num_id_movto_cta_corren
                                 tt.grupo_emitente    = IF AVAIL ems5.grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  ELSE ''
                                 tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
                                 tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(10),";")  .
                             END.
                         END.
                      END.
                   END.
                   ELSE DO:
                       RUN incluirLog IN hLog('agrupamento Geral','nivel 3 - Origem Recebimento').
                       CASE i-ep-codigo-usuario:
                           WHEN '5' THEN
                                RUN piBuscarNotasRateioEms2MED(tit_ap.cod_ser_docto,tit_ap.cod_tit_ap,tit_ap.cdn_fornecedor,tit_ap.cod_parcela, OUTPUT rRowidNota ).
                           WHEN '1' THEN
                                RUN piBuscarNotasRateioEms2IMA(tit_ap.cod_ser_docto,tit_ap.cod_tit_ap,tit_ap.cdn_fornecedor,tit_ap.cod_parcela, OUTPUT rRowidNota ).
                       END CASE.
    
                       FOR EACH ttApropRateio 
                           WHERE ttApropRateio.rowidNota = rRowidNota .
                           RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - ttApropRateio - Valor perc:' + string(ttApropRateio.perc) ).
                           CREATE tt.                                                                                                                                                   
                           ASSIGN 
                           tt.cod_modulo        = movto_cta_corren.cod_modul_dtsul
                           tt.cod_empresa       = IF AVAIL tit_ap THEN tit_ap.cod_empresa ELSE ''                                                                                                             
                           tt.cod_estab         = IF AVAIL tit_ap THEN tit_ap.cod_estab   ELSE ''                                                                                                             
                           tt.cod_emitente      = IF AVAIL tit_ap THEN string(tit_ap.cdn_fornecedor) ELSE '0'                                                                          
                           tt.desc_emitente     = IF AVAIL ems5.fornecedor THEN fornecedor.nom_pessoa  ELSE ''                                                                                  
                           tt.data              = movto_cta_corren.dat_transacao                                                                                                          
                           tt.conta_contabil    = ttApropRateio.conta                                                                                                           
                           tt.valor             = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN movto_tit_ap.val_movto_ap * ttApropRateio.perc 
                                                  ELSE  movto_tit_ap.val_movto_ap * -1 * ttApropRateio.perc     
                           tt.conta_corrente    = movto_cta_corren.cod_cta_corren                                                                                                      
                           tt.base              = pBase                                                                                                                                 
                           tt.origem            = 'Recebimento'                                                                                                                     
                           tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren                                                                                           
                           tt.cc                = ttApropRateio.cc
                           tt.ccusto_gerencial  = tt.cc
                           tt.sequencia         = movto_cta_corren.num_seq_movto_cta_corren
                           tt.id_movto_corren   = movto_cta_corren.num_id_movto_cta_corren
                           tt.grupo_emitente    = IF AVAIL ems5.grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  ELSE ''
                           tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
                           tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(10),";").
                       END.
                   END.
                   RUN incluirLog IN hLog('agrupamento Geral','==fim movto_tit_ap==================================').
               END.
            END.
            WHEN 'cmg'  THEN DO:
                FIND FIRST ems5.tip_trans_cx OF movto_cta_corren NO-LOCK NO-ERROR.
                FOR EACH ems5.aprop_ctbl_cmg OF movto_cta_corren
                    WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_cmg.ind_natur_lancto_ctbl = 'cr')
                    OR  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'sai' AND aprop_ctbl_cmg.ind_natur_lancto_ctbl = 'db') NO-LOCK :
                   CREATE tt.
                   ASSIGN
                   tt.cod_modulo        = movto_cta_corren.cod_modul_dtsul
                   tt.cod_empresa       = aprop_ctbl_cmg.cod_empresa 
                   tt.cod_estab         = aprop_ctbl_cmg.cod_estab
                   tt.cod_emitente      = movto_cta_corren.cod_tip_trans_cx
                   tt.desc_emitente     = IF AVAIL tip_trans_cx THEN tip_trans_cx.des_tip_trans_cx  ELSE ''
                   tt.data              = movto_cta_corren.dat_transacao
                   tt.conta_contabil    = aprop_ctbl_cmg.cod_cta_ctbl
                   tt.valor             = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN aprop_ctbl_cmg.val_movto_cta_corren ELSE aprop_ctbl_cmg.val_movto_cta_corren * -1
                   tt.conta_corrente    = movto_cta_corren.cod_cta_corren
                   tt.base              = pBase
                   tt.origem            = 'Caixa e Bancos'
                   tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren
                   tt.cc                = aprop_ctbl_cmg.cod_ccusto
                   tt.ccusto_gerencial  = tt.cc
                   tt.sequencia         = movto_cta_corren.num_seq_movto_cta_corren
                   tt.id_movto_corren   = movto_cta_corren.num_id_movto_cta_corren
                   tt.grupo_emitente    = ''
                   tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
                   tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(10),";") .    
                END.
            END.
        END CASE.
        RUN incluirLog IN hLog('agrupamento Geral','fim movto_cta_corren=========================').
    END.

    FOR EACH tt BREAK BY tt.conta_contabil:
        IF FIRST-OF(tt.conta_contabil) THEN DO:
           FIND FIRST ems5.cta_ctbl
               WHERE cta_ctbl.cod_cta_ctbl = tt.conta_contabil NO-LOCK NO-ERROR.
        END.
        ASSIGN tt.DESC_conta = IF AVAIL cta_ctbl THEN cta_ctbl.DES_tit_ctbl ELSE ''.
    END.

    FOR EACH tt 
        WHERE tt.cc <> '' BREAK BY tt.cc:
        IF FIRST-OF(tt.cc) THEN DO:
           FIND FIRST ems5.ccusto
               WHERE ccusto.cod_ccusto = tt.cc NO-LOCK NO-ERROR.
        END.
        ASSIGN tt.DESC_cc = IF AVAIL ems5.cCusto THEN ems5.cCusto.Des_tit_ctbl ELSE ''
               tt.ccusto_gerencial = IF AVAIL ems5.ccusto THEN tt.ccusto_gerencial + "-" + ems5.cCusto.Des_tit_ctbl ELSE tt.ccusto_gerencial .
    END. 

END PROCEDURE.

PROCEDURE piBuscarApropFolhaImp:
/******************************************************************************
 busca as contas contabeis de titulos de folha de pagamento e impostos retidos
 regra: esses titulos devido a forma de implantaá∆o dentro do sistema precisam
 buscar a contrapartida diretamente do movimento de baixa
 ******************************************************************************/

DEFINE VARIABLE valor AS DECIMAL     NO-UNDO.

DEFINE INPUT  PARAMETER rowidMovtoTitAP                 AS ROWID                                                NO-UNDO.
DEFINE INPUT  PARAMETER pDataContaCorrente              LIKE ems5.movto_cta_corren.dat_transacao                     NO-UNDO.
DEFINE INPUT  PARAMETER p_ind_fluxo_movto_cta_corren    LIKE ems5.movto_cta_corren.ind_fluxo_movto_cta_corren        NO-UNDO.
DEFINE INPUT  PARAMETER p_val_movto_ap                  LIKE bf_movto_tit_ap_03.val_movto_ap                          NO-UNDO.
DEFINE INPUT  PARAMETER p_cod_cta_corren                LIKE ems5.movto_cta_corren.cod_cta_corren                    NO-UNDO.
DEFINE INPUT  PARAMETER p_num_seq_movto_cta_corren      LIKE ems5.movto_cta_corren.num_seq_movto_cta_corren          NO-UNDO.
DEFINE INPUT  PARAMETER p_num_id_movto_cta_corren       LIKE ems5.movto_cta_corren.num_id_movto_cta_corren           NO-UNDO.

FIND bf_movto_tit_ap_03 
    WHERE ROWID(bf_movto_tit_ap_03) = RowidMovtoTitAp NO-LOCK NO-ERROR.
FIND FIRST bf_tit_ap_05 OF bf_movto_tit_ap_03 NO-LOCK NO-ERROR.
FIND FIRST ems5.fornecedor
    WHERE fornecedor.cdn_fornecedor = bf_tit_ap_05.cdn_fornecedor NO-LOCK NO-ERROR.
IF AVAIL fornecedor THEN
   FIND FIRST ems5.grp_fornec where
     ems5.grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.

FOR EACH ems5.aprop_ctbl_ap OF bf_movto_tit_ap_03 NO-LOCK                                                                                                                   
    WHERE  (p_ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'cr')                                                  
      OR  (p_ind_fluxo_movto_cta_corren  = 'sai' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'db')                                                  
     /*AND aprop_ctbl_ap.cod_indic_econ = 'real'*/ :
    RUN incluirLog IN hLog('agrupamento Geral','====inicio aprop_ctbl_ap - folha ou imposto========================================').
    RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - aprop_ctbl_ap - valor:' + string(aprop_ctbl_ap.val_aprop_ctbl)  + ' moeda:' + aprop_ctbl_ap.cod_indic_econ ).
    IF aprop_ctbl_ap.cod_indic_econ = 'dolar' THEN DO:
        RUN incluirLog IN hLog('agrupamento Geral','nivel 2 - achou emitente?' + string(AVAIL ems5.fornecedor)).
        FIND FIRST ems5.val_aprop_ctbl_ap OF aprop_ctbl_ap
              WHERE val_aprop_ctbl_ap.cod_finalid_econ = 'corrente' NO-LOCK NO-ERROR.
        IF AVAIL val_aprop_ctbl_ap THEN 
           ASSIGN valor = val_aprop_ctbl_ap.val_aprop_ctbl.
        ELSE
           ASSIGN valor = aprop_ctbl_ap.val_aprop_ctbl.
     END.
     ELSE DO:
         ASSIGN valor = aprop_ctbl_ap.val_aprop_ctbl.
     END.
     RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - aprop_ctbl_ap - valor convertido:' + string(valor)).
     CREATE tt.                                                                                                                                                   
     ASSIGN                                                                                                                                                       
     tt.cod_empresa       = aprop_ctbl_ap.cod_empresa                                                                                                             
     tt.cod_estab         = aprop_ctbl_ap.cod_estab                                                                                                               
     tt.cod_emitente      =  IF AVAIL bf_tit_ap_05 THEN string(bf_tit_ap_05.cdn_fornecedor) ELSE '0'                                                                          
     tt.desc_emitente     =  IF AVAIL ems5.fornecedor THEN fornecedor.nom_pessoa  ELSE ''                                                                                   
     tt.data              =  pDataContaCorrente                                                                                                          
     tt.conta_contabil    =  aprop_ctbl_ap.cod_cta_ctbl                                                                                                           
     tt.valor             = IF p_ind_fluxo_movto_cta_corren = 'ent' THEN valor  ELSE  valor * -1     
     tt.conta_corrente    = p_cod_cta_corren                                                                                                      
     tt.base              =  pBase                                                                                                                                 
     tt.origem            =  'Contas a Pagar - Folha/Imp.Retido' 
     tt.cod_modulo        =  'APB'
     tt.tipo              = p_ind_fluxo_movto_cta_corren                                                                                           
     tt.cc                = aprop_ctbl_ap.cod_ccusto
     tt.ccusto_gerencial  = tt.cc
     tt.sequencia         = p_num_seq_movto_cta_corren
     tt.id_movto_corren   = p_num_id_movto_cta_corren
     tt.grupo_emitente    =  IF AVAIL ems5.grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  ELSE ''
     tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
     tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(10),";").
     RUN incluirLog IN hLog('agrupamento Geral','====fim aprop_ctbl_ap========================================').
END.
END.

PROCEDURE piBuscarApropNormal:
/******************************************************************************
 busca as contas contabeis de titulos digitados diretamente no contas a pagar, 
 pegando a conta de debito referente ao movimento de implantaá∆o do titulo.
 caso o titulo seja substituido chama a procedure para tratamento de titulos
 substituidos.
 ******************************************************************************/

DEFINE INPUT  PARAMETER pDataContaCorrente              LIKE ems5.movto_cta_corren.dat_transacao                     NO-UNDO.
DEFINE INPUT  PARAMETER p_ind_fluxo_movto_cta_corren    LIKE ems5.movto_cta_corren.ind_fluxo_movto_cta_corren        NO-UNDO.
DEFINE INPUT  PARAMETER p_val_movto_ap                  LIKE ems5.movto_tit_ap.val_movto_ap                          NO-UNDO.
DEFINE INPUT  PARAMETER p_val_juros                     LIKE ems5.movto_tit_ap.val_juros                             NO-UNDO.
DEFINE INPUT  PARAMETER p_val_desconto                  LIKE ems5.movto_tit_ap.val_desconto                          NO-UNDO.
DEFINE INPUT  PARAMETER p_cod_cta_corren                LIKE ems5.movto_cta_corren.cod_cta_corren                    NO-UNDO.
DEFINE INPUT  PARAMETER p_num_seq_movto_cta_corren      LIKE ems5.movto_cta_corren.num_seq_movto_cta_corren          NO-UNDO.
DEFINE INPUT  PARAMETER p_num_id_movto_cta_corren       LIKE ems5.movto_cta_corren.num_id_movto_cta_corren           NO-UNDO.
DEFINE INPUT  PARAMETER p_rowidTitAp                    AS ROWID                                                NO-UNDO.

DEFINE VARIABLE dIndice     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE valor       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE valorInd    AS DECIMAL     NO-UNDO.



FIND  bf_tit_ap_01
    WHERE ROWID(bf_tit_ap_01) = p_rowidTitAp NO-LOCK NO-ERROR.
FIND FIRST ems5.fornecedor 
    WHERE  fornecedor.cdn_fornecedor = bf_tit_ap_01.cdn_fornecedor NO-LOCK NO-ERROR.
IF AVAIL fornecedor THEN
   FIND FIRST ems5.grp_fornec where
     ems5.grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
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
    RUN piBuscarTitulosSubstituidos(rowid(bf_tit_ap_02),
                                    pDataContaCorrente,
                                    p_ind_fluxo_movto_cta_corren,                                       
                                    (p_val_movto_ap + p_val_juros - p_val_desconto), 
                                    p_cod_cta_corren,  
                                    p_num_seq_movto_cta_corren, 
                                    p_num_id_movto_cta_corren).
    NEXT.
END.


RUN incluirLog IN hLog('agrupamento Geral','====inicio apropNormal - achou bf_movto?' + string(AVAIL bf_movto_tit_ap_01) + ' p========================================').

FOR EACH ems5.aprop_ctbl_ap OF bf_movto_tit_ap_01 NO-LOCK                                                                                                                   
     WHERE  (p_ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'cr')                                                  
      OR  (p_ind_fluxo_movto_cta_corren   = 'sai' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'db') :
    RUN incluirLog IN hLog('agrupamento Geral','====inicio aprop_ctbl_ap========================================').
    RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - aprop_ctbl_ap - valor:' + string(aprop_ctbl_ap.val_aprop_ctbl)  + ' moeda:' + aprop_ctbl_ap.cod_indic_econ ).
    IF aprop_ctbl_ap.cod_indic_econ = 'dolar' THEN DO:
        RUN incluirLog IN hLog('agrupamento Geral','nivel 2 - achou emitente?' + string(AVAIL ems5.fornecedor)).
        FIND FIRST ems5.val_aprop_ctbl_ap OF aprop_ctbl_ap
              WHERE val_aprop_ctbl_ap.cod_finalid_econ = 'corrente' NO-LOCK NO-ERROR.
        IF AVAIL val_aprop_ctbl_ap THEN 
           ASSIGN valor = val_aprop_ctbl_ap.val_aprop_ctbl / dIndice.
        ELSE
           ASSIGN valor = aprop_ctbl_ap.val_aprop_ctbl / dIndice.
     END.
     ELSE DO:
         ASSIGN valor = aprop_ctbl_ap.val_aprop_ctbl / dIndice.
     END.
     RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - aprop_ctbl_ap - valor convertido:' + string(valor)).
     CREATE tt.                                                                                                                                                   
     ASSIGN                                                                                                                                                       
     tt.cod_empresa       = aprop_ctbl_ap.cod_empresa                                                                                                             
     tt.cod_estab         = aprop_ctbl_ap.cod_estab                                                                                                               
     tt.cod_emitente      =  IF AVAIL bf_tit_ap_01 THEN string(bf_tit_ap_01.cdn_fornecedor) ELSE '0'                                                                          
     tt.desc_emitente     =  IF AVAIL fornecedor THEN fornecedor.nom_pessoa  ELSE ''                                                                                  
     tt.data              =  pDataContaCorrente                                                                                                         
     tt.conta_contabil    =  aprop_ctbl_ap.cod_cta_ctbl                                                                                                           
     tt.valor             = IF p_ind_fluxo_movto_cta_corren = 'ent' THEN valor  ELSE  valor * -1     
     tt.conta_corrente    = p_cod_cta_corren                                                                                                      
     tt.base              = pBase                                                                                                                                 
     tt.origem            =  'Contas a Pagar - Titulo' 
     tt.cod_modulo        =  'APB'
     tt.tipo              = p_ind_fluxo_movto_cta_corren                                                                                           
     tt.cc                = aprop_ctbl_ap.cod_ccusto
     tt.ccusto_gerencial  = tt.cc
     tt.sequencia         = p_num_seq_movto_cta_corren
     tt.id_movto_corren   = p_num_id_movto_cta_corren
     tt.grupo_emitente    =  IF AVAIL ems5.grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  ELSE ''
     tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
     tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(10),";").
     RUN incluirLog IN hLog('agrupamento Geral','====fim aprop_ctbl_ap========================================').
END.


END PROCEDURE.

PROCEDURE piBuscarNotasRateioEms2Ima:
/******************************************************************************
 busca as contas contabeis diretamente do estoque a partir dos parametros do 
 documento de entrada.
 Considera o tipo de controle do item para busca do lanáamento de entrada ou 
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
/*DEFINE OUTPUT PARAMETER pVlNota      AS DECIMAL     NO-UNDO.*/
/*DEFINE INPUT  PARAMETER pNatureza   AS CHARACTER   NO-UNDO.*/

/*EMPTY TEMP-TABLE ttApropItem.
EMPTY TEMP-TABLE ttApropRateio.*/
RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - Entrou na procedure piBuscarNotasRateio').

FIND FIRST ems2ima.dupli-apagar 
     WHERE dupli-aPagar.serie-docto     = pSerie
     AND   dupli-aPagar.nro-docto       = pDocumento
     AND   dupli-aPagar.cod-emitente    = pCodEmitente
     AND   dupli-aPagar.parcela         = pParcela
    NO-LOCK NO-ERROR.
RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - piBuscarNotasRateio - achou duplicata ?' + string(AVAIL dupli-apagar) ).
IF AVAIL ems2ima.dupli-apagar THEN DO:
   FIND FIRST ems2ima.docum-est OF dupli-apagar NO-LOCK NO-ERROR.
   RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - piBuscarNotasRateio - achou nota de entrada ?' + string(AVAIL docum-est)).
   
   IF AVAIL docum-est THEN
      ASSIGN pRowid = ROWID(docum-est).
   ELSE
      ASSIGN pRowid = ? .
   FIND FIRST ttApropItem
       WHERE ttApropItem.rRowidNota = pRowid NO-LOCK NO-ERROR.
   RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - piBuscarNotasRateio - achou ttApropItem(se n∆o quer dizer que vai buscar o movimento de estoque) ?' + string(AVAIL ttAPropItem)).
   IF NOT AVAIL ttApropItem AND pRowid <> ? THEN DO:
      RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - piBuscarNotasRateio - ACHOU ttApropItem e pRowid <> ? ').
      RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - piBuscarNotasRateio - documento:' + docum-est.nro-docto + " - serie:" + docum-est.serie-docto + "emitente:" + string(docum-est.cod-emitente) + "data:" + string(docum-est.dt-trans,'99/99/9999')).
      
      FOR EACH ems2ima.movto-estoq OF docum-est NO-LOCK.
          RUN incluirLog IN hLog('agrupamento Geral','======inicio movto-estoq========================================'). 
          RUN incluirLog IN hLog('agrupamento Geral',"item do estoque:" + ems2ima.movto-estoq.it-codigo +  " - conta/cc:" + ems2ima.movto-estoq.ct-codigo
                                  +  "/" + ems2ima.movto-estoq.sc-codigo + "tipo Movto:" + string(ems2ima.movto-estoq.tipo-trans)).   

          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - movto-estoq - valor' +  string(movto-estoq.valor-nota)).

          
          /* MESSAGE 'entrei no movimento de estoque - valor:' movto-estoq.valor-nota  
              VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

          FIND FIRST ems2ima.ITEM OF movto-estoq NO-LOCK NO-ERROR.
          RUN incluirLog IN hLog('agrupamento Geral','tipo controle:' + STRING(ITEM.tipo-contr ) ). 

          IF AVAIL ITEM THEN DO:
              RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - movto-estoq - criaá∆o do ttapropitem - achou o item ? ' + STRING(AVAIL ITEM)).
              IF ITEM.tipo-contr = 2 /*total*/ THEN DO:
                 IF movto-estoq.tipo-trans <> 1 THEN DO:
                    RUN incluirLog IN hLog('agrupamento Geral','======fim movto-estoq====== EH ITEM DE CONTROLE TOTAL E NAO EH ENTRADA ==============================').
                    NEXT.
                 END.
                 ASSIGN cConta = movto-estoq.ct-saldo
                        cCC    = movto-estoq.sc-saldo.
              END.
              ELSE DO:
                IF ITEM.tipo-contr <> 2  THEN DO: 
                   /*IF movto-estoq.it-codigo = '790021' THEN
                        RUN incluirLog IN hLog('agrupamento Geral','======entrei no movto-estoq do item 790021 - tipo de controle <> 2 ========================================').*/
                   IF movto-estoq.tipo-trans = 1 THEN DO:
                      /*IF movto-estoq.it-codigo = '790021' THEN
                         RUN incluirLog IN hLog('agrupamento Geral','======entrei no movto-estoq do item 790021 - tipo de controle <> 2 e transacao tipo-trans = 1===============').*/
                      RUN incluirLog IN hLog('agrupamento Geral','======fim movto-estoq====== NAO EH ITEM DE CONTROLE TOTAL E  EH ENTRADA ==============================').
                      NEXT.
                   END.
                   /*IF movto-estoq.it-codigo = '790021' THEN
                      RUN incluirLog IN hLog('agrupamento Geral','======entrei no movto-estoq do item 790021 - tipo de controle <> 2 e transacao tipo-trans <> 1===============').*/
                   RUN incluirLog IN hLog('agrupamento Geral','======fim movto-estoq====== NAO EH ITEM DE CONTROLE TOTAL E  EH SAIDA ==============================').
                   ASSIGN cConta = movto-estoq.ct-codigo
                          cCC    = movto-estoq.sc-codigo.
                END.
              END.
          END.
          ELSE DO:
                RUN incluirLog IN hLog('agrupamento Geral','n∆o achei o item:' + movto-estoq.it-codigo ). 
                ASSIGN cConta = ""
                       cCC    = "".

          END.
          RUN incluirLog IN hLog('agrupamento Geral','conta atribuida:' + cConta + ' cc:' + cCC ). 
          

          /*FIND FIRST movto-estoq OF item-doc-est NO-LOCK NO-ERROR.*/                   
          /*FIND FIRST ttApropItem                                                         
              WHERE ttApropItem.rRowidnota = ROWID(docum-est)                            
              AND   ttApropItem.conta      =  movto-estoq.ct-codigo                      
              AND   ttApropItem.cc         =  movto-estoq.sc-codigo                      
              AND   ttApropItem.itCodigo   =  movto-estoq.it-codigo                      
              AND   ttApropItem.codRefer   =  movto-estoq.cod-refer                      
              AND   ttApropItem.seq        =  movto-estoq.sequen-nf                      
              NO-LOCK NO-ERROR.                                                      
          IF NOT AVAIL ttApropItem THEN DO: */     
          
          CREATE ttApropItem.                                                         
          ASSIGN ttApropItem.rRowidNota = ROWID(docum-est)                            
                 ttApropItem.conta      = cConta                   
                 ttApropItem.cc         = cCC
                 ttApropItem.itCodigo   = movto-estoq.it-codigo                       
                 ttApropItem.codRefer   = movto-estoq.cod-refer                       
                 ttApropItem.seq        = movto-estoq.sequen-nf                       
                 ttApropItem.valorMat   = movto-estoq.valor-mat-m[1]                  
                 ttApropItem.valorNf    = movto-estoq.valor-nota.                     
          /*END.*/
          RUN incluirLog IN hLog('agrupamento Geral','======fim movto-estoq========================================').
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
          RUN incluirLog IN hLog('agrupamento Geral','====== inicio criaá∆o ttApropRateio========================================').
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - valor:' +  string(ttApropItem.valorNf) ).
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - tot-valor:' +  string(docum-est.tot-valor)  ).
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - icms complem:' +  string(docum-est.icm-complem)).
          ASSIGN dTotalBreak = dTotalBreak + ttAPropItem.valorNf.
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - valor dTotalBreak:' + string(dTotalBreak)).
          IF LAST-OF(ttApropItem.conta) AND LAST-OF(ttApropItem.cc) THEN DO:
             RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - valor dTotalBreak - LAST-OF - criacao ttApropRateio:' + string(dTotalBreak)).
             RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem -  conta contabil - LAST-OF - criacao ttApropRateio:' + ttApropItem.conta).
             CREATE ttApropRateio.
             ASSIGN ttApropRateio.rowidNota   = ROWID(docum-est)
                    ttApropRateio.conta       = ttApropItem.conta
                    ttApropRateio.cc          = ttAPropItem.cc
                    ttApropRateio.valor       = dTotalBreak
                    ttApropRateio.perc        = dTotalBreak / dTotalGeral /*(docum-est.tot-valor + docum-est.icm-complem)*/
                    dTotalBreak               = 0
                    ttApropRateio.documento   = docum-est.nro-docto 
                    ttApropRateio.serie       = docum-est.serie-docto
                    ttApropRateio.codEmitente = docum-est.cod-emitente
                    ttApropRateio.natOperacao = docum-est.nat-Operacao                                                       
                    ttApropRateio.data        = docum-est.dt-trans .
          END.
          RUN incluirLog IN hLog('agrupamento Geral','====== fim criaá∆o ttApropRateio========================================').
      END.
   END.
END.

END PROCEDURE.

PROCEDURE piBuscarNotasRateioEms2Med:
/******************************************************************************
 busca as contas contabeis diretamente do estoque a partir dos parametros do 
 documento de entrada.
 Considera o tipo de controle do item para busca do lanáamento de entrada ou 
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
/*DEFINE OUTPUT PARAMETER pVlNota      AS DECIMAL     NO-UNDO.*/
/*DEFINE INPUT  PARAMETER pNatureza   AS CHARACTER   NO-UNDO.*/

/*EMPTY TEMP-TABLE ttApropItem.
EMPTY TEMP-TABLE ttApropRateio.*/
RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - Entrou na procedure piBuscarNotasRateio').

FIND FIRST ems2med.dupli-apagar 
     WHERE dupli-aPagar.serie-docto     = pSerie
     AND   dupli-aPagar.nro-docto       = pDocumento
     AND   dupli-aPagar.cod-emitente    = pCodEmitente
     AND   dupli-aPagar.parcela         = pParcela
    NO-LOCK NO-ERROR.
RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - piBuscarNotasRateio - achou duplicata ?' + string(AVAIL dupli-apagar) ).
IF AVAIL dupli-apagar THEN DO:
   FIND FIRST ems2med.docum-est OF dupli-apagar NO-LOCK NO-ERROR.
   RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - piBuscarNotasRateio - achou nota de entrada ?' + string(AVAIL docum-est)).
   
   IF AVAIL ems2med.docum-est THEN
      ASSIGN pRowid = ROWID(docum-est).
   ELSE
      ASSIGN pRowid = ? .
   FIND FIRST ttApropItem
       WHERE ttApropItem.rRowidNota = pRowid NO-LOCK NO-ERROR.
   RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - piBuscarNotasRateio - achou ttApropItem(se n∆o quer dizer que vai buscar o movimento de estoque) ?' + string(AVAIL ttAPropItem)).
   IF NOT AVAIL ttApropItem AND pRowid <> ? THEN DO:
      RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - piBuscarNotasRateio - ACHOU ttApropItem e pRowid <> ? ').
      
      FOR EACH ems2med.movto-estoq OF docum-est NO-LOCK.
          RUN incluirLog IN hLog('agrupamento Geral','======inicio movto-estoq========================================'). 
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - movto-estoq - valor' +  string(movto-estoq.valor-nota)).
          
          /* MESSAGE 'entrei no movimento de estoque - valor:' movto-estoq.valor-nota  
              VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

          FIND FIRST ems2med.ITEM OF movto-estoq NO-LOCK NO-ERROR.
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - movto-estoq - criaá∆o do ttapropitem - achou o item ? ' + STRING(AVAIL ITEM)).
          IF ITEM.tipo-contr = 2 /*total*/ THEN DO:
             IF movto-estoq.tipo-trans <> 1 THEN DO:
                RUN incluirLog IN hLog('agrupamento Geral','======fim movto-estoq====== EH ITEM DE CONTROLE TOTAL E NAO EH ENTRADA ==============================').
                NEXT.
             END.
             ASSIGN cConta = movto-estoq.ct-saldo
                    cCC    = movto-estoq.sc-saldo.
          END.
          ELSE DO:
            IF ITEM.tipo-contr <> 2  THEN DO: 
               IF movto-estoq.tipo-trans = 1 THEN DO:
                  RUN incluirLog IN hLog('agrupamento Geral','======fim movto-estoq====== NAO EH ITEM DE CONTROLE TOTAL E  EH ENTRADA ==============================').
                  NEXT.
               END.
               
               ASSIGN cConta = movto-estoq.ct-codigo
                      cCC    = movto-estoq.sc-codigo.
            END.
          END.

          /*FIND FIRST movto-estoq OF item-doc-est NO-LOCK NO-ERROR.*/                   
          /*FIND FIRST ttApropItem                                                         
              WHERE ttApropItem.rRowidnota = ROWID(docum-est)                            
              AND   ttApropItem.conta      =  movto-estoq.ct-codigo                      
              AND   ttApropItem.cc         =  movto-estoq.sc-codigo                      
              AND   ttApropItem.itCodigo   =  movto-estoq.it-codigo                      
              AND   ttApropItem.codRefer   =  movto-estoq.cod-refer                      
              AND   ttApropItem.seq        =  movto-estoq.sequen-nf                      
              NO-LOCK NO-ERROR.                                                      
          IF NOT AVAIL ttApropItem THEN DO: */     
          
          CREATE ttApropItem.                                                         
          ASSIGN ttApropItem.rRowidNota = ROWID(docum-est)                            
                 ttApropItem.conta      = cConta                   
                 ttApropItem.cc         = cCC
                 ttApropItem.itCodigo   = movto-estoq.it-codigo                       
                 ttApropItem.codRefer   = movto-estoq.cod-refer                       
                 ttApropItem.seq        = movto-estoq.sequen-nf                       
                 ttApropItem.valorMat   = movto-estoq.valor-mat-m[1]                  
                 ttApropItem.valorNf    = movto-estoq.valor-nota.                     
          /*END.*/
          RUN incluirLog IN hLog('agrupamento Geral','======fim movto-estoq========================================').
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
          RUN incluirLog IN hLog('agrupamento Geral','====== inicio criaá∆o ttApropRateio========================================').
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - valor:' +  string(ttApropItem.valorNf) ).
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - tot-valor:' +  string(docum-est.tot-valor)  ).
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - icms complem:' +  string(docum-est.icm-complem)).
          ASSIGN dTotalBreak = dTotalBreak + ttAPropItem.valorNf.
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - valor dTotalBreak:' + string(dTotalBreak)).
          IF LAST-OF(ttApropItem.conta) AND LAST-OF(ttApropItem.cc) THEN DO:
             RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - valor dTotalBreak - LAST-OF - criacao ttApropRateio:' + string(dTotalBreak)).

             CREATE ttApropRateio.
             ASSIGN ttApropRateio.rowidNota   = rRowidNota
                    ttApropRateio.conta       = ttApropItem.conta
                    ttApropRateio.cc          = ttAPropItem.cc
                    ttApropRateio.valor       = dTotalBreak
                    ttApropRateio.perc        = dTotalBreak / dTotalGeral /*(docum-est.tot-valor + docum-est.icm-complem)*/
                    dTotalBreak               = 0
                    ttApropRateio.documento   = docum-est.nro-docto 
                    ttApropRateio.serie       = docum-est.serie-docto
                    ttApropRateio.codEmitente = docum-est.cod-emitente
                    ttApropRateio.natOperacao = docum-est.nat-Operacao                                                       
                    ttApropRateio.data        = docum-est.dt-trans .
          END.
          RUN incluirLog IN hLog('agrupamento Geral','====== fim criaá∆o ttApropRateio========================================').
      END.
   END.
END.

END PROCEDURE.



PROCEDURE piBuscarTitulosSubstituidos:
/******************************************************************************
 busca os titulos que foram substituidos pelo movimento de substituiá∆o passado
 como parametro e faz o tratamento da origem do titulo conforme foi feito na 
 procedure buscarDados
******************************************************************************/

DEFINE INPUT  PARAMETER rowidTitAP                      AS ROWID                                                NO-UNDO.
DEFINE INPUT  PARAMETER pDataContaCorrente              LIKE ems5.movto_cta_corren.dat_transacao                     NO-UNDO.
DEFINE INPUT  PARAMETER p_ind_fluxo_movto_cta_corren    LIKE ems5.movto_cta_corren.ind_fluxo_movto_cta_corren        NO-UNDO.
DEFINE INPUT  PARAMETER p_val_movto_ap                  LIKE bf_movto_tit_ap_02.val_movto_ap                    NO-UNDO.
DEFINE INPUT  PARAMETER p_cod_cta_corren                LIKE ems5.movto_cta_corren.cod_cta_corren                    NO-UNDO.
DEFINE INPUT  PARAMETER p_num_seq_movto_cta_corren      LIKE ems5.movto_cta_corren.num_seq_movto_cta_corren          NO-UNDO.
DEFINE INPUT  PARAMETER p_num_id_movto_cta_corren       LIKE ems5.movto_cta_corren.num_id_movto_cta_corren           NO-UNDO.

DEFINE VARIABLE rRowidNota AS ROWID  NO-UNDO.

FIND bf_tit_ap_03
    WHERE ROWID(bf_tit_ap_03) = rowidTitAp NO-LOCK NO-ERROR.

/*
1 - busca os titulos que geraram a duplicata, partindo da premissa que o titulo corrente Ç uma duplicata.
2 - chama as procedures para de busca de informaá‰es conforme a origem do titulo encontrado.

*/
FOR EACH bf_movto_tit_ap_02 NO-LOCK 
        WHERE ind_trans_ap_abrev        = 'BXSB'
        AND   bf_movto_tit_ap_02.num_fatur_ap = bf_tit_ap_03.num_fatur_ap.
    FIND FIRST bf_tit_ap_04 OF bf_movto_tit_ap_02 NO-LOCK NO-ERROR.
    FIND FIRST ems5.fornecedor                                                                                                                                             
         WHERE fornecedor.cdn_fornecedor = bf_tit_ap_04.cdn_fornecedor  NO-LOCK NO-ERROR. 
    IF AVAIL fornecedor THEN
       FIND FIRST ems5.grp_fornec where
       ems5.grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
    IF bf_tit_ap_04.ind_origin_tit_ap <> "REC" /*origem recebimento*/  THEN DO:
       RUN incluirLog IN hLog('agrupamento Geral','nivel 6- Tit- Substituido - Origem NAO EH Recebimento').
       IF SUBSTR(bf_tit_ap_04.cod_tit_ap,1,2) = 'hr' THEN DO : /*caso o titulo seja da folha*/   
          RUN incluirLog IN hLog('agrupamento Geral','nivel 6- Tit- Substituido - Origem NAO EH Recebimento - HR').
          RUN piBuscarApropFolhaImp( ROWID(bf_movto_tit_ap_02),
                                  pDataContaCorrente,
                                  p_ind_fluxo_movto_cta_corren,
                                  bf_movto_tit_ap_02.val_movto_ap,
                                  p_cod_cta_corren,
                                  p_num_seq_movto_cta_corren,                 
                                  p_num_id_movto_cta_corren ).
       END.
       ELSE DO:
          RUN piBuscarApropNormal(pDataContaCorrente,                 
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
       RUN incluirLog IN hLog('agrupamento Geral','nivel 6- Tit- Substituido - Origem Recebimento').
       
       CASE i-ep-codigo-usuario:
           WHEN '5' THEN
                RUN piBuscarNotasRateioEms2MED(bf_tit_ap_04.cod_ser_docto,
                               bf_tit_ap_04.cod_tit_ap,
                               bf_tit_ap_04.cdn_fornecedor,
                               bf_tit_ap_04.cod_parcela, 
                               OUTPUT rRowidNota).
           WHEN '1' THEN
                RUN piBuscarNotasRateioEms2IMA(bf_tit_ap_04.cod_ser_docto,
                               bf_tit_ap_04.cod_tit_ap,
                               bf_tit_ap_04.cdn_fornecedor,
                               bf_tit_ap_04.cod_parcela, 
                               OUTPUT rRowidNota).
       END CASE.

       FOR EACH ttApropRateio 
           WHERE ttApropRateio.rowidNota = rRowidNota .
           RUN incluirLog IN hLog('agrupamento Geral','nivel 6 - titulo substituido - ttApropRateio - Valor perc:' + string(ttApropRateio.perc) ).
           FIND FIRST emitente 
               WHERE emitente.cod-emitente = ttApropRateio.codEmitente
               NO-LOCK NO-ERROR.
           FIND FIRST ems5.fornecedor
               WHERE fornecedor.cdn_fornec = emitente.cod-emitente
               NO-LOCK NO-ERROR.
           FIND FIRST ems5.grp_fornec OF ems5.fornecedor NO-LOCK NO-ERROR.
           CREATE tt.                                                                                                                                                   
           ASSIGN                                                                                                                                                       
           tt.cod_empresa       = IF AVAIL bf_tit_ap_04 THEN bf_tit_ap_04.cod_empresa ELSE ''                                                                                                             
           tt.cod_estab         = IF AVAIL bf_tit_ap_04 THEN bf_tit_ap_04.cod_estab   ELSE ''                                                                                                             
           tt.cod_emitente      = string(ttApropRateio.codEmitente) /*IF AVAIL bf_tit_ap_04 THEN string(ems5.tit_ap.cdn_fornecedor) ELSE '0' */
           tt.desc_emitente     = IF AVAIL emitente THEN emitente.nome-emit  ELSE ''                                                                                    
           tt.data              = pDataContaCorrente                                                                                                          
           tt.conta_contabil    = ttApropRateio.conta                                                                                                           
           tt.valor             = IF p_ind_fluxo_movto_cta_corren = 'ent' THEN bf_movto_tit_ap_02.val_movto_ap * ttApropRateio.perc 
                                  ELSE  bf_movto_tit_ap_02.val_movto_ap * -1 * ttApropRateio.perc     
           tt.conta_corrente    = p_cod_cta_corren                                                                                                      
           tt.base              = pBase                                                                                                                                 
           tt.origem            = 'Recebimento'  
           tt.cod_modulo        =  'APB'
           tt.tipo              = p_ind_fluxo_movto_cta_corren                                                                                           
           tt.cc                = ttApropRateio.cc
           tt.ccusto_gerencial  = tt.cc
           tt.sequencia         = p_num_seq_movto_cta_corren
           tt.id_movto_corren   = p_num_id_movto_cta_corren
           tt.grupo_emitente    = IF AVAIL ems5.fornecedor THEN  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  ELSE ''
           tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
           tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(10),";").
       END.
    END.
END.

END PROCEDURE.


{ems5\esp\desembolso2.i}






