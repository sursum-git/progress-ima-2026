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
       FIELD itCodigo       LIKE ems2ima.movto-estoq.it-codigo
       FIELD codRefer       LIKE ems2ima.movto-estoq.cod-refer
       FIELD seq            LIKE ems2ima.movto-estoq.sequen-nf
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

DEFINE VARIABLE valor       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE rRowidNota  AS ROWID       NO-UNDO.

/*buffer do movto-tit-ap para buscar a apropria‡Æo da implanta‡Æo do titulo*/
DEFINE BUFFER bf_movto_tit_ap_01 FOR ems5.movto_tit_ap.
DEFINE BUFFER bf_movto_tit_ap_02 FOR ems5.movto_tit_ap.
DEFINE BUFFER bf_movto_tit_ap_03 FOR ems5.movto_tit_ap.

DEFINE BUFFER bf_tit_ap_01  FOR ems5.tit_ap.
DEFINE BUFFER bf_tit_ap_02  FOR ems5.tit_ap.
DEFINE BUFFER bf_tit_ap_03  FOR ems5.tit_ap.
DEFINE BUFFER bf_tit_ap_04  FOR ems5.tit_ap.
DEFINE BUFFER bf_tit_ap_05  FOR ems5.tit_ap.
DEFINE BUFFER b_tit_ap      FOR ems5.tit_ap.

DEF VAR h-acomp      AS HANDLE  NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando..").

IF p-analise = 1 THEN DO.  // Transa‡Æo
   FOR EACH ems5.tit_ap WHERE
            ems5.tit_ap.dat_transacao >= pDtIni AND
            ems5.tit_ap.dat_transacao <= pDtFim NO-LOCK
         BY ems5.tit_ap.dat_transacao.
    
       // J  Baixou, ser  tratado pelo Realizado
       IF ems5.tit_ap.dat_liquidac_tit_ap <> 12.31.9999 THEN NEXT. 

       RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(ems5.tit_ap.dat_transacao,"99/99/9999") + " Titulo:" + STRING(ems5.tit_ap.cod_tit_ap) ).
    
       FIND FIRST ems5.espec_docto OF ems5.tit_ap NO-LOCK NO-ERROR.
    
       IF ems5.espec_docto.ind_tip_espec_docto = 'IMPOSTO RETIDO' THEN NEXT.
    
       FIND FIRST ems5.fornecedor WHERE 
                  ems5.fornecedor.cdn_fornecedor = ems5.tit_ap.cdn_fornecedor NO-LOCK NO-ERROR. 
    
       IF AVAIL fornecedor THEN
          FIND FIRST ems5.grp_fornec where
                     ems5.grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
    
       FOR EACH ems5.movto_tit_ap OF ems5.tit_ap WHERE 
                ems5.movto_tit_ap.ind_trans_ap_abrev = 'IMPL' NO-LOCK:
    
           IF ems5.tit_ap.ind_origin_tit_ap <> "REC" THEN DO:
              FOR EACH ems5.aprop_ctbl_ap OF ems5.movto_tit_ap WHERE
                       //ems5.aprop_ctbl_ap.cod_indic_econ = 'REAL' AND
                       ems5.aprop_ctbl_ap.ind_natur_lancto_ctbl = 'DB' NO-LOCK.
                   
                  CREATE tt.
                  ASSIGN tt.cod_modulo        = 'APB'
                         tt.cod_empresa       = ems5.tit_ap.cod_empresa
                         tt.cod_estab         = ems5.tit_ap.cod_estab
                         tt.cod_emitente      = STRING(ems5.tit_ap.cdn_fornecedor)
                         tt.desc_emitente     = fornecedor.nom_pessoa
                         tt.data              = ems5.tit_ap.dat_transacao                                                                                                          
                         tt.conta_contabil    = ems5.aprop_ctbl_ap.cod_cta_ctbl
                         tt.valor             = ems5.aprop_ctbl_ap.val_aprop_ctbl * -1 
                         tt.base              = '10'
                         tt.origem            = 'Contas a Pagar'
                         tt.ccusto_gerencial  = aprop_ctbl_ap.cod_ccusto
                         tt.id_movto_corren   = ems5.tit_ap.num_id_tit_ap
                         tt.cod_tit           = ems5.tit_ap.cod_tit_ap
                         tt.num_id_tit        = ems5.tit_ap.num_id_tit_ap
                         tt.tipo              = 'Aberto'.
    
                  FOR EACH ems5.compl_impto_retid_ap WHERE 
                           ems5.compl_impto_retid_ap.num_id_movto_tit_ap_pai = ems5.movto_tit_ap.num_id_movto_tit_ap AND
                           ems5.compl_impto_retid_ap.cod_estab               = ems5.movto_tit_ap.cod_estab NO-LOCK.
                      FIND b_tit_ap WHERE 
                           b_tit_ap.cod_estab     = ems5.compl_impto_retid_ap.cod_estab AND 
                           b_tit_ap.num_id_tit_ap = ems5.compl_impto_retid_ap.num_id_tit_ap
                           NO-LOCK NO-ERROR.
                      IF AVAIL b_tit_ap THEN
                         ASSIGN tt.valor = tt.valor + b_tit_ap.val_origin_tit_ap * -1. 
                  END.
              END.
           END.
           ELSE DO:
              CASE i-ep-codigo-usuario:
                   WHEN '5' THEN
                         RUN piBuscarNotasRateioEms2 (INPUT ems5.tit_ap.cod_ser_docto,
                                                      INPUT ems5.tit_ap.cod_tit_ap,
                                                      INPUT ems5.tit_ap.cdn_fornecedor,
                                                      INPUT ems5.tit_ap.cod_parcela, 
                                                      OUTPUT rRowidNota ).
                    WHEN '1' THEN
                         RUN piBuscarNotasRateioEms2Aux (INPUT ems5.tit_ap.cod_ser_docto,
                                                         INPUT ems5.tit_ap.cod_tit_ap,
                                                         INPUT ems5.tit_ap.cdn_fornecedor,
                                                         INPUT ems5.tit_ap.cod_parcela, 
                                                         OUTPUT rRowidNota ).
              END CASE.
    
              FOR EACH ttApropRateio WHERE 
                       ttApropRateio.rowidNota = rRowidNota NO-LOCK.
    
                  CREATE tt.                                                                                                                                                   
                  ASSIGN tt.cod_modulo        = 'APB'
                         tt.cod_empresa       = ems5.tit_ap.cod_empresa 
                         tt.cod_estab         = ems5.tit_ap.cod_estab 
                         tt.cod_emitente      = STRING(ems5.tit_ap.cdn_fornecedor)
                         tt.desc_emitente     = fornecedor.nom_pessoa
                         tt.data              = ems5.tit_ap.dat_transacao                                                                                                          
                         tt.conta_contabil    = ttApropRateio.conta                                                                                                           
                         tt.valor             = ems5.tit_ap.val_origin_tit_ap * -1 * ttApropRateio.perc     
                         tt.base              = '10'
                         tt.origem            = 'Contas a Pagar - REC'                                                                                                                     
                         tt.cc                = ttApropRateio.cc
                         tt.ccusto_gerencial  = tt.cc
                         tt.id_movto_corren   = ems5.tit_ap.num_id_tit_ap
                         tt.cod_tit           = ems5.tit_ap.cod_tit_ap
                         tt.num_id_tit        = ems5.tit_ap.num_id_tit_ap
                         tt.tipo              = 'Aberto'.
                    
                  FOR EACH ems5.compl_impto_retid_ap WHERE 
                           ems5.compl_impto_retid_ap.num_id_movto_tit_ap_pai = ems5.movto_tit_ap.num_id_movto_tit_ap AND
                           ems5.compl_impto_retid_ap.cod_estab               = ems5.movto_tit_ap.cod_estab NO-LOCK.
                      FIND b_tit_ap WHERE 
                           b_tit_ap.cod_estab     = ems5.compl_impto_retid_ap.cod_estab AND 
                           b_tit_ap.num_id_tit_ap = ems5.compl_impto_retid_ap.num_id_tit_ap
                           NO-LOCK NO-ERROR.
                      IF AVAIL b_tit_ap THEN
                         ASSIGN tt.valor = tt.valor + b_tit_ap.val_origin_tit_ap * -1. 
                  END.
              END.
           END.
       END.
   END.
    
    
   FOR EACH ems5.tit_acr WHERE
            ems5.tit_acr.dat_transacao >= pDtIni AND
            ems5.tit_acr.dat_transacao <= pDtFim NO-LOCK
         BY ems5.tit_acr.dat_transacao.
        
       // J  Baixou, ser  tratado pelo Realizado
       IF ems5.tit_acr.dat_liquidac <> 12.31.9999 THEN NEXT. 

       RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(ems5.tit_acr.dat_transacao,"99/99/9999") + " Titulo:" + STRING(ems5.tit_acr.cod_tit_acr) ).
    
       FIND FIRST ems5.cliente WHERE 
                  ems5.cliente.cdn_cliente = ems5.tit_acr.cdn_cliente NO-LOCK NO-ERROR. 
    
       FOR EACH ems5.movto_tit_acr OF ems5.tit_acr NO-LOCK:
           FOR EACH ems5.aprop_ctbl_acr OF ems5.movto_tit_acr WHERE
                    ems5.aprop_ctbl_acr.ind_natur_lancto_ctbl = 'CR' NO-LOCK.
               CREATE tt.
               ASSIGN tt.cod_modulo        = 'ACR'
                      tt.cod_empresa       = ems5.tit_acr.cod_empresa
                      tt.cod_estab         = ems5.tit_acr.cod_estab
                      tt.cod_emitente      = STRING(ems5.tit_acr.cdn_cliente)
                      tt.desc_emitente     = ems5.cliente.nom_pessoa
                      tt.data              = ems5.tit_acr.dat_transacao                                                                                                          
                      tt.conta_contabil    = aprop_ctbl_acr.cod_cta_ctbl
                      tt.valor             = ems5.tit_acr.val_origin_tit_acr
                      tt.base              = '10'
                      tt.origem            = 'Contas a Receber'
                      tt.ccusto_gerencial  = aprop_ctbl_acr.cod_ccusto
                      tt.id_movto_corren   = ems5.tit_acr.num_id_tit_acr
                      tt.cod_tit           = ems5.tit_acr.cod_tit_acr
                      tt.num_id_tit        = ems5.tit_acr.num_id_tit_acr
                      tt.tipo              = 'Aberto'.
           END.
       END.
   END.
END.

ELSE DO.  // Pagamento
   FOR EACH ems5.tit_ap WHERE
            ems5.tit_ap.dat_vencto_tit_ap >= pDtIni AND
            ems5.tit_ap.dat_vencto_tit_ap <= pDtFim NO-LOCK
       BY ems5.tit_ap.dat_liquidac_tit_ap.
    
       // J  Baixou, ser  tratado pelo Realizado
       IF ems5.tit_ap.dat_liquidac_tit_ap <> 12.31.9999 THEN NEXT. 

       RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(ems5.tit_ap.dat_liquidac_tit_ap,"99/99/9999") + " Titulo:" + STRING(ems5.tit_ap.cod_tit_ap) ).
    
       FIND FIRST ems5.espec_docto OF ems5.tit_ap NO-LOCK NO-ERROR.
    
       IF ems5.espec_docto.ind_tip_espec_docto = 'IMPOSTO RETIDO' THEN NEXT.
    
       FIND FIRST ems5.fornecedor WHERE 
                  ems5.fornecedor.cdn_fornecedor = ems5.tit_ap.cdn_fornecedor NO-LOCK NO-ERROR. 
    
       IF AVAIL fornecedor THEN
          FIND FIRST ems5.grp_fornec where
                     ems5.grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
    
       FOR EACH ems5.movto_tit_ap OF ems5.tit_ap WHERE 
                ems5.movto_tit_ap.ind_trans_ap_abrev = 'IMPL' NO-LOCK:
    
           IF ems5.tit_ap.ind_origin_tit_ap <> "REC" THEN DO:
              FOR EACH ems5.aprop_ctbl_ap OF ems5.movto_tit_ap WHERE
                       ems5.aprop_ctbl_ap.ind_natur_lancto_ctbl = 'DB' NO-LOCK.
                   
                  CREATE tt.
                  ASSIGN tt.cod_modulo        = 'APB'
                         tt.cod_empresa       = IF AVAIL ems5.tit_ap THEN ems5.tit_ap.cod_empresa ELSE ''                                                                                                             
                         tt.cod_estab         = IF AVAIL ems5.tit_ap THEN ems5.tit_ap.cod_estab   ELSE ''                                                                                                             
                         tt.cod_emitente      = IF AVAIL ems5.tit_ap THEN string(ems5.tit_ap.cdn_fornecedor) ELSE '0'                                                                          
                         tt.desc_emitente     = IF AVAIL ems5.fornecedor THEN fornecedor.nom_pessoa  ELSE ''                                                                                  
                         tt.data              = ems5.tit_ap.dat_liquidac_tit_ap                                                                                                          
                         tt.conta_contabil    = aprop_ctbl_ap.cod_cta_ctbl
                         tt.valor             = ems5.tit_ap.val_origin_tit_ap * -1 
                         tt.base              = '10'
                         tt.origem            = 'Contas a Pagar'
                         tt.ccusto_gerencial  = aprop_ctbl_ap.cod_ccusto
                         tt.id_movto_corren   = ems5.tit_ap.num_id_tit_ap
                         tt.cod_tit           = ems5.tit_ap.cod_tit_ap
                         tt.num_id_tit        = ems5.tit_ap.num_id_tit_ap
                         tt.tipo              = 'Aberto'.
    
                  FOR EACH ems5.compl_impto_retid_ap WHERE 
                           ems5.compl_impto_retid_ap.num_id_movto_tit_ap_pai = ems5.movto_tit_ap.num_id_movto_tit_ap AND
                           ems5.compl_impto_retid_ap.cod_estab               = ems5.movto_tit_ap.cod_estab NO-LOCK.
                      FIND b_tit_ap WHERE 
                           b_tit_ap.cod_estab     = ems5.compl_impto_retid_ap.cod_estab AND 
                           b_tit_ap.num_id_tit_ap = ems5.compl_impto_retid_ap.num_id_tit_ap
                           NO-LOCK NO-ERROR.
                      IF AVAIL b_tit_ap THEN
                         ASSIGN tt.valor = tt.valor + b_tit_ap.val_origin_tit_ap * -1. 
                  END.
              END.
           END.
           ELSE DO:
                CASE i-ep-codigo-usuario:
                    WHEN '5' THEN
                         RUN piBuscarNotasRateioEms2 (INPUT ems5.tit_ap.cod_ser_docto,
                                                      INPUT ems5.tit_ap.cod_tit_ap,
                                                      INPUT ems5.tit_ap.cdn_fornecedor,
                                                      INPUT ems5.tit_ap.cod_parcela, 
                                                      OUTPUT rRowidNota ).
                    WHEN '1' THEN
                         RUN piBuscarNotasRateioEms2Aux (INPUT ems5.tit_ap.cod_ser_docto,
                                                         INPUT ems5.tit_ap.cod_tit_ap,
                                                         INPUT ems5.tit_ap.cdn_fornecedor,
                                                         INPUT ems5.tit_ap.cod_parcela, 
                                                         OUTPUT rRowidNota ).
                END CASE.
    
                FOR EACH ttApropRateio WHERE 
                         ttApropRateio.rowidNota = rRowidNota NO-LOCK.
    
                    CREATE tt.                                                                                                                                                   
                    ASSIGN tt.cod_modulo        = 'APB'
                           tt.cod_empresa       = ems5.tit_ap.cod_empresa
                           tt.cod_estab         = ems5.tit_ap.cod_estab
                           tt.cod_emitente      = STRING(ems5.tit_ap.cdn_fornecedor)
                           tt.desc_emitente     = fornecedor.nom_pessoa
                           tt.data              = ems5.tit_ap.dat_liquidac_tit_ap                                                                                                          
                           tt.conta_contabil    = ttApropRateio.conta                                                                                                           
                           tt.valor             = ems5.tit_ap.val_origin_tit_ap * -1 * ttApropRateio.perc     
                           tt.base              = '10'
                           tt.origem            = 'Contas a Pagar - REC'                                                                                                                     
                           tt.cc                = ttApropRateio.cc
                           tt.ccusto_gerencial  = tt.cc
                           tt.id_movto_corren   = ems5.tit_ap.num_id_tit_ap
                           tt.cod_tit           = ems5.tit_ap.cod_tit_ap
                           tt.num_id_tit        = ems5.tit_ap.num_id_tit_ap
                           tt.tipo              = 'Aberto'.
                    
                    FOR EACH ems5.compl_impto_retid_ap WHERE 
                             ems5.compl_impto_retid_ap.num_id_movto_tit_ap_pai = ems5.movto_tit_ap.num_id_movto_tit_ap AND
                             ems5.compl_impto_retid_ap.cod_estab               = ems5.movto_tit_ap.cod_estab NO-LOCK.
                        FIND b_tit_ap WHERE 
                             b_tit_ap.cod_estab     = ems5.compl_impto_retid_ap.cod_estab AND 
                             b_tit_ap.num_id_tit_ap = ems5.compl_impto_retid_ap.num_id_tit_ap
                             NO-LOCK NO-ERROR.
                        IF AVAIL b_tit_ap THEN
                           ASSIGN tt.valor = tt.valor + b_tit_ap.val_origin_tit_ap * -1. 
                    END.
                END.
           END.
       END.
   END.
    
   FOR EACH ems5.tit_acr WHERE
            ems5.tit_acr.dat_liquidac_tit_acr >= pDtIni AND
            ems5.tit_acr.dat_liquidac_tit_acr <= pDtFim NO-LOCK
         BY ems5.tit_acr.dat_transacao.

       // J  Baixou, ser  tratado pelo Realizado
       IF ems5.tit_acr.dat_liquidac <> 12.31.9999 THEN NEXT. 
        
       RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(ems5.tit_acr.dat_liquidac_tit_acr,"99/99/9999") + " Titulo:" + STRING(ems5.tit_acr.cod_tit_acr) ).
    
       FIND FIRST ems5.cliente WHERE 
                  ems5.cliente.cdn_cliente = ems5.tit_acr.cdn_cliente NO-LOCK NO-ERROR. 
    
       FOR EACH ems5.movto_tit_acr OF ems5.tit_acr NO-LOCK:
           FOR EACH ems5.aprop_ctbl_acr OF ems5.movto_tit_acr WHERE
                   ems5.aprop_ctbl_acr.ind_natur_lancto_ctbl = 'CR' NO-LOCK.
               CREATE tt.
               ASSIGN tt.cod_modulo        = 'ACR'
                      tt.cod_empresa       = ems5.tit_acr.cod_empresa
                      tt.cod_estab         = ems5.tit_acr.cod_estab
                      tt.cod_emitente      = STRING(ems5.tit_acr.cdn_cliente)
                      tt.desc_emitente     = ems5.cliente.nom_pessoa
                      tt.data              = ems5.tit_acr.dat_liquidac_tit_acr
                      tt.conta_contabil    = aprop_ctbl_acr.cod_cta_ctbl
                      tt.valor             = ems5.tit_acr.val_origin_tit_acr
                      tt.base              = '10'
                      tt.origem            = 'Contas a Receber'
                      tt.ccusto_gerencial  = aprop_ctbl_acr.cod_ccusto
                      tt.id_movto_corren   = ems5.tit_acr.num_id_tit_acr
                      tt.cod_tit           = ems5.tit_acr.cod_tit_acr
                      tt.num_id_tit        = ems5.tit_acr.num_id_tit_acr
                      tt.tipo              = 'Aberto'.
           END.
       END.
   END.
END.


RUN pi-finalizar in h-acomp.

//------------------ Procedures ----------------------
PROCEDURE piBuscarNotasRateioEms2:
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
         WHERE ems2ima.dupli-aPagar.serie-docto     = pSerie
         AND   ems2ima.dupli-aPagar.nro-docto       = pDocumento
         AND   ems2ima.dupli-aPagar.cod-emitente    = pCodEmitente
         AND   ems2ima.dupli-aPagar.parcela         = pParcela
        NO-LOCK NO-ERROR.
    IF AVAIL ems2ima.dupli-apagar THEN DO:
       FIND FIRST ems2ima.docum-est OF ems2ima.dupli-apagar NO-LOCK NO-ERROR.
       
       IF AVAIL docum-est THEN
          ASSIGN pRowid = ROWID(docum-est).
       ELSE
          ASSIGN pRowid = ? .
       FIND FIRST ttApropItem
           WHERE ttApropItem.rRowidNota = pRowid NO-LOCK NO-ERROR.
       IF NOT AVAIL ttApropItem AND pRowid <> ? THEN DO:
          FOR EACH ems2ima.movto-estoq OF ems2ima.docum-est NO-LOCK.
              FIND FIRST ems2ima.ITEM OF ems2ima.movto-estoq NO-LOCK NO-ERROR.
              IF AVAIL ITEM THEN DO:
                  IF ITEM.tipo-contr = 2 /*total*/ THEN DO:
                     IF ems2ima.movto-estoq.tipo-trans <> 1 THEN DO:
                        NEXT.
                     END.
                     ASSIGN cConta = ems2ima.movto-estoq.ct-saldo
                            cCC    = ems2ima.movto-estoq.sc-saldo.
                  END.
                  ELSE DO:
                    IF ITEM.tipo-contr <> 2  THEN DO: 
                       IF ems2ima.movto-estoq.tipo-trans = 1 THEN NEXT.
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
           WHERE ttApropRateio.rowidNota = ROWID(ems2ima.docum-est) NO-LOCK NO-ERROR.
       IF NOT AVAIL ttApropRateio AND pRowid <> ? THEN DO: 
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
                        ttApropRateio.data        = ems2ima.docum-est.dt-trans .
              END.
          END.
       END.
    END.

END PROCEDURE.


PROCEDURE piBuscarNotasRateioEms2Aux:
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
              FIND FIRST dbaux.item  OF dbaux.movto-estoq NO-LOCK NO-ERROR.
              IF AVAIL dbaux.item  THEN DO:
                  IF dbaux.item.tipo-contr = 2 /*total*/ THEN DO:
                     IF dbaux.movto-estoq.tipo-trans <> 1 THEN DO:
                        NEXT.
                     END.
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
              END.
              ELSE DO:
                    ASSIGN cConta = ""
                           cCC    = "".
    
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
           WHERE ttApropRateio.rowidNota = ROWID(dbaux.docum-est) NO-LOCK NO-ERROR.
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
                 ASSIGN ttApropRateio.rowidNota   = ROWID(dbaux.docum-est)
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

PROCEDURE piBuscarTitulosSubstituidos:
/******************************************************************************
 busca os titulos que foram substituidos pelo movimento de substitui‡Æo passado
 como parametro e faz o tratamento da origem do titulo conforme foi feito na 
 procedure buscarDados
******************************************************************************/

DEFINE INPUT  PARAMETER rowidTitAP                      AS ROWID                                                 NO-UNDO.
DEFINE INPUT  PARAMETER pDataContaCorrente              LIKE ems5.movto_cta_corren.dat_transacao              NO-UNDO.
DEFINE INPUT  PARAMETER p_ind_fluxo_movto_cta_corren    LIKE ems5.movto_cta_corren.ind_fluxo_movto_cta_corren NO-UNDO.
DEFINE INPUT  PARAMETER p_val_movto_ap                  LIKE bf_movto_tit_ap_02.val_movto_ap                     NO-UNDO.
DEFINE INPUT  PARAMETER p_cod_cta_corren                LIKE ems5.movto_cta_corren.cod_cta_corren             NO-UNDO.
DEFINE INPUT  PARAMETER p_num_seq_movto_cta_corren      LIKE ems5.movto_cta_corren.num_seq_movto_cta_corren   NO-UNDO.
DEFINE INPUT  PARAMETER p_num_id_movto_cta_corren       LIKE ems5.movto_cta_corren.num_id_movto_cta_corren    NO-UNDO.

DEFINE VARIABLE rRowidNota AS ROWID  NO-UNDO.

FIND bf_tit_ap_03
    WHERE ROWID(bf_tit_ap_03) = rowidTitAp NO-LOCK NO-ERROR.

/*
1 - busca os titulos que geraram a duplicata, partindo da premissa que o titulo corrente ‚ uma duplicata.
2 - chama as procedures para de busca de informa‡äes conforme a origem do titulo encontrado.

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
       IF SUBSTR(bf_tit_ap_04.cod_tit_ap,1,2) = 'hr' THEN DO : /*caso o titulo seja da folha*/   
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
       CASE i-ep-codigo-usuario:
           WHEN '5' THEN
                RUN piBuscarNotasRateioems2ima(bf_tit_ap_04.cod_ser_docto,
                                               bf_tit_ap_04.cod_tit_ap,
                                               bf_tit_ap_04.cdn_fornecedor,
                                               bf_tit_ap_04.cod_parcela, 
                                               OUTPUT rRowidNota).
           WHEN '1' THEN
                RUN piBuscarNotasRateioems2IMA(bf_tit_ap_04.cod_ser_docto,
                                               bf_tit_ap_04.cod_tit_ap,
                                               bf_tit_ap_04.cdn_fornecedor,
                                               bf_tit_ap_04.cod_parcela, 
                                               OUTPUT rRowidNota).
       END CASE.

       FOR EACH ttApropRateio 
           WHERE ttApropRateio.rowidNota = rRowidNota .
           FIND FIRST emitente 
               WHERE emitente.cod-emitente = ttApropRateio.codEmitente
               NO-LOCK NO-ERROR.
           FIND FIRST ems5.fornecedor
               WHERE fornecedor.cdn_fornec = emitente.cod-emitente
               NO-LOCK NO-ERROR.
           FIND FIRST ems5.grp_fornec OF ems5.fornecedor NO-LOCK NO-ERROR.
           CREATE tt.                                                                                                                                                   
           ASSIGN                                                                                                                                                       
           tt.cod_modulo        = 'APB'
           tt.cod_empresa       = IF AVAIL bf_tit_ap_04 THEN bf_tit_ap_04.cod_empresa ELSE ''                                                                                                             
           tt.cod_estab         = IF AVAIL bf_tit_ap_04 THEN bf_tit_ap_04.cod_estab   ELSE ''                                                                                                             
           tt.cod_emitente      = string(ttApropRateio.codEmitente) /*IF AVAIL bf_ems5.tit_ap_04 THEN string(ems5.ems5.tit_ap.cdn_fornecedor) ELSE '0' */
           tt.desc_emitente     = IF AVAIL emitente THEN emitente.nome-emit  ELSE ''                                                                                    
           tt.data              = pDataContaCorrente                                                                                                          
           tt.conta_contabil    = ttApropRateio.conta                                                                                                           
           tt.valor             = IF p_ind_fluxo_movto_cta_corren = 'ent' THEN bf_movto_tit_ap_02.val_movto_ap * ttApropRateio.perc 
                                  ELSE  bf_movto_tit_ap_02.val_movto_ap * -1 * ttApropRateio.perc     
           tt.conta_corrente    = p_cod_cta_corren                                                                                                      
           tt.base              = '10'                                                                                                                                 
           tt.origem            = 'Contas a Pagar - REC'  
           tt.tipo              = 'Aberto'
           tt.cc                = ttApropRateio.cc
           tt.ccusto_gerencial  = tt.cc
           tt.sequencia         = p_num_seq_movto_cta_corren
           tt.id_movto_corren   = p_num_id_movto_cta_corren
           tt.cod_tit           = bf_movto_tit_ap_02.cod_tit_ap
           tt.num_id_tit        = bf_movto_tit_ap_02.num_id_tit_ap
           tt.grupo_emitente    = IF AVAIL ems5.fornecedor THEN  string(ems5.fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  ELSE ''
           tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
           tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(10),";").
       END.
    END.
END.

END PROCEDURE.










