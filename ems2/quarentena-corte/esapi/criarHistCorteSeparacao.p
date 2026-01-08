/******************************************************************************************
Programa: esapi/criarHistCorteSeparacao.p
Autor: Tadeu Silva Parreiras
Objetivo:criar o historico de corte feito a partir dos parametros passados.
Data: 09/2025
*******************************************************************************************/
 
 DEFINE INPUT  PARAMETER pTipoReg           AS INTEGER     NO-UNDO.
 DEFINE INPUT  PARAMETER pNumEtqLisa        AS CHAR        NO-UNDO.
 DEFINE INPUT  PARAMETER pNrPedido          AS INTEGER     NO-UNDO. 
 DEFINE INPUT  PARAMETER pQtAnterior        AS DECIMAL     NO-UNDO.
 DEFINE INPUT  PARAMETER pQtNova            AS DECIMAL     NO-UNDO.
 DEFINE INPUT  PARAMETER pTransacao         AS INT64       NO-UNDO.
 DEFINE INPUT  PARAMETER pHistorico         AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER pNumEtqOrigem      AS INTEGER     NO-UNDO.
 DEFINE INPUT  PARAMETER pQtEtqOrigem       AS DECIMAL     NO-UNDO.
 DEFINE INPUT  PARAMETER pCodEtqLisaOrig    AS CHARACTER   NO-UNDO.  
 DEFINE OUTPUT PARAMETER cErro              AS CHARACTER   NO-UNDO.
 
 
 
 
 DEFINE VARIABLE numEtqMed AS INTEGER     NO-UNDO.
 
 RUN esapi/getNumEtqMed.p(pNumEtqLisa,OUTPUT numEtqMed).
 
 IF numEtqMed = 0 THEN DO:
    ASSIGN cErro = "Etiqueta Lisa:" + STRING(pNumEtqLisa) + " n∆o  encontrada na MED".
    RETURN 'nok'.
 END.
 
 FOR FIRST ob-etiqueta FIELDS(cod-estabel num-etiqueta it-codigo cod-refer nr-container num-rolo  )
    WHERE ob-etiqueta.cod-estabel   = '505' 
    AND   ob-etiqueta.num-etiqueta  = numEtqMed :
    
 END.
 
 CREATE hist_corte_separacao.
 ASSIGN hist_corte_separacao.hist_corte_separacao_id = NEXT-VALUE(seq_hist_corte_separacao) 
        hist_corte_separacao.codigo_etq_lisa         = string(pNumEtqLisa)
        hist_corte_separacao.num_tipo                = pTipoReg
        hist_corte_separacao.num_etiqueta            = NumEtqMed
        hist_corte_separacao.it_codigo               = ob-etiqueta.it-codigo
        hist_corte_separacao.cod_refer               = ob-etiqueta.cod-refer
        hist_corte_separacao.nr_container            = ob-etiqueta.nr-container        
        hist_corte_separacao.num_rolo                = ob-etiqueta.num-rolo
        hist_corte_separacao.qt_nova                 = pQtNova
        hist_corte_separacao.qt_anterior             = pQtAnterior
        hist_corte_separacao.transacao_id            = pTransacao
        hist_corte_separacao.historico               = pHistorico  
        hist_corte_separacao.num_etiqueta_origem     = pNumEtqOrigem
        hist_corte_separacao.qt_etiqueta_origem      = pQtEtqOrigem
        hist_corte_separacao.codigo_etq_lisa_origem  = pCodEtqLisaorig 
        hist_corte_separacao.nr_pedido               = pNrPedido        
        hist_corte_separacao.cod_estabel             = '505'   
        .
       
