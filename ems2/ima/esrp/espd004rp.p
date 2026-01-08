/* Programa: ESPD004
** Modulo..: Fat Repres
** Objetivo: 
1- Atualizar a situa‡Æo de pedidos web aprovados que nÆo mudaram para efetivados.
2- Atualizar a situa‡Æo de pedidos web que foram solicitados altera‡Æo e que nÆo
foram mudaram para solicita‡Æo de altera‡Æo.

** Autor...: Tadeu Silva Parreiras - FEV/2022
**
01/2023 - tadeu - acrescimo da tabela fats_repres_cliente_prod_data 
                - acrescimo dos campos dt_ini_mes e dt_fim_mes nas tabela que tem os campos ano e mes  
02/2024 - acrescimo da tabelas fats_04 para armazenamento de quantidade e pre‡o totais para analise de pre‡o m‚dio por 
produto, referencia, tabela, estab e data
03/2024 - inclusÆo dos valores de devolu‡Æo da tabela fats_04                
*/

/* Parametros de entrada logica obrigatoria */
{esp/util.i}
{esinc/espd004.i}
{esbo/esbo_fatur.i}

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.      

DEFINE TEMP-TABLE ttContainerAtu NO-UNDO
    FIELD nrContainer   AS INT
    FIELD itCodigo      AS CHAR FORMAT 'x(20)'
    FIELD codRefer      AS CHAR 
    FIELD qtVendida     AS DECIMAL
    FIELD qtDevolvida   AS DECIMAL
    INDEX unico IS PRIMARY IS UNIQUE nrContainer itCodigo codRefer.

DEFINE VARIABLE hBoTransacoes   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoFatur        AS HANDLE      NO-UNDO.
DEFINE VARIABLE iTrans          AS INTEGER     NO-UNDO.
DEFINE VARIABLE dtFim           AS DATE        NO-UNDO.
DEFINE VARIABLE iCont           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iQtNF           AS INTEGER     NO-UNDO.
DEFINE VARIABLE anoCorrente     AS INTEGER     NO-UNDO.
DEFINE VARIABLE mesCorrente     AS INTEGER     NO-UNDO.
DEFINE VARIABLE h-acomp         AS HANDLE      NO-UNDO.
DEFINE VARIABLE dPropContainer  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTotEtq         AS DECIMAL     NO-UNDO.

DEFINE VARIABLE codEstabel AS CHARACTER   NO-UNDO.
DEFINE VARIABLE serie      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nrNotaFis  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE itCodigo   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE codRefer   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dPropDevol AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iRand      AS INTEGER     NO-UNDO.

{esapi/extrairDadosRomaneioItem.i} 
RUN utp\ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp("Buscando Dados...").

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

{utp/ut-glob.i}
{include/i-rpvar.i}   
{include/i-rpout.i}
//{include/i-rpcab.i}

RUN esbo/boTransacoes.p PERSIST SET hBoTransacoes.
RUN esbo/esbo_fatur.p   PERSIST SET hBoFatur.


RUN setCodPrograma      IN hBoTransacoes('espd004rp').
RUN setChave            IN hboTransacoes('FAT-' + STRING(TODAY,"99/99/9999")).
RUN setLogin            IN hBoTransacoes(c-seg-usuario).
RUN iniciarTransacao    IN  hboTransacoes.
RUN getIDTransCorrente  IN hboTransacoes(OUTPUT iTrans).

RUN retirarAcomp        IN hBoFatur(YES).
RUN limparTtFatur       IN hBoFatur.
RUN iniciarBos          IN hBoFatur.
RUN setIntervalRepres   IN hBoFatur(tt-param.codRepIni, tt-param.codRepfim).
RUN getUltDiaMesAno(tt-param.anoFim,tt-param.mesFim, OUTPUT dtFim).
RUN setInterValDtEmisNota IN hBoFatur(DATE(tt-param.mesIni,1,tt-param.anoIni),dtFim).

RUN buscarFaturados     IN hBoFatur.
RUN buscarDevolucao     IN hBoFatur.
RUN buscarMetasRepres   IN hBoFatur.
RUN retornarTtFatur     IN hBoFatur(OUTPUT  TABLE tt-Fatur).
RUN exportarTtFatur     IN hBoFatur('c:\temp\fatur.txt').
RUN finalizarBos        IN hBoFatur.


/******* apaga os registros existentes para o periodo e repres do filtro de gera‡Æo*/
RUN pi-acompanhar IN h-acomp('Deletando Fats04').
ASSIGN iCont = 0.
FOR EACH fats_04
    WHERE fats_04.data           >=    getPrimeiroDiaMes(tt-param.AnoIni,tt-Param.mesIni)
    AND   fats_04.data           <=    getUltimoDiaMes(tt-param.AnoFim,tt-Param.mesFim)
    .

    ASSIGN icont = iCont + 1 .
    RUN pi-acompanhar IN h-acomp('Qt.Regs' + STRING(iCont)).
    DELETE fats_04.

END.





FOR EACH fats_repres_clientes_prod_data
    WHERE 
    /*fats_repres_clientes_prod_data.cod_estabel    >=    tt-param.codEstabIni
    AND   fats_repres_clientes_prod_data.cod_estabel    <=    tt-param.codEstabFim
    AND*/   fats_repres_clientes_prod_data.cod_rep      >=    tt-param.codRepIni
    AND   fats_repres_clientes_prod_data.cod_rep        <=    tt-param.codRepFim
    AND   fats_repres_clientes_prod_data.data           >=    getPrimeiroDiaMes(tt-param.AnoIni,tt-Param.mesIni)
    AND   fats_repres_clientes_prod_data.data           <=    getUltimoDiaMes(tt-param.AnoFim,tt-Param.mesFim)
    .

    ASSIGN icont = iCont + 1 .
    RUN pi-acompanhar IN h-acomp('Qt.Regs' + STRING(iCont)).
    DELETE fats_repres_clientes_prod_data.

END.
FOR EACH fats_repres_clientes_prod
    WHERE 
    /*fats_repres_clientes_prod.cod_estabel    >=    tt-param.codEstabIni
    AND   fats_repres_clientes_prod.cod_estabel    <=    tt-param.codEstabFim
    AND*/  fats_repres_clientes_prod.cod_rep        >=    tt-param.codRepIni
    AND   fats_repres_clientes_prod.cod_rep        <=    tt-param.codRepFim         
    AND   fats_repres_clientes_prod.ano            >=    tt-param.AnoIni
    AND   fats_repres_clientes_prod.ano            <=    tt-param.AnoFim
    AND   fats_repres_clientes_prod.mes            >=    tt-param.mesIni
    AND   fats_repres_clientes_prod.mes            <=    tt-param.mesFim.

    ASSIGN icont = iCont + 1 .
    RUN pi-acompanhar IN h-acomp('Qt.Regs' + STRING(iCont)).
    DELETE fats_repres_clientes_prod.

END.
RUN pi-acompanhar IN h-acomp('Deletando Dados Repres x Clientes').
ASSIGN iCont = 0.
FOR EACH fats_repres_clientes
    WHERE 
    /*fats_repres_clientes.cod_estabel    >=    tt-param.codEstabIni
    AND   fats_repres_clientes.cod_estabel    <=    tt-param.codEstabFim
    AND*/   fats_repres_clientes.cod_rep        >=    tt-param.codRepIni
    AND   fats_repres_clientes.cod_rep        <=    tt-param.codRepFim
    AND   fats_repres_clientes.ano            >=    tt-param.AnoIni
    AND   fats_repres_clientes.ano            <=    tt-param.AnoFim
    AND   fats_repres_clientes.mes            >=    tt-param.mesIni
    AND   fats_repres_clientes.mes            <=    tt-param.mesFim.

    ASSIGN icont = iCont + 1 .
    RUN pi-acompanhar IN h-acomp('Qt.Regs' + STRING(iCont)).
    DELETE fats_repres_clientes.

END.

RUN pi-acompanhar IN h-acomp('Deletando Dados Repres').
ASSIGN iCont = 0.
FOR EACH fats_repres
    WHERE 
    /*fats_repres.cod_estabel    >=    tt-param.codEstabIni
    AND   fats_repres.cod_estabel    <=    tt-param.codEstabFim
    AND*/   fats_repres.cod_rep      >=    tt-param.codRepIni
    AND   fats_repres.cod_rep        <=    tt-param.codRepFim
    AND   fats_repres.ano            >=    tt-param.AnoIni
    AND   fats_repres.ano            <=    tt-param.AnoFim
    AND   fats_repres.mes            >=    tt-param.mesIni
    AND   fats_repres.mes            <=    tt-param.mesFim.

    ASSIGN icont = iCont + 1 .
    RUN pi-acompanhar IN h-acomp('Qt.Regs' + STRING(iCont)).
    DELETE fats_repres.

END.

RUN pi-acompanhar IN h-acomp('gravando dados extraidos').

FOR EACH tt-fatur
    BREAK 
    //BY tt-fatur.cod_rep 
    BY tt-fatur.tipo BY tt-fatur.nr_nf.

    ASSIGN anoCorrente = year(tt-fatur.dt_emis)
           mesCorrente = MONTH(tt-fatur.dt_emis).

    IF FIRST-OF(tt-fatur.nr_nf) THEN  DO:
       ASSIGN iQtNF = 1.

    END.
    ELSE DO:
       ASSIGN iQtNF = 0.
    END.

    /*************** Fats_04 por Estab,item ,ref, container, tabela de preco, data ****************************************/
    
    IF tt-fatur.tipo = 'fat' OR tt-fatur.tipo = 'dev'  THEN DO:
       EMPTY TEMP-TABLE ttContainer.
       RUN esapi/extrairDadosRomaneioItem.p(tt-fatur.cod_estabel,
                                            tt-fatur.serie,
                                            tt-fatur.nr_nf,
                                            tt-fatur.cod_item,
                                            tt-fatur.cod-refer,
                                            tt-fatur.tipo,
                                            OUTPUT TABLE ttContainer).
       /*IF tt-fatur.cod_item = '535541' THEN DO:
          OUTPUT TO c:\temp\535541.txt .
          FOR EACH ttContainer:
              DISP ttContainer WITH 1 COL WIDTH 550.
          END.

          OUTPUT CLOSE.
       END.*/
       ASSIGN dTotEtq = 0.
       FOR EACH ttContainer.
   
          //ASSIGN  dPropContainer = ttContainer.quantidade / tt-fatur.qt_nf.
           ASSIGN  dPropContainer = ttContainer.perc .
          RUN sincrFat04(tt-fatur.tipo,
                         tt-fatur.tb_preco_id,  
                         tt-fatur.LOG_rubix,    
                         ttContainer.nrContainer, 
                         tt-fatur.cod_item, 
                         tt-fatur.cod-refer,  
                         tt-fatur.cod_estabel,
                         tt-fatur.dt_emis,     
                         tt-fatur.qt_nf,     
                         (tt-fatur.vl_tot_it_nf  + tt-fatur.vl_12)  ,         
                         dPropContainer
                         ).
          ASSIGN dTotEtq   = dTotEtq + ttContainer.quantidade.
   
       END.
       IF tt-fatur.qt_nf <> dTotEtq THEN DO:
             ASSIGN  dPropContainer = (tt-fatur.qt_nf - dTotEtq) / tt-fatur.qt_nf.
             RUN sincrFat04( tt-fatur.tipo,
                             tt-fatur.tb_preco_id,  
                             tt-fatur.LOG_rubix,    
                             0, 
                             tt-fatur.cod_item, 
                             tt-fatur.cod-refer,  
                             tt-fatur.cod_estabel,
                             tt-fatur.dt_emis,     
                             (tt-fatur.qt_nf - dTotEtq),     
                             (tt-fatur.vl_tot_it_nf + tt-fatur.vl_12),         
                             dPropContainer
                             ).
       END.
    END.


    
   /***************************************************************************************************************/






    /*************** por Estab, repres, cliente, item , data ****************************************/
    FIND fats_repres_clientes_prod_data 
         WHERE fats_repres_clientes_prod_data.cod_estabel         = tt-fatur.cod_estabel
         AND   fats_repres_clientes_prod_data.data                = tt-fatur.dt_emis
         AND   fats_repres_clientes_prod_data.it_codigo           = tt-fatur.cod_item
         AND   fats_repres_clientes_prod_data.cod_rep             = tt-fatur.cod_repres
         AND   fats_repres_clientes_prod_data.cod_emitente        = tt-fatur.cod_cli
         AND   fats_repres_clientes_prod_data.data                = tt-fatur.dt_emis
         NO-ERROR.
    IF NOT AVAIL fats_repres_clientes_prod_data THEN DO:
       
       CREATE fats_repres_clientes_prod_data.
       ASSIGN fats_repres_clientes_prod_data.fat_repres_cliente_prod_data_id = NEXT-VALUE(seq_fats_repres_cliente_prod_dt)
       fats_repres_clientes_prod_data.transacao_id       = iTrans
       fats_repres_clientes_prod_data.cod_estabel        = tt-fatur.cod_estabel
       fats_repres_clientes_prod_data.it_codigo          = tt-fatur.cod_item  
       fats_repres_clientes_prod_data.cod_rep            = tt-fatur.cod_repres
       fats_repres_clientes_prod_data.cod_emitente       = tt-fatur.cod_cli   
       fats_repres_clientes_prod_data.data               = tt-fatur.dt_emis .
       FIND ITEM 
           WHERE ITEM.it-codigo = fats_repres_clientes_prod_data.it_codigo NO-LOCK NO-ERROR.
       ASSIGN 
       fats_repres_clientes_prod_data.um                 = IF AVAIL ITEM THEN ITEM.un ELSE ''
        .
        
    END.  
    
    IF tt-fatur.tipo = 'fat' THEN
       ASSIGN fats_repres_clientes_prod_data.vl_faturamento =  fats_repres_clientes_prod_data.vl_faturamento + tt-fatur.vl_tot_it_nf
              fats_repres_clientes_prod_data.vl_desconto    =  fats_repres_clientes_prod_data.vl_desconto    + tt-fatur.vl_12
              fats_repres_clientes_prod_data.qt_fat         =  fats_repres_clientes_prod_data.qt_fat         + tt-fatur.qt_nf .

    IF tt-fatur.tipo = 'dev' THEN
       ASSIGN fats_repres_clientes_prod_data.vl_devolucoes  =  fats_repres_clientes_prod_data.vl_devolucoes     + tt-fatur.vl_tot_it_nf
              fats_repres_clientes_prod_data.vl_desconto    =  fats_repres_clientes_prod_data.vl_desconto_dev   + tt-fatur.vl_12
              fats_repres_clientes_prod_data.qt_dev         =  fats_repres_clientes_prod_data.qt_dev            + tt-fatur.qt_nf.




    /*************** por Estab, repres, cliente, item , mes e ano****************************************/
    FIND fats_repres_clientes_prod 
         WHERE fats_repres_clientes_prod.cod_estabel        = tt-fatur.cod_estabel
         AND   fats_repres_clientes_prod.mes                = mesCorrente
         AND   fats_repres_clientes_prod.ano                = anoCorrente
         AND   fats_repres_clientes_prod.it_codigo          = tt-fatur.cod_item
         AND   fats_repres_clientes_prod.cod_rep            = tt-fatur.cod_repres
         AND   fats_repres_clientes_prod.cod_emitente       = tt-fatur.cod_cli
         NO-ERROR.
    IF NOT AVAIL fats_repres_clientes_prod THEN DO:
       CREATE fats_repres_clientes_prod.
       ASSIGN fats_repres_clientes_prod.fat_repres_cliente_prod_id = NEXT-VALUE(seq_fats_repres_cliente_prod)
       fats_repres_clientes_prod.transacao_id       = iTrans
       fats_repres_clientes_prod.cod_estabel        = tt-fatur.cod_estabel
       fats_repres_clientes_prod.mes                = mesCorrente
       fats_repres_clientes_prod.ano                = anoCorrente
       fats_repres_clientes_prod.dt_ini_mes         = getPrimeiroDiaMes(fats_repres_clientes_prod.ano,fats_repres_clientes_prod.mes)
       fats_repres_clientes_prod.dt_fim_mes         = getUltimoDiaMes(fats_repres_clientes_prod.ano,fats_repres_clientes_prod.mes)
       fats_repres_clientes_prod.it_codigo          = tt-fatur.cod_item  
       fats_repres_clientes_prod.cod_rep            = tt-fatur.cod_repres
       fats_repres_clientes_prod.cod_emitente       = tt-fatur.cod_cli   .
    END.  
    
    IF tt-fatur.tipo = 'fat' THEN
       ASSIGN fats_repres_clientes_prod.vl_faturamento =  fats_repres_clientes_prod.vl_faturamento + tt-fatur.vl_tot_it_nf
              fats_repres_clientes_prod.vl_desconto    =  fats_repres_clientes_prod.vl_desconto    + tt-fatur.vl_12
              fats_repres_clientes_prod.qt_fat         =  fats_repres_clientes_prod.qt_fat         + tt-fatur.qt_nf .

    IF tt-fatur.tipo = 'dev' THEN
       ASSIGN fats_repres_clientes_prod.vl_devolucoes  =  fats_repres_clientes_prod.vl_devolucoes     + tt-fatur.vl_tot_it_nf
              fats_repres_clientes_prod.vl_desconto    =  fats_repres_clientes_prod.vl_desconto_dev   + tt-fatur.vl_12
              fats_repres_clientes_prod.qt_dev         =  fats_repres_clientes_prod.qt_dev            + tt-fatur.qt_nf.

    /*************** por repres, cliente, mes e ano****************************************/
    
    FIND fats_repres_clientes
         WHERE fats_repres_clientes.cod_estabel  = tt-fatur.cod_estabel
         AND   fats_repres_clientes.mes          = mesCorrente
         AND   fats_repres_clientes.ano          = anoCorrente
         AND   fats_repres_clientes.cod_rep      = tt-fatur.cod_repres
         AND   fats_repres_clientes.cod_emitente = tt-fatur.cod_cli 
         NO-ERROR.
    IF  NOT AVAIL fats_repres_clientes THEN DO:
        CREATE fats_repres_clientes.
        ASSIGN fats_repres_clientes.fat_repres_cliente_id = NEXT-VALUE(seq_fats_repres_cliente)
        fats_repres_clientes.transacao_id = iTrans
        fats_repres_clientes.cod_estabel  = tt-fatur.cod_estabel
        fats_repres_clientes.mes          = mesCorrente
        fats_repres_clientes.ano          = anoCorrente
        fats_repres_clientes.dt_ini_mes   = getPrimeiroDiaMes(fats_repres_clientes.ano,fats_repres_clientes.mes)
        fats_repres_clientes.dt_fim_mes   = getUltimoDiaMes(fats_repres_clientes.ano,fats_repres_clientes.mes)
        fats_repres_clientes.cod_rep      = tt-fatur.cod_repres
        fats_repres_clientes.cod_emitente = tt-fatur.cod_cli.
    END.

    IF tt-fatur.tipo = 'fat' THEN
       ASSIGN fats_repres_clientes.vl_faturamento =  fats_repres_clientes.vl_faturamento + tt-fatur.vl_tot_it_nf
              fats_repres_clientes.vl_desconto    =  fats_repres_clientes.vl_desconto    + tt-fatur.vl_12
              fats_repres_clientes.qt_fat         =  fats_repres_clientes.qt_fat         + tt-fatur.qt_nf .
              .

    IF tt-fatur.tipo = 'dev' THEN
       ASSIGN fats_repres_clientes.vl_devolucoes  =  fats_repres_clientes.vl_devolucoes     + tt-fatur.vl_tot_it_nf
              fats_repres_clientes.vl_desconto    =  fats_repres_clientes.vl_desconto_dev   + tt-fatur.vl_12
              fats_repres_clientes.qt_dev         =  fats_repres_clientes.qt_dev            + tt-fatur.qt_nf.

    /*************** por repres, mes e ano****************************************/
    
    FIND fats_repres
         WHERE fats_repres.cod_estabel  = tt-fatur.cod_estabel
         AND   fats_repres.mes          = mesCorrente
         AND   fats_repres.ano          = anoCorrente
         AND   fats_repres.cod_rep      = tt-fatur.cod_repres
         NO-ERROR.

    IF NOT AVAIL fats_repres THEN DO:
        CREATE fats_repres.
        ASSIGN fats_repres.fat_repres_id = NEXT-VALUE(seq_fats_repres)
        fats_repres.transacao_id = iTrans
        fats_repres.cod_estabel  = tt-fatur.cod_estabel
        fats_repres.mes          = mesCorrente
        fats_repres.ano          = anoCorrente
        fats_repres.dt_ini_mes   = getPrimeiroDiaMes(fats_repres.ano,fats_repres.mes)
        fats_repres.dt_fim_mes   = getUltimoDiaMes(fats_repres.ano,fats_repres.mes)
        fats_repres.cod_rep      = tt-fatur.cod_repres.
    END.

    CASE tt-fatur.tipo:
        WHEN 'fat' THEN DO:
            ASSIGN fats_repres.vl_faturamento           =  fats_repres.vl_faturamento           + tt-fatur.vl_tot_it_nf
                   fats_repres.vl_desconto              =  fats_repres.vl_desconto              + tt-fatur.vl_12
                   fats_repres.qt_nf                    =  fats_repres.qt_nf                    + iQtNf
                   fats_repres.qt_fat                   =  fats_repres.qt_fat                   + tt-fatur.qt_nf.

        END.
        WHEN  'dev' THEN DO:
             ASSIGN fats_repres.vl_devolucoes   =  fats_repres.vl_devolucoes     + tt-fatur.vl_tot_it_nf
              fats_repres.vl_desconto           =  fats_repres.vl_desconto_dev   + tt-fatur.vl_12
              fats_repres.qt_nf_dev             =  fats_repres.qt_nf_dev         + iQtNf
              fats_repres.qt_dev                =  fats_repres.qt_dev            + tt-fatur.qt_nf.
        END.

        WHEN  'metas' THEN DO:
            ASSIGN fats_repres.vl_meta  =  fats_repres.vl_meta  + tt-fatur.vl_tot_it_nf.
        END.


    END CASE.
END.

RUN atualizarVendasContainer.

RUN finalizarTransacao IN hBoTransacoes(1).

IF VALID-HANDLE(hBoFatur) THEN
    DELETE PROCEDURE hBoFatur.

IF VALID-HANDLE(hBoTransacoes) THEN
    DELETE PROCEDURE hBoTransacoes.
RUN pi-finalizar IN h-acomp.


PROCEDURE sincrFat04:

    DEFINE INPUT  PARAMETER pTipoReg        AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTbPrecoId      AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pLogRubix       AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pNrContainer    AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pItCodigo       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodRefer       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodEstabel     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pDtEmis         AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pQt             AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pValor          AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pPropContainer  AS DECIMAL     NO-UNDO.
    


    FIND fats_04
    WHERE fats_04.tb_preco_id  = pTbPrecoId
    AND   fats_04.LOG_outlet   = pLogRubix
    AND   fats_04.num_origem   = pNrcontainer
    AND   fats_04.it_codigo    = pItCodigo
    AND   fats_04.cod_refer    = pCodRefer
    AND   fats_04.cod_estabel  = pCodEstabel
    AND   fats_04.data         = pDtEmis               
    NO-ERROR.
    IF NOT AVAIL fats_04 THEN DO:
      CREATE fats_04.
      ASSIGN fats_04.fat_04_id = NEXT-VALUE(seq_fat_04)
             fats_04.tb_preco_id  = pTbPrecoId     
             fats_04.LOG_outlet   = pLogRubix      
             fats_04.num_origem   = pNrcontainer   
             fats_04.it_codigo    = pItCodigo      
             fats_04.cod_refer    = pCodRefer      
             fats_04.cod_estabel  = pCodEstabel    
             fats_04.data         = pDtEmis 
             fats_04.trans_id     = iTrans . 
    END.
    IF pTipoReg = 'fat' THEN DO:
       ASSIGN fats_04.qt_total     = fats_04.qt_total   + (pQt * pPropContainer)
              fats_04.vl_total     = fats_04.vl_total   + (pValor * pPropContainer) .
    END.
    IF pTipoReg = 'dev' THEN DO:
       ASSIGN fats_04.qt_dev_total     = fats_04.qt_dev_total   + (pQt * pPropContainer ) 
              fats_04.vl_dev_total     = fats_04.vl_dev_total   + (pValor * pPropContainer) .
    END.
    ASSIGN fats_04.vl_medio            = (fats_04.vl_total + fats_04.vl_dev_total)/ (fats_04.qt_total + fats_04.qt_dev_total ) .





END PROCEDURE.


PROCEDURE atualizarVendasContainer:



    FOR EACH fats_04 NO-LOCK
    WHERE fats_04.data           >=    getPrimeiroDiaMes(tt-param.AnoIni,tt-Param.mesIni)
    AND   fats_04.data           <=    getUltimoDiaMes(tt-param.AnoFim,tt-Param.mesFim).

        FIND ttContainerAtu
            WHERE ttContainerAtu.nrContainer = fats_04.num_origem
            AND   ttContainerAtu.itCodigo    = fats_04.it_Codigo
            AND   ttContainerAtu.codRefer    = fats_04.cod_Refer NO-ERROR.
        IF NOT AVAIL ttContainerAtu THEN DO:
           CREATE ttContainerAtu.
           ASSIGN ttContainerAtu.nrContainer =  fats_04.num_origem
                  ttContainerAtu.itCodigo    = fats_04.it_Codigo
                  ttContainerAtu.codRefer    = fats_04.cod_Refer
                 .

        END.
    END.

    FOR EACH ttContainerAtu:
        FOR EACH fats_04 NO-LOCK
            WHERE fats_04.num_origem = ttContainerAtu.nrContainer
            AND   fats_04.it_codigo  = ttContainerAtu.itCodigo
            AND   fats_04.cod_Refer  = ttContainerATu.codRefer .

            ASSIGN ttContainerAtu.qtVendida   = ttContainerAtu.qtVendida   + fats_04.qt_total
                   ttContainerAtu.qtDevolvida = ttContainerAtu.qtDevolvida + fats_04.qt_dev_total
                   .
        END.
        FIND pp-it-container EXCLUSIVE-LOCK
            WHERE pp-it-container.nr-container  = ttContainerAtu.nrContainer
            AND   pp-it-container.it-codigo     = ttContainerAtu.itCodigo
            AND   pp-it-container.cod-refer     = ttContainerAtu.codRefer
            NO-ERROR.
        IF AVAIL pp-it-container THEN DO:
           ASSIGN pp-it-container.qt_faturada  = ttContainerAtu.qtVendida
                  pp-it-container.qt_devolvida = ttContainerAtu.qtDevolvida
                  .
           RELEASE pp-it-container.
        END.                       
    END.


END PROCEDURE.
