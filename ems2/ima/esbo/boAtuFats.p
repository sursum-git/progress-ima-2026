/******************************************************************************
Programa: esbo/boAtuFats.p
objetivo: Orquestar a atualiza‡Æo das tabelas de faturamento utilizadas
para montar consultas BI .
autor: Tadeu Silva
data: 05/2024
*****************************************************************************/
DEFINE VARIABLE hBoMsg              AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoTransacao        AS HANDLE      NO-UNDO.
DEFINE VARIABLE HBoFatur            AS HANDLE      NO-UNDO.
DEFINE VARIABLE hAcomp              AS HANDLE      NO-UNDO.
DEFINE VARIABLE idTransacao         AS INT64       NO-UNDO.
DEFINE VARIABLE dtInicial           AS DATE        NO-UNDO.
DEFINE VARIABLE dtFinal             AS DATE        NO-UNDO.
DEFINE VARIABLE pedidoExecucaoID    AS INTEGER     NO-UNDO.
//DEFINE VARIABLE lSoMeta             AS LOGICAL     NO-UNDO.


{esbo/boAtuFats.i}


{esp/util.i}
{utp/ut-glob.i}
{esapi/extrairDadosRomaneioItem.i} 
{esapi/getIdsTransPorDtFats.i ttTransacoes}
{esbo/esbo_fatur.i}
{esp/paramsLog.i}

DEFINE TEMP-TABLE ttContainerAcum NO-UNDO LIKE ttContainer
    FIELD cod_estabel       AS CHAR
    FIELD serie             AS CHAR
    FIELD nr_nf             AS CHAR FORMAT 'x(15)'
    FIELD cod_item          AS CHAR FORMAT 'x(20)'
    FIELD cod_refer         AS CHAR
    INDEX ind-pri-2         AS  PRIMARY cod_estabel serie nr_nf cod_item cod_Refer nrContainer
    .

PROCEDURE iniciar:

    RUN esbo/boMsg.p      PERSIST SET hBoMsg .
    RUN esbo/boTransacoes PERSIST SET hBoTransacao .
    RUN esbo/esbo_fatur   PERSIST SET hBoFatur.
    RUN iniciarBOs        IN  hBoFatur.

END PROCEDURE.



PROCEDURE finalizar:

  IF VALID-HANDLE(hboMsg) THEN
     DELETE PROCEDURE hBoMsg.

  IF VALID-HANDLE(hboTransacao) THEN
     DELETE PROCEDURE hBoTransacao.

  IF VALID-HANDLE(hboFatur) THEN DO:
      RUN finalizarBos IN hBoFatur.
     DELETE PROCEDURE hBoFatur.
  END.

END PROCEDURE.
              
PROCEDURE setIDPedidoExecucao:

    DEFINE INPUT  PARAMETER pId AS INTEGER     NO-UNDO.
    ASSIGN pedidoExecucaoID = pId .

END PROCEDURE.
              
PROCEDURE setHAComp:

    DEFINE INPUT  PARAMETER phAcomp AS HANDLE      NO-UNDO.
    ASSIGN hAcomp = pHAcomp .

END PROCEDURE.
              
              
PROCEDURE setTransacaoID:
    
    DEFINE INPUT  PARAMETER pTransacao AS INT64     NO-UNDO.
    ASSIGN idTransacao = pTransacao.
              
END PROCEDURE.
              
PROCEDURE setInterValDatas:

    DEFINE INPUT  PARAMETER pDtIni AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pDtFim AS DATE        NO-UNDO.

    ASSIGN dtInicial = pDtIni
           dtFinal   = pDtFim .

END PROCEDURE.

/*PROCEDURE setSoMetaRepres:

    DEFINE INPUT  PARAMETER pSoMeta AS LOGICAL     NO-UNDO.

    ASSIGN lSoMeta = pSoMeta.
              
END PROCEDURE.*/



PROCEDURE exec:

    DEFINE VARIABLE cChave AS CHARACTER   NO-UNDO.


    ASSIGN cChave = 'FAT-' + string(dtInicial,'99/99/9999')
                     + "-" + string(dtFinal,'99/99/9999') .
    IF pedidoExecucaoID <> 0 THEN DO:
       ASSIGN cChave = cChave + "-" + STRING(pedidoExecucaoID).
    END.

   DO ON ERROR UNDO, THROW:
        RUN setCodPrograma          IN hBoTransacao(PROGRAM-NAME(1)).
        RUN setChave                IN hBoTransacao(cChave).
        RUN setLogin                IN hBoTransacao(c-seg-usuario).
        RUN iniciarTransacao        IN hBoTransacao.
        RUN getIdTransCorrente      IN hBoTransacao(OUTPUT idTransacao).
        
        //busca dados de faturamento atualizados
        RUN esapi/getDadosFaturPorData.p(dtInicial,
                                       dtFinal,
                                       pedidoExecucaoID <> 0, //retirar Acompanhamento
                                       INPUT-OUTPUT TABLE tt-fatur
                                       ).
        
    
        {esp/exportarTabelaCsv2.i tt-fatur}
        //busca ids de transa‡äes que terÆo seus dados substuidos
        RUN esapi/getIdsTransPorDtFats.p(dtInicial,dtFinal,OUTPUT TABLE ttTransacoes).
         {esp/exportarTabelaCsv2.i ttTransacoes} 
    
        
        //atualiza os dados buscados da bo de faturamento
        RUN atuFats.
    
        
        //delete oa dados dos ids de transa‡äes da tabela ttTransaoes
        RUN esapi\apagarDadosIds.p('todas',
                                   INPUT TABLE ttTransacoes, 
                                   INPUT dtInicial,
                                   INPUT dtFinal, 
                                   INPUT TABLE ttAnoMes,
                                   INPUT TABLE ttContainerPend
                                   ).
      
        RUN finalizarTransacao IN hBoTransacao(1).

        
        
        CATCH eAnyError AS Progress.Lang.ERROR:
         RUN setMsg IN hBoMsg(99, eAnyError:GetMessage(1) + "-" +  string(eAnyError:GetMessageNum(1)),'erro' ). 
        END CATCH.
   END.           

END PROCEDURE.

PROCEDURE getResult:
   DEFINE OUTPUT PARAMETER TABLE FOR tt-fatur.
END PROCEDURE.

PROCEDURE atuFats:

    
    RUN _calcTTAnoMes(dtInicial,dtFinal).
    RUN _calcTtContainerPend.
    EMPTY TEMP-TABLE ttContainerAcum.

    FOR EACH tt-fatur.
        FIND LAST fats_repres_clientes_prod_data 
            WHERE fats_repres_clientes_prod_data.cod_estabel         = tt-fatur.cod_estabel
            AND   fats_repres_clientes_prod_data.data                = tt-fatur.dt_emis
            AND   fats_repres_clientes_prod_data.it_codigo           = tt-fatur.cod_item
            AND   fats_repres_clientes_prod_data.cod_rep             = tt-fatur.cod_repres
            AND   fats_repres_clientes_prod_data.cod_emitente        = tt-fatur.cod_cli
            AND   fats_repres_clientes_prod_data.data                = tt-fatur.dt_emis 
            AND   fats_repres_clientes_prod_data.transacao_id        = idTransacao NO-LOCK NO-ERROR.
        
        IF NOT AVAIL fats_repres_clientes_prod_data THEN DO:
           CREATE fats_repres_clientes_prod_data.
           ASSIGN fats_repres_clientes_prod_data.fat_repres_cliente_prod_data_id = NEXT-VALUE(seq_fats_repres_cliente_prod_dt)
           fats_repres_clientes_prod_data.transacao_id       = idTransacao
           fats_repres_clientes_prod_data.cod_estabel        = tt-fatur.cod_estabel
           fats_repres_clientes_prod_data.it_codigo          = tt-fatur.cod_item  
           fats_repres_clientes_prod_data.cod_rep            = tt-fatur.cod_repres
           fats_repres_clientes_prod_data.cod_emitente       = tt-fatur.cod_cli   
           fats_repres_clientes_prod_data.data               = tt-fatur.dt_emis .
           FIND ITEM 
               WHERE ITEM.it-codigo = fats_repres_clientes_prod_data.it_codigo NO-LOCK NO-ERROR.
           ASSIGN fats_repres_clientes_prod_data.um                 = IF AVAIL ITEM THEN ITEM.un ELSE '' .
        END.  
        RUN acumularValData.
        RUN sincrFats04.
    END.
    {esp/exportarTabelaCsv3.i ttContainerAcum " " " " "ttContainerAcum" }



    // limpa os dados antigos da tabela por data
    RUN esapi\apagarDadosIds.p(    INPUT "fats_Repres_clientes_prod_data",
                                   INPUT TABLE ttTransacoes, 
                                   INPUT dtInicial,
                                   INPUT dtFinal, 
                                   INPUT TABLE ttAnoMes,
                                   INPUT TABLE ttContainerPend
                                   ).

    //faz a atualiza‡Æo das demais tabela de ano/mes a partir da tabela por data
    
    FOR EACH ttAnoMes:
        FOR EACH fats_repres_clientes_prod_data NO-LOCK
            WHERE fats_repres_clientes_prod_data.data >= getPrimeiroDiaMes(ttAnoMes.ano,ttAnoMes.mes)  
            AND   fats_repres_clientes_prod_data.data <= getUltimoDiaMes(ttAnoMes.ano, ttAnoMes.mes).
            
            RUN sincrFatsRepresClientesProd .
            RUN sincrFatsRepresClientes.
            RUN sincrFatsRepres.
        END.                    
    END.
    
    {esp/exportarTabelaCsv3.i tt-Fatur " where tipo ='metas' " " " "ttFaturMeta" }
    //faz a atualiza‡Æo apenas da meta
    FOR EACH tt-fatur
        WHERE tt-fatur.tipo = 'metas':
        RUN sincrFatsRepresMeta.
    END.

    //faz a atualiza‡Æo das vendas por container para os containers que sofreram altera‡Æo
    FOR EACH ttContainerPend:
        ASSIGN ttContainerPend.qtFaturada = 0.
        FOR EACH fats_04 NO-LOCK
            WHERE fats_04.num_origem = ttContainerPend.nrContainer
            AND   fats_04.it_codigo  = ttContainerPend.itCodigo
            AND   fats_04.cod_refer  = ttContainerPend.codRefer :
            ASSIGN ttContainerPend.qtFaturada = ttContainerPend.qtFaturada +  fats_04.qt_total - fats_04.qt_dev_total .
        END.                                                                                                           
        FIND pp-it-container EXCLUSIVE-LOCK
            WHERE pp-it-container.nr-container = ttContainerPend.nrContainer
            AND   pp-it-container.it-codigo    = ttContainerPend.itCodigo   
            AND   pp-it-container.cod-refer    = ttContainerPend.codRefer NO-ERROR.
        IF AVAIL pp-it-container THEN DO:
           ASSIGN pp-it-container.qt_faturada = ttContainerPend.qtFaturada
                  pp-it-container.transacao_id_qt_fat = idTransacao. 
           RELEASE pp-it-container.
        END.
    END.




END PROCEDURE.    

PROCEDURE acumularValData:

   FIND CURRENT fats_repres_clientes_prod_data EXCLUSIVE-LOCK.

   IF tt-fatur.tipo = 'fat' THEN
      ASSIGN fats_repres_clientes_prod_data.vl_faturamento =  fats_repres_clientes_prod_data.vl_faturamento     + tt-fatur.vl_tot_it_nf
             fats_repres_clientes_prod_data.vl_desconto    =  fats_repres_clientes_prod_data.vl_desconto        + tt-fatur.vl_12
             fats_repres_clientes_prod_data.qt_fat         =  fats_repres_clientes_prod_data.qt_fat             + tt-fatur.qt_nf .

   IF tt-fatur.tipo = 'dev' THEN
      ASSIGN fats_repres_clientes_prod_data.vl_devolucoes  =  fats_repres_clientes_prod_data.vl_devolucoes      + tt-fatur.vl_tot_it_nf
             fats_repres_clientes_prod_data.vl_desconto    =  fats_repres_clientes_prod_data.vl_desconto_dev    + tt-fatur.vl_12
             fats_repres_clientes_prod_data.qt_dev         =  fats_repres_clientes_prod_data.qt_dev             + tt-fatur.qt_nf .

   RELEASE fats_repres_clientes_prod_data.

END PROCEDURE.



PROCEDURE sincrFats04:

    DEFINE VARIABLE dTotEtq         AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dPropContainer  AS DECIMAL     NO-UNDO.

    IF tt-fatur.tipo = 'fat' OR tt-fatur.tipo = 'dev'  THEN DO:
       EMPTY TEMP-TABLE ttContainer.
       RUN esapi/extrairDadosRomaneioItem.p(tt-fatur.cod_estabel,
                                            tt-fatur.serie      ,
                                            tt-fatur.nr_nf      ,
                                            tt-fatur.cod_item   ,
                                            tt-fatur.cod-refer  ,
                                           OUTPUT TABLE ttContainer).
       //acumulando dados para log
       FOR EACH ttContainer.
           CREATE ttContainerAcum.
           ASSIGN ttContainerAcum.cod_estabel   = tt-fatur.cod_estabel
                  ttContainerAcum.serie         = tt-fatur.serie
                  ttContainerAcum.nr_nf         = tt-fatur.nr_nf
                  ttContainerAcum.cod_item      = tt-fatur.cod_item
                  ttContainerAcum.cod_refer     = tt-fatur.cod-refer
                  .
           BUFFER-COPY ttContainer TO ttContainerAcum.
       END.

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

END PROCEDURE.



PROCEDURE _calcTtContainerPend:

    FOR EACH fats_04 NO-LOCK
        WHERE fats_04.trans_id = idTransacao.
        FIND ttContainerPend
            WHERE ttContainerPend.nrContainer = fats_04.num_origem 
            AND   ttContainerPend.itCodigo    = fats_04.it_codigo
            AND   ttContainerPend.codRefer    = fats_04.cod_refer
            NO-LOCK NO-ERROR.
        IF NOT AVAIL ttContainerPend THEN DO:
           CREATE ttContainerPend.
           ASSIGN ttContainerPend.nrContainer = fats_04.num_origem
                  ttContainerPend.itCodigo    = fats_04.it_codigo
                  ttContainerPend.codRefer    = fats_04.cod_refer
                  .
        END.
    END.


END PROCEDURE.





PROCEDURE sincrFatsRepresClientesProd:

    /*************** por Estab, repres, cliente, item , mes e ano****************************************/
    FIND LAST fats_repres_clientes_prod 
         WHERE fats_repres_clientes_prod.cod_estabel        = fats_repres_clientes_prod_data.cod_estabel
         AND   fats_repres_clientes_prod.mes                = MONTH(fats_repres_clientes_prod_data.data)
         AND   fats_repres_clientes_prod.ano                = YEAR(fats_repres_clientes_prod_data.data)
         AND   fats_repres_clientes_prod.it_codigo          = fats_repres_clientes_prod_data.it_codigo
         AND   fats_repres_clientes_prod.cod_rep            = fats_repres_clientes_prod_data.cod_rep
         AND   fats_repres_clientes_prod.cod_emitente       = fats_repres_clientes_prod_data.cod_emitente
         AND   fats_repres_clientes_prod.transacao_id       = idTransacao
         NO-ERROR.
    IF NOT AVAIL fats_repres_clientes_prod THEN DO:
       CREATE fats_repres_clientes_prod.
       ASSIGN fats_repres_clientes_prod.fat_repres_cliente_prod_id = NEXT-VALUE(seq_fats_repres_cliente_prod)
       fats_repres_clientes_prod.transacao_id       = idTransacao
       fats_repres_clientes_prod.cod_estabel        = fats_repres_clientes_prod_data.cod_estabel
       fats_repres_clientes_prod.mes                = month(fats_repres_clientes_prod_data.data)
       fats_repres_clientes_prod.ano                = year(fats_repres_clientes_prod_data.data)
       fats_repres_clientes_prod.dt_ini_mes         = getPrimeiroDiaMes(fats_repres_clientes_prod.ano,fats_repres_clientes_prod.mes)
       fats_repres_clientes_prod.dt_fim_mes         = getUltimoDiaMes(fats_repres_clientes_prod.ano,fats_repres_clientes_prod.mes)
       fats_repres_clientes_prod.it_codigo          = fats_repres_clientes_prod_data.it_codigo  
       fats_repres_clientes_prod.cod_rep            = fats_repres_clientes_prod_data.cod_rep
       fats_repres_clientes_prod.cod_emitente       = fats_repres_clientes_prod_data.cod_emitente   .
    END.  
    
    ASSIGN fats_repres_clientes_prod.vl_faturamento =  fats_repres_clientes_prod.vl_faturamento     + fats_repres_clientes_prod_data.vl_faturamento
           fats_repres_clientes_prod.vl_desconto    =  fats_repres_clientes_prod.vl_desconto        + fats_repres_clientes_prod_data.vl_desconto
           fats_repres_clientes_prod.qt_fat         =  fats_repres_clientes_prod.qt_fat             + fats_repres_clientes_prod_data.qt_fat 
           fats_repres_clientes_prod.vl_devolucoes  =  fats_repres_clientes_prod.vl_devolucoes      + fats_repres_clientes_prod_data.vl_devolucoes
           fats_repres_clientes_prod.vl_desconto    =  fats_repres_clientes_prod.vl_desconto_dev    + fats_repres_clientes_prod_data.vl_desconto_dev
           fats_repres_clientes_prod.qt_dev         =  fats_repres_clientes_prod.qt_dev             + fats_repres_clientes_prod_data.qt_dev
           .

    RELEASE fats_repres_clientes_prod.

END PROCEDURE.




PROCEDURE sincrFatsRepresClientes:

    /*************** por Estab, repres, cliente, item , mes e ano****************************************/
    FIND LAST fats_repres_clientes 
         WHERE fats_repres_clientes.cod_estabel        = fats_repres_clientes_prod_data.cod_estabel
         AND   fats_repres_clientes.mes                = MONTH(fats_repres_clientes_prod_data.data)
         AND   fats_repres_clientes.ano                = YEAR(fats_repres_clientes_prod_data.data)
         AND   fats_repres_clientes.cod_rep            = fats_repres_clientes_prod_data.cod_rep
         AND   fats_repres_clientes.cod_emitente       = fats_repres_clientes_prod_data.cod_emitente
         AND   fats_repres_clientes.transacao_id       = idTransacao
         NO-ERROR.
    IF NOT AVAIL fats_repres_clientes THEN DO:
       CREATE fats_repres_clientes.
       ASSIGN fats_repres_clientes.fat_repres_cliente_id      = NEXT-VALUE(seq_fats_repres_cliente)
       fats_repres_clientes.transacao_id                      = idTransacao
       fats_repres_clientes.cod_estabel                       = fats_repres_clientes_prod_data.cod_estabel
       fats_repres_clientes.mes                               = month(fats_repres_clientes_prod_data.data)
       fats_repres_clientes.ano                               = year(fats_repres_clientes_prod_data.data)
       fats_repres_clientes.dt_ini_mes                        = getPrimeiroDiaMes(fats_repres_clientes.ano,fats_repres_clientes.mes)
       fats_repres_clientes.dt_fim_mes                        = getUltimoDiaMes(fats_repres_clientes.ano,fats_repres_clientes.mes)
       fats_repres_clientes.cod_rep                           = fats_repres_clientes_prod_data.cod_rep
       fats_repres_clientes.cod_emitente                      = fats_repres_clientes_prod_data.cod_emitente   .

    END.  


    ASSIGN fats_repres_clientes.vl_faturamento =  fats_repres_clientes.vl_faturamento       + fats_repres_clientes_prod_data.vl_faturamento
           fats_repres_clientes.vl_desconto    =  fats_repres_clientes.vl_desconto          + fats_repres_clientes_prod_data.vl_desconto
           fats_repres_clientes.qt_fat         =  fats_repres_clientes.qt_fat               + fats_repres_clientes_prod_data.qt_fat 
           fats_repres_clientes.vl_devolucoes  =  fats_repres_clientes.vl_devolucoes        + fats_repres_clientes_prod_data.vl_devolucoes
           fats_repres_clientes.vl_desconto    =  fats_repres_clientes.vl_desconto_dev      + fats_repres_clientes_prod_data.vl_desconto_dev
           fats_repres_clientes.qt_dev         =  fats_repres_clientes.qt_dev               + fats_repres_clientes_prod_data.qt_dev
           .

    RELEASE fats_repres_clientes.


END PROCEDURE.



PROCEDURE sincrFatsRepres:
    

    /*************** por Estab, repres, cliente, item , mes e ano****************************************/
    FIND LAST fats_repres 
         WHERE fats_repres.cod_estabel        = fats_repres_clientes_prod_data.cod_estabel
         AND   fats_repres.mes                = MONTH(fats_repres_clientes_prod_data.data)
         AND   fats_repres.ano                = YEAR(fats_repres_clientes_prod_data.data)
         AND   fats_repres.cod_rep            = fats_repres_clientes_prod_data.cod_rep 
         AND   fats_repres.transacao_id       = idTransacao NO-ERROR.
    IF NOT AVAIL fats_repres THEN DO:
       CREATE fats_repres.
       ASSIGN 
       fats_repres.fat_repres_id              = NEXT-VALUE(seq_fats_repres)
       fats_repres.transacao_id               = idTransacao
       fats_repres.cod_estabel                = fats_repres_clientes_prod_data.cod_estabel
       fats_repres.mes                        = month(fats_repres_clientes_prod_data.data)
       fats_repres.ano                        = year(fats_repres_clientes_prod_data.data)
       fats_repres.dt_ini_mes                 = getPrimeiroDiaMes(fats_repres.ano,fats_repres.mes)
       fats_repres.dt_fim_mes                 = getUltimoDiaMes(fats_repres.ano,fats_repres.mes)
       fats_repres.cod_rep                    = fats_repres_clientes_prod_data.cod_rep .

    END.  


    ASSIGN    fats_repres.vl_faturamento =  fats_repres.vl_faturamento    + fats_repres_clientes_prod_data.vl_faturamento
              fats_repres.vl_desconto    =  fats_repres.vl_desconto       + fats_repres_clientes_prod_data.vl_desconto
              fats_repres.qt_fat         =  fats_repres.qt_fat            + fats_repres_clientes_prod_data.qt_fat 
              fats_repres.vl_devolucoes  =  fats_repres.vl_devolucoes     + fats_repres_clientes_prod_data.vl_devolucoes
              fats_repres.vl_desconto    =  fats_repres.vl_desconto_dev   + fats_repres_clientes_prod_data.vl_desconto_dev
              fats_repres.qt_dev         =  fats_repres.qt_dev            + fats_repres_clientes_prod_data.qt_dev.
    RELEASE fats_Repres.


END PROCEDURE.


PROCEDURE sincrFatsRepresMeta:
    

    /*************** por Estab, repres, cliente, item , mes e ano****************************************/
    FIND LAST fats_repres 
         WHERE fats_repres.cod_estabel        = tt-fatur.cod_Estabel
         AND   fats_repres.mes                = MONTH(tt-fatur.dt_emis)
         AND   fats_repres.ano                = YEAR(tt-fatur.dt_emis)
         AND   fats_repres.cod_rep            = tt-fatur.cod_rep
         AND   fats_repres.transacao_id       = idTransacao NO-ERROR.
    IF NOT AVAIL fats_repres THEN DO:
       CREATE fats_repres.
       ASSIGN fats_repres.fat_repres_id                         = NEXT-VALUE(seq_fats_repres)
       fats_repres.transacao_id                                 = idTransacao
       fats_repres.cod_estabel                                  = tt-fatur.cod_Estabel
       fats_repres.mes                                          = month(tt-fatur.dt_emis)
       fats_repres.ano                                          = year(tt-fatur.dt_emis)
       fats_repres.dt_ini_mes                                   = getPrimeiroDiaMes(fats_repres.ano,fats_repres.mes)
       fats_repres.dt_fim_mes                                   = getUltimoDiaMes(fats_repres.ano,fats_repres.mes)
       fats_repres.cod_rep                                      = tt-fatur.cod_rep .

    END. 
    ELSE DO:
        ASSIGN   fats_repres.vl_meta  = tt-fatur.vl_tot_it_nf .
        RELEASE fats_repres.
    END.
    



   

END PROCEDURE.

PROCEDURE sincrFat05:
  
  FOR EACH fats_04 NO-LOCK.
      FIND LAST fats_05 
          WHERE fats_05.cod_estabel     = fats_04.cod_estabel
          AND   fats_05.data            = fats_04.data
          AND   fats_05.nr_container    = fats_04.num_origem
          AND   fats_05.it_codigo       = fats_04.it_codigo
          AND   fats_05.cod_refer       = fats_04.cod_refer
          AND   fats_05.transacao_id    = idTransacao
          NO-ERROR.
      IF NOT AVAIL fats_05 THEN DO:

         CREATE fats_05.
         ASSIGN fats_05.fat_05_id       = NEXT-VALUE(seq_fat_05)
                fats_05.cod_estabel     = fats_04.cod_estabel   
                fats_05.data            = fats_04.data          
                fats_05.nr_container    = fats_04.num_origem    
                fats_05.it_codigo       = fats_04.it_codigo     
                fats_05.cod_refer       = fats_04.cod_refer     
                fats_05.transacao_id    = idTransacao   
               .
      END.

      ASSIGN fats_05.qt_vendida         = fats_05.qt_vendida     + fats_04.qt_total
             fats_05.qt_Devolvida       = fats_05.qt_Devolvida   + fats_04.qt_dev_total
             fats_05.qt_vendida_liq     = fats_05.qt_vendida     - fats_05.qt_Devolvida
             .
  END.        

END PROCEDURE.



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
    


    FIND LAST fats_04
    WHERE fats_04.tb_preco_id  = pTbPrecoId
    AND   fats_04.LOG_outlet   = pLogRubix
    AND   fats_04.num_origem   = pNrcontainer
    AND   fats_04.it_codigo    = pItCodigo
    AND   fats_04.cod_refer    = pCodRefer
    AND   fats_04.cod_estabel  = pCodEstabel
    AND   fats_04.data         = pDtEmis 
    AND   fats_04.TRANS_id     = idTransacao 
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
             fats_04.trans_id     = idTransacao . 
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


    RELEASE fats_04.


END PROCEDURE.

/*
PROCEDURE atuDatasPendentes:

    DEFINE VARIABLE h AS HANDLE      NO-UNDO.
    RUN esbo/bofats99.p PERSIST SET h.
    RUN iniciar IN h.
    RUN getDatasPendentes IN h('faturamento,devolucao,meta', OUTPUT table ttDatas) .
    IF NOT valid-handle(hAcomp) AND pedidoExecucaoId = 0 THEN DO:
       RUN utp/ut-acomp.p PERSIST SET hAComp.
       RUN pi-inicializar IN hAcomp('Atualizando Datas Pendentes').
    END.

    FOR EACH ttDatas:

        EMPTY TEMP-TABLE tt-fatur.
        EMPTY TEMP-TABLE ttTransacoes.
        EMPTY TEMP-TABLE ttAnoMes.
        EMPTY temp-table ttContainerPend.

        RUN acomp('acompanhar','Data:' + string(ttDatas.data) ).
        RUN setId IN h(ttDatas.fat_99_id).
        RUN setSituacao IN h(1, //situacao
                             0 //transacao
                             ).

        //procedures internas desta BO
        RUN setInterValDatas(ttDatas.data,ttDatas.data).
        RUN exec.
        
        //fim
        RUN setSituacao IN h(2, //situacao
                             idTransacao //transacao
                             ).

    END.
    RUN finalizar IN h.
    RUN acomp('finalizar','').


END PROCEDURE.


PROCEDURE acomp:

    DEFINE INPUT  PARAMETER pAcao  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTexto AS CHARACTER   NO-UNDO.

    IF VALID-HANDLE(hAcomp) THEN DO:

       CASE pAcao:

           WHEN 'acompanhar' THEN
               RUN pi-acompanhar IN hAcomp(pTexto).
           WHEN 'finalizar' THEN
               RUN pi-finalizar IN hAcomp.

       END CASE.

    END.
       


END PROCEDURE.*/



PROCEDURE getHBoMsg:


    DEFINE OUTPUT PARAMETER pH AS HANDLE      NO-UNDO.
    ASSIGN pH = hBoMsg.


END PROCEDURE.


PROCEDURE getIdTransacao:

    DEFINE OUTPUT  PARAMETER pId AS INT64     NO-UNDO.
    ASSIGN pId = idTransacao.
END PROCEDURE.
