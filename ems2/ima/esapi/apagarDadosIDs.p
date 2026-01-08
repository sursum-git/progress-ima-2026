/******************************************************************************
Programa: esapi/apagarDadosIDs.p
objetivo: Apaga os dados das tabelas de analise de faturamento 
autor: Tadeu Silva
data: 06/2024
*****************************************************************************/
{esbo/boAtuFats.i}
{esapi/getIDsTransPorDtFats.i ttTransacoes}
{esp/util.i}
DEFINE INPUT  PARAMETER pTabela AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR ttTransacoes.
DEFINE INPUT  PARAMETER pDtIni AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER pDtFim AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER TABLE FOR ttAnoMes.
DEFINE INPUT  PARAMETER TABLE FOR ttContainerPend .


//verifica se foi passado por parametro  dados pela temp-table e preenche caso nÆo tenha sido preenchida
IF NOT CAN-FIND( FIRST ttAnoMes) THEN DO:
    RUN _calcTtAnoMes(pDtIni,pDtFim).
END.
 



/***********************************************************************************************
  primeiro seta os registros como deletando para que nÆo sejam mais considerados
  registros destas transa‡äes
***********************************************************************************************/
IF pTabela = 'todas' THEN
  FOR EACH ttTransacoes:
      FIND transacoes EXCLUSIVE-LOCK
          WHERE transacoes.transacao_id = ttTransacoes.id
          NO-ERROR.
      IF AVAIL transacoes THEN DO:
          ASSIGN transacoes.ind_sit_transacao = 4.
          FIND CURRENT transacoes NO-LOCK.
      END.
  END.
{esp/exportarTabelaCsv2.i TTtransacoes}
//apaga efetivamente os dados gerados que tenham estas transa‡äes como origem, respeitando a data inicial e final                           
FOR EACH ttTransacoes:
    FIND transacoes NO-LOCK
        WHERE transacoes.transacao_id = ttTransacoes.id
        NO-ERROR.

    FOR EACH fats_04 EXCLUSIVE-LOCK
        WHERE fats_04.TRANS_id  =  transacoes.transacao_id 
        AND fats_04.data       >= pDtIni
        AND fats_04.data       <= pDtFim :
        DELETE fats_04.
    END.
    //apaga os registros atualizados
    FOR EACH ttContainerPend:
        FOR EACH fats_05 EXCLUSIVE-LOCK
            WHERE fats_05.nr_container = ttContainerPend.nrContainer
            AND   fats_05.it_codigo    = ttContainerPend.itCodigo
            AND   fats_05.cod_refer    = ttContainerPend.codRefer
            AND   fats_05.TRANSacao_id = transacoes.transacao_id :
            DELETE fats_05.
        END.
    END.

    
    FOR EACH ttAnoMes:
        RUN apagarFatsRepres(transacoes.transacao_id ,ttAnoMes.ano,ttAnoMes.mes).
        RUN apagarFatsRepresClientes(transacoes.transacao_id ,ttAnoMes.ano,ttAnoMes.mes).
        RUN apagarFatsRepresClientesProd(transacoes.transacao_id ,ttAnoMes.ano,ttAnoMes.mes).
        
    END.
    RUN apagarFatsRepresClientesProdData(transacoes.transacao_id,pDtIni,pDtFim).

    /**************************************************************************************************
      Seta os registros como CONCLUIDO para que sejam considerados novamente os que nÆo foram excluidos
     **************************************************************************************************/
    IF pTabela = 'todas' THEN  DO:
       FIND CURRENT transacoes EXCLUSIVE-LOCK.
       IF AVAIL transacoes THEN DO:
          ASSIGN transacoes.ind_sit_transacao = 1.
          RELEASE transacoes.
       END.
    END.
END.

PROCEDURE apagarFatsRepresClientesProdData:

    DEFINE INPUT  PARAMETER pTrans   AS INT64       NO-UNDO.
    DEFINE INPUT  PARAMETER pDataIni AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pDataFim AS DATE        NO-UNDO.

    IF pTabela = 'fats_repres_clientes_prod_data' OR  pTabela =  'todas' THEN
       FOR EACH fats_repres_clientes_prod_data EXCLUSIVE-LOCK
           WHERE fats_repres_clientes_prod_data.transacao_id = pTrans
           AND   fats_repres_clientes_prod_data.data >= pDataIni
           AND   fats_repres_clientes_prod_data.data <= pDataFim:
           DELETE fats_repres_clientes_prod_data.
        END.                                             

END PROCEDURE.

PROCEDURE apagarFatsRepresClientesProd:

    DEFINE INPUT  PARAMETER pTrans AS INT64     NO-UNDO.
    DEFINE INPUT  PARAMETER pAno   AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pMes   AS INTEGER     NO-UNDO.


    IF pTabela = 'fats_repres_clientes_prod' OR  pTabela =  'todas' THEN
       FOR EACH fats_repres_clientes_prod EXCLUSIVE-LOCK
            WHERE fats_repres_clientes_prod.transacao_id = pTrans
            AND   fats_repres_clientes_prod.ano          = pAno
            AND   fats_repres_clientes_prod.mes          = pMes:
            DELETE fats_repres_clientes_prod.
        END.
                                                         
END PROCEDURE.


PROCEDURE apagarFatsRepresClientes:

    DEFINE INPUT  PARAMETER pTrans AS INT64     NO-UNDO.
    DEFINE INPUT  PARAMETER pAno   AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pMes   AS INTEGER     NO-UNDO.


    IF pTabela = 'fats_repres_clientes' OR  pTabela =  'todas' THEN
       FOR EACH fats_repres_clientes EXCLUSIVE-LOCK
           WHERE fats_repres_clientes.transacao_id = pTrans
           AND fats_repres_clientes.ano            = pAno
           AND fats_repres_clientes.mes            = pMes:
           DELETE fats_repres_clientes.
       END.                                              

END PROCEDURE.


PROCEDURE apagarFatsRepres:

    DEFINE INPUT  PARAMETER pTrans AS INT64     NO-UNDO.
    DEFINE INPUT  PARAMETER pAno   AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pMes   AS INTEGER     NO-UNDO.


    IF pTabela = 'fats_repres' OR  pTabela =  'todas' THEN
       FOR EACH fats_repres EXCLUSIVE-LOCK
            WHERE fats_repres.transacao_id = pTrans
            AND fats_repres.ano            = pAno
            AND fats_repres.mes            = pMes :
            DELETE fats_repres.
        END.                                             

END PROCEDURE.

PROCEDURE apagarFats04:

    DEFINE INPUT  PARAMETER pTrans   AS INT64       NO-UNDO.
    DEFINE INPUT  PARAMETER pDataIni AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pDataFim AS DATE        NO-UNDO.

    IF pTabela = 'fats_04' OR  pTabela =  'todas' THEN
       FOR EACH fats_04 EXCLUSIVE-LOCK
        WHERE fats_04.TRANS_id  =  pTrans
        AND fats_04.data       >= pDataIni
        AND fats_04.data       <= pDataFim :
           DELETE fats_04.
        END.                                            

END PROCEDURE.


PROCEDURE apagarFats05:

    DEFINE INPUT  PARAMETER pTrans   AS INT64       NO-UNDO.

    IF pTabela = 'fats_05' OR  pTabela =  'todas' THEN
      FOR EACH ttContainerPend:
        FOR EACH fats_05 EXCLUSIVE-LOCK
            WHERE fats_05.nr_container = ttContainerPend.nrContainer
            AND   fats_05.it_codigo    = ttContainerPend.itCodigo
            AND   fats_05.cod_refer    = ttContainerPend.codRefer
            AND   fats_05.TRANSacao_id = pTrans :
            DELETE fats_05.
        END.
      END.
END PROCEDURE.
