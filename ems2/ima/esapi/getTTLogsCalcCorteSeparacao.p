/**************************************************************************
Programa: esapi/getTTLogsCalcCorteSeparacao.p
Autor: Tadeu Silva Parreiras
Objetivo: retornar a tabela temporaria dos logs de calculo do corte na
separa‡Æo conforme numero do pedido passado por parametro.
Data: 10/2025                                             
*****************************************************************************/
{esapi/getTTLogsCalcCorteSeparacao.i}
{esapi/getDescNumTipoLogCalculo.i}
DEFINE INPUT  PARAMETER pNrPedido AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttLogCalc.

FOR EACH transacoes FIELDS(chave calculo_id) NO-LOCK
    WHERE transacoes.chave = STRING(pNrPedido)
    AND   transacoes.calculo_id = 200 ,
    EACH logs_calculo OF transacoes NO-LOCK:
    CREATE ttLogCalc.
    BUFFER-COPY logs_calc TO ttlogCalc.
    ASSIGN ttlogCalc.descNumTipo = getDescNumTipo(ttLogCalc.num_tipo).
END.






