/******************************************************************************
Programa: esapi/getIDsTransPorDtFats.p
objetivo: Retornar temp-table com os ids de transa‡Æo referentes a 
tabela fats_repres_clientes_prod_data
autor: Tadeu Silva
data: 06/2024
*****************************************************************************/

{esapi/getIdsTransPorDtFats.i ttTransacoes}

DEFINE INPUT  PARAMETER pDtIni AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER pDtFim AS DATE        NO-UNDO.

DEFINE OUTPUT PARAMETER TABLE FOR ttTransacoes.


FOR EACH fats_repres_clientes_prod_data 
    WHERE fats_repres_clientes_prod_data.data >= pDtIni
    AND   fats_repres_clientes_prod_data.data <= pDtFim NO-LOCK,
    EACH transacoes
    WHERE transacoes.transacao_id = fats_repres_clientes_prod_data.transacao_id 
    AND   transacoes.ind_sit_transacao = 1. // concluida

    FIND ttTransacoes
        WHERE ttTransacoes.id = transacoes.transacao_id
        NO-ERROR.
    IF NOT AVAIL ttTransacoes THEN DO:
       CREATE ttTransacoes.
       ASSIGN ttTransacoes.id   = transacoes.transacao_id
              ttTransacoes.data = fats_repres_clientes_prod_data.data
              .
    END.
END.




