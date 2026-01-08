
/****************************************************************************
programa:esapi/getItemPedidoLisaPendLanctoErp.p
objetivo: a partir dos Json's de retorno do endpoint 'notaretorno' verificar
quais notas fiscais ainda n∆o foram lanáadas no ERP.
autor: Tadeu Silva
data: 01/2024
***************************************************************************/
{esapi/ttRemessaTerc.i}
DEFINE  INPUT-OUTPUT PARAMETER TABLE FOR ttRetorno .
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.

FIND LAST ttRetorno NO-ERROR.
IF AVAIL ttRetorno THEN DO:
   ASSIGN iCont = ttRetorno.id .
END.



{esapi/getNfsRetornoLisaPendLanctoErp.i}
RUN esapi/getNfsRetornoLisaPendLanctoErp.p(OUTPUT TABLE ttNfPend).
FOR EACH ttNfPend:
    FOR EACH itens_pedido_lisa
        WHERE int(itens_pedido_lisa.nf_retorno)      =  int(ttNfPend.nrNotaFis)   
        AND   itens_pedido_lisa.serie_nf_retorno     =  ttNfPend.serie
        .
        FIND pedidos_lisa NO-LOCK
            WHERE pedidos_lisa.pedido_lisa_id = itens_pedido_lisa.pedido_lisa_id
            NO-ERROR.
        IF pedidos_lisa.cod_sit_pedido_lisa <> 'finalizado' OR 
           pedidos_lisa.log_nfe_enviada = FALSE THEN NEXT.

        CREATE ttRetorno .
        ASSIGN iCont                = iCont + 1
               ttRetorno.id         = iCont
               ttRetorno.itCodigo   = itens_pedido_lisa.it_codigo
               ttRetorno.codRefer   = itens_pedido_lisa.cod_refer
               ttRetorno.nrNota     = itens_pedido_lisa.nf_retorno
               ttRetorno.nfBaixada  = itens_pedido_lisa.nf_origem
               ttRetorno.dtEntrada  = IF AVAIL pedidos_lisa THEN pedidos_lisa.dt_expedicao ELSE ?
               ttRetorno.quantidade = itens_pedido_lisa.qt_faturada
               ttRetorno.valorUnit  = 0
               ttRetorno.valorTotal = 0
               ttRetorno.nfErp      = ''
               ttRetorno.logLancErp = NO
               ttRetorno.nrPedido   = STRING(pedidos_lisa.nr_pedido)
               .


    END.
END.







