
/****************************************************************************
programa:esapi/getItemPedidoLisa.p
objetivo: a partir dos Json's de retorno do endpoint 'notaretorno' retornar
todas as notas de retorno que foram recebidos com os dados de item, ref e quant.
- retornar tamb‚m a informa‡Æo 
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





FOR EACH pedidos_lisa NO-LOCK
    WHERE pedidos_lisa.cod_sit_pedido_lisa = 'finalizado' .
    FIND retornos_lisa NO-LOCK
        WHERE int(retornos_lisa.nr_pedido) = pedidos_lisa.nr_pedido
        NO-ERROR .

    IF NOT AVAIL retornos_lisa THEN

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








