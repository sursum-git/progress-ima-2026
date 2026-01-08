
LOG-MANAGER:LOGGING-LEVEL = 5.
LOG-MANAGER:LOGFILE-NAME = 'C:\temp\canc_pedido_' + STRING(TIME) + '.txt'.



{lisa\varpropsComuns.i}
{esapi/envia-cancelamento-pedido.i}

DEFINE INPUT PARAMETER pRowidPedVenda   AS ROWID.
DEFINE OUTPUT PARAMETER cErros          AS CHARACTER   NO-UNDO.

FIND ped-venda NO-LOCK
    WHERE ROWID(ped-venda) = pRowidPedVenda
    NO-ERROR.
IF NOT AVAIL ped-venda THEN DO:
   ASSIGN cErros = "Pedido de venda n∆o encontrado".
   RETURN 'nok'.
END.

FIND ped-venda-ext WHERE
     ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
     ped-venda-ext.nr-pedido = ped-venda.nr-pedido
     NO-LOCK NO-ERROR.

IF NOT AVAIL ped-venda-ext THEN DO:
   ASSIGN cErros = "Extens∆o do Pedido de Venda n∆o encontrada ".
   RETURN 'nok'.
END.
IF ped-venda-ext.nr-pedext = '' THEN DO:
   FIND LAST lisa-integra NO-LOCK
       WHERE lisa-integra.cod-trans = 'isf'
       AND   lisa-integra.chave = ped-venda.nr-pedcli + "|"
                                + ped-venda.nome-abrev
       AND lisa-integra.val-livre-1 <> '' NO-ERROR
       .
   IF NOT AVAIL lisa-integra THEN DO:
      ASSIGN cErros = "PrÇ-Pedido n∆o encontrado".
      RETURN 'nok'.
   END.
END.

CREATE ttPedido.
ASSIGN ttPedido.prePedido      = ped-venda-ext.nr-pedext
       ttPedido.codigoCliente  = cChave
       ttPedido.pedidoCliente  = string(ped-venda.nr-pedido).
                               


RUN lisa/enviarCancelamentoPedVenda.p(TABLE ttPedido,
                                     INPUT ttPedido.pedidoCliente,
                                     OUTPUT cErros).
IF cErros <> '' THEN DO:
   RETURN 'ADM-ERROR'.
END.

RETURN 'ADM-OK'.
