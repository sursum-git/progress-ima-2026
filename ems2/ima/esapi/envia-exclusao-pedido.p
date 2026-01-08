
//LOG-MANAGER:LOGGING-LEVEL = 5.
//LOG-MANAGER:LOGFILE-NAME = 'C:\temp\nota' + STRING(TIME) + '.txt'.



{lisa\varpropsComuns.i}
DEF VAR h-dataset               AS HANDLE.
DEFINE VARIABLE prePedido       AS CHARACTER   NO-UNDO.
/*DEFINE TEMP-TABLE ttPedido
    FIELD codigoCliente AS CHAR
    FIELD pedidoCliente AS CHAR
    FIELD prePedido     AS CHAR
    FIELD observacoes   AS CHAR.*/

{esapi/envia-cancelamento-pedido.i}

DEFINE INPUT PARAMETER pRowidPedVenda   AS ROWID.
DEFINE OUTPUT PARAMETER cErros          AS CHARACTER   NO-UNDO.

FIND ped-venda NO-LOCK
    WHERE ROWID(ped-venda) = pRowidPedVenda
    NO-ERROR.
IF NOT AVAIL ped-venda THEN DO:
   ASSIGN cErros = "Pedido de venda n∆o encontrado".
   RETURN 'ADM-ERROR'.
END.

FIND ped-venda-ext WHERE
     ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
     ped-venda-ext.nr-pedido = ped-venda.nr-pedido
     NO-LOCK NO-ERROR.

IF NOT AVAIL ped-venda-ext THEN DO:
   ASSIGN cErros = "Extens∆o do Pedido de Venda n∆o encontrada ".
   RETURN 'ADM-ERROR'.
END.

IF ped-venda-ext.nr-pedext = '' THEN DO:
   FIND LAST lisa-integra NO-LOCK
       WHERE lisa-integra.cod-trans = 'isf'
       AND   lisa-integra.chave = ped-venda.nr-pedcli  + "|" +
                                  ped-venda.nome-abrev
       AND   lisa-integra.val-livre-1 <> ''
       NO-ERROR.
   IF AVAIL lisa-integra THEN DO:
      ASSIGN prePedido = lisa-integra.val-livre-1.
   END.
   ELSE DO:
      ASSIGN cErros = "PrÇ-Pedido N∆o Encontrado".
      RETURN 'ADM-ERROR'.
   END.
END.
ELSE DO:
    ASSIGN prePedido = ped-venda-ext.nr-pedext .
END.

CREATE ttPedido.
ASSIGN ttPedido.prePedido      = prePedido
       ttPedido.codigoCliente  = cChave
       ttPedido.pedidoCliente  = string(ped-venda.nr-pedido).



RUN lisa/enviarExclusaoPedVenda.p(INPUT TABLE ttPedido,
                                     INPUT ttPedido.pedidoCliente,
                                     OUTPUT cErros).
IF cErros <> '' THEN DO:
   RETURN 'ADM-ERROR'.
END.

RETURN 'ADM-OK'.
