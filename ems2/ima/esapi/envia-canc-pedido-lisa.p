
//LOG-MANAGER:LOGGING-LEVEL = 5.
//LOG-MANAGER:LOGFILE-NAME = 'C:\temp\nota' + STRING(TIME) + '.txt'.
{lisa\varpropsComuns.i}

DEFINE VARIABLE prePedido       AS CHARACTER   NO-UNDO.
/*DEFINE TEMP-TABLE ttPedido
    FIELD codigoCliente AS CHAR
    FIELD pedidoCliente AS CHAR
    FIELD prePedido     AS CHAR
    FIELD observacoes   AS CHAR.*/

{esapi/envia-cancelamento-pedido-lisa.i}

DEFINE INPUT PARAMETER pRowidPedVenda   AS ROWID.
DEFINE OUTPUT PARAMETER cErros          AS CHARACTER   NO-UNDO.

FIND ped-venda NO-LOCK
    WHERE ROWID(ped-venda) = pRowidPedVenda
    NO-ERROR.
IF NOT AVAIL ped-venda THEN DO:
   ASSIGN cErros = "Pedido de venda n∆o encontrado".
   RETURN 'ADM-ERROR'.
END.

CREATE ttPedido.
ASSIGN ttPedido.ccodfilial      =  cFilial
       ttPedido.ccgc            = cCnpj
       ttPedido.cpedido         = string(ped-venda.nr-pedido)
       ttPedido.cstatus         = 'C'
      .



RUN lisa/enviarCancelamentoPedVendaLisa.p(INPUT TABLE ttPedido,
                                 INPUT ped-venda.nr-pedcli,
                                 OUTPUT cErros).
IF cErros <> '' THEN DO:
   RETURN 'ADM-ERROR'.
END.

RETURN 'ADM-OK'.
