DEFINE INPUT  PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEFINE OUTPUT PARAMETER p-fob AS LOG INIT YES.

DEFINE VARIABLE i-tp-frete AS INT.

FIND ped-venda WHERE
     ped-venda.nr-pedido = INTEGER(p-nr-pedcli) SHARE-LOCK NO-ERROR.

FIND ped-venda-ext WHERE
     ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
     ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

IF NOT ped-venda-ext.tp-frete MATCHES "*FOB*" THEN RETURN.

RUN esapi/calcula-tipo-frete.p (INPUT ped-venda.nr-pedcli,
                                OUTPUT i-tp-frete).

ASSIGN p-fob = YES.
IF i-tp-frete <> 3 THEN DO.  // NÆo ‚ Fob 
   ASSIGN p-fob = NO.
   RETURN.
END.

