DEFINE INPUT  PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEFINE OUTPUT PARAMETER p-ok AS LOG INIT YES.

DEFINE VARIABLE i-tp-frete AS INT.

FIND ped-venda WHERE
     ped-venda.nr-pedido = int(p-nr-pedcli) SHARE-LOCK NO-ERROR.

FIND ped-venda-ext WHERE
     ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
     ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

/* Inicia como se o Frete estivesse OK... */
ASSIGN ped-venda.cod-sit-com = 2   /* Aprovado */
       p-ok = YES.

IF NOT ped-venda-ext.tp-frete MATCHES "*CIF*" THEN RETURN.

RUN esapi/calcula-tipo-frete.p (INPUT ped-venda.nr-pedcli,
                                OUTPUT i-tp-frete).

IF i-tp-frete = 3 THEN DO.  // Fob  
   ASSIGN ped-venda.cod-sit-com = 1  // Reprovado
          p-ok = NO.
   RETURN.
END.


