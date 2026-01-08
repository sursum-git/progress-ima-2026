FOR EACH ped-venda WHERE
         LOOKUP(STRING(ped-venda.cod-sit-ped),"1,2") > 0. 

    FIND ped-venda-ext WHERE
         ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-venda-ext THEN NEXT.

    IF ped-venda-ext.tp-pedido = "· Vista" OR
       ped-venda-ext.tp-pedido = "Exporta‡Æo" OR 
       ped-venda-ext.tp-pedido = "Amostra" OR
       ped-venda-ext.tp-pedido = "Amostra Exporta‡Æo" OR
       ped-venda-ext.tp-pedido = "Bonifica‡Æo" OR
       ped-venda-ext.tp-pedido = "Doa‡Æo" 
       THEN NEXT.

    FIND emitente WHERE
         emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

    IF emitente.ind-cre-cli = 2 THEN NEXT.  /* Cr‚dito Autom tico */

    ASSIGN ped-venda.cod-sit-ped = 4.

    ASSIGN ped-venda.desc-bloq-cr = "Retorno para Implanta‡Æo do Crivo Financeiro"
           ped-venda.dt-apr-cred = TODAY
           ped-venda.cod-sit-aval = 4 /* simula uma suspensÆo */
           ped-venda.quem-aprovou = "super".
END.
