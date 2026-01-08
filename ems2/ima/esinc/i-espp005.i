PUT "Pedido    Cliente       Situaá∆o                          Moeda      " SKIP
    "--------  ------------  --------------------------------  ---------- " SKIP. 

FIND FIRST para-ped NO-LOCK NO-ERROR.
    
FOR EACH ped-venda-ext WHERE
         ped-venda-ext.cod-estabel = pp-container.cod-estabel AND
         ped-venda-ext.nr-container = pp-container.nr-container NO-LOCK,
    EACH ped-venda WHERE 
         ped-venda.nr-pedido = ped-venda-ext.nr-pedido AND
         ped-venda.tp-pedido = 'PI' SHARE-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT "Fechando Container/Pedido: " + ped-venda.nr-pedcli).

    RUN pi-prazo-medio.

    FIND moeda OF ped-venda NO-LOCK NO-ERROR.

    PUT UNFORMAT ped-venda.nr-pedcli  AT 1  FORMAT "x(6)"
                 ped-venda.nome-abrev AT 11 FORMAT "x(18)"
                 moeda.descricao      AT 59 FORMAT "x(10)".

    ASSIGN ped-venda.tp-pedido = 'PE'. /* Altera para PE para validar o Precáo */
    RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                   INPUT ped-venda.nome-abrev,
                                   INPUT "Alterado Pedido para de PI para PE",
                                   INPUT NO).

    // Recalcula o ID do Preáo para PE
    FOR EACH ped-item OF ped-venda NO-LOCK.
        FIND ped-item-ext WHERE
             ped-item-ext.cod-estabel  = ped-venda.cod-estabel AND
             ped-item-ext.nome-abrev   = ped-item.nome-abrev AND
             ped-item-ext.nr-pedcli    = ped-item.nr-pedcli AND
             ped-item-ext.nr-sequencia = ped-item.nr-sequencia 
             SHARE-LOCK NO-ERROR.


        IF ped-item-ext.cod_controle_preco = 0 THEN DO.
           // Busca o ID do Preáo
           ASSIGN de-vlReal = 0        
                  de-vlDolar = 0.
    
           RUN pi-busca-preco (INPUT  ped-item.it-codigo,
                               INPUT  ped-item.cod-refer,
                               INPUT  "", // Campanha
                               OUTPUT de-vlReal,  
                               OUTPUT de-vlDolar,
                               OUTPUT i-ControlePreco).
    
           ASSIGN ped-item-ext.cod_controle_preco = i-ControlePreco.
        END.

        // Pedidos PI n∆o tem OUTLET
        ASSIGN ped-item-ext.liquida-ima = NO
               ped-item-ext.num-id-liquida-ima = ''.
    END.

    IF ped-venda.cod-sit-ped <> 1 AND
       ped-venda.cod-sit-ped <> 4 THEN NEXT. 

    RUN esapi/completa-pedvenda.p (INPUT ped-venda.nr-pedido).
    IF NOT ped-venda.completo THEN DO.
       PUT UNFORMAT "ERRO ao Completar o Pedido" AT 30 
           SKIP.
       NEXT.
    END.

    ASSIGN ped-venda.completo = YES.
    RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                   INPUT ped-venda.nome-abrev,
                                   INPUT "Pedido Completo pelo Fechamento do Container",
                                   INPUT NO).

    PUT UNFORMAT "OK - Pedido COMPLETO" AT 30 
        SKIP.

    // Valida Frete CIF
    ASSIGN ped-venda.cod-sit-com = 2.
    RUN esapi/valida-frete-cif.p (INPUT ped-venda.nr-pedcli,
                                  OUTPUT l-ok).
    IF l-ok = NO THEN DO.
       RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                      INPUT ped-venda.nome-abrev,
                                      INPUT "Valor do Pedido Inv†lido para Frete CIF, Pedido Requer Aprovaá∆o",
                                      INPUT NO).
    END.

    // Validar Desconto
    IF ped-venda.des-pct-desconto-inform <> "" AND 
       ped-venda.ind-sit-desconto <> 2 THEN DO. /* Tem Desconto e n∆o foi A*/
       ASSIGN ped-venda.ind-sit-desconto = 1.
       RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                      INPUT ped-venda.nome-abrev,
                                      INPUT "Desconto Informado, Pedido Requer Aprovaá∆o",
                                      INPUT NO).
    END.
END.

PUT " " SKIP(2).
PUT UNFORMAT "FECHAMENTO DO CONTAINER FINALIZADO" AT 30
    SKIP.

