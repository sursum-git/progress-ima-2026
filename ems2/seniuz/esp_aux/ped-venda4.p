DEF VAR c-nome1 LIKE transporte.nome-abrev.
DEF VAR c-nome2 LIKE transporte.nome-abrev.
DEF VAR c-nome3 LIKE transporte.nome-abrev.
DEF VAR c-nome4 LIKE transporte.nome-abrev.

FIND transporte WHERE transporte.cod-transp = 565 NO-LOCK.
ASSIGN c-nome1 = transporte.nome-abrev.
FIND transporte WHERE transporte.cod-transp = 550 NO-LOCK.
ASSIGN c-nome2 = transporte.nome-abrev.
FIND transporte WHERE transporte.cod-transp = 35  NO-LOCK.
ASSIGN c-nome3 = transporte.nome-abrev.
FIND transporte WHERE transporte.cod-transp = 480 NO-LOCK.
ASSIGN c-nome4 = transporte.nome-abrev.

OUTPUT TO c:/temp/lixo.csv.
PUT "Transp: " ";" c-nome1 ";" c-nome2 ";" c-nome3 ";" c-nome4 SKIP.
PUT "Pedido;Cliente;Transp;Redesp" SKIP.
FOR EACH ped-venda WHERE (ped-venda.cod-sit-ped < 3 OR
                          ped-venda.cod-sit-ped = 5)
                     AND (ped-venda.nome-transp = c-nome1 OR
                          ped-venda.nome-transp = c-nome2 OR
                          ped-venda.nome-transp = c-nome3 OR
                          ped-venda.nome-tr-red = c-nome4)
                   NO-LOCK.
    PUT ped-venda.nr-pedcli ";"
        ped-venda.nome-abrev ";"
        ped-venda.nome-transp ";"
        ped-venda.nome-tr-red
        SKIP.
END.
OUTPUT CLOSE.
