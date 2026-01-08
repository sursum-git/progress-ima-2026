DEF VAR c-situacao AS CHAR FORMAT "x(20)".

FIND ped-venda WHERE ped-venda.nome-abrev = "delle's"
                 AND ped-venda.nr-pedcli  = "64206"
               NO-LOCK.

{esinc/i-dsrb.i ped-venda.cod-sit-ped ped-venda.cod-sit-ped c-situacao} 
ASSIGN c-situacao = string(ped-venda.cod-sit-ped,'99') + '-' + c-situacao.

DISP ped-venda.nr-pedcli
     c-situacao.
