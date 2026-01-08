DEF VAR c-rep-ori LIKE repres.nome-abrev.
DEF VAR c-rep-des LIKE repres.nome-abrev.

FIND repres WHERE repres.cod-rep = 116 NO-LOCK.
ASSIGN c-rep-ori = repres.nome-abrev.
FIND repres WHERE repres.cod-rep = 167 NO-LOCK.
ASSIGN c-rep-des = repres.nome-abrev.

FOR EACH ped-venda WHERE (ped-venda.cod-sit-ped < 3 OR 
                          ped-venda.cod-sit-ped = 5) AND
                          ped-venda.no-ab-reppri = c-rep-ori
                   EXCLUSIVE-LOCK.
    /*ASSIGN ped-venda.no-ab-reppri = c-rep-des.*/
       DISP ped-venda.nome-abrev
            ped-venda.nr-pedcli
            ped-venda.no-ab-reppri.
END.
