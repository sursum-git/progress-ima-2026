FOR EACH repres WHERE repres.nome-abrev BEGINS "franco" NO-LOCK.
    DISP repres.cod-rep
         repres.nome-abrev
         repres.e-mail.
END.
/*
FIND repres WHERE repres.cod-rep = 11.
UPDATE repres.e-mail FORMAT "x(70)".
*/
