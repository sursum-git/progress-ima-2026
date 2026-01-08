DEF VAR c-mov AS CHAR.

FIND titulo WHERE titulo.ep-codigo = 1
              AND titulo.cod-estabel = "2"
              AND titulo.cod-esp = "dp"
              AND titulo.serie = "1"
              AND titulo.nr-docto = "0160970"
              AND titulo.parcela = "01"
            NO-LOCK.

DISP titulo.dt-liq.
FOR EACH mov-tit OF titulo NO-LOCK.
    {esinc/i-dsrb.i mov-tit.transacao mov-tit.transacao c-mov}
    DISP mov-tit.transacao VIEW-AS FILL-IN
         c-mov
         mov-tit.dt-credito
         mov-tit.dt-baixa
         mov-tit.dt-trans
         mov-tit.dt-movto.
END.
