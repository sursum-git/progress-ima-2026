DEF VAR a1 AS DEC.
DEF VAR a2 AS DEC.

FOR EACH ord-prod WHERE
         ord-prod.nr-ord-prod = 160582 NO-LOCK. /*
         ord-prod.dt-emissao >= 01.01.2008 NO-LOCK. */

    ASSIGN a1 = 0
           a2 = 0.
      
    FOR EACH movto-estoq WHERE
             movto-estoq.nr-ord-prod = ord-prod.nr-ord-prod NO-LOCK.
                  
        ASSIGN a1 = IF movto-estoq.tipo-trans = 1
                    THEN a1 + movto-estoq.valor-mat-m[1]
                    ELSE a1 - movto-estoq.valor-mat-m[1].

        ASSIGN a2 = IF movto-estoq.tipo-trans = 1
                    THEN a2 + movto-estoq.valor-ggf-m[1]
                    ELSE a2 - movto-estoq.valor-ggf-m[1].

        IF movto-estoq.tipo-trans = 2 THEN
        DISP movto-estoq.valor-ggf-p[1] (TOTAL).
    END.
    /*
    FOR EACH movto-ggf WHERE
             movto-ggf.nr-ord-prod = ord-prod.nr-ord-prod NO-LOCK.
        ASSIGN a2 = a2 - movto-ggf.valor-ggf-1-m[1] -
                         movto-ggf.valor-ggf-2-m[1] -
                         movto-ggf.valor-ggf-3-m[1].
    END.
    */
    IF a1 <> 0 OR a2 <> 0 THEN

       DISP ord-prod.nr-ord-prod
            a1
            a2.
END.


