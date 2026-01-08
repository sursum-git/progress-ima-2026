/*
    FOR EACH ordem-benefic WHERE ordem-benefic.nr-ob >= 80000.
    IF ordem-benefic.ind-ob = 0 THEN
       ASSIGN ordem-benefic.ind-ob = 1.
END.
*/

FOR EACH ordem-benefic WHERE ordem-benefic.nr-ob >= 80000 NO-LOCK.
    IF ordem-benefic.ind-ob = 0 THEN
    DISP ordem-benefic.nr-ob
         ordem-benefic.ind-ob.
END.

