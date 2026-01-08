DEF VAR c-tipo-tear AS CHAR.
FOR EACH mov-est-acbm WHERE mov-est-acbm.tipo-tear = "".
    FIND ordem-benefic WHERE ordem-benefic.cod-estabel = mov-est-acbm.cod-estabel
                         AND ordem-benefic.nr-ob = mov-est-acbm.num-lote
                       NO-LOCK NO-ERROR.
    IF AVAIL ordem-benefic THEN DO:
       ASSIGN c-tipo-tear = ordem-benefic.tipo-tear.
       /*
       DISP ordem-benefic.cod-estabel
            ordem-benefic.nr-ob
            c-tipo-tear.
       */
    END.
    /*
    IF c-tipo-tear <> "" THEN DO.
       ASSIGN mov-est-acbm.tipo-tear = c-tipo-tear.
       ASSIGN c-tipo-tear = "".
    END.
    */
END.
