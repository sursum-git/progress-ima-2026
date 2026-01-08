DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR de-qtd AS DEC.
DEF VAR de-perc-pron AS DEC.

FIND FIRST ob-param NO-LOCK NO-ERROR.

FOR EACH ordem-benefic WHERE
         ordem-benefic.cod-estab = '1' AND
         ordem-benefic.dt-ob >= 01.04.2011 AND
         ordem-benefic.num-progr = 0 AND
         ordem-benefic.tipo-ordem = 1 EXCLUSIVE-LOCK.

    IF LOCKED ordem-benefic THEN NEXT.

    FOR EACH ob-pcp WHERE
             ob-pcp.it-codigo = ordem-benefic.it-codigo AND
             ob-pcp.situacao = 1 NO-LOCK
             BY ob-pcp.num-progr.

        FIND ob-pcp-ref OF ob-pcp WHERE
             ob-pcp-ref.cod-refer = ordem-benefic.cod-refer AND 
             ob-pcp-ref.situacao = 1 NO-ERROR.

        IF AVAIL ob-pcp-ref THEN LEAVE.
    END.

    IF NOT AVAIL ob-pcp-ref THEN NEXT.

    ASSIGN ordem-benefic.num-progr = ob-pcp.num-progr.
    
    IF ob-pcp-ref.qtd-pron = 0 THEN NEXT.

    FIND ob-pcp OF ob-pcp-ref NO-ERROR.

    ASSIGN de-qtd = 0.
    FOR EACH ob-etiqueta OF ordem-benefic WHERE
             ob-etiqueta.situacao >= 3 NO-LOCK.
        ASSIGN de-qtd = de-qtd + ob-etiqueta.quantidade.
    END.

    IF de-qtd = 0 THEN NEXT.

    ASSIGN de-perc-pron = de-qtd / ob-pcp-ref.qtd-pron * 100.

    ASSIGN ob-pcp-ref.char-1 = STRING(ob-pcp-ref.qtd-pron,"->>>,>>9.99") + "   " +
                               STRING(de-qtd,"->>>,>>9.99")
           ob-pcp-ref.qtd-pron = ob-pcp-ref.qtd-pron - de-qtd.

    IF de-perc-pron >= ob-param.perc-bx-pronto OR 
       ob-pcp-ref.qtd-pron <= 0 THEN DO.
       ASSIGN ob-pcp-ref.char-1 = ob-pcp-ref.char-1 + "   Zerou   " + STRING(de-perc-pron,">>9.99") + "  " +
                                  STRING(ob-pcp-ref.qtd-pron,"->>>,>>9.99") 
              ob-pcp-ref.qtd-pron = 0.

       IF ob-pcp-ref.qtd-pron = 0 AND
          ob-pcp-ref.qtd-proc = 0 AND
          ob-pcp-ref.qtd-sld-prog = 0 THEN
          ASSIGN ob-pcp-ref.situacao = 2.

       IF NOT CAN-FIND(FIRST ob-pcp-ref OF ob-pcp WHERE
                             ob-pcp-ref.situacao = 1) THEN
          ASSIGN ob-pcp.situacao = 2.
    END.
    ASSIGN ob-pcp-ref.usr-ult-pron = c-seg-usuario
           ob-pcp-ref.dt-ult-pron  = TODAY.

END.
