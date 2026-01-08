FOR EACH ordem-benefic WHERE
         ordem-benefic.situacao <= 3 SHARE-LOCK.

    IF LOCKED ordem-benefic THEN NEXT.

    FOR FIRST ob-pcp-ref WHERE
              ob-pcp-ref.situacao = 1 AND
              ob-pcp-ref.cod-refer = ordem-benefic.cod-refer NO-LOCK,
        FIRST ob-pcp OF ob-pcp-ref WHERE
              ob-pcp.it-codigo = ordem-benefic.it-codigo.

        ASSIGN ordem-benefic.num-progr = ob-pcp-ref.num-progr.
    END.
END.
