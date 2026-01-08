FOR EACH ordem-benefic WHERE
         ordem-benefic.cod-estabel = '1' AND
         ordem-benefic.nr-ob       = 150397 AND
         ordem-benefic.nr-carro    = '03C'  SHARE-LOCK.
    ASSIGN ordem-benefic.situacao = 1.
    DISP ordem-benefic.dt-ob
         ordem-benefic.nr-ob
         ordem-benefic.nr-carro
         ordem-benefic.situacao VIEW-AS FILL-IN.
END.
