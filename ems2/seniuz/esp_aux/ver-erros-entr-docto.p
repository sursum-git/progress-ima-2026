DEF VAR i-qtd-fardos AS INT.  
DEF VAR p-fardo AS INT.
DEF VAR de-peso-fardos AS DEC.
DEF VAR u-fardo AS INT.
  FOR EACH mp-entr-mat WHERE mp-entr-mat.nro-docto = 98 NO-LOCK. 
      FOR EACH mp-fardo WHERE mp-fardo.nr-cdr = mp-entr-mat.nr-cdr NO-LOCK.
          IF i-qtd-fardos = 0 THEN
             ASSIGN p-fardo = mp-fardo.nr-fardo.
          ASSIGN de-peso-fardos = de-peso-fardos + mp-fardo.peso
                 i-qtd-fardos   = i-qtd-fardos + 1
                 u-fardo        = mp-fardo.nr-fardo.
     END.
     DISP  mp-entr-mat.nro-docto FORMAT ">>>>9"
           mp-entr-mat.peso-nf FORMAT ">>,>>9.99"
           (mp-entr-mat.qtd-fardos[1] + mp-entr-mat.qtd-fardos[2] +
           mp-entr-mat.qtd-fardos[3] + mp-entr-mat.qtd-fardos[4] +
           mp-entr-mat.qtd-fardos[5]) LABEL "Fardos" FORMAT ">>>9"
           de-peso-fardos  LABEL "Peso Fardos"
           i-qtd-fardos    LABEL "QTD"
           p-fardo LABEL "Prim" FORMAT "9999,9999"
           u-fardo LABEL "Ultimo" FORMAT "9999,9999".
  END.
