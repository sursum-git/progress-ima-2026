DEF VAR i-cont AS INT.

DEF TEMP-TABLE tt-work
    FIELD nuance     AS CHAR
    FIELD quantidade AS DEC FORMAT ">>>,>>>,>>9.99"
    INDEX ch-work nuance.

FOR EACH mov-est-acbm /*WHERE mov-est-acbm.data-mov = TODAY*/ NO-LOCK.
    DO i-cont = 1 TO 10:
       FIND tt-work WHERE tt-work.nuance = mov-est-acbm.nuance-cla[i-cont] NO-LOCK NO-ERROR.
       IF NOT AVAIL tt-work THEN DO:
          CREATE tt-work.
          ASSIGN tt-work.nuance = mov-est-acbm.nuance-cla[i-cont].
       END.
       ASSIGN tt-work.quantidade = tt-work.quantidade + mov-est-acbm.nuance-qtd[i-cont].
    END.
END.
OUTPUT TO c:/temp/nuance.csv.
FOR EACH tt-work.
    PUT tt-work.nuance ";"
        tt-work.quantidade
        SKIP.
END.
OUTPUT CLOSE.
