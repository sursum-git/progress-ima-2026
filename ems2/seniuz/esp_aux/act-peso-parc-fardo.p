DEF var de-peso-apos  AS DEC.
DEF VAR de-peso-antes AS DEC.
DEF VAR de-peso-abat  AS DEC.
DEF VAR i-ct AS INT.
ASSIGN de-peso-antes = 0.
FOR EACH mp-fardo WHERE mp-fardo.nr-fardo >= 04070001  AND
                        mp-fardo.nr-fardo <= 04079999  AND 
                        mp-fardo.situacao  = 3 NO-LOCK.
    ASSIGN i-ct = i-ct + 1.
    ASSIGN de-peso-antes = de-peso-antes + mp-fardo.peso.
END.
ASSIGN de-peso-abat = round(1598 / i-ct,2).
ASSIGN de-peso-apos = 0.
FOR EACH mp-fardo WHERE mp-fardo.nr-fardo >= 04070001  AND
                        mp-fardo.nr-fardo <= 04079999  AND 
                        mp-fardo.situacao  = 3.
    ASSIGN mp-fardo.peso = mp-fardo.peso - de-peso-abat.
    ASSIGN de-peso-apos = de-peso-apos + mp-fardo.peso.
END.
DISP i-ct
     de-peso-abat
     de-peso-abat * i-ct
     de-peso-antes
     de-peso-apos.
