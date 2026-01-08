DEF VAR i-ct AS INT.
DEF VAR i-fardo AS DEC.
FOR EACH mp-fardo WHERE STRING(mp-fardo.nr-fardo) >= "8070001" AND
                        STRING(mp-fardo.nr-fardo) <= "8079999" AND
                        mp-fardo.situacao = 3.
    
    ASSIGN i-ct = i-ct + 1
           i-fardo = mp-fardo.nr-fardo.
           
    
    ASSIGN mp-fardo.peso = mp-fardo.peso - 6.41.
           
END.
DISP i-ct
     i-fardo FORMAT "9999999".
