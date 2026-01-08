DEFINE VARIABLE i AS INTEGER     NO-UNDO.
DEFINE VARIABLE i2 AS INTEGER     NO-UNDO.    
OUTPUT TO c:\temp\LOG_wp.txt.
REPEAT i = 1 TO 3000:
    ASSIGN i2 = 0.
    apaga_nf:
    FOR EACH wp_nota_fiscal:
        ASSIGN i2 = i2 + 1.
        IF i2 = 1000 THEN DO:
            PUT '1000 registros apagados' SKIP.
            LEAVE apaga_nf.                     
        END.
        DELETE wp_nota_fiscal.
    END.    
END.

