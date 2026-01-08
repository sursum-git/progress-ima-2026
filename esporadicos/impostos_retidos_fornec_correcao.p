DEFINE VARIABLE l AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cLinha AS CHARACTER  FORMAT 'x(20)' NO-UNDO.
DEFINE TEMP-TABLE tt
    FIELD fornecedor AS INT.
/*OUTPUT TO c:\temp\impto_vincul_fornec.txt. 
FOR EACH impto_vincul_fornec:
    EXPORT DELIMITER "|" impto_vincul_fornec.
END.
OUTPUT CLOSE.
OUTPUT TO c:\temp\f.txt.
FOR EACH ems5.fornecedor:
   ASSIGN l = NO.
    FOR EACH impto_vincul_fornec OF fornecedor:
        ASSIGN l = YES.
    END.
    IF l = YES AND fornecedor.log_retenc_impto_pagto = NO  THEN DO:
       ASSIGN fornecedor.log_retenc_impto_pagto = YES.
       PUT fornecedor.cdn_fornec SKIP.
    END.
       
END.
OUTPUT CLOSE.*/


INPUT FROM c:\temp\f.txt.
REPEAT:

    IMPORT UNFORM cLinha.
    CREATE tt.
    ASSIGN  tt.fornecedor = INT(cLinha).

END.

INPUT CLOSE.

FOR EACH tt:
    FIND FIRST ems5.fornecedor 
        WHERE cdn_fornec = tt.fornecedor NO-ERROR.
    ASSIGN  fornecedor.log_retenc_impto_pagto = NO .
    
END.

