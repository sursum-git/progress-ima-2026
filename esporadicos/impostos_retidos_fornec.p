DEFINE VARIABLE l AS LOGICAL     NO-UNDO.
OUTPUT TO c:\temp\impto_vincul_fornec.txt. 
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
OUTPUT CLOSE.
