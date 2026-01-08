DEFINE VARIABLE l AS LOGICAL     NO-UNDO.
OUTPUT TO c:\temp\impto_vincul_fornec.txt. 
FOR EACH impto_vincul_fornec:
    EXPORT DELIMITER "|" impto_vincul_fornec.
END.
OUTPUT CLOSE.
OUTPUT TO c:\temp\f500.txt.
FOR EACH ems5.fornec_financ:
   ASSIGN l = NO.
    FOR EACH impto_vincul_fornec OF fornec_financ
      /*where impto_vincul_fornec.cod_empresa = '500'*/ :
        ASSIGN l = YES.
    END.
    IF l = YES AND fornec_financ.log_retenc_impto = NO  THEN DO:
       ASSIGN fornec_financ.log_retenc_impto = YES.
       PUT fornec_financ.cdn_fornec SKIP.
    END.
       
END.
OUTPUT CLOSE.
