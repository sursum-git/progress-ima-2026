DEF OUTPUT PARAMETER p-pasta-erp AS CHAR FORMAT "x(50)". 
IF OPSYS <> 'UNIX' THEN DO:
   ASSIGN p-pasta-erp = REPLACE(REPLACE(SEARCH("cdp/cd0204.r"),"/","\"),"\ems2\cdp\cd0204.r","").     
END.
ELSE DO:
   ASSIGN p-pasta-erp = '/mnt/datasul/ERP'.
END.
