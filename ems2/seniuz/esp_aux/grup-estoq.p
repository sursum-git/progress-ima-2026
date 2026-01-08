OUTPUT TO c:/temp/grup-estoq.txt CONVERT SOURCE "ibm850".
FOR EACH grup-estoq NO-LOCK.
    DISP grup-estoq.ge-codigo
         grup-estoq.descricao.
END.
OUTPUT CLOSE.
