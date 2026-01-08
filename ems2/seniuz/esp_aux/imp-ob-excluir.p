DEF TEMP-TABLE tt-excluir-ob
    FIELD num-lote LIKE mov-est-acbm.num-lote
    INDEX ch-excluir-ob num-lote.

DEF STREAM aux.
INPUT STREAM aux FROM temp/ob-excluir.csv CONVERT SOURCE "ibm850". 
SET STREAM aux ^.

REPEAT:
   CREATE tt-excluir-ob.
   IMPORT STREAM aux DELIMITER ";" tt-excluir-ob.
END.
INPUT STREAM aux CLOSE.

FIND tt-excluir-ob WHERE tt-excluir-ob.num-lote = 0 NO-ERROR.
IF AVAIL tt-excluir-ob THEN 
   DELETE tt-excluir-ob.  

FOR EACH tt-excluir-ob.
    MESSAGE tt-excluir-ob.num-lote
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
