DEFINE TEMP-TABLE tt
    FIELD itCodigo AS CHAR
    FIELD iClass   AS INT.
INPUT FROM c:\temp\seg_class_item.csv.


REPEAT:
    CREATE tt.
    IMPORT DELIMITER ';'  tt .
END.

FOR EACH tt:
    //DISP tt.
    FIND item-ext
        WHERE item-ext.it-codigo = tt.itCodigo
        NO-ERROR.
    IF AVAIL item-ext THEN
       //ASSIGN item-ext.num_familia_ger = tt.iClass.
        DISP tt.itCodigo item-ext.num_familia_ger .
    ELSE 
        DISP 'nao encontrado item:' tt.itCodigo . PAUSE 0. 

END.

