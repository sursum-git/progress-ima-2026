DEFINE TEMP-TABLE ttItem
    FIELD itCodigo AS CHAR
    FIELD preco   AS DECIMAL
    FIELD codRefer AS CHAR.
    
FOR EACH preco-item
    WHERE preco-item.nr-tabpre = 'tab e12'.
    FIND FIRST ttItem
        WHERE ttItem.itCodigo   = preco-item.it-Codigo
        AND   ttItem.preco      = preco-item.preco-fob 
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ttItem THEN DO:
        
        CREATE ttItem.
        ASSIGN ttItem.itCodigo   = preco-item.it-Codigo
               ttItem.preco      = preco-item.preco-fob 
               ttItem.codRefer   = preco-item.cod-refer.
    END.
END.


OUTPUT TO c:\temp\precos_item.csv.
PUT "item;preco;refer".
FOR EACH ttItem:
    EXPORT DELIMITER ";" ttItem .
END.

OUTPUT CLOSE.
