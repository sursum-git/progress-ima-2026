OUTPUT TO c:\temp\saldo_estoq.csv.
PUT "item;ref;Qt.Em Estoque" SKIP.
FOR EACH saldo-estoq
    WHERE saldo-estoq.qtidade-atu > 0
    AND   substr(saldo-estoq.it-codigo,3,1) <> '5'.
    EXPORT DELIMITER ";" 
        saldo-estoq.it-codigo
        saldo-estoq.cod-refer
        saldo-estoq.qtidade-atu .
    
END.
OUTPUT CLOSE.
