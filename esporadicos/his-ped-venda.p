OUTPUT TO c:\temp\his-ped-venda.txt.
FOR EACH his-ped-venda-ext
    WHERE dt-trans > 01.01.2015:
    EXPORT DELIMITER "|" his-ped-venda-ext.
END.
OUTPUT CLOSE.
