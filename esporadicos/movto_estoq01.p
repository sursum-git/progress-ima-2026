OUTPUT TO c:\temp\mov.txt.
FOR EACH movto-estoq
    WHERE movto-estoq.it-codigo = '560086'
    AND dt-trans = 09/11/2015:
    EXPORT DELIMITER "|" nro-docto it-codigo cod-refer quantidade.


END.
