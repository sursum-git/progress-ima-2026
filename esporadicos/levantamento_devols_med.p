OUTPUT TO c:\temp\devol_ima_med.txt.
FOR EACH docum-est NO-LOCK
    WHERE docum-est.cod-estabel = '5'
    AND   docum-est.serie-docto = '3'
    AND   docum-est.cod-emitente = 1
    AND   docum-est.dt-trans >= 10.06.2017
    AND   docum-est.dt-trans <=  05.18.2018.
    EXPORT DELIMITER "|" docum-est.cod-estabel 
           docum-est.serie-docto
           docum-est.cod-emitente
           docum-est.nro-docto
           docum-est.tot-valor
           docum-est.dt-trans.
END.
