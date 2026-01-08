OUTPUT TO c:\temp\nfs_itajai.txt.
FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.cod-estabel = '505'.
    /*FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.

    END.*/
    FIND FIRST movto-estoq
         WHERE movto-estoq.cod-estabel = nota-fiscal.cod-estabel
         AND   movto-estoq.serie       = nota-fiscal.serie
         AND   movto-estoq.nro-docto   = nota-fiscal.nr-nota-fis
         //AND   movto-estoq.tipo-trans  = 22
         AND  movto-estoq.dt-trans     = nota-fiscal.dt-emis-nota
         NO-LOCK NO-ERROR.
        EXPORT DELIMITER "|" nota-fiscal.nr-nota-fis nota-fiscal.dt-emis-nota
            IF AVAIL movto-estoq THEN movto-estoq.cod-depos ELSE 'sem movto'.
    
     

END.

OUTPUT CLOSE.
