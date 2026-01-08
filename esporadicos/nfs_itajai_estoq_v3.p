DEFINE VARIABLE cListaDep AS CHARACTER   NO-UNDO.
{esp/util.i}
OUTPUT TO c:\temp\nfs_itajai.txt.

FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.cod-estabel = '505'.
    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
        FIND FIRST movto-estoq
         WHERE movto-estoq.cod-estabel = nota-fiscal.cod-estabel
         AND   movto-estoq.serie       = nota-fiscal.serie
         AND   movto-estoq.nro-docto   = nota-fiscal.nr-nota-fis
         AND   movto-estoq.it-codigo   = it-nota-fisc.it-codigo
         AND   movto-estoq.cod-refer   = it-nota-fisc.cod-refer
         //AND   movto-estoq.tipo-trans  = 22
         AND  movto-estoq.dt-trans     = nota-fiscal.dt-emis-nota
         NO-LOCK NO-ERROR.
        IF AVAIL movto-estoq THEN DO:
           IF LOOKUP(movto-estoq.cod-depos,cListaDep) = 0  THEN DO:
              RUN incrValor(INPUT-OUTPUT cListaDep,movto-estoq.cod-depos,',').
           END.                                                               

        END.
    END.
    IF cListaDep = '' THEN 
       ASSIGN cListaDep = "Sem Movto".

    EXPORT DELIMITER "|" nota-fiscal.nr-nota-fis nota-fiscal.dt-emis-nota
            cListaDep.
    
    
     

END.

OUTPUT CLOSE.
