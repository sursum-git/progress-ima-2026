OUTPUT TO p:\tadeu\movto-estoq.txt APPEND.
FOR EACH movto-estoq
    WHERE dt-trans > 01.01.2015
    AND esp-docto = 21  :
    FIND FIRST emitente OF movto-estoq NO-LOCK NO-ERROR.
    FIND ITEM OF movto-estoq NO-LOCK NO-ERROR.

    FIND FIRST cta_ctbl 
        WHERE cta_ctbl.cod_cta_ctbl = movto-estoq.ct-codigo
        NO-LOCK NO-ERROR.
    FIND FIRST ems5.ccusto
        WHERE ccusto.cod_ccusto = movto-estoq.sc-codigo
        NO-LOCK NO-ERROR.
   EXPORT DELIMITER "|" movto-estoq.it-codigo movto-estoq.cod-refer movto-estoq.valor-mat-m[1]     
    movto-estoq.valor-nota 
    movto-estoq.cod-estabel
    {ininc/i01in218.i 4 tipo-trans}
    dt-trans
    {ininc/i03in218.i 4 esp-docto}
     movto-estoq.cod-emitente
     IF AVAIL emitente THEN nome-emit ELSE ''
     serie-docto
     nro-docto
     movto-estoq.nat-operacao
     movto-estoq.sc-codigo
     movto-estoq.ct-codigo
     IF AVAIL cta_ctbl THEN cta_ctbl.des_tit_ctbl ELSE ''
     IF AVAIL ccusto   THEN ccusto.des_tit_ctbl   ELSE ''
     sc-saldo
     ct-saldo
     usuario
     {ininc/i09in122.i 4 tipo-contr} . 
END.
OUTPUT CLOSE.
       
