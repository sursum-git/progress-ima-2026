
DEF VAR c-desp-imp AS CHAR FORMAT "x(25)".
DEF VAR de-vl-contab AS DEC.

FUNCTION fn-desp-imp RETURNS CHARACTER
  ( input p-cod-desp as integer ):
  
  find desp-imp where desp-imp.cod-desp = p-cod-desp no-lock no-error.
  if avail desp-imp then
    RETURN desp-imp.descricao.
  else
    RETURN "".
END FUNCTION.

FUNCTION fn-emitente RETURNS CHARACTER
  ( input p-cod-emitente as integer ) :
  
  find emitente where emitente.cod-emitente = p-cod-emitente no-lock no-error.
  if avail emitente then
    RETURN emitente.nome-abrev.
  else
    RETURN "".

END FUNCTION.



FIND nota-fiscal WHERE
     nota-fiscal.cod-estabel = '5' AND
     nota-fiscal.serie = '3' AND
     nota-fiscal.nr-nota-fis = '0089781'.

FIND FIRST docum-est 
     WHERE docum-est.serie-docto  = nota-fiscal.serie
     AND   docum-est.nro-docto    = nota-fiscal.nr-nota-fis
     AND   docum-est.cod-emitente = nota-fiscal.cod-emitente
     AND   docum-est.nat-operacao = nota-fiscal.nat-operacao
     NO-LOCK NO-ERROR.

FOR EACH docum-est-cex WHERE 
         docum-est-cex.serie-docto  = docum-est.serie-docto AND 
         docum-est-cex.nro-docto    = docum-est.nro-docto  AND 
         docum-est-cex.cod-emitente = docum-est.cod-emitente AND 
         docum-est-cex.nat-operacao = docum-est.nat-operacao NO-LOCK.

    DISP docum-est-cex.cod-emitente-desp
         docum-est-cex.cod-desp
         fn-desp-imp(docum-est-cex.cod-desp) @ c-desp-imp
         docum-est-cex.val-desp  format ">>>>>,>>>,>>9.99".
END.

