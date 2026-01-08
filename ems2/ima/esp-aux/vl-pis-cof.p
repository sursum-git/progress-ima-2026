DEF TEMP-TABLE tt-desp
    FIELD cod-desp  as integer format ">>>>9"
    FIELD descricao as char    format "x(30)"
    FIELD val-desp  as decimal format ">>>>>,>>>,>>9.99999"
    INDEX codigo is unique primary
        cod-desp.

FIND nota-fiscal WHERE
     nota-fiscal.cod-estabel = '5' AND
     nota-fiscal.serie = '3' AND
     nota-fiscal.nr-nota-fis = '0055977'.

FIND docum-est WHERE
     docum-est.serie-docto  = nota-fiscal.serie AND
     docum-est.nro-docto    = nota-fiscal.nr-nota-fis AND
     docum-est.cod-emitente = nota-fiscal.cod-emitente AND
     docum-est.nat-operacao = nota-fiscal.nat-operacao NO-LOCK NO-ERROR.

FOR EACH item-doc-est WHERE
         item-doc-est.serie-docto  = docum-est.serie-docto AND
         item-doc-est.nro-docto    = docum-est.nro-docto   AND
         item-doc-est.cod-emitente = docum-est.cod-emitente AND
         item-doc-est.nat-operacao = docum-est.nat-operacao NO-LOCK
         BREAK BY class-fisc
               BY un:

    DISP item-doc-est.it-codigo 
         item-doc-est.valor-pis (TOTAL)
         item-doc-est.val-cofins (TOTAL).


END.
