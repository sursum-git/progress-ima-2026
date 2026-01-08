DEF VAR de-vl-contab AS DEC.
FIND nota-fiscal WHERE
     nota-fiscal.cod-estabel = '5' AND
     nota-fiscal.serie = '3' AND
     nota-fiscal.nr-nota-fis = '0089781'.

FIND natur-oper OF nota-fiscal NO-LOCK.
 
DISP INT(nota-fiscal.esp-docto).

FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK. 
    FIND FIRST docum-est 
         WHERE docum-est.serie-docto  = nota-fiscal.serie
         AND   docum-est.nro-docto    = nota-fiscal.nr-nota-fis
         AND   docum-est.cod-emitente = nota-fiscal.cod-emitente
         AND   docum-est.nat-operacao = nota-fiscal.nat-operacao
         NO-LOCK NO-ERROR.

    FIND FIRST item-doc-est OF docum-est 
         WHERE item-doc-est.it-codigo = it-nota-fisc.it-codigo 
         AND   item-doc-est.cod-refer = it-nota-fisc.cod-refer
         NO-LOCK NO-ERROR.

    IF nota-fiscal.esp-doct = 21 OR nota-fiscal.esp-docto = 20 THEN
       ASSIGN de-vl-contab = IF AVAIL item-doc-est THEN item-doc-est.val-base-calc-cofins ELSE 0.
    ELSE
       ASSIGN de-vl-contab = it-nota-fisc.vl-tot-item.

    DISP de-vl-contab
         de-vl-contab * dec(substr(it-nota-fisc.char-2,76,5)) / 100   (TOTAL)
         de-vl-contab * dec(SUBSTR(it-nota-fisc.char-2,81,5)) / 100   (TOTAL)
         SKIP.
END.

 
