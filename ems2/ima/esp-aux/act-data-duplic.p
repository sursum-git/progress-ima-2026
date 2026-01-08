FIND docum-est WHERE
     docum-est.cod-estabel = '5' AND
     docum-est.serie = '3' AND
     docum-est.nro-docto = '0146338' NO-LOCK.

FOR EACH dupli-apagar OF docum-est NO-LOCK.
    DISP dupli-apagar WITH 1 COL WIDTH 550.
END.

