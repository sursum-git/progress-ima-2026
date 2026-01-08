DEF VAR i-cont AS INT.
FOR EACH docum-est WHERE docum-est.cod-estabel = "2"
                     AND docum-est.dt-trans    >= 09/01/2010
                     AND docum-est.dt-trans    <= 09/30/2020
                     AND (docum-est.esp-docto = 18 OR docum-est.esp-docto = 21)
                   NO-LOCK.
    ASSIGN i-cont = i-cont + 1.
    /*
    DISP docum-est.serie-docto
         docum-est.nro-docto
         docum-est.cod-emitente
         docum-est.dt-emissao
         docum-est.dt-trans.
    */
END.
DISP i-cont.
