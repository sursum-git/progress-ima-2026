FOR EACH docum-est
    WHERE docum-est.cod-estabel = '5'
    AND   docum-est.cod-emitente = 26691
    AND   docum-est.dt-trans = 04.07.2020
    AND   docum-est.nro-docto = '0386170'
    AND   docum-est.serie = '' .
    DISP docum-est.nat-operacao docum-est.serie   .
    ASSIGN  docum-est.cod-observa = 4.

END.
