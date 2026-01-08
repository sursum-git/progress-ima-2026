FOR EACH mp-entr-mat WHERE
         mp-entr-mat.nro-docto = 1800.
    FIND mp-entr-cam OF mp-entr-mat.
   
    IF AVAIL mp-entr-cam THEN
       DISP mp-entr-mat.dt-recebimento
            mp-entr-mat.cod-emit
            mp-entr-mat.nro-docto
            mp-entr-mat.padrao[1]
            mp-entr-mat.qtd-fardos[1]
            mp-entr-cam.placa.
END.
