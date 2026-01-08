FOR EACH mp-entr-mat WHERE
         mp-entr-mat.nro-docto = 4893 NO-LOCK.
    DISP mp-entr-mat.nro-docto 
         mp-entr-mat.cod-emit
         mp-entr-mat.dt-recebimento
         mp-entr-mat.dt-emissao-nf
         mp-entr-mat.padrao[1]
         mp-entr-mat.peso-nf
         mp-entr-mat.qtd-fardos[1]
         WITH WIDTH 500.
END.
