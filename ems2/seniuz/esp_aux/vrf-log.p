FOR EACH his-ped-venda-ext WHERE
         his-ped-venda-ext.dt-trans    >= 01.13.2015 NO-LOCK.

    IF his-ped-venda-ext.ocorrencia MATCHES "*twdi159*" THEN
       DISP his-ped-venda-ext.nr-pedcli
            his-ped-venda-ext.usuario  
            his-ped-venda-ext.ocorrencia
            WITH WIDTH 550.
END.


