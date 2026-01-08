/*
FIND ob-etiqueta WHERE
     ob-etiqueta.num-etiqueta = 1506434 NO-LOCK.

FIND ped-item-rom WHERE
     ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.

FIND ped-item-res WHERE
     ped-item-res.nr-pedcli = ped-item-rom.nr-pedcli AND
     ped-item-res.nome-abrev = ped-item-rom.nome-abrev AND
     ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia
     NO-ERROR.

DISP ob-etiqueta.situacao
     ped-item-rom.nr-pedcli
     ped-item-res.dt-trans
     .
*/
/*
FOR EACH ped-item-ext WHERE
         ped-item-ext.nr-pedcli = '210666' AND
         ped-item-ext.it-codigo = '175146'.
         
        DISP ped-item-ext.it-codigo
             ped-item-ext.cod-refer
             ped-item-ext.reservado.
END.
*/
