DEFINE PARAMETER BUFFER p-ped-item-res FOR ped-item-res.

FOR EACH ped-item-rom WHERE
         ped-item-rom.nome-abrev = p-ped-item-res.nome-abrev AND
         ped-item-rom.nr-pedcli = p-ped-item-res.nr-pedcli AND
         ped-item-rom.nr-sequencia = p-ped-item-res.nr-sequencia EXCLUSIVE-LOCK.
    DELETE ped-item-rom.
END.
/*
FIND ped-item-ext WHERE ped-item-ext.nome-abrev   = p-ped-item-res.nome-abrev
                    AND ped-item-ext.nr-pedcli    = p-ped-item-res.nr-pedcli
                    AND ped-item-ext.it-codigo    = p-ped-item-res.it-codigo
                    AND ped-item-ext.cod-refer    = p-ped-item-res.cod-refer
                    AND ped-item-ext.nr-sequencia = p-ped-item-res.nr-sequencia
                  NO-ERROR.
IF AVAIL ped-item-ext THEN
   ASSIGN ped-item-ext.reservado = NO.
*/
