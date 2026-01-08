FOR EACH ped-item-rom NO-LOCK.
    FIND ped-item WHERE ped-item.nome-abrev = ped-item-rom.nome-abrev
                    AND ped-item.nr-pedcli  = ped-item-rom.nr-pedcli
                    AND ped-item.nr-sequencia = ped-item-rom.nr-sequencia
                  NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-item-rom THEN
       DISP ped-item-rom.nome-abrev
            ped-item-rom.nr-pedcli.
END.
