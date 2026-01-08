FOR EACH ped-item-rom NO-LOCK WHERE ped-item-rom.nome-abrev BEGINS "tecidos ta"
                                AND ped-item-rom.nr-pedcli = "117791"
    BREAK BY ped-item-rom.nome-abrev
          BY ped-item-rom.nr-pedcli
          BY ped-item-rom.nr-sequencia:
    ACCUMULATE ped-item-rom.nr-sequencia (COUNT BY ped-item-rom.nr-sequencia).
    IF LAST-OF(ped-item-rom.nr-sequencia) THEN 
       IF (ACCUM COUNT BY ped-item-rom.nr-sequencia ped-item-rom.nr-sequencia) > 1 THEN
          DISPLAY ped-item-rom.nome-abrev
                  ped-item-rom.nr-pedcli
                  ped-item-rom.nr-sequencia
                  (ACCUM COUNT BY ped-item-rom.nr-sequencia ped-item-rom.nr-sequencia).

END.
