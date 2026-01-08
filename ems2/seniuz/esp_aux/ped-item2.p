def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.txt" CONVERT SOURCE "ibm850".

FOR EACH ped-item WHERE ped-item.cod-sit-item = 1 NO-LOCK,
    EACH ped-item-res WHERE ped-item-res.nome-abrev   = ped-item.nome-abrev AND
                            ped-item-res.nr-pedcli    = ped-item.nr-pedcli   AND
                            ped-item-res.nr-sequencia = ped-item.nr-sequencia
                      NO-LOCK.
    FIND FIRST ped-item-rom WHERE ped-item-rom.nome-abrev   = ped-item.nome-abrev
                              AND ped-item-rom.nr-pedcli    = ped-item.nr-pedcli
                              AND ped-item-rom.nr-sequencia = ped-item.nr-sequencia
                            NO-LOCK NO-ERROR.

    IF NOT AVAIL ped-item-rom THEN
       DISPLAY ped-item.nome-abrev
               ped-item.nr-pedcli
               ped-item.nr-sequencia
               ped-item.cod-sit-item VIEW-AS FILL-IN.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.

