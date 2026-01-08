FOR EACH dev-item-rom WHERE
         dev-item-rom.nr-pedcli = "123581" NO-LOCK.

    FIND ob-etiqueta WHERE
         ob-etiqueta.num-etiqueta = dev-item-rom.num-etiqueta
         NO-ERROR.

    ASSIGN ob-etiqueta.localiz = '399399'.
END.
