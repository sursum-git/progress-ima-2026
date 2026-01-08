FOR EACH dev-item-rom WHERE
         dev-item-rom.nr-pedcli = '123105'.

    FIND ob-etiqueta where
         ob-etiqueta.num-etiqueta = dev-item-rom.num-etiqueta
         NO-ERROR.

    DISP ob-etiqueta.localiz.
END.
