DISABLE TRIGGERS FOR LOAD OF ped-item-rom.

FOR EACH ped-item-rom WHERE ped-item-rom.nr-ob = 88035
                        AND (ped-item-rom.nr-seq-etq = 209 OR
                             ped-item-rom.nr-seq-etq = 211 OR
                             ped-item-rom.nr-seq-etq = 3).
    /*DELETE ped-item-rom.*/
END.
