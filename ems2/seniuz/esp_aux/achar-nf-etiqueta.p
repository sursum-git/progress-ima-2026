FIND ob-etiqueta WHERE
     ob-etiqueta.num-etiqueta = 700667 NO-LOCK NO-ERROR.
IF AVAIL ob-etiqueta THEN DO:
   FIND ped-item-rom WHERE
        ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
   IF AVAIL ped-item-rom THEN DO:
       FIND ped-item-res WHERE
            ped-item-res.nome-abrev = ped-item-rom.nome-abrev   AND
            ped-item-res.nr-pedcli  = ped-item-rom.nr-pedcli    AND
            ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia NO-LOCK NO-ERROR.
       IF AVAIL ped-item-res THEN
          DISP ped-item-res.nr-nota-fis.
   END.
END.
