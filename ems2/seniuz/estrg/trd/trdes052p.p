DEFINE PARAMETER BUFFER p-ped-item-rom FOR ped-item-rom.

FIND ob-etiqueta WHERE
     ob-etiqueta.cod-estabel  = p-ped-item-rom.cod-estabel and
     ob-etiqueta.num-etiqueta = p-ped-item-rom.num-etiqueta
     SHARE-LOCK NO-ERROR.

IF AVAIL ob-etiqueta THEN
   ASSIGN ob-etiqueta.situacao = 3.
