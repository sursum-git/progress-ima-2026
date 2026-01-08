FIND ob-etiqueta WHERE
     ob-etiqueta.num-etiqueta = 494112.

FIND ped-item-rom WHERE
     ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta.

DELETE ped-item-rom.

ASSIGN ob-etiqueta.situacao = 3.

