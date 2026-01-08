OUTPUT TO c:/temp/ob-etiqueta.csv.
PUT "Num-Etiq;Sit;Item;Refer;Lote" SKIP.
FOR EACH ob-etiqueta WHERE ob-etiqueta.situacao = 4.
    FIND ped-item-rom WHERE ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel
                        AND ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
                      NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-item-rom THEN
       PUT ob-etiqueta.num-etiqueta ";"
           INT(ob-etiqueta.situacao) ";"
           ob-etiqueta.it-codigo ";"
           ob-etiqueta.cod-refer ";"
           ob-etiqueta.nr-lote SKIP.                
END.
OUTPUT CLOSE.
