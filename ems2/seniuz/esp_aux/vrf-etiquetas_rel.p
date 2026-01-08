OUTPUT TO c:/temp/reporte.csv.
PUT "Est;Dt-Emis;Sit;Hora;Num-Etiq;Loc;Lote;Item;Refer;Quant" SKIP.
    FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao  >= 3 AND
         ob-etiqueta.nr-reporte = 0 AND
         ob-etiqueta.tipo-ordem = 1 AND
         ob-etiqueta.quantidade > 0 NO-LOCK
         BY ob-etiqueta.dt-emissao
         BY ob-etiqueta.hr-emissao.

    FIND FIRST item-uni-estab WHERE
               item-uni-estab.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    put ob-etiqueta.cod-estabel ";"
         ob-etiqueta.dt-emissao ";"
         int(ob-etiqueta.situacao) ";"
         ob-etiqueta.hr-emissao FORMAT "x(5)" ";"
         int(ob-etiqueta.num-etiqueta) ";"
         ob-etiqueta.localiz  ";"
         ob-etiqueta.nr-lote FORMAT "x(4)" ";"
         ob-etiqueta.it-codigo ";"
         ob-etiqueta.cod-refer ";"
         ob-etiqueta.quantidade
         SKIP.
END.
OUTPUT CLOSE.

