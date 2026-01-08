DEF BUFFER b-etiqueta OF ob-etiqueta.
FIND FIRST ordem-benefic WHERE
           ordem-benefic.nr-ob = 68075.

FIND LAST b-etiqueta OF ordem-benefic NO-LOCK NO-ERROR.

CREATE ob-etiqueta.
ASSIGN ob-etiqueta.nr-ob = ordem-benefic.nr-ob
       ob-etiqueta.nr-carro = ordem-benefic.nr-carro
       ob-etiqueta.dt-ob = ordem-benefic.dt-ob
       ob-etiqueta.nr-seq = 42
       ob-etiqueta.situacao = 3
       ob-etiqueta.acondic = corte-comerc.descricao
       ob-etiqueta.cod-qualid = c-qualid. 
       ob-etiqueta.num-etiqueta = NEXT-VALUE(seq-etq-estoq).
