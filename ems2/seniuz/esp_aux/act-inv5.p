DEF VAR i-nr-seq LIKE inv-acab.seq.
DEF VAR c-localiz LIKE ob-etiqueta.localizacao.
DEF VAR c-etiqueta LIKE ob-etiqueta.num-etiqueta.
DEF VAR c-cod-estabel LIKE ob-etiqueta.cod-estabel.

UPDATE c-etiqueta c-localiz c-cod-estabel.

FIND LAST inv-acab WHERE
          inv-acab.data-invent = 12.31.2007  AND
          inv-acab.docto = 1459201
          USE-INDEX indice1 NO-LOCK NO-ERROR.

ASSIGN i-nr-seq = IF AVAIL inv-acab
                  THEN inv-acab.seq + 1
                  ELSE 0.

ASSIGN i-nr-seq = i-nr-seq + 1.

FIND ob-etiqueta WHERE
     ob-etiqueta.cod-estabel  = c-cod-estabel AND
     ob-etiqueta.num-etiqueta = c-etiqueta.

CREATE inv-acab.
ASSIGN inv-acab.it-codigo    = ob-etiqueta.it-codigo
       inv-acab.cod-refer    = ob-etiqueta.cod-refer
       inv-acab.data-invent  = 12.31.2007
       inv-acab.docto        = 1459201
       inv-acab.seq          = i-nr-seq
       inv-acab.qtd-inv      = ob-etiqueta.quantidade
       inv-acab.lote         = ob-etiqueta.nr-lote + ob-etiqueta.cod-refer
       inv-acab.num-etiqueta = ob-etiqueta.num-etiqueta
       inv-acab.cod-estabel  = ob-etiqueta.cod-estabel
       inv-acab.situacao     = 1.

ASSIGN ob-etiqueta.localizacao = c-localiz.


