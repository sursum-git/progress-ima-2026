DEF VAR i-nr-seq AS INT.
DEF VAR c-cod-estab AS CHAR.
DEF VAR da-data-inv AS DATE.
DEF VAR c-docto AS CHAR.

ASSIGN c-cod-estab = '1'
       da-data-inv = 12.19.2015
       c-docto = STRING(DAY(da-data-inv)) + 
                 STRING(MONTH(da-data-inv)) + 
                 STRING(YEAR(da-data-inv)).

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = c-cod-estab AND
         ob-etiqueta.situacao >= 3 AND
         ob-etiqueta.situacao <= 4 NO-LOCK.
 
    FIND ITEM WHERE
         ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    FIND LAST inv-acab WHERE
              inv-acab.cod-estabel = c-cod-estab AND
              inv-acab.data-invent = da-data-inv AND
              inv-acab.docto       = INT(c-docto)
              USE-INDEX indice1 NO-LOCK NO-ERROR.
 
    ASSIGN i-nr-seq = IF AVAIL inv-acab
                      THEN inv-acab.seq + 1
                      ELSE 0.
 
    ASSIGN i-nr-seq = i-nr-seq + 1.
    CREATE inv-acab.
    ASSIGN inv-acab.cod-estabel  = ob-etiqueta.cod-estabel
           inv-acab.it-codigo    = ob-etiqueta.it-codigo
           inv-acab.cod-refer    = ob-etiqueta.cod-refer
           inv-acab.lote         = ob-etiqueta.nr-lote + ob-etiqueta.cod-refer
           inv-acab.data-invent  = da-data-inv
           inv-acab.data-trans   = TODAY
           inv-acab.hora-trans   = STRING(TIME,"HH:MM")
           inv-acab.localiz      = ob-etiqueta.localizacao
           inv-acab.docto        = INT(c-docto)
           inv-acab.seq          = i-nr-seq
           inv-acab.num-etiqueta = ob-etiqueta.num-etiqueta
           inv-acab.qtd-inv      = ob-etiqueta.quantidade
           inv-acab.usuario      = 'super'
           inv-acab.origem       = 3
           inv-acab.un           = item.un
           inv-acab.situacao     = 1.
END.

