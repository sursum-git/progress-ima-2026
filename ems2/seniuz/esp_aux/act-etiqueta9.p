FIND ob-etiqueta WHERE
     ob-etiqueta.num-etiqueta = 95069.

FIND ordem-benefic OF ob-etiqueta NO-ERROR.


DISP  ob-etiqueta.it-codigo LABEL "Item Etiqueta" 
      ob-etiqueta.cod-refer
      ordem-benefic.it-codigo LABEL "Item OB" 
      ordem-benefic.cod-refer.

MESSAGE 'Item da OB = Item da Etiqueta = SIM' SKIP
        'Item da Etiqueta = Item da OB = NO'
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE l-conf AS LOG.

CASE l-conf:
    WHEN YES THEN 
        ASSIGN ordem-benefic.it-codigo = ob-etiqueta.it-codigo
               ordem-benefic.cod-refer = ob-etiqueta.cod-refer.
    WHEN NO THEN
        ASSIGN ob-etiqueta.it-codigo = ordem-benefic.it-codigo 
               ob-etiqueta.cod-refer = ordem-benefic.cod-refer.
END CASE.
