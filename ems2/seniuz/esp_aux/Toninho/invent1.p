DEF BUFFER b-inv-acab FOR inv-acab.

FOR EACH inv-acab WHERE
         inv-acab.cod-estabel = '5' AND
         inv-acab.data-invent = 12.19.2015 EXCLUSIVE-LOCK.

    FIND b-inv-acab WHERE
         b-inv-acab.cod-estabel = '5' AND
         b-inv-acab.data-invent = 12.19.2015 AND
         b-inv-acab.num-etiqueta = inv-acab.num-etiqueta NO-LOCK NO-ERROR.

    IF AMBIGUOUS b-inv-acab THEN DO.
       DISP inv-acab.num-etiqueta
            inv-acab.localiz.
       DELETE inv-acab.
    END.
END.


