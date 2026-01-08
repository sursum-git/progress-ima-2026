DEF VAR h-acomp AS HANDLE    NO-UNDO. 

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Processando *}

RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FOR EACH inv-acab WHERE 
         inv-acab.data-inv = 08.07.2007 NO-LOCK
         BREAK BY inv-acab.it-codigo.

    run pi-acompanhar in h-acomp (INPUT inv-acab.it-codigo + " " + STRING(inv-acab.num-etiqueta)).

    FIND ob-etiqueta WHERE
         ob-etiqueta.num-etiqueta = inv-acab.num-etiqueta
         SHARE-LOCK NO-ERROR.

    IF NOT AVAIL ob-etiqueta THEN NEXT.
    IF NOT ob-etiqueta.nr-lote BEGINS 'P' THEN NEXT.

    ASSIGN ob-etiqueta.situacao = 3. 
END.

run pi-finalizar in h-acomp.
