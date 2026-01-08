DEF TEMP-TABLE tt-item
    FIELD it-codigo LIKE saldo-estoq.it-codigo
    FIELD cod-refer LIKE saldo-estoq.cod-refer
    FIELD lote      LIKE ob-etiqueta.nr-lote
    FIELD qtd-estoq LIKE saldo-estoq.qtidade-atu
    FIELD qtd-etiq  LIKE ob-etiqueta.quantidade.

def var h-acomp as handle no-undo.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Consultando *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FOR EACH ITEM WHERE
         ITEM.ge-codigo >= 51 AND
         ITEM.ge-codigo <= 58 NO-LOCK.

    FOR EACH saldo-estoq WHERE
              saldo-estoq.it-codigo = ITEM.it-codigo NO-LOCK.
    
        RUN pi-acompanhar IN h-acomp (INPUT "Item: " + saldo-estoq.it-codigo + " " + saldo-estoq.cod-refer).

        IF SUBSTR(saldo-estoq.lote,1,2) <> 'RP' AND
           SUBSTR(saldo-estoq.lote,1,2) <> 'RD' THEN NEXT.

         FIND tt-item WHERE
              tt-item.it-codigo = saldo-estoq.it-codigo AND
              tt-item.cod-refer = saldo-estoq.cod-refer AND
              tt-item.lote = SUBSTR(saldo-estoq.lote,1,2)
              NO-ERROR.
         IF NOT AVAIL tt-item THEN DO.
            CREATE tt-item.
            ASSIGN tt-item.it-codigo = saldo-estoq.it-codigo
                   tt-item.cod-refer = saldo-estoq.cod-refer
                   tt-item.lote = SUBSTR(saldo-estoq.lote,1,2).
         END.
         ASSIGN tt-item.qtd-estoq = tt-item.qtd-estoq + saldo-estoq.qtidade-atu.
    END.
 END.

 FOR EACH ob-etiqueta WHERE
          (ob-etiqueta.situacao = 3 OR ob-etiqueta.situacao = 4) NO-LOCK.

     RUN pi-acompanhar IN h-acomp (INPUT "Etiqueta: " + STRING(ob-etiqueta.nr-seq)).

     IF ob-etiqueta.nr-lote <> 'RP' AND
        ob-etiqueta.nr-lote <> 'RD' THEN NEXT.

     FIND tt-item WHERE
          tt-item.it-codigo = ob-etiqueta.it-codigo AND
          tt-item.cod-refer = ob-etiqueta.cod-refer AND
          tt-item.lote = ob-etiqueta.nr-lote
          NO-ERROR.
     IF NOT AVAIL tt-item THEN DO.
        CREATE tt-item.
        ASSIGN tt-item.it-codigo = ob-etiqueta.it-codigo
               tt-item.cod-refer = ob-etiqueta.cod-refer
               tt-item.lote = ob-etiqueta.nr-lote.
     END.
     ASSIGN tt-item.qtd-etiq = tt-item.qtd-etiq + ob-etiqueta.quantidade.
END.
 
RUN pi-finalizar in h-acomp.

OUTPUT TO c:\temp\dif.txt.

FOR EACH tt-item WHERE 
         tt-item.qtd-estoq <> tt-item.qtd-etiq NO-LOCK.

    IF ABS(tt-item.qtd-estoq - tt-item.qtd-etiq) <= 10 THEN DO.
       DISP tt-item.

    END.
END.
OUTPUT CLOSE.
