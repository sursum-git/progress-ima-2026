/* Buscna DEVOLU€åES do Representante */
DEFINE TEMP-TABLE tt-work  NO-UNDO 
       FIELD cod-estabel   LIKE ems2ima.nota-fiscal.cod-estabel
       FIELD base          AS INT
       FIELD cod-rep       LIKE ems2ima.nota-fiscal.cod-rep
       FIELD vlr-fat       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD devolucao     AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD liquidez      AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD fat-liq       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD comissao      AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD comis-liq     AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD desconto      AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD desc-base-ir  AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD i-renda       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD adiant        AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD liquido       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD marcar        AS CHAR.

DEFINE TEMP-TABLE tt-calc-repres 
       FIELD cod-rep LIKE ems2ima.repres.cod-rep
       FIELD cod-pai LIKE ems2ima.repres.cod-rep.

DEFINE TEMP-TABLE tt-nfs   LIKE ems2ima.nota-fiscal
       FIELD qt-faturada   AS   DECIMAL
       FIELD comissao      AS   DECIMAL
       FIELD base          AS   INT
       INDEX indice1 IS PRIMARY cod-rep cod-estabel serie nr-nota-fis.

DEFINE TEMP-TABLE tt-digita
       FIELD opcao AS CHAR
       FIELD campo AS CHAR
       FIELD valor AS CHAR.

DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-work.
DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-calc-repres.
DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-nfs.
DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-digita.

DEF INPUT PARAMETER p-cod-estab-ini AS CHAR. 
DEF INPUT PARAMETER p-cod-estab-fin AS CHAR. 
DEF INPUT PARAMETER p-dt-periodo-ini AS DATE.
DEF INPUT PARAMETER p-dt-periodo-fin AS DATE.
DEF INPUT PARAMETER p-no-ab-reppri-ini AS CHAR. 
DEF INPUT PARAMETER p-no-ab-reppri-fin AS CHAR. 
DEF INPUT PARAMETER p-it-codigo-ini AS CHAR. 
DEF INPUT PARAMETER p-it-codigo-fin AS CHAR. 

DEF VAR c-classe         AS CHAR INIT "Gerente Geral,Gerente Loja,Pracista,Interno,Externo".
DEF VAR de-desconto      LIKE ems2ima.it-nota-fisc.val-desconto-total.
DEF VAR de-desc-12       AS DEC.
DEF VAR h-acomp          AS HANDLE NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

{utp/ut-liter.i Gerando_Relat¢rio *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
    
/* Processao Dados da IMA */
FOR EACH ems2ima.docum-est WHERE
         ems2ima.docum-est.cod-estab >= p-cod-estab-ini AND
         ems2ima.docum-est.cod-estab <= p-cod-estab-fin AND
         ems2ima.docum-est.dt-trans >= p-dt-periodo-ini AND
         ems2ima.docum-est.dt-trans <= p-dt-periodo-fin NO-LOCK.

    FIND ems2ima.natur-oper WHERE
         ems2ima.natur-oper.nat-operacao = ems2ima.docum-est.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL ems2ima.natur-oper THEN NEXT.
    IF ems2ima.natur-oper.tipo-compra <> 3 THEN NEXT. /* Devolu‡Æo de Cliente */

    FOR EACH ems2ima.item-doc-est OF ems2ima.docum-est NO-LOCK.

        RUN pi-acompanhar IN h-acomp (INPUT "Data: "    + STRING(ems2ima.docum-est.dt-trans) +
                                            " Nota Fiscal: " + ems2ima.item-doc-est.nro-docto).

        IF ems2ima.item-doc-est.it-codigo < p-it-codigo-ini OR
           ems2ima.item-doc-est.it-codigo > p-it-codigo-fin THEN NEXT.
        RUN pi-ver-digita (INPUT "Produto",
                           INPUT ems2ima.item-doc-est.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND ems2ima.nota-fiscal WHERE
             ems2ima.nota-fiscal.cod-estabel  = ems2ima.docum-est.cod-estabel   AND
             ems2ima.nota-fiscal.serie        = ems2ima.item-doc-est.serie-comp AND
             ems2ima.nota-fiscal.nr-nota-fis  = ems2ima.item-doc-est.nro-comp   NO-LOCK NO-ERROR.

        FIND FIRST tt-calc-repres WHERE
                   tt-calc-repres.cod-rep = ems2ima.nota-fiscal.cod-rep NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-calc-repres THEN NEXT.

        ASSIGN de-desc-12 = 0.
        FOR EACH ems2ima.it-nota-fisc OF ems2ima.nota-fiscal WHERE
                 ems2ima.it-nota-fisc.nr-seq-fat = ems2ima.item-doc-est.seq-comp NO-LOCK.

            ASSIGN de-desconto = IF DEC(SUBSTR(ems2ima.it-nota-fisc.char-2,1500,10)) = 0 
                                 THEN ems2ima.it-nota-fisc.val-desconto-total 
                                 ELSE DEC(SUBSTR(ems2ima.it-nota-fisc.char-2,1500,10)).

            ASSIGN de-desc-12 = ((de-desconto / ems2ima.it-nota-fisc.qt-faturada[1]) * ems2ima.item-doc-est.quantidade).
        END.

        ACCUMULATE ems2ima.item-doc-est.preco-total[1] (TOTAL).
        ACCUMULATE de-desc-12 (TOTAL).
    END.
    IF (ACCUM TOTAL ems2ima.item-doc-est.preco-total[1]) = 0 THEN NEXT.

    FOR EACH tt-calc-repres WHERE
             tt-calc-repres.cod-rep = ems2ima.nota-fiscal.cod-rep NO-LOCK.

        FIND tt-work WHERE
             tt-work.cod-estabel = ems2ima.docum-est.cod-estabel AND
             tt-work.cod-rep     = tt-calc-repres.cod-pai AND 
             tt-work.base        = 10 NO-ERROR.
        IF NOT AVAIL tt-work THEN DO:
           CREATE tt-work.
           ASSIGN tt-work.cod-estabel = ems2ima.docum-est.cod-estabel
                  tt-work.cod-rep     = tt-calc-repres.cod-pai
                  tt-work.base        = 10.
        END.
        ASSIGN tt-work.devolucao = tt-work.devolucao + (ACCUM TOTAL ems2ima.item-doc-est.preco-total[1]).

        FIND tt-nfs OF ems2ima.nota-fiscal WHERE 
             tt-nfs.base = 10 AND
             tt-nfs.esp-docto = 20 NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-nfs THEN DO.
           CREATE tt-nfs.
           BUFFER-COPY ems2ima.nota-fiscal TO tt-nfs
               ASSIGN tt-nfs.base = 10
                      tt-nfs.esp-docto = 20
                      tt-nfs.vl-tot-nota = ems2ima.item-doc-est.preco-total[1].
        END.
        ELSE
           ASSIGN tt-nfs.vl-tot-nota = tt-nfs.vl-tot-nota + ems2ima.item-doc-est.preco-total[1].

        IF (ACCUM TOTAL de-desc-12) > 0 THEN DO.
           FIND tt-work WHERE
                tt-work.cod-estabel = ems2ima.docum-est.cod-estabel AND
                tt-work.cod-rep     = tt-calc-repres.cod-pai AND 
                tt-work.base        = 12 NO-ERROR.
           IF NOT AVAIL tt-work THEN DO:
              CREATE tt-work.
              ASSIGN tt-work.cod-estabel = ems2ima.docum-est.cod-estabel
                     tt-work.cod-rep     = tt-calc-repres.cod-pai
                     tt-work.base        = 12.
           END.
           ASSIGN tt-work.devolucao = tt-work.devolucao + (ACCUM TOTAL de-desc-12).

           FIND tt-nfs OF ems2ima.nota-fiscal WHERE 
                tt-nfs.base = 12 AND
                tt-nfs.esp-docto = 20 NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-nfs THEN DO.
              CREATE tt-nfs.
              BUFFER-COPY ems2ima.nota-fiscal TO tt-nfs
                   ASSIGN tt-nfs.base = 12
                          tt-nfs.esp-docto = 20
                          tt-nfs.vl-tot-nota = de-desc-12.
           END.
           ELSE
              ASSIGN tt-nfs.vl-tot-nota = tt-nfs.vl-tot-nota + de-desc-12.
        END.
    END.
END.    


/* Processao Dados da MED */
FOR EACH dbaux.docum-est WHERE
         dbaux.docum-est.cod-estab >= p-cod-estab-ini AND
         dbaux.docum-est.cod-estab <= p-cod-estab-fin AND
         dbaux.docum-est.dt-trans >= p-dt-periodo-ini AND
         dbaux.docum-est.dt-trans <= p-dt-periodo-fin NO-LOCK.

    FIND dbaux.natur-oper WHERE
         dbaux.natur-oper.nat-operacao = dbaux.docum-est.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL dbaux.natur-oper THEN NEXT.
    IF dbaux.natur-oper.tipo-compra <> 3 THEN NEXT. /* Devolu‡Æo de Cliente */

    FOR EACH dbaux.item-doc-est OF dbaux.docum-est NO-LOCK.

        RUN pi-acompanhar IN h-acomp (INPUT "Data: "    + STRING(dbaux.docum-est.dt-trans) +
                                            " Nota Fiscal: " + dbaux.item-doc-est.nro-docto).

        IF dbaux.item-doc-est.it-codigo < p-it-codigo-ini OR
           dbaux.item-doc-est.it-codigo > p-it-codigo-fin THEN NEXT.
        RUN pi-ver-digita (INPUT "Produto",
                           INPUT dbaux.item-doc-est.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND dbaux.nota-fiscal WHERE
             dbaux.nota-fiscal.cod-estabel  = dbaux.docum-est.cod-estabel   AND
             dbaux.nota-fiscal.serie        = dbaux.item-doc-est.serie-comp AND
             dbaux.nota-fiscal.nr-nota-fis  = dbaux.item-doc-est.nro-comp   NO-LOCK NO-ERROR.

        FIND FIRST tt-calc-repres WHERE
                   tt-calc-repres.cod-rep = dbaux.nota-fiscal.cod-rep NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-calc-repres THEN NEXT.

        FIND ems2ima.repres WHERE
             ems2ima.repres.cod-rep = tt-calc-repres.cod-pai NO-LOCK NO-ERROR.
        IF NOT AVAIL repres THEN NEXT.
       
        FIND tt-work WHERE
             tt-work.cod-estabel = dbaux.docum-est.cod-estabel AND
             tt-work.cod-rep     = ems2ima.repres.cod-rep AND 
             tt-work.base        = 10 NO-ERROR.
        IF NOT AVAIL tt-work THEN DO:
           CREATE tt-work.
           ASSIGN tt-work.cod-estabel = dbaux.docum-est.cod-estabel
                  tt-work.cod-rep     = ems2ima.repres.cod-rep
                  tt-work.base        = 10.
        END.
        ASSIGN tt-work.devolucao = tt-work.devolucao + dbaux.item-doc-est.preco-total[1].

        FIND tt-nfs OF dbaux.nota-fiscal WHERE 
             tt-nfs.base = 10 AND
             tt-nfs.esp-docto = 20 NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-nfs THEN DO.
           CREATE tt-nfs.
           BUFFER-COPY dbaux.nota-fiscal TO tt-nfs
                ASSIGN tt-nfs.base = 10
                       tt-nfs.esp-docto = 20
                       tt-nfs.vl-tot-nota = dbaux.item-doc-est.preco-total[1].
        END.
        ELSE
           ASSIGN tt-nfs.vl-tot-nota = tt-nfs.vl-tot-nota + dbaux.item-doc-est.preco-total[1].

        ASSIGN de-desc-12 = 0.
        FOR EACH dbaux.it-nota-fisc OF dbaux.nota-fiscal WHERE
                 dbaux.it-nota-fisc.nr-seq-fat = dbaux.item-doc-est.seq-comp NO-LOCK.

            ASSIGN de-desconto = IF DEC(SUBSTR(dbaux.it-nota-fisc.char-2,1500,10)) = 0 
                                 THEN dbaux.it-nota-fisc.val-desconto-total 
                                 ELSE DEC(SUBSTR(dbaux.it-nota-fisc.char-2,1500,10)).

            ASSIGN de-desc-12 = de-desc-12 + ((de-desconto / 
                                               dbaux.it-nota-fisc.qt-faturada[1]) *
                                               dbaux.item-doc-est.quantidade).
        END.
        IF de-desc-12 > 0 THEN DO.
           FIND tt-work WHERE
                tt-work.cod-estabel = dbaux.docum-est.cod-estabel AND
                tt-work.cod-rep     = ems2ima.repres.cod-rep AND 
                tt-work.base        = 12 NO-ERROR.
           IF NOT AVAIL tt-work THEN DO:
              CREATE tt-work.
              ASSIGN tt-work.cod-estabel = dbaux.docum-est.cod-estabel
                     tt-work.cod-rep     = ems2ima.repres.cod-rep
                     tt-work.base        = 12.
           END.
           ASSIGN tt-work.devolucao = tt-work.devolucao + de-desc-12.

           FIND tt-nfs OF dbaux.nota-fiscal WHERE 
                tt-nfs.base = 12 AND
                tt-nfs.esp-docto = 20 NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-nfs THEN DO.
              CREATE tt-nfs.
              BUFFER-COPY dbaux.nota-fiscal TO tt-nfs
                   ASSIGN tt-nfs.base = 12
                          tt-nfs.esp-docto = 20
                          tt-nfs.vl-tot-nota = de-desc-12.
           END.
           ELSE
              ASSIGN tt-nfs.vl-tot-nota = tt-nfs.vl-tot-nota + de-desc-12.
        END.
    END.
END.    
RUN pi-finalizar in h-acomp.


PROCEDURE pi-ver-digita.
    DEF INPUT PARAMETER p-campo AS CHAR.
    DEF INPUT PARAMETER p-valor AS CHAR.

    IF CAN-FIND(FIRST tt-digita WHERE
                      tt-digita.opcao = 'D'      AND
                      tt-digita.campo = p-campo) AND
       NOT CAN-FIND(FIRST tt-digita WHERE
                          tt-digita.opcao = 'D'      AND
                          tt-digita.campo = p-campo  AND
                          tt-digita.valor = p-valor) THEN RETURN 'ADM-ERROR'.
    ELSE
       IF CAN-FIND(FIRST tt-digita WHERE
                         tt-digita.opcao = 'E' AND
                         tt-digita.campo = p-campo AND
                         tt-digita.valor = p-valor) THEN RETURN 'ADM-ERROR'.
       ELSE
          RETURN 'OK'.

END PROCEDURE.

