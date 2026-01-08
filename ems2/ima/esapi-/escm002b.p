/* Buscna DEVOLU€åES do Representante */
DEFINE TEMP-TABLE tt-work  NO-UNDO 
       FIELD cod-estabel   LIKE movadm.nota-fiscal.cod-estabel
       FIELD base          AS INT
       FIELD cod-rep       LIKE movadm.nota-fiscal.cod-rep
       FIELD vlr-fat       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD devolucao     AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD liquidez      AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD fat-liq       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD comissao      AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD comis-dev     AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD comis-liq     AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD desconto      AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD desc-base-ir  AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD i-renda       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD adiantamento  AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD emprestimo    AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD liquido       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD vlr-nf        AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD marcar        AS CHAR.

DEFINE TEMP-TABLE tt-calc-repres 
       FIELD cod-rep LIKE repres.cod-rep
       FIELD cod-pai LIKE repres.cod-rep.

DEFINE TEMP-TABLE tt-nfs   LIKE movadm.nota-fiscal
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

DEF VAR c-classe            AS CHAR INIT "Gerente Geral,Gerente Loja,Pracista,Interno,Externo".
DEF VAR de-vlr-tot-nota     AS DEC.
DEF VAR de-vlr-tot-desc     AS DEC.
DEF VAR de-desconto         LIKE movadm.it-nota-fisc.val-desconto-total.
DEF VAR de-desc-12          AS DEC.
DEF VAR de-perc-reduc-comis AS DEC.
DEF VAR h-acomp             AS HANDLE NO-UNDO.

DEF BUFFER natur-oper FOR movadm.natur-oper.
DEF BUFFER ITEM FOR movadm.item.
DEF BUFFER tab-finan FOR movadm.tab-finan.
DEF BUFFER preco-item FOR movadm.preco-item.


RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

{utp/ut-liter.i Gerando_Relat¢rio *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
    
/* Processao Dados da IMA */
FOR EACH movadm.docum-est WHERE
         movadm.docum-est.cod-estab >= p-cod-estab-ini AND
         movadm.docum-est.cod-estab <= p-cod-estab-fin AND
         movadm.docum-est.dt-trans >= p-dt-periodo-ini AND
         movadm.docum-est.dt-trans <= p-dt-periodo-fin NO-LOCK.

    FIND natur-oper WHERE
         natur-oper.nat-operacao = movadm.docum-est.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL natur-oper THEN NEXT.
    IF natur-oper.tipo-compra <> 3 THEN NEXT. /* Devolu‡Æo de Cliente */

    FOR EACH movadm.item-doc-est OF movadm.docum-est NO-LOCK.

        RUN pi-acompanhar IN h-acomp (INPUT "Data: "    + STRING(movadm.docum-est.dt-trans) +
                                            " Nota Fiscal: " + movadm.item-doc-est.nro-docto).

        IF movadm.item-doc-est.it-codigo < p-it-codigo-ini OR
           movadm.item-doc-est.it-codigo > p-it-codigo-fin THEN NEXT.
        RUN pi-ver-digita (INPUT "Produto",
                           INPUT movadm.item-doc-est.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND movadm.nota-fiscal WHERE
             movadm.nota-fiscal.cod-estabel  = movadm.docum-est.cod-estabel   AND
             movadm.nota-fiscal.serie        = movadm.item-doc-est.serie-comp AND
             movadm.nota-fiscal.nr-nota-fis  = movadm.item-doc-est.nro-comp   NO-LOCK NO-ERROR.

        FIND natur-oper WHERE
             natur-oper.nat-operacao = movadm.nota-fiscal.nat-operacao NO-LOCK NO-ERROR.

        IF NOT AVAIL natur-oper THEN NEXT.
        IF natur-oper.tipo = 1 THEN NEXT. /* Movimenta‡Æo de Entrada */
        IF natur-oper.cod-esp <> "DP" THEN NEXT. /* Somente NFs. que Gera Duplicata */
        IF natur-oper.emite-dup = NO THEN NEXT. /* Somente NFs. que Gera Duplicata */

        FIND movadm.ped-venda WHERE
             movadm.ped-venda.nome-abrev = movadm.nota-fiscal.nome-ab-cli AND
             movadm.ped-venda.nr-pedcli = movadm.nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.

        FIND movadm.ped-repre OF movadm.ped-venda WHERE 
             movadm.ped-repre.nome-ab-rep = movadm.ped-venda.no-ab-reppri NO-LOCK NO-ERROR.

        FIND ped-venda-ext WHERE
             ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND    
             ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

        ASSIGN de-perc-reduc-comis = 0.
        FIND tbs_preco WHERE
             tbs_preco.tb_preco_id = ped-venda-ext.tb_preco_id NO-LOCK NO-ERROR.
        IF AVAIL tbs_preco AND 
           tbs_preco.perc_reduc_com <> 0 THEN
           ASSIGN de-perc-reduc-comis = tbs_preco.perc_reduc_com.

        FIND FIRST tt-calc-repres WHERE
                   tt-calc-repres.cod-rep = movadm.nota-fiscal.cod-rep NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-calc-repres THEN NEXT.

        ASSIGN de-desc-12 = 0.
        FOR EACH movadm.it-nota-fisc OF movadm.nota-fiscal WHERE
                 movadm.it-nota-fisc.nr-seq-fat = movadm.item-doc-est.seq-comp NO-LOCK.

            ASSIGN de-desconto = IF DEC(SUBSTR(movadm.it-nota-fisc.char-2,1500,10)) = 0 
                                 THEN movadm.it-nota-fisc.val-desconto-total 
                                 ELSE DEC(SUBSTR(movadm.it-nota-fisc.char-2,1500,10)).

            ASSIGN de-desc-12 = ((de-desconto / movadm.it-nota-fisc.qt-faturada[1]) * movadm.item-doc-est.quantidade).
        END.

        ACCUMULATE movadm.item-doc-est.preco-total[1] (TOTAL).
        ACCUMULATE de-desc-12 (TOTAL).
    END.
    IF (ACCUM TOTAL movadm.item-doc-est.preco-total[1]) = 0 THEN NEXT.

    FOR EACH tt-calc-repres WHERE
             tt-calc-repres.cod-rep = movadm.nota-fiscal.cod-rep NO-LOCK.

        FIND repres WHERE
             repres.cod-rep = tt-calc-repres.cod-pai NO-LOCK NO-ERROR.

        FIND cm-ext-repres WHERE
             cm-ext-repres.cod-rep = repres.cod-rep NO-LOCK NO-ERROR.

        ASSIGN de-vlr-tot-nota = ACCUM TOTAL movadm.item-doc-est.preco-total[1]
               de-vlr-tot-desc = ACCUM TOTAL de-desc-12.

        FIND tt-work WHERE
             tt-work.cod-estabel = movadm.docum-est.cod-estabel AND
             tt-work.cod-rep     = tt-calc-repres.cod-pai AND 
             tt-work.base        = 10 NO-ERROR.
        IF NOT AVAIL tt-work THEN DO:
           CREATE tt-work.
           ASSIGN tt-work.cod-estabel = movadm.docum-est.cod-estabel
                  tt-work.cod-rep     = tt-calc-repres.cod-pai
                  tt-work.base        = 10.
        END.
        ASSIGN tt-work.devolucao = tt-work.devolucao + (ACCUM TOTAL movadm.item-doc-est.preco-total[1]).

        IF cm-ext-repres.tp-aplic = 2 THEN  // Faturamento Pr¢prio
           ASSIGN tt-work.comis-dev = tt-work.comis-dev + 
                                     IF AVAIL movadm.ped-repre
                                     THEN de-vlr-tot-nota * movadm.ped-repre.perc-comis / 100
                                     ELSE de-vlr-tot-nota * repres.comis-direta / 100.
        ELSE 
           ASSIGN tt-work.comis-dev = tt-work.comis-dev + 
                                     IF de-perc-reduc-comis <> 0
                                     THEN (de-vlr-tot-nota * (repres.comis-direta * (100 - de-perc-reduc-comis) / 100) / 100) 
                                     ELSE (de-vlr-tot-nota * repres.comis-direta / 100).

        FIND tt-nfs OF movadm.nota-fiscal WHERE 
             tt-nfs.base = 10 AND
             tt-nfs.esp-docto = 20 NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-nfs THEN DO.
           CREATE tt-nfs.
           BUFFER-COPY movadm.nota-fiscal TO tt-nfs
               ASSIGN tt-nfs.base = 10
                      tt-nfs.esp-docto = 20
                      tt-nfs.vl-tot-nota = (ACCUM TOTAL movadm.item-doc-est.preco-total[1]).
        END.
        ELSE
           ASSIGN tt-nfs.vl-tot-nota = tt-nfs.vl-tot-nota + (ACCUM TOTAL movadm.item-doc-est.preco-total[1]).


        IF de-vlr-tot-desc > 0 THEN DO.
           FIND tt-work WHERE
                tt-work.cod-estabel = movadm.docum-est.cod-estabel AND
                tt-work.cod-rep     = tt-calc-repres.cod-pai AND 
                tt-work.base        = 12 NO-ERROR.
           IF NOT AVAIL tt-work THEN DO:
              CREATE tt-work.
              ASSIGN tt-work.cod-estabel = movadm.docum-est.cod-estabel
                     tt-work.cod-rep     = tt-calc-repres.cod-pai
                     tt-work.base        = 12.
           END.
           ASSIGN tt-work.devolucao = tt-work.devolucao + (ACCUM TOTAL de-desc-12).


           IF cm-ext-repres.tp-aplic = 2 THEN  // Faturamento Pr¢prio
              ASSIGN tt-work.comis-dev = tt-work.comis-dev + 
                                        IF AVAIL movadm.ped-repre
                                        THEN (de-vlr-tot-desc * movadm.ped-repre.perc-comis / 100)
                                        ELSE (de-vlr-tot-desc * repres.comis-direta / 100).
           ELSE 
              ASSIGN tt-work.comis-dev = tt-work.comis-dev + 
                                        IF de-perc-reduc-comis <> 0
                                        THEN (de-vlr-tot-desc * (repres.comis-direta * (100 - de-perc-reduc-comis) / 100) / 100)
                                        ELSE (de-vlr-tot-desc * repres.comis-direta / 100).


           FIND tt-nfs OF movadm.nota-fiscal WHERE 
                tt-nfs.base = 12 AND
                tt-nfs.esp-docto = 20 NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-nfs THEN DO.
              CREATE tt-nfs.
              BUFFER-COPY movadm.nota-fiscal TO tt-nfs
                   ASSIGN tt-nfs.base = 12
                          tt-nfs.esp-docto = 20
                          tt-nfs.vl-tot-nota = de-desc-12.
           END.
           ELSE
              ASSIGN tt-nfs.vl-tot-nota = tt-nfs.vl-tot-nota + de-desc-12.
        END.
    END.
END.    

/*
/* Processao Dados da MED */
FOR EACH dbaux.docum-est WHERE
         dbaux.docum-est.cod-estab >= p-cod-estab-ini AND
         dbaux.docum-est.cod-estab <= p-cod-estab-fin AND
         dbaux.docum-est.dt-trans >= p-dt-periodo-ini AND
         dbaux.docum-est.dt-trans <= p-dt-periodo-fin NO-LOCK.

    FIND natur-oper WHERE
         natur-oper.nat-operacao = dbaux.docum-est.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL natur-oper THEN NEXT.
    IF natur-oper.tipo-compra <> 3 THEN NEXT. /* Devolu‡Æo de Cliente */

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

        FIND natur-oper WHERE
             natur-oper.nat-operacao = dbaux.nota-fiscal.nat-operacao NO-LOCK NO-ERROR.

        IF NOT AVAIL natur-oper THEN NEXT.
        IF natur-oper.tipo = 1 THEN NEXT. /* Movimenta‡Æo de Entrada */
        IF natur-oper.cod-esp <> "DP" THEN NEXT. /* Somente NFs. que Gera Duplicata */
        IF natur-oper.emite-dup = NO THEN NEXT. /* Somente NFs. que Gera Duplicata */

        FIND FIRST tt-calc-repres WHERE
                   tt-calc-repres.cod-rep = dbaux.nota-fiscal.cod-rep NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-calc-repres THEN NEXT.

        ASSIGN de-desc-12 = 0.
        FOR EACH dbaux.it-nota-fisc OF dbaux.nota-fiscal WHERE
                 dbaux.it-nota-fisc.nr-seq-fat = dbaux.item-doc-est.seq-comp NO-LOCK.

            ASSIGN de-desconto = IF DEC(SUBSTR(dbaux.it-nota-fisc.char-2,1500,10)) = 0 
                                 THEN dbaux.it-nota-fisc.val-desconto-total 
                                 ELSE DEC(SUBSTR(dbaux.it-nota-fisc.char-2,1500,10)).

            ASSIGN de-desc-12 = ((de-desconto / dbaux.it-nota-fisc.qt-faturada[1]) * dbaux.item-doc-est.quantidade).
        END.

        ACCUMULATE dbaux.item-doc-est.preco-total[1] (TOTAL).
        ACCUMULATE de-desc-12 (TOTAL).
    END.
    IF (ACCUM TOTAL dbaux.item-doc-est.preco-total[1]) = 0 THEN NEXT.

    FOR EACH tt-calc-repres WHERE
             tt-calc-repres.cod-rep = dbaux.nota-fiscal.cod-rep NO-LOCK.

        FIND tt-work WHERE
             tt-work.cod-estabel = dbaux.docum-est.cod-estabel AND
             tt-work.cod-rep     = tt-calc-repres.cod-pai AND 
             tt-work.base        = 10 NO-ERROR.
        IF NOT AVAIL tt-work THEN DO:
           CREATE tt-work.
           ASSIGN tt-work.cod-estabel = dbaux.docum-est.cod-estabel
                  tt-work.cod-rep     = tt-calc-repres.cod-pai
                  tt-work.base        = 10.
        END.
        ASSIGN tt-work.devolucao = tt-work.devolucao + (ACCUM TOTAL dbaux.item-doc-est.preco-total[1]).

        FIND tt-nfs OF dbaux.nota-fiscal WHERE 
             tt-nfs.base = 10 AND
             tt-nfs.esp-docto = 20 NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-nfs THEN DO.
           CREATE tt-nfs.
           BUFFER-COPY dbaux.nota-fiscal TO tt-nfs
               ASSIGN tt-nfs.base = 10
                      tt-nfs.esp-docto = 20
                      tt-nfs.vl-tot-nota = (ACCUM TOTAL dbaux.item-doc-est.preco-total[1]).
        END.
        ELSE
           ASSIGN tt-nfs.vl-tot-nota = tt-nfs.vl-tot-nota + (ACCUM TOTAL dbaux.item-doc-est.preco-total[1]).

        IF (ACCUM TOTAL de-desc-12) > 0 THEN DO.
           FIND tt-work WHERE
                tt-work.cod-estabel = dbaux.docum-est.cod-estabel AND
                tt-work.cod-rep     = tt-calc-repres.cod-pai AND 
                tt-work.base        = 12 NO-ERROR.
           IF NOT AVAIL tt-work THEN DO:
              CREATE tt-work.
              ASSIGN tt-work.cod-estabel = dbaux.docum-est.cod-estabel
                     tt-work.cod-rep     = tt-calc-repres.cod-pai
                     tt-work.base        = 12.
           END.
           ASSIGN tt-work.devolucao = tt-work.devolucao + (ACCUM TOTAL de-desc-12).

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
*/

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

