/* Buscna NOTAS FISCAIS do Representante */
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

DEFINE TEMP-TABLE tt-nfs   LIKE ems2ima.nota-fiscal
       FIELD qt-faturada   AS   DECIMAL
       FIELD comissao      AS   DECIMAL
       FIELD base          AS   INT
       INDEX indice1 IS PRIMARY cod-rep cod-estabel serie nr-nota-fis.

DEFINE TEMP-TABLE tt-digita
       FIELD opcao AS CHAR
       FIELD campo AS CHAR
       FIELD valor AS CHAR.

DEFINE TEMP-TABLE tt-calc-repres 
       FIELD cod-rep LIKE ems2ima.repres.cod-rep
       FIELD cod-pai LIKE ems2ima.repres.cod-rep.

DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-work.
DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-calc-repres.
DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-nfs.
DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-digita.

DEF INPUT PARAMETER p-cod-estab-ini AS CHAR. 
DEF INPUT PARAMETER p-cod-estab-fin AS CHAR. 
DEF INPUT PARAMETER p-dt-periodo-ini AS DATE.
DEF INPUT PARAMETER p-dt-periodo-fin AS DATE.
DEF INPUT PARAMETER p-nr-nota-fis-ini AS CHAR. 
DEF INPUT PARAMETER p-nr-nota-fis-fin AS CHAR. 
DEF INPUT PARAMETER p-no-ab-reppri-ini AS CHAR. 
DEF INPUT PARAMETER p-no-ab-reppri-fin AS CHAR. 
DEF INPUT PARAMETER p-cond-pagto-ini AS INT. 
DEF INPUT PARAMETER p-cond-pagto-fin AS INT. 
DEF INPUT PARAMETER p-it-codigo-ini AS CHAR. 
DEF INPUT PARAMETER p-it-codigo-fin AS CHAR. 

DEF BUFFER b-repres FOR ems2ima.repres.

DEF VAR c-classe         AS CHAR INIT "Gerente Geral,Gerente Loja,Pracista,Interno,Externo".
DEF VAR de-desconto      LIKE ems2ima.it-nota-fisc.val-desconto-total.
DEF VAR de-perc          AS DEC.
DEF VAR de-ind-finan     AS DEC.
DEF VAR de-preco-tab     AS DEC.
DEF VAR de-comissao      AS DEC.
DEF VAR de-comissao-12   AS DEC.

DEF VAR h-acomp          AS HANDLE NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

/* Processa Dados da IMA */
{utp/ut-liter.i Processando_IMA *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FOR EACH ems2ima.nota-fiscal WHERE
         ems2ima.nota-fiscal.cod-estabel  >= p-cod-estab-ini AND 
         ems2ima.nota-fiscal.cod-estabel  <= p-cod-estab-fin AND
         ems2ima.nota-fiscal.dt-emis-nota >= p-dt-periodo-ini AND 
         ems2ima.nota-fiscal.dt-emis-nota <= p-dt-periodo-fin AND
         ems2ima.nota-fiscal.nr-nota-fis  >= p-nr-nota-fis-ini AND
         ems2ima.nota-fiscal.nr-nota-fis  <= p-nr-nota-fis-fin AND 
         ems2ima.nota-fiscal.dt-cancela    = ? NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(ems2ima.nota-fiscal.dt-emis-nota) +
                                        " Nota Fiscal: " + ems2ima.nota-fiscal.nr-nota-fis).

    IF ems2ima.nota-fiscal.cod-cond-pag < p-cond-pagto-ini OR
       ems2ima.nota-fiscal.cod-cond-pag > p-cond-pagto-fin THEN NEXT.
    RUN pi-ver-digita (INPUT "Condi‡Æo Pagamento",
                       INPUT ems2ima.nota-fiscal.no-ab-reppri).
    IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    
    FIND ems2ima.natur-oper WHERE
         ems2ima.natur-oper.nat-operacao = ems2ima.nota-fiscal.nat-operacao NO-LOCK NO-ERROR.

    IF NOT AVAIL ems2ima.natur-oper THEN NEXT.
    IF ems2ima.natur-oper.tipo = 1 THEN NEXT. /* Movimenta‡Æo de Entrada */
    IF ems2ima.natur-oper.cod-esp <> "DP" THEN NEXT. /* Somente NFs. que Gera Duplicata */
    IF ems2ima.natur-oper.emite-dup = NO THEN NEXT. /* Somente NFs. que Gera Duplicata */

    FIND FIRST tt-calc-repres WHERE
               tt-calc-repres.cod-rep = ems2ima.nota-fiscal.cod-rep NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-calc-repres THEN NEXT.

    FIND ems2ima.repres WHERE
         ems2ima.repres.cod-rep = tt-calc-repres.cod-pai NO-LOCK NO-ERROR.

    ASSIGN de-comissao = 0
           de-comissao-12 = 0.
    FOR EACH ems2ima.it-nota-fisc OF ems2ima.nota-fiscal NO-LOCK.
        IF ems2ima.it-nota-fisc.it-codigo < p-it-codigo-ini OR 
           ems2ima.it-nota-fisc.it-codigo > p-it-codigo-fin  THEN NEXT.

        RUN pi-ver-digita (INPUT "Produto",
                           INPUT ems2ima.it-nota-fisc.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND ems2ima.item WHERE
             ems2ima.item.it-codigo = ems2ima.it-nota-fisc.it-codigo NO-LOCK  NO-ERROR.
        IF ems2ima.item.ge-codigo < 50 OR
           ems2ima.item.ge-codigo > 60 THEN NEXT.  /* somente tecidos */

        ASSIGN de-desconto = IF DEC(SUBSTR(ems2ima.it-nota-fisc.char-2,1500,10)) = 0 
                             THEN ems2ima.it-nota-fisc.val-desconto-total 
                             ELSE DEC(SUBSTR(ems2ima.it-nota-fisc.char-2,1500,10)).

        ACCUMULATE ems2ima.it-nota-fisc.vl-tot-item (TOTAL).
        ACCUMULATE de-desconto (TOTAL).

        ASSIGN de-perc = ems2ima.repres.comis-direta.

        FIND cm-exc-item WHERE
             cm-exc-item.cod-rep = ems2ima.nota-fiscal.cod-rep AND
             cm-exc-item.it-codigo = ems2ima.it-nota-fisc.it-codigo AND
             cm-exc-item.cod-estabel = ems2ima.it-nota-fisc.cod-estabel AND
             cm-exc-item.mes = MONTH(p-dt-periodo-ini) AND
             cm-exc-item.ano = YEAR(p-dt-periodo-ini)
             NO-LOCK NO-ERROR.
        
        IF AVAIL cm-exc-item THEN DO.
           ASSIGN de-ind-finan = 1
                  de-preco-tab = 0
                  de-perc = 0.
           IF ems2ima.nota-fiscal.nr-ind-fin > 0 THEN DO.
              FIND ems2ima.tab-finan OF ems2ima.nota-fiscal NO-LOCK NO-ERROR.
              ASSIGN de-ind-finan = ems2ima.tab-finan.tab-ind-fin[ems2ima.nota-fiscal.nr-ind-fin].
           END.

           FIND FIRST ems2ima.preco-item WHERE
                      ems2ima.preco-item.nr-tabpre = cm-exc-item.tab-preco AND
                      ems2ima.preco-item.it-codigo = ems2ima.it-nota-fisc.it-codigo 
                      NO-LOCK NO-ERROR.

           IF AVAIL ems2ima.preco-item THEN
              ASSIGN de-preco-tab = ems2ima.preco-item.preco-venda * de-ind-finan.

           IF de-preco-tab = 0 OR
              de-preco-tab = ems2ima.it-nota-fisc.vl-preuni THEN
              ASSIGN de-perc = ems2ima.repres.comis-direta.
           ELSE DO.
              IF ems2ima.it-nota-fisc.vl-preuni < de-preco-tab THEN
                 ASSIGN de-perc = cm-exc-item.perc-menor-tab.
              ELSE
                 ASSIGN de-perc = cm-exc-item.perc-maior-tab.
           END.
        END.
        
        FIND cm-exc-condpagto WHERE
             cm-exc-condpagto.cod-rep = ems2ima.nota-fiscal.cod-rep AND
             cm-exc-condpagto.cod-cond-pagto = ems2ima.nota-fiscal.cod-cond-pag AND
             cm-exc-condpagto.cod-estab = ems2ima.it-nota-fisc.cod-estab
             NO-LOCK NO-ERROR.

        IF AVAIL cm-exc-condpagto THEN DO.
           ASSIGN de-ind-finan = 1
                  de-preco-tab = 0
                  de-perc = 0.

           IF ems2ima.nota-fiscal.nr-ind-fin > 0 THEN DO.
              FIND ems2ima.tab-finan OF ems2ima.nota-fiscal NO-LOCK NO-ERROR.
              ASSIGN de-ind-finan = ems2ima.tab-finan.tab-ind-fin[ems2ima.nota-fiscal.nr-ind-fin].
           END.

           FIND FIRST ems2ima.preco-item WHERE
                      ems2ima.preco-item.nr-tabpre = cm-exc-condpagto.tab-preco AND
                      ems2ima.preco-item.it-codigo = ems2ima.it-nota-fisc.it-codigo 
                      NO-LOCK NO-ERROR.

           IF AVAIL ems2ima.preco-item THEN
              ASSIGN de-preco-tab = ems2ima.preco-item.preco-venda * de-ind-finan.

           IF de-preco-tab = 0 OR
              de-preco-tab = ems2ima.it-nota-fisc.vl-preuni THEN
              ASSIGN de-perc = ems2ima.repres.comis-direta.
           ELSE DO.
              IF ems2ima.it-nota-fisc.vl-preuni < de-preco-tab THEN
                 ASSIGN de-perc = cm-exc-condpagto.perc-menor-tab.
              ELSE
                 ASSIGN de-perc = cm-exc-condpagto.perc-maior-tab.
           END.
        END.

        ASSIGN de-comissao = de-comissao + (ems2ima.it-nota-fisc.vl-tot-item * de-perc / 100)
               de-comissao-12 = de-comissao-12 + (de-desconto * de-perc / 100).

    END.
    IF (ACCUM TOTAL ems2ima.it-nota-fisc.vl-tot-item) = 0 THEN NEXT.
    
    FOR EACH tt-calc-repres WHERE 
             tt-calc-repres.cod-rep = ems2ima.nota-fiscal.cod-rep NO-LOCK.

        FIND tt-work WHERE
             tt-work.cod-estabel = ems2ima.nota-fiscal.cod-estabel AND
             tt-work.cod-rep     = tt-calc-repres.cod-pai AND
             tt-work.base        = 10
             NO-ERROR.
        IF NOT AVAIL tt-work THEN DO:
           CREATE tt-work.
           ASSIGN tt-work.cod-estabel = ems2ima.nota-fiscal.cod-estabel
                  tt-work.cod-rep     = tt-calc-repres.cod-pai
                  tt-work.base        = 10.
        END.
        ASSIGN tt-work.vlr-fat = tt-work.vlr-fat +  ACCUM TOTAL ems2ima.it-nota-fisc.vl-tot-item 
               tt-work.comissao = tt-work.comissao + de-comissao.
        
        CREATE tt-nfs.
        ASSIGN tt-nfs.cod-estabel = ems2ima.nota-fiscal.cod-estabel
               tt-nfs.serie = ems2ima.nota-fiscal.serie
               tt-nfs.nr-nota-fis = ems2ima.nota-fiscal.nr-nota-fis
               tt-nfs.base = 10
               tt-nfs.cod-rep = tt-calc-repres.cod-pai
               tt-nfs.comissao = tt-nfs.comissao + de-comissao
               tt-nfs.esp-docto = 22
               tt-nfs.vl-tot-nota = (ACCUM TOTAL ems2ima.it-nota-fisc.vl-tot-item).
               
        IF (ACCUM TOTAL de-desconto) > 0 THEN DO.
           FIND tt-work WHERE
                tt-work.cod-estabel = ems2ima.nota-fiscal.cod-estabel AND
                tt-work.cod-rep     = tt-calc-repres.cod-pai AND
                tt-work.base        = 12
                NO-ERROR.
           IF NOT AVAIL tt-work THEN DO:
              CREATE tt-work.
              ASSIGN tt-work.cod-estabel = ems2ima.nota-fiscal.cod-estabel
                     tt-work.cod-rep     = tt-calc-repres.cod-pai
                     tt-work.base        = 12.
           END.
           ASSIGN tt-work.vlr-fat  = tt-work.vlr-fat +  ACCUM TOTAL de-desconto
                  tt-work.comissao = tt-work.comissao + de-comissao-12.
    
           CREATE tt-nfs.
           ASSIGN tt-nfs.cod-estabel = ems2ima.nota-fiscal.cod-estabel
                  tt-nfs.serie = ems2ima.nota-fiscal.serie
                  tt-nfs.nr-nota-fis = ems2ima.nota-fiscal.nr-nota-fis
                  tt-nfs.base = 12
                  tt-nfs.cod-rep = tt-calc-repres.cod-pai
                  tt-nfs.esp-docto = 22
                  tt-nfs.comissao = tt-nfs.comissao + de-comissao-12
                  tt-nfs.vl-tot-nota = ACCUM TOTAL de-desconto.
        END.
    END.
END.


/* Processa Dados da MED */
{utp/ut-liter.i Processando_MED *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FOR EACH dbaux.nota-fiscal WHERE
         dbaux.nota-fiscal.cod-estabel  >= p-cod-estab-ini AND 
         dbaux.nota-fiscal.cod-estabel  <= p-cod-estab-fin AND
         dbaux.nota-fiscal.dt-emis-nota >= p-dt-periodo-ini AND 
         dbaux.nota-fiscal.dt-emis-nota <= p-dt-periodo-fin AND
         dbaux.nota-fiscal.nr-nota-fis  >= p-nr-nota-fis-ini AND
         dbaux.nota-fiscal.nr-nota-fis  <= p-nr-nota-fis-fin AND 
         dbaux.nota-fiscal.dt-cancela    = ? NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(dbaux.nota-fiscal.dt-emis-nota) +
                                        " Nota Fiscal: " + dbaux.nota-fiscal.nr-nota-fis).
    
    IF dbaux.nota-fiscal.cod-cond-pag < p-cond-pagto-ini OR
       dbaux.nota-fiscal.cod-cond-pag > p-cond-pagto-fin THEN NEXT.
    RUN pi-ver-digita (INPUT "Condi‡Æo Pagamento",
                       INPUT dbaux.nota-fiscal.no-ab-reppri).
    IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    
    FIND dbaux.natur-oper WHERE
         dbaux.natur-oper.nat-operacao = dbaux.nota-fiscal.nat-operacao NO-LOCK NO-ERROR.

    IF NOT AVAIL dbaux.natur-oper THEN NEXT.
    IF dbaux.natur-oper.tipo = 1 THEN NEXT. /* Movimenta‡Æo de Entrada */
    IF dbaux.natur-oper.cod-esp <> "DP" THEN NEXT. /* Somente NFs. que Gera Duplicata */
    IF dbaux.natur-oper.emite-dup = NO THEN NEXT. /* Somente NFs. que Gera Duplicata */

    FIND tt-calc-repres WHERE
         tt-calc-repres.cod-rep = dbaux.nota-fiscal.cod-rep NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-calc-repres THEN NEXT.
    
    FIND ems2ima.repres WHERE
         ems2ima.repres.cod-rep = tt-calc-repres.cod-pai NO-LOCK NO-ERROR.

    ASSIGN de-comissao = 0
           de-comissao-12 = 0.
    FOR EACH dbaux.it-nota-fisc OF dbaux.nota-fiscal NO-LOCK.
        IF dbaux.it-nota-fisc.it-codigo < p-it-codigo-ini OR 
           dbaux.it-nota-fisc.it-codigo > p-it-codigo-fin  THEN NEXT.

        RUN pi-ver-digita (INPUT "Produto",
                           INPUT dbaux.it-nota-fisc.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.


        FIND dbaux.item WHERE
             dbaux.item.it-codigo = dbaux.it-nota-fisc.it-codigo NO-LOCK  NO-ERROR.
        IF dbaux.item.ge-codigo < 50 OR
           dbaux.item.ge-codigo > 60 THEN NEXT.  /* somente tecidos */

        ASSIGN de-desconto = IF DEC(SUBSTR(dbaux.it-nota-fisc.char-2,1500,10)) = 0 
                             THEN dbaux.it-nota-fisc.val-desconto-total 
                             ELSE DEC(SUBSTR(dbaux.it-nota-fisc.char-2,1500,10)).

        ACCUMULATE dbaux.it-nota-fisc.vl-tot-item (TOTAL).
        ACCUMULATE de-desconto (TOTAL).

        ASSIGN de-perc = ems2ima.repres.comis-direta.

        FIND cm-exc-item WHERE
             cm-exc-item.cod-rep = dbaux.nota-fiscal.cod-rep AND
             cm-exc-item.it-codigo = dbaux.it-nota-fisc.it-codigo AND
             cm-exc-item.cod-estabel = dbaux.it-nota-fisc.cod-estabel
             NO-LOCK NO-ERROR.
        
        IF AVAIL cm-exc-item THEN DO.
           ASSIGN de-ind-finan = 1
                  de-preco-tab = 0
                  de-perc = 0.
           IF dbaux.nota-fiscal.nr-ind-fin > 0 THEN DO.
              FIND dbaux.tab-finan OF dbaux.nota-fiscal NO-LOCK NO-ERROR.
              ASSIGN de-ind-finan = dbaux.tab-finan.tab-ind-fin[dbaux.nota-fiscal.nr-ind-fin].
           END.

           FIND FIRST dbaux.preco-item WHERE
                      dbaux.preco-item.nr-tabpre = cm-exc-item.tab-preco AND
                      dbaux.preco-item.it-codigo = dbaux.it-nota-fisc.it-codigo 
                      NO-LOCK NO-ERROR.

           IF AVAIL dbaux.preco-item THEN
              ASSIGN de-preco-tab = dbaux.preco-item.preco-venda * de-ind-finan.

           IF de-preco-tab = 0 OR
              de-preco-tab = dbaux.it-nota-fisc.vl-preuni THEN
              ASSIGN de-perc = ems2ima.repres.comis-direta.
           ELSE DO.
              IF dbaux.it-nota-fisc.vl-preuni < de-preco-tab THEN
                 ASSIGN de-perc = cm-exc-item.perc-menor-tab.
              ELSE
                 ASSIGN de-perc = cm-exc-item.perc-maior-tab.
           END.
        END.
        
        FIND cm-exc-condpagto WHERE
             cm-exc-condpagto.cod-rep = dbaux.nota-fiscal.cod-rep AND
             cm-exc-condpagto.cod-cond-pagto = dbaux.nota-fiscal.cod-cond-pag AND
             cm-exc-condpagto.cod-estab = dbaux.it-nota-fisc.cod-estab
             NO-LOCK NO-ERROR.

        IF AVAIL cm-exc-condpagto THEN DO.
           ASSIGN de-ind-finan = 1
                  de-preco-tab = 0
                  de-perc = 0.

           IF dbaux.nota-fiscal.nr-ind-fin > 0 THEN DO.
              FIND dbaux.tab-finan OF dbaux.nota-fiscal NO-LOCK NO-ERROR.
              ASSIGN de-ind-finan = dbaux.tab-finan.tab-ind-fin[dbaux.nota-fiscal.nr-ind-fin].
           END.

           FIND FIRST dbaux.preco-item WHERE
                      dbaux.preco-item.nr-tabpre = cm-exc-condpagto.tab-preco AND
                      dbaux.preco-item.it-codigo = dbaux.it-nota-fisc.it-codigo 
                      NO-LOCK NO-ERROR.

           IF AVAIL dbaux.preco-item THEN
              ASSIGN de-preco-tab = dbaux.preco-item.preco-venda * de-ind-finan.

           IF de-preco-tab = 0 OR
              de-preco-tab = dbaux.it-nota-fisc.vl-preuni THEN
              ASSIGN de-perc = ems2ima.repres.comis-direta.
           ELSE DO.
              IF dbaux.it-nota-fisc.vl-preuni < de-preco-tab THEN
                 ASSIGN de-perc = cm-exc-condpagto.perc-menor-tab.
              ELSE
                 ASSIGN de-perc = cm-exc-condpagto.perc-maior-tab.
           END.
        END.

        ASSIGN de-comissao = de-comissao + (dbaux.it-nota-fisc.vl-tot-item * de-perc / 100)
               de-comissao-12 = de-comissao-12 + (de-desconto * de-perc / 100).
    END.
    IF (ACCUM TOTAL dbaux.it-nota-fisc.vl-tot-item) = 0 THEN NEXT.
    
    FOR EACH tt-calc-repres WHERE 
             tt-calc-repres.cod-rep = dbaux.nota-fiscal.cod-rep NO-LOCK.
        FIND tt-work WHERE
             tt-work.cod-estabel = dbaux.nota-fiscal.cod-estabel AND
             tt-work.cod-rep     = tt-calc-repres.cod-pai AND
             tt-work.base        = 10
             NO-ERROR.
        IF NOT AVAIL tt-work THEN DO:
           CREATE tt-work.
           ASSIGN tt-work.cod-estabel = dbaux.nota-fiscal.cod-estabel
                  tt-work.cod-rep     = tt-calc-repres.cod-pai
                  tt-work.base        = 10.
        END.
        ASSIGN tt-work.vlr-fat = tt-work.vlr-fat +  (ACCUM TOTAL dbaux.it-nota-fisc.vl-tot-item) 
               tt-work.comissao = tt-work.comissao + de-comissao.
        
        CREATE tt-nfs.
        ASSIGN tt-nfs.cod-estabel = dbaux.nota-fiscal.cod-estabel
               tt-nfs.serie = dbaux.nota-fiscal.serie
               tt-nfs.nr-nota-fis = dbaux.nota-fiscal.nr-nota-fis
               tt-nfs.base = 10
               tt-nfs.comissao = tt-nfs.comissao + de-comissao
               tt-nfs.esp-docto = 22
               tt-nfs.vl-tot-nota = (ACCUM TOTAL dbaux.it-nota-fisc.vl-tot-item).
               
        IF (ACCUM TOTAL de-desconto) > 0 THEN DO.
           FIND tt-work WHERE
                tt-work.cod-estabel = dbaux.nota-fiscal.cod-estabel AND
                tt-work.cod-rep     = tt-calc-repres.cod-pai AND
                tt-work.base        = 12
                NO-ERROR.
           IF NOT AVAIL tt-work THEN DO:
              CREATE tt-work.
              ASSIGN tt-work.cod-estabel = dbaux.nota-fiscal.cod-estabel
                     tt-work.cod-rep     = tt-calc-repres.cod-pai
                     tt-work.base        = 12.
           END.
           ASSIGN tt-work.vlr-fat  = tt-work.vlr-fat +  ACCUM TOTAL de-desconto
                  tt-work.comissao = tt-work.comissao + de-comissao-12.
    
           CREATE tt-nfs.
           ASSIGN tt-nfs.cod-estabel = dbaux.nota-fiscal.cod-estabel
                  tt-nfs.serie = dbaux.nota-fiscal.serie
                  tt-nfs.nr-nota-fis = dbaux.nota-fiscal.nr-nota-fis
                  tt-nfs.base = 12
                  tt-nfs.esp-docto = 22
                  tt-nfs.comissao = tt-nfs.comissao + de-comissao-12
                  tt-nfs.vl-tot-nota = ACCUM TOTAL de-desconto.
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

