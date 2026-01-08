/* Busca NOTAS FISCAIS do Representante */
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

DEFINE TEMP-TABLE tt-nfs   LIKE movadm.nota-fiscal
       FIELD qt-faturada   AS   DECIMAL
       FIELD comissao      AS   DECIMAL
       FIELD base          AS   INT
       INDEX indice1 IS PRIMARY cod-rep cod-estabel serie nr-nota-fis.

DEFINE TEMP-TABLE tt-calc-repres 
       FIELD cod-rep LIKE repres.cod-rep
       FIELD cod-pai LIKE repres.cod-rep.

DEFINE TEMP-TABLE tt-excessao
       FIELD perc-comis   LIKE repres.comis-direta
       FIELD vlr-fat      AS DECIMAL
       FIELD vlr-desconto AS DECIMAL.

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
DEF INPUT PARAMETER p-nr-nota-fis-ini AS CHAR. 
DEF INPUT PARAMETER p-nr-nota-fis-fin AS CHAR. 
DEF INPUT PARAMETER p-no-ab-reppri-ini AS CHAR. 
DEF INPUT PARAMETER p-no-ab-reppri-fin AS CHAR. 
DEF INPUT PARAMETER p-cond-pagto-ini AS INT. 
DEF INPUT PARAMETER p-cond-pagto-fin AS INT. 
DEF INPUT PARAMETER p-it-codigo-ini AS CHAR. 
DEF INPUT PARAMETER p-it-codigo-fin AS CHAR. 

DEF BUFFER b-repres FOR repres.

DEF VAR c-classe                AS CHAR INIT "Gerente Geral,Gerente Loja,Pracista,Interno,Externo".
DEF VAR de-desconto             LIKE movadm.it-nota-fisc.val-desconto-total.
DEF VAR de-perc-exc             AS DEC.
DEF VAR de-ind-finan            AS DEC.
DEF VAR de-perc-reduc-comis     AS DEC.
DEF VAR de-preco-tab            AS DEC.
DEF VAR de-vlr-tot-nota         AS DEC.
DEF VAR de-vlr-tot-desc         AS DEC.
DEF VAR de-vlr-nota-calc        AS DEC.  /* Valores para Calculo da ComissÆo */
DEF VAR de-vlr-desc-calc        AS DEC.  /* Valores para Calculo da ComissÆo Backup */

DEF VAR h-acomp          AS HANDLE NO-UNDO.

DEF BUFFER natur-oper FOR movadm.natur-oper.
DEF BUFFER ITEM FOR movadm.item.
DEF BUFFER tab-finan FOR movadm.tab-finan.
DEF BUFFER preco-item FOR movadm.preco-item.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

/* Processa Dados da IMA */
{utp/ut-liter.i Processando_IMA *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FOR EACH movadm.nota-fiscal WHERE
         movadm.nota-fiscal.cod-estabel  >= p-cod-estab-ini AND 
         movadm.nota-fiscal.cod-estabel  <= p-cod-estab-fin AND
         movadm.nota-fiscal.dt-emis-nota >= p-dt-periodo-ini AND 
         movadm.nota-fiscal.dt-emis-nota <= p-dt-periodo-fin AND
         movadm.nota-fiscal.nr-nota-fis  >= p-nr-nota-fis-ini AND
         movadm.nota-fiscal.nr-nota-fis  <= p-nr-nota-fis-fin AND 
         movadm.nota-fiscal.dt-cancela    = ? NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(movadm.nota-fiscal.dt-emis-nota) +
                                        " Nota Fiscal: " + movadm.nota-fiscal.nr-nota-fis).

    IF movadm.nota-fiscal.cod-cond-pag < p-cond-pagto-ini OR
       movadm.nota-fiscal.cod-cond-pag > p-cond-pagto-fin THEN NEXT.
    RUN pi-ver-digita (INPUT "Condi‡Æo Pagamento",
                       INPUT movadm.nota-fiscal.no-ab-reppri).
    IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    
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

    FOR EACH movadm.it-nota-fisc OF movadm.nota-fiscal NO-LOCK.
        IF movadm.it-nota-fisc.it-codigo < p-it-codigo-ini OR 
           movadm.it-nota-fisc.it-codigo > p-it-codigo-fin  THEN NEXT.

        RUN pi-ver-digita (INPUT "Produto",
                           INPUT movadm.it-nota-fisc.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND item WHERE
             item.it-codigo = movadm.it-nota-fisc.it-codigo NO-LOCK  NO-ERROR.
        IF item.ge-codigo < 50 OR
           item.ge-codigo > 60 THEN NEXT.  /* somente tecidos */

        ASSIGN de-desconto = IF DEC(SUBSTR(movadm.it-nota-fisc.char-2,1500,10)) = 0 
                             THEN movadm.it-nota-fisc.val-desconto-total 
                             ELSE DEC(SUBSTR(movadm.it-nota-fisc.char-2,1500,10)).


        ACCUMULATE movadm.it-nota-fisc.vl-tot-item (TOTAL).
        ACCUMULATE de-desconto (TOTAL).

        ASSIGN de-perc-exc = 0.
        FIND cm-exc-item WHERE
             cm-exc-item.cod-rep = movadm.nota-fiscal.cod-rep AND
             cm-exc-item.it-codigo = movadm.it-nota-fisc.it-codigo AND
             cm-exc-item.cod-estabel = movadm.it-nota-fisc.cod-estabel AND
             cm-exc-item.mes = MONTH(p-dt-periodo-ini) AND
             cm-exc-item.ano = YEAR(p-dt-periodo-ini)
             NO-LOCK NO-ERROR.
        
        IF AVAIL cm-exc-item THEN DO.
           ASSIGN de-ind-finan = 1
                  de-preco-tab = 0.

           IF movadm.nota-fiscal.nr-ind-fin > 0 THEN DO.
              FIND tab-finan OF movadm.nota-fiscal NO-LOCK NO-ERROR.
              //ASSIGN de-ind-finan = tab-finan.tab-ind-fin[movadm.nota-fiscal.nr-ind-fin].

              FIND FIRST tab-finan-indice OF tab-finan WHERE 
                         tab-finan-indice.num-seq = movadm.nota-fiscal.nr-ind-fin NO-LOCK NO-ERROR.
              IF AVAIL tab-finan-indice THEN
                 ASSIGN de-ind-finan = tab-finan-indice.tab-ind-fin.
           END.

           FIND FIRST preco-item WHERE
                      preco-item.nr-tabpre = cm-exc-item.tab-preco AND
                      preco-item.it-codigo = movadm.it-nota-fisc.it-codigo 
                      NO-LOCK NO-ERROR.

           IF AVAIL preco-item THEN
              ASSIGN de-preco-tab = preco-item.preco-venda * de-ind-finan.

           IF de-preco-tab <> 0 AND
              de-preco-tab <> movadm.it-nota-fisc.vl-preuni THEN DO.
              IF movadm.it-nota-fisc.vl-preuni < de-preco-tab THEN
                 ASSIGN de-perc-exc = cm-exc-item.perc-menor-tab.
              ELSE
                 ASSIGN de-perc-exc = cm-exc-item.perc-maior-tab.
           END.
        END.
        
        FIND cm-exc-condpagto WHERE
             cm-exc-condpagto.cod-rep = movadm.nota-fiscal.cod-rep AND
             cm-exc-condpagto.cod-cond-pagto = movadm.nota-fiscal.cod-cond-pag AND
             cm-exc-condpagto.cod-estab = movadm.it-nota-fisc.cod-estab
             NO-LOCK NO-ERROR.

        IF AVAIL cm-exc-condpagto THEN DO.
           ASSIGN de-ind-finan = 1
                  de-preco-tab = 0.

           IF movadm.nota-fiscal.nr-ind-fin > 0 THEN DO.
              FIND tab-finan OF movadm.nota-fiscal NO-LOCK NO-ERROR.
              //ASSIGN de-ind-finan = tab-finan.tab-ind-fin[movadm.nota-fiscal.nr-ind-fin].

              FIND FIRST tab-finan-indice OF tab-finan WHERE 
                         tab-finan-indice.num-seq = movadm.nota-fiscal.nr-ind-fin NO-LOCK NO-ERROR.
              IF AVAIL tab-finan-indice THEN
                 ASSIGN de-ind-finan = tab-finan-indice.tab-ind-fin.
           END.

           FIND FIRST preco-item WHERE
                      preco-item.nr-tabpre = cm-exc-condpagto.tab-preco AND
                      preco-item.it-codigo = movadm.it-nota-fisc.it-codigo 
                      NO-LOCK NO-ERROR.

           IF AVAIL preco-item THEN
              ASSIGN de-preco-tab = preco-item.preco-venda * de-ind-finan.

           IF de-preco-tab <> 0 AND
              de-preco-tab <> movadm.it-nota-fisc.vl-preuni THEN DO.
              IF movadm.it-nota-fisc.vl-preuni < de-preco-tab THEN
                 ASSIGN de-perc-exc = cm-exc-condpagto.perc-menor-tab.
              ELSE
                 ASSIGN de-perc-exc = cm-exc-condpagto.perc-maior-tab.
           END.
        END.

        IF de-perc-exc <> 0 THEN DO.
           FIND tt-excessao WHERE
                tt-excessao.perc-comis = de-perc-exc NO-ERROR.
           IF NOT AVAIL tt-excessao THEN DO.
              CREATE tt-excessao.
              ASSIGN tt-excessao.perc-comis = de-perc-exc.
           END.
           ASSIGN tt-excessao.vlr-fat = tt-excessao.vlr-fat + movadm.it-nota-fisc.vl-tot-item
                  tt-excessao.vlr-desconto = tt-excessao.vlr-desconto + de-desconto.
        END.
    END.

    IF (ACCUM TOTAL movadm.it-nota-fisc.vl-tot-item) = 0 THEN NEXT.
    
    FOR EACH tt-calc-repres WHERE 
             tt-calc-repres.cod-rep = movadm.nota-fiscal.cod-rep NO-LOCK.

        FIND repres WHERE
             repres.cod-rep = tt-calc-repres.cod-pai NO-LOCK NO-ERROR.

        FIND cm-ext-repres WHERE
             cm-ext-repres.cod-rep = repres.cod-rep NO-LOCK NO-ERROR.

        ASSIGN de-vlr-tot-nota = ACCUM TOTAL movadm.it-nota-fisc.vl-tot-item
               de-vlr-tot-desc = ACCUM TOTAL de-desconto.

        ASSIGN de-vlr-nota-calc = de-vlr-tot-nota
               de-vlr-desc-calc = de-vlr-tot-desc.
        FOR EACH tt-excessao.
            ASSIGN de-vlr-nota-calc = de-vlr-nota-calc - tt-excessao.vlr-fat
                   de-vlr-desc-calc = de-vlr-desc-calc - tt-excessao.vlr-desconto.
        END.

        FIND tt-work WHERE
             tt-work.cod-estabel = movadm.nota-fiscal.cod-estabel AND
             tt-work.cod-rep     = tt-calc-repres.cod-pai AND
             tt-work.base        = 10 NO-ERROR.
        IF NOT AVAIL tt-work THEN DO:
           CREATE tt-work.
           ASSIGN tt-work.cod-estabel = movadm.nota-fiscal.cod-estabel
                  tt-work.cod-rep     = tt-calc-repres.cod-pai
                  tt-work.base        = 10.
        END.
        ASSIGN tt-work.vlr-fat = tt-work.vlr-fat + de-vlr-tot-nota.

        CASE cm-ext-repres.tp-aplic.
            WHEN 1 THEN DO.
                ASSIGN tt-work.comissao = tt-work.comissao + 
                                          IF de-perc-reduc-comis <> 0
                                          THEN (de-vlr-nota-calc * (repres.comis-direta * (100 - de-perc-reduc-comis) / 100) / 100) 
                                          ELSE (de-vlr-nota-calc * repres.comis-direta / 100).
            END.
            WHEN 2 THEN DO.
                ASSIGN tt-work.comissao = tt-work.comissao + 
                                          IF AVAIL movadm.ped-repre
                                          THEN (de-vlr-nota-calc * movadm.ped-repre.perc-comis / 100)
                                          ELSE (de-vlr-nota-calc * repres.comis-direta / 100).
            END.
            WHEN 3 THEN DO.
                ASSIGN tt-work.comissao = tt-work.comissao + 
                                          IF de-perc-reduc-comis <> 0
                                          THEN (de-vlr-nota-calc * (repres.comis-direta * (100 - de-perc-reduc-comis) / 100) / 100) 
                                          ELSE (de-vlr-nota-calc * repres.comis-direta / 100).
            END.
            WHEN 4 THEN DO.

            END.
        END CASE.

        FOR EACH tt-excessao.
            ASSIGN tt-work.comissao = tt-work.comissao + 
                                     (tt-excessao.vlr-fat * tt-excessao.perc-comis / 100).
        END.
        
        CREATE tt-nfs.
        ASSIGN tt-nfs.cod-estabel = movadm.nota-fiscal.cod-estabel
               tt-nfs.serie = movadm.nota-fiscal.serie
               tt-nfs.nr-nota-fis = movadm.nota-fiscal.nr-nota-fis
               tt-nfs.base = 10
               tt-nfs.cod-rep = tt-calc-repres.cod-pai
               tt-nfs.esp-docto = 22
               tt-nfs.vl-tot-nota = de-vlr-tot-nota.

        IF cm-ext-repres.tp-aplic = 2 THEN  // Faturamento Pr¢prio
           ASSIGN tt-nfs.comissao = tt-nfs.comissao + IF AVAIL movadm.ped-repre
                                    THEN de-vlr-nota-calc * movadm.ped-repre.perc-comis / 100
                                    ELSE de-vlr-nota-calc * repres.comis-direta / 100.
        ELSE
           ASSIGN tt-nfs.comissao = tt-nfs.comissao + 
                                    (de-vlr-nota-calc * (repres.comis-direta * (100 - de-perc-reduc-comis) / 100) / 100).


        IF de-vlr-tot-desc > 0 THEN DO.
           FIND tt-work WHERE
                tt-work.cod-estabel = movadm.nota-fiscal.cod-estabel AND
                tt-work.cod-rep     = tt-calc-repres.cod-pai AND
                tt-work.base        = 12
                NO-ERROR.
           IF NOT AVAIL tt-work THEN DO:
              CREATE tt-work.
              ASSIGN tt-work.cod-estabel = movadm.nota-fiscal.cod-estabel
                     tt-work.cod-rep     = tt-calc-repres.cod-pai
                     tt-work.base        = 12.
           END.
           ASSIGN tt-work.vlr-fat  = tt-work.vlr-fat + de-vlr-tot-desc .

           IF cm-ext-repres.tp-aplic = 2 THEN  // Faturamento Pr¢prio
              ASSIGN tt-work.comissao = tt-work.comissao + 
                                        IF AVAIL ped-repre 
                                        THEN (de-vlr-desc-calc * ped-repre.perc-comis / 100)
                                        ELSE (de-vlr-desc-calc * repres.comis-direta / 100).
           ELSE
              ASSIGN tt-work.comissao = tt-work.comissao + 
                                        IF de-perc-reduc-comis <> 0
                                        THEN (de-vlr-desc-calc * (repres.comis-direta * (100 - de-perc-reduc-comis) / 100) / 100)
                                        ELSE (de-vlr-desc-calc * repres.comis-direta / 100).
    
           FOR EACH tt-excessao.
               ASSIGN tt-work.comissao = tt-work.comissao + 
                                        (tt-excessao.vlr-desconto * tt-excessao.perc-comis / 100).
           END.
           
           CREATE tt-nfs.
           ASSIGN tt-nfs.cod-estabel = movadm.nota-fiscal.cod-estabel
                  tt-nfs.serie = movadm.nota-fiscal.serie
                  tt-nfs.nr-nota-fis = movadm.nota-fiscal.nr-nota-fis
                  tt-nfs.base = 12
                  tt-nfs.cod-rep = tt-calc-repres.cod-pai
                  tt-nfs.esp-docto = 22
                  tt-nfs.vl-tot-nota = ACCUM TOTAL de-desconto.

           IF cm-ext-repres.tp-aplic = 2 THEN  // Faturamento Pr¢prio
              ASSIGN tt-nfs.comissao = tt-nfs.comissao + 
                                       IF AVAIL ped-repre
                                       THEN (de-vlr-desc-calc * ped-repre.perc-comis / 100)
                                       ELSE (de-vlr-desc-calc * repres.comis-direta / 100).
           ELSE
               ASSIGN tt-nfs.comissao = tt-nfs.comissao + 
                                        (de-vlr-tot-desc * (repres.comis-direta * (100 - de-perc-reduc-comis) / 100) / 100 ).
        END.
    END.
END.



/* Processa Dados da MED */
{utp/ut-liter.i Processando_MED *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

EMPTY TEMP-TABLE tt-excessao.

ASSIGN de-vlr-nota-calc = 0
       de-vlr-desc-calc = 0.
/*
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
    
    FIND natur-oper WHERE
         natur-oper.nat-operacao = dbaux.nota-fiscal.nat-operacao NO-LOCK NO-ERROR.

    IF NOT AVAIL natur-oper THEN NEXT.
    IF natur-oper.tipo = 1 THEN NEXT. /* Movimenta‡Æo de Entrada */
    IF natur-oper.cod-esp <> "DP" THEN NEXT. /* Somente NFs. que Gera Duplicata */
    IF natur-oper.emite-dup = NO THEN NEXT. /* Somente NFs. que Gera Duplicata */

    FIND dbaux.ped-venda WHERE
         dbaux.ped-venda.nome-abrev = dbaux.nota-fiscal.nome-ab-cli AND
         dbaux.ped-venda.nr-pedcli = dbaux.nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.

    FIND dbaux.ped-repre OF dbaux.ped-venda WHERE
         dbaux.ped-repre.nome-ab-rep = dbaux.ped-venda.no-ab-reppri NO-LOCK NO-ERROR.

    FIND FIRST tt-calc-repres WHERE
               tt-calc-repres.cod-rep = dbaux.nota-fiscal.cod-rep NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-calc-repres THEN NEXT.

    FOR EACH dbaux.it-nota-fisc OF dbaux.nota-fiscal NO-LOCK.
        IF dbaux.it-nota-fisc.it-codigo < p-it-codigo-ini OR 
           dbaux.it-nota-fisc.it-codigo > p-it-codigo-fin  THEN NEXT.

        RUN pi-ver-digita (INPUT "Produto",
                           INPUT dbaux.it-nota-fisc.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND item WHERE
             item.it-codigo = dbaux.it-nota-fisc.it-codigo NO-LOCK  NO-ERROR.
        IF item.ge-codigo < 50 OR
           item.ge-codigo > 60 THEN NEXT.  /* somente tecidos */

        ASSIGN de-desconto = IF DEC(SUBSTR(dbaux.it-nota-fisc.char-2,1500,10)) = 0 
                             THEN dbaux.it-nota-fisc.val-desconto-total 
                             ELSE DEC(SUBSTR(dbaux.it-nota-fisc.char-2,1500,10)).

        ACCUMULATE dbaux.it-nota-fisc.vl-tot-item (TOTAL).
        ACCUMULATE de-desconto (TOTAL).

        ASSIGN de-perc-exc = 0.
        FIND cm-exc-item WHERE
             cm-exc-item.cod-rep = dbaux.nota-fiscal.cod-rep AND
             cm-exc-item.it-codigo = dbaux.it-nota-fisc.it-codigo AND
             cm-exc-item.cod-estabel = dbaux.it-nota-fisc.cod-estabel AND
             cm-exc-item.mes = MONTH(p-dt-periodo-ini) AND
             cm-exc-item.ano = YEAR(p-dt-periodo-ini)
             NO-LOCK NO-ERROR.
        
        IF AVAIL cm-exc-item THEN DO.
           ASSIGN de-ind-finan = 1
                  de-preco-tab = 0.

           IF dbaux.nota-fiscal.nr-ind-fin > 0 THEN DO.
              FIND tab-finan OF dbaux.nota-fiscal NO-LOCK NO-ERROR.
              ASSIGN de-ind-finan = tab-finan.tab-ind-fin[dbaux.nota-fiscal.nr-ind-fin].
           END.

           FIND FIRST preco-item WHERE
                      preco-item.nr-tabpre = cm-exc-item.tab-preco AND
                      preco-item.it-codigo = dbaux.it-nota-fisc.it-codigo 
                      NO-LOCK NO-ERROR.

           IF AVAIL preco-item THEN
              ASSIGN de-preco-tab = preco-item.preco-venda * de-ind-finan.

           IF de-preco-tab <> 0 AND
              de-preco-tab <> dbaux.it-nota-fisc.vl-preuni THEN DO.
              IF dbaux.it-nota-fisc.vl-preuni < de-preco-tab THEN
                 ASSIGN de-perc-exc = cm-exc-item.perc-menor-tab.
              ELSE
                 ASSIGN de-perc-exc = cm-exc-item.perc-maior-tab.
           END.
        END.
        
        FIND cm-exc-condpagto WHERE
             cm-exc-condpagto.cod-rep = dbaux.nota-fiscal.cod-rep AND
             cm-exc-condpagto.cod-cond-pagto = dbaux.nota-fiscal.cod-cond-pag AND
             cm-exc-condpagto.cod-estab = dbaux.it-nota-fisc.cod-estab
             NO-LOCK NO-ERROR.

        IF AVAIL cm-exc-condpagto THEN DO.
           ASSIGN de-ind-finan = 1
                  de-preco-tab = 0.

           IF dbaux.nota-fiscal.nr-ind-fin > 0 THEN DO.
              FIND tab-finan OF dbaux.nota-fiscal NO-LOCK NO-ERROR.
              ASSIGN de-ind-finan = tab-finan.tab-ind-fin[dbaux.nota-fiscal.nr-ind-fin].
           END.

           FIND FIRST preco-item WHERE
                      preco-item.nr-tabpre = cm-exc-condpagto.tab-preco AND
                      preco-item.it-codigo = dbaux.it-nota-fisc.it-codigo 
                      NO-LOCK NO-ERROR.

           IF AVAIL preco-item THEN
              ASSIGN de-preco-tab = preco-item.preco-venda * de-ind-finan.

           IF de-preco-tab <> 0 AND
              de-preco-tab <> dbaux.it-nota-fisc.vl-preuni THEN DO.
              IF dbaux.it-nota-fisc.vl-preuni < de-preco-tab THEN
                 ASSIGN de-perc-exc = cm-exc-condpagto.perc-menor-tab.
              ELSE
                 ASSIGN de-perc-exc = cm-exc-condpagto.perc-maior-tab.
           END.
        END.

        IF de-perc-exc <> 0 THEN DO.
           FIND tt-excessao WHERE
                tt-excessao.perc-comis = de-perc-exc NO-ERROR.
           IF NOT AVAIL tt-excessao THEN DO.
              CREATE tt-excessao.
              ASSIGN tt-excessao.perc-comis = de-perc-exc.
           END.
           ASSIGN tt-excessao.vlr-fat = tt-excessao.vlr-fat + dbaux.it-nota-fisc.vl-tot-item
                  tt-excessao.vlr-desconto = tt-excessao.vlr-desconto + de-desconto.
        END.
    END.
    IF (ACCUM TOTAL dbaux.it-nota-fisc.vl-tot-item) = 0 THEN NEXT.
    
    FOR EACH tt-calc-repres WHERE 
             tt-calc-repres.cod-rep = dbaux.nota-fiscal.cod-rep NO-LOCK.

        FIND repres WHERE
             repres.cod-rep = tt-calc-repres.cod-pai NO-LOCK NO-ERROR.

        ASSIGN de-vlr-tot-nota = ACCUM TOTAL dbaux.it-nota-fisc.vl-tot-item
               de-vlr-tot-desc = ACCUM TOTAL de-desconto.

        ASSIGN de-vlr-nota-calc = de-vlr-tot-nota
               de-vlr-desc-calc = de-vlr-tot-desc.
        FOR EACH tt-excessao.
            ASSIGN de-vlr-nota-calc = de-vlr-nota-calc - tt-excessao.vlr-fat
                   de-vlr-desc-calc = de-vlr-desc-calc - tt-excessao.vlr-desconto.
        END.

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
        ASSIGN tt-work.vlr-fat = tt-work.vlr-fat + de-vlr-tot-nota.
               tt-work.comissao = tt-work.comissao + 
                                  IF AVAIL dbaux.ped-repre
                                  THEN (de-vlr-nota-calc * dbaux.ped-repre.perc-comis / 100)
                                  ELSE (de-vlr-nota-calc * repres.comis-direta / 100).
        
        FOR EACH tt-excessao.
            ASSIGN tt-work.comissao = tt-work.comissao + 
                                     (tt-excessao.vlr-fat * tt-excessao.perc-comis / 100).
        END.
        
        CREATE tt-nfs.
        ASSIGN tt-nfs.cod-estabel = dbaux.nota-fiscal.cod-estabel
               tt-nfs.serie = dbaux.nota-fiscal.serie
               tt-nfs.nr-nota-fis = dbaux.nota-fiscal.nr-nota-fis
               tt-nfs.base = 10
               tt-nfs.cod-rep = tt-calc-repres.cod-pai
               tt-nfs.comissao = tt-nfs.comissao + 
                                 IF AVAIL dbaux.ped-repre
                                 THEN (de-vlr-nota-calc * dbaux.ped-repre.perc-comis / 100)
                                 ELSE (de-vlr-nota-calc * repres.comis-direta / 100)
               tt-nfs.esp-docto = 22
               tt-nfs.vl-tot-nota = de-vlr-tot-nota.
               
        IF de-vlr-tot-desc > 0 THEN DO.
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
           ASSIGN tt-work.vlr-fat  = tt-work.vlr-fat + de-vlr-tot-desc 
                  tt-work.comissao = tt-work.comissao + 
                                     IF AVAIL dbaux.ped-repre
                                     THEN (de-vlr-desc-calc * dbaux.ped-repre.perc-comis / 100)
                                     ELSE (de-vlr-desc-calc * repres.comis-direta / 100).
    
           FOR EACH tt-excessao.
               ASSIGN tt-work.comissao = tt-work.comissao + 
                                        (tt-excessao.vlr-desconto * tt-excessao.perc-comis / 100).
           END.
           
           CREATE tt-nfs.
           ASSIGN tt-nfs.cod-estabel = dbaux.nota-fiscal.cod-estabel
                  tt-nfs.serie = dbaux.nota-fiscal.serie
                  tt-nfs.nr-nota-fis = dbaux.nota-fiscal.nr-nota-fis
                  tt-nfs.base = 12
                  tt-nfs.cod-rep = tt-calc-repres.cod-pai
                  tt-nfs.esp-docto = 22
                  tt-nfs.comissao = tt-nfs.comissao + 
                                    IF AVAIL dbaux.ped-repre
                                    THEN (de-vlr-desc-calc * dbaux.ped-repre.perc-comis / 100)
                                    ELSE (de-vlr-desc-calc * repres.comis-direta / 100)
                  tt-nfs.vl-tot-nota = ACCUM TOTAL de-desconto.
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

