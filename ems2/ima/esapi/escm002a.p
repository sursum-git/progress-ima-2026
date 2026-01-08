/* Busca NOTAS FISCAIS do Representante */
DEFINE TEMP-TABLE tt-work  NO-UNDO 
       FIELD cod-estabel   LIKE nota-fiscal.cod-estabel
       FIELD base          AS INT
       FIELD cod-rep       LIKE nota-fiscal.cod-rep
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

DEFINE TEMP-TABLE tt-nfs   LIKE nota-fiscal
       FIELD qt-faturada   AS   DECIMAL
       FIELD comissao      AS   DECIMAL
       FIELD base          AS   INT
       INDEX indice1 IS PRIMARY cod-rep cod-estabel serie nr-nota-fis.

DEFINE TEMP-TABLE tt-calc-repres 
       FIELD cod-rep      LIKE repres.cod-rep
       FIELD nome-ab-rep  LIKE repres.nome-abrev
       FIELD cod-pai      LIKE repres.cod-rep
       FIELD nome-ab-pai  LIKE repres.nome-abrev
       FIELD marca        AS LOGICAL.

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
DEF VAR de-desconto             LIKE it-nota-fisc.val-desconto-total.
DEF VAR de-perc-exc             AS DEC.
DEF VAR de-ind-finan            AS DEC.
DEF VAR de-perc-reduc-comis     AS DEC.
DEF VAR de-preco-tab            AS DEC.
DEF VAR de-vlr-tot-nota         AS DEC.
DEF VAR de-vlr-tot-desc         AS DEC.
DEF VAR de-vlr-nota-calc        AS DEC.  /* Valores para Calculo da Comiss∆o */
DEF VAR de-vlr-desc-calc        AS DEC.  /* Valores para Calculo da Comiss∆o Backup */

DEF VAR h-acomp          AS HANDLE NO-UNDO.

DEF BUFFER natur-oper FOR natur-oper.
DEF BUFFER ITEM FOR item.
DEF BUFFER tab-finan FOR tab-finan.
DEF BUFFER preco-item FOR preco-item.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

/* Processa Dados da IMA */
{utp/ut-liter.i Processando_IMA *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FOR EACH tt-calc-repres.
    FIND repres WHERE
         repres.cod-rep = tt-calc-repres.cod-rep NO-LOCK NO-ERROR.
    ASSIGN tt-calc-repres.nome-ab-rep = repres.nome-abrev.

    FIND repres WHERE
         repres.cod-rep = tt-calc-repres.cod-pai NO-LOCK NO-ERROR.
    ASSIGN tt-calc-repres.nome-ab-pai = repres.nome-abrev.
END.

FOR EACH nota-fiscal WHERE
         nota-fiscal.cod-estabel  >= p-cod-estab-ini AND 
         nota-fiscal.cod-estabel  <= p-cod-estab-fin AND
         nota-fiscal.dt-emis-nota >= p-dt-periodo-ini AND 
         nota-fiscal.dt-emis-nota <= p-dt-periodo-fin AND
         nota-fiscal.nr-nota-fis  >= p-nr-nota-fis-ini AND
         nota-fiscal.nr-nota-fis  <= p-nr-nota-fis-fin AND 
         nota-fiscal.dt-cancela    = ? NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(nota-fiscal.dt-emis-nota) +
                                        " Nota Fiscal: " + nota-fiscal.nr-nota-fis).

    IF nota-fiscal.cod-cond-pag < p-cond-pagto-ini OR
       nota-fiscal.cod-cond-pag > p-cond-pagto-fin THEN NEXT.
    RUN pi-ver-digita (INPUT "Condiá∆o Pagamento",
                       INPUT nota-fiscal.no-ab-reppri).
    IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    
    FIND natur-oper WHERE
         natur-oper.nat-operacao = nota-fiscal.nat-operacao NO-LOCK NO-ERROR.

    IF NOT AVAIL natur-oper THEN NEXT.
    IF natur-oper.tipo = 1 THEN NEXT. /* Movimentaá∆o de Entrada */
    IF natur-oper.cod-esp <> "DP" THEN NEXT. /* Somente NFs. que Gera Duplicata */
    IF natur-oper.emite-dup = NO THEN NEXT. /* Somente NFs. que Gera Duplicata */

    FIND ped-venda WHERE
         ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND
         ped-venda.nr-pedcli = nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.

    FOR EACH tt-calc-repres.
        ASSIGN tt-calc-repres.marca = NO.
    END.

    FOR EACH ped-repre OF ped-venda NO-LOCK.
        FIND FIRST tt-calc-repres WHERE
                   tt-calc-repres.nome-ab-rep = ped-repre.nome-ab-rep  NO-LOCK NO-ERROR.
        IF AVAIL tt-calc-repres THEN
           ASSIGN tt-calc-repres.marca = YES.

        FIND FIRST tt-calc-repres WHERE
                   tt-calc-repres.nome-ab-pai = ped-repre.nome-ab-rep NO-LOCK NO-ERROR.
        IF AVAIL tt-calc-repres THEN
           ASSIGN tt-calc-repres.marca = YES.
    END.
        
    FIND FIRST tt-calc-repres WHERE
               tt-calc-repres.marca = YES NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-calc-repres THEN NEXT.

    FIND ped-venda-ext WHERE
         ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND    
         ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

    ASSIGN de-perc-reduc-comis = 0.
    FIND tbs_preco WHERE
         tbs_preco.tb_preco_id = ped-venda-ext.tb_preco_id NO-LOCK NO-ERROR.
    IF AVAIL tbs_preco AND 
       tbs_preco.perc_reduc_comis <> 0 THEN
       ASSIGN de-perc-reduc-comis = tbs_preco.perc_reduc_comis.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
        IF it-nota-fisc.it-codigo < p-it-codigo-ini OR 
           it-nota-fisc.it-codigo > p-it-codigo-fin  THEN NEXT.

        RUN pi-ver-digita (INPUT "Produto",
                           INPUT it-nota-fisc.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND item WHERE
             item.it-codigo = it-nota-fisc.it-codigo NO-LOCK  NO-ERROR.
        IF item.ge-codigo < 50 OR
           item.ge-codigo > 60 THEN NEXT.  /* somente tecidos */

        ASSIGN de-desconto = IF DEC(SUBSTR(it-nota-fisc.char-2,1500,10)) = 0 
                             THEN it-nota-fisc.val-desconto-total 
                             ELSE DEC(SUBSTR(it-nota-fisc.char-2,1500,10)).


        ACCUMULATE it-nota-fisc.vl-tot-item (TOTAL).
        ACCUMULATE de-desconto (TOTAL).

        ASSIGN de-perc-exc = 0.
        FIND cm-exc-item WHERE
             cm-exc-item.cod-rep = nota-fiscal.cod-rep AND
             cm-exc-item.it-codigo = it-nota-fisc.it-codigo AND
             cm-exc-item.cod-estabel = it-nota-fisc.cod-estabel AND
             cm-exc-item.mes = MONTH(p-dt-periodo-ini) AND
             cm-exc-item.ano = YEAR(p-dt-periodo-ini)
             NO-LOCK NO-ERROR.
        
        IF AVAIL cm-exc-item THEN DO.
           ASSIGN de-ind-finan = 1
                  de-preco-tab = 0.

           IF nota-fiscal.nr-ind-fin > 0 THEN DO.
              FIND tab-finan OF nota-fiscal NO-LOCK NO-ERROR.
              //ASSIGN de-ind-finan = tab-finan.tab-ind-fin[nota-fiscal.nr-ind-fin].

              FIND FIRST tab-finan-indice OF tab-finan WHERE 
                         tab-finan-indice.num-seq = nota-fiscal.nr-ind-fin NO-LOCK NO-ERROR.
              IF AVAIL tab-finan-indice THEN
                 ASSIGN de-ind-finan = tab-finan-indice.tab-ind-fin.
           END.

           FIND FIRST preco-item WHERE
                      preco-item.nr-tabpre = cm-exc-item.tab-preco AND
                      preco-item.it-codigo = it-nota-fisc.it-codigo 
                      NO-LOCK NO-ERROR.

           IF AVAIL preco-item THEN
              ASSIGN de-preco-tab = preco-item.preco-venda * de-ind-finan.

           IF de-preco-tab <> 0 AND
              de-preco-tab <> it-nota-fisc.vl-preuni THEN DO.
              IF it-nota-fisc.vl-preuni < de-preco-tab THEN
                 ASSIGN de-perc-exc = cm-exc-item.perc-menor-tab.
              ELSE
                 ASSIGN de-perc-exc = cm-exc-item.perc-maior-tab.
           END.
        END.
        
        FIND cm-exc-condpagto WHERE
             cm-exc-condpagto.cod-rep = nota-fiscal.cod-rep AND
             cm-exc-condpagto.cod-cond-pagto = nota-fiscal.cod-cond-pag AND
             cm-exc-condpagto.cod-estab = it-nota-fisc.cod-estab
             NO-LOCK NO-ERROR.

        IF AVAIL cm-exc-condpagto THEN DO.
           ASSIGN de-ind-finan = 1
                  de-preco-tab = 0.

           IF nota-fiscal.nr-ind-fin > 0 THEN DO.
              FIND tab-finan OF nota-fiscal NO-LOCK NO-ERROR.
              //ASSIGN de-ind-finan = tab-finan.tab-ind-fin[nota-fiscal.nr-ind-fin].

              FIND FIRST tab-finan-indice OF tab-finan WHERE 
                         tab-finan-indice.num-seq = nota-fiscal.nr-ind-fin NO-LOCK NO-ERROR.
              IF AVAIL tab-finan-indice THEN
                 ASSIGN de-ind-finan = tab-finan-indice.tab-ind-fin.
           END.

           FIND FIRST preco-item WHERE
                      preco-item.nr-tabpre = cm-exc-condpagto.tab-preco AND
                      preco-item.it-codigo = it-nota-fisc.it-codigo 
                      NO-LOCK NO-ERROR.

           IF AVAIL preco-item THEN
              ASSIGN de-preco-tab = preco-item.preco-venda * de-ind-finan.

           IF de-preco-tab <> 0 AND
              de-preco-tab <> it-nota-fisc.vl-preuni THEN DO.
              IF it-nota-fisc.vl-preuni < de-preco-tab THEN
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
           ASSIGN tt-excessao.vlr-fat = tt-excessao.vlr-fat + it-nota-fisc.vl-tot-item
                  tt-excessao.vlr-desconto = tt-excessao.vlr-desconto + de-desconto.
        END.
    END.

    IF (ACCUM TOTAL it-nota-fisc.vl-tot-item) = 0 THEN NEXT.
    
    FOR EACH tt-calc-repres WHERE 
             tt-calc-repres.marca = YES NO-LOCK.

        IF tt-calc-repres.nome-ab-rep = 'FULANO' THEN NEXT.

        FIND ped-repre OF ped-venda WHERE
             ped-repre.nome-ab-rep = tt-calc-repres.nome-ab-rep NO-LOCK NO-ERROR.

        FIND repres WHERE
             repres.cod-rep = tt-calc-repres.cod-pai NO-LOCK NO-ERROR.

        FIND cm-ext-repres WHERE
             cm-ext-repres.cod-rep = repres.cod-rep NO-LOCK NO-ERROR.

        ASSIGN de-vlr-tot-nota = ACCUM TOTAL it-nota-fisc.vl-tot-item
               de-vlr-tot-desc = ACCUM TOTAL de-desconto.

        ASSIGN de-vlr-nota-calc = de-vlr-tot-nota
               de-vlr-desc-calc = de-vlr-tot-desc.
        FOR EACH tt-excessao.
            ASSIGN de-vlr-nota-calc = de-vlr-nota-calc - tt-excessao.vlr-fat
                   de-vlr-desc-calc = de-vlr-desc-calc - tt-excessao.vlr-desconto.
        END.

        FIND tt-nfs WHERE
             tt-nfs.cod-estabel = nota-fiscal.cod-estabel AND
             tt-nfs.serie = nota-fiscal.serie AND
             tt-nfs.nr-nota-fis = nota-fiscal.nr-nota-fis AND
             tt-nfs.base = 10 AND
             tt-nfs.cod-rep = tt-calc-repres.cod-pai 
             NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-nfs THEN DO. 
           CREATE tt-nfs.
           ASSIGN tt-nfs.cod-estabel = nota-fiscal.cod-estabel
                  tt-nfs.serie = nota-fiscal.serie
                  tt-nfs.nr-nota-fis = nota-fiscal.nr-nota-fis
                  tt-nfs.dt-emis = nota-fiscal.dt-emis
                  tt-nfs.nome-ab-cli = nota-fiscal.nome-ab-cli
                  tt-nfs.no-ab-reppri = nota-fiscal.no-ab-reppri
                  tt-nfs.nr-pedcli = nota-fiscal.nr-pedcli
                  tt-nfs.base = 10
                  tt-nfs.cod-rep = tt-calc-repres.cod-pai
                  tt-nfs.esp-docto = 22
                  tt-nfs.vl-tot-nota = de-vlr-nota-calc.
                  
                  
           // Procura o perentual de comiss∆o por estado (UF) 
           FIND cm-unid-feder OF repres WHERE
                cm-unid-feder.uf = nota-fiscal.estado NO-LOCK NO-ERROR.
           IF AVAIL cm-unid-feder THEN DO.
              ASSIGN tt-nfs.dec-2 = cm-unid-feder.perc-comis * (100 - de-perc-reduc-comis) / 100
                     tt-nfs.comissao = tt-nfs.comissao +
                                       (de-vlr-nota-calc * (cm-unid-feder.perc-comis * (100 - de-perc-reduc-comis) / 100) / 100).
           END.                               
           ELSE DO.
               IF cm-ext-repres.tp-aplic = 2 THEN  // Faturamento Pr¢prio
                  ASSIGN tt-nfs.dec-2 = IF AVAIL ped-repre
                                        THEN ped-repre.perc-comis / 100
                                        ELSE repres.comis-direta / 100
                         tt-nfs.comissao = tt-nfs.comissao + IF AVAIL ped-repre
                                           THEN de-vlr-nota-calc * ped-repre.perc-comis / 100
                                           ELSE de-vlr-nota-calc * repres.comis-direta / 100.
               ELSE
                  ASSIGN tt-nfs.dec-2 = repres.comis-direta * (100 - de-perc-reduc-comis) / 100 
                         tt-nfs.comissao = tt-nfs.comissao + 
                                           (de-vlr-nota-calc * (repres.comis-direta * (100 - de-perc-reduc-comis) / 100) / 100).
           END.
                                       
           FIND tt-work WHERE
                tt-work.cod-estabel = nota-fiscal.cod-estabel AND
                tt-work.cod-rep     = tt-calc-repres.cod-pai AND
                tt-work.base        = 10 NO-ERROR.
           IF NOT AVAIL tt-work THEN DO:
              CREATE tt-work.
              ASSIGN tt-work.cod-estabel = nota-fiscal.cod-estabel
                     tt-work.cod-rep     = tt-calc-repres.cod-pai
                     tt-work.base        = 10.
           END.
           ASSIGN tt-work.vlr-fat = tt-work.vlr-fat + de-vlr-nota-calc.
           
           
           FIND cm-unid-feder OF repres WHERE
                cm-unid-feder.uf = nota-fiscal.estado NO-LOCK NO-ERROR.
           IF AVAIL cm-unid-feder THEN DO.
              ASSIGN tt-work.comissao = tt-work.comissao +
                                       (de-vlr-nota-calc * (cm-unid-feder.perc-comis * (100 - de-perc-reduc-comis) / 100) / 100).
           END.                                         
           ELSE DO.

               CASE cm-ext-repres.tp-aplic.
                   WHEN 1 THEN DO.
                       ASSIGN tt-work.comissao = tt-work.comissao + 
                                                 IF de-perc-reduc-comis <> 0
                                                 THEN (de-vlr-nota-calc * (repres.comis-direta * (100 - de-perc-reduc-comis) / 100) / 100) 
                                                 ELSE (de-vlr-nota-calc * repres.comis-direta / 100).
                   END.
                   WHEN 2 THEN DO.
                       ASSIGN tt-work.comissao = tt-work.comissao + 
                                                 IF AVAIL ped-repre
                                                 THEN (de-vlr-nota-calc * ped-repre.perc-comis / 100)
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
           END.
           
           FOR EACH tt-excessao.
               ASSIGN tt-work.comissao = tt-work.comissao + 
                                        (tt-excessao.vlr-fat * tt-excessao.perc-comis / 100).
           END.
        END.

        //---------- Desconto / Prioridade

        IF de-vlr-desc-calc > 0 THEN DO.
           FIND tt-work WHERE
                tt-work.cod-estabel = nota-fiscal.cod-estabel AND
                tt-work.cod-rep     = tt-calc-repres.cod-pai AND
                tt-work.base        = 12
                NO-ERROR.
           IF NOT AVAIL tt-work THEN DO:
              CREATE tt-work.
              ASSIGN tt-work.cod-estabel = nota-fiscal.cod-estabel
                     tt-work.cod-rep     = tt-calc-repres.cod-pai
                     tt-work.base        = 12.
           END.
           ASSIGN tt-work.vlr-fat  = tt-work.vlr-fat + de-vlr-desc-calc.

           
           FIND cm-unid-feder OF repres WHERE
                cm-unid-feder.uf = nota-fiscal.estado NO-LOCK NO-ERROR.
           IF AVAIL cm-unid-feder THEN DO.
              ASSIGN tt-work.comissao = tt-work.comissao +
                                       (de-vlr-desc-calc * (cm-unid-feder.perc-comis * (100 - de-perc-reduc-comis) / 100) / 100). 
           END.                                         
           ELSE DO.           
               CASE cm-ext-repres.tp-aplic.
                   WHEN 1 THEN DO.
                       ASSIGN tt-work.comissao = tt-work.comissao + 
                                                 IF de-perc-reduc-comis <> 0
                                                 THEN (de-vlr-desc-calc * (repres.comis-direta * (100 - de-perc-reduc-comis) / 100) / 100) 
                                                 ELSE (de-vlr-desc-calc * repres.comis-direta / 100).
                   END.
                   WHEN 2 THEN DO.
                       ASSIGN tt-work.comissao = tt-work.comissao + 
                                                 IF AVAIL ped-repre
                                                 THEN (de-vlr-desc-calc * ped-repre.perc-comis / 100)
                                                 ELSE (de-vlr-desc-calc * repres.comis-direta / 100).
                   END.
                   WHEN 3 THEN DO.
                       ASSIGN tt-work.comissao = tt-work.comissao + 
                                                 IF de-perc-reduc-comis <> 0
                                                 THEN (de-vlr-desc-calc * (repres.comis-direta * (100 - de-perc-reduc-comis) / 100) / 100) 
                                                 ELSE (de-vlr-desc-calc * repres.comis-direta / 100).
                   END.
                   WHEN 4 THEN DO.

                   END.
               END CASE.
           END.
           
           /*
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
           */

           FOR EACH tt-excessao.
               ASSIGN tt-work.comissao = tt-work.comissao + 
                                        (tt-excessao.vlr-desconto * tt-excessao.perc-comis / 100).
           END.
           
           FIND tt-nfs WHERE
                tt-nfs.cod-estabel = nota-fiscal.cod-estabel AND
                tt-nfs.serie = nota-fiscal.serie AND
                tt-nfs.nr-nota-fis = nota-fiscal.nr-nota-fis AND
                tt-nfs.base = 12 AND
                tt-nfs.cod-rep = tt-calc-repres.cod-pai 
                NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-nfs THEN DO.
              CREATE tt-nfs.
              ASSIGN tt-nfs.cod-estabel = nota-fiscal.cod-estabel
                     tt-nfs.serie = nota-fiscal.serie
                     tt-nfs.nr-nota-fis = nota-fiscal.nr-nota-fis
                     tt-nfs.dt-emis = nota-fiscal.dt-emis
                     tt-nfs.nr-pedcli = nota-fiscal.nr-pedcli
                     tt-nfs.no-ab-reppri = nota-fiscal.no-ab-reppri
                     tt-nfs.nome-ab-cli = nota-fiscal.nome-ab-cli
                     tt-nfs.base = 12
                     tt-nfs.cod-rep = tt-calc-repres.cod-pai
                     tt-nfs.esp-docto = 22
                     tt-nfs.vl-tot-nota = de-vlr-desc-calc.

              FIND cm-unid-feder OF repres WHERE
                   cm-unid-feder.uf = nota-fiscal.estado NO-LOCK NO-ERROR.
              IF AVAIL cm-unid-feder THEN DO.
                 ASSIGN tt-nfs.comissao = tt-nfs.comissao +
                                          (de-vlr-desc-calc * (cm-unid-feder.perc-comis * (100 - de-perc-reduc-comis) / 100) / 100). 
              END.                                         
              ELSE DO.                               
                  IF cm-ext-repres.tp-aplic = 2 THEN  // Faturamento Pr¢prio
                     ASSIGN tt-nfs.comissao = tt-nfs.comissao + 
                                              IF AVAIL ped-repre
                                              THEN (de-vlr-desc-calc * ped-repre.perc-comis / 100)
                                              ELSE (de-vlr-desc-calc * repres.comis-direta / 100).
                  ELSE
                     ASSIGN tt-nfs.comissao = tt-nfs.comissao + 
                                              (de-vlr-desc-calc * (repres.comis-direta * (100 - de-perc-reduc-comis) / 100) / 100 ).
              END.
           END. 
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

