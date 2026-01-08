/* Programa: ESSP0190.P
** Modulo..: Controle de Expedi‡Æo
** Objetivo: Gerar Registro Totalizador na TEMP-TABLE do programa ESSP0190.W
** Autor...: FµBIO COELHO LANZA - JUNHO/2009
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
*/

FOR EACH b-tt-work WHERE
         {2} BREAK BY {3}   /* it-codigo, no-ab-reppri, matriz, nome-abrev, Regiao, nat-opera‡Æo, cond-pagto */
                   BY {4}.  /* Unidade */

    IF FIRST-OF({4}) THEN
        ASSIGN de-qtd-reg-metro = 0   de-vlr-reg-metro = 0   de-qtd-dev-metro = 0
               de-vlr-dev-metro = 0   de-desc-metro    = 0   de-perc-metro    = 0
               de-qtd-reg-kg    = 0   de-vlr-reg-kg    = 0   de-qtd-dev-kg    = 0
               de-vlr-dev-kg    = 0   de-desc-kg       = 0   de-perc-kg       = 0
               de-qtd-reg-und   = 0   de-vlr-reg-und   = 0   de-qtd-dev-und   = 0
               de-vlr-dev-und   = 0   de-desc-und      = 0   de-perc-und      = 0
               de-vl-icms       = 0.

    CASE b-tt-work.und.
        WHEN "KG" THEN
            ASSIGN de-qtd-reg-kg = de-qtd-reg-kg + b-tt-work.qtd
                   de-vlr-reg-kg = de-vlr-reg-kg + b-tt-work.vlr
                   de-qtd-dev-kg = de-qtd-dev-kg + b-tt-work.qtd-devol
                   de-vlr-dev-kg = de-vlr-dev-kg + b-tt-work.vlr-devol
                   de-desc-kg    = de-desc-kg    + b-tt-work.desc-pratic
                   de-perc-kg    = de-perc-kg    + b-tt-work.perc-sobr-total
                   de-vl-icms    = de-vl-icms    + b-tt-work.vl-icms.
        WHEN "M" THEN
            ASSIGN de-qtd-reg-metro = de-qtd-reg-metro + b-tt-work.qtd
                   de-vlr-reg-metro = de-vlr-reg-metro + b-tt-work.vlr
                   de-qtd-dev-metro = de-qtd-dev-metro + b-tt-work.qtd-devol
                   de-vlr-dev-metro = de-vlr-dev-metro + b-tt-work.vlr-devol
                   de-desc-metro    = de-desc-metro    + b-tt-work.desc-pratic
                   de-perc-metro    = de-perc-metro    + b-tt-work.perc-sobr-total
                   de-vl-icms       = de-vl-icms       + b-tt-work.vl-icms.
        WHEN "UN" THEN
            ASSIGN de-qtd-reg-und = de-qtd-reg-und + b-tt-work.qtd
                   de-vlr-reg-und = de-vlr-reg-und + b-tt-work.vlr
                   de-qtd-dev-und = de-qtd-dev-und + b-tt-work.qtd-devol
                   de-vlr-dev-und = de-vlr-dev-und + b-tt-work.vlr-devol
                   de-desc-und    = de-desc-und    + b-tt-work.desc-pratic
                   de-perc-und    = de-perc-und    + b-tt-work.perc-sobr-total
                   de-vl-icms     = de-vl-icms     + b-tt-work.vl-icms.
    END CASE.

    IF LAST-OF({4}) THEN DO:
       IF (de-qtd-reg-kg + de-vlr-reg-kg + de-qtd-dev-kg + de-vlr-dev-kg) <> 0 THEN DO:
          CREATE tt-work.
          ASSIGN tt-work.uf              = "ZZ" 
                 tt-work.und             = "KG"    
                 tt-work.qtd             = de-qtd-reg-kg
                 tt-work.vlr             = de-vlr-reg-kg
                 tt-work.qtd-devol       = de-qtd-dev-kg
                 tt-work.vlr-devol       = de-vlr-dev-kg
                 tt-work.desc-pratic     = de-desc-kg
                 tt-work.perc-sobr-total = de-perc-kg
                 tt-work.vl-icms         = de-vl-icms.
          IF de-qtd-reg-kg <> 0 THEN
             ASSIGN tt-work.preco-medio = ROUND(de-vlr-reg-kg / de-qtd-reg-kg, 2).
       END. 
       IF (de-qtd-reg-metro + de-vlr-reg-metro + de-qtd-dev-metro + de-vlr-dev-metro) <> 0 THEN DO: 
          CREATE tt-work.
          ASSIGN tt-work.uf              = "ZZ" 
                 tt-work.und             = "M"    
                 tt-work.qtd             = de-qtd-reg-metro
                 tt-work.vlr             = de-vlr-reg-metro
                 tt-work.qtd-devol       = de-qtd-dev-metro
                 tt-work.vlr-devol       = de-vlr-dev-metro
                 tt-work.desc-pratic     = de-desc-metro
                 tt-work.perc-sobr-total = de-perc-metro
                 tt-work.vl-icms         = de-vl-icms.
          IF de-qtd-reg-metro <> 0 THEN
             ASSIGN tt-work.preco-medio = ROUND(de-vlr-reg-metro / de-qtd-reg-metro, 2).
       END.
       IF (de-qtd-reg-und + de-vlr-reg-und + de-qtd-dev-und + de-vlr-dev-und) <> 0 THEN DO:
          CREATE tt-work.
          ASSIGN tt-work.uf              = "ZZ" 
                 tt-work.und             = "UN"    
                 tt-work.qtd             = de-qtd-reg-und
                 tt-work.vlr             = de-vlr-reg-und
                 tt-work.qtd-devol       = de-qtd-dev-und
                 tt-work.vlr-devol       = de-vlr-dev-und
                 tt-work.desc-pratic     = de-desc-und
                 tt-work.perc-sobr-total = de-perc-und
                 tt-work.vl-icms         = de-vl-icms.
          IF de-qtd-reg-und <> 0 THEN
             ASSIGN tt-work.preco-medio = ROUND(de-vlr-reg-und / de-qtd-reg-und, 2).
       END. 

       IF AVAIL tt-work THEN DO.
          CASE "{1}":
               WHEN "1" THEN
                   ASSIGN tt-work.it-codigo    = STRING({3}). 
               WHEN "2" THEN
                   ASSIGN tt-work.no-ab-reppri = STRING({3}).
               WHEN "3" THEN
                   ASSIGN tt-work.matriz       = INTEGER({3}).
               WHEN "4" THEN
                   ASSIGN tt-work.nome-abrev   = STRING({3}).
               WHEN "5" THEN
                   ASSIGN tt-work.regiao       = STRING({3}).
               WHEN "6" THEN
                   ASSIGN tt-work.nat-operacao = STRING({3}).
               WHEN "7" THEN
                   ASSIGN tt-work.cond-pagto   = STRING({3}).
          END CASE.
       END.
    END.
END.     
