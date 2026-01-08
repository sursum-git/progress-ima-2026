/* Programa: ESSP0190.P
** Modulo..: Controle de Expedi‡Æo
** Objetivo: Gerar Registro Totalizador na TEMP-TABLE do programa ESSP0190.W
** Autor...: FµBIO COELHO LANZA - JUNHO/2009
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
*/

DEF TEMP-TABLE tt-work      NO-UNDO
    FIELD visualiza         AS LOG
    FIELD seq-item          AS INT
    FIELD seq-repres        AS INT
    FIELD seq-grupo         AS INT
    FIELD seq-cliente       AS INT
    FIELD seq-regiao        AS INT
    FIELD seq-uf            AS INT
    FIELD seq-nat-oper      AS INT
    FIELD seq-cond-pg       AS INT
    FIELD it-codigo         LIKE ped-item.it-codigo
    FIELD desc-item         LIKE ITEM.desc-item
    FIELD no-ab-reppri      LIKE ped-venda.no-ab-reppri
    FIELD cod-rep           LIKE repres.cod-rep
    FIELD matriz            LIKE it-nota-fisc.aliquota-icm
    FIELD nome-abrev        LIKE ped-venda.nome-abrev
    FIELD cod-emit          LIKE emitente.cod-emit
    FIELD regiao            AS CHAR
    FIELD nat-operacao      LIKE natur-oper.nat-operacao
    FIELD aliq-icms         LIKE natur-oper.aliquota-icm
    FIELD vl-icms           LIKE it-nota-fisc.vl-icms-it
    FIELD desc-pratic       AS DEC
    FIELD cond-pagto        AS CHAR
    FIELD uf                AS CHAR
    FIELD lote              AS CHAR
    FIELD Und               AS CHAR
    FIELD qtd               AS DEC
    FIELD qtd-devol         AS DEC
    FIELD vlr               AS DEC
    FIELD vlr-devol         AS DEC
    FIELD vlr-custo         AS DEC
    FIELD preco-medio       AS DEC
    FIELD prazo-medio       AS DEC
    FIELD rentabilidade     AS DEC
    FIELD perc-sobr-total   AS DEC.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-work.
DEFINE INPUT PARAMETER p-tipo AS CHAR.

DEF BUFFER b-tt-work FOR tt-work.                                                         

DEF VAR de-qtd-reg-metro  AS DEC.
DEF VAR de-vlr-reg-metro  AS DEC.
DEF VAR de-qtd-dev-metro  AS DEC.
DEF VAR de-vlr-dev-metro  AS DEC.
DEF VAR de-desc-metro     AS DEC.
DEF VAR de-perc-metro     AS DEC.
DEF VAR de-qtd-reg-kg     AS DEC.
DEF VAR de-vlr-reg-kg     AS DEC.
DEF VAR de-qtd-dev-kg     AS DEC.
DEF VAR de-vlr-dev-kg     AS DEC.
DEF VAR de-desc-kg        AS DEC.
DEF VAR de-perc-kg        AS DEC.
DEF VAR de-qtd-reg-und    AS DEC.
DEF VAR de-vlr-reg-und    AS DEC.
DEF VAR de-qtd-dev-und    AS DEC.
DEF VAR de-vlr-dev-und    AS DEC.
DEF VAR de-perc-und       AS DEC.
DEF VAR de-desc-und       AS DEC.
DEF VAR de-vl-icms        AS DEC.

FOR EACH b-tt-work WHERE
         {1} BREAK BY {2}   /* it-codigo, no-ab-reppri, matriz, nome-abrev, Regiao, nat-opera‡Æo, cond-pagto */
                   BY {3}.  /* Unidade */

    IF FIRST-OF({3}) THEN
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

    IF LAST-OF({3}) THEN DO:
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
          CASE p-tipo:
               WHEN "1" THEN
                   ASSIGN tt-work.it-codigo    = {2}. 
               WHEN "2" THEN
                   ASSIGN tt-work.no-ab-reppri = {2}.
               WHEN "3" THEN
                   ASSIGN tt-work.matriz       = DEC({2}).
               WHEN "4" THEN
                   ASSIGN tt-work.nome-abrev   = {2}.
               WHEN "5" THEN
                   ASSIGN tt-work.regiao       = {2}.
               WHEN "6" THEN
                   ASSIGN tt-work.nat-operacao = {2}.
               WHEN "7" THEN
                   ASSIGN tt-work.cond-pagto   = {2}.
          END CASE.
       END.
    END.
END.     
