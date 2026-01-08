/* Programa: ESSP0190.P
** Modulo..: Controle de Expedi‡Æo
** Objetivo: Calcular Medias e % na TEMP-TABLE do programa ESSP0190.W
** Autor...: FµBIO COELHO LANZA - JULHO/2009
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
    FIELD regiao            AS CHAR FORMAT "x(20)"
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

DEF BUFFER b-tt-work FOR tt-work.                                                         

DEF VAR de-tot-vlr  AS DEC.

ASSIGN de-tot-vlr = 0.
FOR EACH b-tt-work WHERE
         {1} SHARE-LOCK.  /* it-codigo, no-ab-reppri, matriz, nome-abrev, Regiao, nat-opera‡Æo, cond-pagto */

    ASSIGN de-tot-vlr = de-tot-vlr + b-tt-work.vlr.

END.     

FOR EACH b-tt-work WHERE
         {1} SHARE-LOCK.  
    IF b-tt-work.vlr > 0 THEN
       ASSIGN b-tt-work.preco-medio     = ROUND(b-tt-work.vlr / b-tt-work.qtd, 2)
              b-tt-work.perc-sobr-total = b-tt-work.vlr / de-tot-vlr * 100.
END.     

