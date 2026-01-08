/* Programa: ESSP0190.P
** Modulo..: Controle de Expedi‡Æo
** Objetivo: Calcular Medias e % na TEMP-TABLE do programa ESSP0190.W
** Autor...: FµBIO COELHO LANZA - JULHO/2009
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
*/

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

