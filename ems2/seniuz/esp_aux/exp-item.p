/* Programa: exp-item
** Ojbetivo: Exportar dados de itens para um aruqivo separado por ; para ser aberto
**           no excel, para conferˆncia da Janete, dos principais dados cadastrais.
*/

DEF VAR de-largura    LIKE item-ext.largura.
DEF VAR c-cod-composi LIKE item-ext.cod-composi.
DEF VAR i-cod-rlgp    LIKE item-ext.cod-rlgp.
DEF VAR de-fator-conv LIKE item-ext.fator-conv.

OUTPUT TO "c:/lixo/item.csv" CONVERT SOURCE "ibm850".
PUT "Codigo;"
    "Descricao;"
    "Larg;"
    "Class-Fiscal;"
    "Un;"
    "GE;"
    "Familia;"
    "EstPad;"
    "Compo;"
    "RL;"
    "Peso-Liq;"
    "Fatur;"
    "Fator Kg->M"
    SKIP.

FOR EACH ITEM WHERE ITEM.ge-codigo >= 50 
                AND ITEM.ge-codigo <= 59
              NO-LOCK:
    FIND item-ext OF ITEM NO-LOCK NO-ERROR.
    IF AVAIL item-ext THEN
       ASSIGN de-largura    = item-ext.largura
              c-cod-composi = item-ext.cod-composi
              i-cod-rlgp    = item-ext.cod-rlgp
              de-fator-conv = item-ext.fator-conv.
    ELSE
       ASSIGN de-largura    = 0
              c-cod-composi = ""
              i-cod-rlgp    = 0
              de-fator-conv = 0.

    PUT ITEM.it-codigo ";"
        ITEM.desc-item ";"
        de-largura ";" 
        ITEM.class-fiscal ";"
        ITEM.un ";"
        ITEM.ge-codigo ";"
        ITEM.fm-codigo ";"
        ITEM.cod-estabel ";"
        c-cod-composi ";"
        i-cod-rlgp ";"
        ITEM.peso-liquido ";"
        ITEM.ind-item-fat ";"
        de-fator-conv
        SKIP.
END.
OUTPUT CLOSE.
