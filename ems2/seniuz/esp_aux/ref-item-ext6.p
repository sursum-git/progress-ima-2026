/* ref-item-ext6
** Altera todos os itens come‡ados com 5 e com cod-obsoleto 2 para cod-obsoleto 1.
*/

DEF VAR c-item LIKE ref-item-ext.it-codigo.
DEF VAR c-aux  AS CHAR FORMAT "x(2)".

FOR EACH ref-item-ext WHERE ref-item-ext.it-codigo BEGINS "5"
                        AND ref-item-ext.cod-obsoleto = "2".
    ACCUMULATE ref-item-ext.cod-obsoleto(COUNT).
    
    ASSIGN ref-item-ext.cod-obsoleto = "1".
END.
DISP (ACCUM COUNT ref-item-ext.cod-obsoleto).
