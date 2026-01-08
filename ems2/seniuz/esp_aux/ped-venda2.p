/* Gera planilha com pedidos em aberto e suas observa‡äes, para adequar
** ao modelo para faturamento autom tico
*/

DEF VAR c-observ AS CHAR FORMAT "x(2000)".
OUTPUT TO c:/temp/pedidos.csv CONVERT SOURCE "ibm850".

FOR EACH ped-venda WHERE ped-venda.cod-sit-ped < 3 OR
                         ped-venda.cod-sit-ped = 5
                   NO-LOCK.
    ASSIGN c-observ = replace(replace(ped-venda.observacoes, chr(13), " "), chr(10), " ").
    PUT ped-venda.nr-pedcli ";"
        ped-venda.nome-abrev ";"
        c-observ 
        SKIP.
END.
OUTPUT CLOSE.

