/* Programa: exp-cota-item
** Ojbetivo: Exportar dados de cota-itens para um arquivo separado por ; 
*/

OUTPUT TO "c:/temp/cota-item.txt".
PUT "Item;"
    "Item-adic1;"
    "Item-adic2;"
    "Item-adic3;"
    "Item-adic4;"
    "Item-adic5;"
    "Item-adic6;"
    "Estoque;"
    "Producao;"
    "Cart-mes-ant;"
    "Cota1;"
    "Susp-Cota1;"
    "Cota2;"
    "Susp-Cota2;"
    "Dt-min-ped"
    SKIP.

FOR EACH cota-item NO-LOCK.
    PUT cota-item.it-codigo ";"
        cota-item.item-adic1 ";"
        cota-item.item-adic2 ";"
        cota-item.item-adic3 ";"
        cota-item.item-adic4 ";"
        cota-item.item-adic5 ";"
        cota-item.item-adic6 ";"
        cota-item.estoque ";"
        cota-item.producao ";" 
        cota-item.cart-mes-ant ";"
        cota-item.cota1 ";"
        IF cota-item.susp-cota1 THEN "Yes" ELSE "No"
        ";"
        cota-item.cota2 ";"
        IF cota-item.susp-cota2 THEN "Yes" ELSE "No"
        ";"
        cota-item.dt-min-ped
        SKIP.
END.
OUTPUT CLOSE.
