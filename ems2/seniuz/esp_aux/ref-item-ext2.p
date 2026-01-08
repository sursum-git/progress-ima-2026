/* Verifica divergˆncias entre as amarra‡äes (ref-item-ext) e as respectivas referˆncias (referencia-ext).
*/

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.csv".
PUT "Item;Refer;Obsol-Item;Obsol-Ref" SKIP.

FOR EACH ref-item-ext WHERE ref-item-ext.it-codigo BEGINS "5" 
                      NO-LOCK
                      BY ref-item-ext.it-codigo
                      BY ref-item-ext.cod-refer:
    FIND item-ext WHERE item-ext.it-codigo = ref-item-ext.it-codigo 
                  NO-LOCK NO-ERROR.
        PUT ref-item-ext.it-codigo ";"
            ref-item-ext.cod-refer ";"
            item-ext.cod-obsoleto ";;"
            ref-item-ext.cod-obsoleto          
            SKIP.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "excel.exe", input "c:\lixo\lixo.csv").
delete procedure h-prog.
