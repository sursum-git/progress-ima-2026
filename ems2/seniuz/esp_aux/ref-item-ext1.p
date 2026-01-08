/* Verifica divergˆncias entre as amarra‡äes (ref-item-ext) e as respectivas referˆncias (referencia-ext).
*/

DEF VAR l-prim_vez AS LOG.

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.csv".
PUT "Refer;Obsl;Colec;Fundo;Cor;Item;Obsl;Colec;Fundo;Cor" SKIP.

FOR EACH ref-item-ext NO-LOCK:
    FIND referencia-ext OF ref-item-ext NO-LOCK NO-ERROR.
    IF NOT AVAIL referencia-ext THEN NEXT.
    IF ref-item-ext.cod-obsoleto <> referencia-ext.cod-obsoleto OR
       ref-item-ext.colecao      <> referencia-ext.colecao OR
       ref-item-ext.cod-fundo    <> referencia-ext.cod-fundo OR
       ref-item-ext.cor          <> referencia-ext.cor THEN
       PUT referencia-ext.cod-refer ";"
           referencia-ext.cod-obsoleto ";"
           referencia-ext.colecao ";"
           referencia-ext.cod-fundo ";"
           referencia-ext.cor ";"
           ref-item-ext.it-codigo ";"
           ref-item-ext.cod-obsoleto ";"
           ref-item-ext.colecao ";"     
           ref-item-ext.cod-fundo ";"    
           ref-item-ext.cor          
           SKIP.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "excel.exe", input "c:\lixo\lixo.csv").
delete procedure h-prog.
