/* Programa: referencia-ext.p
*/
def var h-acomp as handle no-undo.

DEF TEMP-TABLE tt-referencia
    FIELD cod-refer LIKE referencia-ext.cod-refer
    INDEX ch-refer cod-refer.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Atualizando *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

output to c:/temp/referencia-ext.csv.
PUT "Refer;COb;Colecao;Fundo;Cor;Estilo-desenho" SKIP.

FOR EACH ped-item WHERE ped-item.cod-sit-item < 3
                     OR ped-item.cod-sit-item = 5
                  NO-LOCK:

    RUN pi-acompanhar IN h-acomp (INPUT "Item: " + ped-item.it-codigo + 
                                        " Refer: " + ped-item.cod-refer + 
                                        " Sit: " + STRING(ped-item.cod-sit-item)). 
    
    FIND FIRST tt-referencia WHERE tt-referencia.cod-refer = ped-item.cod-refer NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-referencia THEN DO:
       CREATE tt-referencia.
       ASSIGN tt-referencia.cod-refer = ped-item.cod-refer.
    END.
END.
FOR EACH saldo-estoq WHERE saldo-estoq.qtidade-atu <> 0
                     NO-LOCK,
    EACH ITEM WHERE ITEM.it-codigo = saldo-estoq.it-codigo
                AND (ITEM.ge-codigo >= 50 AND ITEM.ge-codigo <= 59)
              NO-LOCK:

    RUN pi-acompanhar IN h-acomp (INPUT "Item: " + saldo-estoq.it-codigo + 
                                        " Refer: " + saldo-estoq.cod-refer + 
                                        " Saldo: " + STRING(saldo-estoq.qtidade-atu)). 
    
    IF saldo-estoq.cod-refer <> "" THEN DO:
       FIND FIRST tt-referencia WHERE tt-referencia.cod-refer = saldo-estoq.cod-refer NO-LOCK NO-ERROR.
       IF NOT AVAIL tt-referencia THEN DO:
          CREATE tt-referencia.
          ASSIGN tt-referencia.cod-refer = saldo-estoq.cod-refer.
       END.
    END.
END.

for each referencia-ext no-lock:
    FIND tt-referencia WHERE tt-referencia.cod-refer = referencia-ext.cod-refer NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-referencia THEN NEXT.

    put referencia-ext.cod-refer ";"
        referencia-ext.cod-obsoleto ";"
        referencia-ext.colecao ";"
        referencia-ext.cod-fundo ";"
        referencia-ext.cor ";"
        referencia-ext.estilo-desenho
        skip.
end.
output close.
