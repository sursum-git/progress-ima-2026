OUTPUT TO c:/temp/fatur-ano-regiao.csv.

def var h-acomp as handle no-undo.

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Atualizando *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

DEFINE TEMP-TABLE tt-fatur
       FIELD ano        AS CHAR FORMAT "9999"
       FIELD regiao     AS CHAR FORMAT "x(12)"
       FIELD unidade    LIKE it-nota-fisc.un-fatur[1]
       FIELD quantidade AS DEC FORMAT ">>,>>>,>>>,>>9.99"
       FIELD valor      AS DEC FORMAT ">>,>>>,>>>,>>9.99"
       INDEX ch-fatur ano regiao unidade.

DEFINE VAR c-regiao AS CHAR FORMAT "x(12)".

FOR EACH nota-fiscal WHERE nota-fiscal.dt-cancela = ?
                       AND nota-fiscal.esp-docto  = 22 
                       AND nota-fiscal.emite-dup  = YES
                     NO-LOCK:

    RUN pi-acompanhar IN h-acomp (INPUT "Estab: " + nota-fiscal.cod-estabel + 
                                        " Nota: " + nota-fiscal.nr-nota-fis + 
                                        " Data: " + STRING(nota-fiscal.dt-emis-nota)). 

    FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.
    
    IF emitente.estado = "GO" OR
       emitente.estado = "MT" OR
       emitente.estado = "MS" OR
       emitente.estado = "DF" THEN
       ASSIGN c-regiao = "centro-oeste".
    ELSE
    IF emitente.estado = "MA" OR
       emitente.estado = "PI" OR
       emitente.estado = "CE" OR
       emitente.estado = "RN" OR
       emitente.estado = "PB" OR
       emitente.estado = "PE" OR
       emitente.estado = "AL" OR
       emitente.estado = "SE" OR 
       emitente.estado = "BA" THEN
       ASSIGN c-regiao = "nordeste".
    ELSE
    IF emitente.estado = "AC" OR 
       emitente.estado = "AM" OR 
       emitente.estado = "RO" OR 
       emitente.estado = "RR" OR 
       emitente.estado = "PA" OR 
       emitente.estado = "AP" OR 
       emitente.estado = "TO" THEN 
       ASSIGN c-regiao = "norte".
    ELSE
    IF emitente.estado = "MG" OR
       emitente.estado = "ES" OR
       emitente.estado = "RJ" OR
       emitente.estado = "SP" THEN
       ASSIGN c-regiao = "sudeste".
    ELSE
    IF emitente.estado = "PR" OR
       emitente.estado = "SC" OR
       emitente.estado = "RS" THEN
       ASSIGN c-regiao = "sul".
    ELSE
       ASSIGN c-regiao = "exterior".

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
        FIND FIRST tt-fatur WHERE tt-fatur.ano     = STRING(YEAR(nota-fiscal.dt-emis-nota),"9999")
                              AND tt-fatur.regiao  = c-regiao
                              AND tt-fatur.unidade = it-nota-fisc.un-fatur[1]
                            NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-fatur THEN DO:
           CREATE tt-fatur.
           ASSIGN tt-fatur.ano     = STRING(YEAR(nota-fiscal.dt-emis-nota),"9999") 
                  tt-fatur.regiao  = c-regiao                                       
                  tt-fatur.unidade = it-nota-fisc.un-fatur[1].
        END.
        ASSIGN tt-fatur.quantidade = tt-fatur.quantidade + it-nota-fisc.qt-faturada[1]
               tt-fatur.valor      = tt-fatur.valor + it-nota-fisc.qt-faturada[1] * it-nota-fisc.vl-preuni.
    END.
END.

FOR EACH tt-fatur:
    PUT tt-fatur.ano ";"
        tt-fatur.regiao ";"
        tt-fatur.unidade ";"
        tt-fatur.quantidade ";"
        tt-fatur.valor
        SKIP.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "excel.exe", input "c:\temp\fatur-ano-regiao.csv").
delete procedure h-prog.
