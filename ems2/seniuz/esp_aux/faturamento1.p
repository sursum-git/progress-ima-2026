OUTPUT TO c:/temp/fatur-transp.csv.
PUT "Ano;Mes;Indigo;Cidade;Estado;Transp;Un;Quantidade;Valor;Peso" SKIP.

def var h-acomp as handle no-undo.

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Atualizando *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

DEFINE TEMP-TABLE tt-fatur
       FIELD ano        AS CHAR FORMAT "9999"
       FIELD mes        AS CHAR FORMAT "99"
       FIELD indigo     AS LOG FORMAT "Sim/Nao"
       FIELD cidade     LIKE emitente.cidade
       FIELD estado     LIKE emitente.estado
       FIELD transp     LIKE transporte.nome-abrev
       FIELD unidade    LIKE it-nota-fisc.un-fatur[1]
       FIELD quantidade AS DEC FORMAT ">>,>>>,>>>,>>9.99"
       FIELD valor      AS DEC FORMAT ">>,>>>,>>>,>>9.99"
       FIELD peso-bruto AS DEC FORMAT ">>,>>>,>>>,>>9.99"
       INDEX ch-fatur ano mes indigo cidade estado transp unidade.

DEF VAR l-indigo AS LOG.

FOR EACH nota-fiscal WHERE nota-fiscal.dt-cancela = ?
                       AND nota-fiscal.dt-emis-nota >= 06/01/2009
                       AND nota-fiscal.dt-emis-nota <= 12/31/2009
                       AND nota-fiscal.esp-docto  = 22 
                       AND nota-fiscal.emite-dup  = YES
                     NO-LOCK:

    RUN pi-acompanhar IN h-acomp (INPUT "Estab: " + nota-fiscal.cod-estabel + 
                                        " Nota: " + nota-fiscal.nr-nota-fis + 
                                        " Data: " + STRING(nota-fiscal.dt-emis-nota)). 

    FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.
    
    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
        
        FIND item-ext WHERE item-ext.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL item-ext AND item-ext.indigo = YES THEN
           ASSIGN l-indigo = YES.
        ELSE
           ASSIGN l-indigo = NO.

        FIND FIRST tt-fatur WHERE tt-fatur.ano     = STRING(YEAR(nota-fiscal.dt-emis-nota),"9999")
                              AND tt-fatur.mes     = STRING(MONTH(nota-fiscal.dt-emis-nota),"99")
                              AND tt-fatur.indigo  = l-indigo
                              AND tt-fatur.cidade  = emitente.cidade
                              AND tt-fatur.estado  = emitente.estado
                              AND tt-fatur.transp  = nota-fiscal.nome-transp
                              AND tt-fatur.unidade = it-nota-fisc.un-fatur[1]
                            NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-fatur THEN DO:
           CREATE tt-fatur.
           ASSIGN tt-fatur.ano     = STRING(YEAR(nota-fiscal.dt-emis-nota),"9999")
                  tt-fatur.mes     = STRING(MONTH(nota-fiscal.dt-emis-nota),"99")   
                  tt-fatur.indigo  = l-indigo                                       
                  tt-fatur.cidade  = emitente.cidade                                
                  tt-fatur.estado  = emitente.estado                                
                  tt-fatur.transp  = nota-fiscal.nome-transp                          
                  tt-fatur.unidade = it-nota-fisc.un-fatur[1].
        END.
        ASSIGN tt-fatur.quantidade = tt-fatur.quantidade + it-nota-fisc.qt-faturada[1]
               tt-fatur.valor      = tt-fatur.valor + it-nota-fisc.qt-faturada[1] * it-nota-fisc.vl-preuni
               tt-fatur.peso-bruto = tt-fatur.peso-bruto + it-nota-fisc.peso-bruto.
    END.
END.

FOR EACH tt-fatur:
    PUT tt-fatur.ano ";"
        tt-fatur.mes ";"
        tt-fatur.indigo ";"
        tt-fatur.cidade ";"
        tt-fatur.estado ";"
        tt-fatur.transp ";"
        tt-fatur.unidade ";"
        tt-fatur.quantidade ";"
        tt-fatur.valor ";"
        tt-fatur.peso-bruto
        SKIP.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "excel.exe", input "c:\temp\fatur-transp.csv").
delete procedure h-prog.
