DEF VAR de-qtd AS DEC FORMAT ">>,>>>,>>9.99".
DEF VAR de-vlr AS DEC FORMAT ">>,>>>,>>9.99".

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.txt" CONVERT SOURCE "ibm850".

FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  =  "2"
                       AND nota-fiscal.serie        =  "1"
                       AND nota-fiscal.dt-emis-nota >= 01/01/2006
                       AND nota-fiscal.dt-cancela    = ?
                       AND nota-fiscal.esp-docto     = 22
                       AND nota-fiscal.emite-dup     = YES
                       AND nota-fiscal.nat-operacao BEGINS "7"
                     NO-LOCK,
    EACH emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente
                    AND emitente.pais         <> "brasil"
                  NO-LOCK,
    EACH it-nota-fisc OF nota-fiscal NO-LOCK

    BREAK BY it-nota-fisc.it-codigo
          /*BY SUBSTR(it-nota-fisc.cod-refer,7,1)*/
          BY STRING(YEAR(nota-fiscal.dt-emis-nota))
          BY STRING(MONTH(nota-fiscal.dt-emis-nota)):

    ASSIGN de-qtd = de-qtd + it-nota-fisc.qt-faturada[1]
           de-vlr = de-vlr + (it-nota-fisc.qt-faturada[1] * it-nota-fisc.vl-preuni).

    IF LAST-OF(STRING(MONTH(nota-fiscal.dt-emis-nota))) THEN DO:
       FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
       DISP ITEM.it-codigo
            ITEM.desc-item FORMAT "x(36)"
            /*SUBSTR(it-nota-fisc.cod-refer,7,1)*/
            STRING(MONTH(nota-fiscal.dt-emis-nota))
            STRING(YEAR(nota-fiscal.dt-emis-nota))
            de-qtd(TOTAL)
            de-vlr(TOTAL)
            WITH WIDTH 150.
       ASSIGN de-qtd = 0
              de-vlr = 0.
    END.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.
