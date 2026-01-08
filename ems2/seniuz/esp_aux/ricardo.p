DEF VAR de-qt-conv  AS DEC.
DEF VAR de-tot-conv AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-tot-norm AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-tot-vlr  AS DEC FORMAT ">>>,>>>,>>9.99".

OUTPUT TO "c:\lixo\fatur.csv".
PUT "Item;" "Descricao;" "Qtd-Normal;" "Qtd-Convert;" "Valor" SKIP.

for each it-nota-fisc
    where it-nota-fisc.cod-estabel  =  "2"
      and it-nota-fisc.it-codigo    >= "5"
      AND it-nota-fisc.it-codigo    <= "5z"
      and it-nota-fisc.dt-emis-nota >= 12/01/2007
      and it-nota-fisc.dt-emis-nota <= 12/31/2007
      and it-nota-fisc.dt-cancela   =  ? 
    NO-LOCK,

    EACH item-ext WHERE ITEM-ext.it-codigo = it-nota-fisc.it-codigo
                   /* AND item-ext.indigo    = YES*/
                  NO-LOCK,

    EACH nota-fiscal 
    where nota-fiscal.cod-estabel  = it-nota-fisc.cod-estabel
      and nota-fiscal.nr-nota-fis  = it-nota-fisc.nr-nota-fis
      and nota-fiscal.serie        = it-nota-fisc.serie
      and nota-fiscal.emite-duplic = yes
    NO-LOCK,

    EACH emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente
                   /* AND emitente.natureza     < 3*/
                  NO-LOCK
    BREAK BY it-nota-fisc.it-codigo:

    /*------ Conversao de Kg para M ------- */
    if it-nota-fisc.un-fatur[1] <> "m" then
       assign de-qt-conv = it-nota-fisc.qt-faturada[1] * 
                           item-ext.fator-conv.
    ELSE
       ASSIGN de-qt-conv = it-nota-fisc.qt-faturada[1].

    ASSIGN de-tot-norm = de-tot-norm + it-nota-fisc.qt-faturada[1]
           de-tot-conv = de-tot-conv + de-qt-conv
           de-tot-vlr  = de-tot-vlr  + it-nota-fisc.vl-tot-item.

    IF LAST-OF(it-nota-fisc.it-codigo) THEN DO:
       FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
       put it-nota-fisc.it-codigo ";"
           ITEM.desc-item FORMAT "x(36)" ";"
           it-nota-fisc.un-fatur[1] ";"
           de-tot-norm ";"
           de-tot-conv ";"
           de-tot-vlr
           SKIP.
       ASSIGN de-tot-conv = 0
              de-tot-norm = 0
              de-tot-vlr  = 0.
    END.
END.
OUTPUT CLOSE.
