DEF VAR de-qt-conv  AS DEC.
DEF VAR de-tot-conv AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-tot-norm AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-tot-vlr  AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR da-emis-ini LIKE it-nota-fisc.dt-emis-nota.
DEF VAR da-emis-fin LIKE it-nota-fisc.dt-emis-nota.
DEF VAR i-mercado   AS INT VIEW-AS RADIO-SET 
                                   RADIO-BUTTONS "Interno",1,"Externo",2,"Todos",3.

UPDATE da-emis-ini
       da-emis-fin
       i-mercado.

OUTPUT TO "c:\lixo\fatur.csv".
PUT "Tear Textil - Faturamento - Periodo: " da-emis-ini " a " da-emis-fin 
    " - Mercado: " IF i-mercado = 1 THEN "Interno" 
                                    ELSE IF i-mercado = 2 THEN "Externo"
                                                          ELSE "Todos"
    SKIP(1)
    "Repres;" "Item;" "Descricao;" "Un;" "Qtd-Normal;" "Qtd-Convert;" "Valor;" "Indigo" SKIP.

for each it-nota-fisc
    where it-nota-fisc.cod-estabel  =  "2"
      and it-nota-fisc.it-codigo    >= "5"
      AND it-nota-fisc.it-codigo    <= "5z"
      and it-nota-fisc.dt-emis-nota >= da-emis-ini
      and it-nota-fisc.dt-emis-nota <= da-emis-fin
      and it-nota-fisc.dt-cancela   =  ? 
    NO-LOCK,

    EACH item-ext WHERE ITEM-ext.it-codigo = it-nota-fisc.it-codigo
                  NO-LOCK,

    EACH nota-fiscal 
    where nota-fiscal.cod-estabel  = it-nota-fisc.cod-estabel
      and nota-fiscal.nr-nota-fis  = it-nota-fisc.nr-nota-fis
      and nota-fiscal.serie        = it-nota-fisc.serie
      and nota-fiscal.emite-duplic = yes
    NO-LOCK,

    EACH emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente
                    AND (emitente.natureza < 3 AND i-mercado = 1 OR
                         emitente.natureza > 2 AND i-mercado = 2 OR
                                                   i-mercado = 3)
                  NO-LOCK
    BREAK BY nota-fiscal.no-ab-reppri
          BY it-nota-fisc.it-codigo:

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
       put nota-fiscal.no-ab-reppri ";"
           it-nota-fisc.it-codigo ";"
           ITEM.desc-item FORMAT "x(36)" ";"
           it-nota-fisc.un-fatur[1] ";"
           de-tot-norm ";"
           de-tot-conv ";"
           de-tot-vlr ";"
           IF item-ext.indigo THEN "Sim"
                              ELSE "Nao"
           SKIP.
       ASSIGN de-tot-conv = 0
              de-tot-norm = 0
              de-tot-vlr  = 0.
    END.
END.
OUTPUT CLOSE.
