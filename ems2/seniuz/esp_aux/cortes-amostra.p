OUTPUT TO "c:/temp/cortes.csv" CONVERT SOURCE "ibm850".
PUT "Repres;Cliente;Nota;Seq;Dt-Emiss;Item;Descricao;Quantid;Preco;Dup;Observacoes" SKIP.

DEF VAR c-observ AS CHAR FORMAT "x(2000)".

FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  <= "2"
                       AND nota-fiscal.serie        =  "3"
                       AND nota-fiscal.dt-emis-nota >= 02/01/2011
                       AND nota-fiscal.dt-emis-nota <= 02/28/2011
                       AND nota-fiscal.dt-cancela   =  ?
                     NO-LOCK,
    EACH it-nota-fisc OF nota-fiscal NO-LOCK,
    EACH ped-item-res WHERE
         ped-item-res.cod-estabel = nota-fiscal.cod-estabel AND
         ped-item-res.serie       = nota-fiscal.serie AND
         ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis) AND
         ped-item-res.faturado    = YES AND
         ped-item-res.lote BEGINS "CA"
         NO-LOCK
    BREAK BY nota-fiscal.no-ab-reppri
          BY nota-fiscal.nome-ab-cli
          BY it-nota-fisc.nr-nota-fis
          BY it-nota-fisc.nr-seq-fat:

    IF FIRST-OF(it-nota-fisc.nr-seq-fat) THEN DO:
       FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
       FIND ped-venda WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                        AND ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                      NO-LOCK.
       ASSIGN c-observ = replace(replace(ped-venda.observacoes, chr(13), " "), chr(10), " ").
       PUT nota-fiscal.no-ab-reppri ";"
           nota-fiscal.nome-ab-cli ";"
           nota-fiscal.nr-nota-fis ";"
           it-nota-fisc.nr-seq-fat ";"
           nota-fiscal.dt-emis-nota ";"
           it-nota-fisc.it-codigo ";"
           ITEM.desc-item FORMAT "x(30)" ";"
           it-nota-fisc.qt-faturada[1] ";"
           it-nota-fisc.vl-preuni ";"
           nota-fiscal.emite-dup ";"
           c-observ
           SKIP.
    END.
END.
OUTPUT CLOSE.
