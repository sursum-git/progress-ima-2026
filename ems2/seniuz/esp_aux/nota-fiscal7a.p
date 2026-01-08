DEF VAR tot-icms AS DEC.
DEF VAR c-chave  AS CHAR FORMAT "999999".
DEF VAR c-itens  AS CHAR.

DEF TEMP-TABLE tt-resumo
    FIELD ano-mes    AS CHAR FORMAT "999999"
    FIELD valor-nota AS DEC FORMAT ">>>,>>>,>>9.99"
    FIELD valor-icms AS DEC FORMAT ">>>,>>>,>>9.99"
    INDEX ch-resumo ano-mes.

DEF TEMP-TABLE tt-itens
    FIELD it-codigo LIKE it-nota-fisc.it-codigo
    INDEX ch-itens it-codigo.

OUTPUT TO c:/temp/it-nota-fisc.csv CONVERT SOURCE "ibm850".
PUT "Num.NF;Serie;Data;Cliente;Valor;Val.ICMS;Itens" SKIP.
FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  = '2'
                       AND nota-fiscal.serie        = '1'
                       AND nota-fiscal.dt-emis-nota >= 05/01/2005
                       AND nota-fiscal.dt-emis-nota <= 04/30/2010
                       AND nota-fiscal.dt-cancela   =  ?
                       AND nota-fiscal.esp-docto    = 22 /* nfs */
                     NO-LOCK:
    FIND FIRST it-nota-fisc OF nota-fiscal NO-LOCK.
    FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
 /* IF ITEM.ge-codigo >= 50 AND ITEM.ge-codigo <= 59 THEN DO: */
       FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente
                     NO-LOCK.
       ASSIGN tot-icms = 0.
       FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
           ASSIGN tot-icms = tot-icms + it-nota-fisc.vl-icms-it.
       END.
       PUT STRING(nota-fiscal.nr-nota-fis,"9999999") ";"
           nota-fiscal.serie ";"
           nota-fiscal.dt-emis-nota ";"
           emitente.nome-emit ";"
           nota-fiscal.vl-tot-nota ";"
           tot-icms ";".
       ASSIGN c-chave = STRING(YEAR(nota-fiscal.dt-emis-nota),"9999") +
                        STRING(MONTH(nota-fiscal.dt-emis-nota),"99").
       FIND FIRST tt-resumo WHERE tt-resumo.ano-mes = c-chave NO-ERROR.
       IF NOT AVAIL(tt-resumo) THEN DO:
          CREATE tt-resumo.
          ASSIGN tt-resumo.ano-mes = c-chave.
       END.
       ASSIGN tt-resumo.valor-nota = tt-resumo.valor-nota + nota-fiscal.vl-tot-nota
              tt-resumo.valor-icms = tt-resumo.valor-icms + tot-icms.

       FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
           FIND tt-itens WHERE tt-itens.it-codigo = it-nota-fisc.it-codigo NO-ERROR.
           IF NOT AVAIL tt-itens THEN DO:
              CREATE tt-itens.
              ASSIGN tt-itens.it-codigo = it-nota-fisc.it-codigo.
           END.
       END.
       ASSIGN c-itens = "".
       FOR EACH tt-itens:
           FIND ITEM WHERE ITEM.it-codigo = tt-itens.it-codigo NO-LOCK.
           ASSIGN c-itens = c-itens + tt-itens.it-codigo + "-" + ITEM.desc-item + " ".
       END.
       PUT UNFORMAT c-itens SKIP.
       FOR EACH tt-itens.
           DELETE tt-itens.
       END.
  /*END.*/
END.
PUT SKIP(1)
    "Resumo" SKIP
    "Ano;Mˆs;Valor Nota;Valor ICMS" SKIP.
FOR EACH tt-resumo:
    PUT SUBSTR(tt-resumo.ano-mes,1,4) ";"
        SUBSTR(tt-resumo.ano-mes,5,2) ";"
        tt-resumo.valor-nota ";"
        tt-resumo.valor-icms
        SKIP.
END.

OUTPUT CLOSE.
