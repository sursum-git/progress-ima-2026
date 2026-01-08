DEF VAR c-situacao AS CHAR FORMAT "x(10)".

FIND nota-fiscal WHERE nota-fiscal.cod-estabel = '2'
                   AND nota-fiscal.serie       = '1'
                   AND nota-fiscal.nr-nota-fis = '0111592'
                 NO-LOCK NO-ERROR.

FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK,
    EACH ped-item-rom WHERE ped-item-rom.nome-abrev   = it-nota-fisc.nome-ab-cli
                        AND ped-item-rom.nr-pedcli    = it-nota-fisc.nr-pedcli
                        AND ped-item-rom.nr-sequencia = it-nota-fisc.nr-seq-ped
                      NO-LOCK,
    FIRST ob-etiqueta WHERE ob-etiqueta.nr-ob        = ped-item-rom.nr-ob
                        AND ob-etiqueta.nr-sequencia = ped-item-rom.nr-seq-etq
                      NO-LOCK:
    FIND ped-item-res USE-INDEX INDICE1
        WHERE ped-item-res.nome-abrev   = ped-item-rom.nome-abrev AND
              ped-item-res.nr-pedcli    = ped-item-rom.nr-pedcli   AND
              ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia
        NO-LOCK NO-ERROR.

    FIND ITEM WHERE ITEM.it-codigo = ob-etiqueta.it-codigo.
    {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao} 
    DISPLAY ob-etiqueta.nr-ob
            ob-etiqueta.num-etiqueta(COUNT)
            ob-etiqueta.localizacao
            /*substr(ob-etiqueta.un,20,10)*/
            c-situacao
            ped-item-res.nr-nota-fis
            ITEM.desc-item FORMAT "x(36)"
            WITH WIDTH 150.
END.

