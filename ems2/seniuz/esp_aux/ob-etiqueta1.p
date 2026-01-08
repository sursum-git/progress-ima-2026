/*OUTPUT TO c:/temp/lixo.txt CONVERT SOURCE "ibm850".*/
FOR EACH ordem-benefic WHERE ordem-benefic.nr-ob >= 120712
                         AND ordem-benefic.nr-ob <= 120715 NO-LOCK.
    FOR EACH ob-etiqueta OF ordem-benefic
        WHERE ob-etiqueta.cod-qualid <> "b".
        DISP ob-etiqueta.nr-ob
             ob-etiqueta.it-codigo
             ob-etiqueta.cod-refer
             ob-etiqueta.num-etiqueta
             ob-etiqueta.cod-qualid
             ob-etiqueta.quantidade(TOTAL) WITH WIDTH 130.
        UPDATE ob-etiqueta.cod-qualid.
    END.
END.
/*OUTPUT CLOSE.*/
