OUTPUT TO c:/temp/lixo.csv.
PUT "Item;Descricao;Nuance;Qualidade;Quant(M);Quant(Rolos)" SKIP.

FOR EACH ob-etiqueta WHERE ob-etiqueta.dt-ob >= 02/01/2009
                       AND ob-etiqueta.dt-ob <= 02/28/2009
                     NO-LOCK,
    EACH mov-est-acbm WHERE mov-est-acbm.data-mov  = ob-etiqueta.dt-emissao  
                        AND mov-est-acbm.num-lote  = ob-etiqueta.nr-ob     
                        AND mov-est-acbm.it-codigo = ob-etiqueta.it-codigo
                        AND mov-est-acbm.cod-refer = ob-etiqueta.cod-refer
                      NO-LOCK,
    EACH mov-est-acbd OF mov-est-acbm WHERE mov-est-acbd.cod-tipo-def = "0"
                                        AND mov-est-acbd.cod-defeito  = "06"                    
                                      NO-LOCK,                                                  
    /*
    FIRST mov-est-acbd WHERE mov-est-acbd.num-etiqueta = ob-etiqueta.num-etiqueta
                         and mov-est-acbd.cod-tipo-def = "0"
                         AND mov-est-acbd.cod-defeito  = "06"
                       NO-LOCK,
    */
    EACH item-ext WHERE item-ext.it-codigo = ob-etiqueta.it-codigo
                    AND item-ext.indigo    = YES
                  NO-LOCK
    BREAK BY ob-etiqueta.it-codigo
          BY ob-etiqueta.nuance
          BY ob-etiqueta.cod-qualid:

    ACCUMULATE ob-etiqueta.quantidade (TOTAL BY ob-etiqueta.it-codigo).
    ACCUMULATE ob-etiqueta.quantidade (TOTAL BY ob-etiqueta.nuance).
    ACCUMULATE ob-etiqueta.quantidade (TOTAL BY ob-etiqueta.cod-qualid).
    ACCUMULATE ob-etiqueta.quantidade (TOTAL).
    ACCUMULATE ob-etiqueta.nr-ob (COUNT BY ob-etiqueta.it-codigo).
    ACCUMULATE ob-etiqueta.nr-ob (COUNT BY ob-etiqueta.nuance).
    ACCUMULATE ob-etiqueta.nr-ob (COUNT BY ob-etiqueta.cod-qualid).
    ACCUMULATE ob-etiqueta.nr-ob (COUNT).

    IF LAST-OF(ob-etiqueta.cod-qualid) THEN DO.
       FIND ITEM WHERE ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
       PUT ob-etiqueta.it-codigo ";"
           item.desc-item FORMAT "x(36)" ";" 
           ob-etiqueta.nuance ";"
           ob-etiqueta.cod-qualid ";"
           (ACCUM TOTAL BY ob-etiqueta.cod-qualid ob-etiqueta.quantidade)  ";"
           (ACCUM COUNT BY ob-etiqueta.cod-qualid ob-etiqueta.nr-ob) SKIP.
    END.
    IF LAST-OF(ob-etiqueta.nuance) THEN DO.
       PUT ";Total Nuance;;;"
           (ACCUM TOTAL BY ob-etiqueta.nuance ob-etiqueta.quantidade)  ";"
           (ACCUM COUNT BY ob-etiqueta.nuance ob-etiqueta.nr-ob) SKIP.
    END.
    IF LAST-OF(ob-etiqueta.it-codigo) THEN DO.
       PUT ";Total Item;;;"
           (ACCUM TOTAL BY ob-etiqueta.it-codigo ob-etiqueta.quantidade) ";"
           (ACCUM COUNT BY ob-etiqueta.it-codigo ob-etiqueta.nr-ob) SKIP.
    END.
END.
PUT ";Total Geral;;;"
    (ACCUM TOTAL ob-etiqueta.quantidade) FORMAT ">,>>>,>>9.99" ";"
    (ACCUM COUNT ob-etiqueta.nr-ob) FORMAT ">,>>>,>>9.99" SKIP.

OUTPUT CLOSE.  
