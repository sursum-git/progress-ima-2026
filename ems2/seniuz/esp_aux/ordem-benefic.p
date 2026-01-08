FOR EACH ordem-benefic WHERE ordem-benefic.it-codigo =  "501521" NO-LOCK,
    EACH ob-etiqueta OF ordem-benefic WHERE 
         ob-etiqueta.dt-emissao >= 10/29/2005 NO-LOCK
    BREAK BY ob-etiqueta.qualidade:
    
    ACCUMULATE ob-etiqueta.quantidade (TOTAL BY ob-etiqueta.qualidade).

    IF LAST-OF(ob-etiqueta.qualidade) THEN
       DISPLAY ob-etiqueta.qualidade
               (ACCUM TOTAL BY ob-etiqueta.qualidade ob-etiqueta.quantidade).

END.
