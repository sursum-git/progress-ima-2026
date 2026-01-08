FOR EACH ob-pcp-ref.
    FIND ob-pcp OF ob-pcp-ref.
         
    ASSIGN ob-pcp-ref.situacao = 1.

    ASSIGN ob-pcp-ref.observ = "Programa‡Æo: " + STRING(ob-pcp-ref.num-prog) + CHR(13) + 
                               ob-pcp-ref.observ.

    FIND ref-item-ext WHERE
         ref-item-ext.it-codigo = ob-pcp.it-codigo AND
         ref-item-ext.cod-refer = ob-pcp-ref.cod-refer
         NO-ERROR.

    IF ref-item-ext.qtd-prog = 0 AND
       ref-item-ext.qtd-proc = 0 AND
       ref-item-ext.qtd-pron = 0 THEN DO.
       ASSIGN ob-pcp-ref.qtd-sld-prog = 0
              ob-pcp-ref.qtd-proc = 0
              ob-pcp-ref.qtd-pron = 0
              ob-pcp-ref.situacao = 2.
    END.
    ELSE DO.
       ASSIGN ob-pcp-ref.qtd-sld-prog = ob-pcp-ref.qtd-prog - ob-pcp-ref.qtd-proc - ob-pcp-ref.qtd-pron.

       IF ob-pcp-ref.qtd-sld-prog = 0 AND
          ob-pcp-ref.qtd-proc = 0 AND
          ob-pcp-ref.qtd-pron = 0 THEN
          ASSIGN ob-pcp-ref.situacao = 2.
    END.
END.


