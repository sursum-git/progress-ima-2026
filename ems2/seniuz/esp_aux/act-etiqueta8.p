FOR EACH ob-etiqueta WHERE
         ob-etiqueta.num-etiqueta = 105143.

    FIND ordem-benefic OF ob-etiqueta NO-LOCK NO-ERROR.

    IF NOT AVAIL ordem-benefic THEN DO.
       CREATE ordem-benefic.
       ASSIGN ordem-benefic.nr-ob = ob-etiqueta.nr-ob
              ordem-benefic.dt-ob = ob-etiqueta.dt-emissao
              ordem-benefic.nr-carro = ob-etiqueta.nr-carro
              ordem-benefic.responsavel = 'super'
              ordem-benefic.tipo-ordem = 2
              ordem-benefic.it-codigo = ob-etiqueta.it-codigo
              ordem-benefic.cod-refer = ob-etiqueta.cod-refer
              ordem-benefic.quantidade = 1000
              ordem-benefic.situacao = 5.
    END. 
END.
