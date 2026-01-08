FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao = 2 AND
         ob-etiqueta.nr-reporte = 0 AND
         ob-etiqueta.tipo-ordem = 1 AND
         ob-etiqueta.quantidade > 0 AND 
         ob-etiqueta.localiz <> '' 
         BY ob-etiqueta.dt-emissao
         BY ob-etiqueta.hr-emissao.

    DISP ob-etiqueta.num-etiqueta
         ob-etiqueta.dt-emiss.
    
    ASSIGN ob-etiqueta.situacao = 3.
    
END.
