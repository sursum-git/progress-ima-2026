FOR EACH ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = '1' AND
         ob-etiqueta.situacao    = 2   AND
         ob-etiqueta.dt-emissao  = TODAY AND
         ob-etiqueta.acondic     = 'saco' NO-LOCK
      BY ob-etiqueta.nr-ob
      BY ob-etiqueta.num-etiqueta.
    DISP ob-etiqueta.dt-ob
         ob-etiqueta.nr-ob
         ob-etiqueta.nr-carro
         ob-etiqueta.it-codigo
         ob-etiqueta.cod-refer
         ob-etiqueta.resp-revisao 
         ob-etiqueta.num-etiqueta
         WITH WIDTH 500.
END.
