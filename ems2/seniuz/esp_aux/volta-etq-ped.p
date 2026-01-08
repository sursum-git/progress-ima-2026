FIND ped-venda 266662 NO-LOCK.
FOR EACH ped-item OF ped-venda.
    DISP ped-item.it-codigo
         INT(ped-item.cod-sit-item ).

    FOR EACH ped-item-rom WHERE
             ped-item-rom.nome-abrev = ped-item.nome-abrev AND
             ped-item-rom.nr-pedcli = ped-item.nr-pedcli AND
             ped-item-rom.nr-sequencia = ped-item .nr-sequencia
             EXCLUSIVE-LOCK.

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel = ped-venda .cod-estab AND
             ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta NO-ERROR.

        IF NOT AVAIL ob-etiqueta THEN NEXT.

        DISP ob-etiqueta.num-etiqueta
             int(ob-etiqueta.situacao).

         /*
        IF AVAIL ob-etiqueta THEN 
           ASSIGN ob-etiqueta.situacao = 3
                  ob-etiqueta.ob-origem = "".
          */ 
        //DELETE ped-item-rom.

    END.

END.
