DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-estabelec-dest LIKE estabelec.


DEF TEMP-TABLE tt-notas LIKE movadm.nota-fiscal.

DEF OUTPUT PARAMETER TABLE FOR tt-notas.
DEF INPUT PARAMETER p-cod-emit LIKE movadm.nota-fiscal.cod-emit.
DEF INPUT PARAMETER p-data-inicio LIKE movadm.nota-fiscal.dt-emis-nota.

FOR EACH tt-estabelec-dest NO-LOCK.
    FOR EACH movadm.nota-fiscal WHERE
             movadm.nota-fiscal.cod-estabel = tt-estabelec-dest.cod-estab AND
             movadm.nota-fiscal.cod-emitente = p-cod-emit AND
             movadm.nota-fiscal.dt-emis-nota >= p-data-inicio  NO-LOCK
             BREAK BY YEAR(movadm.nota-fiscal.dt-emis-nota)                      
                   BY MONTH(movadm.nota-fiscal.dt-emis-nota):                            

        IF movadm.nota-fiscal.dt-cancela <> ? THEN NEXT.

        FIND movadm.ped-venda WHERE
             movadm.ped-venda.nome-abrev = movadm.nota-fiscal.nome-ab-cli AND
             movadm.ped-venda.nr-pedcli = movadm.nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.
    
        IF NOT AVAIL movadm.ped-venda THEN NEXT.
    
        FIND tt-notas WHERE
             tt-notas.cod-estabel = movadm.nota-fiscal.cod-estabel AND
             tt-notas.serie  = movadm.nota-fiscal.serie AND
             tt-notas.nr-nota-fis = movadm.nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
              
        IF NOT AVAIL tt-notas THEN DO.
           CREATE tt-notas.
           BUFFER-COPY movadm.nota-fiscal TO tt-notas.
        END.
    END.

    FOR EACH dbaux.nota-fiscal WHERE
             dbaux.nota-fiscal.cod-estabel = tt-estabelec-dest.cod-estab AND
             dbaux.nota-fiscal.cod-emitente = p-cod-emit AND
             dbaux.nota-fiscal.dt-emis-nota >= p-data-inicio  NO-LOCK
             BREAK BY YEAR(dbaux.nota-fiscal.dt-emis-nota)                      
                   BY MONTH(dbaux.nota-fiscal.dt-emis-nota):                            
    
        IF dbaux.nota-fiscal.dt-cancela <> ? THEN NEXT.

        FIND dbaux.ped-venda WHERE
             dbaux.ped-venda.nome-abrev = dbaux.nota-fiscal.nome-ab-cli AND
             dbaux.ped-venda.nr-pedcli = dbaux.nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.
    
        IF NOT AVAIL dbaux.ped-venda THEN NEXT.
    
        FIND tt-notas WHERE
             tt-notas.cod-estabel = dbaux.nota-fiscal.cod-estabel AND
             tt-notas.serie  = dbaux.nota-fiscal.serie AND
             tt-notas.nr-nota-fis = dbaux.nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
              
        IF NOT AVAIL tt-notas THEN DO.
           CREATE tt-notas.
           BUFFER-COPY dbaux.nota-fiscal TO tt-notas.
        END.
    END.
END.

DISCONNECT dbaux.




