DEF VAR i-cont AS INT.
DEF BUFFER b-ped-item-rom FOR ped-item-rom.
DEF VAR l-fat1 AS LOG FORMAT "Sim/Nao".
DEF VAR l-fat2 AS LOG FORMAT "Sim/Nao".

OUTPUT TO "c:/lixo/lixo.txt".

FOR EACH ob-etiqueta NO-LOCK.
    ASSIGN i-cont = 0.
    FOR EACH ped-item-rom USE-INDEX indice2                       
        WHERE ped-item-rom.nr-ob      = ob-etiqueta.nr-ob         
          AND ped-item-rom.nr-seq-etq = ob-etiqueta.nr-sequencia          
        NO-LOCK:
        FIND ped-item-res WHERE ped-item-res.nome-abrev   = ped-item-rom.nome-abrev     
                            AND ped-item-res.nr-pedcli    = ped-item-rom.nr-pedcli     
                            AND ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia  
                          NO-LOCK NO-ERROR.                                            
        IF AVAIL ped-item-res THEN
           ASSIGN l-fat1 = ped-item-res.faturado.
        ELSE
           ASSIGN l-fat1 = NO.

        FIND FIRST b-ped-item-rom USE-INDEX indice2                       
        WHERE b-ped-item-rom.nr-ob      = ob-etiqueta.nr-ob         
          AND b-ped-item-rom.nr-seq-etq = ob-etiqueta.nr-sequencia          
          AND (b-ped-item-rom.nome-abrev <> ped-item-rom.nome-abrev OR
               b-ped-item-rom.nr-pedcli  <> ped-item-rom.nr-pedcli)
        NO-LOCK NO-ERROR.
        
        ASSIGN l-fat2 = NO.
        IF AVAIL b-ped-item-rom THEN DO:
           FIND ped-item-res WHERE ped-item-res.nome-abrev   = b-ped-item-rom.nome-abrev     
                               AND ped-item-res.nr-pedcli    = b-ped-item-rom.nr-pedcli     
                               AND ped-item-res.nr-sequencia = b-ped-item-rom.nr-sequencia  
                             NO-LOCK NO-ERROR.                                            
           IF AVAIL ped-item-res THEN
              ASSIGN l-fat2 = ped-item-res.faturado.

           DISP ped-item-rom.nome-abrev
                ped-item-rom.nr-pedcli
                ped-item-rom.nr-sequencia
                l-fat1
                " x "
                b-ped-item-rom.nome-abrev    
                b-ped-item-rom.nr-pedcli     
                b-ped-item-rom.nr-sequencia
                l-fat2
                WITH WIDTH 120.
        END.    
        ASSIGN i-cont = i-cont + 1.
    END.
END.
DISP i-cont.
OUTPUT CLOSE.
