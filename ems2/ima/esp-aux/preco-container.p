DEF TEMP-TABLE tt-itens
    FIELD nr-container LIKE ob-etiqueta.nr-container
    FIELD it-codigo AS CHAR
    FIELD qt-vend AS DEC 
    FIELD qt-est AS DEC
    FIELD vl-vend AS DEC FORMAT ">>>,>>>,>>9.99".


DEF VAR de-qt-vend AS DEC.
DEF VAR de-qt-est AS DEC.
DEF VAR de-tot-pv AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR i-ct AS INT.
DEF VAR i-tot-ct AS INT.

FOR EACH pp-container WHERE 
         pp-container.situacao = 3 AND
         pp-container.dt-receb >= 01.01.2021 NO-LOCK.
    ASSIGN i-tot-ct = i-tot-ct + 1.
END.


FOR EACH pp-container WHERE 
         pp-container.situacao = 3 AND
         pp-container.dt-receb >= 01.01.2021 NO-LOCK.

    ASSIGN i-ct = i-ct + 1.

    DISP pp-container.nr-container
         STRING(i-ct) +  "/" + STRING(i-tot-ct).

    FOR EACH ob-etiqueta WHERE
             ob-etiqueta.nr-container = pp-container.nr-container NO-LOCK.
        
        FIND tt-itens WHERE 
             tt-itens.nr-container = ob-etiqueta.nr-container AND
             tt-itens.it-codigo = ob-etiqueta.it-codigo NO-ERROR.
        IF NOT AVAIL tt-itens THEN DO.
           CREATE tt-itens.
           ASSIGN tt-itens.nr-container = ob-etiqueta.nr-container
                  tt-itens.it-codigo = ob-etiqueta.it-codigo.
        END.
    
        IF ob-etiqueta.situacao = 5 THEN DO.
           FIND ped-item-rom WHERE
                ped-item-rom.cod-estabel = ob-etiqueta.cod-estabel AND
                ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta 
                NO-LOCK NO-ERROR.
    
           IF AVAIL ped-item-rom THEN DO.
              FIND ped-item WHERE
                   ped-item.nome-abrev = ped-item-rom.nome-abrev AND
                   ped-item.nr-pedcli = ped-item-rom.nr-pedcli AND
                   ped-item.nr-sequencia = ped-item-rom.nr-sequencia
                   NO-LOCK NO-ERROR.
    
              ASSIGN tt-itens.qt-vend = tt-itens.qt-vend + ob-etiqueta.quantidade
                     tt-itens.vl-vend = tt-itens.vl-vend + (ob-etiqueta.quantidade * ped-item.vl-preori).
                     
           END.
        END.
             
        IF ob-etiqueta.situacao <= 3 THEN
           ASSIGN tt-itens.qt-est = tt-itens.qt-est + ob-etiqueta.quantidade.
    END.
END.

FOR EACH tt-itens.
    DISP tt-itens.nr-container
         tt-itens.it-codigo
         tt-itens.qt-vend
         tt-itens.vl-vend
         tt-itens.qt-est
         tt-itens.vl-vend / tt-itens.qt-vend.
END.

