DEF VAR d-data LIKE emitente.data-implant LABEL "Ddata".

ASSIGN d-data = 12.31.9999.
FOR EACH emitente WHERE
         emitente.cod-emit >= 5000 AND 
         emitente.cod-emit < 6000 
    BY emitente.cod-emit DESCENDING .

    IF emitente.cod-emit > 40000 THEN NEXT.

    IF emitente.data-implant > d-data THEN DO.
        DISP emitente.cod-emit
             emitente.data-implant
             d-data. 

       ASSIGN emitente.data-implant = d-data.
       
    END.
    
    ASSIGN d-data = emitente.data-implant.
END.
    
