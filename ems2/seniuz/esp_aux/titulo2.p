DEF VAR i-pag-atr-aci5 AS INT.
DEF VAR i-pag-atr-ate5 AS INT.
DEF VAR i-abe-atr-aci5 AS INT.
DEF VAR i-abe-atr-ate5 AS INT.
DEF VAR i-atraso       AS INT.
DEF VAR i-tot-pag-atr-aci5 AS INT. 
DEF VAR i-tot-pag-atr-ate5 AS INT. 
DEF VAR i-tot-abe-atr-aci5 AS INT. 
DEF VAR i-tot-abe-atr-ate5 AS INT. 

OUTPUT TO "c:/lixo/clientes.csv".

FOR EACH titulo WHERE titulo.ep-codigo   = 1
                  AND titulo.cod-estabel = "2"
                  AND titulo.cod-esp     = "dp"
                  AND titulo.dt-vencimen >= 01/01/2006
                  AND titulo.dt-vencimen <= 12/31/2006
                NO-LOCK,
    EACH emitente WHERE emitente.cod-emitente = titulo.cod-emitente 
                  NO-LOCK
    BREAK BY emitente.estado
          BY emitente.cod-emitente:

    IF titulo.vl-saldo = 0 THEN DO: /* Pago */
       IF titulo.dt-ult-pagto > titulo.dt-vencimen THEN
          ASSIGN i-atraso = titulo.dt-ult-pagto - titulo.dt-vencimen.
       ELSE
          ASSIGN i-atraso = 0.

       IF i-atraso > 5 THEN
          ASSIGN i-pag-atr-aci5 = i-pag-atr-aci5 + 1.
       ELSE
          ASSIGN i-pag-atr-ate5 = i-pag-atr-ate5 + 1.
    END.
    ELSE DO:
       IF titulo.dt-vencimen < TODAY THEN
          ASSIGN i-atraso = TODAY - titulo.dt-vencimen.
       ELSE
          ASSIGN i-atraso = 0.

       IF i-atraso > 5 THEN
          ASSIGN i-abe-atr-aci5 = i-abe-atr-aci5 + 1.
       ELSE
          ASSIGN i-abe-atr-ate5 = i-abe-atr-ate5 + 1.
    END.
 
    IF LAST-OF(emitente.cod-emitente) THEN DO:
       PUT emitente.estado ";"
           emitente.cod-emitente ";"
           emitente.nome-emit ";"
           emitente.cod-gr-cli ";"
           i-pag-atr-aci5 ";"
           i-pag-atr-ate5 ";"
           i-abe-atr-aci5 ";"
           i-abe-atr-ate5 
           SKIP.
       ASSIGN i-tot-pag-atr-aci5 = i-tot-pag-atr-aci5 + i-pag-atr-aci5
              i-tot-pag-atr-ate5 = i-tot-pag-atr-ate5 + i-pag-atr-ate5
              i-tot-abe-atr-aci5 = i-tot-abe-atr-aci5 + i-abe-atr-aci5
              i-tot-abe-atr-ate5 = i-tot-abe-atr-ate5 + i-abe-atr-ate5.
       ASSIGN i-pag-atr-aci5 = 0
              i-pag-atr-ate5 = 0
              i-abe-atr-aci5 = 0
              i-abe-atr-ate5 = 0.
    END.
    IF LAST-OF(emitente.estado) THEN DO:
       PUT ";"
           "Total"
           ";;;"
           i-tot-pag-atr-aci5 ";"
           i-tot-pag-atr-ate5 ";"
           i-tot-abe-atr-aci5 ";"
           i-tot-abe-atr-ate5
           SKIP(1).

       ASSIGN i-tot-pag-atr-aci5 = 0
              i-tot-pag-atr-ate5 = 0
              i-tot-abe-atr-aci5 = 0
              i-tot-abe-atr-ate5 = 0.
    END.


END.
OUTPUT CLOSE.
