DEF VAR i-cont AS INT.
DEF VAR i-atraso AS INT.

DEF STREAM estat.
DEF STREAM cliente.

OUTPUT STREAM estat TO "c:/temp/atraso.txt".
OUTPUT STREAM cliente TO "c:/temp/cliente.txt".

FOR EACH emitente WHERE emitente.identific <> 2 NO-LOCK
                  BREAK BY emitente.lim-credito:
    ASSIGN i-cont = i-cont + 1.
    FIND FIRST titulo WHERE titulo.cod-emitente = emitente.cod-emitente
                        AND titulo.dt-vencimen <  TODAY - 5
                        AND titulo.vl-saldo    <> 0
                      NO-LOCK NO-ERROR.
    IF AVAIL titulo THEN DO:
       ASSIGN i-atraso = i-atraso + 1.
       IF emitente.lim-credito >= 200000 THEN
       PUT STREAM cliente
                  emitente.cod-emitente ";"
                  emitente.nome-emit ";"
                  emitente.lim-credito
                  SKIP.
    END.

    IF LAST-OF(emitente.lim-credito) THEN DO:
       PUT STREAM estat 
                  emitente.lim-credito ";"
                  i-cont ";"
                  i-atraso
                  SKIP.
       ASSIGN i-cont   = 0
              i-atraso = 0.
    END.
END.
OUTPUT STREAM estat CLOSE.
OUTPUT STREAM cliente CLOSE.
