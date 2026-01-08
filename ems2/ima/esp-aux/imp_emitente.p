DEF TEMP-TABLE tt-emitente LIKE mgcad.emitente.
DEF TEMP-TABLE tt-cont-emit LIKE mgcad.cont-emit.

DEF VAR i-cod-emit LIKE mgcad.emitente.cod-emitente.

ASSIGN i-cod-emit = 29595.
 
IF CONNECTED("db-aux") THEN
   DISCONNECT db-aux.

CONNECT -db ems2med -ld db-aux -S 10031 -H 192.168.0.44 -N tcp.
RUN esp-aux/busca-emitente.p (INPUT-OUTPUT TABLE tt-emitente,
                              INPUT-OUTPUT TABLE tt-cont-emit,
                              INPUT i-cod-emit). 
DISCONNECT db-aux.

CONNECT -db ems2ima -ld db-aux -S 10030 -H 192.168.0.44 -N tcp.
RUN esp-aux/busca-emitente.p (INPUT-OUTPUT TABLE tt-emitente,
                              INPUT-OUTPUT TABLE tt-cont-emit,
                              INPUT i-cod-emit). 
DISCONNECT db-aux.

CONNECT -db ems2ima -ld db-aux -S 30030 -H 192.168.0.4 -N tcp.
RUN esp-aux/busca-emitente.p (INPUT-OUTPUT TABLE tt-emitente,
                              INPUT-OUTPUT TABLE tt-cont-emit,
                              INPUT i-cod-emit). 
DISCONNECT db-aux.

CONNECT -db ems2med -ld db-aux -S 30031 -H 192.168.0.4 -N tcp.
RUN esp-aux/busca-emitente.p (INPUT-OUTPUT TABLE tt-emitente,
                              INPUT-OUTPUT TABLE tt-cont-emit,
                              INPUT i-cod-emit). 
DISCONNECT db-aux.

FOR EACH tt-emitente NO-LOCK.
    FIND mgcad.emitente WHERE
         mgcad.emitente.cgc = tt-emitente.cgc NO-LOCK NO-ERROR.

    IF AVAIL mgcad.emitente THEN
       DELETE tt-emitente.
END.


FOR EACH tt-emitente.
    DISP tt-emitente.cod-emit
         tt-emitente.nome-abrev
         tt-emitente.log-2
         WITH WIDTH 550.
END.


RUN esp-aux\importa_emitente.p (INPUT TABLE tt-emitente,
                                INPUT TABLE tt-cont-emit).


