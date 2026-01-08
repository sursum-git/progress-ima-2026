FIND ped-venda WHERE
     ped-venda.nr-pedcli = '128981' NO-LOCK NO-ERROR.
IF AVAIL ped-venda THEN DO:
   FIND emitente WHERE 
        emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.
   IF AVAIL emitente THEN DO:
      FIND cond-pagto WHERE
           cond-pagto.cod-cond-pag = emitente.cod-cond-pag NO-LOCK NO-ERROR.
      IF AVAIL cond-pagto THEN DO.
         DISP emitente.nome-abrev
              ped-venda.nome-abrev.
      END.
   END.
END.
