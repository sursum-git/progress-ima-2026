DEF VAR c-linha AS CHAR FORMAT "x(50)".
DEF VAR i-etiqueta LIKE ob-etiqueta.num-etiqueta.

INPUT FROM "P:\Thiago.Cassimiro\etiquetas.txt" NO-ECHO.
REPEAT.
    SET c-linha.

    ASSIGN i-etiqueta = INT(SUBSTR(c-linha,1,9)).

    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = '1' AND
         ob-etiqueta.num-etiqueta = i-etiqueta NO-ERROR.

    IF NOT AVAIL ob-etiqueta THEN NEXT.

    DISP ob-etiqueta.situacao.
    IF AVAIL ob-etiqueta THEN DO.
       ASSIGN ob-etiqueta.situacao = 3
              ob-etiqueta.ob-origem = "".

       FIND ped-item-rom WHERE
            ped-item-rom.cod-estabel = ob-etiqueta.cod-estabel AND
            ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-ERROR.

       IF AVAIL ped-item-rom THEN
          DELETE ped-item-rom.
    END.
END.

