DEF BUFFER b-reserva FOR ped-reserva.

FIND b-reserva WHERE
     b-reserva.num-reserva = 3848.

CREATE ped-reserva.
BUFFER-COPY b-reserva TO ped-reserva
            ASSIGN ped-reserva.num-reserva = NEXT-VALUE(seq-ped-reserva).

FOR EACH ped-reserva-it WHERE
         ped-reserva-it.num-reserva = b-reserva.num-reserva.

    IF ped-reserva-it.nr-sequencia > 2020 AND
       ped-reserva-it.nr-sequencia <= 2520 THEN DO.
       FOR EACH ped-reserva-etq WHERE
                ped-reserva-etq.num-reserva = b-reserva.num-reserva AND
                ped-reserva-etq.nr-sequencia =  ped-reserva-it.nr-sequencia.
           ASSIGN ped-reserva-etq.num-reserva = ped-reserva.num-reserva.
       END.
       ASSIGN ped-reserva-it.num-reserva = ped-reserva.num-reserva.
    END.
END.

MESSAGE  ped-reserva.num-reserva
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

       
