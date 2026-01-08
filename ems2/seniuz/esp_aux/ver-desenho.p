DEF VAR c-desenho AS CHAR FORMAT "x(4)".
REPEAT :
    UPDATE c-desenho.
    FIND FIRST ped-item WHERE ped-item.cod-sit-item < 3
                          AND SUBSTR(ped-item.cod-refer,3,4) = c-desenho
                        NO-LOCK NO-ERROR.
    DISP AVAIL ped-item LABEL "Existe?" FORMAT "Sim/NÆo".
END.
