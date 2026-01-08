DEF VAR c-rep-grp1 AS CHAR INIT "20,65,68,19,39,28,112,2,12,162".
DEF VAR c-rep-grp2 AS CHAR INIT "119,11,27,161,82,107,81,5".
DEF VAR c-rep-grp3 AS CHAR INIT "102,122,150,101,163,165,3,158,9,175,130,100,160,154,72".
DEF VAR i-grupo    AS INT.
DEF VAR i-cont0    AS INT.
DEF VAR i-cont1    AS INT.
DEF VAR i-cont2    AS INT.
DEF VAR i-cont3    AS INT.

FOR EACH repres NO-LOCK.
    IF LOOKUP(STRING(repres.cod-rep),c-rep-grp1) <> 0 THEN
       ASSIGN i-grupo = 1
              i-cont1 = i-cont1 + 1.
    ELSE
    IF LOOKUP(STRING(repres.cod-rep),c-rep-grp2) <> 0 THEN
       ASSIGN i-grupo = 2
              i-cont2 = i-cont2 + 1.
    ELSE
    IF LOOKUP(STRING(repres.cod-rep),c-rep-grp3) <> 0 THEN
       ASSIGN i-grupo = 3
              i-cont3 = i-cont3 + 1.
    ELSE
    ASSIGN i-grupo = 0
           i-cont0 = i-cont0 + 1.

    DISP repres.cod-rep
         repres.nome-abrev
         i-grupo.
END.
DISP i-cont0
     i-cont1
     i-cont2
     i-cont3.
