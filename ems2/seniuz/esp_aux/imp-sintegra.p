DEF VAR l-cab AS LOG.

DEF STREAM Reg60M.
DEF STREAM Reg60A.
DEF STREAM Reg60D.
DEF STREAM Reg60I.
DEF STREAM Reg60R.

DEF TEMP-TABLE tt-sintegra
    FIELD registro AS CHAR.
    
input from "M:/SINTEGRA/SINTEGRA_BRP_Registro_60.txt".

repeat:
   create tt-sintegra.
   IMPORT DELIMITER "#&@" tt-sintegra.
end.
input close.

OUTPUT STREAM Reg60M TO "M:/SINTEGRA/Analise_BRP/Reg_60M.csv".
OUTPUT STREAM Reg60A TO "M:/SINTEGRA/Analise_BRP/Reg_60A.csv".
OUTPUT STREAM Reg60D TO "M:/SINTEGRA/Analise_BRP/Reg_60D.csv".
OUTPUT STREAM Reg60I TO "M:/SINTEGRA/Analise_BRP/Reg_60I.csv".
OUTPUT STREAM Reg60R TO "M:/SINTEGRA/Analise_BRP/Reg_60R.csv".

PUT STREAM Reg60M "1;2;3;4;5;6;7;8;9;10;11;12;13" SKIP.
PUT STREAM Reg60A "1;2;3;4;5;6;7" SKIP.
PUT STREAM Reg60D "1;2;3;4;5;6;7;8;9;10;11" SKIP.
PUT STREAM Reg60I "1;2;3;4;5;6;7;8;9;10;11;12;13;14" SKIP.
PUT STREAM Reg60R "1;2;3;4;5;6;7;8;9" SKIP.

FOR EACH tt-sintegra:
    IF tt-sintegra.registro = "" THEN NEXT.
    IF tt-sintegra.registro BEGINS "60M" THEN DO:
       PUT STREAM Reg60M UNFORMAT
           SUBSTR(tt-sintegra.registro,1,2) ";"
           SUBSTR(tt-sintegra.registro,3,1) ";"
           SUBSTR(tt-sintegra.registro,4,8) ";"
           SUBSTR(tt-sintegra.registro,12,20) ";"
           SUBSTR(tt-sintegra.registro,32,3) ";"
           SUBSTR(tt-sintegra.registro,35,2) ";"
           SUBSTR(tt-sintegra.registro,37,6) ";"
           SUBSTR(tt-sintegra.registro,43,6) ";"
           SUBSTR(tt-sintegra.registro,49,6) ";"
           SUBSTR(tt-sintegra.registro,55,3) ";"
           SUBSTR(tt-sintegra.registro,58,16) ";"
           SUBSTR(tt-sintegra.registro,74,16) ";"
           SUBSTR(tt-sintegra.registro,90,37) SKIP.
    END.
    IF tt-sintegra.registro BEGINS "60A" THEN DO:
       PUT STREAM Reg60A UNFORMAT
           SUBSTR(tt-sintegra.registro,1,2) ";"
           SUBSTR(tt-sintegra.registro,3,1) ";"
           SUBSTR(tt-sintegra.registro,4,8) ";"
           SUBSTR(tt-sintegra.registro,12,20) ";"
           SUBSTR(tt-sintegra.registro,32,4) ";"
           SUBSTR(tt-sintegra.registro,36,12) ";"
           SUBSTR(tt-sintegra.registro,48,79) SKIP.
    END.
    IF tt-sintegra.registro BEGINS "60D" THEN DO:
       PUT STREAM Reg60D UNFORMAT
           SUBSTR(tt-sintegra.registro,1,2) ";"
           SUBSTR(tt-sintegra.registro,3,1) ";"
           SUBSTR(tt-sintegra.registro,4,8) ";"
           SUBSTR(tt-sintegra.registro,12,20) ";"
           SUBSTR(tt-sintegra.registro,32,14) ";"
           SUBSTR(tt-sintegra.registro,46,13) ";"
           SUBSTR(tt-sintegra.registro,59,16) ";"
           SUBSTR(tt-sintegra.registro,75,16) ";"
           SUBSTR(tt-sintegra.registro,91,4) ";"
           SUBSTR(tt-sintegra.registro,95,13)
           SUBSTR(tt-sintegra.registro,108,19) SKIP.
    END.
    IF tt-sintegra.registro BEGINS "60I" THEN DO:
       PUT STREAM Reg60I UNFORMAT
           SUBSTR(tt-sintegra.registro,1,2) ";"
           SUBSTR(tt-sintegra.registro,3,1) ";"
           SUBSTR(tt-sintegra.registro,4,8) ";"
           SUBSTR(tt-sintegra.registro,12,20) ";"
           SUBSTR(tt-sintegra.registro,32,2) ";"
           SUBSTR(tt-sintegra.registro,34,6) ";"
           SUBSTR(tt-sintegra.registro,40,3) ";"
           SUBSTR(tt-sintegra.registro,43,14) ";"
           SUBSTR(tt-sintegra.registro,57,13) ";"
           SUBSTR(tt-sintegra.registro,70,13) ";"
           SUBSTR(tt-sintegra.registro,83,12) ";"
           SUBSTR(tt-sintegra.registro,95,4) ";"
           SUBSTR(tt-sintegra.registro,99,12) ";"
           SUBSTR(tt-sintegra.registro,111,16) SKIP.
    END.
    IF tt-sintegra.registro BEGINS "60R" THEN DO:
       PUT STREAM Reg60R UNFORMAT
           SUBSTR(tt-sintegra.registro,1,2) ";"
           SUBSTR(tt-sintegra.registro,3,1) ";"
           SUBSTR(tt-sintegra.registro,4,6) ";"
           SUBSTR(tt-sintegra.registro,10,14) ";"
           SUBSTR(tt-sintegra.registro,24,13) ";"
           SUBSTR(tt-sintegra.registro,37,16) ";"
           SUBSTR(tt-sintegra.registro,53,16) ";"
           SUBSTR(tt-sintegra.registro,69,4) ";"
           SUBSTR(tt-sintegra.registro,73,54) SKIP.
    END.
END.
OUTPUT STREAM Reg60M CLOSE.
OUTPUT STREAM Reg60A CLOSE.
OUTPUT STREAM Reg60D CLOSE.
OUTPUT STREAM Reg60I CLOSE.
OUTPUT STREAM Reg60R CLOSE.
