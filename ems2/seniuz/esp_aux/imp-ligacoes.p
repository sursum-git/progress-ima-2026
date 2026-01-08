/* Programa: imp-ligacoes.p
*/

DEF VAR i-cont-a AS INT.
DEF VAR i-cont-d AS INT.
DEF VAR da-data  AS DATE FORMAT 99/99/9999.

def temp-table tt-work
    FIELD ramal        AS char
    FIELD tronco       AS CHAR
    FIELD data         AS CHAR
    FIELD hora         AS CHAR
    FIELD tipo         AS char
    FIELD numero       AS CHAR
    FIELD local        AS CHAR
    FIELD duracao      AS CHAR
    FIELD setor        AS CHAR.
    
input from "c:/lixo/ligacoes.csv".
SET ^.

repeat:
   create tt-work.
   import delimiter ";" tt-work.
end.
input close.

DEF STREAM saida.
OUTPUT STREAM saida TO "c:/lixo/lig-resumo.csv"  CONVERT SOURCE "ibm850".
PUT STREAM saida
    "Ramal;" "Setor;" "Lig-Antes;" "Lig-Depois" SKIP.

FOR EACH tt-work WHERE tt-work.ramal <> ""
                  /* AND substr(tt-work.hora,1,2) >= "17" */
                 BREAK BY tt-work.ramal:

    da-data = DATE(INT(SUBSTR(tt-work.data,4,2)),
                   INT(SUBSTR(tt-work.data,1,2)),
                   INT(SUBSTR(tt-work.data,7,4))).
    IF da-data < 10/02/2006 THEN
       ASSIGN i-cont-a = i-cont-a + 1.
    ELSE
       ASSIGN i-cont-d = i-cont-d + 1.

    IF LAST-OF(tt-work.ramal) THEN DO:
       DISP tt-work.ramal LABEL "Ramal"
            tt-work.setor LABEL "Setor" FORMAT "x(30)"
            i-cont-a      LABEL "Antes"
            i-cont-d      LABEL "Depois".
       PUT STREAM saida
           tt-work.ramal ";"
           tt-work.setor FORMAT "x(30)" ";"
           i-cont-a      ";"
           i-cont-d      skip.              

       ASSIGN i-cont-a = 0
              i-cont-d = 0.
    END.
END.
OUTPUT STREAM saida CLOSE.
