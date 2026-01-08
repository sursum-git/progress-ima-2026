/* Programa: imp-upc.p
** Objetivo: Exportar dados de programas que tˆm upc para posterior importa‡Æo,
**           com o objetivo de recuperar os dados que sÆo destruidos quando da 
**           importa‡Æo de menus da Datasul.
** Autor...: Gilvando - 30/01/2006
*/

DEF VAR i-cont AS INT.

DEF TEMP-TABLE tt-upc LIKE prog_dtsul.
DEF TEMP-TABLE tt-perm LIKE prog_dtsul_segur.
    
DEF STREAM upc.
DEF STREAM perm.

INPUT STREAM upc FROM N:\especificos\esp_Aux\upc.txt.
INPUT STREAM perm FROM N:\especificos\esp_Aux\perm.txt.

REPEAT:
   CREATE tt-upc.
   IMPORT STREAM upc tt-upc.
END.
REPEAT:
   CREATE tt-perm.
   IMPORT STREAM perm tt-perm.
END.

INPUT STREAM upc CLOSE.
INPUT STREAM perm CLOSE.

FOR EACH tt-upc:
    IF tt-upc.cod_prog_dtsul = "" THEN NEXT.
    
    FIND prog_dtsul WHERE prog_dtsul.cod_prog_dtsul = tt-upc.cod_prog_dtsul
                    NO-ERROR.
    IF NOT AVAIL prog_dtsul THEN DO:
       CREATE prog_dtsul.
       BUFFER-COPY tt-upc TO prog_dtsul.
       FOR EACH tt-perm WHERE tt-perm.cod_prog_dtsul = tt-upc.cod_prog_dtsul.
           CREATE prog_dtsul_segur.
           BUFFER-COPY tt-perm TO prog_dtsul_segur.
       END.
       ASSIGN i-cont = i-cont + 1.
    END.
END.
DISPLAY "Atualizados: " STRING(i-cont,">>>9").

