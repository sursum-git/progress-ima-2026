/* Programa: exp-upc.p
** Objetivo: Exportar dados de programas que tàm upc para posterior importaá∆o,
**           com o objetivo de recuperar os dados que s∆o destruidos quando da 
**           importaá∆o de menus da Datasul.
** Autor...: Gilvando - 30/01/2006
*/

DEF STREAM upc.
DEF STREAM perm.

OUTPUT STREAM upc TO N:\especificos\esp_Aux\upc.txt CONVERT SOURCE "IBM850".
OUTPUT STREAM perm TO N:\especificos\esp_Aux\perm.txt CONVERT SOURCE "IBM850".

FOR EACH prog_dtsul WHERE prog_dtsul.nom_prog_upc <> "" NO-LOCK:
    EXPORT STREAM upc prog_dtsul.
    
    FOR EACH prog_dtsul_segur WHERE prog_dtsul_segur.cod_prog_dtsul = prog_dtsul.cod_prog_dtsul
                              NO-LOCK.
        EXPORT STREAM perm prog_dtsul_segur.
    END.
END.
OUTPUT STREAM upc CLOSE.
OUTPUT STREAM perm CLOSE.
