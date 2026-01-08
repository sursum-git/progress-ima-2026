/* programa: prog_dtsul4.p
** Mostra as upc's cadastrados.
*/

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.csv" CONVERT SOURCE "ibm850".
PUT "Programa;Descricao;Prog_UPC" SKIP.

FOR EACH prog_dtsul WHERE prog_dtsul.nom_prog_upc <> "" NO-LOCK:
    PUT UNFORMAT 
        TRIM(prog_dtsul.cod_prog_dtsul) ";"
        TRIM(prog_dtsul.nom_prog_dtsul) ";"
        TRIM(prog_dtsul.nom_prog_upc)
        SKIP.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "excel.exe", input "c:\lixo\lixo.csv").
delete procedure h-prog.

