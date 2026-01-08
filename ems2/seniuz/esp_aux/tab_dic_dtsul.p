/* programa: tab_dic_dtsul.p
** Mostra os gatilhos de upc's cadastrados.
*/

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.csv" CONVERT SOURCE "ibm850".
PUT "Tabela;Descricao;Gatilho DELETE;Gatilho WRITE" SKIP.

FOR EACH tab_dic_dtsul NO-LOCK.
    PUT UNFORMAT 
        tab_dic_dtsul.cod_tab_dic_dtsul ";"
        tab_dic_dtsul.des_tab_dic_dtsul ";"
        tab_dic_dtsul.nom_prog_upc_gat_delete ";"
        tab_dic_dtsul.nom_prog_upc_gat_write
        SKIP.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "excel.exe", input "c:\lixo\lixo.csv").
delete procedure h-prog.

