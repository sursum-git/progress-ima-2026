DEFINE VARIABLE cTrigger AS CHARACTER  FORMAT 'x(100)'   NO-UNDO.
DEFINE VARIABLE lExiste  AS LOGICAL     NO-UNDO.
DEFINE TEMP-TABLE tt
    FIELD trg AS CHAR FORMAT 'x(80)'
    FIELD LOG_achou AS LOGICAL
    FIELD tabela AS CHAR FORMAT 'x(80)'.
OUTPUT TO c:\temp\TAB_dic_dtsul.txt.
FOR EACH TAB_dic_dtsul:
    CREATE tt.
    ASSIGN tt.trg       = tab_dic_dtsul.nom_prog_upc_gat_write
           tt.tabela    = cod_tab_dic_dtsul
           tt.LOG_achou = SEARCH(tt.trg) = ? .

    CREATE tt.
    ASSIGN tt.trg       = tab_dic_dtsul.nom_prog_upc_gat_delete
           tt.tabela    = cod_tab_dic_dtsul
           tt.LOG_achou = SEARCH(tt.trg) = ? .
END.

FOR EACH tt:
    DISP tt WITH WIDTH 550.
END.
