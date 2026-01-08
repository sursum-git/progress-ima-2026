DEFINE TEMP-TABLE tt
    FIELD id AS INT.
INPUT FROM c:\temp\processos.csv.
REPEAT:
     CREATE tt.
     IMPORT  tt. 
 END.


INPUT CLOSE.

OUTPUT TO c:\temp\modificacoes.txt.
FOR EACH tt:
    DISP tt.
    FIND procs_compra
        WHERE procs_Compra.proc_compra_id = tt.id
        NO-LOCK NO-ERROR.
    FOR EACH itens_proc_compra
        WHERE procs_compra.proc_compra_id = itens_proc_compra.proc_compra_id :
        ASSIGN itens_proc_compra.ind_distr_custo = 1.
        DISP procs_compra.proc_compra_id  itens_proc_compra.ITEM_proc_compra_id.
        FIND FIRST itens_container OF itens_proc_compra
            NO-LOCK NO-ERROR.
        FIND FIRST pp-container
            WHERE pp-container.nr-container = itens_container.container_id
            EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL pp-container THEN DO:
          ASSIGN pp-container.LOG_gerar_distr_custo   = YES .
          DISP pp-container.nr-container.

       END.

    END.
END.



