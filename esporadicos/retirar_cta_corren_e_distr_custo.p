FOR EACH procs_compra :
    ASSIGN procs_compra.log_considerar_previsao = NO.
    FOR EACH itens_proc_compra 
        WHERE itens_proc_compra.proc_compra_id = procs_compra.proc_compra_id.
        ASSIGN itens_proc_compra.ind_distr_custo = 0.
    END.
END.

FOR EACH pp-container:
    ASSIGN pp-container.log_gerar_saldo_a_pagar = NO
           pp-container.LOG_gerar_distr_custo   = NO.


END.
