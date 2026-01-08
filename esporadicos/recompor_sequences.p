DEFINE VARIABLE iseq AS INTEGER     NO-UNDO.
DEFINE VARIABLE iseqTb AS INTEGER     NO-UNDO.
DEFINE VARIABLE i AS INTEGER     NO-UNDO.




/*seq_despesas_container*/
ASSIGN iSeq = CURRENT-VALUE(seq_despesas_container).
FOR LAST despesas_container:
     ASSIGN iSeqTb = despesa_container_id.
END.

REPEAT i = 1 TO iSeqTb - iSeq + 1 :
    NEXT-VALUE(seq_despesas_container).
END.
MESSAGE 'seq_despesas_container' SKIP
         CURRENT-VALUE(seq_despesas_container)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.



/*seq_cta_corren_custo_distr*/
ASSIGN iSeq = CURRENT-VALUE(seq_cta_corren_custo_distr).
FOR LAST cta_corren_custo_distr:
     ASSIGN iSeqTb = cta_corren_custo_distr_id.
END.

REPEAT i = 1 TO iSeqTb - iSeq + 1 :
    NEXT-VALUE(seq_cta_corren_custo_distr).
END.

MESSAGE 'seq_cta_corren_custo_distr' SKIP
         CURRENT-VALUE(seq_cta_corren_custo_distr)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*seq_apont_amostra*/
ASSIGN iSeq = CURRENT-VALUE(seq_apont_amostra).
FOR LAST aponts_amostra:
     ASSIGN iSeqTb = apont_amostra_id.
END.

REPEAT i = 1 TO iSeqTb - iSeq + 1 :
    NEXT-VALUE(seq_apont_amostra).
END.

MESSAGE 'seq_apont_amostra' SKIP
         CURRENT-VALUE(seq_apont_amostra)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.



/*seq_calcs_imp_container*/
ASSIGN iSeq = CURRENT-VALUE(seq_calcs_imp_container).

FOR LAST calcs_imp_container:
     ASSIGN iSeqTb = calc_imp_container_id.
END.

REPEAT i = 1 TO iSeqTb - iSeq + 1 :
    NEXT-VALUE(seq_calcs_imp_container).
END.

MESSAGE 'seq_apont_amostra' SKIP
         CURRENT-VALUE(seq_calcs_imp_container)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.



/*seq_hist_cotacao_bc*/
ASSIGN iSeq = CURRENT-VALUE(seq_hist_cotacao_bc).

FOR LAST hist_cotacoes_bc:
     ASSIGN iSeqTb = hist_cotacao_bc_id.
END.

REPEAT i = 1 TO iSeqTb - iSeq + 1 :
    NEXT-VALUE(seq_hist_cotacao_bc).
END.

MESSAGE 'seq hist cotacao bc' SKIP
         CURRENT-VALUE(seq_hist_cotacao_bc)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


/*seq_transacao*/
ASSIGN iSeq = CURRENT-VALUE(seq_transacao).

FOR LAST transacoes:
     ASSIGN iSeqTb = transacao_id.
END.

REPEAT i = 1 TO iSeqTb - iSeq + 1 :
    NEXT-VALUE(seq_transacao).
END.

MESSAGE 'seq hist cotacao bc' SKIP
         CURRENT-VALUE(seq_hist_cotacao_bc)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.



/*seq_item_container_custos*/
ASSIGN iSeq = CURRENT-VALUE(seq_item_container_custos).

FOR LAST ITEM_container_custos:
     ASSIGN iSeqTb = item_container_custo_id.
END.

REPEAT i = 1 TO iSeqTb - iSeq + 1 :
    NEXT-VALUE(seq_item_container_custos).
END.

MESSAGE 'seq_item_container_custos' SKIP
         CURRENT-VALUE(seq_item_container_custos)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.



/*seq_itens_container*/
ASSIGN iSeq = CURRENT-VALUE(seq_itens_container).

FOR LAST itens_container:
     ASSIGN iSeqTb = item_container_id.
END.

REPEAT i = 1 TO iSeqTb - iSeq + 1 :
    NEXT-VALUE(seq_itens_container).
END.

MESSAGE 'seq_itens_container' SKIP
         CURRENT-VALUE(seq_itens_container)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


/*seq_item_custos*/
ASSIGN iSeq = CURRENT-VALUE(seq_item_custos).

FOR LAST item_custos:
     ASSIGN iSeqTb = item_custo_id.
END.

REPEAT i = 1 TO iSeqTb - iSeq + 1 :
    NEXT-VALUE(seq_item_custos).
END.

MESSAGE 'seq_item_custos' SKIP
         CURRENT-VALUE(seq_item_custos)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


/*seq_despesas_proc_compra*/
ASSIGN iSeq = CURRENT-VALUE(seq_despesas_proc_compra).

FOR LAST despesas_proc_compra:
     ASSIGN iSeqTb = despesa_proc_compra_id.
END.

REPEAT i = 1 TO iSeqTb - iSeq + 1 :
    NEXT-VALUE(seq_despesas_proc_compra).
END.

MESSAGE 'seq_despesas_proc_compra' SKIP
         CURRENT-VALUE(seq_despesas_proc_compra)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


/*seq_calcs_imp_proc_compra*/
ASSIGN iSeq = CURRENT-VALUE(seq_calcs_imp_proc_compra).

FOR LAST calcs_imp_proc_compra:
     ASSIGN iSeqTb = calc_imp_proc_compra_id.
END.

REPEAT i = 1 TO iSeqTb - iSeq + 1 :
    NEXT-VALUE(seq_calcs_imp_proc_compra).
END.

MESSAGE 'seq_calcs_imp_proc_compra' SKIP
         CURRENT-VALUE(seq_calcs_imp_proc_compra)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


