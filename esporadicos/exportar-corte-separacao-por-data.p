OUTPUT TO c:\temp\etqs_corte_criadas.txt.
FOR EACH hist_corte_separacao,
EACH transacoes 
WHERE transacoes.transacao_id  = hist_corte_separacao.transacao_id
AND  date(transacoes.dt_hr_ini) >= 01.01.2026 
AND num_tipo = 1
    :
    DISP hist_corte_separacao EXCEPT historico WITH  WIDTH 550.
END.
