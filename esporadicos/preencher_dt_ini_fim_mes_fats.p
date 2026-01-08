{esp/util.i}

FOR EACH fats_repres_clientes_prod .
    ASSIGN fats_repres_clientes_prod.dt_ini_mes = getPrimeiroDiaMes(ano,mes)
           fats_repres_clientes_prod.dt_fim_mes = getUltimoDiaMes(ano,mes)
           .
END.

FOR EACH fats_repres_clientes .
    ASSIGN fats_repres_clientes.dt_ini_mes = getPrimeiroDiaMes(ano,mes)
           fats_repres_clientes.dt_fim_mes = getUltimoDiaMes(ano,mes)
           .

END.

FOR EACH fats_repres .
    ASSIGN fats_repres.dt_ini_mes = getPrimeiroDiaMes(ano,mes)
           fats_repres.dt_fim_mes = getUltimoDiaMes(ano,mes)
           .

END.





/* MESSAGE getUltimoDiaMes(2024,10)
     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
