FOR EACH prog_dtsul WHERE prog_dtsul.log_gera_log_exec = YES NO-LOCK.
    DISP prog_dtsul.cod_prog_dtsul 
         WITH TITLE "Programas Marcados para Gerar Log".
END.

FOR EACH log_exec_prog_dtsul 
    WHERE log_exec_prog_dtsul.cod_prog_dtsul           =  "essp0154" 
      AND log_exec_prog_dtsul.dat_inic_exec_prog_dtsul >= 07/20/2007
      AND log_exec_prog_dtsul.dat_inic_exec_prog_dtsul <= 07/20/2007
    NO-LOCK.
    DISP log_exec_prog_dtsul.cod_prog_dtsul FORMAT "x(10)"
         log_exec_prog_dtsul.dat_inic_exec_prog_dtsul 
         log_exec_prog_dtsul.hra_inic_exec_prog_dtsul 
         LOG_exec_prog_dtsul.cod_usuario
         WITH TITLE "L O G   D E   E X E C U € Ç O".
END.
