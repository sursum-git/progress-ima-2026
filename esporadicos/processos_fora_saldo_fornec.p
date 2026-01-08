DEFINE VARIABLE i AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLista AS CHARACTER   NO-UNDO FORMAT 'x(2000)'.
DEFINE VARIABLE iPedido AS INTEGER     NO-UNDO.
/*lista de processos n∆o inclusos no saldo anterior passado pela Ana Fl†via*/
ASSIGN cLista = '323,324,325,337,342,342,343,345,346,352,356,357,358,359,361,363,364,365,366,367,368,369,370,371,372,376,377,378,379,381,382,383,384,385,386,387,388,389,390,391,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432'.

REPEAT i= 1 TO NUM-ENTRIES(cLista,','):
    ASSIGN iPedido = INT(ENTRY(i,cLista,",")).
    FIND FIRST procs_compra
        WHERE procs_compra.proc_compra_id = iPedido
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL procs_compra THEN DO:
        ASSIGN procs_compra.LOG_considerar_previsao = YES.
        DISP "previsao sim" proc_compra_id.
    END.
    ELSE DO:
        DISP "previsao N«O" proc_compra_id.
    END.


END.
        
FOR EACH procs_compra
    WHERE log_considerar_previsao = YES.
    DISP proc_compra_id procs_compra.cod_proc_fornec.
END.
