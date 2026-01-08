
DEFINE VARIABLE cListaUsuarios AS CHARACTER   NO-UNDO FORMAT 'x(100)'.
DEFINE BUFFER bf_prog_dtsul_segur FOR prog_dtsul_segur.
DEFINE BUFFER bf_prog_dtsul   FOR prog_dtsul.
DEFINE VARIABLE cProgramaCorrente AS CHARACTER   NO-UNDO.
UPDATE cListaUsuarios  WITH WIDTH 550.
OUTPUT TO p:\permissoes.txt.
PUT "origem|tipo permissao|Grupo|Programa" SKIP.
FOR EACH prog_dtsul_segur NO-LOCK
    WHERE  lookup(cod_grp_usuar,cListaUsuarios) > 0:
    FIND FIRST grp_usuar where grp_usuar.cod_grp_usuar = prog_dtsul_segur.cod_grp_usuar
        NO-LOCK NO-ERROR.
    FIND FIRST prog_dtsul 
        WHERE prog_dtsul.cod_prog_dtsul = prog_dtsul_segur.cod_prog_dtsul NO-LOCK NO-ERROR.
    FOR EACH bf_prog_dtsul
        WHERE bf_prog_dtsul.cod_proced     = prog_dtsul.cod_proced
        AND   bf_prog_dtsul.cod_prog_dtsul <> prog_dtsul.cod_prog_dtsul .
        
        FIND FIRST bf_prog_dtsul_segur
            WHERE  bf_prog_dtsul_segur.cod_grp_usuar  = prog_dtsul_segur.cod_grp_usuar
            AND    bf_prog_dtsul_segur.cod_prog_dtsul = bf_prog_dtsul.cod_prog_dtsul EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL bf_prog_dtsul_segur THEN DO:
           CREATE bf_prog_dtsul_segur.
           ASSIGN bf_prog_dtsul_segur.cod_grp_usuar         =  prog_dtsul_segur.cod_grp_usuar
                  bf_prog_dtsul_segur.cod_prog_dtsul        =  bf_prog_dtsul.cod_prog_dtsul
                  bf_prog_dtsul_segur.idi_dtsul_prog_dtsul  =  bf_prog_dtsul.idi_dtsul
                  bf_prog_dtsul_segur.idi_dtsul_grp_usuar   =  grp_usuar.idi_dtsul.
           PUT "procedimento igual|permiss∆o criada|"   bf_prog_dtsul_segur.cod_grp_usuar "|" bf_prog_dtsul_segur.cod_prog_dtsul  SKIP.
        END.
        ELSE DO:
          PUT "procedimento igual|permiss∆o j† existente|"   bf_prog_dtsul.cod_prog_dtsul "|" prog_dtsul_segur.cod_grp_usuar  SKIP.
        END.
    END.    
END.

FOR EACH prog_dtsul_segur
    WHERE  lookup(cod_grp_usuar,cListaUsuarios) > 0:
    FIND FIRST prog_dtsul 
        WHERE prog_dtsul.cod_prog_dtsul = prog_dtsul_segur.cod_prog_dtsul 
        NO-LOCK NO-ERROR.
    FOR EACH PROCED_relat_proced
        WHERE PROCED_relat_proced.cod_proced = prog_dtsul.cod_proced.
        FOR EACH bf_prog_dtsul
            WHERE bf_prog_dtsul.cod_proced = PROCED_relat_proced.cod_proced_relat .

             FIND FIRST bf_prog_dtsul_segur
                  WHERE  bf_prog_dtsul_segur.cod_grp_usuar  = prog_dtsul_segur.cod_grp_usuar
                  AND    bf_prog_dtsul_segur.cod_prog_dtsul = bf_prog_dtsul.cod_prog_dtsul EXCLUSIVE-LOCK NO-ERROR.
             IF NOT AVAIL bf_prog_dtsul_segur THEN DO:
                CREATE bf_prog_dtsul_segur.
                ASSIGN bf_prog_dtsul_segur.cod_grp_usuar         =  prog_dtsul_segur.cod_grp_usuar
                       bf_prog_dtsul_segur.cod_prog_dtsul        =  bf_prog_dtsul.cod_prog_dtsul
                       bf_prog_dtsul_segur.idi_dtsul_prog_dtsul  =  bf_prog_dtsul.idi_dtsul
                       bf_prog_dtsul_segur.idi_dtsul_grp_usuar   =  grp_usuar.idi_dtsul.
                 PUT "procedimento relat|permiss∆o criada|"   bf_prog_dtsul_segur.cod_grp_usuar "|" bf_prog_dtsul_segur.cod_prog_dtsul  SKIP.
             END.
             ELSE DO:
               PUT "procedimento relat|permiss∆o j† existente|"   bf_prog_dtsul.cod_prog_dtsul "|" prog_dtsul_segur.cod_grp_usuar  SKIP.
             END.
        END.
    END.


    FOR EACH PROCED_consult_proced
        WHERE PROCED_consult_proced.cod_proced = prog_dtsul.cod_proced.
        FOR EACH bf_prog_dtsul
            WHERE bf_prog_dtsul.cod_proced = PROCED_consult_proced.cod_proced_con .

             FIND FIRST bf_prog_dtsul_segur
                  WHERE  bf_prog_dtsul_segur.cod_grp_usuar  = prog_dtsul_segur.cod_grp_usuar
                  AND    bf_prog_dtsul_segur.cod_prog_dtsul = bf_prog_dtsul.cod_prog_dtsul EXCLUSIVE-LOCK NO-ERROR.
             IF NOT AVAIL bf_prog_dtsul_segur THEN DO:
                CREATE bf_prog_dtsul_segur.
                ASSIGN bf_prog_dtsul_segur.cod_grp_usuar         =  prog_dtsul_segur.cod_grp_usuar
                       bf_prog_dtsul_segur.cod_prog_dtsul        =  bf_prog_dtsul.cod_prog_dtsul
                       bf_prog_dtsul_segur.idi_dtsul_prog_dtsul  =  bf_prog_dtsul.idi_dtsul
                       bf_prog_dtsul_segur.idi_dtsul_grp_usuar   =  grp_usuar.idi_dtsul.
                 PUT "procedimento cons|permiss∆o criada|"   bf_prog_dtsul_segur.cod_grp_usuar "|" bf_prog_dtsul_segur.cod_prog_dtsul  SKIP.
             END.
             ELSE DO:
               PUT "procedimento cons|permiss∆o j† existente|"   bf_prog_dtsul.cod_prog_dtsul "|" prog_dtsul_segur.cod_grp_usuar  SKIP.
             END.
        END.
    END.
END.
OUTPUT CLOSE.



