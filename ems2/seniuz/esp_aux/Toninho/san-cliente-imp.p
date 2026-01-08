DEF TEMP-TABLE tt-aux
    FIELD cod-emitente   AS INT FORMAT ">>>>>9"
    FIELD nome-emit      AS CHAR FORMAT "x(45)"
    FIELD cod-ramo-ativ  AS INT FORMAT ">>>>>9"
    FIELD desc-ramo-ativ AS CHAR FORMAT "x(45)"
    FIELD cod-finalid    AS INT FORMAT ">>>>>9"
    FIELD desc-finalid   AS CHAR FORMAT "x(45)"
    FIELD varejo         AS CHAR FORMAT "X"
    FIELD atacado        AS CHAR FORMAT "X"
    FIELD industria      AS CHAR FORMAT "X"
    FIELD servico        AS CHAR FORMAT "X"
    FIELD email-fin      AS CHAR FORMAT "x(35)" 
    FIELD email-fis      AS CHAR FORMAT "x(35)"
    FIELD email-com      AS CHAR FORMAT "x(35)"
    FIELD email-out      AS CHAR FORMAT "x(35)".

DEF BUFFER b-cont-emit FOR cont-emit.
DEF VAR i-seq AS INT.

 INPUT FROM p:\saneamento.csv.
 REPEAT WITH WIDTH 550.
     CREATE tt-aux.
     IMPORT DELIMITER ";" tt-aux. 
 END.

 FOR EACH tt-aux WHERE
          tt-aux.cod-emit >= 1 AND
          tt-aux.cod-emit <= 4100 NO-LOCK.

     FIND emitente WHERE
          emitente.cod-emitente = tt-aux.cod-emit NO-LOCK NO-ERROR.

     DISP tt-aux.cod-emit.
     PAUSE 0.

     CREATE tab-ocor.
     ASSIGN tab-ocor.descricao = "Integrando_Emitente" 
            tab-ocor.i-campo[1] = tt-aux.cod-emitente.

     FIND FIRST ext-emitente WHERE
                ext-emitente.cod-emitente = tt-aux.cod-emitente 
                SHARE-LOCK NO-ERROR.

     ASSIGN ext-emitente.cod-ramo-ativ = tt-aux.cod-ramo-ativ
            ext-emitente.cod_finalidade_venda = tt-aux.cod-finalid.

     ASSIGN ext-emitente.log_varejo     = IF tt-aux.varejo = 'N' THEN NO ELSE YES
            ext-emitente.log_atacado    = IF tt-aux.atacado = 'N' THEN NO ELSE YES
            ext-emitente.log_industria  = IF tt-aux.industria = 'N' THEN NO ELSE YES
            ext-emitente.log_servico    = IF tt-aux.servico = 'N' THEN NO ELSE YES.

     IF tt-aux.email-fin <> '' THEN DO.
        FIND FIRST cont-emit OF emitente WHERE 
                   cont-emit.area = "Financeiro" SHARE-LOCK NO-ERROR.
        IF NOT AVAIL cont-emit THEN DO.
           FIND LAST b-cont-emit OF emitente NO-LOCK NO-ERROR.
           ASSIGN i-seq = IF AVAIL b-cont-emit THEN
                          b-cont-emit.seq + 10 ELSE 10.

           CREATE cont-emit.
           ASSIGN cont-emit.seq = i-seq
                  cont-emit.area = "Financeiro"
                  cont-emit.cod-emitente = emitente.cod-emitente.
        END.
        ASSIGN cont-emit.e-mail = tt-aux.email-fin.
     END.

     IF tt-aux.email-fis <> '' THEN DO.
         FIND FIRST cont-emit OF emitente WHERE 
                    cont-emit.area = "Fiscal" SHARE-LOCK NO-ERROR.
         IF NOT AVAIL cont-emit THEN DO.
            FIND LAST b-cont-emit OF emitente NO-LOCK NO-ERROR.
            ASSIGN i-seq = IF AVAIL b-cont-emit THEN
                           b-cont-emit.seq + 10 ELSE 10.

            CREATE cont-emit.
            ASSIGN cont-emit.seq = i-seq
                   cont-emit.area = "Fiscal"
                   cont-emit.cod-emitente = emitente.cod-emitente.
         END.
         ASSIGN cont-emit.e-mail = tt-aux.email-fis.
     END.

     IF tt-aux.email-com <> '' THEN DO.
         FIND FIRST cont-emit OF emitente WHERE 
                    cont-emit.area = "Comercial" SHARE-LOCK NO-ERROR.
         IF NOT AVAIL cont-emit THEN DO.
            FIND LAST b-cont-emit OF emitente NO-LOCK NO-ERROR.
            ASSIGN i-seq = IF AVAIL b-cont-emit THEN
                           b-cont-emit.seq + 10 ELSE 10.

            CREATE cont-emit.
            ASSIGN cont-emit.seq = i-seq
                   cont-emit.area = "Comercial"
                   cont-emit.cod-emitente = emitente.cod-emitente.
         END.
         ASSIGN cont-emit.e-mail = tt-aux.email-com.
     END.

     IF tt-aux.email-out <> '' THEN DO.
         FIND FIRST cont-emit OF emitente WHERE 
                    cont-emit.area = "Outros" SHARE-LOCK NO-ERROR.
         IF NOT AVAIL cont-emit THEN DO.
            FIND LAST b-cont-emit OF emitente NO-LOCK NO-ERROR.
            ASSIGN i-seq = IF AVAIL b-cont-emit THEN
                           b-cont-emit.seq + 10 ELSE 10.

            CREATE cont-emit.
            ASSIGN cont-emit.seq = i-seq
                   cont-emit.area = "Outros"
                   cont-emit.cod-emitente = emitente.cod-emitente.
         END.
         ASSIGN cont-emit.e-mail = tt-aux.email-out.
     END.

     DELETE tab-ocor.
 END.




