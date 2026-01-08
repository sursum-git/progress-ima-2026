DEFINE TEMP-TABLE tt
    FIELD codEmitente AS INT
    FIELD grupo  AS INT.
OUTPUT TO c:\temp\importacoes.txt.
FOR EACH docum-est
    WHERE nat-operacao = '31201'
    AND dt-trans > 11.01.2014 
    NO-LOCK USE-INDEX natoper:
    DISP nro-docto dt-trans.
    /*MESSAGE dt-trans
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    FOR EACH dupli-apagar-cex OF docum-est:
        DISP nro-docto vl-a-pagar valor-a-pagar-me cod-emitente cod-emitente-desp WITH WIDTH 550.
        /*MESSAGE 'entrei na duplicata'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        FIND FIRST tt WHERE tt.codEmitente = dupli-apagar-cex.cod-emitente-desp
             NO-LOCK NO-ERROR.
        IF NOT AVAIL tt THEN DO:
           /*MESSAGE 'nao achei na tabela'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
           FIND FIRST emitente WHERE dupli-apagar-cex.cod-emitente-desp = emitente.cod-emitente
                  NO-LOCK NO-ERROR.
           /*MESSAGE 'grupo:' SKIP
                    emitente.cod-gr-forn
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

           IF AVAIL emitente  THEN DO:
              CREATE tt.
              ASSIGN tt.codEmitente = dupli-apagar-cex.cod-emitente-desp
                     tt.grupo = cod-gr-forn.

           END.
        END.  
    END. 
    /*FOR EACH dupli-apagar OF docum-est:
        /*MESSAGE 'entrei na duplicata'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        FIND FIRST tt WHERE tt.codEmitente = dupli-apagar.cod-emitente
             NO-LOCK NO-ERROR.
        IF NOT AVAIL tt THEN DO:
           /*MESSAGE 'nao achei na tabela'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
           FIND FIRST emitente OF dupli-apagar
                  NO-LOCK NO-ERROR.
           /*MESSAGE 'grupo:' SKIP
                    emitente.cod-gr-forn
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

           IF AVAIL emitente  THEN DO:
              CREATE tt.
              ASSIGN tt.codEmitente = dupli-apagar.cod-emitente
                     tt.grupo = cod-gr-forn.

           END.
        END.  
    END.      */
END.

FOR EACH tt:
    FIND FIRST emitente 
        WHERE emitente.cod-emitente = tt.codEmitente
        NO-LOCK NO-ERROR.

    DISP tt.codEmitente  
         emitente.nome-emit
         tt.grupo WITH 1 COL WIDTH 550.

    FOR EACH movto_tit_ap
        WHERE cod_empresa = '500'
        AND   cdn_fornecedor = tt.codEmitente USE-INDEX mvtttp_razao NO-LOCK.
         FOR EACH aprop_ctbl_ap OF movto_tit_ap
         WHERE cod_cta_ctbl = '19000005'  EXCLUSIVE-LOCK.
             ASSIGN cod_cta_ctbl = '19000002'.
             DISP num_id_aprop_ctbl_ap num_id_movto_tit_ap.
         END.
    END.                                                   
END.



