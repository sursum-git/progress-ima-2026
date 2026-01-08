/*
Programa: esbo/boHistLimCredCli.p
Objetivo: Manter a tabela boHistLimCredCli */

DEFINE VARIABLE iCliente        AS INTEGER     NO-UNDO.
DEFINE VARIABLE cCodUsuario     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dVlCredito      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dtValidade      AS DATE   NO-UNDO.
DEFINE VARIABLE cHistorico      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iIdCorrente     AS INTEGER     NO-UNDO.
{esp/util.i}
DEFINE BUFFER bf FOR hist_limit_cred_cli.
DEFINE TEMP-TABLE ttDados LIKE hist_limit_cred_cli.

PROCEDURE iniciarBos:

END PROCEDURE.

PROCEDURE finalizarBos:


END PROCEDURE.


PROCEDURE setProp:
    DEFINE INPUT  PARAMETER pchave AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pValor AS CHARACTER   NO-UNDO.

    CASE pChave:

        WHEN 'cod_emitente' THEN 
             ASSIGN iCliente = INT(pValor).
        WHEN 'cod_usuario' THEN
            ASSIGN cCodUsuario = pValor.
        WHEN 'vl_credito' THEN DO:
            ASSIGN dVlCredito = DEC(pValor).
        END.                      
        WHEN 'dt_validade' THEN DO:
            RUN convDtApi(pvalor,OUTPUT dtValidade).
        END.                    
        WHEN 'historico' THEN
            ASSIGN cHistorico = pValor.

    END CASE.

END PROCEDURE.

PROCEDURE incluirLimite:
    CREATE hist_limit_cred_cli.
    ASSIGN hist_limit_cred_cli.hist_limit_cred_cli_id = NEXT-VALUE(seq_hist_lim_cred_cli)
           hist_limit_cred_cli.cod_emitente          = iCliente
           hist_limit_cred_cli.dt_hr_registro        = NOW
           hist_limit_cred_cli.dt_validade           = DtValidade
           hist_limit_cred_cli.cod_usuario           = cCodUsuario
           hist_limit_cred_cli.vl_credito            = dVlCredito
           hist_limit_cred_cli.historico             = cHistorico
           iIdCorrente                               = hist_limit_cred_cli.hist_limit_cred_cli_id.

END PROCEDURE.

PROCEDURE getIdCorrente:
    DEFINE OUTPUT PARAMETER pId AS INTEGER     NO-UNDO.
    ASSIGN pId = iIdCorrente.
END PROCEDURE.

/*PROCEDURE setLimiteLiq:
    DEFINE INPUT  PARAMETER pVlLimite AS CHARACTER   NO-UNDO.
    ASSIGN dVlCredito = decimal(pVlLimite).


END PROCEDURE.*/
PROCEDURE getLimiteAtual:
    DEFINE OUTPUT PARAMETER dCredito AS DECIMAL     NO-UNDO.
    FIND LAST bf
         WHERE bf.cod_emitente = icliente
         AND bf.dt_validade >= TODAY 
         USE-INDEX ind_emitente NO-LOCK NO-ERROR.
    IF NOT AVAIL bf THEN
       ASSIGN dCredito = 0.
    ELSE
       ASSIGN dCredito = bf.vl_credito.


END PROCEDURE.

PROCEDURE getLimitesCliente:
    DEFINE OUTPUT PARAMETER TABLE FOR ttDados.

    FOR EACH bf
        WHERE bf.cod_emitente = icliente NO-LOCK.
        CREATE ttDados.
        BUFFER-COPY bf TO ttDados.
    END.
END PROCEDURE.



/*
 pegar o ultimo registro para determinado emitente considerando
 que a data de validade n∆o esteja vencida
*/


// procedure getVlPedidosNaoFat ped-venda

//procedure dpsEmAberto tit_acr


/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 hist_limit_cre_cli_id            inte
   19 cod_emitente                     inte
   20 dt_hr_registro                   datetm
   30 dt_validade                      date
   40 cod_usuario                      char
   50 vl_credito                       deci-2
   60 historico                        char

*/
