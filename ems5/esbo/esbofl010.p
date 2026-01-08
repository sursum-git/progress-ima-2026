/*
programa:   esbofl010
descriá∆o:  Objeto de Neg¢cio para tratamento da inclus∆o, alteraá∆o e busca 
de saldo anterior do fluxo de caixa, quando digitado no programa esfl001.
*/
{utp/ut-glob.i}


PROCEDURE atualizarSaldo:
    /* cria ou altera saldo informado para a empresa , data e tipo de valor de saldo passado por parametro */
    DEFINE INPUT  PARAMETER pData           AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pEmpresa        AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTipoValor      AS INTEGER     NO-UNDO. /* 1-informado 2-calculado */
    DEFINE INPUT  PARAMETER pValor          AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE cHistorico AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
    DEFINE VARIABLE lSaldo AS LOGICAL     NO-UNDO.

    RUN verificarSaldo(pData,pEmpresa,OUTPUT lSaldo).
    CASE pTipoValor:
        WHEN 1 THEN DO:
            ASSIGN cHistorico = 'Saldo Informado digitado:' + string(pValor,'->>>,>>>,>>9.99') +  
                ' - hora:' + string(NOW,"99/99/99 hh:mm:ss") +
                ' - Usu†rio: ' + c-seg-usuario +
                CHR(10)  + CHR(13).
            
        END.
        WHEN 2 THEN DO:
            ASSIGN cHistorico = 'Saldo Calculado:' + string(pValor,'->>>,>>>,>>9.99') +  
                ' - hora:' +  string(NOW,"99/99/99 hh:mm:ss") + ' - Usu†rio: ' + 
                c-seg-usuario  
                + CHR(10)  + CHR(13).
        END.                                                    
    END CASE.
    IF  lSaldo = NO THEN DO:
/*         MESSAGE 'vou criar a tabela, pois, n∆o existe saldo' */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.               */
        CREATE sdo_fluxo_caixa.
        ASSIGN sdo_fluxo_caixa.cod_empresa      = pEmpresa
               sdo_fluxo_caixa.data             = pData
               sdo_fluxo_caixa.data_hora_atu    = NOW
               sdo_fluxo_caixa.cod_usuario      = c-seg-usuario.
    END.
    FIND CURRENT sdo_fluxo_caixa EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN   sdo_fluxo_caixa.historico         = sdo_fluxo_caixa.historico  + cHistorico
              sdo_fluxo_caixa.vl_saldo_inf     = IF pTipoValor = 1 THEN pValor ELSE 0
              sdo_fluxo_caixa.vl_saldo_calc    = IF pTipoValor = 2 THEN pValor ELSE 0.
    RELEASE sdo_fluxo_caixa.

END.
PROCEDURE verificarSaldo:
    DEFINE INPUT  PARAMETER pData           AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pEmpresa        AS CHARACTER   NO-UNDO.
    /*DEFINE INPUT  PARAMETER pTipoValor      AS INTEGER     NO-UNDO.*/
    DEFINE OUTPUT PARAMETER lSaldo          AS LOGICAL     NO-UNDO.

    FIND FIRST sdo_fluxo_caixa
        WHERE sdo_fluxo_caixa.cod_empresa   = pEmpresa
        AND   sdo_fluxo_caixa.data          = pData
         NO-LOCK NO-ERROR.
    ASSIGN lSaldo = AVAIL sdo_fluxo_caixa.

END PROCEDURE.

PROCEDURE buscarSaldo:
    DEFINE INPUT  PARAMETER pData           AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pEmpresa        AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER dValorInf       AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER dValorCalc      AS DECIMAL     NO-UNDO.
    FIND FIRST sdo_fluxo_caixa
        WHERE sdo_fluxo_caixa.cod_empresa   = pEmpresa
        AND   sdo_fluxo_caixa.data          = pData NO-LOCK NO-ERROR.
    ASSIGN dValorInf  = IF AVAIL sdo_fluxo_caixa THEN sdo_fluxo_caixa.vl_saldo_inf ELSE 0
           dValorCalc = IF AVAIL sdo_fluxo_caixa THEN sdo_fluxo_caixa.vl_saldo_calc ELSE 0.
/*     MESSAGE  AVAIL sdo_fluxo_caixa SKIP         */
/*              pEmpresa SKIP                      */
/*              pData SKIP                         */
/*              VIEW-AS ALERT-BOX INFO BUTTONS OK. */
END PROCEDURE.


PROCEDURE buscarHistorico:
    DEFINE INPUT  PARAMETER pData           AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pEmpresa        AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER cHistorico      AS CHARACTER   NO-UNDO FORMAT 'x(8000)' INIT ''.
    FIND FIRST sdo_fluxo_caixa
        WHERE sdo_fluxo_caixa.cod_empresa   = pEmpresa
        AND   sdo_fluxo_caixa.data          = pData
         NO-LOCK NO-ERROR.
    IF AVAIL  sdo_fluxo_caixa THEN 
       ASSIGN cHistorico = sdo_fluxo_caixa.historico .

END PROCEDURE.

/*PROCEDURE alterarSaldo:
    DEFINE INPUT  PARAMETER pData           AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pEmpresa        AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTipoValor      AS INTEGER     NO-UNDO.
    FIND FIRST sdo_fluxo_caixa
        WHERE sdo_fluxo_caixa.cod_empresa   = pEmpresa
        AND   sdo_fluxo_caixa.data          = pData EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL sdo_fluxo_caixa THEN DO:
       CASE pTipoValor:
           WHEN 1 THEN DO:

           END.
       
       END CASE.


    END.

END PROCEDURE.*/
