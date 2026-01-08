/*
Programa: BOFIN533A
Objetivo: Retornar os saldos das contas correntes que est∆o configurados 
para fazerem parte do fluxo de caixa. Ser∆o consideradas apenas contas corrente
que atualizam o fluxo de caixa.
O retorno ser† por temp-table
*/


/*variaveis gerais da BO e  temp-table*/
DEFINE VARIABLE dataCorte       AS DATE        NO-UNDO.
{esbo\esbofin533a.i}

PROCEDURE definirDataCorte:
   DEFINE INPUT  PARAMETER data AS DATE        NO-UNDO.
   ASSIGN dataCorte = data.
END.

PROCEDURE limparDados:
EMPTY TEMP-TABLE ttSaldo.

END.

PROCEDURE buscarSaldos:
    DEFINE VARIABLE dValor          AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE iSinal          AS INTEGER     NO-UNDO.
    OUTPUT TO c:\temp\cta_corren_log.txt.
    FOR EACH cta_corren  NO-LOCK
        WHERE log_atualiz_fluxo_cx = YES,
        EACH movto_cta_corren OF cta_corren NO-LOCK
        WHERE movto_cta_corren.ind_tip_movto_cta_corren = 're' /*realizado*/
        AND  movto_cta_corren.dat_movto_cta_corren < dataCorte:

        FIND FIRST ttSaldo
            WHERE ttSaldo.cod_cta_corren = cta_corren.cod_cta_corren NO-LOCK NO-ERROR.
        IF NOT AVAIL ttSaldo THEN DO:
           FIND FIRST ems5.banco 
               WHERE  banco.cod_banco = cta_corren.cod_banco NO-LOCK NO-ERROR.

           CREATE ttSaldo.
           ASSIGN ttSaldo.cod_cta_corren = cta_corren.cod_cta_corren
                  ttSaldo.cod_banco      = cta_corren.cod_banco
                  ttSaldo.nome_banco     = IF AVAIL ems5.banco THEN banco.nom_banco ELSE ''
                  ttSaldo.cod_estab      = cta_corren.cod_estab .
        END.

        IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN
           ASSIGN iSinal = 1.
        ELSE
          ASSIGN iSinal = -1.
             
        ASSIGN  dValor = movto_cta_corren.val_movto_cta_corren  * iSinal.
                ttSaldo.vl_saldo   = ttSaldo.vl_saldo + dValor. 
        EXPORT DELIMITER "|" movto_cta_corren.cod_cta_corren 
             movto_cta_corren.dat_movto_cta_corren
             dValor.
    END.
    OUTPUT CLOSE.
END.

PROCEDURE retornarRegistros:
  DEFINE OUTPUT PARAM TABLE FOR ttSaldo.
END PROCEDURE.

PROCEDURE retornarSaldoATual:
    DEFINE OUTPUT PARAMETER pSaldo AS DECIMAL     NO-UNDO.
    FOR EACH ttSaldo:
        ASSIGN pSaldo = pSaldo + ttSaldo.vl_saldo.
    END.


END PROCEDURE.


