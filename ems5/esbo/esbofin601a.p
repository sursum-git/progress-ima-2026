/***************************************************************
programa:esbofin601a.p
Objetivo: Retornar as Previsäes feitas manualmente no modulo de 
fluxo de caixa para compor os valores do fluxo de caixa conforme 
parametros definidos.
***************************************************************/


{esbo/esbofin601a.i}

DEFINE VARIABLE empresaIni  AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE empresaFim  AS CHARACTER   NO-UNDO INIT 'zzz'.
DEFINE VARIABLE estabIni    AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE estabFim    AS CHARACTER   NO-UNDO INIT 'zzz'.
DEFINE VARIABLE dataIni     AS DATE        NO-UNDO INIT 01.01.2001.
DEFINE VARIABLE dataFim     AS DATE        NO-UNDO INIT 12.31.9999.
DEFINE VARIABLE fluxo      AS INTEGER     NO-UNDO INIT 0.

PROCEDURE definirEmpresa:
    DEFINE INPUT PARAMETER pEmpresa AS CHARACTER   NO-UNDO.

    ASSIGN empresaIni =  pEmpresa
           empresaFim =  pEmpresa.
END PROCEDURE.

PROCEDURE definirEstab:
    DEFINE INPUT PARAMETER pEstab AS CHARACTER   NO-UNDO.
    ASSIGN estabIni =  pEstab
           estabFim =  pEstab.
END PROCEDURE.


PROCEDURE definirIntervalEstab:
    DEFINE INPUT PARAMETER pEstabIni AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pEstabFim AS CHARACTER   NO-UNDO.
    ASSIGN estabIni =  pEstabIni
           estabFim =  pEstabFim.
END PROCEDURE.

PROCEDURE definirDatas:
    DEFINE INPUT  PARAMETER pDataIni AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pDataFim AS DATE        NO-UNDO.
    ASSIGN dataIni = pDataIni
           dataFim = pDataFim .

END PROCEDURE.

PROCEDURE limparDados:
EMPTY TEMP-TABLE ttPrevisao.

END.

PROCEDURE definirFluxo:
    DEFINE INPUT  PARAMETER pFluxo AS INT        NO-UNDO.
    ASSIGN fluxo = pFluxo .
END PROCEDURE.


PROCEDURE buscarPrevisoes:
    DEFINE VARIABLE iSinal AS INTEGER     NO-UNDO.
    OUTPUT TO C:\TEMP\LOG_PREVICOES.TXT.
    PUT 'EMPRESA INI:'  empresaIni    SKIP
        'EMPRESA FIM:'  empresaFim    SKIP
        'ESTAB INI:'    estabIni      SKIP
        'ESTAB FIM:'    estabFim      SKIP
        'DATA INI:'     dataIni       SKIP
        'DATA FIM:'     dataFim       SKIP
        'FLUXO:'        fluxo         SKIP.
    
    FOR EACH movto_fluxo_cx NO-LOCK
        WHERE movto_fluxo_cx.cod_empresa        >= empresaIni
        AND   movto_fluxo_cx.cod_empresa        <= empresaFim
        AND   movto_fluxo_cx.cod_estab          >= estabIni
        AND   movto_fluxo_cx.cod_estab          <= estabFim 
        AND   movto_fluxo_cx.dat_movto_fluxo_cx >= dataIni
        AND   movto_fluxo_cx.dat_movto_fluxo_cx <= dataFim
        AND   movto_fluxo_cx.num_fluxo_cx        = fluxo 
        AND   movto_fluxo_cx.ind_tip_movto_fluxo_cx = 'PR':
        PUT 'ENTREI' SKIP.

        IF movto_fluxo_cx.ind_fluxo_movto_cx = 'ent' THEN
           ASSIGN  iSinal = 1.
        ELSE
           ASSIGN  iSinal = -1.
        CREATE ttPrevisao.
        ASSIGN ttPrevisao.num_fluxo_cx                  =  movto_fluxo_cx.num_fluxo_cx
               ttPrevisao.cod_estab                     =  movto_fluxo_cx.cod_estab
               ttPrevisao.cod_empresa                   =  movto_fluxo_cx.cod_empresa
               ttPrevisao.dat_movto_fluxo_cx            =  movto_fluxo_cx.dat_movto_fluxo_cx
               ttPrevisao.cod_tip_fluxo_financ          =  movto_fluxo_cx.cod_tip_fluxo_financ
               ttPrevisao.ind_tip_movto_fluxo_cx        =  movto_fluxo_cx.ind_tip_movto_fluxo_cx
               ttPrevisao.ind_fluxo_movto_cx            =  movto_fluxo_cx.ind_fluxo_movto_cx
               ttPrevisao.cod_modul_dtsul               =  movto_fluxo_cx.cod_modul_dtsul
               ttPrevisao.des_histor_movto_fluxo_cx     =  movto_fluxo_cx.des_histor_movto_fluxo_cx
               ttPrevisao.val_movto_fluxo_cx            =  movto_fluxo_cx.val_movto_fluxo_cx * iSinal.
    END.
    OUTPUT CLOSE.
END.

PROCEDURE retornarRegistros:
    DEFINE OUTPUT PARAM TABLE FOR ttPrevisao.
END PROCEDURE.



/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 num_fluxo_cx                     inte        im
   20 dat_movto_fluxo_cx               date        im
   30 num_seq_movto_fluxo_cx           inte        im
   40 cod_estab                        char        im
   50 cod_unid_negoc                   char        im
   60 cod_tip_fluxo_financ             char        im
   70 ind_fluxo_movto_cx               char        m
   80 ind_tip_movto_fluxo_cx           char        im
   90 cod_modul_dtsul                  char        im
  100 val_movto_fluxo_cx               deci-2      m
  110 val_perc_cop_fluxo_cx            deci-2      m
  120 cod_histor_padr                  char        im
  130 des_histor_movto_fluxo_cx        char        im
  140 cod_empresa                      char        m
  150 ind_tip_secao_fluxo_cx           char        m
  160 num_id_movto_fluxo_cx            inte        im
  170 cod_livre_1                      char
  180 dat_prev_orig                    date
  190 cod_estab_orig                   char        m
  200 cod_livre_2                      char
  210 dat_livre_1                      date
  220 dat_livre_2                      date
  230 log_livre_1                      logi
  240 log_livre_2                      logi
  250 num_livre_1                      inte
  260 num_livre_2                      inte
  270 val_livre_1                      deci-4
  280 val_livre_2                      deci-4
*/
