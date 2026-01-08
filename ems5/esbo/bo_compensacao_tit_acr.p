{esbo\bo_compensacao_tit_acr.i}

DEFINE VARIABLE dat_transacao_ini AS DATE        NO-UNDO INIT 01.01.2001.
DEFINE VARIABLE dat_transacao_fim AS DATE        NO-UNDO INIT 12.31.2999.
DEFINE BUFFER bf_movto_tit_acr FOR movto_tit_acr.
DEFINE BUFFER bf_tit_acr FOR tit_acr.


PROCEDURE setDataTransacao:

DEFINE INPUT  PARAMETER dtIni AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER dtFim AS DATE        NO-UNDO.

ASSIGN dat_transacao_ini = dtIni
       dat_transacao_fim = dtFim.

END PROCEDURE.


PROCEDURE buscarRegistros:

FOR EACH movto_tit_acr 
    WHERE movto_tit_acr.dat_transacao >= dat_transacao_ini
    AND   movto_tit_acr.dat_transacao <= dat_transacao_fim 
    AND   movto_tit_acr.ind_trans_acr_abrev = 'liq'
    AND   movto_tit_acr.log_movto_estordo = NO,
    EACH ems5.espec_docto OF movto_tit_acr
    WHERE espec_docto.ind_tip_espec_docto = 'antecipa‡Æo'
    BY movto_tit_acr.dat_transacao.
    FIND FIRST tit_acr OF movto_tit_acr NO-LOCK NO-ERROR.
    FIND FIRST emitente
        WHERE emitente.cod-emitente = tit_acr.cdn_cliente
        NO-LOCK NO-ERROR.

    FOR EACH    bf_movto_tit_acr 
        WHERE  bf_movto_tit_acr.cod_estab   = movto_tit_acr.cod_estab_tit_acr_pai
        /*AND     bf_movto_tit_acr.cod_refer             = movto_tit_acr.cod_refer*/
        AND     bf_movto_tit_acr.num_id_movto_tit_acr  = movto_tit_acr.num_id_movto_tit_acr_pai
        NO-LOCK USE-INDEX mvtttcr_token .
       FIND FIRST bf_tit_acr OF bf_movto_tit_acr
           NO-LOCK NO-ERROR.
       IF AVAIL bf_tit_acr THEN DO:
            CREATE ttCompCR.
            ASSIGN 
            ttCompCR.cod_estab                =    tit_acr.cod_estab                 
            ttCompCR.nome_emit                =    emitente.nome-emit                
            ttCompCR.cod_espec_docto          =    tit_acr.cod_espec_docto           
            ttCompCR.cod_ser_docto            =    tit_acr.cod_ser_docto             
            ttCompCR.cod_parcela              =    tit_acr.cod_parcela               
            ttCompCR.cdn_cliente              =    tit_acr.cdn_cliente               
            ttCompCR.cod_tit_acr              =    tit_acr.cod_tit_acr               
            ttCompCR.dat_transacao            =    movto_tit_acr.dat_transacao       
            ttCompCR.dat_liquidac_tit_acr     =    movto_tit_acr.dat_liquidac_tit_acr
            ttCompCR.val_movto_tit_ar         =    movto_tit_acr.val_movto_tit_acr   
            ttCompCR.cod_espec_docto_comp     =    bf_tit_acr.cod_espec_docto            
            ttCompCR.cod_ser_docto_comp       =    bf_tit_acr.cod_ser_docto          
            ttCompCR.cod_parcela_comp         =    bf_tit_acr.cod_parcela        
            ttCompCR.cod_tit_acr_comp         =    bf_tit_acr.cod_tit_acr .
       END.
    END.
END.
END PROCEDURE.

PROCEDURE retornarRegistros:
DEFINE OUTPUT PARAMETER TABLE FOR ttCompCR.

END PROCEDURE.

PROCEDURE exportarRegistros:

DEFINE INPUT  PARAMETER cArquivo AS CHARACTER FORMAT 'x(200)'   NO-UNDO.
DEFINE VARIABLE logBranco AS LOGICAL     NO-UNDO INIT YES.

OUTPUT TO VALUE(cArquivo).
FOR EACH ttCompCR:
    ASSIGN logBranco = NO.
    EXPORT DELIMITER "|" ttCompCR.
END.
IF logBranco THEN
   PUT "NÆo Existem Registros." SKIP.
OUTPUT CLOSE.
END PROCEDURE.


       
             


           
