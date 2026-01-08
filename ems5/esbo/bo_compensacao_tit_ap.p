{esbo\bo_compensacao_tit_ap.i}

DEFINE VARIABLE dat_transacao_ini AS DATE        NO-UNDO INIT 01.01.2001.
DEFINE VARIABLE dat_transacao_fim AS DATE        NO-UNDO INIT 12.31.2999.
DEFINE BUFFER bf_movto_tit_ap     FOR movto_tit_ap.
DEFINE BUFFER bf_tit_ap           FOR tit_ap.


PROCEDURE setDataTransacao:

DEFINE INPUT  PARAMETER dtIni AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER dtFim AS DATE        NO-UNDO.

ASSIGN dat_transacao_ini = dtIni
       dat_transacao_fim = dtFim.

END PROCEDURE.


PROCEDURE buscarRegistros:
DEFINE VARIABLE cListaEspec AS CHARACTER   NO-UNDO.

FOR EACH movto_tit_ap 
    WHERE movto_tit_ap.dat_transacao >= dat_transacao_ini
    AND   movto_tit_ap.dat_transacao <= dat_transacao_fim 
    AND   movto_tit_ap.ind_trans_ap_abrev = 'BXA',
    EACH ems5.espec_docto OF movto_tit_ap
    WHERE espec_docto.ind_tip_espec_docto = 'antecipa‡Æo'
    BY movto_tit_ap.dat_transacao.             
    FIND FIRST tit_ap OF movto_tit_ap NO-LOCK NO-ERROR.
    FIND FIRST emitente
        WHERE emitente.cod-emitente = tit_ap.cdn_fornecedor
        NO-LOCK NO-ERROR.
    FOR EACH bf_movto_tit_ap 
        WHERE  bf_movto_tit_ap.cod_estab              = movto_tit_ap.cod_estab_tit_ap_pai
        AND    bf_movto_tit_ap.num_id_movto_tit_ap    = movto_tit_ap.num_id_movto_tit_ap_pai
        NO-LOCK USE-INDEX mvtttp_token.

       FIND FIRST bf_tit_ap OF bf_movto_tit_ap
           NO-LOCK NO-ERROR.
      /* MESSAGE AVAIL bf_tit_ap
           VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
       IF AVAIL bf_tit_ap THEN DO:
            CREATE ttCompAP.
            ASSIGN 
            ttCompAP.cod_estab                =    tit_ap.cod_estab                 
            ttCompAP.nome_emit                =    emitente.nome-emit                
            ttCompAP.cod_espec_docto          =    tit_ap.cod_espec_docto           
            ttCompAP.cod_ser_docto            =    tit_ap.cod_ser_docto             
            ttCompAP.cod_parcela              =    tit_ap.cod_parcela               
            ttCompAP.cdn_fornecedor           =    tit_ap.cdn_fornecedor               
            ttCompAP.cod_tit_ap               =    tit_ap.cod_tit_ap               
            ttCompAP.dat_transacao            =    movto_tit_ap.dat_transacao       
            ttCompAP.val_movto_ap             =    movto_tit_ap.val_movto_ap   
            ttCompAP.cod_espec_docto_comp     =    bf_tit_ap.cod_espec_docto            
            ttCompAP.cod_ser_docto_comp       =    bf_tit_ap.cod_ser_docto          
            ttCompAP.cod_parcela_comp         =    bf_tit_ap.cod_parcela        
            ttCompAP.cod_tit_ap_comp          =    bf_tit_ap.cod_tit_ap .
       END.
    END.
END.
END PROCEDURE.

PROCEDURE retornarRegistros:
DEFINE OUTPUT PARAMETER TABLE FOR ttCompAP.

END PROCEDURE.

PROCEDURE exportarRegistros:

DEFINE INPUT  PARAMETER cArquivo AS CHARACTER FORMAT 'x(200)'   NO-UNDO.
DEFINE VARIABLE logBranco AS LOGICAL     NO-UNDO INIT YES.

OUTPUT TO VALUE(cArquivo).
FOR EACH ttCompAP:
    ASSIGN logBranco = NO.
    EXPORT DELIMITER "|" ttCompAP.
END.
IF logBranco =  YES THEN
   PUT "NÆo Existem Registros." SKIP.
OUTPUT CLOSE.
END PROCEDURE.


       
             


           
