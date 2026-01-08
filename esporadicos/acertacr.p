DEFINE VARIABLE debito AS DECIMAL     NO-UNDO.
DEFINE VARIABLE credito AS DECIMAL     NO-UNDO.
/*DEFINE BUFFER bfAprop FOR aprop_ctbl_ap.
DEFINE BUFFER bfMovto FOR movto.*/
DEFINE  TEMP-TABLE tt
    FIELD numMovAp AS INT
    FIELD contaDebito AS CHAR FORMAT 'x(20)'
    FIELD contacredito AS CHAR FORMAT 'x(20)'
    FIELD valor AS DECIMAL
    FIELD codEstab AS CHAR.

FOR EACH aprop_ctbl_ap NO-LOCK
    WHERE ind_natur_lancto_ctbl = 'cr'
    AND cod_cta_ctbl = '19000005'
    /*OR (ind_natur_lancto_ctbl = 'cr'
    AND (cod_cta_ctbl = '21201001' OR cod_cta_ctbl = '21202001'))*/ ,
    EACH movto_tit_ap OF aprop_ctbl_ap NO-LOCK
    WHERE movto_tit_ap.ind_trans_ap_abrev = 'impl' /*AND log_bxa_contra_antecip*/ ,
    EACH tit_ap OF movto_tit_ap 
    WHERE tit_ap.cod_espec_docto = 'cr' NO-LOCK:
    DISP tit_ap.cod_tit_ap tit_ap.cod_espec_docto  tit_ap.cdn_fornecedor movto_tit_ap.dat_transacao val_movto_ap aprop_ctbl_ap.num_id_movto_tit_ap WITH   WIDTH 550.
    IF  ind_natur_lancto_ctbl = 'db' THEN
        ASSIGN debito = debito + val_movto_ap.
    ELSE
       ASSIGN credito = credito + val_movto_ap.
    CREATE tt.
    ASSIGN tt.numMovAP     = aprop_ctbl_ap.num_id_movto_tit_ap
           tt.contacredito = '21304002'
           tt.valor        = val_movto_ap
           tt.codEstab     = tit_ap.cod_estab
           tt.contaDebito  = '19000005'.
    /*RUN retornarDebitoBaixa(tit_ap.cod_estab, tit_ap.num_id_tit_ap, OUTPUT tt.contaDebito ).*/


END.
DISP debito credito .

PROCEDURE retornarDebitoBaixa:
DEFINE INPUT  PARAMETER codEstab  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER numTitulo AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER cConta   AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
FIND FIRST tit_ap 
    WHERE num_id_tit_ap = numtitulo
    AND   cod_estab = codestab USE-INDEX titap_token NO-LOCK NO-ERROR.
IF AVAIL tit_ap THEN DO:
   FIND FIRST movto_tit_ap OF tit_ap
       WHERE movto_tit_ap.ind_trans_ap_abrev = 'impl' NO-LOCK NO-ERROR.
   FIND FIRST aprop_ctbl_ap OF movto_tit_ap
       WHERE ind_natur_lancto_ctbl = 'cr' NO-LOCK NO-ERROR.
   IF AVAIL aprop_ctbl_ap THEN
      ASSIGN cConta = aprop_ctbl_ap.cod_cta_ctbl.

END.

END PROCEDURE.
ASSIGN credito = 0.
/*OUTPUT TO c:\temp\correcoesbaixa.txt.*/
FOR EACH tt:
    DISP tt WITH WIDTH 550.
    ASSIGN credito = credito + tt.valor.
    FOR EACH aprop_ctbl_ap
        WHERE  aprop_ctbl_ap.cod_estab = tt.codestab
        AND  aprop_ctbl_ap.num_id_movto_tit_ap = tt.nummovap    
        USE-INDEX aprpctbl_aprop EXCLUSIVE-LOCK.
        IF  ind_natur_lancto_ctbl = 'db' THEN 
            ASSIGN cod_cta_ctbl = tt.contadebito.
        ELSE
            ASSIGN cod_cta_ctbl = tt.contacredito.
    END.
END.
/*OUTPUT CLOSE.*/
DISP credito COLUMN-LABEL "valor".
