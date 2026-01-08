

DISABLE TRIGGERS for load OF tit_acr.
DISABLE TRIGGERS for load OF movto_tit_acr.
DISABLE TRIGGERS for load OF aprop_ctbl_acr.
DISABLE TRIGGERS for load OF val_movto_tit_acr.
DISABLE TRIGGERS for load OF demonst_ctbl_acr .
DISABLE TRIGGERS for load OF his_tit_acr_histor.
DISABLE TRIGGERS for load OF item_bord_acr      .
DISABLE TRIGGERS for load OF item_lote_impl_tit_acr .
DISABLE TRIGGERS for load OF repres_tit_acr_fechado.
DISABLE TRIGGERS for load OF repres_tit_acr        .

DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
DEFINE TEMP-TABLE tt
       FIELD titulo AS CHAR
       FIELD logAlt AS LOGICAL.
OUTPUT TO c:\temp\tit_acr_01.txt.
FOR EACH tit_acr
    WHERE tit_acr.dat_emis_docto  >= 11.03.2017
    AND   tit_acr.dat_emis_docto  <= 11.06.2017
    AND cod_estab = '501'
    AND tit_acr.cod_tit_acr >= '0088202'
    AND tit_acr.cod_tit_acr <= '0088275'
    BY tit_acr.cod_tit_acr.
    ASSIGN iCont = 0.
    FOR EACH movto_tit_acr OF tit_acr:
        ASSIGN iCont = iCont + 1.
    END.
    FIND FIRST tt
        WHERE tt.titulo = tit_acr.cod_tit_acr NO-ERROR.
    IF NOT AVAIL tt THEN DO:
       CREATE tt.
       ASSIGN tt.titulo = tit_acr.cod_tit_acr
              tt.logAlt = iCont > 1.
    END.
    IF icont > 1 THEN DO:
       DISP tit_acr.cod_tit_acr
            tit_acr.cod_ser_docto
            tit_acr.cod_parcela
            tit_acr.cdn_cliente.
       FIND FIRST fat-duplic
           WHERE fat-duplic.cod-estabel = '5'
           AND   fat-duplic.nr-fatura = tit_acr.cod_tit_acr
           AND   fat-duplic.serie     = tit_acr.cod_ser_docto
           AND   fat-duplic.parcela   = tit_acr.cod_parcela
           NO-LOCK NO-ERROR.
       DISP AVAIL fat-duplic.
    END.
       
END.


FOR EACH tt
    WHERE tt.logAlt = YES  BY titulo:
    DISP tt.
END.


