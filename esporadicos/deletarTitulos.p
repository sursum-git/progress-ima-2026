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
FOR EACH tit_acr
    WHERE tit_acr.dat_emis_docto  >= 11.03.2017
    AND   tit_acr.dat_emis_docto  <= 11.06.2017
    AND cod_estab = '501'
    AND tit_acr.cod_tit_acr >= '0088216'
    AND tit_acr.cod_tit_acr <= '0088216'
    AND tit_acr.cod_parcela = '01'
    AND tit_acr.cod_espec_docto = 'dm'.
    
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
    IF icont = 1 THEN
       RUN deletarTitulo(ROWID(tit_acr)).
END.


FOR EACH tt
    WHERE tt.logAlt = YES  BY titulo:
    DISP tt.
END.

PROCEDURE deletarTitulo:

DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
FIND tit_acr WHERE
    ROWID(tit_acr) = pRowid NO-ERROR.
IF AVAIL tit_acr THEN DO:
   FOR EACH compl_movto_tit_acr OF tit_Acr:
       DELETE compl_movto_tit_acr. 
   END.
   FOR EACH movto_tit_acr OF tit_acr:
       FOR EACH val_movto_tit_acr OF movto_tit_acr:
           DELETE val_movto_tit_acr.
       END.
       FOR EACH aprop_ctbl_acr OF movto_tit_acr.
           DELETE aprop_ctbl_acr.
       END.
       DELETE movto_tit_acr.
   END.
   FOR EACH val_tit_acr OF tit_acr.
       DELETE val_tit_acr.
   END.

   FOR EACH demonst_ctbl_acr OF tit_acr:
       DELETE demonst_ctbl_acr.
   END.

   FOR EACH his_tit_acr_histor OF tit_acr:
       DELETE his_tit_acr_histor.
   END.

   FOR EACH item_bord_acr OF tit_acr:
       DELETE ITEM_bord_acr.
   END.

   FOR EACH item_lote_impl_tit_acr OF tit_acr:
       DELETE ITEM_lote_impl_tit_acr.
   END.

   FOR EACH repres_tit_acr_fechado OF tit_acr:
       DELETE repres_tit_Acr_fechado.
   END.
   FOR EACH repres_tit_acr 
       WHERE repres_tit_acr.cod_estab = repres_tit_acr.cod_estab
       AND   repres_tit_acr.num_id_tit_acr = repres_tit_acr.num_id_tit_Acr:
       DELETE repres_tit_acr.
   END.
   DELETE tit_acr.
END.

END PROCEDURE.
