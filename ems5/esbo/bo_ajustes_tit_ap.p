/*
BO para ajustes de titulos do contas a pagar
*/


PROCEDURE eliminarTitAp:

DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.

FIND tit_ap
    WHERE rowid(tit_ap) = pRowid EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL tit_ap THEN DO:
   FOR EACH movto_tit_ap OF tit_ap.
       FOR EACH aprop_ctbl_ap OF movto_tit_ap:
           DELETE aprop_ctbl_ap.
       END.
       FOR EACH compl_movto_pagto OF movto_tit_ap:
           DELETE compl_movto_pagto.
       END.
       FOR EACH impto_pagto OF movto_tit_ap:
           DELETE impto_pagto.
       END.

       FOR EACH rat_movto_tit_ap OF movto_tit_ap:
           DELETE rat_movto_tit_ap.
       END.

       FOR EACH relacto_acum_pagto OF movto_tit_ap:
           DELETE relacto_acum_pagto.
       END.
       FOR EACH val_movto_ap OF movto_tit_ap:
           DELETE val_movto_ap.
       END.

       FOR EACH val_movto_ap_correc_val OF movto_tit_ap:
           DELETE val_movto_ap_correc_val.
       END.

       FOR EACH item_cheq_ap OF movto_tit_ap:
           DELETE ITEM_cheq_ap.
       END.
       FOR EACH impto_val_agreg_movto_ap OF movto_tit_ap:
           DELETE impto_val_agreg_movto_ap.
       END.
       DELETE movto_tit_ap.
   END.
   FOR EACH relacto_tit_ap  OF tit_ap:
       DELETE relacto_tit_ap.
   END.
   FOR EACH proces_pagto OF tit_ap:
       FOR EACH ext_proces_pagto OF proces_pagto:
           DELETE ext_proces_pagto.
       END.
       DELETE proces_pagto.
   END.
   FOR EACH compl_impto_retid_ap OF tit_ap.
    DELETE compl_impto_retid_ap.
   END.
   FOR EACH compl_retenc_impto_pagto OF tit_ap.
       DELETE compl_retenc_impto_pagto.
   END.
   FOR EACH movto_tit_ap_avp OF tit_ap.
       DELETE movto_tit_ap_avp.
   END.
   FOR EACH relacto_tit_em_bco OF tit_ap.
       DELETE  relacto_tit_em_bco.
   END.
   
   FOR EACH relac_movto_tit_ap_avp OF tit_ap.
      DELETE relac_movto_tit_ap_avp.
   END.
   
   FOR EACH tit_ap_bcio OF tit_ap.
       DELETE tit_ap_bcio.
   END.
   
   FOR EACH val_tit_ap OF tit_ap.
       DELETE val_tit_ap.
   END.
   
   FOR EACH impto_nao_retid_ap OF tit_ap.
       DELETE impto_nao_retid_ap.
   END.
   DELETE tit_ap.
END.             

END PROCEDURE.

PROCEDURE zerarTitAP:


END PROCEDURE.
