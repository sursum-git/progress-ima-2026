FOR EACH tit_ap NO-LOCK
  WHERE cod_empresa = '500'
  AND   cod_estab = '501'
  /*AND   cod_serie = '3'*/
  AND   cod_espec_docto = 'SE'
  AND   cod_tit_ap = '1293/14':
  DISP tit_ap WITH 1 COL WIDTH 550 TITLE "tit ap".
  FOR EACH movto_tit_ap OF tit_ap NO-LOCK:
      DISP movto_tit_ap WITH 1 COL WIDTH 550.
      
      /*
      FOR EACH compl_movto_pagto OF movto_tit_ap NO-LOCK:
          DISP compl_movto_pagto WITH 1 COL WIDTH 550 TITLE "complemento".  
      END.
      FOR EACH val_movto_ap OF movto_tit_ap NO-LOCK:
          DISP val_movto_ap WITH 1 COL WIDTH 550 TITLE "val.movto.ap".
      END.
      FOR EACH val_movto_ap_correc_val OF movto_tit_ap NO-LOCK:
          DISP val_movto_ap_correc_val WITH 1 COL WIDTH 550 TITLE "corre‡Æo".
      END.
      */

  END.

END.
