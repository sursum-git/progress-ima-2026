
//DEFINE VARIABLE p_cod_empresa       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE p_cod_estab         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE p_cod_espec_docto   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE p_cod_ser_docto     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE p_cdn_fornecedor    AS INTEGER     NO-UNDO.
DEFINE VARIABLE p_cod_tit_ap        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE p_cod_parcela       AS CHARACTER   NO-UNDO.



 

    UPDATE 
        // p_cod_empresa     
         p_cod_estab       
         p_cod_espec_docto 
         p_cod_ser_docto   
         p_cdn_fornecedor  
         p_cod_tit_ap      
         p_cod_parcela     WITH 1 COL .
    FOR EACH proces_pagto
        WHERE  proces_pagto.cod_estab                    =  p_cod_estab  
        AND  proces_pagto.cod_espec_docto              =  p_cod_espec_docto             
        AND  proces_pagto.cod_ser_docto                =  p_cod_ser_docto                
        AND  proces_pagto.cdn_fornecedor               =  p_cdn_fornecedor               
        AND  proces_pagto.cod_tit_ap                   =  p_cod_tit_ap                   
        AND  proces_pagto.cod_parcela                  =  p_cod_parcela .
          DISP proces_pagto WITH 1 COL WIDTH 550.
          MESSAGE 'deseja deletar?' UPDATE lresposta AS log
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL.
         IF lresposta THEN
           DELETE proces_pagto.
    END.

