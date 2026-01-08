DEFINE VARIABLE dSaldo AS DECIMAL     NO-UNDO.
OUTPUT TO p:\registrosApagadosMovtoAP.txt.
DO TRANSACTION ON ERROR UNDO:
     FOR EACH movto_cta_corren WHERE                                                
              movto_cta_corren.cod_cta_corren = "47154-2" AND                       
              movto_cta_corren.dat_movto_cta_corren = 02.22.2017 AND                
              movto_cta_corren.num_seq_movto_cta_corren = 3.      
         FOR EACH movto_tit_ap OF movto_cta_corren:                                 
             ASSIGN dSaldo = movto_tit_ap.val_movto_ap.                             
             EXPORT DELIMITER "|" movto_tit_ap.                                     
             FOR EACH val_movto_Ap OF movto_tit_ap:                                 
                 EXPORT DELIMITER "|" val_movto_ap.                                 
                 DELETE val_movto_ap.                                               
             END.                                                                   
             FOR EACH aprop_ctbl_ap OF movto_tit_ap.                                
                 FOR EACH val_aprop_ctbl_ap OF aprop_ctbl_ap.                       
                     EXPORT DELIMITER "|" val_aprop_ctbl_ap.                        
                     DELETE val_aprop_ctbl_ap.                                      
                 END.                                                               
                 EXPORT DELIMITER "|" aprop_ctbl_ap.                                
                 DELETE aprop_ctbl_ap.                                              
             END.                                                                   
                                                                                    
             FIND FIRST tit_ap OF movto_tit_ap NO-LOCK NO-ERROR.                    
             IF AVAIL tit_ap THEN DO:                                               
                ASSIGN tit_ap.val_sdo_tit_ap = tit_ap.val_sdo_tit_ap + dSaldo.      
                FOR EACH val_tit_ap OF tit_ap:                                      
                    ASSIGN val_sdo_tit_ap = tit_ap.val_sdo_tit_ap.                  
                END.                                                                
             END.                                                                   
             DELETE movto_tit_ap.                                                   
         END.                                                                       
         EXPORT movto_cta_corren.
         DELETE movto_cta_corren.
     END. 
END.































