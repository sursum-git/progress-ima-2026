FOR EACH tit_ap WHERE
         tit_ap.val_sdo_tit_ap > 0 NO-LOCK.

    DISP tit_ap.cod_tit_ap
         tit_ap.dat_vencto_tit_ap
         tit_ap.ind_origin_tit_ap.                

    /*
    FOR EACH ems5.movto_tit_ap OF tit_ap NO-LOCK:

        IF tit_ap.ind_origin_tit_ap <> "REC" /*origem recebimento*/ ) OR NOT AVAIL tit_Ap  THEN DO:
          RUN incluirLog IN hLog('agrupamento Geral','nivel 3- Origem NAO EH Recebimento ou Movimento Sem Titulo').
              IF AVAIL tit_ap THEN DO:
                 IF SUBSTR(tit_ap.cod_tit_ap,1,2) = 'hr' OR espec_docto.ind_tip_espec_docto = 'imposto retido' THEN DO : /*caso o titulo seja da folha*/     
                    RUN piBuscarApropFolhaImp( ROWID(movto_tit_ap),
                                            movto_cta_corren.dat_transacao,
                                            movto_cta_corren.ind_fluxo_movto_cta_corren,
                                            movto_tit_ap.val_movto_ap,
                                            movto_cta_corren.cod_cta_corren,
                                            movto_cta_corren.num_seq_movto_cta_corren,                 
                                            movto_cta_corren.num_id_movto_cta_corren ).
                 END.                                                                                  
                 ELSE DO:     
                    RUN incluirLog IN hLog('agrupamento Geral','====inicio apropNormal - achou titulo?' + string(AVAIL tit_ap) + ' p========================================').

                    RUN piBuscarApropNormal(movto_cta_corren.dat_transacao,                            
                                            movto_cta_corren.ind_fluxo_movto_cta_corren,               
                                            movto_tit_ap.val_movto_ap,                                 
                                            movto_tit_ap.val_juros,                                    
                                            movto_tit_ap.val_desconto,                                 
                                            movto_cta_corren.cod_cta_corren,                           
                                            movto_cta_corren.num_seq_movto_cta_corren,                 
                                            movto_cta_corren.num_id_movto_cta_corren,                  
                                            ROWID(tit_ap)
                                            ).                
                 END.                                                                                  
              END.
              ELSE DO:
                 RUN incluirLog IN hLog('agrupamento Geral','nivel 3- Lancamento com ORIGEM no AP, PEF ou Adiantamento:' + string(movto_tit_ap.val_movto_ap)).
                 FOR EACH ems5.antecip_pef_pend OF movto_tit_ap  NO-LOCK:
                     FIND FIRST ems5.fornecedor OF ems5.antecip_pef_pend NO-LOCK NO-ERROR.
                     IF AVAIL fornecedor THEN
                     FIND FIRST ems5.grp_fornec where
                           ems5.grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
                     FOR EACH ems5.aprop_ctbl_pend_ap OF antecip_pef_pend
                         NO-LOCK:
                         CREATE tt.                                                                                                                                                   
                         ASSIGN  
                         tt.cod_modulo        = movto_cta_corren.cod_modul_dtsul
                         tt.cod_empresa       = aprop_ctbl_pend_ap.cod_empresa                                                                                                             
                         tt.cod_estab         = aprop_ctbl_pend_ap.cod_estab                                                                                                               
                         tt.cod_emitente      = string(ems5.antecip_pef_pend.cdn_fornecedor)                                                                           
                         tt.desc_emitente     =  IF AVAIL fornecedor THEN fornecedor.nom_pessoa  ELSE ''                                                                                   
                         tt.data              =  movto_cta_corren.dat_transacao                                                                                                        
                         tt.conta_contabil    =  aprop_ctbl_pend_ap.cod_cta_ctbl                                                                                                           
                         tt.valor             = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN  movto_tit_ap.val_movto_ap ELSE movto_tit_ap.val_movto_ap * -1     
                         tt.conta_corrente    =  movto_cta_corren.cod_cta_corren                                                                                                      
                         tt.base              =  pBase                                                                                                                                 
                         tt.origem            =  'Contas a Pagar - PEF'                                                                                                                     
                         tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren                                                                                           
                         tt.cc                = aprop_ctbl_pend_ap.cod_ccusto
                         tt.ccusto_gerencial  = tt.cc
                         tt.sequencia         = movto_cta_corren.num_seq_movto_cta_corren
                         tt.id_movto_corren   = movto_cta_corren.num_id_movto_cta_corren
                         tt.grupo_emitente    = IF AVAIL ems5.grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  ELSE ''
                         tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
                         tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(10),";")  .
                     END.
                 END.
              END.
           END.
           ELSE DO:
               RUN incluirLog IN hLog('agrupamento Geral','nivel 3 - Origem Recebimento').
               CASE ems5.cta_corren.cod_estab:
                   WHEN '501' THEN
                        RUN piBuscarNotasRateioEms2MED(tit_ap.cod_ser_docto,tit_ap.cod_tit_ap,tit_ap.cdn_fornecedor,tit_ap.cod_parcela, OUTPUT rRowidNota ).
                   WHEN '101' OR WHEN '108' THEN
                        RUN piBuscarNotasRateioEms2IMA(tit_ap.cod_ser_docto,tit_ap.cod_tit_ap,tit_ap.cdn_fornecedor,tit_ap.cod_parcela, OUTPUT rRowidNota ).
               END CASE.

               FOR EACH ttApropRateio 
                   WHERE ttApropRateio.rowidNota = rRowidNota .
                   RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - ttApropRateio - Valor perc:' + string(ttApropRateio.perc) ).
                   CREATE tt.                                                                                                                                                   
                   ASSIGN 
                   tt.cod_modulo        = movto_cta_corren.cod_modul_dtsul
                   tt.cod_empresa       = IF AVAIL tit_ap THEN tit_ap.cod_empresa ELSE ''                                                                                                             
                   tt.cod_estab         = IF AVAIL tit_ap THEN tit_ap.cod_estab   ELSE ''                                                                                                             
                   tt.cod_emitente      = IF AVAIL tit_ap THEN string(tit_ap.cdn_fornecedor) ELSE '0'                                                                          
                   tt.desc_emitente     = IF AVAIL ems5.fornecedor THEN fornecedor.nom_pessoa  ELSE ''                                                                                  
                   tt.data              = movto_cta_corren.dat_transacao                                                                                                          
                   tt.conta_contabil    = ttApropRateio.conta                                                                                                           
                   tt.valor             = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN movto_tit_ap.val_movto_ap * ttApropRateio.perc 
                                          ELSE  movto_tit_ap.val_movto_ap * -1 * ttApropRateio.perc     
                   tt.conta_corrente    = movto_cta_corren.cod_cta_corren                                                                                                      
                   tt.base              = pBase                                                                                                                                 
                   tt.origem            = 'Recebimento'                                                                                                                     
                   tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren                                                                                           
                   tt.cc                = ttApropRateio.cc
                   tt.ccusto_gerencial  = tt.cc
                   tt.sequencia         = movto_cta_corren.num_seq_movto_cta_corren
                   tt.id_movto_corren   = movto_cta_corren.num_id_movto_cta_corren
                   tt.grupo_emitente    = IF AVAIL ems5.grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  ELSE ''
                   tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";")
                   tt.historico         = replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(10),";").
               END.
        END.
    END.
    */
END.
