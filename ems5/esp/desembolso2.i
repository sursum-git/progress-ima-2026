
PROCEDURE piClassificarDados:
    DEFINE VARIABLE cRetorno            AS CHARACTER   NO-UNDO FORMAT 'x(100)'.
    DEFINE VARIABLE cGrupoRetorno       AS CHARACTER   NO-UNDO FORMAT 'x(100)'.
    DEFINE VARIABLE iCodParamDesemb     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE LOG_desconsiderar   AS LOGICAL     NO-UNDO.
    FOR EACH tt:
        RUN piBuscarClassCcusto('classificacao',
                                IF tt.cod_modulo = 'cmg' THEN YES ELSE NO,
                                ENTRY(1,tt.grupo,"-"),
                                tt.cod_emitente,
                                tt.conta_contabil,
                                tt.cc,
                                OUTPUT cRetorno,
                                OUTPUT cGrupoRetorno,
                                OUTPUT icodParamDesemb,
                                OUTPUT LOG_desconsiderar).
        
        
         ASSIGN tt.classificacao        = cRetorno
                tt.cod_param_desemb     = iCodParamDesemb
                tt.LOG_desconsiderar    = LOG_desconsiderar
                tt.grupo                = cGrupoRetorno.

         RUN piBuscarClassCcusto('ccusto',
                                IF tt.cod_modulo = 'cmg' THEN YES ELSE NO,
                                ENTRY(1,tt.grupo,"-"),
                                tt.cod_emitente,
                                tt.conta_contabil,
                                tt.cc,
                                OUTPUT cRetorno,
                                OUTPUT cGrupoRetorno,
                                OUTPUT icodParamDesemb,
                                OUTPUT LOG_desconsiderar).
         IF cRetorno <> '' THEN
            ASSIGN tt.ccusto_gerencial         = cRetorno
                   tt.cod_param_desemb_ccusto  = iCodParamDesemb. 
    END.
END.


PROCEDURE piBuscarClassCcusto:
    DEFINE INPUT  PARAMETER pTipo            AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCaixaBanco      AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pGrupoEmitente   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pEmitente        AS CHAR        NO-UNDO FORMAT 'x(8)'.
    DEFINE INPUT  PARAMETER pConta           AS CHARACTER   NO-UNDO FORMAT 'x(8)'.
    DEFINE INPUT  PARAMETER pCCusto          AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER cRetorno         AS CHAR FORMAT 'x(50)'.
    DEFINE OUTPUT PARAMETER cGrupoRetorno    AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
    DEFINE OUTPUT PARAMETER icodParamDesemb  AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER LOG_Desconsiderar AS LOGICAL     NO-UNDO.

    CASE pTipo:
        WHEN 'classificacao' THEN DO:
             IF pCaixaBanco = YES THEN DO: /* modulo caixa e bancos*/
                FIND FIRST PARAM_desembolso                                      
                 WHERE PARAM_desembolso.LOG_caixa_banco = pCaixaBanco         
                 AND pEmitente      >= param_desembolso.tipo_trans_cx_ini        
                 AND pEmitente      <= param_desembolso.tipo_trans_cx_fim 
                 AND   pConta       >= PARAM_desembolso.cod_cta_ctbl_ini         
                 AND   pConta       <= PARAM_desembolso.cod_cta_ctbl_fim         
                 AND   pCCusto      >= PARAM_desembolso.cod_cCusto_ini           
                 AND   pCCusto      <= PARAM_desembolso.cod_cCusto_fim 
                 AND   cod_tipo_param   = 2
                 USE-INDEX ind_prioridade
                 NO-LOCK NO-ERROR.                                            
                 IF AVAIL PARAM_desembolso THEN DO:   
                    FIND FIRST agrup_desemb
                        WHERE agrup_desemb.cod_agrup_desemb = PARAM_desembolso.cod_agrup_desemb
                        NO-LOCK NO-ERROR.
                     FIND FIRST grupos_desemb OF agrup_desemb
                             NO-LOCK NO-ERROR.
                    ASSIGN cRetorno = IF AVAIL agrup_desemb THEN agrup_desemb.desc_agrup_desemb ELSE string(PARAM_desembolso.cod_agrup_desemb).
                    ASSIGN icodParamDesemb = PARAM_desembolso.cod_param_desembolso.
                    ASSIGN LOG_desconsiderar = IF AVAIL agrup_desemb THEN agrup_desemb.LOG_desconsiderar ELSE NO.
                    ASSIGN cGrupoRetorno     = IF AVAIL grupos_desemb THEN DESC_grupo_desemb ELSE ''.
                 END.
                 ELSE DO:
                    FIND FIRST PARAM_desembolso                                                                                                              
                     WHERE PARAM_desembolso.LOG_caixa_banco = pCaixaBanco
                     AND   param_desembolso.tipo_trans_cx_ini = ''
                     AND   pConta       >= PARAM_desembolso.cod_cta_ctbl_ini                                                                                 
                     AND   pConta       <= PARAM_desembolso.cod_cta_ctbl_fim                                                                                 
                     AND   pCCusto      >= PARAM_desembolso.cod_cCusto_ini                                                                                   
                     AND   pCCusto      <= PARAM_desembolso.cod_cCusto_fim                                                                                   
                     AND   cod_tipo_param   = 2                                                                                                              
                     USE-INDEX ind_prioridade                                                                                                                
                     NO-LOCK NO-ERROR.                                                                                                                       
                     IF AVAIL PARAM_desembolso THEN DO:  
                        FIND FIRST agrup_desemb                                                                                                              
                            WHERE agrup_desemb.cod_agrup_desemb = PARAM_desembolso.cod_agrup_desemb                                                          
                            NO-LOCK NO-ERROR.   
                        FIND FIRST grupos_desemb OF agrup_desemb
                             NO-LOCK NO-ERROR.
                        ASSIGN cRetorno = IF AVAIL agrup_desemb THEN agrup_desemb.desc_agrup_desemb ELSE string(PARAM_desembolso.cod_agrup_desemb).
                        ASSIGN icodParamDesemb   = PARAM_desembolso.cod_param_desembolso.
                        ASSIGN LOG_desconsiderar = IF AVAIL agrup_desemb THEN agrup_desemb.LOG_desconsiderar ELSE NO.
                        ASSIGN cGrupoRetorno     = IF AVAIL grupos_desemb THEN DESC_grupo_desemb ELSE ''.
                     END.                                                                                                                                    
                     ELSE DO:
                        ASSIGN cRetorno = "Classifica‡Æo NÆo Encontrada".                                                                                    
                     END.
                 END.
             END.
             ELSE DO: /* modulo <>  de caixa e bancos */
                 /*busca primeiramente por emitente especifico*/
                 FIND FIRST PARAM_desembolso                                      
                 WHERE PARAM_desembolso.LOG_caixa_banco = pCaixaBanco         
                 AND pEmitente      = string(param_desembolso.cod_emitente)
                 AND   pConta       >= PARAM_desembolso.cod_cta_ctbl_ini         
                 AND   pConta       <= PARAM_desembolso.cod_cta_ctbl_fim         
                 AND   pCCusto      >= PARAM_desembolso.cod_cCusto_ini           
                 AND   pCCusto      <= PARAM_desembolso.cod_cCusto_fim 
                 AND   cod_tipo_param   = 2
                 USE-INDEX ind_prioridade
                 NO-LOCK NO-ERROR.                                            
                 IF AVAIL PARAM_desembolso THEN DO:
                    FIND FIRST agrup_desemb
                        WHERE agrup_desemb.cod_agrup_desemb = PARAM_desembolso.cod_agrup_desemb
                        NO-LOCK NO-ERROR.
                    FIND FIRST grupos_desemb OF agrup_desemb
                         NO-LOCK NO-ERROR.
                    ASSIGN cRetorno = IF AVAIL agrup_desemb THEN agrup_desemb.desc_agrup_desemb ELSE string(PARAM_desembolso.cod_agrup_desemb).
                    ASSIGN icodParamDesemb = PARAM_desembolso.cod_param_desembolso.
                    ASSIGN LOG_desconsiderar = IF AVAIL agrup_desemb THEN agrup_desemb.LOG_desconsiderar ELSE NO.
                    ASSIGN cGrupoRetorno     = IF AVAIL grupos_desemb THEN DESC_grupo_desemb ELSE ''.
                 END.
                 ELSE DO: /*busca por grupo de fornecedor especifico*/
                    FIND FIRST PARAM_desembolso                                                                                                    
                    WHERE PARAM_desembolso.LOG_caixa_banco = pCaixaBanco  
                    AND PARAM_desembolso.cod_emitente = 0
                    AND pGrupoEmitente      = param_desembolso.cod_grupo_emitente
                    AND   pConta            >= PARAM_desembolso.cod_cta_ctbl_ini                                                                        
                    AND   pConta            <= PARAM_desembolso.cod_cta_ctbl_fim                                                                        
                    AND   pCCusto           >= PARAM_desembolso.cod_cCusto_ini                                                                          
                    AND   pCCusto           <= PARAM_desembolso.cod_cCusto_fim                                                                          
                    AND   cod_tipo_param    = 2                                                                                                     
                    USE-INDEX ind_prioridade                                                                                                       
                    NO-LOCK NO-ERROR.                                                                                                              
                    IF AVAIL PARAM_desembolso THEN DO:  
                       FIND FIRST agrup_desemb                                                                                                     
                           WHERE agrup_desemb.cod_agrup_desemb = PARAM_desembolso.cod_agrup_desemb                                                 
                           NO-LOCK NO-ERROR.     
                        FIND FIRST grupos_desemb OF agrup_desemb
                         NO-LOCK NO-ERROR.
                       ASSIGN cRetorno = IF AVAIL agrup_desemb THEN agrup_desemb.desc_agrup_desemb ELSE string(PARAM_desembolso.cod_agrup_desemb). 
                       ASSIGN icodParamDesemb = PARAM_desembolso.cod_param_desembolso.
                       ASSIGN LOG_desconsiderar = IF AVAIL agrup_desemb THEN agrup_desemb.LOG_desconsiderar ELSE NO.
                       ASSIGN cGrupoRetorno     = IF AVAIL grupos_desemb THEN DESC_grupo_desemb ELSE ''.
                    END.                                                                                                                           
                    ELSE DO: /*busca por faixa de conta e centro de custo*/ 
                       FIND FIRST PARAM_desembolso                                                                                                    
                       WHERE PARAM_desembolso.LOG_caixa_banco = pCaixaBanco 
                       AND PARAM_desembolso.cod_emitente = 0
                       AND PARAM_desembolso.cod_grupo_emitente = '' 
                       AND   pConta       >= PARAM_desembolso.cod_cta_ctbl_ini                                                                        
                       AND   pConta       <= PARAM_desembolso.cod_cta_ctbl_fim                                                                        
                       AND   pCCusto      >= PARAM_desembolso.cod_cCusto_ini                                                                          
                       AND   pCCusto      <= PARAM_desembolso.cod_cCusto_fim                                                                          
                       AND   cod_tipo_param   = 2                                                                                                     
                       USE-INDEX ind_prioridade                                                                                                       
                       NO-LOCK NO-ERROR.                                                                                                              
                       IF AVAIL PARAM_desembolso THEN DO:
                          FIND FIRST agrup_desemb                                                                                                     
                              WHERE agrup_desemb.cod_agrup_desemb = PARAM_desembolso.cod_agrup_desemb                                                 
                              NO-LOCK NO-ERROR.  
                          FIND FIRST grupos_desemb OF agrup_desemb
                              NO-LOCK NO-ERROR.
                          ASSIGN cRetorno = IF AVAIL agrup_desemb THEN agrup_desemb.desc_agrup_desemb ELSE string(PARAM_desembolso.cod_agrup_desemb). 
                          ASSIGN icodParamDesemb = PARAM_desembolso.cod_param_desembolso.
                          ASSIGN LOG_desconsiderar = IF AVAIL agrup_desemb THEN agrup_desemb.LOG_desconsiderar ELSE NO.
                          ASSIGN cGrupoRetorno     = IF AVAIL grupos_desemb THEN DESC_grupo_desemb ELSE ''.

                       END.                                                                                                                           
                       ELSE DO:                                                                         
                          ASSIGN cRetorno = "Classifica‡Æo NÆo Encontrada".
                       END.                                                                                                                                                                                                                                                       
                    END.
                 END.
             END.
        END.
        WHEN 'cCusto' THEN DO:
            IF pCaixaBanco THEN DO: /* modulo caixa e bancos*/
                FIND FIRST PARAM_desembolso                                      
                 WHERE PARAM_desembolso.LOG_caixa_banco = pCaixaBanco         
                 AND pEmitente      >= param_desembolso.tipo_trans_cx_ini        
                 AND pEmitente      <= param_desembolso.tipo_trans_cx_fim 
                 AND   pConta       >= PARAM_desembolso.cod_cta_ctbl_ini         
                 AND   pConta       <= PARAM_desembolso.cod_cta_ctbl_fim         
                 AND   pCCusto      >= PARAM_desembolso.cod_cCusto_ini           
                 AND   pCCusto      <= PARAM_desembolso.cod_cCusto_fim 
                 AND   cod_tipo_param   = 1
                 USE-INDEX ind_prioridade
                 NO-LOCK NO-ERROR.                                            
                 IF AVAIL PARAM_desembolso THEN DO:   
                    FIND FIRST ems5.ccusto
                        WHERE ccusto.cod_ccusto = string(PARAM_desembolso.cod_ccusto_gerencial)
                        NO-LOCK NO-ERROR.
                    ASSIGN cRetorno = IF AVAIL ccusto THEN ccusto.des_tit_ctbl ELSE string(PARAM_desembolso.cod_ccusto_gerencial).
                    ASSIGN icodParamDesemb = PARAM_desembolso.cod_param_desembolso.
                 END.
                 ELSE DO:
                    ASSIGN cRetorno = "".
                 END.
            END.
            ELSE DO:
                FIND FIRST PARAM_desembolso                                                      
                     WHERE PARAM_desembolso.LOG_caixa_banco = pCaixaBanco                        
                     AND pEmitente            = string(param_desembolso.cod_emitente)              
                     AND   pConta            >= PARAM_desembolso.cod_cta_ctbl_ini                
                     AND   pConta            <= PARAM_desembolso.cod_cta_ctbl_fim                
                     AND   pCCusto           >= PARAM_desembolso.cod_cCusto_ini                  
                     AND   pCCusto           <= PARAM_desembolso.cod_cCusto_fim                  
                     AND   cod_tipo_param    = 1                                                 
                     USE-INDEX ind_prioridade                                                    
                     NO-LOCK NO-ERROR.                                                           
                IF AVAIL PARAM_desembolso THEN DO:                                                                   
                   FIND FIRST ems5.ccusto
                        WHERE ccusto.cod_ccusto = string(PARAM_desembolso.cod_ccusto_gerencial)
                        NO-LOCK NO-ERROR.
                    ASSIGN cRetorno = IF AVAIL ccusto THEN ccusto.cod_ccusto + '-' + ccusto.des_tit_ctbl ELSE string(PARAM_desembolso.cod_ccusto_gerencial).
                    ASSIGN icodParamDesemb = PARAM_desembolso.cod_param_desembolso.
                END.
                ELSE DO:
                    ASSIGN cRetorno = "".
                END.
            END.
        END.
    END CASE.
END PROCEDURE.
