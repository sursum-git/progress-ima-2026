/*****************************************************************************
Programa: esbofin271.p
Objetivo: Retornar os valores de titulos a pagar em aberto
na data corrente, assim como informa‡äes de contas cont beis e tipos de fluxo.
*****************************************************************************/

/*defini‡äes*/
{esbo/esbofin271a.i}

DEFINE VARIABLE empresaIni  AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE empresaFim  AS CHARACTER   NO-UNDO INIT 'zzz'.
DEFINE VARIABLE estabIni    AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE estabFim    AS CHARACTER   NO-UNDO INIT 'zzz'.
DEFINE VARIABLE dataLimite  AS DATE        NO-UNDO.
DEFINE VARIABLE dataInicio  AS DATE        NO-UNDO.

PROCEDURE definirEmpresa:
    DEFINE INPUT PARAMETER pEmpresa AS CHARACTER   NO-UNDO.
    ASSIGN empresaIni =  pEmpresa
           empresaFim =  pEmpresa.
END.

PROCEDURE definirEstab:
    DEFINE INPUT PARAMETER pEstabIni AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pEstabFim     AS CHARACTER   NO-UNDO.
    ASSIGN estabIni =  pEstabIni
           estabFim =  pEstabFim.
END.

PROCEDURE definirDataLimite:
   DEFINE INPUT  PARAMETER pDataLimite AS DATE        NO-UNDO.
   ASSIGN dataLimite = pDataLimite.
END.

PROCEDURE definirDataInicio:
   DEFINE INPUT  PARAMETER pDataInicio AS DATE        NO-UNDO.
   ASSIGN dataInicio = pDataInicio.
END.

PROCEDURE limparDados:
EMPTY TEMP-TABLE ttTituloAP.

END.
PROCEDURE buscarTitulos:
    DEFINE VARIABLE lVencido AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE iSinal   AS INTEGER     NO-UNDO.
    FOR EACH tit_ap NO-LOCK
        WHERE tit_ap.cod_empresa    >= empresaIni
        AND tit_ap.cod_empresa      <= empresaFim
        AND tit_ap.cod_estab        >= estabIni
        AND tit_ap.cod_estab        <= estabFim
        AND tit_ap.log_sdo_tit_ap  = YES
        AND tit_ap.dat_vencto_tit_ap >= dataInicio
        AND tit_ap.dat_vencto_tit_ap <= dataLimite ,
        EACH espec_docto_financ OF tit_ap NO-LOCK
        WHERE espec_docto_financ.LOG_atualiz_fluxo_cx = YES.
        FOR EACH val_tit_ap OF tit_ap NO-LOCK:
            /**regra para desconsiderar titulos da ima para med ***/                                                 
            IF tit_ap.cod_empresa = '500' AND tit_ap.cdn_fornecedor = 1  THEN NEXT.                                   

            /**regra para desconsiderar titulos da med para ima ***/                                                 
            IF tit_ap.cod_empresa = '100' AND tit_ap.cdn_fornecedor = 10535 THEN NEXT.                                

            ASSIGN lVencido = tit_ap.dat_vencto_tit_ap < TODAY.                                                    
            FIND FIRST ttTituloAP                                                                                      
                WHERE ttTituloAP.cod_empresa      = tit_ap.cod_empresa                                                
                AND   ttTituloAP.cod_estab        = tit_ap.cod_estab                                                  
                AND   ttTituloAP.cod_tit_ap      = tit_ap.cod_tit_ap                                                
                AND   ttTituloAP.cod_parcela      = tit_ap.cod_parcela                                                
                AND   ttTituloAP.cod_ser_docto    = tit_ap.cod_ser_docto                                              
                AND   ttTituloAP.cod_espec_docto  = tit_ap.cod_espec_docto                                            
                AND   ttTituloAP.cdn_fornecedor   = tit_ap.cdn_fornecedor                                                
                NO-LOCK NO-ERROR.                                                                                    
            IF NOT AVAIL ttTituloAP THEN DO:                                                                           
               FIND FIRST ems5.espec_docto OF espec_docto_financ NO-LOCK NO-ERROR.                                   
               IF AVAIL espec_docto  THEN DO:                                                                        
                    IF espec_docto.ind_tip_espec_docto = 'Antecipa‡Æo' THEN DO:                                      
                        ASSIGN iSinal = 1.                                                                          
                    END.                                                                                             
                    ELSE DO:                                                                                         
                        ASSIGN iSinal = -1.                                                                           
                    END.                                                                                             
               END.                                                                                                  
               ELSE DO:                                                                                              
                ASSIGN iSinal = -1.                                                                                   
               END.                                   
               FIND FIRST emitente 
                   WHERE emitente.cod-emitente = tit_ap.cdn_fornecedor NO-LOCK NO-ERROR.

               CREATE ttTituloAP.                                                                                      
               ASSIGN ttTituloAP.cod_empresa          = tit_ap.cod_empresa                                            
                      ttTituloAP.cod_estab            = tit_ap.cod_estab                                              
                      ttTituloAP.cod_tit_ap           = tit_ap.cod_tit_ap                                            
                      ttTituloAP.cod_parcela          = tit_ap.cod_parcela                                            
                      ttTituloAP.cod_ser_docto        = tit_ap.cod_ser_docto                                          
                      ttTituloAP.cod_espec_docto      = tit_ap.cod_espec_docto                                        
                      ttTituloAP.cdn_fornecedor       = tit_ap.cdn_fornecedor                                            
                      ttTituloAP.nom_abrev            = IF AVAIL emitente THEN emitente.nome-abrev ELSE ''
                      ttTituloAP.dat_emis_docto       = tit_ap.dat_emis_docto                                         
                      ttTituloAP.dat_vencto_tit_ap    = tit_ap.dat_vencto_tit_ap                                     
                      ttTituloAP.val_origin_tit_ap    = val_tit_ap.val_origin_tit_ap  * iSinal                           
                      ttTituloAP.val_sdo_tit_ap       = val_tit_ap.val_sdo_tit_ap     * iSinal                           
                      ttTituloAP.situacao             = lVencido
                      ttTituloAP.tipo_fluxo           = val_tit_ap.cod_tip_fluxo_financ .                                                      
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE retornarRegistros:
    DEFINE OUTPUT PARAM TABLE FOR ttTituloAP.
END.



 






