/****************************************************************************
** Programa: upc-boin090.p - EPC de BO da tabela docum-est
** Objetivo: Voltar a situaá∆o das etiquetas e eliminar romaneios
**           das Notas Fiscais devolvidas
*****************************************************************************/
{include/i-epc200.i} /*Definicao tt-EPC*/

DEFINE TEMP-TABLE tt-docum-est LIKE docum-est
    FIELD r-Rowid AS ROWID.

DEF INPUT        PARAM p-ind-event AS CHAR   NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEFINE VARIABLE h-bo            AS HANDLE NO-UNDO.
DEFINE VARIABLE l-ok            AS LOGICAL NO-UNDO.
DEFINE VARIABLE l-exclui        AS LOGICAL    NO-UNDO.

/*
OUTPUT TO c:\temp\ponto-bo.txt APPEND.
PUT "boin090 " p-ind-event FORMAT "X(30)" SKIP.
FOR EACH tt-epc:
    DISPLAY tt-epc
            WITH STREAM-IO DOWN WIDTH 200.
END.
PUT "--------------" SKIP(1).
OUTPUT CLOSE.
*/

/*
IF  p-ind-event = "beforecreateRecord" OR
    p-ind-event = "beforeupdateRecord" OR
    p-ind-event = "beforedeleteRecord" OR
    p-ind-event = "afterupdateRecord" THEN DO:

    FIND FIRST tt-epc WHERE 
               tt-epc.cod-event     = p-ind-event AND
               tt-epc.cod-parameter = "Object-Handle" NO-LOCK NO-ERROR.
    IF AVAILABLE tt-epc THEN DO:
        
        ASSIGN h-bo = WIDGET-HANDLE(tt-epc.val-parameter).
        
        RUN getRecord IN h-bo (OUTPUT TABLE tt-docum-est).
        
        FIND FIRST tt-docum-est NO-LOCK NO-ERROR.



        CASE p-ind-event:
            WHEN "beforecreateRecord" THEN DO:
                
                FIND emitente WHERE
                     emitente.cod-emitente = tt-docum-est.cod-emitente NO-LOCK.
        
                /* Haver† digitaá∆o de notas apenas para clientes do grupo Transportadores
                   -----------------------------------------------------------------------*/
                IF emitente.cod-gr-forn = 84 THEN  DO:
        
                    l-ok = NO.
    
                    SESSION:SET-WAIT-STATE("":U).
    
                    RUN esepc/li-epc-boin090.w (INPUT  TABLE tt-docum-est,
                                                OUTPUT l-ok).

                    /*
                    IF NOT l-ok THEN DO:
                        CREATE tt-epc.
                        ASSIGN tt-epc.cod-event     = "ERROR"
                               tt-epc.cod-parameter = "EPC-ERROR" 
                               tt-epc.val-parameter = "Notas fiscais n∆o informadas.".
                    END.
                    */
                END.
            END.

            WHEN "beforeupdateRecord" THEN DO:
                FIND emitente WHERE
                     emitente.cod-emitente = tt-docum-est.cod-emitente NO-LOCK.
        
                /* Haver† digitaá∆o de notas apenas para clientes do grupo Transportadores
                   -----------------------------------------------------------------------*/
                IF emitente.cod-gr-forn = 84 THEN  DO:

                    ASSIGN de-valor-mercad = 0
                           de-tot-peso     = 0.

                    FOR EACH ps-nf-docum-est WHERE                                   
                             ps-nf-docum-est.serie-docto  = tt-docum-est.serie-docto AND         
                             ps-nf-docum-est.nro-docto    = tt-docum-est.nro-docto AND  
                             ps-nf-docum-est.cod-emitente = tt-docum-est.cod-emitente AND
                             ps-nf-docum-est.nat-operacao = tt-docum-est.nat-operacao
                             NO-LOCK:

                        FIND nota-fiscal WHERE 
                             nota-fiscal.cod-estabel = ps-nf-docum-est.cod-estabel AND         
                             nota-fiscal.serie       = ps-nf-docum-est.serie-nf AND  
                             nota-fiscal.nr-nota-fis = ps-nf-docum-est.nr-nota-fis 
                             NO-LOCK.

                        FIND nota-trans WHERE  
                             nota-trans.cod-estabel = ps-nf-docum-est.cod-estabel AND 
                             nota-trans.serie       = ps-nf-docum-est.serie-nf AND 
                             nota-trans.nr-nota-fis = ps-nf-docum-est.nr-nota-fis   
                             NO-LOCK.


                        ASSIGN de-valor-mercad = de-valor-mercad + nota-trans.vl-servico
                               de-tot-peso     = de-tot-peso     + nota-fiscal.peso-bru-tot.
                    END.

                    IF (de-valor-mercad <> 0 AND de-valor-mercad <> tt-docum-est.valor-mercad) OR
                       (de-tot-peso     <> 0 AND de-tot-peso     <> tt-docum-est.tot-peso) THEN DO:

                       MESSAGE "Frete ou Peso das NF's de saida difere do documento"
                               VIEW-AS ALERT-BOX.
                        /*
                        RUN utp/ut-msg-liasa.w (INPUT 3,
                                                INPUT "Valores N∆o Conferem. Confirma?",
                                                INPUT "Valor Servicos ou Peso das NF's de saida diferem do documento. Confirma?").
                        */
                        

                                                               /*
                        IF RETURN-VALUE = "no" THEN DO:
                            CREATE tt-epc.
                            ASSIGN tt-epc.cod-event     = "ERROR"
                                   tt-epc.cod-parameter = "EPC-ERROR" 
                                   tt-epc.val-parameter = "Valor Servicos ou Peso das NF's de saida diferem do documento.".

                        END.                                     */
                    END.
                END.
            END.

            WHEN "beforedeleteRecord" THEN DO:
                
                SESSION:SET-WAIT-STATE("general":U).

                /* Elimina relacionamento entre NF e docum-est
                   -------------------------------------------*/
                FOR EACH ps-nf-docum-est WHERE                                   
                         ps-nf-docum-est.serie-docto  = tt-docum-est.serie-docto AND         
                         ps-nf-docum-est.nro-docto    = tt-docum-est.nro-docto AND  
                         ps-nf-docum-est.cod-emitente = tt-docum-est.cod-emitente AND
                         ps-nf-docum-est.nat-operacao = tt-docum-est.nat-operacao:
                    DELETE ps-nf-docum-est.
                END.

                IF tt-docum-est.nat-operacao = "121qz" THEN DO: 

                    FIND ps-entr-mat WHERE
                         ps-entr-mat.nro-docto    = tt-docum-est.nro-docto    AND
                         ps-entr-mat.serie-docto  = tt-docum-est.serie-docto  AND
                         ps-entr-mat.cod-emitente = tt-docum-est.cod-emitente NO-ERROR.

                    IF AVAILABLE ps-entr-mat then do.

                        l-exclui = YES.
                        FIND FIRST ps-it-sol-anl WHERE
                                   ps-it-sol-anl.nr-solic = ps-entr-mat.nr-sol-anl NO-ERROR.

                        IF AVAILABLE ps-it-sol-anl THEN DO:
                      
                            RUN utp/ut-msg-liasa.w (INPUT 3,
                                                    INPUT "Confirma Exclus∆o?",
                                                    INPUT "Material ja analisado. Ao ser exclu°do o mesmo devera ser re-analisado.").
                            IF RETURN-VALUE = "no" THEN
                                l-exclui = NO.
                        END.

                        IF l-exclui THEN DO:
                      
                            FIND FIRST item-doc-est WHERE
                                       item-doc-est.serie-docto  = tt-docum-est.serie-docto   AND
                                       item-doc-est.nro-docto    = tt-docum-est.nro-docto     AND
                                       item-doc-est.cod-emitente = tt-docum-est.cod-emitente 
                                       NO-LOCK NO-ERROR.

                            FIND item WHERE 
                                 item.it-codigo = item-doc-est.it-codigo
                                 NO-LOCK NO-ERROR.

                            IF item.fm-codigo = "34" THEN DO:
                                FIND ps-it-ct-loc WHERE
                                     ps-it-ct-loc.nr-cdr = ps-entr-mat.nr-cdr NO-ERROR.

                                FIND ps-ct-localiz WHERE
                                     ps-ct-localiz.lote = ps-it-ct-loc.lote AND
                                     ps-ct-localiz.localizacao = ps-it-ct-loc.localizacao NO-ERROR.
                                ASSIGN ps-ct-localiz.nr-entradas = ps-ct-localiz.nr-entradas - 1.
                                DELETE ps-it-ct-loc.
                            END.


                            FOR EACH ps-it-sol-anl WHERE
                                     ps-it-sol-anl.nr-solic = ps-entr-mat.nr-sol-anl.
                                DELETE ps-it-sol-anl.
                            END.

                            FIND ps-solic-anl WHERE
                                 ps-solic-anl.nr-solic = ps-entr-mat.nr-sol-anl NO-ERROR.
    
                            IF AVAIL ps-solic-anl THEN 
                                DELETE ps-solic-anl.
    
                            DELETE ps-entr-mat VALIDATE(TRUE,"").
                        END.      
                    END. 
                END.

                SESSION:SET-WAIT-STATE("":U).
            END.

            WHEN "afterupdateRecord" THEN DO:

                IF tt-docum-est.nat-operacao = "111ii"  OR 
                   tt-docum-est.nat-operacao = "111cv"  OR 
                   tt-docum-est.nat-operacao = "111ln"  OR 
                   tt-docum-est.nat-operacao = "121ln"  OR 
                   tt-docum-est.nat-operacao = "111di"  THEN DO:

                    FIND pl-florestal WHERE 
                         pl-florestal.dt-emissao = tt-docum-est.dt-emissao AND
                         pl-florestal.nro-docto  = tt-docum-est.nro-docto NO-ERROR.
                    IF NOT AVAILABLE pl-florestal THEN DO:
                        FIND dm-ent-mat WHERE
                             dm-ent-mat.serie-docto   = tt-docum-est.serie-docto AND
                             dm-ent-mat.cod-emitente  = tt-docum-est.cod-emitente AND
                             dm-ent-mat.nro-docto     = tt-docum-est.nro-docto
                             NO-LOCK NO-ERROR.

                        FIND dm-ent-cam WHERE
                             dm-ent-cam.placa      = dm-ent-mat.placa      AND
                             dm-ent-cam.dt-entrada = dm-ent-mat.dt-entrada AND
                             dm-ent-cam.hr-entrada = dm-ent-mat.hr-entrada
                             NO-LOCK NO-ERROR.

                        IF AVAILABLE dm-ent-mat THEN DO:
                            FIND FIRST dm-projetos WHERE
                                       dm-projetos.projeto = dm-ent-mat.projeto 
                                       NO-LOCK NO-ERROR.

                            CREATE pl-florestal.
                            ASSIGN pl-florestal.dt-emissao  = dm-ent-cam.dt-emis-nf
                                   pl-florestal.nro-docto   = dm-ent-mat.nro-docto
                                   pl-florestal.serie-docto = dm-ent-mat.serie-docto
                                   pl-florestal.qt-do-forn  = dm-ent-mat.qtidade-nf
                                   pl-florestal.num-gca     = dm-ent-mat.gcc
                                   pl-florestal.processo    = IF AVAIL dm-projetos THEN
                                                             dm-projetos.processo ELSE "".
                        END.                                     
                    END.
                END.


                /*
                /* Digitaá∆o da quantidade real
                   ----------------------------*/
                IF  tt-docum-est.nat-operacao = "199qzr"  OR 
                    SUBSTRING(tt-docum-est.nat-operacao,4,3) = "qz" OR
                    SUBSTRING(tt-docum-est.nat-operacao,4,3) = "cv" OR 
                    SUBSTRING(tt-docum-est.nat-operacao,4,3) = "ln" THEN DO:

                    FIND FIRST item-doc-est WHERE
                               item-doc-est.serie-docto  = tt-docum-est.serie-docto   AND
                               item-doc-est.nro-docto    = tt-docum-est.nro-docto     AND
                               item-doc-est.cod-emitente = tt-docum-est.cod-emitente 
                               NO-LOCK NO-ERROR.
                    IF AVAILABLE item-doc-est THEN DO:

                       /* cdr */
                        FIND item WHERE 
                             item.it-codigo = item-doc-est.it-codigo
                             NO-LOCK NO-ERROR.
    
                        IF item.fm-codigo = "34" THEN DO: /* QUARTZO */
                       
                            FIND dm-ent-mat WHERE
                                 dm-ent-mat.nro-docto    = tt-docum-est.nro-docto and
                                 dm-ent-mat.serie-docto  = tt-docum-est.serie-docto and
                                 dm-ent-mat.cod-emitente = tt-docum-est.cod-emitente
                                 NO-LOCK NO-ERROR.

                            FIND dm-ent-cam WHERE
                                 dm-ent-cam.placa      = dm-ent-mat.placa      AND
                                 dm-ent-cam.dt-entrada = dm-ent-mat.dt-entrada AND
                                 dm-ent-cam.hr-entrada = dm-ent-mat.hr-entrada 
                                 NO-LOCK NO-ERROR.

                            IF AVAILABLE dm-ent-cam THEN
                                ASSIGN de-qtd-real = 
                                       (dm-ent-cam.peso-bruto - dm-ent-cam.tara) / 1000.

                            SESSION:SET-WAIT-STATE("":U).
          
                            RUN esepc/li-epc-boin176.w (INPUT-OUTPUT de-qtd-real).
           
                            FIND ps-docum-est-ext WHERE
                                 ps-docum-est-ext.serie-docto  = tt-docum-est.serie-docto AND
                                 ps-docum-est-ext.nro-docto    = tt-docum-est.nro-docto AND  
                                 ps-docum-est-ext.cod-emitente = tt-docum-est.cod-emitente AND
                                 ps-docum-est-ext.nat-operacao = tt-docum-est.nat-operacao
                                 NO-ERROR.
                            IF NOT AVAILABLE ps-docum-est-ext THEN DO:
                                CREATE ps-docum-est-ext.
                                ASSIGN ps-docum-est-ext.serie-docto  = tt-docum-est.serie-docto
                                       ps-docum-est-ext.nro-docto    = tt-docum-est.nro-docto
                                       ps-docum-est-ext.cod-emitente = tt-docum-est.cod-emitente 
                                       ps-docum-est-ext.nat-operacao = tt-docum-est.nat-operacao. 
                            END.
                            ASSIGN ps-docum-est-ext.qtd-real = de-qtd-real.
                        END.
                    END.
                    ELSE DO:
                        /* Se n∆o foi conclu°da a digitaá∆o dos itens do documento, zera a quantidade real
                           da nota
                           -------------------------------------------------------------------------------*/
                        FIND ps-docum-est-ext WHERE
                             ps-docum-est-ext.serie-docto  = tt-docum-est.serie-docto AND
                             ps-docum-est-ext.nro-docto    = tt-docum-est.nro-docto AND  
                             ps-docum-est-ext.cod-emitente = tt-docum-est.cod-emitente AND
                             ps-docum-est-ext.nat-operacao = tt-docum-est.nat-operacao
                             NO-ERROR.
                        IF AVAILABLE ps-docum-est-ext THEN 
                            ASSIGN ps-docum-est-ext.qtd-real = de-qtd-real.
                    END.
                END.
                */
            END.
        END CASE.
    END.
END.
  */
