/***********************************************************************************************************************************************
**
**  Programa: esapi/enviarEmailNF.p
**
**  Objetivo: Enviar e-mail da nota fiscal 
**
**  data: 08/2025
** autor:Tadeu silva
***********************************************************************************************************************************************/
DEFINE INPUT  PARAMETER pRowid           AS ROWID       NO-UNDO.
DEFINE INPUT  PARAMETER pEmailTST        AS CHARACTER   NO-UNDO.
/*DEFINE INPUT  PARAMETER pArqDanfe        AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pArqXML          AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pListaEmailCompl AS CHARACTER   NO-UNDO.*/


{esp/util.i}
{esp/params.i}
DEFINE VARIABLE cDestinatarios AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRemetente     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMensagem      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTituloEmail   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEmailLog      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cArqDanfe      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cArqXML        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBoParam       AS HANDLE      NO-UNDO.


ASSIGN  cTituloEmail = getTituloEmailNFVenda() .
ASSIGN  cEmailLog    = getEndEmailLogNfVenda().



FOR FIRST nota-fiscal FIELDS(cod-estabel serie nr-nota-fis cod-emitente)
    WHERE rowid(nota-fiscal) = pRowid NO-LOCK:    
    RUN esapi/getArqsNF.p(ROWID(nota-fiscal),OUTPUT cArqDanfe,OUTPUT cArqXML).                         
    FOR FIRST cont-emit FIELDS( cod-emitente e-mail)
        WHERE cont-emit.cod-emitente  = nota-fiscal.cod-emitente NO-LOCK:
        ASSIGN cDestinatarios = cont-emit.e-mail.         
    END.
    
    
    
    /*IF pListaEmailCompl <> '' THEN   DO:
       RUN incrValor(INPUT-OUTPUT cDestinatarios, pListaEmailCompl , ",") .
       
    END.     */
    ASSIGN cDestinatarios = cDestinatarios              + "," + cEmailLog.    
    RUN _setMensagem.     
    IF cDestinatarios <> '' THEN DO.
     /*  MESSAGE 'vou enviar o e-mail:' cDestinatarios
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
           
       RUN esapi/esapi002.p (/*c-remetente  */  INPUT cRemetente,                                 
                          /*c-destino    */     INPUT IF pEmailTst = '' THEN cDestinatarios ELSE pEmailTst,                       //  /*c-copia      */ INPUT REPLACE(REPLACE(REPLACE(h-ed-copia-email:SCREEN-VALUE,CHR(10),""),CHR(13),"")," ",""), 
                          /*c-assunto    */     INPUT cTituloEmail, 
                          /*cMensagem   */      INPUT cMensagem,
                          /*c-arq-anexo  */     INPUT cArqDanfe + "," + cArqXML,
                          /*l-mostra-erro*/     INPUT NO
                          ).
    END.
    ELSE  DO:
        OUTPUT TO value( SESSION:TEMP-DIRECTORY     + 'log-mail-nf-' 
                        + nota-fiscal.cod-estabel   + '-' 
                        + nota-fiscal.nr-nota-fis   + '-' 
                        + STRING(TIME)              + '.txt'
                        ).
            PUT 'nota fiscal:' nota-fiscal.nr-nota-fis 
            ' n∆o enviada, pois o destinat†rio estava em branco' 
            SKIP
            .
            
        OUTPUT CLOSE.
    END.
    
END.

PROCEDURE _setMensagem:
    DEFINE VARIABLE hBoMem AS HANDLE      NO-UNDO.
    RUN esbo/boMemorandos.p PERSIST SET hBoMem .
    RUN iniciar             IN hBoMem.                      
    RUN setProp             IN hBoMem('codEstab'        ,0, nota-fiscal.cod-estabel).
    RUN setProp             IN hBoMem('tipoMemorandoId' ,0, '1').
    RUN exec                IN hBoMem.
    RUN getTextoMemorando   IN hBoMem(OUTPUT cMensagem).
    RUN finalizar IN hBoMem.
    FOR FIRST emitente fields(cod-emitente nome-abrev nome-emit cgc ) NO-LOCK OF nota-fiscal:
    END.
    ASSIGN 
    cMensagem = REPLACE(cMensagem,'$$serie$$'        ,   nota-fiscal.serie)
    cMensagem = REPLACE(cMensagem,'$$nrnotafis$$'    ,   nota-fiscal.nr-nota-fis)
    cMensagem = REPLACE(cMensagem,'$$nomeEmit$$'     ,   emitente.nome-emit)
    cMensagem = REPLACE(cMensagem,'$$CNPJ$$'         ,   emitente.cgc)
    cMensagem = REPLACE(cMensagem,'$$ChaveAcesso$$'  ,   nota-fiscal.cod-chave-aces-nf-eletro)
    . 


END PROCEDURE.
/*

    
  */
