/**************************************************************************
Programa: esapi/cortarEtqLisa.p
Autor: Tadeu silva
Objetivo: Utiliza a boes049 para simular corte de peáa, ajustando a 
quantidade da peáa cortada e criando a nova peáa originada do corte.
Data: 09/2025
*****************************************************************************/
USING classes.ENUMs.TipoHistCorteSeparacao FROM PROPATH.
DEFINE INPUT  PARAMETER pIdTransacao        AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pNrPedido           AS INTEGER     NO-UNDO.
//DEFINE INPUT  PARAMETER pNumEtqOrigem       AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pCodEtqLisaOrigem   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodEtqLisaCorte    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pQtAtuEtqOri        AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER pQtCorte            AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER pNumRolo            AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pPrePedido          AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER hBoMsg              AS HANDLE      NO-UNDO.
/*DEFINE INPUT  PARAMETER pItem               AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pRefer              AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pContainer          AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pNumRoloOrigem      AS INTEGER     NO-UNDO.*/
DEFINE OUTPUT PARAMETER cErro               AS CHARACTER   NO-UNDO.

DEFINE VARIABLE qtEtqOriAntesAjuste         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE numEtqMedOrigem             AS INTEGER     NO-UNDO. 
DEFINE VARIABLE  numEtqNova                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE hBoEs049                    AS HANDLE      NO-UNDO.
DEFINE VARIABLE hMsgEs049                   AS HANDLE      NO-UNDO.
DEFINE VARIABLE cMsg                        AS CHARACTER   NO-UNDO.
DEFINE TEMP-TABLE ttRegAux  LIKE ob-etiqueta .
{esbo/bomsg.i} 

DEFINE TEMP-TABLE ttBoEs049 LIKE ttMsg.


/*1-posiciona na etiqueta origem e altera a mesma na quantidade cortada
  regra: so iguala a quantidade da etiqueta origem, se a quantidade vinda
  da Lisa for menor que da etiqueta atual na MED.
*/



RUN esapi/getNumEtqMed.p(pCodEtqLisaOrigem, OUTPUT numEtqMedOrigem).
/*IF numEtqMedOrigem = 0 THEN DO:
   RUN esapi/getNumEtqPorRolo.p(pItem,pRefer,pContainer,pNumRoloOrigem, OUTPUT numEtqMedOrigem).   
END.
IF numEtqMedOrigem = 0 THEN
DO:
    ASSIGN cMsg =   'Etiqueta LISA que foi cortada:'  +  pCodEtqLisaOrigem +
                          ' - NAO Encontrada com a chave item/ref/container/rolo:' + pItem  + "/"  + pRefer  + "/" + string(pContainer)  + "/" + string(pNumRolo).
    RUN setMsg IN hBoMsg(101, cMsg,'erro').        
END.*/
IF numEtqMedorigem = 0  THEN DO:
    ASSIGN cMsg = cMsg + CHR(13) +   'Etiqueta LISA que foi cortada:'  +  pCodEtqLisaOrigem +
                          ' - NAO Encontrada a Etiqueta MED. '.
    ASSIGN cErro = cMsg.                      
    RETURN ERROR.
END.

ELSE DO:
    ASSIGN cMsg =   'Etiqueta LISA que foi cortada:'         +  pCodEtqLisaOrigem +
                          ' - Encontrada a Etiqueta MED:'    +  string(numEtqMedOrigem) .
    RUN setMsg IN hBoMsg(102, cMsg,'log').   
END.

FOR FIRST ob-etiqueta 
//FIELDS(cod-estabel num-etiqueta quantidade it-codigo cod-refer nr-container localizacao)
WHERE ob-etiqueta.cod-estabel  = '505'
AND   ob-etiqueta.num-etiqueta = numEtqMedOrigem NO-LOCK .
END.      
IF AVAIL ob-etiqueta THEN DO:
   /*MESSAGE 'etq.qt.atual na med:'  ob-etiqueta.quantidade SKIP
           'etq.qt.atual na lisa:' pQtAtuEtqOri SKIP 
           'etq:' ob-etiqueta.num-etiqueta SKIP
           'item:' ob-etiqueta.it-codigo SKIP
           'refer:' ob-etiqueta.cod-refer SKIP
           'cod.estabel:' ob-etiqueta.cod-estabel   SKIP
           'rolo:' pNumrolo
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */
   IF ob-etiqueta.quantidade >= pQtAtuEtqOri THEN DO ON ERROR UNDO: 
      RUN setMsg IN hBoMsg(103, 'Quantidade da etiqueta no banco de dados: ' + STRING(ob-etiqueta.quantidade) +
                                ' MAIOR OU IGUAL que a quantidade j† cortada da etiqueta constante na LISA:' + string(pQtAtuEtqOri) + 
                                ' - O sistema tentar† ajustar a etiqueta original e/ou criar a nova etiqueta que veio do corte ',
                                'log').  
       
      IF ob-etiqueta.quantidade > pQtAtuEtqOri THEN DO: //apenas se n∆o foi ajustado
         /*MESSAGE 'antrei para ajustar'
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
         RUN setMsg IN hBoMsg('104','A quantidade da etiqueta no banco de dados Ç maior e por isso ser† feito o ajuste','log').
         RUN ajustarEtqCortada. 
      END.      
      ELSE DO:
        RUN setMsg IN hBoMsg('105','A quantidade da etiqueta no banco de dados Ç IGUAL  e por isso ser† N«O feito o ajuste','log').
        
      END.
      /*MESSAGE 'vou incluir os cortes'
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.     */
      RUN incluirEtqCorte.      
   END.        
END.
RETURN 'ok'.

CATCH erroSistema   AS Progress.Lang.SysError:
    ASSIGN  cErro = erroSistema:GetMessage(1).
    RUN setMsg IN hBoMsg('999',cErro,'erro').
    /*MESSAGE 'erro sistema:' cErro
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    RETURN ERROR.
END CATCH.

CATCH erroAplicacao AS Progress.Lang.AppError:
    ASSIGN  cErro = erroAplicacao:GetMessage(1).
    RUN setMsg IN hBoMsg('998',cErro,'erro').
    /*MESSAGE 'erro aplicaá∆o:' cErro
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    RETURN ERROR.
END CATCH.                                                                             


FINALLY:

IF VALID-HANDLE(hBoes049) THEN
DO:
    RUN finalizar IN hBoEs049.      
END.

END FINALLY.

PROCEDURE sinrBosMsg:
    
    RUN getTTMsg IN hMsgEs049(OUTPUT TABLE ttBoEs049).
    FOR EACH ttBoEs049:
        RUN setMsg IN hBoMsg(ttBoes049.cod,"BOES049-" + ttBoes049.descricao,ttBoes049.tipo).
    END.


END PROCEDURE.


PROCEDURE ajustarEtqCortada:

    RUN esbo/boes049.p PERSIST SET hBoEs049.
    RUN iniciar IN hBoEs049.
    
    EMPTY TEMP-TABLE ttRegAux.
    CREATE ttRegAux.
    BUFFER-COPY ob-etiqueta TO ttRegAux.
    ASSIGN qtEtqOriAntesAjuste    = ob-etiqueta.quantidade
         ttRegAux.quantidade    = pQtAtuEtqOri .
    RUN setTTReg        IN hBoEs049(TABLE ttRegAux)  .   
    RUN alterar         IN hboEs049.       
    RUN getErros        IN hBoEs049(OUTPUT cErro).
    RUN getHandleMsg    IN hBoEs049(OUTPUT hMsgEs049).
    
    /*MESSAGE 'erro registro ajuste' SKIP
     cErro
     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    
    IF cErro <> '' THEN RETURN ERROR.    
         .         
    FIND CURRENT ob-etiqueta NO-LOCK .
    //1.1-chamar a api que gera o historico de corte da separacao referente ao ajuste da etq que foi cortada          
    RUN esapi/criarHistCorteSeparacao.p(
    TipoHistCorteSeparacao:ajuste, //ok
    pCodEtqLisaOrigem, //ok 
    pNrPedido, //ok
    qtEtqOriAntesAjuste, //ok
    pQtAtuEtqOri, //OK
    pIdTransacao, //ok
    'Ajuste de Etiqueta Original por corte enviado na separaá∆o do pedido',
    0, //s¢ se aplica quando TipoHistCorteSeparacao:inclusao
    0,
    0,
    OUTPUT cErro         
    ). 
    /*MESSAGE 'erro registro de historico do ajuste' SKIP
     cErro
     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    
    IF cErro <> '' THEN RETURN ERROR.   

    IF VALID-HANDLE(hBoes049) THEN
    DO:
        RUN finalizar IN hBoEs049.      
    END.
    
END PROCEDURE.

PROCEDURE incluirEtqCorte.     

      /*MESSAGE 'incluirEtqCorte' SKIP
            'rolo:' pNumrolo  SKIP
            'qt.corte:' pQtCorte
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
     //2-cria nova etiqueta referente ao corte
     //criar tabela temporaria auxiliar e chamar incluir  
     
     RUN esbo/boes049.p PERSIST SET hBoEs049.
     RUN iniciar IN hBoEs049.
     
     EMPTY TEMP-TABLE ttRegAux.
     CREATE ttRegAux.
      ASSIGN 
       ttRegAux.cod-estabel     = '505'
       ttRegAux.dt-emissao      = TODAY
       ttRegAux.hr-emissao      = STRING(TIME,"HH:MM")
       ttRegAux.acondic         = ""
       ttRegAux.it-codigo       = ob-etiqueta.it-codigo
       ttRegAux.cod-refer       = ob-etiqueta.cod-refer
       ttRegAux.nr-container    = ob-etiqueta.nr-container
       ttRegAux.nr-lote         = 'CA'
       ttRegAux.cod-qualid      = 'D' 
       ttRegAux.corte-comerc    = ''
       ttRegAux.quantidade      = pQtCorte
       ttRegAux.localizacao     = ob-etiqueta.localizacao
       ttRegAux.situacao        = 3
       ttRegAux.cod-depos       = 'ITA'
       ttRegAux.num-rolo-imp    = pNumRolo 
       .
     RUN setTTReg IN hBoEs049(TABLE ttRegAux)  .
     /*MESSAGE 'numero da etiqueta lisa passada para a tabela etiqueta_lisa' SKIP
         pCodEtqLisaCorte
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
     RUN  setEtqLisa    IN hBoEs049(pCodEtqLisaCorte).
     RUN  setNrPedido   IN hBoEs049(pNrPedido).
     RUN  setPrePedido  IN hBoEs049(pPrePedido).
     RUN  setNumOrigem  IN hBoEs049(2). 
     RUN incluir  IN hBoEs049.  
     RUN getErros  IN hBoEs049(OUTPUT cErro).
     /*MESSAGE 'erro apos incluir' SKIP
        cErro
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
     IF cErro <> '' THEN RETURN ERROR.
     RUN getUltEtqIncluida IN hBoEs049(OUTPUT numEtqNova).
    
    /* MESSAGE 'apos criar a etiqueta' SKIP
           numEtqNova   SKIP
           'quantidade do corte:' SKIP
           pQtCorte
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
     //3-chamar a api que gera o historico de corte da separacao           
     RUN esapi/criarHistCorteSeparacao.p(
     TipoHistCorteSeparacao:inclusao,
     pCodEtqLisaCorte,
     pNrPedido,
     0,
     pQtCorte,
     pIdTransacao,
     'Criaá∆o de Etiqueta por corte enviado na separaá∆o do pedido',
     numEtqMedOrigem,
     pQtAtuEtqOri,        
     pCodEtqLisaOrigem,
     OUTPUT cErro
     ).  
     IF cErro <> '' THEN RETURN ERROR.    
    
     IF VALID-HANDLE(hBoes049) THEN
     DO:
         RUN finalizar IN hBoEs049.      
     END.

END PROCEDURE.


