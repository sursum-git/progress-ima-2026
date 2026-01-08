
/******************************************************************************************************
** Programa : TWin403 - trigger de Write para a tabela saldo-estoq
** Data     : 10/2020
** Objetivo : trigger de Write para a tabela saldo-estoq
** Empresa  : IMA 
** Vers∆o   : 12.6.001
** Alterado : 
** Fluxo    : Avisar por e-mail para itens que est∆o zerados e voltam saldo
22/08/2023 - a pedido da jessica, colocado parametro para enviar o e-mail, pois 
em geral n∆o est† sendo mais necess†rio.
17/04/2024 - tadeu - tsp01 - acrescimo da BO para geraá∆o de pendencia de geraá∆o de Book
******************************************************************************************************/
DEFINE PARAMETER BUFFER bSENovo FOR saldo-estoq.
DEFINE PARAMETER BUFFER bSEOld  FOR saldo-estoq.
{esbo/boSaldo.i}
DEFINE VARIABLE hBoConsParam    AS HANDLE      NO-UNDO.
DEFINE VARIABLE qtMinKG         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE qtMinMT         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE qtTotal         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cListaEmail     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vlParam         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE natOperImp      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListaItens     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lDesconsiderar  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lGerarBook      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lPendBook       AS LOGICAL     NO-UNDO. //determina se o deposito e o estab corrente entra na regra do pend_book
DEFINE VARIABLE cErro           AS CHARACTER   NO-UNDO.

/*DEFINE STREAM s1.
output STREAM s1 TO value('c:\temp\LOG_twin403_' + STRING(RANDOM(1,9999)) + STRING(TIME) + '.txt').*/

{utp/ut-glob.i}
RUN esbo/boConsParam.p PERSISTENT SET hBoConsParam.

FIND FIRST bSeNovo NO-LOCK NO-ERROR.
FIND FIRST bSeOld  NO-LOCK NO-ERROR.
IF AVAIL bSeNovo AND AVAIL  bSeOld THEN DO:
   RUN getQtMinMtBook IN hBoConsParam(output vlParam).
   ASSIGN qtMinMT = DEC(vlParam).

   RUN getQtMinKGBook IN hBoConsParam(output vlParam).
   ASSIGN qtMinKG = DEC(vlParam).

   RUN getEmailsErrosArqDesign      IN hBoConsParam(output cListaEmail).
   RUN getListaItensEmailSaldoEstoq IN hBoConsParam(output cListaItens).
   FIND ITEM OF bSeNovo NO-LOCK NO-ERROR.

   // tratamento para item que voltou estoque
   IF AVAIL bSeOld AND AVAIL ITEM AND ITEM.ge-codigo = 60 
      AND bSeOld.qtidade-atu <= 0
      AND (
          (bSeNovo.qtidade-atu >= qtMinMT AND ITEM.un = 'm')
          OR 
          (bSeNovo.qtidade-atu >= qtMinKG AND ITEM.un = 'kg')
          ) THEN DO:
      FIND LAST movto-estoq 
          WHERE movto-estoq.it-codigo   =  bSeNovo.it-codigo
          AND   movto-estoq.cod-estabel =  bSeNovo.cod-estabel
          USE-INDEX item-data NO-LOCK NO-ERROR.

      IF AVAIL movto-estoq THEN DO:
         //RUN getNatOperacaoImp IN hboConsParam(output natOperImp).
         IF substr(movto-estoq.nat-operacao,1,1) = '3' THEN DO:
            ASSIGN lDesconsiderar = YES.
         END.
      END.
      ELSE DO:
          ASSIGN lDesconsiderar = YES.
      END.
      IF lDesconsiderar = NO AND lookup(bSeNovo.it-codigo,cListaItens) > 0 THEN DO:
         ASSIGN lGerarBook = YES.
         RUN exportarLog( 'Item_' + bSeOld.it-codigo + '_ref_' + bSeOld.cod-refer,
                       'qt.anterior:' + STRING(bSeold.qtidade-atu) + " - Ref:"  +  STRING(bSeNovo.qtidade-atu) ).



        RUN enviarEmail( input 'imatextil@imatextil.com.br' ,
                         input cListaEmail,
                         input " Item:" +  bSeold.it-codigo + " Ref.:" +  bSeold.cod-refer  + " - Voltou ao Estoque - Data/Hora: " + STRING(NOW,'99/99/9999 hh:mm:ss') ,        /* Assunto */      
                         input 'Saldo anterior:' + STRING(bSeold.qtidade-atu) + " - Saldo Atual: " + STRING(bSeNovo.qtidade-atu),  /* Mensagem */
                         input '' ) .
      END.
      ELSE DO:
         RUN exportarLog( 'DESCONSIDERADO_Item_' + bSeOld.it-codigo + '_ref_' + bSeOld.cod-refer,
                       'qt.anterior:' + STRING(bSeold.qtidade-atu) + " - Ref:"  +  STRING(bSeNovo.qtidade-atu) ).

      END.
   END.

   //tratamento para item que saiu do estoque
   //RUN getSaldoTotalItemRef(output qtTotal, output lPendBook). dando erro no recebimento verificar
   
   //put STREAM s1   "pend.book:" lPendBook "estab:" bSeNovo.cod-estabel "depos." bSeNovo.cod-depos  "saldo total:" qtTotal  " - qt.atual:"   bSeNovo.qtidade-atu  ' - qt.anterior:'  bSeOld.qtidade-atu  SKIP.
   IF lPendBook THEN DO:
      //saldo de estoque acabou
      IF (bSeNovo.qtidade-atu <= 0 AND bSeOld.qtidade-atu > 0 ) AND ITEM.ge-codigo = 60  THEN DO:
         IF bSeNovo.qtidade-atu >= qtTotal THEN
            ASSIGN lGerarBook = YES.
      END.
    
      //saldo de estoque voltou ou Ç o primeiro saldo
      IF  (bSeNovo.qtidade-atu  > 0 AND bSeOld.qtidade-atu <= 0 ) AND ITEM.ge-codigo = 60  THEN DO:
          IF qtTotal - bSeNovo.qtidade-atu <= 0 THEN
             ASSIGN lGerarBook = YES.
      END.
   END.    
END.


IF lGerarBook THEN DO:
    //put STREAM s1 'entrei no gerar book' SKIP.
    //tsp01
    RUN esapi/apiesp800.p(input bSeNovo.it-codigo,
                          input qtTotal - bSeNovo.qtidade-atu + bSeOld.qtidade-atu,
                          input qtTotal,
                          output cErro).
    IF cErro <> '' THEN DO:

       
       RUN getEmailsErroPendBook IN hBoConsParam(output cListaEmail).

       RUN enviarEmail( input 'imatextil@imatextil.com.br' ,
                         input cListaEmail,
                         input " Item:" +  bSeold.it-codigo + " Ref.:" +  bSeold.cod-refer  + " - registro de ERRO ao incluir pendencia de book - Data/Hora: " + STRING(NOW,'99/99/9999 hh:mm:ss') ,        /* Assunto */      
                         input cErro,  /* Mensagem */
                         input '' ) .
    END.
END.

IF VALID-HANDLE(hBoConsParam) THEN
   DELETE PROCEDURE hBoConsParam.

//output STREAM s1 CLOSE.
PROCEDURE exportarLog:
    DEFINE input  PARAMETER cNomeArquivo AS CHARACTER   NO-UNDO.
    DEFINE input  PARAMETER cTexto AS CHARACTER   NO-UNDO.
    /*output TO value('p:\LOG_retorno_estoque_' + cNomeArquivo + '_' + STRING(TIME) +  '.txt') .
        //put UNFORMAT cTexto SKIP.
    output CLOSE. */

END PROCEDURE.

    
PROCEDURE enviarEmail:

    DEFINE input  PARAMETER rementente   AS CHARACTER   NO-UNDO.
    DEFINE input  PARAMETER destinatario AS CHARACTER   NO-UNDO.
    DEFINE input  PARAMETER titulo       AS CHARACTER   NO-UNDO.
    DEFINE input  PARAMETER corpo        AS CHARACTER   NO-UNDO.
    DEFINE input  PARAMETER anexo        AS CHARACTER   NO-UNDO.


    RUN esapi/esapi002.p (input rementente,        /* e-mail remetente */
                          input destinatario,         /* e-mail destinat†rio */
                          input titulo ,        /* Assunto */
                          input corpo,  /* Mensagem */
                          input anexo,  /* Anexo */
                          input NO) NO-ERROR .   /* Mostra Erros */ 




END PROCEDURE.


PROCEDURE getSaldoTotalItemRef:

    DEFINE output PARAMETER qtTotal     AS DECIMAL     NO-UNDO.
    DEFINE output PARAMETER pPendBook   AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE hBo AS HANDLE      NO-UNDO.

RUN esbo/boSaldo.p PERSIST SET hBo.
RUN iniciarBos IN hBo.
RUN getSaldoItemRef IN hBo(FALSE, // saldo por item
                           TRUE, //buscarPE
                           FALSE, //buscar PI
                           TRUE, // considerar negativo 
                           '520078',
                           '554',
                           0,
                           99999) .
RUN finalizarBos    IN hBo.
RUN getTTSaldo      IN hBo(output TABLE ttSaldo).
RUN getTtEstabDepos IN hBo(output TABLE ttEstabDepos).
IF CAN-FIND(FIRST TtEstabDepos WHERE 
            ttEstabDepos.codEstab = bSeNovo.cod-estabel
            AND ttEstabDepos.codDepos = bSeNovo.cod-depos) THEN
    ASSIGN pPendBook = YES.
ASSIGN qtTotal = 0.
FOR EACH ttSaldo:
    ASSIGN qtTotal = qtTotal + ttSaldo.qtSaldoPe.
END.

END PROCEDURE.
