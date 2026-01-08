/****************************************************************************
** Programa: Twdi159 - Trigger de write na Tabela Nota Fiscal  
** Data    : Novembro de 2002
** Objetivo: Trigger de Write para a tabela Nota Fiscal 
** Empresa: IMA E INTERMALHAS 
** VersÆo:  2.04.001
** Alterado: 19/01/2004 - FAL
** Fluxo:    Cadastrado na Oficial =>  Backup
29/05/2024 - Tadeu - tsp01 - Acrescimo de registro de altera‡Æo de nota fiscal
para atualiza‡Æo das tabelas de analise de faturamento (fats)
*****************************************************************************/
DEFINE PARAMETER BUFFER b-nota-fiscal FOR nota-fiscal. /** atual **/
DEFINE PARAMETER BUFFER b-nota-fiscal-old FOR nota-fiscal. /** antes **/   

{include/i-prgvrs.i twdi135 2.04.00.001}

DEFINE TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item
       FIELD r-Rowid AS ROWID
       INDEX ch-item-ped IS PRIMARY nome-abrev
                                    nr-pedcli
                                    nr-sequencia
                                    it-codigo
                                    cod-refer.

DEFINE TEMP-TABLE tt-erros-geral
       FIELD identif-msg           AS CHARACTER FORMAT "X(60)"
       FIELD num-sequencia-erro    AS INTEGER FORMAT "999"
       FIELD cod-erro              AS INTEGER FORMAT "99999"
       FIELD des-erro              AS CHARACTER FORMAT "X(60)"
       FIELD cod-maq-origem        AS INTEGER FORMAT "999"
       FIELD num-processo          AS INTEGER FORMAT "999999999".

DEFINE TEMP-TABLE rowerrors no-undo
       FIELD errorsequence    as int
       FIELD errornumber      as int
       FIELD errordescription as char
       FIELD errorparameters  as char
       FIELD errortype        as char
       FIELD errorhelp        as char
       FIELD errorsubtype     as char.

DEF BUFFER b-fat-ser-lote FOR fat-ser-lote.

DEF VAR cErro   AS CHAR.
DEF VAR c-base  AS CHAR    NO-UNDO.
DEF VAR c-chave AS CHAR.
DEF VAR de-peso-bru-total AS DEC FORMAT ">>>,>>9.99".
DEF VAR de-peso-liq-total AS DEC FORMAT ">>>,>>9.99".

//tsp01
DEFINE VARIABLE hBoFats99 AS HANDLE      NO-UNDO.

// Altera o Representante do Cliente para o Representante da Nota
IF NEW b-nota-fiscal THEN DO.

    
   // altera o deposito sempre para ITA quando faturar por Itajai (505)
   IF b-nota-fiscal.cod-estabel = '505' AND
      b-nota-fiscal.nr-pedcli <> '' THEN DO.

      FOR EACH fat-ser-lote OF b-nota-fiscal WHERE
               fat-ser-lote.cod-depos <> 'ITA' SHARE-LOCK.

          ASSIGN b-nota-fiscal.obs-gerada = 'Trocado DEPOSITO para ITA'.

          FIND b-fat-ser-lote OF b-nota-fiscal WHERE
               b-fat-ser-lote.cod-depos = 'ITA' AND
               b-fat-ser-lote.nr-serlote = fat-ser-lote.nr-serlote AND
               b-fat-ser-lote.it-codigo = fat-ser-lote.it-codigo AND
               b-fat-ser-lote.cod-refer = fat-ser-lote.cod-refer
               SHARE-LOCK NO-ERROR.
          IF AVAIL b-fat-ser-lote THEN DO.
             ASSIGN b-fat-ser-lote.qt-baixada[1] = b-fat-ser-lote.qt-baixada[1] + 
                                                   fat-ser-lote.qt-baixada[1].
             DELETE fat-ser-lote.
          END.   
          ELSE
             ASSIGN fat-ser-lote.cod-depos = 'ITA'.
      END.
   END.

   IF b-nota-fiscal.cod-rep <> 0 THEN DO.
      FIND emitente WHERE
           emitente.cod-emitente = b-nota-fiscal.cod-emitente SHARE-LOCK NO-ERROR.
      ASSIGN emitente.cod-rep = b-nota-fiscal.cod-rep.
      FOR EACH clien_financ WHERE
               clien_financ.cdn_cliente = b-nota-fiscal.cod-emitente SHARE-LOCK.
          ASSIGN clien_financ.cdn_repres = b-nota-fiscal.cod-rep.
      END.
   END.

   IF b-nota-fiscal.esp-docto = 22 THEN DO.
      ASSIGN de-peso-bru-total = 0
             de-peso-liq-total = 0.
      FOR EACH it-nota-fisc OF b-nota-fiscal SHARE-LOCK. 
          FIND ITEM WHERE
               ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
        
          ASSIGN it-nota-fisc.peso-liq-fat = ITEM.peso-liquido * it-nota-fisc.qt-faturada[1]
                 it-nota-fisc.peso-bruto = ITEM.peso-bruto * it-nota-fisc.qt-faturada[1].

          ASSIGN de-peso-bru-total = de-peso-bru-total + it-nota-fisc.peso-bruto
                 de-peso-liq-total = de-peso-liq-total + it-nota-fisc.peso-liq-fat.
      END.
      ASSIGN b-nota-fiscal.peso-bru-tot = de-peso-bru-total
             b-nota-fiscal.peso-liq-tot = de-peso-liq-total.
   END.

   /* Tabelas Seniuz */
   FOR EACH it-nota-fisc OF b-nota-fiscal NO-LOCK.
       FOR EACH ped-item-res WHERE 
                ped-item-res.nome-abrev    = b-nota-fiscal.nome-ab-cli AND 
                ped-item-res.nr-pedcli     = it-nota-fisc.nr-pedcli AND
                ped-item-res.it-codigo     = it-nota-fisc.it-codigo AND 
                ped-item-res.nr-sequencia  = it-nota-fisc.nr-seq-ped.

           ASSIGN ped-item-res.faturado    = YES 
                  ped-item-res.cod-estabel = b-nota-fiscal.cod-estabel
                  ped-item-res.serie       = b-nota-fiscal.serie
                  ped-item-res.nr-nota-fis = INT(b-nota-fiscal.nr-nota-fis).

           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                    EXCLUSIVE-LOCK.
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    SHARE-LOCK NO-ERROR.
               IF AVAIL ob-etiqueta THEN
                  ASSIGN ob-etiqueta.situacao = 5.
           END.
       END.
   END.
END.

IF (b-nota-fiscal.dt-cancela <> b-nota-fiscal-old.dt-cancela AND
    b-nota-fiscal.dt-cancela <> ?) THEN DO.
   /*
   (b-nota-fiscal.idi-sit-nf-eletro = 4 AND  // Uso Denegado
    b-nota-fiscal.idi-sit-nf-eletro <> b-nota-fiscal-old.idi-sit-nf-eletro) THEN DO.
   */ 
   FOR EACH ped-item-res WHERE
            ped-item-res.cod-estabel = b-nota-fiscal.cod-estabel AND
            ped-item-res.serie       = b-nota-fiscal.serie AND
            ped-item-res.nr-nota-fis = INT(b-nota-fiscal.nr-nota-fis) AND
            ped-item-res.faturado    = YES SHARE-LOCK.

       ASSIGN ped-item-res.serie       = ""
              ped-item-res.nr-nota-fis = 0
              ped-item-res.faturado    = NO.

       FOR EACH ped-item-rom WHERE
                ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                BREAK BY ped-item-rom.nr-volume.

           FIND ob-etiqueta WHERE
                ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                SHARE-LOCK NO-ERROR.
           IF AVAIL ob-etiqueta THEN
              ASSIGN ob-etiqueta.situacao = 4.
       END.
   END.
END.
/* Fim Seniuz */ 


// Est  Cancelando
IF b-nota-fiscal.dt-cancela <> b-nota-fiscal-old.dt-cancela AND
   b-nota-fiscal.dt-cancela <> ? THEN DO.

   IF b-nota-fiscal.cod-estabel = '505' THEN DO.   // Itaja¡ (integra com Lisa)

      IF b-nota-fiscal.nr-pedcli <> '' THEN DO.
         ASSIGN c-chave = b-nota-fiscal.nr-pedcli + "|" + 
                          b-nota-fiscal.nome-ab-cli.
    
         FIND lisa-integra WHERE
              lisa-integra.cod-trans = "ISF" AND
              lisa-integra.chave = c-chave SHARE-LOCK NO-ERROR.
         IF AVAIL lisa-integra THEN
            ASSIGN lisa-integra.ind-situacao = 1
                   lisa-integra.acao = 'FATURAR'.
    
         ASSIGN c-chave = b-nota-fiscal.cod-estabel + "|" + 
                          b-nota-fiscal.serie + "|" +       
                          b-nota-fiscal.nr-nota-fis.        
    
         FIND lisa-integra WHERE
              lisa-integra.cod-trans = "RemessaNotaVenda" AND
              lisa-integra.chave = c-chave SHARE-LOCK NO-ERROR.
    
         IF AVAIL lisa-integra THEN DO.
            IF lisa-integra.acao = '' OR
               lisa-integra.ind-situacao = 2 THEN DO. // Finalizado (J  foi enviado para Lisa)
               ASSIGN lisa-integra.ind-situacao = 1 
                      lisa-integra.acao = 'EXCLUIR'. 

               FIND ped-venda WHERE
                    ped-venda.nome-abrev = b-nota-fiscal.nome-ab-cli AND
                    ped-venda.nr-pedcli = b-nota-fiscal.nr-pedcli 
                    NO-LOCK NO-ERROR.

               // Exclui a Nota na Lisa
               RUN esapi/envia-canc-pedido-lisa.p (INPUT ROWID(ped-venda), 
                                                   OUTPUT cErro).
            END.
            ELSE 
               DELETE lisa-integra.
         END.
      END.

      // Nota de Importa‡Æo 
      IF b-nota-fiscal.nro-proc-entrada <> 0 THEN DO.  // numero do Container
         FOR EACH lisa-integra WHERE
                  lisa-integra.cod-trans = 'NotaEntrada' AND
                  lisa-integra.ind-situacao = 1 AND 
                  lisa-integra.acao = 'ENVIAR' AND
                  lisa-integra.chave = STRING(b-nota-fiscal.nro-proc-entrada)
                  SHARE-LOCK.
             ASSIGN lisa-integra.acao = 'GERAR'.
         END.
      END.
   END.
END.

//tsp01
RUN esbo/boFats99.p PERSIST SET hBoFats99.
RUN iniciar         IN hBoFats99.
RUN setData         IN hBoFats99(b-nota-fiscal.dt-emis-nota).
RUN setProgOrigem   IN hBoFats99('tw-di135').
RUN setTipoRegistro IN hBoFats99('faturamento').
RUN inserir         IN hBoFats99.
RUN finalizar       IN hBoFats99.





/*
IF b-nota-fiscal.dt-cancela   = ? AND 
   b-nota-fiscal.ind-sit-nota <> 4 THEN DO:
    
    FIND FIRST im-ext-nota-fiscal WHERE
               im-ext-nota-fiscal.cod-estabel = b-nota-fiscal.cod-estabel AND
               im-ext-nota-fiscal.serie       = b-nota-fiscal.serie       AND
               im-ext-nota-fiscal.nr-nota-fis = b-nota-fiscal.nr-nota-fis
               SHARE-LOCK NO-ERROR.
               
    GET-KEY-VALUE SECTION 'Startup':U key 'BASE':U VALUE c-base.    
    
    IF NOT AVAIL im-ext-nota-fiscal THEN DO:
        CREATE im-ext-nota-fiscal.
        BUFFER-COPY b-nota-fiscal TO im-ext-nota-fiscal.
        IF c-base = "OFICIAL" THEN
            ASSIGN im-ext-nota-fiscal.origem = 1.
        ELSE IF c-base = "BACKUP" THEN
            ASSIGN im-ext-nota-fiscal.origem = 2.

        FIND FIRST estabelec WHERE
                   estabelec.cod-estabel = im-ext-nota-fiscal.cod-estabel 
                   NO-LOCK NO-ERROR.
        IF AVAIL estabelec THEN
            ASSIGN im-ext-nota-fiscal.ep-codigo = estabelec.ep-codigo.               
    END.
END.
*/

/*
IF b-nota-fiscal.dt-confirma <> ? THEN DO:   
   FOR EACH tt-ped-item.
       DELETE tt-ped-item.
   END.

   FOR EACH tt-erros-geral.
       DELETE tt-erros-geral.
   END.

   FOR EACH rowerrors.
       DELETE rowerrors.
   END.

   FOR EACH ped-item WHERE  
            ped-item.nome-abrev    = b-nota-fiscal.nome-ab-cli AND
            ped-item.nr-pedcli     = b-nota-fiscal.nr-pedcli AND
            (ped-item.cod-sit-item  = 1 OR ped-item.cod-sit-item  = 2)
            NO-LOCK.
       
       CREATE tt-ped-item.
       BUFFER-COPY ped-item TO tt-ped-item.       
   END.
 
   FIND FIRST tt-ped-item NO-LOCK NO-ERROR.
   IF AVAIL tt-ped-item THEN DO.
      RUN cancela-item.

      FOR EACH tt-erros-geral NO-LOCK.
          RUN MESSAGE.p (INPUT "Cancelamento de Itens",
                         INPUT STRING(tt-erros-geral.cod-erro) + " - " + tt-erros-geral.des-erro + " . ").
      END.
   END.
END.
*/

/*
PROCEDURE cancela-item.

   DEF VAR bo-ped-item AS HANDLE.
   DEF VAR v-row-ped   AS ROWID.
   DEF VAR v-row-item  AS ROWID.
   DEF VAR v-motivo    AS INT.

   ASSIGN v-motivo = 0.
   FIND FIRST motivo WHERE motivo.ind-tp-trans = 1 NO-LOCK NO-ERROR.
   IF AVAIL motivo THEN
      v-motivo = motivo.cod-motivo.
   ELSE DO:
        FIND FIRST motivo NO-LOCK NO-ERROR.
        IF AVAIL motivo THEN
           v-motivo = motivo.cod-motivo.
   END.
      
   if  not valid-handle(bo-ped-item) or
       bo-ped-item:type <> "PROCEDURE":U or
       bo-ped-item:file-name <> "dibo/bodi154can.p":U then do:           
       run dibo/bodi154can.p persistent set bo-ped-item.       
   end.


   FOR EACH tt-ped-item:                
       FIND FIRST ped-venda WHERE 
                  ped-venda.nome-abrev   = tt-ped-item.nome-abrev AND
                  ped-venda.nr-pedcli    = tt-ped-item.nr-pedcli
                  NO-LOCK NO-ERROR.
       IF NOT AVAIL ped-venda THEN NEXT.

       ASSIGN v-row-ped = ROWID(ped-venda).

       FIND FIRST ped-item WHERE ped-item.nome-abrev   = tt-ped-item.nome-abrev
                             AND ped-item.nr-pedcli    = tt-ped-item.nr-pedcli
                             AND ped-item.it-codigo    = tt-ped-item.it-codigo
                             AND ped-item.nr-sequencia = tt-ped-item.nr-sequencia                          
                             AND ped-item.cod-refer    = tt-ped-item.cod-refer                         
                             NO-LOCK NO-ERROR.
       IF AVAIL ped-item THEN ASSIGN v-row-item = ROWID(ped-item).
       ELSE NEXT.

       run emptyRowErrors in bo-ped-item.    
       
       run validateCancelation in bo-ped-item(INPUT v-row-item , INPUT "Cancelamento de Saldos em Aberto" , INPUT-OUTPUT TABLE RowErrors).   

       run updateCancelation IN bo-ped-item(INPUT v-row-item , INPUT "Cancelamento de Saldos em Aberto" , INPUT TODAY , INPUT v-motivo).

       run getRowErrors in bo-ped-item(output table RowErrors).   

       FOR EACH RowErrors WHERE RowErrors.ErrorType <> "INTERNAL":U:
           CREATE tt-erros-geral.
           UPDATE tt-erros-geral.num-sequencia-erro = RowErrors.ErrorSequence
                  tt-erros-geral.cod-erro           = RowErrors.ErrorNumber
                  tt-erros-geral.des-erro           = ROwErrors.ErrorDescription
                  tt-erros-geral.Identif-Msg        = "Erro BO154CAN".
       END.
   END.

   /*elimina todos os handles usado na bodi154can*/
   if  valid-handle(bo-ped-item) then do:
       RUN destroy IN bo-ped-item.
       ASSIGN bo-ped-item = ?.
   end. 

END PROCEDURE.
*/

RETURN 'OK'.
