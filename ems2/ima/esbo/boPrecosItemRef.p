/***********************************************************
programa:esbo/boPrecosItemREf.p
objetivo:retornar os pre‡os dos itens e referencias
conforme as regras especificas da IMA.
autor:Tadeu Silva 
Data: 09/2020 
*************************************************************/

{esbo/boPrecosItemRef.i}
{utp/ut-glob.i}
{esbo\boMsg.i}

DEFINE VARIABLE cItem               LIKE ITEM.it-codigo   NO-UNDO.
DEFINE VARIABLE cRef                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUF                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nrContainer         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPrazoMedio         AS INTEGER     NO-UNDO.
DEFINE VARIABLE lIndPrazo           AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lPedRubiX           AS LOGICAL     NO-UNDO.
/*DEFINE VARIABLE precoTb     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE precoPI     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE PrecoOut    AS DECIMAL     NO-UNDO.*/
DEFINE VARIABLE iTipoBusca          AS INTEGER     NO-UNDO.  //0-todos 1-PE 2-PI
DEFINE VARIABLE hBoMsgPreco         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoControlePreco    AS HANDLE      NO-UNDO.
DEFINE VARIABLE iTbPreco            AS INTEGER     NO-UNDO INIT 1.

//variaveis tsp01
DEFINE VARIABLE lContrAgrup                 AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lPrimeiroItem               AS LOGICAL     NO-UNDO INIT YES.
DEFINE VARIABLE lDivideComis                AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE dPercComisVend              AS DECIMAL     NO-UNDO INIT 0.
DEFINE VARIABLE dPercComisRepres            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cMsgAgrup                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dtRefer                     AS DATE        NO-UNDO.
DEFINE VARIABLE cLoginPortal                AS CHARACTER   NO-UNDO.
/*DEFINE VARIABLE itCodigoAgrup               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE codReferAgrup               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lDivideComisCorrente        AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE dPercComisVendCorrente      AS DECIMAL     NO-UNDO INIT 0.
DEFINE VARIABLE dPercComisRepresCorrente    AS DECIMAL     NO-UNDO.*/

PROCEDURE iniciarBos:
    RUN esbo/boMsg.p PERSISTENT SET hBoMsgPreco.
    RUN esbo/boControlePreco.p PERSISTENT SET hBoControlePreco.

END PROCEDURE.


PROCEDURE finalizarBos:

    IF VALID-HANDLE(hBoMsgPreco) THEN
       DELETE PROCEDURE hBoMsgPreco.
    IF VALID-HANDLE(hBoControlePreco) THEN
       DELETE PROCEDURE hBoControlePreco.

END PROCEDURE.

PROCEDURE getVarsAgrup:

    DEFINE OUTPUT PARAMETER pDivideComis     AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER pPercComisVend   AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER pPercComisRepres AS DECIMAL     NO-UNDO.

    ASSIGN pDivideComis     = lDivideComis
           pPercComisVend   = dPercComisVend
           pPercComisRepres = dPercComisRepres .


END PROCEDURE.

PROCEDURE limparTTPreco:
    EMPTY TEMP-TABLE ttPreco.
END PROCEDURE.

PROCEDURE limparTTMsg:
    EMPTY TEMP-TABLE ttMsg.
END PROCEDURE.

PROCEDURE setTbPreco:
    DEFINE INPUT  PARAMETER pTbPreco AS INTEGER     NO-UNDO.
    ASSIGN iTbPreco = pTbPreco .
    RUN setMsg IN hBoMsgPreco(0,"Tabela de pre‡o->" + string(iTbPreco) ,'aviso').


END PROCEDURE.


PROCEDURE setPedRubiX:

    DEFINE INPUT  PARAMETER pPedRubiX AS LOGICAL     NO-UNDO.
    ASSIGN lPedRubiX  = pPedRubiX .

END PROCEDURE.


PROCEDURE setLoginPortal:

    DEFINE INPUT  PARAMETER pLogin AS CHARACTER   NO-UNDO.
    ASSIGN cLoginPortal = pLogin.


END PROCEDURE.
PROCEDURE setDtRefer:
    DEFINE INPUT  PARAMETER pData AS DATE        NO-UNDO.
    ASSIGN dtRefer = pdata.

END PROCEDURE.

PROCEDURE setItem:
  DEFINE INPUT  PARAMETER pItem AS CHARACTER   NO-UNDO.
  ASSIGN cItem = pItem.
  RUN setMsg IN hBoMsgPreco(0,"Item->" + string(cItem) ,'aviso').
END PROCEDURE.

PROCEDURE setRef:
  DEFINE INPUT  PARAMETER pRef AS CHARACTER   NO-UNDO.
  ASSIGN cRef = pRef.
  RUN setMsg IN hBoMsgPreco(0,"referencia->" + string(cref) ,'aviso').
END PROCEDURE.

PROCEDURE setNrContainer:
  DEFINE INPUT  PARAMETER pNrContainer aS INTEGER   NO-UNDO.
  ASSIGN nrContainer = pnrContainer.
  RUN setMsg IN hBoMsgPreco(0,"Container->" + string(nrContainer) ,'aviso').
END PROCEDURE.

PROCEDURE setTipoBusca:
    DEFINE INPUT  PARAMETER pTipo AS INTEGER     NO-UNDO.
    ASSIGN iTipoBusca = pTipo.
    RUN setMsg IN hBoMsgPreco(0,"Tipo Busca->" + string(iTipoBusca) ,'aviso').
END PROCEDURE.


PROCEDURE setPrazoMedio:
    DEFINE INPUT  PARAMETER pPrazoMedio AS INTEGER     NO-UNDO.
    ASSIGN iPrazoMedio = pPrazoMedio.
    RUN setMsg IN hBoMsgPreco(0,"Prazo M‚dio->" + string(iPrazoMedio) ,'aviso').
END PROCEDURE.

PROCEDURE setUfCliente:
    DEFINE INPUT  PARAMETER pUF AS CHARACTER   NO-UNDO.
    ASSIGN cUF = pUF.
    RUN setMsg IN hBoMsgPreco(0,"UF(necess rio apenas para empresa IMA)->" + cUF ,'aviso').

END PROCEDURE.

PROCEDURE buscarPrecos:
    EMPTY TEMP-TABLE ttPreco .
    RUN setLoginPortal IN hBoControlePreco(cLoginPortal).
    RUN setPedRubiX    IN hBoControlePreco(lPedRubiX).
    RUN getTTIndPrazo.
   
    CASE iTipoBusca:
        WHEN 0 THEN DO:   //todos
           RUN buscarPrecosPI.
           RUN buscarPrecoPE.
        END.
        WHEN 1 THEN DO: // PE
           RUN buscarPrecoPE.
        END.
        WHEN 2 THEN DO: //PI
           RUN buscarPrecosPI.

        END.

    END CASE.
    RUN setIndicesPreco.
END PROCEDURE.

PROCEDURE getPrecoPrazoPorId:
    DEFINE INPUT  PARAMETER pID        AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER dReal      AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER dDolar     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER iTipoPreco AS INTEGER.

    DEFINE VARIABLE dIndice AS DECIMAL     NO-UNDO.

    RUN getPrecoPorId in hboControlePreco(INPUT pID, 
                                          OUTPUT dReal, 
                                          OUTPUT dDolar, 
                                          OUTPUT iTipoPreco).

    IF iPrazoMedio <= 1 THEN
       RUN getIndicePrazo (1,OUTPUT dIndice).
    else
       IF iPrazoMedio <= 30 THEN
          RUN getIndicePrazo(30,OUTPUT dIndice).
       ELSE 
          IF iPrazoMedio <= 60 THEN
             RUN getIndicePrazo(60,OUTPUT dIndice).
          ELSE
             RUN getIndicePrazo(90,OUTPUT dIndice).

    ASSIGN dReal = dReal * dIndice
           dDolar = dDolar * dIndice.

END PROCEDURE.


PROCEDURE setIndicesPreco:
    FOR EACH ttPreco:
        DEFINE VARIABLE indice AS DECIMAL     NO-UNDO.
        
        RUN getIndicePrazo(1,OUTPUT ttPreco.vlPrecoAVista).
        RUN getIndicePrazo(30,OUTPUT ttPreco.vlPreco30Dias).
        RUN getIndicePrazo(60,OUTPUT ttPreco.vlPreco60dias).
        RUN getIndicePrazo(90, OUTPUT ttPreco.vlPreco90dias).
        
        /*IF ttPreco.tipo = 'outlet' THEN DO:
           ASSIGN ttPreco.vlPrecoBase = ttPreco.vlPrecoBase / ttPreco.vlPreco90dias.
               
        END.*/
        ASSIGN  ttPreco.vlprecoAVista  =    ttPreco.vlprecoAVista * ttPreco.vlprecoBase
                ttPreco.vlpreco30Dias  =    ttPreco.vlpreco30Dias * ttPreco.vlprecoBase
                ttPreco.vlpreco60Dias  =    ttPreco.vlpreco60Dias * ttPreco.vlprecoBase
                ttPreco.vlpreco90Dias  =    ttPreco.vlpreco90Dias * ttPreco.vlprecoBase .


        RUN getIndicePrazo(1,OUTPUT ttPreco.vlprecoAVistaDolar).
        RUN getIndicePrazo(30,OUTPUT ttPreco.vlpreco30DiasDolar).
        RUN getIndicePrazo(60,OUTPUT ttPreco.vlpreco60DiasDolar).
        RUN getIndicePrazo(90,OUTPUT ttPreco.vlpreco90DiasDolar).

        ASSIGN  ttPreco.vlprecoAVistaDolar  =    ttPreco.vlprecoAVistaDolar * ttPreco.vlprecoBaseDolar
                ttPreco.vlpreco30DiasDolar  =    ttPreco.vlpreco30DiasDolar * ttPreco.vlprecoBaseDolar
                ttPreco.vlpreco60DiasDolar  =    ttPreco.vlpreco60DiasDolar * ttPreco.vlprecoBaseDolar
                ttPreco.vlpreco90DiasDolar  =    ttPreco.vlpreco90DiasDolar * ttPreco.vlprecoBaseDolar .
        
        IF iPrazoMedio <= 1 THEN
           RUN getIndicePrazo(1,OUTPUT indice).
        else
            IF iPrazoMedio <= 30 THEN
               RUN getIndicePrazo(30,OUTPUT indice).
            ELSE 
               IF iPrazoMedio <= 60 THEN
                  RUN getIndicePrazo(60,OUTPUT indice).
               ELSE
                  RUN getIndicePrazo(90,OUTPUT indice).

        ASSIGN ttPreco.vlPrecoPrazo  = ttPreco.vlprecoBase  *  indice  
               ttPreco.vlPrecoPrazoDolar = ttPreco.vlprecoBaseDolar *  indice  .
        RUN setMsg IN hBoMsgPreco(5,"indice:" + STRING(indice) + " - pre‡o base em REAL:" + STRING(ttPreco.vlprecoBase) + 
                                    " - pre‡o base em DOLAR:" + STRING(ttPreco.vlprecoBaseDolar),'aviso').

    END.                                                  
END PROCEDURE.


PROCEDURE getIndicePrazo:

    DEFINE INPUT  PARAMETER pDia AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER dInd AS DECIMAL     NO-UNDO.
    FIND FIRST ttIndPrazo
        WHERE ttIndPrazo.qtDias = pDia
        NO-LOCK NO-ERROR.
    IF AVAIL ttIndPrazo THEN DO:
       ASSIGN dInd = ttIndPrazo.indice.
       RUN setMsg IN hBoMsgPreco(6,'Encontrada ttIndPrazo para qt.dias:' + STRING(pDia),'aviso').
    END.
       
    ELSE DO:
         RUN setMsg IN hBoMsgPreco(60,'nao encontrada ttIndPrazo para qt.dias:' + STRING(pDia),'erro').
    END.

END PROCEDURE.

PROCEDURE getPrecoPI:

    DEFINE OUTPUT PARAMETER pId          AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER pvlReal      AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER pvlDolar     AS DECIMAL     NO-UNDO.

    RUN setTpPreco      IN hBoControlePreco(2).
    RUN setTbPreco      IN hBoControlePreco(iTbPreco).
    RUN setItem         IN hBoControlePreco(cItem).
    RUN setDtRefer      IN hBoControlePreco(dtRefer).
    RUN setCodRefer     IN hBoControlePreco(cRef).
    RUN setNrContainer  IN hBoControlePreco(nrContainer).
    RUN getPrecoAtual   IN hBoControlePreco(OUTPUT pId,OUTPUT pvlReal, OUTPUT pvlDolar).
    RUN getVarsAgrup    IN hBoControlePreco(OUTPUT lDivideComis , OUTPUT dPercComisVend, OUTPUT dPercComisRepres).

END PROCEDURE.

PROCEDURE buscarPrecosPI:
    DEFINE VARIABLE pId         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE vlReal      AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE vlDolar     AS DECIMAL     NO-UNDO.

    DEFINE VARIABLE lAchou AS LOGICAL INIT NO     NO-UNDO.
    IF nrContainer = 0 THEN DO:
       FOR EACH pp-it-container WHERE 
                pp-it-container.it-comprado  = cItem AND
                pp-it-container.ref-comprada = cRef
                NO-LOCK,
           EACH pp-container OF pp-it-container
           WHERE pp-container.situacao = 1.

           ASSIGN lAchou = YES.

           RUN getPrecoPI(OUTPUT pId,
                          OUTPUT vlReal,
                          OUTPUT vlDolar).

           RUN inserirTTPreco(
                pp-it-container.it-comprado,        
                pp-it-container.ref-comprada,          
                pp-container.Nr-Container,  
                vlReal,
                vlDolar,
                'pi',
                pId,
                iTbPreco
                ).
       END.
       IF lAchou = NO THEN 
          RUN setMsg IN hBoMsgPreco(10,"NÇO foi encontrado pre‡o para o item/ref nos container's em aberto.",'erro').
       ELSE 
         RUN setMsg IN hBoMsgPreco(1,"FOI encontrado pre‡o para o item/ref nos container's em aberto.",'aviso').

    END.
    ELSE DO:
        FIND FIRST  pp-it-container WHERE 
        pp-it-container.nr-container = nrContainer AND
        pp-it-container.it-comprado  = cItem AND
        pp-it-container.ref-comprada = cRef  NO-LOCK NO-ERROR.
        IF AVAIL pp-it-container THEN DO.
           RUN getPrecoPI(OUTPUT pId,
                          OUTPUT vlReal,
                          OUTPUT vlDolar).

           RUN inserirTTPreco(
                pp-it-container.it-comprado,        
                pp-it-container.ref-comprada,          
                nrContainer,  
                vlReal,
                vlDolar,
               'pi',
                pId,
                iTbPreco
                 ).
        END.
        ELSE DO:
            RUN setMsg IN hBoMsgPreco(11,"NÇO foi encontrado pre‡o para o item/ref/container:" + cItem
                                        + "/" + cRef + "/" + STRING(nrContainer),'erro').
        END.
    END.    

END PROCEDURE.

PROCEDURE getControleEspPE:

    DEFINE OUTPUT PARAMETER pId         AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER vlReal      AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER vlDolar     AS DECIMAL     NO-UNDO.

    RUN settpPreco      IN hBoControlePreco(1).
    RUN settbPreco      IN hBoControlePreco(iTbPreco).
    RUN setItem         IN hBoControlePreco(cItem).
    RUN setDtRefer      IN hBoControlePreco(dtRefer).
    RUN setCodRefer     IN hBoControlePreco(cRef).
    RUN setNrContainer  IN hBoControlePreco(0).
    RUN getPrecoAtual   IN hBoControlePreco(OUTPUT pId, OUTPUT vlReal,OUTPUT vlDolar).
    RUN getVarsAgrup    IN hBoControlePreco(OUTPUT lDivideComis, OUTPUT dPercComisVend, OUTPUT dPercComisRepres).
END PROCEDURE.

PROCEDURE getPrecoOutlet:

    DEFINE OUTPUT PARAMETER pId         AS INTEGER     NO-UNDO INIT 0.
    DEFINE OUTPUT PARAMETER vlReal      AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER vlDolar     AS DECIMAL     NO-UNDO.

    DEFINE VARIABLE lBuscarLiquidaIma   AS INT    NO-UNDO.


    RUN esapi/getImParam.p('buscar_liquida_ima',OUTPUT lBuscarLiquidaIma).

    IF lBuscarLiquidaIma = 1 AND iTbPreco = 1 THEN DO:
       FIND LAST liquida-ima WHERE
       liquida-ima.cod-estabel  = i-ep-codigo-usuario AND
       liquida-ima.it-codigo    = cItem AND
       liquida-ima.cod-refer    = cRef  AND 
       liquida-ima.dt-ini      <= TODAY AND
       (liquida-ima.dt-final   >= TODAY OR liquida-ima.dt-final = ?)
       NO-LOCK NO-ERROR.
       IF AVAIL liquida-ima THEN DO:
          
           ASSIGN pId      =  0 //int(num-id-liquida-ima)
                 vlReal   = liquida-ima.preco-item
                 vlDolar  = 0
                 iTbPreco = 1.
       END.
    END.
    ELSE DO:
        RUN setTpPreco          IN hBoControlePreco(3).
        RUN setTbPreco          IN hBoControlePreco(iTbPreco).
        RUN setItem             IN hBoControlePreco(cItem).
        RUN setDtRefer          IN hBoControlePreco(dtRefer).
        RUN setCodRefer         IN hBoControlePreco(cRef).
        RUN setNrContainer      IN hBoControlePreco(0).
        RUN getPrecoAtual       IN hBoControlePreco(OUTPUT pId, OUTPUT vlReal,OUTPUT vlDolar).
        RUN getVarsAgrup        IN hBoControlePreco(OUTPUT lDivideComis , OUTPUT dPercComisVend, OUTPUT dPercComisRepres).
    END.
    
END PROCEDURE.

PROCEDURE getPrecoERPPE:

   DEFINE OUTPUT PARAMETER pId         AS INTEGER     NO-UNDO INIT 0.
   DEFINE OUTPUT PARAMETER vlReal      AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER vlDolar     AS DECIMAL     NO-UNDO.

   DEFINE VARIABLE cTabela AS CHARACTER   NO-UNDO.
   RUN getTbPrecoPadrao(OUTPUT cTabela).
   FIND ITEM 
       WHERE ITEM.it-codigo = cItem
       NO-LOCK NO-ERROR.
   FIND FIRST preco-item WHERE
        preco-item.nr-tabpre = cTabela AND
        preco-item.it-codigo = cItem   AND
        preco-item.cod-refer = cRef    AND 
        preco-item.cod-unid-med = IF AVAIL ITEM THEN item.un ELSE ''
        NO-LOCK NO-ERROR.
   IF AVAIL preco-item THEN DO:
      ASSIGN vlReal  = preco-item.preco-venda
             vlDolar = 0.
   END.
  /*MESSAGE 'tabela' SKIP
           cTabela SKIP
          'item' SKIP
          cItem SKIP
          'ref' SKIP
          cRef SKIP
         AVAIL preco-item

      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/


END PROCEDURE.

PROCEDURE buscarPrecoPE:

   DEFINE VARIABLE cTabela AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE pId           AS INTEGER     NO-UNDO INIT 0.
   DEFINE VARIABLE vlReal        AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE vlDolar       AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE lBuscarTbERP  AS INT         NO-UNDO.

   RUN esapi/getImParam.p('buscar_tb_erp',OUTPUT lBuscarTbERP).
  
   IF lBuscarTbERP = 1 THEN DO:
      IF iTbPreco = 1 THEN //tabela padrÆo
         RUN getPrecoERPPE(OUTPUT pId,OUTPUT vlReal,OUTPUT vlDolar).
      ELSE DO:
         RUN getControleEspPE(OUTPUT pId,OUTPUT vlReal,OUTPUT vlDolar).
      END.
         
   END.  
   ELSE
      RUN getControleEspPE(OUTPUT pId,OUTPUT vlReal,OUTPUT vlDolar).
   
   RUN inserirTTPreco(
             cItem,        
              cRef,          
              0,  
              vlReal,
              vlDolar,
              'pe',
               pId,
               IF lBuscarTbERP = 1 THEN 0 ELSE itbPreco
               ).

   
   
   IF vlReal = 0 THEN DO:
      RUN setMsg IN hBoMsgPreco(20,"NÇO foi encontrado pre‡o para o item/ref:" +  cItem + "/" + cRef + " no PE." ,'erro').
   END.
   ELSE DO:
       RUN setMsg IN hBoMsgPreco(2,"FOI encontrado pre‡o para o item/ref no PE:" + STRING(vlReal),'aviso').
   END.

   RUN getPrecoOutlet(OUTPUT pid,OUTPUT vlReal, OUTPUT vlDolar).


   IF vlReal > 0 THEN DO:
     RUN inserirTTPreco(
                cItem,        
                cRef,          
                0,  
                vlReal,
                vlDolar,
               'outlet',
                pId,
                itbPreco
                 ).
      RUN setMsg IN hBoMsgPreco(3,"FOI encontrado pre‡o para o item/ref no outlet.",'aviso').
   END.
   ELSE DO:
     RUN setMsg IN hBoMsgPreco(30,"NÆo foi encontrado pre‡o para o item/ref no outlet.",'aviso').  
   END.

END PROCEDURE.

PROCEDURE getPrecoPrazo:
    DEFINE INPUT  PARAMETER pTipo              AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pvlPrecoPrazoReal  AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER pvlPrecoPrazoDolar AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER pvControlePreco     LIKE ttPreco.codControlePreco NO-UNDO.

    FIND FIRST ttPreco WHERE
               ttPreco.tipo = pTipo NO-ERROR.
    IF AVAIL ttPreco THEN DO:
       ASSIGN pvlPrecoPrazoReal  = ttPreco.vlPrecoPrazo
              pvlPrecoPrazoDolar = ttPreco.vlPrecoPrazoDolar
              pvControlePreco = ttPreco.codControlePreco.

       RUN setMsg IN hBoMsgPreco(4,"Tipo de Pre‡o:" + pTipo + " - preco com prazo para item:" + ttPreco.itCodigo  + " - ref:" + ttPreco.codRefer + 
                                 " - vl.real:" + STRING(ttPreco.vlPrecoPrazo) + " - vl.dolar" + 
                                STRING(ttPreco.vlPrecoPrazoDolar) ,'aviso').
    END.
    ELSE DO:
       RUN setMsg IN hBoMsgPreco(4, "Tipo de Pre‡o:" + pTipo + " - ttPreco nao encontrada",'erro').
    END.
END PROCEDURE.

PROCEDURE inserirTTPreco:
    DEFINE INPUT  PARAMETER pITem               AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pRef                AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pNrContainer        AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pvlPrecoREal        AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pvlPrecoDolar       AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pTipo               AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodControlePreco   AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pTbPreco            AS INTEGER     NO-UNDO.

    CREATE ttPreco.
    ASSIGN ttPreco.itCodigo         = pItem
           ttPreco.codRefer         = pRef
           ttPreco.nrContainer      = pNrContainer 
           ttPreco.vlPrecoBase      = pVlPrecoReal
           ttPreco.vlPrecoBaseDolar = pVlPrecoDolar
           ttPreco.tipo             = pTipo
           ttPreco.codControlePreco = pCodControlePreco
           ttPreco.tbPrecoId        = pTbPreco .

END PROCEDURE.



PROCEDURE getTbPrecoPadrao:
    DEFINE OUTPUT PARAMETER cTabela AS CHARACTER   NO-UNDO.
    CASE i-ep-codigo-usuario:
        WHEN '1' THEN DO: //ima
            FIND mgcad.unid-feder WHERE 
            unid-feder.estado = cUF NO-LOCK NO-ERROR.
            IF unid-feder.char-2 = 'SUL' OR
           (unid-feder.char-2 = 'SUDESTE' AND unid-feder.estado <> "ES") THEN
               ASSIGN cTabela = "TABELA_IMA_12". 
            ELSE
               ASSIGN cTabela = "TABELA_IMA_07". 
        END.
        WHEN '5' THEN DO: //med
           ASSIGN cTabela = "TABELA_MED".
        END.
    END CASE.
    FIND im-param NO-LOCK
        WHERE im-param.cod-param = cTabela
        NO-ERROR.
    IF AVAIL im-param THEN
       ASSIGN cTabela = im-param.val-param .
END PROCEDURE.


PROCEDURE getTTIndPrazo:
    DEFINE VARIABLE  iCont AS INTEGER     NO-UNDO.
    IF lIndPrazo = NO THEN DO:
       EMPTY TEMP-TABLE ttIndPrazo.
       FIND FIRST tab-finan WHERE
                  tab-finan.dt-ini-val <= TODAY AND 
                  tab-finan.dt-fim-val >= TODAY NO-LOCK NO-ERROR.
       IF AVAIL tab-finan THEN DO:
           REPEAT iCont = 1 TO  4:
               CREATE ttIndPrazo.
               ASSIGN ttIndPrazo.qtDias = tab-finan.tab-dia-fin[iCont]
                      ttIndPrazo.indice = tab-finan.tab-ind-fin[iCont]
                      lIndPrazo = TRUE.

           END.                                        
       END.     
    END.
                                                  

END PROCEDURE.

PROCEDURE expttMsg:
    DEFINE INPUT  PARAMETER cArquivo AS CHARACTER   NO-UNDO.
    RUN expttMsg IN hBoMsgPreco(cArquivo).


END PROCEDURE.

PROCEDURE getTtMsg:
    DEFINE INPUT  PARAMETER pTipo AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttMsg.

    RUN getTTMsg IN hBoMsgPreco(pTipo, OUTPUT TABLE ttMsg ).


END PROCEDURE.


PROCEDURE getQtMaxPrecoPorId:
    DEFINE INPUT  PARAMETER pId AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER dQt AS DECIMAL     NO-UNDO.
    RUN getQtMaxPrecoPorId IN hBoControlePreco(pId,OUTPUT dQt).


END PROCEDURE.

PROCEDURE getPercClasseComis:
    
    DEFINE INPUT  PARAMETER pId AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER percComis AS DECIMAL     NO-UNDO.

    RUN getPercClasseComis IN hBoControlePreco(pId,OUTPUT percComis).
    

END PROCEDURE.

PROCEDURE getIdClasseComis:
    
    DEFINE INPUT  PARAMETER pId AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER pIdClasse AS INTEGER     NO-UNDO.

    RUN getIdClasseComis IN hBoControlePreco(pId,OUTPUT pIdClasse).
    

END PROCEDURE.
              
