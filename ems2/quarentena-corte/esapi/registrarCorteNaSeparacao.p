/**************************************************************************
Programa: esapi/registrarCorteNaSeparacao.p
Autor: Tadeu Silva Parreiras
Objetivo: A partir do arquivo json enviado pela LISA referente a separaá∆o
do pedido de venda, fazer a operaá∆o de criaá∆o e ajuste de peáas que 
representam cortes feitos e alocados para o pedido.
Logica:
1- Verifica se existe alguma etiqueta da separaá∆o que tenha uma origem 
de outra etiqueta por ser corte.
2- se sim verifica se a etiqueta j† existe
3- sen∆o existir cria a etiqueta e ajusta a etiqueta origem para a quantidade
que consta na separaá∆o, condicionado a esta quantidade ser menor que 
a quantidade atual.


Data: 09/2025

*****************************************************************************/

{esp/using_json.i}
{esp/utiljson.i}
{esapi/analisarJsonObject2.i}
{esp/util.i}
{esapi/retorno-isf-lisa.i}


DEFINE INPUT PARAMETER TABLE FOR ttJson.
DEFINE OUTPUT PARAMETER cErro   AS CHAR.



DEFINE VARIABLE oJson           AS jsonObject   NO-UNDO.
DEFINE VARIABLE hBoes049        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoTrans        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoMsg          AS HANDLE      NO-UNDO.
DEFINE VARIABLE iCont           AS INTEGER     NO-UNDO.
DEFINE VARIABLE x-arr-log-id    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE idTransacao     AS INTEGER     NO-UNDO.
//DEFINE VARIABLE cErro           AS CHARACTER   NO-UNDO.

/*RUN convFileJson2JsonObject('c:\temp\corte.json',OUTPUT oJson).
RUN esapi/analisarJsonobject3.p(oJson, OUTPUT TABLE ttJson).     */

RUN esbo/boMsg.p        PERSIST SET hBoMsg.
RUN esbo/boTransacoes.p PERSIST SET hBoTrans.  

//{esp/exportarTabelaCsv3.i ttJson " " " " "ttjson"}



FIND ttjson
WHERE ttJson.tag_pai = 'headers' 
AND   ttJson.tag     = "x-arr-log-id"  no-error.
IF AVAIL ttjson THEN
   ASSIGN x-arr-log-id = ttJson.valor.

RUN setMsg IN hBoMsg(999,'x-arr-log-id:' + x-arr-log-id,'log').
 FOR EACH ttJson NO-LOCK 
         USE-INDEX pri.
    IF ttJson.tag_pai = 'payload' AND 
       ttJson.tag     = "pedidoCliente" THEN
       ASSIGN c-nr-pedcli = ttJson.valor.

    IF ttJson.tag_pai = 'arquivo' AND 
       ttJson.tag     = "separacao" THEN
       ASSIGN c-arq-retorno = ttJson.valor.

    IF ttJson.tag_pai = 'payload' AND 
       ttJson.tag     = "prePedido" THEN
       ASSIGN c-pre-pedido = ttJson.valor.    
       
       

    IF ttJson.tag_pai = 'Separacao'  AND ttjson.tag = '' AND ttjson.valor = 'json' THEN DO.
        CREATE ttReservas.
        ASSIGN ttReservas.cod-estabel = '505'
               ttReservas.nr-pedcli = c-nr-pedcli
               ttReservas.pedido-lisa = c-pre-pedido.
    END.
    IF ttJson.tag_pai = 'Separacao' THEN  DO:   
       CASE ttJson.tag:
           WHEN 'produto' THEN DO.
              ASSIGN  ttReservas.it-codigo = ttJson.valor.   
           END.
           WHEN 'lote' THEN
               ASSIGN ttReservas.cod-refer = ttJson.valor.
           WHEN 'rolo' THEN
               ASSIGN ttReservas.num-rolo-imp = ttJson.valor.
           WHEN 'cntr' THEN
               ASSIGN ttReservas.nr-container = ttJson.valor.
           WHEN 'id' THEN
               ASSIGN ttReservas.idEtqLisa   = ttJson.valor.
           WHEN 'quantidade' THEN
              ASSIGN ttReservas.quantidade  = DECIMAL(replace(ttjson.valor,'.',',')).
           WHEN 'origem' THEN DO:
               IF ttjson.valor = 'jsonarray'  THEN
                  ASSIGN ttReservas.agrupOrigem = ttJson.id + 1 .
           END.    
       END CASE.       
    END.    
END.

RUN iniciarTransacao IN hBoTrans.
RUN gerarTransacao   IN hBoTrans(INPUT 'registrarCorteNaSeparacao',
                                 INPUT c-seg-usuario,
                                 INPUT 200,
                                 INPUT c-nr-pedcli,
                                 OUTPUT idTransacao                                    
                                ).

 RUN setMsg IN hBoMsg(0,'Inicio leitura de dados do Json','log').
FOR EACH ttReservas:
    FOR EACH ttJson
        WHERE ttJson.agrupJson = ttReservas.agrupOrigem:
        CASE ttJson.tag:
            WHEN 'id' THEN
            DO:
               ASSIGN ttReservas.numEtqOrigemCorte =  ttjson.valor.                
            END.
            WHEN 'quantidade' THEN
            DO:
             // ASSIGN ttReservas.qtAtuEtqCortada =  dec(ttjson.valor).  
             ASSIGN ttReservas.qtAtuEtqCortada =  dec(replace(ttjson.valor,'.',',')) .  
              /*MESSAGE 'qt.json:' ttjson.valor SKIP
                     'qt.conv:' ttReservas.qtAtuEtqCortada SKIP
                  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
            END.
        END CASE.
    END.
    ASSIGN iCont = iCont + 1 .
    
    RUN setMsg IN hBoMsg(iCont,'Registro:'                + STRING(iCont),'log').
    RUN setMsg IN hBoMsg(iCont,'Produto:'                  + ttReservas.it-codigo,'log' ).
    RUN setMsg IN hBoMsg(iCont,'referencia:'              + ttReservas.cod-refer,'log' ).
    RUN setMsg IN hBoMsg(iCont,'rolo:'                    + string(ttReservas.num-rolo-imp),'log' ).
    RUN setMsg IN hBoMsg(iCont,'container:'               + string(ttReservas.nr-container),'log' ).
    RUN setMsg IN hBoMsg(iCont,'id_etq_lisa:'             + ttReservas.idEtqLisa,'log').
    RUN setMsg IN hBoMsg(iCont,'quantidade:'              + string(ttReservas.quantidade),'log').
    RUN setMsg IN hBoMsg(iCont,'agrup.origem:'            + string(ttReservas.agrupOrigem),'log').
    RUN setMsg IN hBoMsg(iCont,'id_etq_lisa_origem:'      + string(ttReservas.numEtqOrigemCorte),'log').
    RUN setMsg IN hBoMsg(iCont,'quantidade:'              + string(ttReservas.qtAtuEtqCortada),'log').
    
   
    IF ttReservas.numEtqOrigemCorte  <> '' THEN DO: 
       RUN setMsg IN hBoMsg(100,'Chamada de corte, pois peáa possui a etiqueta lisa de origem:' + ttReservas.numEtqOrigemCorte ,'log').
       RUN esapi/cortarEtqLisa.p(
            INPUT idTransacao,
            INPUT c-nr-pedcli,
            INPUT ttReservas.numEtqOrigemCorte,
            INPUT ttReservas.idEtqLisa,
            INPUT ttReservas.qtAtuEtqCortada,
            INPUT ttReservas.quantidade,
            INPUT ttReservas.num-rolo-imp ,
            INPUT c-pre-pedido,
            INPUT  hBoMsg,
            OUTPUT cErro).      
        IF cErro <> '' THEN
        DO:
            RUN setMsg IN hBoMsg(2,cErro,'erro'). 
            RUN finalizarTransacao     IN hBoTrans(2).
/*             MESSAGE 'erro:' cErro                         */
/*                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */
            
        END.
        ELSE DO:
            RUN setMsg IN hBoMsg(3,'Corte sem Erro','log'). 
            RUN finalizarTransacao     IN hBoTrans(1).
        END.
                                    
    END.
END.
 RUN setMsg IN hBoMsg(0,'Fim leitura de dados do Json','log').

CATCH erroAplicacao AS Progress.Lang.AppError:
    RUN setMsg IN hBoMsg(999,erroAplicacao:GetMessage(1),'erro'). 
    RUN finalizarTransacao     IN hBoTrans(2).
END CATCH.

CATCH erroSistema   AS Progress.Lang.SysError:
    RUN setMsg IN hBoMsg(998,erroSistema:GetMessage(1),'erro'). 
    RUN finalizarTransacao     IN hBoTrans(2). 
END CATCH.

FINALLY: 

    
    IF VALID-HANDLE(hBoMsg) THEN
    DO:
        RUN getErros               IN  hBoMsg(OUTPUT cErro).
        RUN setTransacaoLogCalculo IN hBoMsg(idTransacao).
        RUN gravarLogCalculo       IN hBoMsg(200). //200 Ç o codigo do calculo para corte na separacao        
        DELETE OBJECT hBoMsg.
    END.     
    
    IF VALID-HANDLE(hBoTrans) THEN
    DO:       
        DELETE OBJECT hBoTrans.
    END.
   

END FINALLY.




