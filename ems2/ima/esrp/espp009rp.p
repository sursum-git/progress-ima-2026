DEF BUFFER moeda FOR mgcad.moeda.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEFINE SHARED VARIABLE var-container AS INTEGER     NO-UNDO.

DEF VAR c-arq-saida   AS CHARACTER   NO-UNDO.
DEF VAR l-ok          AS LOG.
DEF VAR l-erro        AS LOG.
DEF VAR i-nr-seq      AS INT.
DEF VAR h-prog        AS HANDLE.
DEF VAR c-arq-pedidos AS CHAR.
DEF VAR i-ct-ped      AS INT.
DEF VAR h-acomp AS HANDLE NO-UNDO.

DEF VAR de-vlReal          AS DECIMAL.
DEF VAR de-vlDolar         AS DECIMAL.
DEF VAR de-vlRealOut       AS DECIMAL.
DEF VAR de-vlDolarOut      AS DECIMAL.
DEF VAR i-ControlePreco    AS INTEGER.
DEF VAR i-controleprecoOut AS INTEGER.

DEF VAR i-prazo-medio      AS INT.
DEF VAR h-bo-preco-item    AS HANDLE    NO-UNDO.

IF VALID-HANDLE(h-bo-preco-item) THEN
   DELETE PROCEDURE h-bo-preco-item.

RUN esbo/boPrecosItemRef.p PERSISTENT SET h-bo-preco-item.
RUN iniciarBos      IN h-bo-preco-item.
RUN limparTTPreco   IN h-bo-preco-item.
RUN limparTTMsg     IN h-bo-preco-item.


RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FIND pp-container WHERE 
     pp-container.nr-container = var-container NO-ERROR.

IF NOT AVAIL pp-container THEN DO.
   MESSAGE 'Container n∆o cadastrado'
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

ASSIGN pp-container.situacao = 3
       pp-container.data-1 = TODAY.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.nr-container = pp-container.nr-container SHARE-LOCK.
    ASSIGN ob-etiqueta.situacao = 3.
END.


ASSIGN c-arq-saida = SESSION:TEMP-DIRECTORY + "container-" + string(var-container) + ".txt".
OUTPUT TO VALUE(c-arq-saida) NO-CONVERT .
   {esinc/i-espp005.i}
OUTPUT CLOSE.
OS-COMMAND NO-WAIT VALUE(c-arq-saida).


RUN pi-finalizar IN h-acomp.


//----------- Procedures -----------------
                                          
PROCEDURE pi-busca-preco :
    DEF INPUT  PARAMETER p-it-codigo AS CHAR.
    DEF INPUT  PARAMETER p-cod-refer AS CHAR.
    DEF INPUT  PARAMETER p-campanha  AS CHAR.
    DEF OUTPUT PARAMETER p-vlReal    AS DECIMAL NO-UNDO. 
    DEF OUTPUT PARAMETER p-vlDolar   AS DECIMAL NO-UNDO. 
    DEF OUTPUT PARAMETER p-ControlePreco AS CHAR.

    DEF VAR l-divide-comis AS LOGICAL.     
    DEF VAR de-perc-comis-vend AS DECIMAL.
    DEF VAR de-perc-comis-rep AS DECIMAL.
    
    DEF VAR i-tp-busca  AS INT.

    ASSIGN i-tp-busca = 1.  // PE
    IF p-campanha = '' AND  // N∆o Exste campnha para PI
       ped-venda-ext.tp-pedido = 'PI' AND
       ped-venda-ext.nr-container <> 0 THEN 
       ASSIGN i-tp-busca = 2.  // PI
    
    RUN setTbPreco      IN h-bo-preco-item (INPUT ped-venda-ext.tb_preco_id). 
    RUN setItem         IN h-bo-preco-item (INPUT p-it-codigo). 
    RUN setRef          IN h-bo-preco-item (INPUT p-cod-refer). 
    RUN setNrContainer  IN h-bo-preco-item (INPUT ped-venda-ext.nr-container).
    RUN setTipoBusca    IN h-bo-preco-item (INPUT i-tp-busca). 
    RUN setPrazoMedio   IN h-bo-preco-item (INPUT i-prazo-medio).
        
    RUN buscarPrecos    IN h-bo-preco-item.

    IF p-campanha <> '' THEN
       RUN getPrecoPrazo   IN h-bo-preco-item (INPUT p-campanha,
                                               OUTPUT p-vlReal,
                                               OUTPUT p-vlDolar,
                                               OUTPUT p-ControlePreco).
    ELSE
       RUN getPrecoPrazo IN h-bo-preco-item (INPUT ped-venda.tp-pedido,
                                             OUTPUT p-vlReal,
                                             OUTPUT p-vlDolar,
                                             OUTPUT p-ControlePreco).
END PROCEDURE.


PROCEDURE pi-prazo-medio.
    DEF VAR i-ct AS INT.
    DEF VAR de-tot-prazo LIKE cond-ped.nr-dias-venc.

    FIND cond-pagto WHERE
         cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag NO-LOCK NO-ERROR.
    IF AVAIL cond-pagto THEN 
       ASSIGN i-prazo-medio = cond-pagto.qtd-dias-prazo-medio.
    ELSE DO.
       ASSIGN de-tot-prazo = 0
              i-ct = 0.
       FOR EACH cond-ped WHERE
                cond-ped.nr-pedido = ped-venda.nr-pedido NO-LOCK.
           IF cond-ped.data-pagto <> ? THEN
              ASSIGN de-tot-prazo = de-tot-prazo + (cond-ped.data-pagto - ped-venda.dt-implant).
           ELSE
              ASSIGN de-tot-prazo = de-tot-prazo + cond-ped.nr-dias-venc.

           ASSIGN i-ct = i-ct + 1.
       END.
       ASSIGN i-prazo-medio = de-tot-prazo / i-ct.
    END.
END.


