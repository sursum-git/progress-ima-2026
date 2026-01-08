{esp/utilapi.i2}
{esp/util.i}
{esp/params.i}
{lisa/codProdUnif.i}
{esapi/analisarJsonObject2.i}
//{esp/ttChave.i}


{esapi/retorno-isf-lisa.i}

DEF INPUT  PARAMETER TABLE FOR ttJson.
DEF OUTPUT PARAMETER TABLE FOR ttChave.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEFINE VARIABLE cErro AS CHARACTER   NO-UNDO.


RUN esapi/registrarCorteNaSeparacao.p(INPUT TABLE ttJson,OUTPUT cErro).


/*IF cErro <> '' THEN
DO:
    CREATE ttChave.
    ASSIGN ttChave.chave    = 'erro-corte'
           ttChave.valor    = cErro.
           
    RETURN ERROR.       
END.*/



FOR EACH ttJson NO-LOCK 
         USE-INDEX pri.
    IF ttJson.tag_pai = 'payload' AND 
       ttJson.tag = "pedidoCliente" THEN
       ASSIGN c-nr-pedcli = ttJson.valor.

    IF ttJson.tag_pai = 'arquivo' AND 
       ttJson.tag = "separacao" THEN
       ASSIGN c-arq-retorno = ttJson.valor.

    IF ttJson.tag_pai = 'payload' AND 
       ttJson.tag = "prePedido" THEN
       ASSIGN c-pre-pedido = ttJson.valor.

    IF ttJson.tag_pai = 'Separacao' THEN DO.

       CASE ttJson.tag:
          WHEN 'produto' THEN DO.
              CREATE ttReservas.
              ASSIGN ttReservas.cod-estabel = '505'
                     ttReservas.nr-pedcli = c-nr-pedcli
                     ttReservas.pedido-lisa = c-pre-pedido
                     ttReservas.it-codigo = ttJson.valor.   
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
              ASSIGN ttReservas.quantidade  = DECIMAL(ttJson.valor).
       END CASE.
    END.
END.

IF lCodigoProdUnificado THEN DO:
   FOR EACH ttReservas:
       ASSIGN ttReservas.cod-refer = ENTRY(2,ttreservas.it-codigo,"-")
              ttReservas.it-codigo = ENTRY(1,ttreservas.it-codigo,"-") 
               
              .   
   END.
    
END.


// Valida Arquivo Recebido
ASSIGN l-erro = NO.
IF c-arq-retorno = '' THEN DO. 
   ASSIGN c-msg = 'Arquivo de N«O foi Informado -' + 
                  ' Pedido: ' +  c-nr-pedcli +
                  ' Arquivo: ' + c-arq-retorno.

   RUN pi-cria-log (INPUT c-pre-pedido,
                    INPUT c-msg,
                    INPUT YES).
END.
IF l-erro THEN RETURN 'NOK'.

ASSIGN c-msg = 'Arquivo de Separaáao Recebido -' + 
               ' Pedido: ' +  c-nr-pedcli +
               ' Arquivo: ' + c-arq-retorno.

RUN pi-cria-log (INPUT c-pre-pedido,
                 INPUT c-msg,
                 INPUT NO).

// Validar se o Pedido de venda existe
IF c-nr-pedcli = '' THEN DO.
   ASSIGN c-msg = 'Pedido de Venda n∆o foi Informado  -' + 
                  ' Pedido: ' +  c-nr-pedcli +
                  ' Arquivo: ' + c-arq-retorno.

   RUN pi-cria-log (INPUT c-pre-pedido,
                    INPUT c-msg,
                    INPUT YES).
END.
IF l-erro THEN RETURN 'NOK'.

IF c-pre-pedido = '' THEN DO.
   ASSIGN c-msg = 'PrÇ-Pedido n∆o foi Informado  -' + 
                  ' Pedido: ' +  c-nr-pedcli +
                  ' Arquivo: ' + c-arq-retorno.

   RUN pi-cria-log (INPUT c-pre-pedido,
                    INPUT c-msg,
                    INPUT YES).
END.
IF l-erro THEN RETURN 'NOK'.

// Executa a aá∆o de Ajuste e Corte das Peáas
ASSIGN c-msg = 'Executado Ajuste e Corte das Peáas -' + 
              ' Pedido: ' +  c-nr-pedcli +
              ' Arquivo: ' + c-arq-retorno.

RUN pi-cria-log (INPUT c-pre-pedido,
                INPUT c-msg,
                INPUT NO).

ASSIGN c-msg = ''.
//RUN pi-ajusta-etiquetas.
IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
   RUN pi-cria-log (INPUT c-pre-pedido,
                    INPUT c-msg,
                    INPUT YES).
   RETURN 'NOK'.
END.


// Inicio das Validaáoes
ASSIGN c-msg = 'Inicar Validaáao do Pedido -' + 
              ' Pedido: ' +  c-nr-pedcli +
              ' Arquivo: ' + c-arq-retorno .

RUN pi-cria-log (INPUT c-pre-pedido,
                INPUT c-msg,
                INPUT NO).


// validar se o Pedido de vebda existe e se teve itens cancelados
FIND FIRST ttReservas NO-LOCK NO-ERROR.
IF NOT AVAIL ttReservas THEN DO.
   ASSIGN c-msg = 'N∆o foi encontrado Peáas para Reservar -' + 
                 ' Pedido: ' +  c-nr-pedcli +
                 ' Arquivo: ' + c-arq-retorno .

   RUN pi-cria-log (INPUT c-pre-pedido,
                   INPUT c-msg,
                   INPUT YES).
END.
IF l-erro THEN RETURN 'NOK'.

ASSIGN l-erro = NO.
FOR EACH ttReservas BREAK BY ttReservas.nr-pedcli.
    IF FIRST-OF(ttReservas.nr-pedcli) THEN DO.
       FIND ped-venda WHERE
            ped-venda.cod-estabe = ttReservas.cod-estabel AND
            ped-venda.nr-pedcli = ttReservas.nr-pedcli NO-LOCK NO-ERROR.
       IF NOT AVAIL ped-venda THEN DO.
          ASSIGN c-msg = 'Pedido de Venda n∆o Pertence a MEDTEXTIL - ' + 
                         'Pedido: ' +  ttReservas.nr-pedcli.

          RUN pi-cria-log (INPUT ttReservas.pedido-lisa,
                           INPUT c-msg,
                           INPUT YES).
          NEXT.
       END.

       IF ped-venda.cod-sit-ped <> 1 THEN DO.
          ASSIGN c-msg = 'Pedido de Venda j† foi Atendido ou Cancelado - ' + 
                         'Pedido: ' +  ttReservas.nr-pedcli.

          RUN pi-cria-log (INPUT ttReservas.pedido-lisa,
                           INPUT c-msg,
                           INPUT YES).
          NEXT.
       END.

       FIND ped-item OF ped-venda WHERE
            ped-item.it-codigo = ttReservas.it-codigo AND
            ped-item.cod-refer = ttReservas.cod-refer AND
            ped-item.cod-sit-item = 1 NO-LOCK NO-ERROR.
       IF NOT AVAIL ped-item THEN DO.
           ASSIGN c-msg = 'Item do Pedido de Venda j† foi Atendido ou Cancelado -' + 
                          ' Pedido: ' +  ttReservas.nr-pedcli + 
                          ' Item: ' +  ttReservas.it-codigo + 
                          ' Referància: ' +  ttReservas.cod-refer.

           RUN pi-cria-log (INPUT ttReservas.pedido-lisa,
                            INPUT c-msg,
                            INPUT YES).
           NEXT.
       END.

       IF ttReservas.nr-container = '' THEN DO.
          ASSIGN c-msg = 'Separaáao sem Container Informado - ' + 
                         'Pedido: ' +  ttReservas.nr-pedcli.

          RUN pi-cria-log (INPUT ttReservas.pedido-lisa,
                           INPUT c-msg,
                           INPUT YES).
          NEXT.
       END.
    END.
END.

IF l-erro THEN RETURN 'NOK'.

ASSIGN c-msg = 'Inicar Validaáao das Etiquetas -' + 
              ' Pedido: ' +  c-nr-pedcli +
              ' Arquivo: ' + c-arq-retorno .

RUN pi-cria-log (INPUT c-pre-pedido,
                INPUT c-msg,
                INPUT NO).


// Validando Etiquetas Reservadas
FOR EACH ttReservas.
    ASSIGN lAchouEtq = NO.
    FIND etiqueta_lisa NO-LOCK
        WHERE etiqueta_lisa.id_etq_lisa = ttReservas.idEtqLisa NO-ERROR.
    IF AVAIL etiqueta_lisa THEN DO:
       FIND ob-etiqueta NO-LOCK
           WHERE ob-etiqueta.cod-estabel  = etiqueta_lisa.cod_estabel
           AND   ob-etiqueta.num-etiqueta = etiqueta_lisa.num_etiqueta
           AND   ob-etiqueta.situacao     = 3
           NO-ERROR.
       ASSIGN lAchouEtq = AVAIL ob-etiqueta.
    END.
    IF NOT lAchouEtq  THEN
       FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = '505' AND
         ob-etiqueta.nr-container = INTEGER(ttReservas.nr-container) AND
         ob-etiqueta.it-codigo = ttReservas.it-codigo AND
         ob-etiqueta.cod-refer = ttReservas.cod-refer AND
         ob-etiqueta.num-rolo-imp = INTEGER(ttReservas.num-rolo-imp) AND
         ob-etiqueta.situacao = 3 NO-LOCK NO-ERROR.

    IF NOT AVAIL ob-etiqueta THEN DO.
       ASSIGN c-msg = 'Etiqueta n∆o est† em Estoque -' + 
                      ' Container: ' +  ttReservas.nr-container + 
                      ' Item: ' +  ttReservas.it-codigo + 
                      ' Referància: ' +  ttReservas.cod-refer + 
                      ' Rolo: ' +  ttReservas.num-rolo-imp. 

       RUN pi-cria-log (INPUT ttReservas.pedido-lisa,
                        INPUT c-msg,
                        INPUT YES).
       NEXT.
    END.

    /*IF ob-etiqueta.situacao <> 3 THEN DO.
       ASSIGN c-msg = 'Etiqueta n∆o est† em Estoque -' +
                      ' Container: ' +  ttReservas.nr-container + 
                      ' Item: ' +  ttReservas.it-codigo + 
                      ' Referància: ' +  ttReservas.cod-refer + 
                      ' Rolo: ' +  ttReservas.num-rolo-imp. 

       RUN pi-cria-log (INPUT ttReservas.pedido-lisa,
                        INPUT c-msg,
                        INPUT YES).
       NEXT.
    END.*/

    ASSIGN ttReservas.cod-estabel = ob-etiqueta.cod-estabel
           ttReservas.num-etiqueta = ob-etiqueta.num-etiqueta.
END.

IF l-erro THEN RETURN 'NOK'.

ASSIGN c-msg = 'Dados Validados OK, Reservar -' + 
              ' Pedido: ' +  c-nr-pedcli .

RUN pi-cria-log (INPUT c-pre-pedido,
                INPUT c-msg,
                INPUT NO).

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY +  'retorno_isf_lisa_' + c-pre-pedido + "-" + STRING(TIME) + "txt").
FOR EACH ttReservas NO-LOCK.
    EXPORT DELIMITER ";" ttReservas.
    FIND ped-venda WHERE
         ped-venda.cod-estabe = ttReservas.cod-estabel AND
         ped-venda.nr-pedcli = ttReservas.nr-pedcli NO-LOCK NO-ERROR.
    ASSIGN c-chave = ped-venda.nr-pedcli + "|" + 
                     ped-venda.nome-abrev.
    PUT "chave:" c-chave SKIP.

    FIND lisa-integra WHERE
         lisa-integra.cod-trans = 'ISF' AND    // Instruáao de Separaáao
         lisa-integra.chave = c-chave AND
         lisa-integra.acao = 'SEPARAR' SHARE-LOCK NO-ERROR.
    IF AVAIL lisa-integra THEN DO.
       ASSIGN lisa-integra.acao = 'RESERVAR'.
       IF lisa-integra.val-livre-1 = '' THEN
          ASSIGN lisa-integra.val-livre-1 = ttReservas.pedido-lisa.

       PUT "ACHOU o registro de separaá∆o ISF e mudou a aá∆o para RESERVAR" SKIP.
    END.
    ELSE DO:
        PUT "N∆o achou o registro de separaá∆o" SKIP.
    END.
    ASSIGN c-chave = ttReservas.nr-pedcli + "|" + 
                     ttReservas.it-codigo + "|" +
                     ttReservas.cod-refer.
    FIND lisa-integra WHERE
         lisa-integra.cod-trans = "RetornoISF" AND
         lisa-integra.chave = c-chave AND 
         lisa-integra.conteudo = STRING(ttReservas.num-etiqueta) SHARE-LOCK NO-ERROR.
    
    IF NOT AVAIL lisa-integra THEN DO.
       PUT "N∆o achou o lisa integra e vai criar o registro RETORNOISF" SKIP.
       CREATE lisa-integra.
       ASSIGN lisa-integra.cod-trans    = "RetornoISF"
              lisa-integra.chave        = c-chave
              lisa-integra.conteudo     = STRING(ttReservas.num-etiqueta)
              lisa-integra.acao         = 'Reservar'
              lisa-integra.ind-situacao = 1
              lisa-integra.dt-trans     = TODAY
              lisa-integra.hr-trans     = TIME
              lisa-integra.val-livre-4  = ttReservas.idEtqLisa
              lisa-integra.val-livre-5  = string(ttReservas.quantidade)
              .

       IF lisa-integra.val-livre-1 = '' THEN
          ASSIGN lisa-integra.val-livre-1 = ttReservas.pedido-lisa.

    END.
    ELSE DO:
       PUT "ACHOU o lisa integra e n∆o vai criar" SKIP.
    END.

    PUT FILL("=",50) SKIP.
END.
OUTPUT CLOSE.






PROCEDURE pi-cria-log.
    DEF INPUT PARAMETER p-pedido AS CHAR.
    DEF INPUT PARAMETER p-msg AS CHAR.
    DEF INPUT PARAMETER p-log-erro AS LOG.

    CREATE lisa-log-integr.
    ASSIGN lisa-log-integr.cod-trans = 'RetornoISF'   
           lisa-log-integr.data = TODAY
           lisa-log-integr.hora = TIME
           lisa-log-integr.usuario = c-seg-usuario
           lisa-log-integr.chave = c-nr-pedcli
           lisa-log-integr.arq-retorno = c-arq-retorno
           lisa-log-integr.acao = 'RESERVAR' 
           lisa-log-integr.log-erro = p-log-erro
           lisa-log-integr.narrativa = 'PrePedido: ' + p-pedido + '-' + p-msg.

    ASSIGN l-erro = p-log-erro.
    PAUSE 1 NO-MESSAGE.
END.
