{esp/utilapi.i2}
{esapi/analisarJsonObject2.i}
//{esp/ttChave.i}


DEF TEMP-TABLE ttReservas
    FIELD cod-estabel  AS CHAR
    FIELD pedido-lisa  AS CHAR
    FIELD nr-pedcli    AS CHAR
    FIELD it-codigo    AS CHAR
    FIELD cod-refer    AS CHAR
    FIELD nr-container AS CHAR
    FIELD num-rolo-imp AS CHAR
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD idTransacao  AS INT64
    .

DEF INPUT  PARAMETER TABLE FOR ttJson.
DEF OUTPUT PARAMETER TABLE FOR ttChave.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR c-nr-pedcli         AS CHAR.
DEF VAR c-pre-pedido        AS CHAR.
DEF VAR c-chave             AS CHAR.
DEF VAR c-msg               AS CHAR.
DEF VAR c-it-codigo         AS CHAR.
DEF VAR c-arq-retorno       AS CHAR.
DEF VAR l-erro              AS LOG.

DEFINE VARIABLE idTransacao AS INTEGER     NO-UNDO.

ASSIGN idTransacao      = integer(getChaveTTJson('','id_transacao')).
ASSIGN c-nr-pedcli      = getChaveTTJson('payload','pedidoCliente').
ASSIGN c-arq-retorno    = getChaveTTJson('arquivo','separacao').
ASSIGN c-pre-pedido     = getChaveTTJson('payload','prePedido').


PROCEDURE criarReservasEtqPai:

    FOR EACH ttJson
        WHERE ttJson.tag_pai    = 'origem'
        AND   ttJson.tag        = 'json'
    END.

END PROCEDURE.

FOR EACH ttJson NO-LOCK USE-INDEX pri.

    IF ttJson.tag_pai = 'Separacao' THEN DO.
       CASE ttJson.tag:
          WHEN 'produto' THEN DO.
              CREATE ttReservas.
              ASSIGN ttReservas.cod-estabel     = '505'
                     ttReservas.nr-pedcli       = c-nr-pedcli
                     ttReservas.pedido-lisa     = c-pre-pedido
                     ttReservas.it-codigo       = ttJson.valor.   
          END.
          WHEN 'lote' THEN
              ASSIGN ttReservas.cod-refer       = ttJson.valor.
          WHEN 'rolo' THEN
              ASSIGN ttReservas.num-rolo-imp    = ttJson.valor.
          WHEN 'cntr' THEN
              ASSIGN ttReservas.nr-container    = ttJson.valor.
       END CASE.
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

RUN pi-ajusta-etiquetas.


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


FOR EACH ttReservas NO-LOCK.
    FIND ped-venda WHERE
         ped-venda.cod-estabe = ttReservas.cod-estabel AND
         ped-venda.nr-pedcli = ttReservas.nr-pedcli NO-LOCK NO-ERROR.
    ASSIGN c-chave = ped-venda.nr-pedcli + "|" + 
                     ped-venda.nome-abrev.

    FIND lisa-integra WHERE
         lisa-integra.cod-trans = 'ISF' AND    // Instruáao de Separaáao
         lisa-integra.chave = c-chave AND
         lisa-integra.acao = 'SEPARAR' SHARE-LOCK NO-ERROR.
    IF AVAIL lisa-integra THEN DO.
       ASSIGN lisa-integra.acao = 'RESERVAR'.
       IF lisa-integra.val-livre-1 = '' THEN
          ASSIGN lisa-integra.val-livre-1 = ttReservas.pedido-lisa.
    END.

    ASSIGN c-chave = ttReservas.nr-pedcli + "|" + 
                     ttReservas.it-codigo + "|" +
                     ttReservas.cod-refer.
    FIND lisa-integra WHERE
         lisa-integra.cod-trans = "RetornoISF" AND
         lisa-integra.chave = c-chave AND 
         lisa-integra.conteudo = STRING(ttReservas.num-etiqueta) SHARE-LOCK NO-ERROR.
    
    IF NOT AVAIL lisa-integra THEN DO.
       CREATE lisa-integra.
       ASSIGN lisa-integra.cod-trans = "RetornoISF"
              lisa-integra.chave = c-chave
              lisa-integra.conteudo = STRING(ttReservas.num-etiqueta)
              lisa-integra.acao = 'Reservar'
              lisa-integra.ind-situacao = 1
              lisa-integra.dt-trans = TODAY
              lisa-integra.hr-trans = TIME.

       IF lisa-integra.val-livre-1 = '' THEN
          ASSIGN lisa-integra.val-livre-1 = ttReservas.pedido-lisa.

    END.
END.

PROCEDURE pi-ajusta-etiquetas.
    // ajusta as Etiquetas
    FOR EACH lisa-integra WHERE
             lisa-integra.cod-trans = "ConfEtiquetas" AND
             lisa-integra.ind-situacao = 1 // Aguardando Integraáao
             SHARE-LOCK.

        IF lisa-integra.chave <> '0' THEN DO.
           FIND ob-etiqueta WHERE
                ob-etiqueta.cod-estabel = '505' AND
                ob-etiqueta.num-etiqueta = INTEGER(lisa-integra.chave)
                SHARE-LOCK NO-ERROR.

           IF lisa-integra.acao = 'ALTERAR' THEN 
              ASSIGN ob-etiqueta.quantidade = DECIMAL(lisa-integra.conteudo).

           IF ob-etiqueta.quantidade = 0 THEN
              ASSIGN ob-etiqueta.situacao = 7.
        END.
        ELSE DO.
            ASSIGN c-it-codigo = IF lisa-integra.val-livre-2 <> ''
                                 THEN lisa-integra.val-livre-2 
                                 ELSE SUBSTR(lisa-integra.val-livre-4,4).

            FIND ob-etiqueta WHERE
                 ob-etiqueta.cod-estab    = '505' AND 
                 ob-etiqueta.nr-container = INT(lisa-integra.val-livre-5) AND
                 ob-etiqueta.it-codigo    = c-it-codigo                   AND
                 ob-etiqueta.cod-refer    = lisa-integra.val-livre-3      AND
                 ob-etiqueta.num-rolo-imp  = integer(lisa-integra.val-livre-1)
                 NO-LOCK NO-ERROR.
            IF NOT AVAIL ob-etiqueta THEN DO.
               RUN pi-cria-etiqueta.
               IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
            END.
        END.

        ASSIGN lisa-integra.ind-situacao = 2.
    END.
END PROCEDURE.


PROCEDURE pi-cria-etiqueta.
    FIND ITEM WHERE
         ITEM.it-codigo = c-it-codigo NO-LOCK NO-ERROR.

    CREATE ob-etiqueta.
    ASSIGN ob-etiqueta.cod-estabel     = '505'
           ob-etiqueta.dt-emissao      = TODAY
           ob-etiqueta.hr-emissao      = STRING(TIME,"HH:MM")
           ob-etiqueta.acondic         = ""
           ob-etiqueta.it-codigo       = c-it-codigo
           ob-etiqueta.num-rolo-imp    = INTEGER(lisa-integra.val-livre-1)
           ob-etiqueta.cod-refer       = lisa-integra.val-livre-3
           ob-etiqueta.nr-container    = INT(lisa-integra.val-livre-5)
           ob-etiqueta.quantidade      = DECIMAL(lisa-integra.conteudo)
           ob-etiqueta.nr-lote         = 'CA'
           ob-etiqueta.cod-qualid      = 'D' 
           ob-etiqueta.corte-comerc    = ''
           ob-etiqueta.localizacao     = ''
           ob-etiqueta.situacao        = 3
           ob-etiqueta.cod-depos       = 'ITA'
           ob-etiqueta.ob-origem       = ''.

     ASSIGN ob-etiqueta.num-etiqueta = NEXT-VALUE(seq-etq-estoq-itj).


END PROCEDURE.


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

