DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER cErro  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-chave     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-pedido    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-acomp     AS HANDLE      NO-UNDO.
DEFINE VARIABLE l-erro-res  AS LOGICAL     NO-UNDO.
DEFINE BUFFER b-lisa-integra FOR lisa-integra.


DEF TEMP-TABLE tt-ped-venda 
    FIELD row-ped-venda AS ROWID.

DEF TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item
     FIELD r-rowid AS ROWID.


{utp/ut-glob.i}
RUN utp/ut-acomp PERSIST SET h-acomp.
{utp/ut-liter.i Retorno ISF *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FIND FIRST para-ped NO-LOCK NO-ERROR.

FIND lisa-integra NO-LOCK
    WHERE ROWID(lisa-integra) = pRowid NO-ERROR.

ASSIGN c-pedido = ENTRY(1,lisa-integra.chave,"|").

RUN pi-acompanhar IN h-acomp (INPUT "Processando Retorno ISF " + lisa-integra.chave ).

FIND ped-venda WHERE
    ped-venda.nr-pedcli = ENTRY(1,lisa-integra.chave,"|") AND
    ped-venda.nome-abrev = ENTRY(2,lisa-integra.chave,"|")
    NO-LOCK NO-ERROR.
IF NOT AVAIL ped-venda THEN DO:
   ASSIGN cErro = "Pedido NÆo Existe:" + ped-venda.nr-pedcli.
   RUN pi-finalizar IN h-acomp.
   RETURN 'adm-error'.
END.

FIND ped-venda-ext WHERE
    ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
    ped-venda-ext.nr-pedido   = ped-venda.nr-pedido SHARE-LOCK NO-ERROR.

IF NOT AVAIL ped-venda-ext THEN DO.
  ASSIGN cErro = ped-venda.nr-pedcli + " sem tabela de extensÆo cadastrada".
  RETURN 'adm-error'.
END.

IF ped-venda-ext.nr-pedext = '' THEN
  ASSIGN ped-venda-ext.nr-pedext = lisa-integra.val-livre-1.


ASSIGN ped-venda-ext.qt-fardos = 0.

EMPTY TEMP-TABLE tt-ped-venda.



FOR EACH b-lisa-integra WHERE
        b-lisa-integra.cod-trans = "RetornoISF" AND
        b-lisa-integra.chave BEGINS c-pedido
        SHARE-LOCK.

   RUN pi-acompanhar IN h-acomp (INPUT "Processando Retorno ISF " + b-lisa-integra.chave ).

   FIND ped-item OF ped-venda WHERE
        ped-item.cod-sit-item = 1 AND
        ped-item.it-codigo = ENTRY(2,b-lisa-integra.chave,"|") AND
        ped-item.cod-refer = ENTRY(3,b-lisa-integra.chave,"|") 
        NO-LOCK NO-ERROR.

   /* comentado pois j  foi tratado antes de entrar no foreach
   FIND ped-venda-ext WHERE
        ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
        ped-venda-ext.nr-pedido = ped-venda.nr-pedido
        SHARE-LOCK NO-ERROR.

   IF NOT AVAIL ped-venda-ext THEN DO.
      MESSAGE ped-venda.nr-pedcli SKIP ' 2-pedido com erro'
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      NEXT.
   END.

   IF ped-venda-ext.nr-pedext = '' THEN
      ASSIGN ped-venda-ext.nr-pedext = b-lisa-integra.val-livre-1.*/

   FIND ob-etiqueta WHERE
        ob-etiqueta.cod-estabel = ped-venda.cod-estabel AND
        ob-etiqueta.num-etiqueta = INTEGER(b-lisa-integra.conteudo)
        NO-LOCK NO-ERROR.

   IF AVAIL ob-etiqueta AND ob-etiqueta.situacao = 3 THEN DO.
      RUN esapi/cria-reserva.p (INPUT ROWID(ped-item),
                                INPUT ROWID(ob-etiqueta)).

      IF RETURN-VALUE = 'ADM-OK' THEN DO.
         CREATE tt-ped-venda.
         ASSIGN tt-ped-venda.row-ped-venda = ROWID(ped-venda).
      END.
   END.
END.




// Ajusta o Pedido
ASSIGN l-erro-res = NO.
FOR EACH tt-ped-venda NO-LOCK.
   ASSIGN c-chave = ped-venda.nr-pedcli + "|" + 
                    ped-venda.nome-abrev.

   RUN pi-acompanhar IN h-acomp (INPUT "Ajustando Pedido de Venda " + c-chave ).

   RUN pi-ajusta-pedido (INPUT tt-ped-venda.row-ped-venda).
   IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
      ASSIGN l-erro-res = YES.
      NEXT.
   END.
END.

IF l-erro-res = NO THEN DO.
  FIND current lisa-integra EXCLUSIVE-LOCK.
  ASSIGN lisa-integra.acao = 'FATURAR'.
  RELEASE lisa-integra.

  FOR EACH b-lisa-integra WHERE
           b-lisa-integra.cod-trans = "RetornoISF" AND
           b-lisa-integra.chave BEGINS c-pedido SHARE-LOCK.
      ASSIGN b-lisa-integra.acao = ''
             b-lisa-integra.ind-situacao = 2.  // Efetuado
  END.
END.

RUN pi-finalizar IN h-acomp.

PROCEDURE pi-ajusta-pedido.

   DEF INPUT PARAMETER p-row-ped-venda AS ROWID.
   DEF VAR c-desc-reserva AS CHAR.

   FIND ped-venda WHERE
        ROWID(ped-venda) = p-row-ped-venda NO-LOCK NO-ERROR.

   ASSIGN l-erro-res = NO.
   FOR EACH ped-item-res WHERE
            ped-item-res.cod-estabel    = ped-venda.cod-estabel AND
            ped-item-res.nome-abrev     = ped-venda.nome-abrev  AND
            ped-item-res.nr-pedcli      = ped-venda.nr-pedcli   AND
            ped-item-res.qt-pedida      > 0 NO-LOCK.    

       FIND ped-item OF ped-venda WHERE
            ped-item.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK NO-ERROR.

       ASSIGN c-desc-reserva = "Seq. " + TRIM(STRING(ped-item.nr-sequencia,">>>9")) + " RESERVADA" .

       // Qtde Reservada está diferente do Item do Pedido
       IF ped-item.qt-pedida <> ped-item-res.qt-pedida THEN DO.
          EMPTY TEMP-TABLE tt-ped-item.
          CREATE tt-ped-item.
          BUFFER-COPY ped-item TO tt-ped-item
                      ASSIGN tt-ped-item.qt-pedida = ped-item-res.qt-pedida.
           
          RUN esapi/altera-peditem-v2.p (INPUT TABLE tt-ped-item).
          IF RETURN-VALUE = 'NOK' THEN DO.
             ASSIGN l-erro-res = YES.
             UNDO,NEXT.              
          END.

          ASSIGN c-desc-reserva = c-desc-reserva + " e Ajustada a Quantidade" + 
                          "   De: " + TRIM(STRING(ped-item.qt-pedida,">>>,>>9.99")) +                   
                          " Para: " + TRIM(STRING(ped-item-res.qt-pedida,">>>,>>9.99")).

          RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                         INPUT ped-venda.nome-abrev,
                                         INPUT c-desc-reserva,
                                         INPUT YES).
       END.
    END.
    IF l-erro-res THEN DO: 
       RUN pi-finalizar IN h-acomp.
       RETURN 'ADM-ERROR'.
    END.

    RUN esapi/completa-pedvenda.p (INPUT ped-venda.nr-pedcli).
    RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                   INPUT ped-venda.nome-abrev,
                                   INPUT "SEPARA€ÇO Completa, disponivel para Faturamento", 
                                   INPUT YES).

    

END PROCEDURE.
