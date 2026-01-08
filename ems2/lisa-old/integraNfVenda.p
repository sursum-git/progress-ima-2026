DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER cErro  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-chave             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-acomp             AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoParam            AS HANDLE      NO-UNDO.
DEFINE VARIABLE tpNfAprovPedLisa    AS CHARACTER   NO-UNDO.
DEFINE BUFFER b-lisa-integra FOR lisa-integra.
{utp/ut-glob.i}
RUN utp/ut-acomp PERSIST SET h-acomp.
{utp/ut-liter.i Integra‡Æo NF Venda *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).


FIND lisa-integra NO-LOCK
    WHERE ROWID(lisa-integra) = pRowid NO-ERROR.


RUN pi-acompanhar IN h-acomp (INPUT "Enviando Nota de Venda " + lisa-integra.chave ).

FIND nota-fiscal WHERE
    nota-fiscal.cod-estabel = ENTRY(1,lisa-integra.chave,"|") AND
    nota-fiscal.serie       = ENTRY(2,lisa-integra.chave,"|") AND
    nota-fiscal.nr-nota-fis = ENTRY(3,lisa-integra.chave,"|") 
    NO-LOCK NO-ERROR.
IF nota-fiscal.dt-cancela <> ? THEN DO.
  FIND CURRENT lisa-integra EXCLUSIVE-LOCK.
  DELETE lisa-integra.
  RELEASE lisa-integra.
  ASSIGN cErro = "Nota Fiscal j  cancelada".
  RUN pi-finalizar IN h-acomp.
  RETURN 'adm-error' .
END.

// Verifica se tem o PrePedido Gravado
FIND ped-venda WHERE
    ped-venda.nr-pedcli = nota-fiscal.nr-pedcli AND
    ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
    NO-LOCK NO-ERROR.

FIND ped-venda-ext WHERE
    ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND 
    ped-venda-ext.nr-pedido = ped-venda.nr-pedido
    SHARE-LOCK NO-ERROR.

IF ped-venda-ext.nr-pedext = '' THEN DO.
  ASSIGN c-chave = ped-venda.nr-pedcli + "|" + 
                   ped-venda.nome-abrev.

  // Instruçao de Separaçao
  FIND b-lisa-integra WHERE
       b-lisa-integra.cod-trans = 'ISF' AND  
       b-lisa-integra.chave = c-chave NO-LOCK.

  IF ped-venda-ext.nr-pedext = '' THEN
     ASSIGN ped-venda-ext.nr-pedext = lisa-integra.val-livre-1.

  RELEASE ped-venda-ext.
END.

//tsp01 - inicio
RUN esbo/boconsParam.p PERSIST SET hBoParam.
RUN getTipoNFAprovPedLisa IN hBoParam(OUTPUT tpNfAprovPedLisa).
IF VALID-HANDLE(hBoParam) THEN
  DELETE PROCEDURE hBoParam.

IF lisa-integra.acao = 'ENVIAR' THEN
  /*RUN esapi/envia-nfs-venda-lisa.p (INPUT ROWID(nota-fiscal),
                                      INPUT NO
                                    ).   // Não é Troca*/


  RUN esapi/envia-nfs-venda-lisa-2.p (INPUT ROWID(nota-fiscal),
                                      INPUT NO,
                                      INPUT tpNfAProvPedLisa).   // Não é Troca
//tsp01- fim

ELSE IF lisa-integra.acao = 'EXCLUIR' THEN DO.
   // Exclui a Nota na Lisa
   RUN esapi/envia-canc-pedido-lisa.p (INPUT ROWID(ped-venda), 
                                       OUTPUT cErro).
END.

IF RETURN-VALUE = 'ADM-ERROR' THEN DO: 
   RUN pi-finalizar IN h-acomp.
   RETURN RETURN-VALUE.
END.
FIND CURRENT lisa-integra EXCLUSIVE-LOCK.
ASSIGN lisa-integra.ind-situacao = 2
      lisa-integra.acao          = ''.   
RELEASE lisa-integra.


FIND ped-venda WHERE
    ped-venda.nr-pedcli = nota-fiscal.nr-pedcli AND
    ped-venda.nome-abrev = nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.

ASSIGN c-chave = ped-venda.nr-pedcli + "|" + 
                ped-venda.nome-abrev.

// Instruçao de Separaçao
FIND b-lisa-integra WHERE
    b-lisa-integra.cod-trans = 'ISF' AND  
    b-lisa-integra.chave = c-chave SHARE-LOCK.
ASSIGN b-lisa-integra.acao        = ''
      b-lisa-integra.ind-situacao = 2.  // Finalizado

RELEASE b-lisa-integra.

RUN pi-finalizar IN h-acomp.
