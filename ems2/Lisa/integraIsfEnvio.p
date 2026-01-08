DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER cErro  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE h-acomp   AS HANDLE      NO-UNDO.
{utp/ut-glob.i}

RUN utp/ut-acomp PERSIST SET h-acomp.
{utp/ut-liter.i Integra‡Æo Packlist Avulso*}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).


FIND lisa-integra NO-LOCK
    WHERE ROWID(lisa-integra) = pRowid NO-ERROR.


RUN pi-acompanhar IN h-acomp (INPUT "Enviando ISF " + lisa-integra.chave ).

FIND ped-venda WHERE
     ped-venda.nr-pedcli = ENTRY(1,lisa-integra.chave,"|") AND
     ped-venda.nome-abrev = ENTRY(2,lisa-integra.chave,"|") NO-LOCK NO-ERROR.

IF lisa-integra.acao = 'ENVIAR' THEN DO.
   IF lisa-integra.val-livre-1 = '' THEN
      RUN esapi/envia-isf-lisa.p (INPUT ROWID(ped-venda)).
   ELSE DO:
      RUN esapi/envia-alteracao-isf-lisa.p (INPUT ROWID(ped-venda), INPUT lisa-integra.val-livre-1).
   END.                                            
   IF RETURN-VALUE <> 'ADM-OK' THEN DO:              
      ASSIGN cErro = "Erro ao enviar a ISF".
      RUN pi-finalizar IN h-acomp.
      RETURN RETURN-VALUE.
   END.
   FIND CURRENT lisa-integra EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN lisa-integra.acao = 'SEPARAR'.
   RELEASE lisa-integra.

END.

FINALLY:

IF VALID-HANDLE(h-acomp) THEN
DO:
    RUN pi-finalizar IN h-acomp.
    
END.

END FINALLY.
