DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER cErro  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE h-acomp   AS HANDLE      NO-UNDO.
{utp/ut-glob.i}

RUN utp/ut-acomp PERSIST SET h-acomp.
{utp/ut-liter.i Integra‡Æo Packlist Avulso*}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).


FIND lisa-integra NO-LOCK
    WHERE ROWID(lisa-integra) = pRowid NO-ERROR.


RUN pi-acompanhar IN h-acomp (INPUT "Enviando Packing Avulso " + lisa-integra.chave ).

FIND nota-fiscal WHERE
    nota-fiscal.cod-estabel = ENTRY(1,lisa-integra.chave,"|") AND
    nota-fiscal.serie = ENTRY(2,lisa-integra.chave,"|") AND
    nota-fiscal.nr-nota-fis = ENTRY(3,lisa-integra.chave,"|") 
    NO-LOCK NO-ERROR.
IF NOT AVAIL nota-fiscal THEN DO:
   ASSIGN cErro = "NÆo encontrada nota fiscal:" + ENTRY(3,lisa-integra.chave,"|").
   RUN pi-finalizar IN h-acomp.
   RETURN 'adm-error'.

END.

RUN esapi/envia-pckl-avulso.p (INPUT ROWID(nota-fiscal)).
IF RETURN-VALUE = 'ADM-ERROR' THEN  DO:
   ASSIGN cErro = "Erro no envio da nota fiscal:" + ENTRY(3,lisa-integra.chave,"|").
   RUN pi-finalizar IN h-acomp.
   RETURN 'adm-error'.
END.
FIND CURRENT lisa-integra EXCLUSIVE-LOCK NO-ERROR.

ASSIGN lisa-integra.acao        = ''
      lisa-integra.ind-situacao = 2.

RELEASE lisa-integra.

RUN pi-finalizar IN h-acomp.
