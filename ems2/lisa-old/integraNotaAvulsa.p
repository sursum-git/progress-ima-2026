DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER cErro  AS CHARACTER   NO-UNDO.

{utp/ut-glob.i}
DEFINE VARIABLE h-acomp   AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-arq-xml AS CHARACTER   NO-UNDO.
RUN utp/ut-acomp PERSIST SET h-acomp.
{utp/ut-liter.i Integraá∆o Nota de Remessa *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FIND lisa-integra NO-LOCK
    WHERE ROWID(lisa-integra) = pRowid NO-ERROR.


RUN pi-acompanhar IN h-acomp (INPUT "Integrando Nota Avulsa: " + lisa-integra.chave).

FIND nota-fiscal WHERE
    nota-fiscal.cod-estabel = ENTRY(1,lisa-integra.chave,"|") AND
    nota-fiscal.serie       = ENTRY(2,lisa-integra.chave,"|") AND
    nota-fiscal.nr-nota-fis = ENTRY(3,lisa-integra.chave,"|") NO-LOCK NO-ERROR.

FIND estabelec OF nota-fiscal       NO-LOCK NO-ERROR.
FIND param-nf-estab OF estabelec    NO-LOCK NO-ERROR.

FIND emitente WHERE
    emitente.cod-emit = nota-fiscal.cod-emit NO-LOCK NO-ERROR.

IF AVAIL nota-fiscal THEN DO.
  ASSIGN c-arq-xml = param-nf-estab.cod-caminho-xml + "\" + nota-fiscal.cod-estabel + FILL("0", (3 - LENGTH(nota-fiscal.serie))) + nota-fiscal.serie + nota-fiscal.nr-nota-fis + ".xml".
  FILE-INFO:FILE-NAME = c-arq-xml.
  IF FILE-INFO:FILE-NAME = ? THEN DO:
     ASSIGN cErro = "o Arquivo:" + c-arq-xml + " n∆o foi encontrado".
     RUN pi-finalizar IN h-acomp.
     RETURN 'adm-error'.
  END.

  // se chegar aqui È porque achou o xml
  RUN esapi/envia-nfs-remessa-lisa.p (INPUT ROWID(nota-fiscal)).
  IF RETURN-VALUE <> 'ADM-OK' THEN DO:
     ASSIGN cErro = "Erro ao enviar a nota fiscal de Remessa".
     RUN pi-finalizar IN h-acomp.
     RETURN 'adm-error'.
  END.
  FIND CURRENT lisa-integra EXCLUSIVE-LOCK NO-ERROR.
  ASSIGN lisa-integra.acao = ''
         lisa-integra.ind-situacao = 2.
  RELEASE lisa-integra.
END.

RUN pi-finalizar IN h-acomp.
