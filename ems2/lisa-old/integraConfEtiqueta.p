DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER cErro  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE nrContainer AS INTEGER     NO-UNDO.
DEFINE VARIABLE cEtqLisa    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLocaliz    AS CHARACTER   NO-UNDO.

DEFINE VARIABLE h-acomp   AS HANDLE      NO-UNDO.
{utp/ut-glob.i}

DEFINE VARIABLE c-it-codigo AS CHARACTER   NO-UNDO.
RUN utp/ut-acomp PERSIST SET h-acomp.
{utp/ut-liter.i Integra‡Æo Conf.Etiqueta *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).


FIND lisa-integra NO-LOCK
    WHERE ROWID(lisa-integra) = pRowid NO-ERROR.


IF lisa-integra.dt-trans = ? THEN
  ASSIGN lisa-integra.dt-trans = TODAY.

ASSIGN lisa-integra.acao = TRIM(lisa-integra.acao).

IF lisa-integra.chave <> '0' THEN DO.
  FIND ob-etiqueta WHERE
       ob-etiqueta.cod-estabel = '505' AND
       ob-etiqueta.num-etiqueta = INTEGER(lisa-integra.chave) SHARE-LOCK NO-ERROR.

  IF lisa-integra.acao = 'ALTERAR' THEN DO.
     IF ob-etiqueta.situacao = 3  THEN //somente pode alterar se estiver em estoque.
        ASSIGN ob-etiqueta.quantidade = DECIMAL(lisa-integra.conteudo).
     ELSE DO:
        ASSIGN cErro =  "A Etiqueta(J  Faturada):" + STRING(ob-etiqueta.num-etiqueta) + " tem a quantidade de:" + STRING(ob-etiqueta.quantidade) + 
                        " sendo que no arquivo da LISA a quantidade informada ‚ :" + lisa-integra.conteudo .
        RETURN 'ADM-ERROR'.
     END.
        
  END.

  IF ob-etiqueta.quantidade = 0 OR lisa-integra.acao = 'BAIXAR' THEN DO:
     ASSIGN ob-etiqueta.situacao = 9.  // Consumida
  END.
     
END.
ELSE DO.
   ASSIGN c-it-codigo = IF lisa-integra.val-livre-2 <> '' THEN lisa-integra.val-livre-2  ELSE SUBSTR(lisa-integra.val-livre-4,4).
   FIND FIRST ob-etiqueta WHERE
              ob-etiqueta.cod-estab    = '505' AND 
              ob-etiqueta.nr-container = INT(lisa-integra.val-livre-5) AND
              ob-etiqueta.it-codigo    = c-it-codigo                   AND
              ob-etiqueta.cod-refer    = lisa-integra.val-livre-3      AND
              ob-etiqueta.num-rolo-imp  = INTEGER(lisa-integra.val-livre-1) NO-LOCK NO-ERROR.
   IF NOT AVAIL ob-etiqueta THEN DO.
      RUN pi-cria-etiqueta.
      IF RETURN-VALUE = 'ADM-ERROR' THEN do:
         ASSIGN cErro = "Erro na cria‡Æo da Etiqueta" .
         RETURN RETURN-VALUE.
      END.
   END.
END.

ASSIGN lisa-integra.ind-situacao = 2.
RELEASE lisa-integra.

RUN pi-finalizar IN h-acomp.

PROCEDURE pi-cria-etiqueta.
    DEFINE VARIABLE idEtqLisa AS CHARACTER   NO-UNDO.
    FIND ITEM WHERE ITEM.it-codigo = c-it-codigo NO-LOCK NO-ERROR.

    
    CREATE ob-etiqueta.
    ASSIGN ob-etiqueta.cod-estabel     = '505'
           ob-etiqueta.dt-emissao      = TODAY
           ob-etiqueta.hr-emissao      = STRING(TIME,"HH:MM")
           ob-etiqueta.acondic         = ""
           ob-etiqueta.it-codigo       = c-it-codigo
           ob-etiqueta.cod-refer       = lisa-integra.val-livre-3
           ob-etiqueta.nr-container    = INT(entry(1,lisa-integra.val-livre-5,"|"))
           ob-etiqueta.nr-lote         = 'CA'
           ob-etiqueta.cod-qualid      = 'D' 
           ob-etiqueta.corte-comerc    = ''
           ob-etiqueta.quantidade      = DECIMAL(lisa-integra.conteudo)
           ob-etiqueta.localizacao     = ''
           ob-etiqueta.situacao        = 3
           ob-etiqueta.cod-depos       = 'ITA'.

     ASSIGN ob-etiqueta.num-etiqueta = NEXT-VALUE(seq-etq-estoq-itj).

     ASSIGN ob-etiqueta.num-rolo-imp    = INTEGER(lisa-integra.val-livre-1)
            ob-etiqueta.ob-origem       = ''.

     IF NUM-ENTRIES(lisa-integra.val-livre-5,"|") > 1 THEN DO:
        ASSIGN idEtqLisa = ENTRY(2,lisa-integra.val-livre-5,"|").
        RUN esapi/gravarEtqLisa.p(ob-etiqueta.cod-estabel,
                                 ob-etiqueta.num-etiqueta,
                                 ob-etiqueta.it-Codigo,
                                 ob-etiqueta.cod-estabel,
                                 ob-etiqueta.nr-container,
                                 0,
                                 0,
                                 idEtqLisa,
                                 2).
     END.

END PROCEDURE.
