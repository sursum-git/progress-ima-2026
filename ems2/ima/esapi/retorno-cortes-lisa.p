{esapi/analisarJsonObject2.i}
{esp/ttChave.i}
{esp/util.i}
DEF TEMP-TABLE ttEtiquetas
    FIELD cod-estabel   AS CHAR
    FIELD nr-container  AS CHAR
    FIELD it-codigo     AS CHAR
    FIELD it-lisa       AS CHAR
    FIELD cod-refer     AS CHAR
    FIELD num-rolo-imp  AS CHAR
    FIELD num-rolo-orig AS CHAR
    FIELD num-etiqueta  AS INTEGER
    FIELD quantidade    AS DECIMAL
    FIELD falta         AS LOG
    FIELD acao          AS CHAR
    FIELD idEtqLisa     AS CHAR.

DEF INPUT  PARAMETER TABLE FOR ttJson.
DEF OUTPUT PARAMETER TABLE FOR ttChave.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR c-arq-retorno  AS CHAR.
DEF VAR c-container    AS CHAR.
DEF VAR c-nr-nota-fis  AS CHAR.
DEF VAR c-serie        AS CHAR.
DEF VAR c-it-codigo    AS CHAR.
DEF VAR c-acao         AS CHAR.
DEF VAR c-chave        AS CHAR.
DEF VAR c-msg          AS CHAR.

{lisa/extrairInfCp5ConfEtq.i}
DEFINE VARIABLE nrContainer AS INTEGER     NO-UNDO.
DEFINE VARIABLE idEtqLisa   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE localiz     AS CHARACTER   NO-UNDO.


DEFINE VARIABLE lAchou AS LOGICAL     NO-UNDO.
FUNCTION fn-busca-json RETURNS CHARACTER
        (INPUT p-tag-pai AS CHAR, INPUT p-tag AS CHAR):

    DEF VAR c-retorno AS CHAR.

    FIND FIRST ttJson WHERE
         ttJson.tag_pai = p-tag-pai AND 
         ttJson.tag = p-tag  and
         ttJson.valor <> 'jsonArray' AND
         ttJson.valor <> 'json' NO-ERROR.
    IF AVAIL ttJson THEN
       ASSIGN c-retorno = ttJson.valor.

    RETURN c-retorno.

END FUNCTION.

// buscar do header
ASSIGN c-container = fn-busca-json('payload',"cntr").
ASSIGN c-arq-retorno = fn-busca-json('arquivo',"separacao").

OUTPUT TO c:\temp\tst\retorno-cortes.txt APPEND.
  PUT UNFORMATTED  SKIP(2) 
      'entrei... ' STRING(NOW) SKIP
      'container: ' c-container
      SKIP.
OUTPUT CLOSE.

ASSIGN c-acao = fn-busca-json('queryParams',"acao").

IF c-acao <> 'CORTE' THEN DO.
   // Retorno 'Ess∆o acaáo n∆o Ç de corte' 
   ASSIGN c-msg = 'Aáao enviada N«O Ç de Corte' + CHR(10) + 
                  'Dados Retornados ' + CHR(10) +
                  '  Aá∆o: ' + c-acao + CHR(10) +
                  '  Container: ' +  c-container.

   RUN pi-cria-retorno (INPUT c-container,
                        INPUT c-msg).
END.

FIND FIRST ttChave NO-ERROR.
IF AVAIL ttChave THEN DO.
   //RUN criarTTJson('RetornoRomaneio', '500').
   RETURN 'ADM-ERROR'.
END.

FOR EACH ttJson WHERE
         ttJson.tag_pai = 'Itens' NO-LOCK 
         USE-INDEX pri.

    CASE ttJson.tag:
        WHEN 'Quantidade' THEN DO.
            ASSIGN ttJson.valor = REPLACE(ttJson.valor,".",",").

            CREATE ttEtiquetas.
            ASSIGN ttEtiquetas.quantidade = DECIMAL(ttJson.valor)
                   ttEtiquetas.nr-container = c-container.   
        END.
        WHEN 'produto' THEN 
            ASSIGN ttEtiquetas.it-codigo = ttJson.valor.   
        WHEN 'produtoLisa' THEN 
            ASSIGN ttEtiquetas.it-lisa = ttJson.valor.   
        WHEN 'codigoRolo' THEN
           ASSIGN ttEtiquetas.num-rolo-imp = ttJson.valor.
        WHEN 'lote' THEN
           ASSIGN ttEtiquetas.cod-refer = ttJson.valor.
        WHEN 'falta' THEN
           ASSIGN ttEtiquetas.falta = LOGICAL(ttJson.valor).
        WHEN 'roloOrigem' THEN
           ASSIGN ttEtiquetas.num-rolo-orig = ttJson.valor.
        WHEN 'id' THEN
           ASSIGN ttEtiquetas.idEtqLisa = ttJson.valor .

   END CASE.
END.

{esp/exportarTabelaCsv3.i  ttEtiquetas " " " " "ttEtiquetasCorte"}


FOR EACH ttEtiquetas.
    IF ttEtiquetas.falta THEN DO.
       DELETE ttEtiquetas.
       NEXT.
    END.

    ASSIGN lAchou                   = NO
           ttEtiquetas.num-etiqueta = 0
           ttEtiquetas.acao         = 'OK-MANTER'.


    FIND etiqueta_lisa NO-LOCK
        WHERE etiqueta_lisa.id_etq_lisa = ttEtiquetas.idEtqLisa
        NO-ERROR.
    IF AVAIL etiqueta_lisa THEN DO:
       FIND ob-etiqueta NO-LOCK
       WHERE ob-etiqueta.cod-estabel  = etiqueta_lisa.cod_estabel
       AND   ob-etiqueta.num-etiqueta = etiqueta_lisa.num_etiqueta
       NO-ERROR.
       ASSIGN lAchou = AVAIL ob-etiqueta.
    END.                                 
    IF NOT lAchou  THEN DO:
       FIND ob-etiqueta WHERE
        ob-etiqueta.nr-container  = INTEGER(ttEtiquetas.nr-container) AND
        ob-etiqueta.it-codigo     = ttEtiquetas.it-codigo AND
        ob-etiqueta.cod-refer     = ttEtiquetas.cod-refer AND
        ob-etiqueta.num-rolo-imp  = INTEGER(ttEtiquetas.num-rolo-imp)
        NO-LOCK NO-ERROR.
        ASSIGN lAchou = AVAIL ob-etiqueta.
    END.

    IF lAchou THEN DO.
       ASSIGN ttEtiquetas.cod-estabel = ob-etiqueta.cod-estabel
              ttEtiquetas.num-etiqueta = ob-etiqueta.num-etiqueta.

       IF ob-etiqueta.quantidade <> ttEtiquetas.quantidade THEN DO:
          ASSIGN ttEtiquetas.acao = 'ALTERAR'.
       END.
    END.
    ELSE
       ASSIGN ttEtiquetas.acao = 'CRIAR'.
END.

OUTPUT TO c:\temp\tst\retorno-cortes.txt APPEND.
    FOR EACH ttEtiquetas.
        PUT UNFORMATTED 
            ttEtiquetas.acao "-"   
            ttEtiquetas.it-codigo "-" 
            ttEtiquetas.it-lisa "-"     
            ttEtiquetas.cod-refer "-" 
            ttEtiquetas.nr-container "- " 
            ttEtiquetas.num-rolo-imp "-" 
            ttEtiquetas.num-rolo-orig "-" 
            ttEtiquetas.quantidade "-"
            ttEtiquetas.falta  "-"
            ttEtiquetas.idEtqLisa "-"
            ttEtiquetas.num-etiqueta
            NOW
            SKIP.
    END.
OUTPUT CLOSE.

// Cria a Tabela de Integraáao
FOR EACH ttEtiquetas WHERE
         ttEtiquetas.acao <> ''.
    ASSIGN c-msg = 'Rolo IMP: ' + ttEtiquetas.num-rolo-imp + ' ' + 
                    ttEtiquetas.acao.
    IF ttEtiquetas.acao = 'ALTERAR' THEN
       ASSIGN c-msg = c-msg +
                      ' Qtde para:' + STRING(ttEtiquetas.quantidade).

    RUN pi-cria-log (INPUT "ConfEtiquetas",
                     INPUT c-msg,
                     INPUT NO).


    FIND lisa-integra WHERE
         lisa-integra.cod-trans    = "ConfEtiquetas" AND
         lisa-integra.chave        = STRING(ttEtiquetas.num-etiqueta) AND
         lisa-integra.val-livre-1  = ttEtiquetas.num-rolo-imp AND
         lisa-integra.val-livre-2  = ttEtiquetas.it-codigo AND
         lisa-integra.val-livre-3  = ttEtiquetas.cod-refer AND
         entry(1,lisa-integra.val-livre-5,"|") = STRING(ttEtiquetas.nr-container)
         NO-LOCK NO-ERROR.
    IF NOT AVAIL lisa-integra THEN DO.
       CREATE lisa-integra.
       ASSIGN lisa-integra.cod-trans = "ConfEtiquetas"
              lisa-integra.chave = STRING(ttEtiquetas.num-etiqueta)
              lisa-integra.conteudo = STRING(ttEtiquetas.quantidade)
              lisa-integra.val-livre-1 = ttEtiquetas.num-rolo-imp
              lisa-integra.val-livre-2 = ttEtiquetas.it-codigo
              lisa-integra.val-livre-3 = ttEtiquetas.cod-refer
              lisa-integra.val-livre-4 = ttEtiquetas.it-lisa
              lisa-integra.val-livre-5 = ttEtiquetas.nr-container + "|" + ttEtiquetas.idEtqLisa
              lisa-integra.acao = ttEtiquetas.acao
              lisa-integra.ind-situacao = 1
              lisa-integra.dt-trans = TODAY
              lisa-integra.hr-trans = TIME.
    END.
    
    
END.


// ajusta as Etiquetas
FOR EACH lisa-integra WHERE
         lisa-integra.cod-trans = "ConfEtiquetas" AND
         lisa-integra.ind-situacao = 1 // Aguardando Integraáao
         SHARE-LOCK.
   
    ASSIGN lisa-integra.acao = TRIM(lisa-integra.acao).

    IF lisa-integra.chave <> '0' THEN DO.
       FIND ob-etiqueta WHERE
            ob-etiqueta.cod-estabel = '505' AND
            ob-etiqueta.num-etiqueta = INTEGER(lisa-integra.chave)
            SHARE-LOCK NO-ERROR.

       IF lisa-integra.acao = 'ALTERAR' THEN DO.
          IF ob-etiqueta.situacao = 3  THEN //somente pode alterar se estiver em estoque.
             ASSIGN ob-etiqueta.quantidade = DECIMAL(lisa-integra.conteudo).
       END.

       IF ob-etiqueta.quantidade = 0 OR 
          lisa-integra.acao = 'BAIXAR' THEN 
          ASSIGN ob-etiqueta.situacao = 9.  // Consumida
    END.
    ELSE DO.
       ASSIGN c-it-codigo = IF lisa-integra.val-livre-2 <> ''
                            THEN lisa-integra.val-livre-2 
                            ELSE SUBSTR(lisa-integra.val-livre-4,4).

       RUN extrairInfCp5ConfEtq(lisa-integra.val-livre-5,OUTPUT nrContainer,OUTPUT idEtqLisa, OUTPUT localiz).

       FIND FIRST ob-etiqueta WHERE
                  ob-etiqueta.cod-estab    = '505' AND 
                  ob-etiqueta.nr-container = nrContainer                   AND
                  ob-etiqueta.it-codigo    = c-it-codigo                   AND
                  ob-etiqueta.cod-refer    = lisa-integra.val-livre-3      AND
                  ob-etiqueta.num-rolo-imp  = INTEGER(lisa-integra.val-livre-1)
                  NO-LOCK NO-ERROR.
       IF NOT AVAIL ob-etiqueta THEN DO.
          RUN pi-cria-etiqueta.
          IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
       END.
   END.

   ASSIGN lisa-integra.ind-situacao = 2.
END.


// Procedures ------------

PROCEDURE pi-cria-retorno.
    DEF INPUT PARAMETER p-chave AS CHAR.
    DEF INPUT PARAMETER p-valor AS CHAR.

    CREATE ttChave.
    ASSIGN ttChave.chave = p-chave
           ttChave.valor = p-valor.
END.

PROCEDURE pi-cria-log.
    DEF INPUT PARAMETER p-cod-trans AS CHAR.
    DEF INPUT PARAMETER p-msg AS CHAR.
    DEF INPUT PARAMETER p-log-erro AS LOG.

    CREATE lisa-log-integr.
    ASSIGN lisa-log-integr.cod-trans = p-cod-trans   
           lisa-log-integr.data = TODAY
           lisa-log-integr.hora = TIME
           lisa-log-integr.usuario = c-seg-usuario
           lisa-log-integr.chave = STRING(ttEtiquetas.num-etiqueta)
           lisa-log-integr.arq-retorno = c-arq-retorno
           lisa-log-integr.acao = ttEtiquetas.acao 
           lisa-log-integr.log-erro = p-log-erro
           lisa-log-integr.narrativa = p-msg.

    //PAUSE 1 NO-MESSAGE.
END.

PROCEDURE pi-cria-etiqueta.
    FIND ITEM WHERE
         ITEM.it-codigo = c-it-codigo NO-LOCK NO-ERROR.

  
    CREATE ob-etiqueta.
    ASSIGN ob-etiqueta.cod-estabel     = '505'
           ob-etiqueta.dt-emissao      = TODAY
           ob-etiqueta.hr-emissao      = STRING(TIME,"HH:MM")
           ob-etiqueta.acondic         = ""
           ob-etiqueta.it-codigo       = c-it-codigo
           ob-etiqueta.cod-refer       = lisa-integra.val-livre-3
           ob-etiqueta.nr-container    = nrContainer
           ob-etiqueta.nr-lote         = 'CA'
           ob-etiqueta.cod-qualid      = 'D' 
           ob-etiqueta.corte-comerc    = ''
           ob-etiqueta.quantidade      = DECIMAL(lisa-integra.conteudo)
           ob-etiqueta.localizacao     = localiz
           ob-etiqueta.situacao        = 3
           ob-etiqueta.cod-depos       = 'ITA'.

     ASSIGN ob-etiqueta.num-etiqueta = NEXT-VALUE(seq-etq-estoq-itj).

     ASSIGN ob-etiqueta.num-rolo-imp    = INTEGER(lisa-integra.val-livre-1)
            ob-etiqueta.ob-origem       = ''.
    
      RUN esapi/gravarEtqLisa.p(ob-etiqueta.cod-estabel,
                                ob-etiqueta.num-etiqueta,
                                ob-etiqueta.it-Codigo,
                                ob-etiqueta.cod-estabel,
                                ob-etiqueta.nr-container,
                                0,
                                0,
                                idEtqLisa,
                                 3 //ajuste etq - corte
                                ).


END PROCEDURE.

