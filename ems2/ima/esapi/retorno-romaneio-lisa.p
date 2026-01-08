{esapi/analisarJsonObject2.i}
{esp/ttChave.i}
{esp/params.i}
{lisa/codProdUnif.i}
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
    FIELD id_etq_lisa   LIKE etiqueta_lisa.id_etq_lisa
    FIELD localiz       AS CHAR
    .

DEF INPUT  PARAMETER TABLE FOR ttJson.
DEF OUTPUT PARAMETER TABLE FOR ttChave.

DEF VAR c-container    AS CHAR.
DEF VAR c-nr-nota-fis  AS CHAR.
DEF VAR c-serie        AS CHAR.
DEF VAR c-acao         AS CHAR.
DEF VAR c-chave        AS CHAR.
DEF VAR c-msg          AS CHAR.
DEFINE VARIABLE cArquivo AS CHARACTER   NO-UNDO.

FUNCTION fn-busca-json RETURNS CHARACTER
        (INPUT p-tag-pai AS CHAR, INPUT p-tag AS CHAR):

    DEF VAR c-retorno AS CHAR.

    FIND ttJson WHERE
         ttJson.tag_pai = p-tag-pai AND 
         ttJson.tag = p-tag NO-ERROR.
    IF AVAIL ttJson THEN
       ASSIGN c-retorno = ttJson.valor.

    RETURN c-retorno.

END FUNCTION.

// buscar do header
ASSIGN c-container = fn-busca-json('payload',"cntr").

ASSIGN c-acao = fn-busca-json('',"acao").
IF c-acao = 'CORTE' THEN DO.
   // Retorno 'Ess∆o ac∆o Ç de corte' 
   ASSIGN c-msg = 'Aáao enviada Ç de Corte' + CHR(10) + 
                  'Dados Retornados ' + CHR(10) +
                  '  Aá∆o: ' + c-acao + CHR(10) +
                  '  Container: ' +  c-container.

   RUN pi-cria-retorno (INPUT c-container,
                        INPUT c-msg).
END.

FIND FIRST ttChave NO-ERROR.
IF AVAIL ttChave THEN DO.
   //RUN criarTTRetorno('RetornoRomaneio', 500).
   RETURN 'ADM-ERROR'.
END.
ASSIGN cArquivo = 'retorno-romaneio' + c-container + '.txt'.
OUTPUT TO value( SESSION:TEMP-DIRECTORY + cArquivo) APPEND.
  PUT UNFORMATTED  SKIP(2) 
      'entrei... ' STRING(NOW) SKIP
      'container: ' c-container
      SKIP.
OUTPUT CLOSE.

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
            ASSIGN ttEtiquetas.it-codigo    = ttJson.valor.   
        WHEN 'produtoLisa' THEN 
            ASSIGN ttEtiquetas.it-lisa      = ttJson.valor.   
        WHEN 'codigoRolo' THEN
           ASSIGN ttEtiquetas.num-rolo-imp  = ttJson.valor.
        WHEN 'lote' THEN
           ASSIGN ttEtiquetas.cod-refer     = ttJson.valor.
        WHEN 'falta' THEN
           ASSIGN ttEtiquetas.falta         = LOGICAL(ttJson.valor).
        WHEN 'roloOrigem' THEN
           ASSIGN ttEtiquetas.num-rolo-orig = ttJson.valor.
        WHEN 'id' THEN
           ASSIGN ttEtiquetas.id_etq_lisa   = ttJson.valor.
        WHEN 'endereco' THEN
           ASSIGN ttEtiquetas.localiz       = ttJson.valor. 

        IF lCodigoProdUnificado THEN DO:
           ASSIGN ttEtiquetas.it-codigo = ENTRY(1,ttEtiquetas.it-codigo,"-")
                  ttEtiquetas.cod-refer = ENTRY(2,ttEtiquetas.it-codigo,"-")
        END.

   END CASE.
END.

FOR EACH ttEtiquetas.


    FIND ob-etiqueta WHERE
         ob-etiqueta.nr-container = INTEGER(ttEtiquetas.nr-container) AND
         ob-etiqueta.it-codigo = ttEtiquetas.it-codigo AND
         ob-etiqueta.cod-refer = ttEtiquetas.cod-refer AND
         ob-etiqueta.num-rolo-imp = INTEGER(ttEtiquetas.num-rolo-imp)
         NO-LOCK NO-ERROR.

    ASSIGN ttEtiquetas.num-etiqueta = 0.
    IF AVAIL ob-etiqueta THEN DO.
       RUN esapi/gravarEtqLisa.p(ob-etiqueta.cod-estabel,
                                 ob-etiqueta.num-etiqueta,
                                 ob-etiqueta.it-Codigo,
                                 ob-etiqueta.cod-estabel,
                                 ob-etiqueta.nr-container,
                                 0,
                                 0,
                                 ttEtiquetas.id_etq_lisa,
                                 1).


       ASSIGN ttEtiquetas.cod-estabel = ob-etiqueta.cod-estabel
              ttEtiquetas.num-etiqueta = ob-etiqueta.num-etiqueta.

       IF ttEtiquetas.falta THEN 
          ASSIGN ttEtiquetas.acao = 'BAIXAR'.
       ELSE IF ob-etiqueta.quantidade <> ttEtiquetas.quantidade THEN
          ASSIGN ttEtiquetas.acao = 'ALTERAR'.
    END.
    ELSE DO.
       IF ttEtiquetas.falta = NO THEN
          ASSIGN ttEtiquetas.acao = 'CRIAR'.
    END.
END.

OUTPUT TO value(SESSION:TEMP-DIRECTORY + cArquivo) APPEND.
    FOR EACH ttEtiquetas.
        PUT UNFORMATTED 
            ttEtiquetas.acao " "   
            ttEtiquetas.it-codigo " " 
            ttEtiquetas.it-lisa " "     
            ttEtiquetas.cod-refer " " 
            ttEtiquetas.nr-container " " 
            ttEtiquetas.num-rolo-imp " " 
            ttEtiquetas.num-rolo-orig " " 
            ttEtiquetas.quantidade " "
            ttEtiquetas.falta  
            ttEtiquetas.id_etq_lisa
            ttEtiquetas.localiz
            SKIP.
    END.
OUTPUT CLOSE.

FOR EACH ttEtiquetas WHERE
         ttEtiquetas.acao <> ''.

    CREATE lisa-integra.
    ASSIGN lisa-integra.cod-trans = "ConfEtiquetas"
           lisa-integra.chave = STRING(ttEtiquetas.num-etiqueta)
           lisa-integra.conteudo = STRING(ttEtiquetas.quantidade)
           lisa-integra.val-livre-1 = ttEtiquetas.num-rolo-imp
           lisa-integra.val-livre-2 = ttEtiquetas.it-codigo
           lisa-integra.val-livre-3 = ttEtiquetas.cod-refer
           lisa-integra.val-livre-4 = ttEtiquetas.it-lisa
           lisa-integra.val-livre-5 = ttEtiquetas.nr-container + "|" + ttEtiquetas.id_etq_lisa + "|" + ttEtiquetas.localiz
           lisa-integra.acao = ttEtiquetas.acao
           lisa-integra.ind-situacao = 1. 
END.

// Procedures ------------

PROCEDURE pi-cria-retorno.
    DEF INPUT PARAMETER p-chave AS CHAR.
    DEF INPUT PARAMETER p-valor AS CHAR.

    CREATE ttChave.
    ASSIGN ttChave.chave = p-chave
           ttChave.valor = p-valor.
END.
