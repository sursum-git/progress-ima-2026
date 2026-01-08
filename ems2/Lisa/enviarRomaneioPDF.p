
DEFINE INPUT  PARAMETER rowidNF AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER cErro   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cArqRomaneio    AS CHARACTER   NO-UNDO.
{lisa\varpropsComuns.i}

DEF TEMP-TABLE infRomaneio   NO-UNDO
    FIELD ccodFilial            AS CHAR
    FIELD cpedido               AS CHAR 
    FIELD ccgc                  AS CHAR
    FIELD ctipo                 AS CHAR 
    FIELD cdocumentoPDFbase64   AS BLOB
    FIELD curldocumentoPDF      AS CHAR.

FIND FIRST nota-fiscal NO-LOCK
    WHERE ROWID(nota-fiscal) = RowidNf
    NO-ERROR.
IF NOT AVAIL nota-fiscal THEN
   RETURN 'nok'.

FIND nota-fiscal NO-LOCK
    WHERE rowid(nota-fiscal) = rowidNF
    NO-ERROR.
IF NOT AVAIL nota-fiscal THEN DO:
   ASSIGN cErro = 'Nota Fiscal N∆o Encontrada'.
   RETURN 'nok'.
END.

/*FIND ped-venda NO-LOCK
    WHERE ped-venda.nome-abrev  = nota-fiscal.nome-ab-cli
    AND   ped-venda.nr-pedcli   = nota-fiscal.nr-pedcli NO-ERROR.

IF NOT AVAIL ped-venda THEN DO:
   ASSIGN cErro = 'Pedido de venda da Nota Fiscal n∆o Encontrado'.
   RETURN 'nok'.
END.*/




RUN esapi/dadosRomaneioExcel.p(nota-fiscal.cod-estabel,
                               nota-fiscal.serie,
                               nota-fiscal.nr-nota-fis,
                               NO, // gerar mesmo se j† existir o arquivo
                               YES, // converte para PDF
                               OUTPUT cArqRomaneio
                              ).

IF SEARCH(cArqRomaneio) = ? THEN DO:
   ASSIGN cErro = "Arquivo de Romaneio N∆o Encontrado".
   RETURN 'nok'.
END.
CREATE infRomaneio.
ASSIGN 
infRomaneio.ccodFilial           = cFilial
infRomaneio.cpedido              = nota-fiscal.nr-pedcli
infRomaneio.ccgc                 = cCNPJ
infRomaneio.ctipo                = 'romaneio'.
COPY-LOB FROM FILE cArqRomaneio TO infRomaneio.cdocumentoPDFbase64.

RUN lisa\enviarRomaneio.p(TEMP-TABLE infRomaneio:HANDLE,
                          nota-fiscal.nr-pedcli,
                          OUTPUT cErro).


IF cErro <> '' THEN
   RETURN 'nok'.


RETURN 'ok'.





