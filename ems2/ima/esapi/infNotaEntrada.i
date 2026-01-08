DEF TEMP-TABLE infNotaEntrada NO-UNDO SERIALIZE-NAME ""
    FIELD nfid            AS INTEGER SERIALIZE-HIDDEN
    FIELD ccodfilial      AS CHAR
    FIELD cnota           AS CHAR
    FIELD cserie          AS CHAR
    FIELD cemissaonf      AS CHAR
    FIELD ccgcfornecedor  AS CHAR
    FIELD ccgc            AS CHAR
    FIELD cchavenfe       AS CHAR
    FIELD cnforiginal     AS CHAR
    FIELD cserieoriginal  AS CHAR
    FIELD citemoriginal   AS CHAR
    FIELD ccodigopostagem AS CHAR
    FIELD cnumdocsap      AS CHAR
    FIELD cxmldanfe       AS CHAR
    FIELD cpdfdanfe64     AS BLOB.  
{lisa\varPropsComuns.i}

PROCEDURE criarInfNotaEntrada:

    DEFINE INPUT  PARAMETER pRowid      AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER logExclusao AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER i-id        AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER cErros      AS CHARACTER   NO-UNDO.
    DEF VAR c-arq-xml                   AS CHAR.
    DEF VAR c-arq-pdf                   AS CHAR.
    DEF VAR lc-xml                      AS LONGCHAR.
    DEF VAR lc-pdf                      AS LONGCHAR.

    FIND nota-fiscal WHERE
         ROWID(nota-fiscal) = pRowid NO-LOCK NO-ERROR.
    
    FIND estabelec OF nota-fiscal NO-LOCK NO-ERROR.
    FIND param-nf-estab OF estabelec NO-LOCK NO-ERROR.
    
    ASSIGN i-id = RANDOM(1,99999).
    
    CREATE infNotaEntrada.
    ASSIGN infNotaEntrada.nfid              = i-id
           infNotaEntrada.ccodfilial        = cFilial
           infNotaEntrada.cnota             = nota-fiscal.nr-nota-fis
           infNotaEntrada.cserie            = nota-fiscal.serie
           infNotaEntrada.cemissaonf        = STRING(YEAR(nota-fiscal.dt-emis),"9999") +  STRING(MONTH(nota-fiscal.dt-emis),"99") +  STRING(DAY(nota-fiscal.dt-emis),"99") 
           infNotaEntrada.ccgcfornecedor    = estabelec.cgc 
           infNotaEntrada.ccgc              = estabelec.cgc
           infNotaEntrada.cchavenfe         = nota-fiscal.cod-chave-aces-nf-eletro
           infNotaEntrada.cnforiginal       = ""   
           infNotaEntrada.cserieoriginal    = ""
           infNotaEntrada.citemoriginal     = "" 
           infNotaEntrada.ccodigopostagem   = ""
           infNotaEntrada.cnumdocsap        = "" 
           infNotaEntrada.cxmldanfe         = ""
           .
    
    IF NOT logExclusao THEN DO: // json de exclus∆o n∆o precisa do xml

        ASSIGN c-arq-xml = param-nf-estab.cod-caminho-xml + "\" + nota-fiscal.cod-estabel + FILL("0", (3 - LENGTH(nota-fiscal.serie))) + nota-fiscal.serie + nota-fiscal.nr-nota-fis + ".xml".
        FILE-INFO:FILE-NAME = c-arq-xml.
        IF FILE-INFO:FULL-PATHNAME = ? THEN DO.
           ASSIGN cErros = 'N∆o foi encontrado o XML da nota,Nota n∆o poder† ser Enviada'.
           RETURN 'adm-error'.
        END.
        COPY-LOB FROM FILE c-arq-xml TO lc-xml.
        ASSIGN infNotaEntrada.cxmldanfe = lc-xml.
        RUN pi-gera-danfe.
        ASSIGN c-arq-pdf = SESSION:TEMP-DIRECTORY + "FT0518" + nota-fiscal.nr-nota-fis + ".PDF".
        FILE-INFO:FILE-NAME = c-arq-pdf.
        IF FILE-INFO:FULL-PATHNAME = ? THEN DO.
           ASSIGN cErros =  'N∆o foi possivel Gerar o DANFE,a Nota n∆o poder† ser Enviada'.
           RETURN 'adm-error'.
        END.           
        COPY-LOB FROM FILE c-arq-pdf TO infNotaEntrada.cpdfdanfe64.

    END.


END PROCEDURE.
