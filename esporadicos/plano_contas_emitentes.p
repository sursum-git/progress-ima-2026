DEFINE VARIABLE cEstab  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dia     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE mes     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ano     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hora    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE minuto  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE segundo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE agora   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDir    AS CHARACTER   NO-UNDO .
DEFINE VARIABLE cFileStream AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLinha AS INTEGER     NO-UNDO.
DEFINE VARIABLE cArquivo AS CHARACTER   NO-UNDO FORMAT 'X(100)'.
DEFINE TEMP-TABLE ttArquivos
    FIELD nome  AS CHAR FORMAT 'x(40)'
    INDEX nomeArquivo nome DESC.
    
DEFINE TEMP-TABLE tt
    FIELD tipo                  AS CHAR
    FIELD codigo                AS INT
    FIELD nome                  AS CHAR FORMAT 'x(50)'
    FIELD logNovo               AS LOGICAL FORMAT "Sim/NÆo"
    FIELD arquivoOrigem         AS CHAR FORMAT 'x(150)'
    /*FIELD ano_titulo            AS INT  
    FIELD mes_titulo            AS INT
    FIELD dt_cadastro           AS DATE
    FIELD ano_impl              AS INT
    FIELD mes_impl              AS INT*/ 
    .
DEFINE TEMP-TABLE ttAnt LIKE tt.
{utp/ut-glob.i}
    

ASSIGN dia = STRING(day(TODAY))
       mes = STRING(MONTH(TODAY))
       ano = STRING(YEAR(TODAY))
       agora = STRING(TIME,'hh:mm:ss')
       hora  = SUBSTR(agora,1,2)
       minuto = SUBSTR(agora,4,2)
       segundo = SUBSTR(agora,7,2).
ASSIGN cDir =  't:\especificos\integracao\' + i-ep-codigo-usuario + "\".
ASSIGN cArquivo = cDir   + ano + '-' + mes + '-' + dia  + '-' +  hora + '-' + minuto + '-' + segundo  + '-plano_emitentes.csv'.
IF i-ep-codigo-usuario = '1' THEN
   ASSIGN cEstab = '101'.

IF i-ep-codigo-usuario = '5' THEN
   ASSIGN cEstab = '501'.

/* MESSAGE cEstab                          */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.  */

INPUT FROM OS-DIR (cdir) NO-ATTR-LIST ECHO.
REPEAT:
    IMPORT cFileStream.
    /*MESSAGE cFileStream
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    CREATE ttArquivos.
    ASSIGN ttArquivos.nome = cFileStream.
    /*FILE-INFO:FILE-NAME = cDir + cFileStream.
    DISPLAY cFileStream FORMAT "X(18)" LABEL 'name of the file'
            FILE-INFO:FULL-PATHNAME FORMAT "X(21)" LABEL 'FULL-PATHNAME'
            FILE-INFO:PATHNAME FORMAT "X(21)" LABEL 'PATHNAME'
            FILE-INFO:FILE-TYPE FORMAT "X(5)" LABEL 'FILE-TYPE'
            file-info:FILE-MOD-DATE FORMAT '99/99/9999' LABEL "data mod." 
            FILE-INFO:FILE-CREATE-DATE   LABEL "data cria‡Æo"
            WITH 1 COL 1 DOWN WIDTH 550.*/
END.
INPUT CLOSE.

FIND FIRST ttArquivos USE-INDEX nomeArquivo.
IF AVAIL ttArquivos THEN DO:
   /*MESSAGE ttArquivos.nome
       VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
   INPUT FROM VALUE(cDir + ttArquivos.nome).
   REPEAT:
        ASSIGN iLinha = iLinha + 1.
        IF iLinha > 1  THEN DO:
            CREATE ttAnt.
            IMPORT  DELIMITER ";" ttAnt NO-ERROR.
        END.
   END.
   INPUT CLOSE.
END.


FOR EACH emitente NO-LOCK.
    /*DISP emitente.cod-emitente .
    PAUSE 0. */
    FIND FIRST tit_ap 
        WHERE tit_ap.cdn_fornecedor = emitente.cod-emitente
        AND   tit_ap.LOG_sdo_tit_ap = YES NO-LOCK USE-INDEX titap_safra NO-ERROR.
    
    IF AVAIL tit_ap THEN DO:
       FIND FIRST ttAnt
           WHERE ttAnt.codigo  = emitente.cod-emitente
           AND   ttAnt.tipo    = 'fornecedor' NO-LOCK NO-ERROR.

       CREATE tt.
       ASSIGN tt.tipo    = 'fornecedor'
              tt.codigo  = emitente.cod-emitente
              tt.nome    = emitente.nome-emit
              tt.logNovo = NOT AVAIL ttAnt 
              tt.arquivoOrigem = cArquivo
             /*tt.ano_titulo = YEAR(tit_ap.dat_emis_docto)
              tt.mes_titulo = MONTH(tit_ap.dat_emis_docto)
              tt.dt_cadastro = emitente.data-implant
              tt.ano_impl = YEAR(emitente.data-implant)
              tt.mes_impl = month(emitente.data-implant)*/
             .
    END.

    FIND FIRST tit_acr 
        WHERE tit_acr.cod_estab = cEstab
        AND tit_acr.cdn_cliente = emitente.cod-emitente
        AND   tit_acr.LOG_sdo_tit_acr = YES NO-LOCK  USE-INDEX titacr_cliente NO-ERROR.

    IF AVAIL tit_acr THEN DO:
        FIND FIRST ttAnt
           WHERE ttAnt.codigo  = emitente.cod-emitente
           AND   ttAnt.tipo    = 'cliente' NO-LOCK NO-ERROR.
       //DISP emitente.cod-emitente.
       CREATE tt.
       ASSIGN tt.tipo    = 'cliente'
              tt.codigo  = emitente.cod-emitente
              tt.nome    = emitente.nome-emit
              tt.logNovo = NOT AVAIL ttAnt
              tt.arquivoOrigem = cArquivo
              /*tt.ano_titulo = YEAR(tit_acr.dat_emis_docto)
              tt.mes_titulo = MONTH(tit_acr.dat_emis_docto)
              tt.dt_cadastro = emitente.data-implant
              tt.ano_impl = YEAR(emitente.data-implant)
              tt.mes_impl = month(emitente.data-implant)*/
              .
    END.
END.

FOR EACH ttAnt:
    FIND FIRST tt
        WHERE tt.codigo = ttAnt.codigo
       AND   tt.tipo    = ttAnt.tipo  NO-ERROR.
    IF NOT AVAIL tt THEN DO:
        CREATE tt.
        BUFFER-COPY ttant TO tt.
    END.

        
END.

OUTPUT TO value(cArquivo) NO-CONVERT.
PUT "TIPO;CODIGO;NOME;NOVO EMITENTE;ARQUIVO ORIGEM" SKIP.
FOR EACH tt 
    WHERE tt.tipo <> '' AND tt.tipo <> 'tipo' BY tt.tipo BY tt.logNovo:
    EXPORT DELIMITER ";" tt.
END.

OUTPUT CLOSE.
