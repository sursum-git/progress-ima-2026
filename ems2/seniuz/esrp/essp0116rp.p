/* Programa: essp0116rp.p
** Objetivo: Efetuar a Importaá∆o das analises dos Fardo de Algod∆o, efetuados
**           pelo aparelhos Fibr¢grafo, Mirocronaire, Presley e Shirley...
** Autor...: Prodb - Toninho  Junho/2006
** Observ..: O Fibr¢grafo gera as analises em DBF, portanto Ç utilizado o
**           utilit†rio "DBF" do progress para converter os arquivos .DBF
**           para texto, e depois importado para as temp-tables.
**           Utilizamos o mesmo utilit†rio DBF para importar as definiá‰es
**           dos DBF's para o Progress, para maior detalhes sobre o DBF execute-o
**           em prompt de comando que ser† mostrado uma breve ajuda.
**           As analises do Micronaire, Presley e Shirlei, s∆o a mÇdia da
**           Nota Fiscal, ou seja analisa apenas parte da carga e repassa o 
**           resultado para todos os Fardos.
*/

/* include de controle de vers∆o */
{include/i-prgvrs.i ESSP0116RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param
       FIELD destino          AS INTEGER 
       FIELD arq-destino      AS CHAR
       FIELD todos            AS INTEGER 
       FIELD usuario          AS CHAR
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD tg-fibro         AS LOG
       FIELD dir-fibro        AS CHAR
       FIELD nr-arq-dbf       AS INTEGER
       FIELD arq-entrada      AS CHAR 
       FIELD arq-entrada-f    AS CHAR
       FIELD tg-micronaire    AS LOG
       FIELD finura           AS DEC 
       FIELD maturidade       AS DEC
       FIELD tg-presley       AS LOG
       FIELD resistencia      AS DEC
       FIELD tg-shirley       AS LOG
       FIELD residuos         AS DEC
       FIELD nr-nota-fis      AS INT
       FIELD cod-fornec       AS INT.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* definiá∆o de vari†veis  */
DEF VAR h-acomp AS HANDLE NO-UNDO.

DEF TEMP-TABLE tt-fibro-c    LIKE mp-fibro-c.
DEF TEMP-TABLE tt-fibro-f    LIKE mp-fibro-f
    FIELD nr-fardo LIKE mp-fardo.nr-fardo.

DEF STREAM s-imp.
DEF VAR l-erro     AS LOG INIT NO.
DEF VAR c-comando  AS CHAR.
DEF VAR c-arq-dbf  AS CHAR.
DEF VAR c-arq-err  AS CHAR.
DEF VAR c-arq-dump AS CHAR.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

{include/i-rpout.i &STREAM="stream str-rp" &TOFILE=tt-param.arq-destino}

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i &STREAM="str-rp"}

/* bloco principal do programa */
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND first empresa
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 

ASSIGN c-programa = "ESSP0116RP"
       c-versao	  = "2.04"
       c-revisao  = ".00.001"
       c-empresa  = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

{utp/ut-liter.i MATERIA_PRIMA * r}
ASSIGN c-sistema = TRIM(RETURN-VALUE).

{utp/ut-liter.i Importa_Analises_do_Fibr¢grafo * r}
ASSIGN c-titulo-relat = TRIM(RETURN-VALUE).

VIEW STREAM str-rp FRAME f-cabec.
VIEW STREAM str-rp FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Importando *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

IF tt-param.tg-fibro THEN DO.
   RUN pi-imp-c.
   IF RETURN-VALUE = 'C-ERROR' THEN DO.
      {include/i-rpclo.i &STREAM="stream str-rp"}
      RUN pi-finalizar IN h-acomp.
      RETURN 'NOK'.
   END.
    
   RUN pi-imp-f.
   IF RETURN-VALUE = 'F-ERROR' THEN DO.
      {include/i-rpclo.i &STREAM="stream str-rp"}
      RUN pi-finalizar IN h-acomp.
      RETURN 'NOK'.
   END.
    
   FOR EACH mp-fibro-c WHERE
            mp-fibro-c.nr-arquivo = tt-param.nr-arq-dbf NO-LOCK,
       EACH mp-fibro-f OF mp-fibro-c NO-LOCK
            BY mp-fibro-f.nfabr.
    
       FIND mp-entr-mat WHERE
            mp-entr-mat.cod-emit = INT(mp-fibro-c.fornecedor) AND
            mp-entr-mat.nro-docto = mp-fibro-c.nfiscal
            NO-LOCK NO-ERROR.
    
       FIND mp-fardo WHERE
            mp-fardo.nr-cdr = mp-entr-mat.nr-cdr AND
            INT(SUBSTR(STRING(mp-fardo.nr-fardo,"99999999"),5,4)) = mp-fibro-f.fardo
            SHARE-LOCK NO-ERROR.
    
       ASSIGN mp-fardo.sl1 = mp-fibro-f.sl1 * mp-fibro-c.fc1
              mp-fardo.sl2 = mp-fibro-f.sl2 * mp-fibro-c.fc2
              mp-fardo.ur = (mp-fardo.sl1 / mp-fardo.sl2) * 100.
    
       FIND mp-classif WHERE
            mp-classif.compr-min <= mp-fardo.sl2 AND
            mp-classif.compr-max >= mp-fardo.sl2 NO-LOCK NO-ERROR.
       IF NOT AVAIL mp-classif THEN DO.
          PUT STREAM str-rp
              "Fardo:" mp-fardo.nr-fardo SKIP
              "Comprimento " mp-fardo.sl2  SKIP
              "N«O foi encontrado nos Comprimentos Cadastrados..." 
              SKIP.
          NEXT.
       END.
       ASSIGN mp-fardo.cd-compr = mp-classif.codigo
              mp-fardo.letra = mp-classif.letra
              mp-fardo.situacao = 3.  /* Dispon°vel para Consumo */
   END.
END.

IF tt-param.tg-micronaire THEN DO.
   FIND mp-entr-mat WHERE
        mp-entr-mat.cod-emit = tt-param.cod-fornec AND
        mp-entr-mat.nro-docto = tt-param.nr-nota-fis NO-LOCK NO-ERROR.
   IF NOT AVAIL mp-entr-mat THEN DO.
      PUT STREAM str-rp
          "Nota Fiscal n∆o Encontrada na Descarga de Fardos..." SKIP
          "Fornecedor:" tt-param.cod-fornec SKIP
          "Nota Fiscal:" tt-param.nr-nota-fis
          SKIP.
   END.
   FOR EACH mp-fardo OF mp-entr-mat share-LOCK.
       ASSIGN mp-fardo.finura = tt-param.finura
              mp-fardo.maturidade = tt-param.maturidade.
   END.
END.

IF tt-param.tg-presley THEN DO.
   FIND mp-entr-mat WHERE
        mp-entr-mat.cod-emit = tt-param.cod-fornec AND
        mp-entr-mat.nro-docto = tt-param.nr-nota-fis NO-LOCK NO-ERROR.
   IF NOT AVAIL mp-entr-mat THEN DO.
      PUT STREAM str-rp
          "Nota Fiscal n∆o Encontrada na Descarga de Fardos..." SKIP
          " Fornecedor:" tt-param.cod-fornec SKIP
          "Nota Fiscal:" tt-param.nr-nota-fis
          SKIP.
   END.
   FOR EACH mp-fardo OF mp-entr-mat share-LOCK.
       ASSIGN mp-fardo.resistencia = tt-param.resistencia.
   END.
END.

IF tt-param.tg-shirley THEN DO.
   FIND mp-entr-mat WHERE
        mp-entr-mat.cod-emit = tt-param.cod-fornec AND
        mp-entr-mat.nro-docto = tt-param.nr-nota-fis EXCLUSIVE-LOCK.
   IF NOT AVAIL mp-entr-mat THEN DO.
      PUT STREAM str-rp
          "Nota Fiscal n∆o Encontrada na Descarga de Fardos..." SKIP
          "Fornecedor:" tt-param.cod-fornec SKIP
          "Nota Fiscal:" tt-param.nr-nota-fis
          SKIP.
   END.
   ELSE
      ASSIGN mp-entr-mat.disperdicio = tt-param.residuos.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.

/*-------- Procedures ---------*/
PROCEDURE pi-imp-c.
    DEF VAR l-erro-c AS LOG INIT NO.
    DEF VAR i-fornec AS INT.

    ASSIGN c-arq-dbf = ENTRY(NUM-ENTRIES(tt-param.arq-entrada,"\"),tt-param.arq-entrada,"\")
           c-arq-err = SESSION:TEMP-DIRECTORY + REPLACE(c-arq-dbf,"DBF","ERR")
           c-arq-dump = SESSION:TEMP-DIRECTORY + REPLACE(c-arq-dbf,"DBF","D").

    ASSIGN c-comando = "K:\bin\dbf.exe 1 1 " + tt-param.arq-entrada + " " +
                       c-arq-err + " > " + c-arq-dump.

    OS-COMMAND SILENT VALUE(c-comando).

    FOR EACH tt-fibro-c.
        DELETE tt-fibro-c.
    END.

    INPUT STREAM s-imp FROM VALUE(c-arq-dump) NO-ECHO.
    REPEAT.
        CREATE tt-fibro-c.
        IMPORT STREAM s-imp tt-fibro-c.
        ASSIGN tt-fibro-c.nr-arquivo = tt-param.nr-arq-dbf
               tt-fibro-c.dt-analise = TODAY.
    END.

    FOR EACH tt-fibro-c WHERE
             tt-fibro-c.nfiscal <> 0 EXCLUSIVE-LOCK.

        ASSIGN i-fornec = INT(tt-fibro-c.fornecedor) NO-ERROR.
        IF i-fornec = 0 THEN DO.
           PUT STREAM str-rp
               "Erro no Codigo do Fornecedor Enviado pelo Fibr¢grafo... " 
               SKIP.
           ASSIGN l-erro-c = YES.
           LEAVE.
        END.

        ASSIGN tt-fibro-c.fc1     = tt-fibro-c.fc1 / 1000
               tt-fibro-c.minsl1  = tt-fibro-c.minsl1 / 100
               tt-fibro-c.iv1     = tt-fibro-c.iv1 / 100
               tt-fibro-c.fc2     = tt-fibro-c.fc2 / 1000
               tt-fibro-c.minsl2  = tt-fibro-c.minsl2 / 100
               tt-fibro-c.iv2     = tt-fibro-c.iv2 / 100
               tt-fibro-c.minunif = tt-fibro-c.minunif / 100.

        FIND mp-entr-mat WHERE
             mp-entr-mat.cod-emit = INT(tt-fibro-c.fornecedor) AND
             mp-entr-mat.nro-docto = tt-fibro-c.nfiscal
             NO-LOCK NO-ERROR.

        IF NOT AVAIL mp-entr-mat THEN DO.
           PUT STREAM str-rp
               "Nota Fiscal " tt-fibro-c.nfiscal SKIP
               "Fornecedor:" tt-fibro-c.fornecedor SKIP
               "N«O Encontrada no Sistema..." 
               SKIP.
           ASSIGN l-erro-c = YES.
        END.
    END.

    OS-DELETE VALUE(c-arq-err).
    OS-DELETE VALUE(c-arq-dump).
    
    IF l-erro-c = YES THEN
       RETURN 'C-ERROR'.

    FOR EACH tt-fibro-c.
        FIND mp-fibro-c OF tt-fibro-c NO-LOCK NO-ERROR.
        IF NOT AVAIL mp-fibro-c THEN DO.
           CREATE mp-fibro-c.
           BUFFER-COPY tt-fibro-c TO mp-fibro-c.
        END.
    END.

END PROCEDURE.

PROCEDURE pi-imp-f.
    DEF VAR l-erro-f AS LOG INIT NO.
    ASSIGN c-arq-dbf = ENTRY(NUM-ENTRIES(tt-param.arq-entrada-f,"\"),tt-param.arq-entrada-f,"\")
           c-arq-err = SESSION:TEMP-DIRECTORY + REPLACE(c-arq-dbf,"DBF","ERR")
           c-arq-dump = SESSION:TEMP-DIRECTORY + REPLACE(c-arq-dbf,"DBF","D").
    
    ASSIGN c-comando = "K:\bin\dbf.exe 1 1 " + tt-param.arq-entrada-f + " " +
                       c-arq-err + " > " + c-arq-dump.

    OS-COMMAND SILENT VALUE(c-comando).

    FOR EACH tt-fibro-f.
        DELETE tt-fibro-f.
    END.

    INPUT STREAM s-imp FROM VALUE(c-arq-dump) NO-ECHO.
    REPEAT.
        CREATE tt-fibro-f.
        IMPORT STREAM s-imp tt-fibro-f.
        ASSIGN tt-fibro-f.nr-arquivo = tt-param.nr-arq-dbf. 
    END.

    FOR EACH tt-fibro-f WHERE
             tt-fibro-f.fardo <> 0 EXCLUSIVE-LOCK.

        FIND mp-fibro-c OF tt-fibro-f NO-LOCK NO-ERROR.

        IF NOT AVAIL mp-fibro-c THEN DO.
           PUT STREAM str-rp
                "Inconsistencia na Base, Arquivo de Cabeáalho do Fibr¢grafo " SKIP
                "N«O Encontrado no Sistema..." 
                SKIP.
            ASSIGN l-erro-f = YES.
        END.

        FIND mp-entr-mat WHERE
             mp-entr-mat.cod-emit = INT(mp-fibro-c.fornecedor) AND
             mp-entr-mat.nro-docto = mp-fibro-c.nfiscal
             NO-LOCK NO-ERROR.

        FIND mp-fardo WHERE
             mp-fardo.nr-cdr = mp-entr-mat.nr-cdr AND
             INT(SUBSTR(STRING(mp-fardo.nr-fardo,"99999999"),5,4)) = tt-fibro-f.fardo
             SHARE-LOCK NO-ERROR.

        IF NOT AVAIL mp-fardo THEN DO.
           PUT STREAM str-rp
               "Nota Fiscal " mp-fibro-c.nfiscal SKIP
               "Fornecedor:" mp-fibro-c.fornecedor  SKIP
               "Fardo:" tt-fibro-f.fardo SKIP
               "N«O Encontrado no Sistema..." 
               SKIP.
           ASSIGN l-erro-f = YES.
        END.
        ELSE DO.
           ASSIGN tt-fibro-f.sl1 = tt-fibro-f.sl1 / 100
                  tt-fibro-f.sl2 = tt-fibro-f.sl2 / 100
                  tt-fibro-f.unif = tt-fibro-f.unif / 100
                  tt-fibro-f.z = ''.
        END.
    END.

    OS-DELETE VALUE(c-arq-err).
    OS-DELETE VALUE(c-arq-dump).

    IF l-erro-f = YES THEN
       RETURN 'F-ERROR'.
       
    FOR EACH tt-fibro-f.
        FIND mp-fibro-f OF tt-fibro-f NO-LOCK NO-ERROR.
        IF NOT AVAIL mp-fibro-f THEN DO.
           CREATE mp-fibro-f.
           BUFFER-COPY tt-fibro-f TO mp-fibro-f.
        END.
    END.
END PROCEDURE.
