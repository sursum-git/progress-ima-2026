{lisa/etqlisa.i}
{esp/util.i}


LOG-MANAGER:LOGGING-LEVEL = 5.
LOG-MANAGER:LOGFILE-NAME =  SESSION:TEMP-DIRECTORY + 'etqs_' + STRING(TIME) + '.txt'.
 DEFINE TEMP-TABLE ttDifAnalise NO-UNDO
        FIELD tipoAnalise       AS CHAR
        FIELD itCodigo          AS CHAR
        FIELD codRefer          AS CHAR
        FIELD nrContainer       AS INT
        FIELD numRolo           AS INT
        FIELD situacao          AS CHAR
        FIELD qtEtq             AS INT
        FIELD qtMetros          AS DECIMAL
        FIELD qtEtqOutro        AS INT
        FIELD qtMetrosOutro     AS DECIMAL 
        FIELD logDivergencia    AS LOGICAL
        FIELD logEtqDuplicada   AS LOGICAL
        FIELD logSemEtqOutro    AS LOGICAL
        FIELD logQtEtqDifer     AS LOGICAL
        FIELD logqtMetrosDif    AS LOGICAL
        FIELD qtEtqDif          AS INT
        FIELD qtMetrosDif       AS DECIMAL
        INDEX unico AS PRIMARY itCodigo codRefer nrContainer numRolo situacao
     .
       

 DEFINE VARIABLE cITem      AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cRef       AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cOrigem    AS CHARACTER   NO-UNDO EXTENT 2.
 DEFINE VARIABLE cDestino   AS CHARACTER   NO-UNDO EXTENT 2.
 DEFINE VARIABLE iCont      AS INTEGER     NO-UNDO.
 DEFINE BUFFER bfAnalise FOR ttEtq.
 UPDATE cItem cRef.


RUN lisa/getDadosEtqLisa.p(cItem,cRef, OUTPUT TABLE ttEtq).

RUN esapi/getDadosEtqMed.p('505',cItem,cRef, INPUT-OUTPUT TABLE ttEtq).

{esp/exportarTabelaCsv.i ttEtq}
//analise 1 - med x lisa
ASSIGN cOrigem[1]  = 'med'
       cDestino[1] = 'lisa'.
//analise 2 - lisa x med
ASSIGN cOrigem[2]  = 'lisa'
       cDestino[2] = 'med'.


REPEAT iCont = 1 TO 2:

    FOR EACH bfAnalise
        WHERE bfAnalise.origem = cOrigem[iCont].

        FIND  ttDifAnalise 
            WHERE ttDifAnalise.itCodigo          =  bfAnalise.itCodigo
            AND   ttDifAnalise.codRefer          =  bfAnalise.codRefer
            AND   ttDifAnalise.nrContainer       =  bfAnalise.nrContainer
            AND   ttDifAnalise.numRolo           =  bfAnalise.numRolo
            AND   ttDifAnalise.situacao          =  bfAnalise.codsituacao
            AND   ttDifAnalise.tipoAnalise       =  cOrigem[iCont] + 'x' + cDestino[iCont]
            NO-ERROR.
        IF NOT AVAIL ttDifAnalise THEN DO:
            CREATE ttDifAnalise.
            ASSIGN 
            ttDifAnalise.itCodigo          =  bfAnalise.itCodigo    
            ttDifAnalise.codRefer          =  bfAnalise.codRefer    
            ttDifAnalise.nrContainer       =  bfAnalise.nrContainer 
            ttDifAnalise.numRolo           =  bfAnalise.numRolo     
            ttDifAnalise.situacao          =  bfAnalise.codsituacao
            ttDifAnalise.tipoAnalise       =  cOrigem[iCont] + 'x' + cDestino[iCont]  .
        END.                                             
        ASSIGN ttDifAnalise.qtEtq           = ttDifAnalise.qtEtq + 1
               ttDifAnalise.qtMetros        = ttDifAnalise.qtMetros  + bfAnalise.quantidade.
        FOR EACH ttEtq
            WHERE ttEtq.origem = cDestino[iCont]
            AND   ttEtq.itCodigo          =  bfAnalise.itCodigo      
            AND   ttEtq.codRefer          =  bfAnalise.codRefer      
            AND   ttEtq.nrContainer       =  bfAnalise.nrContainer   
            AND   ttEtq.numRolo           =  bfAnalise.numRolo       
            AND   ttEtq.codsituacao       =  bfAnalise.codsituacao   :
            ASSIGN ttDifAnalise.qtEtqOutro    = ttDifAnalise.qtEtqOutro + 1
                   ttDifAnalise.qtMetrosOutro = ttDifAnalise.qtMetrosOutro + bfAnalise.quantidade.
        END.   
    END.

END.

FOR EACH ttDifAnalise:
    IF ttDifAnalise.qtEtq > 1 THEN 
       ASSIGN ttDifAnalise.logEtqDuplicada  = YES
              ttDifAnalise.logDivergencia   = YES.

    IF ttDifAnalise.qtEtqOutro = 0 THEN 
       ASSIGN ttDifAnalise.logSemEtqOutro   = YES
              ttDifAnalise.logDivergencia   = YES.

    IF ttDifAnalise.qtEtq - qtEtqOutro <> 0 THEN
       ASSIGN ttDifAnalise.logQtEtqDifer    = YES
              ttDifAnalise.qtEtqDif         =  ttDifAnalise.qtEtq - qtEtqOutro
              ttDifAnalise.logDivergencia   = YES.

    IF ttDifAnalise.qtMetros - ttDifAnalise.qtMetrosOutro <> 0 THEN
      ASSIGN ttDifAnalise.logQtMetrosDif    = YES
             ttDifAnalise.qtMetrosDif       =  ttDifAnalise.qtMetros - qtMetrosOutro
             ttDifAnalise.logDivergencia    = YES.

END.

{esp/exportarTabelaCsv.i ttDifAnalise " where 1 = 1 " 2}

IF SEARCH('excel/analiseEtqLisa.xlsx') <> ? THEN
   OS-COMMAND SILENT VALUE('start excel -t ' + SEARCH('excel/analiseEtqLisa.xlsx') ).
ELSE
   MESSAGE 'NÆo Encontrada planilha'
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.




