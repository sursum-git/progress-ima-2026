/* include de controle de versío */
{include/i-prgvrs.i conciliacao_etq_rp 1.00.00.000}

/* definiªío das temp-tables para recebimento de parÉmetros */
{lisa/conciliacao_etq.i}
{lisa/etqlisa.i}
{esp/util.i}
{lisa\varPropsComuns.i}
DEFINE BUFFER ttEtqMed      FOR ttEtq.
DEFINE TEMP-TABLE ttEtqAux  LIKE ttEtq.
DEFINE TEMP-TABLE ttReg     LIKE etiqueta_lisa .
DEFINE TEMP-TABLE ttRoloRepetido NO-UNDO
    FIELD itCodigo          AS CHAR
    FIELD codRefer          AS CHAR
    FIELD nrcontainer       AS INT
    FIELD numRolo           AS INT
    FIELD quantidade        AS DECIMAL
    FIELD origem            AS CHAR
    INDEX pri IS PRIMARY origem itCodigo codRefer nrContainer numRolo .

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
        FIELD id                AS DECIMAL
        INDEX unico AS PRIMARY itCodigo codRefer nrContainer numRolo situacao
        .

{lisa/ttItemRef.i}
DEFINE VARIABLE cOrigem    AS CHARACTER   NO-UNDO EXTENT 2.
DEFINE VARIABLE cDestino   AS CHARACTER   NO-UNDO EXTENT 2.
DEFINE VARIABLE iCont      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iEtq       AS INTEGER     NO-UNDO.

DEFINE VARIABLE cPlanilhaExcel AS CHARACTER   NO-UNDO.
DEFINE BUFFER bfAnalise FOR ttEtq.

/* Parametros de entrada logica obrigatoria */
DEF TEMP-TABLE tt-raw-digita
   	FIELD raw-digita    AS RAW.

/* recebimento de parÉmetros */
def input parameter raw-param as raw no-undo.
def input parameter TABLE for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padr∆o para variˇveis de relat¢rio  */
{include/i-rpvar.i}

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR h-acomp     AS HANDLE NO-UNDO.
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Buscando Dados *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
{include/i-rpout.i &stream = "stream str-rp"}
/* bloco principal do programa */

IF tt-param.lGerarLogAPI THEN DO:
   LOG-MANAGER:LOGGING-LEVEL = 5.
   LOG-MANAGER:LOGFILE-NAME =  SESSION:TEMP-DIRECTORY + 'etqs_' + STRING(TIME) + '.txt'.
END.

RUN pi-acompanhar IN h-acomp(INPUT " Buscando Etiquetas API LISA  ").
PUT STREAM str-rp "Inicio busca Etiquetas Lisa:" NOW SKIP.

IF tt-param.nrContainer > 0 THEN DO:
   RUN pi-acompanhar IN h-acomp(INPUT " Buscando Itens/Refers."). 
   RUN esapi/getItemRefContainer.p(tt-param.nrContainer, OUTPUT TABLE ttItemRef).
   PUT STREAM str-rp "Container:" tt-param.nrContainer.
END.
ELSE DO:
   PUT STREAM str-rp "Container n∆o informado".
   RETURN 'nok'.
END.


FOR EACH ttItemRef:
    RUN _getDados(ttItemRef.itCodigo,ttItemRef.codRefer).    
END.


 
PUT STREAM str-rp "Inicio Conciliacao:" NOW SKIP.                             
IF tt-Param.lConciliar THEN DO:
    {esp/exportarTabelaCsv.i ttEtq}
    OS-COPY "c:\temp\ttEtq.csv"  value(cDirJson + "/DIVERGENCIAS/dados_" + STRING(TODAY,'9999-99-99') + ".csv") .
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
                ttDifAnalise.tipoAnalise       =  cOrigem[iCont] + 'x' + cDestino[iCont] 
                ttDifAnalise.id                =  bfAnalise.id .
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

    
END.


IF lAtuEtq THEN DO:
    RUN pi-acompanhar IN h-acomp("Atualizando dados Etq conforme LISA").
    PUT STREAM str-rp "Inicio Atualizaá∆o de Etiquetas:" NOW SKIP.  
    RUN atualizarInfEtq.
END.

RUN pi-finalizar IN h-acomp.

{esp/exportarTabelaCsv.i ttDifAnalise " where 1 = 1 " 2}
IF tt-param.lConciliar THEN DO:
    OS-COPY "c:\temp\ttDifAnalise.csv"  value(cDirJson + "/DIVERGENCIAS/diver_" + STRING(TODAY,'9999-99-99') + ".csv") .
    ASSIGN cPlanilhaExcel = 'excel/analiseEtqLisa.xltx'.
    IF SEARCH(cPlanilhaExcel) <> ? THEN DO:
       OS-COMMAND SILENT VALUE('start ' + SEARCH(cPlanilhaExcel) ).
       PUT STREAM str-rp "Excel Gerado com os Dados" SKIP. 
    END.                                                  
    ELSE DO:
       PUT STREAM str-rp "Planilha n∆o Encontrada" SKIP. 
       MESSAGE 'N∆o Encontrada planilha'
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
    
END.






/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &stream = "stream str-rp"}
RETURN "OK":U.     

PROCEDURE atualizarInfEtq:

DEFINE VARIABLE hBo     AS HANDLE      NO-UNDO.
DEFINE VARIABLE numEtq  AS INTEGER     NO-UNDO.
RUN verifRolosDupl.   
//parte das etiquetas da lisa n∆o repetidas
FOR EACH ttEtq
    WHERE ttEtq.origem = 'lisa':
    RUN pi-acompanhar IN h-acomp("Etq:" + ttEtq.idEtq).
    RUN esapi/getNumEtqMed.p(ttEtq.idEtq, OUTPUT numEtq).

    IF numEtq = 0 THEN DO:
       FIND ttRoloRepetido
       WHERE ttRoloRepetido.itCodigo           = ttEtq.itCodigo
            AND   ttRoloRepetido.codRefer       = ttEtq.codRefer
            AND   ttRoloRepetido.nrcontainer    = ttEtq.nrContainer
            AND   ttRoloRepetido.numRolo        = ttEtq.numRolo
            AND   ttRoloRepetido.origem         = ttEtq.origem NO-ERROR.
       IF AVAIL ttRoloRepetido THEN NEXT.
       FIND ttEtqMed 
        WHERE ttEtqMed.ORIGEM    =  'med'
        AND ttEtqMed.itCodigo    =  ttEtq.itCodigo
        AND ttEtqMed.codRefer    =  ttEtq.codRefer
        AND ttEtqMed.nrContainer =  ttEtq.nrContainer
        AND ttEtqMed.numRolo     =  ttEtq.numRolo
        NO-ERROR.
        IF  AVAIL ttEtqMed THEN DO:
            RUN pi-acompanhar IN h-acomp("Criando Relaá∆o ETQ com a LISA:" + ttEtq.idEtq).
            RUN esapi/gravarEtqLisa.p(
                '505',
                 INT(ttEtqMed.idEtq),
                 ttEtqMed.itCodigo,
                 ttEtqMed.codRefer,
                 ttEtqMed.nrContainer,
                 INT(ttEtqMed.pedido),
                 INT(ttEtqMed.prePedido),
                 ttEtq.idEtq,
                 9).
        END.        
    END.
    ELSE DO:
        FIND ttEtqMed
            WHERE ttEtqMed.idEtq = string(numEtq)
            USE-INDEX ind-etq  NO-ERROR.
    END.    

    IF AVAIL ttEtqMed THEN DO:
       RUN pi-acompanhar IN h-acomp("Atualizando Localizaá∆o:" + ttEtq.idEtq).
       RUN esapi/atuLocalizEtq.p('505',ttEtqMed.idEtq,ttEtq.localiz).
    END.


END.


END PROCEDURE.

PROCEDURE verifRolosDupl:
   
    FOR EACH ttEtq.
        FIND ttRoloRepetido
            WHERE ttRoloRepetido.itCodigo       = ttEtq.itCodigo
            AND   ttRoloRepetido.codRefer       = ttEtq.codRefer
            AND   ttRoloRepetido.nrcontainer    = ttEtq.nrContainer
            AND   ttRoloRepetido.numRolo        = ttEtq.numRolo
            AND   ttRoloRepetido.origem         = ttEtq.origem
            NO-ERROR.
        IF NOT AVAIL ttRoloRepetido THEN DO:
           CREATE ttRoloRepetido.
           ASSIGN ttRoloRepetido.itCodigo       = ttEtq.itCodigo      
                  ttRoloRepetido.codRefer       = ttEtq.codRefer      
                  ttRoloRepetido.nrcontainer    = ttEtq.nrContainer   
                  ttRoloRepetido.numRolo        = ttEtq.numRolo 
                  ttRoloRepetido.origem         = ttEtq.origem
                  .
        END.
        ASSIGN ttRoloRepetido.quantidade = ttRoloRepetido.quantidade + 1 .
    END.
    
    FOR EACH ttRoloRepetido
        WHERE ttRoloRepetido.quantidade = 1 .
        DELETE ttRoloRepetido.
    END.





END PROCEDURE.


PROCEDURE _getDados.

    DEFINE INPUT  PARAMETER pItCodigo AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodRefer AS CHARACTER   NO-UNDO.
    
    RUN pi-acompanhar IN h-acomp(INPUT "LISA - Item:" + pItCodigo + "-Refer:" + pCodRefer).
    
    PUT STREAM str-rp "Inicio busca Etiquetas LISA:" NOW SKIP. 
    EMPTY TEMP-TABLE ttEtqAux.
    RUN lisa/getDadosEtqLisa.p(pItCodigo,
                           pCodRefer, 
                           NO,
                           01.01.2001,
                           01.01.2999,
                           OUTPUT TABLE ttEtqAux).
     RUN pi-acompanhar IN h-acomp(INPUT "MED - Item:" + pItCodigo + "-Refer:" + pCodRefer).
    PUT STREAM str-rp "Inicio busca Etiquetas Med:" NOW SKIP.                          
    RUN pi-acompanhar IN h-acomp(INPUT " Buscando Etiquetas MED  ").
    RUN esapi/getDadosEtqMed.p('505', // estab. LISA
                               pItCodigo,
                               pCodRefer, 
                               INPUT-OUTPUT TABLE ttEtqAux). 
                               
                               
    FOR EACH ttEtqAux:
        CREATE ttEtq.
        ASSIGN iEtq = iEtq + 1.
        ASSIGN ttEtq.id = iEtq.
        BUFFER-COPY ttEtqAux EXCEPT ttEtqAux.id TO ttEtq.
        
    END.


END PROCEDURE.

/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 etiqueta_lisa_id                 int6        i
   11 cod_estabel                      char        i
   20 num_etiqueta                     inte        i
   30 id_etq_lisa                      char        i
   40 dt_hr_criacao                    datetm
   50 cod_usuario_criacao              char
   60 nr_pedido                        inte        i
   70 pre_pedido                       inte        i
   80 num_origem                       inte        i
*/
