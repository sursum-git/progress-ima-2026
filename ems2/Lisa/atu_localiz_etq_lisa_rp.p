/* include de controle de versío */
{include/i-prgvrs.i conciliacao_etq_rp 1.00.00.000}

/* definiªío das temp-tables para recebimento de parÉmetros */
{lisa/conciliacao_etq.i}
{lisa/etqlisa.i}
{esp/util.i}
{lisa\varPropsComuns.i}
{utp/ut-glob.i}

DEFINE BUFFER ttEtqMed FOR ttEtq.                                               

DEFINE TEMP-TABLE ttReg LIKE etiqueta_lisa .
DEFINE TEMP-TABLE ttRoloRepetido NO-UNDO
    FIELD itCodigo      AS CHAR
    FIELD codRefer      AS CHAR
    FIELD nrcontainer   AS INT
    FIELD numRolo       AS INT
    FIELD quantidade    AS DECIMAL
    FIELD origem        AS CHAR
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
     
DEFINE TEMP-TABLE ttResult NO-UNDO
    FIELD numEtiqueta   AS CHAR
    FIELD resultado     AS CHAR
    FIELD etqMEd        AS INT
    INDEX primario IS PRIMARY numEtiqueta.



/* Parametros de entrada logica obrigatoria */
DEF TEMP-TABLE tt-raw-digita
   	FIELD raw-digita    AS RAW.

/* recebimento de parÉmetros */
def input parameter raw-param as raw no-undo.
def input parameter TABLE for tt-raw-digita.


DEFINE VARIABLE cOrigem         AS CHARACTER   NO-UNDO EXTENT 2.
DEFINE VARIABLE cDestino        AS CHARACTER   NO-UNDO EXTENT 2.
DEFINE VARIABLE iCont           AS INTEGER     NO-UNDO.
DEFINE VARIABLE idTransacao     AS INTEGER     NO-UNDO.
DEFINE VARIABLE lAmbiguidade    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cPlanilhaExcel  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEstabSC        AS CHARACTER   NO-UNDO INIT '505'.
DEFINE VARIABLE cErro           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lErro           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE hBoTransCorte   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoMsgCorte     AS HANDLE      NO-UNDO.

DEFINE BUFFER bfAnalise FOR ttEtq.

create tt-param.
raw-transfer raw-param to tt-param.

RUN esbo/boMsg.p                PERSIST SET hBoMsgCorte.
RUN esbo/boTransacoes.p         PERSIST SET hBoTransCorte. 

RUN iniciarTransacao IN hBoTransCorte.
RUN gerarTransacao   IN hBoTransCorte(INPUT 'registrarCorteNaAtuLocaliz',
                                 INPUT c-seg-usuario,
                                 INPUT 201,
                                 INPUT 'corte-atu-localiz-' + STRING(TIME) + STRING(RANDOM(1,99999)) ,
                                 OUTPUT idTransacao                                    
                                ).


/* include padr∆o para variˇveis de relat¢rio  */
{include/i-rpvar.i}

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR h-acomp     AS HANDLE NO-UNDO.
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Buscando Dados *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
/* bloco principal do programa */

IF tt-param.lGerarLogAPI THEN DO:
   LOG-MANAGER:LOGGING-LEVEL = 5.
   LOG-MANAGER:LOGFILE-NAME =  SESSION:TEMP-DIRECTORY + 'etqs_' + STRING(TIME) + '.txt'.
END.

RUN pi-acompanhar IN h-acomp(INPUT " Buscando Etiquetas API LISA  ").
IF tt-param.lDtEndInf  = NO THEN  DO:
    ASSIGN tt-param.dtIniEnd = TODAY
           tt-param.dtFimEnd = TODAY .     
END.

/*MESSAGE   tt-param.lDtEndInf SKIP
          tt-param.dtIniEnd  SKIP
          tt-param.dtFimEnd SKIP
          
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

RUN lisa/getDadosEtqLisa.p(tt-param.itCodigo,
                           tt-param.codRefer, 
                           YES,
                           tt-param.dtIniEnd,
                           tt-param.dtFimEnd,
                           OUTPUT TABLE ttEtq BY-REFERENCE).
                           
                         
             
RUN pi-acompanhar IN h-acomp("Atualizando dados Etq conforme LISA").
{include/i-rpout.i &stream = "stream str-rp"}
{esp/exportarTabelacsv3.i ttEtq " " " " "ttetq"}
RUN atualizarInfEtq.


RUN pi-finalizar IN h-acomp.

{esp/exportarTabelaCsv.i ttResult " " 2}


/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &stream = "stream str-rp"}
RETURN "OK":U.     

FINALLY:

 IF VALID-HANDLE(hBoMsgCorte) THEN DO:
     DELETE PROCEDURE hBoMsgCorte.     
 END.
 IF VALID-HANDLE(hBoTransCorte) THEN  DO:
    IF  NOT lErro THEN
    DO:             
        RUN finalizarTransacao     IN hBoTransCorte(1). 
    END.
    ELSE DO:
        RUN finalizarTransacao     IN hBoTransCorte(2). 
    END.
    
    DELETE PROCEDURE hBoTransCorte.     
 END.
 

END FINALLY.


PROCEDURE atualizarInfEtq:

    DEFINE VARIABLE hBo         AS HANDLE      NO-UNDO.
    DEFINE VARIABLE numEtqMed  AS INTEGER      NO-UNDO.
    DEFINE VARIABLE localizAnt  AS CHARACTER   NO-UNDO.
    
    PUT STREAM str-rp "Inicio AtualizarInfEtq"  NOW SKIP.
    
    
    //parte das etiquetas da lisa n∆o repetidas
    FOR EACH ttEtq
        WHERE ttEtq.origem = 'lisa':
        CREATE ttResult.
        ASSIGN ttResult.numEtiqueta = ttEtq.idEtq.
        RUN pi-acompanhar IN h-acomp("Etq:" + ttEtq.idEtq).
        
        
        RUN esapi/getNumEtqMed2.p(ttEtq.idEtq, OUTPUT numEtqMed, OUTPUT lAmbiguidade).
        
        PUT STREAM str-rp "Item:" ttEtq.itCodigo "- Refer." ttEtq.codRefer "- Container:" ttEtq.nrContainer "- Rolo:" ttEtq.numRolo "- Etiqueta MED:" numEtqMed "- Ambiguo:" lAmbiguidade NOW SKIP.
        PUT FILL("-",80) SKIP.
        // IMPORTANTE: s¢ considera etiquetas em estoque para avaliar a ambiguidade
        IF lAmbiguidade THEN DO:
            ASSIGN ttResult.resultado = "Ambiguidade encontrada".
            NEXT.    
        END.
       
        IF numEtqMed = 0 THEN DO:
           PUT STREAM str-rp "Etiqueta da MED n∆o encontrada pelo numero da etiqueta da lisa"  NOW SKIP.
           FOR FIRST ob-etiqueta FIELDS(num-etiqueta it-codigo cod-refer nr-container localiz ) NO-LOCK 
               WHERE ob-etiqueta.it-codigo      = ttEtq.itCodigo
               AND   ob-etiqueta.cod-refer      = ttEtq.codRefer
               AND   ob-etiqueta.nr-container   = ttEtq.nrContainer
               AND   ob-etiqueta.num-rolo       = ttEtq.numRolo
               .
           END.
           IF NOT AVAIL ob-etiqueta THEN DO:
              PUT STREAM str-rp "Etiqueta da MED n∆o encontrada a partir da chave do rolo"  NOW SKIP.
              ASSIGN ttResult.resultado = "Nenhuma Etiqueta encontrada na MED com a CHAVE item-refer-container-rolo:" + ttEtq.itCodigo 
                                            + '-' + ttEtq.codRefer + '-' + string(ttEtq.nrContainer) + '-' + string(ttEtq.numRolo) .
              PUT STREAM str-rp "Vou verificar se Ç uma etiqueta de corte que precisa ser criada"  NOW SKIP.                                             
              RUN _cortarEtq.                                            
              NEXT.
           END.
           ASSIGN ttResult.etqMed = ob-etiqueta.num-etiqueta.
           RUN pi-acompanhar IN h-acomp("Criando Relaá∆o ETQ com a LISA:" + ttEtq.idEtq).
                RUN esapi/gravarEtqLisa.p(
                     cEstabSC,
                     ob-etiqueta.num-etiqueta,
                     ob-etiqueta.it-codigo,
                     ob-etiqueta.cod-refer,
                     ob-etiqueta.nr-container,
                     INT(ttEtq.pedido),
                     INT(ttEtq.prePedido),
                     ttEtq.idEtq,
                     9).  
           PUT STREAM str-rp "Ap¢s gravar o numero da etiqueta da lisa na tabela etiqueta_lisa"  NOW SKIP.          
              
        END.  
        ELSE DO:
            PUT STREAM str-rp "Encontrada a Etiqueta MED:" numEtqMed  NOW SKIP.    
            FOR FIRST ob-etiqueta FIELDS(num-etiqueta cod-estabel localiz) NO-LOCK
                WHERE ob-etiqueta.cod-estabel  = cEstabSc
                AND   ob-etiqueta.num-etiqueta = numEtqMed :
            END.
            ASSIGN ttResult.etqMed = ob-etiqueta.num-etiqueta.
        END.    
        IF AVAIL ob-etiqueta THEN  DO:      
           RUN pi-acompanhar IN h-acomp("Atualizando Localizaá∆o:" + ttEtq.idEtq).
           /*MESSAGE cEstabSC   SKIP
                   ob-etiqueta.num-etiqueta
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
           ASSIGN localizAnt = ob-etiqueta.localiz.    
           RUN esapi/atuLocalizEtq.p(cEstabSC,ob-etiqueta.num-etiqueta,ttEtq.localiz).
           ASSIGN ttResult.resultado = "Etiqueta Localizada. Localizaá∆o Anterior:" + localizAnt + " - Nova localizaá∆o:" + ttEtq.localiz .
        END.
        ELSE DO:       
                     
        END.    
    END.

END PROCEDURE.


PROCEDURE _cortarEtq:
    
   PUT STREAM str-rp "Etq.Original:"  ttEtq.numEtqLisaOri "- Localizacao:" ttEtq.localiz  NOW SKIP.   
   IF ttEtq.numEtqLisaOri <> '' AND ttEtq.localiz = 'gramatura' THEN DO:
      RUN esapi/cortarEtqLisa.p(INPUT idTransacao,
                                INPUT 0,
                                INPUT ttEtq.numEtqLisaOri,
                                INPUT ttEtq.idEtq,
                                INPUT ttEtq.qtEtqLisaOri,
                                INPUT ttEtq.quantidade,
                                INPUT ttEtq.numRolo,
                                INPUT '',
                                INPUT hBoMsgCorte,
                                OUTPUT cErro              
                                ). 
      PUT STREAM str-rp "Erro ao cortar a Etiqueta:" cErro  NOW SKIP.                                   
      IF cErro = '' THEN DO:                
        ASSIGN ttResult.resultado = "Corte sem erros Etiqueta LISA:" + ttEtq.numEtqLisaOri +
                                    " que gerou a etiqueta LISA:" + ttEtq.idEtq .
      END.
      ELSE DO:
        ASSIGN lErro = TRUE.   
         ASSIGN ttResult.resultado = "Corte COM erros Etiqueta LISA:" + ttEtq.numEtqLisaOri +
                                    ":" + cErro .
      END.              
   END.
   ELSE DO:
    ASSIGN ttResult.resultado = "Nenhuma aá∆o feita pois o registro da etiqueta n∆o foi encontrado".
   END.  


END PROCEDURE.


