{esapi/getNfsPortal.i}
{esapi/analisarJsonObject2.i}
{esp/util.i}
DEFINE INPUT PARAMETER TABLE FOR ttJson.
DEFINE OUTPUT PARAMETER TABLE FOR ttNf.

DEFINE VARIABLE dtIni       AS DATE        NO-UNDO INIT 01.01.2001.
DEFINE VARIABLE dtFim       AS DATE        NO-UNDO INIT 01.01.2999.
DEFINE VARIABLE cDtAux      AS CHARACTER   NO-UNDO.

DEFINE VARIABLE iRepresIni  AS INTEGER     NO-UNDO INIT 0.
DEFINE VARIABLE iRepresFim  AS INTEGER     NO-UNDO INIT 999999.

DEFINE VARIABLE iCliIni  AS INTEGER     NO-UNDO INIT 0.
DEFINE VARIABLE iCliFim  AS INTEGER     NO-UNDO INIT 999999.

RUN setVars.

FOR EACH nota-fiscal 
    FIELDS( nr-nota-fis serie cod-emitente  no-ab-reppri cod-rep nr-pedcli vl-tot-nota val-desconto-total
	        esp-docto cod-estabel cidade estado dt-emis-nota) NO-LOCK
    WHERE   nota-fiscal.idi-sit-nf-eletro = 3
    AND     nota-fiscal.dt-cancela       = ?
    AND     nota-fiscal.dt-emis-nota    >= dtIni
    AND     nota-fiscal.dt-emis-nota    <= dtFim
    AND     nota-fiscal.cod-rep         >= iRepresIni
    AND     nota-fiscal.cod-rep         <= iRepresFim
    AND     nota-fiscal.cod-emitente    >= iCliIni
    AND     nota-fiscal.cod-emitente    <= iCliFim,
    natur-oper OF nota-fiscal NO-LOCK 
    WHERE natur-oper.tp-rec-desp    =  1 
    AND   natur-oper.tipo-compra    <> 3 :
    
    CREATE ttNF.
    BUFFER-COPY nota-fiscal TO ttNf.    
    
END.

PROCEDURE setVars:

    FIND LAST ttJson 
    WHERE ttJson.tag = 'dt_ini' NO-ERROR.
    IF AVAIL ttJson THEN
    DO:
        ASSIGN cDtAux = ttJson.valor.
        RUN convDtApi(cDtAux,OUTPUT dtIni).
        
    END.

    FIND LAST ttJson 
        WHERE ttJson.tag = 'dt_fim' NO-ERROR.
    IF AVAIL ttJson THEN
    DO:
        ASSIGN cDtAux = ttJson.valor.
        RUN convDtApi(cDtAux,OUTPUT dtFim).
        
    END.
    
    FIND LAST ttJson 
    WHERE ttJson.tag = 'repres_ini' NO-ERROR.
    IF AVAIL ttJson THEN
    DO:
       ASSIGN iRepresIni = INT(ttJson.valor).        
    END.
    
     FIND LAST ttJson 
    WHERE ttJson.tag = 'repres_fim' NO-ERROR.
    IF AVAIL ttJson THEN
    DO:
       ASSIGN iRepresFim = INT(ttJson.valor).        
    END.
    
    
    

END PROCEDURE.

PROCEDURE porCliente:   

END PROCEDURE.


PROCEDURE porNota:


END PROCEDURE.


PROCEDURE PorContainer:



END.
