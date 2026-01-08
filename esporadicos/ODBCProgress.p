/* A sample procedure to test an ADO connection */
 SESSION:DEBUG-ALERT = yes.
DEFINE VARIABLE ObjRecordSet AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE ObjConnection AS COM-HANDLE NO-UNDO .
DEFINE VARIABLE ObjCommand AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE ODBC-DSN AS CHARACTER NO-UNDO.
DEFINE VARIABLE ODBC-SERVER AS CHARACTER NO-UNDO.
DEFINE VARIABLE ODBC-USERID AS CHARACTER NO-UNDO.
DEFINE VARIABLE ODBC-PASSWD AS CHARACTER NO-UNDO.
DEFINE VARIABLE ODBC-QUERY AS CHARACTER NO-UNDO.
DEFINE VARIABLE ODBC-STATUS AS CHARACTER NO-UNDO.
DEFINE VARIABLE ODBC-RECCOUNT AS INTEGER NO-UNDO.
DEFINE VARIABLE ODBC-NULL AS CHARACTER NO-UNDO.
DEFINE VARIABLE ODBC-CURSOR AS INTEGER NO-UNDO.
DEFINE VARIABLE d1 AS DATETIME    NO-UNDO.
DEFINE VARIABLE d2 AS DATETIME    NO-UNDO.
 


ASSIGN d1 = NOW.

/* If not executing against a sports2000 like database this temp table will need to be redefined */
DEFINE TEMP-TABLE tt LIKE emitente.

/*DEFINE QUERY q1 FOR tt SCROLLING.
DEFINE BROWSE b1 QUERY q1 NO-LOCK
DISPLAY cod-emitente nome-emit 
    WITH NO-ROW-MARKERS SEPARATORS SIZE 70 BY 12.62 EXPANDABLE.
DEFINE FRAME f1 b1 WITH NO-BOX.                                */



/* Create the connection object for the link to SQL */
CREATE "ADODB.Connection" ObjConnection.
/* Create a recordset object ready to return the data */
CREATE "ADODB.RecordSet" ObjRecordSet.
/* Create a command object for sending the SQL statement */
CREATE "ADODB.Command" ObjCommand.
 
/* Change the below values as necessary */
ASSIGN ODBC-DSN = "comumTst" /* The ODBC DSN */
       ODBC-SERVER = "192.168.0.39:20133" /* The name of the server hosting the SQL DB and DSN */
       ODBC-USERID = "sysprogress" /* The user id for access to the SQL Database */
       ODBC-PASSWD = "sysprogress" /* Password required by above user-id */
       ODBC-QUERY = 'SELECt "cod-emitente" as cod_emitente, "nome-emit" as nome_emit, "nome-abrev" as nome_abrev from pub.emitente '.
 
/* Open up the connecti.on to the ODBC Layer */
ObjConnection:OPEN ( "data source=" + ODBC-DSN + ";server=" + ODBC-SERVER, ODBC-USERID, ODBC-PASSWD, 0 ) NO-ERROR .
 
/* Check for connection errors */
IF ( ERROR-STATUS:NUM-MESSAGES > 0 ) THEN DO:
    ODBC-STATUS = "Error: Could not establish connection.".
    MESSAGE odbc-status
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.
ELSE DO:
   /* MESSAGE 'conectou'
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    ASSIGN ObjCommand:ActiveConnection  = ObjConnection
           ObjCommand:CommandText       = ODBC-QUERY
           ObjCommand:CommandType       = 1 /* adCmdText */
           ObjConnection:CursorLocation = 3 /* adUseClient */
           ObjRecordSet:CursorType      = 3 /* adOpenStatic */
           ObjRecordSet = ObjCommand:EXECUTE ( OUTPUT ODBC-NULL, "", 32 )
           ODBC-RECCOUNT = ObjRecordSet:RecordCount.
           
           ASSIGN d2 =  NOW.
           
    /* Have we returned any rows ? */
    /*IF ( ODBC-RECCOUNT > 0 ) AND NOT ( ODBC-RECCOUNT = ? ) THEN
        DO:
        ObjRecordSet:MoveFirst NO-ERROR.
        
            DO WHILE ODBC-CURSOR < ODBC-RECCOUNT:
                /* Display the data from the query (or create a Progress temp-table for future use) */
                /* Display ObjRecordSet:Fields ("name"):Value format "x(20)". */

                /*MESSAGE ObjRecordSet:FIELDS("cod_emitente"):VALUE
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
                IF  ObjRecordSet:FIELDS("cod_emitente"):VALUE <> '' AND ObjRecordSet:FIELDS("nome_emit"):VALUE <> ''  THEN DO:
                    CREATE tt.
                    ASSIGN tt.cod-Emitente      = ObjRecordSet:FIELDS("cod_emitente"):VALUE
                           tt.nome-emit         = ObjRecordSet:FIELDS("nome_emit"):VALUE
                           tt.nome-abrev        = ObjRecordSet:FIELDS("nome_abrev"):VALUE.
                END.
                                
                ASSIGN ODBC-CURSOR = ODBC-CURSOR + 1.
                ObjRecordSet:MoveNext NO-ERROR.
            END. /* retrieved a single data row */
        
        END. /* retrieved all data rows */
    ELSE DO:
        ASSIGN ODBC-STATUS = "No records found.".
        MESSAGE 'oi'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
    */
    
    /* Close the ADO connection */
    ObjConnection:CLOSE NO-ERROR.
 
END. /* The connection opened correctly */

DISP INTERVAL(d2,d1,'milliseconds'). 
 
/* Don't forget to release the memory!! */
RELEASE OBJECT ObjConnection NO-ERROR .
RELEASE OBJECT ObjCommand NO-ERROR .
RELEASE OBJECT ObjRecordSet NO-ERROR .
 
ASSIGN ObjConnection = ? 
       ObjCommand = ? 
       ObjRecordSet = ?.
 
/*OPEN QUERY q1 FOR EACH tt.
ENABLE ALL WITH FRAME f1.
 
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.*/
