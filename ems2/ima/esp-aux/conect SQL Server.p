 
 //https://www.w3schools.com/asp/met_comm_createparameter.asp
 
 /* A sample procedure to run stored procedure using an ADO connection */
 
    Def var ObjRecordSet  as com-handle no-undo.
    Def var ObjConnection as com-handle no-undo.
    Def var ObjCommand    as com-handle no-undo.
 
    Def var ODBC-DSN      as character no-undo.
    Def var ODBC-SERVER  as character no-undo.
    Def var ODBC-USERID  as character no-undo.
    Def var ODBC-PASSWD  as character no-undo.
    Def var ODBC-QUERY    as character no-undo.
    Def var ODBC-STATUS  as character no-undo.
    Def var ODBC-RECCOUNT as integer  no-undo.
    Def var ODBC-NULL    as character no-undo.
    Def var ODBC-CURSOR  as integer  no-undo.
    Def var sCreate      as character no-undo.
    Def var sDrop        as character no-undo.
    Def var sSP          as character no-undo.
 
    /*  If not executing against a sports2000 like database this temp table will need to be redefined */         
    DEFINE TEMP-TABLE tt
        FIELD CustNum AS integer
        FIELD cName AS character
        FIELD address AS character
        FIELD address2 AS character
        FIELD City AS character
        FIELD State AS character
        FIELD Country AS character
        FIELD Phone AS character
        FIELD Contact AS character
        FIELD SalesRep AS character
        FIELD Comments AS character
        FIELD CreditLimit AS decimal
        FIELD Balance AS decimal
        FIELD Terms AS character
        FIELD Discount AS integer
        FIELD PostalCode AS character
        FIELD Fax AS character
        FIELD EmailAddress AS character .
    /*
    DEFINE QUERY q1 FOR tt SCROLLING.
    DEFINE BROWSE b1 QUERY q1 NO-LOCK
            DISPLAY custnum cname address address2 City State Country
                    Phone Contact SalesRep Comments CreditLimit Balance
                    Terms Discount PostalCode Fax EmailAddress
            WITH NO-ROW-MARKERS SEPARATORS SIZE 70 BY 12.62 EXPANDABLE.
 
    DEFINE FRAME f1 b1  WITH NO-BOX.
   
      sCreate = "create proc sp_AdoTest " +
                "as "  +
                "SELECT * FROM Customer WHERE "  +
                "Country = 'Finland' "  +
                " " .
 
      sDrop  = "if exists "  +
                "(select * from sysobjects where "  +
                "id = object_id('dbo.sp_AdoTest') and " +
                "sysstat & 0xf = 4)"  +
                "drop procedure dbo.sp_AdoTest" .
 
      sSP    = "sp_Adotest" .
 
 
    /* Create the connection object for the link to SQL */
    Create "ADODB.Connection" ObjConnection.
    /* Create a recordset object ready to return the data */
    Create "ADODB.RecordSet"  ObjRecordSet.
    /* Create a command object for sending the SQL statement */
    Create "ADODB.Command"    ObjCommand.
 
    /* Change the below values as necessary */                       
 
    Assign ODBC-DSN    = "YouDataSourceName"    /* The ODBC DSN */
            ODBC-SERVER = "localhost"    /* The name of the server hosting the SQL DB and DSN */
            ODBC-USERID = "YourLoginName"    /* The user id for access to the SQL Database */
            ODBC-PASSWD = "YourPassword"      /* Password required by above user-id */
            ODBC-QUERY  = "SELECT * from customer".
   
   
    /* Open up the connection to the ODBC Layer */
    ObjConnection:Open ( "data source=" + ODBC-DSN + ";server=" +
                          ODBC-SERVER, ODBC-USERID, ODBC-PASSWD, 0 ) no-error.
 
    Assign ObjCommand:ActiveConnection  = ObjConnection
            ObjCommand:CommandText      = sDrop
            ObjCommand:CommandType      = 1 /* adCmdText */
            ObjRecordSet                = ObjCommand:Execute ( OUTPUT ODBC-NULL, "", 1 ).
    Release object ObjRecordSet  no-error.
 
    Assign ObjCommand:ActiveConnection  = ObjConnection
            ObjCommand:CommandText      = sCreate
            ObjCommand:CommandType      = 1 /* adCmdText */
            ObjRecordSet                = ObjCommand:Execute ( OUTPUT ODBC-NULL, "", 1 ).
    Release object ObjRecordSet  no-error.
 
   
    Assign ObjCommand:ActiveConnection  = ObjConnection
            ObjCommand:CommandText      = "sp_AdoTest"
            ObjCommand:CommandType      = 1 /* adCmdText */
            ObjConnection:CursorLocation = 3 /* adUseClient */
            ObjRecordSet                = ObjCommand:Execute(OUTPUT ODBC-NULL, "", 1)
            ODBC-RECCOUNT                = ObjRecordSet:RecordCount.
 
      MESSAGE "Number od records returned: " ODBC-RECCOUNT
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
   
      /* Check for connection errors */
    If ( error-status:num-messages > 0 ) then
        ODBC-STATUS = "Error: Could not establish connection.".
 
    Else
    DO:
      /* Have we returned any rows ? */
      If ( ODBC-RECCOUNT > 0 ) and not ( ODBC-RECCOUNT = ? ) then
      Do:
//&n.       "drop procedure dbo.sp_AdoTest" .
 
      sSP    = "sp_Adotest" .
    */
 
    /* Create the connection object for the link to SQL */
    Create "ADODB.Connection" ObjConnection.
    /* Create a recordset object ready to return the data */
    Create "ADODB.RecordSet"  ObjRecordSet.
    /* Create a command object for sending the SQL statement */
    Create "ADODB.Command"    ObjCommand.
 
    /* Change the below values as necessary */                       
 
    Assign //ODBC-DSN    = "DMPACESSOII"    /* The ODBC DSN */
           ODBC-DSN    = "Provider=SQLNCLI11;Server=192.168.0.36/SQLEXPRESS;DataBase=TSS_NFE_TESTE;"
           ODBC-SERVER = "192.168.0.36/SQLEXPRESS"    /* The name of the server hosting the SQL DB and DSN */
           ODBC-USERID = "sa"    /* The user id for access to the SQL Database */
           ODBC-PASSWD = "H0i9#taB1"      /* Password required by above user-id */
           //ODBC-QUERY  = "SELECT TOP (1000) [CD_SITUACAO_PESSOA] ,[DS_SITUACAO_PESSOA] ,[TP_ACESSO] FROM [DMPACESSOII].[dbo].[SITUACAO_PESSOA]".
           ODBC-QUERY  = "select * from sped050 where nfe_id like '%0059820%'".

   
   
    /* Open up the connection to the ODBC Layer */
    //ObjConnection:Open ( "data source=" + ODBC-DSN + ";server=" + ODBC-SERVER, ODBC-USERID, ODBC-PASSWD, 0 ) no-error.
    ObjConnection:Open (ODBC-DSN, ODBC-USERID, ODBC-PASSWD, 0 ) no-error.
 
    /*Assign ObjCommand:ActiveConnection  = ObjConnection
            ObjCommand:CommandText      = sDrop
            ObjCommand:CommandType      = 1 /* adCmdText */
            ObjRecordSet                = ObjCommand:Execute ( OUTPUT ODBC-NULL, "", 1 ).
    Release object ObjRecordSet  no-error.
 
    Assign ObjCommand:ActiveConnection  = ObjConnection
            ObjCommand:CommandText      = sCreate
            ObjCommand:CommandType      = 1 /* adCmdText */
            ObjRecordSet                = ObjCommand:Execute ( OUTPUT ODBC-NULL, "", 1 ).
    Release object ObjRecordSet  no-error.*/
 
   
    Assign ObjCommand:ActiveConnection   = ObjConnection
            ObjCommand:CommandText       = ODBC-QUERY
            ObjCommand:CommandType       = 1 /* adCmdText */
            ObjConnection:CursorLocation = 3 /* adUseClient */
            ObjRecordSet                 = ObjCommand:Execute(OUTPUT ODBC-NULL, "", 1)
            ODBC-RECCOUNT                = ObjRecordSet:RecordCount.
 
      MESSAGE "Number od records returned: " ODBC-RECCOUNT
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
   
      /* Check for connection errors */
    If ( error-status:num-messages > 0 ) then
        ODBC-STATUS = "Error: Could not establish connection.".
 
    Else
    DO:
      /* Have we returned any rows ? */
      If ( ODBC-RECCOUNT > 0 ) and not ( ODBC-RECCOUNT = ? ) then
      Do:
        //&n.bsp;       ObjRecordSet:MoveFirst no-error.
 
        Do while ODBC-CURSOR < ODBC-RECCOUNT:
          /* Display the data from the query (or create a Progress temp-table for future use) */
          /* Display ObjRecordSet:Fields ("name"):Value format "x(20)". */

            MESSAGE ObjRecordSet:Fields ("CD_SITUACAO_PESSOA"):VALUE SKIP
                    ObjRecordSet:Fields ("DS_SITUACAO_PESSOA"):VALUE SKIP
                    ObjRecordSet:Fields ("TP_ACESSO"):Value
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

            /*CREATE tt.
            ASSIGN tt.cName = ObjRecordSet:Fields ("name"):Value
                  tt.custnum = ObjRecordSet:Fields ("custnum"):Value
                  tt.address = ObjRecordSet:Fields ("address"):Value
                  tt.address2 = ObjRecordSet:Fields ("address2"):Value   
                  tt.City = ObjRecordSet:Fields ("City"):Value
                  tt.State = ObjRecordSet:Fields ("State"):Value
                  tt.Country = ObjRecordSet:Fields ("Country"):Value
                  tt.Phone = ObjRecordSet:Fields ("Phone"):Value
                  tt.Contact = ObjRecordSet:Fields ("Contact"):Value
                  tt.SalesRep = ObjRecordSet:Fields ("SalesRep"):Value
                  tt.Comments = ObjRecordSet:Fields ("Comments"):Value
                  tt.CreditLimit = ObjRecordSet:Fields ("CreditLimit"):Value
                  tt.Balance = ObjRecordSet:Fields ("Balance"):Value
                  tt.Terms = ObjRecordSet:Fields ("Terms"):Value
                  tt.Discount = ObjRecordSet:Fields ("Discount"):Value
                  tt.PostalCode = ObjRecordSet:Fields ("PostalCode"):Value
                  tt.Fax = ObjRecordSet:Fields ("Fax"):Value
                  tt.EmailAddress = ObjRecordSet:Fields ("EmailAddress"):Value.*/
 
          Assign ODBC-CURSOR = ODBC-CURSOR + 1.
          ObjRecordSet:MoveNext no-error.
        End. /* retrieved a single data row */
 
      End. /* retrieved all data rows */
      Else
        Assign ODBC-STATUS = "No records found.".
 
      /* Close the ADO connection */
      ObjConnection:Close no-error.
 
    End. /* The connection opened correctly */
 
 
    /* Don't forget to release the memory!! */
    Release object ObjConnection no-error.
    Release object ObjCommand    no-error.
    Release object ObjRecordSet  no-error.
 
    Assign ObjConnection = ? ObjCommand = ? ObjRecordSet = ? .
 
    OPEN QUERY q1 FOR EACH tt.
    ENABLE ALL WITH FRAME f1.
 
    WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
    
