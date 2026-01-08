DEFINE TEMP-TABLE ttMD
       FIELD pid                AS INT
       FIELD usr                AS CHAR FORMAT 'x(100)'
       FIELD nome               AS CHAR FORMAT 'x(50)'
       FIELD data               AS CHAR FORMAT 'x(30)'
       FIELD banco              AS CHAR.
DEFINE VARIABLE cArqLog         AS CHARACTER   NO-UNDO FORMAT 'x(100)'.
DEFINE VARIABLE cAgora          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAgoraArq       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iAgoraDia       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAgoraHora      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAgoraMinuto    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTempoTimeOut AS INTEGER       NO-UNDO.
DEFINE VARIABLE lDesconsiderar AS LOGICAL      NO-UNDO.
DEFINE VARIABLE indDerrubou      AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE ttExecutar  LIKE ttMD
    FIELD comando AS CHAR FORMAT 'x(500)'.

DEFINE VARIABLE hQuery          AS HANDLE  NO-UNDO.
DEFINE VARIABLE hQueryMD        AS HANDLE  NO-UNDO.
DEFINE VARIABLE hBf             AS HANDLE  NO-UNDO EXTENT 10.
DEFINE VARIABLE iNumVar         AS INTEGER NO-UNDO INITIAL 10. 
DEFINE VARIABLE i               AS INTEGER     NO-UNDO.
DEFINE VARIABLE iDB             AS INTEGER     NO-UNDO.
DEFINE VARIABLE i2              AS INTEGER     NO-UNDO.
DEFINE VARIABLE cListaCampos    AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE cBanco          AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE bhconnect       AS HANDLE      NO-UNDO.
DEFINE VARIABLE bhField         AS HANDLE      NO-UNDO.
DEFINE VARIABLE bhTabelas       AS HANDLE      NO-UNDO EXTENT 18.
DEFINE VARIABLE cCondicao       AS CHARACTER   NO-UNDO FORMAT 'x(60)'.
DEFINE VARIABLE LOG_mata        AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE iDiaUsuario     AS INT64       NO-UNDO.
DEFINE VARIABLE iHoraUsuario    AS INT64       NO-UNDO.
DEFINE VARIABLE iMinutoUsuario  AS INT64       NO-UNDO.

ASSIGN cAgora       = string(NOW,"99/99/9999 HH:MM:SS").
ASSIGN iAgoraDia    = int(SUBSTR(cAgora,1,2)).
ASSIGN iAgoraHora   = int(SUBSTR(cAgora,12,2)).
ASSIGN iAgoraMinuto = int(SUBSTR(cAgora,15,2)).
ASSIGN cAgoraArq    = cAgora.
ASSIGN cAgoraArq    = REPLACE(cAGoraArq,'/','_').
ASSIGN cAgoraArq    = REPLACE(cAGoraArq,':','_').
ASSIGN cAgoraArq    = REPLACE(cAGoraArq,' ','_').
ASSIGN cArqLog      = "/log/mata_usuarios/usuarios" + cAgoraArq + ".txt".
//ASSIGN cArqLog      = "c:\temp\timeout" + cAgoraArq + ".txt".

FIND FIRST im-param
    WHERE im-param.cod-param = 'timeout_banco'
    NO-LOCK NO-ERROR.
IF AVAIL im-param THEN
   ASSIGN iTempoTimeout = int(im-param.val-param).
ELSE
   ASSIGN iTempoTimeOut = 0.
IF iTempoTimeOut <> 0 THEN DO: //se o valor do parametro for zero  o timeout ficar  desabilitado
                                                                                                                                                                 
   REPEAT iDB = 1 TO NUM-DBS:                                                                                                                                    
       ASSIGN cBanco = LDBNAME(iDB).                                                                                                                             
       CREATE QUERY hQueryMD.                                                                                                                                    
       CREATE BUFFER bhconnect  FOR TABLE cBanco + '._connect'.                                                                                                  
       hQueryMD:ADD-BUFFER(bhconnect).                                                                                                                           
       hQueryMD:QUERY-PREPARE('for each ' + cBanco + '._connect no-lock where  _Connect-Pid <> INT(?) AND _Connect-Usr <> INT (?) AND _Connect-Name <> (?)').    
       hQueryMD:QUERY-OPEN.                                                                                                                                      
       REPEAT:                                                                                                                                                   
         hQueryMD:GET-NEXT().                                                                                                                                    
         IF hQueryMD:QUERY-OFF-END THEN LEAVE.                                                                                                                   
         CREATE ttMD.                                                                                                                                            
         ASSIGN ttMD.pid     = bhconnect:BUFFER-FIELD('_connect-pid'):BUFFER-VALUE()                                                                             
                ttMD.usr     = bhconnect:BUFFER-FIELD('_connect-usr'):BUFFER-VALUE()                                                                             
                ttMD.nome    = bhconnect:BUFFER-FIELD('_connect-name'):BUFFER-VALUE()                                                                            
                ttMD.data    = bhconnect:BUFFER-FIELD('_connect-time'):BUFFER-VALUE()                                                                            
                ttMd.banco   = cBanco.          
         
                                                                                                                                                                 
       END.                                                                                                                                                      
       hQueryMD:QUERY-CLOSE().                                                                                                                                   
       bhconnect:BUFFER-RELEASE().                                                                                                                               
       DELETE OBJECT hQueryMD.                                                                                                                                   
       DELETE OBJECT bhconnect.                                                                                                                                  
   END.                                                                                                                                                          
   OUTPUT TO  value("/log/mata_usuarios/usuarios_conectados_" + STRING(TIME) + ".csv").                                                                                                                                                              
   FOR EACH ttMD:  

       EXPORT DELIMITER ";" ttMD.
       RUN desconsiderarUsuario(ttMd.usr, OUTPUT lDesconsiderar).
       IF lDesconsiderar = YES THEN NEXT.

       ASSIGN LOG_mata = NO.                                                                                                                                     
       ASSIGN iDiaUsuario = int(ENTRY(3,ttMD.data,' '))                                                                                                          
              iHoraUsuario = INT(entry                                                                                                                           
                                 (1,                                                                                                                             
                                  ENTRY(4,ttMD.data,' '),                                                                                                        
                                  ":"                                                                                                                            
                                  )                                                                                                                              
                                ).                                                                                                                               
             iMinutoUsuario = INT(entry                                                                                                                          
                                 (2,                                                                                                                             
                                  ENTRY(4,ttMD.data,' '),                                                                                                        
                                  ":"                                                                                                                            
                                  )                                                                                                                              
                                ).                                                                                                                               
   /*     MESSAGE iDiaUsuario                    */                                                                                                              
   /*             iHoraUsuario                   */                                                                                                              
   /*             iMinutoUsuario                 */                                                                                                              
   /*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */                                                                                                              
                                                                                                                                                                 
                                                                                                                                                                 
       IF(iAgoraDia > iDiaUsuario) THEN                                                                                                                          
          ASSIGN LOG_mata = YES.                                                                                                                                 
       ELSE DO:                                                                                                                                                  
          IF iAgoraHora > iHoraUsuario THEN                                                                                                                      
             ASSIGN LOG_mata = YES.                                                                                                                              
          ELSE                                                                                                                                                   
             IF iAgoraMinuto > (iMinutoUsuario + 5) THEN                                                                                                         
                 ASSIGN LOG_mata = YES.                                                                                                                          
                                                                                                                                                                 
       END.                                                                                                                                                      
       IF LOG_mata = YES THEN DO:                                                                                                                                
         CREATE ttExecutar.                                                                                                                                      
         BUFFER-COPY ttMd TO ttExecutar.                                                                                                                         
         ASSIGN ttExecutar.comando = "proshut /datasul/totvs/database/pro/"+ ttMd.banco + " -C disconnect " + STRING(ttMD.usr).                                  
        // OS-COMMAND SILENT VALUE( ).                                                                                                                           
         //EXPORT DELIMITER "|" ttMD .                                                                                                                           
       END.                                                                                                                                                      
   END.  
   OUTPUT CLOSE.
                                                                                                                                                                 
   FIND FIRST ttExecutar NO-ERROR.                                                                                                                               
   IF AVAIL ttExecutar THEN DO:                                                                                                                                  
      OUTPUT TO VALUE(cArqLog) NO-CONVERT.                                                                                                                       
      FOR EACH ttExecutar.                                                                                                                                       
          OS-COMMAND SILENT VALUE(ttExecutar.comando).                                                                                                           
          EXPORT DELIMITER "|" ttExecutar.                                                                                                                       
      END.                                                                                                                                                       
      OUTPUT CLOSE.                                                                                                                                              
   END.  
END.
   

PROCEDURE desconsiderarUsuario:
    DEFINE INPUT  PARAMETER pUsuario AS CHARACTER  FORMAT 'x(15)' NO-UNDO.
    DEFINE OUTPUT PARAMETER pDesconsiderar AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE grupoTimeOut AS CHARACTER   NO-UNDO.

    FIND FIRST PARAM_basic NO-LOCK NO-ERROR .
    ASSIGN grupoTimeOut = param_basic.cod_grp_exec_timeout.

    FIND usuar_grp_usuar WHERE
        usuar_grp_usuar.cod_grp_usuar =  grupoTimeOut
        AND usuar_grp_usuar.cod_usuar =  pUsuario
        NO-LOCK NO-ERROR.

    ASSIGN pDesconsiderar = AVAIL usuar_grp_usuar .
    


END PROCEDURE.










































































   



