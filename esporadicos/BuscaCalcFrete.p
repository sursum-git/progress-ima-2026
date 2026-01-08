DEFINE VARIABLE vcHost     AS CHARACTER    INITIAL "192.168.0.43"   NO-UNDO.
DEFINE VARIABLE vcPort     AS CHARACTER    INITIAL "8090"        NO-UNDO.
DEFINE VARIABLE vhSocket   AS HANDLE                             NO-UNDO.
 
 
CREATE SOCKET vhSocket.
vhSocket:CONNECT('-H ' + vcHost + ' -S ' + vcPort) NO-ERROR.
    
IF vhSocket:CONNECTED() = FALSE THEN
DO:
    MESSAGE "Connection failure" VIEW-AS ALERT-BOX.
    
    MESSAGE ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX.
    RETURN.
END.
ELSE
   MESSAGE "Connect"
       VIEW-AS ALERT-BOX.
 
vhSocket:SET-READ-RESPONSE-PROCEDURE('getResponse').
/* supposes there is an webspeed app called yourapp.w that receives param1, param2, param3 */
RUN PostRequest (
    INPUT 'http://192.168.0.43:8090/scriptcase/app/Correios/bl_calc_frete/bl_calc_frete.php?cepDestino=60120100&cepOrigem=32241395&peso=1&servico=04162', 
    INPUT ''
    ).
 
WAIT-FOR READ-RESPONSE OF vhSocket. 
vhSocket:DISCONNECT() NO-ERROR.
DELETE OBJECT vhSocket.
QUIT.
 
PROCEDURE getResponse:
    DEFINE VARIABLE vcWebResp    AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE lSucess      AS LOGICAL          NO-UNDO.
    DEFINE VARIABLE mResponse    AS MEMPTR           NO-UNDO.
    
    IF vhSocket:CONNECTED() = FALSE THEN do:
        MESSAGE 'Not Connected' VIEW-AS ALERT-BOX.
        RETURN.
    END.
    lSucess = TRUE.
        
   
    DO WHILE vhSocket:GET-BYTES-AVAILABLE() > 0:
            
         SET-SIZE(mResponse) = vhSocket:GET-BYTES-AVAILABLE() + 1.
         SET-BYTE-ORDER(mResponse) = BIG-ENDIAN.
         vhSocket:READ(mResponse,1,1,vhSocket:GET-BYTES-AVAILABLE()).
         vcWebResp = vcWebResp + GET-STRING(mResponse,1).
         
    END.
    MESSAGE vcWebResp
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
    /*
    *PUT HERE THE CODE TO MANIPULATE THE ANSWER
    */
    
END.
PROCEDURE PostRequest:
    DEFINE VARIABLE vcRequest      AS CHARACTER.
    DEFINE VARIABLE mRequest       AS MEMPTR.
    DEFINE INPUT PARAMETER postUrl AS CHAR. 
    /* URL that will send the data. It must be all the path after the server.  IE:/scripts/cgiip.exe/WService=wsbroker1/myApp.htm */
    DEFINE INPUT PARAMETER postData AS CHAR.
    /* Parameters to be sent in the format paramName=value&paramName=value&paramName=value */
    vcRequest =
        'GET ' +
        postUrl +
        ' HTTP/1.0~r~n' +
        'Content-Type: text/html~r~n' +
        'Content-Length:' + string(LENGTH(postData)) +
        '~r~n' + '~r~n' +
        postData + '~r~n'.
 
    MESSAGE vcREquest
        VIEW-AS ALERT-BOX.
 
    SET-SIZE(mRequest)            = 0.
    SET-SIZE(mRequest)            = LENGTH(vcRequest) + 1.
    SET-BYTE-ORDER(mRequest)      = BIG-ENDIAN.
    PUT-STRING(mRequest,1)        = vcRequest .
 
    vhSocket:WRITE(mRequest, 1, LENGTH(vcRequest)).
END PROCEDURE.
