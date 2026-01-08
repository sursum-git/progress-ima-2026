&SCOPED-DEFINE WSADESCRIPTION_LEN       256
&SCOPED-DEFINE WSASYS_STATUS_LEN        128
 
&SCOPED-DEFINE WSADATA_VERSION_LOW        1    /* WORD(2)  */
&SCOPED-DEFINE WSADATA_VERSION_HIGH       3    /* WORD(2)  */
&SCOPED-DEFINE WSADATA_DESCRIPTION        5    /* CHAR(WSADESCRIPTION_LEN + 1) */ 
&SCOPED-DEFINE WSADATA_SYSTEM_STATUS    262    /* CHAR(WSASYS_STATUS_LEN + 1)  */ 
&SCOPED-DEFINE WSADATA_MAX_SOCKETS      391    /* SHORT(4) */ 
&SCOPED-DEFINE WSADATA_MAX_UDP          395    /* SHORT(4) */ 
&SCOPED-DEFINE WSADATA_VENDOR_INFO      399    /* CHAR*(4) */ 
&SCOPED-DEFINE WSADATA_LENGTH           403   
 
&SCOPED-DEFINE HOSTENT_NAME               1    /* CHAR*(4)  */
&SCOPED-DEFINE HOSTENT_ALIASES            5    /* CHAR**(4) */ 
&SCOPED-DEFINE HOSTENT_ADDR_TYPE          9    /* SHORT(2)  */ 
&SCOPED-DEFINE HOSTENT_ADDR_LENGTH       11    /* SHORT(2)  */ 
&SCOPED-DEFINE HOSTENT_ADDR_LIST         13    /* CHAR**(4) */ 
&SCOPED-DEFINE HOSTENT_LENGTH            16
 
DEFINE VARIABLE         w-TcpName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE         w-TcpAddr      AS CHARACTER NO-UNDO.

RUN i-GetTcpInfo (OUTPUT w-TcpName,
                  OUTPUT w-TcpAddr).

procedure GetComputerNameA external "kernel32.dll":
    define output parameter ptrToString as memptr.
    define input-output parameter intBufferSize as long.
    define return parameter intResult as short.
end procedure.

function Computador returns character:
    define variable chrComputerName as character no-undo format "x(16)".
    define variable intBufferSize as integer no-undo initial 16.
    define variable intResult as integer no-undo.

    define variable ptrToString as memptr no-undo.

    set-size(ptrToString) = 16.
    run GetComputerNameA(output ptrToString, input-output intBufferSize, output intResult).

    return get-string(ptrToString, 1).
end function.

function login-windows returns character.
    define variable cUserID as character no-undo.
    define variable iResult as integer no-undo.
    define variable iSize as integer no-undo.

    assign cUserID = fill(' ', 256)
             iSize = 255.

    run GetUserNameA(output cUserID, input-output iSize, output iResult).

    return cUserID.
end function.

procedure GetUserNameA external "advapi32.dll":
    define output parameter cUserID as character no-undo.
    define input-output parameter iBuffer as long no-undo.
    define return parameter iResult as short no-undo.
end procedure.

/*DISP "CONFIGURACOES GLOBAIS" at 10
     "---------------------" at 10
     "NOME DO COMPUTADOR: " to 25 computador()
     "USUARIO LOGADO: " to 25 login-windows()
     "OPSYS: " to 25 opsys
     "PLATAFORMA: " to 25 "{&window-system}"
     "INTERFACE: " to 25 session:display-type
     "LINGUAGEM ATUAL: " to 25 if current-language = ? then "NAO DISPONIVEL"
                                     else current-language
     "PROGRESS: " to 25 progress
     "VERSAO DO PROGRESS: " to 25 proversion
     "TIPO DE TERMINAL: " to 25 terminal
     "PROPATH: " to 25 substring(propath, 01, 30) "..."
     "PROMSGS: " to 25 promsgs
     "FORMATO DATA: " to 25 session:date-format
     "DATA/HORA ORIGEM: " to 25 session:time-source
     "FORMATO NUMERO: " to 25 session:numeric-format
     "DISPLAY IMEDIATO: " to 25 session:immediate-display format "Sim/Nao"
     "INTERVALO MULTITAREFA: " to 25 session:multitasking-interval
     "DIRETORIO TEMPORARIO: " to 25 session:temp-directory
     "DISPLAY VERSAO 6: " to 25 session:v6display
     "GATEWAYS: " to 25 gateways
     "SERVIDORES: " to 25 dataservers
     "PIXELS POR LINHA: " to 25 session:pixels-per-row format "zz9"
     "PIXELS POR COLUNA: " to 25 session:pixels-per-column format "zz9"
     "PATH: " to 25 os-getenv("path")
     "COMSPEC: " to 25 os-getenv("comspec")
     "ESTACAO: " to 25 os-getenv("estacao") WITH FRAME X SIZE 120 BY 60.*/

PROCEDURE i-GetTcpInfo:
/*------------------------------------------------------------------------
  Procedure   : i-GetTcpInfo
 
  Description : Return the windows TCP host name and address of this PC.
 
  Parms       : - Host name. (OUTPUT, CHARACTER)
                - Host address. (OUTPUT, CHARACTER):
 
  Sample usage: RUN i-GetTcpInfo (OUTPUT w-TcpName,
                                  OUTPUT w-TcpAddr).
 
  Notes       : -
------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER p-TcpName      AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER p-TcpAddr      AS CHARACTER NO-UNDO.
 
  DEFINE VARIABLE         w-TcpName      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE         w-Length       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE         w-Return       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE         ptr-WsaData    AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE         w-Hostent      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE         ptr-Hostent    AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE         ptr-AddrString AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE         ptr-AddrList   AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE         ptr-ListEntry  AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE         w-TcpLong      AS INTEGER   NO-UNDO.

  /* Initialize return values */
  ASSIGN p-TcpName = ?
         p-TcpAddr = ?
         .
 
  /* Allocate work structure for WSADATA */
  SET-SIZE(ptr-WsaData) = {&WSADATA_LENGTH}.
 
  /* Ask Win32 for winsock usage */
  RUN WSAStartup (INPUT  257,        /* requested version 1.1 */
                  INPUT  GET-POINTER-VALUE(ptr-WsaData),
                  OUTPUT w-Return).
 
  /* Release allocated memory */
  SET-SIZE(ptr-WsaData) = 0.
 
  /* Check for errors */
  IF w-Return NE 0 THEN DO:
    MESSAGE "Error accessing WINSOCK support." VIEW-AS ALERT-BOX.
    RETURN.
  END.
 
  /* Set up variables */
  ASSIGN w-Length  = 100
         w-TcpName = FILL(" ", w-Length)
         .
 
  /* Call Win32 routine to get host name */
  RUN gethostname (OUTPUT w-TcpName,
                   INPUT  w-Length,
                   OUTPUT w-Return).

  /* Check for errors */
  IF w-Return NE 0 THEN DO:
    MESSAGE "Error getting tcp name." VIEW-AS ALERT-BOX.
    RUN WSACleanup (OUTPUT w-Return).
    RETURN.
  END.
 
  /* Pass back gathered info */
  /* remember: the string is null-terminated so there is a CHR(0)
               inside w-TcpName. We have to trim it:  */
  p-TcpName = ENTRY(1,w-TcpName,CHR(0)).
 
  /* Call Win32 routine to get host address */
  RUN gethostbyname (INPUT  w-TcpName,
                     OUTPUT w-Hostent).
 
  /* Check for errors */
  IF w-Hostent EQ 0 THEN DO:
    MESSAGE "Error resolving host name." VIEW-AS ALERT-BOX.
    RUN WSACleanup (OUTPUT w-Return).
    RETURN.
  END.
 
  /* Set pointer to HostEnt data structure */
  SET-POINTER-VALUE(ptr-Hostent) = w-Hostent.
 
  /* "Chase" pointers to get to first address list entry */
  SET-POINTER-VALUE(ptr-AddrList)  = GET-LONG(ptr-Hostent, 
                                              {&HOSTENT_ADDR_LIST}).
  SET-POINTER-VALUE(ptr-ListEntry) = GET-LONG(ptr-AddrList, 1).
  w-TcpLong                        = GET-LONG(ptr-ListEntry, 1).
 
  RUN inet_ntoa (INPUT w-TcpLong,
                 OUTPUT ptr-AddrString).
 
  /* Pass back gathered info */
  p-TcpAddr = GET-STRING(ptr-AddrString, 1).
 
  /* Terminate winsock usage */
  RUN WSACleanup (OUTPUT w-Return).
 
END PROCEDURE.

PROCEDURE gethostname EXTERNAL "wsock32.dll" :
  DEFINE OUTPUT       PARAMETER p-Hostname      AS CHARACTER.
  DEFINE INPUT        PARAMETER p-Length        AS LONG.
  DEFINE RETURN       PARAMETER p-Return        AS LONG.
END PROCEDURE.
 
PROCEDURE gethostbyname EXTERNAL "wsock32.dll" :
  DEFINE INPUT        PARAMETER p-Name          AS CHARACTER.
  DEFINE RETURN       PARAMETER p-Hostent       AS LONG.
END PROCEDURE.
 
PROCEDURE inet_ntoa EXTERNAL "wsock32.dll" :
  DEFINE INPUT        PARAMETER p-AddrStruct    AS LONG.
  DEFINE RETURN       PARAMETER p-AddrString    AS MEMPTR.
END PROCEDURE.
 
PROCEDURE WSAStartup EXTERNAL "wsock32.dll" :
  DEFINE INPUT        PARAMETER p-VersionReq    AS SHORT.
  DEFINE INPUT        PARAMETER ptr-WsaData     AS LONG.
  DEFINE RETURN       PARAMETER p-Return        AS LONG.
END PROCEDURE.
 
PROCEDURE WSACleanup EXTERNAL "wsock32":
  DEFINE RETURN       PARAMETER p-Return        AS LONG.
END PROCEDURE.
