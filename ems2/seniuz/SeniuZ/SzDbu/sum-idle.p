DEF TEMP-TABLE tt-usr NO-UNDO
    FIELD banco          AS CHAR 
    FIELD cod-usuario    AS CHAR
    FIELD usr            AS INT
    FIELD acesso         AS INT
    FIELD tempo          AS INT
    FIELD maquina        AS CHAR
    FIELD pid            AS INT
    FIELD login          AS CHAR
    FIELD ultimo-acesso  AS CHAR
    FIELD marca          AS CHAR
    INDEX indice1 cod-usuario banco.

DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-usr. 

DEF VAR c-user-out AS CHAR.

GET-KEY-VALUE SECTION "Startup":U KEY "UserOut":U VALUE c-user-out.
IF c-user-out = ? THEN
   ASSIGN c-user-out = ''.

FOR EACH {1}._userIO WHERE
         {1}._userIO._userIO-Name <> ? NO-LOCK.
  
    IF {1}._userio._UserIO-Name = '' THEN NEXT.

    FIND {1}._connect WHERE
         {1}._connect._connect-usr = {1}._userIO._UserIO-usr
         NO-LOCK NO-ERROR.

    IF NOT AVAIL {1}._connect THEN NEXT.
    IF {1}._connect._connect-type <> 'REMC' THEN NEXT.

    FIND tt-usr WHERE
         tt-usr.banco = "{1}" AND
         tt-usr.usr = {1}._userio._userIO-usr AND
         tt-usr.pid = {1}._connect._connect-pid
         NO-ERROR.

    IF NOT AVAIL tt-usr THEN DO.
       CREATE tt-usr.
       ASSIGN tt-usr.banco = "{1}"
              tt-usr.usr = {1}._userio._userIO-usr
              tt-usr.maquina = {1}._connect._connect-device
              tt-usr.pid = {1}._connect._connect-pid
              tt-usr.login = {1}._connect._connect-time.
    END.
    ASSIGN tt-usr.cod-usuario = {1}._userio._UserIO-Name.

    IF tt-usr.acesso = {1}._userio._UserIO-DbAccess THEN
       ASSIGN tt-usr.tempo = tt-usr.tempo + 1.
    ELSE
       ASSIGN tt-usr.acesso = {1}._userio._UserIO-DbAccess
              tt-usr.ultimo-acesso = STRING(TODAY,"99/99/9999") + "  " + STRING(TIME,"HH:MM:SS")
              tt-usr.tempo = 0.

   IF (TRIM(OS-GETENV("USERNAME")) = {1}._userio._UserIO-Name AND
       TRIM(OS-GETENV("COMPUTERNAME")) = {1}._connect._connect-device) OR
      LOOKUP({1}._userio._UserIO-Name,c-user-out) > 0 THEN
      ASSIGN tt-usr.marca = 's'.
END.

FOR EACH tt-usr.
    IF tt-usr.banco = "{1}" THEN DO.
       FIND {1}._connect WHERE
            {1}._connect._connect-usr = tt-usr.usr
            NO-LOCK NO-ERROR.
    
       IF NOT AVAIL {1}._connect THEN 
          DELETE tt-usr.
    END.
END.

