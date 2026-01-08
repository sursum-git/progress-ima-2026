IF "{2}" = 'o' THEN DO.  /*out*/
    FIND {1}._user WHERE 
         {1}._user._userid = "{3}" NO-ERROR.

    IF NOT AVAIL {1}._user THEN DO.
       CREATE {1}._user.
       ASSIGN {1}._user._userid = "{3}"
              {1}._user._user-name = 'Desconectar'
              {1}._user._password = ENCODE("out")
              {1}._user._disabled = YES.
    END.
END.
ELSE DO.
    FIND {1}._user WHERE 
         {1}._user._userid = "{3}" NO-ERROR.
    IF AVAIL {1}._user THEN
       DELETE {1}._user.
END.
