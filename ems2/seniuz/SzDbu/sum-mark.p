FIND {1}._user WHERE 
     {1}._user._userid = "{2}" NO-ERROR.

IF NOT AVAIL {1}._user THEN DO.
   CREATE {1}._user.
   ASSIGN {1}._user._userid = "{2}"
          {1}._user._user-name = "{3}"
          {1}._user._password = ENCODE("out")
          {1}._user._disabled = YES.
END.

