FOR EACH _field WHERE
         _field._field-name MATCHES "*nr-ped*" NO-LOCK.

    FIND _file OF _field NO-LOCK NO-ERROR.

    DISP _file._file-name
         _field._field-name.
END.
