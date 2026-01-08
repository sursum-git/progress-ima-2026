DEF VAR i-hora AS INT.
RUN esapi/cv-hora.p (INPUT "23:59:59", OUTPUT i-hora).

MESSAGE i-hora
        STRING(i-hora,"HH:MM:SS")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
