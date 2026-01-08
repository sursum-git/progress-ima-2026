    DEF VAR c-acrobat AS CHARACTER NO-UNDO.
    DEF VAR i-pos     AS INT.

    LOAD "AcroExch.Document" BASE-KEY "HKEY_CLASSES_ROOT".
    USE "AcroExch.Document".

    GET-KEY-VALUE SECTION "shell\open\command"
    KEY DEFAULT 
    VALUE c-acrobat.
    
    unload "AcroExch.Document".


    ASSIGN i-pos = INDEX(c-acrobat,".exe").


    MESSAGE c-acrobat SKIP
            INDEX(c-acrobat,".exe") SKIP
            SUBSTR(c-acrobat, 2, i-pos + 2)
        VIEW-AS ALERT-BOX INFO BUTTONS OK.




