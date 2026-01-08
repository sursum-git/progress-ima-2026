    FOR EACH it-doc-fisc  EXCLUSIVE-LOCK:
        FIND doc-fisc OF it-doc-fisc NO-LOCK NO-ERROR.

        IF AVAIL doc-fisc THEN
           DISP doc-fisc.dt-emis.

        PAUSE 0.
        ASSIGN SUBSTR(it-doc-fisc.char-2,21,1) = '2'.
    END.
