DEF INPUT PARAMETER w-usuario       AS CHAR NO-UNDO.
DEF INPUT PARAMETER w-arquivo       AS CHAR NO-UNDO.
DEF INPUT PARAMETER w-bat           AS CHAR NO-UNDO.       

DEF VAR c-comand AS CHAR NO-UNDO.

FOR EACH DICTDB._connect 
   WHERE CAPS(DICTDB._connect._Connect-Device)  = CAPS(w-usuario) 
     AND DICTDB._connect._Connect-Device <> "" NO-LOCK:
    
    OUTPUT TO VALUE("\siserp\rpw\spool\mortos.txt") APPEND.
        PUT "                     " 
            "   "
            TODAY " " STRING(TIME,"HH:MM:SS") " "
            CAPS(LDBNAME("DICTDB"))             FORMAT "x(12)"
            " - "
            DICTDB._connect._Connect-Name       FORMAT "x(12)"
            " - "
            DICTDB._connect._Connect-Usr
            " - " 
            DICTDB._connect._Connect-Pid SKIP.
    OUTPUT CLOSE.

    IF PDBNAME("DICTDB") BEGINS "hr" THEN 
    ASSIGN c-comand = "/progress/dlc101c/bin/proshut /siserp/hcm210/" + 
                    PDBNAME("DICTDB") +                             
                    " -C disconnect "  +                            
                    STRING(DICTDB._connect._Connect-Usr).   
    ELSE
    IF PDBNAME("DICTDB") BEGINS "ems2uni" THEN 
    ASSIGN c-comand = "/progress/dlc101c/bin/proshut /siserp/hcm210/" + 
                        PDBNAME("DICTDB") +                             
                        " -C disconnect "  +                            
                        STRING(DICTDB._connect._Connect-Usr).   
    ELSE
    IF PDBNAME("DICTDB") BEGINS "dt" THEN 
    ASSIGN c-comand = "/progress/dlc101c/bin/proshut /siserp/hcm210/" + 
                        PDBNAME("DICTDB") +                             
                        " -C disconnect "  +                            
                        STRING(DICTDB._connect._Connect-Usr).   
    ELSE
    IF PDBNAME("DICTDB") BEGINS "ems5" 
       THEN ASSIGN c-comand = "/progress/dlc101c/bin/proshut /siserp/ems506/" + 
                   PDBNAME("DICTDB") +                             
                   " -C disconnect "  +                            
                   STRING(DICTDB._connect._Connect-Usr).   
       ELSE ASSIGN c-comand = "/progress/dlc101c/bin/proshut /siserp/ems206ttd/" +                   
                   PDBNAME("DICTDB") +                                                                                                                    
                   " -C disconnect "  +                                                            
                   STRING(DICTDB._connect._Connect-Usr).                                           

    OS-COMMAND SILENT VALUE(c-comand).



END.
