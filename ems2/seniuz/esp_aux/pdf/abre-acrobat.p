  DEF VAR c-acrobat AS CHAR.
  DEF VAR c-Arq     AS CHAR INITIAL "p:\teste.pdf".
  DEF VAR c-Imp     AS CHAR.

  ASSIGN c-Imp = (STRING(CAPS(SESSION:PRINTER-NAME))) .

  LOAD "AcroExch.Document" BASE-KEY "HKEY_CLASSES_ROOT".
  USE "AcroExch.Document".

  GET-KEY-VALUE SECTION "shell\open\command" KEY DEFAULT VALUE c-acrobat.
  UNLOAD "AcroExch.Document".

  IF c-acrobat = ? THEN DO:
     MESSAGE "O Utilitario ADOBE READ n∆o foi encontrado." SKIP
             "N∆o Ç possivel a execuá∆o do programa."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN.
  END.

  ASSIGN c-acrobat = ENTRY(2,c-acrobat,'"').

  RUN WinExec (c-acrobat + " " + "/n" + c-Arq + " ", INPUT 1). 
  
  
PROCEDURE WinExec EXTERNAL "kernel32.dll":
    DEF INPUT PARAMETER prog_name    AS CHAR.
    DEF INPUT PARAMETER visual_style AS SHORT.
END PROCEDURE


