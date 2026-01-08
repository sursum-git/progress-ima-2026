DEFINE VARIABLE listx   AS CHARACTER VIEW-AS SELECTION-LIST SIZE 36 BY 5.
DEFINE VARIABLE rowi    AS INTEGER.
DEFINE VARIABLE sys     AS INTEGER.
DEFINE VARIABLE sheet   AS INTEGER.
DEFINE VARIABLE itemn   AS CHARACTER.
DEFINE VARIABLE sty     AS CHARACTER.
DEFINE VARIABLE ed      AS CHARACTER VIEW-AS EDITOR SIZE 20 by 2.
DEFINE VARIABLE log_i   AS LOGICAL.
DEFINE VARIABLE excelon AS LOGICAL INITIAL FALSE.
DEFINE BUTTON bq LABEL "Quit".
DEFINE BUTTON bg LABEL "Start Excel".

DEFINE FRAME MainFrame
    SKIP(1) SPACE(1) bq SPACE(1) bg SPACE(1) SKIP(1) 
    SPACE(1) listx LABEL "DDE History" SPACE(1) SKIP(1)
    SPACE(1) ed LABEL "Cell R4C2 (Row 4 Col B)" SKIP(1)
WITH SIDE-LABELS.
ENABLE ALL WITH FRAME MainFrame TITLE "Worksheet Monitor".

listx:DELIMITER = "|". /* No server commands use "|" */

PROCEDURE WinExec EXTERNAL "kernel32.dll": /* Run Windows application */
    DEFINE INPUT PARAMETER prog_name AS CHARACTER. 
    DEFINE INPUT PARAMETER prog_style AS LONG. 
END PROCEDURE.

PROCEDURE LogExchange: /* Log latest DDE in selection list */
    log_i = listx:ADD-LAST(FRAME MainFrame:DDE-NAME + " " + 
                           FRAME MainFrame:DDE-TOPIC + " " + 
                           FRAME MainFrame:DDE-ITEM) IN FRAME MainFrame.
END PROCEDURE.

ON CHOOSE OF bq IN FRAME MainFrame  
DO:
   IF (excelon = FALSE) THEN RETURN.
   DDE ADVISE sheet STOP ITEM "r4c2". RUN LogExchange.
   DDE TERMINATE sheet. RUN LogExchange.
   DDE EXECUTE sys COMMAND "[activate(~"sheet1~")]". RUN LogExchange.
   DDE EXECUTE sys COMMAND "[close(0)]". RUN LogExchange.
   DDE EXECUTE sys COMMAND "[activate(~"chart1~")]". RUN LogExchange.
   DDE EXECUTE sys COMMAND "[close(0)]". RUN LogExchange.
   DDE EXECUTE sys COMMAND "[error(0)]". RUN LogExchange.
   DDE EXECUTE sys COMMAND "[quit()]". RUN LogExchange.
END.

/*
ActiveSheet:NAME = "Detalhado".

*/


ON CHOOSE OF bg IN FRAME MainFrame
DO:
   /* INPUT: 1=normal 2=minimized */
   RUN WinExec (INPUT "C:\Program Files\Microsoft Office\Office10\Excel /e", INPUT 2).
   excelon = TRUE.
   DDE INITIATE sys FRAME FRAME MainFrame:HANDLE 
      APPLICATION "Excel" TOPIC "System". RUN LogExchange.
   IF sys = 0 THEN
   DO:
      MESSAGE "Excel not available".
      RETURN.
   END.
   DDE EXECUTE sys COMMAND "[new(1)]". RUN LogExchange.
   DDE INITIATE sheet FRAME FRAME MainFrame:HANDLE 
      APPLICATION "Excel" TOPIC "Sheet1". RUN LogExchange.   
   
   DDE SEND sheet SOURCE "Name" ITEM "r1c1". RUN LogExchange.
   DDE SEND sheet SOURCE "Balance" ITEM "r1c2". RUN LogExchange.
   DDE SEND sheet SOURCE "State" ITEM "r1c3". RUN LogExchange.
   rowi = 2.
   FOR EACH customer WHERE balance < 5000 BY balance:
      itemn = "R" + STRING(rowi) + "C1".
      DDE SEND sheet SOURCE customer.name ITEM itemn. 
         RUN LogExchange.
      itemn = "R" + STRING(rowi) + "C2".
      DDE SEND sheet SOURCE STRING(customer.balance) ITEM itemn. 
         RUN LogExchange.
      itemn = "R" + STRING(rowi) + "C3".
      DDE SEND sheet SOURCE STRING(customer.state) ITEM itemn. 
         RUN LogExchange.   
      rowi = rowi + 1.
   END.
   DDE EXECUTE sheet COMMAND "[select(~"C1:C2~")]". RUN LogExchange.
   DDE EXECUTE sys COMMAND "[column.width(,,,3)]". RUN LogExchange.
   DDE EXECUTE sys COMMAND "[format.number(~"$#,##0~")]". RUN LogExchange.
   DDE EXECUTE sheet COMMAND "[select(~"C1:C2~")]". RUN LogExchange.
   DDE EXECUTE sys COMMAND "[new(2,1,1)]". RUN LogExchange.
   DDE EXECUTE sys COMMAND "[activate(~"chart1~")]". RUN LogExchange.
   DDE EXECUTE sys COMMAND "[gallery.3d.pie(3)]". RUN LogExchange.
   DDE EXECUTE sys COMMAND "[app.restore()]". RUN LogExchange.
   DDE EXECUTE sys COMMAND "[arrange.all()]". RUN LogExchange.
   
   DDE ADVISE sheet START ITEM "r4c2". RUN LogExchange.
END.

ON DDE-NOTIFY OF FRAME MainFrame 
DO:   
   DDE GET sheet TARGET sty ITEM "r4c2". RUN LogExchange.
   sty = SUBSTR(sty, 1, 20).             /* Drop the CR/LF */
   ed:SCREEN-VALUE IN FRAME MainFrame = sty.
END.
WAIT-FOR CHOOSE OF bq IN FRAME MainFrame.
