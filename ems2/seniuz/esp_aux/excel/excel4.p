/* This program extracts data from a Progress database 
This program leaves Excel open. You should close it manually when the program completes. 
You must create a folder that named Excel under `C:` and copy the Excel.exe and *.dll files 

Program name : excel.p
Date : 21 May 2000 
Author : Cem DAGLI cem@progteg.com
*/

/* *********************** Control Definitions ********************** */
DEFINE VARIABLE rowi    AS INTEGER.
DEFINE VARIABLE sys     AS INTEGER.
DEFINE VARIABLE sheet1  AS INTEGER.
DEFINE VARIABLE kl      AS INTEGER.
DEFINE VARIABLE itemn   AS CHARACTER.
DEFINE VARIABLE sty     AS CHARACTER.
DEFINE VARIABLE cl      AS CHARACTER.
DEFINE VARIABLE excelon AS LOGICAL INITIAL FALSE.
DEFINE VARIABLE custno1 LIKE item.it-codigo LABEL "Cust-No>=".
DEFINE VARIABLE custno2 LIKE item.it-codigo LABEL "Cust-No<=".

DEFINE BUTTON bt_excel 
       LABEL "Excel" 
       SIZE 15 BY 1.05.

/* Query definitions */
DEFINE QUERY MainFrame FOR mgcad.ITEM SCROLLING.

/* ************************ Frame Definitions *********************** */
DEFINE FRAME MainFrame
       custno1 AT ROW 4.79 COL 20.5 COLON-ALIGNED
       custno2 AT ROW 4.79 COL 45.5 COLON-ALIGNED
       Bt_Excel AT ROW 8.11 COL 37.5
       "CUSTOMER INFORMATION" VIEW-AS TEXT
       SIZE 20 BY .84 AT ROW 2.26 COL 25.5
       FGCOLOR 4 
       WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
       SIDE-LABELS NO-UNDERLINE THREE-D
       AT COL 1 ROW 1
       SIZE 85.25 BY 15.42. 

ENABLE ALL WITH FRAME MainFrame.

PROCEDURE WinExec EXTERNAL "kernel32.dll":
   DEFINE INPUT PARAMETER prog_name AS CHARACTER. 
   DEFINE INPUT PARAMETER prog_style AS SHORT. 
END PROCEDURE.

ON CHOOSE OF bt_excel IN FRAME MainFrame DO: 
   RUN WinExec (INPUT "c:\Arquivos de Programas\Microsoft Office\Office11\Excel.exe /e", INPUT 2). /* 1=normal, 2=minimized */
   excelon = TRUE.
   
   DDE INITIATE sys FRAME FRAME MainFrame:HANDLE APPLICATION "Excel" TOPIC "System". 
   IF sys = 0 THEN
   DO:
      MESSAGE "Couldn't find Excel" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   
   DDE EXECUTE sys COMMAND "[new(1)]".    
   DDE INITIATE sheet1 FRAME FRAME MainFrame:HANDLE APPLICATION "Excel" TOPIC "Plan1". 
   DDE SEND sheet1 SOURCE "CODIGO"    ITEM "L1C1". 
   DDE SEND sheet1 SOURCE "DESCRICAO" ITEM "L1C2". 
   DDE SEND sheet1 SOURCE "UN"        ITEM "L1C3".

   rowi = 3.
   FOR EACH ITEM WHERE (item.it-codigo >= custno1:SCREEN-VALUE) AND 
                       (item.it-codigo <= custno2:SCREEN-VALUE) 
       NO-LOCK BY ITEM.it-codigo:
               
      itemn = "L" + STRING(rowi) + "C1".
      DDE SEND sheet1 SOURCE STRING(item.it-codigo) ITEM itemn. 
      itemn = "L" + STRING(rowi) + "C2".
      DDE SEND sheet1 SOURCE item.desc-item ITEM itemn.
      itemn = "L" + STRING(rowi) + "C3".
      DDE SEND sheet1 SOURCE item.un ITEM itemn. 
      
      rowi = rowi + 1.
   END.
   
   DDE EXECUTE sheet1 COMMAND "[select(~"C2~")]". 
   DDE EXECUTE sys    COMMAND "[column.width(20)]". 
   DDE EXECUTE sheet1 COMMAND "[select(~"C3~")]". 
   DDE EXECUTE sys    COMMAND "[column.width(20)]". 
   DDE EXECUTE sheet1 COMMAND "[select(~"C4~")]". 
   DDE EXECUTE sys    COMMAND "[column.width(12)]". 
   DDE EXECUTE sheet1 COMMAND "[select(~"C1~")]". 
   DDE EXECUTE sys    COMMAND "[column.width(6)]".

   /* DDE EXECUTE sheet1 COMMAND "[select('L1C1:L10C10')]". */
   /* DDE EXECUTE sys    COMMAND "[font.Bold(True)]". */
   
   DDE EXECUTE sys COMMAND "[app.restore()]". 
   DDE EXECUTE sys COMMAND "[arrange.all()]". 
   
   /* DDE EXECUTE sys COMMAND "[SAVE.AS('c:/lixo/lixo.xls')]". */

   DDE TERMINATE sys.
   DDE TERMINATE sheet1.
   
END. 

APPLY "ENTRY" TO custno1 IN FRAME MainFrame.
WAIT-FOR CHOOSE OF bt_excel.
