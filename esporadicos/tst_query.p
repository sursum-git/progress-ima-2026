/*
   Works in OpenEdge 11.x

   Replace "SomeTable", "SomeConditions" and "SomeField" with your values
*/

DEFINE VARIABLE HQuery  AS HANDLE NO-UNDO.
DEFINE VARIABLE HBuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE cCampos AS CHARACTER   NO-UNDO.
DEFINE VARIABLE qtCampos AS INTEGER     NO-UNDO INIT 1.
DEFINE VARIABLE lSegue AS LOGICAL     NO-UNDO.

ASSIGN cCampos = 'distinct it_codigo as it_codigo,cod_refer'.

DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.

MESSAGE INDEX(cCampos,'distinct')
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.


CREATE BUFFER HBuffer FOR TABLE "class_item_ref".

CREATE QUERY HQuery.

HQuery:SET-BUFFERS(HBuffer).
HQuery:QUERY-PREPARE("FOR each class_item_ref fields(it_codigo cod_refer) BREAK BY class_item_ref.it_codigo by class_item_ref.cod_refer").
HQuery:QUERY-OPEN().
//HQuery:GET-FIRST().

/*
   NOTE: Do Not Use FIRST-OF(0) As That Returns True Only
   For The First Record Read In The Query, The BREAK BY
   Phrase Starts With FIRST-OF(1) And Increments From There
*/   

/*REPEAT WHILE HQuery:QUERY-OFF-END = FALSE:
    IF HQuery:FIRST-OF(1) AND HQuery:FIRST-OF(2) THEN DO:
    /*   DISP hbuffer:BUFFER-FIELD('it_codigo'):BUFFER-VALUE()
             hbuffer:BUFFER-FIELD('cod_refer'):BUFFER-VALUE() .*/
       ASSIGN icont = iCont + 1.
    END.
        
       /* Do What You Want With The Record Here */
    HQuery:GET-NEXT().
END.*/

REPEAT:                                                                                                                                               
  hQuery:GET-NEXT().                                                                                                                                  
  IF hQuery:QUERY-OFF-END THEN LEAVE. 
  /*RUN verificarCondBreak(hQUERY,OUTPUT lSegue).
  IF  lSegue = NO THEN NEXT.*/
   
  IF HQuery:FIRST-OF(1) 
      //AND HQuery:FIRST-OF(2) 
      THEN DO:
     ASSIGN icont = iCont + 1. 
  END.
  

  /*FOR EACH ttTabelas:
      REPEAT iCont2 = 1 TO num-entries(ttTabelas.campos,","):                                                                                                      
        ASSIGN cCampo = ENTRY(iCont2,ttTabelas.campos,",").
        RUN getVlCp.
        PUT UNFORMAT cVlCampo ";".
      END.                                                                                                                                                
  END.
  PUT SKIP.*/
END.


HQuery:QUERY-CLOSE().

DELETE OBJECT HQuery.
DELETE OBJECT HBuffer.

DISP iCont.



PROCEDURE verificarCondBreak:
    DEFINE INPUT  PARAMETER hQuery AS HANDLE      NO-UNDO.
    DEFINE OUTPUT PARAMETER pSegue AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
    ASSIGN pSegue = YES.
    REPEAT iCont = 1 TO qtCampos:
            IF NOT hQuery:FIRST-OF(iCont) THEN DO:
                ASSIGN pSegue = NO.
                LEAVE.
            END.      
    END.          
    
END PROCEDURE.
