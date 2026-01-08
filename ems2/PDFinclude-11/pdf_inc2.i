/******************************************************************************

    Program:        pdf_inc2.i
    
    Written By:     Gordon Campbell - PRO-SYS Consultants Ltd.
    Written On:     June 2002
    
    Description:    Contains function and variable definitions for 
                    generating a PDF document from within Progress

    Note:           This can only be included once per program

    --------------------- Revision History ------------------
    
    Date:     Author        Change Description
    
******************************************************************************/

{ pdfinclude/pdfglobal.i "NEW SHARED"}

/* PEKI - Find the handle if on the air */
&IF "{1}" = "THIS-PROCEDURE" &THEN
    h_PDFinc = THIS-PROCEDURE. 

    DO WHILE VALID-HANDLE(h_PDFinc)
    AND h_PDFinc:PRIVATE-DATA <> 'Persistent PDFinc':
       h_PDFinc = h_PDFinc:NEXT-SIBLING.
    END.

&ELSEIF "{1}" = "" &THEN
     h_PDFinc = SESSION:FIRST-PROCEDURE.

     DO WHILE VALID-HANDLE(h_PDFinc)
     AND h_PDFinc:PRIVATE-DATA <> 'Persistent PDFinc':
         h_PDFinc = h_PDFinc:NEXT-SIBLING.
     END.
&ENDIF

IF  NOT VALID-HANDLE(h_PDFinc) THEN 
DO:

  /* Call pdf_inc2.p Persistenly */
  RUN pdfinclude/pdf_inc2.p PERSISTENT 
                SET h_PDFinc.

  ASSIGN h_PDFinc:PRIVATE-DATA = 'Persistent PDFinc'.

  &IF NOT PROVERSION BEGINS "8" &THEN
    &IF "{1}" = "THIS-PROCEDURE" &THEN
      IF VALID-HANDLE(THIS-PROCEDURE) THEN THIS-PROCEDURE:ADD-SUPER-PROCEDURE(h_PDFinc).
    &ELSEIF "{1}" = "" &THEN
      IF VALID-HANDLE(SESSION) THEN SESSION:ADD-SUPER-PROCEDURE(h_PDFinc).
    &ENDIF
  &ENDIF
END. /* If Not a valid Handle to PDFinclude */

/* ------------------------ Pre-Define Functions -------------------------- */

{ pdfinclude/pdf_func.i h_PDFinc}

/* --------------------- End of Pre-Define Functions ---------------------- */
