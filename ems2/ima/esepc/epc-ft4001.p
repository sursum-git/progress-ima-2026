/****** Parametros ******/
DEF INPUT PARAMETER p-ind-event  AS CHARACTER.              
DEF INPUT PARAMETER p-ind-object AS CHARACTER.              
DEF INPUT PARAMETER p-wgh-object AS HANDLE.                 
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.          
DEF INPUT PARAMETER p-cod-table  AS CHARACTER.              
DEF INPUT PARAMETER p-row-table  AS ROWID.                  

DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo  AS HANDLE        NO-UNDO.

/* variaveis para utiliza‡Æo em UPc no bodi317va.p */
DEF NEW GLOBAL SHARED VAR h-nr-embarque AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-save AS HANDLE NO-UNDO.

IF p-ind-event = "AFTER-DISPLAY" THEN DO.
   FIND embarque WHERE
        ROWID(embarque) = p-row-table NO-LOCK NO-ERROR.

   ASSIGN h-bt-save:SENSITIVE = YES.
   FOR EACH pre-fatur OF embarque NO-LOCK.
       FIND ped-venda WHERE
            ped-venda.nr-pedcli = pre-fatur.nr-pedcli AND
            ped-venda.nome-abrev = pre-fatur.nome-abrev
            NO-LOCK NO-ERROR.
       IF AVAIL ped-venda AND
          VALID-HANDLE(h-bt-save) THEN DO.
          IF ped-venda.cod-priori = 12 THEN 
             ASSIGN h-bt-save:SENSITIVE = NO.
       END.
   END.
END.

IF p-ind-event = "before-initialize" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):            
      IF h-objeto:NAME = "btSave" THEN do:
         ASSIGN h-bt-save = h-objeto.
         ASSIGN h-bt-save:SENSITIVE = NO.
      END.

      IF h-objeto:NAME = "nr-embarque" THEN do:
         ASSIGN h-nr-embarque = h-objeto.
      END.
        
      IF h-objeto:TYPE = "field-group" THEN 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
      ELSE 
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.       
   END.           
END.

IF p-ind-event = "after-initialize" THEN DO:
    ASSIGN h-campo = p-wgh-frame:FIRST-CHILD. 
    DO WHILE VALID-HANDLE(h-campo):              
        IF  h-campo:NAME = "nr-embarque" THEN DO:
            ASSIGN h-nr-embarque = h-campo.
            LEAVE.
        END.
        
        IF h-campo:TYPE = "field-group" THEN 
            ASSIGN h-campo = h-campo:FIRST-CHILD.
        ELSE  
            ASSIGN h-campo = h-campo:NEXT-SIBLING.       
    END.
END.
