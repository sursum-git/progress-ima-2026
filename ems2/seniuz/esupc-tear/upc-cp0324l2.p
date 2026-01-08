/* Programa: upc-cp0324l2.p
** Objetivo: 
** Autor...: Prodb - Toninho  Mar‡o/2004
*/

DEF NEW GLOBAL SHARED VAR h-it-codigo AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-cod-refer AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-lote-serie AS HANDLE.

FIND item WHERE 
     item.it-codigo = h-it-codigo:SCREEN-VALUE NO-LOCK NO-ERROR.

IF item.tipo-con-est = 4 THEN DO.
   IF LOOKUP(SUBSTR(h-lote-serie:SCREEN-VALUE,1,2),"PP,PD,RP,RD") = 0 THEN DO.
      MESSAGE "Lote deve inciar com PP,PD,RP,RD" VIEW-AS ALERT-BOX.
      APPLY 'entry' TO h-lote-serie.
      RETURN NO-APPLY. 
   END.
      
   IF NOT h-lote-serie:SCREEN-VALUE MATCHES "*" + h-cod-refer:SCREEN-VALUE THEN DO.
      MESSAGE "Lote deve ser composto de PP,PD,RP,RD + Referencia"
              VIEW-AS ALERT-BOX.
      APPLY 'entry' TO h-lote-serie.
      RETURN NO-APPLY.
   END.
END.
