/****************************************************************************
** Programa: upc-cr0901v.p 
** Objetivo: Preencher Automaticamente a Mensagem do Boleto Bancario
**           PROTESTAR APOS 5 DIAS DO VENCIMENTO  
**           Quando o Portador for '23702' e a modalidade for 'CB SIMPLES'.
**           
**           
** AUTOR   : F bio Coelho Lanza (JULHO-2010)
*****************************************************************************/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.

DEF VAR h-win    AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-frame  AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo  AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-query  AS HANDLE.
DEF VAR h-browse AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-col    AS HANDLE.
DEF VAR i-ct     AS INT.

DEF NEW GLOBAL SHARED VAR h-portador    AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR h-bt-add      AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR h-modalid     AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR h-bloqueto    AS WIDGET-HANDLE.
/* Main Block ***************************************************************/
  
IF NOT VALID-HANDLE(h-portador) THEN DO.
   ASSIGN h-win    = p-wgh-object:WINDOW.
   ASSIGN h-frame  = h-win:FIRST-CHILD.
   ASSIGN h-objeto = h-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = "c-bloqueto" THEN
         ASSIGN h-bloqueto = h-objeto.

      IF h-objeto:NAME = "br-disponivel" THEN DO.
         ON 'MOUSE-SELECT-DBLCLICK':U OF h-objeto PERSISTENT RUN esupc/upc-cr0901vb.p.  

         ASSIGN h-browse = h-objeto
                h-query  = h-browse:QUERY.
    
         DO i-ct = 1 TO h-browse:NUM-COLUMNS.
            ASSIGN h-col = h-browse:GET-BROWSE-COLUMN(i-ct).
    
            IF h-col:NAME = "cod-portador" THEN 
               ASSIGN h-portador = h-col.
    
            IF h-col:NAME = "c-modalid" THEN
               ASSIGN h-modalid = h-col.
         END.
      END.

      IF h-objeto:NAME = 'bt-add' THEN DO.
         ON 'MOUSE-SELECT-CLICK':U OF h-objeto PERSISTENT RUN esupc/upc-cr0901va.p.  
         ASSIGN h-bt-add = h-objeto.
      END.

      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
END.

