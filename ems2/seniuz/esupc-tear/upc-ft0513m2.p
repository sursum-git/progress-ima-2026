DEF INPUT PARAM p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.

DEFINE VAR h-objeto          AS WIDGET-HANDLE NO-UNDO.
DEFINE VAR c-programa-mg97   AS CHAR INIT "ft0513r". 
DEFINE VAR c-dispositivo AS CHAR INITIAL "LexT520n1:Padr∆o_132_R".

DEF NEW GLOBAL SHARED VAR c-seg-usuario     AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-romaneio       AS WIDGET-HANDLE NO-UNDO.

/* Procura por Dispositivo de Impress∆o */
FIND layout_impres_padr WHERE
     layout_impres_padr.cod_usuario = c-seg-usuario AND 
     layout_impres_padr.cod_proced  = "ESSP0099" 
     NO-LOCK NO-ERROR.

IF AVAIL layout_impres_padr THEN
   ASSIGN c-dispositivo = TRIM(layout_impres_padr.nom_impressora) + ":" +
                          TRIM(layout_impres_padr.cod_layout_impres).

APPLY 'VALUE-CHANGED' TO SELF.  /* Executa as Triggers Originais */

IF wh-romaneio:SCREEN-VALUE = 'YES' THEN DO.
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = "c-arquivo-bloq" THEN DO.
         CASE SELF:SCREEN-VALUE.
             WHEN '1' THEN
                 ASSIGN h-objeto:SCREEN-VALUE = c-dispositivo.      
             WHEN '2' THEN
                 ASSIGN h-objeto:SCREEN-VALUE = SESSION:TEMP-DIRECTORY + c-programa-mg97 + ".tmp".
         END CASE.
      END.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
END.

