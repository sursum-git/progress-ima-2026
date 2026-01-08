/****************************************************************************
** Programa: upc-cr0901v.p 
** Objetivo: Preencher Automaticamente a Mensagem do Boleto Bancario
**           PROTESTAR APOS 5 DIAS DO VENCIMENTO  
**           Quando o Portador for '23702' e a modalidade for 'CB SIMPLES'.
**           
**           
** AUTOR   : F†bio Coelho Lanza (JULHO-2010)
*****************************************************************************/

/* Parameter Definitions ****************************************************/
DEF NEW GLOBAL SHARED VAR h-bloqueto    AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR h-portador    AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR h-modalid     AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR h-bt-add      AS WIDGET-HANDLE.

APPLY 'CHOOSE' TO h-bt-add.

IF h-bloqueto:SENSITIVE THEN DO.
   IF h-portador:SCREEN-VALUE = '23702' AND 
      h-modalid:SCREEN-VALUE = 'Cb Simples' THEN DO.
      ASSIGN h-bloqueto:SCREEN-VALUE = "PROTESTAR AP‡S 5 DIAS DO VENCIMENTO".
      APPLY "ENTRY" TO h-bloqueto.
      APPLY "TAB" TO h-bloqueto.
      APPLY "LEAVE" TO h-bloqueto.
   END.
END.


