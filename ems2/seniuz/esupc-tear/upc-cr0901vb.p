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
DEF NEW GLOBAL SHARED VAR h-bt-add  AS WIDGET-HANDLE.

APPLY 'MOUSE-SELECT-CLICK' TO h-bt-add.
