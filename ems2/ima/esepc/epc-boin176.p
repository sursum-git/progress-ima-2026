{include/i-epc200.i1} /* Defini‡Æo da temp-table tt-epc */

DEFINE INPUT PARAMETER P-ind-event AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.

/* Variaveis de uso especifico */
/*def new global shared var gr-nota-fiscal as rowid no-undo.*/


/*OUTPUT TO c:/boin176.txt APPEND.
FOR EACH tt-epc.
    PUT UNFORMAT tt-epc.cod-event "  ---  " tt-epc.cod-parameter /*"  ---  " val-parameter*/ SKIP.
END.
OUTPUT CLOSE.
*/

FIND FIRST tt-epc WHERE tt-epc.cod-event = "aftercreateRecord" /*"CreateItemOfNotaFiscal" */
                    AND tt-epc.cod-parameter = "Table-Rowid"
                  NO-LOCK NO-ERROR.
IF AVAIL tt-epc THEN DO:
  
  FIND FIRST item-doc-est WHERE ROWID(item-doc-est) = TO-ROWID(tt-epc.val-parameter) share-lock NO-ERROR.
  
  IF AVAIL item-doc-est           AND 
     item-doc-est.narrativa =  "" AND
     item-doc-est.cod-refer <> "" THEN DO:
     
    FIND FIRST ITEM     WHERE ITEM.it-codigo     = item-doc-est.it-codigo NO-LOCK NO-ERROR.
    FIND FIRST item-ext WHERE item-ext.it-codigo = ITEM.it-codigo         NO-LOCK NO-ERROR. 

    /* Altera a descri‡Æo do item para buscar da primeira linha da narrativa quando o item estiver como narrativa informada e quando nÆo for nota de importa‡Æo */
    IF ITEM.ind-imp-desc = 7 THEN
       ASSIGN item-doc-est.narrativa = ENTRY(1,ITEM.narrativa,CHR(10)). /* Busca uma linha narrativa*/

    ASSIGN item-doc-est.narrativa = item-doc-est.narrativa + " REF: " + item-doc-est.cod-refer. /* Adiciona Referencia do item*/

    /* Adiciona Gramatura do item */            
    IF AVAIL item-ext AND item-ext.gramatura > 0 THEN
       ASSIGN item-doc-est.narrativa = item-doc-est.narrativa + "  G/M: " + STRING(item-ext.gramatura).

    /* Adiciona Regra de lavagem do item*/
    IF ITEM.cod-imagem <> "" THEN 
       ASSIGN item-doc-est.narrativa = item-doc-est.narrativa + "  RL: " + ITEM.cod-imagem.      
  END.
  
END.


