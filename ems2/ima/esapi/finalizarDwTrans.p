/**********************************************************************
 programa:esapi/criarDwTrans.p
 objetivo: Finalizar uma Transa‡Æo de DW em aberto.
 Autor:Tadeu Silva
 Data: 01/2026
***********************************************************************/
DEFINE INPUT  PARAMETER pID AS INTEGER     NO-UNDO.

FIND dw_trans
    EXCLUSIVE-LOCK 
    WHERE dw_trans.id = pId NO-ERROR.
 IF AVAIL dw_trans THEN  DO:
    ASSIGN dw_trans.dt_hr_fim = NOW.     
 END.
 RELEASE dw_trans.
 
 {esp/lancarErros.i}
