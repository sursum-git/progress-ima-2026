/**********************************************************************
 programa:esapi/criarDwTrans.p
 objetivo> Criar uma Transa‡Æo de DW
 Autor:Tadeu Silva
 Data: 01/2026
***********************************************************************/

DEFINE INPUT  PARAMETER pUsuario AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pChave   AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER idNovo   AS INTEGER     NO-UNDO.

CREATE dw_trans.
ASSIGN dw_trans.id              =   NEXT-VALUE(seq_dw_trans)
       dw_trans.dt_hr_registro  = NOW 
       dw_trans.cod_usuario     = pUsuario
       dw_trans.chave           = pChave
       .







{esp/lancarErros.i}




/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 id                               int6        i
   20 dt_hr_registro                   datetm      i
   30 dt_hr_fim                        datetm
   40 cod_usuario                      char
   50 chave                            char        i
*/
