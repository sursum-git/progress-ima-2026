/****************************************************************************
** Programa : TWIN090 - trigger de Write para a tabela docum-est
** Data     : 06/2024
** Objetivo : trigger de Write para a tabela docum-est
** Empresa  : IMA 
** Autor    : Tadeu Silva
** Alterado : 
** *************************************************************************/


DEFINE PARAMETER BUFFER doc     FOR docum-est.
DEFINE PARAMETER BUFFER doc-old FOR docum-est.  
DEFINE VARIABLE hBoFats99       AS HANDLE      NO-UNDO.

{include/i-prgvrs.i twin090 2.06.00.001}

FIND natur-oper OF doc NO-LOCK NO-ERROR.
IF natur-oper.tipo-compra = 3 THEN DO: //devolu‡Æo de cliente

    RUN esbo/boFats99.p PERSIST SET hBoFats99.
    RUN iniciar         IN hBoFats99.
    RUN setData         IN hBoFats99(doc.dt-trans).
    RUN setProgOrigem   IN hBoFats99('twin090').
    RUN setTipoRegistro IN hBoFats99('devolucao').
    RUN inserir         IN hBoFats99.
    RUN finalizar       IN hBoFats99.

END.


RETURN 'OK':u.
