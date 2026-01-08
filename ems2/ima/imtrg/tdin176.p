
/****************************************************************************
** Programa : TWIN176 - trigger de delete para a tabela item-doc-est 
** Data     : 06/2024
** Objetivo : trigger de delete para a tabela item-doc-est
** Empresa  : IMA 
** Autor    : Tadeu Silva
** Alterado : 
** *****************************************************************************/
DEFINE PARAMETER BUFFER b-item-doc-est FOR item-doc-est.

{include/i-prgvrs.i tdin176 2.06.00.001}

DEFINE VARIABLE hBoFats99       AS HANDLE      NO-UNDO.

FIND docum-est   OF b-item-doc-est NO-LOCK NO-ERROR.
FIND natur-oper  OF docum-est      NO-LOCK NO-ERROR.
IF natur-oper.tipo-compra = 3 THEN DO: //devolu‡Æo de cliente
    RUN esbo/boFats99.p PERSIST SET hBoFats99.
    RUN iniciar         IN hBoFats99.
    RUN setData         IN hBoFats99(docum-est.dt-trans).
    RUN setProgOrigem   IN hBoFats99('tdin176').
    RUN setTipoRegistro IN hBoFats99('devolucao').
    RUN inserir         IN hBoFats99.
    RUN finalizar       IN hBoFats99.

END.


RETURN 'OK':u.


