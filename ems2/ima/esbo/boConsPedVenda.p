/**************************************************************************
BO DE CONSULTA DE PEDIDOS DE VENDA
Programa: 
Autor: 
Objetivo: 
Data: 
Modificacoes:
*****************************************************************************/
{esp/util.i}
{esbo/boConsPedVenda.i {&ttparam} ttLocal}

&SCOPED-DEFINE ttParam  ttParam


DEFINE VARIABLE hAcomp AS HANDLE      NO-UNDO.
DEFINE TEMP-TABLE {&ttResult} NO-UNDO LIKE ped-venda.

PROCEDURE iniciarBo:
RUN esbo/boAcomp.p PERSIST SET hAcomp.
END PROCEDURE.

PROCEDURE finalizarBo:
IF VALID-HANDLE(hAcomp) THEN
   RUN finalizar IN hAcomp.
DELETE PROCEDURE THIS-PROCEDURE.    
END PROCEDURE.  

PROCEDURE setParamsIni:
    FIND FIRST {&ttParam} NO-ERROR.
    ASSIGN {&ttParam}.data[1] = 01.01.2001
           {&ttParam}.data[2] = 01.01.2099
           {&ttParam}.estab[1] = ''
           {&ttParam}.estab[2] = 'zzzz'
           {&ttParam}.cliente[1] = 0
           {&ttParam}.cliente[2] = 99999
           {&ttParam}.repres[1] = ''
           {&ttParam}.repres[2] = 'zzzz'.

END PROCEDURE.



PROCEDURE exec: 
    
    FOR EACH ped-venda NO-LOCK
        WHERE ped-venda.dt-implant    >= {&ttParam}.data[1]
        AND   ped-venda.dt-implant    <= {&ttParam}.data[2]
        AND   ped-venda.cod-estabel   >= {&ttParam}.estab[1]
        AND   ped-venda.cod-estabel   <= {&ttParam}.estab[2]
        AND   ped-venda.cod-emitente  >= {&ttParam}.cliente[1]
        AND   ped-venda.cod-emitente  <= {&ttParam}.cliente[2]
        AND   ped-venda.no-ab-reppri  >= {&ttParam}.repres[1]
        AND   ped-venda.no-ab-reppri  <= {&ttParam}.repres[2].
        
        CREATE {&ttResult}.
        BUFFER-COPY ped-venda TO {&ttResult}.
        
    END.
        
   

END PROCEDURE.
