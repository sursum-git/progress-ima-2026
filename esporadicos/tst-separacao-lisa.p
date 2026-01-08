{esp/using_json.i}
{esp/util.i}
{esp/utiljson.i}
{esapi/analisarJsonObject2.i}
{esp/ttChave.i}
DEFINE VARIABLE oPedido  AS jsonObject   NO-UNDO.
DEFINE VARIABLE cArquivo AS CHARACTER   NO-UNDO FORMAT 'x(80)'.
DEFINE VARIABLE cArqCriado AS CHARACTER   NO-UNDO.
DEFINE VARIABLE idTransacao AS INTEGER     NO-UNDO.
ASSIGN cArquivo = '\\pv1\pv2\integracao_lisa\'.
UPDATE cARquivo WITH WIDTH 550.

RUN convFileToJson(cArquivo,OUTPUT oPedido).

/*RUN esapi/criarLogAPIIMA2.p( 
       INPUT 'pdp_api_v1_pedvenda',
       INPUT 54,
       INPUT 'api_ima_pedvenda_',
       INPUT oPedido,
       OUTPUT cArqCriado,
       OUTPUT idTransacao
       ).

 MESSAGE cArqCriado SKIP
         idTransacao

     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

*/

RUN esapi/analisarJsonObject2.p(oPedido, OUTPUT TABLE ttJson).
RUN esapi/retorno-Isf-Lisa.p(INPUT TABLE ttJson,OUTPUT TABLE ttChave) NO-ERROR.
FOR EACH ttChave:
    DISP ttChave.
END.

CATCH eSysError AS Progress.Lang.SysError:
    MESSAGE "From CATCH block..." SKIP 
             eSysError:GetMessage(1) 
        VIEW-AS ALERT-BOX.
END CATCH.


 CATCH twoError AS Progress.Lang.AppError:
        MESSAGE twoError:GetMessage(1) VIEW-AS ALERT-BOX.
    END CATCH.
