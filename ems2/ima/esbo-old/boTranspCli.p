/********************************************************************
programa: esbo/boTranspCli.p
objetivo: Implementar busca da Transportadora do Cliente conforme 
o estabelecimento de Origem
*******************************************************************/
{esp/util.i}
{esbo/boTranspCli.i ttParam }

 DEFINE VARIABLE hBoEmitente   AS HANDLE      NO-UNDO.
 DEFINE VARIABLE hBoes150      AS HANDLE      NO-UNDO.
 DEFINE VARIABLE iTransp       AS INT         NO-UNDO.
 DEFINE VARIABLE idTranspEstab AS INTEGER     NO-UNDO.

{esp/setProp.i ttParam}
PROCEDURE iniciar:
CREATE ttParam.
RUN esbo/boEmitente.p PERSISTENT SET hboEmitente.
RUN esbo/boEs150.p   PERSISTENT SET hBoEs150.
RUN iniciar IN hboEs150.

END.

PROCEDURE finalizar:

IF VALID-HANDLE(hBoEmitente) THEN
   DELETE PROCEDURE hBoEmitente.
IF VALID-HANDLE(hBoEs150) THEN
   RUN finalizar IN hBoEs150.

DELETE PROCEDURE THIS-PROCEDURE.

END PROCEDURE.

PROCEDURE setTTParam:

    DEFINE INPUT PARAMETER TABLE FOR ttParam.

END PROCEDURE.



PROCEDURE exec:

   DEFINE VARIABLE cCidade AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE iIBGE   AS INTEGER     NO-UNDO.
   DEFINE VARIABLE cParam  AS CHAR        NO-UNDO.
   FIND FIRST ttParam NO-ERROR.
   RUN setCodEmitente IN hboEmitente(ttParam.codCliente).
   CASE ttParam.codEstab:
       WHEN '5' THEN DO:
           RUN getTransportador IN hboEmitente(OUTPUT iTransp).
           IF ttParam.codTpFrete = 'Cif at‚ Redesp' THEN DO:
           RUN getvlParametro('cod_transp_redesp_5',OUTPUT cParam).
                IF cParam = '' THEN
                ASSIGN iTransp = 640.
           ELSE
                ASSIGN iTransp = INT(cParam).
           END.
       END.
       WHEN '505' THEN DO:
           RUN getCidade   IN hboEmitente(OUTPUT cCidade, OUTPUT iIBGE). 
           RUN setProp IN hBoEs150('codEstab',0,ttParam.codEstab) .
           RUN setProp IN hBoEs150('codTpFrete',0,ttParam.codTpFrete).
           IF iIBGE > 0 THEN
              RUN setProp IN hBoEs150('ibge',0,iIBGE) .
           ELSE
              RUN setProp IN hBoEs150('cidade',0,cCidade) .
          RUN exec  IN hBoEs150.
          RUN getTransportadora IN hBoEs150(OUTPUT iTransp, OUTPUT idTranspEstab).
       END.
   END CASE.



END PROCEDURE.


PROCEDURE getTransportadora:
    DEFINE OUTPUT PARAMETER pTransp AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER pId     AS INTEGER     NO-UNDO.
    ASSIGN pTransp = iTransp
           pid     = idTranspEstab .
END PROCEDURE.





