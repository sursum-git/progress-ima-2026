{esbo\boMsg.i}
//DEFINE VARIABLE percComisPad AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iRepres      AS INTEGER     NO-UNDO.
DEFINE VARIABLE cNomeAbrev   LIKE representante.nom_abrev   NO-UNDO.
DEFINE VARIABLE cEmpresa     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE rRowid       AS ROWID       NO-UNDO.

DEFINE VARIABLE hboMsgRepres AS HANDLE      NO-UNDO.

PROCEDURE iniciarBos:
    IF NOT VALID-HANDLE(hboMsgRepres) THEN
       RUN esbo/boMsg.p PERSISTENT SET hboMsgRepres.
    RUN limparTTMsg IN hboMsgRepres.
END PROCEDURE.

PROCEDURE finalizarBos:
    IF VALID-HANDLE(hboMsgRepres) THEN
       DELETE PROCEDURE hboMsgRepres.

END PROCEDURE.

PROCEDURE setCodRep:

    DEFINE INPUT  PARAMETER pCodRep AS INTEGER     NO-UNDO.
    ASSIGN iRepres = pCodRep.
    FIND representante 
        WHERE representante.cdn_repres = pCodRep
        NO-LOCK NO-ERROR.
    IF AVAIL representante THEN DO:
       ASSIGN  rRowid = ROWID(representante)
               cNomeAbrev = representante.nom_abrev .
    END.
    ELSE DO:
       ASSIGN rRowid  = ?
           cNomeAbrev = ''.  
    END.

END PROCEDURE.

PROCEDURE getCodRep:

    DEFINE OUTPUT PARAMETER pCodRep AS INTEGER   NO-UNDO.
    ASSIGN pCodRep = iRepres.

END PROCEDURE.


PROCEDURE setNomeAbrev:

    DEFINE INPUT  PARAMETER pNomeAbrev LIKE representante.nom_abrev   NO-UNDO.
    ASSIGN cNomeAbrev = pNomeAbrev.
    FIND FIRST representante 
        WHERE representante.nom_abrev = pNomeAbrev
        NO-LOCK NO-ERROR.
    IF AVAIL representante THEN DO:
       ASSIGN  rRowid = ROWID(representante)
               iRepres = representante.cdn_repres .
       /*MESSAGE 'achei o repres' pNomeAbrev
           VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    END.
    ELSE DO:
       ASSIGN rRowid  = ?
              iRepres = 0.  
    END.

END PROCEDURE.

PROCEDURE getNomeAbrev:

    DEFINE OUTPUT PARAMETER pNomeAbrev AS CHARACTER   NO-UNDO.
    ASSIGN pNomeAbrev = cNomeAbrev.

END PROCEDURE.



PROCEDURE setEmpresa:

    DEFINE INPUT  PARAMETER pEmpresa AS CHARACTER   NO-UNDO.
    ASSIGN cEmpresa = pEmpresa.

END PROCEDURE.

PROCEDURE getPercComisPad:
    DEFINE OUTPUT PARAMETER percComisPad AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE cEmpresaEms5 AS CHARACTER   NO-UNDO.
    RUN esapi/conv_Empresa_ems2_Ems5.p(cEmpresa,OUTPUT cEmpresaEms5).
    /*MESSAGE 'empresa' cEmpresaems5 SKIP
         'repres' iRepres
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    FIND FIRST repres_financ
        WHERE repres_financ.cod_empresa = cEmpresaEms5
        AND   repres_financ.cdn_repres = iRepres
        NO-LOCK NO-ERROR.
    IF AVAIL repres_financ THEN DO:
       ASSIGN percComisPad =  repres_financ.val_perc_comis_repres .
    END.
    ELSE DO:
       ASSIGN percComisPad =  0.
       RUN setMsg IN hboMsgRepres(1,'Repres NÆo encontrado para a empresa:' + cEmpresa,'erro').
    END.
   
                          

END PROCEDURE.
PROCEDURE getRegiaoGerencialRepres:
    DEFINE OUTPUT PARAMETER iRegiao AS INTEGER     NO-UNDO.

    FIND repres_classif
        WHERE repres_classif.cod_rep = iRepres
        AND   lista_id = 6
        NO-LOCK NO-ERROR.
    IF AVAIL repres_classif THEN
       ASSIGN iRegiao = repres_classif.opcao_id.

END PROCEDURE.


