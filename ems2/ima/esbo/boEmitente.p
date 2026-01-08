{esbo\boMsg.i}
DEFINE VARIABLE iEmitente               AS INTEGER     NO-UNDO.
DEFINE VARIABLE cNomeAbrev              LIKE emitente.nome-abrev   NO-UNDO.
DEFINE VARIABLE RROWID                  AS ROWID       NO-UNDO.
DEFINE VARIABLE rRowidExt               AS ROWID       NO-UNDO.
DEFINE TEMP-TABLE ttEmitente            LIKE emitente.
DEFINE TEMP-TABLE ttEmitenteExt         LIKE ext-emitente.
DEFINE VARIABLE hBoMsgEmitente          AS HANDLE      NO-UNDO.
DEFINE VARIABLE lDesconsiderarInativo   AS LOGICAL     NO-UNDO .

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

PROCEDURE iniciarBos:
    IF NOT VALID-HANDLE(hBoMsgEmitente) THEN
       RUN esbo/boMsg.p PERSISTENT SET hBoMsgEmitente.
    RUN limparTTMsg IN hboMsgEmitente.


END PROCEDURE.

PROCEDURE finalizarBos:
    IF VALID-HANDLE(hBoMsgEmitente) THEN
       DELETE PROCEDURE hBoMsgEmitente.

END PROCEDURE.
PROCEDURE setCodEmitente:
    DEFINE INPUT  PARAMETER pEmitente AS INTEGER     NO-UNDO.
    ASSIGN iEmitente = pEmitente.

    FIND emitente 
        WHERE emitente.cod-emitente = pEmitente NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN DO:
       FIND ext-emitente OF emitente NO-LOCK NO-ERROR.
       ASSIGN  rRowid = ROWID(emitente)
               rRowidExt = ROWID(ext-emitente)
               cNomeAbrev = emitente.nome-abrev .
    END.
    ELSE DO:
       ASSIGN rRowid  = ?
           rRowidExt  = ? 
           cNomeAbrev = ''.  
    END.
    

END PROCEDURE.

PROCEDURE setNomeAbrev:
    DEFINE INPUT  PARAMETER pNomeAbrev LIKE emitente.nome-abrev  NO-UNDO.

    IF pNomeAbrev <> '' THEN DO.
       ASSIGN cNomeAbrev = pNomeAbrev.
       FIND emitente 
           WHERE emitente.nome-abrev = pNomeAbrev
           NO-LOCK NO-ERROR.

        IF AVAIL emitente THEN DO:
           FIND ext-emitente OF emitente
               NO-LOCK NO-ERROR.
           ASSIGN  rRowid = ROWID(emitente)
                   rRowidExt = ROWID(ext-emitente)
                   iEmitente = emitente.cod-emitente .
        END.
        ELSE DO:
           ASSIGN rRowid  = ?
               rRowidExt  = ? 
               iEmitente = 0 .  
        END.
    END.

END PROCEDURE.



PROCEDURE getUf.
    DEFINE OUTPUT PARAMETER cUf AS CHARACTER   NO-UNDO.
    FIND emitente
        WHERE rowid(emitente) =rRowid
        NO-LOCK NO-ERROR.
    ASSIGN cUf = IF AVAIL emitente THEN emitente.estado ELSE ''.


END PROCEDURE.

PROCEDURE getCidade.
    DEFINE OUTPUT PARAMETER pCidade AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pIBGE   AS INTEGER     NO-UNDO.
    

    FIND emitente
        WHERE rowid(emitente) =rRowid
        NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN DO:
       ASSIGN pCidade =  emitente.cidade .
       FIND FIRST ems2med.cidade OF emitente NO-LOCK NO-ERROR.
       ASSIGN pIBGE = IF AVAIL cidade THEN cidade.cdn-munpio-ibge ELSE 0.
            
    END.                                                                 

END PROCEDURE.


PROCEDURE getNomeAbrev.
    DEFINE OUTPUT PARAMETER  pNomeAbrev LIKE emitente.nome-abrev   NO-UNDO.
    ASSIGN pNomeAbrev = cNomeAbrev.
   


END PROCEDURE.

PROCEDURE getCodEmitente.
    DEFINE OUTPUT PARAMETER pEmitente LIKE emitente.cod-Emitente   NO-UNDO.
   
    ASSIGN pEmitente = iEmitente.


END PROCEDURE.

PROCEDURE getNomeEmit.
    DEFINE OUTPUT PARAMETER  cNomeEmit LIKE emitente.nome-emit   NO-UNDO.
    FIND emitente
        WHERE rowid(emitente) =rRowid
        NO-LOCK NO-ERROR.
    ASSIGN cNomeEmit = IF AVAIL emitente THEN emitente.nome-emit ELSE ''.

END PROCEDURE.

PROCEDURE getRegEmit.
    DEFINE OUTPUT PARAMETER TABLE FOR ttEmitente.
    EMPTY TEMP-TABLE ttEmitente.
    FIND emitente
        WHERE rowid(emitente) =rRowid
        NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN DO:
       CREATE ttEmitente.
       BUFFER-COPY emitente TO ttEmitente.
    END.
    

END PROCEDURE.

PROCEDURE getRegEmitExt.
    DEFINE OUTPUT PARAMETER TABLE FOR ttEmitenteExt.
    EMPTY TEMP-TABLE ttEmitenteExt.
    FIND ext-emitente
        WHERE rowid(ext-emitente) = rRowidExt NO-LOCK NO-ERROR.
    IF AVAIL ext-emitente THEN DO:
       CREATE ttEmitenteExt.
       BUFFER-COPY ext-emitente TO ttEmitente .
    END.
END PROCEDURE.

PROCEDURE getColigPrinc:
    DEFINE BUFFER bfEmit FOR emitente.
    DEFINE OUTPUT PARAMETER cColigada AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iColigada AS INTEGER     NO-UNDO INIT 0.
    DEFINE VARIABLE iCont     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE coligadaCorrente AS CHARACTER   NO-UNDO.
    FIND CURRENT ext-emitente NO-LOCK NO-ERROR.
    IF NOT AVAIL ext-emitente THEN
       ASSIGN iColigada = iEmitente.
    ELSE DO:
       IF ext-emitente.coligada = '' THEN
          ASSIGN iColigada = iEmitente.
       ELSE DO:
           REPEAT iCont = 1 TO NUM-ENTRIES(ext-emitente.coligada,','):
              ASSIGN coligadaCorrente = ENTRY(iCont,ext-emitente.coligada,',').
              IF INT(coligadaCorrente) < iColigada THEN
                 ASSIGN iColigada = INT(coligadaCorrente).
              IF iColigada = 0 THEN
                 ASSIGN iColigada = INT(coligadaCorrente).
           END.
       END.    
    END.    
    IF iEmitente < iColigada THEN
       ASSIGN iColigada = iEmitente.
    FIND bfEmit
        WHERE bfEmit.cod-emitente = iColigada
        NO-LOCK NO-ERROR.
    ASSIGN cColigada = IF AVAIL bfEmit THEN STRING(bfEmit.cod-emitente) + '-' + bfEmit.nome-emit ELSE ''.
END PROCEDURE.


/*PROCEDURE getSitCli:

    DEFINE VARIABLE clista AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iSit   AS INTEGER       NO-UNDO.
    DEFINE OUTPUT PARAMETER cRetorno AS CHARACTER   NO-UNDO.
    ASSIGN cLista = {adinc/i10ad098.i 3}
           cLista =  cLista + ',Bloqueado Adm.Vendas'.


    FIND CURRENT emitente NO-LOCK NO-ERROR.
    FIND CURRENT ext-emitente NO-LOCK NO-ERROR.

    IF AVAIL emitente THEN
       ASSIGN iSit =  emitente.ind-cre-cli.
    IF AVAIL ext-emitente AND ext-emitente.hist_aval_cli_id <> 0 THEN
       ASSIGN iSit = 6.

    ASSIGN cRetorno = ENTRY(iSit,cLista).


END PROCEDURE.*/

PROCEDURE getSitCliAdmVendas:
    DEFINE OUTPUT PARAMETER lBloqueado AS LOGICAL     NO-UNDO.
    FIND CURRENT ext-emitente NO-LOCK NO-ERROR.
    IF AVAIL ext-emitente AND ext-emitente.hist_aval_cli_id <>  0 THEN DO:
       FIND FIRST hist_aval_cli
           WHERE hist_aval_cli.hist_aval_cli_id = ext-emitente.hist_aval_cli_id
           AND   hist_aval_cli.cod_sit_aval = 1
           NO-LOCK NO-ERROR.
       ASSIGN lBloqueado = AVAIL hist_aval_cli.

    END.
    ELSE 
      ASSIGN lBloqueado = FALSE.

END PROCEDURE.

PROCEDURE setDesconsiderarInativo:
    DEFINE INPUT  PARAMETER pDesconsiderar AS LOGICAL     NO-UNDO.
    ASSIGN lDesconsiderarInativo = pDesconsiderar.
END PROCEDURE.

PROCEDURE getCodRepresCliente:

    DEFINE OUTPUT PARAMETER iRepres AS INTEGER     NO-UNDO.

    FIND CURRENT emitente NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN
       ASSIGN iRepres = emitente.cod-rep.

    IF lDesconsiderarInativo THEN DO:
       FIND ext-emitente OF emitente NO-LOCK NO-ERROR.
       IF AVAIL ext-emitente AND ext-emitente.situacao = 2 THEN
          ASSIGN iRepres = 0.
    END.



END PROCEDURE.

PROCEDURE getRegimeSimples:


  DEFINE OUTPUT PARAMETER pSimples AS LOGICAL     NO-UNDO.    

  FIND CURRENT emitente NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN DO:
     ASSIGN pSimples = IF substr(emitente.char-1,133,1) = 'S'
     THEN YES ELSE  NO .
  END.

END PROCEDURE.


PROCEDURE getEnquadrFiscal:
    DEFINE INPUT  PARAMETER  cEnquadTtd AS CHAR NO-UNDO.
    FIND CURRENT ext-emitente NO-LOCK NO-ERROR.
    IF AVAIL ext-emitente  THEN DO:
       ASSIGN cEnquadTtd = ext-emitente.enquad_ttd .
    END.
    ELSE 
      ASSIGN cEnquadTtd = ''.

END PROCEDURE.

PROCEDURE getTransportador:
    DEFINE OUTPUT PARAMETER pTransp AS INT    NO-UNDO.    

  FIND CURRENT emitente NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN DO:
     ASSIGN pTransp = emitente.cod-transp .
  END.

END PROCEDURE.
