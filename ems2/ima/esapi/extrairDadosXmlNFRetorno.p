DEFINE TEMP-TABLE ttReg NO-UNDO LIKE retornos_lisa.
DEFINE INPUT-OUTPUT PARAMETER TABLE for  ttReg .
DEFINE INPUT  PARAMETER lcXML  AS LONGCHAR   NO-UNDO.
DEFINE OUTPUT PARAMETER retOk  AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER pErroProgress AS CHARACTER   NO-UNDO.

{xsd/retorno/schema0.i}


DEFINE VARIABLE iCont   AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDtEmis AS CHARACTER   NO-UNDO.
assign retOK = dataset nfeProcDset:READ-XML('longchar',   
                                        lcXML,
                                        "empty",
                                        ?,
                                        ?,
                                        ?,
                                        "ignore")
                                         NO-ERROR.

{esp/erroProgress.i}
ASSIGN pErroProgress = cErroProgress.


FIND FIRST ttReg NO-ERROR.
IF NOT AVAIL ttReg THEN DO:
   CREATE ttReg.
END.

//buscar data,serie e NF
FIND FIRST ide NO-ERROR.
IF AVAIL ide THEN DO:
   ASSIGN ttReg.nr_nota_fis = string(ide.nNF,'9999999')
          ttReg.serie       = string(ide.serie)
          cDtEmis           = ide.dhemi.
   RUN esapi/getdataDeDthrCompleta.p(cDtEmis,OUTPUT ttReg.dt_nf).
END.


//busca fornecedor
FIND FIRST emit NO-ERROR.
IF AVAIL emit THEN DO:
   FIND LAST emitente NO-LOCK
       WHERE emitente.cgc = string(emit.cnpj) NO-ERROR.
   IF AVAIL emitente THEN DO:
      ASSIGN ttReg.cod_emitente = emitente.cod-emitente .
       // criar campo de cod-emitente e colocar aqui
   END.                
END.
 

//busca estabelecimento
FIND FIRST dest NO-ERROR.
IF AVAIL dest THEN DO:
   FIND LAST estabelec NO-LOCK
       WHERE estabelec.cgc = string(dest.cnpj)
       NO-ERROR.
   IF AVAIL estabelec THEN DO:
      ASSIGN ttReg.cod_estabel = estabelec.cod-estabel.
   END.                
END.


//busca chave
FIND FIRST infProt NO-ERROR.
IF AVAIL infProt THEN
   ASSIGN ttReg.chave = STRING(infProt.chnfe).

// busca chave das nota fiscais referenciadas
FOR EACH NFref.
    ASSIGN iCont = iCont + 1.
    ASSIGN ttReg.chaves_nfs_refer[iCont] = string(nfref.refnfe) .
END.


// busca o valor da nota fiscal
FIND FIRST IcmsTot NO-ERROR.
IF AVAIL IcmsTot THEN DO:
   ASSIGN ttReg.vl_nota = IcmsTot.vNF .
END.

{esp/retornarErrosMsg.i}
