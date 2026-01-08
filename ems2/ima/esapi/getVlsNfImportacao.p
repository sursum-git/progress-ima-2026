/**********************************************************************************************
**
**  Programa: esapi/getVlsNFImportacao.p
**
**  Objetivo: Retornar os impostos e despesas da nota fiscal de importa‡Æo em uma tabela
**  temporaria para posterior utiliza‡Æo
**
**  data: 08/2025
** autor:Tadeu silva
*********************************************************************************************/
{esapi/getVlsNfImportacao.i ttDanfeAux}


DEFINE INPUT  PARAMETER rowidNFVenda AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttDanfeAux .
DEFINE VARIABLE de-vl-contab AS DECIMAL     NO-UNDO.
FOR FIRST nota-fiscal 
    FIELDS(cod-estabel serie nr-nota-fis nat-operacao esp-docto char-1)
    WHERE ROWID(nota-fiscal) = rowidNFVenda:
    
    CREATE ttDanfeAux.
    ASSIGN 
           ttDanfeAux.vlFOB        = 0
           ttDanfeAux.vliinota     = 0
           ttDanfeAux.vlpisnota    = 0
           ttDanfeAux.vlcofinsnota = 0           
           .
    ASSIGN ttDanfeAux.nrContainer = int(replace(substr(nota-fiscal.char-1,1,10),"/","")).  
    RUN esapi/getDescrTTProcsFornecImpContainer.p(ttDanfeAux.nrContainer,OUTPUT ttDanfeAux.pedsImp).    
    
    FOR FIRST docum-est 
    FIELDS(serie-docto cod-emitente nro-docto cod-emitente nat-operacao char-1)
         WHERE docum-est.serie-docto  = nota-fiscal.serie
         AND   docum-est.nro-docto    = nota-fiscal.nr-nota-fis
         AND   docum-est.cod-emitente = nota-fiscal.cod-emitente
         AND   docum-est.nat-operacao = nota-fiscal.nat-operacao
         NO-LOCK .         
    END.    
    
    
    FOR EACH it-nota-fisc 
       fields(nr-nota-fis serie cod-estabel it-codigo cod-refer char-1 char-2 vl-icms-it vl-tot-item)  OF nota-fiscal NO-LOCK:
       FOR FIRST item-doc-est 
           FIELDS(it-codigo cod-refer nro-docto serie-docto cod-emitente nat-operacao val-base-calc-cofins) 
           OF docum-est 
           WHERE item-doc-est.it-codigo = it-nota-fisc.it-codigo 
           AND   item-doc-est.cod-refer = it-nota-fisc.cod-refer
           NO-LOCK.
       END.
       IF nota-fiscal.esp-docto = 21  // NFE 
       OR nota-fiscal.esp-docto = 20 // NFD
       THEN
          ASSIGN de-vl-contab = IF AVAIL item-doc-est THEN item-doc-est.val-base-calc-cofins ELSE 0.
       ELSE
           ASSIGN de-vl-contab = it-nota-fisc.vl-tot-item.   
           
       ASSIGN de-vl-contab = de-vl-contab - it-nota-fisc.vl-icms-it.
       
       

       ASSIGN ttDanfeAux.vlpisnota    = ttDanfeAux.vlpisnota + 
                                        (de-vl-contab * DEC(SUBSTR(it-nota-fisc.char-2,76,5)) / 100)
             ttDanfeAux.vlcofinsnota  = ttDanfeAux.vlcofinsnota + 
                                       (de-vl-contab * DEC(SUBSTR(it-nota-fisc.char-2,81,5)) / 100).  
                             
    /***********************************************************************************
    MESSAGE  
       //item-doc-est.val-base-calc-cofins SKIP
         it-nota-fisc.it-codigo
       AVAIL item-doc-est SKIP
      de-vl-contab SKIP
      DEC(SUBSTR(it-nota-fisc.char-2,76,5)) SKIP
                DEC(SUBSTR(it-nota-fisc.char-2,81,5))
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. 
     ***********************************************************************************/
       
        
    END.
    
    IF NOT AVAIL docum-est THEN
    DO:
        RETURN 'nok'.
        
    END.
    
    
    //Valores 
    ASSIGN ttDanfeAux.vlFOB     =  nota-fiscal.vl-mercad
           ttDanfeAux.vlFrete   =  nota-fiscal.vl-frete
           ttDanfeAux.vlSeguro  =  nota-fiscal.vl-seguro
          .
    
    //DI
    FOR FIRST embarque-imp FIELDS(embarque declaracao-import)
    WHERE embarque-imp.embarque = trim(SUBSTR(docum-est.char-1,1,20))NO-LOCK:
    END.
    IF AVAIL embarque-imp THEN
    DO:
        ASSIGN ttDanfeAux.di = embarque-imp.declaracao-import .
        
    END.
    /*MESSAGE AVAIL embarque-imp  SKIP
            embarque-imp.declaracao-import  SKIP
             SUBSTR(docum-est.char-1,21,20)
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    // Busca o Imposto de Importa‡Æo
    FOR EACH docum-est-cex WHERE 
             docum-est-cex.serie-docto  = docum-est.serie-docto AND 
             docum-est-cex.nro-docto    = docum-est.nro-docto  AND 
             docum-est-cex.cod-emitente = docum-est.cod-emitente AND 
             docum-est-cex.nat-operacao = docum-est.nat-operacao NO-LOCK.
        IF docum-est-cex.cod-desp     = 1 THEN
          ASSIGN ttDanfeAux.vlIINota = docum-est-cex.val-desp.

        IF docum-est-cex.cod-desp           = 58 THEN
           ASSIGN ttDanfeAux.vlDespesas     = docum-est-cex.val-desp.
    END.

    // Retira o Pis e Cofins das Despesas
    IF DEC(ttDanfeAux.vlDespesas) > 0 THEN DO.
       ASSIGN ttDanfeAux.vlDespesas = DEC(ttDanfeAux.vlDespesas) - DEC(ttDanfeAux.vlpisnota) - DEC(ttDanfeAux.vlcofinsnota).
       IF ttDanfeAux.vlDespesas < 0 THEN
          ASSIGN ttDanfeAux.vlDespesas = 0.
    END.
    
    

END.

