DEFINE VARIABLE iRepresIni      AS INTEGER INIT 0                NO-UNDO.
DEFINE VARIABLE iRepresFim      AS INTEGER INIT 999999           NO-UNDO.
DEFINE VARIABLE logSoAtivos     AS LOGICAL INIT NO             NO-UNDO.
DEFINE VARIABLE dtIniEmissao    AS DATE INIT 01.01.1991        NO-UNDO.
DEFINE VARIABLE dtFimEmissao    AS DATE INIT 12.31.9999        NO-UNDO.
DEFINE VARIABLE cFornecedores   AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE empresaIni        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE empresaFim        AS CHARACTER   NO-UNDO.

{esbo/bo_repres.i}
       

PROCEDURE setDtEmissao:

DEFINE INPUT  PARAMETER dtIni AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER dtFim AS DATE        NO-UNDO.

ASSIGN dtIniEmissao = dtIni
       dtFimEmissao = dtFim.


END PROCEDURE.

PROCEDURE setSoAtivos:

ASSIGN  logSoAtivos = YES .


END PROCEDURE.


PROCEDURE retornarRepresentantes:

DEFINE OUTPUT PARAMETER TABLE FOR ttRepres.


END PROCEDURE.

PROCEDURE setEmpresa:
              
    DEFINE INPUT  PARAMETER pIni AS CHARACTER   NO-UNDO.              
    DEFINE INPUT  PARAMETER pFim AS CHARACTER   NO-UNDO.
    
    ASSIGN empresaIni = pIni
           empresaFim = pFim.
              
              
END PROCEDURE.

PROCEDURE buscarRepresentantes:

/*     MESSAGE datDesligIni  SKIP             */
/*             datDesligFim  SKIP             */
/*             iRepresIni    SKIP             */
/*             iRepresFim    SKIP             */
/*                                            */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */

FOR EACH representante FIELDS(cdn_repres nom_pessoa num_pessoa)
    WHERE (
            (logSoAtivos = YES AND 
            representante.dat_desligto = ?) 
            OR 
            (logSoAtivos = NO) 
          )
    AND   representante.cdn_repres   >= iRepresIni
    AND   representante.cdn_repres   <= iRepresFim   NO-LOCK:
    FOR FIRST pessoa_jurid FIELDS(num_pessoa_jurid) NO-LOCK
        WHERE pessoa_jurid.num_pessoa_jurid = representante.num_pessoa .
    END.
    IF NOT AVAIL pessoa_jurid THEN NEXT.
    
    FOR FIRST ems5.fornecedor FIELD(cdn_fornecedor)
        WHERE fornecedor.num_pessoa  = representante.num_pessoa
        NO-LOCK.
    END.
    
    FIND FIRST ttRepres
        WHERE ttRepres.codRepres  = representante.cdn_repres
        NO-LOCK NO-ERROR.
    /*MESSAGE AVAIL ttRepres
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    IF NOT AVAIL ttRepres THEN DO:
       CREATE ttRepres.
       ASSIGN ttRepres.codRepres    = representante.cdn_repres
              ttRepres.nomeRepres   = representante.nom_pessoa
              ttRepres.numPessoa    = representante.num_pessoa
              ttRepres.codEmitente  = IF AVAIL fornecedor THEN fornecedor.cdn_fornecedor ELSE 0.
    END.

END. 

END PROCEDURE.

PROCEDURE setRepresentante:

DEFINE INPUT  PARAMETER pRepres AS INTEGER     NO-UNDO.
ASSIGN iRepresIni = pRepres
       iRepresFim = pRepres.


END PROCEDURE.

PROCEDURE setFornecedores:

    DEFINE INPUT  PARAMETER pFornecedores AS CHAR  FORMAT 'x(50)'     NO-UNDO.
    ASSIGN cFornecedores = pFornecedores.
    
END PROCEDURE.

PROCEDURE getFornecsPorMatriz:

    DEFINE INPUT  PARAMETER pFornecedor   AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER cFornecedores AS CHARACTER   NO-UNDO.
    DEFINE BUFFER bfFornec FOR ems5.fornecedor .
    DEFINE BUFFER bfPJ     FOR ems5.pessoa_jurid .
    /*MESSAGE 'entrei'
            pfornecedor
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    FIND FIRST bfFornec 
        WHERE bfFornec.cdn_fornecedor = pFornecedor
        NO-LOCK NO-ERROR.
    FIND FIRST bfPJ
        WHERE bfPJ.num_pessoa_jurid = bfFornec.num_pessoa
        NO-LOCK NO-ERROR.

    IF AVAIL bfFornec THEN DO:
       FOR EACH pessoa_jurid                                                                              
           WHERE pessoa_jurid.num_pessoa_jurid_matriz = IF AVAIL bfPJ THEN bfPJ.num_pessoa_jurid_matriz ELSE 0  .                                      
           /*MESSAGE pessoa_jurid.num_pessoa_jurid                                                          
               VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                         */
           FIND FIRST ems5.fornecedor                                                                     
               WHERE ems5.fornecedor.num_pessoa =  pessoa_jurid.num_pessoa_jurid                          
               NO-LOCK NO-ERROR.                                                                          
           IF AVAIL ems5.fornecedor  AND ems5.fornecedor.cdn_fornecedor <> pFornecedor THEN DO:           
              /*MESSAGE fornecedor.cdn_fornecedor                                                           
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.   */
              IF cfornecedores <> '' THEN                                                                 
                 ASSIGN cFornecedores = cFornecedores + "," + string(fornecedor.cdn_fornecedor).          
              ELSE                                                                                        
                 ASSIGN cFornecedores = string(fornecedor.cdn_fornecedor).                                
           END.                                                                                           
       END.                                                                                               
    END.
END PROCEDURE.


PROCEDURE buscarNfsTit:


FIND FIRST ttRepres
    WHERE ttRepres.codRepres = iRepresIni
    NO-LOCK NO-ERROR.
IF NOT AVAIL ttRepres THEN DO:
   RUN buscarRepresentantes.
END.

FOR EACH  ems2med.docum-est NO-LOCK
   WHERE LOOKUP(string(ems2med.docum-est.cod-emitente),cFornecedores) > 0
   AND   ems2med.docum-est.dt-emissao >= dtIniEmissao
   AND   ems2med.docum-est.dt-emissao <= dtFimEmissao,
   EACH estabelec fields(cod-estabel ep-codigo) WHERE
   docum-est.cod-estabel = estabelec.cod-estabel
   AND estabelec.ep-codigo >= empresaIni
   AND estabelec.ep-codigo <= empresaFim:
   CREATE ttDocs.
   ASSIGN ttDocs.codEstabel     =  ems2med.docum-est.cod-estabel   
          ttDocs.codEmitente    =  ems2med.docum-est.cod-emitente  
          ttDocs.dtEmissao      =  ems2med.docum-est.dt-emissao    
          ttDocs.dtTransacao    =  ems2med.docum-est.dt-trans      
          ttDocs.nroDocto       =  ems2med.docum-est.nro-docto     
          ttDocs.serieDocto     =  ems2med.docum-est.serie-docto   
          ttDocs.valorNota      =  ems2med.docum-est.tot-valor.
   FOR FIRST ems2med.dupli-apagar OF ems2med.docum-est NO-LOCK.
       FOR EACH tit_ap NO-LOCK
           WHERE tit_ap.cdn_fornec      = ems2med.dupli-apagar.cod-emitente     
           AND   tit_ap.cod_ser_doc     = ems2med.dupli-apagar.serie-docto      
           AND   tit_ap.cod_espec_docto = ems2med.dupli-apagar.cod-esp          
           AND   tit_ap.cod_tit_ap      = ems2med.dupli-apagar.nro-docto        
           AND   tit_ap.cod_parcela     = ems2med.dupli-apagar.parcela           
           AND   tit_ap.dat_emis        = ems2med.dupli-apagar.dt-emissao 
           AND   tit_ap.LOG_sdo_tit_ap   = NO.
           ASSIGN ttDocs.dtVencto       =  tit_ap.dat_vencto        
                  ttDocs.valorLiquido   =  tit_ap.val_origin_tit_ap. 
       END.
   END.
END.
/*FOR EACH  dbaux.docum-est NO-LOCK
   WHERE dbaux.docum-est.cod-emitente = ttRepres.codEmitente
   AND   dbaux.docum-est.dt-emissao >= dtIniEmissao
   AND   dbaux.docum-est.dt-emissao <= dtFimEmissao:
   CREATE ttDocs.
   ASSIGN ttDocs.codEstabel     =  dbaux.docum-est.cod-estabel   
          ttDocs.codEmitente    =  dbaux.docum-est.cod-emitente  
          ttDocs.dtEmissao      =  dbaux.docum-est.dt-emissao    
          ttDocs.dtTransacao    =  dbaux.docum-est.dt-trans      
          ttDocs.nroDocto       =  dbaux.docum-est.nro-docto     
          ttDocs.serieDocto     =  dbaux.docum-est.serie-docto   
          ttDocs.valorNota      =  dbaux.docum-est.tot-valor.
   FOR FIRST dbaux.dupli-apagar OF dbaux.docum-est NO-LOCK.
       FOR EACH tit_ap NO-LOCK
           WHERE tit_ap.cdn_fornec      = dbaux.dupli-apagar.cod-emitente     
           AND   tit_ap.cod_ser_doc     = dbaux.dupli-apagar.serie-docto      
           AND   tit_ap.cod_espec_docto = dbaux.dupli-apagar.cod-esp          
           AND   tit_ap.cod_tit_ap      = dbaux.dupli-apagar.nro-docto        
           AND   tit_ap.cod_parcela     = dbaux.dupli-apagar.parcela           
           AND   tit_ap.dat_emis        = dbaux.dupli-apagar.dt-emissao 
           AND   tit_ap.LOG_sdo_tit_ap   = NO.
           ASSIGN ttDocs.dtVencto       =  tit_ap.dat_vencto        
                  ttDocs.valorLiquido   =  tit_ap.val_origin_tit_ap. 
       END.
   END.
END. */


END PROCEDURE.

PROCEDURE retornarNfsTit.

DEFINE OUTPUT PARAMETER TABLE FOR ttDocs.


END PROCEDURE.


PROCEDURE setNfsTit.

DEFINE INPUT PARAMETER TABLE FOR ttDocs.


END PROCEDURE.


PROCEDURE exportarNfsTit.

DEFINE INPUT PARAMETER cArquivo AS CHARACTER   NO-UNDO.
OUTPUT TO VALUE(cArquivo).
PUT "estab.|c¢digo Fornec.|dt.emissÆo|dt.transa‡Æo|documento|S‚rie|Valor Bruto|Dt.Vencto|Valor Liquido Pago" SKIP.
FOR EACH ttDocs BY ttDocs.dtEmissao:
    EXPORT DELIMITER "|" ttDocs.
END.
OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE sumarizarMesEmissao:

FOR EACH ttDocs:
     FIND FIRST ttMeses
         WHERE ttMeses.ano = year(ttDocs.dtEmissao)
         AND   ttMeses.mes = MONTH(ttDocs.dtEmissao)
         AND   ttMeses.empresa = ttdocs.codEstabel
         NO-ERROR.
     IF NOT AVAIL ttMeses THEN DO:
        CREATE ttMeses.
        ASSIGN ttMeses.ano      = year(ttDocs.dtEmissao) 
               ttMeses.mes      = MONTH(ttDocs.dtEmissao)
               ttmeses.empresa  = ttDocs.codEstabel .
     END.
     ASSIGN ttMeses.valorNota       = ttMeses.valorNota     + ttDocs.valorNota
            ttMeses.valorLiquido    = ttMeses.valorLiquido  + ttDocs.valorLiquido.
END.

END PROCEDURE.

PROCEDURE retornarMesEmissao:

DEFINE OUTPUT PARAMETER TABLE FOR ttMeses.

END PROCEDURE.


PROCEDURE exportarMesEmissao:

DEFINE INPUT PARAMETER cArquivo AS CHARACTER   NO-UNDO.

OUTPUT TO VALUE(cArquivo).
PUT "Ano|Mˆs|Valor Bruto|Valor Liquido Pago|Empresa" SKIP.
FOR EACH ttMeses BY ttMeses.ano BY ttMeses.mes:
    EXPORT DELIMITER "|" ttMeses.
END.
OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE retornarQtMeses:

DEFINE OUTPUT PARAMETER iQt     AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER iQtIMA  AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER iQtMED  AS INTEGER     NO-UNDO.
ASSIGN iQt = 0
       iQtIMA = 0
       iQtMED = 0.
FOR EACH ttMeses BREAK BY ttMeses.ano BY ttMeses.mes:

    IF FIRST-OF(ttMeses.mes) THEN
       ASSIGN iQt = iQt + 1.
    CASE ttMeses.empresa :
        WHEN '1' THEN
            ASSIGN iQtIMA = iQtIMA + 1.
        WHEN '5' THEN
            ASSIGN iQtMED = iQtMED + 1.

    END CASE.
END.
END PROCEDURE.


PROCEDURE retornarVlTotal:
DEFINE OUTPUT PARAMETER valorNota           AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER valorLiquido        AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER valorNotaIMA        AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER valorLiquidoIMA     AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER valorNotaMED        AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER valorLiquidoMED     AS DECIMAL     NO-UNDO.

FOR EACH ttDocs:
    ASSIGN valorNota     = valorNota + ttDocs.valorNota
           valorLiquido  = valorLiquido + ttDocs.valorLiquido. 
    CASE ttDocs.codEstabel:
        WHEN '1' THEN
          ASSIGN valorNotaIMA        = valorNotaIMA + ttDocs.valorNota
                 valorLiquidoIMA     = valorLiquidoIMA + ttDocs.valorLiquido.
        WHEN '5' THEN
         ASSIGN valorNotaMED           = valorNotaMED + ttDocs.valorNota
                valorLiquidoMED        = valorLiquidoMED + ttDocs.valorLiquido.

    END CASE.
END.                                                         

END PROCEDURE.

PROCEDURE limparTTs.

EMPTY TEMP-TABLE ttRepres.
EMPTY TEMP-TABLE ttDocs.
EMPTY TEMP-TABLE ttMeses.


END PROCEDURE.



/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod-emitente                     inte        im
   20 serie-docto                      char        im
   25 cod-esp                          char        m
   30 nro-docto                        char        im
   40 parcela                          char        im
   55 dt-emissao                       date        m
   60 dt-trans                         date        m
   80 dt-vencim                        date        m
  110 vl-a-pagar                       deci-2      m
  150 desconto                         deci-2      m
  160 dt-venc-desc                     date
  170 esp-movto                        inte        m
  190 estado-cq                        inte        m
  200 nat-operacao                     char        im
  210 vl-desconto                      deci-2      m
  220 vl-ap-fasb                       deci-2[2]   m
  230 nr-duplic                        char        im
  240 observacao                       char        m
  250 perc-juros                       deci-5      m
  260 dias-atraso                      inte        m
  270 perc-mora                        deci-5      m
  280 ep-codigo                        char
  290 cod-estabel                      char        im
  300 tp-despesa                       inte
  370 historico                        char        m
  380 aliq-irf                         deci-2      m
  390 esp-irf                          char        m
  400 cod-imp-irf                      char        m
  410 cod-ret-irf                      inte        m
  420 dia-util-irf                     inte
  430 imposto-ir                       deci-3
  440 imposto-iss                      deci-3
  450 imposto-inss                     deci-3
  460 char-1                           char
  470 char-2                           char
  480 dec-1                            deci-8
  490 dec-2                            deci-8
  500 int-1                            inte
  510 int-2                            inte
  520 log-1                            logi
  530 log-2                            logi
  540 data-1                           date
  550 data-2                           date
  560 vl-taxa                          deci-2
  570 vl-frete                         deci-2
  580 vl-outros                        deci-2
  590 Valor-a-pagar-me                 deci-2
  600 check-sum                        char
  610 mo-codigo                        inte
  620 vl-a-pagar-mo                    deci-2
  630 vl-descto-mo                     deci-2
  640 val-difer-remito-fatur           deci-2
  650 val-difer-remito-fatur-me        deci-5
*/
