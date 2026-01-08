/*************************************************************************************
Programa:boConsParam
Objetivo: Consulta de parametros 
Autor: Tadeu Silva Parreiras
Data: Outubro/2020
03/10/2022 - inclus∆o do parametro de natureza de operaá∆o de importaá∆o 
04/2024 - inclus∆o do procedimento getTipoNFAprovPedLisa  
06/2024 - inclus∆o do procedimento getEspd004aAut
***************************************************************************************/
DEFINE VARIABLE cCodParam    AS CHARACTER   NO-UNDO.
{esp/convDirOs.i}
PROCEDURE setCodParam:

    DEFINE INPUT  PARAMETER pCodParam AS CHARACTER   NO-UNDO.
    ASSIGN cCodParam = pCodParam.
    FIND im-param
        WHERE im-param.cod-param = cCodParam
        NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE getVlParam:
    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    FIND CURRENT im-param NO-LOCK NO-ERROR.
    IF AVAIL im-param THEN
       ASSIGN vlParam = im-param.val-param.
    


END PROCEDURE.


PROCEDURE getVlParam2:
    DEFINE INPUT  PARAMETER pValorPadrao AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    FIND CURRENT im-param NO-LOCK NO-ERROR.
    IF AVAIL im-param THEN
       ASSIGN vlParam = im-param.val-param.

    IF vlParam = '' THEN
       ASSIGN vlParam = pValorPadrao.
    


END PROCEDURE.


PROCEDURE getQtMinMtBook:
    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    

    RUN setCodParam('qt_minima_mt_book').
    RUN getVlParam(OUTPUT vlParam).
    IF vlParam = '' THEN
       ASSIGN vlParam = '5'.

END PROCEDURE.


PROCEDURE getQtMinKgBook:
    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('qt_minima_kg_book').
    RUN getVlParam(OUTPUT vlParam).
    IF vlParam = '' THEN
       ASSIGN vlParam = '2'.

END PROCEDURE.

PROCEDURE getEmailsErrosArqDesign:
    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.

    RUN setCodParam('emails_erros_arq_design').
    RUN getVlParam(OUTPUT vlParam).
    

END PROCEDURE.

PROCEDURE getCalendAprovGer:
    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('calend_aprov_ger').
    RUN getVlParam(OUTPUT vlParam).
    IF vlParam = '' THEN
       ASSIGN vlParam = 'FISCAL'.

END PROCEDURE.

PROCEDURE getNatOperacaoImp:

    //NAT-OPERACAO-IMP
    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    

    RUN setCodParam('NAT-OPERACAO-IMP').
    RUN getVlParam(OUTPUT vlParam).
    IF vlParam = '' THEN
       ASSIGN vlParam = '31201M'.         


END PROCEDURE.

PROCEDURE getParamEmailErroDesign:

    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('EMAILS_ERROS_ARQ_DESIGN').
    RUN getVlParam(OUTPUT vlParam).
    IF vlParam = '' THEN
       ASSIGN vlParam = 'jessica.ribeiro@imatextil.com.br'.
    

END PROCEDURE.

PROCEDURE getListaItensEmailSaldoEstoq:

    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('itens_email_saldo_estoq').
    RUN getVlParam(OUTPUT vlParam).

END PROCEDURE.


FUNCTION getBaseDirIntegrLisaWindows RETURNS CHAR():

    DEFINE VARIABLE cParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('base_dir_integra_lisa_win').
    RUN getVlParam(OUTPUT cParam).
    IF cParam = '' THEN
       ASSIGN cParam = '\\pv1\pv2\integracao_lisa'.

    RETURN cParam.

END FUNCTION.

FUNCTION getBaseDirIntegrLisaLinux RETURNS CHAR():

    DEFINE VARIABLE cParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('base_dir_integra_lisa_linux').
    RUN getVlParam(OUTPUT cParam).
    IF cParam = '' THEN
       ASSIGN cParam = '/mnt/lisa'.

    RETURN cParam.

END FUNCTION.


PROCEDURE getDirRomaneioPDF:

    DEFINE INPUT  PARAMETER pEstab  AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('dir_romaneio_' + pEstab).
    RUN getVlParam(OUTPUT vlParam).
    IF vlParam = '' THEN DO:
       RUN setCodParam('dir_romaneio').
       RUN getVlParam(OUTPUT vlParam).
       IF vlParam = '' THEN 
          ASSIGN vlParam = '\\pv1\pv2\romaneio'.

       ASSIGN vlParam = convDirOs(vlParam,
                                   getBaseDirIntegrLisaWindows(),
                                   getBaseDirIntegrLisaLinux()).
        
    END.

       
     

END PROCEDURE.



PROCEDURE getEstabsIntegraLISA:

    DEFINE OUTPUT PARAMETER cParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('estabs_integra_lisa').
    RUN getVlParam(OUTPUT cParam).
    IF cParam = '' THEN
       ASSIGN cParam = '505'.


END PROCEDURE.


PROCEDURE getDirJsonLisa:

    DEFINE OUTPUT  PARAMETER cVlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('dir_json_lisa').
    RUN getVlParam2('\\pv1\pv2\integracao_lisa',
                    OUTPUT cVlParam ).

    ASSIGN cVlParam = convDirOs(cVlParam,
                                getBaseDirIntegrLisaWindows(),
                                getBaseDirIntegrLisaLinux()).


END PROCEDURE.





PROCEDURE getUrlJsonLisa:
    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.

    RUN setCodParam('url_integracao_lisa_json').
    RUN getVlParam2('http://imaonline.imatextil.com.br/integracao_lisa', 
                        OUTPUT cVlParam).
END PROCEDURE.




PROCEDURE getDirCompletoRetornoLisa:

    DEFINE OUTPUT  PARAMETER cVlParam AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDir AS CHARACTER   NO-UNDO.

    RUN getDirJsonLisa(OUTPUT cDir).
    RUN getDirRetornoLisa(OUTPUT cVlParam).
    ASSIGN cVlParam = cDir + "\" + cVlParam .

END PROCEDURE.

PROCEDURE getDirRetornoLisa:

    DEFINE OUTPUT  PARAMETER cVlParam AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDir AS CHARACTER   NO-UNDO.
    RUN setCodParam('dir_retorno_lisa').
    RUN getVlParam2('retorno', 
                     OUTPUT cVlParam).       

END PROCEDURE.


PROCEDURE getDirCompletoRetornoProcessadoLisa:

    DEFINE OUTPUT  PARAMETER cVlParam AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDir AS CHARACTER   NO-UNDO.

    RUN getDirJsonLisa(OUTPUT cDir).
    RUN getDirRetornoProcessadoLisa(OUTPUT cVlParam).
    ASSIGN cVlParam = cDir + "\" + cVlParam .

END PROCEDURE.

PROCEDURE getDirRetornoProcessadoLisa:

    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDir AS CHARACTER   NO-UNDO.
    RUN getDirRetornoLisa(OUTPUT cDir)  .
    RUN setCodParam('dir_retorno_proc_lisa').
    RUN getVlParam2('processado', 
                        OUTPUT cVlParam).
    ASSIGN cVlParam = cDir + "\" + cVlParam.


END PROCEDURE.

PROCEDURE getEmitPadrSaldoTercLisa:

  DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.

  RUN setCodParam('emitente_terc_lisa').
  RUN getVlParam2('38284',OUTPUT cVlParam).

END PROCEDURE.

PROCEDURE getSeriePadrSaldoTercLisa:

  DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.

  RUN setCodParam('serie_terc_lisa').
  RUN getVlParam2('2',OUTPUT cVlParam).


END PROCEDURE.

PROCEDURE getNatOperPadrSaldoTercLisa:

  DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.

  RUN setCodParam('nat_oper_terc_lisa').
  RUN getVlParam2('59207i',OUTPUT cVlParam).


END PROCEDURE.

PROCEDURE getTipoNFAprovPedLisa:


    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.

    RUN setCodParam('tp_nf_aprov_ped_lisa').
    RUN getVlParam2('triangular',OUTPUT cVlParam).



END PROCEDURE.



PROCEDURE getEmailsErroPendBook:

    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('emails_erro_pend_book').
    RUN getVlParam(OUTPUT vlParam).
    IF vlParam = '' THEN
       ASSIGN vlParam = 'ti@imatextil.com.br'.

END PROCEDURE.



PROCEDURE getEspd004aAut:

    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('espd004a_automatico').
    RUN getVlParam(OUTPUT vlParam).
    IF vlParam = '' THEN
       ASSIGN vlParam = '1'.



END PROCEDURE.


PROCEDURE getDirLogEspd001:

    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.

    IF OPSYS = 'unix' THEN    DO:
       RUN getDirLogEspd001Linux(OUTPUT cVlParam).        
    END.
    ELSE DO:
       RUN getDirLogEspd001Windows(OUTPUT cVlParam).    
    END.



END PROCEDURE.


PROCEDURE getDirLogEspd001Windows:

    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.

    RUN setCodParam('dir_log_espd001_windows').
    RUN getVlParam2('\\192.168.0.137\erp\especificos\logs',OUTPUT cVlParam).



END PROCEDURE.

PROCEDURE getDirLogEspd001Linux:

    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.

    RUN setCodParam('dir_log_espd001_linux').
    RUN getVlParam2('/mnt/datasul/ERP/especificos/logs',OUTPUT cVlParam).



END PROCEDURE.



PROCEDURE getDirErp:

    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('dir_erp').
    RUN getVlParam2('\\192.168.0.136\ERP',OUTPUT cVlParam).


END PROCEDURE.

PROCEDURE getMapeamentoErp:

    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('mapeamento_erp').
    RUN getVlParam2('T',OUTPUT cVlParam).


END PROCEDURE.  



PROCEDURE getListaNaturezaDadosImpNF:

     
    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('nats_operacao_dados_imp_obs').
    RUN getVlParam2('31206,31201M',OUTPUT cVlParam).

END PROCEDURE.


PROCEDURE getDirDanfe:

    DEFINE INPUT  PARAMETER pEstab      AS CHARACTER   NO-UNDO. 
    DEFINE OUTPUT PARAMETER cVlParam    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE nomeParam AS CHARACTER   NO-UNDO.
    ASSIGN nomeParam =  "dir_danfe_" + pEstab .
    RUN setCodParam( nomeParam ).
    RUN getVlParam2('',OUTPUT cVlParam).

END PROCEDURE.

PROCEDURE getTituloEmailNFVenda:
    
    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('titulo_email_envio_nf_venda').
    RUN getVlParam2('EMISSAO DE NF',OUTPUT cVlParam).


END PROCEDURE.

PROCEDURE getEndEmailLogNfVenda:

    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('end_email_log_nf_venda').
    RUN getVlParam2('log@imatextil.com.br',OUTPUT cVlParam).


END PROCEDURE.
