
/****************************************************************************
** Programa : TWIN176 - trigger de Write para a tabela item-doc-est 
** Data     : Outubro 2015
** Objetivo : trigger de Write para a tabela item-doc-est
** Empresa  : IMA 
** Vers∆o   : TOTVS 12.1.5
** Alterado : 
** *****************************************************************************/
DEFINE PARAMETER BUFFER b-item-doc-est-new FOR item-doc-est.
DEFINE PARAMETER BUFFER b-item-doc-est-old FOR item-doc-est.  

{include/i-prgvrs.i twin176 2.06.00.001}

DEFINE VARIABLE hBoConsParam    AS HANDLE      NO-UNDO.
DEFINE VARIABLE cListaEmail     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBoFats99       AS HANDLE      NO-UNDO.



IF AVAIL b-item-doc-est-new THEN DO:
   FIND FIRST ITEM OF b-item-doc-est-new NO-LOCK NO-ERROR.
   IF AVAIL ITEM THEN DO:
      IF ITEM.tipo-contr  = 2 /*controle total*/ 
          AND b-item-doc-est-new.ct-codigo <> '19000017' THEN
         ASSIGN b-item-doc-est-new.conta-contabil = ''
                b-item-doc-est-new.ct-codigo      = ''
                b-item-doc-est-new.sc-codigo      = ''.

   END.
   IF NEW b-item-doc-est-new THEN DO:
      RUN esbo/boConsParam.p PERSISTENT SET hBoConsParam. 
      RUN getParamEmailErroDesign IN hboConsParam(OUTPUT cListaEmail).
      FIND docum-est OF b-item-doc-est-new NO-LOCK NO-ERROR.
      FIND natur-oper OF b-item-doc-est-new NO-LOCK NO-ERROR.
      IF AVAIL natur-oper AND natur-oper.tipo-compra = 3 /* devolucao cliente*/  
         AND b-item-doc-est-new.data-comp <= 12.07.2022 // data fixada a pedido da Jessica
            THEN DO:
         
         FIND referencias_subst 
          WHERE referencias_subst.cod_refer =  b-item-doc-est-new.cod-refer  NO-LOCK NO-ERROR.
          IF AVAIL referencias_subst THEN DO:
             RUN esapi/esapi002.p ( INPUT 'imatextil@imatextil.com.br',        /* e-mail remetente */
                                    INPUT cListaEmail,         /* e-mail destinat†rio */
                                    INPUT " Item:" +  b-item-doc-est-new.it-codigo + " Ref.:" +  b-item-doc-est-new.cod-refer  + " - Devoluá∆o de Cliente - Data/Hora: "
                                    + STRING(NOW,'99/99/9999 hh:mm:ss') ,        /* Assunto */
                                    INPUT 'Cliente:' + STRING(docum-est.cod-emitente)  + ' - Serie:' + docum-est.serie-docto + ' - Docto:' + docum-est.nro-docto   + ' - Ref.Subst.:' +  referencias_subst.cod_refer_subst   + ' - Qte:' + STRING(b-item-doc-est-new.quantidade) ,  /* Mensagem */
                                    INPUT '',  /* Anexo */
                                    INPUT NO) NO-ERROR .   /* Mostra Erros */ 
    
          END.
      END.
   END.                                  
END.
FIND docum-est OF b-item-doc-est-new NO-LOCK NO-ERROR.
FIND natur-oper OF docum-est NO-LOCK NO-ERROR.
IF natur-oper.tipo-compra = 3 THEN DO: //devoluá∆o de cliente

    RUN esbo/boFats99.p PERSIST SET hBoFats99.
    RUN iniciar         IN hBoFats99.
    RUN setData         IN hBoFats99(docum-est.dt-trans).
    RUN setProgOrigem   IN hBoFats99('twin176').
    RUN setTipoRegistro IN hBoFats99('devolucao').
    RUN inserir         IN hBoFats99.
    RUN finalizar       IN hBoFats99.

END.


RETURN 'OK':u.


