DEFINE TEMP-TABLE ttVendaTriangular
       FIELD valor AS LOG.
DEFINE TEMP-TABLE ttEstabel
      FIELD valor AS CHAR.
DEFINE TEMP-TABLE ttTipoAtividade
      FIELD valor AS CHAR FORMAT 'x(50)'.
DEFINE TEMP-TABLE ttFinalidade
      FIELD valor AS CHAR FORMAT 'x(50)'.
DEFINE TEMP-TABLE ttContrICMS
     FIELD valor AS LOG.   
DEFINE TEMP-TABLE ttContrICMSTriang
     FIELD valor AS LOG.
DEFINE TEMP-TABLE ttCLienteUF
     FIELD valor AS INT.
DEFINE TEMP-TABLE ttCLienteUFTriang
     FIELD valor AS INT.
DEFINE TEMP-TABLE ttSuframa
     FIELD valor AS LOG.
DEFINE TEMP-TABLE ttSuframaTriang
     FIELD valor AS LOG.

DEFINE TEMP-TABLE ttMatriz
    FIELD cod_param_nat_operacao AS INT
    FIELD LOG_venda_triangular  AS CHAR
    FIELD LOG_suframa           AS CHAR
    FIELD ind_ClienteUF         AS CHAR
    FIELD LOG_contribuinte_icms AS CHAR
    FIELD cod_finalidade        AS INT
    FIELD des_finalidade        AS CHAR
    FIELD cod_tipo_atividade    AS INT
    FIELD des_tipo_atividade    AS CHAR
    FIELD cod_estabel           AS CHAR
    FIELD cod_nat_operacao      AS CHAR
    FIELD denominacao           AS CHAR FORMAT 'x(50)'
    FIELD cd-tri-icm            AS CHAR FORMAT 'x(50)'
    FIELD cod-cfop              AS CHAR
    FIELD aliquota-icm          AS DECIMAL
    FIELD perc-red-icm          AS DECIMAL
    FIELD aliq-icm-com          AS DECIMAL
    FIELD perc-pis              AS DECIMAL
    FIELD per-fin-soc           AS DECIMAL
    FIELD per-des-icms          AS DECIMAL
    FIELD merc-base-icms        AS CHAR FORMAT 'x(50)'
    FIELD tipo-trib             AS CHAR FORMAT 'x(50)'
    FIELD dest-red-icms         AS DECIMAL
    FIELD estados               AS CHAR EXTENT 27
    FIELD LOG_contribuinte_icms_triang AS CHAR
    FIELD ind_ClienteUF_triang AS CHAR
    FIELD LOG_suframa_triang   AS CHAR
    FIELD estadosTriang         AS CHAR EXTENT 27.

DEFINE VARIABLE iUF             AS INTEGER     NO-UNDO.
DEFINE VARIABLE iUFTriang       AS INTEGER     NO-UNDO.
DEFINE VARIABLE lVendaTriang    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSuframa        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSuframaTriang  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lContrIcms      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lContrIcmsTriang AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cListaId        AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE LOG_param       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cListaEstados   AS CHARACTER   NO-UNDO FORMAT 'x(300)'
       INIT "AC|AL|AP|AM|BA|CE|DF|ES|GO|MA|MT|MS|MG|PA|PB|PR|PE|PI|RJ|RN|RS|RO|RR|SC|SP|SE|TO". 
DEFINE VARIABLE iCont               AS INTEGER     NO-UNDO.
DEFINE VARIABLE cResultEstado       AS CHARACTER   NO-UNDO FORMAT 'x(300)'.
DEFINE VARIABLE cResultEstadoBranco AS CHARACTER   NO-UNDO FORMAT 'x(300)' INIT "|||||||||||||||||||||||||||".


DEFINE BUFFER bf_param_nat_operacao FOR PARAM_nat_operacao.
/*CREATE ttVendaTriangular.
ASSIGN ttVendaTriangular.valor = YES.*/

CREATE ttVendaTriangular.
ASSIGN ttVendaTriangular.valor = YES.

CREATE ttEstabel.
ASSIGN ttEstabel.valor = '1'.

CREATE ttEstabel.
ASSIGN ttEstabel.valor = '5'.

CREATE ttTipoAtividade.
ASSIGN ttTipoAtividade.valor = '1-varejo'.

CREATE ttTipoAtividade.
ASSIGN ttTipoAtividade.valor = '2-atacado'.

CREATE ttTipoAtividade.
ASSIGN ttTipoAtividade.valor = '3-industria'.

CREATE ttTipoAtividade.
ASSIGN ttTipoAtividade.valor = '4-servico'.



CREATE ttFinalidade.
ASSIGN ttFinalidade.valor = '1-envio para desposito'.

CREATE ttFinalidade.
ASSIGN ttFinalidade.valor = '2-consumo pr¢prio'.

CREATE ttFinalidade.
ASSIGN ttFinalidade.valor = '3-envio para industrializaá∆o'.

CREATE ttFinalidade.
ASSIGN ttFinalidade.valor = '4-revenda de mercadoria'.



CREATE ttContrIcms.
ASSIGN ttContrIcms.valor = YES.

CREATE ttContrIcms.
ASSIGN ttContrIcms.valor = NO.

CREATE ttContrIcmsTriang.
ASSIGN ttContrIcmsTriang.valor = YES.

CREATE ttContrIcmsTriang.
ASSIGN ttContrIcmsTriang.valor = NO.


CREATE ttClienteUF.
ASSIGN ttClienteUF.valor = 1.

CREATE ttClienteUF.
ASSIGN ttClienteUF.valor = 2.


CREATE ttClienteUFTriang.
ASSIGN ttClienteUFTriang.valor = 1.

CREATE ttClienteUFTriang.
ASSIGN ttClienteUFTriang.valor = 2.


CREATE ttSuframa.
ASSIGN ttSuframa.valor = YES.

CREATE ttSuframa.
ASSIGN ttSuframa.valor = NO.

CREATE ttSuframaTriang.
ASSIGN ttSuframaTriang.valor = YES.

CREATE ttSuframaTriang.
ASSIGN ttSuframaTriang.valor = NO.

/*geraá∆o de possibilidades para regras n∆o triangulares*/

FOR EACH ttVendaTriangular:
    FOR EACH ttEstabel:
        FOR EACH ttTipoAtividade:
            FOR EACH ttFinalidade:
                FOR EACH ttContrIcms:
                    FOR EACH ttclienteUF:
                        FOR EACH ttSuframa:
                            FOR EACH ttSuframaTriang:
                                FOR EACH ttClienteUfTriang:
                                    FOR EACH ttContrIcmsTriang:
                                        //IF ttsuframa.valor  = YES AND ttclienteUf.valor = 1  THEN NEXT.
                                        CREATE ttmatriz.
                                        ASSIGN 
                                        ttmatriz.LOG_venda_triangular           = IF ttVendaTriangular.valor THEN 'SIM' ELSE 'N«O'
                                        ttMatriz.LOG_contribuinte_icms          = IF ttContrIcms.valor THEN 'SIM' ELSE 'N«O'
                                        ttMatriz.LOG_contribuinte_icms_triang   = IF ttContrIcmsTriang.valor THEN 'SIM' ELSE 'N«O'
                                        ttmatriz.LOG_suframa                    = IF ttSuframa.valor THEN 'SIM' ELSE 'N«O'
                                        ttmatriz.LOG_suframa_triang             = IF ttSuframaTriang.valor THEN 'SIM' ELSE 'N«O'
                                        ttmatriz.ind_ClienteUF                  = IF ttClienteUF.valor = 1 THEN 'N«O' ELSE 'SIM'
                                        ttmatriz.ind_ClienteUF_triang           = IF ttClienteUFTriang.valor = 1 THEN 'N«O' ELSE 'SIM'
                                        ttmatriz.cod_finalidade                 = int(entry(1,ttFinalidade.valor,"-"))
                                        ttmatriz.des_finalidade                 = entry(2,ttFinalidade.valor,"-")
                                        ttmatriz.cod_tipo_atividade             = int(entry(1,ttTipoAtividade.valor,"-"))    
                                        ttmatriz.des_tipo_atividade             = entry(2,ttTipoAtividade.valor,"-")         
                                        ttmatriz.cod_estabel                    = ttestabel.valor .

                                    END.
                                END.
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.
END.

OUTPUT TO c:\temp\matriz_pos_triang.txt.
PUT  "Cod.Parametro|Venda Triang.?|Suframa?|Fora do Estado?|Contr.ICMS|Cod.Finalidade|Desc.Finalidade|Cod.Tipo Ativ.|Desc.Tipo Atividade|Estab.|Nat.Operaá∆o|Denominaá∆o|Trib.ICMS|CFOP|Aliq.ICMS|% Red.ICMS|Aliq.ICMS Compl.|% Desc.ICMS|PIS|COFINS|BASE ICMS|TIPO TRIB|Red.ICMS|"  cListaEstados FORMAT 'x(82)'  "|Contribuinte ICMS Triang.|Fora Do Estado Triang?|Suframa Triang?|" cListaEstados FORMAT 'x(82)' SKIP.
FOR EACH ttmatriz:
    ASSIGN cListaId = ''.
    /*PUT  "venda trinag.:" ttMatriz.LOG_venda_triangular        SKIP
         "cod.estabel:"   ttMatriz.cod_estabel                 SKIP
         "tipo Ativ."     ttMatriz.cod_tipo_atividade          SKIP
         "finalid.:"      ttMatriz.cod_finalidade              SKIP
         "contr.ICMS:"    ttMatriz.LOG_contribuinte_icms       SKIP
         "ind.cliente:"   ttMatriz.ind_clienteUF               SKIP
         "suframa:"       ttMatriz.LOG_suframa SKIP.
     */

    ASSIGN LOG_param = NO.
    ASSIGN iUF              = int(IF ttMatriz.ind_clienteUF  = 'sim' THEN 2 ELSE 1)
           lSuframa         = IF ttMatriz.LOG_suframa   = 'sim' THEN YES ELSE NO
           lContrIcms       = IF ttMatriz.LOG_contribuinte_icms = 'sim' THEN YES ELSE NO 
           iUFTriang        = int(IF ttMatriz.ind_clienteUF_triang  = 'sim' THEN 2 ELSE 1)
           lSuframaTriang   = IF ttMatriz.LOG_suframa_triang   = 'sim' THEN YES ELSE NO
           lContrIcmsTriang = IF ttMatriz.LOG_contribuinte_icms_triang = 'sim' THEN YES ELSE NO 
           lVendaTriang     = IF ttMatriz.LOG_venda_triangular  = 'sim' THEN YES ELSE NO .

           
    
    FOR EACH 
        PARAM_nat_operacao 
        WHERE PARAM_nat_operacao.log_venda_triangular         = lVendaTriang
        AND   PARAM_nat_operacao.cod_estab                    = ttMatriz.cod_estabel  
        AND   PARAM_nat_operacao.cod_tipo_atividade           = ttMatriz.cod_tipo_atividade   
        AND   PARAM_nat_operacao.cod_finalidade_venda         = ttMatriz.cod_finalidade  
        AND   PARAM_nat_operacao.log_contribuinte_icms        = lContrIcms
        AND   PARAM_nat_operacao.ind_cliente_uf               = iUF
        AND   PARAM_nat_operacao.log_suframa                  = lSuframa
        AND   PARAM_nat_operacao.cod_param_nat_operacao_pai   = 0
        AND   PARAM_nat_operacao.LOG_inativo = FALSE  .
/*         IF PARAM_nat_operacao.cod_param_nat_operacao = 22 THEN */
/*             MESSAGE PARAM_nat_operacao.cod_param_nat_operacao  */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.         */
        REPEAT iCont = 1 TO NUM-ENTRIES(cListaEstados,"|"):
            FIND FIRST ufs_params_nat_operacao 
                WHERE  ufs_params_nat_operacao.cod_param_nat_operacao =  PARAM_nat_operacao.cod_param_nat_operacao
                AND ufs_params_nat_operacao.uf = ENTRY(iCont,cListaEstados,"|")
                NO-LOCK NO-ERROR. 
            IF AVAIL ufs_params_nat_operacao THEN
               ASSIGN ttMatriz.estados[iCont] = 'X'.
        END.
/*         IF PARAM_nat_operacao.cod_param_nat_operacao = 22 THEN */
/*         MESSAGE lSuframaTriang SKIP                            */
/*                 lContrIcmsTriang  SKIP                         */
/*                 iUfTriang   SKIP                               */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */
        FOR EACH bf_param_nat_operacao NO-LOCK
            WHERE bf_param_nat_operacao.cod_param_nat_operacao_pai = PARAM_nat_operacao.cod_param_nat_operacao
            AND   bf_param_nat_operacao.LOG_suframa = lSuframaTriang
            AND   bf_param_nat_operacao.LOG_contribuinte_icms = lContrIcmsTriang
            AND   bf_param_nat_operacao.ind_cliente_uf = iUfTriang .

            REPEAT iCont = 1 TO NUM-ENTRIES(cListaEstados,"|"):
/*                 MESSAGE PARAM_nat_operacao.cod_param_nat_operacao  skip */
/*                     bf_PARAM_nat_operacao.cod_param_nat_operacao        */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.                  */
                FIND FIRST ufs_params_nat_operacao 
                    WHERE  ufs_params_nat_operacao.cod_param_nat_operacao =  bf_PARAM_nat_operacao.cod_param_nat_operacao
                    AND ufs_params_nat_operacao.uf = ENTRY(iCont,cListaEstados,"|")
                    NO-LOCK NO-ERROR. 
                IF AVAIL ufs_params_nat_operacao THEN
                   ASSIGN ttMatriz.estadostriang[iCont] = 'X'.
/*                 MESSAGE PARAM_nat_operacao.cod_param_nat_operacao  skip */
/*                     bf_PARAM_nat_operacao.cod_param_nat_operacao   SKIP */
/*                     ENTRY(iCont,cListaEstados,"|") SKIP  */
/*                     AVAIL ufs_params_nat_operacao        */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.   */


            END.
            ASSIGN LOG_param = YES.
            /*IF cListaId = '' THEN
                ASSIGN cListaId = string(PARAM_nat_operacao.cod_param_nat_operacao).
            ELSE
               ASSIGN cListaId = cListaId + ',' + string(PARAM_nat_operacao.cod_param_nat_operacao).
            */
            FIND FIRST natur-oper
                WHERE natur-oper.nat-operacao = bf_PARAM_nat_operacao.cod_nat_operacao
                NO-LOCK NO-ERROR.
            IF AVAIL natur-oper THEN DO:
               ASSIGN ttMatriz.cod_param_nat_operacao =   bf_PARAM_nat_operacao.cod_param_nat_operacao
                      ttMatriz.cod_nat_operacao = bf_PARAM_nat_operacao.cod_nat_operacao
                      ttMatriz.denominacao      = natur-oper.denominacao
                      ttMatriz.cd-tri-icm       = {ininc/i01in245.i 4 natur-oper.cd-trib-icm }
                      ttMatriz.cod-cfop         = natur-oper.cod-cfop
                      ttMatriz.aliquota-icm     = natur-oper.aliquota-icm
                      ttMatriz.perc-red-icm     = natur-oper.perc-red-icm 
                      ttMatriz.aliq-icm-com     = natur-oper.aliq-icm-com
                      ttMatriz.perc-pis         = natur-oper.perc-PIS[1]  
                      ttMatriz.per-fin-soc      = natur-oper.per-fin-soc[1]
                      ttMatriz.per-des-icms     = natur-oper.per-des-icms  
                      ttMatriz.merc-base-icms   = {ininc/i05in245.i 4 natur-oper.merc-base-icms  }
                      ttMatriz.tipo-trib        = {ininc/i07in245.i 4 natur-oper.tipo-trib }
                      ttMatriz.dest-red-icms    = natur-oper.dest-red-icms  .
                
            END.
            ELSE DO:
                ASSIGN ttMatriz.denominacao = "N∆o Encontrada".
                
            END.
            EXPORT DELIMITER "|" ttMatriz.
        END.
    END.
    
    IF LOG_param = NO THEN
     EXPORT DELIMITER "|" ttMatriz.
   
END.
OUTPUT CLOSE.


/*    
    Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_param_nat_operacao           inte        i
   20 log_venda_triangular             logi
   30 cod_estab                        char        i
   40 cod_tipo_atividade               inte        i
   50 cod_finalidade_venda             inte
   60 log_contribuinte_icms            logi
   70 ind_cliente_uf                   inte
   90 cod_nat_operacao                 char        i
  100 aliquota_icms                    deci-2
  110 log_inativo                      logi        i
  120 cod_origem                       inte
  130 log_suprama                      logi
  140 log_suframa                      logi
  150 cod_param_nat_operacao_pai       inte

*/

