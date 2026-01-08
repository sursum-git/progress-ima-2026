/*************************************************************************************************************
**  Empresa.: Datasul Minas Gerais
**  Cliente.: 
**  Programa: DTS0912E.p - Atualiza Documento ERP 
**  Data....: 01/05/2012
**  ACRA....: HARRY PFAFF JUNIOR - @HPJ - DTS Consultoria 19/08/2012
**            ADEQUAÄ«O DO PROGRAMA PARA TRATAR MAIS DE UMA NATUREZA DE OPERAÄ«O E 
**            SPLIT DE ORDENS DE COMPRA E ORDENS DE PRODUÄ«O
**  ACRA....: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 29/05/2013
**            ADEQUAÄ«O PARA O PROGRAMA TRATAR RETORNO DE MERCADORIA REMETIDA
**            PARA INDUSTRIALIZAÄ«O E QUE GERA NOTA FISCAL NO FATURAMENTO.
**            FOI ALTERADA A PROCEDURE piAtualizaAgregado VALIDANDO SE A CFOP
**            GERA NOTA NO FATURAMENTO. SE GERAR A BUSCA DO DOCUMENTO PARA 
**            AMARRAR O AGREGADO SERµ EFETUADA PELO NOVO NÈMERO DA NF GERADA
**            NO FATURAMENTO E N«O PELA NUMERAÄ«O DE NF IGUAL ∑ NF DO ITEM AGREGADO.
**  ACRA 008: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 01/08/2013
**           - AJUSTADA FORMATAÄ«O CAMPO DE NOTA FISCAL DE "9999999" PARA ">>>9999999"
**  ACRA 009: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 21/08/2013
**            AJUSTE REGRA DE GERAÄ«O DE AGREGADO PARA TRATAR O RETORNO DE MAIS DE
**            UM AGREGADO EM UMA MESMA NOTA.
**  ACRA 010: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 28/08/2013
**            AJUSTE SERIE DE NOTA FISCAL PARA RETORNO DE MATêRIA-PRIMA COM
**            NOTA PR‡PRIA
**  ACRA 011: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 29/08/2013
**            AJUSTE PARA TRATAR NOTA FISCAL DE TRANSFER“NCIA
**  ACRA 012: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 30/10/2013
**            AJUSTE PARA GRAVAR O CAMPO tpCTE e modFrete do documento na docum-est.
**           (campo docum-est.char-2 - PACOTE C.03 EM DIANTE)
**           substring(docum-est.char-2,143,1) /* modalidade de frete */ 
**           substring(docum-est.char-2,151,1) /* tipo cte */           
**  ACRA 013: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 18/02/2014
**           - INCLUS«O DO CAMPO REFER“NCIA NA TT-ITEM-DOC-EST
**  ACRA 014: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 13/03/2014
**           - AJUSTE CONTA CONTµBIL PARA ITEM DO DOCUMENTO - ERRO NA L‡GICA
**  ACRA 015: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 15/04/2014
**           - AJUSTE PROCEDURE piCriaItemAgregado - PARA GERAR O ACABADO DO ITEM
**             AGREGADO ê NECESSµRIO QUE A SEQUENCIA DO ITEM SEJA FEITA DE MANEIRA 
**             CORRETA - FOI ALTERADA A FORMA DE MONTAGEM DA SEQUENCIA DO ITEM DA
**             NOTA FISCAL QUE CONTEM O ITEM AGREGADO.
**  ACRA 016: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 14/05/2014
**           - AJUSTE ROTINA PARA RATEIO DE CT-e. PARA A BO PADR«O, PARA CADA NOTA/REGISTRO 
**             DA rat-docum DEVE SER CRIADO UM REGISTRO E EXECUTADA A BO PARA GERAÄ«O.
**  ACRA 017: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 15/05/2014
**           - AJUSTE CONTA CONTµBIL - ALTERADA A L‡GICA DE GRAVAÄ«O DA CONTA CONTµBIL 
**             DO ITEM DA NOTA FISCAL. SE A CONTA CONTµBIL VIER PREENCHIDA, USAR A CONTA
**             PREENCHIDA. 
**  ACRA 018: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 14/07/2014
**           - AJUSTE VALIDAÄ«O DE CONTROLE DE QUALIDADE.
**             ANTES DE ENTRAR COM O ITEM, FAZ VALIDAÄ«O VERIFICANDO SE O MESMO TEM CONTROLE DE
**             QUALIDADE. SE HOUVER, ALTERA O DEP‡SITO PARA CQ E AJUSTA A TABELA DO XML LOADER
**  ACRA 019: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 14/07/2014
**           - AJUSTE VALIDAÄ«O DE CONTROLE DE QUALIDADE.
**             SE N«O EXISTIR RATEIO DO LOTE, ENT«O FAZ A VALIDAÄ«O NA CRIAÄ«O DO ITEM
**           - AJUSTE NA ALTERAÄ«O DE SEQUENCIA DOS DADOS ENTRE A tt-item-doc-est E tt-rat-lote.
**  ACRA 020: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 23/07/2014
**           - AJUSTE VALOR DE OUTRAS DESPESAS - tt-docum-est.valor-outras - ESTAVA SENDO ZERADO
**             DE FORMA INCORRETA.
**  ACRA 021: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 24/07/2014
**           - INCLU÷DA A CHAMADA DA FUNÄ«O calculateDespesaNota DA BOIN090 PARA EFETUAR O CµLCULO
**             DAS DESPESAS DA NOTA FISCAL E GRAVAR NO LUGAR CORRETO.
**           - AJUSTE NA FUNÄ«O DE CµLCULO DE BASE DO IPI DA FUNÄ«O piAtualizaValoresIPI.]
**             DEIXAR O SISTEMA GRAVAR AUTOMATICAMENTE.
**  ACRA 022: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 11/08/2014
**           - AJUSTE NO RATEIO DE CTe. N«O ESTAVA CONSIDERANDO OS IMPOSTOS INFORMADOS NO RATEIO
**
**  ACRA 023: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 14/08/2014
**           - AJUSTE NO FIFO DA ORDEM DE COMPRA. USAR O MêTODO PADR«O, BAIXANDO PARCELAS DA MAIS
**             ANTIGA PARA A MAIS NOVA CASO O USUµRIO ESTEJA MARCADO PARA FAZER FIFO DE COMPRAS
**             NO PROGRAMA re0101.
**  ACRA 024: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 21/08/2014
**           - ALTERAÄ«O DA ROTINA DE CRIAÄ«O DE ITEM x FORNECEDOR (item-fornec)
**
**  ACRA 025: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 21/08/2014
**           - ALTERAÄ«O DA ROTINA DE CRIAÄ«O DE ITEM x FORNECEDOR (item-fornec)
**           - ERRO QUANDO O ITEM EMS Jµ ESTµ ASSOCIADO A OUTRO ITEM DO MESMO FORNECEDOR
**  ACRA 026: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 18/09/2014
**           - AJUSTE FIFO - QUANDO FIZER FIFO (PARAMETRO USUµRIO DO RECEBIMENTO)
**             NO MOMENTO DA CRIAÄ«O DA tt-item-doc-est QUE IRµ GERAR O ITEM DO DOCUMENTO
**             DEVE-SE PREENCHER O CAMPO log-1 COMO YES PARA QUE N«O SEJA VALIDADA A ORDEM DE 
**             COMPRA NA IMPLANTAÄ«O DO ITEM.
**  ACRA 027: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 22/09/2014
**           - ALTERAÄ«O NO CADASTRO DE ITEM FORNECEDOR - RETORNANDO PARA A TABELA item-fornec-estab
**  ACRA 028: HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 01/10/2014
**           - ALTERAÄ«O NO DEP‡SITO QUANDO ê NOTA FISCAL DE DEVOLUÄ«O. QUANDO FOR DEVOLVIDO, USAR
**             O DEP‡SITO INFORMADO NO XML LOADER.
**  ACRA 029  HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 15/10/2014
**           - RETIRADA DA ROTINA PARA FIFO PADR«O DO RECEBIMENTO - NECESSITA MAIS 
**             VALIDAÄÂES.
**  ACRA 030  HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 13/11/2014
**           - AJUSTE PARA A PROCEDURE piAtualizaItemDocEst. COMO ê GRAVADO O DEP‡SITO (VERIFICAÄ«O DE CQ)
**             A BUSCA DA tt-item-doc-est ESTAVA COM no-lock OCASIONANDO ERRO NA GRAVAÄ«O. FOI ALTERADA
**             A BUSCA DA MESMA PARA exclusive-lock.
**  ACRA 031  HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 18/11/2014
**           - AJUSTE NA DEFINIÄ«O DA TEMP-TABLE tt2-item-devol-cli DEFINIDA NA boin176.i4.
**             INCLUIDO NOVO CAMPO nat-of.
**  ACRA 032  HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 28/11/2014
**           - AJUSTE DO PROGRAMA PARA RECEBER O CST DO ITEM QUE FICA ARMAZENADO NO CAMPO 
**             substring(item-doc-est.char-2,502,3) DA TABELA PADR«O E NO CAMPO dt-it-docum-est.cod-cst-item
**             NA TABELA ESPEC÷FICA DO XML LOADER
**  ACRA 033  HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 22/01/2015
**           - AJUSTE NA GRAVAÄ«O DO NCM (CLASS-FISCAL) DO ITEM DO DOCUMENTO. GRAVAR O NCM QUE FOI INFORMADO
**             NO DOCUMENTO (XML) DE ENTRADA E N«O MAIS BUSCAR DO ITEM - tt-item-doc-est.class-fiscal  = tt-dt-it-docum-est.class-fiscal
**  ACRA 034  HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 27/01/2015
**           - AJUSTE NA GRAVAÄ«O DO PESO LIQUIDO DO ITEM DO DOCUMENTO PARA TRATAR RATEIO
**  ACRA 035  HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 12/02/2015 
**           - AJUSTE NA REGRA DE CONTA CONTµBIL. SE item.tipo-con-est  = 3 (CONSIGNADO) ENT«O CONTA DO PROGRAMA 
**            FT0301. (TABELA PARA-FAT, CAMPO CT-CUSCON).
**  ACRA 036  HARRY PFAFF JUNIOR - @HPJ - DTS CONSULTORIA - 27/02/2015 
**           - AJUSTE NA GRAVAÄ«O DE CLASSIFICAÄ«O FISCAL - tt-item-doc-est.class-fiscal - BUSCANDO ∑ PARTIR DA
**            PARAMETRIZAÄ«O DA EMPRESA - USA DO FORNECEDOR OU USA DA BASE.
*****************************************************************************************************************/
{include/i_dbvers.i}
{dtp/dts0101.i}
{inbo/boin367.i tt-rat-lote }
{inbo/boin367.i tt2-rat-lote }
{inbo/boin366.i tt-rat-docum }
{inbo/boin366.i tt2-rat-docum }
{inbo/boin366.i1 tt-imposto}        /* Definiá∆o TT-IMPOSTO */
{inbo/boin366.i1 tt2-imposto}        /* Definiá∆o TT-IMPOSTO */
{inbo/boin176.i4 tt-item-devol-cli}
{inbo/boin176.i4 tt2-item-devol-cli}
{dibo/bodi515.i tt-nota-fisc-adc}
{inbo/boin176.i2}                       /* Definiá∆o TT-TOTAL-ITEM */
{cdp/cd0667.i -conting-nfe}             /*DEFINIÄ«O tt-erro-conting-nfe*/
{include/boerrtab.i} /* Definicao tt-bo-erro             */
/* {inbo/boin223.i2} /* tt-docum-est */ */
{utp/ut-glob.i}
{include/i-prgvrs.i DTS0912E 2.06.00.036} 
PROCEDURE ShellExecuteA EXTERNAL "shell32.dll": 
    DEF INPUT PARAM HWND AS LONG. 
    DEF INPUT PARAM lpOperation AS CHAR. 
    DEF INPUT PARAM lpFile AS CHAR. 
    DEF INPUT PARAM lpParameters AS CHAR. 
    DEF INPUT PARAM lpDirectory AS CHAR. 
    DEF INPUT PARAM nShowcmd AS LONG. 
    DEF RETURN PARAMETER hInstance AS LONG. 
END. 
DEFINE VARIABLE hInstance AS INTEGER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-nat-terc     AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-nota-imp     AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-nat-cte      AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-nat-cte-rat  AS LOGICAL NO-UNDO. /* rateio de frete */
DEFINE NEW GLOBAL SHARED VARIABLE l-nat-devol    AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-nat-nft      AS LOGICAL NO-UNDO. /* transferància */

DEFINE TEMP-TABLE tt-movto-pend NO-UNDO LIKE movto-pend
    field r-rowid as rowid.

DEFINE TEMP-TABLE tt-it-movto-pend NO-UNDO LIKE tt-dt-it-docum-est.

DEFINE INPUT PARAM  TABLE FOR tt-dt-docum-est.
DEFINE INPUT PARAM  TABLE FOR tt-dt-it-docum-est.
/* DEFINE INPUT PARAM  TABLE FOR tt-dt-rat-ordem. */ /* N«O ê MAIS UTILIZADA - ACRA - 18/08/2012 @HPJ*/
/* DEFINE INPUT PARAM  TABLE FOR tt-item-terc. */ /* N«O ê MAIS UTILIZADA - ACRA - 18/08/2012 @HPJ*/
DEFINE INPUT PARAM  TABLE FOR tt-rat-lote.
DEFINE INPUT PARAM  TABLE FOR tt-movto-pend.
DEFINE INPUT PARAM  TABLE FOR tt-it-movto-pend.
DEFINE INPUT PARAM  TABLE FOR tt-rat-docum.
DEFINE INPUT PARAM  TABLE FOR tt-imposto.

DEFINE VARIABLE hXMLGen   AS HANDLE    NO-UNDO.

/* {rep/reapi190a.i} */
DEFINE TEMP-TABLE tt2-movto-pend NO-UNDO LIKE tt-movto-pend .

DEFINE VARIABLE h-acomp   AS HANDLE      NO-UNDO.


FIND FIRST tt-dt-docum-est NO-ERROR.
/* FIND FIRST tt-item-terc NO-ERROR. */

/***************** DEFINIÄ«O VARIµVEIS BO RECEBIMENTO **************/

define temp-table tt-notas-geradas no-undo
    field rw-nota-fiscal as   rowid
    field nr-nota        like docum-est.nro-docto.

define temp-table RowErrors    no-undo
    field ErrorSequence    as integer
    field ErrorNumber      as integer
    field ErrorDescription as character
    field ErrorParameters  as character
    field ErrorType        as character
    field ErrorHelp        as character
    field ErrorSubType     as character
    .
define temp-table ErroRecebimento like RowErrors.

/* def temp-table tt-rat-docum no-undo like rat-docum */
/*     field r-rowid as rowid.                        */
def temp-table tt-item-doc-est no-undo like item-doc-est
    field r-rowid as rowid.
def temp-table tt-dupli-apagar no-undo like dupli-apagar
    field r-rowid as rowid.
def temp-table tt-dupli-imp no-undo like dupli-imp
    field r-rowid as rowid.
def temp-table tt-erro no-undo
    field identif-segment as char
    field cd-erro         as INTEGER FORMAT ">,>>>,>>9"
    field desc-erro       as char format "x(80)".

def temp-table tt2-docum-est no-undo like docum-est
    field r-rowid as rowid.
def temp-table tt2-item-doc-est no-undo like item-doc-est
    field r-rowid as rowid.

def temp-table tt3-item-doc-est no-undo like item-doc-est
    field r-rowid as rowid.

def temp-table tt-docum-est-aux no-undo like docum-est
    field r-rowid as rowid.
/* def temp-table tt-item-doc-est no-undo like item-doc-est */
/*     field r-rowid as rowid.                              */


def temp-table tt-docum-est no-undo like docum-est
    field r-rowid as rowid.

define buffer bDocumEstAgregado   for docum-est.
define buffer bDtDocumEstAgregado for dt-docum-est.
define buffer bDtItDocEstAgregado for dt-it-docum-est.
define buffer bDocumEst           for docum-est. 
define buffer bf-estabelec        for estabelec.
DEFINE BUFFER bNaturOper          FOR natur-oper.
DEFINE BUFFER bDtItDocum          FOR dt-it-docum-est.
define buffer bfparam-re          for param-re.


define variable lErro                    as logical                     no-undo.
define variable hshowmsg                 as handle                      no-undo.
define variable h-boRecebimento          as handle                      no-undo.
define variable h-boin366                as handle                      no-undo.
define variable h-boin092                as handle                      no-undo.
define variable h-boin567                as handle                      no-undo.
define variable h-boin404                as handle                      no-undo.
define variable h-boin223                as handle                      no-undo.
define variable h-boin223a               as handle                      no-undo.
define variable h-bodi515                as handle                      no-undo.
define variable h-bocx378                as handle                      no-undo.
define variable h-boin367                as handle                      no-undo.
define variable h-bocx090                as handle                      no-undo.
define variable h-bocx100                as handle                      no-undo.
define variable h-bocx255                as handle                      no-undo.
define variable h-cdapi024               as handle                      no-undo.
DEFINE VARIABLE h-boin176                AS HANDLE                      NO-UNDO.
DEFINE VARIABLE h-cd4337                 AS HANDLE                      NO-UNDO.
define variable l-erro-create            as logical                     no-undo.
define variable l-terceiros              as logical                     no-undo.
define variable l-rec-fisico             as logical                     no-undo.
define variable c-nro-docto              like docum-est.nro-docto       no-undo.
define variable l-rateia-itens           as logical                     no-undo.
define variable l-embarq-import          as logical                     no-undo.
define variable c_id_reg_bloco_modul_edi as character                   no-undo.
define variable l-validateTotalDuplicata as logical                     no-undo.
define variable rDocumEst                as rowid                       no-undo.
define variable h-boin245                as handle                      no-undo.
define variable h-boad107                as handle                      no-undo.
define variable v-cod-emitente-aux       like docum-est.cod-emitente    no-undo.
define variable v-nat-operacao-aux       like docum-est.nat-operacao    no-undo.
define variable v-serie-docto-aux        like docum-est.serie-docto     no-undo.
define variable v-nro-docto-aux          like docum-est.nro-docto       no-undo.
define variable i-Sequencia              as integer                     no-undo.
define variable l-possui-rat-lote        AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE l-deposito-cq            as logical                     no-undo.
DEFINE VARIABLE l-ncm-fornec             AS LOGICAL                     NO-UNDO.
DEFINE VARIABLE l-bloq-ncm               AS LOGICAL                     NO-UNDO.

/*Integracao Drawback*/
define variable h-boin245na              as handle                      no-undo.
define variable h-boad098                as handle                      no-undo.
define variable h-bocx312                as handle                      no-undo.
define variable h-im9046                 as handle                      no-undo.
define variable h-re1001a1               as handle                      no-undo.
define variable c-return                 as character                   no-undo.
define variable c-nat-op-drb             as character                   no-undo.
define variable l-nat-op-drb             as logical                     no-undo.
define variable i-natureza               as integer                     no-undo.
define variable l-nok                    as logical                     no-undo.
define variable l-fifo                   as logical                     no-undo.
define variable i-tipo-compra            as integer                     no-undo.
define variable l-spp-nfe                as log                         no-undo.
define variable iCodEmitente             as int                         no-undo.
define variable cCodEstabel              as char                        no-undo.
define variable cserie-docto             as char                        no-undo.
define variable cnro-docto               as char                        no-undo.
define variable cnat-operacao            as char                        no-undo.
define variable lAtualizaReceb           as logical                     no-undo.
define variable iQtItensNota             as integer                     no-undo.
define variable deBaseIPIOutras          LIKE item-doc-est.ipi-outras[1] NO-UNDO.
define variable pRowidDtDocEst           as rowid                       no-undo.

/**** VARIµVEIS PARA PIS E COFINS */
def var de-aliquota-pis      like natur-oper.perc-pis[1]        no-undo.
def var de-base-pis          as dec                             no-undo.
def var de-vl-pis            as dec                             no-undo.
def var de-aliquota-cofins   like natur-oper.perc-pis[1]        no-undo.
def var de-base-cofins       as dec                             no-undo.
def var de-vl-cofins         as dec                             no-undo.
def var de-trib-pis          as int                             no-undo.
def var de-trib-cofins       as int                             no-undo.
def var de-valor-un-pis      as dec format ">>>9.99999"         no-undo.
def var de-valor-un-cofins   as dec format ">>>9.99999"         no-undo.
def var de-val-reduc-pis-normal    as decimal format ">>9.99":U no-undo.
def var de-val-reduc-cofins-normal as decimal format ">>9.99":U no-undo.
def var de-vlr-merc-aux      like docum-est.valor-mercad        no-undo.
def var de-vlr-tot-nota-aux  like docum-est.tot-valor           no-undo.
def var lGeraFaturamento     as logical             initial no  no-undo. /* gera nota-fiscal no faturamento */
def var lBenefNFPropria      as logical             initial no  no-undo.
/****************** FIM VARIµVEIS BO RECEBIMENTO ********************/


/* RUN dtp\ut-genXML.p PERSISTENT SET hXMLGen. */
/* RUN loadXMLFromFile IN hXMLGen (nome-arq).  */
assign l-spp-nfe = can-find(first funcao where 
                                  funcao.cd-funcao = "SPP-NFE":U and funcao.ativo = yes).



run utp/ut-acomp.p PERSISTENT SET h-acomp.
run pi-inicializar IN h-acomp (INPUT "Importando").

define variable c-arquivo AS CHAR NO-UNDO.

    
define variable seq AS INT NO-UNDO. 
define variable l-retorno AS LOGICAL NO-UNDO.   
define variable i AS INTEGER NO-UNDO.

FORM  /* tt-dt-it-docum-est.item-ems NO-LABEL  */
      tt-erro.cd-erro NO-LABEL    AT 01
      tt-erro.desc-erro NO-LABEL  AT 17 
      tt-dt-it-docum-est.nome-arq AT 98 NO-LABEL
      WITH STREAM-IO NO-BOX DOWN WIDTH 255 NO-ATTR-SPACE FRAME f-relat.  
                         
FORM  "Codigo"          AT 1 
      "Status"          AT 17
      "Nome do Arquivo" AT 98 SKIP
      "--------------"  AT 1  
      "-----------"     AT 17
      "-------------"   AT 98
      WITH STREAM-IO NO-BOX DOWN WIDTH 255 NO-ATTR-SPACE FRAME f-cab.    




/* Main block */


/* Carrega informaá‰es */
empty temp-table tt-docum-est.
empty temp-table rowErrors.

for each tt-item-doc-est:
    delete tt-item-doc-est.
end.
for each tt-dupli-apagar:
    delete tt-dupli-apagar.
end.
FOR EACH tt-dupli-imp:
    DELETE tt-dupli-imp.
END.
FOR EACH tt-erro:
    DELETE tt-erro.
END.
empty temp-table tt-docum-est-aux.
empty temp-table tt-docum-est.

ASSIGN lAtualizaReceb = NO.

FIND FIRST tt-dt-docum-est NO-LOCK NO-ERROR.
FIND FIRST param-global    NO-LOCK NO-ERROR.
FIND FIRST param-cq        NO-LOCK NO-ERROR.
FIND FIRST dt-empresa 
    WHERE dt-empresa.cod-estabel = tt-dt-docum-est.cod-estabel NO-LOCK NO-ERROR.
IF NOT AVAIL dt-empresa
THEN FIND FIRST dt-empresa NO-LOCK NO-ERROR.
ASSIGN lAtualizaReceb = dt-empresa.l-atualiza-receb
       l-ncm-fornec   = dt-empresa.log-ncm-fornec
       l-bloq-ncm     = dt-empresa.log-bloqueio-ncm.

FIND FIRST natur-oper
    WHERE natur-oper.nat-operacao = tt-dt-docum-est.nat-operacao NO-LOCK NO-ERROR.
FIND FIRST emitente
    WHERE emitente.cod-emitente = tt-dt-docum-est.cod-emitente NO-LOCK NO-ERROR.
FIND FIRST estabelec
    WHERE estabelec.cod-estabel = tt-dt-docum-est.cod-estabel NO-LOCK NO-ERROR.
FIND FIRST bf-estabelec NO-LOCK
     WHERE bf-estabelec.cod-emitente = tt-dt-docum-est.cod-emitente NO-ERROR.

assign lGeraFaturamento = natur-oper.imp-nota.
/* se gera nota fiscal no faturamento, alterar a sÇrie do documento */
if lGeraFaturamento then do:
    for each tt-dt-docum-est exclusive-lock:
        FOR EACH tt-dt-it-docum-est NO-LOCK
            WHERE tt-dt-it-docum-est.serie-docto  = tt-dt-docum-est.serie-docto    
              AND tt-dt-it-docum-est.nro-docto    = tt-dt-docum-est.nro-docto      
              AND tt-dt-it-docum-est.cod-emitente = tt-dt-docum-est.cod-emitente
              AND tt-dt-it-docum-est.nat-operacao = tt-dt-docum-est.nat-operacao: 
            FOR EACH tt-rat-lote
                WHERE tt-rat-lote.cod-emitente = tt-dt-it-docum-est.cod-emitente
                 and  tt-rat-lote.serie-docto  = tt-dt-it-docum-est.serie-docto 
                 and  tt-rat-lote.nro-docto    = tt-dt-it-docum-est.nro-docto   
                 and  tt-rat-lote.nat-operacao = tt-dt-it-docum-est.nat-operacao
                 AND  tt-rat-lote.it-codigo    = tt-dt-it-docum-est.item-ems
                 /* and  tt-rat-lote.sequencia    = tt-dt-it-docum-est.sequencia   */ EXCLUSIVE-LOCK:
                ASSIGN tt-rat-lote.serie-docto  = estabelec.serie.
            END.

            assign tt-dt-it-docum-est.serie-docto = estabelec.serie.
        end.
        assign tt-dt-docum-est.serie-docto = estabelec.serie.
    end.
end.
find first tt-dt-it-docum-est
    where tt-dt-it-docum-est.log-2 = yes no-lock no-error.
if avail tt-dt-it-docum-est then
    assign lBenefNFPropria = yes.
else
    assign lBenefNFPropria = no.
CREATE tt-docum-est.
ASSIGN tt-docum-est.serie-docto    = tt-dt-docum-est.serie-docto 
       tt-docum-est.nro-docto      = TRIM(STRING(INT(tt-dt-docum-est.nro-docto),">>>9999999"))
       tt-docum-est.nat-operacao   = tt-dt-docum-est.nat-operacao
       tt-docum-est.cod-emitente   = tt-dt-docum-est.cod-emitente
       tt-docum-est.cod-estabel    = tt-dt-docum-est.cod-estabel 
       tt-docum-est.estab-fisc     = tt-dt-docum-est.cod-estabel
       tt-docum-est.tot-peso       = 0 /* tt-dt-docum-est.peso-liq */ /* ser† deixado como 0 por problemas de atualizaá∆o */
       tt-docum-est.peso-bruto-tot = 0 /* tt-dt-docum-est.peso-bru      */ /* ser† deixado como 0 por problemas de atualizaá∆o */
       tt-docum-est.tot-desconto   = tt-dt-docum-est.tot-desconto
       tt-docum-est.valor-frete    = tt-dt-docum-est.valor-frete 
       tt-docum-est.valor-seguro   = tt-dt-docum-est.valor-seguro
       tt-docum-est.valor-outras   = tt-dt-docum-est.valor-outras
       tt-docum-est.valor-mercad   = tt-dt-docum-est.valor-mercad 
       tt-docum-est.tot-valor      = tt-dt-docum-est.tot-valor 
       tt-docum-est.mod-frete      = tt-dt-docum-est.mod-frete
       
       /* tt-docum-est.registro       = 1 */
       tt-docum-est.cod-observa    = tt-dt-docum-est.cod-observa
       tt-docum-est.uf             = tt-dt-docum-est.uf
       tt-docum-est.via-transp     = 1
       /* tt-docum-est.esp-docto      = 21 */
       tt-docum-est.dt-emissao     = tt-dt-docum-est.dt-emissao
       tt-docum-est.observacao     = tt-dt-docum-est.narrativa /* + "Arquivo carregado do xml: " + tt-dt-docum-est.nome-arq. */.
ASSIGN tt-docum-est.esp-docto      = if   natur-oper.transf 
                                     then 23  /* NFT */
                                     else if   emitente.identific = 1        /* cliente */
                                          or  (emitente.identific = 3 
                                              and not natur-oper.emite-dup
                                              and not natur-oper.transf 
                                              and not natur-oper.terceiros
                                              and (substr(natur-oper.nat-operacao,2,1) = "2" 
                                              or substr(natur-oper.nat-operacao,2,1) = "3" 
                                              or substr(natur-oper.nat-operacao,2,1) = "7" 
                                              or substr(natur-oper.nat-operacao,2,1) = "9" )) 
                                         then 20 /* NFD */
                                         else 21 /* NFE */    .
IF tt-docum-est.esp-docto = 23 THEN
    ASSIGN tt-docum-est.estab-de-or = bf-estabelec.cod-estabel WHEN AVAIL bf-estabelec.

ASSIGN tt-docum-est.base-icm    = tt-dt-docum-est.base-icm   
       tt-docum-est.base-ipi    = IF tt-dt-docum-est.ipi-deb-cre > 0 THEN tt-dt-docum-est.base-ipi   ELSE 0
       tt-docum-est.base-iss    = IF tt-dt-docum-est.iss-deb-cre > 0 THEN tt-dt-docum-est.base-iss   ELSE 0
       tt-docum-est.base-subs   = tt-dt-docum-est.base-subs  
       tt-docum-est.icm-deb-cre = tt-dt-docum-est.icm-deb-cre
       tt-docum-est.ipi-deb-cre = tt-dt-docum-est.ipi-deb-cre
       tt-docum-est.despesa-nota = 0
       /* tt-docum-est.valor-outras = 0 */
       overlay(tt-docum-est.char-2,143,1) = substring(tt-dt-docum-est.char-2,143,1)
       overlay(tt-docum-est.char-2,151,1) = substring(tt-dt-docum-est.char-2,151,1).
       /* tt-docum-est.vl-subs     = tt-dt-docum-est.vl-subs    . */
IF CAN-FIND(FIRST tt-rat-docum) then do:
    ASSIGN tt-docum-est.base-icm     = 0
           tt-docum-est.base-ipi     = 0
           tt-docum-est.icm-deb-cre  = 0
           tt-docum-est.ipi-deb-cre  = 0.
end.
IF CAN-FIND (FIRST tt-movto-pend NO-LOCK) THEN DO:
    ASSIGN tt-docum-est.base-icm     = 0
           tt-docum-est.base-ipi     = 0
           tt-docum-est.base-iss     = 0
           tt-docum-est.base-subs    = 0
           tt-docum-est.icm-deb-cre  = 0
           tt-docum-est.ipi-deb-cre  = 0
           tt-docum-est.iss-deb-cre  = 0
           tt-docum-est.tot-valor    = 0
           tt-docum-est.valor-mercad = 0
           tt-docum-est.vl-subs      = 0.


    FOR EACH tt-dt-it-docum-est NO-LOCK
        WHERE tt-dt-it-docum-est.serie-docto  = tt-dt-docum-est.serie-docto    
          AND tt-dt-it-docum-est.nro-docto    = tt-dt-docum-est.nro-docto      
          AND tt-dt-it-docum-est.cod-emitente = tt-dt-docum-est.cod-emitente
          AND tt-dt-it-docum-est.nat-operacao = tt-dt-docum-est.nat-operacao: 
        ASSIGN tt-docum-est.base-icm     = tt-docum-est.base-icm     + tt-dt-it-docum-est.base-icm  /* tt-docum-est.base-icm     - tt-it-movto-pend.base-icm */
               tt-docum-est.base-ipi     = tt-docum-est.base-ipi     + tt-dt-it-docum-est.base-ipi  /* tt-docum-est.base-ipi     - tt-it-movto-pend.base-ipi     */
               tt-docum-est.base-iss     = tt-docum-est.base-iss     + tt-dt-it-docum-est.base-iss  /* tt-docum-est.base-iss     - tt-it-movto-pend.base-iss       */
               tt-docum-est.base-subs    = tt-docum-est.base-subs    + tt-dt-it-docum-est.base-subs /* tt-docum-est.base-subs    - tt-it-movto-pend.base-subs      */
               tt-docum-est.icm-deb-cre  = tt-docum-est.icm-deb-cre  + tt-dt-it-docum-est.valor-icm /* tt-docum-est.icm-deb-cre  - tt-it-movto-pend.valor-icm      */
               tt-docum-est.ipi-deb-cre  = tt-docum-est.ipi-deb-cre  + tt-dt-it-docum-est.valor-ipi /* tt-docum-est.ipi-deb-cre  - tt-it-movto-pend.valor-ipi      */
               tt-docum-est.iss-deb-cre  = tt-docum-est.iss-deb-cre  + tt-dt-it-docum-est.valor-iss /* tt-docum-est.iss-deb-cre  - tt-it-movto-pend.valor-iss      */
               tt-docum-est.tot-valor    = tt-docum-est.tot-valor    + tt-dt-it-docum-est.preco-total /* tt-docum-est.tot-valor    - tt-it-movto-pend.preco-total  */
               tt-docum-est.valor-mercad = tt-docum-est.valor-mercad + tt-dt-it-docum-est.preco-total /* (tt-dt-it-docum-est.preco-unit * tt-dt-it-docum-est.quantidade) */ /* tt-docum-est.valor-mercad - (tt-it-movto-pend.preco-unit * tt-it-movto-pend.quantidade). */
               tt-docum-est.vl-subs      = tt-docum-est.vl-subs      + tt-dt-it-docum-est.valor-subs.
    END.
/*     FOR EACH tt-it-movto-pend NO-LOCK:                                                                                              */
/*         ASSIGN tt-docum-est.base-icm     = tt-docum-est.base-icm     - tt-it-movto-pend.base-icm                                    */
/*                tt-docum-est.base-ipi     = tt-docum-est.base-ipi     - tt-it-movto-pend.base-ipi                                    */
/*                tt-docum-est.base-iss     = tt-docum-est.base-iss     - tt-it-movto-pend.base-iss                                    */
/*                tt-docum-est.base-subs    = tt-docum-est.base-subs    - tt-it-movto-pend.base-subs                                   */
/*                tt-docum-est.icm-deb-cre  = tt-docum-est.icm-deb-cre  - tt-it-movto-pend.valor-icm                                   */
/*                tt-docum-est.ipi-deb-cre  = tt-docum-est.ipi-deb-cre  - tt-it-movto-pend.valor-ipi                                   */
/*                tt-docum-est.iss-deb-cre  = tt-docum-est.iss-deb-cre  - tt-it-movto-pend.valor-iss                                   */
/*                tt-docum-est.tot-valor    = tt-docum-est.tot-valor    - tt-it-movto-pend.preco-total                                 */
/*                tt-docum-est.valor-mercad = tt-docum-est.valor-mercad - (tt-it-movto-pend.preco-unit * tt-it-movto-pend.quantidade). */
/*                /* tt-docum-est.vl-subs      = tt-docum-est.vl-subs      - tt-it-movto-pend.valor-subs. */                           */
/*                                                                                                                                     */
/*     END.                                                                                                                            */
END.
FOR FIRST estab-mat
    WHERE estab-mat.cod-estabel = tt-docum-est.cod-estabel NO-LOCK:
    FOR FIRST natur-oper
        WHERE natur-oper.nat-operacao = tt-docum-est.nat-operacao NO-LOCK:
        IF  natur-oper.terceiros AND natur-oper.tp-oper-terc <> 4 /* No considera Faturamento Consignao */
        THEN DO:
                IF  natur-oper.tp-oper-terc     = 1 THEN ASSIGN tt-docum-est.conta-transit  = estab-mat.conta-ent-benef.       /* Entrada Benef */
                ELSE IF natur-oper.tp-oper-terc = 2 THEN ASSIGN tt-docum-est.conta-transit  = estab-mat.conta-sai-benef.       /* Retorno Benef */
                ELSE IF natur-oper.tp-oper-terc = 5 THEN ASSIGN tt-docum-est.conta-transit  = estab-mat.conta-sai-consig.      /* Devol Consig */
                ELSE                                     ASSIGN tt-docum-est.conta-transit  = estab-mat.conta-ent-consig.      /* Compra Consig */
        END.
        ELSE DO:
            IF natur-oper.nota-rateio THEN /* Conhecimento de Transporte */
                ASSIGN tt-docum-est.conta-transit = estab-mat.conta-frete.
            ELSE DO:
                IF natur-oper.tipo-compra = 3 THEN /* Devoluá∆o de Venda */
                    ASSIGN tt-docum-est.conta-transit = estab-mat.conta-dev-cli.
                ELSE
                    ASSIGN tt-docum-est.conta-transit = estab-mat.conta-fornec.        
            END.
        END.
    END.
END.

ASSIGN /* tt-docum-est.sequencia      = 1  */
       tt-docum-est.rec-fisico     = NO
       tt-docum-est.origem         = "I"
       tt-docum-est.pais-origem    = "RE1001"
       tt-docum-est.dt-trans       = tt-dt-docum-est.dt-trans
       tt-docum-est.usuario        = c-seg-usuario .
       
ASSIGN tt-docum-est.dt-venc-ipi    = TODAY /*tt-dt-docum-est.dt-emissao*/
       tt-docum-est.dt-venc-icm    = TODAY /*tt-dt-docum-est.dt-emissao*/.
/* if  l-spp-nfe then do: */
     assign tt-docum-est.cod-chave-aces-nf-eletro = tt-dt-docum-est.chave-xml
            tt-docum-est.cdn-sit-nfe              = 3 no-error.
     /* foi colocado aqui pois no T11 est† mostrando no RE1001 e n∆o no RE0701 */
     assign overlay(tt-docum-est.char-1,93,60) = tt-dt-docum-est.chave-xml
            overlay(tt-docum-est.char-1,153,1) = "3" no-error.

/* end. */
if valid-handle(h-boRecebimento) then do:
    if not valid-handle(h-boRecebimento) then do:
       run inbo/boin090.p persistent set h-boRecebimento.
    end.
    run calculateDespesaNota in h-boRecebimento(input tt-docum-est.valor-frete,
                                                input tt-docum-est.valor-seguro,
                                                input tt-docum-est.valor-embal,
                                                input tt-docum-est.valor-outras,
                                                output tt-docum-est.despesa-nota ).

    IF  VALID-HANDLE(h-boRecebimento) THEN DO:
        RUN destroy IN h-boRecebimento.
        ASSIGN h-boRecebimento = ?.
    END.


end.
ASSIGN seq = 0
       iQtItensNota = 0. 
FOR EACH tt-dt-it-docum-est NO-LOCK
    WHERE tt-dt-it-docum-est.serie-docto  = tt-dt-docum-est.serie-docto    
      AND tt-dt-it-docum-est.nro-docto    = tt-dt-docum-est.nro-docto      
      AND tt-dt-it-docum-est.cod-emitente = tt-dt-docum-est.cod-emitente
      AND tt-dt-it-docum-est.nat-operacao = tt-dt-docum-est.nat-operacao:
    ASSIGN iQtItensNota = iQtItensNota + 1.
END.
FOR EACH tt-dt-it-docum-est NO-LOCK
    WHERE tt-dt-it-docum-est.serie-docto  = tt-dt-docum-est.serie-docto    
      AND tt-dt-it-docum-est.nro-docto    = tt-dt-docum-est.nro-docto      
      AND tt-dt-it-docum-est.cod-emitente = tt-dt-docum-est.cod-emitente
      AND tt-dt-it-docum-est.nat-operacao = tt-dt-docum-est.nat-operacao: 
     ASSIGN seq = seq + 10. 
        
    RUN pi-acompanhar IN h-acomp (INPUT "Documento: " + tt-dt-docum-est.nro-docto).        
    FIND FIRST ITEM
        WHERE ITEM.it-codigo = tt-dt-it-docum-est.item-ems NO-LOCK NO-ERROR.
    CREATE tt-item-doc-est.
    ASSIGN tt-item-doc-est.serie-docto   = tt-dt-docum-est.serie-docto
           tt-item-doc-est.nro-docto     = TRIM(STRING(INT(tt-dt-docum-est.nro-docto),">>>9999999"))
           tt-item-doc-est.nat-operacao  = tt-dt-it-docum-est.nat-operacao
           tt-item-doc-est.cod-emitente  = tt-dt-it-docum-est.cod-emitente
           tt-item-doc-est.it-codigo     = tt-dt-it-docum-est.item-ems
           tt-item-doc-est.etiquetas     = tt-dt-it-docum-est.etiquetas   
           tt-item-doc-est.qt-do-forn    = tt-dt-it-docum-est.qt-do-forn
           tt-item-doc-est.quantidade    = tt-dt-it-docum-est.dec-1  /* isso esta correto ??? */             
           tt-item-doc-est.preco-total[1] = tt-dt-it-docum-est.preco-total 
           tt-item-doc-est.preco-unit[1] = tt-dt-it-docum-est.preco-unit
           tt-item-doc-est.desconto      = tt-dt-it-docum-est.desconto    
           tt-item-doc-est.vl-subs[1]    = tt-dt-it-docum-est.valor-subs
           tt-item-doc-est.narrativa     = tt-dt-it-docum-est.narrativa
           tt-item-doc-est.sequencia     = seq
           
           tt-item-doc-est.dt-vali-lote  = tt-dt-it-docum-est.dt-vali-lote
           tt-item-doc-est.numero-ordem  = tt-dt-it-docum-est.numero-ordem
           tt-item-doc-est.nr-ord-prod   = tt-dt-it-docum-est.nr-ord-produ
           tt-item-doc-est.aliquota-icm  = tt-dt-it-docum-est.aliquota-icm /* tirei */
           tt-item-doc-est.valor-icm     = tt-dt-it-docum-est.valor-icm
           tt-item-doc-est.aliquota-ipi  = tt-dt-it-docum-est.aliquota-ipi
           tt-item-doc-est.valor-ipi     = tt-dt-it-docum-est.valor-ipi /* tirei */ 
           tt-item-doc-est.base-icm[1]   = tt-dt-it-docum-est.base-icm   
           tt-item-doc-est.base-ipi[1]   = tt-dt-it-docum-est.base-ipi   
           tt-item-doc-est.base-pis      = tt-dt-it-docum-est.base-pis
           tt-item-doc-est.base-subs[1]  = tt-dt-it-docum-est.base-subs  
           tt-item-doc-est.serie-comp    = tt-dt-it-docum-est.serie-comp /* IF AVAIL tt-item-terc THEN tt-item-terc.serie     ELSE "" */
           tt-item-doc-est.nro-comp      = tt-dt-it-docum-est.nro-comp   /* IF AVAIL tt-item-terc THEN tt-item-terc.nro-docto ELSE "" */
           tt-item-doc-est.nat-comp      = tt-dt-it-docum-est.nat-comp   /* IF AVAIL tt-item-terc THEN tt-item-terc.nat-oper  ELSE "" */
           tt-item-doc-est.seq-comp      = tt-dt-it-docum-est.seq-comp
           overlay(tt-item-doc-est.char-2,502,3) = tt-dt-it-docum-est.cod-cst-item   /* CST DO ITEM - 28/11/2014 */
           tt-item-doc-est.peso-liquido  = if avail item then item.peso-liquido *  tt-item-doc-est.quantidade else 0   
           .
    IF l-ncm-fornec THEN DO:
        ASSIGN tt-item-doc-est.class-fiscal  = tt-dt-it-docum-est.class-fiscal.
    END.
    ELSE DO:
        IF AVAIL ITEM THEN
            ASSIGN tt-item-doc-est.class-fiscal  = ITEM.class-fiscal.
        ELSE
            ASSIGN tt-item-doc-est.class-fiscal  = "".

    END.
        


           /* tt-item-doc-est.cod-refer     = tt-dt-it-docum-est.cod-refer    . */
           /* tt-item-doc-est.lote          = tt-dt-it-docum-est.lote . */
    if item.tipo-con-est = 4 then  
        assign tt-item-doc-est.cod-refer    = tt-dt-it-docum-est.cod-refer
               tt-item-doc-est.lote         = tt-dt-it-docum-est.lote
               tt-item-doc-est.dt-vali-lote = tt-dt-it-docum-est.dt-vali-lote.
    else if item.tipo-con-est <> 1 then 
        assign tt-item-doc-est.lote         = tt-dt-it-docum-est.lote
               tt-item-doc-est.dt-vali-lote = tt-dt-it-docum-est.dt-vali-lote.
    ELSE ASSIGN tt-item-doc-est.dt-vali-lote  = tt-dt-it-docum-est.dt-vali-lote.

    ASSIGN tt-docum-est.vl-subs          = tt-docum-est.vl-subs + tt-dt-it-docum-est.valor-subs.
    FOR EACH tt-rat-lote
        WHERE tt-rat-lote.cod-emitente = tt-dt-it-docum-est.cod-emitente
         and  tt-rat-lote.serie-docto  = tt-dt-it-docum-est.serie-docto 
         and  tt-rat-lote.nro-docto    = tt-dt-it-docum-est.nro-docto   
         and  tt-rat-lote.nat-operacao = tt-dt-it-docum-est.nat-operacao
         AND  tt-rat-lote.it-codigo    = tt-dt-it-docum-est.item-ems
         and  tt-rat-lote.sequencia    = tt-dt-it-docum-est.sequencia   EXCLUSIVE-LOCK:
        ASSIGN tt-rat-lote.sequencia = tt-item-doc-est.sequencia.
    END.
    FIND ITEM WHERE ITEM.it-codigo = tt-item-doc-est.it-codigo NO-LOCK NO-ERROR.
    IF natur-oper.tipo-compra = 3 THEN DO:
        FIND FIRST it-nota-fisc 
             WHERE it-nota-fisc.serie        = tt-item-doc-est.serie-comp
               and it-nota-fisc.nr-nota-fis  = tt-item-doc-est.nro-comp  
               and it-nota-fisc.nat-operacao = tt-item-doc-est.nat-comp  
               and it-nota-fisc.nr-seq-fat   = tt-item-doc-est.seq-comp NO-LOCK NO-ERROR.
        IF AVAIL it-nota-fisc THEN DO:
            ASSIGN tt-item-doc-est.nr-pedcli      = it-nota-fisc.nr-pedcli   
                   tt-item-doc-est.data-comp      = it-nota-fisc.dt-emis-nota
                   tt-item-doc-est.baixa-ce       = it-nota-fisc.baixa-estoq 
                   tt-item-doc-est.nr-pd-seq      = it-nota-fisc.nr-seq-fat.

        END.
    END.
    /***** COMENTADO EM 15/10/2014 - VALIDAR OS AJUSTES ***********/
/*     FIND FIRST bfparam-re where bfparam-re.usuario = c-seg-usuario NO-LOCK NO-ERROR.              */
/*     if avail bfparam-re and bfparam-re.log-1 then do:                                             */
/*         if NOT(l-nat-terc)                  and                                                   */
/*            NOT(l-nat-devol)                 AND                                                   */
/*            NOT(l-nat-nft)                   AND                                                   */
/*            NOT(l-nat-cte-rat)               and                                                   */
/*            tt-item-doc-est.numero-ordem = 0  THEN DO:                                             */
/*             find first bNaturOper                                                                 */
/*                 where bNaturOper.nat-operacao = tt-dt-it-docum-est.nat-operacao no-lock no-error. */
/*              if bNaturOper.tp-oper-terc = 3 or                                                    */
/*                 not bNaturOper.terceiros then do:                                                 */
/*                  assign tt-item-doc-est.log-1         = yes                                       */
/*                         tt-item-doc-est.log-fifo-oc   = YES /* campo TOTVS 11 */.                 */
/*              end.                                                                                 */
/*         end.                                                                                      */
/*     end.                                                                                          */
    IF tt-item-doc-est.nr-ord-prod <> 0 THEN DO:
        FIND FIRST ord-prod
            WHERE ord-prod.nr-ord-produ = tt-item-doc-est.nr-ord-prod NO-LOCK NO-ERROR.
        IF AVAIL ord-prod THEN
            ASSIGN tt-item-doc-est.conta-contabil = ord-prod.conta-ordem.
    END.
    ELSE DO:
        IF ITEM.tipo-con-est = 3 THEN DO: /* CONSIGNADO - ACRA 035 */ 
            FIND FIRST para-fat NO-LOCK NO-ERROR.
            IF AVAIL para-fat THEN
                ASSIGN tt-item-doc-est.conta-contabil = para-fat.ct-cuscon.
        END.
        ELSE DO:
            if tt-dt-it-docum-est.conta-contabil = "" then do:
                IF AVAIL ITEM and 
                        (item.tipo-con-est = 1 OR item.tipo-con-est = 4) THEN 
                    ASSIGN tt-item-doc-est.conta-contabil = ITEM.conta-aplicacao.
            end.
            else 
                ASSIGN tt-item-doc-est.conta-contabil = tt-dt-it-docum-est.conta-contabil.
        END.
            
    END.
    FIND FIRST bfparam-re where bfparam-re.usuario = c-seg-usuario NO-LOCK NO-ERROR.
    if avail bfparam-re and not(bfparam-re.log-1) then do:

        FIND FIRST prazo-compra
           where prazo-compra.numero-ordem = tt-item-doc-est.numero-ordem
             and prazo-compra.situacao     =  2                 /* CONFIRMADA */
             and ( prazo-compra.quant-saldo - prazo-compra.dec-1 ) >= tt-item-doc-est.quantidade  no-lock no-error.
        IF AVAIL prazo-compra 
        THEN ASSIGN tt-item-doc-est.parcela = prazo-compra.parcela.
        FIND FIRST ordem-compra
            WHERE ordem-compra.numero-ordem = tt-item-doc-est.numero-ordem NO-LOCK NO-ERROR.
        IF AVAIL ordem-compra THEN DO:
            ASSIGN tt-item-doc-est.num-pedido = ordem-compra.num-pedido.
            IF tt-item-doc-est.conta-contabil = "" THEN DO:
                ASSIGN tt-item-doc-est.conta-contabil = ordem-compra.conta-contabil.
            END.
            ELSE DO:
                IF tt-item-doc-est.nr-ord-prod = 0 THEN 
                    tt-item-doc-est.conta-contabil = ordem-compra.conta-contabil.
    
            END.
    
        END.
    end.
        

    FIND FIRST natur-oper
        WHERE natur-oper.nat-operacao = tt-dt-docum-est.nat-operaca NO-LOCK NO-ERROR.
    IF AVAIL natur-oper  THEN DO:
        ASSIGN tt-item-doc-est.cd-trib-icm  = natur-oper.cd-trib-icm
               tt-item-doc-est.cd-trib-iss  =  natur-oper.cd-trib-iss
               tt-item-doc-est.cd-trib-ipi  =  natur-oper.cd-trib-ipi
               tt-item-doc-est.aliquota-icm = natur-oper.aliquota-icm.
/*         MESSAGE "PIS: "    INT(substring(natur-oper.char-1,86,1)) SKIP                                 */
/*                 "COFINS: " INT(substring(natur-oper.char-1,87,1))                                      */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                         */
/*         &if '{&bf_mat_versao_ems}' >= '2.06' &then                                                     */
/*             assign tt-item-doc-est.idi-tributac-pis     = INT(substring(natur-oper.char-1,86,1))       */
/*                    tt-item-doc-est.idi-tributac-cofins  = INT(substring(natur-oper.char-1,87,1)).      */
/*         &else                                                                                          */
/*             ASSIGN OVERLAY(tt-item-doc-est.char-2,21,1)   = string(substring(natur-oper.char-1,86,1))  */
/*                    OVERLAY(tt-item-doc-est.char-2,83,1)   = string(substring(natur-oper.char-1,87,1)). */
/*         &endif                                                                                         */
    END.
    /* CALCULANDO PIS/COFINS */

    if  not valid-handle(h-cd4337) then 
        run cdp/cd4337.p persistent set h-cd4337.
    RUN pi-tributa-aliquota-pis IN h-cd4337 ( BUFFER estabelec,
                                              BUFFER item,
                                              BUFFER emitente,
                                              BUFFER natur-oper,
                                              INPUT  tt-dt-docum-est.dt-emissao,
                                              OUTPUT de-trib-pis,
                                              OUTPUT de-aliquota-pis,
                                              OUTPUT de-valor-un-pis,
                                              OUTPUT de-val-reduc-pis-normal ).

        assign tt-item-doc-est.idi-tributac-pis = de-trib-pis
               tt-item-doc-est.val-aliq-pis     = de-aliquota-pis.
/*     &if '{&bf_dis_versao_ems}' >= '2.06' &then                                                 */
/*         assign tt-item-doc-est.idi-tributac-pis = de-trib-pis                                  */
/*                tt-item-doc-est.val-aliq-pis     = de-aliquota-pis.                             */
/*     &else                                                                                      */
/*         assign overlay(tt-item-doc-est.char-2,21,1) = string(de-trib-pis)                      */
/*                overlay(tt-item-doc-est.char-2,22,5) = string(dec(de-aliquota-pis), ">9.99":U). */
/*     &endif                                                                                     */
    
    /* Busca aliquota e tributacao de COFINS */
    RUN pi-tributa-aliquota-cofins IN h-cd4337 ( BUFFER estabelec,
                                                 BUFFER item,
                                                 BUFFER emitente,
                                                 BUFFER natur-oper,
                                                 INPUT  tt-dt-docum-est.dt-emissao,
                                                 OUTPUT de-trib-cofins,
                                                 OUTPUT de-aliquota-cofins,
                                                 OUTPUT de-valor-un-cofins,
                                                 OUTPUT de-val-reduc-cofins-normal ).
                                                 
    
        assign tt-item-doc-est.idi-tributac-cofins = de-trib-cofins
               tt-item-doc-est.val-aliq-cofins     = de-aliquota-cofins.
/*     &if '{&bf_dis_versao_ems}' >= '2.06' &then                                                    */
/*         assign tt-item-doc-est.idi-tributac-cofins = de-trib-cofins                               */
/*                tt-item-doc-est.val-aliq-cofins     = de-aliquota-cofins.                          */
/*     &else                                                                                         */
/*         assign overlay(tt-item-doc-est.char-2,83,1) = string(de-trib-cofins)                      */
/*                overlay(tt-item-doc-est.char-2,84,5) = string(dec(de-aliquota-cofins), ">9.99":U). */
/*     &endif                                                                                        */
    if valid-handle(h-cd4337) then do:
        delete procedure h-cd4337.
        assign h-cd4337 = ?.
    end.
    IF tt-item-doc-est.cd-trib-ipi = 3 
    THEN ASSIGN tt-item-doc-est.ipi-outras[1] = IF tt-item-doc-est.base-ipi[1] > 0 THEN tt-item-doc-est.base-ipi[1] ELSE tt-item-doc-est.preco-total[1]
                deBaseIPIOutras               = deBaseIPIOutras + tt-item-doc-est.ipi-outras[1].

    IF tt-item-doc-est.cd-trib-ipi = 1 
    THEN ASSIGN tt-item-doc-est.base-ipi[1] = tt-item-doc-est.preco-total[1] - tt-item-doc-est.desconto[1].
               
    ASSIGN tt-item-doc-est.cod-depos   = tt-dt-it-docum-est.cod-depos
           tt-item-doc-est.cod-localiz = tt-dt-it-docum-est.cod-localiz.
END.
ASSIGN tt-docum-est.base-ipi = deBaseIPIOutras.


IF CAN-FIND (FIRST docum-est
             WHERE docum-est.nro-docto    = tt-docum-est.nro-docto
               AND docum-est.serie        = tt-docum-est.serie
               AND docum-est.cod-emitente = tt-docum-est.cod-emitente
               AND docum-est.nat-operacao = tt-docum-est.nat-operacao NO-LOCK) THEN DO:
    RUN utp/ut-msgs.p (INPUT "show":U,
                       INPUT  17006,
                       INPUT "Documento j† existe.":U).
    RUN pi-finalizar IN h-acomp.
    RETURN "NOK":U.
END.
EMPTY TEMP-TABLE tt-erro.
cria_docto:
DO TRANSACTION: 
    RUN InitializeDBOs.
    run piGeraDoctoRecebimento.
    IF RETURN-VALUE = "NOK" or
       can-find (first tt-erro no-lock)   then DO:
       UNDO cria_docto.
    END.
    else do:
        FIND FIRST tt-dt-docum-est NO-LOCK NO-ERROR.
        /***** COMENTADO EM 15/10/2014 - VALIDAR OS AJUSTES ***********/
/*         FIND FIRST bfparam-re where bfparam-re.usuario = c-seg-usuario NO-LOCK NO-ERROR.                                                                                                                       */
/*         if avail bfparam-re and bfparam-re.log-1 then do:                                                                                                                                                      */
/*             FOR FIRST docum-est                                                                                                                                                                                */
/*                 WHERE docum-est.serie-docto  = tt-dt-docum-est.serie-docto                                                                                                                                     */
/*                   AND docum-est.nro-docto    = tt-dt-docum-est.nro-docto                                                                                                                                       */
/*                   AND docum-est.cod-emitente = tt-dt-docum-est.cod-emitente                                                                                                                                    */
/*                   AND docum-est.nat-operacao = tt-dt-docum-est.nat-operacao NO-LOCK:                                                                                                                           */
/*                 FOR EACH item-doc-est OF docum-est EXCLUSIVE-LOCK:                                                                                                                                             */
/*                     if NOT(l-nat-terc)                  and                                                                                                                                                    */
/*                        NOT(l-nat-devol)                 AND                                                                                                                                                    */
/*                        NOT(l-nat-nft)                   AND                                                                                                                                                    */
/*                        NOT(l-nat-cte-rat)               and                                                                                                                                                    */
/*                        item-doc-est.numero-ordem = 0 THEN DO:                                                                                                                                                  */
/*                         find first bNaturOper                                                                                                                                                                  */
/*                             where bNaturOper.nat-operacao = item-doc-est.nat-operacao no-lock no-error.                                                                                                        */
/*                          if bNaturOper.tp-oper-terc = 3 or                                                                                                                                                     */
/*                             not bNaturOper.terceiros then do:                                                                                                                                                  */
/*                              RUN rep\re1001p.p(INPUT ROWID(item-doc-est)).                                                                                                                                     */
/*                              IF NOT CAN-FIND(FIRST rat-ordem                                                                                                                                                   */
/*                                              WHERE rat-ordem.nro-docto    = docum-est.nro-docto                                                                                                                */
/*                                                AND rat-ordem.serie-docto  = docum-est.serie                                                                                                                    */
/*                                                AND rat-ordem.cod-emitente = docum-est.cod-emitente                                                                                                             */
/*                                                AND rat-ordem.nat-operacao = docum-est.nat-operacao                                                                                                             */
/*                                                AND rat-ordem.sequencia    = item-doc-est.sequencia NO-LOCK) THEN DO:                                                                                           */
/*                                  RUN utp/ut-msgs.p (INPUT "show":U,                                                                                                                                            */
/*                                                     INPUT  27100,                                                                                                                                              */
/*                                                     INPUT "N∆o foi encontrada ordem de compra no conceito de FIFO para o item: " + item-doc-est.it-codigo + " Deseja continuar a importaá∆o do documento?":U). */
/*                                                                                                                                                                                                                */
/*                                  IF LOGICAL(return-value) = NO THEN DO:                                                                                                                                        */
/*                                       UNDO cria_docto.                                                                                                                                                         */
/*                                  END.                                                                                                                                                                          */
/*                                  ELSE ASSIGN item-doc-est.numero-ordem = 0                                                                                                                                     */
/*                                              item-doc-est.num-pedido = 0.                                                                                                                                      */
/*                              END.                                                                                                                                                                              */
/*                          end.                                                                                                                                                                                  */
/*                      END.                                                                                                                                                                                      */
/*                 END.                                                                                                                                                                                           */
/*             END.                                                                                                                                                                                               */
/*         END.                                                                                                                                                                                                   */
        FOR FIRST  docum-est
             WHERE docum-est.serie-docto  = tt-dt-docum-est.serie-docto 
               AND docum-est.nro-docto    = tt-dt-docum-est.nro-docto   
               AND docum-est.cod-emitente = tt-dt-docum-est.cod-emitente
               AND docum-est.cod-estabel  = tt-dt-docum-est.cod-estabel 
               AND docum-est.nat-operacao = tt-dt-docum-est.nat-operacao EXCLUSIVE-LOCK:
            if avail docum-est then
            assign docum-est.idi-sit-nf-eletro        = 3
                   docum-est.cod-chave-aces-nf-eletro = string(tt-dt-docum-est.chave-xml).
        END.
        RELEASE docum-est.
        /*** TRATAMENTO CTE -> rateio  ******/
        IF CAN-FIND(FIRST tt-rat-docum) THEN DO:
            FOR EACH tt-rat-docum NO-LOCK:
                empty temp-table tt2-rat-docum.
                empty temp-table tt2-imposto.
                CREATE tt2-rat-docum.
                BUFFER-COPY tt-rat-docum TO tt2-rat-docum.
/*                 FOR EACH tt-imposto:                       */
/*                     CREATE tt2-imposto.                    */
/*                     BUFFER-COPY tt-imposto TO tt2-imposto. */
/*                 END.                                       */
/*                 EMPTY TEMP-TABLE tt-imposto.               */
                if not valid-handle(h-boin366) then do:
                    RUN inbo/boin366.p PERSISTENT SET h-boin366.
                    RUN openQueryStatic IN h-boin366 ( "Main":U ).
                end.
                run emptyRowObject in h-boin366.
                run openQueryStatic in h-boin366(input "Main":U).
                RUN setConstraintOfDocumEst IN h-boin366(INPUT tt-dt-docum-est.cod-emitente,
                                                         INPUT tt-dt-docum-est.serie-docto,
                                                         INPUT tt-dt-docum-est.nro-docto,
                                                         INPUT tt-dt-docum-est.nat-operacao).

                run setRecord in h-boin366 ( input table tt2-rat-docum ).

                run createRecord in h-boin366.
                /* RUN assignValueMercad IN h-boRecebimento (INPUT FRAME fpage0 de-valor-mercad ).*/
/*                 run emptyRowErrors in h-boin366.                        */
/*                 /* CALCULA VALORES DE RATEIO */                         */
/*                 RUN setTTImposto IN h-boin366 (INPUT TABLE tt-imposto). */
/*                                                                         */
/*                 run createRateio in h-boin366.                          */
/*                                                                         */
/*                 RUN piGeraErros (INPUT h-boin366).                      */
/*                                                                         */
/*                 if l-erro-create = yes THEN DO:                         */
/*                     UNDO cria_docto.                                    */
/*                 END.                                                    */
            END. /* for each tt-rat-docum */
            run emptyRowErrors in h-boin366.
            /* CALCULA VALORES DE RATEIO */
            RUN setTTImposto IN h-boin366 (INPUT TABLE tt-imposto).

            run createRateio in h-boin366.

            RUN piGeraErros (INPUT h-boin366).

            if l-erro-create = yes THEN DO:
                UNDO cria_docto.
            END.
        END. /* IF CAN-FIND(FIRST tt-rat-docum) */
    END.
    
END.
RUN DestroyDBOs.
ASSIGN c-arquivo = dt-empresa.pasta-erros + "\" + "xml-" + STRING(tt-dt-docum-est.cod-emitente) + "-" + tt-dt-docum-est.nro-docto + ".txt".
/*assign c-arquivo = "C:\DATASUL\EMS" + "\" + "xml-" + string(tt-docum-est.cod-emitente) + "-" + tt-docum-est.nro-docto + ".txt".*/
IF CAN-FIND (FIRST tt-erro NO-LOCK) THEN DO:
    OUTPUT TO VALUE(c-arquivo) CONVERT TARGET "ISO8859-1".

    DISP  "------------------------------------  RELATORIO VALIDACAO CARGA XML --------------------------------------". 

    DISP ""
        WITH FRAME f-cab.
        DOWN WITH FRAME f-cab.

    FOR EACH tt-dt-docum-est :                           
         FOR FIRST tt-erro:

            DISP tt-erro.cd-erro
                 tt-erro.desc-erro FORMAT "x(60)"   
                 tt-dt-docum-est.nome-arq @ tt-dt-it-docum-est.nome-arq FORMAT "x(60)"              
                 WITH FRAME f-relat.
                       DOWN WITH FRAME f-relat.
         END. 
    END.
    OUTPUT CLOSE.

    RUN ShellExecuteA(0, "open", "notepad",c-arquivo, "", 1, OUTPUT hInstance).
END.
IF NOT CAN-FIND (FIRST tt-erro NO-LOCK)    THEN DO:
    FIND FIRST tt-docum-est NO-LOCK NO-ERROR. 
    FOR EACH tt-dt-it-docum-est
        WHERE tt-dt-it-docum-est.serie-docto  = tt-dt-docum-est.serie-docto    
          AND tt-dt-it-docum-est.nro-docto    = tt-dt-docum-est.nro-docto      
          AND tt-dt-it-docum-est.cod-emitente = tt-dt-docum-est.cod-emitente
          AND tt-dt-it-docum-est.nat-operacao = tt-dt-docum-est.nat-operacao:
        
        FIND FIRST item-fornec-estab
             WHERE item-fornec-estab.item-do-forn = tt-dt-it-docum-est.it-codigo
               AND item-fornec-estab.cod-emitente = tt-dt-it-docum-est.cod-emitente
               AND item-fornec-estab.cod-estabel  = tt-docum-est.cod-estabel EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL item-fornec-estab THEN DO:
            RUN utp/ut-msgs.p (INPUT "show":U,
                               INPUT  27100,
                               INPUT "Deseja criar relacionamento Item X Fornecedor X Estabelecimento no EMS?":U).

            ASSIGN l-retorno = LOGICAL(RETURN-VALUE).
            LEAVE.
        END.
    END.

    IF l-retorno = YES THEN DO:
        FOR EACH tt-dt-it-docum-est
            WHERE tt-dt-it-docum-est.serie-docto  = tt-dt-docum-est.serie-docto    
              AND tt-dt-it-docum-est.nro-docto    = tt-dt-docum-est.nro-docto      
              AND tt-dt-it-docum-est.cod-emitente = tt-dt-docum-est.cod-emitente
              AND tt-dt-it-docum-est.nat-operacao = tt-dt-docum-est.nat-operacao:
            
            FIND FIRST item-fornec-estab EXCLUSIVE-LOCK
                 WHERE item-fornec-estab.item-do-forn = tt-dt-it-docum-est.it-codigo
                   AND item-fornec-estab.cod-emitente = tt-dt-it-docum-est.cod-emitente
                   AND item-fornec-estab.cod-estabel  = tt-docum-est.cod-estabel NO-ERROR.
            IF NOT AVAIL item-fornec-estab THEN DO:

                FIND FIRST item-fornec-estab
                     WHERE item-fornec-estab.it-codigo = tt-dt-it-docum-est.item-ems
                     AND   item-fornec-estab.cod-emitente = tt-dt-it-docum-est.cod-emitente
                     AND   item-fornec-estab.cod-estabel  = tt-docum-est.cod-estabel
                     NO-LOCK NO-ERROR.
                IF NOT AVAIL item-fornec-estab THEN DO:
                    CREATE item-fornec-estab.
                    ASSIGN item-fornec-estab.it-codigo    = tt-dt-it-docum-est.item-ems
                           item-fornec-estab.cod-emitente = tt-dt-it-docum-est.cod-emitente
                           item-fornec-estab.item-do-forn = tt-dt-it-docum-est.it-codigo
                           item-fornec-estab.cod-estabel  = tt-docum-est.cod-estabel
                           item-fornec-estab.lote-mul-for = 0.
                END.
            END.
        END.
    END.
/* ALTERADO PELA ACRA 027 - RETORNANDO ∑ L‡GICA ANTIGA */
/*     FIND FIRST tt-docum-est NO-LOCK NO-ERROR.                                                                            */
/*     FOR EACH tt-dt-it-docum-est                                                                                          */
/*         WHERE tt-dt-it-docum-est.serie-docto  = tt-dt-docum-est.serie-docto                                              */
/*           AND tt-dt-it-docum-est.nro-docto    = tt-dt-docum-est.nro-docto                                                */
/*           AND tt-dt-it-docum-est.cod-emitente = tt-dt-docum-est.cod-emitente                                             */
/*           AND tt-dt-it-docum-est.nat-operacao = tt-dt-docum-est.nat-operacao:                                            */
/*                                                                                                                          */
/*         FIND FIRST item-fornec                                                                                           */
/*              WHERE item-fornec.cod-emitente = tt-dt-it-docum-est.cod-emitente                                            */
/*                AND item-fornec.item-do-forn = tt-dt-it-docum-est.it-codigo                                               */
/*                /* AND item-fornec-estab.cod-estabel  = tt-docum-est.cod-estabel */ use-index it-forn no-lock NO-ERROR.   */
/*         IF NOT AVAIL item-fornec THEN DO:                                                                                */
/*             find first item-fornec                                                                                       */
/*                 where item-fornec.it-codigo    = tt-dt-it-docum-est.item-ems                                             */
/*                   and item-fornec.cod-emitente = tt-dt-it-docum-est.cod-emitente use-index onde-compra no-lock no-error. */
/*             if avail item-fornec then do:                                                                                */
/*                                                                                                                          */
/*             end.                                                                                                         */
/*             else do:                                                                                                     */
/*                 RUN utp/ut-msgs.p (INPUT "show":U,                                                                       */
/*                                    INPUT  27100,                                                                         */
/*                                    INPUT "Deseja criar relacionamento Item X Fornecedor no EMS?":U).                     */
/*                                                                                                                          */
/*                 ASSIGN l-retorno = LOGICAL(RETURN-VALUE).                                                                */
/*                 LEAVE.                                                                                                   */
/*             end.                                                                                                         */
/*         END.                                                                                                             */
/*     END.                                                                                                                 */
/*                                                                                                                          */
/*     IF l-retorno = YES THEN DO:                                                                                          */
/*         FOR EACH tt-dt-it-docum-est                                                                                      */
/*             WHERE tt-dt-it-docum-est.serie-docto  = tt-dt-docum-est.serie-docto                                          */
/*               AND tt-dt-it-docum-est.nro-docto    = tt-dt-docum-est.nro-docto                                            */
/*               AND tt-dt-it-docum-est.cod-emitente = tt-dt-docum-est.cod-emitente                                         */
/*               AND tt-dt-it-docum-est.nat-operacao = tt-dt-docum-est.nat-operacao:                                        */
/*             /** ACRA 024 **/                                                                                             */
/*             find first item-fornec                                                                                       */
/*                 where item-fornec.item-do-forn = tt-dt-it-docum-est.it-codigo                                            */
/*                   AND item-fornec.cod-emitente = tt-dt-it-docum-est.cod-emitente exclusive-lock no-error.                */
/*             if not avail item-fornec then do:                                                                            */
/*                 find first emitente where emitente.cod-emitente = tt-dt-it-docum-est.cod-emitente no-lock no-error.      */
/*                 create item-fornec.                                                                                      */
/*                 assign item-fornec.it-codigo    = tt-dt-it-docum-est.item-ems                                            */
/*                        item-fornec.cod-emitente = tt-dt-it-docum-est.cod-emitente                                        */
/*                        item-fornec.item-do-forn = tt-dt-it-docum-est.it-codigo                                           */
/*                        item-fornec.unid-med-for = substring(tt-dt-it-docum-est.un,1,2)                                   */
/*                        item-fornec.fator-conver = 1                                                                      */
/*                        item-fornec.num-casa-dec = 0                                                                      */
/*                        item-fornec.ativo        = yes                                                                    */
/*                        item-fornec.cod-cond-pag = emitente.cod-cond-pag                                                  */
/*                        item-fornec.classe-repro = 1.                                                                     */
/*                                                                                                                          */
/*             end.                                                                                                         */
/*             /** FIM ACRA 024 **/                                                                                         */
/*                                                                                                                          */
/* /*             FIND FIRST item-fornec-estab EXCLUSIVE-LOCK                                     */                        */
/* /*                  WHERE item-fornec-estab.item-do-forn = tt-dt-it-docum-est.it-codigo        */                        */
/* /*                    AND item-fornec-estab.cod-emitente = tt-dt-it-docum-est.cod-emitente     */                        */
/* /*                    AND item-fornec-estab.cod-estabel  = tt-docum-est.cod-estabel NO-ERROR.  */                        */
/* /*             IF NOT AVAIL item-fornec-estab THEN DO:                                         */                        */
/* /*                                                                                             */                        */
/* /*                 FIND FIRST item-fornec-estab                                                */                        */
/* /*                      WHERE item-fornec-estab.it-codigo = tt-dt-it-docum-est.item-ems        */                        */
/* /*                      AND   item-fornec-estab.cod-emitente = tt-dt-it-docum-est.cod-emitente */                        */
/* /*                      AND   item-fornec-estab.cod-estabel  = tt-docum-est.cod-estabel        */                        */
/* /*                      NO-LOCK NO-ERROR.                                                      */                        */
/* /*                 IF NOT AVAIL item-fornec-estab THEN DO:                                     */                        */
/* /*                     CREATE item-fornec-estab.                                               */                        */
/* /*                     ASSIGN item-fornec-estab.it-codigo    = tt-dt-it-docum-est.item-ems     */                        */
/* /*                            item-fornec-estab.cod-emitente = tt-dt-it-docum-est.cod-emitente */                        */
/* /*                            item-fornec-estab.item-do-forn = tt-dt-it-docum-est.it-codigo    */                        */
/* /*                            item-fornec-estab.cod-estabel  = tt-docum-est.cod-estabel        */                        */
/* /*                            item-fornec-estab.lote-mul-for = 0.                              */                        */
/* /*                 END.                                                                        */                        */
/* /*             END.                                                                            */                        */
/*         END.                                                                                                             */
/*     END.                                                                                                                 */
    FIND FIRST docum-est
        WHERE docum-est.serie-docto  = tt-dt-docum-est.serie-docto  
          AND docum-est.cod-emitente = tt-dt-docum-est.cod-emitente
          AND docum-est.nro-docto    = tt-dt-docum-est.nro-docto
          AND docum-est.nat-operacao = tt-dt-docum-est.nat-operacao NO-LOCK NO-ERROR.

    IF AVAIL docum-est THEN DO:
        FIND FIRST dt-docum-est
            WHERE dt-docum-est.nome-arq = tt-dt-docum-est.nome-arq EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL dt-docum-est THEN DO:
            FIND FIRST dt-empresa 
                WHERE dt-empresa.cod-estabel = dt-docum-est.cod-estabel NO-LOCK NO-ERROR.
             IF NOT AVAIL dt-empresa THEN FIND FIRST dt-empresa NO-LOCK NO-ERROR.
             IF CAN-FIND (FIRST tt-erro NO-LOCK)    THEN DO:
                 RUN utp/ut-msgs.p (INPUT "show",
                               INPUT 19085,
                               INPUT "Documento importado com sucesso. Arquivo de acompanhamento gerado no diretorio: " + dt-empresa.pasta-erros).
             END.
             ELSE DO:
                 RUN utp/ut-msgs.p (INPUT "show",
                               INPUT 19085,
                               INPUT "Documento importado com sucesso. ").

             END.

            ASSIGN dt-docum-est.log-situacao = YES
/*                    dt-docum-est.nat-operacao = docum-est.nat-operacao */
                   dt-docum-est.dt-trans     = docum-est.dt-trans
                   dt-docum-est.dt-rec       = docum-est.dt-trans.
/*             FOR EACH dt-it-docum-est                                                          */
/*                WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto                    */
/*                  AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto                  */
/*                  AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente EXCLUSIVE-LOCK: */
/*                 ASSIGN dt-it-docum-est.nat-operacao = docum-est.nat-operacao.                 */
/*             END.                                                                              */
            IF NOT VALID-HANDLE(h-boin176) THEN
                RUN inbo/boin176.p PERSISTENT SET h-boin176.
/*             RUN openQueryStatic IN h-boin176 ("Main":U). */

            IF VALID-HANDLE(h-boin176) THEN DO:
                FIND FIRST tt-docum-est NO-LOCK NO-ERROR.
                FIND FIRST natur-oper
                    WHERE natur-oper.nat-operacao = tt-docum-est.nat-operacao NO-LOCK NO-ERROR.
                IF AVAIL tt-docum-est THEN DO:
                    RUN TransferTotalItensNota IN h-boin176 ( INPUT tt-docum-est.cod-emitente, 
                                                              INPUT tt-docum-est.serie-docto,
                                                              INPUT tt-docum-est.nro-docto,     
                                                              INPUT tt-docum-est.nat-operacao ). 
                END.
            END.
            IF  VALID-HANDLE(h-boin176) THEN DO: 
                RUN destroy in h-boin176.
                ASSIGN h-boin176 = ?.
            END.
        END.
    END.   
END.
ELSE DO:
    FIND FIRST dt-empresa 
        WHERE dt-empresa.cod-estabel = dt-docum-est.cod-estabel NO-LOCK NO-ERROR.
    IF NOT AVAIL dt-empresa THEN FIND FIRST dt-empresa NO-LOCK NO-ERROR.
    RUN utp/ut-msgs.p (INPUT "show",
                       INPUT 19085,
                       INPUT "Documento nao importado. Arquivo de acompanhamento gerado no diretorio: " + dt-empresa.pasta-erros).
    RUN pi-finalizar IN h-acomp.
    RETURN "NOK".
END.

RUN pi-finalizar IN h-acomp.
RETURN "OK".

/********************* Fim do c¢digo pricipal ******************************/

procedure piGeraDoctoRecebimento.
    DEFINE VARIABLE rw-docum-est   AS ROWID   NO-UNDO.
    DEFINE VARIABLE l-autom-retenc AS LOGICAL NO-UNDO. /* Automaá∆o das retená‰es no recebimento */
    
    ASSIGN i-Sequencia = 0.
    

    FOR EACH tt-docum-est BY rowid(tt-docum-est):
        FOR FIRST natur-oper
            FIELDS (nat-operacao tp-oper-terc terceiros nota-rateio tipo-compra emite-duplic)
            WHERE natur-oper.nat-operacao = tt-docum-est.nat-operacao NO-LOCK: 
        END.

        IF AVAIL natur-oper
        AND natur-oper.tipo-compra = 4 THEN /* Material Agregado */
            ASSIGN tt-docum-est.mod-atual = 1.

        /* Documento Importado pelo EDI */
        IF tt-docum-est.origem = "" THEN /*Documentos originados pelo TMS recebem "T", nao podem receber "I"*/
            ASSIGN tt-docum-est.origem = "i". 

        RUN piAtualizaDocumEst.
        /* FOI COLOCADA ESTA VALIDAÄ«O POIS EM ALGUNS CASOS A NATUREZA DE OPERAÄ«O
           ê SETADA PARA GERAR NOTA NO FATURAMENTO E COM ISSO, ALTERA O NÈMERO DA NOTA
           DO DOCUMENTO DE ENTRADA. QUANDO ISSO OCORRE, DEVE-SE ALTERAR OS DADOS DO 
           DOCUMENTO DO XML E DOS ITENS DA NOTA */
        IF tt-docum-est.nro-docto <> tt-dt-docum-est.nro-docto THEN DO:
            FIND FIRST dt-docum-est
                WHERE dt-docum-est.nome-arq = tt-dt-docum-est.nome-arq EXCLUSIVE-LOCK NO-ERROR.
    
            IF AVAIL dt-docum-est THEN DO:
                FOR EACH dt-it-docum-est
                   WHERE dt-it-docum-est.nro-docto    = dt-docum-est.nro-docto
                     AND dt-it-docum-est.serie-docto  = dt-docum-est.serie-docto
                     AND dt-it-docum-est.cod-emitente = dt-docum-est.cod-emitente 
                     AND dt-it-docum-est.nat-operacao = dt-docum-est.nat-operacao EXCLUSIVE-LOCK:
                    ASSIGN dt-it-docum-est.nro-docto    = tt-docum-est.nro-docto
                           dt-it-docum-est.serie-docto  = tt-docum-est.serie-docto.
                END.
                ASSIGN dt-docum-est.Narrativa    = dt-docum-est.Narrativa + CHR(13) + " Alterado o N£mero do Documento de " + dt-docum-est.nro-docto + " para " + tt-docum-est.nro-docto + ". Alteraá∆o autom†tica do EMS pois gera nota no Faturamento.".
                ASSIGN dt-docum-est.nro-docto    = tt-docum-est.nro-docto 
                       dt-docum-est.serie-docto  = tt-docum-est.serie-docto.
                assign pRowidDtDocEst       = rowid(dt-docum-est)
                       de-vlr-merc-aux      = tt-docum-est.valor-mercad   
                       de-vlr-tot-nota-aux  = tt-docum-est.tot-valor      .
                FOR EACH tt-item-doc-est:
                    ASSIGN tt-item-doc-est.nro-docto = tt-docum-est.nro-docto.
                    for each tt-rat-lote
                        WHERE tt-rat-lote.cod-emitente = tt-item-doc-est.cod-emitente
                         and  tt-rat-lote.serie-docto  = tt-item-doc-est.serie-docto 
                         and  tt-rat-lote.nro-docto    = tt-item-doc-est.nro-docto   
                         and  tt-rat-lote.nat-operacao = tt-item-doc-est.nat-operacao
                         and  tt-rat-lote.sequencia    = tt-item-doc-est.sequencia   EXCLUSIVE-LOCK:
                        ASSIGN tt-rat-lote.nro-docto   = tt-docum-est.nro-docto.
                    END.
                END.
            END.

        END.
        IF RETURN-VALUE = "NOK" AND l-erro-create = YES THEN
        DO:
            ASSIGN lErro = YES.

            RETURN "NOK".
        END.
        IF tt-docum-est.esp-docto <> 23 THEN DO:
            for each tt-item-doc-est exclusive-lock:
                
/*                 run inbo/boin176.p persistent set h-boin176. */
                RUN openQueryStatic IN h-boin176 ("Main":U).
                /*-- Deixar a boin176 criar automaticamente a item-doc-est para as notas de devolucao --*/
                IF natur-oper.especie-doc = "NFD" THEN DO.
                    
                   EMPTY TEMP-TABLE tt2-item-devol-cli.
                   /*-- Buca item da NF de saida --*/
                   FIND it-nota-fisc NO-LOCK
                       WHERE it-nota-fisc.cod-estabel = tt-docum-est.cod-estabel
                       AND   it-nota-fisc.serie       = tt-item-doc-est.serie-comp
                       AND   it-nota-fisc.nr-nota-fis = tt-item-doc-est.nro-comp
                       AND   it-nota-fisc.nr-seq-fat  > 0
                       AND   it-nota-fisc.it-codigo   = tt-item-doc-est.it-codigo NO-ERROR.

                   /*-- Gera item de devolucao --*/
                   create tt2-item-devol-cli.
                   assign tt2-item-devol-cli.rw-it-nota-fisc = ROWID(it-nota-fisc)
                          tt2-item-devol-cli.quant-devol     = tt-item-doc-est.quantidade
                          tt2-item-devol-cli.preco-devol     = tt-item-doc-est.preco-unit[1] * tt-item-doc-est.quantidade
                          tt2-item-devol-cli.cod-depos       = tt-item-doc-est.cod-depos /* it-nota-fisc.cod-depos -> alterado para buscar o informado */
                          tt2-item-devol-cli.reabre-pd       = NO
                          tt2-item-devol-cli.vl-desconto     = tt-item-doc-est.desconto[1]
                          tt2-item-devol-cli.nat-of          = tt-item-doc-est.nat-of. /* somente Ö partir da 11.5.12 */

                    run getRowid in h-boRecebimento ( output rw-docum-est ).
                    run repositionRecord in h-boRecebimento ( input rw-docum-est ).
                    RUN linktoDocumEst IN h-boin176 (INPUT h-boRecebimento).
    
                    /*-- Cria item-doc-est com base no item da nota de saida --*/
                    run createItemOfNotaFiscal in h-boin176 ( input h-boRecebimento,
                                                              input table tt2-item-devol-cli ).
        
                    IF  RETURN-VALUE = "NOK":U then do: 
                        RUN piGeraErros (INPUT h-boin176 ).
                        if l-erro-create = yes THEN DO.
                            ASSIGN lErro = YES.
                            return "NOK":U.
                        END.
                    end.   
                    
                END. /*-- Fim - Deixar a boin176 criar automaticamente a item-doc-est para as notas de devolucao --*/
                ELSE DO.

                    RUN setDefaultCalculoImpostos IN h-boin176 (INPUT YES,  /*** cΩdigo de tributaªío ***/
                                                                INPUT YES).  /*** al≠quota             ***/
                    ASSIGN l-possui-rat-lote = NO.

                    RUN piAtualizaItemDocEst.
                    IF RETURN-VALUE = "NOK" AND l-erro-create = YES THEN
                    DO:
    
                        ASSIGN lErro = YES.
    
                        RETURN "NOK".
                    END.
                    /***** ATUALIZANDO RAT-LOTE ******/
                    IF CAN-FIND(FIRST tt-rat-lote
                                WHERE tt-rat-lote.cod-emitente = tt-item-doc-est.cod-emitente
                                  AND tt-rat-lote.serie-docto  = tt-item-doc-est.serie-docto 
                                  AND tt-rat-lote.nro-docto    = tt-item-doc-est.nro-docto   
                                  AND tt-rat-lote.nat-operacao = tt-item-doc-est.nat-operacao
                                  AND tt-rat-lote.sequencia    = tt-item-doc-est.sequencia   NO-LOCK) THEN DO:
                        FOR EACH rat-lote
                            WHERE rat-lote.cod-emitente = tt-item-doc-est.cod-emitente
                              AND rat-lote.serie-docto  = tt-item-doc-est.serie-docto 
                              AND rat-lote.nro-docto    = tt-item-doc-est.nro-docto   
                              AND rat-lote.nat-operacao = tt-item-doc-est.nat-operacao
                              AND rat-lote.sequencia    = tt-item-doc-est.sequencia   EXCLUSIVE-LOCK:
                            DELETE rat-lote.
                        END.
                        for each tt-rat-lote 
                           WHERE tt-rat-lote.cod-emitente = tt-item-doc-est.cod-emitente
                             AND tt-rat-lote.serie-docto  = tt-item-doc-est.serie-docto 
                             AND tt-rat-lote.nro-docto    = tt-item-doc-est.nro-docto   
                             AND tt-rat-lote.nat-operacao = tt-item-doc-est.nat-operacao
                             AND tt-rat-lote.sequencia    = tt-item-doc-est.sequencia
                            BY tt-rat-lote.int-2 DESCENDING:
                            FIND FIRST ITEM
                                WHERE ITEM.it-codigo = tt-rat-lote.it-codigo NO-LOCK NO-ERROR.
    /*                         MESSAGE  "item: "  tt-rat-lote.it-codigo SKIP        */
    /*                                  "quantidade : " tt-rat-lote.quantidade SKIP */
    /*                                  "lote: " tt-rat-lote.lote                   */
    /*                             VIEW-AS ALERT-BOX INFO BUTTONS OK.               */
    
                            /***** VERIFICANDO SE O ITEM POSSUI CONTROLE DE QUALIDADE */
                            IF NOT(l-possui-rat-lote) THEN DO:
                                FOR FIRST bNaturOper FIELDS(tipo terceiros tp-oper-terc transf) 
                                    WHERE bNaturOper.nat-operacao = tt-item-doc-est.nat-operacao NO-LOCK:
                                END.
                                FIND FIRST item-uni-estab 
                                    WHERE item-uni-estab.cod-estabel = tt-docum-est.cod-estabel
                                      AND item-uni-estab.it-codigo   = tt-rat-lote.it-codigo USE-INDEX estab NO-LOCK NO-ERROR.
                                if  bNaturOper.tipo = 1
                                    and not bNaturOper.terceiros 
                                    or  (    bNaturOper.tp-oper-terc <> 1
                                         and bNaturOper.tp-oper-terc <> 2 ) then 

                                        if  param-global.modulo-cq          /* Possui modulo CQ */
                                        and param-cq.tipo-cq > 1            /* SΩ Gera Roteiro ou CQ Total */
                                        and not bNaturOper.transf           /* Nao e Transferencia */
                                        and avail item-uni-estab
                                        and item-uni-estab.contr-qualid               /* Item possui CQ */
                                        and item.it-codigo <> " " then do:  /* Item deve ser codificado */ 

                                            run findParamCQ   in h-boin367.
                                            run findItem      in h-boin367 ( input tt-rat-lote.it-codigo ).
                                            run findItemUniEstab in h-boin367 ( input tt-rat-lote.it-codigo,
                                                                                input tt-docum-est.cod-estabel ).
                                            run findDocumento in h-boin367 ( input tt-rat-lote.cod-emitente,
                                                                             input tt-rat-lote.serie-docto,
                                                                             input tt-rat-lote.nro-docto,
                                                                             input tt-rat-lote.nat-operacao ).
                                            run getDepositoCQ in h-boin367 ( output l-deposito-cq ).
                                        end.   
                                assign tt-rat-lote.cod-depos = if  estabelec.deposito-cq <> tt-rat-lote.cod-depos 
                                                               and l-deposito-cq
                                                               then estabelec.deposito-cq
                                                               else tt-rat-lote.cod-depos.
                                FIND FIRST bDtItDocum
                                    WHERE bDtItDocum.serie-docto   =    tt-rat-lote.serie-docto  
                                      AND bDtItDocum.nro-docto     =    tt-rat-lote.nro-docto  
                                      AND bDtItDocum.cod-emitente  =    tt-rat-lote.cod-emitente 
                                      AND bDtItDocum.nat-operacao  =    tt-rat-lote.nat-operacao 
                                      AND bDtItDocum.sequencia     =    tt-rat-lote.sequencia   SHARE-LOCK NO-ERROR.
                                IF AVAIL bDtItDocum AND bDtItDocum.cod-depos <> tt-rat-lote.cod-depos THEN
                                    ASSIGN bDtItDocum.cod-depos = tt-rat-lote.cod-depos.
                            END.
                            ELSE DO:
                                IF tt-rat-lote.cod-depos <> tt-item-doc-est.cod-depos THEN
                                    ASSIGN tt-rat-lote.cod-depos = tt-item-doc-est.cod-depos.
                            END.
                            for each tt2-rat-lote:
                                delete tt2-rat-lote.
                            end.
                            create tt2-rat-lote.
                            buffer-copy tt-rat-lote to tt2-rat-lote no-error.
                            EMPTY TEMP-TABLE RowErrors.
                            run emptyRowErrors in h-boin367.
    
                            run emptyRowObject in h-boin367.
                            run openQueryStatic in h-boin367(input "Main":U).
                            run setRecord in h-boin367( input table tt2-rat-lote ).
                            run createRecord in h-boin367.
                            if  return-value <> "OK":U then do:
                                RUN piGeraErros (INPUT h-boin367 ).
                                ASSIGN lErro = YES
                                       l-erro-create = YES.
                                RETURN "NOK".
                            end.
                        end.  /* for each tt-rat-lote */
                    END. /* IF CAN-FIND(FIRST tt-rat-lote */
                end. /*else natur-oper.especi-doc = 'NFD' */
                
            end. /* for each tt-item-doc-est */
            /**** PRECISO VERIFICAR POIS ELE ESTµ ATUALIZANDO A RAT-LOTE -> ANTES N«O ESTAVA - SIMULAR COM O AURêLIO */

        END.
        IF natur-oper.emite-duplic THEN DO:
            run getRowid in h-boRecebimento ( output rw-docum-est ).

            /*--- cria duplicata a partir do documento corrente ---*/
            run rep/re9341.p (input rw-docum-est, input no).
/*             ASSIGN l-autom-retenc = CAN-FIND(FIRST funcao WHERE                                            */
/*                                                    funcao.cd-funcao = "spp-aut-impto":U AND                */
/*                                                    funcao.ativo).                                          */
/*             IF l-autom-retenc THEN DO:                                                                     */
/*                 EMPTY TEMP-TABLE tt-dupli-apagar.                                                          */
/*                 FOR FIRST docum-est                                                                        */
/*                     WHERE ROWID(docum-est) = rw-docum-est NO-LOCK:                                         */
/*                     FOR EACH dupli-apagar OF docum-est NO-LOCK:                                            */
/*                         CREATE tt-dupli-apagar.                                                            */
/*                         BUFFER-COPY dupli-apagar TO tt-dupli-apagar.                                       */
/*                     END.                                                                                   */
/*                 END.                                                                                       */
/*                 empty temp-table RowErrors.                                                                */
/*                                                                                                            */
/*                 run rep/re9343.p (input table tt-dupli-apagar, output table RowErrors).                    */
/*                 FOR EACH RowErrors                                                                         */
/*                     WHERE RowErrors.ErrorSubType = "ERROR":U:                                              */
/*                     IF NOT (RowErrors.ErrorType = "INTERNAL":U                                             */
/*                     AND    (RowErrors.ErrorNumber = 8                                                      */
/*                             OR RowErrors.ErrorNumber = 10                                                  */
/*                             OR RowErrors.ErrorNumber = 3)) then do:                                        */
/*                                                                                                            */
/*                         FIND FIRST tt-erro                                                                 */
/*                              WHERE tt-erro.identif-segment  = "Retená∆o"                                   */
/*                                AND tt-erro.cd-erro          = RowErrors.ErrorNumber                        */
/*                                AND tt-erro.desc-erro        = RowErrors.ErrorDescription NO-LOCK NO-ERROR. */
/*                         IF  NOT AVAIL tt-erro THEN DO:                                                     */
/*                             CREATE tt-erro.                                                                */
/*                             ASSIGN tt-erro.identif-segment  = "Retená∆o"                                   */
/*                                    tt-erro.cd-erro          = RowErrors.ErrorNumber                        */
/*                                    tt-erro.desc-erro        = RowErrors.ErrorDescription .                 */
/*                                                                                                            */
/*                         END.                                                                               */
/*                     END.                                                                                   */
/*                 END.                                                                                       */
/*                 ASSIGN lErro = YES                                                                         */
/*                        l-erro-create = YES.                                                                */
/*                 RETURN "NOK".                                                                              */
/*             END. */

        END.
/*         IF VALID-HANDLE(h-boin176) THEN DO:                                     */
/*             run TransferTotalItensNota in h-boin176 ( bDocumEst.cod-emitente,   */
/*                                                       bDocumEst.serie-docto,    */
/*                                                       bDocumEst.nro-docto,      */
/*                                                       bDocumEst.nat-operacao ). */
/*         END.                                                                    */
        IF lAtualizaReceb THEN  DO:
            RUN piAtualizaNota. 
            IF RETURN-VALUE = "NOK" AND l-erro-create = YES THEN
            DO:

                ASSIGN lErro = YES.

                RETURN "NOK".
            END.
        END.

        RUN destroyDBOs.
        /******** VERIFICANDO AGREGADO - ESTE AINDA N«O ESTµ PRONTO *********/
        IF CAN-FIND (FIRST tt-movto-pend) THEN DO:
            RUN piAtualizaAgregado.
        END.
        
        if pRowidDtDocEst <> ? then do:
            find bDtDocumEstAgregado
                where rowid(bDtDocumEstAgregado) = pRowidDtDocEst exclusive-lock no-error.
            if avail bDtDocumEstAgregado then do:
                assign bDtDocumEstAgregado.valor-mercad = de-vlr-merc-aux      
                       bDtDocumEstAgregado.tot-valor    = de-vlr-tot-nota-aux 
                       bDtDocumEstAgregado.log-situacao = yes.
            end.
        end.


    END. /* tt-docum-est */
    


end procedure.
procedure piAtualizaDocumEst.

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Abertura de Query */
    RUN openQueryStatic IN h-boRecebimento ("Main":U).

    /* Limpa Temp-Table de Itens */

    RUN emptyRowObject  IN h-boRecebimento.

    FOR EACH tt2-docum-est.
        DELETE tt2-docum-est.
    END.               

    /* Como a BO somente trabalha com 1 registro, o conteúdo da temp-table 
       ≤ transferido para outra temp-table idºntica */

    CREATE tt2-docum-est.
    BUFFER-COPY tt-docum-est TO tt2-docum-est.

    /* Transfere TT-DOCUM-EST para BO */
    RUN setRecord IN h-boRecebimento (INPUT TABLE tt2-docum-est ).
    
    /* Determina Defaults da Nota Fiscal */
    RUN setDefaultsNota IN h-boRecebimento.
    
    /* Cria DOCUM-EST */    
    RUN createRecord IN h-boRecebimento.
    IF  RETURN-VALUE = "NOK":U then do: 
        RUN piGeraErros (INPUT h-boRecebimento ).
        if l-erro-create = yes then
            return "NOK":U.

    end.
    run getCharField in h-boRecebimento ( input "nro-docto":U,
                                          output cnro-docto ).
    ASSIGN tt-docum-est.nro-docto = cnro-docto.

end procedure.
procedure piAtualizaItemDocEst.
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Atualiza FunØ o FIFO nas Ordens de Compra */
/*     IF  ( tt-item-doc-est.num-pedido <> 0     */
/*     OR    tt-item-doc-est.numero-ordem <> 0 ) */
/*     AND tt-item-doc-est.parcela = 0 THEN      */
/*         ASSIGN tt-item-doc-est.log-1 = YES.   */
    /* Acessa Chave do Documento */
    RUN linktoDocumEst IN h-boin176 (INPUT h-boRecebimento).

    /* Limpa Temp-Table de Itens */
    RUN emptyRowObject IN h-boin176.

    FOR EACH tt2-item-doc-est.
        DELETE tt2-item-doc-est.
    END.               

    /* Como a BO somente trabalha com 1 registro, o conteﬂdo da temp-table 
       ˝ transferido para outra temp-table id¨ntica */
    IF NOT CAN-FIND(FIRST tt-rat-lote
                    WHERE tt-rat-lote.cod-emitente = tt-item-doc-est.cod-emitente
                      AND tt-rat-lote.serie-docto  = tt-item-doc-est.serie-docto 
                      AND tt-rat-lote.nro-docto    = tt-item-doc-est.nro-docto   
                      AND tt-rat-lote.nat-operacao = tt-item-doc-est.nat-operacao
                      AND tt-rat-lote.sequencia    = tt-item-doc-est.sequencia   NO-LOCK) THEN DO:
        FOR FIRST bNaturOper FIELDS(tipo terceiros tp-oper-terc transf) 
            WHERE bNaturOper.nat-operacao = tt-item-doc-est.nat-operacao NO-LOCK:
        END.
        FIND FIRST item-uni-estab 
            WHERE item-uni-estab.cod-estabel = tt-docum-est.cod-estabel
              AND item-uni-estab.it-codigo   = tt-item-doc-est.it-codigo USE-INDEX estab NO-LOCK NO-ERROR.
        if  bNaturOper.tipo = 1
            and not bNaturOper.terceiros 
            or  (    bNaturOper.tp-oper-terc <> 1
                 and bNaturOper.tp-oper-terc <> 2 ) then 

                if  param-global.modulo-cq          /* Possui modulo CQ */
                and param-cq.tipo-cq > 1            /* SΩ Gera Roteiro ou CQ Total */
                and not bNaturOper.transf           /* Nao e Transferencia */
                and avail item-uni-estab
                and item-uni-estab.contr-qualid               /* Item possui CQ */
                and item.it-codigo <> " " then do:  /* Item deve ser codificado */ 

                    run findParamCQ   in h-boin367.
                    run findItem      in h-boin367 ( input tt-item-doc-est.it-codigo ).
                    run findItemUniEstab in h-boin367 ( input tt-item-doc-est.it-codigo,
                                                        input tt-docum-est.cod-estabel ).
                    run findDocumento in h-boin367 ( input tt-item-doc-est.cod-emitente,
                                                     input tt-item-doc-est.serie-docto,
                                                     input tt-item-doc-est.nro-docto,
                                                     input tt-item-doc-est.nat-operacao ).
                    run getDepositoCQ in h-boin367 ( output l-deposito-cq ).
                end. 
        assign tt-item-doc-est.cod-depos = if  estabelec.deposito-cq <> tt-item-doc-est.cod-depos 
                                           and l-deposito-cq
                                           then estabelec.deposito-cq
                                           else tt-item-doc-est.cod-depos.
        FIND FIRST bDtItDocum
            WHERE bDtItDocum.serie-docto   =    tt-item-doc-est.serie-docto  
              AND bDtItDocum.nro-docto     =    tt-item-doc-est.nro-docto  
              AND bDtItDocum.cod-emitente  =    tt-item-doc-est.cod-emitente 
              AND bDtItDocum.nat-operacao  =    tt-item-doc-est.nat-operacao 
              AND bDtItDocum.sequencia     =    tt-item-doc-est.sequencia   SHARE-LOCK NO-ERROR.
        IF AVAIL bDtItDocum AND bDtItDocum.cod-depos <> tt-item-doc-est.cod-depos THEN
            ASSIGN bDtItDocum.cod-depos = tt-item-doc-est.cod-depos.
        ASSIGN l-possui-rat-lote = YES.
    END.
    ELSE
        ASSIGN l-possui-rat-lote = NO.
    

    CREATE tt2-item-doc-est.
  
    BUFFER-COPY tt-item-doc-est TO tt2-item-doc-est.
    /* Transfere TT-ITEM-DOC-EST para BO */
    RUN setRecord IN h-boin176 (INPUT TABLE tt2-item-doc-est ).

    RUN setDefaultsFields IN h-boin176.

    /* Cria ITEM-DOC-EST */
    RUN createRecord IN h-boin176.
    RUN piGeraErros (INPUT h-boin176).
    if l-erro-create = yes THEN DO:
        return "NOK":U.

    END.
    /* Caso os Valores de IPI sejam informados, assume os valores do MAPA */
    IF  tt-item-doc-est.base-ipi[1]  <> 0
    OR  tt-item-doc-est.valor-ipi[1] <> 0  THEN
        RUN piAtualizaValoresIPI.
end procedure.
PROCEDURE piAtualizaAgregado.
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE h-boin223 AS HANDLE NO-UNDO.
    DEFINE VARIABLE rw-docum-est AS ROWID NO-UNDO.
    DEFINE BUFFER b1NatOper FOR natur-oper.
    IF NOT VALID-HANDLE(h-boin223) THEN
        RUN inbo/boin223.p PERSISTENT SET h-boin223.
    if not valid-handle(h-boRecebimento) then do:
       run inbo/boin090.p persistent set h-boRecebimento.

    end.
    /* Abertura de Query */
    RUN openQueryStatic IN h-boRecebimento ("Main":U).

    /* Limpa Temp-Table de Itens */
    RUN emptyRowObject  IN h-boRecebimento.

    FOR EACH tt2-docum-est.
        DELETE tt2-docum-est.
    END.   
    FIND FIRST bDocumEstAgregado
        WHERE bDocumEstAgregado.cod-emitente =  tt-docum-est.cod-emitente
          and bDocumEstAgregado.serie-docto  =  tt-docum-est.serie-docto 
          and bDocumEstAgregado.nro-docto    =  tt-docum-est.nro-docto   
          and bDocumEstAgregado.nat-operaca  =  tt-docum-est.nat-operacao NO-LOCK NO-ERROR.
    IF AVAIL bDocumEstAgregado THEN
        run setHandleDocumEst in h-boin223 ( input h-boRecebimento,
                                             input ROWID(bDocumEstAgregado) ). /*Rowid do docum-est corrente*/
    EMPTY TEMP-TABLE tt2-movto-pend.
    FOR EACH tt-movto-pend:
        CREATE tt2-movto-pend.
        BUFFER-COPY tt-movto-pend EXCEPT
            tipo
            nr-ord-produ
            it-codigo
            quantidade
            TO tt2-movto-pend.
        /* A ALTERAÄ«O DO NÈMERO DO DOCUMENTO FOI POSTA AQUI PELO FATO DE A NOTA DE RETORNO SIMB‡LICA GERAR NOTA NO FATURAMENTO */
        ASSIGN tt2-movto-pend.tipo = 2
               tt2-movto-pend.nat-operacao = tt-docum-est.nat-operacao
               tt2-movto-pend.nro-docto    = tt-docum-est.nro-docto
               tt2-movto-pend.serie-docto  = tt-docum-est.serie-docto.

    END.
    RUN openQueryStatic IN h-boin223 ("Main":U).
    RUN findDocumento IN h-boin223(input  tt-docum-est.cod-emitente,
                                   input  tt-docum-est.serie-docto,
                                   input  tt-docum-est.nro-docto,
                                   input  tt-docum-est.nat-operacao).
    RUN setConstraintOfDocumEst IN h-boin223(input  tt-docum-est.cod-emitente,
                                             input  tt-docum-est.serie-docto,
                                             input  tt-docum-est.nro-docto,
                                             input  tt-docum-est.nat-operacao).
    RUN emptyRowErrors  IN h-boin223.
    RUN setRecord IN h-boin223 (INPUT TABLE tt2-movto-pend ).

    RUN createRecord IN h-boin223. 

    RUN piGeraErros (INPUT h-boin223).

    if l-erro-create = yes THEN DO:
        return "NOK":U.
    END.
    ELSE DO: 
        EMPTY TEMP-TABLE tt-total-item.
        run calculateTotalItem in h-boin176 ( input tt-docum-est.cod-emitente,
                                              input tt-docum-est.serie-docto,
                                              input tt-docum-est.nro-docto,
                                              input tt-docum-est.nat-operacao,
                                              output table tt-total-item ).
        find first tt-total-item no-lock.

        run getRowid in h-boRecebimento ( output rw-docum-est ).
        if  avail tt-total-item then do: 
            FIND FIRST docum-est
                WHERE ROWID(docum-est) = rw-docum-est EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL docum-est THEN DO:
                ASSIGN docum-est.tot-peso       = tt-total-item.tot-peso.
            END.
            RELEASE docum-est.
        END.

        FIND FIRST tt2-movto-pend NO-LOCK NO-ERROR.
        FOR FIRST natur-oper
            WHERE natur-oper.nat-operacao = tt2-movto-pend.nat-operacao NO-LOCK:
        END.
        IF natur-oper.emite-duplic THEN DO:
            run getRowid in h-boRecebimento ( output rw-docum-est ).
            /*--- cria duplicata a partir do documento corrente ---*/
            run rep/re9341.p (input rw-docum-est, input no).
        END.

    END.

    EMPTY TEMP-TABLE tt2-movto-pend.
    /* CRIANDO O ITEM DO DOCUMENTO AGREGADO  - ESTE TEM QUE SER SOMENTE PARA O PRIMEIRO REGISTRO PORQUE ELE VAI LER OS ITENS DO DOCUMENTO AGREGADO - N«O ALTERAR PARA FOR EACH */
    FOR /* each */ first tt-movto-pend:
        EMPTY TEMP-TABLE tt2-movto-pend.
        CREATE tt2-movto-pend.
        BUFFER-COPY tt-movto-pend EXCEPT
                nat-comp
            TO tt2-movto-pend.
        /* A ALTERAÄ«O DO NÈMERO DO DOCUMENTO FOI POSTA AQUI PELO FATO DE A NOTA DE RETORNO SIMB‡LICA GERAR NOTA NO FATURAMENTO */
        ASSIGN tt2-movto-pend.tipo = 1
               tt2-movto-pend.nro-comp    = tt-docum-est.nro-docto
               tt2-movto-pend.nat-comp    = tt-docum-est.nat-operacao
               tt2-movto-pend.serie-comp  = tt-docum-est.serie-docto.

        RUN openQueryStatic IN h-boin223 ("Main":U).
        empty temp-table tt3-item-doc-est.
        RUN piCriaItemAgregado.
        /** CRIANDO ITEM DE RETORNO DO AGREGADO **/
        FOR EACH tt3-item-doc-est NO-LOCK:
            IF NOT VALID-HANDLE(h-boin176) THEN
                RUN inbo/boin176.p PERSISTENT SET h-boin176.
            RUN openQueryStatic IN h-boin176 ("Main":U).

            RUN setDefaultCalculoImpostos IN h-boin176 (INPUT YES,  /*** cΩdigo de tributaªío ***/
                                                        INPUT YES).  /*** al≠quota             ***/

            /* Acessa Chave do Documento */
            RUN linktoDocumEst IN h-boin176 (INPUT h-boRecebimento).
            IF NOT VALID-HANDLE(h-boin176) THEN
                RUN inbo/boin176.p PERSISTENT SET h-boin176.

            /* Limpa Temp-Table de Itens */
            RUN emptyRowObject IN h-boin176.

            FOR EACH tt2-item-doc-est.
                DELETE tt2-item-doc-est.
            END.               
            /* Como a BO somente trabalha com 1 registro, o conteﬂdo da temp-table 
               ˝ transferido para outra temp-table id¨ntica */
            CREATE tt2-item-doc-est.

            BUFFER-COPY tt3-item-doc-est TO tt2-item-doc-est.
            /* Transfere TT-ITEM-DOC-EST para BO */
            RUN setRecord IN h-boin176 (INPUT TABLE tt2-item-doc-est ).

            RUN setDefaultsFields IN h-boin176.

            /* Cria ITEM-DOC-EST */
            RUN createRecord IN h-boin176.
            RUN piGeraErros (INPUT h-boin176).
            if l-erro-create = yes THEN DO:
                return "NOK":U.

            END.
        END.

        RUN emptyRowErrors  IN h-boin223.

        RUN setRecord IN h-boin223 (INPUT TABLE tt2-movto-pend ).



        RUN createRecord IN h-boin223. /**** ESTµ DANDO ERRO NO OpenQueryStatic aqui *****/

        RUN piGeraErros (INPUT h-boin223).

        if l-erro-create = yes THEN DO:
            return "NOK":U.
        END.
        ELSE DO:
            IF VALID-HANDLE(h-boin176) THEN DO:
                run TransferTotalItensNota in h-boin176 ( tt-movto-pend.cod-emitente,
                                                          tt-movto-pend.serie-comp,
                                                          tt-movto-pend.nro-comp,
                                                          tt-movto-pend.nat-comp ).

            END.
            find first movto-pend
                where movto-pend.cod-emitente   = tt-movto-pend.cod-emitente
                  and movto-pend.serie-docto    = tt-movto-pend.serie-docto
                  and movto-pend.nro-docto      = tt-movto-pend.nro-docto
                  and movto-pend.nat-operacao   = tt-movto-pend.nat-operacao exclusive-lock no-error.
            if avail movto-pend then /* s¢ vou excluir a movto-pend quando o retorno for com nota pr¢pria - n∆o me pergunte pq isso n∆o funciona qdo n∆o Ç nota pr¢pria */
                delete movto-pend.
            IF NOT VALID-HANDLE(h-boin223a) THEN DO:   
                RUN inbo/boin223.p PERSISTENT SET h-boin223a.
            END.
            run findDocumento in h-boin223a( input tt-movto-pend.cod-emitente,
                                             input tt-movto-pend.serie-docto,
                                             input tt-movto-pend.nro-docto,
                                             input tt-movto-pend.nat-operacao ).

            /*--- cria o acabado a partir no documento posicionado ---*/
            run createAcabadoByOP in h-boin223a.
            if return-value = "NOK":U then do:
               RUN piGeraErros (INPUT h-boin223a).
               IF  VALID-HANDLE(h-boin223a) THEN DO:
                   RUN destroy IN h-boin223a.
                   ASSIGN h-boin223a = ?.
               END.
               if l-erro-create = yes THEN DO:
                   return "NOK":U.
               END.
            end.
            /* a criaá∆o do registro s¢ vale se a nota de retorno do item for diferente da nota de retorno do material beneficiado (agregado) */
            if tt-movto-pend.nro-docto <> tt-docum-est.nro-docto then do:
                find first dt-docum-est
                     WHERE dt-docum-est.nro-docto    = tt-docum-est.nro-docto
                       AND dt-docum-est.serie-docto  = tt-dt-docum-est.serie-docto
                       AND dt-docum-est.cod-emitente = tt-dt-docum-est.cod-emitente 
                       AND dt-docum-est.nat-operacao = tt-dt-docum-est.nat-operacao exclusive-lock no-error.
                if avail dt-docum-est then do:
                    create bDtDocumEstAgregado.
                    buffer-copy dt-docum-est except 
                        dt-docum-est.nro-docto   
                        dt-docum-est.nat-operacao
/*                         dt-docum-est.nome-arq  */
/*                         dt-docum-est.chave-xml */
/*                         dt-docum-est.protocolo */
                           to bDtDocumEstAgregado.
                    assign bDtDocumEstAgregado.nro-docto    = tt-movto-pend.nro-docto
                           bDtDocumEstAgregado.nat-operacao = tt-movto-pend.nat-operacao
                           bDtDocumEstAgregado.log-situacao = yes
/*                            bDtDocumEstAgregado.nome-arq     = "" /* tt-dt-docum-est.nome-arq */ */
/*                            bDtDocumEstAgregado.chave-xml    = ""                                */
/*                            bDtDocumEstAgregado.protocolo    = ""                                */
                           bDtDocumEstAgregado.narrativa    = dt-docum-est.narrativa.
/*                            dt-docum-est.dt-trans            = tt-docum-est.dt-trans  */
/*                            dt-docum-est.dt-rec              = tt-docum-est.dt-trans. */

                        
                           .
                    assign dt-docum-est.log-situacao = yes
                           dt-docum-est.dt-trans     = tt-docum-est.dt-trans
                           dt-docum-est.dt-rec       = tt-docum-est.dt-trans.
                end.
            end.

        END.
    END.

    IF  VALID-HANDLE(h-boin223) THEN DO:
        RUN destroy IN h-boin223.
        ASSIGN h-boin223 = ?.
    END.
    RUN destroyDBOs.
END PROCEDURE.
procedure piAtualizaNota.
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define variable c-serie-docto         AS CHARACTER  NO-UNDO.
       define variable c-nro-docto           AS CHARACTER  NO-UNDO.
       define variable i-cod-emitente        AS INTEGER    NO-UNDO.
       define variable c-nat-oper            AS CHARACTER  NO-UNDO.
       define variable l-ce-atual            AS LOGICAL    NO-UNDO.


       /* Integraªío Drawback */
       IF NOT AVAIL param-global THEN
           FIND FIRST param-global NO-LOCK NO-ERROR.

           IF  param-global.modulo-07
           AND param-global.modulo-ex THEN DO:

           IF NOT VALID-HANDLE(h-boin245na) 
           OR h-boin245na:TYPE      <> "PROCEDURE":U 
           OR h-boin245na:FILE-NAME <> "inbo/boin245na.p":U THEN 
             RUN inbo/boin245na.p PERSISTENT SET h-boin245na.

           IF NOT VALID-HANDLE(h-boad098) 
           OR h-boad098:TYPE      <> "PROCEDURE":U 
           OR h-boad098:FILE-NAME <> "adbo/boad098.p":U THEN 
             RUN adbo/boad098.p PERSISTENT SET h-boad098.             

           IF NOT VALID-HANDLE(h-bocx312) 
           OR h-bocx312:TYPE      <> "PROCEDURE":U 
           OR h-bocx312:FILE-NAME <> "cxbo/bocx312.p":U THEN 
             RUN cxbo/bocx312.p PERSISTENT SET h-bocx312.    

           RUN openQueryStatic IN h-boin245na (INPUT "Main").
           RUN goToKey         IN h-boin245na (INPUT tt-docum-est.nat-operacao).

           ASSIGN l-nat-op-drb = NO
                  c-nat-op-drb = "".
           /* RUN getLogField IN h-boin245na (INPUT "log-natur-operac-draw":U, OUTPUT l-nat-op-drb). */
           RUN getLogField IN h-boin245na (INPUT "log-natur-operac-draw":U, OUTPUT l-nat-op-drb).

           RUN findCodigo IN h-boad098 (INPUT tt-docum-est.cod-emitente,
                                        OUTPUT c-return).
           IF c-return = "" THEN
               RUN getIntField IN h-boad098 (INPUT "natureza":U, OUTPUT i-natureza).

           IF VALID-HANDLE(h-boin245na) THEN DO:
               DELETE PROCEDURE h-boin245na.
               ASSIGN h-boin245na = ?.
           END.

           IF VALID-HANDLE(h-boad098) THEN DO:
               DELETE PROCEDURE h-boad098.
               ASSIGN h-boad098 = ?.
           END.

           IF  (INT(SUBSTRING(c-nat-op-drb,94,1)) = 1 OR l-nat-op-drb) 
           AND (i-natureza = 3 OR i-natureza = 4) THEN DO:
               RUN ValidaValorAtoConcessorio IN h-bocx312 (INPUT tt-docum-est.serie-docto,
                                                           INPUT tt-docum-est.nro-docto,
                                                           INPUT tt-docum-est.cod-emitente,
                                                           INPUT tt-docum-est.nat-operacao,
                                                           OUTPUT l-nok).
               IF /*NOT*/ l-nok THEN DO:
                   RUN utp/ut-msgs.p (INPUT "show":U,
                                      INPUT 32476,
                                      INPUT "":U).
                   IF RETURN-VALUE = "NO":U THEN DO:

                      RUN imp/im9046rp.p (INPUT tt-docum-est.serie-docto,
                                          INPUT tt-docum-est.nro-docto,
                                          INPUT tt-docum-est.cod-emitente,
                                          INPUT tt-docum-est.nat-operacao,
                                          INPUT h-bocx312).


                      IF VALID-HANDLE(h-bocx312) THEN DO:
                          DELETE PROCEDURE h-bocx312.
                          ASSIGN h-bocx312 = ?.
                      END.

                      RETURN NO-APPLY.
                   END.

                   IF VALID-HANDLE(h-bocx312) THEN DO:
                       DELETE PROCEDURE h-bocx312.
                       ASSIGN h-bocx312 = ?.
                   END.
               END.
           END.
       END.
       ASSIGN c-serie-docto     = tt-docum-est.serie-docto 
              c-nro-docto       = tt-docum-est.nro-docto   
              i-cod-emitente    = tt-docum-est.cod-emitente
              c-nat-oper        = tt-docum-est.nat-operacao.


       /*--- Procede com a atualizaÙío do documento ---*/
       run atualizaDocumento in h-boRecebimento.

       RUN piGeraErros (INPUT h-boRecebimento ).


       IF l-erro-create = NO THEN
       DO:
           /*feita a leitura docum-est e nío da temp-table, pois caso o documento tenha sido atualizado
             com sucesso o registro nío vai estar mais na temp-table */
           RUN verificaSitNota IN h-boRecebimento (INPUT c-serie-docto,
                                             INPUT c-nro-docto,
                                             INPUT i-cod-emitente,
                                             INPUT c-nat-oper,
                                             OUTPUT l-ce-atual).
            RUN getRowid IN h-boRecebimento (OUTPUT rDocumEst).
       END.
       else 
           return "NOK":U.

       RETURN "OK":U. 

end procedure.
procedure piAtualizaValoresIPI.
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE rw-item-doc-est     AS ROWID NO-UNDO.
    IF NOT VALID-HANDLE(h-boin176) THEN
        RUN inbo/boin176.p PERSISTENT SET h-boin176.

    RUN getRowid IN h-boin176 (OUTPUT rw-item-doc-est).

    FIND item-doc-est
        WHERE ROWID(item-doc-est) = rw-item-doc-est EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL item-doc-est 
    AND item-doc-est.cd-trib-ipi <> 2 THEN DO: /* Isento */    

/*         IF  item-doc-est.cd-trib-ipi = 3 THEN /* Outros */                    */
/*             ASSIGN item-doc-est.ipi-outras[1] = tt-item-doc-est.base-ipi[1]   */
/*                    item-doc-est.valor-ipi[1]  = tt-item-doc-est.valor-ipi[1]. */
/*         ELSE                                                                  */
/*         IF  item-doc-est.cd-trib-ipi = 1        /* Tributado */               */
/*         OR  item-doc-est.cd-trib-ipi = 4 THEN   /* Reduzido  */               */
/*             ASSIGN item-doc-est.base-ipi[1]  = tt-item-doc-est.base-ipi[1]    */
/*                    item-doc-est.valor-ipi[1] = tt-item-doc-est.valor-ipi[1].  */

        /* Limpa Temp-Table de Itens */
        RUN emptyRowObject   IN h-boin176.

        /* Acessa Registro Item-doc-est na BO */
        RUN repositionRecord IN h-boin176 (INPUT ROWID(item-doc-est)).

        /* Valida Valores IPI */
        RUN validateValoresIpiPisCofins IN h-boin176.

        IF  RETURN-VALUE = "NOK":U THEN
            RUN piGeraErros (INPUT h-boin176).
        if l-erro-create = yes then
            return "NOK":U.

    END.            

end procedure.
procedure piGeraErros.
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER piHandle    AS HANDLE   NO-UNDO.

    RUN getRowErrors IN piHandle (OUTPUT TABLE RowErrors ).

    ASSIGN l-erro-create = NO.
    FOR EACH RowErrors 
        WHERE RowErrors.ErrorSubType = "ERROR":U:
        IF NOT (RowErrors.ErrorType = "INTERNAL":U 
        AND    (RowErrors.ErrorNumber = 8 
                OR RowErrors.ErrorNumber = 10 
                OR RowErrors.ErrorNumber = 3)) then do:

            FIND FIRST tt-erro
                 WHERE tt-erro.identif-segment  = c_id_reg_bloco_modul_edi 
                   AND tt-erro.cd-erro          = RowErrors.ErrorNumber
                   AND tt-erro.desc-erro        = RowErrors.ErrorDescription NO-LOCK NO-ERROR.
            IF  NOT AVAIL tt-erro THEN DO:
                CREATE tt-erro.
                ASSIGN tt-erro.identif-segment  = c_id_reg_bloco_modul_edi 
                       tt-erro.cd-erro          = RowErrors.ErrorNumber
                       tt-erro.desc-erro        = RowErrors.ErrorDescription .

            END.
            ASSIGN l-erro-create = YES.
        END.
    END.        

    RUN EmptyRowErrors IN piHandle.

end procedure.

PROCEDURE InitializeDBOs.
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN inbo/boin090.p PERSISTENT SET h-boRecebimento.
    RUN inbo/boin176.p PERSISTENT SET h-boin176.
    RUN inbo/boin092.p PERSISTENT SET h-boin092.
    RUN inbo/boin367.p PERSISTENT SET h-boin367.
    RUN inbo/boin366.p PERSISTENT SET h-boin366.


END PROCEDURE.

PROCEDURE DestroyDBOs.
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF  VALID-HANDLE(h-boRecebimento) THEN DO:
        RUN destroy IN h-boRecebimento.
        ASSIGN h-boRecebimento = ?.
    END.
    IF  VALID-HANDLE(h-boin176) THEN DO: 
        RUN destroy in h-boin176.
        ASSIGN h-boin176 = ?.
    END.
    IF  VALID-HANDLE(h-boin092) THEN DO: 
        RUN destroy IN h-boin092.
        ASSIGN h-boin092 = ?.
    END.
    IF  VALID-HANDLE(h-boin367) THEN DO:
        RUN destroy IN h-boin367.
        ASSIGN h-boin367 = ?.
    END.
    IF  VALID-HANDLE(h-boin366) THEN DO:
        RUN destroy IN h-boin366.
        ASSIGN h-boin366 = ?.
    END.
END PROCEDURE.
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

PROCEDURE piCriaItemAgregado.
    DEF VAR iSeqAgregado AS INTEGER NO-UNDO.
    ASSIGN  iSeqAgregado = 0.
    FOR EACH tt-it-movto-pend NO-LOCK
        WHERE tt-it-movto-pend.serie-docto  = tt-movto-pend.serie-docto    
          AND tt-it-movto-pend.nro-docto    = tt-movto-pend.nro-docto      
          AND tt-it-movto-pend.cod-emitente = tt-movto-pend.cod-emitente
          AND tt-it-movto-pend.nat-operacao = tt-movto-pend.nat-operacao
        /*  AND tt-it-movto-pend.nat-operacao    = tt-movto-pend.nat-operacao  */ : 
         ASSIGN iSeqAgregado = iSeqAgregado + 10. 
        RUN pi-acompanhar IN h-acomp (INPUT "Documento: " + tt-movto-pend.nro-docto).        
        FIND FIRST ITEM
            WHERE ITEM.it-codigo = tt-it-movto-pend.item-ems NO-LOCK NO-ERROR.
        CREATE tt3-item-doc-est.
        ASSIGN tt3-item-doc-est.serie-docto   = tt-movto-pend.serie-docto
               tt3-item-doc-est.nro-docto     = trim(STRING(INT(tt-movto-pend.nro-docto),">>>9999999"))
               tt3-item-doc-est.nat-operacao  = tt-it-movto-pend.nat-operacao
               tt3-item-doc-est.cod-emitente  = tt-it-movto-pend.cod-emitente
               tt3-item-doc-est.it-codigo     = tt-it-movto-pend.item-ems
               tt3-item-doc-est.etiquetas     = tt-it-movto-pend.etiquetas   
               tt3-item-doc-est.qt-do-forn    = tt-it-movto-pend.qt-do-forn
               tt3-item-doc-est.quantidade    = tt-it-movto-pend.dec-1  /* isso esta correto ??? */             
               tt3-item-doc-est.preco-total[1] = tt-it-movto-pend.preco-total 
               tt3-item-doc-est.preco-unit[1] = tt-it-movto-pend.preco-unit
               tt3-item-doc-est.desconto      = tt-it-movto-pend.desconto    
               tt3-item-doc-est.vl-subs[1]    = tt-it-movto-pend.valor-subs  
               tt3-item-doc-est.class-fiscal  = IF AVAIL ITEM THEN ITEM.class-fisc ELSE ""
               tt3-item-doc-est.narrativa     = tt-it-movto-pend.narrativa
               tt3-item-doc-est.sequencia     = iSeqAgregado
               tt3-item-doc-est.lote          = tt-it-movto-pend.lote
               tt3-item-doc-est.dt-vali-lote  = tt-it-movto-pend.dt-vali-lote
               tt3-item-doc-est.numero-ordem  = tt-it-movto-pend.numero-ordem
               tt3-item-doc-est.nr-ord-prod   = tt-it-movto-pend.nr-ord-produ
               tt3-item-doc-est.aliquota-icm  = tt-it-movto-pend.aliquota-icm /* tirei */
               tt3-item-doc-est.valor-icm     = tt-it-movto-pend.valor-icm
               tt3-item-doc-est.aliquota-ipi  = tt-it-movto-pend.aliquota-ipi
               tt3-item-doc-est.valor-ipi     = tt-it-movto-pend.valor-ipi /* tirei */ 
               tt3-item-doc-est.base-icm[1]   = tt-it-movto-pend.base-icm   
               tt3-item-doc-est.base-ipi[1]   = tt-it-movto-pend.base-ipi   
               tt3-item-doc-est.base-pis      = tt-it-movto-pend.base-pis
               tt3-item-doc-est.base-subs[1]  = tt-it-movto-pend.base-subs  
               tt3-item-doc-est.serie-comp    = tt-it-movto-pend.serie-comp /* IF AVAIL tt-item-terc THEN tt-item-terc.serie     ELSE "" */
               tt3-item-doc-est.nro-comp      = /* if tt-docum-est.nro-docto <> tt-it-movto-pend.nro-comp then tt-docum-est.nro-docto else */ tt-it-movto-pend.nro-comp /* esta validaá∆o foi colocada para quando a NF gerasse Faturamento */
               tt3-item-doc-est.nat-comp      = tt-it-movto-pend.nat-comp   /* IF AVAIL tt-item-terc THEN tt-item-terc.nat-oper  ELSE "" */
               tt3-item-doc-est.seq-comp      = tt-it-movto-pend.seq-comp.
        ASSIGN tt-docum-est.vl-subs          = tt-docum-est.vl-subs + tt-it-movto-pend.valor-subs.
    /*     ASSIGN tt3-item-doc-est.peso-liquido      = IF iQtItensNota = 1 THEN tt-movto-pend.peso-liq ELSE 1  */
    /*            tt3-item-doc-est.peso-liquido-item = IF iQtItensNota = 1 THEN tt-movto-pend.peso-liq ELSE 1  */
    /*            tt3-item-doc-est.peso-bruto-item   = IF iQtItensNota = 1 THEN tt-movto-pend.peso-bru ELSE 1. */
        FIND ITEM WHERE ITEM.it-codigo = tt3-item-doc-est.it-codigo NO-LOCK NO-ERROR.
        IF tt3-item-doc-est.nr-ord-prod <> 0 THEN DO:
            FIND FIRST ord-prod
                WHERE ord-prod.nr-ord-produ = tt3-item-doc-est.nr-ord-prod NO-LOCK NO-ERROR.
            IF AVAIL ord-prod THEN
                ASSIGN tt3-item-doc-est.conta-contabil = ord-prod.conta-ordem.
        END.
        ELSE DO:
            IF AVAIL ITEM AND item.tipo-con-est = 1 OR item.tipo-con-est = 4 
            THEN ASSIGN tt3-item-doc-est.conta-contabil = ITEM.conta-aplicacao.
            ASSIGN tt3-item-doc-est.conta-contabil = tt-it-movto-pend.conta-contabil.
        END.
/*         FIND FIRST prazo-compra                                                              */
/*             WHERE prazo-compra.numero-ordem = tt3-item-doc-est.numero-ordem                  */
/*               AND prazo-compra.quant-saldo  >= tt3-item-doc-est.quantidade NO-LOCK NO-ERROR. */
        FIND FIRST bfparam-re where bfparam-re.usuario = c-seg-usuario NO-LOCK NO-ERROR.
        if avail bfparam-re and not(bfparam-re.log-1) then do:
            FIND FIRST prazo-compra
               where prazo-compra.numero-ordem = tt3-item-doc-est.numero-ordem
                 and prazo-compra.situacao     =  2                 /* CONFIRMADA */
                 and ( prazo-compra.quant-saldo - prazo-compra.dec-1 ) >= tt3-item-doc-est.quantidade  no-lock no-error.
            IF AVAIL prazo-compra
            THEN ASSIGN tt3-item-doc-est.parcela = prazo-compra.parcela.
            FIND FIRST ordem-compra
                WHERE ordem-compra.numero-ordem = tt3-item-doc-est.numero-ordem NO-LOCK NO-ERROR.
            IF AVAIL ordem-compra THEN
                ASSIGN tt3-item-doc-est.num-pedido = ordem-compra.num-pedido
                       tt3-item-doc-est.quantidade = ordem-compra.qt-solic.
        end.
        FIND FIRST natur-oper
            WHERE natur-oper.nat-operacao = tt-movto-pend.nat-operacao NO-LOCK NO-ERROR.
        IF AVAIL natur-oper  THEN DO:
            ASSIGN tt3-item-doc-est.cd-trib-icm  = natur-oper.cd-trib-icm
                   tt3-item-doc-est.cd-trib-iss  =  natur-oper.cd-trib-iss
                   tt3-item-doc-est.cd-trib-ipi  =  natur-oper.cd-trib-ipi
                   tt3-item-doc-est.aliquota-icm = natur-oper.aliquota-icm.
/*             assign tt3-item-doc-est.idi-tributac-pis     = INT(substring(natur-oper.char-1,86,1))  */
/*                    tt3-item-doc-est.idi-tributac-cofins  = INT(substring(natur-oper.char-1,87,1)). */
            assign tt3-item-doc-est.idi-tributac-pis     = INT(substring(natur-oper.char-1,86,1))
                   tt3-item-doc-est.idi-tributac-cofins  = INT(substring(natur-oper.char-1,87,1)).
        END.
        IF tt3-item-doc-est.cd-trib-ipi = 3 
        THEN ASSIGN tt3-item-doc-est.ipi-outras[1] = tt3-item-doc-est.preco-total[1]
                    deBaseIPIOutras               = deBaseIPIOutras + tt3-item-doc-est.ipi-outras[1].

        IF tt3-item-doc-est.cd-trib-ipi = 1 
        THEN ASSIGN tt3-item-doc-est.base-ipi[1] = tt3-item-doc-est.preco-total[1] - tt3-item-doc-est.desconto[1].

        ASSIGN tt3-item-doc-est.cod-depos   = tt-it-movto-pend.cod-depos
               tt3-item-doc-est.cod-localiz = tt-it-movto-pend.cod-localiz.
    END.

END PROCEDURE.
