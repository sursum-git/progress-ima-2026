/*****************************************************************************
**         PROGRAMA: ESCC0305
**         DATA....: Fevereiro DE 2009
**         OBJETIVO: Emissao de Pedido de Compras
**         VERSAO..: 2.04.00.001
**         Programador: Juliana de Oliveira
******************************************************************************/
/***** DEFINI€åES ************************************************************/
{include/i-prgvrs.i CC0305RP 2.04.00.001}
        
{cdp/cdcfgmat.i}
{utp/utapi019.i}

{include/i-rpvar.i}
DEF VAR iPagina                  AS INTEGER  INIT 0   NO-UNDO.
def shared var l-imprime        as logical no-undo.

def temp-table tt-b2b
        field nr-pedido           like ordem-compra.num-pedido
        field cod-emitente         like pedido-compr.cod-emitente
        field nome-abrev                 like emitente.nome-abrev
        field numero-ordem         like ordem-compra.numero-ordem
        field it-codigo           like ordem-compra.it-codigo
        index pedido-emitente is primary 
                  nr-pedido
                  cod-emitente.

DEFINE TEMP-TABLE tt-param
        FIELD destino              AS INTEGER
        FIELD arquivo              AS CHAR
        FIELD diretorio          AS CHAR
        FIELD usuario              AS CHAR
        FIELD data-exec          AS DATE
        FIELD hora-exec          AS INTEGER
        FIELD i-pedi-i            AS INTEGER
        FIELD i-pedi-f            AS INTEGER
        FIELD l-narrativa-item  AS LOGICAL
        FIELD l-narrativa-ordem AS LOGICAL
        FIELD l-descricao          AS LOGICAL
        FIELD i-param-c          AS INTEGER
        FIELD i-ordem-ini          LIKE ordem-compra.numero-ordem
        FIELD i-ordem-fim          LIKE ordem-compra.numero-ordem
        FIELD l-email              AS LOG
        FIELD dir-pedido                AS CHAR.

DEFINE TEMP-TABLE tt-especifica
        FIELD wk-pedido    LIKE pedido-compr.num-pedido 
        FIELD wk-estab          LIKE pedido-compr.cod-estabel
        FIELD wk-data-ped        LIKE pedido-compr.data-pedido
        FIELD wk-cod-emit        LIKE pedido-compr.cod-emitente
        FIELD wk-cod-comprado LIKE ordem-compra.cod-comprado
        FIELD wk-seq              AS INTEGER FORMAT ">>9"
        FIELD wk-ordem          LIKE ordem-compra.numero-ordem
        FIELD wk-ordem-serv   LIKE ordem-compra.ordem-servic
        FIELD wk-item            LIKE ITEM.it-codigo     
        FIELD wk-descricao      AS CHAR FORMAT "x(2000)"
        FIELD wk-quant          LIKE prazo-compra.qtd-sal-forn
        FIELD wk-un                LIKE ITEM.un
        FIELD wk-contato          LIKE cotacao-item.contato
        FIELD wk-pa                LIKE prazo-compra.parcela
        FIELD wk-preco-unit   AS DEC FORMAT ">>>>>,>>9.99"
        FIELD wk-preco-total  AS DEC FORMAT ">>,>>>,>>9.99"
        FIELD wk-perc-des        AS DEC FORMAT ">>9.99"
        FIELD wk-val-des          AS DEC FORMAT ">>>>>,>>9.99"
        FIELD wk-perc-ipi        AS DEC FORMAT ">>9.99"
        FIELD wk-perc-icms      AS DEC FORMAT ">>9.99"
        FIELD wk-val-ipi          AS DEC FORMAT ">>>>,>>9.99"
        FIELD wk-entrega          AS DATE FORMAT "99/99/9999"
        FIELD wk-coment    AS CHAR FORMAT "x(400)"
        FIELD wk-cond-pagto   AS CHAR FORMAT "x(50)"    /*** Cond  Pagamento ***/
        FIELD wk-transport      AS CHAR FORMAT "x(40)"
        FIELD wk-trans-tel      AS CHAR FORMAT "x(10)"
        FIELD wk-moedaped        AS CHAR FORMAT "x(12)"
        FIELD wk-frete          AS CHAR FORMAT "x(40)"
        FIELD wk-end-ent          LIKE pedido-compr.end-entrega /*** Endere‡o de Entrega ***/
        FIELD wk-cod-emit-ter LIKE pedido-compr.cod-emit-terc
        FIELD wk-traco          AS CHAR FORMAT "x(36)" EXTENT 3 /*** Aprovador ***/
        FIELD wk-nome            AS CHAR FORMAT "x(30)" EXTENT 3 /*** Aprovador ***/
        FIELD wk-cargo          AS CHAR FORMAT "x(30)" EXTENT 3 /*** Aprovador ***/
        FIELD wk-processo        LIKE ordem-compra.nr-processo
        INDEX ch-ind wk-pedido wk-ordem wk-pa.

/*** Unir documento para enviar por e-mail ***/
DEF TEMP-TABLE tt-documentos
        FIELD nome-arquivo AS CHAR FORMAT "x(200)".

DEF BUFFER b-ped         FOR pedido-compr.
DEF BUFFER b-ord         FOR ordem-compra.
DEF BUFFER b-estabel FOR estabelec.

DEF TEMP-TABLE tt-raw-digita
        FIELD raw-digita AS RAW.

DEF INPUT PARAM raw-param AS RAW NO-UNDO.
DEF INPUT PARAM TABLE FOR tt-raw-digita.

{include/tt-edit.i}

{ccp/ccapi201.i}
{ccp/ccapi202.i}
{ccp/ccapi203.i}
{ccp/ccapi204.i}
{ccp/ccapi205.i}
{include/i_dbvers.i}
 /*
{prghur\fpp\fp3500tt.i shared} 
{prghur\fpp\fp9200.i10 shared}
{prghur\fpp\fp9200.i8}
{prghur\fpp\fp9400.i}
{prghur\fpp\fp9240.i}
*/
DEF VAR c-estabel-dest LIKE estabelec.cod-estabel.                 

DEF TEMP-TABLE tt-param-maq-ep-est
        FIELD cod-versao-integracao AS INT 
        FIELD cod-estabel LIKE c-estabel-dest
        FIELD opcao AS INT.

DEF TEMP-TABLE tt-maq-ep-est                                                             
        FIELD cod-versao-integracao AS INT                                         
        FIELD cod-estabel LIKE c-estabel-dest                                   
        FIELD opcao AS INT.                                                                       

{ccp/ccapi207.i}
{cdp/cd0666.i}  


/*** Variaveis do E-mail ***/
DEF VAR c-remetente     AS CHAR FORMAT "x(100)".
DEF VAR c-destino-mail AS CHAR FORMAT "x(100)".
DEF VAR c-assunto         AS CHAR FORMAT "x(100)".
DEF VAR c-arq-email     AS CHAR FORMAT "x(100)".
DEF VAR c-novo-arquivo AS CHAR NO-UNDO.

 DEF VAR cFontTTF                                               AS CHAR          NO-UNDO.
 DEF VAR cFontAFM                                               AS CHAR          NO-UNDO.

/*** Variaveis de Geracao do pdf ***/
DEF VAR cont-itens AS INT.
DEF VAR i-pagina   AS INT.
DEF VAR i-conta AS INT.
DEF VAR c-Logotipo AS CHAR NO-UNDO.

/*** Variaveis do Cabe‡alho ***/
DEF VAR v-nr-pedido   AS CHAR FORMAT "x(20)".
DEF VAR v-dt-pedido   AS CHAR FORMAT "x(10)".
DEF VAR v-nr-processo AS CHAR FORMAT "x(20)".

/*** Variaveis da Empresa de Faturamento ***/                                                   
DEF VAR v-emp-razao      LIKE empresa.razao-social.
DEF VAR v-emp-endereco1 AS CHAR FORMAT "x(60)".
DEF VAR v-emp-endereco2 AS CHAR FORMAT "x(60)".
DEF VAR v-emp-fone        AS CHAR FORMAT "x(30)".
DEF VAR v-emp-cgc          AS CHAR FORMAT "x(20)".
DEF VAR v-emp-cep          AS CHAR FORMAT "x(10)".
DEF VAR v-emp-fax          AS CHAR FORMAT "x(20)".
DEF VAR v-emp-insc        LIKE empresa.inscr-estad.

/*** Variaveis do Fornecedor ***/
DEF VAR v-for-razao      LIKE emitente.nome-emit.
DEF VAR v-for-endereco1 LIKE emitente.endereco.
DEF VAR v-for-endereco2 AS CHAR FORMAT "x(60)".
DEF VAR v-for-cep          AS CHAR FORMAT "x(10)".
DEF VAR v-for-fax          AS CHAR FORMAT "x(40)".
DEF VAR v-for-tel          LIKE emitente.telefone[1].
DEF VAR v-for-e-mail    LIKE comprador.e-mail.
DEF VAR v-for-cgc          AS CHAR FORMAT "x(20)".
DEF VAR v-for-insc        LIKE emitente.ins-estadual.
DEF VAR v-for-contato   AS CHAR FORMAT "x(60)".
                                                        
/*** Variaveis do Comprador ***/
DEF VAR v-comp-nome   LIKE comprador.nome.
DEF VAR v-comp-e-mail LIKE comprador.e-mail.

/*** Variaveis dos Itens - Detalhe (C01 = Coluna 1) ***/
DEF VAR c01-oc    AS CHAR FORMAT "x(100)" EXTENT 17. /*** Ordem Compra   ***/
DEF VAR c02-item        AS CHAR FORMAT "x(100)" EXTENT 17. /*** Item               ***/
DEF VAR c03-desc        AS CHAR FORMAT "x(100)" EXTENT 17. /*** Descri‡Æo         ***/
DEF VAR c04-qtde        AS CHAR FORMAT "x(100)" EXTENT 17. /*** Quantidade       ***/
DEF VAR c05-unid        AS CHAR FORMAT "x(100)" EXTENT 17. /*** Unidade         ***/
DEF VAR c06-vl-unid AS CHAR FORMAT "x(100)" EXTENT 17. /*** Valor Unitario ***/
DEF VAR c07-vl-tot  AS CHAR FORMAT "x(100)" EXTENT 17. /*** Valor Total ***/
DEF VAR c08-ipi  AS CHAR FORMAT "x(100)" EXTENT 17. /*** Perc. IPI        ***/
DEF VAR c09-icms        AS CHAR FORMAT "x(100)" EXTENT 17. /*** Perc. ICMS       ***/
DEF VAR c10-entrega AS CHAR FORMAT "x(100)" EXTENT 17. /*** Data Entrega   ***/
DEF VAR c11-os    AS CHAR FORMAT "x(100)" EXTENT 17. /*** Ordem Servi‡o  ***/

/*** Variaveis do Rodape ***/
DEF VAR v-desc-frete  AS CHAR NO-UNDO.          /*** Frete                 ***/
DEF VAR v-transp-nome AS CHAR FORMAT "x(40)".
DEF VAR v-transp-tele AS CHAR FORMAT "x(10)".
DEF VAR v-moeda    AS CHAR FORMAT "x(12)". /*** Moeda do Pedido ***/
DEF VAR v-cond-pagto  AS CHAR FORMAT "x(50)". /*** Condi‡Æo Pagto  ***/

DEF VAR v-tot-ipi       AS CHAR FORMAT "x(15)".
DEF VAR v-tot-pedido AS CHAR FORMAT "x(16)".
DEF VAR v-sub-total  AS CHAR FORMAT "x(16)".

/*** Variaveis de Local Entrega ***/
DEF VAR v-ent-nome        LIKE estabelec.nome.
DEF VAR v-ent-tel          LIKE emitente.telefone[1].
DEF VAR v-ent-fax          LIKE emitente.telefone[1].
DEF VAR v-ent-endereco1 AS CHAR FORMAT "x(60)".
DEF VAR v-ent-endereco2 AS CHAR FORMAT "x(60)".
DEF VAR v-ent-cep          AS CHAR FORMAT "x(10)".

/*** Variavel da Observa‡Æo ***/
DEF VAR v-obs AS CHAR FORMAT "x(85)" EXTENT 10 NO-UNDO.

/*** Variaveis de Assinatura - Aprovador ***/
DEF VAR v-traco AS CHAR FORMAT "x(36)" EXTENT 3.
DEF VAR v-nome  AS CHAR FORMAT "x(30)" EXTENT 3.
DEF VAR v-cargo AS CHAR FORMAT "x(30)" EXTENT 3.

DEF VAR v-pagina AS CHAR FORMAT "x(10)".
DEF VAR v-texto  AS CHAR FORMAT "x(30)".
DEF VAR c-end   AS CHAR.

DEF VAR l-first-emitente        AS LOG NO-UNDO.
DEF VAR l-pendente                AS LOG NO-UNDO.
DEF VAR c-desc-contrato  AS CHAR FORMAT "x(32)" LABEL "         " NO-UNDO.
DEF VAR de-preco-conv      LIKE ordem-compra.preco-unit FORMAT ">>>>>,>>>,>>9.9999" DECIMALS 8 NO-UNDO.
DEF VAR de-preco-unit      LIKE ordem-compra.preco-unit FORMAT ">>>>>,>>>,>>9.9999" DECIMALS 8 NO-UNDO.
DEF VAR de-ipi1                  AS DEC FORMAT ">>>>>,>>>,>>9.99"          DECIMALS 8 NO-UNDO.
DEF VAR de-preco-total    AS DEC FORMAT ">>>>>>>>,>>>,>>9.99"   DECIMALS 2 NO-UNDO.
DEF VAR de-enc                    AS DEC FORMAT ">,>>>,>>>,>>9.99".
DEF VAR de-desc                  AS DEC FORMAT ">>>>>>,>>9.9999".
DEF VAR de-tot-ipi                AS DEC FORMAT ">>>>>>,>>9.9999".


DEF VAR c-narrativa-it    AS CHAR FORMAT "x(900)".
DEF VAR c-narrativa-or    AS CHAR FORMAT "x(900)".
DEF VAR c-narrativa-des  AS CHAR FORMAT "x(60)".
DEF VAR c-desc-var-1            AS CHAR FORMAT "x(14)" NO-UNDO.
DEF VAR c-desc-var-2            AS CHAR FORMAT "x(14)" NO-UNDO.
DEF VAR c-tax-aux                  AS CHAR FORMAT "x(1)"  NO-UNDO.
DEF VAR c-pe-aux                        LIKE pedido-compr.num-pedido NO-UNDO.
DEF VAR de-ipi                    AS DEC FORMAT ">>>>>>,>>9.9999" DECIMALS 8 NO-UNDO.
DEF VAR de-ipi-tot                AS DEC FORMAT ">>>>>>,>>9.9999" DECIMALS 8 NO-UNDO.
DEF VAR de-preco-tot-aux        LIKE de-preco-total NO-UNDO.
DEF VAR de-total-pedido  AS DEC FORMAT ">>>>>,>>>,>>9.99"          DECIMALS 8 NO-UNDO.
DEF VAR de-sub-total-pedido AS DEC FORMAT ">>>>>,>>>,>>9.99"       DECIMALS 8 NO-UNDO.
DEF VAR de-preco-calc      LIKE de-preco-conv  NO-UNDO.
DEF VAR l-ordem                  AS LOG  NO-UNDO.
DEF VAR l-branco                        AS LOG  NO-UNDO.
DEF VAR l-r2                            AS LOG  NO-UNDO.
DEF VAR l-r3                            AS LOG  NO-UNDO.
DEF VAR l-imprimiu                AS LOG  NO-UNDO.
DEF VAR h-acomp                  AS HANDLE  NO-UNDO.
DEF VAR l-multiplanta      AS LOG INITIAL NO.
DEF VAR i-editor                        AS INT.
DEF VAR j                                  AS INT.
DEF VAR k                                  AS INT.
DEF VAR l                                  AS INT.
DEF VAR tot                              AS CHAR FORMAT "x(20)".
DEF VAR v-seq                      AS INT.




/*Variaveis para o bloqueio do fornecedor na 2.05*/
&IF defined(bf_mat_bloqueio_fornec) &THEN
        DEFINE VARIABLE h-api029   AS HANDLE  NO-UNDO.
        DEFINE VARIABLE i-situacao AS INTEGER NO-UNDO.
        DEFINE VARIABLE dt-vig-ini AS DATE      NO-UNDO.
        DEFINE VARIABLE dt-vig-fim AS DATE      NO-UNDO.

        DEF TEMP-TABLE tt-erro-2 NO-UNDO
                FIELD i-sequen AS INT                    
                FIELD cd-erro  AS INT
                FIELD mensagem AS CHAR FORMAT "x(255)"
                FIELD c-param  AS CHAR.

&ENDIF

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.


{ccp/cc0305.i2}

FIND FIRST param-compra NO-LOCK NO-ERROR.
FIND FIRST param-global NO-LOCK NO-ERROR.

ASSIGN c-empresa          = IF AVAIL param-global THEN param-global.grupo ELSE ""
           c-sistema      = "COMPRAS"
           c-titulo-relat = "EmissÆo de Pedido de Compras"
           l-branco        = NO.

{include\i-rpcab.i}

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "EmissÆo de Pedidos").





/***** PROCEDURE **********************************************************************/        

PROCEDURE PI-GERAR-PEDIDO:

DEF VAR c-arq-pedido AS CHAR FORMAT "x(100)".

/* Cria um novo arquivo .pdf no caminho especificado */
IF tt-param.destino = 1 THEN DO:
        ASSIGN c-end = "c:\teste.pdf". 
END.
ELSE IF tt-param.destino = 2 THEN DO:
        ASSIGN c-end = tt-param.arquivo. 
END.
ELSE DO:
        ASSIGN c-end = "c:\teste.pdf". 
END.

RUN pdf_new ("Spdf",c-end). 

        /* Carrega fonte */
        FILE-INFO:FILE-NAME = "PDFInclude\fonts\code39.ttf".
        ASSIGN cFontTTF = FILE-INFO:PATHNAME.
        FILE-INFO:FILE-NAME = "PDFInclude\fonts\code39.afm".
        ASSIGN cFontAFM = FILE-INFO:PATHNAME.
        RUN pdf_load_font ("Spdf","Code39",cFontTTF,cFontAFM,"").
        
        /* Carrega fonte Arial */
        FILE-INFO:FILE-NAME = "PDFinclude\fonts\arial.ttf".
        ASSIGN cFontTTF = FILE-INFO:PATHNAME.
        FILE-INFO:FILE-NAME = "PDFinclude\fonts\arial.afm".
        ASSIGN cFontAFM = FILE-INFO:PATHNAME.
        RUN pdf_load_font("Spdf","Arial",cFontTTF,cFontAFM,"").
        
        FILE-INFO:FILE-NAME = "PDFinclude\fonts\ariblk.ttf".
        ASSIGN cFontTTF = FILE-INFO:PATHNAME.
        FILE-INFO:FILE-NAME = "PDFInclude\fonts\arial.afm".
        ASSIGN cFontAFM = FILE-INFO:PATHNAME.
        RUN pdf_load_font("Spdf","ArialB",cFontTTF,cFontAFM,"").

        FILE-INFO:FILE-NAME = "PDFinclude\fonts\cour.ttf".
        ASSIGN cFontTTF = FILE-INFO:PATHNAME.
        FILE-INFO:FILE-NAME = "PDFinclude\fonts\cour.afm".
        ASSIGN cFontAFM = FILE-INFO:PATHNAME.
        RUN pdf_load_font("Spdf","CourierNew",cFontTTF,cFontAFM,"").

        /*Include da geracao de .pdf*/  
/* Cria um novo arquivo .pdf no caminho especificado */
/* RUN pdf_new ("Spdf","c:\teste.pdf"). */
/*RUN pdf_set_PageHeight ("Spdf",800). */         /* Zoom */

        /* Define o tipo de p gina portrait  = Retrato ou landscape = Paisagem */
        RUN pdf_set_Orientation ("Spdf","landscape").
        
        
        ASSIGN c-Logotipo = "C:\Datasul\image\logo.jpg".
        FILE-INFO:FILE-NAME = c-Logotipo.
        ASSIGN c-Logotipo = FILE-INFO:FULL-PATHNAME.
        RUN pdf_load_image  ("Spdf","ProSysLogo",c-Logotipo).

        RUN novo_arq.

        /*** Arquivos Gerados - TT usada para unir os documentos dos fornecedores e enviar por e-mail ***/
        IF tt-param.l-email THEN DO:
                DOS SILENT COPY VALUE(c-arq-pedido) VALUE(tt-param.dir-pedido + "pedido-" + STRING(v-nr-pedido) + "-" + STRING(TIME) + ".doc") /Y.

                CREATE tt-documentos.
                ASSIGN tt-documentos.nome-arquivo = tt-param.dir-pedido + "pedido-" + STRING(v-nr-pedido) + "-" + STRING(TIME) + ".doc".
        END.
        MESSAGE tt-param.dir-pedido
                VIEW-AS alert-BOX INFO BUTTONS OK.

        ASSIGN i-pagina = i-pagina + 1  /*** Contador de Pagina ***/
                   c01-oc         = ""
                   c02-item     = ""
                   c03-desc     = ""
                   c04-qtde     = ""
                   c05-unid     = ""
                   c06-vl-unid = ""
                   c07-vl-tot  = ""
                   c08-ipi       = ""
                   c09-icms     = ""
                   c10-entrega = ""
                   c11-os         = "".

        /*
        ASSIGN iPagina = iPagina + 1. 
        IF iPagina > 1 THEN DO:
           RUN pdf_new_page ("Spdf").
           RUN novo_arq.
        END.
          */

        
 RUN pdf_close("Spdf"). 
   
END PROCEDURE.


/*Procedure que define o relatorio*/
PROCEDURE novo_arq :

        /*FIND FIRST pedido-compr WHERE pedido-compr.num-pedido = tt-especifica.wk-pedido NO-LOCK NO-ERROR.*/
        
        ASSIGN iPagina = iPagina + 1.
         IF iPagina > 1 THEN DO:
           RUN pdf_new_page ("Spdf").
         END.
          

         /*ASSIGN l-imprime = YES.      */

        RUN pdf_place_image ("Spdf", "ProSysLogo",650,90,90,80).

        /*RUN pdf_new_page("Spdf").  */         /* Cria uma nova pagina no arquivo */
        RUN pdf_set_parameter("Spdf","Encrypt","false"). /* Coloca a senha se "yes" */ 
        RUN pdf_set_parameter("Spdf","Compress","false"). /* Executa sem chamar a Dll no caminho C:\suporte\PDFInclude\Dll\zlib.dll */  
        
        /***** Titulo *****/
        RUN pdf_set_font ("Spdf","Courier-Bold",10). 
        RUN pdf_set_font ("Spdf","ArialB",15).
        RUN pdf_text_at("Spdf","Pedido de Compra", 55).
        RUN pdf_set_font ("Spdf","Couriernew",10).
        RUN pdf_skip("Spdf").
        
        
        /************* Cabe‡alho ***************************************************/
        RUN pdf_set_BottomMargin ("Spdf", 80).
        RUN pdf_rect2 ("Spdf", 20,450,370,80,0.5). /* Retangulor maior da esquerda */
        RUN pdf_skipn ("Spdf" , 3).
        RUN pdf_set_font ("Spdf","Courier-Bold",10).
        RUN pdf_text_at("Spdf","Pedido:", 3).
        RUN pdf_set_font ("Spdf","Couriernew",10).
        RUN pdf_text_at("Spdf",STRING(v-nr-pedido), 13).
        RUN pdf_set_font ("Spdf","Courier-Bold",10).
        RUN pdf_text_at("Spdf","Processo:", 23).
        RUN pdf_set_font ("Spdf","Couriernew",10).
        RUN pdf_text_at("Spdf",STRING(v-nr-processo), 34).
        RUN pdf_set_font ("Spdf","Courier-Bold",10).
        RUN pdf_text_at("Spdf","Data:", 44).
        RUN pdf_set_font ("Spdf","Couriernew",10).
        RUN pdf_text_at("Spdf",STRING(v-dt-pedido), 54).
        RUN pdf_rect2 ("Spdf", 20,520,370,10,0.5).
        RUN pdf_rect2 ("Spdf", 80,520,60,10,0.5).
        RUN pdf_rect2 ("Spdf", 205,520,60,10,0.5).
        RUN pdf_rect2 ("Spdf", 265,520,60,10,0.5).
        
        RUN pdf_skip("Spdf").
        RUN pdf_set_font ("Spdf","Courier-Bold",10).
        RUN pdf_text_at("Spdf","DADOS PARA FATURAMENTO:", 3).
        RUN pdf_text_at("Spdf","FORNECEDOR", 67).
        RUN pdf_set_font ("Spdf","Couriernew",10).
        RUN pdf_skip("Spdf").
        RUN pdf_text_at("Spdf","Empresa:", 5). /*faturamento*/
        RUN pdf_text_at("Spdf",STRING(v-emp-razao), 15).
        RUN pdf_text_at("Spdf","Empresa:", 68).  /*fornecedor*/
        RUN pdf_text_at("Spdf",STRING(v-for-razao), 78).

        RUN pdf_skip("Spdf").
        RUN pdf_text_at("Spdf","Endere‡o:", 4). /*faturamento*/
        RUN pdf_text_at("Spdf",STRING(v-emp-endereco1), 15).
        RUN pdf_text_at("Spdf","Endere‡o:", 67). /*fornecedor*/
        RUN pdf_text_at("Spdf",STRING(v-emp-endereco1), 78).

        RUN pdf_skip("Spdf").
        RUN pdf_text_at("Spdf",STRING(v-emp-endereco2), 15). /*faturamento*/
        RUN pdf_text_at("Spdf",STRING(v-for-endereco2), 78). /*fornecedor*/

        RUN pdf_skip("Spdf").
        RUN pdf_text_at("Spdf","CNPJ:", 8).
        RUN pdf_text_at("Spdf",STRING(v-emp-cgc), 15). /*faturamento*/
        RUN pdf_text_at("Spdf","CNPJ:", 71).
        RUN pdf_text_at("Spdf",STRING(v-for-cgc), 78). /*fornecedor*/

        RUN pdf_text_at("Spdf","Insc.Estadual", 38).
        RUN pdf_text_at("Spdf",STRING(v-emp-insc), 50). /*faturamento*/
        RUN pdf_text_at("Spdf","Insc.Estadual", 100).
        RUN pdf_text_at("Spdf",STRING(v-for-insc), 115). /*fornecedor*/
        RUN pdf_skip("Spdf").
        RUN pdf_text_at("Spdf","CEP:", 9).
        RUN pdf_text_at("Spdf",STRING(v-emp-cep), 15). /*faturamento*/
        RUN pdf_text_at("Spdf","CEP:", 72).
        RUN pdf_text_at("Spdf",STRING(v-for-cep), 78). /*fornecedor*/

        RUN pdf_text_at("Spdf","Tel:", 30).
        RUN pdf_text_at("Spdf",STRING(v-emp-fone), 15). /*faturamento*/
        RUN pdf_text_at("Spdf","Tel:", 92).
        RUN pdf_text_at("Spdf",STRING(v-for-tel), 96). /*fornecedor*/

        RUN pdf_text_at("Spdf","Fax:", 50).
        RUN pdf_text_at("Spdf",STRING(v-emp-fax), 15). /*faturamento*/
        RUN pdf_text_at("Spdf","Fax:", 110).
        RUN pdf_text_at("Spdf",STRING(v-for-fax), 115). /*fornecedor*/

        RUN pdf_skip("Spdf").
        RUN pdf_text_at("Spdf","Comprador:", 3).
        RUN pdf_text_at("Spdf",STRING(v-comp-nome), 15). /*faturamento*/
        RUN pdf_text_at("Spdf",STRING(v-comp-e-mail), 15).

        RUN pdf_text_at("Spdf","Contato:", 68).
        RUN pdf_text_at("Spdf",STRING(v-for-contato), 72). /*fornecedor*/
        RUN pdf_text_at("Spdf",STRING(v-for-e-mail), 92).

        
        RUN pdf_rect2 ("Spdf", 20,510,370,20,0.5).
        RUN pdf_rect2 ("Spdf", 400,450,370,70,0.5).
        RUN pdf_rect2 ("Spdf", 400,510,370,10,0.5).
        RUN pdf_rect2 ("Spdf", 20,450,65,60,0.5). /* retangulo dentro de dados para faturamento */
        RUN pdf_rect2 ("Spdf", 400,450,65,60,0.5). /* retangulo dentro de FORNECEDOR */
        
        /*Retangulos do Browser*/
        RUN pdf_rect2 ("Spdf", 20,260,125,180,0.5). 
        RUN pdf_rect2 ("Spdf", 75,260,305,180,0.5). 
        RUN pdf_rect2 ("Spdf", 380,260,390,180,0.5).
        RUN pdf_rect2 ("Spdf", 440,260,155,180,0.5). 
        RUN pdf_rect2 ("Spdf", 470,260,60,180,0.5).
        RUN pdf_rect2 ("Spdf", 595,260,85,180,0.5).   
        RUN pdf_rect2 ("Spdf", 640,260,103,180,0.5).       

        RUN pdf_rect2 ("Spdf", 20,430,750,10,0.5). /*Linha do titulo do Browser*/
        RUN pdf_skipn("Spdf",2).
        RUN pdf_set_font ("Spdf","Courier-Bold",10).
        RUN pdf_text_at("Spdf","OC", 6).
        RUN pdf_text_at("Spdf","ITEM", 15).
        RUN pdf_text_at("Spdf","DESCRI€ÇO", 39).
        RUN pdf_text_at("Spdf","QTDE", 66).
        RUN pdf_text_at("Spdf","UN", 74).
        RUN pdf_text_at("Spdf","VL.UNIT", 79).
        RUN pdf_text_at("Spdf","VL.TOTAL", 89).
        RUN pdf_text_at("Spdf","%IPI", 100).
        RUN pdf_text_at("Spdf","%ICMS", 106).
        RUN pdf_text_at("Spdf","Entrega", 114).
        RUN pdf_text_at("Spdf","OP", 124).
        RUN pdf_set_font ("Spdf","Couriernew",10).

        /********* Preenchimento dos Itens *******/
   
        REPEAT cont-itens = 1 TO 17.
           if c03-desc[cont-itens] = "" then
           next.
        
        RUN pdf_skip("Spdf").
        RUN pdf_text_at("Spdf",c03-desc[cont-itens], 25).
        RUN pdf_text_at("Spdf",c01-oc[cont-itens], 3).
        RUN pdf_text_at("Spdf",c02-item[cont-itens], 14).
        RUN pdf_text_at("Spdf",c04-qtde[cont-itens], 60).
        RUN pdf_text_at("Spdf",c05-unid[cont-itens], 74).
        RUN pdf_text_at("Spdf",c06-vl-unid[cont-itens], 76).
        RUN pdf_text_at("Spdf",c07-vl-tot[cont-itens], 85).
        RUN pdf_text_at("Spdf",c08-ipi[cont-itens], 100).
        RUN pdf_text_at("Spdf",c09-icms[cont-itens], 106).
        RUN pdf_text_at("Spdf",c10-entrega[cont-itens], 113).
        RUN pdf_text_at("Spdf",c11-os[cont-itens], 119).
        
        END.

        DEFINE VARIABLE cont AS INTEGER   NO-UNDO.
        DEFINE VARIABLE num AS INTEGER  INITIAL 10   NO-UNDO.

        do cont = 1 to 33.
        RUN pdf_rect2 ("Spdf", 20,430 - num,750,10,0.5).
        RUN pdf_skip("Spdf").
        ASSIGN cont = cont + 1 .
        ASSIGN num = num + 10.
        END.
   
        DO cont = 1 TO 4.
        RUN pdf_rect2 ("Spdf", 20,420 - num,750,10,0.5).
        ASSIGN cont = cont + 1 .
        ASSIGN num = num + 10.
        END.

        RUN pdf_rect2 ("Spdf",20,210,160,40,0.5). 
        
        DO cont = 1 TO 4.
        RUN pdf_rect2 ("Spdf", 20,420 - num,450,10,0.5).
        ASSIGN cont = cont + 1 .
        ASSIGN num = num + 10.
        END.

        RUN pdf_rect2 ("Spdf", 470,210,300,40,0.5). /* retangulo do valor total do pedido */
        RUN pdf_rect2 ("Spdf", 620,210,150,40,0.5). 
        
        RUN pdf_text_at("Spdf","FRETE:", 3).
        RUN pdf_text_at("Spdf",v-desc-frete, 30). 

        RUN pdf_text_at("Spdf","SUBTOTAL:", 93).
        RUN pdf_text_at("Spdf",v-sub-total, 111).
        RUN pdf_skip("Spdf").
        RUN pdf_text_at("Spdf","TRANSPORTADORA:", 3).
        RUN pdf_text_at("Spdf",v-transp-nome, 30).
        RUN pdf_text_at("Spdf","VALOR IPI:", 92).
        RUN pdf_text_at("Spdf",v-tot-ipi, 114).
        RUN pdf_skip("Spdf").
        RUN pdf_text_at("Spdf","MOEDA PEDIDO:", 3).
        RUN pdf_text_at("Spdf",v-moeda, 30).
        RUN pdf_skip("Spdf").
        RUN pdf_text_at("Spdf","CONDI€ÇO PAGAMENTO:", 3).
        RUN pdf_text_at("Spdf", STRING(v-cond-pagto), 30).

        RUN pdf_text_at("Spdf","VALOR TOTAL DO PEDIDO:", 80).
        RUN pdf_text_at("Spdf",v-tot-pedido, 111).

        RUN pdf_skipn("Spdf",2).
        RUN pdf_set_font ("Spdf","Courier-Bold",10).
        RUN pdf_text_at("Spdf","OBSERVA€åES", 3).
        RUN pdf_text_at("Spdf","LOCAL DE ENTREGA", 73).
        RUN pdf_set_font ("Spdf","Couriernew",10).
        
        RUN pdf_skip("Spdf").
        RUN pdf_text_at("Spdf","Empresa:", 81). /*faturamento*/
        RUN pdf_text_at("Spdf", STRING(v-ent-nome), 90).

        RUN pdf_skip("Spdf").
        RUN pdf_text_at("Spdf","Endere‡o:", 80). /*faturamento*/
        RUN pdf_text_at("Spdf", STRING(v-ent-endereco1), 90).

        RUN pdf_skip("Spdf").                                                   
        RUN pdf_text_at("Spdf", STRING(v-ent-endereco2), 90).  

        RUN pdf_skip("Spdf").
        RUN pdf_text_at("Spdf","CEP:", 85).
        RUN pdf_text_at("Spdf",STRING(v-ent-cep), 90).

        RUN pdf_skip("Spdf").
        RUN pdf_text_at("Spdf","Tel:", 85).
        RUN pdf_text_at("Spdf",STRING(v-ent-tel), 90).
        
        RUN pdf_text_at("Spdf","Fax:", 100).
        RUN pdf_text_at("Spdf",STRING(v-ent-fax), 110).

         /*** Dados da Observa‡Æo ***/
        DEFINE VARIABLE cText AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE dFontSize AS CHARACTER   NO-UNDO.

        REPEAT i-conta = 1 TO 10:
                if v-obs[i-conta] = "" then
                        next.
        RUN pdf_skip("Spdf"). 
        RUN pdf_text_align("Spdf",v-obs[i-conta], "left" , 5,180).
        /*RUN pdf_text_at("Spdf",v-obs[i-conta],3). */
        END.
   
        RUN pdf_rect2 ("Spdf", 20,110,420,80,0.5). /* retangulo do valor total do pedido */
        RUN pdf_rect2 ("Spdf", 440,130,330,60,0.5).
        RUN pdf_rect2 ("Spdf", 440,130,100,60,0.5). /* RETANGULO DO LOCAL DA ENTREGA */

        RUN pdf_rect2 ("Spdf", 20,90,180,1,0.5). /* LINHA DA ASSINATURA DO DIRETOR */
        RUN pdf_rect2 ("Spdf", 230,90,180,1,0.5).  /* LINHA DA ASSINATURA DO GERENTE */
        RUN pdf_rect2 ("Spdf", 440,90,180,1,0.5).  /* LINHA DA ASSINATURA DO COMPRADOR */
        RUN pdf_skipn("Spdf",3).
        RUN pdf_set_font ("Spdf","Courier-Bold",10).  
        RUN pdf_text_at("Spdf","DIRETOR", 12).
        RUN pdf_text_at("Spdf","GERENTE", 50).
        RUN pdf_text_at("Spdf","COMPRADOR", 83).
        RUN pdf_text_at("Spdf",STRING(v-pagina), 115).
        RUN pdf_set_font ("Spdf","Couriernew",10).

        /* 
        ASSIGN iPagina = iPagina + 1. 
        IF iPagina > 1 THEN DO:
           RUN pdf_new_page ("Spdf").
           RUN novo_arq.
        END.
        */
        
        
END PROCEDURE.


PROCEDURE PI-ENVIAR-EMAIL:

        FOR EACH tt-envio2.
                DELETE tt-envio2.
        END.

        FOR EACH tt-mensagem.
                DELETE tt-mensagem.
        END.

        RUN utp/utapi019.p PERSISTENT SET h-utapi019.

        CREATE tt-envio2.
        ASSIGN tt-envio2.versao-integracao = 1
                   tt-envio2.exchange             = IF AVAIL param-global THEN param-global.log-1 ELSE NO
                   tt-envio2.servidor             = IF AVAIL param-global THEN param-global.serv-mail ELSE ""
                   tt-envio2.porta                       = IF AVAIL param-global THEN param-global.porta-mail ELSE 25
                   tt-envio2.remetente           = c-remetente
                   tt-envio2.destino               = c-destino-mail
                   tt-envio2.assunto               = c-assunto
                   tt-envio2.arq-anexo           = c-arq-email
                   tt-envio2.formato               = "HTML".

        CREATE tt-mensagem.
        ASSIGN tt-mensagem.seq-mensagem = 1
                   tt-mensagem.mensagem  = "Prezados Senhores, favor responder ao PEDIDO DE COMPRA em anexo. Atenciosamente.".

        RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                                                   INPUT  TABLE tt-mensagem,
                                                                   OUTPUT TABLE tt-erros).

        IF RETURN-VALUE = "NOK" THEN DO:
                OUTPUT TO VALUE(tt-param.dir-pedido + "envemail.txt").
                        FOR EACH tt-erros:
                                DISP tt-erros with 1 column width 300.
                        END.
                OUTPUT CLOSE.
                MESSAGE "E-mail nÆo foi enviado." SKIP
                                "Visualizar erros no arquivo: " tt-param.dir-pedido + "envemail.txt" VIEW-AS alert-BOX ERROR.
        END.

        DELETE PROCEDURE h-utapi019.

END PROCEDURE.

PROCEDURE PI-UNIAO-DOC-PDF:
        /*
        /*** Junta todos os arquivos gerados na TT-Documentos e salva com o nome do fornecedor ***/
        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DEF VAR i-arq AS INT.
        /*
        CREATE "word.application" ChWord.
        ChWord:VISIBLE = FALSE.
        ChWord:Displayalerts = TRUE NO-ERROR.
        */
        
        ASSIGN i-arq = 0.

        /*** Abre o primeiro registro da tt-documento, insere os demais registros no primeiro arquivo ***/
        FOR EACH tt-documentos:
                ASSIGN i-arq = i-arq + 1.

                IF i-arq = 1 THEN DO:
                        RUN pdf_open_pdf ("Spdf","c:\teste.pdf","INV").
                        /*
                        ChWord:SELECTION:ENDKEY(6).
                        ChWord:SELECTION:TypeParagraph.
                        */
                END.
                ELSE
                        /* ChWord:Selection:InsertFile(tt-documentos.nome-arquivo) NO-ERROR.*/
                        MESSAGE "chego ateh aki e nao sei o q colocar"
                                VIEW-AS alert-BOX INFO BUTTONS OK.
        END.
        
        /*
        ChWord:SELECTION:TypeBackspace.
        ChWord:ActiveDocument:Protect(0).
        */
        
        /*** Muda o nome do novo arquivo gerado ***/
        ASSIGN c-novo-arquivo = tt-param.dir-pedido + "arq-email-" + STRING(tt-especifica.wk-pedido) + ".doc".
        
        /*
        ChWord:ActiveDocument:SaveAs(c-novo-arquivo).
        ChWord:Documents:CLOSE NO-ERROR.
        ChWord:APPLICATION:QUIT.
        */ 
        /*
        RELEASE OBJECT ChDoc.
        RELEASE OBJECT ChWord.
        */
        /*** Esse novo arquivo criado ‚ a uniao dos registro da tt-documento ***/
        FOR EACH tt-documentos:
                DELETE tt-documentos.
        END.

        /*** Apagar os documento Unidos ***/
        DOS SILENT DEL VALUE(tt-param.dir-pedido + "pedido-*.doc") NO-ERROR.
  */
END PROCEDURE.

PROCEDURE PI-TIRAR-CARAC:

        DEF INPUT-OUTPUT  PARAMETER v-entrada AS CHAR FORMAT "x(1000)".
        DEF VAR i AS INT.
        DEF VAR l AS INT.

        REPEAT i = 1 TO LENGTH(v-entrada).
                   IF SUBSTRING(v-entrada,i,1) = CHR(10) THEN DO:
                          OVERLAY(v-entrada,i,1) = " ".
                   END.            
        END.

        REPEAT:
           l = INDEX(v-entrada,"  ").
           IF l = 0 THEN LEAVE.
           REPEAT i = 1 TO LENGTH(v-entrada).                                                     
                          IF SUBSTRING(v-entrada,i,1) = " " AND SUBSTRING(v-entrada,i + 1,1) = " " THEN DO:
                                 v-entrada = SUBSTRING(v-entrada,1,i) + SUBSTRING(v-entrada,i + 2).                       
                          END.             
           END.
        END.

END PROCEDURE.


/***** BLOCO PRINCIPAL *****************************************************************/
FOR EACH b-ped
   WHERE b-ped.num-pedido >= tt-param.i-pedi-i
         AND b-ped.num-pedido <= tt-param.i-pedi-f
BREAK BY b-ped.cod-emitente:

        &IF defined(bf_mat_bloqueio_fornec) &THEN
                RUN cdp/cdapi029.p (INPUT b-ped.responsavel,
                                                        INPUT 1,
                                                        INPUT b-ped.data-pedido,
                                                        INPUT b-ped.cod-emitente,
                                                        OUTPUT i-situacao,
                                                        OUTPUT dt-vig-ini,
                                                        OUTPUT dt-vig-fim,
                                                        OUTPUT TABLE tt-erro-2).

                
                /*--- NÆo imprime pedido quando a api retornar "NOK" ---*/
                IF RETURN-VALUE = "NOK":U THEN
                   NEXT.
        &ENDIF

        ASSIGN l-first-emitente = first-of (b-ped.cod-emitente)
                   v-seq = 0.

        IF NOT (b-ped.impr-pedido OR
                &if DEFINED(bf_mat_contratos) &then
                        b-ped.nr-contrato <> 0)
                &else
                        b-ped.nr-contrato <> "")
                &endif
        THEN NEXT.

        FIND pedido-compr WHERE ROWID(pedido-compr) = ROWID(b-ped) NO-ERROR NO-WAIT.
        IF LOCKED pedido-compr THEN NEXT.

        FIND emitente NO-LOCK
   WHERE emitente.cod-emitente = pedido-compr.cod-emitente NO-ERROR.

        /**** Aprova‡Æo Eletr“nica ****/
        /*
        ASSIGN l-pendente = NO.
        IF param-compra.log-1 THEN DO:
                FOR EACH b-ord NO-LOCK
                   WHERE b-ord.num-pedido = pedido-compr.num-pedido:
                        IF pedido-compr.emergencial THEN
                                RUN cdp/cdapi172 (6, ROWID(b-ord), OUTPUT l-pendente).
                        ELSE
                                RUN cdp/cdapi172 (4, ROWID(b-ord), OUTPUT l-pendente).

                        IF l-pendente THEN DO:
                                MESSAGE "Pedido nao pode ser impresso!! Pedido nao foi Aprovado!!" VIEW-AS alert-BOX INFORMATION.

                                LEAVE.
                        END.
                        ELSE
                                LEAVE.
                END.
        END.
*/
        IF l-pendente THEN NEXT.

        RUN pi-acompanhar IN h-acomp(INPUT "Lendo Pedido:" + STRING(pedido-compr.num-pedido)).

        ASSIGN c-pe-aux         = pedido-compr.num-pedido
                   de-total-pedido = 0
                   c-desc-var-1 = ""
                   c-desc-var-2 = "".

        /*** Buscar descricao da Moeda do pedido ***/
        FIND FIRST moeda NO-LOCK
                 WHERE moeda.mo-codigo = pedido-compr.i-moeda NO-ERROR.
        IF AVAIL moeda THEN
                ASSIGN v-moeda = moeda.descricao.
        ELSE
                ASSIGN v-moeda = "".

        IF pedido-compr.frete = 1 THEN
                ASSIGN v-desc-frete  = "CIF".
        ELSE
                ASSIGN v-desc-frete  = "FOB".

        ASSIGN v-transp-nome = ""
                   v-transp-tele = "".

        FIND transporte WHERE transporte.cod-transp = pedido-compr.cod-transp NO-LOCK NO-ERROR.
        IF AVAIL transporte THEN
           ASSIGN v-transp-nome = LC(transporte.nome)
                          v-transp-tele = transporte.telefone.

        /*** Colocar a Primeira letra do Transportador em Caixa Alta ***/
        REPEAT l = 1 TO LENGTH(v-transp-nome).
                IF l = 1 THEN
                   OVERLAY(v-transp-nome,l,1) = CAPS(SUBSTRING(v-transp-nome,l,1)).
                IF SUBSTRING(v-transp-nome,l,1) = " " AND SUBSTRING(v-transp-nome,l + 1,1) <> " " THEN
                   OVERLAY(v-transp-nome,l + 1,1) = CAPS(SUBSTRING(v-transp-nome,l + 1,1)).
        END.

        FIND cond-pagto NO-LOCK
   WHERE cond-pagto.cod-cond-pag = pedido-compr.cod-cond-pag NO-ERROR.
        IF AVAIL cond-pagto THEN DO:
                IF cond-pagto.cod-cond-pag = 0 THEN DO:
                        FIND FIRST cond-especif NO-LOCK
                                 WHERE cond-especif.num-pedido = pedido-compr.num-pedido NO-ERROR.
                        IF AVAIL cond-especif THEN
                                REPEAT i-conta = 1 TO 12:
                                        IF cond-especif.perc-pagto[i-conta] <> 0 THEN
                                                ASSIGN v-cond-pagto = v-cond-pagto +
                                                                                          STRING(cond-especif.data-pagto[i-conta],"99/99/99") + "-" +
                                                                                          STRING(cond-especif.perc-pagto[i-conta],">>9.99") + "% ".

                                END.

                        ASSIGN c-desc-var-1 = "Especifica"
                                   c-desc-var-2 = "".
                END.
                ELSE
                        ASSIGN v-cond-pagto = "".

                IF cond-pagto.cod-vencto = 1 THEN
                        assign c-desc-var-1 = STRING(prazos[1]) + " "
                                                                + STRING(prazos[2]) + " "
                                                                + STRING(prazos[3])
                                   c-desc-var-2 = STRING(prazos[4]) + " "
                                                                + STRING(prazos[5]) + " "
                                                                + STRING(prazos[6]) + " DD".

                IF cond-pagto.cod-vencto = 2 THEN
                        ASSIGN c-desc-var-1 = "· Vista"
                                   c-desc-var-2 = "".

                IF cond-pagto.cod-vencto = 3 THEN
                        ASSIGN c-desc-var-1 = "Antecipado"
                                   c-desc-var-2 = "".

                IF cond-pagto.cod-vencto = 4 THEN
                        ASSIGN c-desc-var-1 = "Contra Entrega"
                                   c-desc-var-2 = "".

                IF cond-pagto.cod-vencto = 5 THEN
                        ASSIGN c-desc-var-1 = STRING(prazos[1]) + " "
                                                                + STRING(prazos[2]) + " "
                                                                + STRING(prazos[3])
                                   c-desc-var-2 = STRING(prazos[4]) + " "
                                                                + STRING(prazos[5]) + " "
                                                                + STRING(prazos[6]) + " FD".

                IF cond-pagto.cod-vencto = 6 THEN
                        ASSIGN c-desc-var-1 = STRING(prazos[1]) + " "
                                                                + STRING(prazos[2]) + " "
                                                                + STRING(prazos[3])
                                   c-desc-var-2 = STRING(prazos[4]) + " "
                                                                + STRING(prazos[5]) + " "
                                                                + STRING(prazos[6]) + " FQ".

                IF cond-pagto.cod-vencto = 7 THEN
                        ASSIGN c-desc-var-1 = STRING(prazos[1]) + " "
                                                                + STRING(prazos[2]) + " "
                                                                + STRING(prazos[3])
                                   c-desc-var-2 = STRING(prazos[4]) + " "
                                                                + STRING(prazos[5]) + " "
                                                                + STRING(prazos[6]) + " FM".

                IF cond-pagto.cod-vencto = 8 THEN
                        ASSIGN c-desc-var-1 = STRING(prazos[1]) + " "
                                                                + STRING(prazos[2]) + " "
                                                                + STRING(prazos[3])
                                   c-desc-var-2 = STRING(prazos[4]) + " "
                                                                + STRING(prazos[5]) + " "
                                                                + STRING(prazos[6]) + " FS".

                IF cond-pagto.cod-vencto = 9 THEN
                        ASSIGN c-desc-var-2 = STRING(prazos[4]) + " "
                                                                + STRING(prazos[5]) + " "
                                                                + STRING(prazos[6]) + "Apresenta‡Æo"
                                   c-desc-var-1 = STRING(prazos[1]) + " "
                                                                + STRING(prazos[2]) + " "
                                                                + STRING(prazos[3]).
        END.

        FIND emitente  WHERE emitente.cod-emitente = pedido-compr.cod-emitente NO-LOCK NO-ERROR.

        ASSIGN l-ordem  = NO
                   l-imprimiu = NO.

        FOR EACH ordem-compra USE-INDEX pedido
           WHERE ordem-compra.num-pedido = pedido-compr.num-pedido
                 AND ordem-compra.situacao  <> 4
                 AND ordem-compra.situacao  <> 6 NO-LOCK:

                /*** Integra‡Æo M¢dulo de Contratos ***/
                &if DEFINED(bf_mat_contratos) &then
                IF param-global.modulo-cn AND ordem-compra.nr-contrato <> 0 THEN DO:
                &else
                IF param-global.modulo-cn AND ordem-compra.nr-contrato <> "" THEN DO:
                &endif

                        IF ordem-compra.numero-ordem < tt-param.i-ordem-ini OR
                           ordem-compra.numero-ordem > tt-param.i-ordem-fim THEN
                                NEXT.

                        FIND contrato-for EXCLUSIVE-LOCK
                   WHERE contrato-for.nr-contrato = ordem-compra.nr-contrato NO-ERROR.
                        IF AVAIL contrato-for THEN DO:
                                ASSIGN c-desc-contrato = contrato-for.des-contrat.

                                IF tt-param.i-param-c = 1 AND ordem-compra.sit-ordem-contrat <> 2 THEN
                                        NEXT.
                                ELSE
                                        IF tt-param.i-param-c = 2 AND ordem-compra.sit-ordem-contrat <> 1 THEN
                                                NEXT.
                                        ELSE
                                                IF tt-param.i-param-c = 3 AND ordem-compra.sit-ordem-contrat = 3 THEN
                                                        NEXT.

                                ASSIGN contrato-for.ind-sit-contrat = 2.           
                        END.
                END.

                /* Fim Integra‡Æo Contratos */

                FIND FIRST cotacao-item NO-LOCK
                         WHERE cotacao-item.numero-ordem = ordem-compra.numero-ordem
                           AND cotacao-item.cod-emitente = ordem-compra.cod-emitente
                           AND cotacao-item.cot-aprovada NO-ERROR.
                FIND item-fornec NO-LOCK
           WHERE item-fornec.it-codigo  = ordem-compra.it-codigo
                 AND item-fornec.cod-emitente = pedido-compr.cod-emitente NO-ERROR.

                FIND item NO-LOCK
           WHERE item.it-codigo = ordem-compra.it-codigo NO-ERROR.

                FIND narrativa NO-LOCK
           WHERE narrativa.it-codigo = ordem-compra.it-codigo NO-ERROR.

                ASSIGN c-narrativa-des = "".

                IF tt-param.l-descricao AND AVAIL item THEN
                        ASSIGN c-narrativa-des = ITEM.desc-item.

                IF ordem-compra.mo-codigo > 0 THEN DO:
                        RUN cdp/cd0812.p (INPUT  ordem-compra.mo-codigo,
                                                          INPUT  0,
                                                          INPUT  ordem-compra.preco-fornec,
                                                          INPUT  pedido-compr.data-pedido,
                                                          OUTPUT de-preco-conv).

                        IF de-preco-conv = ? THEN
                                ASSIGN de-preco-conv = ordem-compra.preco-fornec.
                END.
                ELSE
                        ASSIGN de-preco-conv = ordem-compra.preco-fornec.

                ASSIGN de-preco-unit = de-preco-conv
                           de-ipi-tot   = 0.

                /*** Buscar totais ***/
                FOR EACH  prazo-compra NO-LOCK
                        WHERE prazo-compra.numero-ordem  = ordem-compra.numero-ordem
                        AND   prazo-compra.situacao      <> 4
                        AND   prazo-compra.situacao      <> 6:

                        ASSIGN de-preco-tot-aux = de-preco-conv * prazo-compra.qtd-sal-forn
                                   de-preco-calc        = 0
                                   de-desc                = 0
                                   de-enc                  = 0
                                   de-ipi                  = 0.

                        IF param-compra.ipi-sobre-preco = 2 THEN DO:
                                IF ordem-compra.perc-descto > 0 THEN
                                        ASSIGN de-desc = ROUND(de-preco-tot-aux * ordem-compra.perc-descto / 100,2).

                                IF ordem-compra.taxa-financ = NO THEN DO:
                                        ASSIGN de-enc = de-preco-tot-aux - de-desc.

                                        RUN ccp/cc9020.p (INPUT  YES,
                                                                          INPUT  ordem-compra.cod-cond-pag,
                                                                          INPUT  ordem-compra.valor-taxa,
                                                                          INPUT  ordem-compra.nr-dias-taxa,
                                                                          INPUT  de-enc,
                                                                          OUTPUT de-preco-calc).

                                        ASSIGN de-enc = ROUND(de-preco-calc - de-enc,2).
                                END.
                                ELSE
                                        ASSIGN de-preco-calc = de-preco-tot-aux - de-desc.

                                IF ordem-compra.aliquota-ipi > 0 AND ordem-compra.codigo-ipi = NO THEN
                                        ASSIGN de-ipi    = de-preco-calc * ordem-compra.aliquota-ipi / 100
                                                   de-ipi-tot = de-ipi-tot + de-ipi.
                        END.
                        ELSE DO:
                                IF  ordem-compra.taxa-financ = NO THEN DO:
                                        RUN ccp/cc9020.p (INPUT  YES,
                                                                          INPUT  ordem-compra.cod-cond-pag,
                                                                          INPUT  ordem-compra.valor-taxa,
                                                                          INPUT  ordem-compra.nr-dias-taxa,
                                                                          INPUT  de-preco-tot-aux,
                                                                          OUTPUT de-preco-calc).

                                        ASSIGN de-enc = ROUND(de-preco-calc - de-preco-tot-aux,2).
                                END.
                                ELSE
                                        ASSIGN de-preco-calc = de-preco-tot-aux.

                                IF ordem-compra.aliquota-ipi > 0 AND ordem-compra.codigo-ipi = NO THEN
                                        ASSIGN de-ipi = de-preco-calc * ordem-compra.aliquota-ipi / 100
                                                   de-ipi-tot = de-ipi-tot + de-ipi.

                                IF ordem-compra.perc-descto > 0 THEN
                                        ASSIGN de-desc = ROUND((de-preco-calc + de-ipi) * ordem-compra.perc-descto / 100,2).
                        END.

                        /* de-preco-total  = de-preco-tot-aux + de-enc + de-ipi - de-desc */
                        ASSIGN de-preco-total  = de-preco-tot-aux + de-enc - de-desc
                                   de-total-pedido = de-total-pedido  + de-preco-total.
                END.

                FIND mensagem NO-LOCK
           WHERE mensagem.cod-mensagem = pedido-compr.cod-mensagem NO-ERROR.

                /*** Parcelas ***/
                FOR EACH  prazo-compra NO-LOCK
                        WHERE prazo-compra.numero-ordem =  ordem-compra.numero-ordem
                        AND   prazo-compra.situacao     <> 4
                        AND   prazo-compra.situacao     <> 6:

                        IF ordem-compra.taxa-financ THEN
                                ASSIGN c-tax-aux = "%"
                                           de-enc       = 0.
                        ELSE
                                ASSIGN c-tax-aux = "%".

                        ASSIGN de-preco-tot-aux = de-preco-conv * prazo-compra.qtd-sal-forn
                                   de-preco-calc        = 0
                                   de-desc                = 0
                                   de-enc                  = 0
                                   de-ipi                  = 0.

                        IF param-compra.ipi-sobre-preco = 2 THEN DO:
                                IF ordem-compra.perc-descto > 0 THEN
                                        ASSIGN de-desc = ROUND(de-preco-tot-aux
                                                                   * ordem-compra.perc-descto / 100,2).
                                IF ordem-compra.taxa-financ = NO THEN DO:
                                        ASSIGN de-enc = de-preco-tot-aux - de-desc.

                                        RUN ccp/cc9020.p (INPUT  YES,
                                                                          INPUT  ordem-compra.cod-cond-pag,
                                                                          INPUT  ordem-compra.valor-taxa,
                                                                          INPUT  ordem-compra.nr-dias-taxa,
                                                                          INPUT  de-enc,
                                                                          OUTPUT de-preco-calc).

                                        ASSIGN de-enc = ROUND(de-preco-calc - de-enc,2).
                                END.
                                ELSE
                                        ASSIGN de-preco-calc = de-preco-tot-aux - de-desc.

                                IF ordem-compra.aliquota-ipi > 0 AND ordem-compra.codigo-ipi = NO THEN
                                        ASSIGN de-ipi = de-preco-calc * ordem-compra.aliquota-ipi / 100.
                        END.
                        ELSE DO:
                                IF ordem-compra.taxa-financ = NO THEN DO:
                                        RUN ccp/cc9020.p (INPUT  YES,
                                                                          INPUT  ordem-compra.cod-cond-pag,
                                                                          INPUT  ordem-compra.valor-taxa,
                                                                          INPUT  ordem-compra.nr-dias-taxa,
                                                                          INPUT  de-preco-tot-aux,
                                                                          OUTPUT de-preco-calc).

                                        ASSIGN de-enc = ROUND(de-preco-calc - de-preco-tot-aux,2).
                                END.
                                ELSE
                                        ASSIGN de-preco-calc = de-preco-tot-aux.

                                IF ordem-compra.aliquota-ipi > 0 AND ordem-compra.codigo-ipi = NO THEN
                                        ASSIGN de-ipi = de-preco-calc * ordem-compra.aliquota-ipi / 100.

                                IF ordem-compra.perc-descto > 0 THEN
                                        ASSIGN de-desc = ROUND((de-preco-calc + de-ipi) * ordem-compra.perc-descto / 100,2).
                        END.

                        ASSIGN de-preco-total = de-preco-tot-aux + de-enc - de-desc.

                        IF l-ordem = NO THEN DO:

                                ASSIGN l-branco = YES.

                                IF AVAIL estabelec THEN DO:
                                        ASSIGN l-imprimiu = YES.

                                        IF de-ipi-tot > 0 THEN
                                                ASSIGN de-ipi1 = ROUND(de-ipi-tot,2).
                                        ELSE
                                                ASSIGN de-ipi1 = 0.
                                
                                        IF AVAIL tt-especifica THEN DO:
                                                ASSIGN tt-especifica.wk-perc-ipi = ordem-compra.aliquota-ipi
                                                           tt-especifica.wk-perc-icm = ordem-compra.aliquota-icm.
                                
                                                IF NOT ordem-compra.codigo-ipi THEN
                                                        ASSIGN tt-especifica.wk-val-ipi  = de-ipi1 .
                                        END.
                                END.
                                                                                 
                                /*** ImpressÆo dos Dados da Narrativa ***/ 
                                ASSIGN c-narrativa-it = "".
                                IF tt-param.l-narrativa-item  THEN DO:
                                        FIND FIRST narrativa WHERE narrativa.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
                                        IF AVAIL narrativa THEN
                                                ASSIGN c-narrativa-it = SUBSTRING(narrativa.descricao,1,LENGTH(narrativa.descricao)).
                                END.

                                ASSIGN c-narrativa-or = "".
                                IF tt-param.l-narrativa-ordem THEN
                                        ASSIGN c-narrativa-or = SUBSTRING(ordem-compra.narrativa,1,LENGTH(ordem-compra.narrativa)).
                        END.

                        ASSIGN v-seq = v-seq + 1.
                        CREATE tt-especifica.
                        ASSIGN tt-especifica.wk-pedido     = pedido-compr.num-pedido
                                   tt-especifica.wk-processo     = ordem-compra.nr-processo
                                   tt-especifica.wk-data-ped     = pedido-compr.data-pedido
                                   tt-especifica.wk-estab               = pedido-compr.end-cobran
                                   tt-especifica.wk-cod-emit     = pedido-compr.cod-emitente
                                   tt-especifica.wk-cod-comprado = ordem-compra.cod-comprado
                                   tt-especifica.wk-seq           = v-seq 
                                   tt-especifica.wk-ordem               = prazo-compra.numero-ordem
                                   tt-especifica.wk-ordem-serv   = ordem-compra.ordem-servic
                                   tt-especifica.wk-item                 = ordem-compra.it-codigo
                                   tt-especifica.wk-quant               = prazo-compra.qtd-sal-forn
                                   tt-especifica.wk-un             = cotacao-item.un
                                   tt-especifica.wk-contato       = cotacao-item.contato
                                   tt-especifica.wk-pa             = prazo-compra.parcela
                                   tt-especifica.wk-preco-unit   = de-preco-unit
                                   tt-especifica.wk-preco-total  = de-preco-total
                                   tt-especifica.wk-perc-des     = ordem-compra.perc-descto
                                   tt-especifica.wk-val-des       = de-desc
                                   tt-especifica.wk-end-ent       = pedido-compr.end-entrega
                                   tt-especifica.wk-cod-emit-ter = pedido-compr.cod-emit-terc
                                   tt-especifica.wk-entrega       = prazo-compra.data-entrega.

                                /*** Buscar o Aprovador ***/
                                ASSIGN l-r2 = (de-total-pedido > param-compra.limite[1]).
                                           l-r3 = (de-total-pedido > param-compra.limite[2]).

                                ASSIGN tt-especifica.wk-traco[1] = FILL("_",36)
                                           tt-especifica.wk-traco[2] = ""
                                           tt-especifica.wk-traco[3] = ""
                                           tt-especifica.wk-nome[1]  = ""
                                           tt-especifica.wk-nome[2]  = ""
                                           tt-especifica.wk-nome[3]  = ""
                                           tt-especifica.wk-cargo[1] = ""
                                           tt-especifica.wk-cargo[2] = ""
                                           tt-especifica.wk-cargo[3] = "".
                
                                IF  l-r2 = YES THEN
                                        ASSIGN tt-especifica.wk-traco[2] = FILL("_",36)
                                                   tt-especifica.wk-nome[2]  = (FILL(" ", INTEGER((36
                                                                                                          - LENGTH(param-compra.nome-ass[2])) /  2))
                                                                                                          + param-compra.nome-ass[2])
                                                   tt-especifica.wk-cargo[2] = (FILL(" ", INTEGER((36
                                                                                                          - LENGTH(param-compra.cargo-ass[2])) / 2))
                                                                                                          + param-compra.cargo-ass[2]).
                
                                IF l-r3 = YES THEN
                                        ASSIGN tt-especifica.wk-traco[3] = FILL("_",36)
                                                   tt-especifica.wk-nome[3]  = (FILL(" ", INTEGER((36
                                                                                                          - LENGTH(param-compra.nome-ass[3])) /  2))
                                                                                                          + param-compra.nome-ass[3])
                                                   tt-especifica.wk-cargo[3] = (FILL(" ", INTEGER((36
                                                                                                          - LENGTH(param-compra.cargo-ass[3])) / 2))
                                                                                                          + param-compra.cargo-ass[3]).
                                
                                ASSIGN tt-especifica.wk-nome[1]  = (FILL(" ", INTEGER((36
                                                                                                  - LENGTH(param-compra.nome-ass[1])) / 2))
                                                                                                  + param-compra.nome-ass[1])
                                           tt-especifica.wk-cargo[1] = (FILL(" ", INTEGER((36
                                                                                                  - LENGTH(param-compra.cargo-ass[1])) / 2))
                                                                                                  + param-compra.cargo-ass[1]).

                        IF v-cond-pagto = "" THEN DO:
                                ASSIGN v-cond-pagto = c-desc-var-1 + " " + c-desc-var-2.
                                REPEAT:
                                        l = INDEX(v-cond-pagto," 0").
                                        IF l > 0 THEN
                                           v-cond-pagto = SUBSTRING(v-cond-pagto,1,l - 1) + SUBSTRING(v-cond-pagto,l + 2).
                                        ELSE LEAVE.
                                END.
                        END.

                        ASSIGN tt-especifica.wk-cond-pagto = v-cond-pagto
                                   tt-especifica.wk-transport  = v-transp-nome
                                   tt-especifica.wk-trans-tel  = v-transp-tele
                                   tt-especifica.wk-moedaped   = v-moeda
                                   tt-especifica.wk-frete         = v-desc-frete.

                        IF c-narrativa-des <> "" THEN DO:
                                RUN PI-TIRAR-CARAC(INPUT-OUTPUT c-narrativa-des).
                                ASSIGN tt-especifica.wk-descricao = SUBSTRING(c-narrativa-des,1,LENGTH(c-narrativa-des)) + " ".
                        END.
                        
                        IF c-narrativa-it <> "" THEN DO:
                                RUN PI-TIRAR-CARAC(INPUT-OUTPUT c-narrativa-it).
                                ASSIGN tt-especifica.wk-descricao = tt-especifica.wk-descricao + SUBSTRING(c-narrativa-it,1,LENGTH(c-narrativa-it)) + " ".
                        END.
                        
                        IF c-narrativa-or <> "" THEN DO:
                                RUN PI-TIRAR-CARAC(INPUT-OUTPUT c-narrativa-or).
                                ASSIGN tt-especifica.wk-descricao = tt-especifica.wk-descricao + SUBSTRING(c-narrativa-or,1,LENGTH(c-narrativa-or)).                                             
                        END.

                        ASSIGN tt-especifica.wk-perc-ipi  = ordem-compra.aliquota-ipi
                                   tt-especifica.wk-perc-icms = ordem-compra.aliquota-icm.

                        IF NOT ordem-compra.codigo-ipi THEN
                                ASSIGN tt-especifica.wk-val-ipi  = ROUND(de-ipi,2) .


                        IF AVAIL mensagem THEN
                                ASSIGN tt-especifica.wk-coment = CAPS(mensagem.texto-mensag).
        
                        IF pedido-compr.comentarios <> "" THEN
                                ASSIGN tt-especifica.wk-coment = tt-especifica.wk-coment + CAPS(pedido-compr.comentarios).

                        ASSIGN l-ordem = YES.
                END.

                ASSIGN c-pe-aux = pedido-compr.num-pedido
                           l-ordem  = no.

                /*** Atualiza ordem ***/
                RUN ccp/cc0305b.p (input rowid(ordem-compra)).

                IF AVAIL contrato-for AND
                   ordem-compra.nr-contrato = contrato-for.nr-contrato AND
                   ordem-compra.sit-ordem-contrat = 1 THEN
                        ASSIGN ordem-compra.sit-ordem-contrat = 2.

        END.

        IF l-imprimiu THEN DO:
                
                RUN ccp/cc0305a.p(ROWID(pedido-compr)).

                /*** Multiplanta ***/
                &if DEFINED(bf_mat_contratos) &then /* EMS 202 */
                FOR EACH tt-erro:
                        DELETE tt-erro.
                END.

                RUN ccp/ccapi200.p (INPUT "Pedido-compr":U, INPUT ROWID(pedido-compr),
                                                        INPUT 2, INPUT-OUTPUT TABLE tt-erro).

                FIND FIRST tt-erro NO-LOCK NO-ERROR. 
                IF AVAIL tt-erro THEN
                        RUN cdp/cd0666.w (INPUT TABLE tt-erro).
                &else


                ASSIGN c-estabel-dest = pedido-compr.end-entrega
                           c-transacao  = "mat006".

                &endif
        END.
END.

/***** BLOCO DE IMPRESSAO *****************************************************/
DOS SILENT DEL VALUE(tt-param.dir-pedido + "arq-email-*.doc") NO-ERROR.
DOS SILENT DEL VALUE(tt-param.dir-pedido + "pedido*.doc") NO-ERROR.

RUN pi-acompanhar IN h-acomp(INPUT "Iniciando Gera‡Æo Pdf...").

FOR EACH tt-especifica NO-LOCK
BREAK BY tt-especifica.wk-pedido
          BY tt-especifica.wk-ordem 
          BY tt-especifica.wk-pa:

        RUN pi-acompanhar IN h-acomp(INPUT "Imprimindo Pedido:" + STRING(tt-especifica.wk-pedido)).
        

        IF FIRST-OF(tt-especifica.wk-pedido) THEN DO:
                ASSIGN i-pagina                 = 1
                           cont-itens             = 1
                           de-total-pedido       = 0
                           de-sub-total-pedido = 0
                           v-nr-pedido           = STRING(tt-especifica.wk-pedido)
                           v-dt-pedido           = STRING(tt-especifica.wk-data-ped,"99/99/9999")
                           v-nr-processo           = STRING(tt-especifica.wk-processo).

                /*** CABECALHO DO PEDIDO ***/
                /*** Empresa Faturamento ***/
                FIND FIRST b-estabel WHERE b-estabel.cod-estabel = tt-especifica.wk-estab NO-LOCK NO-ERROR.
                IF AVAIL b-estabel THEN
                        ASSIGN v-emp-razao       = b-estabel.nome
                                   v-emp-endereco1 = b-estabel.endereco
                                   v-emp-endereco2 = b-estabel.bairro + " - " + b-estabel.cidade + " - " + b-estabel.estado
                                   v-emp-cep       = STRING(STRING(b-estabel.cep),"99999-999")
                                   v-emp-insc     = b-estabel.ins-estadual
                                   v-emp-cgc       = IF param-global.formato-id-federal <> "" THEN
                                                                   STRING(b-estabel.cgc, param-global.formato-id-federal)
                                                                   ELSE b-estabel.cgc.

                /*** Comprador - Contato ***/
                ASSIGN de-tot-ipi = 0.
                FIND FIRST comprador WHERE comprador.cod-comprado = tt-especifica.wk-cod-comprado NO-LOCK NO-ERROR.
                IF AVAIL comprador THEN DO:
                        FIND FIRST usuar-mater NO-LOCK
                                 WHERE usuar-mater.cod-usuario = comprador.cod-compr NO-ERROR.
                        IF AVAIL usuar-mater THEN
                                ASSIGN v-emp-fone        = usuar-mater.telefone[1]
                                           v-emp-fax      = usuar-mater.telefax
                                           v-comp-e-mail  = usuar-mater.e-mail.

                        ASSIGN v-comp-nome = string(comprador.nome,"x(13)").
                END.

                /*** Fornecedor ***/
                FIND FIRST emitente WHERE emitente.cod-emit = tt-especifica.wk-cod-emit NO-LOCK NO-ERROR.
                IF AVAIL emitente THEN DO:
                        ASSIGN v-for-razao       = emitente.nome-emit
                                   v-for-endereco1 = emitente.endereco
                                   v-for-endereco2 = emitente.bairro + " - " + emitente.cidade + " - " + emitente.estado
                                   v-for-cep       = STRING(emitente.cep,"99999-999")                                                   
                                   v-for-fax       = emitente.telefax
                                   v-for-tel       = emitente.telefone[1]
                                   v-for-e-mail = emitente.e-mail
                                   v-for-insc     = emitente.ins-estadual
                                   v-for-contato   = string(tt-especifica.wk-contato,"x(13)").
                  
                        IF emitente.natureza = 1 THEN
                                ASSIGN v-for-cgc = IF param-global.formato-id-pessoal <> "" THEN
                                                                   STRING(emitente.cgc, param-global.formato-id-pessoal)
                                                                   ELSE emitente.cgc.
                        ELSE
                                ASSIGN v-for-cgc = IF param-global.formato-id-federal <> "" THEN
                                                                   STRING(emitente.cgc, param-global.formato-id-federal)
                                                                   ELSE emitente.cgc.
                END.
                ELSE
                        ASSIGN v-for-razao       = ""
                                   v-for-endereco1 = ""  
                                   v-for-endereco2 = ""
                                   v-for-cep       = ""
                                   v-for-cgc       = ""
                                   v-for-fax       = ""
                                   v-for-tel       = ""
                                   v-for-e-mail = ""
                                   v-for-insc     = ""
                                   v-for-contato   = string(tt-especifica.wk-contato,"x(13)").

                /*** RODAPE DO PEDIDO ***/
                ASSIGN v-cond-pagto  = tt-especifica.wk-cond-pagto
                           v-transp-nome = tt-especifica.wk-transport
                           v-transp-tele = tt-especifica.wk-trans-tel
                           v-moeda         = tt-especifica.wk-moedaped
                           v-desc-frete  = tt-especifica.wk-frete
                           v-tot-ipi     = "---------------"
                           v-tot-pedido  = "----------------"
                           v-sub-total   = "----------------".

                /*** Observa‡Æo ***/
                RUN PI-TIRAR-CARAC(INPUT-OUTPUT tt-especifica.wk-coment).
                RUN pi-print-editor (INPUT tt-especifica.wk-coment, INPUT 60).
                ASSIGN i-editor = 0.
                FOR EACH tt-editor NO-LOCK:
                        IF tt-editor.conteudo <> "" AND i-editor < 10 THEN
                                ASSIGN i-editor         = i-editor + 1
                                           v-obs[i-editor] = tt-editor.conteudo.
                END.

                /*** Endere‡o de Entrega ***/
                IF tt-especifica.wk-cod-emit-ter = 0 THEN DO:
                        FIND estabelec WHERE estabelec.cod-estabel = tt-especifica.wk-end-ent NO-LOCK NO-ERROR.
                        IF AVAIL estabelec THEN DO:
                                FIND FIRST emitente NO-LOCK
                                         WHERE emitente.cod-emit = estabelec.cod-emitente NO-ERROR.
                                IF AVAIL emitente THEN
                                        ASSIGN v-ent-nome         = estabelec.nome
                                                   v-ent-endereco1 = estabelec.endereco
                                                   v-ent-endereco2 = estabelec.bairro + " - " + estabelec.cidade + " - " + estabelec.estado
                                                   v-ent-cep       = STRING(STRING(estabelec.cep),"99999-999")
                                                   v-ent-tel       = emitente.telefone[1]
                                                   v-ent-fax       = emitente.telefax.
                        END.
                END.
                ELSE DO:
                        FIND FIRST emitente NO-LOCK
                                 WHERE emitente.cod-emit = tt-especifica.wk-cod-emit-ter NO-ERROR.
                        IF AVAIL emitente THEN
                                ASSIGN v-ent-nome         = emitente.nome-emit
                                           v-ent-endereco1 = emitente.endereco
                                           v-ent-endereco2 = emitente.bairro + " - " + emitente.cidade + " - " + emitente.estado
                                           v-ent-cep       = STRING(emitente.cep,"99999-999")                                                   
                                           v-ent-tel       = emitente.telefone[1]
                                           v-ent-fax       = emitente.telefax.
                END.

                /*** Assinatura ***/
                REPEAT i-editor = 1 TO 3:
                        ASSIGN v-traco[i-editor] = tt-especifica.wk-traco[i-editor]
                                   v-cargo[i-editor] = tt-especifica.wk-cargo[i-editor]
                                   v-nome[i-editor]  = tt-especifica.wk-nome[i-editor].
                END.

                /*** Pagina ***/
                DEFINE VARIABLE tot-pag AS INTEGER       NO-UNDO.
                ASSIGN tot-pag = tot-pag + i-pagina. 
                ASSIGN v-texto  = "Continua‡Æo do pedido ..."
                           v-pagina = "Pag: " + STRING(i-pagina,"99") + " de " +  STRING(tot-pag) .
        END.

        /*** Ordens do pedido - ITENS - Detalhe ***/
        ASSIGN de-sub-total-pedido = de-sub-total-pedido + tt-especifica.wk-preco-total
                   de-tot-ipi             = de-tot-ipi + (tt-especifica.wk-preco-total / 100) * tt-especifica.wk-perc-ipi
                   de-total-pedido       = de-total-pedido + tt-especifica.wk-preco-total +
                                                                 ((tt-especifica.wk-preco-total / 100) * tt-especifica.wk-perc-ipi). /*** Valor do IPI ***/

        /*** Quebrar Descricao do Item ***/
        FOR EACH tt-editor. DELETE tt-editor. END.
        RUN pi-print-editor (INPUT tt-especifica.wk-descricao, INPUT 40).

        FOR EACH tt-editor NO-LOCK
        BREAK BY tt-editor.linha:
                IF tt-editor.linha = 1 THEN
                   ASSIGN c01-oc[cont-itens]      = STRING(tt-especifica.wk-ordem,"zzzzz9,99")
                                  c02-item[cont-itens]  = STRING(tt-especifica.wk-item,"x(12)")
                                  c03-desc[cont-itens]  = STRING(tt-editor.conteudo,"x(40)")
                                  c04-qtde[cont-itens]  = STRING(tt-especifica.wk-quant,">>>,>>9.99")
                                  c05-unid[cont-itens]  = STRING(tt-especifica.wk-un,"x(4)")
                                  c06-vl-unid[cont-itens] = STRING(tt-especifica.wk-preco-unit,">>>,>>9.99")
                                  c07-vl-tot[cont-itens]  = STRING(tt-especifica.wk-preco-total,">>>>,>>9.99")
                                  c08-ipi[cont-itens]    = STRING(tt-especifica.wk-perc-ipi,">9.99")
                                  c09-icms[cont-itens]  = STRING(tt-especifica.wk-perc-icm,">9.99")
                                  c10-entrega[cont-itens] = STRING(tt-especifica.wk-entrega,"99/99/9999")
                                  c11-os[cont-itens]      = STRING(tt-especifica.wk-ordem-serv,">>>,>>9")
                                  cont-itens                      = cont-itens + 1.
                ELSE
                   ASSIGN c03-desc[cont-itens]  = STRING(tt-editor.conteudo,"x(40)")
                                  cont-itens                      = cont-itens + 1.

/*                      MESSAGE cont-itens " - " tt-editor.conteudo */
/*                              VIEW-AS alert-BOX INFO BUTTONS OK.        */
/*                                                                                                              */

                /*** Se imprimir todas as linhas disponiveis ele gera um outro documento ***/
                IF cont-itens > 17 THEN DO:
                        ASSIGN cont-itens = 1.

                        /*** Imprimir correto caso tem 17 linha exatamnte ***/
                        IF LAST(tt-editor.linha) AND LAST-OF(tt-especifica.wk-pedido) THEN
                                ASSIGN cont-itens = 2.
                        ELSE
                                RUN PI-GERAR-PEDIDO.
                           /* Desenha o formato do relatorio */
                                
                END.
        END.

        /*** DADOS DO RODAPE ***/
        IF FIRST-OF(tt-especifica.wk-pedido) THEN DO:
                ASSIGN v-tot-ipi        = STRING(de-tot-ipi,">>>>>>,>>9.99")
                           v-tot-pedido = STRING(de-total-pedido,">>>>>,>>>,>>9.99")
                           v-sub-total  = STRING(de-sub-total-pedido,">>>>>,>>>,>>9.99")
                           v-texto        = ""
                           v-pagina      = "Pag: " + STRING(i-pagina,"99") + " de " +  STRING(tot-pag) .
                MESSAGE tot-pag
                        VIEW-AS alert-BOX INFO BUTTONS OK.

                IF cont-itens > 1 THEN
                        {PDFInclude/pdf_inc.i} 
                        RUN PI-GERAR-PEDIDO.
                        /* Desenha o formato do relatorio */
                        
                   
                        
                /*** Enviar e-mail ***/
                IF tt-param.l-email THEN DO:
                        RUN PI-UNIAO-DOC-PDF.

                        FIND FIRST empresa NO-LOCK NO-ERROR.
                        ASSIGN c-remetente      = IF AVAIL empresa THEN empresa.e-mail ELSE "PedidoCompra@tecnowatt.com.br"
                                   c-destino-mail = v-for-e-mail
                                   c-assunto      = "Pedido de Compra"
                                   c-arq-email  = c-novo-arquivo.

                        RUN PI-ENVIAR-EMAIL.
                END.
        END.
END.


RUN pi-finalizar IN h-acomp.
{include\i-rpclo.i &STREAM="stream str-rp"}
{include/pi-edit.i}
