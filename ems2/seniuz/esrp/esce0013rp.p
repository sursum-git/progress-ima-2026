/* Programa: ESCE0013.P
** Sistema.: EMS da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Listar o catalogo de Itens x Referˆncias.
** Autor...: Gilvando de Souza Araujo - Abril/2005.
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
** Auterado: Fabio Coelho Lanza - Agosto/2010
**           Selecao de Codigo Obsoleto estavam imprimindo codigos nÆo selecionados
**           NÆ impressÆo dos dados da tabela REFERENCIA-EXT somente se existir.
*/ 

/* include de controle de versÆo */
{include/i-prgvrs.i ESCE0013RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       field desc-classifica  as char format "x(40)"
       FIELD ini-ge-codigo     LIKE ITEM.ge-codigo
       FIELD fin-ge-codigo     LIKE item.ge-codigo
       FIELD ini-fm-codigo     LIKE item.fm-codigo
       FIELD fin-fm-codigo     LIKE item.fm-codigo 
       FIELD ini-it-codigo     LIKE item.it-codigo
       FIELD fin-it-codigo     LIKE item.it-codigo
       FIELD ini-cod-refer     LIKE ref-item.cod-refer
       FIELD fin-cod-refer     LIKE ref-item.cod-refer
       FIELD ini-acabamento    AS CHAR FORMAT "x(2)"
       FIELD fin-acabamento    AS CHAR FORMAT "x(2)"
       FIELD ini-des-cor       AS CHAR FORMAT "x(4)"
       FIELD fin-des-cor       AS CHAR FORMAT "x(4)"
       FIELD ini-cod-obsoleto  AS CHAR FORMAT "x(1)"
       FIELD fin-cod-obsoleto  AS CHAR FORMAT "x(1)"
       FIELD opc-acabado       AS INT
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.
DEF VAR c-descricao AS CHAR FORMAT "x(36)".

form
    "*-------------- Parƒmetros/Sele‡Æo --------------*" SKIP
    tt-param.ini-ge-codigo    LABEL "Grupo Estoque" AT 1
    "a"  AT 33                
    tt-param.fin-ge-codigo    NO-LABELS
    tt-param.ini-fm-codigo    LABEL "Familia......" AT 1
    "a"  AT 33                
    tt-param.fin-fm-codigo    NO-LABELS
    tt-param.ini-it-codigo    LABEL "Item........." AT 1
    "a"  AT 33                
    tt-param.fin-it-codigo    NO-LABELS
    tt-param.ini-cod-refer    LABEL "Referˆncia..." AT 1
    "a"  AT 33
    tt-param.fin-cod-refer    NO-LABELS
    tt-param.ini-acabamento   LABEL "Acabamento..." AT 1
    "a"  AT 33                                          
    tt-param.fin-acabamento   NO-LABELS                 
    tt-param.ini-des-cor      LABEL "Desenho/Cor.." AT 1
    "a"  AT 33
    tt-param.fin-des-cor      NO-LABELS
    tt-param.ini-cod-obsoleto LABEL "Cod.Obsoleto." AT 1
    "a"  AT 33
    tt-param.fin-cod-obsoleto NO-LABELS
    tt-param.opc-acabado      LABEL "Op‡Æo Acabam." AT 1
    tt-param.desc-classifica  LABEL "Classificacao" AT 1
    with no-box side-labels width 132 stream-io frame f-param.

form
    item.it-codigo                LABEL "Codigo"     FORMAT "x(6)"   
    c-descricao                   LABEL "Descricao"
    ref-item.cod-refer            LABEL "Referenc."  FORMAT "XX XXXX X"
    referencia.descricao          LABEL "Des"  FORMAT "x(3)"
    item.un                       LABEL "Un"
    item.ge-codigo                LABEL "GE"
    item.fm-codigo                LABEL "Familia"    FORMAT "x(7)"
    referencia-ext.cod-obsoleto   LABEL "Ob"
    referencia-ext.colecao        LABEL "Cole‡Æo"
    referencia-ext.cod-fundo      LABEL "Fund"
    referencia-ext.cor            LABEL "Cor"
    referencia-ext.estilo-desenho LABEL "Estilo Des" FORMAT "x(11)"
    WITH NO-BOX NO-LABEL 55 down width 134 STREAM-IO frame f-detalhe1.

form
    ref-item.cod-refer            LABEL "Referenc." FORMAT "XX XXXX X"
    referencia.descricao          LABEL "Des"  FORMAT "x(3)"
    item.it-codigo                LABEL "Codigo"        
    c-descricao                   LABEL "Descricao"
    item.un                       LABEL "Un"
    item.ge-codigo                LABEL "GE"
    item.fm-codigo                LABEL "Familia"
    referencia-ext.cod-obsoleto   LABEL "Ob"
    referencia-ext.colecao        LABEL "Cole‡Æo"
    referencia-ext.cod-fundo      LABEL "Fund"
    referencia-ext.cor            LABEL "Cor"
    referencia-ext.estilo-desenho LABEL "Estilo Des" FORMAT "x(11)"
    WITH NO-BOX NO-LABEL 55 down width 134 STREAM-IO frame f-detalhe2.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i MATERIAIS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Cat logo_de_Itens_x_Referˆncias * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH item  /*USE-INDEX grupo*/ WHERE
         item.ge-codigo >= tt-param.ini-ge-codigo AND
         item.ge-codigo <= tt-param.fin-ge-codigo AND
         item.it-codigo >= tt-param.ini-it-codigo AND
         item.it-codigo <= tt-param.fin-it-codigo AND 
         item.fm-codigo >= tt-param.ini-fm-codigo AND
         item.fm-codigo <= tt-param.fin-fm-codigo 
/*          ((SUBSTR(item.it-codigo,6,1) = "0" AND tt-param.opc-acab = 1) OR  */
/*           (SUBSTR(item.it-codigo,6,1) > "0" AND tt-param.opc-acab = 2) OR  */
/*                                                (tt-param.opc-acab = 3))    */
         NO-LOCK,
    EACH ref-item WHERE
         ref-item.it-codigo =  item.it-codigo          AND  
         ref-item.cod-refer >= tt-param.ini-cod-refer  AND 
         ref-item.cod-refer <= tt-param.fin-cod-refer  AND 
         ((SUBSTR(ref-item.cod-refer,7,1) = "0" AND tt-param.opc-acab = 1) OR
          (SUBSTR(ref-item.cod-refer,7,1) > "0" AND tt-param.opc-acab = 2) OR
                                                   (tt-param.opc-acab = 3)) AND
         SUBSTR(ref-item.cod-refer,1,2) >= tt-param.ini-acabamento AND 
         SUBSTR(ref-item.cod-refer,1,2) <= tt-param.fin-acabamento AND
         SUBSTR(ref-item.cod-refer,3,5) >= tt-param.ini-des-cor    AND
         SUBSTR(ref-item.cod-refer,3,5) <= tt-param.fin-des-cor    
         NO-LOCK,
    EACH ref-item-ext WHERE
         ref-item-ext.it-codigo    =  ref-item.it-codigo        AND
         ref-item-ext.cod-refer    =  ref-item.cod-refer        AND
         ref-item-ext.cod-obsoleto >= tt-param.ini-cod-obsoleto AND 
         ref-item-ext.cod-obsoleto <= tt-param.fin-cod-obsoleto
         NO-LOCK
    BY IF tt-param.classifica = 1 THEN ref-item.it-codigo +
                                       ref-item.cod-refer
                                  ELSE ref-item.cod-refer +
                                       ref-item.it-codigo:
    
    RUN pi-acompanhar IN h-acomp (INPUT "Item: " + STRING(item.it-codigo)).

    ASSIGN c-descricao = item.descricao-1 + item.descricao-2.
    
    FIND referencia OF ref-item NO-LOCK.
    FIND referencia-ext WHERE
         referencia-ext.cod-ref = referencia.cod-refer NO-LOCK NO-ERROR.

    IF tt-param.classifica = 1 THEN DO: /* Item/Referˆncia */
       DISPLAY item.it-codigo
               c-descricao
               ref-item.cod-refer
               referencia.descricao
               item.un
               item.ge-codigo
               item.fm-codigo
               ref-item-ext.cod-obsoleto @ referencia-ext.cod-obsoleto 
               referencia-ext.colecao        WHEN AVAIL referencia-ext
               referencia-ext.cod-fundo      WHEN AVAIL referencia-ext
               referencia-ext.cor            WHEN AVAIL referencia-ext
               referencia-ext.estilo-desenho WHEN AVAIL referencia-ext
               WITH FRAME f-detalhe1.
       DOWN WITH FRAME f-detalhe1.
    END.

    IF tt-param.classifica = 2 THEN DO: /* Referˆncia/Item */
       DISPLAY item.it-codigo
               c-descricao
               ref-item.cod-refer
               referencia.descricao
               item.un
               item.ge-codigo
               item.fm-codigo
               ref-item-ext.cod-obsoleto @ referencia-ext.cod-obsoleto 
               referencia-ext.colecao        WHEN AVAIL referencia-ext
               referencia-ext.cod-fundo      WHEN AVAIL referencia-ext
               referencia-ext.cor            WHEN AVAIL referencia-ext
               referencia-ext.estilo-desenho WHEN AVAIL referencia-ext
               WITH FRAME f-detalhe2.
       DOWN WITH FRAME f-detalhe2.
    END.
end.

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.ini-ge-codigo                          
           tt-param.fin-ge-codigo                          
           tt-param.ini-fm-codigo                          
           tt-param.fin-fm-codigo                          
           tt-param.ini-it-codigo                          
           tt-param.fin-it-codigo                          
           tt-param.ini-cod-refer                          
           tt-param.fin-cod-refer                          
           tt-param.ini-acabamento                         
           tt-param.fin-acabamento                         
           tt-param.ini-des-cor                            
           tt-param.fin-des-cor   
           tt-param.ini-cod-obsoleto  
           tt-param.fin-cod-obsoleto  
           tt-param.desc-classifica                        
           tt-param.opc-acabado                            
           WITH FRAME f-param.                             
END.                                                       
                                                           
/* fechamento do output do relat¢rio  */                 
{include/i-rpclo.i}                                      
run pi-finalizar in h-acomp.                             
return "OK":U.                                           
                                                         
                                                         
