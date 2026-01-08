/* Programa: ESSP0093
** Sistema.: EMS da Datasul
** Modulo..: Espec°ficos
** Objetivo: Relatorio de Itens que receberam C¢digo EAN e DV para emiss∆o
**           da etiqueta de C¢digo de Barras.
** Autor...: Gilvando de Souza Araujo
** Data....: Maráo/2005
** Obs.....: Programa especifico da TEAR TEXTIL IND COM LTDA
**
*/

/* include de controle de vers∆o */
{include/i-prgvrs.i ESSP0093RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       field desc-classifica  as char format "x(40)"
       FIELD it-codigo        LIKE ITEM.it-codigo
       FIELD cod-refer-ini    like ref-item.cod-refer
       FIELD cod-refer-fin    like ref-item.cod-refer
       FIELD impr-param       AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

/* include padr∆o para impress∆o de campos editores em relat¢rio  */
{include/tt-edit.i}

/* definiá∆o de vari†veis  */
def var h-acomp as handle no-undo.

def var i-list-fat  as int extent 13 init[6,5,4,3,2,9,8,7,6,5,4,3,2].
def var i-digito    as int format "9" label "DV".
def var i-cont      as int.
def var i-soma      as int.
def var i-ean       as int.
def var i-cont1     as int.
def var c-item-ref  as char format "x(13)".

DEFINE BUFFER b-ref-item-ext FOR ref-item-ext.

form 
    "*------ ParÉmetros/Seleá∆o ------*" SKIP
    tt-param.it-codigo      label "Item........" SKIP
    tt-param.cod-refer-ini  LABEL "Referencia.."
    "A"  AT 24
    tt-param.cod-refer-fin  NO-LABEL SKIP
    i-cont                  LABEL "Atualizaá‰es"
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    ref-item-ext.it-codigo
    item.desc-item
    ref-item-ext.cod-ean
    ref-item-ext.dv
    with no-box 55 down width 132 STREAM-IO frame f-detalhe.

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i}

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECIFICOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Itens_da_Geraá∆o_de_C¢digo_EAN/D°gito_Verificador * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FIND ITEM WHERE ITEM.it-codigo = tt-param.it-codigo NO-LOCK NO-ERROR.
IF AVAIL ITEM THEN DO:
   IF ITEM.tipo-con-est <> 4 THEN DO: /* N∆o usa referància */
      
      run pi-acompanhar in h-acomp (input "Item: " + string(item.it-codigo)).

      find ref-item-ext where
           ref-item-ext.it-codigo = item.it-codigo and
           ref-item-ext.cod-refer = "" no-error.

      if avail ref-item-ext then
         if int(substr(ref-item-ext.cod-ean,8,5)) <> 0 then next.

      if not avail ref-item-ext then do:
         create ref-item-ext.
         assign ref-item-ext.it-codigo = item.it-codigo
                ref-item-ext.cod-refer = "".
      end.

      /* Calculo do Digito verificador do item */
      assign i-soma = 0.
      do i-cont = 1 to 13:
         assign c-item-ref = item.it-codigo + fill(" ",7).

         if  substr(c-item-ref,i-cont,1) >= "0" 
         and substr(c-item-ref,i-cont,1) <= "9" then
              assign i-soma = i-soma + int(substr(c-item-ref,i-cont,1)) *
                              i-list-fat[i-cont].
      end.
      assign i-digito = 11 - (i-soma MODULO 11).
      if i-digito > 9 then
         assign i-digito = 0.
         
      FIND LAST b-ref-item-ext USE-INDEX indice2 NO-LOCK NO-ERROR.
      IF AVAIL b-ref-item-ext THEN
         ASSIGN i-ean = INT(SUBSTR(b-ref-item-ext.cod-ean,8,5)) + 1.
      ELSE
         ASSIGN i-ean = 1.

      assign ref-item-ext.cod-ean = string("7890413") +
                                    string(i-ean,"99999")
             ref-item-ext.dv      = string(i-digito,"9").

      DISPLAY ref-item-ext.it-codigo
              ITEM.desc-item
              ref-item-ext.cod-refer
              ref-item-ext.cod-ean
              ref-item-ext.dv
              WITH FRAME f-detalhe.
      DOWN WITH FRAME f-detalhe.
   END.

   ELSE DO:  /* Usa referància */
      for each ref-item where ref-item.it-codigo  = tt-param.it-codigo
                          and ref-item.cod-refer >= tt-param.cod-refer-ini
                          and ref-item.cod-refer <= tt-param.cod-refer-fin 
                        NO-LOCK:

          run pi-acompanhar in h-acomp (input "Item: " + string(ref-item.it-codigo) + " " +
                                              "Refer: " + STRING(ref-item.cod-refer)).

          find ref-item-ext where
               ref-item-ext.it-codigo = ref-item.it-codigo and
               ref-item-ext.cod-refer = ref-item.cod-refer no-error.

          if avail ref-item-ext then
             if int(substr(ref-item-ext.cod-ean,8,5)) <> 0 then next.

          if not avail ref-item-ext then do:
             create ref-item-ext.
             assign ref-item-ext.it-codigo = ref-item.it-codigo
                    ref-item-ext.cod-refer = ref-item.cod-refer.
          end.

          /* Calculo do Digito verificador do item */
          assign i-soma = 0.
          do i-cont = 1 to 13:
             assign c-item-ref = ref-item.it-codigo + ref-item.cod-refer.

             if  substr(c-item-ref,i-cont,1) >= "0" 
             and substr(c-item-ref,i-cont,1) <= "9" then
                  assign i-soma = i-soma + int(substr(c-item-ref,i-cont,1)) *
                                  i-list-fat[i-cont].
           end.
           assign i-digito = 11 - (i-soma MODULO 11).
           if i-digito > 9 then
              assign i-digito = 0.
           
           FIND LAST b-ref-item-ext USE-INDEX indice2 NO-LOCK NO-ERROR.
           IF AVAIL b-ref-item-ext THEN
              ASSIGN i-ean = INT(SUBSTR(b-ref-item-ext.cod-ean,8,5)) + 1.
           ELSE
              ASSIGN i-ean = 1.

           assign ref-item-ext.cod-ean = string("7890413") +
                                         string(i-ean,"99999")
                  ref-item-ext.dv      = string(i-digito,"9").

           DISPLAY ref-item-ext.it-codigo
                   ITEM.desc-item
                   ref-item-ext.cod-refer
                   ref-item-ext.cod-ean
                   ref-item-ext.dv
                   WITH FRAME f-detalhe.
           DOWN WITH FRAME f-detalhe.
      end.
   END.
END.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.it-codigo
           tt-param.cod-refer-ini
           tt-param.cod-refer-fin
           i-cont
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

