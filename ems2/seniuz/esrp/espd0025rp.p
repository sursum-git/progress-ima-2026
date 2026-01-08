/* Programa: ESPD0025RP.P
** Autor...: F bio Coelho Lanza
** Objetivo: Imprimir Estatistica de Separa‡Æo de Pedidos
** Data....: Abril/2006
** Observ..: Chamado pelo programa ESPD0025.W
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0025RP 2.04.00.000}

DEFINE temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       FIELD dt-corte         LIKE ped-item-res.dt-trans
       FIELD dt-implant-ini   LIKE ped-item-res.dt-trans
       FIELD dt-implant-fin   LIKE ped-item-res.dt-trans
       FIELD imp-param        AS   LOGICAL.

DEFINE temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defimi‡Æo de vari veis  */
def var h-acomp as handle no-undo.

DEF VAR de-vol00 AS DECIMAL EXTENT 4 FORMAT "->>,>>>,>>9.99".
DEF VAR de-vol10 AS DECIMAL EXTENT 4 FORMAT "->>,>>>,>>9.99".
DEF VAR de-vol20 AS DECIMAL EXTENT 4 FORMAT "->>,>>>,>>9.99".
DEF VAR de-vol30 AS DECIMAL EXTENT 4 FORMAT "->>,>>>,>>9.99".
DEF VAR de-vol40 AS DECIMAL EXTENT 4 FORMAT "->>,>>>,>>9.99".
DEF VAR de-vol50 AS DECIMAL EXTENT 4 FORMAT "->>,>>>,>>9.99".
DEF VAR de-vol60 AS DECIMAL EXTENT 4 FORMAT "->>,>>>,>>9.99".
DEF VAR de-vol70 AS DECIMAL EXTENT 4 FORMAT "->>,>>>,>>9.99".
DEF VAR de-vol80 AS DECIMAL EXTENT 4 FORMAT "->>,>>>,>>9.99".
DEF VAR de-vol90 AS DECIMAL EXTENT 4 FORMAT "->>,>>>,>>9.99".
DEF VAR de-vol99 AS DECIMAL EXTENT 4 FORMAT "->>,>>>,>>9.99".

DEF VAR i-hr-trans AS INT.
DEF VAR hr-ini-t1 AS INT.
DEF VAR hr-fim-t1 AS INT.
DEF VAR hr-ini-t2 AS INT.
DEF VAR hr-fim-t2 AS INT.
DEF VAR i-ct      AS INT.
DEF VAR i-ind     AS INT.

RUN esapi/cv-hora.p (INPUT "06:00:00", OUTPUT hr-ini-t1).
RUN esapi/cv-hora.p (INPUT "14:29:59", OUTPUT hr-fim-t1).
RUN esapi/cv-hora.p (INPUT "14:30:00", OUTPUT hr-ini-t2).
RUN esapi/cv-hora.p (INPUT "23:00:00", OUTPUT hr-fim-t2).

form
    "*--------- Parƒmetros/Sele‡Æo ----------*" SKIP
    tt-param.dt-corte         LABEL "Data de Corte..." SKIP
    tt-param.dt-implant-ini   LABEL "Data Implanta‡Æo" 
    "a" AT 30
    tt-param.dt-implant-fin   NO-LABELS SKIP
    with no-box side-labels width 132 STREAM-IO frame f-param.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defimi‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
FIND first param-global no-lock no-error.
FIND first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FATURAMENTO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Estat¡stica_de_Atendimento_de_Pedidos * r}
assign c-titulo-relat = trim(return-value).


view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH ped-item-res WHERE ped-item-res.dt-trans >= tt-param.dt-implant-ini
                        AND ped-item-res.dt-trans <= tt-param.dt-implant-fin
                        NO-LOCK USE-INDEX indice4:

   RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(ped-item-res.dt-trans,
                         "99/99/9999") + " Pedido: " + ped-item-res.nr-pedcli).
   
   RUN esapi/cv-hora.p (INPUT ped-item-res.hr-trans, OUTPUT i-hr-trans).

   IF i-hr-trans >= hr-ini-t1 AND i-hr-trans <= hr-fim-t1 THEN
      ASSIGN i-ind = 1.
   ELSE 
      ASSIGN i-ind = 3.

   IF ped-item-res.volume-fim < 10000 THEN DO:
      ASSIGN de-vol00[i-ind + 1] = de-vol00[i-ind + 1] + ped-item-res.qt-pedida.
      IF ped-item-res.dt-trans = tt-param.dt-corte THEN
         ASSIGN de-vol00[i-ind] = de-vol00[i-ind] + ped-item-res.qt-pedida.
   END.
   ELSE
   IF ped-item-res.volume-fim  < 20000 THEN DO:
      ASSIGN de-vol10[i-ind + 1] = de-vol10[i-ind + 1] + ped-item-res.qt-pedida.
      IF ped-item-res.dt-trans = tt-param.dt-corte THEN
         ASSIGN de-vol10[i-ind] = de-vol10[i-ind] + ped-item-res.qt-pedida.
   END.
   ELSE
   IF ped-item-res.volume-fim < 30000 THEN DO:
      ASSIGN de-vol20[i-ind + 1] = de-vol20[i-ind + 1] + ped-item-res.qt-pedida.
      IF ped-item-res.dt-trans = tt-param.dt-corte THEN
         ASSIGN de-vol20[i-ind] = de-vol20[i-ind] + ped-item-res.qt-pedida.
   END.
   ELSE
   IF ped-item-res.volume-fim < 40000 THEN DO:
      ASSIGN de-vol30[i-ind + 1] = de-vol30[i-ind + 1] + ped-item-res.qt-pedida.
      IF ped-item-res.dt-trans = tt-param.dt-corte THEN
         ASSIGN de-vol30[i-ind] = de-vol30[i-ind] + ped-item-res.qt-pedida.
   END.
   ELSE
   IF ped-item-res.volume-fim  < 50000 THEN DO:
      ASSIGN de-vol40[i-ind + 1] = de-vol40[i-ind + 1] + ped-item-res.qt-pedida.
      IF ped-item-res.dt-trans = tt-param.dt-corte THEN
         ASSIGN de-vol40[i-ind] = de-vol40[i-ind] + ped-item-res.qt-pedida.
   END.
   ELSE 
   IF ped-item-res.volume-fim < 60000 THEN DO:
      ASSIGN de-vol50[i-ind + 1] = de-vol50[i-ind + 1] + ped-item-res.qt-pedida.
      IF ped-item-res.dt-trans = tt-param.dt-corte THEN
         ASSIGN de-vol50[i-ind] = de-vol50[i-ind] + ped-item-res.qt-pedida.
   END.
   ELSE
   IF ped-item-res.volume-fim < 70000 THEN DO:
      ASSIGN de-vol60[i-ind + 1] = de-vol60[i-ind + 1] + ped-item-res.qt-pedida.
      IF ped-item-res.dt-trans = tt-param.dt-corte THEN
         ASSIGN de-vol60[i-ind] = de-vol60[i-ind] + ped-item-res.qt-pedida.
   END.
   ELSE 
   IF ped-item-res.volume-fim < 80000 THEN DO:
      ASSIGN de-vol70[i-ind + 1] = de-vol70[i-ind + 1] + ped-item-res.qt-pedida.
      IF ped-item-res.dt-trans = tt-param.dt-corte THEN
         ASSIGN de-vol70[i-ind] = de-vol70[i-ind] + ped-item-res.qt-pedida.
   END.
   ELSE
   IF ped-item-res.volume-fim < 90000 THEN DO:
      ASSIGN de-vol80[i-ind + 1] = de-vol80[i-ind + 1] + ped-item-res.qt-pedida.
      IF ped-item-res.dt-trans = tt-param.dt-corte THEN
         ASSIGN de-vol80[i-ind] = de-vol80[i-ind] + ped-item-res.qt-pedida.
   END.
   ELSE
   IF ped-item-res.volume-fim < 100000 THEN DO:
      ASSIGN de-vol90[i-ind + 1] = de-vol90[i-ind + 1] + ped-item-res.qt-pedida.
      IF ped-item-res.dt-trans = tt-param.dt-corte THEN
         ASSIGN de-vol90[i-ind] = de-vol90[i-ind] + ped-item-res.qt-pedida.
   END.
   ELSE
   IF ped-item-res.volume-fim < 110000 THEN DO:
      ASSIGN de-vol99[i-ind + 1] = de-vol99[i-ind + 1] + ped-item-res.qt-pedida.
      IF ped-item-res.dt-trans = tt-param.dt-corte THEN
         ASSIGN de-vol99[i-ind] = de-vol99[i-ind] + ped-item-res.qt-pedida.
   END.
END.

PUT "Volumes"         AT   1
    tt-param.dt-corte AT  21
    "T1"              AT  33
    "Acumulado"       AT  42 
    tt-param.dt-corte AT  53
    "T2"              AT  65
    "Acumulado"       AT  74
    "Ac."             AT  85
    tt-param.dt-corte AT  89
    "Acumulado Total  "   AT 102
    SKIP
    FILL("-",18)  AT   1 FORMAT "x(18)"
    FILL("-",14)  AT  21 FORMAT "x(14)"
    FILL("-",14)  AT  37 FORMAT "x(14)"
    FILL("-",14)  AT  53 FORMAT "x(14)"
    FILL("-",14)  AT  69 FORMAT "x(14)"
    FILL("-",14)  AT  85 FORMAT "x(14)"
    FILL("-",15)  AT 102 FORMAT "x(15)"
    SKIP.

DO i-ct = 1 TO 11.
   CASE i-ct.
       WHEN  1 THEN
           RUN pi-imprime (" 00.001 a   9.999",de-vol00[1],de-vol00[2],de-vol00[3],de-vol00[4]).
       WHEN  2 THEN 
           RUN pi-imprime (" 10.001 a  19.999",de-vol10[1],de-vol10[2],de-vol10[3],de-vol10[4]).
       WHEN  3 THEN
           RUN pi-imprime (" 20.001 a  29.999",de-vol20[1],de-vol20[2],de-vol20[3],de-vol20[4]).
       WHEN  4 THEN
           RUN pi-imprime (" 30.001 a  39.999",de-vol30[1],de-vol30[2],de-vol30[3],de-vol30[4]).
       WHEN  5 THEN
           RUN pi-imprime (" 40.001 a  49.999",de-vol40[1],de-vol40[2],de-vol40[3],de-vol40[4]).
       WHEN  6 THEN
           RUN pi-imprime (" 50.001 a  59.999",de-vol50[1],de-vol50[2],de-vol50[3],de-vol50[4]).
       WHEN  7 THEN
           RUN pi-imprime (" 60.001 a  69.999",de-vol60[1],de-vol60[2],de-vol60[3],de-vol60[4]).
       WHEN  8 THEN
           RUN pi-imprime (" 70.001 a  79.999",de-vol70[1],de-vol70[2],de-vol70[3],de-vol70[4]).
       WHEN  9 THEN
           RUN pi-imprime (" 80.001 a  89.999",de-vol80[1],de-vol80[2],de-vol80[3],de-vol80[4]).
       WHEN 10 THEN
           RUN pi-imprime (" 90.001 a  99.999",de-vol90[1],de-vol90[2],de-vol90[3],de-vol90[4]).
       WHEN 11 THEN
           RUN pi-imprime ("100.001 a 109.999",de-vol90[1],de-vol90[2],de-vol90[3],de-vol90[4]).
   END CASE.
END.

PUT SKIP(1)
    "TOTAIS........." AT 01
    de-vol00[1] + de-vol10[1] + de-vol20[1] + 
    de-vol30[1] + de-vol40[1] + de-vol50[1] + de-vol60[1] +
    de-vol70[1] + de-vol80[1] + de-vol90[1] + de-vol99[1] AT 21 FORMAT "->>,>>>,>>9.99".
    
PUT de-vol00[2] + de-vol10[2] + de-vol20[2] + 
    de-vol30[2] + de-vol40[2] + de-vol50[2] + de-vol60[2] +
    de-vol70[2] + de-vol80[2] + de-vol90[2] + de-vol99[2] AT 37 FORMAT "->>,>>>,>>9.99".
                                                                       
PUT de-vol00[3] + de-vol10[3] + de-vol20[3] + 
    de-vol30[3] + de-vol40[3] + de-vol50[3] + de-vol60[3] +
    de-vol70[3] + de-vol80[3] + de-vol90[3] + de-vol99[3] AT 53 FORMAT "->>,>>>,>>9.99".
    
PUT de-vol00[4] + de-vol10[4] + de-vol20[4] + 
    de-vol30[4] + de-vol40[4] + de-vol50[4] + de-vol60[4] +
    de-vol70[4] + de-vol80[4] + de-vol90[4] + de-vol99[4] AT 69 FORMAT "->>,>>>,>>9.99".
  
PUT de-vol00[1] + de-vol10[1] + de-vol20[1] + 
    de-vol30[1] + de-vol40[1] + de-vol50[1] +
    de-vol60[1] + de-vol70[1] + de-vol80[1] + de-vol90[ 1] + de-vol99[ 1] +
    de-vol00[3] + de-vol10[3] + de-vol20[3] + 
    de-vol30[3] + de-vol40[3] + de-vol50[3] + de-vol60[3] +
    de-vol70[3] + de-vol80[3] + de-vol90[3] + de-vol99[3] AT 85 FORMAT "->>,>>>,>>9.99".
   
PUT de-vol00[2] + de-vol10[2] + de-vol20[2] + 
    de-vol30[2] + de-vol40[2] + de-vol50[2] +
    de-vol60[2] + de-vol70[2] + de-vol80[2] + de-vol90[2] + de-vol99[2] +
    de-vol00[4] + de-vol10[4] + de-vol20[4] + 
    de-vol30[4] + de-vol40[4] + de-vol50[4] + de-vol60[4] +
    de-vol70[4] + de-vol80[4] + de-vol90[4] + de-vol99[4] AT 103 FORMAT "->>,>>>,>>9.99"  
    FILL("-",116)  AT 01 FORMAT "x(116)".

/* Imprime Parametros da Pedida do Relatorio */

IF tt-param.imp-param THEN
PUT SKIP(3).
display tt-param.dt-corte
        tt-param.dt-implant-ini
        tt-param.dt-implant-fin
        with frame f-param.


/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

/* ************* PROCEDIMENTOS ************* */

PROCEDURE pi-imprime.
    DEF INPUT PARAMETER Faixa AS CHAR FORMAT "X(17)".
    DEF INPUT PARAMETER Valor1 AS DEC.
    DEF INPUT PARAMETER Valor2 AS DEC.
    DEF INPUT PARAMETER Valor3 AS DEC.
    DEF INPUT PARAMETER Valor4 AS DEC.

    PUT Faixa           AT   1 
        Valor1          AT  21 FORMAT "->>,>>>,>>9.99"
        Valor2          AT  37 FORMAT "->>,>>>,>>9.99"
        Valor3          AT  53 FORMAT "->>,>>>,>>9.99"
        Valor4          AT  69 FORMAT "->>,>>>,>>9.99"
        Valor1 + Valor3 AT  85 FORMAT "->>,>>>,>>9.99"
        Valor2 + Valor4 AT 103 FORMAT "->>,>>>,>>9.99"
        SKIP(1).
END PROCEDURE.
