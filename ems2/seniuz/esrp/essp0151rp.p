/* Programa: ESSP0151.W
** Modulo..: Controle de Expedi‡Æo
** Objetivo: Listar as Pesagem de Ve¡culos na Balan‡a
** Autor...: Fábio Coelho Lanza - Mar‡o/2007
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0151RP 2.04.00.000}

define temp-table tt-param   no-undo
    field destino            as integer
    field arquivo            as char format "x(35)"
    field usuario            as char format "x(12)"
    field data-exec          as date
    field hora-exec          as integer
    field classifica         as integer
    field desc-classifica    as char format "x(40)"
    field nome-transp-ini    LIKE mp-entr-cam.nome-transp
    field nome-transp-fin    LIKE mp-entr-cam.nome-transp
    field dt-entrada-ini     LIKE mp-entr-cam.dt-entrada 
    field dt-entrada-fin     LIKE mp-entr-cam.dt-entrada 
    field placa-ini          LIKE mp-entr-cam.placa 
    field placa-fin          LIKE mp-entr-cam.placa 
    field tipo-mov-ini       LIKE mp-entr-cam.tipo-mov   
    field tipo-mov-fin       LIKE mp-entr-cam.tipo-mov   
    field imp-param          AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}


/* defini‡Æo de vari veis  */
DEF VAR h-acomp      AS HANDLE NO-UNDO.
DEF VAR c-situacao   AS CHAR FORMAT "x(15)".
DEF VAR c-hr-entr    AS CHAR FORMAT "x(5)".
DEF VAR c-hr-sai     AS CHAR FORMAT "x(5)".
DEF VAR de-diferenca AS DEC  FORMAT "->>>,>>9.99".

form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*" SKIP
    tt-param.nome-transp-ini LABEL  "Transportadora"
    "A"  AT 40
    tt-param.nome-transp-fin NO-LABELS SKIP
    tt-param.dt-entrada-ini  LABEL  "Data Entrada.."
    "A"  AT 40
    tt-param.dt-entrada-fin  NO-LABELS SKIP
    tt-param.tipo-mov-ini    LABEL  "Tipo Movimento"
    "A"  AT 40
    tt-param.tipo-mov-fin    NO-LABELS SKIP
    WITH FRAME f-param SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.

form
    mp-entr-cam.nome-transp                         LABEL "Transportadora"
    c-situacao                                      LABEL "Movto"
    mp-entr-cam.placa        FORMAT "XXX-9999"      LABEL "Placa" 
    mp-entr-cam.dt-entrada   FORMAT "99/99/99"      LABEL "Dt.Ent"
    c-hr-entr                                       LABEL "H.Ent"
    mp-entr-cam.dt-saida     FORMAT "99/99/99"      LABEL "Dt.Sai"
    c-hr-sai                                        LABEL "H.Sai"
    mp-entr-cam.peso-bruto   FORMAT "->>>,>>9.9"    LABEL "Peso Bruto"
    mp-entr-cam.peso-tara    FORMAT "->>>,>>9.9"    LABEL "Peso Tara" 
    mp-entr-cam.peso-liquido FORMAT "->>>,>>9.9"    lABEL "Peso Balan‡a"
    mp-entr-cam.peso-nf      FORMAT "->>>,>>9.9"    LABEL "Peso NF(s)"
    de-diferenca                                    LABEL "Diferen‡a"
    with no-box NO-LABEL 55 down WIDTH 132 STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECÖFICOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Pesagem_de_Veiculos * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
 
FOR EACH mp-entr-cam WHERE mp-entr-cam.nome-transp >= tt-param.nome-transp-ini  AND  
                           mp-entr-cam.nome-transp <= tt-param.nome-transp-fin  AND 
                           mp-entr-cam.dt-entrada  >= tt-param.dt-entrada-ini   AND           
                           mp-entr-cam.dt-entrada  <= tt-param.dt-entrada-fin   AND 
                           mp-entr-cam.placa       >= tt-param.placa-ini        AND
                           mp-entr-cam.placa       <= tt-param.placa-fin        AND 
                           mp-entr-cam.tipo-mov    >= tt-param.tipo-mov-ini     AND
                           mp-entr-cam.tipo-mov    <= tt-param.tipo-mov-fin  
                           NO-LOCK
                           BREAK BY mp-entr-cam.nome-transp
                                 BY mp-entr-cam.dt-entrada.

    {esinc/i-dsrb.i mp-entr-cam.tipo-mov mp-entr-cam.tipo-mov c-situacao}  

    ASSIGN c-hr-entr    = STRING(mp-entr-cam.hr-entrada, "HH:MM")
           c-hr-sai     = STRING(mp-entr-cam.hr-saida, "HH:MM")
           de-diferenca = 0.
    IF mp-entr-cam.peso-nf <> 0  THEN
       ASSIGN de-diferenca = mp-entr-cam.peso-nf - (mp-entr-cam.peso-bruto - mp-entr-cam.peso-tara).

    DISPLAY mp-entr-cam.nome-transp WHEN FIRST-OF(mp-entr-cam.nome-transp)                                
            c-situacao
            mp-entr-cam.placa                                
            mp-entr-cam.dt-entrada
            c-hr-entr               
            mp-entr-cam.dt-saida
            c-hr-sai              
            mp-entr-cam.peso-bruto
            mp-entr-cam.peso-tara
            mp-entr-cam.peso-bruto - mp-entr-cam.peso-tara  @ mp-entr-cam.peso-liquido
            mp-entr-cam.peso-nf
            de-diferenca  WHEN de-diferenca <>  0 
            WITH FRAME f-detalhe.
     DOWN WITH FRAME f-detalhe.

     ACCUMULATE mp-entr-cam.peso-bruto (TOTAL BY mp-entr-cam.nome-transp).
     ACCUMULATE mp-entr-cam.peso-tara  (TOTAL BY mp-entr-cam.nome-transp).
     ACCUMULATE mp-entr-cam.peso-nf    (TOTAL BY mp-entr-cam.nome-transp).

     IF LAST-OF(mp-entr-cam.nome-transp) THEN DO:
        DISPLAY "T O T A L......"  @ c-situacao 
                ACCUM TOTAL BY  mp-entr-cam.nome-transp mp-entr-cam.peso-bruto 
                                FORMAT "->>>>,>>9.9" @ mp-entr-cam.peso-bruto
                ACCUM TOTAL BY  mp-entr-cam.nome-transp mp-entr-cam.peso-tara 
                                FORMAT "->>>>,>>9.9" @ mp-entr-cam.peso-tara
               (ACCUM TOTAL BY  mp-entr-cam.nome-transp mp-entr-cam.peso-bruto) -
                ACCUM TOTAL BY  mp-entr-cam.nome-transp mp-entr-cam.peso-tara 
                                FORMAT "->>>>,>>9.9" @ mp-entr-cam.peso-liquido
                ACCUM TOTAL BY  mp-entr-cam.nome-transp mp-entr-cam.peso-nf 
                                FORMAT "->>>>,>>9.9" @ mp-entr-cam.peso-nf
               ((ACCUM TOTAL BY  mp-entr-cam.nome-transp mp-entr-cam.peso-bruto) -
                 ACCUM TOTAL BY  mp-entr-cam.nome-transp mp-entr-cam.peso-tara ) -
                 ACCUM TOTAL BY  mp-entr-cam.nome-transp mp-entr-cam.peso-nf 
                                 FORMAT "->>>>,>>9.99" @ de-diferenca
                WITH FRAME f-detalhe.
        DOWN 2 WITH FRAME f-detalhe.
     END.

END. /* FIM OB-ETIQUETA */

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.nome-transp-ini
           tt-param.nome-transp-fin
           tt-param.dt-entrada-ini
           tt-param.dt-entrada-fin
           tt-param.placa-ini
           tt-param.placa-fin
           tt-param.tipo-mov-ini
           tt-param.tipo-mov-fin
           WITH FRAME f-param.
END. 

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.


