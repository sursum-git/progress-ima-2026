/* Programa: ESSP0158.W
** Modulo..: Controle de Expedi‡Æo
** Objetivo: Listar os PARAMETROS das Politicas de Extoque X Faturamento
** Autor...: Fábio Coelho Lanza - MAIO/2007
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0158RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param  NO-UNDO
       FIELD destino         AS INTEGER
       FIELD arquivo         AS CHAR format "x(35)"
       FIELD usuario         AS CHAR format "x(12)"
       FIELD data-exec       AS DATE
       FIELD hora-exec       AS INTEGER
       FIELD classifica      AS INTEGER
       FIELD desc-classifica AS CHAR FORMAT "x(40)"
       FIELD cod-gr-cli-ini  LIKE param-pef.cod-gr-cli   
       FIELD cod-gr-cli-fin  LIKE param-pef.cod-gr-cli   
       FIELD imp-param       AS LOG.

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
DEF VAR h-acomp         AS HANDLE NO-UNDO.
DEF VAR c-tipos         AS CHAR.
DEF VAR c-desc-tipos    AS CHAR FORMAT "x(12)".
DEF VAR c-obsoleto      AS CHAR.
DEF VAR c-desc-obsoleto AS CHAR FORMAT "x(20)".
DEF VAR i-ct            AS INT.

form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*" SKIP
    tt-param.cod-gr-cli-ini LABEL  "Grupo Cliente"
    "A"  AT 40
    tt-param.cod-gr-cli-fin  NO-LABELS SKIP
    WITH FRAME f-param SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.

form                                                     
    gr-cli.descricao FORMAT "x(16)"                 LABEL "Grupo do Cliente"
    c-desc-tipos                                    LABEL "Tipo Cliente"
    param-pef.lote                                  LABEL "Lote"
    c-desc-obsoleto                                 LABEL "Codigo Obsoleto"
    param-pef.seq-qualid                            LABEL "Seq Qualid"
    param-pef.ate-500                               LABEL "At‚ 500" 
    param-pef.ate-1000                              LABEL "At‚ 1000"
    param-pef.aci-1000                              LABEL "Acima 1000"
    param-pef.lim-Inferior                          LABEL "Lim-Inferior"
    param-pef.lim-superior                          LABEL "Lim-Superior"
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
{utp/ut-liter.i Parametros_das_Politicas_de_Estoque_X_Faturamento * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
 
FOR EACH param-pef  WHERE param-pef.cod-gr-cli >= tt-param.cod-gr-cli-ini
                      AND param-pef.cod-gr-cli <= tt-param.cod-gr-cli-fin
                          NO-LOCK
                          BREAK BY param-pef.cod-gr-cli    
                                BY param-pef.tipo-cliente
                                BY param-pef.lote
                                BY param-pef.cod-obsoleto.

    {esinc/i-dsallrb.i param-pef.tipo-cliente c-tipos}
    {esinc/i-dsallrb.i param-pef.cod-obsoleto c-obsoleto}
    ASSIGN c-desc-tipos    =  ENTRY(param-pef.tipo-cliente,c-tipos)              
           c-desc-obsoleto = ENTRY(INT(param-pef.cod-obsoleto) + 1,c-obsoleto).

    FIND gr-cli WHERE
         gr-cli.cod-gr-cli = param-pef.cod-gr-cli NO-LOCK NO-ERROR.
    DISPLAY  gr-cli.descricao WHEN FIRST-OF(param-pef.cod-gr-cli)                              
             c-desc-tipos     WHEN FIRST-OF(param-pef.tipo-cliente)                                        
             param-pef.lote   WHEN FIRST-OF(param-pef.lote)          
             c-desc-obsoleto  WHEN FIRST-OF(param-pef.cod-obsoleto)       
             param-pef.seq-qualid       
             param-pef.ate-500          
             param-pef.ate-1000         
             param-pef.aci-1000            
             param-pef.lim-Inferior     
             param-pef.lim-superior       
             WITH FRAME f-detalhe.
     DOWN WITH FRAME f-detalhe.

     IF LAST-OF(param-pef.lote) THEN
        DOWN 1 WITH FRAME f-detalhe.


END. /* FIM param-pef */

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.cod-gr-cli-ini 
           tt-param.cod-gr-cli-fin 
           WITH FRAME f-param.
END. 

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.


