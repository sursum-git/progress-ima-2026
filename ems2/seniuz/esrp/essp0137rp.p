/* Programa: ESSP0126.W
** Modulo..: Controle de AlgodÆo
** Objetivo: Listar Resultado de Testes em Fibrografo
** Autor...: Fábio Coelho Lanza - Novembro/2006
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0126RP 2.04.00.000}

DEF BUFFER b-mp-fardo for mp-fardo.

DEFINE TEMP-TABLE tt-param   NO-UNDO
       FIELD destino            AS INTEGER
       FIELD arquivo            AS CHAR format "x(35)"
       FIELD usuario            AS CHAR format "x(12)"
       FIELD data-exec          AS DATE
       FIELD hora-exec          AS INTEGER
       FIELD classifica         AS INTEGER
       FIELD desc-classifica    AS CHAR format "x(40)"
       FIELD cod-emit-ini       LIKE mp-entr-mat.cod-emit
       FIELD cod-emit-fin       LIKE mp-entr-mat.cod-emit
       FIELD dt-recebimento-ini LIKE mp-entr-mat.dt-recebimento
       FIELD dt-recebimento-fin LIKE mp-entr-mat.dt-recebimento
       FIELD nf-ini             LIKE mp-entr-mat.nro-docto
       FIELD nf-fin             LIKE mp-entr-mat.nro-docto
       FIELD imp-param          as log.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}


/* defini‡Æo de vari veis  */
DEF VAR h-acomp AS HANDLE NO-UNDO.
DEF VAR c-residuo    AS CHAR.
DEF VAR c-micronaire AS CHAR.
DEF VAR c-presley    AS CHAR.
DEF VAR c-fardo      AS CHAR FORMAT "x(13)".

form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*" SKIP
    tt-param.cod-emit-ini       LABEL  "Fornecedor "
    "A"  AT 40
    tt-param.cod-emit-fin       NO-LABELS SKIP
    tt-param.dt-recebimento-ini LABEL  "Dt Receb.  "
    "A"  AT 40                  
    tt-param.dt-recebimento-fin NO-LABELS SKIP
    tt-param.nf-ini             LABEL  "Nota Fiscal"
    "A"  AT 40                    
    tt-param.nf-fin             NO-LABELS SKIP
    WITH FRAME f-param SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.

form
    mp-entr-mat.dt-recebimento             LABEL "Data"
    mp-entr-mat.nro-docto                  LABEL "N.Fiscal" 
    c-fardo                                LABEL "N§ Fardo"
    emitente.nome-emit                     LABEL "Fornecedor"     
    mp-entr-mat.procedencia                LABEL "Procedˆncia" FORMAT "x(15)"
    mp-entr-mat.nro-contrato               LABEL "Contrato"
    c-residuo                              LABEL "Residuo"
    c-micronaire                           LABEL "Micronaire"
    c-presley                              LABEL "Presley"
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
{utp/ut-liter.i Conferˆncia_das_Importa‡äes * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
 
FOR EACH mp-entr-mat WHERE mp-entr-mat.cod-emit        >= tt-param.cod-emit-ini    
                       AND mp-entr-mat.cod-emit        <= tt-param.cod-emit-fin
                       AND mp-entr-mat.dt-recebimento  >= tt-param.dt-recebimento-ini    
                       AND mp-entr-mat.dt-recebimento  <= tt-param.dt-recebimento-fin
                       AND mp-entr-mat.nro-docto       >= tt-param.nf-ini  
                       AND mp-entr-mat.nro-docto       <= tt-param.nf-fin
                       USE-INDEX indice4
                   NO-LOCK,
    FIRST emitente   WHERE emitente.cod-emit = mp-entr-mat.cod-emit NO-LOCK,    
    FIRST mp-fardo   WHERE mp-fardo.nr-cdr = mp-entr-mat.nr-cdr NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT "Data: "  + string(mp-entr-mat.dt-recebimento)).
    
    ASSIGN c-residuo    = ""
           c-micronaire = ""
           c-presley    = "".
    IF AVAIL mp-fardo THEN DO:
       IF mp-fardo.finura + mp-fardo.maturidade = 0 THEN 
          ASSIGN c-micronaire = "     X".
       IF mp-fardo.resistencia = 0  THEN
          ASSIGN c-presley = "   X".
    END.
    IF mp-entr-mat.disperdicio = 0 THEN
       ASSIGN c-residuo = "   X".

    ASSIGN c-fardo = STRING(mp-fardo.nr-fardo) + "-".
    FIND LAST b-mp-fardo OF mp-entr-mat NO-LOCK.
    IF AVAIL b-mp-fardo THEN
       ASSIGN c-fardo = c-fardo + SUBSTR(STRING(b-mp-fardo.nr-fardo),5,4).
    IF c-residuo <> "" OR c-micronaire <> "" OR  c-presley <> ""  THEN DO:
       DISPLAY mp-entr-mat.dt-recebimento
               mp-entr-mat.nro-docto
               c-fardo
               emitente.nome-emit 
               mp-entr-mat.procedencia
               mp-entr-mat.nro-contrato
               c-residuo
               c-micronaire
               c-presley
               WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.
    END.

END.

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.cod-emit-ini
           tt-param.cod-emit-fin
           tt-param.dt-recebimento-ini
           tt-param.dt-recebimento-fin
           tt-param.nf-ini
           tt-param.nf-fin
           WITH FRAME f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.

