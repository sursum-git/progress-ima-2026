/* Programa: ESSP0153.W
** Modulo..: Controle de Expedi‡Æo
** Objetivo: Listar Transorma‡äes
** Autor...: Fábio Coelho Lanza - Mar‡o/2007
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0153RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param   NO-UNDO
       FIELD destino            AS INTEGER
       FIELD arquivo            AS CHAR FORMAT "x(35)"
       FIELD usuario            AS CHAR FORMAT "x(12)"
       FIELD data-exec          AS DATE
       FIELD hora-exec          AS INTEGER
       FIELD classifica         AS INTEGER
       FIELD desc-classifica    AS CHAR FORMAT "x(40)"
       FIELD num-trf-ini        LIKE ob-trf.num-trf       
       FIELD num-trf-fin        LIKE ob-trf.num-trf          
       FIELD imp-param          AS LOG.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita as raw.

DEF TEMP-TABLE tt-work
    FIELD num-trf      LIKE ob-trf.num-trf         
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta 
    FIELD cod-estabel  LIKE ob-etiqueta.cod-estabel
    FIELD quantidade   LIKE ob-etiqueta.quantidade
    FIELD localizacao  LIKE ob-etiqueta.localizacao
    FIELD origem       AS CHAR
    FIELD destino      AS CHAR
    FIELD qualid       AS CHAR
    FIELD situacao     AS CHAR
    INDEX ch-work num-trf 
                  localizacao
                  num-etiqueta.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}


/* defini‡Æo de vari veis  */
def var h-acomp     as handle no-undo.
DEF VAR c-situacao  AS CHAR FORMAT "x(12)".
DEF VAR c-origem    AS CHAR FORMAT "x(15)".
DEF VAR c-destino   AS CHAR FORMAT "x(15)".
DEF VAR c-qualid    AS CHAR FORMAT "x(11)".

form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*" SKIP
    tt-param.num-trf-ini   LABEL  "Transforma‡Æo"
    "A"  AT 40
    tt-param.num-trf-fin   NO-LABELS SKIP
    WITH FRAME f-param SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.

form
    tt-work.num-trf                      LABEL "Transf"
    ob-etiqueta.it-codigo FORMAT "x(6)"  LABEL "Item"     
    ob-etiqueta.cod-refer                LABEL "Referˆncia"
    tt-work.origem                       LABEL "ORIGEM"
    tt-work.destino                      LABEL "DESTINO"
    tt-work.localizacao FORMAT "XXX/XXX" LABEL "Localiz"
    tt-work.num-etiqueta                 LABEL "Etiqueta"
    ob-etiqueta.nr-ob                    LABEL "Nr.OB"
    ob-etiqueta.nr-sequencia             LABEL "Seq"
    tt-work.quantidade                   LABEL "Qtde"
    tt-work.qualid                       LABEL "Qualidade"
    ob-etiqueta.nuance                   LABEL "Nuance" 
    tt-work.situacao                     LABEL "Situacao"
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

{utp/ut-liter.i ESPECÖFICOS r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i TRANSFORMA€åES r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
 

FOR EACH ob-trf WHERE
         ob-trf.num-trf >= tt-param.num-trf-ini AND
         ob-trf.num-trf <= tt-param.num-trf-fin NO-LOCK.

    ASSIGN c-destino  = ""
           c-origem   = ""
           c-qualid   = ""
           c-situacao = "".
    FIND corte-comerc WHERE 
         corte-comerc.codigo = ob-trf.corte-comerc NO-LOCK NO-ERROR.
    IF AVAIL corte-comerc THEN
       ASSIGN c-destino = corte-comerc.descricao.

    FOR EACH ob-etq-trf WHERE
             ob-etq-trf.num-trf = ob-trf.num-trf NO-LOCK.

        ASSIGN c-origem = "".
        FIND ob-etiqueta WHERE 
             ob-etiqueta.cod-estabel  = ob-etq-trf.cod-estabel AND
             ob-etiqueta.num-etiqueta = ob-etq-trf.num-etiqueta NO-LOCK NO-ERROR.
        IF AVAIL ob-etiqueta THEN DO:
           FIND corte-comerc WHERE 
                corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.
           IF AVAIL corte-comerc THEN
              ASSIGN c-origem = corte-comerc.descricao.

           {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao}  

           FIND qualid-tecido WHERE
                qualid-tecido.codigo = ob-etiqueta.cod-qualid NO-LOCK NO-ERROR.
           IF AVAIL qualid-tecido THEN
              ASSIGN c-qualid = qualid-tecido.descricao.
        END.

        FIND FIRST tt-work WHERE 
                   tt-work.num-trf      = ob-etq-trf.num-trf AND 
                   tt-work.num-etiqueta = ob-etq-trf.num-etiqueta AND 
                   tt-work.localizacao  = ob-etiqueta.localizacao NO-ERROR.
        IF NOT AVAIL tt-work THEN DO:
           CREATE tt-work.
           ASSIGN tt-work.num-trf      = ob-etq-trf.num-trf
                  tt-work.num-etiqueta = ob-etq-trf.num-etiqueta
                  tt-work.cod-estabel  = ob-etq-trf.cod-estabel
                  tt-work.quantidade   = ob-etiqueta.quantidade
                  tt-work.localizacao  = ob-etiqueta.localizacao
                  tt-work.origem       = c-origem
                  tt-work.destino      = c-destino
                  tt-work.qualid       = c-qualid
                  tt-work.situacao     = c-situacao.
        END.
    END.
    IF ob-trf.dec-1 > 0 AND c-origem = "" AND c-destino = "" AND 
                            c-qualid = "" AND c-situacao = ""  THEN DO: /* E RETRABALHO */
        CREATE tt-work.
        ASSIGN tt-work.num-trf = ob-trf.num-trf
               tt-work.origem  = "RETRABALHO".
    END.
END.

FOR EACH tt-work BREAK BY tt-work.num-trf
                       BY tt-work.localizacao
                       BY tt-work.num-etiqueta.

    FIND ob-trf WHERE 
         ob-trf.num-trf = tt-work.num-trf NO-LOCK NO-ERROR.

    IF tt-work.origem <> "RETRABALHO" THEN DO:
       FIND ob-etiqueta WHERE 
            ob-etiqueta.cod-estabel  = tt-work.cod-estabel AND
            ob-etiqueta.num-etiqueta = tt-work.num-etiqueta NO-LOCK NO-ERROR.
       DISPLAY tt-work.num-trf        WHEN FIRST-OF(tt-work.num-trf)                                
               ob-etiqueta.it-codigo  WHEN FIRST-OF(tt-work.num-trf)                              
               ob-etiqueta.cod-refer  WHEN FIRST-OF(tt-work.num-trf)
               tt-work.origem         WHEN FIRST-OF(tt-work.num-trf)
               tt-work.destino        WHEN FIRST-OF(tt-work.num-trf)
               tt-work.localizacao
               tt-work.num-etiqueta
               ob-etiqueta.nr-ob
               ob-etiqueta.nr-sequencia
               tt-work.quantidade
               tt-work.qualid
               ob-etiqueta.nuance 
               tt-work.situacao    FORMAT "X(15)"
               WITH FRAME f-detalhe.

       DOWN WITH FRAME f-detalhe.
       ACCUMULATE tt-work.quantidade (TOTAL BY tt-work.num-trf).
       ACCUMULATE tt-work.num-trf    (COUNT TOTAL BY tt-work.num-trf).

       IF LAST-OF(tt-work.num-trf) THEN DO:
          DISPLAY "TOTAL"  @ ob-etiqueta.nr-ob
                  "..."    @ ob-etiqueta.nr-sequencia
                  ACCUM TOTAL BY  tt-work.num-trf tt-work.quantidade @ tt-work.quantidade
                  "QTD: " + STRING(ACCUM COUNT BY tt-work.num-trf tt-work.num-trf) @ tt-work.qualid
                  WITH FRAME f-detalhe.
          DOWN 1 WITH FRAME f-detalhe.
          PAGE.
       END.
    END.
    ELSE DO: /* E RETRABALHO */
       DISPLAY tt-work.num-trf                                        
               ob-trf.it-codigo @ ob-etiqueta.it-codigo                              
               ob-trf.cod-refer @ ob-etiqueta.cod-refer 
               tt-work.origem   FORMAT "x(12)"
               ob-trf.dec-1     @ tt-work.quantidade
               WITH FRAME f-detalhe.
        DOWN WITH FRAME f-detalhe.
        PAGE.
    END.
END. 

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.num-trf-ini
           tt-param.num-trf-fin
           WITH FRAME f-param.
END. 

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.





