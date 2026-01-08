/* Programa: ESPD0021.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Pedidos         
** Objetivo: Listar Cadastro de Transportadores
** Autor...: Gilvando de Souza Araujo - Julho/2005
** Obs.....: Especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0021RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino            as integer
       field arquivo            as char format "x(35)"
       field usuario            as char format "x(12)"
       field data-exec          as date
       field hora-exec          as integer
       field classifica         as integer
       field desc-classifica    as char format "x(40)"
       FIELD cod-transp-ini     LIKE transporte.cod-transp
       FIELD cod-transp-fin     LIKE transporte.cod-transp
       FIELD nome-abrev-ini     LIKE transporte.nome-abrev
       FIELD nome-abrev-fin     LIKE transporte.nome-abrev
       FIELD transp-ativos      AS LOG FORMAT "Sim/NÆo"
       FIELD impr-param         AS LOG.

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
def var h-acomp as handle no-undo.
DEF VAR l-imprimir AS LOG.

form 
    "*---------- Parƒmetros/Sele‡Æo -----------*" SKIP
    tt-param.cod-transp-ini   label "C¢digo........"
    "A"  AT 30
    tt-param.cod-transp-fin   no-labels SKIP
    tt-param.nome-abrev-ini   label "Nome Abreviado"
    "A"  AT 30
    tt-param.nome-abrev-fin   NO-LABELS SKIP
    tt-param.desc-classifica  LABEL "Classifica‡Æo." SKIP
    tt-param.transp-ativos    LABEL "Apenas Ativos." SKIP(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    transporte.cod-transp   label "C¢digo"
    transporte.nome-abrev   label "Nome Abrev"
    transporte.nome         LABEL "Nome"     FORMAT "x(32)"
    transporte.endereco     label "Endereco"
    transporte.cidade       label "Cidade"   format "x(20)"
    transporte.estado       label "UF"
    transporte.telefone     label "Telefone" format "x(12)"
    with no-labels no-box 55 down width 132 STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i PEDIDOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Cadastro_de_Transportadores * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-cabecalho.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each transporte where transporte.cod-transp >= tt-param.cod-transp-ini
                      and transporte.cod-transp <= tt-param.cod-transp-fin
                      and transporte.nome-abrev >= tt-param.nome-abrev-ini
                      and transporte.nome-abrev <= tt-param.nome-abrev-fin
                    no-lock
                    BY IF tt-param.classifica = 1 THEN string(transporte.cod-transp)
                                                  ELSE transporte.nome-abrev:

   run pi-acompanhar in h-acomp (input "Tranp.: " + string(transporte.cod-transp)).
   
   ASSIGN l-imprimir = YES.
   IF tt-param.transp-ativos THEN DO:
      FIND FIRST nota-fiscal WHERE nota-fiscal.nome-transp  =  transporte.nome-abrev
                               AND nota-fiscal.dt-emis-nota >= TODAY - 180
                               AND nota-fiscal.dt-emis-nota <= TODAY
                             NO-LOCK NO-ERROR.
      IF NOT AVAIL nota-fiscal THEN
         ASSIGN l-imprimir = NO.
   END.

   IF l-imprimir THEN DO:
      display transporte.cod-transp
              transporte.nome-abrev
              transporte.nome
              transporte.endereco
              transporte.cidade
              transporte.estado
              transporte.telefone
              with frame f-detalhe.
      down with frame f-detalhe.
   END.
end.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.cod-transp-ini 
           tt-param.cod-transp-fin 
           tt-param.nome-abrev-ini 
           tt-param.nome-abrev-fin 
           tt-param.desc-classifica
           tt-param.transp-ativos  
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

