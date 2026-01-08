/* Programa: ESFT0024RP.P
** Sistema.: EMS da Datasul
** M¢dulo..: Faturamento
** Objetivo: EmissÆo do relat¢rio de NF para consulta do Sintegra
** Autor...: Gilvando Souza Araujo
** Data....: 16/05/2005
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0024RP 2.04.00.000}

define temp-table tt-param  no-undo
       FIELD destino           as integer
       FIELD arquivo           as char format "x(35)"
       FIELD usuario           as char format "x(12)"
       FIELD data-exec         as date
       FIELD hora-exec         as integer
       FIELD classifica        as integer
       FIELD desc-classifica   as char format "x(40)"
       FIELD cod-estabel       like nota-fiscal.cod-estabel 
       FIELD ini-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD fin-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD ini-dt-saida      LIKE nota-fiscal.dt-saida
       FIELD fin-dt-saida      LIKE nota-fiscal.dt-saida
       FIELD ini-transp        LIKE nota-fiscal.nome-transp
       FIELD fin-transp        LIKE nota-fiscal.nome-transp
       FIELD tipo-nota         AS INTEGER
       FIELD desc-tipo-nota    AS CHAR FORMAT "x(31)"
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

DEFINE TEMP-TABLE tt-rep-email
       FIELD cod-rep LIKE repres.cod-rep
       INDEX ch-rep cod-rep.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

DEF VAR i-end-cobranca LIKE emitente.end-cobranca.
DEF VAR c-endereco     LIKE emitente.endereco.
DEF VAR c-cep          LIKE emitente.cep.
DEF VAR c-cidade       LIKE emitente.cidade.
DEF VAR c-bairro       LIKE emitente.bairro.
DEF VAR c-estado       LIKE emitente.estado.
DEF VAR c-cgc          LIKE emitente.cgc.
DEF VAR c-ins-est      LIKE emitente.ins-estadual no-undo.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelec....."  SKIP
    tt-param.ini-dt-emissao   label "Data Emissao.."
    "A"  AT 33
    tt-param.fin-dt-emissao   NO-LABELS               SKIP
    tt-param.ini-dt-saida     label "Data Sa¡da...."
    "A"  AT 33
    tt-param.fin-dt-saida     NO-LABELS               SKIP
    tt-param.ini-transp       label "Transportadora"
    "A"  AT 33
    tt-param.fin-transp       NO-LABELS               SKIP
    tt-param.desc-tipo-nota   LABEL "Tipos de Notas"  SKIP
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM
    nota-fiscal.cod-emitente   LABEL "Cliente"       FORMAT "9999999"
    nota-fiscal.nome-ab-cli    LABEL "Nome-Abrev"
    c-cgc                      LABEL "CNPJ"          FORMAT "x(18)"
    c-ins-est                  LABEL "Insc.Estadual"
    c-endereco                 LABEL "Endere‡o"
    c-cidade                   LABEL "Cidade"        FORMAT "x(17)"
    c-estado                   LABEL "UF"
    nota-fiscal.nr-nota-fis    LABEL "N.Fiscal"      FORMAT "x(8)"
    WITH NO-LABEL NO-BOX 55 down width 132 STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FATURAMENTO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Notas_Fiscais_para_Consulta_de_Sintegra * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each nota-fiscal WHERE nota-fiscal.cod-estabel  =  tt-param.cod-estabel
                       AND nota-fiscal.dt-emis-nota >= tt-param.ini-dt-emissao
                       AND nota-fiscal.dt-emis-nota <= tt-param.fin-dt-emissao
                       AND nota-fiscal.nome-transp  >= tt-param.ini-transp
                       AND nota-fiscal.nome-transp  <= tt-param.fin-transp
                       AND ((nota-fiscal.dt-saida   >= tt-param.ini-dt-saida AND 
                             nota-fiscal.dt-saida   <= tt-param.fin-dt-saida AND tt-param.tipo-nota = 1) OR
                            (nota-fiscal.dt-saida   = ? AND tt-param.tipo-nota = 2))
                       AND nota-fiscal.dt-cancela   =  ? 
                     NO-LOCK
                     BY nota-fiscal.estado
                     BY nota-fiscal.cidade:

    run pi-acompanhar in h-acomp (input string(nota-fiscal.cod-rep) + " " + nota-fiscal.nr-nota-fis).

    FIND emitente OF nota-fiscal NO-LOCK.
    
    IF emitente.natureza = 1 THEN 
       assign c-cgc = if param-global.formato-id-pessoal <> ""
                      then string(nota-fiscal.cgc, param-global.formato-id-pessoal)
                      else nota-fiscal.cgc.
    else 
       if emitente.natureza = 2 then 
          assign c-cgc = if param-global.formato-id-federal <> ""
                         then string(nota-fiscal.cgc, param-global.formato-id-federal)
                         else nota-fiscal.cgc.
       else
          assign c-cgc = nota-fiscal.cgc.

    ASSIGN c-ins-est = nota-fiscal.ins-estadual.

    IF nota-fiscal.endereco <> "" then do:
       assign c-endereco = nota-fiscal.endereco
              c-bairro   = nota-fiscal.bairro
              c-cidade   = nota-fiscal.cidade
              c-estado   = nota-fiscal.estado
              c-cep      = nota-fiscal.cep.
    end.
    else do:
       if nota-fiscal.cod-entrega <> "" then do:
          find loc-entr
               where loc-entr.nome-abrev  = nota-fiscal.nome-ab-cli
                 and loc-entr.cod-entrega = nota-fiscal.cod-entrega
                     no-lock no-error.
          if avail loc-entr then
             assign c-endereco = loc-entr.endereco
                    c-bairro   = loc-entr.bairro
                    c-cidade   = loc-entr.cidade
                    c-estado   = loc-entr.estado
                    c-cep      = loc-entr.cep.
       end.
       else do:
          if avail ped-venda then do:
             if ped-venda.local-entreg <> "" then
                assign c-endereco = ped-venda.local-entreg
                       c-bairro   = ped-venda.bairro
                       c-cidade   = ped-venda.cidade
                       c-estado   = ped-venda.estado
                       c-cep      = ped-venda.cep.
             else do:
               if ped-venda.cod-entrega <> "" then do:
                  find loc-entr
                       where loc-entr.nome-abrev =
                             ped-venda.nome-abrev
                         and loc-entr.cod-entrega =
                             ped-venda.cod-entrega.
                  if avail loc-entr then
                     assign c-endereco = loc-entr.endereco
                            c-bairro   = loc-entr.bairro
                            c-cidade   = loc-entr.cidade
                            c-estado   = loc-entr.estado
                            c-cep      = loc-entr.cep.
               end.
               else do:
                  assign c-endereco = emitente.endereco
                         c-bairro   = emitente.bairro
                         c-cidade   = emitente.cidade
                         c-estado   = emitente.estado
                         c-cep      = emitente.cep.
               end.
             end.
          end.
          else do:
             assign c-endereco = emitente.endereco
                    c-bairro   = emitente.bairro
                    c-cidade   = emitente.cidade
                    c-estado   = emitente.estado
                    c-cep      = emitente.cep.
          end.
       END.
    END.

    DISPLAY nota-fiscal.cod-emitente
            nota-fiscal.nome-ab-cli 
            c-cgc            
            c-ins-est   
            c-endereco
            c-cidade
            c-estado
            nota-fiscal.nr-nota-fis 
            WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.
END.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.cod-estabel
           tt-param.ini-dt-emissao
           tt-param.fin-dt-emissao
           tt-param.ini-dt-saida
           tt-param.fin-dt-saida
           tt-param.ini-transp  
           tt-param.fin-transp  
           tt-param.desc-tipo-nota
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

