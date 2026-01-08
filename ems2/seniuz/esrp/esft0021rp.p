/* Programa: ESCD002.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Listar Clientes em Ordem Albabetica por REPRESENTANTE.
** Autor...: Alterado Por Fabio Coelho Lanza - NOVEMBRO/98
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESCD002.P  =>  ESFT0021RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 20/02/2005
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0021RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD dt-emis-nota-ini  LIKE nota-fiscal.dt-emis-nota
       FIELD dt-emis-nota-fin  LIKE nota-fiscal.dt-emis-nota
       FIELD cod-rep-ini       LIKE nota-fiscal.cod-rep
       FIELD cod-rep-fin       LIKE nota-fiscal.cod-rep
       FIELD cod-gr-cli-ini    LIKE emitente.cod-gr-cli
       FIELD cod-gr-cli-fin    LIKE emitente.cod-gr-cli
       FIELD tp-pedido         AS CHAR
       FIELD com-sem-fat       AS INT
       FIELD desc-com-sem-fat  AS CHAR FORMAT "x(15)"
       FIELD so-resumo         AS LOG FORMAT "Sim/NÆo"
       FIELD salta-pagina      AS LOG FORMAT "Sim/NÆo"
       FIELD gerar-excel       AS LOG FORMAT "Sim/NÆo"
       FIELD arq-excel         AS CHAR FORMAT "x(45)"
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def TEMP-TABLE tt-work
    field cod-rep  like repres.cod-rep
    field cli-cfat as int
    field cli-sfat as int.

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

def var c-cidade    as char format "x(21)" label "Cidade".
def var c-nome-emit as char format "x(38)".
def var c-endereco  as char format "x(38)".
def var l-primvez   as log init yes.
def var l-faturou   as log.
def var i-result    as int.
def var i-pgnumb    as int.

IF tt-param.gerar-excel THEN
   DEF STREAM arq-excel.

form 
    "*--------- Parƒmetros/Sele‡Æo ---------*"  SKIP
    tt-param.dt-emis-nota-ini  label "Data emissÆo NF"
    "A"  AT 29
    tt-param.dt-emis-nota-fin  NO-LABELS               SKIP
    tt-param.cod-rep-ini       label "Representante.."
    "A"  AT 29
    tt-param.cod-rep-fin       no-labels               SKIP
    tt-param.cod-gr-cli-ini    label "Grupo Cliente.."
    "A"  AT 29  
    tt-param.cod-gr-cli-fin    NO-LABELS               SKIP
    tt-param.tp-pedido         LABEL "Tipos de Pedido" SKIP
    tt-param.desc-com-sem-fat  LABEL "Compr/NÆo Compr" SKIP
    tt-param.so-resumo         LABEL "S¢ resumo......" SKIP
    tt-param.salta-pagina      LABEL "Salta p gina..." SKIP
    tt-param.gerar-excel       LABEL "Gerar p/Excel.." SKIP
    tt-param.arq-excel         LABEL "Arquivo Excel.." SKIP(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    "Representante: "
    repres.cod-rep format "99999"
    repres.nome
    c-cidade
    repres.estado
    repres.telefone[1]
    skip(1)
    with no-box no-labels 55 down width 132 STREAM-IO FRAME f-repres.

form
    emitente.cod-emitente           column-label "Cod." format ">>>>9"
    c-nome-emit                     column-label "Nome"
    c-endereco                      column-label "Endereco"
    c-cidade
    emitente.estado
    emitente.telefone[1]            column-label "Telefone"
    nota-fiscal.dt-emis-nota        column-label "Ultima NF"
    gr-cli.descricao                column-label "Grupo do Cliente"     
    skip(1)
    with no-box 55 down width 132 STREAM-IO frame f-detalhe.

form
    repres.cod-rep      column-label "Cod." 
    repres.nome         column-label "Nome"
    tt-work.cli-cfat     column-label "Clientes Ativos"
    tt-work.cli-sfat     column-label "Clientes Inativos"
    with no-box 55 down width 132 STREAM-IO frame f-detalhe1.

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
{utp/ut-liter.i Cadastro_de_Clientes_x_Ult.Fat._por_Representante * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

IF tt-param.gerar-excel THEN DO:
   OUTPUT STREAM arq-excel TO VALUE(tt-param.arq-excel) CONVERT SOURCE "ibm850".
   PUT STREAM arq-excel "Repres;Cod.;E-mail;CNPJ;Nome;Endereco;Cidade;UF;Telefone;Ultima NF;Grupo do Cliente"
                        SKIP.
END.

for each emitente where emitente.cod-rep    >= tt-param.cod-rep-ini
                    and emitente.cod-rep    <= tt-param.cod-rep-fin
                    and emitente.cod-gr-cli >= tt-param.cod-gr-cli-ini
                    and emitente.cod-gr-cli <= tt-param.cod-gr-cli-fin
                    and emitente.identific  <> 2 
                    no-lock break by emitente.cod-rep
                                  by emitente.nome-emit
                                  by emitente.cod-emitente:
   
    run pi-acompanhar in h-acomp (input "Repres/Cliente: " + string(emitente.cod-rep) + 
                                        " " + emitente.nome-abrev).

    find gr-cli where gr-cli.cod-gr-cli = emitente.cod-gr-cli 
                no-lock no-error.

    FOR LAST nota-fiscal USE-INDEX ch-emi-nota 
        WHERE nota-fiscal.cod-emitente =  emitente.cod-emitente
          AND nota-fiscal.dt-emis-nota >= tt-param.dt-emis-nota-ini
          AND nota-fiscal.dt-emis-nota <= tt-param.dt-emis-nota-fin
        NO-LOCK,
        FIRST ped-venda WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                          AND ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                        NO-LOCK,
        FIRST ped-venda-ext WHERE ped-venda-ext.nome-abrev = ped-venda.nome-abrev 
                              AND ped-venda-ext.nr-pedcli  = ped-venda.nr-pedcli
                              AND LOOKUP(ped-venda-ext.tp-pedido,tt-param.tp-pedido) <> 0
                            NO-LOCK.
    END.

    IF avail nota-fiscal then
       assign l-faturou = yes.
    ELSE DO:
       assign l-faturou = no.
       find LAST nota-fiscal use-index ch-emi-nota 
            where nota-fiscal.cod-emitente = emitente.cod-emitente
              and nota-fiscal.dt-emis-nota < tt-param.dt-emis-nota-ini
            no-lock no-error.
    END.

    if first-of(emitente.cod-rep) then do:
       find repres where repres.cod-rep = emitente.cod-rep
            no-lock no-error.
       assign c-cidade = IF AVAIL repres THEN substr(repres.cidade,1,22) ELSE "".

       if tt-param.salta-pagina and 
          l-primvez = no and 
          tt-param.so-resumo = no then do:
          PAGE.
          IF PAGE-NUMBER MODULO 2 = 0 THEN DO:
             DISPLAY " ".
             PAGE.
          END.
       end.
       if tt-param.so-resumo = no AND 
          AVAIL repres then do:
          assign l-primvez = no.
          display repres.cod-rep
                  repres.nome
                  c-cidade
                  repres.estado
                  repres.telefone[1]
                  with frame f-repres.
          down with frame f-repres.
       end.
    end.
   
    if (l-faturou     and tt-param.com-sem-fat = 1)
    or (not l-faturou and tt-param.com-sem-fat = 2)
    or tt-param.com-sem-fat = 3 then do:
       assign c-nome-emit   = substr(emitente.nome-emit,1,38)
              c-endereco    = substr(emitente.endereco,1,38)
              c-cidade      = substr(emitente.cidade,1,21).
       if tt-param.so-resumo = no then do:
          display emitente.cod-emitente
                  c-nome-emit
                  c-endereco
                  c-cidade
                  emitente.estado
                  emitente.telefone[1]
                  nota-fiscal.dt-emis-nota   
                   when avail nota-fiscal
                  IF AVAIL gr-cli THEN gr-cli.descricao ELSE "Gr nÆo def"
                   @ gr-cli.descricao
                  with frame f-detalhe.
          down with frame f-detalhe.

          IF tt-param.gerar-excel THEN DO:
             IF AVAIL repres THEN
                PUT STREAM arq-excel
                    repres.nome-abrev ";".
             ELSE
                PUT STREAM arq-excel
                    " ;".

             PUT STREAM arq-excel
                 emitente.cod-emitente ";"
                 emitente.e-mail ";".
             
             IF emitente.natureza = 1 THEN
                PUT STREAM arq-excel
                    emitente.cgc FORMAT "999.999.999-99" ";".
             ELSE
             IF emitente.natureza = 2 THEN
                    PUT STREAM arq-excel
                        emitente.cgc FORMAT "99.999.999/9999-99" ";".
             ELSE
                PUT STREAM arq-excel ";".

             PUT STREAM arq-excel
                 c-nome-emit ";"              
                 c-endereco ";"               
                 c-cidade ";"                 
                 emitente.estado ";"          
                 emitente.telefone[1] ";".
             IF AVAIL nota-fiscal THEN
                PUT STREAM arq-excel
                    nota-fiscal.dt-emis-nota ";".
             ELSE
                PUT STREAM arq-excel
                    " ;".
             IF AVAIL gr-cli THEN
                PUT STREAM arq-excel
                    gr-cli.descricao
                    SKIP.
             ELSE
                PUT STREAM arq-excel
                    " ;"
                    SKIP.
          END.
       END.
    END.
    find first tt-work where tt-work.cod-rep = repres.cod-rep
                      no-lock no-error.
    if not avail tt-work then do:
       create tt-work.
       assign tt-work.cod-rep  = IF AVAIL repres THEN repres.cod-rep ELSE 0
              tt-work.cli-cfat = 0
              tt-work.cli-sfat = 0.
    end.
    if l-faturou then
       assign tt-work.cli-cfat = tt-work.cli-cfat + 1.
    else
       assign tt-work.cli-sfat = tt-work.cli-sfat + 1.
end.

if tt-param.so-resumo = no then 
   page.

for each tt-work:
    find repres where repres.cod-rep = tt-work.cod-rep no-lock.
    display IF AVAIL repres THEN repres.cod-rep ELSE 0 @ repres.cod-rep
            IF AVAIL repres THEN repres.nome    ELSE "NÆo definido" @ repres.nome
            tt-work.cli-cfat
            tt-work.cli-sfat
            with frame f-detalhe1.
    down with frame f-detalhe1.
end.

for each tt-work:
    delete tt-work.
end.

IF tt-param.gerar-excel THEN
   OUTPUT STREAM arq-excel CLOSE.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.dt-emis-nota-ini
           tt-param.dt-emis-nota-fin
           tt-param.cod-rep-ini     
           tt-param.cod-rep-fin     
           tt-param.cod-gr-cli-ini  
           tt-param.cod-gr-cli-fin  
           tt-param.tp-pedido FORMAT "x(100)"
           tt-param.desc-com-sem-fat 
           tt-param.so-resumo       
           tt-param.salta-pagina
           tt-param.gerar-excel
           tt-param.arq-excel  
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

