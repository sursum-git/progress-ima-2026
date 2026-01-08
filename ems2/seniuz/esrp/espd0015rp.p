/* Programa: ESPD042.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Pedidos
** Objetivo: Gerar o relatorio de Programacao Processo/Pronto
** Autor...: Gilvando de Souza Araujo - Fevereiro/97
**           Alterado em AGOSTO/2001 - Fabio Coelho Lanza
**           Incluido Selecao: Programado (Sim/Nao)
**                             Processo   (Sim/Nao)
**                             Pronto     (Sim/Nao) 
** Obs.....: Especifico da TEAR TEXTIL IND COM LTDA
**
** Conversao para EMS 2.04:
**   Programa: ESPD042.P  =>  ESPD0015RP.P
**   Autor...: F bio Coelho Lanza
**   Data....: 22/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0015RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       field fi-ini-it-codigo  like ob-pcp.it-codigo
       field fi-fin-it-codigo  like ob-pcp.it-codigo
       FIELD fi-ini-cod-refer  LIKE ob-pcp-ref.cod-refer
       FIELD fi-fin-cod-refer  LIKE ob-pcp-ref.cod-refer
       field fi-desenho        as char format "x(4)"
       field l-tipo-rel        as log  format "Analitico/Sintetico"
       field l-inc-exc         as log  format "Inclusive/Exclusive"
       field c-imp-comp-fab    as char format "x"
       field c-imp-liso-est    as char format "x"
       field tg-programado     as log  format "Sim/NÆo"
       field tg-processo       as log  format "Sim/NÆo"
       field tg-pronto         as log  format "Sim/NÆo"
       field tg-sem-estrutura  as log  format "Sim/NÆo"
       field imp-param         as log.

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

def var i-tot-prog-fnd  as int.
def var i-tot-proc-fnd  as int.
def var i-tot-pron-fnd  as int.
def var i-tot-prog-item as int.
def var i-tot-proc-item as int.
def var i-tot-pron-item as int.
def var i-tot-prog-ger  as int.
def var i-tot-proc-ger  as int.
def var i-tot-pron-ger  as int.
def var i-tot-prog-proc as int.
def var c-descricao     as char format "x(30)".
DEF VAR c-desc-refer    AS CHAR FORMAT "x(3)".
def var c-compr-fabric  as char format "x(1)".
def var c-fundo         as char format "x(4)".
def var l-pulou         as log.
def var l-imprimir      as LOG INIT YES.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.desc-classifica    LABEL "Classifica‡Æo...." SKIP
    tt-param.fi-ini-it-codigo   label "Item............."
    "A"  AT 36
    tt-param.fi-fin-it-codigo   NO-LABELS SKIP
    tt-param.fi-ini-cod-refer   label "Referencia......."
    "A"  AT 36
    tt-param.fi-fin-cod-refer   NO-LABELS SKIP
    tt-param.fi-desenho         LABEL "Desenho.........."
    tt-param.l-inc-exc          NO-LABELS SKIP
    tt-param.l-tipo-rel         LABEL "Tipo de Relatorio" SKIP
    tt-param.c-imp-comp-fab     LABEL "Origem..........." FORMAT "x(15)" SKIP
    tt-param.c-imp-liso-est     LABEL "Acabamento......." FORMAT "x(15)" SKIP
    tt-param.tg-programado      LABEL "Fase Programado.." SKIP
    tt-param.tg-processo        LABEL "     Processo...." SKIP
    tt-param.tg-pronto          LABEL "     Pronto......" SKIP
    tt-param.tg-sem-estrutura   LABEL "So sem estrutura." SKIP
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
   ob-pcp.it-codigo       label "Item"
   c-descricao            label "Descricao"
   ob-pcp-ref.cod-refer   LABEL "Refer"       FORMAT "XX XXXX X"
   c-desc-refer           LABEL "Dsc"    
   c-compr-fabric         label "C/F"
   c-fundo                label "Fundo"
   ob-pcp-ref.qtd-prog  label "Programado"  format "->>>,>>9"
   ob-pcp-ref.qtd-proc  label "Processo"    format "->>>,>>9"
   i-tot-prog-proc        label "Prog+Proc"   format "->>>,>>9"
   ob-pcp-ref.qtd-pron  label "Pronto"      format "->>>,>>9"
   with no-box 55 down width 132 STREAM-IO frame f-detalhe no-label.

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
{utp/ut-liter.i Programacao_de_Produ‡Æo_com_Processo/Pronto * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).


IF tt-param.l-tipo-rel THEN DO:  /* Relatorio Detalhado */
   CASE tt-param.classifica. 
       WHEN 1 THEN DO.
            {esinc/espd0015rp.i "by ob-pcp.it-codigo by ob-pcp-ref.cod-refer"}.
       END.
       WHEN 2 THEN DO.
           {esinc/espd0015rp.i "by item.desc-item by ob-pcp.it-codigo BY ob-pcp-ref.cod-refer"}.
       END.
       WHEN 3 THEN DO.
           {esinc/espd0015rp.i "by substr(ob-pcp-ref.cod-refer,3,5) by ob-pcp.it-codigo by ob-pcp-ref.cod-refer"}.
       END.
       WHEN 4 THEN DO.
            {esinc/espd0015rp.i "by item.desc-item BY ob-pcp.it-codigo by ref-item-ext.cod-fundo by ob-pcp-ref.cod-refer"}.
       END.
   END CASE.

   /*
   IF tt-param.classifica = 1 THEN DO: /* 1 - Ordem C¢digo do Item */
      for each ob-pcp WHERE 
               ob-pcp.situacao = 1 AND
               ob-pcp.it-codigo >= tt-param.fi-ini-it-codigo AND
               ob-pcp.it-codigo <= tt-param.fi-fin-it-codigo NO-LOCK,
          EACH ob-pcp-ref OF ob-pcp WHERE
               ob-pcp-ref.cod-refer >= tt-param.fi-ini-cod-refer AND 
               ob-pcp-ref.cod-refer <= tt-param.fi-fin-cod-refer and
               ((substr(ob-pcp-ref.cod-refer,3,4)= tt-param.fi-desenho AND   
                 tt-param.l-inc-exc = yes) or
                (substr(ob-pcp-ref.cod-refer,3,4) <> tt-param.fi-desenho and 
                 tt-param.l-inc-exc = no)  or
                (tt-param.fi-desenho = "")) no-lock
                 break by ob-pcp.it-codigo
                       by ob-pcp-ref.cod-refer:
                      
          RUN pi-acompanhar in h-acomp (input ob-pcp.it-codigo + " " + ob-pcp-ref.cod-refer).
      
          FIND item WHERE 
               item.it-codigo = ob-pcp.it-codigo NO-LOCK NO-ERROR.

          FIND ref-item-ext WHERE
               ref-item-ext.it-codigo = ob-pcp.it-codigo AND
               ref-item-ext.cod-refer = ob-pcp-ref.cod-refer 
               NO-LOCK NO-ERROR.
      
          ASSIGN l-imprimir = YES.
      
          if avail item then
             if (item.compr-fabric = 1 and tt-param.c-imp-comp-fab = "C")
             or (item.compr-fabric = 2 and tt-param.c-imp-comp-fab = "F") then
             assign l-imprimir = no.
      
          if avail item then
             if (substr(ob-pcp-ref.cod-refer,7,1) =  "0" and tt-param.c-imp-liso-est = "E")
             or (substr(ob-pcp-ref.cod-refer,7,1) <> "0" AND tt-param.c-imp-liso-est = "L") then 
                assign l-imprimir = no.
                
          IF ob-pcp-ref.qtd-prog = 0 AND  
             ob-pcp-ref.qtd-proc = 0 AND 
             ob-pcp-ref.qtd-pron = 0 THEN 
             ASSIGN l-imprimir = NO.
      
          find first ref-estrut 
               where ref-estrut.it-codigo  = ob-pcp.it-codigo 
                 and ref-estrut.cod-ref-it = ob-pcp-ref.cod-refer
               no-lock no-error.
           
          if avail ref-estrut and tt-param.tg-sem-estrutura = yes then 
             assign l-imprimir = no.
           
          if l-imprimir = yes then do:
             assign l-imprimir = no.
             if  tt-param.tg-programado = yes and ob-pcp-ref.qtd-prog <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-processo = yes and ob-pcp-ref.qtd-proc <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-pronto = yes and ob-pcp-ref.qtd-pron <> 0 then
                 assign l-imprimir = yes. 
          end.
              
          if l-imprimir then
             assign i-tot-prog-item = i-tot-prog-item + ob-pcp-ref.qtd-prog
                    i-tot-proc-item = i-tot-proc-item + ob-pcp-ref.qtd-proc
                    i-tot-pron-item = i-tot-pron-item + ob-pcp-ref.qtd-pron
                    i-tot-prog-ger  = i-tot-prog-ger + ob-pcp-ref.qtd-prog
                    i-tot-proc-ger  = i-tot-proc-ger + ob-pcp-ref.qtd-proc
                    i-tot-pron-ger  = i-tot-pron-ger + ob-pcp-ref.qtd-pron.
          
          if last-of(ob-pcp-ref.cod-refer) and l-imprimir then do:
             assign c-fundo = if avail ob-pcp-ref
                              then ref-item-ext.cod-fundo
                              else "".
                                     
             if avail item then
                ASSIGN c-compr-fabric = (if item.compr-fabric = 1 then "C" else "F").
             ELSE
                assign c-compr-fabric = "".
        
             if ob-pcp-ref.qtd-prog <> 0 or ob-pcp-ref.qtd-proc <> 0 or
                ob-pcp-ref.qtd-pron <> 0 then do:
                assign i-tot-prog-proc = ob-pcp-ref.qtd-prog +
                                         ob-pcp-ref.qtd-proc.
                find referencia where
                     referencia.cod-refer = ob-pcp-ref.cod-refer no-lock no-error.
                IF AVAIL referencia THEN 
                   ASSIGN c-desc-refer = SUBSTR(referencia.descricao,1,3).
                ELSE 
                   ASSIGN c-desc-refer = "".
                   
                FIND ITEM WHERE 
                    ITEM.it-codigo = ob-pcp.it-codigo NO-LOCK NO-ERROR.
                if avail item then
                   assign c-descricao = item.descricao-1 + item.descricao-2.
                else
                   assign c-descricao = "*** Item nao cadastrado ***".
      
                display ob-pcp.it-codigo
                        c-descricao
                        ob-pcp-ref.cod-refer
                        c-desc-refer
                        c-compr-fabric
                        c-fundo     
                        ob-pcp-ref.qtd-prog
                        ob-pcp-ref.qtd-proc
                        i-tot-prog-proc
                        ob-pcp-ref.qtd-pron
                        with frame f-detalhe.
                down with frame f-detalhe.
                assign l-pulou = no.
             end.
          end.
        
          if last-of(ob-pcp.it-codigo) and
             (i-tot-prog-item <> 0 or
              i-tot-proc-item <> 0 or
              i-tot-pron-item <> 0) then do:
             assign i-tot-prog-proc = i-tot-prog-item + i-tot-proc-item.
             display "Total do Item"  @ ob-pcp.it-codigo
                     i-tot-prog-item  @ ob-pcp-ref.qtd-prog
                     i-tot-proc-item  @ ob-pcp-ref.qtd-proc
                     i-tot-prog-proc
                     i-tot-pron-item  @ ob-pcp-ref.qtd-pron
                     with frame f-detalhe.
             down 2 with frame f-detalhe.
             assign i-tot-prog-item = 0
                    i-tot-proc-item = 0
                    i-tot-pron-item = 0
                    l-pulou        = yes.
          end.
          else
             if last-of(ob-pcp.it-codigo)
             and not l-pulou then do:
                 put skip(1).
                 assign l-pulou = yes.
             end.
      end. /* ob-pcp-ref */
   END.
   ELSE
   IF tt-param.classifica = 2 THEN DO: /* 2 - Ordem Descri‡Æo do Item */
      for each ob-pcp-ref WHERE 
               ob-pcp-ref.it-codigo >= tt-param.fi-ini-it-codigo AND
               ob-pcp-ref.it-codigo <= tt-param.fi-fin-it-codigo AND 
               ob-pcp-ref.cod-refer >= tt-param.fi-ini-cod-refer AND 
               ob-pcp-ref.cod-refer <= tt-param.fi-fin-cod-refer and
               ((substr(ob-pcp-ref.cod-refer,3,4)= tt-param.fi-desenho AND   
                 tt-param.l-inc-exc = yes) or
                (substr(ob-pcp-ref.cod-refer,3,4) <> tt-param.fi-desenho and 
                 tt-param.l-inc-exc = no)  or
                (tt-param.fi-desenho = "")) no-lock,
          EACH ITEM WHERE ITEM.it-codigo = ob-pcp-ref.it-codigo NO-LOCK

          break by item.desc-item
                BY ob-pcp-ref.it-codigo
                by ob-pcp-ref.cod-refer:
      
          RUN pi-acompanhar in h-acomp (input ob-pcp-ref.it-codigo + " " + ob-pcp-ref.cod-refer).
      
          ASSIGN l-imprimir = YES.
      
          if (item.compr-fabric = 1 and tt-param.c-imp-comp-fab = "C")
          or (item.compr-fabric = 2 and tt-param.c-imp-comp-fab = "F") then
          assign l-imprimir = no.
      
          if (substr(ob-pcp-ref.cod-refer,7,1) =  "0" and tt-param.c-imp-liso-est = "E")
          or (substr(ob-pcp-ref.cod-refer,7,1) <> "0" AND tt-param.c-imp-liso-est = "L") then 
             assign l-imprimir = no.
      
          IF ob-pcp-ref.qtd-prog = 0 AND  
             ob-pcp-ref.qtd-proc = 0 AND 
             ob-pcp-ref.qtd-pron = 0 THEN 
             ASSIGN l-imprimir = NO.
      
          find first ref-estrut 
               where ref-estrut.it-codigo  = ob-pcp-ref.it-codigo 
                 and ref-estrut.cod-ref-it = ob-pcp-ref.cod-refer
               no-lock no-error.
      
          if avail ref-estrut and tt-param.tg-sem-estrutura = yes then 
             assign l-imprimir = no.
      
          if l-imprimir = yes then do:
             assign l-imprimir = no.
             if  tt-param.tg-programado = yes and ob-pcp-ref.qtd-prog <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-processo = yes and ob-pcp-ref.qtd-proc <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-pronto = yes and ob-pcp-ref.qtd-pron <> 0 then
                 assign l-imprimir = yes. 
          end.
      
          if l-imprimir then
             assign i-tot-prog-item = i-tot-prog-item + ob-pcp-ref.qtd-prog
                    i-tot-proc-item = i-tot-proc-item + ob-pcp-ref.qtd-proc
                    i-tot-pron-item = i-tot-pron-item + ob-pcp-ref.qtd-pron
                    i-tot-prog-ger  = i-tot-prog-ger + ob-pcp-ref.qtd-prog
                    i-tot-proc-ger  = i-tot-proc-ger + ob-pcp-ref.qtd-proc
                    i-tot-pron-ger  = i-tot-pron-ger + ob-pcp-ref.qtd-pron.
      
          if last-of(ob-pcp-ref.cod-refer) and l-imprimir then do:
             assign c-fundo = if avail ob-pcp-ref
                              then ob-pcp-ref.cod-fundo
                              else "".
      
             if avail item then
                ASSIGN c-compr-fabric = (if item.compr-fabric = 1 then "C" else "F").
             ELSE
                assign c-compr-fabric = "".
      
             if ob-pcp-ref.qtd-prog <> 0 or ob-pcp-ref.qtd-proc <> 0 or
                ob-pcp-ref.qtd-pron <> 0 then do:
                assign i-tot-prog-proc = ob-pcp-ref.qtd-prog +
                                         ob-pcp-ref.qtd-proc.
                find referencia where
                     referencia.cod-refer = ob-pcp-ref.cod-refer no-lock no-error.
                IF AVAIL referencia THEN 
                   ASSIGN c-desc-refer = SUBSTR(referencia.descricao,1,3).
                ELSE 
                   ASSIGN c-desc-refer = "".
      
                assign c-descricao = item.descricao-1 + item.descricao-2.
      
                display ob-pcp-ref.it-codigo
                        c-descricao
                        ob-pcp-ref.cod-refer
                        c-desc-refer
                        c-compr-fabric
                        c-fundo     
                        ob-pcp-ref.qtd-prog
                        ob-pcp-ref.qtd-proc
                        i-tot-prog-proc
                        ob-pcp-ref.qtd-pron
                        with frame f-detalhe.
                down with frame f-detalhe.
                assign l-pulou = no.
             end.
          end.
      
          if last-of(ob-pcp-ref.it-codigo) and
             (i-tot-prog-item <> 0 or
              i-tot-proc-item <> 0 or
              i-tot-pron-item <> 0) then do:
             assign i-tot-prog-proc = i-tot-prog-item + i-tot-proc-item.
             display "Total do Item"  @ ob-pcp-ref.it-codigo
                     i-tot-prog-item  @ ob-pcp-ref.qtd-prog
                     i-tot-proc-item  @ ob-pcp-ref.qtd-proc
                     i-tot-prog-proc
                     i-tot-pron-item  @ ob-pcp-ref.qtd-pron
                     with frame f-detalhe.
             down 2 with frame f-detalhe.
             assign i-tot-prog-item = 0
                    i-tot-proc-item = 0
                    i-tot-pron-item = 0
                    l-pulou        = yes.
          end.
          else
             if last-of(ob-pcp-ref.it-codigo)
             and not l-pulou then do:
                 put skip(1).
                 assign l-pulou = yes.
             end.
      
      END. /* ob-pcp-ref */
   END.
   ELSE
   IF tt-param.classifica = 3 THEN DO: /* 3 - Ordem Cor/Desenho/Variante */
      for each ob-pcp-ref WHERE 
               ob-pcp-ref.it-codigo >= tt-param.fi-ini-it-codigo AND
               ob-pcp-ref.it-codigo <= tt-param.fi-fin-it-codigo AND 
               ob-pcp-ref.cod-refer >= tt-param.fi-ini-cod-refer AND 
               ob-pcp-ref.cod-refer <= tt-param.fi-fin-cod-refer and
               ((substr(ob-pcp-ref.cod-refer,3,4)= tt-param.fi-desenho AND   
                 tt-param.l-inc-exc = yes) or
                (substr(ob-pcp-ref.cod-refer,3,4) <> tt-param.fi-desenho and 
                 tt-param.l-inc-exc = no)  or
                (tt-param.fi-desenho = "")) no-lock,
          EACH ITEM WHERE ITEM.it-codigo = ob-pcp-ref.it-codigo NO-LOCK

          break by substr(ob-pcp-ref.cod-refer,3,5)
                BY ob-pcp-ref.it-codigo
                BY ob-pcp-ref.cod-refer:

          RUN pi-acompanhar in h-acomp (input ob-pcp-ref.it-codigo + " " + ob-pcp-ref.cod-refer).

          ASSIGN l-imprimir = YES.

          if (item.compr-fabric = 1 and tt-param.c-imp-comp-fab = "C")
          or (item.compr-fabric = 2 and tt-param.c-imp-comp-fab = "F") then
          assign l-imprimir = no.

          if (substr(ob-pcp-ref.cod-refer,7,1) =  "0" and tt-param.c-imp-liso-est = "E")
          or (substr(ob-pcp-ref.cod-refer,7,1) <> "0" AND tt-param.c-imp-liso-est = "L") then 
             assign l-imprimir = no.

          IF ob-pcp-ref.qtd-prog = 0 AND  
             ob-pcp-ref.qtd-proc = 0 AND 
             ob-pcp-ref.qtd-pron = 0 THEN 
             ASSIGN l-imprimir = NO.

          find first ref-estrut 
               where ref-estrut.it-codigo  = ob-pcp-ref.it-codigo 
                 and ref-estrut.cod-ref-it = ob-pcp-ref.cod-refer
               no-lock no-error.

          if avail ref-estrut and tt-param.tg-sem-estrutura = yes then 
             assign l-imprimir = no.

          if l-imprimir = yes then do:
             assign l-imprimir = no.
             if  tt-param.tg-programado = yes and ob-pcp-ref.qtd-prog <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-processo = yes and ob-pcp-ref.qtd-proc <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-pronto = yes and ob-pcp-ref.qtd-pron <> 0 then
                 assign l-imprimir = yes. 
          end.

          if l-imprimir then
             assign i-tot-prog-item = i-tot-prog-item + ob-pcp-ref.qtd-prog
                    i-tot-proc-item = i-tot-proc-item + ob-pcp-ref.qtd-proc
                    i-tot-pron-item = i-tot-pron-item + ob-pcp-ref.qtd-pron
                    i-tot-prog-ger  = i-tot-prog-ger + ob-pcp-ref.qtd-prog
                    i-tot-proc-ger  = i-tot-proc-ger + ob-pcp-ref.qtd-proc
                    i-tot-pron-ger  = i-tot-pron-ger + ob-pcp-ref.qtd-pron.

          if last-of(ob-pcp-ref.cod-refer) and l-imprimir then do:
             assign c-fundo = if avail ob-pcp-ref
                              then ob-pcp-ref.cod-fundo
                              else "".

             if avail item then
                ASSIGN c-compr-fabric = (if item.compr-fabric = 1 then "C" else "F").
             ELSE
                assign c-compr-fabric = "".

             if ob-pcp-ref.qtd-prog <> 0 or ob-pcp-ref.qtd-proc <> 0 or
                ob-pcp-ref.qtd-pron <> 0 then do:
                assign i-tot-prog-proc = ob-pcp-ref.qtd-prog +
                                         ob-pcp-ref.qtd-proc.
                find referencia where
                     referencia.cod-refer = ob-pcp-ref.cod-refer no-lock no-error.
                IF AVAIL referencia THEN 
                   ASSIGN c-desc-refer = SUBSTR(referencia.descricao,1,3).
                ELSE 
                   ASSIGN c-desc-refer = "".

                assign c-descricao = item.descricao-1 + item.descricao-2.

                display ob-pcp-ref.it-codigo
                        c-descricao
                        ob-pcp-ref.cod-refer
                        c-desc-refer
                        c-compr-fabric
                        c-fundo     
                        ob-pcp-ref.qtd-prog
                        ob-pcp-ref.qtd-proc
                        i-tot-prog-proc
                        ob-pcp-ref.qtd-pron
                        with frame f-detalhe.
                down with frame f-detalhe.
                assign l-pulou = no.
             end.
          end.

          if last-of(substr(ob-pcp-ref.cod-refer,3,5)) and
             (i-tot-prog-item <> 0 or
              i-tot-proc-item <> 0 or
              i-tot-pron-item <> 0) then do:
             assign i-tot-prog-proc = i-tot-prog-item + i-tot-proc-item.
             display "Total do Item"  @ ob-pcp-ref.it-codigo
                     i-tot-prog-item  @ ob-pcp-ref.qtd-prog
                     i-tot-proc-item  @ ob-pcp-ref.qtd-proc
                     i-tot-prog-proc
                     i-tot-pron-item  @ ob-pcp-ref.qtd-pron
                     with frame f-detalhe.
             down 2 with frame f-detalhe.
             assign i-tot-prog-item = 0
                    i-tot-proc-item = 0
                    i-tot-pron-item = 0
                    l-pulou        = yes.
          end.
          else
             if last-of(substr(ob-pcp-ref.cod-refer,3,5))
             and not l-pulou then do:
                 put skip(1).
                 assign l-pulou = yes.
             end.

      END. /* ob-pcp-ref */
   END.
   ELSE DO: /* 4 - Ordem Descri‡Æo do Item/Fundo/Refer */
      for each ob-pcp-ref WHERE 
               ob-pcp-ref.it-codigo >= tt-param.fi-ini-it-codigo AND
               ob-pcp-ref.it-codigo <= tt-param.fi-fin-it-codigo AND 
               ob-pcp-ref.cod-refer >= tt-param.fi-ini-cod-refer AND 
               ob-pcp-ref.cod-refer <= tt-param.fi-fin-cod-refer and
               ((substr(ob-pcp-ref.cod-refer,3,4)= tt-param.fi-desenho AND   
                 tt-param.l-inc-exc = yes) or
                (substr(ob-pcp-ref.cod-refer,3,4) <> tt-param.fi-desenho and 
                 tt-param.l-inc-exc = no)  or
                (tt-param.fi-desenho = "")) no-lock,
          EACH ITEM WHERE ITEM.it-codigo = ob-pcp-ref.it-codigo NO-LOCK

          break by item.desc-item
                BY ob-pcp-ref.it-codigo
                by ob-pcp-ref.cod-fundo
                by ob-pcp-ref.cod-refer:
      
          RUN pi-acompanhar in h-acomp (input ob-pcp-ref.it-codigo + " " + ob-pcp-ref.cod-refer).
      
          ASSIGN l-imprimir = YES.
      
          if (item.compr-fabric = 1 and tt-param.c-imp-comp-fab = "C")
          or (item.compr-fabric = 2 and tt-param.c-imp-comp-fab = "F") then
          assign l-imprimir = no.
      
          if (substr(ob-pcp-ref.cod-refer,7,1) =  "0" and tt-param.c-imp-liso-est = "E")
          or (substr(ob-pcp-ref.cod-refer,7,1) <> "0" AND tt-param.c-imp-liso-est = "L") then 
             assign l-imprimir = no.
      
          IF ob-pcp-ref.qtd-prog = 0 AND  
             ob-pcp-ref.qtd-proc = 0 AND 
             ob-pcp-ref.qtd-pron = 0 THEN 
             ASSIGN l-imprimir = NO.
      
          find first ref-estrut 
               where ref-estrut.it-codigo  = ob-pcp-ref.it-codigo 
                 and ref-estrut.cod-ref-it = ob-pcp-ref.cod-refer
               no-lock no-error.
      
          if avail ref-estrut and tt-param.tg-sem-estrutura = yes then 
             assign l-imprimir = no.
      
          if l-imprimir = yes then do:
             assign l-imprimir = no.
             if  tt-param.tg-programado = yes and ob-pcp-ref.qtd-prog <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-processo = yes and ob-pcp-ref.qtd-proc <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-pronto = yes and ob-pcp-ref.qtd-pron <> 0 then
                 assign l-imprimir = yes. 
          end.
      
          if l-imprimir then
             assign i-tot-prog-item = i-tot-prog-item + ob-pcp-ref.qtd-prog
                    i-tot-proc-item = i-tot-proc-item + ob-pcp-ref.qtd-proc
                    i-tot-pron-item = i-tot-pron-item + ob-pcp-ref.qtd-pron
                    i-tot-prog-fnd  = i-tot-prog-fnd + ob-pcp-ref.qtd-prog
                    i-tot-proc-fnd  = i-tot-proc-fnd + ob-pcp-ref.qtd-proc
                    i-tot-pron-fnd  = i-tot-pron-fnd + ob-pcp-ref.qtd-pron
                    i-tot-prog-ger  = i-tot-prog-ger + ob-pcp-ref.qtd-prog
                    i-tot-proc-ger  = i-tot-proc-ger + ob-pcp-ref.qtd-proc
                    i-tot-pron-ger  = i-tot-pron-ger + ob-pcp-ref.qtd-pron.
      
          if last-of(ob-pcp-ref.cod-refer) and l-imprimir then do:
             assign c-fundo = if avail ob-pcp-ref
                              then ob-pcp-ref.cod-fundo
                              else "".
      
             if avail item then
                ASSIGN c-compr-fabric = (if item.compr-fabric = 1 then "C" else "F").
             ELSE
                assign c-compr-fabric = "".
      
             if ob-pcp-ref.qtd-prog <> 0 or ob-pcp-ref.qtd-proc <> 0 or
                ob-pcp-ref.qtd-pron <> 0 then do:
                assign i-tot-prog-proc = ob-pcp-ref.qtd-prog +
                                         ob-pcp-ref.qtd-proc.
                find referencia where
                     referencia.cod-refer = ob-pcp-ref.cod-refer no-lock no-error.
                IF AVAIL referencia THEN 
                   ASSIGN c-desc-refer = SUBSTR(referencia.descricao,1,3).
                ELSE 
                   ASSIGN c-desc-refer = "".
      
                assign c-descricao = item.descricao-1 + item.descricao-2.
      
                display ob-pcp-ref.it-codigo
                        c-descricao
                        ob-pcp-ref.cod-refer
                        c-desc-refer
                        c-compr-fabric
                        c-fundo     
                        ob-pcp-ref.qtd-prog
                        ob-pcp-ref.qtd-proc
                        i-tot-prog-proc
                        ob-pcp-ref.qtd-pron
                        with frame f-detalhe.
                down with frame f-detalhe.
                assign l-pulou = no.
             end.
          end.
      
          IF LAST-OF(ob-pcp-ref.cod-fundo) AND
             (i-tot-prog-fnd <> 0 or
              i-tot-proc-fnd <> 0 or
              i-tot-pron-fnd <> 0) then do:
             assign i-tot-prog-proc = i-tot-prog-fnd + i-tot-proc-fnd.
             display "Total do Fundo" @ ob-pcp-ref.it-codigo
                     i-tot-prog-fnd   @ ob-pcp-ref.qtd-prog
                     i-tot-proc-fnd   @ ob-pcp-ref.qtd-proc
                     i-tot-prog-proc
                     i-tot-pron-fnd   @ ob-pcp-ref.qtd-pron
                     with frame f-detalhe.
             down 2 with frame f-detalhe.
             assign i-tot-prog-fnd = 0
                    i-tot-proc-fnd = 0
                    i-tot-pron-fnd = 0
                    l-pulou        = yes.
          END.
          else
             if last-of(ob-pcp-ref.cod-fundo)
             and not l-pulou then do:
                 put skip(1).
                 assign l-pulou = yes.
             end.
          
          if last-of(ob-pcp-ref.it-codigo) and
             (i-tot-prog-item <> 0 or
              i-tot-proc-item <> 0 or
              i-tot-pron-item <> 0) then do:
             assign i-tot-prog-proc = i-tot-prog-item + i-tot-proc-item.
             display "Total do Item"  @ ob-pcp-ref.it-codigo
                     i-tot-prog-item  @ ob-pcp-ref.qtd-prog
                     i-tot-proc-item  @ ob-pcp-ref.qtd-proc
                     i-tot-prog-proc
                     i-tot-pron-item  @ ob-pcp-ref.qtd-pron
                     with frame f-detalhe.
             down 2 with frame f-detalhe.
             assign i-tot-prog-item = 0
                    i-tot-proc-item = 0
                    i-tot-pron-item = 0
                    l-pulou        = yes.
          end.
          else
             if last-of(ob-pcp-ref.it-codigo)
             and not l-pulou then do:
                 put skip(1).
                 assign l-pulou = yes.
             end.
      
      END. /* ob-pcp-ref */
   END.
   */
   
END. /* Fim - Detalhado */

ELSE DO: /* Inicio - Resumido */

    /*
    CASE tt-param.classifica. 
        WHEN 1 THEN DO.
             {esinc/espd0015rp.i2 "by ob-pcp.it-codigo by ob-pcp-ref.cod-refer"}.
        END.
        WHEN 2 THEN DO.
            {esinc/espd0015rp.i2 "by item.desc-item by ob-pcp.it-codigo BY ob-pcp-ref.cod-refer"}.
        END.
        WHEN 3 THEN DO.
            {esinc/espd0015rp.i2 "by substr(ob-pcp-ref.cod-refer,3,5) by ob-pcp.it-codigo by ob-pcp-ref.cod-refer"}.
        END.
        WHEN 4 THEN DO.
             {esinc/espd0015rp.i2 "by item.desc-item BY ob-pcp.it-codigo by ref-item-ext.cod-fundo by ob-pcp-ref.cod-refer"}.
        END.
    END CASE.
    */
      
   /*
   IF tt-param.classifica = 1 THEN DO: /* 1 - Ordem C¢digo do Item */
      FOR EACH ob-pcp-ref WHERE 
               ob-pcp-ref.it-codigo >= tt-param.fi-ini-it-codigo AND
               ob-pcp-ref.it-codigo <= tt-param.fi-fin-it-codigo AND 
               ob-pcp-ref.cod-refer >= tt-param.fi-ini-cod-refer AND 
               ob-pcp-ref.cod-refer <= tt-param.fi-fin-cod-refer AND 
               ((substr(ob-pcp-ref.cod-refer,3,4)= tt-param.fi-desenho and
                 tt-param.l-inc-exc = yes) or
                (substr(ob-pcp-ref.cod-refer,3,4) <> tt-param.fi-desenho and 
                 tt-param.l-inc-exc = no) OR (tt-param.fi-desenho = "")) NO-LOCK,
          each item where
               item.it-codigo = ob-pcp-ref.it-codigo NO-LOCK
               break by ob-pcp-ref.it-codigo
                     by ob-pcp-ref.cod-fundo:
      
          assign l-imprimir = yes.
          
          if avail item then
             if (item.compr-fabric = 1 and tt-param.c-imp-comp-fab = "C")
             or (item.compr-fabric = 2 and tt-param.c-imp-comp-fab = "F") then
             assign l-imprimir = no.
      
          if avail item then
             if (substr(ob-pcp-ref.cod-refer,7,1) =  "0" and tt-param.c-imp-liso-est = "E")
             or (substr(ob-pcp-ref.cod-refer,7,1) <> "0" AND tt-param.c-imp-liso-est = "L") then 
                assign l-imprimir = no.
      
          if  ob-pcp-ref.qtd-prog = 0
          and ob-pcp-ref.qtd-proc = 0
          and ob-pcp-ref.qtd-pron = 0 then
              assign l-imprimir    = no.
          
          if l-imprimir = yes then do:
             assign l-imprimir = no.
             if  tt-param.tg-programado = yes and ob-pcp-ref.qtd-prog <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-processo = yes and ob-pcp-ref.qtd-proc <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-pronto = yes and ob-pcp-ref.qtd-pron <> 0 then
                 assign l-imprimir = yes. 
          end.
          /*
          find first ref-estrut 
               where ref-estrut.it-codigo = ob-pcp-ref.it-codigo 
                 and ref-estrut.al-codigo = ob-pcp-ref.cod-refer
               no-lock no-error.
           
          if avail ref-estrut and tt-param.tg-sem-estrutura = yes then 
             assign l-imprimir = no.
          */
          if l-imprimir then
             assign i-tot-prog-fnd = i-tot-prog-fnd + ob-pcp-ref.qtd-prog
                    i-tot-proc-fnd = i-tot-proc-fnd + ob-pcp-ref.qtd-proc
                    i-tot-pron-fnd = i-tot-pron-fnd + ob-pcp-ref.qtd-pron
                    i-tot-prog-item = i-tot-prog-item + ob-pcp-ref.qtd-prog
                    i-tot-proc-item = i-tot-proc-item + ob-pcp-ref.qtd-proc
                    i-tot-pron-item = i-tot-pron-item + ob-pcp-ref.qtd-pron
                    i-tot-prog-ger = i-tot-prog-ger + ob-pcp-ref.qtd-prog
                    i-tot-proc-ger = i-tot-proc-ger + ob-pcp-ref.qtd-proc
                    i-tot-pron-ger = i-tot-pron-ger + ob-pcp-ref.qtd-pron.
      
          if first-of(ob-pcp-ref.it-codigo) then do.
             if avail item then
                assign c-descricao = item.descricao-1 + item.descricao-2.
             else
                assign c-descricao = "*** Item nao cadastrado ***".
         
             display ob-pcp-ref.it-codigo
                     c-descricao
                     with frame f-detalhe.
          end.
      
          if last-of(ob-pcp-ref.cod-fundo) then do:
             assign c-fundo        = ob-pcp-ref.cod-fundo
                    c-compr-fabric = (if item.compr-fabric = 1 then "C" else "F").
             if i-tot-prog-fnd <> 0
             or i-tot-proc-fnd <> 0
             or i-tot-pron-fnd <> 0 then do:
                assign i-tot-prog-proc = i-tot-prog-fnd +
                                         i-tot-proc-fnd.
                display /*ob-pcp-ref.cod-refer
                        c-compr-fabric*/
                        c-fundo
                        i-tot-prog-fnd @ ob-pcp-ref.qtd-prog
                        i-tot-proc-fnd @ ob-pcp-ref.qtd-proc
                        i-tot-prog-proc
                        i-tot-pron-fnd @ ob-pcp-ref.qtd-pron
                        with frame f-detalhe.
                down with frame f-detalhe.
                assign l-pulou        = no
                       i-tot-prog-fnd = 0
                       i-tot-proc-fnd = 0
                       i-tot-pron-fnd = 0.
             end.
          end.
        
          if last-of(ob-pcp-ref.it-codigo)
          and (i-tot-prog-item <> 0 or
               i-tot-proc-item <> 0 or
               i-tot-pron-item <> 0) then do:
             assign i-tot-prog-proc = i-tot-prog-item +
                                      i-tot-proc-item.
             display "Total Item"     @ ob-pcp-ref.it-codigo
                     i-tot-prog-item  @ ob-pcp-ref.qtd-prog
                     i-tot-proc-item  @ ob-pcp-ref.qtd-proc
                     i-tot-prog-proc
                     i-tot-pron-item  @ ob-pcp-ref.qtd-pron
                     with frame f-detalhe.
             down 2 with frame f-detalhe.
             assign i-tot-prog-item = 0
                    i-tot-proc-item = 0
                    i-tot-pron-item = 0
                    l-pulou        = yes.
          end.
          else
             if last-of(ob-pcp-ref.it-codigo)
             and not l-pulou then do:
                 put skip(1).
                 assign l-pulou = yes.
             end.
      
      end. /* ob-pcp-ref */
   END.
   ELSE
   IF tt-param.classifica = 2 THEN DO: /* 2 - Ordem Descri‡Æo do Item */
      FOR EACH ob-pcp-ref WHERE 
               ob-pcp-ref.it-codigo >= tt-param.fi-ini-it-codigo AND
               ob-pcp-ref.it-codigo <= tt-param.fi-fin-it-codigo AND 
               ob-pcp-ref.cod-refer >= tt-param.fi-ini-cod-refer AND 
               ob-pcp-ref.cod-refer <= tt-param.fi-fin-cod-refer AND 
               ((substr(ob-pcp-ref.cod-refer,3,4)= tt-param.fi-desenho and
                 tt-param.l-inc-exc = yes) or
                (substr(ob-pcp-ref.cod-refer,3,4) <> tt-param.fi-desenho and 
                 tt-param.l-inc-exc = no) OR (tt-param.fi-desenho = "")) NO-LOCK,
          each item where
               item.it-codigo = ob-pcp-ref.it-codigo NO-LOCK
               break BY ITEM.desc-item
                     by ob-pcp-ref.it-codigo
                     by ob-pcp-ref.cod-fundo:
      
          assign l-imprimir = yes.
      
          if (item.compr-fabric = 1 and tt-param.c-imp-comp-fab = "C")
          or (item.compr-fabric = 2 and tt-param.c-imp-comp-fab = "F") then
             assign l-imprimir = no.
      
          if (substr(ob-pcp-ref.cod-refer,7,1) =  "0" and tt-param.c-imp-liso-est = "E")
          or (substr(ob-pcp-ref.cod-refer,7,1) <> "0" AND tt-param.c-imp-liso-est = "L") then 
             assign l-imprimir = no.
      
          if  ob-pcp-ref.qtd-prog = 0
          and ob-pcp-ref.qtd-proc = 0
          and ob-pcp-ref.qtd-pron = 0 then
              assign l-imprimir    = no.
      
          if l-imprimir = yes then do:
             assign l-imprimir = no.
             if  tt-param.tg-programado = yes and ob-pcp-ref.qtd-prog <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-processo = yes and ob-pcp-ref.qtd-proc <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-pronto = yes and ob-pcp-ref.qtd-pron <> 0 then
                 assign l-imprimir = yes. 
          end.
          /*
          find first ref-estrut 
               where ref-estrut.it-codigo = ob-pcp-ref.it-codigo 
                 and ref-estrut.al-codigo = ob-pcp-ref.cod-refer
               no-lock no-error.
      
          if avail ref-estrut and tt-param.tg-sem-estrutura = yes then 
             assign l-imprimir = no.
          */
          if l-imprimir then
             assign i-tot-prog-fnd = i-tot-prog-fnd + ob-pcp-ref.qtd-prog
                    i-tot-proc-fnd = i-tot-proc-fnd + ob-pcp-ref.qtd-proc
                    i-tot-pron-fnd = i-tot-pron-fnd + ob-pcp-ref.qtd-pron
                    i-tot-prog-item = i-tot-prog-item + ob-pcp-ref.qtd-prog
                    i-tot-proc-item = i-tot-proc-item + ob-pcp-ref.qtd-proc
                    i-tot-pron-item = i-tot-pron-item + ob-pcp-ref.qtd-pron
                    i-tot-prog-ger = i-tot-prog-ger + ob-pcp-ref.qtd-prog
                    i-tot-proc-ger = i-tot-proc-ger + ob-pcp-ref.qtd-proc
                    i-tot-pron-ger = i-tot-pron-ger + ob-pcp-ref.qtd-pron.
      
          if first-of(ob-pcp-ref.it-codigo) then do.
             assign c-descricao = item.descricao-1 + item.descricao-2.
      
             display item.it-codigo @ ob-pcp-ref.it-codigo
                     c-descricao
                     with frame f-detalhe.
          end.
      
          if last-of(ob-pcp-ref.cod-fundo) then do:
             assign c-fundo        = ob-pcp-ref.cod-fundo
                    c-compr-fabric = (if item.compr-fabric = 1 then "C" else "F").
             if i-tot-prog-fnd <> 0
             or i-tot-proc-fnd <> 0
             or i-tot-pron-fnd <> 0 then do:
                assign i-tot-prog-proc = i-tot-prog-fnd +
                                         i-tot-proc-fnd.
                display /*ob-pcp-ref.cod-refer
                        c-compr-fabric*/
                        c-fundo
                        i-tot-prog-fnd @ ob-pcp-ref.qtd-prog
                        i-tot-proc-fnd @ ob-pcp-ref.qtd-proc
                        i-tot-prog-proc
                        i-tot-pron-fnd @ ob-pcp-ref.qtd-pron
                        with frame f-detalhe.
                down with frame f-detalhe.
                assign l-pulou        = no
                       i-tot-prog-fnd = 0
                       i-tot-proc-fnd = 0
                       i-tot-pron-fnd = 0.
             end.
          end.
      
          if last-of(ob-pcp-ref.it-codigo)
          and (i-tot-prog-item <> 0 or
               i-tot-proc-item <> 0 or
               i-tot-pron-item <> 0) then do:
             assign i-tot-prog-proc = i-tot-prog-item +
                                      i-tot-proc-item.
             display "Total Item"    @ ob-pcp-ref.it-codigo
                     i-tot-prog-item  @ ob-pcp-ref.qtd-prog
                     i-tot-proc-item  @ ob-pcp-ref.qtd-proc
                     i-tot-prog-proc
                     i-tot-pron-item  @ ob-pcp-ref.qtd-pron
                     with frame f-detalhe.
             down 2 with frame f-detalhe.
             assign i-tot-prog-item = 0
                    i-tot-proc-item = 0
                    i-tot-pron-item = 0
                    l-pulou        = yes.
          end.
          else
             if last-of(ob-pcp-ref.it-codigo)
             and not l-pulou then do:
                 put skip(1).
                 assign l-pulou = yes.
             end.
      end. /* ob-pcp-ref */
   END.
   ELSE 
   IF tt-param.classifica = 3 THEN DO: /* 3 - Ordem Cor/Desenho/Variante */
      FOR EACH ob-pcp-ref WHERE 
               ob-pcp-ref.it-codigo >= tt-param.fi-ini-it-codigo AND
               ob-pcp-ref.it-codigo <= tt-param.fi-fin-it-codigo AND 
               ob-pcp-ref.cod-refer >= tt-param.fi-ini-cod-refer AND 
               ob-pcp-ref.cod-refer <= tt-param.fi-fin-cod-refer AND 
               ((substr(ob-pcp-ref.cod-refer,3,4)= tt-param.fi-desenho and
                 tt-param.l-inc-exc = yes) or
                (substr(ob-pcp-ref.cod-refer,3,4) <> tt-param.fi-desenho and 
                 tt-param.l-inc-exc = no) OR (tt-param.fi-desenho = "")) NO-LOCK,
          each item where
               item.it-codigo = ob-pcp-ref.it-codigo NO-LOCK
               break BY substr(ob-pcp-ref.cod-refer,3,5)
                     by ob-pcp-ref.it-codigo
                     by ob-pcp-ref.cod-fundo:

          assign l-imprimir = yes.

          if (item.compr-fabric = 1 and tt-param.c-imp-comp-fab = "C")
          or (item.compr-fabric = 2 and tt-param.c-imp-comp-fab = "F") then
             assign l-imprimir = no.

          if (substr(ob-pcp-ref.cod-refer,7,1) =  "0" and tt-param.c-imp-liso-est = "E")
          or (substr(ob-pcp-ref.cod-refer,7,1) <> "0" AND tt-param.c-imp-liso-est = "L") then 
             assign l-imprimir = no.

          if  ob-pcp-ref.qtd-prog = 0
          and ob-pcp-ref.qtd-proc = 0
          and ob-pcp-ref.qtd-pron = 0 then
              assign l-imprimir    = no.

          if l-imprimir = yes then do:
             assign l-imprimir = no.
             if  tt-param.tg-programado = yes and ob-pcp-ref.qtd-prog <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-processo = yes and ob-pcp-ref.qtd-proc <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-pronto = yes and ob-pcp-ref.qtd-pron <> 0 then
                 assign l-imprimir = yes. 
          end.
          /*
          find first ref-estrut 
               where ref-estrut.it-codigo = ob-pcp-ref.it-codigo 
                 and ref-estrut.al-codigo = ob-pcp-ref.cod-refer
               no-lock no-error.

          if avail ref-estrut and tt-param.tg-sem-estrutura = yes then 
             assign l-imprimir = no.
          */
          if l-imprimir then
             assign i-tot-prog-fnd = i-tot-prog-fnd + ob-pcp-ref.qtd-prog
                    i-tot-proc-fnd = i-tot-proc-fnd + ob-pcp-ref.qtd-proc
                    i-tot-pron-fnd = i-tot-pron-fnd + ob-pcp-ref.qtd-pron
                    i-tot-prog-item = i-tot-prog-item + ob-pcp-ref.qtd-prog
                    i-tot-proc-item = i-tot-proc-item + ob-pcp-ref.qtd-proc
                    i-tot-pron-item = i-tot-pron-item + ob-pcp-ref.qtd-pron
                    i-tot-prog-ger = i-tot-prog-ger + ob-pcp-ref.qtd-prog
                    i-tot-proc-ger = i-tot-proc-ger + ob-pcp-ref.qtd-proc
                    i-tot-pron-ger = i-tot-pron-ger + ob-pcp-ref.qtd-pron.

          if first-of(ob-pcp-ref.it-codigo) then do.
             assign c-descricao = item.descricao-1 + item.descricao-2.

             display item.it-codigo @ ob-pcp-ref.it-codigo
                     c-descricao
                     with frame f-detalhe.
          end.

          if last-of(ob-pcp-ref.cod-fundo) then do:
             assign c-fundo        = ob-pcp-ref.cod-fundo
                    c-compr-fabric = (if item.compr-fabric = 1 then "C" else "F").
             if i-tot-prog-fnd <> 0
             or i-tot-proc-fnd <> 0
             or i-tot-pron-fnd <> 0 then do:
                assign i-tot-prog-proc = i-tot-prog-fnd +
                                         i-tot-proc-fnd.
                display /*ob-pcp-ref.cod-refer
                        c-compr-fabric*/
                        c-fundo
                        i-tot-prog-fnd @ ob-pcp-ref.qtd-prog
                        i-tot-proc-fnd @ ob-pcp-ref.qtd-proc
                        i-tot-prog-proc
                        i-tot-pron-fnd @ ob-pcp-ref.qtd-pron
                        with frame f-detalhe.
                down with frame f-detalhe.
                assign l-pulou        = no
                       i-tot-prog-fnd = 0
                       i-tot-proc-fnd = 0
                       i-tot-pron-fnd = 0.
             end.
          end.

          if last-of(substr(ob-pcp-ref.cod-refer,3,5))
          and (i-tot-prog-item <> 0 or
               i-tot-proc-item <> 0 or
               i-tot-pron-item <> 0) then do:
             assign i-tot-prog-proc = i-tot-prog-item +
                                      i-tot-proc-item.
             display "Total Item"    @ ob-pcp-ref.it-codigo
                     i-tot-prog-item  @ ob-pcp-ref.qtd-prog
                     i-tot-proc-item  @ ob-pcp-ref.qtd-proc
                     i-tot-prog-proc
                     i-tot-pron-item  @ ob-pcp-ref.qtd-pron
                     with frame f-detalhe.
             down 2 with frame f-detalhe.
             assign i-tot-prog-item = 0
                    i-tot-proc-item = 0
                    i-tot-pron-item = 0
                    l-pulou        = yes.
          end.
          else
             if last-of(substr(ob-pcp-ref.cod-refer,3,5))
             and not l-pulou then do:
                 put skip(1).
                 assign l-pulou = yes.
             end.
      end. /* ob-pcp-ref */
   END.
   ELSE DO: /* 4 - Ordem Descri‡Æo do Item/Fundo/Refer */
      FOR EACH ob-pcp-ref WHERE 
               ob-pcp-ref.it-codigo >= tt-param.fi-ini-it-codigo AND
               ob-pcp-ref.it-codigo <= tt-param.fi-fin-it-codigo AND 
               ob-pcp-ref.cod-refer >= tt-param.fi-ini-cod-refer AND 
               ob-pcp-ref.cod-refer <= tt-param.fi-fin-cod-refer AND 
               ((substr(ob-pcp-ref.cod-refer,3,4)= tt-param.fi-desenho and
                 tt-param.l-inc-exc = yes) or
                (substr(ob-pcp-ref.cod-refer,3,4) <> tt-param.fi-desenho and 
                 tt-param.l-inc-exc = no) OR (tt-param.fi-desenho = "")) NO-LOCK,
          each item where
               item.it-codigo = ob-pcp-ref.it-codigo NO-LOCK
               break BY ITEM.desc-item
                     by ob-pcp-ref.it-codigo
                     by ob-pcp-ref.cod-fundo:
      
          assign l-imprimir = yes.
      
          if (item.compr-fabric = 1 and tt-param.c-imp-comp-fab = "C")
          or (item.compr-fabric = 2 and tt-param.c-imp-comp-fab = "F") then
             assign l-imprimir = no.
      
          if (substr(ob-pcp-ref.cod-refer,7,1) =  "0" and tt-param.c-imp-liso-est = "E")
          or (substr(ob-pcp-ref.cod-refer,7,1) <> "0" AND tt-param.c-imp-liso-est = "L") then 
             assign l-imprimir = no.
      
          if  ob-pcp-ref.qtd-prog = 0
          and ob-pcp-ref.qtd-proc = 0
          and ob-pcp-ref.qtd-pron = 0 then
              assign l-imprimir    = no.
      
          if l-imprimir = yes then do:
             assign l-imprimir = no.
             if  tt-param.tg-programado = yes and ob-pcp-ref.qtd-prog <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-processo = yes and ob-pcp-ref.qtd-proc <> 0 then
                 assign l-imprimir = yes.
             if  tt-param.tg-pronto = yes and ob-pcp-ref.qtd-pron <> 0 then
                 assign l-imprimir = yes. 
          end.
          /*
          find first ref-estrut 
               where ref-estrut.it-codigo = ob-pcp-ref.it-codigo 
                 and ref-estrut.al-codigo = ob-pcp-ref.cod-refer
               no-lock no-error.
      
          if avail ref-estrut and tt-param.tg-sem-estrutura = yes then 
             assign l-imprimir = no.
          */
          if l-imprimir then
             assign i-tot-prog-fnd = i-tot-prog-fnd + ob-pcp-ref.qtd-prog
                    i-tot-proc-fnd = i-tot-proc-fnd + ob-pcp-ref.qtd-proc
                    i-tot-pron-fnd = i-tot-pron-fnd + ob-pcp-ref.qtd-pron
                    i-tot-prog-item = i-tot-prog-item + ob-pcp-ref.qtd-prog
                    i-tot-proc-item = i-tot-proc-item + ob-pcp-ref.qtd-proc
                    i-tot-pron-item = i-tot-pron-item + ob-pcp-ref.qtd-pron
                    i-tot-prog-ger = i-tot-prog-ger + ob-pcp-ref.qtd-prog
                    i-tot-proc-ger = i-tot-proc-ger + ob-pcp-ref.qtd-proc
                    i-tot-pron-ger = i-tot-pron-ger + ob-pcp-ref.qtd-pron.
      
          if first-of(ob-pcp-ref.it-codigo) then do.
             assign c-descricao = item.descricao-1 + item.descricao-2.
      
             display item.it-codigo @ ob-pcp-ref.it-codigo
                     c-descricao
                     with frame f-detalhe.
          end.
      
          if last-of(ob-pcp-ref.cod-fundo) then do:
             assign c-fundo        = ob-pcp-ref.cod-fundo
                    c-compr-fabric = (if item.compr-fabric = 1 then "C" else "F").
             if i-tot-prog-fnd <> 0
             or i-tot-proc-fnd <> 0
             or i-tot-pron-fnd <> 0 then do:
                assign i-tot-prog-proc = i-tot-prog-fnd +
                                         i-tot-proc-fnd.
                display /*ob-pcp-ref.cod-refer
                        c-compr-fabric*/
                        c-fundo
                        i-tot-prog-fnd @ ob-pcp-ref.qtd-prog
                        i-tot-proc-fnd @ ob-pcp-ref.qtd-proc
                        i-tot-prog-proc
                        i-tot-pron-fnd @ ob-pcp-ref.qtd-pron
                        with frame f-detalhe.
                down with frame f-detalhe.
                assign l-pulou        = no
                       i-tot-prog-fnd = 0
                       i-tot-proc-fnd = 0
                       i-tot-pron-fnd = 0.
             end.
          end.
      
          if last-of(ob-pcp-ref.it-codigo)
          and (i-tot-prog-item <> 0 or
               i-tot-proc-item <> 0 or
               i-tot-pron-item <> 0) then do:
             assign i-tot-prog-proc = i-tot-prog-item +
                                      i-tot-proc-item.
             display "Total Item"    @ ob-pcp-ref.it-codigo
                     i-tot-prog-item  @ ob-pcp-ref.qtd-prog
                     i-tot-proc-item  @ ob-pcp-ref.qtd-proc
                     i-tot-prog-proc
                     i-tot-pron-item  @ ob-pcp-ref.qtd-pron
                     with frame f-detalhe.
             down 2 with frame f-detalhe.
             assign i-tot-prog-item = 0
                    i-tot-proc-item = 0
                    i-tot-pron-item = 0
                    l-pulou        = yes.
          end.
          else
             if last-of(ob-pcp-ref.it-codigo)
             and not l-pulou then do:
                 put skip(1).
                 assign l-pulou = yes.
             end.
      end. /* ob-pcp-ref */
   END.
   */
end. /* Fim - Resumido */

assign i-tot-prog-proc = i-tot-prog-ger + i-tot-proc-ger.
display "Total Geral"   @ ob-pcp.it-codigo
        i-tot-prog-ger  @ ob-pcp-ref.qtd-prog
        i-tot-proc-ger  @ ob-pcp-ref.qtd-proc
        i-tot-prog-proc
        i-tot-pron-ger  @ ob-pcp-ref.qtd-pron
        with frame f-detalhe.
down with frame f-detalhe.

assign i-tot-prog-ger = 0
       i-tot-proc-ger = 0
       i-tot-pron-ger = 0.

IF tt-param.imp-param THEN DO:
   PAGE.
   ASSIGN tt-param.c-imp-comp-fab = IF tt-param.c-imp-comp-fab = "C"
                                    THEN "Comprado"
                                    ELSE IF tt-param.c-imp-comp-fab = "F"
                                         THEN "Fabricado" 
                                         ELSE "Ambos"
          tt-param.c-imp-liso-est = IF tt-param.c-imp-liso-est = "L"
                                    THEN "Liso"
                                    ELSE IF tt-param.c-imp-liso-est = "E"
                                         THEN "Estampado" 
                                         ELSE "Ambos".
   display tt-param.desc-classifica
           tt-param.fi-ini-it-codigo
           tt-param.fi-fin-it-codigo
           tt-param.fi-ini-cod-refer
           tt-param.fi-fin-cod-refer
           tt-param.fi-desenho
           tt-param.l-inc-exc WHEN fi-desenho <> ""
           tt-param.l-tipo-rel
           tt-param.c-imp-comp-fab
           tt-param.c-imp-liso-est
           tt-param.tg-programado
           tt-param.tg-processo
           tt-param.tg-pronto
           tt-param.tg-sem-estrutura
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

