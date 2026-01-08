/* Programa: ESMECR03.P
** Sistema.: Controle de Servicos de Manutencao Mecanica
** Setor...: Mecanica
** Objetivo: Imprimir o Relatorio de Incidencia de Manutencao por Tipo de Manu-
**           tencao
** Autor...: Gilvando Souza Araujo - Julho/2000
**
** Conversao para EMS 2.04:
**   Programa: ESMECR03.P  =>  ESSP0081RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 08/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0081RP 2.04.00.000}

def temp-table w-work
    field tipo-man   like mov-man.tipo-man
    field sist-exec  like mov-man.sist-exec[1]
    field cod-area   like mov-man.cod-area
    field qtd-os     as int
    field min-man    as int
    field min-par    as int
    INDEX ch-work tipo-man
                  sist-exec
                  cod-area.
    
def buffer b-w-work for w-work.

define temp-table tt-param  no-undo
       field destino         as integer
       field arquivo         as char format "x(35)"
       field usuario         as char format "x(12)"
       field data-exec       as date
       field hora-exec       as integer
       field classifica      as integer
       field desc-classifica as char format "x(40)"
       FIELD ini-num-os      LIKE mov-man.num-os
       FIELD fin-num-os      LIKE mov-man.num-os
       FIELD ini-cod-setor   LIKE mov-man.cod-setor
       FIELD fin-cod-setor   LIKE mov-man.cod-setor
       FIELD ini-data-abe    LIKE mov-man.data-abe
       FIELD fin-data-abe    LIKE mov-man.data-abe
       FIELD ini-cod-maq     LIKE mov-man.cod-maq
       FIELD fin-cod-maq     LIKE mov-man.cod-maq
       FIELD ini-func-abe    LIKE mov-man.func-abe
       FIELD fin-func-abe    LIKE mov-man.func-abe
       FIELD ini-sist-const  LIKE mov-man.sist-const
       FIELD fin-sist-const  LIKE mov-man.sist-const
       FIELD ini-cod-area    LIKE mov-man.cod-area
       FIELD fin-cod-area    LIKE mov-man.cod-area
       FIELD ini-ssist-exec  LIKE mov-man.ssist-exec[1]
       FIELD fin-ssist-exec  LIKE mov-man.ssist-exec[1]
       FIELD ini-func-exec   LIKE mov-man.func-exec[1]
       FIELD fin-func-exec   LIKE mov-man.func-exec[1]
       FIELD masc-ssist      LIKE mov-man.ssist-exec[1] 
       FIELD tp-man1         AS CHAR FORMAT "x(3)"
       FIELD tp-man2         AS CHAR FORMAT "x(3)"
       FIELD tp-man3         AS CHAR FORMAT "x(3)"
       FIELD tp-man4         AS CHAR FORMAT "x(3)"
       FIELD tp-man5         AS CHAR FORMAT "x(3)"
       FIELD tp-man6         AS CHAR FORMAT "x(3)"
       FIELD all-types       AS LOG FORMAT "Sim/NÆo"
       FIELD situacao        AS INT
       FIELD desc-situacao   AS CHAR FORMAT "x(10)"
       field imp-param       as log.

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

DEF VAR l-inclui  AS LOG.
def var c-hor-man as char format "99999:99".
def var c-hor-par as char format "99999:99".
def var i-cont    as int.
def var i-cont1 as int.
def var i-cont2 as int.
def var i-minutos as int.
def var i-min-man as int.
def var i-min-par as int.
def var i-qtd-os-tpman  as int.
def var i-min-man-tpman as int.
def var i-min-par-tpman as int.
def var i-qtd-os-ger  as int.
def var i-min-man-ger as int.
def var i-min-par-ger as int.
def var i-perc1 as int format ">>9".
def var i-perc2 as int format ">>9".
def var i-perc3 as int format ">>9".

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.ini-num-os        LABEL "N£mero OS........." 
    "A"  at 32
    tt-param.fin-num-os        NO-LABELS SKIP
    tt-param.ini-cod-setor     label "Setor............."
    "A"  at 32                    
    tt-param.fin-cod-setor     NO-LABELS SKIP
    tt-param.ini-data-abe      LABEL "Data Abertura....." 
    "A"  at 32
    tt-param.fin-data-abe      NO-LABELS SKIP
    tt-param.ini-cod-maq       label "M quina..........."
    "A"  at 32                    
    tt-param.fin-cod-maq       NO-LABELS SKIP
    tt-param.ini-func-abe      LABEL "Funcion rio Abert." 
    "A"  at 32
    tt-param.fin-func-abe      NO-LABELS SKIP
    tt-param.ini-sist-const    label "Sistema Constatado"
    "A"  at 32                    
    tt-param.fin-sist-const    NO-LABELS SKIP
    tt-param.ini-cod-area      label "µrea.............."
    "A"  at 32                    
    tt-param.fin-cod-area      NO-LABELS SKIP
    tt-param.ini-ssist-exec    LABEL "SubSistema Exec..." 
    "A"  at 32
    tt-param.fin-ssist-exec    NO-LABELS SKIP
    tt-param.masc-ssist        LABEL "M sc.SubSistema..." SKIP
    tt-param.tp-man1           label "Tipos Manuten‡Æo.."
    tt-param.tp-man2  AT 25    NO-LABEL
    tt-param.tp-man3  AT 29    NO-LABEL
    tt-param.tp-man4  AT 33    NO-LABEL
    tt-param.tp-man5  AT 37    NO-LABEL
    tt-param.tp-man6  AT 41    NO-LABEL SKIP
    tt-param.all-types         LABEL "Todos os Tipos...." SKIP
    tt-param.ini-func-exec     LABEL "Funcion rio Exec.." 
    "A"  at 32
    tt-param.fin-func-exec     NO-LABELS SKIP
    tt-param.desc-situacao     LABEL "Situa‡Æo.........."
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    w-work.tipo-man      label "TipMan"
    w-work.sist-exec     label "Sist"
    w-work.cod-area      label "Area"
    area-mec.descricao   label "Descricao"
    w-work.qtd-os        label "Qtd.OS"    format ">>>>9"
    i-perc1              label " % "
    c-hor-man            label "H.Manut."
    i-perc2              label " % "
    c-hor-par            label "H.Parada"
    i-perc3              label " % "
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

{utp/ut-liter.i ESPECÖFICOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Incidˆncia_de_Manuten‡Æo_por_Tipo_Manuten‡Æo * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each mov-man where mov-man.num-os     >= tt-param.ini-num-os
                   and mov-man.num-os     <= tt-param.fin-num-os
                   and mov-man.cod-setor  >= tt-param.ini-cod-set
                   and mov-man.cod-setor  <= tt-param.fin-cod-set
                   and mov-man.data-abe   >= tt-param.ini-data-abe
                   and mov-man.data-abe   <= tt-param.fin-data-abe
                   and mov-man.cod-maq    >= tt-param.ini-cod-maq
                   and mov-man.cod-maq    <= tt-param.fin-cod-maq
                   and mov-man.func-abe   >= tt-param.ini-func-abe
                   and mov-man.func-abe   <= tt-param.fin-func-abe
                   and mov-man.sist-const >= tt-param.ini-sist-const
                   and mov-man.sist-const <= tt-param.fin-sist-const
                   and mov-man.cod-area   >= tt-param.ini-cod-area
                   and mov-man.cod-area   <= tt-param.fin-cod-area
                   and (mov-man.tipo-man  =  tt-param.tp-man1 or
                        mov-man.tipo-man  =  tt-param.tp-man2 or
                        mov-man.tipo-man  =  tt-param.tp-man3 or
                        mov-man.tipo-man  =  tt-param.tp-man4 or
                        mov-man.tipo-man  =  tt-param.tp-man5 or
                        mov-man.tipo-man  =  tt-param.tp-man6 OR
                                             tt-param.all-types = YES)
                   and ((mov-man.os-visada = yes and
                         tt-param.situacao = 1) or
                        (mov-man.os-visada = no and
                         tt-param.situacao = 2) or
                        (tt-param.situacao = 3))
                       no-lock:

   run pi-acompanhar in h-acomp (input "N£mero OS: " + mov-man.num-os).

   do i-cont = 1 to 5:
         if mov-man.sist-exec[i-cont] = "" then leave.
         do i-cont1 = 1 to 10:
          assign l-inclui = yes.
          do i-cont2 = 1 to 5:
             if  substr(tt-param.masc-ssist,i-cont2,1) <> "*"
             and substr(mov-man.ssist-exec[i-cont],i-cont2,1) <>
                 substr(tt-param.masc-ssist,i-cont2,1) then
             assign l-inclui = no.
          end.
          if  l-inclui
          and mov-man.ssist-exec[i-cont] >= tt-param.ini-ssist-exec
          and mov-man.ssist-exec[i-cont] <= tt-param.fin-ssist-exec
          and mov-man.func-exec[i-cont1] >= tt-param.ini-func-exec
          and mov-man.func-exec[i-cont1] <= tt-param.fin-func-exec
          and int(mov-man.ativ-exec[i-cont1]) = i-cont then do:
              assign i-minutos = 
                     (int(substr(mov-man.hora-texec[i-cont1],1,2)) * 60 +
                      int(substr(mov-man.hora-texec[i-cont1],3,2))) -
                     (int(substr(mov-man.hora-iexec[i-cont1],1,2)) * 60 +
                      int(substr(mov-man.hora-iexec[i-cont1],3,2))).
              if i-minutos < 0 then
                 assign i-minutos = i-minutos + 1440.
              assign i-min-man = i-min-man + i-minutos
                     i-min-par = i-min-par + mov-man.min-parado[i-cont1].
          end.
         end. 
         if i-min-man <> 0 
         or i-min-par <> 0 then do:
            find first w-work
                 where w-work.tipo-man  = mov-man.tipo-man
                   and w-work.sist-exec = mov-man.sist-exec[i-cont]
                   and w-work.cod-area  = mov-man.cod-area
                 no-error.
            if not avail w-work then do:
               create w-work.
               assign w-work.tipo-man  = mov-man.tipo-man
                      w-work.sist-exec = mov-man.sist-exec[i-cont]
                      w-work.cod-area  = mov-man.cod-area
                      w-work.qtd-os    = 0
                      w-work.min-man   = 0
                      w-work.min-par   = 0.
            end.
            assign w-work.qtd-os  = w-work.qtd-os + 1
                   i-qtd-os-ger   = i-qtd-os-ger + 1
                   w-work.min-man = w-work.min-man + i-min-man
                   w-work.min-par = w-work.min-par + i-min-par
                   i-min-man-ger  = i-min-man-ger + i-min-man
                   i-min-par-ger  = i-min-par-ger + i-min-par.
         end.
         assign i-min-man = 0
                i-min-par = 0.
      end.                            
   end.
   
   for each w-work break by w-work.tipo-man
                         by w-work.sist-exec
                         by w-work.cod-area:
      if first-of(w-work.tipo-man) then do:
         for each b-w-work where b-w-work.tipo-man = w-work.tipo-man:
             assign i-qtd-os-tpman  = i-qtd-os-tpman +  b-w-work.qtd-os
                    i-min-man-tpman = i-min-man-tpman + b-w-work.min-man
                    i-min-par-tpman = i-min-par-tpman + b-w-work.min-par.
         end.
         display w-work.tipo-man
                 with frame f-detalhe.
      end.
      assign i-perc1 = w-work.qtd-os / i-qtd-os-tpman * 100
             i-perc2 = w-work.min-man / i-min-man-tpman * 100
             i-perc3 = w-work.min-par / i-min-par-tpman * 100.
             
      assign c-hor-man = string((w-work.min-man - 
                               (w-work.min-man MODULO 60)) / 60,"99999") + 
                         string((w-work.min-man MODULO 60),"99")
             c-hor-par = string((w-work.min-par - 
                               (w-work.min-par MODULO 60)) / 60,"99999") + 
                         string((w-work.min-par MODULO 60),"99").
      find area-mec where area-mec.codigo = w-work.cod-area no-lock.
      display w-work.sist-exec
              w-work.cod-area
              area-mec.descricao
              w-work.qtd-os
              i-perc1
              c-hor-man
              i-perc2
              c-hor-par
              i-perc3
              with frame f-detalhe.
      down with frame f-detalhe.
              
      if last-of(w-work.tipo-man) then do:
         assign i-perc1 = i-qtd-os-tpman / i-qtd-os-ger * 100
                i-perc2 = i-min-man-tpman / i-min-man-ger * 100
                i-perc3 = i-min-par-tpman / i-min-par-ger * 100.
             
         assign c-hor-man = string((i-min-man-tpman - 
                                   (i-min-man-tpman MODULO 60)) / 
                                    60,"99999") + 
                            string((i-min-man-tpman MODULO 60),"99")
                c-hor-par = string((i-min-par-tpman - 
                                   (i-min-par-tpman MODULO 60)) / 
                                    60,"99999") + 
                            string((i-min-par-tpman MODULO 60),"99").
      
         display "Total"        @ w-work.tipo-man
                 i-qtd-os-tpman @ w-work.qtd-os
                 i-perc1
                 c-hor-man
                 i-perc2
                 c-hor-par
                 i-perc3
                 with frame f-detalhe.
         down 2 with frame f-detalhe.
   
         assign i-qtd-os-tpman  = 0
                i-min-man-tpman = 0
                i-min-par-tpman = 0.
      end.
   end.
   
   assign c-hor-man = string((i-min-man-ger - 
                            (i-min-man-ger MODULO 60)) / 
                              60,"99999") + 
                     string((i-min-man-ger MODULO 60),"99")
         c-hor-par = string((i-min-par-ger - 
                            (i-min-par-ger MODULO 60)) / 
                              60,"99999") + 
                     string((i-min-par-ger MODULO 60),"99").
      
   display "Total "     @ w-work.tipo-man
           i-qtd-os-ger @ w-work.qtd-os
           c-hor-man
           c-hor-par
           with frame f-detalhe.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.ini-num-os
           tt-param.fin-num-os
           tt-param.ini-cod-setor
           tt-param.fin-cod-setor
           tt-param.ini-data-abe
           tt-param.fin-data-abe
           tt-param.ini-cod-maq
           tt-param.fin-cod-maq
           tt-param.ini-func-abe
           tt-param.fin-func-abe
           tt-param.ini-sist-const
           tt-param.fin-sist-const
           tt-param.ini-cod-area
           tt-param.fin-cod-area
           tt-param.ini-ssist-exec
           tt-param.fin-ssist-exec
           tt-param.masc-ssist
           tt-param.tp-man1
           tt-param.tp-man2
           tt-param.tp-man3
           tt-param.tp-man4
           tt-param.tp-man5
           tt-param.tp-man6
           tt-param.all-types
           tt-param.ini-func-exec
           tt-param.fin-func-exec
           tt-param.desc-situacao
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

