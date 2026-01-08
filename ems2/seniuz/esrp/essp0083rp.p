/* Programa: ESMECR05.P
** Sistema.: Controle de Servicos de Manutencao Mecanica
** Setor...: Mecanica
** Objetivo: Imprimir o Relatorio de Incidencia de Manutencao por Operador
** Autor...: Gilvando Souza Araujo - Julho/2000
**
** Conversao para EMS 2.04:
**   Programa: ESMECR05.P  =>  ESSP0083RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 08/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0083RP 2.04.00.000}

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

def var c-dif-horas  as char format "99999:99".
def var i-minutos as int.
def var i-min as int.

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
    mov-man.func-abe      label "Ope-Abe"
    mov-man.cod-area      label "Area"
    area-mec.descricao    label "Descricao"
    mov-man.tipo-man      label "TpMan"
    mov-man.cod-maq       label "Maquin"
    maq-mec.descricao     label "Descricao"
    mov-man.hora-abe      label "H-Abe"
    mov-man.hora-iexec[1] label "H-Ini"
    c-dif-horas           label "Tempo"
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
{utp/ut-liter.i Incidˆncia_de_Manuten‡Æo_por_Operador * r}
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

   assign i-minutos = (int(substr(mov-man.hora-iexec[1],1,2)) * 60 +
                       int(substr(mov-man.hora-iexec[1],3,2))) -
                      (int(substr(mov-man.hora-abe,1,2)) * 60 +
                       int(substr(mov-man.hora-abe,3,2))).
   if i-minutos < 0 then
      assign i-minutos = i-minutos + 1440.
   assign c-dif-horas = string((i-minutos - 
                              (i-minutos MODULO 60)) / 60,"99999") + 
                        string((i-minutos MODULO 60),"99").
  
   find maq-mec where maq-mec.codigo = mov-man.cod-maq no-lock.
   find area-mec where area-mec.codigo = mov-man.cod-area no-lock.

   display mov-man.func-abe
           mov-man.cod-area
           area-mec.descricao
           mov-man.tipo-man
           mov-man.cod-maq
           maq-mec.descricao
           mov-man.hora-abe
           mov-man.hora-iexec[1]
           c-dif-horas
           with frame f-detalhe.
   down with frame f-detalhe.
end.

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

