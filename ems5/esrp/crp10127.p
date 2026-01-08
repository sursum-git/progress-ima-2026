/*****************************************************************************
**
**       Programa: crp10127.p
**
**       Data....: 31/07/03
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Pagamentos Quitados
**
**       VersÆo..: 1.00.000 - CDIAS
**
**       OBS.....: Este fonte foi gerado pelo Data Viewer
**
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "CRP10127".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.

run grapi/gr2002.p (input c-prog-gerado, input "2.00.00.000").

/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/

def temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            as integer
    field i-cod-fornec-ini like tit-ap.cod-fornec
    field i-cod-fornec-fim like tit-ap.cod-fornec
    field da-dt-prev-pag-ini like tit-ap.dt-prev-pag
    field da-dt-prev-pag-fim like tit-ap.dt-prev-pag
    field i-portador-ini like tit-ap.portador
    field i-portador-fim like tit-ap.portador
    field c-cod-esp-ini like mov-ap.cod-esp
    field c-cod-esp-fim like mov-ap.cod-esp
    field i-modalidade-ini like mov-ap.modalidade
    field i-modalidade-fim like mov-ap.modalidade
.

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

def new global shared var i-ep-codigo-usuario  like empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new global shared var i-num-ped-exec-rpw  as integer no-undo.   
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new global shared var l-rpc as logical no-undo.
def new global shared var r-registro-atual as rowid no-undo.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var i-num-ped as integer no-undo.         
def new global shared var v_cdn_empres_usuar   like empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.


/****************** FIM INCLUDE COM VARIµVEIS GLOBAIS *********************/
/****************** Defini‡ao de Parƒmetros do Relat¢rio *********************/ 

/****************** Defini‡ao de Vari veis de Sele‡Æo do Relat¢rio *********************/ 

def new shared var i-cod-fornec-ini like tit-ap.cod-fornec format ">>>>>>>>9" initial 0 no-undo.
def new shared var i-cod-fornec-fim like tit-ap.cod-fornec format ">>>>>>>>9" initial 999999999 no-undo.
def new shared var da-dt-prev-pag-ini like tit-ap.dt-prev-pag format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var da-dt-prev-pag-fim like tit-ap.dt-prev-pag format "99/99/9999" initial "12/31/9999" no-undo.
def new shared var i-portador-ini like tit-ap.portador format ">>>>9" initial 0 no-undo.
def new shared var i-portador-fim like tit-ap.portador format ">>>>9" initial 99999 no-undo.
def new shared var c-cod-esp-ini like mov-ap.cod-esp format "!!" initial "  " no-undo.
def new shared var c-cod-esp-fim like mov-ap.cod-esp format "!!" initial "ZZ" no-undo.
def new shared var i-modalidade-ini like mov-ap.modalidade format "9" initial 0 no-undo.
def new shared var i-modalidade-fim like mov-ap.modalidade format "9" initial 9 no-undo.

/****************** Defini‡ao de Vari veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Defini‡ao de Vari veis Campo do Layout do Relat¢rio **********************/ 

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 

/****************** Defini‡ao de Vari veis de Total do Relat¢rio *****************************/ 

def var de-vl-orig-me-tt-001 like tit-ap.vl-orig-me no-undo.

/****************** Defini‡ao de Vari veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Defini‡ao de Vari veis de Processamento do Relat¢rio *********************/

def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.

/****************** Defini‡ao de Forms do Relat¢rio 132 Colunas ***************************************/ 

form tit-ap.cod-fornec column-label "Cod" format ">>>>>9" at 001
     tit-ap.nome-abrev column-label "Fornecedor" format "X(12)" at 008
     /*tit-ap.dt-prev-pag column-label "Prev Pagto" format "99/99/9999" at 021*/
     mov-ap.dt-vencimen column-label "Prev Pagto" format "99/99/9999" at 021
     tit-ap.nr-docto column-label "Documento" format "x(12)" at 032
     mov-ap.cod-esp column-label "Esp" format "!!" at 045
     tit-ap.portador column-label "Port" format ">>>>9" at 049
     /*Comentado por Anderson Fganer 06/01/2011 - Valor diferente do valor q foi pago
     tit-ap.vl-orig-me column-label "Vl Titulo" format "->>>,>>>,>>9.99" at 055
     */
     mov-ap.valor-mov column-label "Vl Mov"     format "->>>,>>>,>>9.99" at 055
     tit-ap.observacao column-label "Observa‡Æo" format "x(62)" at 071
     with down width 132 no-box stream-io frame f-relat-09-132.

create tt-param.
raw-transfer raw-param to tt-param.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.


def var rw-log-exec                            as rowid no-undo.
def var c-erro-rpc as character format "x(60)" initial " " no-undo.
def var c-erro-aux as character format "x(60)" initial " " no-undo.
def var c-ret-temp as char no-undo.
def var h-servid-rpc as handle no-undo.     
define var c-empresa       as character format "x(40)"      no-undo.
define var c-titulo-relat  as character format "x(50)"      no-undo.
define var i-numper-x      as integer   format "ZZ"         no-undo.
define var da-iniper-x     as date      format "99/99/9999" no-undo.
define var da-fimper-x     as date      format "99/99/9999" no-undo.
define var i-page-size-rel as integer                       no-undo.
define var c-programa      as character format "x(08)"      no-undo.
define var c-versao        as character format "x(04)"      no-undo.
define var c-revisao       as character format "999"        no-undo.

define new shared var c-impressora   as character                      no-undo.
define new shared var c-layout       as character                      no-undo.
define new shared var v_num_count     as integer                       no-undo.
define new shared var c-arq-control   as character                     no-undo.
define new shared var c-sistema       as character format "x(25)"      no-undo.
define new shared var c-rodape        as character                     no-undo.


DEFINE VARIABLE var-qt-nr-docto     AS INTEGER     NO-UNDO.
DEFINE VARIABLE var-tot-vl-nr-docto AS DECIMAL     NO-UNDO.


define new shared buffer b_ped_exec_style for ped_exec.
define new shared buffer b_servid_exec_style for servid_exec.

define new shared stream str-rp.

assign c-programa     = "crp10127"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Pagamentos Quitados"
       c-sistema      = "crp".

if  tt-param.formato = 2 then do:


form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabec.

form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    "Periodo:" i-numper-x at 08 "-"
    da-iniper-x at 14 "to" da-fimper-x
    fill("-", 74) format "x(72)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabper.

run grapi/gr2004.p.

form header
    c-rodape format "x(132)"
    with stream-io width 132 no-labels no-box page-bottom frame f-rodape.


end. /* tt-param.formato = 2 */


run grapi/gr2009.p (input tt-param.destino,
                    input tt-param.arquivo,
                    input tt-param.usuario,
                    input no).

    assign i-ep-codigo-usuario = tt-param.ep-codigo
           v_cdn_empres_usuar  = i-ep-codigo-usuario.

    assign i-cod-fornec-ini = tt-param.i-cod-fornec-ini
           i-cod-fornec-fim = tt-param.i-cod-fornec-fim
           i-cod-fornec-fim = tt-param.i-cod-fornec-fim
           da-dt-prev-pag-ini = tt-param.da-dt-prev-pag-ini
           da-dt-prev-pag-fim = tt-param.da-dt-prev-pag-fim
           i-portador-ini = tt-param.i-portador-ini
           i-portador-fim = tt-param.i-portador-fim
           c-cod-esp-ini = tt-param.c-cod-esp-ini
           c-cod-esp-fim = tt-param.c-cod-esp-fim
           i-modalidade-ini = tt-param.i-modalidade-ini
           i-modalidade-fim = tt-param.i-modalidade-fim
.


find first empresa no-lock
    where empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail empresa
then
    assign c-empresa = empresa.razao-social.
else
    assign c-empresa = "".

/* for each e disp */

def var l-imprime as logical no-undo.


        assign de-vl-orig-me-tt-001 = 0.
assign l-imprime = no.
if  tt-param.destino = 1 then
    assign v-cod-destino-impres = "Impressora".
else
    if  tt-param.destino = 2 then
        assign v-cod-destino-impres = "Arquivo".
    else
        assign v-cod-destino-impres = "Terminal".


run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

 


for each tit-ap no-lock
         where tit-ap.cod-fornec >= i-cod-fornec-ini and 
               tit-ap.cod-fornec <= i-cod-fornec-fim and
               tit-ap.dt-prev-pag >= da-dt-prev-pag-ini and 
               tit-ap.dt-prev-pag <= da-dt-prev-pag-fim and
               tit-ap.portador >= i-portador-ini and 
               tit-ap.portador <= i-portador-fim,
    each mov-ap no-lock
         where mov-ap.ep-codigo = tit-ap.ep-codigo and
               mov-ap.cod-fornec = tit-ap.cod-fornec and
               mov-ap.cod-estabel = tit-ap.cod-estabel and
               mov-ap.cod-esp = tit-ap.cod-esp and
               mov-ap.serie = tit-ap.serie and
               mov-ap.nr-docto = tit-ap.nr-docto and
               mov-ap.parcela = tit-ap.parcela and
               mov-ap.cod-esp >= c-cod-esp-ini and 
               mov-ap.cod-esp <= c-cod-esp-fim and
               mov-ap.modalidade >= i-modalidade-ini and 
               mov-ap.modalidade <= i-modalidade-fim and
               /*mov-ap.transacao  = 1*/
               mov-ap.transacao  >= 2 AND
               mov-ap.transacao  <= 4
              /*tit-ap.nr-docto = "MR160026" */
    break by tit-ap.dt-prev-pag
          BY tit-ap.cod-fornec
          BY mov-ap.nr-docto
          BY mov-ap.dt-vencimen:

    
    /*IF tit-ap.nr-docto = "MR160026" THEN
    MESSAGE mov-ap.valor-mov SKIP mov-ap.transacao SKIP mov-ap.cod-esp
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

    /***  CàDIGO PARA SAÖDA EM 132 COLUNAS ***/

    if  tt-param.formato = 2 then do:
        /*
        IF first-of(mov-ap.nr-docto) THEN DO:
           ASSIGN var-tot-vl-nr-docto = 0
                  var-qt-nr-docto     = 0.
           
           /*PUT stream str-rp SKIP(1).*/
        END.
        */
        ASSIGN var-tot-vl-nr-docto = var-tot-vl-nr-docto + mov-ap.valor-mov
               var-qt-nr-docto     = var-qt-nr-docto     + 1.

        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
        display stream str-rp tit-ap.cod-fornec
            tit-ap.nome-abrev
            /*tit-ap.dt-prev-pag*/
            mov-ap.dt-vencimen
            tit-ap.nr-docto
            mov-ap.cod-esp
            tit-ap.portador
            /* Comentado por Anderson Fganer 06/01/2011 - Valor diferente do valor q foi pago
            tit-ap.vl-orig-me*/
            mov-ap.valor-mov
            tit-ap.observacao
                with stream-io frame f-relat-09-132.
            down stream str-rp with frame f-relat-09-132.
        /*
        IF LAST-OF(mov-ap.nr-docto) AND var-qt-nr-docto > 1 THEN DO:
           PUT stream str-rp
               /*FILL("-",50) FORMAT "x(50)" AT 021*/
               "Total Titulo: " AT 01
               tit-ap.nr-docto
               "Dt Prev Pag: "
               tit-ap.dt-prev-pag
               
               var-tot-vl-nr-docto format "->>>,>>>,>>9.99" at 055
               SKIP.
        END.
        */

    end.
    /* Comentado por Anderson Fganer 06/01/2011 - Valor diferente do valor q foi pago
    assign de-vl-orig-me-tt-001 = de-vl-orig-me-tt-001 + tit-ap.vl-orig-me.
    
    */
    assign de-vl-orig-me-tt-001 = de-vl-orig-me-tt-001 +  mov-ap.valor-mov.

    
end.


if  l-imprime = no then do:
    if  tt-param.formato = 2 then do:
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
    end.
    disp stream str-rp " " with stream-io frame f-nulo.
end.
    display stream str-rp "---------------" @ 
           /* tit-ap.vl-orig-me*/
            mov-ap.valor-mov
            with stream-io frame f-relat-09-132.
    down stream str-rp with frame f-relat-09-132.
put stream str-rp "Total:" at 048 de-vl-orig-me-tt-001 format "->>>,>>>,>>9.99" to 069.

run pi-finalizar in h-acomp.

if  tt-param.destino <> 1 then

    page stream str-rp.

else do:

    if   tt-param.parametro = yes then

         page stream str-rp.

end.

if  tt-param.parametro then do:


   disp stream str-rp "SELE€ÇO" skip(01) with stream-io frame f-imp-sel.
   disp stream str-rp 
      i-cod-fornec-ini colon 18 "|< >|"   at 42 i-cod-fornec-fim no-label
      da-dt-prev-pag-ini colon 18 "|< >|"   at 42 da-dt-prev-pag-fim no-label
      i-portador-ini colon 18 "|< >|"   at 42 i-portador-fim no-label
      c-cod-esp-ini colon 18 "|< >|"   at 42 c-cod-esp-fim no-label
      i-modalidade-ini colon 18 "|< >|"   at 42 i-modalidade-fim no-label
        with stream-io side-labels overlay row 030 frame f-imp-sel.

   put stream str-rp unformatted skip(1) "IMPRESSÇO" skip(1).

   put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
   put stream str-rp unformatted skip "    " "Execu‡Æo: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
   put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
   put stream str-rp unformatted skip "    " "Usu rio : " tt-param.usuario.

end.

else
    output stream str-rp close.

procedure pi-print-editor:

    def input param c-editor    as char    no-undo.
    def input param i-len       as integer no-undo.

    def var i-linha  as integer no-undo.
    def var i-aux    as integer no-undo.
    def var c-aux    as char    no-undo.
    def var c-ret    as char    no-undo.

    for each tt-editor:
        delete tt-editor.
    end.

    assign c-ret = chr(255) + chr(255).

    do  while c-editor <> "":
        if  c-editor <> "" then do:
            assign i-aux = index(c-editor, chr(10)).
            if  i-aux > i-len or (i-aux = 0 and length(c-editor) > i-len) then
                assign i-aux = r-index(c-editor, " ", i-len + 1).
            if  i-aux = 0 then
                assign c-aux = substr(c-editor, 1, i-len)
                       c-editor = substr(c-editor, i-len + 1).
            else
                assign c-aux = substr(c-editor, 1, i-aux - 1)
                       c-editor = substr(c-editor, i-aux + 1).
            if  i-len = 0 then
                assign entry(1, c-ret, chr(255)) = c-aux.
            else do:
                assign i-linha = i-linha + 1.
                create tt-editor.
                assign tt-editor.linha    = i-linha
                       tt-editor.conteudo = c-aux.
            end.
        end.
        if  i-len = 0 then
            return c-ret.
    end.
    return c-ret.
end procedure.

return 'OK'.

/* fim do programa */
