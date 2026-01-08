/* include de controle de vers∆o */
{include/i-prgvrs.i ESPD0002RP 2.04.00.000}

DEF BUFFER empresa FOR mgcad.empresa.
DEF BUFFER moeda FOR mgcad.moeda.

{esinc/espd0002.i}

define temp-table tt-raw-digita
       field raw-digita as raw.

DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-pedidos no-undo
       FIELD nr-pedcli       like ped-venda.nr-pedcli
       FIELD qt-aberto       LIKE ped-item.qt-atendida
       FIELD perc-pronto     AS   DEC FORMAT ">>9.99"
       FIELD qt-reserva      LIKE ped-item.qt-atendida
       FIELD it-ares         AS   INT
       FIELD atendido        AS   CHAR FORMAT "x(30)"
       FIELD marca           AS   LOG INIT YES
       INDEX ch-pedido nr-pedcli.

def TEMP-TABLE w-aux
    field volume-ini like ped-item-res.volume-ini
    field volume-fim like ped-item-res.volume-fim.

def buffer b-transporte for transporte.
def buffer b-emitente for emitente.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

/* Includes para Impress∆o de Observaáoes */
{include/tt-edit.i}
{include/pi-edit.i}

/* definiá∆o de vari†veis  */
def var h-acomp as handle no-undo.

DEF VAR c-sit-prog         AS CHAR FORMAT "x(3)".
def var c-it-codigo        as char format "x(6)"  label "Item".
def var c-descricao        as char format "x(28)" label "Descricao".
def var i-nr-pecas         like ped-item-res.qt-dentro-emb.
def var c-sit-item         as char format "x(3)"  label "Sit".
def var i-volume           as int format "999"    label "Volumes".
def var c-moeda            as char format "x(10)" label "Moeda".
def var c-situacao         as char format "x(10)" label "Situacao".
def var c-cgc2             as char format "x(18)" label "CNPJ".
def var c-sit-aval         as char format "x(12)" label "Cred".
def var c-linha1           as char format "x(132)".
def var c-linha2           AS char format "x(132)".
def var de-tot-qtd-pedido  as dec format ">>>>,>>>,>>9.99"
    label "Qtd Total Pre-Nota".
def var de-vl-tot-pedido   as dec format ">>>>,>>>,>>9.99"
     label "Vl total Pre-Nota".
def var de-tot-qtd-reserva as dec format ">>>,>>>,>>9.99" label "Qtd Reserva".
def var de-perc-desco1     as dec format ">>9.99" label "Perc. Desconto".
def var de-perc-desco2     as dec format ">>9.99" label "+".
def var de-tot-qtd-aberto  as dec format ">>>>,>>>,>>9.99"
    label "Qtd Total Aberto".
def var de-vl-tot-aberto   as dec format ">>>>,>>>,>>9.99"
     label "Vl total aberto".
def var de-tot-vl-reserva  as dec format ">>>,>>>,>>9.99" label "VL Reserva".
def var de-frete-redesp    as dec format ">>>9.99".
def var i-tot-volume       as dec format ">>>,>>>,>>9".
def var c-desc-pagto       like cond-pagto.descricao.
DEF VAR c-lote             AS CHAR FORMAT "x(2)".
DEF VAR c-nome-abrev       LIKE ped-venda.nome-abrev.
def var c-item-ref         as char format "x(13)".
def var c-obs              as char format "x(12)".
def var i-cont             as int.
def var i-item             as int.
DEF VAR i-lote             AS INT  FORMAT 9.
def var de-aux-vl-pedido   as dec.
def var de-aux-vl-aberto   as dec.
def var de-aux-vl-reserva  as dec.
def var l-passou           as log.
DEF VAR c-alt-sit-cred     AS CHAR FORMAT "x(120)".

DEF VAR de-qtd-prog        AS DEC.
DEF VAR de-qtd-proc        AS DEC.
DEF VAR de-qtd-pron        AS DEC.

DEF BUFFER b-ped-item-res FOR ped-item-res.

form
   ped-item.nr-sequencia   FORMAT "9999"         COLUMN-LABEL "Seq" 
   c-it-codigo             FORMAT "x(6)"         COLUMN-LABEL "Item"
   /*ref-item-ext.dv         FORMAT "9"            COLUMN-LABEL "DV"*/
   ped-item.cod-refer      FORMAT "x(7)"         COLUMN-LABEL "Ref" 
   c-lote                  FORMAT "x(2)"         COLUMN-LABEL "Lt" 
   c-descricao             
   ped-item.qt-pedida      FORMAT "->>,>>9.99"   COLUMN-LABEL "Quantidade"
   item.un
   ped-item.vl-preori      FORMAT ">>>9.99"      COLUMN-LABEL "PrecoUn"
   c-obs                   FORMAT "x(10)"        COLUMN-LABEL "Embalagem"
   c-sit-item
   ped-item-res.qt-pedida  FORMAT ">>,>>9.99"    COLUMN-LABEL "Qtd Res"
   ped-item-res.sigla-emb                        COLUMN-LABEL "Sig"
   i-volume                                      COLUMN-LABEL "Vol"
   ped-item-res.volume-ini                       COLUMN-LABEL "VolINI"
   ped-item-res.volume-fim                       COLUMN-LABEL "VolFIM"
   c-sit-prog                                    COLUMN-LABEL "Prg"
   ped-item-res.sol-deposito FORMAT "S/N"        COLUMN-LABEL "SD"
   with no-box DOWN width 132 stream-io frame f-detalhe.

FORM 
   "Endereáo:" AT 1
   estabelec.endereco     NO-LABEL
   estabelec.estado       NO-LABEL 
   estabelec.cep
   "           "
   estabelec.cgc           LABEL "CNPJ" FORMAT "99.999.999/9999-99"
   "    "
   ped-venda-ext.tp-pedido label "TipoPed" skip
   ped-venda.nr-pedcli     label "Pedido.."
   repres.cod-rep          label "Repres"
   "-"
   repres.nome-abrev       no-label
   ped-venda.cod-cond-pag    label "Cond Pag"
   "-"
   cond-pagto.descricao    no-label skip
   "                    "
   ped-venda.nr-pedrep     label "Ped Repr"
   ped-repre.perc-comis    label "Comissao"
   ped-venda.nat-operacao  label "Nat Oper"
   c-situacao              label "Sit"
   c-moeda
   ped-venda.ind-fat-par   label "Fat Parcial" skip
   ped-venda.dt-emissao    label "Emissao."
   "-"
   ped-venda.dt-implant    label "Implantacao"
   "(" 
   ped-venda.user-impl     no-label
   ") -"
   ped-venda.dt-apr-cred   label "Aprovacao"
   "-"
   ped-venda.dt-entrega    label "Entrega"
   ped-venda.cod-priori    label "Priori" skip
   ped-venda.cod-emitente  label "Cliente."
   "-"
   emitente.nome-emit      FORMAT "x(37)" no-label
   c-cgc2                  FORMAT "99.999.999/9999-99" 
   " "
   emitente.ins-estadual   label "Inscr" format "x(17)"
   emitente.telefone[1]    label "Fone"  format "x(14)" skip
   ped-venda.local-entreg  label "Entrega."
   "-"
   ped-venda.bairro        no-label format "x(25)"
   "-"
   ped-venda.cidade        no-label
   "-"
   ped-venda.estado        no-label
   ped-venda.cep           LABEL "CEP"  FORMAT "99999-999" skip
   emitente.endereco-cob   label "Cobranca"
   "-"
   emitente.bairro-cob     NO-LABEL FORMAT "x(25)"
   "-"
   emitente.cidade-cob     no-label
   "-"
   emitente.estado-cob     no-label
   emitente.cep-cob        label "CEP"  FORMAT "99999-999"    skip
   c-sit-aval              label "Credito." 
   " (" 
   ped-venda.desc-bloq-cr  FORMAT "x(10)" no-label
   ")"                     SKIP
   c-alt-sit-cred          SKIP
   transporte.cod-transp   label "Transp.."
   "-"
   transporte.nome-abrev   no-label
   "-"
   transporte.endereco     no-label
   "-"
   transporte.cidade       no-label
   "-"
   transporte.estado       no-label
   "-"
   transporte.telefone     label "Fone" format "x(10)"  skip
   b-transporte.cod-transp label "Redesp.."
   "-"
   b-transporte.nome-abrev no-label
   "-"
   b-transporte.endereco   no-label
   "-"
   b-transporte.cidade     no-label
   "-"
   b-transporte.estado     no-label
   "-"
   b-transporte.telefone   label "Fone" format "x(10)" skip
   c-linha1  at 1          no-labels
   with no-box side-label width 143 stream-io frame f-2.

form
   c-linha1 at 1  no-labels
   de-tot-qtd-pedido
   de-vl-tot-pedido
   de-tot-qtd-reserva
   de-perc-desco1
   "+"
   de-perc-desco2 no-label  skip
   de-tot-qtd-aberto
   de-vl-tot-aberto
   de-tot-vl-reserva
   de-frete-redesp  SKIP
   i-tot-volume 
   tt-pedidos.atendido   skip
   c-linha2 at 1   NO-LABELS
   with no-box side-label width 143 stream-io frame f-final.

FORM
   c-obs
   tt-editor.conteudo FORMAT 'x(115)' AT 14
   with no-box DOWN NO-LABELS width 143 stream-io frame f-observ.

FORM
   emitente.endereco label "Local de Faturamento"
   emitente.bairro no-label
   emitente.cidade no-label
   emitente.cep no-label
   emitente.estado no-label
   with no-box side-label width 143 stream-io frame f-final2.
   
/* include padr∆o retirar os acentos */
{include/i-freeac.i}

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i}

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i PEDIDOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Listagem_de_PrÇ-Nota * r}
assign c-titulo-relat = trim(return-value).

ASSIGN c-linha1 = FILL("-",132)
       c-linha2 = FILL("-",132).

IF tt-param.destino <> 2 THEN DO.
   view frame f-cabec.
   view frame f-rodape.
END.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH tt-pedidos WHERE 
         tt-pedidos.marca = YES NO-LOCK.

    run pi-acompanhar in h-acomp (input string(tt-pedidos.nr-pedcli)).

    FIND ped-venda WHERE
         ped-venda.nr-pedcli = tt-pedidos.nr-pedcli NO-LOCK NO-ERROR.

    FIND estabelec where
         estabelec.cod-estabel = ped-venda.cod-estabel no-lock.

    FIND empresa where
         empresa.ep-codigo = param-global.empresa-prin no-lock.
    
    FIND repres
         WHERE repres.nome-abrev = ped-venda.no-ab-reppri NO-LOCK NO-ERROR.

    IF ped-venda.cod-cond-pag <> 0 THEN DO:
       FIND cond-pagto WHERE cond-pagto.cod-cond-pag =
                             ped-venda.cod-cond-pag
                             NO-LOCK.
       ASSIGN c-desc-pagto = cond-pagto.descricao.
    END.
    ELSE do:
        find con-pges where
             con-pges.nr-pedido = ped-venda.nr-pedido
             no-lock no-error.

        if avail con-pges then do i-cont = 1 to 6.
           if  con-pges.nr-dias-venc[i-cont] <> 0
           and con-pges.nr-dias-venc[i-cont] <> ? then do:
               assign c-desc-pagto = c-desc-pagto +
                      string(con-pges.nr-dias-venc[i-cont],"999") +
                      "/".
           end.
           else do:
               if  con-pges.data-pagto[i-cont] <> ? then
                   assign c-desc-pagto = c-desc-pagto +
                          string(con-pges.data-pagto[i-cont]
                          ,"99/99/9999") +
                          "-".
           end.
        end.
    END.

    {esinc/i-dsrb.i ped-venda.cod-sit-ped ped-venda.cod-sit-ped c-situacao}.
    {esinc/i-dsrb.i ped-venda.cod-sit-aval ped-venda.cod-sit-aval c-sit-aval}.

    find ped-repre where
         ped-repre.nome-ab-rep = ped-venda.no-ab-reppri and
         ped-repre.nr-pedido = ped-venda.nr-pedido no-lock no-error.

    find first emitente where
               emitente.cod-emit = ped-venda.cod-emitente and
               emitente.identific <> 2 no-lock no-error.
    find transporte where
         transporte.nome-abrev = ped-venda.nome-trans no-lock no-error.
    find b-transporte where
         b-transporte.nome-abrev = ped-venda.nome-tr-red no-lock no-error.

    FIND moeda WHERE
         moeda.mo-codigo = ped-venda.mo-codigo NO-LOCK NO-ERROR.
    
    assign c-moeda = IF AVAIL moeda
                     THEN moeda.descricao
                     ELSE "Moeda n∆o Cadastrada".

    assign c-cgc2 = emitente.cgc.

    if ped-venda.local-entreg <> "" AND tt-param.destino <> 2 then
       display ped-venda.local-entreg
               ped-venda.bairro
               ped-venda.cidade
               ped-venda.estado
               ped-venda.cep
               with frame f-2.

    if ped-venda.cod-entrega <> "" then do:
       find loc-entr where loc-entr.cod-entrega =
            ped-venda.cod-entrega
            and loc-entr.nome-abrev =
            emitente.nome-abrev no-lock no-error.
       if  avail loc-entr AND tt-param.destino <> 2 then
           display loc-entr.endereco @ ped-venda.local-entreg
                   loc-entr.cidade   @ ped-venda.cidade
                   loc-entr.bairro   @ ped-venda.bairro
                   loc-entr.estado   @ ped-venda.estado
                   loc-entr.cep      @ ped-venda.cep
                   with frame f-2.
    end.

    IF tt-param.destino <> 2 AND
       (ped-venda.cod-entrega = "" and ped-venda.local-entreg = "") OR 
       NOT AVAIL loc-entr THEN
       display emitente.endereco @ ped-venda.local-entreg
               emitente.bairro   @ ped-venda.bairro
               emitente.cidade   @ ped-venda.cidade
               emitente.estado   @ ped-venda.estado
               emitente.cep      @ ped-venda.cep
               with frame f-2.

    if emitente.endereco-cob <> "" AND tt-param.destino <> 2 THEN 
       display emitente.endereco-cob
               emitente.bairro-cob
               emitente.cidade-cob
               emitente.estado-cob
               emitente.cep-cob
               with frame f-2.

    if tt-param.destino <> 2 AND
       emitente.endereco-cob = "" and emitente.end-cobranca = 0 then
       display emitente.endereco @ emitente.endereco-cob
               emitente.bairro   @ emitente.bairro-cob
               emitente.cidade   @ emitente.cidade-cob
               emitente.estado   @ emitente.estado-cob
               emitente.cep      @ emitente.cep-cob
               with frame f-2.

    IF tt-param.destino <> 2 AND
       emitente.endereco-cob = "" and
       emitente.end-cobranca <> 0 then do:
       find first b-emitente where
                  b-emitente.cod-emit = emitente.cod-emit and
                  b-emitente.identific <> 2 no-lock no-error.
        display b-emitente.endereco @ emitente.endereco-cob
                b-emitente.bairro   @ emitente.bairro-cob
                b-emitente.cidade   @ emitente.cidade-cob
                b-emitente.estado   @ emitente.estado-cob
                b-emitente.cep      @ emitente.cep-cob
                with frame f-2.
    END.

    IF tt-param.destino = 2 THEN DO:
       ASSIGN c-nome-abrev = fn-free-accent(ped-venda.nome-abrev).

       IF INDEX(ped-venda.nome-abrev,'&') > 0 THEN
          OVERLAY(c-nome-abrev,INDEX(ped-venda.nome-abrev,'&',1)) = '&'.

       PUT "#"
           c-nome-abrev FORMAT "x(12)"
           ped-venda.nr-pedcli
           ped-venda.cidade
           ped-venda.estado
           ped-venda.nome-transp
           SKIP.
    END.
    ELSE DO:
        FIND ped-venda-ext WHERE
             ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
             ped-venda-ext.nr-pedido = ped-venda.nr-pedido 
             NO-LOCK NO-ERROR.

        /*
       /* --- Busca alteraá‰es ocorridas na situaá∆o de crÇdito do pedido ---*/
       FIND hist-cred-ped WHERE
            hist-cred-ped.nome-abrev = ped-venda.nome-abrev AND
            hist-cred-ped.nr-pedcli  = ped-venda.nr-pedcli
            NO-LOCK NO-ERROR.

       IF AVAIL hist-cred-ped THEN
          ASSIGN c-alt-sit-cred = TRIM(SUBSTR(hist-cred-ped.ult-alter,1,12)) + " " +
                                  SUBSTR(hist-cred-ped.ult-alter,13,10) + " " +
                                  SUBSTR(hist-cred-ped.ult-alter,23,8) + " " +
                                  SUBSTR(hist-cred-ped.ult-alter,31,2) + " " +
                                  SUBSTR(hist-cred-ped.ult-alter,33,2) + " - " +
                                  TRIM(SUBSTR(hist-cred-ped.pen-alter,1,12)) + " " +  
                                  SUBSTR(hist-cred-ped.pen-alter,13,10) + " " + 
                                  SUBSTR(hist-cred-ped.pen-alter,23,8) + " " +  
                                  SUBSTR(hist-cred-ped.pen-alter,31,2) + " " +
                                  SUBSTR(hist-cred-ped.pen-alter,33,2).
       ELSE
          ASSIGN c-alt-sit-cred = "".
       */

       DISPLAY estabelec.endereco
               estabelec.estado
               estabelec.cep
               estabelec.cgc
               ped-venda-ext.tp-pedido WHEN AVAIL ped-venda-ext
               ped-venda.nr-pedcli
               repres.cod-rep         when avail repres
               repres.nome-abrev      when avail repres
               ped-venda.cod-cond-pag 
               c-desc-pagto @ cond-pagto.descricao
               c-situacao
               c-sit-aval
               ped-venda.desc-bloq-cr
               c-alt-sit-cred        LABEL "Alt.Cred"
               ped-venda.nr-pedrep
               ped-repre.perc-comis when avail ped-repre
               ped-venda.cod-emitente
               emitente.nome-emit when avail emitente
               c-cgc2 when avail emitente
               emitente.ins-estadual when avail emitente
               emitente.telefone[1]  when avail emitente
               ped-venda.nr-pedcli
               ped-venda.nat-operacao
               transporte.nome-abrev     when avail transporte
               transporte.endereco when avail transporte
               transporte.cidade   when avail transporte
               transporte.estado   when avail transporte
               transporte.telefone when avail transporte
               c-moeda
               ped-venda.ind-fat-par
               ped-venda.dt-emissao
               ped-venda.dt-implant
               ped-venda.user-impl
               ped-venda.dt-apr-cred
               ped-venda.dt-entrega
               ped-venda.cod-priori
               b-transporte.nome-abrev when avail b-transporte
               b-transporte.endereco when avail b-transporte
               b-transporte.cidade when avail b-transporte
               b-transporte.estado when avail b-transporte
               b-transporte.telefone when avail b-transporte
               c-linha1
               with frame f-2.
    END.

    ASSIGN i-item       = 0
           c-desc-pagto = "".

    IF tt-param.classifica = 1 THEN DO.
       FOR EACH ped-item WHERE
                ped-item.nome-abrev = ped-venda.nome-abrev AND
                ped-item.nr-pedcli  = ped-venda.nr-pedcli NO-LOCK:

           RUN pi-imprime.
       END.
    END.
    ELSE DO.
        FOR EACH ped-item WHERE
                 ped-item.nome-abrev = ped-venda.nome-abrev AND
                 ped-item.nr-pedcli  = ped-venda.nr-pedcli NO-LOCK,
            EACH b-ped-item-res WHERE
                 b-ped-item-res.nome-abrev   = ped-item.nome-abrev AND
                 b-ped-item-res.nr-pedcli    = ped-item.nr-pedcli AND
                 b-ped-item-res.it-codigo    = ped-item.it-codigo AND
                 b-ped-item-res.cod-refer    = ped-item.cod-refer AND
                 b-ped-item-res.nr-sequencia = ped-item.nr-sequencia NO-LOCK
                 BY b-ped-item-res.volume-ini.

            RUN pi-imprime.
        END.
    END.
               
    ASSIGN de-perc-desco1 = ped-venda.perc-desco1
           de-perc-desco2 = ped-venda.perc-desco2.

    IF SUBSTR(ped-venda.nat-operacao,4,1) = "z" THEN
       ASSIGN de-perc-desco1 = 7.
       
    REPEAT.
       IF i-item >= 47 THEN
          ASSIGN i-item = i-item - 47.
       ELSE
          LEAVE.
    END.
    IF tt-param.destino <> 2 AND i-item < 33 THEN DO:
       ASSIGN i-item = 33 - i-item.
       PUT " " SKIP(i-item).
    END.
    
    find first emitente where emitente.cod-emit = ped-venda.cod-emit
                          and emitente.identific <> 2 no-lock
                             no-error.
    IF ped-venda.nome-tr-red <> "" THEN DO.
       /*
       FIND transporte-ext WHERE
            transporte-ext.nome-transp = ped-venda.nome-transp NO-LOCK NO-ERROR.

       IF AVAIL transporte-ext THEN DO.
          ASSIGN de-frete-redesp = de-tot-vl-reserva * transporte-ext.perc-frete / 100.
          IF de-frete-redesp < transporte-ext.frete-minimo THEN
              ASSIGN de-frete-redesp = transporte-ext.frete-minimo.
       END.
       */
    END.
    ELSE
       ASSIGN de-frete-redesp = 0.

    IF tt-param.destino <> 2 THEN DO.
       display c-linha1
               de-tot-qtd-pedido     label "     Qtd Total"
               de-tot-qtd-aberto     label "Qtd Tot Aberto"
               de-vl-tot-pedido      label "     Vl Total"
               de-vl-tot-aberto      label "Vl Tot Aberto"
               de-tot-qtd-reserva    label "Qtd Reserva"
               de-tot-vl-reserva     label " VL Reserva"
               de-frete-redesp       label "+ Frete Red"
               de-perc-desco1        label " Perc Desc"
               de-perc-desco2
               i-tot-volume          label "   Tot Volumes"
               tt-pedidos.atendido   no-label
               c-linha2
               WITH FRAME f-final.

        RUN pi-print-editor(input replace(replace(ped-venda.observacoes, chr(13), " "), chr(10), " "), INPUT 115). 
        FOR EACH tt-editor:
            IF tt-editor.linha = 1 THEN
               DISP "Observaá‰es:" @ c-obs 
                    WITH FRAME f-observ.
    
            DISP tt-editor.conteudo 
                 WITH FRAME f-observ.
            DOWN WITH FRAME f-observ.
        END.
        DOWN 1 WITH FRAME f-observ.
    
        DISPLAY emitente.endereco
                emitente.bairro  FORMAT "x(25)"
                emitente.cidade
                emitente.estado
                emitente.cep FORMAT "99999-999"
                with frame f-final2.
        PAGE.
    END.

    assign de-tot-qtd-pedido  = 0
           de-tot-qtd-aberto  = 0
           de-tot-qtd-reserva = 0
           de-tot-vl-reserva  = 0
           i-tot-volume       = 0
           i-nr-pecas         = 0
           de-vl-tot-aberto   = 0
           de-vl-tot-pedido   = 0
           i-item             = 0.
end.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.


PROCEDURE pi-imprime.
/* Itens do Pedido */
       FIND ped-item-ext OF ped-item NO-LOCK NO-ERROR.
       IF AVAIL ped-item-ext THEN DO:
           IF ped-item-ext.corte-comerc < tt-param.corte-comerc-ini OR
              ped-item-ext.corte-comerc > tt-param.corte-comerc-fin THEN
              NEXT.
       END.
       ELSE DO:
          IF tt-param.corte-comerc-ini <> "" OR
             tt-param.corte-comerc-fin <> "Z" THEN NEXT.
       END.

       if substr(ped-venda.nat-operacao,4,1) = "z" then /* Z.Franca */
          assign de-aux-vl-pedido = ped-item.qt-pedida *
                                    (int(ped-item.vl-preori * .93 *                                                   100) / 100).
       else
          assign de-aux-vl-pedido = ped-item.qt-pedida *
                                    ped-item.vl-preori.
                                        
       assign de-tot-qtd-pedido = de-tot-qtd-pedido +
                                  ped-item.qt-pedida
              de-vl-tot-pedido  = de-vl-tot-pedido + de-aux-vl-pedido.
       
       if  ped-item.cod-sit-item = 3 
       and tt-param.sit-total = no then next.

       if  tt-param.sit-aberto = no
       and ped-item.cod-sit-item = 1 then next.

       if  tt-param.sit-parcial = no
       and ped-item.cod-sit-item = 2  then next.

       if  tt-param.sit-pendentes = no
       and ped-item.cod-sit-item = 4 then next.

       if  tt-param.sit-suspensos = no
       and ped-item.cod-sit-item = 5 then next.

       if  tt-param.sit-cancelados = no
       and ped-item.cod-sit-item = 6 then next.

       if (ped-item.qt-pedida -
           ped-item.qt-atendida +
           ped-item.qt-devolvida -
           ped-item.qt-pendente) <= 0 and tt-param.sit-total = no then next.

       /*-- Verifica se reserva foi completada --*/
       FIND ped-item-res WHERE
            ped-item-res.nome-abrev   = ped-item.nome-abrev AND
            ped-item-res.nr-pedcli    = ped-item.nr-pedcli AND
            ped-item-res.it-codigo    = ped-item.it-codigo AND
            ped-item-res.cod-refer    = ped-item.cod-refer AND
            ped-item-res.nr-sequencia = ped-item.nr-sequencia NO-LOCK NO-ERROR.

       IF AVAIL ped-item-res AND tt-param.it-reservados = NO THEN NEXT.
       
       if substr(ped-venda.nat-operacao,4,1) = "z" then /* Z.Franca */
          assign de-aux-vl-aberto = (ped-item.qt-pedida -
                                     ped-item.qt-atendida +
                                     ped-item.qt-devolvida -
                                     ped-item.qt-pendente) *
                                    (int(ped-item.vl-preori * .93 *
                                         100) / 100).
       else
          assign de-aux-vl-aberto = (ped-item.qt-pedida -
                                     ped-item.qt-atendida +
                                     ped-item.qt-devolvida -
                                     ped-item.qt-pendente) *
                                    ped-item.vl-preori.
       assign de-tot-qtd-aberto = de-tot-qtd-aberto +
                                  (ped-item.qt-pedida -
                                   ped-item.qt-atendida +
                                   ped-item.qt-devolvida -
                                   ped-item.qt-pendente)
              de-vl-tot-aberto = de-vl-tot-aberto + de-aux-vl-aberto.

       FIND ITEM WHERE item.it-codigo = ped-item.it-codigo
                       NO-LOCK NO-ERROR.
       IF avail ITEM THEN DO:
          ASSIGN c-descricao = item.descricao-1 + item.descricao-2.
          
          /*-- Verifica se o Item tem nome comercial diferenciado --*/
          IF SUBSTR(ped-item.cod-refer,1,2) <> "" THEN DO:
             FIND item-ext WHERE item-ext.it-codigo = ITEM.it-codigo 
                           NO-LOCK NO-ERROR.
             IF AVAIL item-ext THEN DO:
                /*
                DO i-cont = 1 TO 99:
                   IF item-ext.cod-gr-ref[i-cont] = SUBSTR(ped-item.cod-refer,1,2) THEN DO:
                      ASSIGN c-descricao = item-ext.des-gr-ref[i-cont].
                      LEAVE.
                   END.
                END.
                */
             END.
          END.
       END.
       ELSE
          ASSIGN c-descricao = "Item nao cadastrado".

       /*
       ASSIGN de-qtd-prog = 0   
              de-qtd-proc = 0    
              de-qtd-pron = 0.
       FOR EACH ob-pcp WHERE
                ob-pcp.it-codigo = ped-item.it-codigo NO-LOCK,
           EACH ob-pcp-ref OF ob-pcp WHERE
                ob-pcp-ref.situacao = 1 AND
                ob-pcp-ref.cod-refer = ped-item.cod-refer NO-LOCK.

           ASSIGN de-qtd-prog = de-qtd-prog + ob-pcp-ref.qtd-sld-prog
                  de-qtd-proc = de-qtd-proc + ob-pcp-ref.qtd-proc
                  de-qtd-pron = de-qtd-pron + ob-pcp-ref.qtd-pron.
       END.
       */

       IF de-qtd-prog <> 0 then
          ASSIGN c-sit-prog = "S".
       ELSE
          ASSIGN c-sit-prog = "N".
       IF de-qtd-proc <> 0 then
          ASSIGN c-sit-prog = c-sit-prog + "S".
       ELSE
          ASSIGN c-sit-prog = c-sit-prog + "N".
       IF de-qtd-pron <> 0 then
          ASSIGN c-sit-prog = c-sit-prog + "S".
       ELSE
          ASSIGN c-sit-prog = c-sit-prog + "N".

       /* Comentado por Toninho em 23/12 devido Ö nova metodologia de programaá∆o 
       **
       find ref-item-ext where 
            ref-item-ext.it-codigo = item.it-codigo AND
            ref-item-ext.cod-refer = ped-item.cod-refer no-lock no-error.

       IF NOT AVAIL ref-item-ext then
          ASSIGN c-sit-prog = "NNN".
       ELSE DO:
          IF ref-item-ext.qtd-prog <> 0 then
             ASSIGN c-sit-prog = "S".
          ELSE
             ASSIGN c-sit-prog = "N".
          IF ref-item-ext.qtd-proc <> 0 then
             ASSIGN c-sit-prog = c-sit-prog + "S".
          ELSE
             ASSIGN c-sit-prog = c-sit-prog + "N".
          IF ref-item-ext.qtd-pron <> 0 then
             ASSIGN c-sit-prog = c-sit-prog + "S".
          ELSE
             ASSIGN c-sit-prog = c-sit-prog + "N".
       END.
       */
       
       ASSIGN c-it-codigo = SUBSTR(item.it-codigo,1,6). 

       {esinc/i-dsrb.i ped-item.cod-sit-item ped-item.cod-sit-item c-sit-item}.

       IF AVAIL ped-item-ext THEN
          ASSIGN c-obs  = ped-item-ext.acondicionamento
                 c-lote = SUBSTR(ped-item-ext.lote,1,2).
       ELSE
          ASSIGN c-obs  = ""
                 c-lote = "".

       IF tt-param.destino = 2 THEN DO.
          ASSIGN i-lote = IF SUBSTR(c-lote,1,2) = "RP"
                          THEN 1 
                          ELSE IF SUBSTR(c-lote,1,2) = "PP"
                               THEN 2
                               ELSE IF SUBSTR(c-lote,1,2) = "RD"
                                    THEN 3
                                    ELSE IF SUBSTR(c-lote,1,2) = "PD"
                                         THEN 4
                                         ELSE 0 .
          PUT "*"
              c-it-codigo        FORMAT "x(6)"
              IF ped-item.cod-refer = ''
                 THEN FILL('0',7)
                 ELSE ped-item.cod-refer FORMAT "x(7)" 
              i-lote            
              c-descricao        FORMAT "x(30)"
              (ped-item.qt-pedida - ped-item.qt-atendida + 
               ped-item.qt-devolvida - ped-item.qt-pendente) FORMAT "999999.999"
              SKIP.
       END.
       ELSE DO.
          DISPLAY ped-item.nr-sequencia
                  c-it-codigo
                  ped-item.cod-refer
                  c-lote
                  c-descricao
                  ped-item.qt-pedida -
                  ped-item.qt-atendida +
                  ped-item.qt-devolvida -
                  ped-item.qt-pendente when ped-venda.cod-sit-ped <> 3
                           @ ped-item.qt-pedida
                  ped-item.qt-pedida   when ped-venda.cod-sit-ped = 3
                           @ ped-item.qt-pedida
                  item.un
                  ped-item.vl-preori
                  c-obs 
                  c-sit-item
                  c-sit-prog
                  with frame f-detalhe.

           for each ped-item-res
               where ped-item-res.nome-abrev   = ped-item.nome-abrev
                 and ped-item-res.nr-pedcli    = ped-item.nr-pedcli
                 and ped-item-res.nr-sequencia = ped-item.nr-sequencia
                 and ped-item-res.it-codigo    = ped-item.it-codigo
                 AND ped-item-res.cod-refer    = ped-item.cod-refer
                 and ped-item-res.faturado     = no
                     no-lock.
               if substr(ped-venda.nat-operacao,4,1) = "z" then /* ZF */
                  assign de-aux-vl-reserva = ped-item-res.qt-pedida *
                                             (int(ped-item.vl-preori *
                                                  0.93 * 100) / 100).
               else
                  assign de-aux-vl-reserva = ped-item-res.qt-pedida *
                                             ped-item.vl-preori.
    
               assign de-tot-qtd-reserva = de-tot-qtd-reserva +
                                           ped-item-res.qt-pedida
                      de-tot-vl-reserva = de-tot-vl-reserva +
                                            de-aux-vl-reserva.
               assign i-nr-pecas = i-nr-pecas + ped-item-res.qt-dentro-emb.
               find first w-aux
                    where w-aux.volume-ini = ped-item-res.volume-ini
                      and w-aux.volume-fim = ped-item-res.volume-fim
                          no-error.
               if avail w-aux then
                  assign i-volume = 0.
               else do:
                  create w-aux.
                  assign w-aux.volume-ini = ped-item-res.volume-ini
                         w-aux.volume-fim = ped-item-res.volume-fim
                         i-volume = ped-item-res.volume-fim -
                                    ped-item-res.volume-ini + 1.
               end.
               display ped-item-res.qt-pedida
                       ped-item-res.sigla-emb
                       i-volume     when i-volume > 0
                       ped-item-res.volume-ini
                       ped-item-res.volume-fim
                       ped-item-res.sol-deposito
                       with frame f-detalhe.
    
               assign i-tot-volume = i-tot-volume + i-volume.
               down with frame f-detalhe.
               assign i-item   = i-item + 1
                      l-passou = yes.
           end.
    
           if l-passou = no then do:
              assign i-item = i-item + 1.
              down with frame f-detalhe.
           end.
           assign l-passou = no.
       END.
    
END PROCEDURE.


