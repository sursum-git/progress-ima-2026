/* Programa: ESSP0198.P
** Sistema.: EMS da DATASUL S/A.
** Modulo..: REVIS«O TECIDO ACABADO
** Objetivo: Exportaá∆o de Revis‰es de Tecidos Acabados.
** Autor...: Gilvando de Souza Araujo - Novembro/2010
** Obs.....: Especifico da TEAR T“XTIL INDÈSTRIA E COMêRCIO LTDA.
**
*/

/* include de controle de vers∆o */
{include/i-prgvrs.i ESSP0198RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino          AS INTEGER
       FIELD arquivo          AS CHAR FORMAT "x(35)"
       FIELD usuario          AS CHAR FORMAT "x(12)"
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD cod-estabel-ini  LIKE ob-etiqueta.cod-estabel
       FIELD cod-estabel-fin  LIKE ob-etiqueta.cod-estabel
       FIELD dt-emissao-ini   LIKE ob-etiqueta.dt-emissao
       FIELD dt-emissao-fin   LIKE ob-etiqueta.dt-emissao
       FIELD num-etiqueta-ini LIKE ob-etiqueta.num-etiqueta
       FIELD num-etiqueta-fin LIKE ob-etiqueta.num-etiqueta
       FIELD arq-saida        AS CHAR FORMAT "x(45)"
       FIELD imp-param        AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

DEFINE input parameter raw-param as raw no-undo.
DEFINE input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

/* include para remover acentuaªío de strings */
{include/i-freeac.i}

/* definiá∆o de vari†veis  */
DEFINE var h-acomp     as handle no-undo.

DEF VAR c-situacao     AS CHAR FORMAT "x(15)".
DEF VAR c-qualidade    AS CHAR FORMAT "x(15)".
DEF VAR c-corte        AS CHAR FORMAT "x(15)".
DEF VAR c-desc-item    LIKE ITEM.desc-item.
DEF VAR l-indigo       AS LOG.
DEF VAR c-defeitos     AS CHAR EXTENT 5.
DEF VAR i-cont         AS INT.

/* Variaveis para o Excel */
DEFINE VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEFINE VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEFINE VAR chWorksheet AS COM-HANDLE NO-UNDO.
DEFINE VAR i-Lin       AS INTEGER.

FORM
    "*--------- ParÉmetros/Seleá∆o ---------*" SKIP
    tt-param.cod-estabel-ini  LABEL "Estabelecimento"
    "a" AT 29
    tt-param.cod-estabel-fin  NO-LABEL SKIP
    tt-param.dt-emissao-ini   LABEL "Data Emiss∆o..."
    "a" AT 29
    tt-param.dt-emissao-fin   NO-LABEL SKIP
    tt-param.num-etiqueta-ini LABEL "N£mero Etiqueta"
    "a" AT 29
    tt-param.num-etiqueta-fin NO-LABEL SKIP
    tt-param.arq-saida        LABEL "Arquivo Sa°da.."
    with no-box side-labels width 132 stream-io frame f-param.

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i}

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECIFICOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Planilha_Dados_Revis∆o_Tecidos_Acabados * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

/* Inicializaá∆o da Planilha */
CREATE "Excel.Application" chExcelApp NO-ERROR.
IF chExcelApp <> ? THEN /* Cria a Planilha */
   ASSIGN chExcelApp:SheetsInNewWorkbook = 1 /* Nß PLANILHAS A SEREM CRIADAS */
          chExcelApp:VISIBLE = FALSE  /* A Planilha n∆o Ficar† Visivel */
          chWorkbook         = chExcelApp:Workbooks:ADD() /* Cria Planilha */
          chWorksheet        = chExcelapp:Sheets:ITEM(1)
          chWorkSheet:NAME   = "Rolos Revisados".
ELSE DO:
   MESSAGE "O Aplicativo EXCEL n∆o foi encontrado. N∆o foi poss°vel executar o programa."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN.
END.

chWorksheet:Range("A1"):VALUE = c-empresa + " - ROLOS REVISADOS DE TECIDOS ACABADOS - DATA: " + 
                                STRING(TODAY,"99/99/9999") + " - " + STRING(TIME,"HH:MM").

/* Cabeáalho das Colunas */
chWorksheet:Range("A3"):VALUE  = "EST".
chWorksheet:Range("B3"):VALUE  = "NR-OB".
chWorksheet:Range("C3"):VALUE  = "DATA-OB".  
chWorksheet:Range("D3"):VALUE  = "NR-CARRO".
chWorksheet:Range("E3"):VALUE  = "ACONDIC".
chWorksheet:Range("F3"):VALUE  = "NR-SEQ".
chWorksheet:Range("G3"):VALUE  = "NR-REVIS".
chWorksheet:Range("H3"):VALUE  = "RESP-REVIS".
chWorksheet:Range("I3"):VALUE  = "DT-EMISS".
chWorksheet:Range("J3"):VALUE  = "HR-EMISS".
chWorksheet:Range("K3"):VALUE  = "EMBALAGEM".
chWorksheet:Range("L3"):VALUE  = "NR-LOTE".
chWorksheet:Range("M3"):VALUE  = "NUANCE".
chWorksheet:Range("N3"):VALUE  = "NR-CORTES".
chWorksheet:Range("O3"):VALUE  = "SITUAÄ«O".
chWorksheet:Range("P3"):VALUE  = "QUANTIDADE".
chWorksheet:Range("Q3"):VALUE  = "UN".
chWorksheet:Range("R3"):VALUE  = "QLD".
chWorksheet:Range("S3"):VALUE  = "NR-REPORTE".
chWorksheet:Range("T3"):VALUE  = "NR-ORD-PRD".
chWorksheet:Range("U3"):VALUE  = "QTD-ESTORN".
chWorksheet:Range("V3"):VALUE  = "ITEM".
chWorksheet:Range("W3"):VALUE  = "REFER“NCIA".
chWorksheet:Range("X3"):VALUE  = "TIPO-ORDEM".
chWorksheet:Range("Y3"):VALUE  = "QTD-0RIGINAL".
chWorksheet:Range("Z3"):VALUE  = "NUM-ETIQUETA".
chWorksheet:Range("AA3"):VALUE = "QUALIDADE".
chWorksheet:Range("AB3"):VALUE = "LOCALIZ".
chWorksheet:Range("AC3"):VALUE = "CORTE".
chWorksheet:Range("AD3"):VALUE = "PESO-BRUTO".
chWorksheet:Range("AE3"):VALUE = "ERRO-PESO".
chWorksheet:Range("AF3"):VALUE = "EMB-NEUTRA".
chWorksheet:Range("AG3"):VALUE = "DT-FATUR".
chWorksheet:Range("AH3"):VALUE = "OB-ORIGEM".
chWorksheet:Range("AI3"):VALUE = "INDIGO".
chWorksheet:Range("AJ3"):VALUE = "DEF1".
chWorksheet:Range("AK3"):VALUE = "DEF2".
chWorksheet:Range("AL3"):VALUE = "DEF3".
chWorksheet:Range("AM3"):VALUE = "DEF4".
chWorksheet:Range("AN3"):VALUE = "DEF5".

/* Configura Alinhamento Horizontal do Cabeáalho das Colunas */
chWorkSheet:Columns("A"):ColumnWidth = 3.29.
chWorkSheet:Columns("P"):NumberFormat = "###.##0,00".
chWorkSheet:Columns("U"):NumberFormat = "###.##0,00".
chWorkSheet:Columns("Y"):NumberFormat = "###.##0,00".
chWorkSheet:Columns("AD"):NumberFormat = "###.##0,00".

ASSIGN i-Lin = 4.

FOR EACH ob-etiqueta WHERE ob-etiqueta.cod-estabel  >= tt-param.cod-estabel-ini
                       AND ob-etiqueta.cod-estabel  <= tt-param.cod-estabel-fin
                       AND ob-etiqueta.dt-emissao   >= tt-param.dt-emissao-ini
                       AND ob-etiqueta.dt-emissao   <= tt-param.dt-emissao-fin
                       AND ob-etiqueta.num-etiqueta >= tt-param.num-etiqueta-ini
                       AND ob-etiqueta.num-etiqueta <= tt-param.num-etiqueta-fin
                       AND ob-etiqueta.situacao     > 1
                     NO-LOCK:
    /*
    IF ob-etiqueta.it-codigo <> '504891' OR
       ob-etiqueta.cod-refer <> '0101010' THEN NEXT.
    */

    RUN pi-acompanhar IN h-acomp (INPUT "Estab: " + ob-etiqueta.cod-estabel + " Num-OB: " +
                                        STRING(ob-etiqueta.nr-ob)).
    
    {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao} 

    FIND qualid-tecido WHERE
         qualid-tecido.codigo = ob-etiqueta.cod-qualid NO-LOCK NO-ERROR.
    IF AVAIL qualid-tecido THEN
       ASSIGN c-qualidade = qualid-tecido.descricao.
    ELSE
       ASSIGN c-qualidade = "".

    FIND corte-comerc WHERE
         corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.
    IF AVAIL corte-comerc THEN
       ASSIGN c-corte = corte-comerc.descricao.
    ELSE
       ASSIGN c-corte = "".

    FIND item WHERE item.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
    IF AVAIL item THEN
       ASSIGN c-desc-item = item.desc-item.
    ELSE
       ASSIGN c-desc-item = "".

    FIND item-ext WHERE item-ext.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
    IF AVAIL item-ext AND item-ext.indigo = YES THEN
       ASSIGN l-indigo = YES.
    ELSE
       ASSIGN l-indigo = NO.

    ASSIGN c-defeitos = ""
           i-cont     = 1.
    FOR EACH mov-est-acbd WHERE mov-est-acbd.cod-estabel  =  ob-etiqueta.cod-estabel
                            AND mov-est-acbd.num-etiqueta =  ob-etiqueta.num-etiqueta
                            AND mov-est-acbd.classific    <> "rt"
                          NO-LOCK:
        ASSIGN c-defeitos[i-cont] = STRING(mov-est-acbd.cod-tipo-def,'9') + "_" +
                                    STRING(mov-est-acbd.cod-defeito,'99')
               i-cont = i-cont + 1.
        IF i-cont > 5 THEN LEAVE.
    END.

    chWorksheet:range("A" + STRING(i-Lin)):VALUE = ob-etiqueta.cod-estabel.
    chWorksheet:range("B" + STRING(i-Lin)):VALUE = ob-etiqueta.nr-ob.
    chWorksheet:range("C" + STRING(i-Lin)):VALUE = ob-etiqueta.dt-ob.
    chWorksheet:range("D" + STRING(i-Lin)):VALUE = ob-etiqueta.nr-carro.
    chWorksheet:range("E" + STRING(i-Lin)):VALUE = ob-etiqueta.acondic.
    chWorksheet:range("F" + STRING(i-Lin)):VALUE = ob-etiqueta.nr-sequencia.
    chWorksheet:range("G" + STRING(i-Lin)):VALUE = ob-etiqueta.nr-revisadeira.
    chWorksheet:range("H" + STRING(i-Lin)):VALUE = ob-etiqueta.resp-revisao.
    chWorksheet:range("I" + STRING(i-Lin)):VALUE = ob-etiqueta.dt-emissao.
    chWorksheet:range("J" + STRING(i-Lin)):VALUE = ob-etiqueta.hr-emissao.
    chWorksheet:range("K" + STRING(i-Lin)):VALUE = ob-etiqueta.embalagem.
    chWorksheet:range("L" + STRING(i-Lin)):VALUE = ob-etiqueta.nr-lote.
    chWorksheet:range("M" + STRING(i-Lin)):VALUE = ob-etiqueta.nuance.
    chWorksheet:range("N" + STRING(i-Lin)):VALUE = ob-etiqueta.nr-cortes.
    chWorksheet:range("O" + STRING(i-Lin)):VALUE = STRING(ob-etiqueta.situacao) + '-' + c-situacao.
    chWorksheet:range("P" + STRING(i-Lin)):VALUE = ob-etiqueta.quantidade.
    chWorksheet:range("Q" + STRING(i-Lin)):VALUE = ob-etiqueta.un.
    chWorksheet:range("R" + STRING(i-Lin)):VALUE = ob-etiqueta.qualidade.
    chWorksheet:range("S" + STRING(i-Lin)):VALUE = ob-etiqueta.nr-reporte.
    chWorksheet:range("T" + STRING(i-Lin)):VALUE = ob-etiqueta.nr-ord-prod.
    chWorksheet:range("U" + STRING(i-Lin)):VALUE = ob-etiqueta.qtd-estornar.
    chWorksheet:range("V" + STRING(i-Lin)):VALUE = ob-etiqueta.it-codigo + '-' + c-desc-item.
    chWorksheet:range("W" + STRING(i-Lin)):VALUE = ob-etiqueta.cod-refer.
    chWorksheet:range("X" + STRING(i-Lin)):VALUE = ob-etiqueta.tipo-ordem.
    chWorksheet:range("Y" + STRING(i-Lin)):VALUE = ob-etiqueta.qtd-original.
    chWorksheet:range("Z" + STRING(i-Lin)):VALUE = ob-etiqueta.num-etiqueta.
    chWorksheet:range("AA" + STRING(i-Lin)):VALUE = ob-etiqueta.cod-qualid + '-' + c-qualidade.
    chWorksheet:range("AB" + STRING(i-Lin)):VALUE = ob-etiqueta.localizacao.
    chWorksheet:range("AC" + STRING(i-Lin)):VALUE = ob-etiqueta.corte-comerc + '-' + c-corte.
    chWorksheet:range("AD" + STRING(i-Lin)):VALUE = ob-etiqueta.peso-bruto.
    chWorksheet:range("AE" + STRING(i-Lin)):VALUE = IF ob-etiqueta.erro-peso = YES THEN "Sim" ELSE "N∆o".
    chWorksheet:range("AF" + STRING(i-Lin)):VALUE = IF ob-etiqueta.emb-neutra = YES THEN "Sim" ELSE "N∆o".
    chWorksheet:range("AG" + STRING(i-Lin)):VALUE = ob-etiqueta.dt-fatur.
    chWorksheet:range("AH" + STRING(i-Lin)):VALUE = ob-etiqueta.ob-origem.
    chWorksheet:range("AI" + STRING(i-Lin)):VALUE = IF l-indigo = YES THEN "Sim" ELSE "N∆o".
    chWorksheet:range("AJ" + STRING(i-Lin)):VALUE = c-defeitos[1].
    chWorksheet:range("AK" + STRING(i-Lin)):VALUE = c-defeitos[2].
    chWorksheet:range("AL" + STRING(i-Lin)):VALUE = c-defeitos[3].
    chWorksheet:range("AM" + STRING(i-Lin)):VALUE = c-defeitos[4].
    chWorksheet:range("AN" + STRING(i-Lin)):VALUE = c-defeitos[5].
    ASSIGN i-Lin = i-Lin + 1.
END.

/* Foco no Inicio da Planilha e Congela PainÇis */
chWorkSheet:Range("B:AN"):EntireColumn:AutoFit.

chExcelApp:Range("B4"):SELECT.
chExcelApp:ActiveWindow:FreezePanes = TRUE.

/* Salva e Fecha Planilha */
OS-DELETE VALUE(tt-param.arq-saida).
IF chExcelApp:Version BEGINS "8":U THEN 
   chWorkBook:SaveAs(arq-saida,39,,,,,,,TRUE). /* Salva em formato mais Antigo EXCEL97 */ 
ELSE 
   chWorkBook:SaveAs(arq-saida,,,,,,,).  /* Salva na Vers∆o da planilha da ESTAÄ«O */
chWorkBook:CLOSE().
chExcelApp:QUIT().
RELEASE OBJECT chExcelApp. 
RELEASE OBJECT chworkBook.
RELEASE OBJECT chWorksheet.

IF tt-param.imp-param THEN DO:
   DISPLAY tt-param.cod-estabel-ini      
           tt-param.cod-estabel-fin
           tt-param.dt-emissao-ini
           tt-param.dt-emissao-fin
           tt-param.num-etiqueta-ini
           tt-param.num-etiqueta-fin
           tt-param.arq-saida         
           WITH FRAME f-param.
END.
 
/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.
