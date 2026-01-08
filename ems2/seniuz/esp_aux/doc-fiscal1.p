DEF VAR c-descricao-db AS CHAR FORMAT "x(2000)".
def var h-acomp as handle no-undo.
DEF VAR c-arq-sai AS CHAR FORMAT "x(55)".

ASSIGN c-arq-sai = "c:/lixo/lixo.txt".

UPDATE c-arq-sai.

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).


OUTPUT TO value(c-arq-sai) CONVERT SOURCE "ibm850".

FOR EACH doc-fiscal WHERE doc-fiscal.dt-docto >= 01/01/2002
                      AND doc-fiscal.dt-docto <= 12/31/2002
                    NO-LOCK,
    EACH it-doc-fisc OF doc-fiscal WHERE it-doc-fisc.it-codigo = "" OR
                                         it-doc-fisc.it-codigo = "," OR
                                         it-doc-fisc.it-codigo = "0000000" OR  
                                         it-doc-fisc.it-codigo = "0000006" OR
                                         it-doc-fisc.it-codigo = "4693"
                                   NO-LOCK:

    run pi-acompanhar in h-acomp (input "Doc: " + doc-fiscal.nr-doc-fis + 
                                        "/" + doc-fiscal.serie +
                                        " " + STRING(doc-fiscal.dt-emis-doc,"99/99/9999")). 
    ASSIGN c-descricao-db = REPLACE(REPLACE(it-doc-fisc.descricao-db, chr(13), " "), chr(10), " ").
    DISPLAY doc-fiscal.cod-estabel  LABEL "Est"
            doc-fiscal.serie        LABEL "Ser"         
            doc-fiscal.nr-doc-fis   LABEL "Docto"    
            doc-fiscal.cod-emitente LABEL "Emitente" 
            doc-fiscal.nat-operacao LABEL "Nat-Oper"
            doc-fiscal.dt-docto     LABEL "Dt-Docto"   
            it-doc-fisc.it-codigo   LABEL "Item" 
            it-doc-fisc.nr-seq-doc  LABEL "Seq"
            c-descricao-db          LABEL "Descricao" FORMAT "x(100)"
            WITH WIDTH 300 NO-LABELS.
END.

MESSAGE "Leia o resultado e pressione Ok..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
run pi-finalizar in h-acomp.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input c-arq-sai).
delete procedure h-prog.


