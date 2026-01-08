DEFINE VARIABLE c_estab    AS CHARACTER NO-UNDO.
DEFINE VARIABLE c_serie    AS CHARACTER NO-UNDO.
DEFINE VARIABLE c_nota_ini AS CHARACTER NO-UNDO.
DEFINE VARIABLE c_nota_fim AS CHARACTER NO-UNDO. 
DEFINE VARIABLE d_emissao  AS DATE      NO-UNDO.
DEFINE VARIABLE c_natoper  AS CHARACTER NO-UNDO.
DEFINE VARIABLE aux        AS INTEGER   NO-UNDO.
DEFINE VARIABLE i AS INTEGER     NO-UNDO.
{utp/ut-glob.i}
/*FORM
    SKIP(01)
    " Programa desenvolvido para permitir alterar os campos CST PIS  "
    " e CST COFINS de acordo com o filtro selecionado.               "
    "                                                                "   
    SKIP(01)
    WITH FRAME f-mensagem ROW 3 CENTERED TITLE "spftvitoriaemcristo.p ".
    */

FORM
    c_estab       COLON 20 LABEL "Estabelecimento"      SKIP
    c_serie       COLON 20 LABEL "Serie"                SKIP
    c_nota_ini    COLON 20 LABEL "Nota Fiscal Ini"      SKIP
    c_nota_fim    COLON 20 LABEL "Nota Fiscal Fim"      SKIP
    /*d_emissao     COLON 20 LABEL "Data Emissao"         SKIP*/
    c_natoper     COLON 20 LABEL "Nat. Operacao"        
   
    WITH FRAME f-selecao ROW 1 SIDE-LABELS TITLE " SELE€ÇO ".
            
HIDE  ALL NO-PAUSE.     
CLEAR ALL NO-PAUSE.

/*view frame f-mensagem.*/

REPEAT:
    
    UPDATE c_estab
           c_serie   
           c_nota_ini
           c_nota_fim
           /*d_emissao*/
           c_natoper
          
        WITH NO-LABEL CENTERED FRAME f-selecao.
        
    ASSIGN i = 0.
    OUTPUT TO value('p:\notas_alteradas' + string(TIME) + '.txt').
    FOR EACH it-nota-fisc
        WHERE it-nota-fisc.cod-estabel  = c_estab 
        AND   it-nota-fisc.serie        = c_serie
        AND   it-nota-fisc.nr-nota-fis >= c_nota_ini
        AND   it-nota-fisc.nr-nota-fis <= c_nota_fim
        /*AND   it-nota-fisc.dt-emis-nota = d_emissao*/
        AND   it-nota-fisc.nat-operacao = c_natoper EXCLUSIVE-LOCK: 
        /*DISP 'ITEM '  it-nota-fisc.it-codigo ' encontrado e alterado'.*/
        DISP c-seg-usuario it-nota-fisc.it-codigo it-nota-fisc.nr-nota-fis.
        
        ASSIGN i = i + 1.

        assign it-nota-fisc.cod-sit-tributar-pis    = "01"
               it-nota-fisc.cod-sit-tributar-cofins = "01".
    END.
    OUTPUT CLOSE.
    IF i = 0 THEN DO:
        DISP  'nenhuma nota fiscal foi encontrada' WITH FRAME f-selecao.
        PAUSE 3.
        CLEAR FRAME f-selecao NO-PAUSE.
    END.
       

    ELSE DO:
       DISP i 'itens  encontrado(s) e alterado(s)' WITH FRAME f-selecao.
        PAUSE 3.
        CLEAR FRAME f-selecao NO-PAUSE.

    END.
       

END.


    

    /*esta fixo no programa para gravar 02 e ira fazer para itens x natureza*/

