/***************************************************************
Programa:esbo/boPedVenda.p
Objetivo: Manter os pedidos de venda com as regras customizadas
e integra‡äes com Bo's padräes
autor:Tadeu Silva 
Data: 05/2021- implementa‡Æo do controle de agrupamento de pre‡os

******************************************************************/

{utp/ut-glob.i}
{esbo\boMsg.i}

DEFINE VARIABLE tbPrecoId           AS INTEGER     NO-UNDO.
DEFINE VARIABLE codMoeda            AS INTEGER     NO-UNDO.
DEFINE VARIABLE nrContainer         AS INTEGER     NO-UNDO.
DEFINE VARIABLE lDivideComis        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE dPercComisVend      AS INTEGER     NO-UNDO.
DEFINE VARIABLE dPercComisRepres    AS INTEGER     NO-UNDO.





DEFINE TEMP-TABLE ttPrecos
    FIELD itCodigo          LIKE ped-item.it-codigo
    FIELD codRefer          LIKE ped-item.cod-refer
    FIELD idPreco           AS INT
    FIELD iPrecoOut         AS INT
    FIELD logDivideComis    AS LOGICAL 
    FIELD percComisVend     AS INT
    FIELD percComisRepres   AS INT.



PROCEDURE setProp:
    DEFINE INPUT  PARAMETER prop AS CHAR FORMAT 'x(200)'   NO-UNDO.
    CASE prop:
        WHEN 'tb_preco' THEN DO:
            ASSIGN tbPrecoId = int(prop).
        END.
        WHEN 'cod_moeda' THEN DO:
            ASSIGN codMoeda = INT(prop).
        END.
        WHEN 'nr_container' THEN DO:
            ASSIGN nrContainer = INT(prop).
        END.
        WHEN 'log_divide_comis' THEN DO:
            ASSIGN lDivideComis =LOGICAL(prop).
        END.
        WHEN 'perc_comis_vend' THEN DO:
            ASSIGN dPercComisVend = DEC(prop).
        END.
        WHEN 'perc_comis_repres' THEN DO:
            ASSIGN dPercComisRepres = DEC(prop).
        END.                     
    END CASE.
END PROCEDURE.


















