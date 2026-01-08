
/*------------------------------------------------------------------------
    File        : implBoCxBanco.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : administrator
    Created     : Thu Apr 24 18:11:05 BRT 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

routine-level on error undo, throw.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{esbo/bo_ctbl_cx.i}



define variable hBoCtblCx   as handle    no-undo.


define variable nomeEmpresa as character format 'x(100)' no-undo.
define variable cHistorico  as character no-undo.
define variable numLinha    as integer   no-undo.
define variable h_Acomp     as handle    no-undo.



run esbo/bo_ctbl_cx.p persistent set hBoCtblCx.



run limparTTs        in hBoCtblCx.
run SETMostrarMsg    in hBoCtblCx(no).
run setIntervalEstab in hBoCtblCx('501','501'). 
run setDataTransacao in hBoCtblCx(04.07.2025, 04.07.2025 ).
run setNivel in hBoCtblCx('fluxo').
run setlistaTpFluxoDescons      in hBoCtblCx('1.02,2.09').
run setCtaCorrenDesconsiderar   in hBoCtblCx('95'). // carterira med - solicitado por Newson em 16/02/2022
run setCtaCorrenDesconsiderar   in hBoCtblCx('mutuo').
run setCtaCorrenDesconsiderar   in hBoCtblCx('555-mut im').
run setMostrarMsg               in hBoCtblCx(yes).   
   
   
run setCtaCorren in hBoCtblCx('906118832').

run setSeq in hBoCtblCx(6).

run buscarRegistros         in hBoCtblCx.
run exportarRegsFluxo       in hBoCtblCx('c:\temp\fluxo_realizado.txt' ).
run exportarLancsSemCtbl    in hBoCtblCx('c:\temp\fluxo_realizado_sem_ctbl.txt').
run getTTFluxoFechamento    in hBoCtblCx(output TABLE ttFluxoFechamento).
run exportarLancsFechamento in hBoCTblCx('c:\temp\fechamento.txt').


if valid-object(hBoCtblCx) then
   delete object hBoCtblCx. 



