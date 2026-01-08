/* Programa: ESCR020.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Listar Etiquetas de Cadastro de Clientes 
**           para pasta de documentos da cobranca.
** Autor...: Gilvando Souza Araujo - Marco/2000
** Obs.....: Especifico da TEAR TEXTIL INDUSTRIA E COMERCIO LTDA.
**
** Conversao para EMS 2.04:
**   Programa: ESCR020.P  =>  ESCR0012RP.P
**   Autor...: Prodb - Toninho
**   Data....: 10/02/2004
*/

/* include de controle de vers∆o */
{include/i-prgvrs.i ESCR0012RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD i-emit-ini       LIKE emitente.cod-emitente
       FIELD i-emit-fim       LIKE emitente.cod-emitente
       FIELD cod-gr-cli       LIKE emitente.cod-gr-cli EXTENT 10
       FIELD l-todos-gr-cli   AS   LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

/* definiá∆o de vari†veis  */
def var h-acomp as handle no-undo.

def var x-cod-emit like emitente.cod-emitente extent 3.
def var x-nom-emit like emitente.nome-emit extent 3.
def var i-cont as int init 1.
DEF VAR i-ct   AS INT.
DEF VAR l-continua AS LOG.

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i}

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each emitente where
         emitente.identific    <> 2 AND
         emitente.cod-emitente >= tt-param.i-emit-ini AND
         emitente.cod-emitente <= tt-param.i-emit-fim NO-LOCK.

    run pi-acompanhar in h-acomp (input emitente.cod-emit). 

    IF NOT tt-param.l-todos-gr-cli THEN DO.
       ASSIGN l-continua = NO.
       DO i-ct = 1 TO EXTENT(tt-param.cod-gr-cli).
          IF tt-param.cod-gr-cli[i-ct] = emitente.cod-gr-cli THEN DO.
             ASSIGN l-continua = YES.
             LEAVE.
          END.
       END.
       IF NOT l-continua THEN NEXT.
    END.
    assign x-cod-emit[i-cont] = emitente.cod-emitente
           x-nom-emit[i-cont] = emitente.nome-emit.
    assign i-cont = i-cont + 1.

    if i-cont = 4 then do:
       put string(x-cod-emit[1],"999999") at  1
           string(x-cod-emit[2],"999999") at 44 
           string(x-cod-emit[3],"999999") at 87 skip
           substr(x-nom-emit[1],1,34)     at  1 format "x(34)"
           substr(x-nom-emit[2],1,34)     at 44 format "x(34)"
           substr(x-nom-emit[3],1,34)     at 87 format "x(34)" skip
           substr(x-nom-emit[1],35,6)     at  1 format "x(34)"
           substr(x-nom-emit[2],35,6)     at 44 format "x(34)"
           substr(x-nom-emit[3],35,6)     at 87 format "x(34)" skip(3).
       do i-cont = 1 to 3:
          assign x-cod-emit[i-cont] = 0
                 x-nom-emit[i-cont] = "".
       end.
       assign i-cont = 1.
    end.
end.
  
if i-cont > 1 then do:
   put string(x-cod-emit[1],"999999") at  1
       string(x-cod-emit[2],"999999") at 44
       string(x-cod-emit[3],"999999") at 87 skip
       substr(x-nom-emit[1],1,34)     at  1 format "x(34)"
       substr(x-nom-emit[2],1,34)     at 44 format "x(34)" 
       substr(x-nom-emit[3],1,34)     at 87 format "x(34)" skip
       substr(x-nom-emit[1],35,6)     at  1 format "x(34)"
       substr(x-nom-emit[2],35,6)     at 44 format "x(34)" 
       substr(x-nom-emit[3],35,6)     at 87 format "x(34)" skip(3).
   do i-cont = 1 to 3:
      assign x-cod-emit[i-cont] = 0
             x-nom-emit[i-cont] = "".
   end.
   assign i-cont = 1.
end.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

