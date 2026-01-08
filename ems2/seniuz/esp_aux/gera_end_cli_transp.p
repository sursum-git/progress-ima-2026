/* Programa: gera_end_cli_transp.p
** Objetivo: Importar lista de codigos de clientes e transportadores e
**           gera .csv com dados de endereco para emissao de etiquetas. 
*/

DEF TEMP-TABLE tt-cli-transp
    FIELD tipo    AS CHAR
    FIELD codigo  AS CHAR.

DEF VAR c-nome   LIKE emitente.nome-emit.
DEF VAR c-ender  LIKE emitente.endereco.
DEF VAR c-bairro LIKE emitente.bairro.
DEF VAR c-cep    LIKE emitente.cep.
DEF VAR c-cidade LIKE emitente.cidade.
DEF VAR c-estado LIKE emitente.estado.          

INPUT FROM "O:\Arquivos Inform tica\Vendas\Lista_Clientes_Transportadores.csv" CONVERT SOURCE "ibm850". 
SET ^.

REPEAT:
    CREATE tt-cli-transp.
    IMPORT DELIMITER ";" tt-cli-transp.
END.
INPUT CLOSE.

OUTPUT TO "O:\Arquivos Inform tica\Vendas\Ender_Clientes_Transportadores.csv" CONVERT SOURCE "ibm850".
PUT "Nome;Endereco;Bairro;Cep;Cidade;Estado" SKIP.

FOR EACH tt-cli-transp:
    IF tt-cli-transp.tipo = "" THEN NEXT.
    IF tt-cli-transp.tipo = "c" THEN DO:
       FIND emitente WHERE emitente.cod-emitente = INT(tt-cli-transp.codigo)
                     NO-LOCK NO-ERROR.
       IF NOT AVAIL emitente THEN
          MESSAGE "Cliente" tt-cli-transp.codigo "NÆo existe."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
       ELSE DO:
          ASSIGN c-nome   = emitente.nome-emit
                 c-ender  = emitente.endereco
                 c-bairro = emitente.bairro
                 c-cep    = emitente.cep
                 c-cidade = emitente.cidade
                 c-estado = emitente.estado.
       END.
    END.
    ELSE DO:
        FIND transporte WHERE transporte.cod-transp = INT(tt-cli-transp.codigo)
                        NO-LOCK NO-ERROR.
        IF NOT AVAIL transporte THEN
           MESSAGE "Transportador" tt-cli-transp.codigo "NÆo existe."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ELSE DO:
           ASSIGN c-nome   = transporte.nome
                  c-ender  = transporte.endereco
                  c-bairro = transporte.bairro
                  c-cep    = transporte.cep
                  c-cidade = transporte.cidade
                  c-estado = transporte.estado.
        END.
    END.
    PUT UNFORMAT
        c-nome ";"
        c-ender ";"
        c-bairro ";"
        c-cep ";"
        c-cidade ";"
        c-estado
        SKIP.
END.
OUTPUT CLOSE.
