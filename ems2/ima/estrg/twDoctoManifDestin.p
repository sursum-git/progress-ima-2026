/*****************************************************************************
**
**     Programa: twDoctoManifDestin.P
**     Data....: Agosto de 2022
**     Objetivo: Grava as alteraá‰es de manifestaá∆o e situaá∆o da nota fiscal
**
*****************************************************************************/

DEFINE PARAMETER BUFFER b-new FOR docto-manif-destin.
DEFINE PARAMETER BUFFER b-old FOR docto-manif-destin. 
/*
{include/i-prgvrs.i TWDoctoManifDestin 2.06.00.001}
{utp/ut-glob.i}

DEFINE VARIABLE h AS HANDLE      NO-UNDO.

RUN esbo/boRetManifDestin.p PERSIST SET h NO-ERROR.

RUN setChaveNfe IN h(b-new.cod-chave-nfe). 
RUN setCodUsuario IN h(c-seg-usuario).

IF NEW b-new THEN DO:
    //RUN criarRetRecbtoResumo IN h.
END.
ELSE DO:
    IF b-old.idi-sit-manif-destin <> b-new.idi-sit-manif-destin THEN DO:
       CASE b-new.idi-sit-manif-destin :
           WHEN 1 THEN
                RUN criarRetOperConf IN  h.
           WHEN 2 THEN
                RUN criarRetDescOper IN  h.
           WHEN 3 THEN
                RUN criarRetOperNaoRealiz IN  h.
           WHEN 4 THEN
                RUN criarRetCiencia IN h.
       END CASE.
    END.        
END.


IF VALID-HANDLE(h) THEN DO:
    DELETE PROCEDURE h NO-ERROR.
END.
*/
/*
  idi-sit-manif-destin
  1- Confirmaá∆o da Operaá∆o
  2- Desconhecimento da Operaá∆o
  3- Operaá∆o N∆o Realizada
  4- Ciància da Emiss∆o
  5- Em Processamento
  6- Sem Situaá∆o
*/

/*riz-nf-eletro            date        im
   50 dat-emis-nf-eletro               date        im
   60 ind-sit-nf-eletro                char        m
   70 idi-sit-manif-destin             inte        im
   80 idi-tip-operac                   inte        m
   90 val-tot                          deci-2      m
  100 nom-emitente                     char        m
  110 cod-cgc-emit-nf-eletro           char        im
  120 cod-cpf-emit-nf-eletro           char        im
  130 cod-inscr-estad-emit-nf-eletro   char        m
  140 cod-cancel-ult-seqcial-unico     char
  150 dat-autoriz-cancel               date        m
  160 dat-emis-cancel                  date        m
  170 cod-ult-seqcial-carta-correc     char
  180 dsl-correc                       char        m
  190 dat-autoriz-carta-correc-eletro  date        m
  200 dat-emis-carta-correc-eletro     date        m
  210 hra-emis-carta-correc-eletro     char        m
  220 cod-padr-horar-carta-correc      char        m
  230 des-respos-desc                  char        m
  240 dsl-xml-sefaz                    char        m
  250 cod-ident-empres                 char        m
  260 cod-ser-docto                    char        im
  270 cod-docto                        char        im
  280 num-emit                         inte        im
  290 cod-nat-operac                   char        im
  300 idi-ambien-manif                 inte        m
  310 cod-respos-status                char
  330 cod-livre-1                      char
  340 cod-livre-2                      char
  350 cod-livre-3                      char
  360 cod-livre-4                      char
  370 cod-livre-5                      char
  380 dat-livre-1                      date
  390 dat-livre-2                      date
  400 dat-livre-3                      date
  410 dat-livre-4                      date
  420 dat-livre-5                      date
  430 val-livre-1                      deci-10
  440 val-livre-2                      deci-10
  450 val-livre-3                      deci-8
  460 val-livre-4                      deci-8
  470 val-livre-5                      deci-8
  480 log-livre-1                      logi
  490 log-livre-2                      logi
  500 log-livre-3                      logi
  510 log-livre-4                      logi
  520 log-livre-5                      logi
  530 num-livre-1                      inte
  540 num-livre-2                      inte
  550 num-livre-3                      inte
  560 num-livre-4                      inte
  570 num-livre-5                      inte
*/
