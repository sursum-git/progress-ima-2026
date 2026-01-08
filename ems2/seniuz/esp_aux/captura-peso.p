/****************************************************************
Programa para captura de porta serial
*****************************************************************/


/* Define as variaveis */
DEFINE VARIABLE       serial      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE       c-temp      AS CHARACTER  FORMAT "x(70)" NO-UNDO.
DEFINE OUTPUT PARAM   c-peso      AS CHARACTER  FORMAT "x(70)" NO-UNDO.

/* Seta o handle para a biblioteca da Microsoft de controle de porta */
CREATE "mscommlib.mscomm" serial.


/* Realiza as configurações da porta */
serial:CommPort = 1.
serial:SETTINGS = "9600,e,7,2".
serial:inputlen = 0.
serial:PortOpen = TRUE.

/*Variaveis para controlar o tempo*/
DEFINE VARIABLE sec       AS INTEGER NO-UNDO.
DEFINE VARIABLE timerigth AS INTEGER NO-UNDO.
DEFINE VARIABLE espera-fi AS INT    NO-UNDO.
DEFINE VARIABLE espera-in AS INT    NO-UNDO.

ASSIGN timerigth = TIME - (24 * 60 * 60)
       sec = timerigth MOD 60
       espera-in = sec
       espera-fi = 30.

/*Recebe e acumula em loop os dados enviados pela balança*/
REPEAT WHILE (LENGTH(c-temp) < 18) AND (sec - espera-in <= espera-fi):
    c-temp = c-temp + serial:INPUT.
    timerigth = TIME - (24 * 60 * 60).  
    sec = timerigth MOD 60.
    PROCESS EVENTS.
END.

/* Fecha a porta serial */
serial:PortOpen = FALSE.

/*Desaloca a componete*/
RELEASE OBJECT serial.
serial = ?.

c-peso = c-temp.



