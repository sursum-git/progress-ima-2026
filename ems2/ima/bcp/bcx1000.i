/********************************************************************************************
**   Programa..: bcx1000.I                                                                  **
**                                                                                          **
**   Objetivo..: DEFINICAO DE TEMP-TABLES                                                   **
**                                                                                          **
**                                                                                          **
********************************************************************************************/

DEF TEMP-TABLE TT-EMBARQUE   NO-UNDO LIKE EMBARQUE.

DEF TEMP-TABLE TT-IT-PRE-FAT NO-UNDO LIKE IT-PRE-FAT.

DEF TEMP-TABLE tt-campo NO-UNDO
    FIELD campo AS CHAR
    FIELD caracter AS CHAR
    FIELD logico AS LOG
    FIELD DECIMAL AS DEC DECIMALS 10
    FIELD inteiro AS INT
    INDEX id IS PRIMARY campo.
