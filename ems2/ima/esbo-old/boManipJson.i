DEFINE TEMP-TABLE ttJsonConv NO-UNDO
    FIELD id            AS INT
    FIELD nivel         AS INT
    FIELD tagPai        AS CHAR
    FIELD tag           AS CHAR
    FIELD valor         AS CHAR FORMAT 'x(32000)'
    FIELD logColchAbert AS LOG
    FIELD logColchFech  AS LOG
    FIELD logChaveAbert AS LOG
    FIELD logChaveFech  AS LOG
    FIELD logVirgula    AS LOG
    FIELD letraCriacao  AS CHAR
    FIELD letraAnterior AS CHAR EXTENT 2
    FIELD tamReg        AS INT
    FIELD tamVirgula    AS INT
    FIELD tamColcheteAb AS INT
    FIELD tamColcheteFe AS INT
    FIELD tamChaveAb    AS INT
    FIELD tamChaveFe    AS INT
    FIELD posicao       AS INT
    INDEX primario  nivel tagpai tag
    index indtag    tag
    INDEX indtagpai tagpai tag 
    INDEX indid IS PRIMARY  id      .
