PROMPT-FOR user-web.login WITH SIDE-LABELS CENTERED.
FOR EACH user-web NO-LOCK WHERE user-web.login = INPUT user-web.login:
    DISP user-web.senha
    WITH CENTERED WIDTH 600.
END.
