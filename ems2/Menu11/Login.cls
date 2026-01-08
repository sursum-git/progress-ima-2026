USING Progress.Lang.*.
USING Progress.Windows.Form.

CLASS Login INHERITS Form   : 
    
	DEFINE PRIVATE VARIABLE windowContainer1 AS Progress.Windows.WindowContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE lblUsuario AS System.Windows.Forms.Label NO-UNDO.
    DEFINE PRIVATE VARIABLE lblSenha   AS System.Windows.Forms.Label NO-UNDO.

    DEFINE PRIVATE VARIABLE txtUsuario AS System.Windows.Forms.TextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE txtSenha AS System.Windows.Forms.TextBox NO-UNDO.
    DEFINE PRIVATE VARIABLE btOk AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE btCancel AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE PROPERTY situacaoLogin AS CHAR NO-UNDO
        PRIVATE GET. PRIVATE SET.

    /* tt de retorno da api padrao de login. */
    DEF PRIVATE TEMP-TABLE tt-erro-logon NO-UNDO 
        FIELD cod-erro          AS INT 
        FIELD desc-erro         AS CHAR FORMAT 'x(256)':u
        FIELD desc-arq          AS CHAR .

    CONSTRUCTOR PUBLIC Login (  ):
        SUPER().
        InitializeComponent().
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
        
	END CONSTRUCTOR.
    
    METHOD PRIVATE VOID InitializeComponent(  ):

        @VisualDesigner.FormMember (NeedsInitialize="true").
        THIS-OBJECT:windowContainer1 = NEW Progress.Windows.WindowContainer().
        THIS-OBJECT:lblUsuario = NEW System.Windows.Forms.Label().
        THIS-OBJECT:lblSenha   = NEW System.Windows.Forms.Label().
        THIS-OBJECT:txtUsuario = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:txtSenha = NEW System.Windows.Forms.TextBox().
        THIS-OBJECT:btOk = NEW System.Windows.Forms.Button().
        THIS-OBJECT:btCancel = NEW System.Windows.Forms.Button().
        THIS-OBJECT:SuspendLayout().
        
        /* windowContainer1 */
        THIS-OBJECT:windowContainer1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:windowContainer1:Name = "windowContainer1".
        THIS-OBJECT:windowContainer1:Size = NEW System.Drawing.Size(400, 120).
        THIS-OBJECT:windowContainer1:TabIndex = 0.
        THIS-OBJECT:windowContainer1:TabStop = FALSE.

        /* lblUsuario */
        THIS-OBJECT:lblUsuario:Location = NEW System.Drawing.Point(15, 23).
        THIS-OBJECT:lblUsuario:Size = NEW System.Drawing.Size(50, 20).
        THIS-OBJECT:lblUsuario:TEXT = "Usu rio:".

        /* lblSenha */
        THIS-OBJECT:lblSenha:Location = NEW System.Drawing.Point(23, 45).
        THIS-OBJECT:lblSenha:Size = NEW System.Drawing.Size(40, 20).
        THIS-OBJECT:lblSenha:TEXT = "Senha:".

        /* txtUsuario */
        THIS-OBJECT:txtUsuario:Location = NEW System.Drawing.Point(60, 20).
        THIS-OBJECT:txtUsuario:Size = NEW System.Drawing.Size(200, 20).
        
        /* txtSenha */
        THIS-OBJECT:txtSenha:Location = NEW System.Drawing.Point(60,42).
        THIS-OBJECT:txtSenha:Size = NEW System.Drawing.Size(200, 20).
        THIS-OBJECT:txtSenha:PasswordChar = "*".
		THIS-OBJECT:txtSenha:KeyPress:SUBSCRIBE(txtSenhaCmdKey).
        
        /* btOk */
        THIS-OBJECT:btOk:Location = NEW System.Drawing.Point(60,64).
        THIS-OBJECT:btOk:Size = NEW System.Drawing.Size(80, 25).
        THIS-OBJECT:btOk:TEXT = "OK".
        THIS-OBJECT:btOk:Click:SUBSCRIBE(botaoOk).

        /* btCancel */
        THIS-OBJECT:btCancel:Location = NEW System.Drawing.Point(142,64).
        THIS-OBJECT:btCancel:Size = NEW System.Drawing.Size(80, 25).
        THIS-OBJECT:btCancel:TEXT = "Cancelar".
        THIS-OBJECT:btCancel:Click:SUBSCRIBE(botaoCancelar).

        /* Window */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(300, 110).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:txtUsuario).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:txtSenha).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btOk).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:btCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:lblUsuario).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:lblSenha).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowContainer1).
        THIS-OBJECT:Name = "wLogin".
        THIS-OBJECT:Text = "Login Totvs 11".
        THIS-OBJECT:ResumeLayout(FALSE).

    END METHOD.
    
    METHOD PRIVATE VOID botaoCancelar (sender AS System.Object, e AS System.EventArgs):
        /* Seta variavel de situacao como cancelada e fecha a tela. */
        ASSIGN situacaoLogin = 'CANCELADO'.
        THIS-OBJECT:CLOSE().
    END METHOD.
	
	METHOD PRIVATE VOID txtSenhaCmdKey (sender AS System.Object, e AS System.EventArgs):
        /* Se pressionar a tecla enter, executa a rotina do botao ok */
        IF CAST(e,System.Windows.Forms.KeyPressEventArgs):KeyChar  = CHR(13) THEN
            botaoOk(sender,e).
	END.	
	
    METHOD PRIVATE VOID botaoOk (sender AS System.Object, e AS System.EventArgs):

        /* Api de login padrao do totvs 11 */
        RUN btb/btapi910za.p (
            txtUsuario:TEXT, 
            txtSenha:TEXT, 
            OUTPUT TABLE tt-erro-logon).
            
        /* Se encontrar erro, exibe na tela, senao fecha o programa 
           setando a variavel de situacao como ok. */
        FIND FIRST tt-erro-logon NO-ERROR.
        IF AVAIL tt-erro-logon THEN DO:
            MESSAGE tt-erro-logon.desc-erro
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ASSIGN situacaoLogin = 'ERRO'.
        END.
        ELSE DO:
            ASSIGN situacaoLogin = 'OK'.
            THIS-OBJECT:CLOSE().
        END.
        
    END METHOD.
    
    /* Metodo publico para descobrir a situacao do login. */
    METHOD PUBLIC CHAR getSituacaoLogin ():
        RETURN situacaoLogin.
    END METHOD.
    
    DESTRUCTOR PUBLIC Login ( ):
        
	END DESTRUCTOR.


END CLASS.
