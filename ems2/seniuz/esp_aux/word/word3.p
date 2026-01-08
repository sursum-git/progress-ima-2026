define var ch-Word as component-handle no-undo.
define var ch-Documento as component-handle no-undo.
define var ch-FormField as component-handle no-undo.
define var ch-Range as component-handle no-undo.
CREATE "Word.Application" ch-Word.
ch-Word:Documents:ADD().
ch-Word:visible = YES.
ch-word:Selection:TypeParagraph.
ch-word:Selection:Font:Name = "Arial Narrow" .
ch-word:Selection:Font:Size = 12.
ch-word:Selection:Font:Bold = true.
FOR EACH funcionario WHERE cdn_empresa = 1 AND cdn_funcionario = 581.
    ch-word:Selection:Font:underline = TRUE.
    ch-word:Selection:TypeText("TRABALHO A TÖTULO DE EXPERIÒNCIA"). 
    ch-word:Selection:Font:underline = FALSE. /* Sublinhado */   
    ch-word:Selection:ParagraphFormat:Alignment = "1". /* Centralizado */ 
    ch-word:Selection:TypeParagraph. /* Qubra de Linha */ 
    ch-word:Selection:TypeParagraph.    
    ch-word:Selection:ParagraphFormat:Alignment = "0". /* Alinhado a Esquerda */
    ch-word:Selection:Font:Bold = FALSE.
    ch-word:Selection:TypeText("    Pelo presente instrumento de Contrato de Trabalho entre a ").
    ch-word:Selection:Font:Bold = TRUE.
    ch-word:Selection:TypeText(&NomePessoaFisica).
    ch-word:Selection:Font:Bold = FALSE.
    ch-word:Selection:TypeText(" , recebo ").
    ch-word:Selection:Font:Bold = TRUE.
    ch-word:Selection:TypeText(&ValorSalario). 
    ch-word:Selection:TypeText(" R$ e a minha data de amissao foi ").
    ch-word:Selection:Font:Bold = FALSE.
    ch-word:Selection:TypeText(&DataAdmissao).
END.
release object ch-Word.
