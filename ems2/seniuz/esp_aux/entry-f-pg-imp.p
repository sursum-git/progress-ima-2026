  IF rs-destino:DISABLE(ENTRY(1,(rs-destino:RADIO-BUTTONS IN FRAME f-pg-imp))) THEN.
  IF rs-destino:DISABLE(ENTRY(5,(rs-destino:RADIO-BUTTONS IN FRAME f-pg-imp))) THEN.

  rs-execucao:SENSITIVE = NO.

  ASSIGN rs-destino:SCREEN-VALUE = '2'.
  APPLY 'value-changed' TO rs-destino.
  ASSIGN c-arquivo:SCREEN-VALUE = "C:\TEMP\VENDAS.XLS".
