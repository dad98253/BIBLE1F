	PROGRAM TE
	CHARACTER*50 A
	OPEN ( UNIT = 1 , FILE = 'BWORDS' , STATUS = 'OLD' )
	OPEN ( UNIT = 2 , FILE = 'NWORDS' , STATUS = 'NEW' )
	DO 100 I = 1 , 100
	READ ( 1 , '(A)' ) A
	WRITE ( 2 , '(A)' ) A
  100	CONTINUE
	END
