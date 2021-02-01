       PROGRAM COMPARE
       CHARACTER*80 A,B
       OPEN ( UNIT = 1 , FILE = 'BWORDS' , STATUS = 'OLD' ,
     +	BLOCKSIZE = 60 000 )
       OPEN ( UNIT = 2 , FILE = 'BWORDS.TXT' , STATUS = 'OLD' ,
     +	BLOCKSIZE = 60 000 )
	GO TO 3
    1	CONTINUE
	READ ( 1 , 5 , END = 1000 ) A
	GO TO 4
    2	CONTINUE
	READ ( 2 , 5 , END = 1000 ) B
	GO TO 4
    3	CONTINUE
	READ ( 1 , 5 , END = 1000 ) A
	READ ( 2 , 5 , END = 1000 ) B
    4	CONTINUE
	IF ( A .EQ. B ) GO TO 3
	WRITE ( * , 6 ) A(1:79) , B(1:79)
    5	FORMAT ( A )
    6	FORMAT ( 1X , A79 )
	READ ( * , 7 , END = 1000 ) I
    7	FORMAT ( I1 )
	IF ( I .LE. 0 ) STOP
	IF ( I - 2 ) 1 , 2 , 3
 1000	CONTINUE
	END
