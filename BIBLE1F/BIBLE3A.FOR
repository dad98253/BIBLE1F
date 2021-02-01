	PROGRAM BIBLE3A
	PARAMETER ( MAXVER = 33 000 )
	PARAMETER ( MAXWRD = 25 000 )
	PARAMETER ( NDIMW  =	190 )
	PARAMETER ( NLENW  =   1024 )
	PARAMETER ( NPUN = 8 )
        parameter ( maxlin =     80 )
c
	character*(maxlin) a
c       character*maxvrs b
	character*16 books(67) , BOOK
	CHARACTER*32 AWORD
	CHARACTER*81 ALN
	character*(nlenw)  words(ndimw)
	character*10	 fname , FMT1 , FMT2
	INTEGER*2 I2WRD , IBSHI , IBSLOW
c
	dimension lws(256) , lwl(256)
        dimension iws(maxwrd) , iwl(maxwrd)
c
	integer*2 itvers , ivers , ichap
	integer*1 iwl , lwl , nwl
C
	LOGICAL ICAP
	LOGICAL GOODNA
c
	common /prsdat/ a , lws , lwl , nwl , itvers , ivers , ichap
	COMMON /BOOK/ BOOK
	common /cnts/	aword , laword , lastwd , lastsb
     +			, NHASH
	COMMON /ERR/ IER
        common /iws/ iws
        common /iwl/ iwl
        common /words/ words
	COMMON /FMTS/ FMT1 , FMT2
	COMMON /BUF/ I2IX(256) , LASTAD , IRECIN , IRMOD , I2ZERO
	INTEGER*2 I2ZERO
	INTEGER*2 I2IX
	COMMON /VERTOC/ IADVER(500) , LASTVR , IBASVR , ITOTVR
	INTEGER*2 LASTVR
	INTEGER*2 I2AV(1000)
	EQUIVALENCE ( I2AV , IADVER )
	COMMON /BOKTOC/ IADBOK(67)
	COMMON /CHPTOC/ IADCHP(1200) , LASTCH
	INTEGER*2 I2AC(2400) , I2BK(134)
	EQUIVALENCE ( I2AC , IADCHP )
	EQUIVALENCE ( I2BK , IADBOK )
	COMMON /XREF/ IADIN , IADOFF
	INTEGER*2 I2WORD
	INTEGER*2 I2REF
	INTEGER*2 I2WOLD
	INTEGER*2 I2NREF
	DIMENSION I2WLOW(512)
	INTEGER*2 I2WLOW
	EQUIVALENCE ( I2WLOW , IWS )
C
	INTEGER*2 IDUP , I2ADD
	DIMENSION IDUP(MAXWRD)
C
	FMT1 = '( I1 )'
	FMT2 = '( I2 )'
	NHASH = 0
	I2ZERO = 0
	LASTVR = 0
	LASTCH = 0
	LASTAD = 10
	MLAWRD = 0
	ITVERS = 0
	IRMOD = 0
	IRECIN = -1
c
	DO 88 I = 1 , MAXWRD
	iws(I) = 0
	iwl(I) = 0
	IDUP(I) = I
   88	CONTINUE
c
	lastwd = 0
	lastsb = 1
c
	OPEN ( UNIT = 2 , FILE = 'DUPWRD.TXT' , STATUS = 'OLD' ,
     +	       BLOCKSIZE = 15 000 )
   89	CONTINUE
	READ ( 2 , 90 , END = 91 ) I , I2ADD
   90	FORMAT ( 2I10 )
	IDUP(I) = I2ADD
	GO TO 89
   91	CONTINUE
	CLOSE ( 2 )
C
	OPEN ( UNIT = 10 , FILE = 'KJB2P0.DAT' , STATUS = 'UNKNOWN' ,
     +	       FORM = 'UNFORMATTED' , ACCESS = 'DIRECT' , RECL = 512 )
c
	LASTAD = 1
	CALL CHGWRD ( AWORD , 6 , LASTAD )
	PRINT * , ' VERSION = ' , AWORD(1:6)
	LASTAD = 4
	CALL I4GWRD ( LOCTOC , LASTAD )
	PRINT * , ' LOCATION OF TOC =' ,  LOCTOC
	LASTAD = 6
	CALL I4GWRD ( LOCWL , LASTAD )
	PRINT * , ' LOCATION OF WORD LIST =' ,	LOCWL
	LASTAD = 8
	CALL I4GWRD ( IADOFF , LASTAD )
	PRINT * , ' LOCATION OF INDEX =' ,  IADOFF
	CALL GETOC ( LOCTOC )
	CALL GETWL ( LOCWL )
	PRINT * , ' LASTAD =' , LASTAD
	LSTAD = LASTAD - 8
	CALL CHGWRD ( AWORD , 32 , LSTAD )
	PRINT * , AWORD
	PRINT * , ' '
	DO 677 IB = 1 , MAXVER , 499
	    IJ = 1
	    DO 600 J = IB * 2 - 1 , ( IB + 499 ) * 2
		CALL GETWRD ( I2AV(IJ) , J + 10 )
		IJ = IJ + 1
  600	    CONTINUE
	    DO 674 K = 1 , 499
		IVS = IADVER(K)
		IVE = IADVER(K+1)
		IF ( IVE .EQ. 0 ) GO TO 678
		DO 673 I = IVS , IVE
		    CALL GETWRD ( I2WRD , I )
		    IF ( I2WRD .NE. 0 ) THEN
			CALL PUTWRD ( IDUP(I2WRD) , I )
		    ELSE
			CALL PUTWRD ( I2WRD , I )
		    END IF
  673		CONTINUE
  674	    CONTINUE
  677	CONTINUE
C
  678	CONTINUE
C
	IADIN = IADOFF
	DO 99 IA = IADIN , IADIN + MAXWRD * 2 - 1
	    CALL PUTWRD ( I2WLOW(1) , IA )
   99	CONTINUE
	DO 98 IX = 1 , MAXWRD
	    IWS(IX) = 0
   98	CONTINUE
	OPEN ( UNIT = 11 , FILE = 'TAPE1.DAT' , STATUS = 'UNKNOWN' ,
     +	       FORM = 'UNFORMATTED' , BLOCKSIZE = 15 000 )
	I2WOLD = 0
  100	CONTINUE
	READ ( 11 , END = 9100 ) I2WORD , I2REF
	IF ( I2WORD .EQ. 32 000 ) GO TO 400
	IF ( I2WORD .NE. I2WOLD ) THEN
	    IF ( IDUP(I2WORD) .LT. NPUN ) GO TO 100
	    IF ( I2WOLD .NE. 0 ) THEN
		CALL PUTWRD ( I2NREF , ISTART )
		WRITE ( * , 101 ) I2WORD , LASTAD , I2NREF
  101		FORMAT ( '+ WORD =' , I6 , I10 , I5 )
	    END IF
	    I2NREF = 0
	    ISTART = LASTAD + 1
	    CALL PUTWRD ( I2ZERO , ISTART )
C	     IW = IDUP(I2WORD)
C	     IWS(IW) = ISTART
	    IWS(I2WORD) = ISTART
	    I2WOLD = I2WORD
	END IF
	I2NREF = I2NREF + 1
	CALL PUTWRD ( I2REF , LASTAD + 1 )
	GO TO 100
  400	CONTINUE
C
C ALL DONE WITH LOADING THE CROSS REFERENCE DATA
C
	CLOSE ( UNIT = 11 )
	CALL PUTWRD ( I2NREF , ISTART )
	IX = 0
	DO 500 IA = IADOFF , IADOFF + MAXWRD * 2 - 1
	    IX = IX + 1
	    CALL PUTWRD ( I2WLOW(IX) , IA )
  500	CONTINUE
C
C FORCE THE LAST BUFFER TO FLUSH
C
	CALL GETWRD ( I2WLOW(1) , 5 )
	GO TO 10 000
 9100	CONTINUE
	STOP 9100
10000	CONTINUE
	END
	SUBROUTINE GETOC ( LOCTC )
	PARAMETER ( MAXVER = 33 000 )
	COMMON /BUF/ I2IX(256) , LASTAD , IRECIN , IRMOD , I2ZERO
	INTEGER*2 I2ZERO
	INTEGER*2 I2IX
	COMMON /BOKTOC/ IADBOK(67)
	COMMON /CHPTOC/ IADCHP(1200) , LASTCH
	INTEGER*2 I2AC(2400) , I2BK(134)
	EQUIVALENCE ( I2AC , IADCHP )
	EQUIVALENCE ( I2BK , IADBOK )
	IAD = LOCTC + MAXVER * 2 - 1
	DO 100 I = 1 , 2400
	IAD = IAD + 1
	    CALL GETWRD ( I2AC(I) , IAD )
C	    IF ( I .LT. 4 .OR. I .GT. LASTCH - 5 )
C     + 	 PRINT * , ' LOCATION OF CHAPTER' , I , ' IS' ,
C		PRINT * , ' LOCATION OF CHAPTER' , I , ' IS' ,
C    +			  IADCHP(I)
  100	CONTINUE
	DO 200 I = 1 , 134
	IAD = IAD + 1
	    CALL GETWRD ( I2BK(I) , IAD )
C	    IF ( I .LT. 4 .OR. I .GT. 65 )
C    +		PRINT * , ' LOCATION OF BOOK' , I , ' IS' ,
C    +			  IADBOK(I)
  200	CONTINUE
	RETURN
	END
	SUBROUTINE GETWL ( IAD )
	PARAMETER ( MAXWRD = 25 000 )
	PARAMETER ( NDIMW  =	190 )
	PARAMETER ( NLENW  =   1024 )
c
	character*(nlenw)  words(ndimw)
	CHARACTER*32 AWORD
c
        dimension iws(maxwrd) , iwl(maxwrd)
c
	integer*1 iwl
C
	common /cnts/	aword , laword , lastwd , lastsb
     +			, NHASH
        common /iws/ iws
        common /iwl/ iwl
        common /words/ words
	COMMON /BUF/ I2IX(256) , LASTAD , IRECIN , IRMOD , I2ZERO
	INTEGER*2 I2ZERO
	INTEGER*2 I2IX
	CALL I4GWRD ( MAXW , IAD )
C	 PRINT * , ' MAXWORD IN FILE =' , MAXW , ' MAXWORD OF PROGRAM ='
C     + 	 , MAXWRD
C	 MAXW = MAXWRD
	IF ( MAXW .NE. MAXWRD ) STOP 3000
	IAD1 = IAD + 2
	CALL I1GWRD ( IWL , MAXW , IAD1 )
	IAD1 = IAD1 + MAXW / 2 + MOD ( MAXW , 2 )
	ICJK = 0
	DO 3000 I = 1 , MAXW
	    ICJK = ICJK + 1
	    IF ( IWL(I) .EQ. 0 ) GO TO 3000
	    CALL I4GWRD ( IWS(I) , IAD1 )
	    IF ( ICJK .LT. 10 ) THEN
		PRINT * , ' LENGTH OF WORD' , I , ' IS' , IWL(I) ,
     +		' STARTING ADDRESS = ' , IWS(I)
	    END IF
	   IAD1 = IAD1 + 2
 3000	CONTINUE
	CALL I4GWRD ( LASTSB , IAD1 )
	PRINT * , ' TOTAL NUMBER OF BLOCKS USED FOR WORD LIST =',LASTSB
	IAD1 = IAD1 + 2
	CALL I4GWRD ( LASTWD , IAD1 )
	PRINT * , ' NUMBER OF CHRACTERS STORED IN LAST BLOCK =',LASTWD
	IAD1 = IAD1 + 2
	PRINT * , ' START OF WORD LIST =' , IAD1
	DO 3100 I = 1 , LASTSB - 1
	CALL CHGWRD ( WORDS(I)(1:NLENW) , NLENW , IAD1 )
c	 IF ( I .LT. 3 ) PRINT * , WORDS(I)(1:78)
	IAD1 = IAD1 + NLENW / 2 + MOD ( NLENW , 2 )
 3100	CONTINUE
	IF ( LASTWD .NE. 0 ) THEN
	   LSTWDF = ( ( LASTWD + 1 ) / 2 ) * 2
	   CALL CHGWRD ( WORDS(LASTSB)(1:LSTWDF) , LSTWDF , IAD1 )
	END IF
	LASTAD = IAD1 + LASTWD / 2 + MOD ( LASTWD , 2 ) + 1
	MADD = 0
	DO 3101 I = 1 , MAXW
	    IF ( IWL(I) .EQ. 0 ) GO TO 3101
	    IS1 = IWS(I)
	    IF ( IWS(I) .GT. MADD ) IW = I
	    MADD = MAX ( MADD , IS1 + IWL(I) - 1 )
C	     IS1A = MOD ( IS1 , NLENW )
C	     ID1 = IS1 / NLENW + 1
C	     IF ( IS1A .EQ. 0 ) THEN
C		 ID1 = ID1 - 1
C		 IS1A = NLENW
C	     END IF
C	     IS2 = IS1A + IWL(I) - 1
C	    PRINT * , ' WORD' , I , ' = ' , WORDS(ID1)(IS1A:IS2)
 3101	CONTINUE
	PRINT * , ' LAST WORD ADDRESS USED WAS' , LASTAD
	PRINT * , ' HIGHEST STRING ADDRESS FOUND =' , MADD
     +		, ' FOR INDEX =' , IW
	    IS1 = IWS(IW)
	    IS1A = MOD ( IS1 , NLENW )
	    ID1 = IS1 / NLENW + 1
	    IF ( IS1A .EQ. 0 ) THEN
		ID1 = ID1 - 1
		IS1A = NLENW
	    END IF
	    IS2 = IS1A + IWL(IW) - 1
	    PRINT * , ' LAST WORD  = ' , WORDS(ID1)(IS1A:IS2)
	RETURN
	END
	SUBROUTINE FNDWRD ( I2HASH )
	PARAMETER ( MAXWRD = 25 000 )
	PARAMETER ( NDIMW  =	190 )
	PARAMETER ( NLENW  =   1024 )
C       PARAMETER ( MAXVRS =    250 )
        parameter ( maxlin =     80 )
c
	character*(maxlin) a
c       character*maxvrs b
	character*16 BOOK
	CHARACTER*32 AWORD
	character*(nlenw)  words(ndimw)
c
	dimension lws(256) , lwl(256)
        dimension iws(maxwrd) , iwl(maxwrd)
c
	integer*2 itvers , ivers , ichap
	integer*1 iwl , lwl , nwl
c
	common /prsdat/ a , lws , lwl , nwl , itvers , ivers , ichap
	COMMON /BOOK/ BOOK
	common /cnts/	aword , laword , lastwd , lastsb
     +			, NHASH
	COMMON /ERR/ IER
        common /iws/ iws
        common /iwl/ iwl
        common /words/ words
c
	INTEGER*2 I2HASH
	IHASH = 0
	LASTC = 1
	DO 10 I = 1 , LAWORD
	    IHASH = IHASH + ICHAR ( AWORD(I:I) ) * LASTC
	    LASTC = ICHAR ( AWORD(I:I) )
   10	CONTINUE
	IHASH = MOD ( IHASH , MAXWRD )
	IF ( IHASH .EQ. 0 ) IHASH = MAXWRD
    1	FORMAT ( ' AWORD,IHASH = ' , A , I10 )
	GO TO 30
   20	CONTINUE
	IHASH = IHASH + 1
	IF ( IHASH .GT. MAXWRD ) IHASH = 1
   30	CONTINUE
	IF ( IWL(IHASH) .EQ. 0 ) THEN
       WRITE ( * , 1 ) ' AWORD(1:LAWORD),IHASH',AWORD(1:LAWORD),IHASH
       WRITE ( * , * ) ' IWL(IHASH),LAWORD = ' ,IWL(IHASH),LAWORD
	    STOP 3030
	ELSE
	    IF ( IWL(IHASH) .NE. LAWORD ) GO TO 20
	    INDX = IWS(IHASH)/NLENW + 1
	    ILOC = MOD ( IWS(IHASH) , NLENW )
	    IF ( ILOC .EQ. 0 ) THEN
		INDX = INDX - 1
		ILOC = NLENW
	    END IF
C	     WRITE ( * , * ) ' INDX,ILOC = ' , INDX , ILOC
C	     WRITE ( * , * ) ' ' , WORDS(INDX)(ILOC:ILOC-1+IWL(IHASH))
	    IF ( WORDS(INDX)(ILOC:ILOC-1+IWL(IHASH)) .NE.
     +		 AWORD(1:LAWORD) ) GO TO 20
	END IF
	I2HASH = IHASH
	RETURN
	END
	SUBROUTINE NCODE ( K , ALN , IO )
	CHARACTER*(*) ALN
	IF ( K .LT. 10 ) THEN
	    WRITE ( ALN , 10 ) K
   10	    FORMAT ( I1 )
	    IO = 1
	    RETURN
	ELSE IF ( K .LT. 100 ) THEN
	    WRITE ( ALN , 100 ) K
  100	    FORMAT ( I2 )
	    IO = 2
	    RETURN
	ELSE
	    WRITE ( ALN , 1000 ) K
 1000	    FORMAT ( I3 )
	    IO = 3
	    RETURN
	END IF
	END
