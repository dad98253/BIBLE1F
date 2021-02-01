	PROGRAM BIBLE1
	parameter ( maxver = 33 000 )
	PARAMETER ( MAXWRD = 25 000 )
	PARAMETER ( NDIMW  =	190 )
	PARAMETER ( NLENW  =   1024 )
C       PARAMETER ( MAXVRS =    250 )
        parameter ( maxlin =     80 )
c
	character*(maxlin) a
c       character*maxvrs b
	character*16	 books(66) , BOOK
	CHARACTER*32 AWORD
	character*(nlenw)  words(ndimw)
	character*10	 fname , FMT1 , FMT2
C
	CHARACTER*1 PUNCT
	PARAMETER ( NPUN = 8 )
	DIMENSION PUNCT(NPUN)
	COMMON / PUNCTS / PUNCT
c
	dimension lws(256) , lwl(256) , NCOUNT(MAXWRD)
        dimension iws(maxwrd) , iwl(maxwrd)
c
	integer*2 itvers , ivers , ichap , I2LOC
	integer*1 iwl , lwl , nwl
C
	LOGICAL NEWSENT
	LOGICAL GOODNA
c
	common /prsdat/ a , lws , lwl , nwl , itvers , ivers , ichap
	COMMON /BOOK/ BOOK
	common /cnts/	aword , laword , lastwd , lastsb
     +			, NHASH
	COMMON / NCNT / NCOUNT
	COMMON /ERR/ IER
        common /iws/ iws
        common /iwl/ iwl
        common /words/ words
	COMMON /FMTS/ FMT1 , FMT2
	COMMON /ILOCS/ IADV , IADC , IADB
	COMMON /BUF/ I2IX(256) , LASTAD , IRECIN , IRMOD , I2ZERO
	INTEGER*2 I2ZERO
	INTEGER*2 I2IX
	COMMON /VERTOC/ IADVER(500) , LASTVR , IBASVR , ITOTVR
	INTEGER*2 LASTVR
	INTEGER*2 I2AV(1000)
	EQUIVALENCE ( I2AV , IADVER )
	COMMON /BOKTOC/ IADBOK(67)
	COMMON /CHPTOC/ IADCHP(1200) , LASTCH
C
	INTEGER*2 IOVER
C
C	OPEN ( UNIT = 2 , FILE = 'BWORDS.DAT' , STATUS = 'OLD' ,
C    +	       FORM = 'UNFORMATTED' , BLOCKSIZE = 30 000 )
C	READ ( 2 ) aword , laword , lastwd , lastsb
C    +		 , NHASH , iws , iwl , words
C	CLOSE ( 2 )
	OPEN ( UNIT = 3 , FILE = 'COUNT.TXT' , STATUS = 'UNKNOWN' )
	OPEN ( UNIT = 10 , FILE = 'KJB2P0.DAT' , STATUS = 'UNKNOWN' ,
     +	       FORM = 'UNFORMATTED' , ACCESS = 'DIRECT' , RECL = 512 )
	OPEN ( UNIT = 11 , FILE = 'SORTFILE.DAT' , STATUS = 'UNKNOWN' ,
     +	       FORM = 'UNFORMATTED' , BLOCKSIZE = 15 000 )
c
	FMT1 = '( I1 )'
	FMT2 = '( I2 )'
	NEWSENT = .TRUE.
	NHASH = 0
	I2ZERO = 0
	LASTVR = 0
	LASTCH = 0
	LASTAD = 10
	MLAWRD = 0
	ITVERS = 0
	IRMOD = 0
	IRECIN = 1
	IADV = LASTAD + 1
C
	DO 5 I = 1 , MAXVER * 2
    5	CALL PUTWRD ( I2ZERO , LASTAD + 1 )
C
	IADC = LASTAD + 1
	DO 6 I = 1 , 1200 * 2
    6	CALL PUTWRD ( I2ZERO , LASTAD + 1 )
C
	IADB = LASTAD + 1
	DO 7 I = 1 , 134
    7	CALL PUTWRD ( I2ZERO , LASTAD + 1 )
C
	LASTVR = 0
	IBASVR = 0
	ITOTVR = 0
c
	DO 88 I = NPUN + 1 , MAXWRD
	iws(I) = 0
	iwl(I) = 0
   88	CONTINUE
c
	DO 89 I = 1 , NPUN
	iws(I) = I
	iwl(I) = 1
	WORDS(1)(I:I) = PUNCT(I)
   89	CONTINUE
c
	lastwd = NPUN
	lastsb = 1
c
	DO 1000 I =   1 , 66
	IADBOK(I) = LASTCH + 1
	IF ( I .NE. 1 ) WRITE ( 3 , 33 ) BOOK , ICHAP , IVERS , LASTSB
   33	FORMAT ( 1X , A , 2I5 , I8 )
	IVERS = 0
	ICHAP = 0
	WRITE ( FNAME , 1 ) I
    1	FORMAT ( 'BOOK' , I2.2 , '.TXT' )
	INQUIRE ( FILE = FNAME , EXIST = GOODNA )
	IF ( .NOT. GOODNA ) THEN
	   WRITE ( 3 , * ) ' ' , FNAME , ' NOT FOUND'
	   PRINT * , ' ' , FNAME , ' NOT FOUND'
	   PRINT * , ' '
	   GO TO 900
	END IF
	IF ( I .NE. 1 ) CLOSE ( 1 )
	OPEN ( UNIT = 1 , FILE = FNAME , STATUS = 'OLD' ,
     +		BLOCKSIZE = 15 000 )
   99	READ ( 1 , 2 ) BOOKS(I)
	BOOK = BOOKS(I)
	IF ( BOOK(1:4) .EQ. 'BOOK' ) THEN
	    READ ( 1 , 2 )
	    GO TO 99
	END IF
    2	FORMAT ( A )
    3	FORMAT ( 1X , A )
  100	CONTINUE
	READ ( 1 , 2 , END = 900 ) A
	DO 73 JK = 1 , MAXLIN
	    IF ( A(JK:JK) .LT. ' ' ) A(JK:JK) = ' '
   73	CONTINUE
	JK2 = 0
	DO 74 JK = 1 , MAXLIN
	    IF ( A(JK:JK) .GT. 'z' ) GO TO 74
	    JK2 = JK2 + 1
	    A(JK2:JK2) = A(JK:JK)
   74	CONTINUE
	IF ( JK2 .NE. MAXLIN ) THEN
	    DO 75 JK = JK2+1 , MAXLIN
		A(JK:JK) = ' '
   75	    CONTINUE
	END IF
	CALL PARSE
	IF ( IER .NE. 0 ) THEN
	    WRITE ( * , * ) ' ERROR NUMBER ' , IER
	    GO TO 1001
	END IF
	IF ( NWL .EQ. 0 ) GO TO 100
	DO 200 J = 1 , NWL
	AWORD = A(LWS(J):LWS(J)+LWL(J)-1)
	LAWORD = LWL(J)
	MLAWRD = MAX0 ( MLAWRD , LAWORD )
	IF ( NEWSENT ) THEN
	    IF ( IOVER .NE. IVERS ) CALL PUTWRD ( I2ZERO , LASTAD + 1 )
	    NEWSENT = .FALSE.
	END IF
	IOVER = IVERS
	IF ( LAWORD .EQ. 1 ) THEN
	    IF ( AWORD(1:1) .EQ. '.' .OR. AWORD(1:1) .EQ. '?' .OR.
     +		 AWORD(1:1) .EQ. '!' )
     +	       NEWSENT = .TRUE.
	END IF
C	 IF ( AWORD(1:1) .GE. 'A' .AND. AWORD(1:1) .LE. 'Z' ) THEN
C	     ICH1 = ICHAR ( AWORD(1:1) )
C	     AWORD(1:1) = CHAR ( ICH1 + 32 )
C	 END IF
	DO 7671 IX = 1 , LAWORD
	    IF ( AWORD(IX:IX) .EQ. '-' ) GO TO 7672
 7671	CONTINUE
	GO TO 7673
 7672	CONTINUE
	WRITE ( 3 , 76 )
     +	AWORD(1:LAWORD) , BOOK , ICHAP , IVERS
   76	FORMAT ( ' AWORD = ' , A , ' AT ' , A , 2I5 )
 7673	CONTINUE
	CALL CNTWRD
	IF ( IER .NE. 0 ) THEN
	    WRITE ( * , * ) ' ERROR NUMBER ' , IER
	    GO TO 1001
	END IF
  200	CONTINUE
	GO TO 100
  900	CONTINUE
	CALL WRTCT
 1000	CONTINUE
 1001	CONTINUE
	IADBOK(67) = LASTCH + 1
	IVERS = 0
	ICHAP = 0
	LASTVR = LASTVR + 1
	IADVER(LASTVR) = LASTAD + 1 - IBASVR
	CALL WRTCT
	CLOSE ( 1 )
	WRITE ( * , * ) ' MAX LAWORD = ' , MLAWRD
	IF ( LASTVR .NE. 0 ) THEN
	    DO 1002 IX = 1 , LASTVR * 2
		CALL PUTWRD ( I2AV(IX) , IADV + IX + ITOTVR - 1 )
 1002	    CONTINUE
	END IF
	CALL WRTOC
	LOCWL = LASTAD + 1
	CALL WRTWL
	IADOFF = LASTAD + 1
	IF ( MOD ( IADOFF , 2 ) .EQ. 1 ) IADOFF = IADOFF + 1
	CALL CHPWRD ( 'BIB1.0' , 6 , 1 )
	CALL I4PWRD ( IADC , 4 )
	CALL I4PWRD ( LOCWL , 6 )
	CALL I4PWRD ( IADOFF , 8 )
	WRITE ( 10 , REC = 1 ) I2IX
	CLOSE ( 10 )
	OPEN ( UNIT = 2 , FILE = 'DUPWRD.TXT' , STATUS = 'UNKNOWN' ,
     +	       BLOCKSIZE = 15 000 )
	DO 1900 I = 1 , MAXWRD
	IF ( I .LT. 10 ) PRINT * , ' I,IWS(I),IWL(I)',I,IWS(I),IWL(I)
	IF ( IWS(I) .EQ. 0 ) GO TO 1900
	INDX = IWS(I) / NLENW + 1
	ILOC = MOD ( IWS(I) , NLENW )
	IF ( ILOC .EQ. 0 ) THEN
	    INDX = INDX - 1
	    ILOC = NLENW
	END IF
	ICNUM = ICHAR ( WORDS(INDX)(ILOC:ILOC) )
	IF ( ICNUM .GT. 64 .AND. ICNUM .LT. 91 ) THEN
	    IF ( IWL(I) .GT. 1 ) THEN
		ICNUM2 = ICHAR ( WORDS(INDX)(ILOC+1:ILOC+1) )
		IF ( ICNUM2 .GT. 64 .AND. ICNUM2 .LT. 91 ) GO TO 1900
	    END IF
	    LAWORD = IWL(I)
	    AWORD = WORDS(INDX)(ILOC:ILOC-1+IWL(I))
	    AWORD(1:1) = CHAR ( ICNUM +32 )
	    CALL FNDWRD ( I2LOC )
C	     PRINT * , ' I,IWL(I),I2LOC,WORD=',I,IWL(I),I2LOC,
C     + 		 WORDS(INDX)(ILOC:ILOC-1+IWL(I))
	    IF ( I2LOC .EQ. 0 ) GO TO 1900
	    NCOUNT(I2LOC) = NCOUNT(I2LOC) + NCOUNT(I)
	    NCOUNT(I) = 0
	    WRITE ( 2 , 1901 ) I , I2LOC
 1901	    FORMAT ( 2I10 )
	END IF
 1900	CONTINUE
	LAWORD = 1
	DO 1903 I = 1 , NPUN
	    AWORD = PUNCT(I)
	    CALL FNDWRD (I2LOC)
	    WRITE ( 2 , 1901 ) I2LOC , I
 1903	CONTINUE
	CLOSE ( 2 )
C
	OPEN ( UNIT = 2 , FILE = 'BWORDS.TXT' , STATUS = 'UNKNOWN' ,
     +	       BLOCKSIZE = 15 000 )
	DO 2000 I = 1 , MAXWRD
	IF ( IWS(I) .EQ. 0 ) GO TO 2000
	INDX = IWS(I) / NLENW + 1
	ILOC = MOD ( IWS(I) , NLENW )
	IF ( ILOC .EQ. 0 ) THEN
	    INDX = INDX - 1
	    ILOC = NLENW
	END IF
	WRITE ( 2 , 2001 ) I , NCOUNT(I) ,
     +			   WORDS(INDX)(ILOC:ILOC-1+IWL(I))
 2000	CONTINUE
 2001	FORMAT ( 2I10 , 1X ,	   A	   )
	CLOSE ( 2 )
	END
	SUBROUTINE CNTWRD
	PARAMETER ( MAXWRD = 25 000 )
	PARAMETER ( MAXWR9 = MAXWRD * 9 / 10 )
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
	dimension lws(256) , lwl(256) , NCOUNT(MAXWRD)
        dimension iws(maxwrd) , iwl(maxwrd)
c
	integer*2 itvers , ivers , ichap
	integer*1 iwl , lwl , nwl
c
	COMMON /BUF/ I2IX(256) , LASTAD , IRECIN , IRMOD , I2ZERO
	INTEGER*2 I2ZERO
	INTEGER*2 I2IX
	common /prsdat/ a , lws , lwl , nwl , itvers , ivers , ichap
	COMMON /BOOK/ BOOK
	common /cnts/	aword , laword , lastwd , lastsb
     +			, NHASH
	COMMON / NCNT / NCOUNT
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
	GO TO 30
   20	CONTINUE
	IHASH = IHASH + 1
	IF ( IHASH .GT. MAXWRD ) IHASH = 1
   30	CONTINUE
	IF ( IWL(IHASH) .EQ. 0 ) THEN
	    NCOUNT(IHASH) = 1
	    IF ( ( LASTWD + LAWORD ) .GT. NLENW ) THEN
		LASTWD = 0
		LASTSB = LASTSB + 1
		WRITE ( * , 77 ) LASTSB
   77		FORMAT ( '+' , 30X , ' LASTSB = ' , I8 )
		IF ( LASTSB .GT. NDIMW ) THEN
		    IER = 1
		    RETURN
		END IF
	    END IF
	    WORDS(LASTSB)(LASTWD+1:LASTWD+LAWORD) = AWORD(1:LAWORD)
C	     WRITE ( * , * ) ' ' , WORDS(LASTSB)(LASTWD+1:LASTWD+LAWORD)
C	     WRITE ( * , * ) ' AWORD = ' , AWORD(1:LAWORD)
C	 WRITE ( * , * ) ' LASTWD,LASTSB,LAWORD = ' ,LASTWD,LASTSB,
C     +  LAWORD
	    IWS(IHASH) = ( LASTSB - 1 ) * NLENW + LASTWD + 1
	    IWL(IHASH) = LAWORD
	    LASTWD = LASTWD + LAWORD
	    NHASH = NHASH + 1
	    IF ( NHASH .GT. MAXWR9 ) THEN
		IER = 2
		RETURN
	    END IF
C	 WRITE ( * , * ) ' LASTWD,LASTSB,LAWORD = ' ,LASTWD,LASTSB,
C     +  LAWORD
C	 WRITE ( * , * ) ' IWS,IWL = ' , IWS(IHASH) , IWL(IHASH)
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
	    NCOUNT(IHASH) = NCOUNT(IHASH) + 1
	END IF
	I2HASH = IHASH
	CALL PUTWRD ( I2HASH , LASTAD + 1 )
	WRITE ( 11 ) I2HASH , ITVERS
	RETURN
	END
	SUBROUTINE PARSE
        parameter ( maxlin =     80 )
	PARAMETER ( MAXWRD = 25 000 )
	CHARACTER*32 AWORD
	CHARACTER*16 BOOK
	CHARACTER*10 FORM , FMT1 , FMT2
C
	CHARACTER*1 PUNCT
	PARAMETER ( NPUN = 8 )
	DIMENSION PUNCT(NPUN)
	COMMON / PUNCTS / PUNCT
C
	character*(maxlin) a
c
	DIMENSION IJK(20)
	dimension lws(256) , lwl(256) , NCOUNT(MAXWRD)
c
	integer*2 itvers , ivers , ichap
	integer*1 lwl , nwl
c
	common /prsdat/ a , lws , lwl , nwl , itvers , ivers , ichap
	COMMON /BOOK/ BOOK
	common /cnts/	aword , laword , lastwd , lastsb
     +			, NHASH
	COMMON / NCNT / NCOUNT
	COMMON /FMTS/ FMT1 , FMT2
	COMMON /ERR/ IER
	COMMON /BUF/ I2IX(256) , LASTAD , IRECIN , IRMOD , I2ZERO
	INTEGER*2 I2ZERO
	INTEGER*2 I2IX
	COMMON /VERTOC/ IADVER(500) , LASTVR , IBASVR , ITOTVR
	INTEGER*2 LASTVR
	INTEGER*2 I2AV(1000)
	EQUIVALENCE ( I2AV , IADVER )
C
	NWL = 0
	IBEG = 1
	IF ( A(1:1) .EQ. 'C' ) THEN
	    IF ( A(1:7) .NE. 'CHAPTER' ) GO TO 1
	    IF ( ICHAP .NE. 0 ) THEN
		WRITE ( 3 , 33 ) BOOK , ICHAP , IVERS , LASTSB
		CALL WRTCT
	    END IF
   33	    FORMAT ( 1X , A , 2I5 , I8 )
	    DO 2 I = 8 , MAXLIN
		J = I
		IF ( A(I:I) .NE. ' ' ) GO TO 3
    2	    CONTINUE
    3	    CONTINUE
	    IF ( J .EQ. MAXLIN ) STOP 222
	    DO 4 I = J+1 , MAXLIN
		K = I
		IF ( A(I:I) .EQ. ' ' ) GO TO 5
    4	    CONTINUE
    5	    CONTINUE
	    IF ( K - J .EQ. 1 ) THEN
		READ ( A(J:K-1) , FMT1 ) ICHAP
	    ELSE IF ( K - J .EQ. 2 ) THEN
		READ ( A(J:K-1) , FMT2 ) ICHAP
	    ELSE
		DO 88 JK = 1 , 20
		   IJK(JK) = ICHAR ( A(JK:JK) )
   88		CONTINUE
		WRITE ( * , 89 ) IJK
   89		FORMAT ( 16I5 )
		IER = 4
		RETURN
	    END IF
	    IVERS = 0
	    RETURN
	END IF
    1	CONTINUE
	IF ( A(1:1) .GE. '0' .AND. A(1:1) .LE. '9' ) THEN
C	    LAST ?
	    DO 10 I = 1 , MAXLIN
		J = I
		IF ( A(I:I) .EQ. ' ' ) GO TO 20
		IF ( A(I:I) .LT. '0' .OR. A(I:I) .GT. '9' )
     +		     GO TO 30
   10		CONTINUE
		STOP 101
   20	    CONTINUE
	    J = J - 1
	    IF ( J .EQ. 1 ) THEN
		READ ( A(1:J) , FMT1 ) NEWVER
	    ELSE  IF ( J .EQ. 2 ) THEN
		READ ( A(1:J) , FMT2 ) NEWVER
	    ELSE
		GO TO 30
	    END IF
	    IF ( NEWVER .NE. IVERS + 1 ) GO TO 30
	    IVERS = IVERS + 1
	    ITVERS = ITVERS + 1
	    WRITE ( * , 44 ) BOOK(1:16) , ICHAP , IVERS
   44	    FORMAT ( 1H+ , A , 2I4 )
	    LASTVR = LASTVR + 1
	    IADVER(LASTVR) = LASTAD + 1 - IBASVR
	    IBEG = J + 2
	END IF
   30	CONTINUE
	DO 40 I = IBEG , MAXLIN
	    IF ( A(I:I) .NE. ' ' ) THEN
	       NWL = NWL + 1
	       LWS(NWL) = I
	       J = I
	       GO TO 50
	    END IF
   40	CONTINUE
	RETURN
   50	CONTINUE
	K = J
	DO 60 I = J , MAXLIN
	    IF ( A(I:I) .EQ. ' ') GO TO 70
	    DO 80 I2 = 1 , NPUN
		IF ( A(I:I) .EQ. PUNCT(I2) ) GO TO 70
   80	    CONTINUE
	    K = I
   60	CONTINUE
   70	CONTINUE
	LWL(NWL) = K - J + 1
   90	CONTINUE
	IF ( K .EQ. MAXLIN ) RETURN
	IBEG = K + 1
	GO TO 30
	END
	SUBROUTINE WRTOC
	COMMON /ILOCS/ IADV , IADC , IADB
	COMMON /BUF/ I2IX(256) , LASTAD , IRECIN , IRMOD , I2ZERO
	INTEGER*2 I2ZERO
	INTEGER*2 I2IX
	COMMON /BOKTOC/ IADBOK(67)
	COMMON /CHPTOC/ IADCHP(1200) , LASTCH
	INTEGER*2 I2AC(2400) , I2BK(134)
	EQUIVALENCE ( I2AC , IADCHP )
	EQUIVALENCE ( I2BK , IADBOK )
	DO 100 I = 1 , LASTCH * 2
	    CALL PUTWRD ( I2AC(I) , I - 1 + IADC )
  100	CONTINUE
	DO 200 I = 1 , 134
	    CALL PUTWRD ( I2BK(I) , I - 1 + IADB )
  200	CONTINUE
	RETURN
	END
	SUBROUTINE WRTCT
	COMMON /ILOCS/ IADV , IADC , IADB
	COMMON /BUF/ I2IX(256) , LASTAD , IRECIN , IRMOD , I2ZERO
	INTEGER*2 I2ZERO
	INTEGER*2 I2IX
	COMMON /VERTOC/ IADVER(500) , LASTVR , IBASVR , ITOTVR
	INTEGER*2 LASTVR
	INTEGER*2 I2AV(1000)
	EQUIVALENCE ( I2AV , IADVER )
	COMMON /BOKTOC/ IADBOK(67)
	COMMON /CHPTOC/ IADCHP(1200) , LASTCH
	INTEGER*2 IHIADD , ILOADD , ICH
	LASTCH = LASTCH + 1
	IADCHP(LASTCH) = LASTVR + ITOTVR
	DO 100 I = 1 , LASTVR * 2
	    CALL PUTWRD ( I2AV(I) , I + ITOTVR + IADV - 1 )
  100	CONTINUE
	ITOTVR = ITOTVR + LASTVR * 2
	LASTVR = 0
	RETURN
	END
	SUBROUTINE WRTWL
	COMMON /BUF/ I2IX(256) , LASTAD , IRECIN , IRMOD , I2ZERO
	INTEGER*2 I2ZERO
	INTEGER*2 I2IX
	PARAMETER ( MAXWRD = 25 000 )
	PARAMETER ( NDIMW  =	190 )
	PARAMETER ( NLENW  =   1024 )
c
	character*(nlenw)  words(ndimw)
	CHARACTER*32 AWORD
c
	dimension iws(maxwrd) , iwl(maxwrd) , ncount(maxwrd)
c
	integer*1 iwl
C
	common /cnts/	aword , laword , lastwd , lastsb
     +			, NHASH
	COMMON / NCNT / NCOUNT
        common /iws/ iws
        common /iwl/ iwl
        common /words/ words
	CALL I4PWRD ( MAXWRD , LASTAD + 1 )
	CALL I1PWRD ( IWL , MAXWRD , LASTAD + 1 )
	DO 3000 I = 1 , MAXWRD
	    IF ( IWL(I) .EQ. 0 ) GO TO 3000
	    CALL I4PWRD ( IWS(I) , LASTAD + 1 )
 3000	CONTINUE
	CALL I4PWRD ( LASTSB , LASTAD + 1 )
	IF ( MOD ( LASTWD , 2 ) .NE. 0 ) LASTWD = LASTWD + 1
	CALL I4PWRD ( LASTWD , LASTAD + 1 )
	DO 3100 I = 1 , LASTSB - 1
	CALL CHPWRD ( WORDS(I)(1:NLENW) , NLENW , LASTAD + 1 )
 3100	CONTINUE
	IF ( LASTWD .NE. 0 )
     +	   CALL CHPWRD ( WORDS(LASTSB)(1:LASTWD) , LASTWD
     +		       , LASTAD + 1  )
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
	I2HASH = 0
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
	    R E T U R N
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
	BLOCK DATA
C
	CHARACTER*1 PUNCT
	PARAMETER ( NPUN = 8 )
	DIMENSION PUNCT(NPUN)
	COMMON / PUNCTS / PUNCT
C
	DATA PUNCT / '.' , '!' , '?' , ':' , ';' , ',' , ')' , '(' /
C
	END
