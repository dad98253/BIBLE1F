	PROGRAM BIBLE4A
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
	COMMON /VERTOC/ IADVER(500) , LASTVR , IBASVR
	INTEGER*2 IADVER , LASTVR
	COMMON /BOKTOC/ IADBOK(67)
	COMMON /CHPTOC/ IADCHP(1200) , LASTCH
	COMMON /XREF/ IADIN , IADOFF
	INTEGER*2 I2WORD
	INTEGER*2 I2REF
	INTEGER*2 I2WOLD
	INTEGER*2 I2NREF
	DIMENSION I2WLOW(512)
C
	INTEGER*1 LV
	COMMON /LVS/ LV(2400)
	INTEGER*2 I2VER
	INTEGER*2 I2POI(2)
	EQUIVALENCE ( I2POI(1) , IPOINT )
	INTEGER*2 I2HILO(2)
	EQUIVALENCE ( I2HILO(1) , I4HILO )
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
   88	CONTINUE
c
	lastwd = 0
	lastsb = 1
c
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
C
	itemp = 2
C	 print * , ' enter itemp:'
C	 read ( * , '(i3)' ) itemp
C	 print	* , ' itemp =' , itemp
  100	CONTINUE
c	 PRINT * , ' IADOFF=',IADOFF
	WRITE ( * , 200 )
  200	FORMAT ( ' ENTER SEARCH WORD : ' \ )
	READ ( * , 300 ) AWORD
	IF ( AWORD .EQ. ' ' ) STOP 200
  300	FORMAT ( A )
	DO 400 I = 1 , 32
	    IF ( AWORD(I:I) .EQ. ' ' ) GO TO 500
	    LAWORD = I
  400	CONTINUE
  500	CONTINUE
	CALL FNDWRD ( I2WRD )
	IF ( I2WRD .LE. IPUN .OR. IWL(I2WRD) .EQ. 0 ) THEN
	    PRINT * , ' WORD NOT FOUND'
	    GO TO 100
	END IF
	ID = I2WRD
	IF ( ID .LE. NPUN ) STOP 501
C	 PRINT * , ' ID,IADOFF=',ID,IADOFF
C	 CALL I4GWRD ( IPOINT , IADOFF + ID * 2 - itemp )
	CALL GETWRD ( I2POI(1) , IADOFF + ID * 2 - itemp )
	CALL GETWRD ( I2POI(2) , IADOFF + ID * 2 - itemp + 1 )
C	 PRINT * , ' IPOINT=' , IPOINT
	CALL GETWRD ( I2NREF , IPOINT )
	WRITE ( * , * ) ' NUMBER OF REFERENCES =' , I2NREF
	NLINE = 1
	DO 700 IR = 1 , I2NREF
	    IO = 1
C	     PRINT * , ' IPOINT+IR=',IPOINT+IR
	    CALL GETWRD ( I2VER , IPOINT + IR )
C	     PRINT * , ' I2VER=',I2VER
	    IAD = 11 + ( I2VER - 1 ) * 2
	    CALL GETWRD ( I2HILO(1) , IAD )
	    IAD = IAD + 1
	    CALL GETWRD ( I2HILO(2) , IAD )
	    IAD = IAD + 1
	    IVS = I4HILO
	    CALL GETWRD ( I2HILO(1) , IAD )
	    IAD = IAD + 1
	    CALL GETWRD ( I2HILO(2) , IAD )
	    IVE = I4HILO - 1
C	     PRINT * , ' IVS,IVE=',IVS,IVE
	    DO 773 I = IVS , IVE
C		 PRINT * , ' I=',I
		CALL GETWRD ( I2WRD , I )
C		 PRINT * , ' I2WRD=',I2WRD
		IF(ICHAP.EQ.-38)PRINT*,'I2WRD=',I2WRD
		IF ( I .EQ. IVS .AND. I2WRD .EQ. 0 ) THEN
		    ICAP = .TRUE.
		    GO TO 773
		END IF
		IF ( IWL(I2WRD) .LE. 0 .OR. IWL(I2WRD) .GT.
     +						   127 ) THEN
		    PRINT * , ' **** ERROR **** I,IVS,I2WRD,IWS='
     +			    , I , IVS , I2WRD , IWS(I2WRD)
		    GO TO 773
		END IF
		IS1 = IWS(I2WRD)
		IS1A = MOD ( IS1 , NLENW )
		ID1 = IS1 / NLENW + 1
		IF ( IS1A .EQ. 0 ) THEN
		    ID1 = ID1 - 1
		    IS1A = NLENW
		END IF
		IS2 = IS1A + IWL(I2WRD) - 1
		IF(ICHAP.EQ.-38)PRINT*,'IS1,IS1A,ID1,IS2=',
     +				       IS1,IS1A,ID1,IS2
		IF(ICHAP.EQ.-38)PRINT*,'WORD=',WORDS(ID1)(IS1A:IS2)
		IF ( IO + IWL(I2WRD) + 1 .GT. 79 .AND. I2WRD
     +		     .GT. 7 ) THEN
		    WRITE ( * , 300 ) ALN(1:IO)
		    NLINE = NLINE + 1
		    IF ( NLINE .GT. 23 ) THEN
			WRITE ( * , 101 )
			READ ( * , 300 )
			NLINE = 0
		    END IF
  101		    FORMAT ( ' ENTER RETURN TO CONTINUE :' \ )
		    IO = 1
		END IF
		IF ( I2WRD .GT. 7 .AND. IO .GT. 0 ) THEN
		    IO = IO + 1
		    ALN(IO:IO) = ' '
		END IF
		IOS = IO + 1
		IO = IO + IWL(I2WRD)
		ALN(IOS:IO) = WORDS(ID1)(IS1A:IS2)
		IF ( ICAP ) THEN
		    IC1 = ICHAR ( WORDS(ID1)(IS1A:IS1A) )
		    IF ( IC1 .GT. 96 ) IC1 = IC1 - 32
		    ALN(IOS:IOS) = CHAR ( IC1 )
		    ICAP = .FALSE.
		END IF
		IF ( I2WRD .LT. 4 ) THEN
		    ICAP = .TRUE.
		    IO = IO + 1
		    ALN(IO:IO) = ' '
		END IF
  773	    CONTINUE
	    WRITE ( * , 300 ) ALN(1:IO)
	    NLINE = NLINE + 1
	    IF ( NLINE .GT. 23 ) THEN
		WRITE ( * , 101 )
		READ ( * , 300 )
		NLINE = 0
	    END IF
  700	CONTINUE
	GO TO 100
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
	IF ( MAXW .NE. MAXWRD ) STOP 3000
	IAD1 = IAD + 2
	CALL I1GWRD ( IWL , MAXW , IAD1 )
	IAD1 = IAD1 + MAXW / 2 + MOD ( MAXW , 2 )
	ICJK = 0
	DO 3000 I = 1 , MAXW
	    ICJK = ICJK + 1
	    IF ( IWL(I) .EQ. 0 ) GO TO 3000
	    CALL I4GWRD ( IWS(I) , IAD1 )
C	     IF ( ICJK .LT. 10 ) THEN
C		 PRINT * , ' LENGTH OF WORD' , I , ' IS' , IWL(I) ,
C     + 	 ' STARTING ADDRESS = ' , IWS(I)
C	     END IF
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
C	 IF ( I .LT. 3 ) PRINT * , WORDS(I)(1:78)
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
c	     IS1A = MOD ( IS1 , NLENW )
c	     ID1 = IS1 / NLENW + 1
c	     IF ( IS1A .EQ. 0 ) THEN
c		 ID1 = ID1 - 1
c		 IS1A = NLENW
c	     END IF
c	     IS2 = IS1A + IWL(I) - 1
c	    PRINT * , ' WORD' , I , ' = ' , WORDS(ID1)(IS1A:IS2)
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
	I2HASH = 0
	LASTC = 1
C	 PRINT * , ' AWORD,LAWORD=',AWORD,LAWORD
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
c	     WRITE ( * , 1 ) AWORD(1:LAWORD) , IHASH
c    1	     FORMAT ( ' AWORD,IHASH = ' , A , I10 )
c	     WRITE ( * , * ) ' IWL(IHASH),LAWORD = ' ,IWL(IHASH),LAWORD
	    RETURN
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
	SUBROUTINE DMPTXT
	PARAMETER ( MAXWRD = 25 000 )
	PARAMETER ( NDIMW  =	190 )
	PARAMETER ( NLENW  =   1024 )
C       PARAMETER ( MAXVRS =    250 )
        parameter ( maxlin =     80 )
c
	CHARACTER*81 ALN
	character*(nlenw)  words(ndimw)
	INTEGER*2 I2WRD , IBSHI , IBSLOW
c
        dimension iws(maxwrd) , iwl(maxwrd)
c
	integer*2 itvers , ivers , ichap
	integer*1 iwl , lwl , nwl
C
	LOGICAL ICAP
c
        common /iws/ iws
        common /iwl/ iwl
        common /words/ words
	COMMON /BUF/ I2IX(256) , LASTAD , IRECIN , IRMOD , I2ZERO
	INTEGER*2 I2ZERO
	INTEGER*2 I2IX
	COMMON /VERTOC/ IADVER(500) , LASTVR , IBASVR
	INTEGER*2 IADVER , LASTVR
	COMMON /BOKTOC/ IADBOK(67)
	COMMON /CHPTOC/ IADCHP(1200) , LASTCH
C
	OPEN ( UNIT = 3 , FILE = 'BIBLE3.TXT' , STATUS = 'UNKNOWN' ,
     +	       BLOCKSIZE = 15 000 )
	DO 777 IB = 1 , 66
	    WRITE ( 3 , 678 ) IB
  678	    FORMAT ( ' BOOK' , I2.2 )
	    ICS = IADBOK(IB)
	    ICE = IADBOK(IB+1) - 1
	    ICHAP = 0
	    DO 776 J = ICS , ICE
		ICHAP = ICHAP + 1
		IF ( IO .NE. 0 ) WRITE ( 3 , 770 ) ALN(1:IO)
  770		FORMAT ( A )
		IO = 0
		WRITE ( 3 , 771 ) ICHAP
  771		FORMAT ( /// ' CHAPTER' , I4 / )
		ICP = IADCHP(J)
		IF(ICHAP.EQ.-38)PRINT*,'ICP=',ICP
		CALL GETWRD ( LASTVR , ICP )
		IF(ICHAP.EQ.-38)PRINT*,'LASTVR=',LASTVR
		ICP = ICP + 1
		CALL GETWRD ( IBSHI , ICP )
		IF(ICHAP.EQ.-38)PRINT*,'IBSHI=',IBSHI
		ICP = ICP + 1
		CALL GETWRD ( IBSLOW , ICP )
		IF(ICHAP.EQ.-38)PRINT*,'IBSLOW=',IBSLOW
		IBASVR = IBSHI * 32 767 + IBSLOW
		IF(ICHAP.EQ.-38)PRINT*,'IBASVR=',IBASVR
		DO 775 K = 1 , LASTVR
		    ICP = ICP + 1
		    CALL GETWRD ( IADVER(K) , ICP )
  775		CONTINUE
		DO 774 K = 1 , LASTVR - 1
		    IF(ICHAP.EQ.-38)PRINT*,'IADVER(K)=',IADVER(K)
		    IVS = IADVER(K) + IBASVR
		    IVE = IADVER(K+1) + IBASVR - 1
		    IF(ICHAP.EQ.-38)PRINT*,'IVS,IVE=',IVS,IVE
		    IF ( IO .NE. 0 ) WRITE ( 3 , 770 ) ALN(1:IO)
		    CALL NCODE ( K , ALN , IO )
		    DO 773 I = IVS , IVE
			CALL GETWRD ( I2WRD , I )
			IF(ICHAP.EQ.-38)PRINT*,'I2WRD=',I2WRD
			IF ( I .EQ. IVS .AND. I2WRD .EQ. 0 ) THEN
			    ICAP = .TRUE.
			    GO TO 773
			END IF
			IF ( IWL(I2WRD) .LE. 0 .OR. IWL(I2WRD) .GT.
     +							   127 ) THEN
			    PRINT * , ' **** ERROR **** I,IVS,I2WRD,IWS='
     +				    , I , IVS , I2WRD , IWS(I2WRD)
			    GO TO 773
			END IF
			IS1 = IWS(I2WRD)
			IS1A = MOD ( IS1 , NLENW )
			ID1 = IS1 / NLENW + 1
			IF ( IS1A .EQ. 0 ) THEN
			    ID1 = ID1 - 1
			    IS1A = NLENW
			END IF
			IS2 = IS1A + IWL(I2WRD) - 1
			IF(ICHAP.EQ.-38)PRINT*,'IS1,IS1A,ID1,IS2=',
     +					       IS1,IS1A,ID1,IS2
			IF(ICHAP.EQ.-38)PRINT*,'WORD=',WORDS(ID1)(IS1A:IS2)
			IF ( IO + IWL(I2WRD) + 1 .GT. 79 .AND. I2WRD
     +			     .GT. 7 ) THEN
			    WRITE ( 3 , 770 ) ALN(1:IO)
			    IO = 0
			END IF
			IF ( I2WRD .GT. 7 .AND. IO .GT. 0 ) THEN
			    IO = IO + 1
			    ALN(IO:IO) = ' '
			END IF
			IOS = IO + 1
			IO = IO + IWL(I2WRD)
			ALN(IOS:IO) = WORDS(ID1)(IS1A:IS2)
			IF ( ICAP ) THEN
			    IC1 = ICHAR ( WORDS(ID1)(IS1A:IS1A) )
			    IF ( IC1 .GT. 96 ) IC1 = IC1 - 32
			    ALN(IOS:IOS) = CHAR ( IC1 )
			    ICAP = .FALSE.
			END IF
			IF ( I2WRD .LT. 4 ) THEN
			    ICAP = .TRUE.
			    IO = IO + 1
			    ALN(IO:IO) = ' '
			END IF
  773		    CONTINUE
  774		CONTINUE
  776	    CONTINUE
  777	CONTINUE
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
