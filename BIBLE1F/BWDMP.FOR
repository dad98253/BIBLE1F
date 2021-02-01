	PROGRAM BIBLE2
	PARAMETER ( MAXWRD = 30 000 )
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
c
	dimension lws(256) , lwl(256)
        dimension iws(maxwrd) , iwl(maxwrd)
c
	integer*2 ivers , ichap , i2zero
	integer*1 iwl , lwl , nwl
C
	LOGICAL NEWSENT
	LOGICAL GOODNA
c
	common /prsdat/ a , lws , lwl , nwl , ivers , ichap
	COMMON /BOOK/ BOOK
	common /cnts/	aword , laword , lastwd , lastsb
     +			, NHASH
	COMMON /ERR/ IER
        common /iws/ iws
        common /iwl/ iwl
        common /words/ words
	COMMON /FMTS/ FMT1 , FMT2
	COMMON /BUF/ I2IX(256) , LASTAD
	INTEGER*2 I2IX
	COMMON /VERTOC/ IADVER(500) , LASTVR , IBASVR
	INTEGER*2 IADVER , LASTVR
	COMMON /BOKTOC/ IADBOK(66)
	COMMON /CHPTOC/ IADCHP(1200) , LASTCH
	FMT1 = '( I1 )'
	FMT2 = '( I2 )'
	NEWSENT = .TRUE.
	NHASH = 0
	I2ZERO = 0
	LASTVR = 0
	LASTCH = 0
	LASTAD = 10
	MLAWRD = 0
	OPEN ( UNIT = 2 , FILE = 'BWORDS.DAT' , STATUS = 'OLD' ,
     +	       FORM = 'UNFORMATTED' , BLOCKSIZE = 30 000 )
	READ ( 2 ) aword , laword , lastwd , lastsb
     +		 , NHASH , iws , iwl , words
	CLOSE ( 2 )
	PRINT * , ' AWORD= ' , AWORD
	PRINT * , ' LAWORD = ' , LAWORD
	PRINT * , ' LASTWD = ' , LASTWD
	PRINT * , ' LASTSB = ' , LASTSB
	PRINT * , ' NHASH = ' , NHASH
	PRINT * , ' IWS = '
	PRINT 1 , IWS
    1	FORMAT ( 12I6 )
	PRINT * , ' IWL = '
	PRINT 2 , IWL
    2	FORMAT ( 20I4 )
	END
