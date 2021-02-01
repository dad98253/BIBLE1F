PROJ = bible2
PROJFILE = bible2.mak
DEBUG = 0

PWBRMAKE  = pwbrmake
NMAKEBSC1  = set
NMAKEBSC2  = nmake
CC  = cl
CFLAGS_G  = /W2 /BATCH
CFLAGS_D  = /Gi$(PROJ).mdt /Zi /Od
CFLAGS_R  = /Ot /Oi /Ol /Oe /Og /Gs
MAPFILE_D  = NUL
MAPFILE_R  = NUL
LFLAGS_G  = /NOI /BATCH
LFLAGS_D  = /CO /FAR /PACKC
LFLAGS_R  = /EXE /FAR /PACKC
LINKER	= link
ILINK  = ilink
LRF  = echo > NUL
FORFLAGS_G  = /AH /MW1 /G2
LLIBS_R  =  /NOD:LLIBFOR7 LLIBF7R
LLIBS_D  =  /NOD:LLIBFOR7 LLIBF7R
FORFLAGS_R  = /Ot

OBJS  = BIBLE2.obj
SBRS  = BIBLE2.sbr

all: $(PROJ).exe

.SUFFIXES:
.SUFFIXES: .obj .sbr .for

BIBLE2.obj : BIBLE2.FOR

BIBLE2.sbr : BIBLE2.FOR


$(PROJ).bsc : $(SBRS)
	$(PWBRMAKE) @<<
$(BRFLAGS) $(SBRS)
<<

$(PROJ).exe : $(OBJS)
!IF $(DEBUG)
	$(LRF) @<<$(PROJ).lrf
$(RT_OBJS: = +^
) $(OBJS: = +^
)
$@
$(MAPFILE_D)
$(LLIBS_G: = +^
) +
$(LLIBS_D: = +^
) +
$(LIBS: = +^
)
$(DEF_FILE) $(LFLAGS_G) $(LFLAGS_D);
<<
!ELSE
	$(LRF) @<<$(PROJ).lrf
$(RT_OBJS: = +^
) $(OBJS: = +^
)
$@
$(MAPFILE_R)
$(LLIBS_G: = +^
) +
$(LLIBS_R: = +^
) +
$(LIBS: = +^
)
$(DEF_FILE) $(LFLAGS_G) $(LFLAGS_R);
<<
!ENDIF
	$(LINKER) @$(PROJ).lrf


.for.obj :
!IF $(DEBUG)
	$(FOR) /c $(FORFLAGS_G) $(FORFLAGS_D) /Fo$@ $<
!ELSE
	$(FOR) /c $(FORFLAGS_G) $(FORFLAGS_R) /Fo$@ $<
!ENDIF

.for.sbr :
!IF $(DEBUG)
	$(FOR) /Zs $(FORFLAGS_G) $(FORFLAGS_D) /FR$@ $<
!ELSE
	$(FOR) /Zs $(FORFLAGS_G) $(FORFLAGS_R) /FR$@ $<
!ENDIF


run: $(PROJ).exe
	$(PROJ).exe $(RUNFLAGS)

debug: $(PROJ).exe
	CV $(CVFLAGS) $(PROJ).exe $(RUNFLAGS)
