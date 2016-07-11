	subroutine desorb(ianz,zp,ap,ep,loste)

C **  CALCULATES ENERGY LOSS IN AN ABSORBER SANDWICH *******
C ****  ENERGY DEPOSIT IN SECTIONS OF IONIZATION  **********
C ***************  CHAMBER (DE1, DE2, AND DE3)  ************


	PARAMETER (DSEP0=4.0,isold0=1)

        COMMON ISG(19),INN(19),DEN(19),THK(19),PRS(19),XLN(19),ARDEN(19)
	1      ,ZNUMB(19,4),ANUMB(19,4),ELNUM(19,4),CONCN(19,4)
	1         ,PRDEN(19,4),PRTHK(19,4)
	DIMENSION ZNUMBW(4),ANUMBW(4),ELNUMW(4),CONCNW(4)
	1         ,PRDENW(4),PRTHKW(4)
	DIMENSION E(19),DE(19),xmem(19)
	DIMENSION TOUT(19),TOUTE(19)
	dimension eptable(50,500,2),emintabz(50),ianzv(5)
	real loste(19)
	DATA io1,IO2,IO3,io0/9,11,12,1/
	data iopt,ianzi,ianzide/1,2,1/
c
	save eltimo,izmax,izth

c       the mass table is to be used only for iopt = 5,6
c       use atomic masses to average for isotipic composition.
c       taken from Formulas Facts and Constants, H. J. Fischbeck and
c       K. H. Fischbeck. Springer - Verlag 1987 2nd ed, pages 164-183.

        dimension amass(70)
        data amass/1.01,4.00,6.94,9.01,10.81,12.01,14.01,16.00,19.00
     1,20.18,22.99,24.31,26.98,28.09,30.97,32.07,35.45,39.95
     2,39.10,40.08,44.96,47.88,50.94,52.00,54.94,55.85,58.93
     3,58.69,63.55,65.39,69.72,72.59,74.92,78.96,79.90,83.80
     4,85.47,87.62,88.91,91.22,92.91,95.94,98.,101.07,102.91
     5,106.42,107.87,112.41,114.82,118.71,121.75,127.60,126.90
     6,131.29,132.91,137.33,138.91,140.12,140.91,144.24,147.
     7,150.36,151.96,157.25,158.93,162.5,164.93,167.26,168.93
     8,173.04/

c       for Z > 70, you are in trouble !!
c
c      open(unit=IO1, status='OLD', file='absorb.inp')
      open(unit=IO2, status='OLD', file='desorb.out')
c      rewind IO1
c      rewind IO2
c
10	CONTINUE


C      IOPT = 1 - SUPPLY ENERGY OF PARTICLE ENTERING
C                 THE ABSORBER ARRAY AND GET LOSS AND
C                 RANGES
C      IOPT = 2 - SUPPLY TARGET, PROECTILE AND EJECTILE
C                 INFO. AND THEN GO THROUGH ABSORBER
C                 SANDWICH
C      IOPT = 3 - CALCULATE ENERGY DEPOSITS IN DETECTOR
C                 DETECTOR DIMENSIONS ARE STANDARD AND
C                 THE VARIABLE -'IDET' - CHOOSES BETWEEN
C                 VARIETY OF AVAILABLE DETECTORS
C      IOPT = 4 - FINDS MAXIMUM ENERGY THAT CAN BE STOPPED IN
C                 IANZ ELEMENTS OF THE SANDWICH FOR GIVEN
C                 ZP, AP.
C                 WHEN CALCULATION IS FINISHED, THE PROGRAM READS
C                 IN NEW VALUES OF ZP, AP AND RESTARTS. TO END
C                 THE PROGRAM, GIVE ZP < 0.
C                 IN ORDER TO HELP THE SPEED OF THE PROGRAM,
C                 GIVE THE PARTICLE'S "Z" IN increasing  ORDER.
C      IOPT = 5 - STORES ARRAYS OF Edet AS A FUNCTION OF INCIDENT    
C                 ENERGY AND THE PARTICLE'S ID (Z,A)
C                 ARRAY LABELED  eptable(Z-Zth,Einc,ipunch)
C                 ipunch = 1 stopped,  = 2 punched through
c                 Einc = E(incident)/detable
C                 Zth  = lowest Z considered - 1
C
C ************************************************************

c	read(io1,*) iopt
c1	FORMAT(10I)
	DSEP = DSEP0
	isold = isold0
c
	IF(iopt.LT.0) GO TO 2000


C ***********************************************************
c
	if(iopt.eq.3) then
      open(unit=IO3, status='OLD', file='absorbv.out')
      rewind IO3
	endif
c
	if(iopt.ge.5) then
		do itpun = 1,2
		do itblz = 1,50
		emintabz(itblz) = 0.
		do itble = 1,500
		eptable ( itblz,itble,itpun ) = 0.
		enddo
		enddo
		enddo
c
		open(unit=io0, status='UNKNOWN', file='abs.tbl')
		rewind io0
c
	else
	endif

C ***********************************************************

c      ianz  = number of elements in absorber "sandwich" - the 
c              particle deposits all its energy in these layers.
c      ianzi = index of last layer in which the energy of the 
c              particle is not recorded - this unreecorded energy
c              is used in the DST production in two modes:
c          when making DST tape from data:
c              Since the detector records only deposited energy
c              the output tables are used to correct for this 
c              deficinecy and for a given dharge and mass extrapolate
c              the measured energy to the target exit energy
c          when making DST tape from model calculations:
c              The "lost" energy is a source of broadening of the 
c              energy spectra due to straggling - this "smearing" 
c              is estimated and superposed on the calculated spectra.
c	ianzide = element # for DE calculation
c
c	read(IO1,*) ianz,ianzi,ianzide
c
c	ianzv(5)
c	     = Index of layer where exiting particle velocit is 
c	       calculated (only for option 3) max = 5
c
c	if(iopt.eq.3) read (IO1,*) knz
c	if(iopt.eq.3) read (IO1,*) (ianzv(k),k=1,knz)
C ***********************************************************

C      AP = PROJECTILE MASS
C      ZP = PROJECTILE CHARGE
C      EP = PROJECTILE ENERGY
c
c	read(io1,*) zp,ap,ep
2	FORMAT(f10.3)

C *************************************************************
c
c      zth= threshold Z for incident energy tble calc. (iopt=5,6)
c      zmax = maximum z for table calculation
c      detable = the energy step size used for array storage   
c      emin = starting incident energy for table
c      emax = mazximum incident energy for table calculation
c	EP is ignored when iopt = 5 or 6
c
	if(iopt.ge.5) then
		read(IO1,*) zth,zmax,emintab,emaxtab,detable
		eltimeo = secnds(0.0)  !  start timing
		izth = ifix (zth + 0.01)
		izmax = ifix (zmax + 0.01)
c
c	if detable > emintab change emintab to 1/2 * detable
c
		if(emintab.lt.detable) emintab = 0.5*detable
		write(io2,291) zth+1,zmax,emintab,emaxtab,detable
	else
	endif
c
C ************************************************************

C      IN THE FOLLOWING THE LISTED VARIABLES ARE INDEXED
C      THE INDICES I AND J STAND FOR THE FOLLOWING:
C      - I -  SERIAL NUMBER OF ABSORBER LAYER (<20)
C      - J -  SERIAL NUMBER OF ELEMENT WITHIN LAYER (<5)

C *************************************************************


C       ISG(I) = 0  - FOR SOLID ABSORBERS
C          (I) = 1  - FOR GASEOUS ABSORBERS
C       INN(I) = NUMBER OF ELEMENTS IN ONE LAYER
C             (E.G. CH4 HAS TWO ELEMENTS C AND H)
C       DEN(I) = DENSITY OF ABSORBE (FOR SOLIDS)
C       THK(I) = THICKNESS OF ABSORBER IN MG/CMSQ (FOR SOLIDS)
C       PRS(I) = PRESSURE (IN MM HG) FOR GAS ABSORBER
C       XLN(I) = PHYSICAL LENGTH OF ABSORBER ( IN CM ) FOR GAS

C  **** ABSORBER COMPOSITION ****

C      ELNUM(I,J) = NUMBER OF ATOMS OF ELEMENT J IN LAYER I
C      CONCN(I,J) = CONCENTRATION (MOLAR) OF ELEMENT J IN LAYER I
C      ANUMB(I,J) = MASS   NUMBER   OF ELEMENT J IN LAYER I
C      ZNUMB(I,J) = ATOMIC NUMBER   OF ELEMENT J IN LAYER I

C      PRDEN(I,J) = PARTIAL DENSITY OF ELEMENT J IN LAYER I
C      PRTHK(I,J) = PARTIAL THICKNESS OF ELEMENT J IN LAYER I (MG/CMSQ)

C      MAXIMUM OF NINETEEN LAYERS SPECIFIED


C *************************************************************

	DO 100 ISN=1,IANZ
c		read(io1,*) ISG(ISN),INN(ISN)
		IF(ISG(ISN).EQ.1) GO TO 50
c		read(io1,*) DEN(ISN),THK(ISN)
		TOUT(ISN)=THK(ISN)/(DEN(ISN)*1000.)
		TOUTE(ISN)=TOUT(ISN)/2.54
		DO 20 IMN=1,INN(ISN)
c			read(io1,*) ANUMB(ISN,IMN),ZNUMB(ISN,IMN),
c	1		ELNUM(ISN,IMN),CONCN(ISN,IMN)
20		CONTINUE
		GO TO 100
50              continue
c		read(io1,*) PRS(ISN),XLN(ISN)
		DO 60 IMN=1,INN(ISN)
c			read(io1,2) ANUMB(ISN,IMN),ZNUMB(ISN,IMN),
c	1		ELNUM(ISN,IMN),CONCN(ISN,IMN)
60		CONTINUE
100	CONTINUE

C ****************************************************************

	if(iopt.ne.5.and.iopt.ne.6) WRITE(IO2,101) ap,zp,ep
	if(iopt.ne.5.and.iopt.ne.6) WRITE(IO2,102) ianz

C *****************************************************************

	DO 200 I=1,ianz
		INNW=INN(I)
		DO 210 J=1,INNW
			ANUMBW(J)=ANUMB(I,J)
			ZNUMBW(J)=ZNUMB(I,J)
			ELNUMW(J)=ELNUM(I,J)
			CONCNW(J)=CONCN(I,J)
210		CONTINUE
		DENW=DEN(I)
		XLNW=XLN(I)
		PRSW=PRS(I)
		THKW=THK(I)
		IF(ISG(I).EQ.1) GO TO 250
		CALL SETABS(INNW,ANUMBW,ZNUMBW,ELNUMW,PRTHKW,THKW,PRDENW,DENW)
		DO 230 J=1,INNW
			PRDEN(I,J)=PRDENW(J)
			PRTHK(I,J)=PRTHKW(J)
230		CONTINUE
		GO TO 200

250		CALL SETABG(INNW,ANUMBW,ZNUMBW,ELNUMW,CONCNW,PRTHKW,THKW
	1,	PRDENW,DENW,PRSW,XLNW)
		DEN(I)=0.
		THK(I)=0.
		DO 270 J=1,INNW
			PRDEN(I,J)=PRDENW(J)
			PRTHK(I,J)=PRTHKW(J)
			DEN(I)=DEN(I)+PRDEN(I,J)
			THK(I)=THK(I)+PRTHK(I,J)
270		CONTINUE
200	CONTINUE

C *************************************************************

C      START CALCULATION AND DETAILED PRINTOUT

C *************************************************************
c
	if(iopt.ge.5) then
	ep = emintab
	zp = zth + 1.
	indexz = ifix (zp - zth + 0.001)
	endif
c
299	continue   !  come here for new particle (zp change)

	izp = ifix (zp+0.001)
c
	if(iopt.ge.5) then
		if (izp.gt.70) then
			write (6,*) 'no mass for Z = ',izp
			stop
		else
			ap = amass(izp)

c       The trick!. To calculate energy losses for deuterons and tritons,
c       enter zth = 0.2 and 0.3 respectively.    E. Chavez jul/92

			if (izp.eq.1) then
				iap = ifix (zth*10.0 + 0.1)
				ap = float (iap)
			end if
		end if
	end if
c
	if (iopt.eq.6) then
		ideltalay = 8                 ! choose DE3 for eloss signal
		if(izp.eq.1) ideltalay = 16   ! choose DEh for eloss signal
	endif
c
300	CONTINUE    ! come here for new energy (ep)
c
	EI=ep
	XUPDN=-1.
	EPS=0.0001
	I1STPASS = 1

	IF (iopt.EQ.4) GO TO 600

	ipunch = 2
	DO 510 I=1,IANZ    ! begin loop over absorber layers
c
	if(iopt.ge.5) go to 504
	IF(ISG(I).EQ.0) WRITE(IO2,311) I,THK(I),TOUT(I),TOUTE(I),DEN(I)
	IF(ISG(I).EQ.1) WRITE(IO2,312) I,THK(I),PRS(I),XLN(I),DEN(I)
	DO 320 J=1,INN(I)
	WRITE(IO2,321) ANUMB(I,J),ZNUMB(I,J),PRTHK(I,J)
320	CONTINUE
504	continue
c
c       XNS - initial no. of intervals for integration of DE
		XNS = 2.
c	EI = energy in   
		CALL ADS(I,XUPDN,XNS,EPS,ap,zp,EI,DEI,ISTAT)
c       DEI = energy out - energy in ( < 0. for energy loss)
		EIOLD=EI
c       E(I) = energy left after I'th element (EP-DE(1)-DE(2)+...)
c       if particle stopped in detector this is equal to energy lost
c       in remaining layers
		DE(I) = DEI
		E(I)  = EI + DEI
		EI    = E(I)
		INS=IFIX(XNS+0.001)
c
	if (iopt.ge.5) go to 505
	WRITE(IO2,401) INS,EIOLD,EI
	loste(i)=-1.*de(i)
	if (EI.LT.EPS.OR.ISTAT.EQ.-1) WRITE(IO2,402) I
505	continue
        loste(ianz + 1)=e(ianz)
c
c	if particle stopped in layer beyond ianzi we must 
c	check if iopt=5 or6 and calculate the energy loss in the
c	front part (layers 1 thru ianzi).
c
	istore = I
	if (EI.lt.EPS) ipunch=1
c	control loop exit
c	exit when particle runs out of energy in last layer
	if (EI.LT.EPS.OR.ISTAT.EQ.-1) go to 701
c	if interested in DE signal only (iopt=6) exit after 
c	layer for which DE is sought was traversed
	if(iopt.eq.6.and.I.ge.ianzide) go to 701
c
510	CONTINUE     ! end loop over absorber layers

c	this part for iopt=5,6 stores incident energy values

701	continue
	if(iopt.ne.5.and.iopt.ne.6) go to 520
c
c  establish higher energy cutoof for next step with higher z
c  should save time in calulating energies of particles stopped
c  in the dead layer
	if( I.le.ianzi) emintabz(izp)=ep
c
c  evalue = energy of particle when entering sensitive volume of det.
c           when particle is stopped (ipunch=1) this is what is left
c           once you take off the energy lost in the dead layer.
	evalue = E(ianzi) 
c         = when particle punches thouough detector its energy at the
c           end is EI - subtract this from what it entered with (after
c           dead layers) and again you got the energy deposited.
	if(ipunch.eq.2) evalue = E(ianzi) - EI 
c      roundoff errors could resutl in negative energies !!
	if (evalue.lt.0.0) evalue = 0.0

c  EI is current energy after last layer - is nonzero when particle
c  punched through and to get energy deposited must subtract this 
c  "left over" energy from the energy of particle had when it entered
c  the detector's sensitive volume
	indexe = ifix((ep + 0.001)/detable) + 1
        indexz = ifix(zp - zth + 0.001)
	if(iopt.eq.5) eptable(indexz,indexe,ipunch) = evalue
	if(iopt.eq.6) ipunch = 1
	if(iopt.eq.6) eptable(indexz,indexe,ipunch) = -DE(ianzide)

c	now repeat calculation for same Z but new energy
	ep = ep + detable 
	if(ep.gt.emaxtab) go to 709
	go to 300

709	continue
c	reset energy to emintab and up zp by one until we top
c	zmax - this portion controls looping over Z!

	do ieps = 1,ianz
		E(ieps) = 0.
		DE(ieps) = 0.
	enddo

	ep = emintabz(izp)
	zp = zp + 1.
	izp = ifix(zp + 0.001)
	emintabz(izp) = emintabz(izp-1)

	eltime = secnds (eltimeo)
	itminutes = ifix(eltime/60.)
	tminutes = float(itminutes)
	tseconds = eltime - tminutes*60.
	eltimeo = eltimeo + eltime
	zpm1 = zp - 1.
	if((izp-1).le.izmax) write(io2,714) zpm1,ap,tminutes,tseconds
  714 format(' finished Z=',f3.0,'   A=',f4.0,' -  ',f5.0,
     +' minutes and ',f3.0,' seconds elapsed')

	if (izp.ge.izmax) go to 711
	go to 299

711	continue

c	get here when iopt = 5 or 6 calculation is done
c	now ready to store array on disk
	write(io0,712) zth,zmax,emintab,emaxtab,detable

	iemintab = ifix( (emintab + 0.001)/detable ) + 1
	iemaxtab = ifix( (emaxtab + 0.001)/detable ) 
	iztop = ifix(zp - 1. - zth + 0.001)

	do ipp = 1,2  
	do indexz = 1, iztop
	iztab = indexz + ifix(zth+0.01)
	imasstab = ifix(amass(iztab) + 0.1)
	if(iztop.eq.1) imasstab = ifix(ap + 0.1)
	write(io0,7712) indexz,iztab,imasstab,iemintab,iemaxtab
7712	format(5i20)
	itblow = iemintab 
	do indexe = iemintab,iemaxtab,10
	itbup = itblow + 9
	write(io0,713) (eptable(indexz,itbe,ipp),itbe=itblow,itbup)
	itblow = itbup + 1
	enddo
	enddo
	enddo
712	format(5e16.8)
713	format(10e16.8)
c
	close (unit = io0)
c
C      MORE INPUT FOR NEW CALCULATION WITH SAME ABSORBERS

520	CONTINUE


c	read(io1,*) zp,ap,ep
        zp = -1.
	IF(zp.le.0.) GO TO 2000
	izp = ifix(zp + 0.001)
	if(ap.le.0.) AP = amass(izp)
	IF(zp.gt.0.) WRITE(IO2,101) ap,zp,ep
	GO TO 299

600	DO I = 1, ianz
		ILAY = I
		XNS = 2.0
		CALL ADS(I,XUPDN,XNS,EPS,ap,zp,EI,DEI,ISTAT)
		EIOLD = EI
		DE(I) = DEI
		E(I) = EI + DEI
c       E(I) = energy left after I'th element (EP+DE(1)+DE(2)+...)
c       if particle stopped in detector this is equal to energy lost
c       in remaining layers
		xmem(i) = E(I)
		EI = E(I)
		INS = IFIX(XNS + 0.001)
c		WRITE (IO2,613) ILAY,INS,EI,ISTAT
		IF (EI.LE.0.0.OR.ISTAT.EQ.-1) GO TO 601
	END DO


601	IF (ISTAT.EQ.0)THEN
		IF (EI.LT.0.003.AND.EI.GE.0.0) THEN
	WRITE(IO2,611) ap,zp,ep,ILAY,xmem(5),xmem(6),xmem(7)
		read(io1,*) zp,ap
		izp = ifix (zp + 0.001)
		if(ap.le.0.) ap = amass(izp)
			IF (zp.LT.0.0) GO TO 2000
			IPASS = 0
			I1STPASS = 1
			EI = ep
			GO TO 600
		END IF
		isign = isold
		isold = 1
	ELSE
		isign = - isold
		isold = -1
	END IF

	IF (I1STPASS.gt.0) THEN
		isign = 1
		I1STPASS = 0
		IF (ISTAT.EQ.0) THEN
			DSEP =  - DSEP0
		ELSE
			DSEP = DSEP0
		END IF
	END IF

C	IF THE INITIAL ENERGY WAS TOO LARGE, THEN THE ION WILL PUNCH THROUGH
C	THE DETECTOR A NUMBER OF TIMES UNTIL THE ENERGY IS REDUCED BELOW 
C	THE PUNCH-THROUGH ENERGY (PTE). IF THE INITIAL ENERGY WAS TOO SHORT
C	THEN IT WON'T UNTIL PTE IS REACHED. IN THIS MOMENT, IPASS IS SET TO
C	ONE, AND FROM THIS POINT EVERY FURTHER CALCULATION WILL IMPLY A
C	REDUCTION BY HALF OF THE SIZE OF "DSEP", AND A CHANGE OF SIGN ONLY
C	IF APROPRIATE.

	IF (isign.LT.0) IPASS = 1		!IPASS=1 UNTIL "PTE" IS FOUND.
	IF (IPASS.EQ.1) DSEP = isign * DSEP * 0.5

	IF (ABS(DSEP).LT.0.05) THEN
		EI = 0.00001
		ISTAT = 0
		GO TO 601
	END IF

	EP = EP + DSEP
	ei = ep

	GO TO 600

1000	CONTINUE
	GO TO 10
2000	CONTINUE

101	FORMAT(//////' PASSAGE OF CHARGED PARTICLE THROUGH ABSORBER',
	1       ' SANDWICH '////'   AP = ',F6.0,'   ZP = ',F5.0,
	1       '    INITIAL ENERGY = ',F12.5//)
102	FORMAT('   ABSORBER SANDWICH CONTAINS - ',I2,' LAYERS'//)
291	format(' start absorber calculations for   z =',f3.0
     +,'  to z =',f3.0,/' and energies from emin =',f6.2
     +,'  to emax =',f7.2,'  in ',f6.2,'MeV steps')
311	FORMAT(//'  LAYER # ',I2,'    - SOLID ABSORBER -   ',
	1' AREAL DENSITY = ',E10.4/' THICKNESS = ',E10.4,
	1' CM   OR ',E10.4,' INCH      DENSITY =',E10.3,' G/CM3')
312	FORMAT(//'   LAYER # ',I2,'    -  GAS  ABSORBER -   ',
	1'  AREAL DENSITY = ',E10.4/'  PRESSURE = ',E10.4,
	1' TORR    LENGTH ',E9.4,'CM    DENSITY =',E9.3,'MG/CM3')
321	FORMAT(7X,' A =',F6.0,'  Z =',F5.0,'   AREAL DENSITY'
	1,' (PARTIAL) = ',E12.5,' MG/CMSQ')
401	FORMAT(' CALC IN-',I4,' STEPS'
	1,'   ENERGY IN = ',F8.3,'    ENERGY OUT = ',F8.3
	2,'(MEV)')
402	FORMAT(' CHARGED PARTICLE STOPPED IN LAYER # ',I2)
613	FORMAT (2X,'LAYER= ',I2,': ',I6,' ITERATIONS'
	1,   ', E final= ',F10.4,' STATUS= ',I2)
611	FORMAT (2X,'Ion  (A , Z): (',F4.0,' , ',F3.0
	1,'), E(MeV)= ',F7.2,'  STOPPED IN LAYER ',I2/ 
	1'   Esum = ',f7.2,'    Esum-E1 = ',f7.2,
	1'   Esum-E1-E2 = ',f7.2)
703	format('  eptable (',i2,', ',i3,', ',i1,' ) =',f8.2)

	return
	END

	SUBROUTINE ADS(I1,SIGN,XN1,EPS,A,Z,E,DEE,ISTAT)

C	SUBROUTINE FOR ENERGY LOSS CALCULATIONS
C	CALL DEDX FRO STOPPING POWER CALCULATIONS

	COMMON ISG(19),INN(19)
	1      ,DEN(19),THK(19),PRS(19),XLN(19),ARDEN(19)
	1      ,ZNUMB(19,4),ANUMB(19,4),ELNUM(19,4),CONCN(19,4)
	1         ,PRDEN(19,4),PRTHK(19,4)

C	N1= NUMBER OF SUBDIVISIONS FOR INTEGRATION
C		OF ENERGY LOSS

1000	CONTINUE
	EH = E
	N1 = IFIX(XN1+0.001)
	DEDNEXT = 0.
	DO 1010 K=1,N1
		J1 = INN(I1)
		ISGW = ISG(I1)
		I = I1
		DO 1001 J = 1,J1
			AX = ANUMB(I,J)
			ZX = ZNUMB(I,J)
			FX = PRTHK(I,J)/XN1
			DENST = PRDEN(I,J)
			VH = VEL(EH,A)
			CALL DEDX(Z,A,ZX,AX,DENST,EH,VH,ISGW,DEX,DE)
			EH = EH + DE*SIGN*FX
			IF(EH.LE.0.) THEN
				IF (K.LE.2) THEN
					N1 = N1 * 2
					XN1 = FLOAT(N1)
					GO TO 1000
				END IF
				ISTAT = -1
				GO TO 9910
			END IF
			IF (K.LE.2) DEDNEXT = DEDNEXT + DE * FX
1001		CONTINUE
		IF (K.EQ.1) THEN
			DED1ST = DEDNEXT
			DEDNEXT = 0.0
		END IF
		IF (K.EQ.2) THEN
			DDD = DED1ST - DEDNEXT
			IF(DDD.LT.0.) DDD=-DDD
			DDS = DED1ST + DEDNEXT
			DDR = DDD/DDS
			IF(DDR.GT.EPS) THEN
				N1 = N1 * 2
				XN1 = FLOAT(N1)
				GO TO 1000
			END IF
		END IF
1010	CONTINUE

	ISTAT = 0
9910	DEE = EH-E

	RETURN
	END

	SUBROUTINE SETABS(INW,A,Z,AN,T,TH,D,DN)

C	SUBROUTINE FOR SETTING UP COMPOSITE ABSORBER
C	DATA (PARTIAL DENSITIES AND THICKNESSES)

	DIMENSION A(4),Z(4),AN(4),T(4),D(4)

	AW=0.
	DO 1 I=1,INW
	AW=AW+A(I)*AN(I)
1	CONTINUE
	DO 2 I=1,INW
	AN(I)=A(I)*AN(I)/AW
	T(I) = TH*AN(I)
	D(I) = DN*AN(I)
2	CONTINUE

	RETURN
	END

	SUBROUTINE SETABG(INW,A,Z,AN,CN,T,TH,D,DN,PR,XL)

C	SUBROUTINE FOR SETTING UP COMPOSITE ABSORBER DATA
C	FOR GASEOUS LAYERS.

	DIMENSION A(4),Z(4),AN(4),CN(4),T(4),D(4)
	

	P = PR/760.
	X = XL/22.4

	AWW=0.
	AW=0.
	DO 1 I=1,INW
	AW = AW +A(I)*AN(I)
	AWW= AWW+A(I)*AN(I)*CN(I)
	T(I) = P*X*A(I)*AN(I)*CN(I)
	D(I) = T(I)/XL
1	CONTINUE
	RETURN
	END

	FUNCTION VEL(ENER,A1)

	VV=SQRT(2.13E-3*ENER/A1)
	VEL=VV
	RETURN
	END

	FUNCTION FKINEM(EP,AP,AT,TH)

	IF(AP.GT.AT) GOTO 100
	E=EP*AP**2/(AP+AT)**2
	E=E*(COS(TH)+SQRT((AT/AP)**2-SIN(TH)**2))**2
	FKINEM=E
	RETURN
100	FKINEM=0.
	RETURN
	END

	FUNCTION FFKIN(EP,AP,AT,TH,Q)

C	INELASTIC SCATTERING
	B=AP**2*EP/(AP+AT)**2/(EP+Q)
	D=AT**2/(AP+AT)**2*(1.+AP*Q/AT/(EP+Q))
	E=(EP+Q)*B*(COS(TH)+SQRT(D/B-SIN(TH)**2))**2
	FFKIN=E
	RETURN
	END

	SUBROUTINE DEDX(Z1,A1,Z2,A2,RHO,ENER,V,IFG,DEDXHI,DEDXTO)

C PROGRAM CALCULATES THE DIFFERENTIAL ENERGY LOSS DE/DX IN SOLID
C TARGETS USING A SEMIEMPIRICAL FORMULA DEDUCED FROM EXPERIMENTAL
C THE PROGRAM IS MODIFIED FOR GAS ABSORBERS.

C REF.: K.BRAUNE,R.NOVOTNY,D.PELTE,D.HUSAR,D.SCHWALM,
C PROCEEDINGS - SPRING MEETING OF THE GERMAN PHYSICAL
C SOCIETY, VERHANDLUNGEN 4/1978

C K.BRAUNE, DIPLOM, HEIDELBERG 1979


C 	  H(Z2) IS A SUM OF FIVE GAUSSIAN FUNCTIONS.
C 	       A1        MASS NUMBER  - PROJECTILE
C 	       Z2        ATOMIC NUMBER ABSORBER
C 	       A1        MASS NUMBER   ABSORBER
C 	       RHO       DENSITY OF THE ABSORBER (GRAMM/CM**3)
C 	                 (MEANLESS IF GAS ABSORBER )
C 	       ENER      ENERGY OF THE PROJECTILE (MEV)
C 	       V         VELOCITY OF THE PROJECTILE
C 	                 IN MEV/(MG/CM**2)
C       	Z1       ATOMIC NUMBER - PROJECTILE

	IF(IFG.EQ.1) RHO=1.
	XI=V**2/Z2
C 	
C	ABSORBER - FUNCTION
C 	G(XI)=Y(EXP)-Y(THEORY) - IS DEDUCED FROM EXPERIMENTAL ENERGY LOSS
C 	MEASUREMENTS.
C 	
C 	IF THE SAME ABSORBER WAS USED BEFORE , GO TO STATEMENT # 55

	IF(A2.EQ.A2SAV.AND.Z2.EQ.Z2SAV) GOTO 55

	Z2SAV=Z2
	A2SAV=A2

C  FUNCTION  Y

	FY=54721.*(1.+5.15E-2*SQRT(A2/RHO)-EXP(-0.23*Z2))
	IF(IFG.NE.1) GOTO 10
	FY=54721.*(1.35-EXP(Z2*(-.13+.0014*Z2)))

C  G(XI) IS THE DERIVATION OF A GASSIAN WITH VARIABLE HEIGHT H(Z2).

10	IF(Z2.GT.26.) GOTO 20
	G1=19.84*EXP(-.17*(Z2-4.25)**2)
	GOTO 35
20	G1=0.000001
	IF(Z2.GT.38.) GOTO 40
35	G2=17.12*EXP(-.12*(Z2-11.63)**2)
	GOTO 50
40	G2=0.0000001
50	G3=7.95*EXP(-.015*(Z2-30.2)**2)
	G4=5.84*EXP(-.022*(Z2-48.63)**2)
	G5=7.27*EXP(-.005*(Z2-73.06)**2)
	HZ2=(9.-(G1+G2+G3+G4+G5))*1.32E-5
	ZWD=2./3.
	Z2ZWD=Z2**ZWD

C  MULTIPLICATIONFACTORS OF G(XI)

	FG=1.2E-4*Z2*Z2+2.49E-2*A2/RHO
	IF(IFG.NE.1) GOTO 52
	FG=1.3/(1.+EXP(3.-Z2/5.))
52	ALEFG=ALOG(2.7E-5/FG)

C  CALCULATION OF G(XI)

55	GXI=0.
	IF(XI.GE.1.E-9.AND.XI.LE.5.E-4) THEN

	SQXI=SQRT(XI)
	C=2./Z2*SQXI/(1.+1.E4*SQXI)
	IF(IFG.EQ.1) C=C/2.
	FG0=1./(1.+(XI*10000.)**3)
	AL=ALOG(XI)-ALEFG
	GXI=(C-HZ2*AL*EXP(-.32*AL*AL))*FG0
	ENDIF

C  CALCULATION OF Y(XI)
	Y=3.3E-4*ALOG(1.+XI*FY)+GXI
C  ENERGY LOSS OF HEAVY IONS
C  EFFECTIVE CHARGE
	VV0=V*137.
	FV=1.
	IF(V.LE..62) FV=1.-EXP(-VV0)
	AZ1=ALOG(1.035-.4*EXP(-.16*Z1))

	QQ=V/Z1**.509
	GHI=Z1
	VZ1=(-116.79-3350.4*QQ)*QQ
	IF(VZ1.GT.-85.2) GHI=Z1*(1.-EXP(VZ1))
	IF(Z1.GT.2.) GHI=Z1*(1.-EXP(FV*AZ1-0.879*VV0/Z1**0.65))

C  EFFECTIVE CHARGE FOR PROTONS AND ALPHA PARTICLES

C ********************   RESULTS  ********************

C  ELECTRONIC ENERGY LOSS DEDXHI

	DEDXHI=GHI*GHI*Z2*Y/(A2*V**2)

C  NUCLEAR ENERGY LOSS DEDXNU

	ZA=SQRT(Z1**(ZWD)+Z2ZWD)
	EPS=3.25E4*A2*ENER/(Z1*Z2*(A1+A2)*ZA)
	SIGMAN=1.7*SQRT(EPS)*ALOG(EPS+2.1718282)/
	1     (1.+6.8*EPS+3.4*EPS**1.5)
	DEDXNU=SIGMAN*5.105*Z1*Z2*A1/(ZA*A2*(A1+A2))

C  TOTAL ENERGY LOSS DEDXTO

	DEDXTO=DEDXHI+DEDXNU

	RETURN
	END
c
