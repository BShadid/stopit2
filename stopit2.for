      PROGRAM stopit
      
C 
C PROGRAM DESCRIPTION: 
C 
C	Program calculates energy loss in absorber sandwich.  Uses energy 
C	routine "absorb.for" obtained from Dan Shapira who obtained it from
C	somebody in Germany.  I don't know - it works!
C
C
C	The the program can be compiled using the command
C	g77 stopit.for desorb.for -o stopit
C		or, for more recent compilers
C	gfortran stopit.for desorb.for -o stopit

c
c	
C 
C AUTHORS: 
C 
C	 Dan Bardayan
C	 Jonathan Wheeler
C	Patrick O'Malleyst
C 
C CREATION DATE: 	12/23/97
C 
C 
C	     C H A N G E   L O G
C 
C	 Date     | Name             | Description
C ----------------+------------------+-----------------------------------------
C  12/23/97       |  Dan Bardayan    | Creation of the program
C  10/03/03       |  Dan Bardayan    | Modified to compile w/ g77
C  05/27/11	  |  Jonathan Wheeler| Modified to include standardized media
C		  | 		     |  and to find the thickness required to
C		  |		     |  stop a "stopee"
C ----------------+------------------+-----------------------------------------
C ----------------+------------------+-----------------------------------------
C 
	IMPLICIT NONE
	
	INTEGER i, j, k,imm, ism, iedit, issm
	integer ianz, ianzi, ianzide
	integer isg,inn
	integer stdm, siupb, soupb, gaupb

c	stdm: index for standard media

c	imm: which main menu item we are on
c	ism: which sub menu item we are on
c	iedit: which layer to edit	
c	ianz : # of energy loss layers
c	ianzi : index of last layer in which the energy  of the pariticle
c		is not recorded - don't blame me for this vague description
c		i got it straight from desorb.for- i always set it to 2
c	ianzide : element # for DE calculation- i always set it to 1
c	isg(i): 0,1-0 means ith layer is solid,1 means ith layer is gas
c	inn(i): no. of elements in ith layer 

c   sithk: thickness in microns of standard silicon
c	tethk: test thickness for iteration purposes
c	error: error in iteration thickness for bisection method


	real den, thk,prs,xln,elnum,concn
	real anumb, znumb
	real arden, prden, prthk
	real z, a, energy, loste(19),zp, edep
	real sithk, tethk, error

c	z : proton number of stopee
c	a : mass number of stopee
c	energy: energy of stopee in MeV	
c	den(i) : density (g/cm3) of ith layer-only for solid
c	thk(i): thickness (mg/cm3) of ith layer-only for solid
c	prs(i): pressure (torr) of ith layer-only for gas
c	xln(i): thickness (cm) of ith layer-only for gas
c	elnum(i,j):# atoms of jth element in ith layer per molecule
c	concn(i,j): concentration(molar) j element in ith layer = 1 usually
c	anumb(i,j): atomic mass in amu for j element in ith layer
c	znumb(i,j): atomic no. for jth element in ith layer
c	loste(i):energy lost in ith layer
c	edep: energy deposited in absorber = sum(loste)

	COMMON ISG(19),INN(19)
     +      ,DEN(19),THK(19),PRS(19),XLN(19),ARDEN(19)
     +      ,ZNUMB(19,4),ANUMB(19,4),ELNUM(19,4),CONCN(19,4)
     +         ,PRDEN(19,4),PRTHK(19,4)


	open(unit=7,file='stopit.log',status='unknown')

c define some initial values in case the program is run before all the
c important parameters are defined

	ianz = 1
	ianzi = 2
	ianzide = 1
	isg(1) = 1
	inn(1) = 1
	stdm = 1
	sithk = 10.
	z = 1.
	a = 1.
	energy = 10.
	den(1) = 1.0
	thk(1) = 0.1
	prs(1) = 10.
	xln(1) = 10.
	elnum(1,1) = 1.
	concn(1,1) = 1.
	anumb(1,1) = 12.
	znumb(1,1) = 6.

c For finding thicknesses, upper bounds need to be set.
c Silicon upper bound in microns
	siupb = 1500
c Other solids' upper bound in mg/cm2
	soupb = 1000
c Gas upper bound in cm
	gaupb = 100

c This is the control portion of the program
	
10	write(6,15)
15    	format(/'  1 define stopee'/                                              
     *       '  2 define absorber'/                                                
     *       '  3 edit absorber'/                                     
     *       '  4 run with current parameters'/
     *       '  5 find thickness of absorber to stop the stopee'/                                         
     *       '  6 print status of data'/                             
     *       '  7 stop')
     	read(5,*)imm
     	
     	if(imm.eq.1)then
     		goto 100
     	elseif(imm.eq.2)then
     		goto 200
     	elseif(imm.eq.3)then
     		goto 300
     	elseif(imm.eq.4)then
     		goto 400
c This section was editted to add a new function, it's easier to
c have a discontinuity in labels than to relabel everything.
     	elseif(imm.eq.5)then
     		goto 600
		elseif(imm.eq.6)then
			goto 500
     	elseif(imm.eq.7)then
     		goto 1000
     	else
     		goto 10
     	endif
     	
c Define stopee     	
100	write(6,*)'Enter Z and A of stopee.'
	read(5,*)z,a
	write(6,*)'Enter energy in MeV.'
	read(5,*)energy
	goto 10

c Define absorber
200	write(6,*)
	write(6,*)'How many stopping layers are there in the absorber?'
	read(5,*)ianz
	
	do i = 1, ianz
201	   write(6,210)i
	   read(5,*)isg(i)
	   
	   if(isg(i).eq.0)then
	      write(6,220)i
	      read(5,*)den(i),thk(i)
	   elseif(isg(i).eq.1)then
	      write(6,230)i
	      read(5,*)prs(i),xln(i)
	   elseif(isg(i).eq.2)then
	      write(6,260)i
	      read(5,*)stdm
	      if(stdm.eq.1)then
	         write(6,230)i
		 read(5,*)prs(i),xln(i)
c CO2 standardized medium
		 isg(i) = 1
		 inn(i) = 2
		 znumb(i,1)=6
		 znumb(i,2)=8
		 anumb(i,1)=12
		 anumb(i,2)=16
		 elnum(i,1)=1
		 elnum(i,2)=2
		 goto 202
	      elseif(stdm.eq.2)then
		 write(6,221)i
		 read(5,*)sithk
c Si standardized medium
		 isg(i) = 0
		 den(i) = 2.329
		 thk(i) = 0.2329*sithk
		 inn(i) = 1
		 znumb(i,1)=14
		 anumb(i,1)=28
		 elnum(i,1)=1
		 goto 202
	      elseif(stdm.eq.3)then
		 write(6,222)i
		 read(5,*)thk(i)
c C (graphite) standardized medium
		 isg(i) = 0
		 den(i) = 2.267
		 inn(i) = 1
		 znumb(i,1)=6
		 anumb(i,1)=12
		 elnum(i,1)=1
		 goto 202
	      elseif(stdm.eq.4)then
		 write(6,230)i
		 read(5,*)prs(i),xln(i)
c C4H10 standardized medium
		 isg(i) = 1
		 inn(i) = 2
		 znumb(i,1)=6
		 znumb(i,2)=1
		 anumb(i,1)=12
		 anumb(i,2)=1
		 elnum(i,1)=4
		 elnum(i,2)=10
		 goto 202
	      elseif(stdm.eq.5)then
		 write(6,230)i
		 read(5,*)prs(i),xln(i)
c CF4 standardized medium
		 isg(i) = 1
		 inn(i) = 2
		 znumb(i,1)=6
		 znumb(i,2)=9
		 anumb(i,1)=12
		 anumb(i,2)=19
		 elnum(i,1)=1
		 elnum(i,2)=4
		 goto 202
	      elseif(stdm.eq.6)then
		 write(6,222)i
		 read(5,*)thk(i)
c CH2 standardized medium
		 isg(i) = 0
		 inn(i) = 2
		 den(i) = 0.94
		 znumb(i,1)=6
		 znumb(i,2)=1
		 anumb(i,1)=12
		 anumb(i,2)=1
		 elnum(i,1)=1
		 elnum(i,2)=2
		 goto 202
	      elseif(stdm.eq.7)then
		 write(6,222)i
		 read(5,*)thk(i)
c CD2 standardized medium
		 isg(i) = 0
		 inn(i) = 2
		 den(i) = 0.94
		 znumb(i,1)=6
		 znumb(i,2)=1
		 anumb(i,1)=12
		 anumb(i,2)=2
		 elnum(i,1)=1
		 elnum(i,2)=2
		 goto 202
	      elseif(stdm.eq.8)then
		 write(6,230)i
		 read(5,*)prs(i),xln(i)
c helium gas standardized medium
		 isg(i) = 1
		 inn(i) = 1
		 znumb(i,1)=2
		 anumb(i,1)=4
		 elnum(i,1)=1
		 goto 202
	      elseif(stdm.eq.9)then
		 write(6,230)i
		 read(5,*)prs(i),xln(i)
c hydrogen gas standardized medium
		 isg(i) = 1
		 inn(i) = 1
		 znumb(i,1)=1
		 anumb(i,1)=1
		 elnum(i,1)=2
		 goto 202
	      else
		 goto 201
	      endif
	   else
	      goto 201
	   endif
	   
	   write(6,240)i
	   read(5,*)inn(i)
	   
	   do j = 1, inn(i)
	      write(6,250)i,j
	      read(5,*)znumb(i,j),anumb(i,j), elnum(i,j)
	      concn(i,j)=1.
	   enddo

202	j = 1
	   
	enddo
	
	goto 10

210	format(/'Is layer ',I2,' a solid, gas, or standardized medium?',
     +/'(0 = solid, 1 = gas, 2 = standardized medium)',
     +/'(Standardized media are: CO2, Si, C [graphite], C4H10, CF4,',
     +/'CH2, CD2, He, and H2)')
220	format('Enter density(g/cm3) and thickness(mg/cm2) for layer ',I2)
221	format('Enter thickness(microns) for layer ',I2)
222	format('Enter thickness(mg/cm2) for layer ',I2)
230	format('Enter pressure(Torr) and length(cm) for layer ',I2)
240	format('How many elements are in layer ', I2 ,'?')
250	format('For layer ',I2,' ,element ',I2,' - enter Z, A, and the 
     + number of atoms per molecule. ')
260	format('Which standardized medium for layer ', I2, '?',
     +/'1: CO2',
     +/'2: Si',
     +/'3: C (graphite)',
     +/'4: C4H10',
     +/'5: CF4',
     +/'6: CH2',
     +/'7: CD2',
     +/'8: He-gas',
     +/'9: H2-gas')

c Edit absorber	
300	write(6,301)
	read(5,*)ism
301	format(/'  1 edit layer'/                                              
     *       '  2 add layer onto end of absorber'/                                                
     *       '  3 add new layer in front of absorber'/                                     
     *       '  4 return to main menu')
     
      	if(ism.eq.1)then
      		goto 310
      	elseif(ism.eq.2)then
      		goto 340
      	elseif(ism.eq.3)then
      		goto 360
      	elseif(ism.eq.4)then
      		goto 10
      	else
      		goto 300
      	endif

c Edit layer      	
310	write(6,311)
	read(5,*)iedit
311	format(/'Which layer would you like to edit?')

312	write(6,313)
	read(5,*)issm
313	format(/'  1 change state, density, thickness, etc...'/                                              
     *       '  2 change elemental composition'/                                                
     *       '  3 go back to previous menu'/)
     
     	if(issm.eq.1)then
     		goto 315
     	elseif(issm.eq.2)then
     		goto 320
     	elseif(issm.eq.3)then
     		goto 300
     	else
     		goto 312
     	endif
     	
315	write(6,210)iedit
	read(5,*)isg(iedit)

	if(isg(iedit).eq.0)then
	      write(6,220)iedit
	      read(5,*)den(iedit),thk(iedit)
	elseif(isg(iedit).eq.1)then
	      write(6,230)iedit
	      read(5,*)prs(iedit),xln(iedit)
	elseif(isg(iedit).eq.2)then
	      write(6,260)iedit
	      read(5,*)stdm
	      if(stdm.eq.1)then
	         write(6,230)iedit
		 read(5,*)prs(iedit),xln(iedit)
c CO2 standardized medium
		 isg(iedit) = 1
		 inn(iedit) = 2
		 znumb(iedit,1)=6
		 znumb(iedit,2)=8
		 anumb(iedit,1)=12
		 anumb(iedit,2)=16
		 elnum(iedit,1)=1
		 elnum(iedit,2)=2
		 goto 312
	      elseif(stdm.eq.2)then
		 write(6,221)iedit
		 read(5,*)sithk
c Si standardized medium
		 isg(iedit) = 0
		 den(iedit) = 2.329
		 thk(iedit) = 0.2329*sithk
		 inn(iedit) = 1
		 znumb(iedit,1)=14
		 anumb(iedit,1)=28
		 elnum(iedit,1)=1
		 goto 312
	      elseif(stdm.eq.3)then
		 write(6,222)iedit
		 read(5,*)thk(iedit)
c C (graphite) standardized medium
		 isg(iedit) = 0
		 den(iedit) = 2.267
		 inn(iedit) = 1
		 znumb(iedit,1)=6
		 anumb(iedit,1)=12
		 elnum(iedit,1)=1
		 goto 312
	      elseif(stdm.eq.4)then
		 write(6,230)iedit
		 read(5,*)prs(iedit),xln(iedit)
c C4H10 standardized medium
		 isg(iedit) = 1
		 inn(iedit) = 2
		 znumb(iedit,1)=6
		 znumb(iedit,2)=1
		 anumb(iedit,1)=12
		 anumb(iedit,2)=1
		 elnum(iedit,1)=4
		 elnum(iedit,2)=10
		 goto 312
	      elseif(stdm.eq.5)then
		 write(6,230)iedit
		 read(5,*)prs(iedit),xln(iedit)
c C4H10 standardized medium
		 isg(iedit) = 1
		 inn(iedit) = 2
		 znumb(iedit,1)=6
		 znumb(iedit,2)=9
		 anumb(iedit,1)=12
		 anumb(iedit,2)=19
		 elnum(iedit,1)=1
		 elnum(iedit,2)=4
		 goto 312
	      elseif(stdm.eq.6)then
		 write(6,222)iedit
		 read(5,*)thk(iedit)
c CH2 standardized medium
		 isg(iedit) = 0
		 inn(iedit) = 2
		 den(iedit) = 0.94
		 znumb(iedit,1)=6
		 znumb(iedit,2)=1
		 anumb(iedit,1)=12
		 anumb(iedit,2)=1
		 elnum(iedit,1)=1
		 elnum(iedit,2)=2
		 goto 312
	      elseif(stdm.eq.7)then
		 write(6,222)iedit
		 read(5,*)thk(iedit)
c CD2 standardized medium
		 isg(iedit) = 0
		 inn(iedit) = 2
		 den(iedit) = 0.94
		 znumb(iedit,1)=6
		 znumb(iedit,2)=1
		 anumb(iedit,1)=12
		 anumb(iedit,2)=2
		 elnum(iedit,1)=1
		 elnum(iedit,2)=2
		 goto 312
	      elseif(stdm.eq.8)then
		 write(6,230)iedit
		 read(5,*)prs(iedit),xln(iedit)
c helium gas standardized medium
		 isg(iedit) = 1
		 inn(iedit) = 1
		 znumb(iedit,1)=2
		 anumb(iedit,1)=4
		 elnum(iedit,1)=1
	      elseif(stdm.eq.9)then
		 write(6,230)iedit
		 read(5,*)prs(iedit),xln(iedit)
c hydrogen gas standardized medium
		 isg(iedit) = 1
		 inn(iedit) = 1
		 znumb(iedit,1)=1
		 anumb(iedit,1)=1
		 elnum(iedit,1)=2		 
	      else
		 goto 315
	      endif
	else
	      goto 315
	endif
	
	goto 312
	
320	write(6,240)iedit
	read(5,*)inn(iedit)
	   
	do j = 1, inn(iedit)
	      write(6,250)iedit,j
	      read(5,*)znumb(iedit,j),anumb(iedit,j), elnum(iedit,j)
	      concn(iedit,j)=1.
	enddo
	
	goto 312

c Add layer onto end
340	ianz = ianz + 1

345	write(6,210)ianz
	read(5,*)isg(ianz)

	if(isg(ianz).eq.0)then
	      write(6,220)ianz
	      read(5,*)den(ianz),thk(ianz)
	elseif(isg(ianz).eq.1)then
	      write(6,230)ianz
	      read(5,*)prs(ianz),xln(ianz)
	elseif(isg(ianz).eq.2)then
	      write(6,260)ianz
	      read(5,*)stdm
	      if(stdm.eq.1)then
	         write(6,230)ianz
		 read(5,*)prs(ianz),xln(ianz)
c CO2 standardized medium
		 isg(ianz) = 1
		 inn(ianz) = 2
		 znumb(ianz,1)=6
		 znumb(ianz,2)=8
		 anumb(ianz,1)=12
		 anumb(ianz,2)=16
		 elnum(ianz,1)=1
		 elnum(ianz,2)=2
		 goto 300
	      elseif(stdm.eq.2)then
		 write(6,221)ianz
		 read(5,*)sithk
c Si standardized medium
		 isg(ianz) = 0
		 den(ianz) = 2.329
		 thk(ianz) = 0.2329*sithk
		 inn(ianz) = 1
		 znumb(ianz,1)=14
		 anumb(ianz,1)=28
		 elnum(ianz,1)=1
		 goto 300
	      elseif(stdm.eq.3)then
		 write(6,222)ianz
		 read(5,*)thk(ianz)
c C (graphite) standardized medium
		 isg(ianz) = 0
		 den(ianz) = 2.267
		 inn(ianz) = 1
		 znumb(ianz,1)=6
		 anumb(ianz,1)=12
		 elnum(ianz,1)=1
		 goto 300
	      elseif(stdm.eq.4)then
		 write(6,230)ianz
		 read(5,*)prs(ianz),xln(ianz)
c C4H10 standardized medium
		 isg(ianz) = 1
		 inn(ianz) = 2
		 znumb(ianz,1)=6
		 znumb(ianz,2)=1
		 anumb(ianz,1)=12
		 anumb(ianz,2)=1
		 elnum(ianz,1)=4
		 elnum(ianz,2)=10
		 goto 300
	      elseif(stdm.eq.5)then
		 write(6,230)ianz
		 read(5,*)prs(ianz),xln(ianz)
c CF4 standardized medium
		 isg(ianz) = 1
		 inn(ianz) = 2
		 znumb(ianz,1)=6
		 znumb(ianz,2)=9
		 anumb(ianz,1)=12
		 anumb(ianz,2)=19
		 elnum(ianz,1)=1
		 elnum(ianz,2)=4
		 goto 300
	      elseif(stdm.eq.6)then
		 write(6,222)ianz
		 read(5,*)thk(ianz)
c CH2 standardized medium
		 isg(ianz) = 0
		 inn(ianz) = 2
		 den(ianz) = 0.94
		 znumb(ianz,1)=6
		 znumb(ianz,2)=1
		 anumb(ianz,1)=12
		 anumb(ianz,2)=1
		 elnum(ianz,1)=1
		 elnum(ianz,2)=2
		 goto 300
	      elseif(stdm.eq.7)then
		 write(6,222)ianz
		 read(5,*)thk(ianz)
c CD2 standardized medium
		 isg(ianz) = 0
		 inn(ianz) = 2
		 den(ianz) = 0.94
		 znumb(ianz,1)=6
		 znumb(ianz,2)=1
		 anumb(ianz,1)=12
		 anumb(ianz,2)=2
		 elnum(ianz,1)=1
		 elnum(ianz,2)=2
		 goto 300
	      elseif(stdm.eq.8)then
		 write(6,230)ianz
		 read(5,*)prs(ianz),xln(ianz)
c helium gas standardized medium
		 isg(ianz) = 1
		 inn(ianz) = 1
		 znumb(ianz,1)=2
		 anumb(ianz,1)=4
		 elnum(ianz,1)=1
	      elseif(stdm.eq.9)then
		 write(6,230)ianz
		 read(5,*)prs(ianz),xln(ianz)
c hydrogen gas standardized medium
		 isg(ianz) = 1
		 inn(ianz) = 1
		 znumb(ianz,1)=1
		 anumb(ianz,1)=1
		 elnum(ianz,1)=2
		 goto 300
	      else
		 goto 345
	      endif
	else
	      goto 345
	endif
	
	write(6,240)ianz
	read(5,*)inn(ianz)
	   
	do j = 1, inn(ianz)
	      write(6,250)ianz,j
	      read(5,*)znumb(ianz,j),anumb(ianz,j), elnum(ianz,j)
	      concn(ianz,j)=1.
	enddo
	
	goto 300

c Add new layer in front of absorber 
360	ianz = ianz + 1

	do j = ianz, 2, -1
	   isg(j)=isg(j-1)
	   den(j)=den(j-1)
	   thk(j)=thk(j-1)
	   prs(j)=prs(j-1)
	   xln(j)=xln(j-1)
	   inn(j)=inn(j-1)
	   
	   do k = 1, inn(j)
	      znumb(j,k)=znumb(j-1,k)
	      anumb(j,k)=anumb(j-1,k)
	      elnum(j,k)=elnum(j-1,k)
	      concn(j,k)=concn(j-1,k)
	   enddo
	   
	enddo
	
365	write(6,210)1
	read(5,*)isg(1)

	if(isg(1).eq.0)then
	      write(6,220)1
	      read(5,*)den(1),thk(1)
	elseif(isg(1).eq.1)then
	      write(6,230)1
	      read(5,*)prs(1),xln(1)
	elseif(isg(1).eq.2)then
	      write(6,260)1
	      read(5,*)stdm
	      if(stdm.eq.1)then
	         write(6,230)1
		 read(5,*)prs(1),xln(1)
c CO2 standardized medium
		 isg(1) = 1
		 inn(1) = 2
		 znumb(1,1)=6
		 znumb(1,2)=8
		 anumb(1,1)=12
		 anumb(1,2)=16
		 elnum(1,1)=1
		 elnum(1,2)=2
		 goto 300
	      elseif(stdm.eq.2)then
		 write(6,221)1
		 read(5,*)sithk
c Si standardized medium
		 isg(1) = 0
		 den(1) = 2.329
		 thk(1) = 0.2329*sithk
		 inn(1) = 1
		 znumb(1,1)=14
		 anumb(1,1)=28
		 elnum(1,1)=1
		 goto 300
	      elseif(stdm.eq.3)then
		 write(6,222)1
		 read(5,*)thk(1)
c C (graphite) standardized medium
		 isg(1) = 0
		 den(1) = 2.267
		 inn(1) = 1
		 znumb(1,1)=6
		 anumb(1,1)=12
		 elnum(1,1)=1
		 goto 300
	      elseif(stdm.eq.4)then
		 write(6,230)1
		 read(5,*)prs(1),xln(1)
c C4H10 standardized medium
		 isg(1) = 1
		 inn(1) = 2
		 znumb(1,1)=6
		 znumb(1,2)=1
		 anumb(1,1)=12
		 anumb(1,2)=1
		 elnum(1,1)=4
		 elnum(1,2)=10
		 goto 300
	      elseif(stdm.eq.5)then
		 write(6,230)1
		 read(5,*)prs(1),xln(1)
c CF4 standardized medium
		 isg(1) = 1
		 inn(1) = 2
		 znumb(1,1)=6
		 znumb(1,2)=9
		 anumb(1,1)=12
		 anumb(1,2)=19
		 elnum(1,1)=1
		 elnum(1,2)=4
		 goto 300
	      elseif(stdm.eq.6)then
		 write(6,222)1
		 read(5,*)thk(1)
c CH2 standardized medium
		 isg(1) = 0
		 inn(1) = 2
		 den(1) = 0.94
		 znumb(1,1)=6
		 znumb(1,2)=1
		 anumb(1,1)=12
		 anumb(1,2)=1
		 elnum(1,1)=1
		 elnum(1,2)=2
		 goto 300
	      elseif(stdm.eq.7)then
		 write(6,222)1
		 read(5,*)thk(1)
c CD2 standardized medium
		 isg(1) = 0
		 inn(1) = 2
		 den(1) = 0.94
		 znumb(1,1)=6
		 znumb(1,2)=1
		 anumb(1,1)=12
		 anumb(1,2)=2
		 elnum(1,1)=1
		 elnum(1,2)=2
		 goto 300
	      elseif(stdm.eq.8)then
		 write(6,230)1
		 read(5,*)prs(1),xln(1)
c helium gas standardized medium
		 isg(1) = 1
		 inn(1) = 1
		 znumb(1,1)=2
		 anumb(1,1)=4
		 elnum(1,1)=1
		 goto 300
	      elseif(stdm.eq.9)then
		 write(6,230)1
		 read(5,*)prs(1),xln(1)
c hydrogen gas standardized medium
		 isg(1) = 1
		 inn(1) = 1
		 znumb(1,1)=1
		 anumb(1,1)=1
		 elnum(1,1)=2
		 goto 300
	      else
		 goto 365
	      endif
	else
	      goto 365
	endif
	
	write(6,240)1
	read(5,*)inn(1)
	   
	do j = 1, inn(1)
	      write(6,250)1,j
	      read(5,*)znumb(1,j),anumb(1,j), elnum(1,j)
	      concn(1,j)=1.
	enddo
	
	goto 300

c Find the thickness of the material required to stop the stopee.
600	write(6,610)
	read(5,*)j

615	tethk = 0.01
	if(den(j).eq.(2.329).AND.znumb(j,1).eq.14.AND.
     +       anumb(j,1).eq.28.AND.elnum(j,1).eq.1.and.isg(j).eq.0)then
     	error = siupb * 0.2329
		do while (1.eq.1)
			zp = z
			thk(j) = tethk
			edep = 0.0
			call desorb(ianz, zp, a, energy, loste)
			do k=1,ianz
				edep = edep + loste(k)
			enddo
			if(edep.ge.energy.AND.error.lt.(0.01/0.2329))then
				write(6,625)
				write(6,620)tethk/0.2329+0.01
				exit
			elseif(edep.ge.energy.AND.tethk.eq.0.01)then
				write(6,625)
				write(6,620)0.01
				write(6,*)edep
				exit
			elseif(edep.lt.energy.AND.tethk.ge.siupb*0.2329)then
				write(6,630)
				exit
			elseif(edep.ge.energy)then
				tethk = tethk - error
				error = error/2.0
			elseif(edep.lt.energy)then
				tethk = tethk + error
				error = error/2.0
			endif
		enddo
	elseif(isg(j).eq.0)then
		error = soupb
		do while (1.eq.1)
			zp = z
			thk(j) = tethk
			edep = 0.0
			call desorb(ianz, zp, a, energy, loste)
			do k=1,ianz
				edep = edep + loste(k)
			enddo
			if(edep.ge.energy.AND.error.lt.0.01)then
				write(6,625)
				write(6,621)tethk+0.01
				exit
			elseif(edep.ge.energy.AND.tethk.eq.0.01)then
				write(6,625)
				write(6,621)0.01
				exit
			elseif(edep.lt.energy.AND.tethk.ge.siupb)then
				write(6,631)
				exit
			elseif(edep.ge.energy)then
				tethk = tethk - error
				error = error/2.0
			elseif(edep.lt.energy)then
				tethk = tethk + error
				error = error/2.0
			endif
		enddo
	elseif(isg(j).eq.1)then
		error = gaupb
		do while (1.eq.1)
			zp = z
			xln(j) = tethk
			edep = 0.0
			call desorb(ianz, zp, a, energy, loste)
			do k=1,ianz
				edep = edep + loste(k)
			enddo
			if(edep.ge.energy.AND.error.lt.0.01)then
				write(6,625)
				write(6,622)tethk+0.01
				exit
			elseif(edep.ge.energy.AND.tethk.eq.0.01)then
				write(6,625)
				write(6,622)0.01
				exit
			elseif(edep.lt.energy.AND.tethk.ge.siupb)then
				write(6,632)
				exit
			elseif(edep.ge.energy)then
				tethk = tethk - error
				error = error/2.0
			elseif(edep.lt.energy)then
				tethk = tethk + error
				error = error/2.0
			endif
		enddo
	else
		write(6,630)
	endif

	goto 10

610	format(/'Which layer would you like to vary to determine',
     +/'how much of it is required to stop the stopee?')

620	format(/f10.2, ' microns')
621	format(/f10.2, ' mg/cm2')
622	format(/f10.2, ' cm')
625	format(/'Amount of absorber to stop the stopee')

630	format(/'Not even 232 microns of the absorber stop the stopee.')
631	format(/'Not even 1 g/cm2 of the absorber stop the stopee.')
632	format(/'Not even 1 m of the absorber stop the stopee.')

633	format(/'The absorber is neither a gas nor a solid...')


c Perform caculation
400	zp = z
	call desorb(ianz, zp, a, energy, loste)
	
	edep = 0.0


c If particle stopped in a layer set the energy loss to zero in the rest
	
	do j = 1, ianz
	   edep = edep + loste(j)
	   if(edep.ge.energy)then
	      do k = j + 1, ianz + 1
	         loste(k) = 0.0
	      enddo
	      goto 402
	   endif
	enddo

c Produce output
402	write(6,510)z,a, energy
	write(7,510)z,a, energy
	write(6,520)ianz
	write(7,520)ianz
	
	do i = 1 ,ianz
	   if(isg(i).eq.0)then
	      write(6,530)i
	      write(7,530)i
	      write(6,540)den(i),thk(i)
	      write(7,540)den(i),thk(i)
	   elseif(isg(i).eq.1)then
	      write(6,550)i
	      write(7,550)i
	      write(6,560)prs(i),xln(i)
	      write(7,560)prs(i),xln(i)
	   endif
	   
	   do j = 1, inn(i)
	      write(6,570)znumb(i,j),anumb(i,j),elnum(i,j)
	      write(7,570)znumb(i,j),anumb(i,j),elnum(i,j)
	   enddo
	   
	   write(6,410)loste(i)
	   write(7,410)loste(i)
	   
	enddo
	
	write(6,420)loste(ianz+1)
	write(7,420)loste(ianz+1)
	write(6,430)
	
	goto 10
	
410	format('Energy lost in layer = ',f7.3,' MeV')
420	format(/'Energy remaining = ',f7.3,' MeV')
430	format(/'Output also written to stopit.log')

500	write(6,510)z,a, energy
	write(6,520)ianz
	
	do i = 1 ,ianz
	   if(isg(i).eq.0)then
	      write(6,530)i
	      write(6,540)den(i),thk(i)
	   elseif(isg(i).eq.1)then
	      write(6,550)i
	      write(6,560)prs(i),xln(i)
	   endif
	   
	   do j = 1, inn(i)
	      write(6,570)znumb(i,j),anumb(i,j),elnum(i,j)
	   enddo
	enddo
	
	goto 10
	
510	format(//'Charged particle has Z = ',f3.0,'  A = ',f3.0,'  Initial 
     +energy = ',f8.3,' MeV.')
520	format(//'Absorber contains ',I2,' layers.')
530	format(//'Layer # ',I2,' -  Solid Absorber')
540	format('Density = ',e10.4,' g/cm3      Thickness = ',e10.4,' 
     +mg/cm2')
550	format(//'Layer # ',I2,' -  Gas Absorber')
560	format('Pressure = ',e10.4,' Torr      Length = ',e10.4,' cm')
570	format('Z = ',f3.0,'  A = ',f3.0,4x,'Atoms per molecule = ',f3.0)	
     	
1000 	stop
	end	
