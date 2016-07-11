// ConsoleApplication1.cpp : Defines the entry point for the console application.
//

#include <fstream>
#include <iostream>
#include <cmath>
#include <TRandom3.h>


static TRandom3 *rando = new TRandom3(0.);


class Common
{
public:
	int aISG[19], aINN[19];
	double aDEN[19], aTHK[19], aPRS[19], aXLN[19], aARDEN[19];
	double aZNUMB[19][4], aANUMB[19][4], aELNUM[19][4], aCONCN[19][4];
	double aPRDEN[19][4], aPRTHK[19][4];
	double loste[19];

	double desorb(int ianz, double zp, double ap, double ep);
	double ads(int * pI1, double * psign, double * pXN1, double * pEPS, double * pA, double * pZ, double * pE, double * pDEE, int * pISTAT);
	double setabs(int * pINW, double A[], double Z[], double AN[], double T[], double * pTH, double D[], double * pDN);
	double setabg(int * pINW, double A[], double Z[], double AN[], double CN[], double T[], double * pTH, double D[], double * pDN, double * pPR, double * pXL);
	double dedx(double * pZ1, double * pA1, double * pZ2, double * pA2, double * pRHO, double * pENER, double * pV, int * pIFG, double * pDEDXHI, double * pDEDXTO);

};
double Common::desorb(int ianz, double zp, double ap, double ep)
{
	double aZNUMBW[4], aANUMBW[4], aELNUMW[4], aCONCNW[4];
	double aPRDENW[4], aPRTHKW[4];
	double aE[19], aDE[19], aXmem[19];
	double aTOUT[19], aTOUTE[19];
	double aEptable[50][500][2], aEmintabz[50];
	int INNW, ISTAT, ISTORE, INS, iArrayValue, aIANZV[5];
	int io1, IO2, IO3, io0, iopt, ianzi, ianzide, ISN, I, J, izp, I1STPASS, ipunch;
	double DENW, THKW, PRSW, XLNW, EI, EPS, XUPDN, XNS, DEI, EIOLD;

	io1 = 9;
	IO2 = 11;
	IO3 = 12;
	io0 = 1;
	iopt = 1;
	ianzi = 2;
	ianzide = 1;
	ISTAT = 0;
	XNS = 2.0;
	DEI = 0.0;
	


	/* the mass table is to be used only for iopt = 5,6
	use atomic masses to average for isotipic composition.
	taken from Formulas Facts and Constants, H. J. Fischbeck 
	K. H. Fischbeck. Springer - Verlag 1987 2nd ed, pages 164-183.*/

	double amass[70] = { 1.01, 4.00, 6.94, 9.01, 10.81, 12.01, 14.01, 16.00, 19.00,
		20.18, 22.99, 24.31, 26.98, 28.09, 30.97, 32.07, 35.45, 39.95,
		39.10, 40.08, 44.96, 47.88, 50.94, 52.00, 54.94, 55.85, 58.93,
		58.69, 63.55, 65.39, 69.72, 72.59, 74.92, 78.96, 79.90, 83.80,
		85.47, 87.62, 88.91, 91.22, 92.91, 95.94, 98., 101.07, 102.91,
		106.42, 107.87, 112.41, 114.82, 118.71, 121.75, 127.60, 126.90,
		131.29, 132.91, 137.33, 138.91, 140.12, 140.91, 144.24, 147.,
		150.36, 151.96, 157.25, 158.93, 162.5, 164.93, 167.26, 168.93,
		173.04 };

	// For Z > 70, you are in trouble!
	std::ofstream outf("desorb.dat");
	

/*************************************************************
	  IOPT = 1 - SUPPLY ENERGY OF PARTICLE ENTERING
                 THE ABSORBER ARRAY AND GET LOSS AND
                 RANGES
      IOPT = 2 - SUPPLY TARGET, PROECTILE AND EJECTILE
                 INFO. AND THEN GO THROUGH ABSORBER
                 SANDWICH
      IOPT = 3 - CALCULATE ENERGY DEPOSITS IN DETECTOR
                 DETECTOR DIMENSIONS ARE STANDARD AND
                 THE VARIABLE -'IDET' - CHOOSES BETWEEN
                 VARIETY OF AVAILABLE DETECTORS
      IOPT = 4 - FINDS MAXIMUM ENERGY THAT CAN BE STOPPED IN
                 IANZ ELEMENTS OF THE SANDWICH FOR GIVEN
                 ZP, AP.
                 WHEN CALCULATION IS FINISHED, THE PROGRAM READS
                 IN NEW VALUES OF ZP, AP AND RESTARTS. TO END
                 THE PROGRAM, GIVE ZP < 0.
                 IN ORDER TO HELP THE SPEED OF THE PROGRAM,
                 GIVE THE PARTICLE'S "Z" IN increasing  ORDER.
      IOPT = 5 - STORES ARRAYS OF Edet AS A FUNCTION OF INCIDENT    
                 ENERGY AND THE PARTICLE'S ID (Z,A)
                 ARRAY LABELED  eptable(Z-Zth,Einc,ipunch)
                 ipunch = 1 stopped,  = 2 punched through
                 Einc = E(incident)/detable
                 Zth  = lowest Z considered - 1
*************************************************************/

	
/*    ianz  = number of elements in absorber "sandwich" - the 
              particle deposits all its energy in these layers.
      ianzi = index of last layer in which the energy of the 
              particle is not recorded - this unreecorded energy
              is used in the DST production in two modes:
              when making DST tape from data:
              Since the detector records only deposited energy
              the output tables are used to correct for this 
              deficinecy and for a given dharge and mass extrapolate
              the measured energy to the target exit energy
              when making DST tape from model calculations:
              The "lost" energy is a source of broadening of the 
              energy spectra due to straggling - this "smearing" 
              is estimated and superposed on the calculated spectra.
	   ianzide = element # for DE calculation*
	   
      zth= threshold Z for incident energy tble calc. (iopt=5,6)
      zmax = maximum z for table calculation
      detable = the energy step size used for array storage   
      emin = starting incident energy for table
      emax = mazximum incident energy for table calculation
      EP is ignored when iopt = 5 or 6*/

	for (ISN = 0; ISN < ianz; ISN++)
	{
		if (Common::aISG[ISN] != 1)
		{
			aTOUT[ISN] = Common::aTHK[ISN] / (Common::aDEN[ISN] * 1000.0);
			aTOUTE[ISN] = aTOUT[ISN] / 2.54;
		}

	}

	outf << "Passage of a charged particle through absorber sandwich\n";
	outf << "AP = " << ap << "      ZP = " << zp << "      Initial Energy = " << ep << "\n";
	outf << "Absorber sanwhich contains " << ianz << "layers \n";

	for (I = 0; I < ianz; I++)
	{
		INNW = Common::aINN[I];
		for (J = 0; J < INNW; J++)
		{
			aANUMBW[J] = Common::aANUMB[I][J];
			aZNUMBW[J] = Common::aZNUMB[I][J];
			aELNUMW[J] = Common::aELNUM[I][J];
			aCONCNW[J] = Common::aCONCN[I][J];


		}
		DENW = Common::aDEN[I];
		XLNW = Common::aXLN[I];
		PRSW = Common::aPRS[I];
		THKW = Common::aTHK[I];

		if (Common::aISG[I] == 1)
		{
			Common::setabg( &INNW, aANUMBW, aZNUMBW, aELNUMW, aPRTHKW, aCONCNW, &THKW, aPRDENW, &DENW, &PRSW, &XLNW);
			Common::aDEN[I] = 0.0;
			Common::aTHK[I] = 0.0;

			for (J = 0; J < INNW; J++)
			{
				Common::aPRDEN[I][J] = aPRDENW[J];
				Common::aPRTHK[I][J] = aPRTHKW[J];
				Common::aDEN[I] = Common::aDEN[I] + Common::aPRDEN[I][J];
				Common::aTHK[I] = Common::aTHK[I] + Common::aPRTHK[I][J];
			}
		}
		else if (Common::aISG[I] != 1)
		{
			Common::setabs(&INNW, aANUMBW, aZNUMBW, aELNUMW, aPRTHKW, &THKW, aPRDENW, &DENW);
			for (J = 0; J < INNW; J++)
			{
				Common::aPRDEN[I][J] = aPRDENW[J];
				Common::aPRTHK[I][J] = aPRTHKW[J];
			}

		}
	}
	zp = zp + 0.5;
	izp = (int)zp;
	zp = zp - 0.5;

	// EI energy in
	EI = ep;
	XUPDN = -1.0;
	EPS = 0.0001;
	I1STPASS = 1;

	ipunch = 2;


	for (I = 0; I < ianz; I++)
	{

		if (Common::aISG[I] == 0)
		{
			outf << "Layer # " << I << "      - Solid Absorber -      Areal Density = " << Common::aTHK[I] << "   Thickness " << aTOUT[I] << "CM    or" << aTOUTE[I] << " inch    Density = " << Common::aDEN[I] << "g/cm^3 \n";
		}
		else if (Common::aISG[I] == 1)
		{ 		
			outf << "Layer # " << I << "      - Gas Absorber -      Areal Density = " << Common::aTHK[I] << "   Pressure " << Common::aPRS[I] << "Torr    Length" << Common::aXLN[I] << " cm    Density = " << Common::aDEN[I] << "mg/cm^3 \n";
		}
		
		for (J = 0; J < Common::aINN[I]; J++)
		{
			outf << "A = " << Common::aANUMB[I][J] << "      Z = " << Common::aZNUMB[I][J] << "   Areal Density    (Partial) " << Common::aPRTHK[I][J] << "mg/cm^2" << "\n";
		}
	}
	/*XNS Initial nuber of intervals for integration of DE
	DEI energy out- energy in(<0 for energy loss)
	E[I] Energy left after I'th element (EP-DE[0] - DE[1] + .....)
	if particle stopped in detector this is equal to energy lost in
	remaining layers */

	I = 0;
	Common::ads(&I, &XUPDN, &XNS, &EPS, &ap, &zp, &EI, &DEI, &ISTAT);
	EIOLD = EI;
	aDE[I] = DEI;
	aE[I] = EI + DEI;
	EI = aE[I];
	XNS = XNS + 0.1;
	INS = (int)XNS;

	outf << "Calc in- " << INS << " steps   Energy in = " << EIOLD << "Partial =" << EI << "mg/cm^3\n";

	Common::loste[I] = -1.0 * aDE[I];

	if (EI < EPS || ISTAT == -1)
	{
		outf << "Charged particle stopped in layer # " << I << "\n";
	}

	iArrayValue = ianz + 1;
	Common::loste[iArrayValue] = aE[ianz];

	ISTORE = I;
	
	if (EI < EPS)
	{
		ipunch = 1;
	}


	return 0;

}

double Common::setabs(int * pINW, double A[], double Z[], double AN[], double T[], double * pTH, double D[], double * pDN)
{
	//Function for setting up composite absorber data
	//Partial Densities and Thicknesses

	int INW;
	double DN, TH;

	INW = *pINW;
	DN = *pDN;
	TH = *pTH;

	int I;
	double AW;

	AW = 0;

	for (I = 0; I < INW; I++)
	{
		AW = AW + (A[I] * AN[I]);
	}
	for (I = 0; I < INW; I++)
	{
		AN[I] = A[I] * AN[I] / AW;
		T[I] = TH * AN[I];
		D[I] = DN*AN[I];

		
	}

	*pINW = INW;
	*pDN = DN;
	*pTH = TH;

	return 0;
}

double Common::setabg(int * pINW, double A[], double Z[], double AN[], double CN[], double T[], double * pTH, double D[], double * pDN, double * pPR, double * pXL)
{
	double P, X, AWW, AW, TH, DN, PR, XL;
	int I, INW;
	TH = *pTH;
	DN = *pDN;
	PR = *pPR;
	XL = *pXL;
	INW = *pINW;

	P = PR / 760.0;
	X = XL / 22.4;
	AWW = 0.0;
	AW = 0.0;

	for (I = 0; I < INW; I++)
	{
		AW = AW + (A[I] * AN[I]);
		AWW = AWW + (A[I] * AN[I] * CN[I]);
		T[I] = P * X * A[I] * AN[I] * CN[I];
		D[I] = T[I] / XL;
	}

	*pTH = TH;
	*pDN = DN;
	*pPR = PR;
	*pXL = XL;
	*pINW = INW;

	return 0;

}

double Common::ads(int * pI1, double * psign, double * pXN1, double * pEPS, double * pA, double * pZ, double * pE, double * pDEE, int * pISTAT)
{
	//Subroutine for energy loss calculations
	//Call DEDX for stopping power calculations
	double EH, DEDNEXT, AX, ZX, FX, DENST, VH, DE, DEX, sign, XN1, EPS, A, Z, E, DEE;
	double DDD, DED1ST, DDS, DDR;
	int N1, K, J1, J, I, ISGW, I1, ISTAT;
	I1 = *pI1;
	ISTAT = *pISTAT;
	sign = *psign;
	XN1 = *pXN1;
	EPS = *pEPS;
	A = *pA;
	Z = *pZ;
	E = *pE;
	DEE = *pDEE;
	DE = 0.0;
	DEX = 0.0;
	// NI number of integrations for energy loss
	EH = E;
	XN1 = XN1 + 0.001;
	N1 = (int)XN1;
	DEDNEXT = 0.0;

	for (K = 1; K <= N1; K++)
	{
		J1 = Common::aINN[I1];
		ISGW = Common::aISG[I1];
		I = I1;
		for (J = 0; J < J1; J++)
		{
			AX = Common::aANUMB[I][J];
			ZX = Common::aZNUMB[I][J];
			FX = Common::aPRTHK[I][J]/XN1;
			DENST = Common::aPRDEN[I][J];
			VH = sqrt(2.13e-3 * EH / A);

			Common::dedx(&Z, &A, &ZX, &AX, &DENST, &EH, &VH, &ISGW, &DEX, &DE);
			EH = EH + DE * sign * FX;


		
			if (EH <= 0.0)
			{
				if (K <= 2)
				{
					N1 = N1 * 2;
                                        XN1 = (double)N1;
					J = -1;
					K = 0;
					DEDNEXT = 0.0;
					EH = E;

				}
				ISTAT = -1;
				break;

			}
			if (K <= 2)
			{
				DEDNEXT = DEDNEXT + DE * FX;
			}

		}
		if (K == 1)
		{
			DED1ST = DEDNEXT;
			DEDNEXT = 0.0;
		}
		if (K == 2)
		{
			DDD = DED1ST - DEDNEXT;
			if (DDD < 0.0)
			{
				DDD = -DDD;
			}

			DDS = DED1ST + DEDNEXT;
			DDR = DDD / DDS;
			if (DDR > EPS)
			{
				N1 = N1 * 2;
                                XN1 = (double)N1;
				J = -1;
				K = 0;
				DEDNEXT = 0.0;
				EH = E;


			}
		}
	}

	ISTAT = 0;
	DEE = EH - E;

	*pI1 = I1;
	*pISTAT = ISTAT;
	*psign = sign;
	*pXN1 = XN1;
	*pEPS = EPS;
	*pA = A;
	*pZ = Z;
	*pE = E;
	*pDEE = DEE;
	return 0;
}

double Common::dedx(double * pZ1, double * pA1, double * pZ2, double * pA2, double * pRHO, double * pENER, double * pV, int * pIFG, double * pDEDXHI, double * pDEDXTO)
{

	double Z1, A1, Z2, A2, RHO, ENER, V, DEDXHI, DEDXTO;
	int IFG;

	Z1 = *pZ1;
	A1 = *pA1;
	Z2 = *pZ2;
	A2 = *pA2;
	RHO = *pRHO;
	ENER = *pENER;
	V = *pV;
	DEDXHI = *pDEDXHI;
	DEDXTO = *pDEDXTO;
	IFG = *pIFG;



	/*Program calculates the differential energy loss DE/DX in solid targets using a semiempirical formula
	deduced from experimental work
	
	This program is modified for gas absorbers
	REF.: K.BRAUNE,R.NOVOTNY,D.PELTE,D.HUSAR,D.SCHWALM,
	PROCEEDINGS - SPRING MEETING OF THE GERMAN PHYSICAL
	SOCIETY, VERHANDLUNGEN 4/1978
	K.BRAUNE, DIPLOM, HEIDELBERG 1979
	 	  H(Z2) IS A SUM OF FIVE GAUSSIAN FUNCTIONS.
	      A1        MASS NUMBER  - PROJECTILE
	      Z2        ATOMIC NUMBER ABSORBER
	      A1        MASS NUMBER   ABSORBER
	      RHO       DENSITY OF THE ABSORBER (GRAMM/CM**3)
	      (MEANLESS IF GAS ABSORBER )
	      ENER      ENERGY OF THE PROJECTILE (MEV)
	      V         VELOCITY OF THE PROJECTILE
	      IN MEV/(MG/CM**2)
		  Z1       ATOMIC NUMBER - PROJECTILE	*/
	double XI, A2SAV, Z2SAV, FY, G1, G2, G3, G4, G5, HZ2, Z2ZWD, FG;
	double ALEFG, GXI, SQXI, C, FG0, AL, Y, VV0, FV, AZ1, QQ, GHI, VZ1;
	double ZA, EPS, SIGMAN, DEDXNU;
	A2SAV = 0.0;
	Z2SAV = 0.0;

	if (IFG == 1)
	{
		RHO = 1.0;
	}

	XI = V * V / Z2;

	/*Absorber - Function
	G(XI) = Y(EXP) - Y(Theory) Is deduced from experimental energy loss measurements*/

	if (A2 != A2SAV && Z2 != Z2SAV)
	{
		A2SAV = A2;
		Z2SAV = Z2;

		//FY is function Y
		FY = 54721.0 * (1.0 + 5.15E-2 * sqrt(A2 / RHO) - exp(-0.23*Z2));
		
		if (IFG == 1)
		{
			FY = 54721.0 * (1.35 - exp(Z2*((-0.13 + 0.0014*Z2))));
		}
	}

	//G(XI) Is the derivation of a guassian with variable height H(Z2)
	if (Z2 <= 26.0)
	{
		G1 = 19.84 * exp(-0.17 * (Z2 - 4.25)*(Z2 - 4.25));
	}
	else if (Z2 > 26.0)
	{
		G1 = 0.000001;
	}

	if (Z2 <= 38.0)
	{
		G2 = 17.12 * exp(-0.12 * (Z2 - 11.63)* (Z2 - 11.63));
	}
	else if (Z2 > 38.0)
	{
		G2 = 0.0000001;
	}

	G3 = 7.95 * exp(-0.015 * (Z2 - 30.2) * (Z2 - 30.2));
	G4 = 5.84 * exp(-0.022 * (Z2 - 48.63) * (Z2 - 48.63));
	G5 = 7.27 * exp(-0.005 * (Z2 - 73.06) * (Z2 - 73.06));
	HZ2 = (9.0 - (G1 + G2 + G3 + G4 + G5)) * 1.32e-5;
	Z2ZWD = cbrt(Z2) * cbrt(Z2);

	//Multiplication factors of G(XI)

	FG = (1.2e-4)*Z2*Z2 + ((2.49e-2)* A2 / RHO);
	if (IFG == 1)
	{
		FG = 1.3 / (1.0 + exp(3.0 - (Z2 / 5.0)));
	}

	ALEFG = log((2.7e-5) / FG);

	//Calculation of G(XI)

	GXI = 0.0;

	if (XI >= 1.0e-9 && XI <= 5.0e-4)
	{
		SQXI = sqrt(XI);
		C = (2.0 / Z2) * (SQXI / (1.0 + 1.0e4 * SQXI));

		if (IFG == 1)
		{
			C = C / 2.0;
		}

		FG0 = 1.0 / (1.0 + (XI * 10000.0)*(XI * 10000.0)*(XI * 10000.0));
		AL = log(XI) - ALEFG;
		GXI = (C - HZ2 * AL * exp(-0.32 * AL *AL))*FG0;
	}

	// Calculation of Y(XI)

	Y = 3.3e-4 * log(1.0 + (XI*FY)) + GXI;

	//Energy Loss of heavy ions
	//Effective charge

	VV0 = V * 137.0;
	FV = 1.0;

	if (V >= 0.62)
	{
		FV = 1.0 - exp(-VV0);
	}

	AZ1 = log(1.035 - 0.4 * exp(-0.16*Z1));

	QQ = V / (pow(Z1, 0.509));
	GHI = Z1;
	VZ1 = (-116.79 - 3350.4 * QQ) * QQ;
	if (VZ1 > -85.2)
	{
		GHI = Z1 * (1.0 - exp( VZ1 ));
	}
	if (Z1 > 2.0)
	{
		GHI = Z1 * (1.0 - exp(FV * AZ1 - 0.879 * (VV0 / pow(Z1, 0.65))));
	}

	//Effective charge for protons and alpha particles

	//Electronic energy loss DEDXHI
	DEDXHI = GHI * GHI * Z2 * Y / (A2 * V * V);
	//Nuclear energy loss DEDXNU
	ZA = sqrt(cbrt(Z1) * cbrt(Z1) + Z2ZWD);

	EPS = 3.25e4 * A2 * ENER / (Z1 * Z2 * (A1 + A2) * ZA);
	SIGMAN = 1.7 * sqrt(EPS) * log(EPS + 2.1718282) / (1.0 + 6.8 * EPS + 3.4 * sqrt(EPS) *  sqrt(EPS) *  sqrt(EPS));

	DEDXNU = SIGMAN * 5.105 * Z1 * Z2 * A1 / (ZA * A2 * (A1 + A2));
	//Total energy loss
	DEDXTO = DEDXHI + DEDXNU;

	*pZ1 = Z1;
	*pA1 = A1;
	*pZ2 = Z2;
	*pA2 = A2;
	*pRHO = RHO;
	*pENER = ENER;
	*pV = V;
	*pDEDXHI = DEDXHI;
	*pDEDXTO = DEDXTO;
	*pIFG = IFG;

	return 0;
}

void defprta( double energy, double * elost, double depth)
{
	Common Target;

	Target.aPRS[0] = 10.0;
	Target.aXLN[0] = 10.0;
	Target.aCONCN[0][0] = 1.0;
	Target.aCONCN[0][1] = 1.0;


	Target.aISG[0] = 0;
	Target.aINN[0] = 1;
	Target.aDEN[0] = 2.329;
	Target.aZNUMB[0][0] = 14;
	Target.aZNUMB[0][1] = 9;
	Target.aANUMB[0][0] = 40;
	Target.aANUMB[0][1] = 18;
	Target.aELNUM[0][0] = 1;
	Target.aELNUM[0][1] = 2;
	Target.loste[0] = 0;
	Target.aTHK[0] = 232.9;//*depth;

	Target.desorb(1, 1.0, 3.0, energy);
        
        *elost = Target.loste[0];

}
void defprtb( double energy, double * elost, double depth, double angle )
{
	Common Target;

        /*ISG(I) = 0  - FOR SOLID ABSORBERS
          (I) = 1  - FOR GASEOUS ABSORBERS
        INN(I) = NUMBER OF ELEMENTS IN ONE LAYER
             (E.G. CH4 HAS TWO ELEMENTS C AND H)
        DEN(I) = DENSITY OF ABSORBE (FOR SOLIDS)
        THK(I) = THICKNESS OF ABSORBER IN MG/CMSQ (FOR SOLIDS)
        PRS(I) = PRESSURE (IN MM HG) FOR GAS ABSORBER
        XLN(I) = PHYSICAL LENGTH OF ABSORBER ( IN CM ) FOR GAS

        **** ABSORBER COMPOSITION ****

        ELNUM(I,J) = NUMBER OF ATOMS OF ELEMENT J IN LAYER I
        CONCN(I,J) = CONCENTRATION (MOLAR) OF ELEMENT J IN LAYER I
        ANUMB(I,J) = MASS   NUMBER   OF ELEMENT J IN LAYER I
        ZNUMB(I,J) = ATOMIC NUMBER   OF ELEMENT J IN LAYER I

        PRDEN(I,J) = PARTIAL DENSITY OF ELEMENT J IN LAYER I
        PRTHK(I,J) = PARTIAL THICKNESS OF ELEMENT J IN LAYER I (MG/CMSQ)

        MAXIMUM OF NINETEEN LAYERS SPECIFIED*/

	Target.aPRS[0] = 10.0;
	Target.aXLN[0] = 10.0;
	Target.aCONCN[0][0] = 1.0;
	Target.aCONCN[0][1] = 1.0;


	Target.aISG[0] = 0;
	Target.aINN[0] = 2;
	Target.aDEN[0] = 3.18;
	Target.aZNUMB[0][0] = 20;
	Target.aZNUMB[0][1] = 9;
	Target.aANUMB[0][0] = 40;
	Target.aANUMB[0][1] = 10;
	Target.aELNUM[0][0] = 1;
	Target.aELNUM[0][1] = 2;
	Target.loste[0] = 0;
	Target.aTHK[0] = 0.5*(1.0-depth)/cos(angle);

	Target.desorb(1, 1.0, 3.0, energy);
        
        *elost = Target.loste[0];


}


int main()
{
	Common Target;
	double energy;
        double eloss;
        eloss = 0.0;
        double pendpth = rando->Uniform(0.0,1.0);
        double randtheta = acos( 2.0*rando->Uniform(0.0,1.0) - 1.0 )*100/180;


	energy = 20.0;

        defprta( energy, &eloss, pendpth);
        std::cout << "Depth = " << pendpth << " Energy Loss = " << eloss <<std::endl;

        energy = energy - eloss;

        defprtb( energy, &eloss, pendpth, randtheta);

	std::cout << "Energy Lost = " << eloss << " Theta " << randtheta << std::endl;

}


