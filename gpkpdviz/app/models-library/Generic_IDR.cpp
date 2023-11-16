[PROB]
Author: Jenny Nguyen, 2018
Source: mrgsolve model library

This combines all four indirect response models as found in the mrgsolve model library.

Change appropriate variables in PARAM block before compiling model. 

This assumes that there is a two-compartment PK model with one EV dosing compartment.
	- Modify if you need a different structure.
	- This can account for nonlinear clearance (CLNL).

Models:
	(1) Inhibition of k_in   (i.e. inhibition of response production)
	(2) Inhibition of k_out  (i.e. inhibition of response loss)
	(3) Stimulation of k_in  (i.e. stimulation of response production)
	(4) Stimulation of k_out (i.e. stimulation of response loss)

Time unit: day (generic)
Volume units: L (generic)
Validated: No

[CMT] @annotated
EV     : Extravascular compartment (mass)
CENT   : Central compartment (mass)
PERIPH : Peripheral compartment (mass) 
RESP   : Response compartment

[PARAM] @annotated
IDM  :  0  : Indirect response model type (1 - 4). CHANGE BEFORE COMPILING. Set at 0 to remind user to change.
CL   :  1  : Clearance (volume/time)
VC   : 20  : Central volume (volume)
Q    :  2  : Inter-compartmental clearance (volume/time)
VP   : 10  : Peripheral volume of distribution (volume)
KA   :  1  : Absorption rate constant (1/time)
KIN  : 10  : Response in rate constant (1/time)
KOUT :  2  : Response out rate constant (1/time)
IC50 :  2  : Concentration for 50% of max inhibition (mass/volume)
IMAX :  1  : Maximum inhibition 
EC50 :  2  : Concentration for 50% of max effect (mass/volume)
EMAX :  1  : Maximum effect 
n    :  1  : Emax model sigmoidicity
VMAX :  0  : Maximum reaction velocity (mass/time)
KM   :  2  : Michaelis constant (mass/volume)

[GLOBAL]
#define CP   (CENT / VC)
#define CT   (PERIPH / VP)
#define CLNL (VMAX / (KM + CP))
#define INH  (IMAX * pow(CP, n) / (pow(IC50, n) + pow(CP, n)))
#define STIM (EMAX * pow(CP, n) / (pow(EC50, n) + pow(CP, n)))

[MAIN]
RESP_0 = KIN/KOUT;

double INH1 = 0;
double INH2 = 0;
double STIM1 = 0;
double STIM2 = 0;

// define response for each IDM
if (IDM == 1) {
	INH1 = INH;
}

if (IDM == 2) {
	INH2 = INH;
}

if (IDM == 3) {
	STIM1 = STIM;
}

if (IDM == 4) {
	STIM2 = STIM;
}

[ODE]
dxdt_EV     = -KA*EV;
dxdt_CENT   =  KA*EV - (CL+CLNL+Q)*CP  + Q*CT;
dxdt_PERIPH =  Q*CP - Q*CT;
dxdt_RESP   =  KIN*(1-INH1)*(1+STIM1) - KOUT*(1-INH2)*(1+STIM2)*RESP;

[CAPTURE] @annotated
CP   : Plasma concentration (mass/volume)
