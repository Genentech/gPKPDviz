[PROB]
Author: Jenny Nguyen, Tong Lu 2018; 
Source: mrgsolve model library

Generic two-compartment model for oral dosing with the following features:
	- Option for 2 continuous (AGE, BWT) and 2 categorical variables (SEX, RACE).
		- CL: AGE, SEX, RACE
		- V1: BWT

Comment:  
Time unit: day (generic)
Volume units: L (generic)
Validated: Yes

[CMT] @annotated
DEPOT  : Extravascular compartment
CENT   : Central compartment
PERIPH : Peripheral compartment (mass) 

[PARAM] @annotated
TVCL   :  1    : Clearance (L/day)
TVV1   : 20    : Central volume of distribution (L)
TVV2   : 10    : Peripheral volume of distribution (L)
Q      :  2    : Inter-compartmental clearnace (L/day)
TVKA   :  1    : Absorption rate constant (1/day)
TVALAG1:  0    : Lag time - EV dose
AGE    : 40    : Typical individual age (yrs)
BWT    : 70    : Typical individual weight (kg)
AGEref : 40    : Reference age (yrs)
BWTref : 70    : Reference body weight (kg)
RACE   :  0    : 1 = white, 0 = black
SEX    :  0    : 1 = male, 0 = female
BWTV1  :  1.5  : Effect of BWT on central compartment volume
AGECL  :  0.5  : Effect of AGE on CL
SEXCL  :  2    : Effect of SEX=1 (male) on CL
RACECL :  0.75 : Effect of RACE=1 (white) on CL

[OMEGA] @annotated @block
// note: if you want to enter off-diagonal elements as correlations, add "@correlation" to this block
ETA_CL :  0.200                : ETA on CL
ETA_V1 :  0      0.150         : ETA on V1

[OMEGA] @annotated
ETA_KA   :0.150                : ETA on KA
ETA_ALAG1:0.150                : ETA on ALAG1

[SIGMA] @annotated
ADD  : 0.04 : Additive error 
PROP : 0.09 : Proportional error

[GLOBAL]
#define C1   (CENT / V1) 

[MAIN]
// covariate effects on parameters
double CLCOV = pow(AGE/AGEref, AGECL) * pow(SEXCL, SEX) * pow(RACECL, RACE);
double V1COV = pow(BWT/BWTref, BWTV1);

double CL   = TVCL * CLCOV * exp(ETA_CL);
double V1   = TVV1 * V1COV * exp(ETA_V1); 
double V2   = TVV2;
double KA   = TVKA * exp(ETA_KA);  
double ALAG1= TVALAG1 * exp(ETA_ALAG1); 

double K   = CL/V1;
double K12 = Q/V1;
double K21 = Q/V2;

ALAG_DEPOT = ALAG1;

[ODE]
dxdt_DEPOT  = -KA*DEPOT;
dxdt_CENT   = KA*DEPOT - (K+K12)*CENT + K21*PERIPH;
dxdt_PERIPH = K12*CENT - K21*PERIPH;

[TABLE]
double IPRED = C1;
double DV    = IPRED*(1+PROP) + ADD;

[CAPTURE] @annotated
IPRED : Concentration without residual variability
DV    : Concentration with residual variability 
