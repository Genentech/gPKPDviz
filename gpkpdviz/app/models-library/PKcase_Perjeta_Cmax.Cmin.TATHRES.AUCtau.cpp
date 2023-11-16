[PROB] 
Perjeta 

Author: Tong Lu
Source: run20d.lst

Time unit: day
Volume units: L
Validated: no
note: derive Cmax, Tmax, Cmin, Tmin after each dose; derive time above threshold (TATHRES);

[CMT] @annotated
CENT   : Central PK compartment
PERIPH : Peripheral Compartment
AUC    : AUC Compartment
TATHRES: Time above threshold Compartment

[GLOBAL]
double Cmax = -1;
double Tmax = -1;
double Cmin = 9999;
double Tmin = -1;
double AUCpre = 0;
double AUCtau = 0;
double THRES = 20;
double ATHRESFLAG = 0;

[PARAM] @annotated // list model parameters and covariates
TVCL  :  0.235   : Clearance (L/day), theta 1
TVV1  :  3.11    : Volume of central compartment (L), theta 2
TVQ   :  0.534   : Intercompartmental clearance (L/day), theta 3
TVV2  :  2.46    : Volume of peripheral compartment (L), theta 4

LBWCL :  0.516   : Effect of LBW on CL, theta 5
LBWV1 :  0.747   : Effect of LBW on V1, theta 6
ALBCL : -1.06    : Effect of ALBU on CL, theta 7
LBWV2 :  0.83    : Effect of LBW on V2, theta 8

LBW   : 48       : Typical individual value of LBW
ALBU  :  3.9     : Typical individual value of ALBU

[OMEGA] @annotated @block // describe between-subject variability
ETA_CL :  0.116                   : ETA on CL
ETA_V1 :  0.0239   0.0342         : ETA on V1
ETA_V2 : -0.0416   0.0179   0.211 : ETA on V2

[SIGMA] @annotated // describe residual error
ADD : 0.0328 : Additive Error (log scale)

[MAIN] // NONMEM equivalent: $PK
double TAD = self.tad();

if (NEWIND < 2 || EVID == 1) {
  Cmax = -1;
  Tmax = -1;
  Cmin = 9999;
  Tmin = -1;  
 }
if (NEWIND < 2) {
  AUCtau = 0;
  AUCpre = 0;
}	 

// effect of covariates on parameters 
double CLCOV = pow((LBW/48), LBWCL) * pow((ALBU/3.9), ALBCL);
double V1COV = pow((LBW/48), LBWV1);
double V2COV = pow((LBW/48), LBWV2); 

// PK parameters
double CL    = TVCL * CLCOV * exp(ETA_CL); 
double V1    = TVV1 * V1COV * exp(ETA_V1);
double Q     = TVQ;
double V2    = TVV2 * V2COV * exp(ETA_V2);

double K   = CL/V1;
double K12 = Q/V1;
double K21 = Q/V2;

[ODE] 
dxdt_CENT = -K12*CENT + K21*PERIPH - K*CENT;
dxdt_PERIPH = K12*CENT - K21*PERIPH;
dxdt_AUC = CENT/V1;

if(CENT/V1 > Cmax) {
  Cmax = CENT/V1;
  Tmax = SOLVERTIME;
} 

if(CENT/V1 < Cmin && TAD >=1.0/24.0) {
  Cmin = CENT/V1;
  Tmin = SOLVERTIME;
} 
//set TAD > = 1.0/24.0 to avoid picking the value during infusion (1hr infusion in this case) as Cmin

ATHRESFLAG = 0; 
if (CENT/V1 > THRES) ATHRESFLAG = 1;
dxdt_TATHRES = ATHRESFLAG;

[TABLE] // NONMEM equivalent: $ERROR
if(EVID == 1) {
 AUCtau = AUC - AUCpre;
 AUCpre = AUC;
}

double val = CENT/V1;
double IPRED = 0; 
double IPREDnormal = 0; 

if (val > 0) IPRED = log(val); 
if (val > 0) IPREDnormal = exp(IPRED);
 
double DV = IPRED + ADD;
double DVnormal = exp(DV);

[CAPTURE] @annotated
IPREDnormal : Concentration without residual variability (normal scale)
DVnormal    : Concentration with residual variability (normal scale)
IPRED : Concentration without residual variability (log scale)
DV    : Concentration with residual variability (log scale)
Cmax  : Cmax after each dose
Tmax  : Tmax after each dose
Cmin  : Cmin after each dose
Tmin  : Tmin after each dose
TAD   : TIME after each dose
AUCpre: AUC after previous dose
AUCtau: AUC after current dose
ATHRESFLAG: Above threshold flag

