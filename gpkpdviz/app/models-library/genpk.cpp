[GLOBAL]
#define IPRED (CENT/VCi)

[PARAM] @annotated
CL      :    1 : Clearance
VC      :   25 : Central volume
Q       :    0 : Intercomp clearance
VP      :   50 : Peripheral volume
KA      :    1 : Absorption rate constant
VMAX    :    0 : Max velocity
KM      :    1 : Michaelis constant
WT      :   70 : Weight [covariate,ok]
WTref   :   70 : Reference weight 
WTCL    : 0.75 : Weight effect on CL 
WTVC    :    1 : Weight effect on VC
WTQ     : 0.75 : Weight effect on Q
WTVP    :    1 : Weight effect on VP
CONT    :   70 : Continuous covariate
CONTCL  :    0 : Continuous covariate effect on CL
CONTref :  100 : Continuous covariate reference value
CAT     :    0 : Categorical covariate (0 or 1) (.)
CATCL   :    1 : Categorical covariate effect on CL when CAT==1
CATVC   :    1 : Categorical covariate effect on VC when CAT==1
CATF1   :    1 : F1 when CAT==1
F1      :    1 : Reference bioavability - EV dose
ALAG1   :    0 : Lag time - EV dose

[OMEGA] @annotated
ECL : 0.09 : ETA on CL
EVC : 0.09 : ETA on VC
EQ  : 0.09 : ETA on Q
EVP : 0.09 : ETA on VP
EKA : 0.09 : ETA on KA

[SIGMA] @annotated
EPSP : 0.01 : PROPORTIONAL ERROR
EPSA : 0.00 : ADDITIVE ERROR

[CMT] @annotated
EV     : Extravascular dosing compartment
CENT   : Central compartment
PERIPH : Peripheral compartment

[MAIN]
double CLi   = CL*pow(WT/WTref,WTCL)*pow(CONT/CONTref,CONTCL);
double VCi   = VC*pow(WT/WTref,WTVC);
double Qi    =  Q*pow(WT/WTref,WTQ );
double VPi   = VP*pow(WT/WTref,WTVP);

F_EV    = F1;
ALAG_EV = ALAG1;

if(CAT==1) {
  CLi = CLi * CATCL;
  VCi = VCi * CATVC;
  F_EV = CATF1;
}

CLi = CLi*exp(ECL);
VCi = VCi*exp(EVC);
Qi  = Qi *exp(EQ);
VPi = VPi*exp(EVP);
double KAi = KA*exp(EKA);

[ODE]
double CLNLi =  VMAX/(IPRED+KM);
dxdt_EV      = -KAi*EV;
dxdt_CENT    =  KAi*EV - (CLi+CLNLi+Qi)*CENT/VCi + Qi*PERIPH/VPi;
dxdt_PERIPH  =  Qi*(CENT/VCi - PERIPH/VPi);

[TABLE]
double DV = IPRED*(1+EPSP)+EPSA;
  
[CAPTURE] @annotated
IPRED : Concentration, no RUV (.)
DV    : Concentration, with RUV (.)
