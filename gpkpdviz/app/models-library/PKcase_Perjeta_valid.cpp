[PROB] 
Perjeta 

Author: Jenny Nguyen / Tong Lu
Source: run20d.lst

Time unit: day
Volume units: L
Validated: Yes (Tong Lu May 2020)

[PKMODEL] // option to use analytical solution for 1- and 2- cpt models
cmt = "CENT PERIPH", depot = FALSE

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

// effect of covariates on parameters 
double CLCOV = pow((LBW/48), LBWCL) * pow((ALBU/3.9), ALBCL);
double V1COV = pow((LBW/48), LBWV1);
double V2COV = pow((LBW/48), LBWV2); 

// PK parameters
double CL    = TVCL * CLCOV * exp(ETA_CL); 
double V1    = TVV1 * V1COV * exp(ETA_V1);
double Q     = TVQ;
double V2    = TVV2 * V2COV * exp(ETA_V2);

[TABLE] // NONMEM equivalent: $ERROR
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
