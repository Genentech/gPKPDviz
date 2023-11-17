[PROB] 
PACLITAXEL POP-PK + FRIBERG NEUTROPENIA MODEL

Author: Jenny Nguyen, 2018
Source: PacPKPD_actural_E0.ctl
//PKPD model: neutropenia PD model Lena F 2002
//popPK model: Clin Cancer Res 2006;12:2150-2157

Time unit: hr
Volume units: L
Validated: Yes (Tong 2022)
Dosing route: infusion over 1hr or 3hr (for higher dose)
Dosing interval: weekly or every three weeks (for higher dose)
Dose unit: umol (for 80mg/m2 and 1.8m2 BSA: 80*1.8*1000/854=168.6umol; or 93.68umol/m2 * BSA)

[CMT] @annotated
// PK compartments
CENT    : Central PK compartment
PERIPH1 : Peripheral Compartment 1
PERIPH2 : Peripheral Compartment 2

// PD compartments
STEM   : Compartment representng stem cells and progenitor cells (i.e. proliferating cells)
TRANS1 : Transit compartment 1 (mature cell)
TRANS2 : Transit compartment 2 (mature cell)
TRANS3 : Transit compartment 3 (mature cell)
CIRC   : Compartment of circulating observed blood cells

[PARAM] @annotated
TVV1    :  12.8  : Volume in central compartment (L)
TVV3    : 252    : Volume in peripheral compartment (L)
TVVM_el :  37.4  : VMel (umol/hr)
TVKM_el :   0.53 : KMel (umol/L)
TVVM_tr : 169    : VMtr (umol/hr)
TVKM_tr :   0.83 : KMtr (umol/L)
TVK21   :   1.15 : (hr^-1)
TVQ     :  20.1  : (L/hr)
TVE0    :   3.78 : Baseline neutrophil count (x10^9/L)  (use the actual mean baseline in study 4629)
TVMTT   : 127    : Mean transit time (hr)
TVSLP   :   2.21 : Slope (uM^-1)
TVGAM   :   0.23 : Gamma

BSAV3   :   1.17 : covariate parameter for BSA on V3
BSAVMel :   0.842: covariate parameter for BSA on VMel
BSAVMtr :   0.911: covariate parameter for BSA on VMtr
BSAQ    :   0.724: covariate parameter for BSA on Q
GENDVMel:   1.2  : covariate parameter for GEND on VMel
GENDVMtr:   1.2  : covariate parameter for GEND on VMtr
GENDKMtr:   2.11 : covariate parameter for GEND on KMtr
GENDK21 :   0.893: covariate parameter for GEND on K21
BILIVMel:  -0.167: covariate parameter for BILI on VMel
AGEVMel :  -0.352: covariate parameter for AGE on VMel

BSAmed  :   1.8  : median value of BSA (m2)
BILImed :   7    : median value of BILI (umol/L)
AGEmed  :  56    : median value of AGE (year)

OCC     :   1    : Interoccasion variability (1 or 2)
BSA     :   1.8  : 
GEND    :   0    : 0 or 1 (0 for female and 1 for male)
BILI    :   7    :
AGE     :  56    :

[OMEGA] @annotated// eta
ETA_V1   : 0.031  : 
ETA_V3   : 0.051  : 
ETA_VMel : 0.025  : 
ETA_KMel : 0      : 
ETA_VMtr : 0.077  : 
ETA_KMtr : 0.222  : 
ETA_K21  : 0      : 
ETA_Q    : 0.135  : 
ETA_1V1  : 0.067  : 
ETA_1V3  : 0.116  :
ETA_1VM  : 0.023  : 
ETA_1K   : 0.039  : 
ETA_2V1  : 0.067  : 
ETA_2V3  : 0.116  : 
ETA_2VM  : 0.023  : 
ETA_2K   : 0.039  : 
ETA_E0   : 0.1225 : 
ETA_MTT  : 0.0324 : 
ETA_SLP  : 0.1849 : 

[SIGMA] @annotated // epsilon
EPS_1 : 0.0239 : //residual error in variance at the log scale is 0.0236 (weighted average): exp(0.0236)-1 = 0.0239
EPS_8 : 0.159: //proportional error of 39.9%

[MAIN]
// inter-occasions variability

double BOVV1 = ETA_1V1;
double BOVV3 = ETA_1V3;
double BOVVM = ETA_1VM;
double BOVK  = ETA_1K;

if (OCC == 2) {
   BOVV1 = ETA_2V1;
   BOVV3 = ETA_2V3;
   BOVVM = ETA_2VM;
   BOVK  = ETA_2K;
}

// PK parameters
double V1   = TVV1 * exp(ETA_V1+BOVV1);
double V3   = TVV3 * exp(ETA_V3+BOVV3) * pow((BSA/BSAmed), BSAV3);
double VMel = TVVM_el * exp(ETA_VMel+BOVVM) * pow(GENDVMel, GEND) * pow((BSA/BSAmed), BSAVMel) * pow((BILI/BILImed), BILIVMel) * pow((AGE/AGEmed), AGEVMel);
double KMel = TVKM_el * exp(ETA_KMel);
double VMtr = TVVM_tr * exp(ETA_VMtr) * pow(GENDVMtr, GEND) * pow((BSA/BSAmed), BSAVMtr);
double KMtr = TVKM_tr * exp(ETA_KMtr) * pow(GENDKMtr, GEND);
double K21  = TVK21 * exp(ETA_K21+BOVK) * pow(GENDK21, GEND); 
double Q    = TVQ * exp(ETA_Q) * pow((BSA/BSAmed), BSAQ);


// PD parameters
double E0 = TVE0 * exp(ETA_E0);

// initialise amount in compartments
STEM_0   = E0;
TRANS1_0 = E0;
TRANS2_0 = E0;
TRANS3_0 = E0;
CIRC_0   = E0;

// PD model parameters
double MTT   = TVMTT * exp(ETA_MTT); // unit: hrs
double SLP   = TVSLP * exp(ETA_SLP); // unit: uM^-1
double GAM   = TVGAM;                //
double KTR   = 4 / MTT; 
double KCIRC = KTR; 

[ODE] 
double CP    = CENT / V1;    // paclitaxel concentration in uM
double CP3   = PERIPH2 / V3;
 
dxdt_CENT    = -VMel*CP/(KMel+CP) - VMtr*CP/(KMtr+CP) - Q*CP + Q*CP3 + K21*PERIPH1;
dxdt_PERIPH1 = VMtr*CP/(KMtr+CP) - K21*PERIPH1;
dxdt_PERIPH2 = Q*CP - Q*CP3;

double BASE  = E0 / CIRC;
double RBD   = pow(BASE, GAM);
double DRUG  = SLP * CP;  

dxdt_STEM    = KTR * (RBD*(1 - DRUG)-1) * STEM;
dxdt_TRANS1  = KTR * (STEM - TRANS1);
dxdt_TRANS2  = KTR * (TRANS1 - TRANS2);
dxdt_TRANS3  = KTR * (TRANS2 - TRANS3);
dxdt_CIRC    = KTR*TRANS3 - KCIRC*CIRC;

[TABLE]
double IPRED_PK = (CENT/V1)*854; // 1 uM = 854 ng/ml for paclitaxel 
double IPRED_PK_log = log(IPRED_PK);
double DV_PK_log    = IPRED_PK_log + EPS_1;
double DV_PK = exp(DV_PK_log);

double IPRED_PD = CIRC;
double DV_PD = IPRED_PD + EPS_8*IPRED_PD;

[CAPTURE] @annotated
IPRED_PK : Concentration without residual variability
DV_PK    : Concentration with residual variability
IPRED_PK_log : Concentration without residual variability in nature log scale
DV_PK_log    : Concentration with residual variability in nature log scale
IPRED_PD : WBC count without residual variability
DV_PD : WBC count with residual variability
