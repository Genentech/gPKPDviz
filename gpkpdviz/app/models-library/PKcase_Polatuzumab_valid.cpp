[PROB] 
Pola popPK model

Author: Victor Poon, 2020
Source: 201.lst

Time unit: hr
Volume units: L
Validated: Yes (Victor June 2020)

[CMT] @annotated
CENT : central compartment for acMMAE
PERI : peripheral compartment for acMMAE
CMMAE : central compartment for MMAE
CMMAE2: peripheral compartment for MMAE

[PARAM] @annotated

BBCC:19: Baseline B cell count 10^6/L (median)
COMBO:1: 
SEXN:1: 
RRFN:1: 
RACEN:1: 0 Not Asian, 1 Asian
BHPTGRPN: 1: Baseline Hepatic Function Group 1 'Normal'; 2 'MILD1'; 3 'MILD2'; 4 'MODERATE'; 5 'SEVERE'; 9999 'missing'.
BECOG:1: Baseline ECOG
BTMBD:3031: Baseline tumor size
BALBUM:39: Baseline Albumin
BWT:77.9: Baseline Weight

TVKDES :0.0046: 1~KDES
TVCLT:0.0062 : 2~CLT
TVCLINF:0.0344 : 3~CLINF
TVV1:3.14 : 4~V1
TVV2: 3.96: 5~V2
TVQ:0.014 : 6~Q
TVVMAX:0.0204 : 7~VMAX
TVKM:0.604 : 8~KM
TVCLIMAX: 0.223: 9~CLinf decrease EMAX
T50_Mon :3.51:10~CLinf T50 (month)
TVGAM  :2.27 :11~CLing GAM
 
TVVMMAE:   83.3 :12~VMMAE
TVCLMMAE    :1.91:13~CLMMAE
TVQMMAE    :36.5:14~QMMAE
TVV2MMAE:  202   :15~V2MMAE
TVVMAXMM  :0.0305:16~VMAXMMAE
TVKSS   :0.584:17~KSS
TVFRAC1    :3.71:18~FRAC1
TVFRAC2    :2.70:19~FRAC2
ALPH_H   :0.167:20~alpha 
TVFREMAX: 0.138:21~FREMAX

WTCLF: 0.735:22~WT to CLinf
WTVQ:  0.501 :23~WT to V1 V2 Q
SEXV1: 1.2:24~SEX   on V1
ASIV1: 0.93 :25~ASIAN on V1
NAIVV1: 1.2 :26~NAIVE on V1
SEXCLINF: 1.1:27~SEX on CLinf
ALBCLINF:-0.247 :28~ALBUM to CLinf 
RTXCLINF: 0.843:29~RTX GA101 to CLinf 
BCECLINF:0.0210 :30~power BCEL1 to CLinf
TMBCLINF: 0.052 :31~TMBD to CLinf
NAIVKDES: 3.36:32~NAIVE to KDES
RTXKDES:0.936 :33~RTX GA101 to KDES
NAIVECLT: 3.53:34~NAIVE to CLT
BTMBCLT:1152 :35~BTMBD50 to CLT
BBCC_CUT:120:36~BBCC cutoff  
BCELCLT: 0.589:37~BCEL to CLT power

BWTFRAC0:-0.468 :38~BWT to FRAC0
SEXFRAC0: 0.918:39~SEX to FRAC0
NAIFRAC0:0.760 :40~NAIVE to FRAC0
RTXFRAC0:0.714 :41~RTX/OB to FRAC0
HEPFRAC0:1.18 :42~HEPA to FRAC0
ECOFRAC0: 0.904:43~ECOG0 to FRAC0
ALBFRAC0: -0.613:44~ALBUM to FRAC0

[OMEGA] @annotated

ECLT: 1.89      : 1~IIV_CLT
ECLINF: 0.037      : 2~IIV_CLINF
EV1: 0.015      : 3~IIV_V1
EV2: 0.107      : 4~IIV_V2
EQ: 0.054      : 5~IIV_Q
EVMAX: 0.462      : 6~IIV_VMAX
EFRAC0: 0.097      : 7~IIV_FRAC0
ECLMMAE: 0.115      : 8~IIV_CLMMAE
EV2MMAE: 0.042      : 9~IIV_V2MMAE

[OMEGA] @annotated @block

EERR1: 0.052       :10~IIV_ERR1
EERR2: 0.038 0.043 :11~IIV_ERR2
 
[SIGMA] @annotated

PROP1: 0.0254      : ~Err_PROP
PROP2: 0.0726      : ~Err_PROP

[MAIN]

double BCEL = 1;
if (BBCC > BBCC_CUT) BCEL=BBCC/BBCC_CUT;
double BCEL1 = 1;
if (BBCC > 1) BCEL1=BBCC;

double RTX = 0;
if (COMBO==1) RTX = 1;
double GA101 = 0;
if (COMBO==2) GA101 = 1;

double SEX = SEXN-1;

double NAIVE = 0;
if (RRFN==0) NAIVE=1;

double ASIAN = 0;
if (RACEN==1) ASIAN = 1;

double HEPA = 0;
if ((BHPTGRPN>1.5) & (BHPTGRPN!=9999)) HEPA = 1;

double ECOG0 = 0;
if (BECOG==0) ECOG0 = 1;
double ECOG2 = 0;
if (BECOG==2) ECOG2 =1;


double COVV1    = pow(SEXV1,SEX)*pow(ASIV1,ASIAN)*pow(NAIVV1,NAIVE);   
double COVCLINF = pow(SEXCLINF,SEX)*pow((BALBUM/35),ALBCLINF)*pow(RTXCLINF,(RTX+GA101))*pow(BCEL1,BCECLINF)*(1+TMBCLINF*(BTMBD/5000-1));
double COVKDES  = pow(NAIVKDES,NAIVE)*pow(RTXKDES,(RTX+GA101)); 
double COVCLT   = pow(NAIVECLT,NAIVE)*BTMBD/(BTMBCLT+BTMBD)*pow(BCEL,BCELCLT);

double KDES  = TVKDES*COVKDES;
double CLT   = TVCLT*COVCLT*exp(ECLT) ;
double CLINF = TVCLINF*pow((BWT/75),WTCLF)*COVCLINF*exp(ECLINF);   		
double V1    = TVV1*pow((BWT/75),WTVQ)*COVV1*exp(EV1);   
double V2    = TVV2*pow((BWT/75),WTVQ)*exp(EV2);
double Q     = TVQ*pow((BWT/75),WTVQ)*exp(EQ);
double VMAX  = TVVMAX*exp(EVMAX); 
double KM    = TVKM;
double CLINFEMAX= TVCLIMAX;  
double T50      = T50_Mon*24*30; 
double GAM      = TVGAM;
double T50GAM   = pow(T50,GAM);

//double S1 = V1;


double K12   = Q/V1;
double K21   = Q/V2;


double COVMMAE1= pow((BWT/75), BWTFRAC0)*pow(SEXFRAC0,SEX)*pow(NAIFRAC0,NAIVE)*pow(RTXFRAC0,(RTX+GA101))*pow(HEPFRAC0,HEPA); 
double COVMMAE = COVMMAE1*pow(ECOFRAC0,ECOG0)*pow((BALBUM/35),ALBFRAC0) ;

double FRAC0  = COVMMAE*exp(EFRAC0) ;
double VMMAE  = TVVMMAE ;
double CLMMAE = TVCLMMAE*exp(ECLMMAE);
double QMMAE  = TVQMMAE;
double V2MMAE = TVV2MMAE*exp(EV2MMAE);
double VMAXMMAE = TVVMAXMM ;
double KSS      = TVKSS;
double FRAC1    = TVFRAC1 ;
double FRAC2    = TVFRAC2 ;
double ALPH     = ALPH_H/24/30;
double FREMAX   = TVFREMAX;
double K34    = QMMAE/VMMAE;
double K43    = QMMAE/V2MMAE;
double K30    = CLMMAE/VMMAE;

[ODE] 

double FRAC     = FRAC0*(1+FREMAX*exp(-ALPH*SOLVERTIME));   
double TGAM = 0;
if (SOLVERTIME > 0) TGAM = pow(SOLVERTIME,GAM); 
double CL=CLT*exp(-KDES*SOLVERTIME)+CLINF*(1+CLINFEMAX*T50GAM/(T50GAM+TGAM));
double K10   = CL/V1;
double KINPUT = FRAC*(FRAC1*CLT*exp(-KDES*SOLVERTIME)/V1+CLINF*(1+CLINFEMAX*T50GAM/(T50GAM+TGAM))/V1+FRAC2*VMAX/(KM+CENT/V1));

dxdt_CENT= K21*PERI-K12*CENT-K10*CENT-VMAX*CENT/(KM+CENT/V1);
dxdt_PERI=-K21*PERI+K12*CENT;  
dxdt_CMMAE= KINPUT*CENT-K30*CMMAE - K34*CMMAE + K43*CMMAE2 - VMAXMMAE*CMMAE/(KSS+CMMAE/VMMAE);  
dxdt_CMMAE2=                        K34*CMMAE - K43*CMMAE2;    

[TABLE]

double ACMMAE = CENT/V1;
double MMAE   = CMMAE/VMMAE; 

double TY=ACMMAE; // Ab Conjugated
double TY6=MMAE; // For Type==6, MMAE

double DV_acMMAE=TY*(1+PROP1*exp(EERR1));
double DV_MMAE=TY6*(1+PROP2*exp(EERR2)) ; // For Type==6

[CAPTURE] @annotated
ACMMAE: Concentration without residual variability for acMMAE
MMAE: Concentration without residual variability for MMAE
DV_acMMAE: Concentration with residual variability for acMMAE
DV_MMAE : Concentration with residual variability for MMAE
