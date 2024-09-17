* Encoding: UTF-8.


GET DATA
  /TYPE=XLSX
  /FILE='C:\Users\serdar.yesildag\Desktop\VAM M&E\1Reports\FSMS\Dataset\RAW\FSMS August '+
    '22-SystemC1.xlsx'
  /SHEET=name 'FSMS Aug 22'
  /CELLRANGE=FULL
  /READNAMES=ON
  /DATATYPEMIN PERCENTAGE=95.0
  /HIDDEN IGNORE=YES.
EXECUTE.
DATASET NAME DataSet2 WINDOW=FRONT.

DATASET ACTIVATE DataSet2.
STRING HHH_Age_Cat (A16).
RECODE HHHAge (Lowest thru 24='16 to 24') (25 thru 34='25 to 34') (35 thru 44='35 to 44') (45 thru 
    54='45 to 54') (55 thru 64='55 to 64') (65 thru Highest='65 and Older') INTO HHH_Age_Cat.
VARIABLE LABELS  HHH_Age_Cat 'HHH_Age_Cat'.
EXECUTE.

FREQUENCIES VARIABLES=HHH_Age_Cat
  /ORDER=ANALYSIS.

FREQUENCIES VARIABLES=HHHSex
  /ORDER=ANALYSIS.


* HHH Gender by Region.
CTABLES
  /VLABELS VARIABLES=ADMIN1Name HHHSex DISPLAY=LABEL
  /TABLE ADMIN1Name [C] > HHHSex [COUNT F40.0]
  /CATEGORIES VARIABLES=ADMIN1Name HHHSex ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CRITERIA CILEVEL=95.


* HHH Gender by Districts.
CTABLES
  /VLABELS VARIABLES=ADMIN2Name HHHSex DISPLAY=LABEL
  /TABLE ADMIN2Name > HHHSex [COUNT F40.0, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=ADMIN2Name HHHSex ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CRITERIA CILEVEL=95.



* HHH Age Cat by Region.
CTABLES
  /VLABELS VARIABLES=ADMIN1Name HHH_Age_Cat DISPLAY=LABEL
  /TABLE ADMIN1Name > HHH_Age_Cat [C][COUNT F40.0, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=ADMIN1Name HHH_Age_Cat ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CRITERIA CILEVEL=95.

* HHH Age Cat by Districts.
CTABLES
  /VLABELS VARIABLES=ADMIN2Name HHH_Age_Cat DISPLAY=LABEL
  /TABLE ADMIN2Name [C] > HHH_Age_Cat [C][COUNT F40.0, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=ADMIN2Name HHH_Age_Cat ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CRITERIA CILEVEL=95.


* Mean HHH Age by Region.
CTABLES
  /VLABELS VARIABLES=ADMIN1Name HHHAge DISPLAY=LABEL
  /TABLE ADMIN1Name [C] > HHHAge [MEAN]
  /CATEGORIES VARIABLES=ADMIN1Name ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CRITERIA CILEVEL=95.

* Mean HHH Age by District.
CTABLES
  /VLABELS VARIABLES=ADMIN2Name HHHAge DISPLAY=LABEL
  /TABLE ADMIN2Name [C] > HHHAge [MEAN]
  /CATEGORIES VARIABLES=ADMIN2Name ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CRITERIA CILEVEL=95.

*HH Total SIze.
COMPUTE HH_Size_Total=HHSize01M + HHSize01F+HHSize24M+HHSize24F+HHSize511M+HHSize511F+HHSize1217M+
    HHSize1217F+HHSize1859M+HHSize1859F+HHSize60AboveM+HHSize60AboveF.
EXECUTE.

* Custom Tables.
CTABLES
  /VLABELS VARIABLES=ADMIN1Name HH_Size_Total DISPLAY=LABEL
  /TABLE ADMIN1Name [C] > HH_Size_Total [MEAN, COUNT F40.0]
  /CATEGORIES VARIABLES=ADMIN1Name ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CRITERIA CILEVEL=95.

*HH Total SIze Cat.
STRING HH_Size_Cat (A22).
RECODE HH_Size_Total (Lowest thru 4='1 to 4 ') (5 thru 8='5 to 8') (9 thru 12='9 to 12') (13 thru 
    Highest='13 and higher') INTO HH_Size_Cat.
VARIABLE LABELS  HH_Size_Cat 'HH_Size_Cat'.
EXECUTE.

*Mean HH Size by Region.
CTABLES
  /VLABELS VARIABLES=ADMIN1Name HH_Size_Total DISPLAY=LABEL
  /TABLE ADMIN1Name [C] > HH_Size_Total [S][MEAN, COUNT F40.0]
  /CATEGORIES VARIABLES=ADMIN1Name ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CRITERIA CILEVEL=95.

*Mean HH Size by District.
CTABLES
  /VLABELS VARIABLES=ADMIN2Name HH_Size_Total DISPLAY=LABEL
  /TABLE ADMIN2Name [C] > HH_Size_Total [S][MEAN, COUNT F40.0]
  /CATEGORIES VARIABLES=ADMIN2Name ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CRITERIA CILEVEL=95.

*Food Consumption Score.
Compute FCS = sum(FCSStap*2, FCSPulse*3, FCSDairy*4, FCSPr*4, FCSVeg*1, FCSFruit*1, FCSFat*0.5, FCSSugar*0.5).
Variable labels FCS "Food Consumption Score".
EXECUTE.

*FCS Cat.
Recode FCS (lowest thru 21 =1) (21.5 thru 35 =2) (35.5 thru highest =3) into FCSCat21.
Variable labels FCSCat21 "FCS Categories".
EXECUTE.

*FCS Cat21.
Value labels FCSCat21 1.00 'Poor' 2.00 'Borderline' 3.00 'Acceptable '.
EXECUTE.




* FCS Cat by Region.
CTABLES
  /VLABELS VARIABLES=ADMIN1Name FCSCat21 DISPLAY=LABEL
  /TABLE ADMIN1Name [C] > FCSCat21 [COUNT F40.0, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=ADMIN1Name ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=FCSCat21 ORDER=A KEY=VALUE EMPTY=INCLUDE
  /CRITERIA CILEVEL=95.

* FCS Cat by District.
CTABLES
  /VLABELS VARIABLES=ADMIN2Name FCSCat21 DISPLAY=LABEL
  /TABLE ADMIN2Name [C] > FCSCat21 [COUNT F40.0, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=ADMIN2Name ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=FCSCat21 ORDER=A KEY=VALUE EMPTY=INCLUDE
  /CRITERIA CILEVEL=95.

* FCS Cat by HHHGender.
CTABLES
  /VLABELS VARIABLES=HHHSex FCSCat21 DISPLAY=LABEL
  /TABLE HHHSex > FCSCat21 [C][COUNT F40.0, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=HHHSex ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CATEGORIES VARIABLES=FCSCat21 ORDER=A KEY=VALUE EMPTY=INCLUDE
  /CRITERIA CILEVEL=95.

* FCS Cat by HHsize.
CTABLES
  /VLABELS VARIABLES=HH_Size_Cat FCSCat21 DISPLAY=LABEL
  /TABLE HH_Size_Cat > FCSCat21 [C][COUNT F40.0, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=HH_Size_Cat ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CATEGORIES VARIABLES=FCSCat21 ORDER=A KEY=VALUE EMPTY=INCLUDE
  /CRITERIA CILEVEL=95.

*FGVita.
Compute FGVitA = sum(FCSDairy, FCSPrMeatO, FCSPrEgg, FCSVegOrg, FCSVegGre, FCSFruitOrg).
Variable labels FGVitA 'Consumption of vitamin A-rich foods'.
EXECUTE.

*FGVita Groups.
Recode FGVitA (0=1) (1 thru 6=2) (7 thru 42=3) into FGVitACat.
Variable labels FGVitACat 'Consumption of vitamin A-rich foods'.
EXECUTE.

*FGProtein.
Compute FGProtein = sum(FCSPulse, FCSDairy, FCSPrMeatF, FCSPrMeatO, FCSPrFish, FCSPrEgg).
Variable labels FGProtein 'Consumption of protein-rich foods'.
EXECUTE.

*FGProtein Groups.
Recode FGProtein (0=1) (1 thru 6=2) (7 thru 42=3) into FGProteinCat.
Variable labels FGProteinCat 'Consumption of protein-rich foods'.
EXECUTE.

*FGHiron.
Compute FGHIron = sum(FCSPrMeatF, FCSPrMeatO, FCSPrFish).
Variable labels FGHIron 'Consumption of hem iron-rich foods'.
EXECUTE.

*FGHiron Groups.
Recode FGHIron (0=1) (1 thru 6=2) (7 thru 42=3) into FGHIronCat.
Variable labels FGHIronCat 'Consumption of hem iron-rich foods'.
EXECUTE.

*Define variables labels and properties for " FGVitACat FGProteinCat FGHIronCat ".
Value labels FGVitACat FGProteinCat FGHIronCat
1.00 '0 days' 2.00 '1-6 days' 3.00 '7 days'.
EXECUTE.

***HDDS***.
*Convert to values numeric.

DATASET ACTIVATE DataSet2.
RECODE HDDSStapCer HDDSStapRoot HDDSPulse HDDSDairy HDDSPrMeatF HDDSPrMeatO HDDSPrFish HDDSPrEgg 
    HDDSVegOrg HDDSVegGre HDDSVegOth HDDSFruitOrg HDDSFruitOth HDDSFat HDDSSugar HDDSCond ('Yes'='1') 
    (ELSE='0').
EXECUTE.

*Calculate HDDVegSum.
Compute HDDSVeg = sum(HDDSVegGre, HDDSVegOth, HDDSVegOrg).
Variable labels HDDVeg "HDDVeg".
EXECUTE.

*Modify HDDVegSum  to MAX 1 value.
RECODE HDDSVegSum (0=0) (ELSE=1) INTO HHDSVeg.
VARIABLE LABELS  HHDSVeg 'HHDSVeg'.
EXECUTE.


*Calculate HDDPrMeatSum.
Compute HDDSPrMeatSum = sum(HDDSPrMeatO, HDDSPrMeatF).
Variable labels HDDSPrMeat "HDDSPrMeat".
EXECUTE.

*Modify HDDPrMeatSum to MAX 1 value.
RECODE HDDSPrMeatSum (0=0) (ELSE=1) INTO HDDSPrMeat.
VARIABLE LABELS  HDDSPrMeat 'HDDSPrMeat'.
EXECUTE.


*Calculate HDDSFruitSum.
Compute HDDSFruitSum = sum(HDDSFruitOrg, HDDSFruitOth).
Variable labels HDDSFruitSum "HDDSFruitSum".
EXECUTE.

*Modify HDDSFruitSum to MAX 1 value.
RECODE HDDSFruitSum (0=0) (ELSE=1) INTO HDDSFruit.
VARIABLE LABELS  HDDSFruit 'HDDSFruit'.
EXECUTE.

*HDD Score.
COMPUTE HDDS=HDDSStapCer+HDDSStapRoot+HHDSVeg+HDDSFruit+HDDSPrMeat+HDDSPrEgg+HDDSPrFish+HDDSPulse+
    HDDSDairy+HDDSFat+HDDSSugar+HDDSCond.
VARIABLE LABELS  HDDS 'HDDS'.
EXECUTE.

*HDD Score Groups.
Recode HDDS (lowest thru 2=1) (3 thru 4=2) (5=3) (6 thru highest = 4) into HDDSCat_IPC.
Value labels HDDSCat_IPC 1 '0 - 2 food groups (phase 4 to 5)' 2 '3 - 4 food groups (phase 3)' 3 '5 food groups (phase 2)' 4 '6-12 food groups (phase 1)'.
EXECUTE.

*RCSi.
Compute rCSI = sum(rCSILessQlty*1,rCSIBorrow*2,rCSIMealNb*1,rCSIMealSize*1,rCSIMealAdult*3).
Variable labels rCSI 'Reduced coping strategies index (rCSI)'.
EXECUTE.

*LCSi.
DATASET ACTIVATE DataSet1.

*Recode LCSi Value labels.
RECODE Lcs_em_IllegalAct Lcs_em_Begged Lcs_crisis_HealthEdu Lcs_crisis_ProdAssets 
    Lcs_stress_CrdtFood Lcs_stress_Saving Lcs_stress_DomAsset LcsR_em_FemAnimal LcsR_crisis_Seed 
    LcsR_stress_Animals LcsEN_em_IllegalAct LcsEN_em_Begged LcsEN_crisis_HealthEdu 
    LcsEN_crisis_ProdAssets LcsEN_stress_BorrowCash LcsEN_stress_CrdtFood LcsEN_stress_Saving 
    LcsEN_stress_DomAsset ('Yes'='30') ('No, because I did not need to'='10') ('No, because I '+
    'already sold those assets or have engaged in this activity within the last 12 months and '+
    'cannot continue to do it'='20') ("Not applicable (don't have children/ these assets)"='9999').
EXECUTE.

*LCSi Stress.
do if (Lcs_stress_DomAsset = 20) | (Lcs_stress_DomAsset = 30) | (Lcs_stress_CrdtFood = 20) | (Lcs_stress_CrdtFood = 30) | (Lcs_stress_saving =20) | (Lcs_stress_saving =30) | (Lcs_stress_BorrowCash =20) | (Lcs_stress_BorrowCash =30).
compute stress_coping =1.
else.
compute stress_coping =0.
end if.
EXECUTE.

*LCSi crisis.
do if (Lcs_crisis_ProdAssets = 20) | (Lcs_crisis_ProdAssets = 30) | (Lcs_crisis_HealthEdu = 20) | (Lcs_crisis_HealthEdu = 30) | (Lcs_crisis_Seed =20) | (Lcs_crisis_Seed =30).
compute crisis_coping =1.
else.
compute crisis_coping =0.
end if.
EXECUTE.

*LCSi emergency.
do if (Lcs_em_IllegalAct = 20) | (Lcs_em_IllegalAct = 30) | (Lcs_em_Begged = 20) | (Lcs_em_Begged = 30) | (Lcs_em_FemAnimal =20) | (Lcs_em_FemAnimal =30).
compute emergency_coping =1.
else.
compute emergency_coping =0.
end if.
EXECUTE.


variable labels stress_coping 'Did the HH engage in stress coping strategies?'.
variable labels crisis_coping 'Did the HH engage in crisis coping strategies?'.
variable labels emergency_coping  'Did the HH engage in emergency coping strategies?'.

*** recode variables to compute one variable with coping behavior 

recode  stress_coping (0=0) (1=2).
recode  crisis_coping (0=0) (1=3).
recode  emergency_coping (0=0) (1=4).

COMPUTE Max_coping_behaviour=MAX(stress_coping,  crisis_coping,  emergency_coping).
RECODE Max_coping_behaviour (0=1).

Value labels Max_coping_behaviour 1 'HH not adopting coping strategies' 2 'Stress coping strategies ' 3 'Crisis coping strategies ' 4 'Emergencies coping strategies'.

Variable Labels Max_coping_behaviour 'Summary of asset depletion'.
EXECUTE.

***Food Expenditure Share ***.


*HHExpFood_Cash_1M.


DATASET ACTIVATE DataSet1.
RECODE HHExpFCer_MN_1M HHExpFTub_MN_1M HHExpFPuls_MN_1M HHExpFVeg_MN_1M HHExpFFrt_MN_1M 
    HHExpFAnimMeat_MN_1M HHExpFAnimFish_MN_1M HHExpFFats_MN_1M HHExpFDairy_MN_1M HHExpFAnimEgg_MN_1M 
    HHExpFSgr_MN_1M HHExpFCond_MN_1M HHExpFBeverage_MN_1M HHExpFOut_MN_1M (MISSING=0).
EXECUTE.

Compute HHExpFood_MN_1Mt =sum(HHExpFCer_MN_1M, HHExpFTub_MN_1M, 
HHExpFPuls_MN_1M,  HHExpFVeg_MN_1M, HHExpFFrt_MN_1M, HHExpFAnimMeat_MN_1M, 
HHExpFAnimFish_MN_1M, HHExpFFats_MN_1M, HHExpFDairy_MN_1M, 
HHExpFAnimEgg_MN_1M, HHExpFSgr_MN_1M, HHExpFCond_MN_1M, HHExpFBeverage_MN_1M, 
HHExpFOut_MN_1M).
EXECUTE. 

***HHExp_Food_CRD_1M.
*Recode missing values into 0.

RECODE HHExpFCer_CRD_1M HHExpFTub_CRD_1M HHExpFPuls_CRD_1M HHExpFVeg_CRD_1M HHExpFFrt_CRD_1M 
    HHExpFAnimMeat_CRD_1M HHExpFAnimFish_CRD_1M HHExpFFats_CRD_1M HHExpFDairy_CRD_1M 
    HHExpFAnimEgg_CRD_1M HHExpFSgr_CRD_1M HHExpFCond_CRD_1M HHExpFBeverage_CRD_1M HHExpFOut_CRD_1M 
    (MISSING=0).
EXECUTE.


Compute HHExp_Food_CRD_1M =sum(HHExpFCer_CRD_1M, HHExpFTub_CRD_1M, 
HHExpFPuls_CRD_1M,  HHExpFVeg_CRD_1M, HHExpFFrt_CRD_1M, HHExpFAnimMeat_CRD_1M, 
HHExpFAnimFish_CRD_1M, HHExpFFats_CRD_1M, HHExpFDairy_CRD_1M, 
HHExpFAnimEgg_CRD_1M, HHExpFSgr_CRD_1M, HHExpFCond_CRD_1M, HHExpFBeverage_CRD_1M, 
HHExpFOut_CRD_1M).
EXECUTE. 

***HHExp_Food_GiftAid_1M.
*Recode missing values into 0.

RECODE HHExpFCer_GiftAid_1M HHExpFTub_GiftAid_1M HHExpFPuls_GiftAid_1M HHExpFVeg_GiftAid_1M HHExpFFrt_GiftAid_1M HHExpFAnimMeat_GiftAid_1M HHExpFAnimFish_GiftAid_1M HHExpFFats_GiftAid_1M HHExpFDairy_GiftAid_1M 
HHExpFAnimEgg_GiftAid_1M HHExpFSgr_GiftAid_1M HHExpFCond_GiftAid_1M HHExpFBeverage_GiftAid_1M HHExpFOut_GiftAid_1M 
(MISSING=0).
EXECUTE.

Compute HHExp_Food_GiftAid_1M =sum(HHExpFCer_GiftAid_1M, HHExpFTub_GiftAid_1M, 
HHExpFPuls_GiftAid_1M, HHExpFVeg_GiftAid_1M, HHExpFFrt_GiftAid_1M, HHExpFAnimMeat_GiftAid_1M, 
HHExpFAnimFish_GiftAid_1M, HHExpFFats_GiftAid_1M, HHExpFDairy_GiftAid_1M, 
HHExpFAnimEgg_GiftAid_1M, HHExpFSgr_GiftAid_1M, HHExpFCond_GiftAid_1M, HHExpFBeverage_GiftAid_1M, 
HHExpFOut_GiftAid_1M).
EXECUTE. 

***HHExp_Food_Own_1M.
*Recode missing values into 0.

RECODE HHExpFCer_Own_1M HHExpFTub_Own_1M HHExpFPuls_Own_1M  HHExpFVeg_Own_1M HHExpFFrt_Own_1M HHExpFAnimMeat_Own_1M HHExpFAnimFish_Own_1M HHExpFFats_Own_1M HHExpFDairy_Own_1M 
HHExpFAnimEgg_Own_1M HHExpFSgr_Own_1M HHExpFCond_Own_1M HHExpFBeverage_Own_1M HHExpFOut_Own_1M
(MISSING=0).
EXECUTE.


Compute HHExp_Food_Own_1M =sum(HHExpFCer_Own_1M, HHExpFTub_Own_1M, 
HHExpFPuls_Own_1M,  HHExpFVeg_Own_1M, HHExpFFrt_Own_1M, HHExpFAnimMeat_Own_1M, 
HHExpFAnimFish_Own_1M, HHExpFFats_Own_1M, HHExpFDairy_Own_1M, 
HHExpFAnimEgg_Own_1M, HHExpFSgr_Own_1M, HHExpFCond_Own_1M, HHExpFBeverage_Own_1M, 
HHExpFOut_Own_1M).
EXECUTE.


*Food Expenditure variable names.
Variable labels 
HHExpFood_MN_1M   'Total food expenditure on cash'
HHExp_Food_CRD_1M    'Total food expenditure on credit'
HHExp_Food_GiftAid_1M    'Total food expenditure value from assistance'
HHExp_Food_Own_1M    'Total food expenditure value from own production'.
Execute.

**Total Monthly HH Food Expenditure.
Compute HHExpFood_1M=sum( HHExpFood_MN_1M, HHExp_Food_CRD_1M, HHExp_Food_Own_1M, HHExp_Food_GiftAid_1M).
EXECUTE.


**NFT Expenditure Share.

**NFT Cash Expenditure Share.
*Recode missing values into 0.

RECODE HHExpNFRent_MN_6M HHExpNFMedServ_MN_6M HHExpNFMedGood_MN_6M HHExpNFCloth_MN_6M HHExpNFEduFee_MN_6M HHExpNFEduGood_MN_6M 
HHExpNFSoft_MN_6M HHExpNFSav_MN_6M HHExpNFInsurance_MN_6M HHExpNFDebt_MN_6M
(MISSING=0).
EXECUTE.


Compute HHExpNFTotal_MN_6M=sum(HHExpNFRent_MN_6M,HHExpNFMedServ_MN_6M, 
HHExpNFMedGood_MN_6M, HHExpNFCloth_MN_6M, HHExpNFEduFee_MN_6M, HHExpNFEduGood_MN_6M,
HHExpNFSoft_MN_6M, HHExpNFSav_MN_6M, HHExpNFInsurance_MN_6M, HHExpNFDebt_MN_6M).
EXECUTE.

RECODE HHExpNFAlcTobac_MN_6M HHExpNFHyg_MN_6M HHExpNFTransp_MN_6M HHExpNFWat_MN_6M HHExpNFDwelServ_MN_6M 
HHExpNFElec_MN_6M HHExpNFEnerg_MN_6M HHExpNFPhone_MN_6M
(MISSING=0).
EXECUTE.

Compute HHExpNFTotal_MN_30D=(sum(HHExpNFAlcTobac_MN_6M, HHExpNFHyg_MN_6M, HHExpNFTransp_MN_6M, HHExpNFWat_MN_6M, HHExpNFDwelServ_MN_6M, 
HHExpNFElec_MN_6M, HHExpNFEnerg_MN_6M, HHExpNFPhone_MN_6M)/6).
Execute.

**Monthly NFT Cash Expenditure Share.
Compute HHExpNFTotal_MN_1M=(HHExpNFTotal_MN_30D+(HHExpNFTotal_MN_6M/6)).
Execute.

**NFT Credit Expenditure Share.
*Recode missing values into 0.

RECODE HHExpNFRent_CRD_6M HHExpNFMedServ_CRD_6M
HHExpNFMedGood_CRD_6M HHExpNFCloth_CRD_6M HHExpNFEduFee_CRD_6M HHExpNFEduGood_CRD_6M
HHExpNFSoft_CRD_6M HHExpNFInsurance_CRD_6M
(MISSING=0).
EXECUTE.

Compute HHExpNFTotal_CRD_6M=sum(HHExpNFRent_CRD_6M, HHExpNFMedServ_CRD_6M, HHExpNFMedGood_CRD_6M, HHExpNFCloth_CRD_6M, 
HHExpNFEduFee_CRD_6M, HHExpNFEduGood_CRD_6M, HHExpNFSoft_CRD_6M, HHExpNFInsurance_CRD_6M).
EXECUTE.

RECODE HHExpNFAlcTobac_CRD_6M HHExpNFHyg_CRD_6M HHExpNFTransp_CRD_6M HHExpNFWat_CRD_6M HHExpNFDwelServ_CRD_6M 
HHExpNFElec_CRD_6M HHExpNFEnerg_CRD_6M HHExpNFPhone_CRD_6M
(MISSING=0).
EXECUTE.

Compute HHExpNFTotal_CRD_30D=(sum(HHExpNFAlcTobac_CRD_6M, HHExpNFHyg_CRD_6M, HHExpNFTransp_CRD_6M, HHExpNFWat_CRD_6M, HHExpNFDwelServ_CRD_6M, 
HHExpNFElec_CRD_6M, HHExpNFEnerg_CRD_6M, HHExpNFPhone_CRD_6M)/6).
Execute.

****REDO Below****

**Monthly NFT Credit Expenditure Share.
Compute HHExpNFTotal_CRD_1M=(HHExpNFTotal_CRD_30D+(HHExpNFTotal_CRD_6M/6)).
Execute.

**NFT Gift Expenditure Share.
RECODE HHExpNFRent_GiftAid_6M HHExpNFMedServ_GiftAid_6M HHExpNFMedGood_GiftAid_6M HHExpNFCloth_GiftAid_6M HHExpNFEduFee_GiftAid_6M HHExpNFEduGood_GiftAid_6M HHExpNFSoft_GiftAid_6M 
HHExpNFInsurance_GiftAid_6M 
(MISSING=0).
EXECUTE.

Compute HHExpNFTotal_GiftAid_6M=sum(HHExpNFRent_GiftAid_6M,HHExpNFMedServ_GiftAid_6M,
    HHExpNFMedGood_GiftAid_6M, HHExpNFCloth_GiftAid_6M, HHExpNFEduFee_GiftAid_6M, HHExpNFEduGood_GiftAid_6M,
    HHExpNFSoft_GiftAid_6M, HHExpNFInsurance_GiftAid_6M).
EXECUTE.

RECODE HHExpNFAlcTobac_GiftAid_6M HHExpNFHyg_GiftAid_6M HHExpNFTransp_GiftAid_6M HHExpNFWat_GiftAid_6M HHExpNFDwelServ_GiftAid_6M 
HHExpNFElec_GiftAid_6M HHExpNFEnerg_GiftAid_6M HHExpNFPhone_GiftAid_6M
(MISSING=0).
EXECUTE.

Compute HHExpNFTotal_GiftAid_30D=(sum(HHExpNFAlcTobac_GiftAid_6M, HHExpNFHyg_GiftAid_6M, HHExpNFTransp_GiftAid_6M, HHExpNFWat_GiftAid_6M, HHExpNFDwelServ_GiftAid_6M, 
HHExpNFElec_GiftAid_6M, HHExpNFEnerg_GiftAid_6M, HHExpNFPhone_GiftAid_6M)/6).
Execute.


**Monthly NFT Gift Expenditure Share.
Compute HHExpNFTotal_GiftAid_1M=(HHExpNFTotal_GiftAid_30D+(HHExpNFTotal_GiftAid_6M/6)).
Execute.


Variable labels HHExpNFTotal_MN_1M 'Total non-food exp on cash'.
Variable labels HHExpNFTotal_CRD_1M 'Total non-food exp on credit'.
Variable labels HHExpNFTotal_GiftAid_1M 'Total non-food exp from gift aid'.
execute.

**Total Monthly HHNFT Expenditure.
COMPUTE HHExpNFTotal_1M=HHExpNFTotal_MN_1M+HHExpNFTotal_CRD_1M+HHExpNFTotal_GiftAid_1M.
EXECUTE.


***HH FES.
Compute FES= HHExpFood_1M/SUM(HHExpFood_1M, HHExpNFTotal_1M).
EXECUTE.

Variable labels FES 'Household food expenditure share' 
EXECUTE.

Recode FES (Lowest thru .4999999=1) (.50 thru .64999999=2) (.65 thru .74999999=3) (.75 thru Highest=4) 
    into Foodexp_4pt.
Variable labels Foodexp_4pt 'Food expenditure share categories'.
EXECUTE.


DATASET ACTIVATE DataSet1.
* FES table.
CTABLES
  /VLABELS VARIABLES=Foodexp_4pt DISPLAY=LABEL
  /TABLE BY Foodexp_4pt [COUNT F40.0, ROWPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=Foodexp_4pt ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CRITERIA CILEVEL=95.


* LCSI Table.
CTABLES
  /VLABELS VARIABLES=Max_coping_behaviour DISPLAY=LABEL
  /TABLE Max_coping_behaviour [C][COUNT F40.0, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=Max_coping_behaviour ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES 
    POSITION=AFTER
  /CRITERIA CILEVEL=95.

*HDDs
* Custom Tables.
CTABLES
  /VLABELS VARIABLES=HDDSCat_IPC DISPLAY=LABEL
  /TABLE HDDSCat_IPC [COUNT F40.0, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=HDDSCat_IPC ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CRITERIA CILEVEL=95.


*FCS.
CTABLES
  /VLABELS VARIABLES=FCSCat21 DISPLAY=LABEL
  /TABLE FCSCat21 [COUNT F40.0, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=FCSCat21 ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CRITERIA CILEVEL=95.

**FCS 21 4 Cat.
Recode FCSCat21 (1=4) (2=3) (3=1) INTO FCS_4pt. 
Variable labels FCS_4pt '4pt FCG'.
EXECUTE.

Frequencies VARIABLES=FCS_4pt /ORDER=ANALYSIS. 
Value labels FCS_4pt 1.00 'Acceptable' 3.00 'Borderline' 4.00 'Poor'. 
EXECUTE.

Do if (rCSI  >= 4).
Recode FCS_4pt (1=2).
End if.
EXECUTE.

Value labels FCS_4pt 1.00 'Acceptable' 2.00 ' Acceptable and rCSI>4' 3.00 'Borderline' 4.00 'Poor'. 
EXECUTE.

Frequencies FCS_4pt.

*FCS 28 4 Cat.

Recode FCS (lowest thru 28 =1) (28.5 thru 42 =2) (42.5 thru highest =3) into FCSCat28.
Variable labels FCSCat28 ‘FCS Categories’.
EXECUTE.

Value labels FCSCat28 1.00 'Poor' 2.00 'Borderline' 3.00 'Acceptable '.
EXECUTE.

Recode FCSCat28 (1=4) (2=3) (3=1) INTO FCS_4pt28. 
Variable labels FCS_4pt28 '4pt FCG'.
EXECUTE.

Frequencies VARIABLES=FCS_4pt28 /ORDER=ANALYSIS. 
Value labels FCS_4pt28 1.00 'Acceptable' 3.00 'Borderline' 4.00 'Poor'. 
EXECUTE.

Do if (rCSI  >= 4).
Recode FCS_4pt28 (1=2).
End if.
EXECUTE.


Value labels FCS_4pt28 1.00 'Acceptable' 2.00 'Acceptable and rCSI>4' 3.00 'Borderline' 4.00 'Poor'. 
EXECUTE.

Frequencies FCS_4pt28.

*--------------------------------------------------------------FCS28-----------------------------------------------------------------------------------------------------------------.

* Custom Tables.
CTABLES
  /VLABELS VARIABLES=ADMIN2Name FCS_4pt28 DISPLAY=LABEL
  /TABLE ADMIN2Name [C] BY FCS_4pt28 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=ADMIN2Name ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=FCS_4pt28 ORDER=A KEY=VALUE EMPTY=INCLUDE
  /CRITERIA CILEVEL=95.


* Custom Tables.
CTABLES
  /VLABELS VARIABLES=HHHSex FCS_4pt28 DISPLAY=LABEL
  /TABLE HHHSex BY FCS_4pt28 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=HHHSex ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CATEGORIES VARIABLES=FCS_4pt28 ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CRITERIA CILEVEL=95.


* Custom Tables.
CTABLES
  /VLABELS VARIABLES=HHH_Age_Cat FCS_4pt28 DISPLAY=LABEL
  /TABLE HHH_Age_Cat [C] BY FCS_4pt28 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=HHH_Age_Cat ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CATEGORIES VARIABLES=FCS_4pt28 ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CRITERIA CILEVEL=95.


* Custom Tables.
CTABLES
  /VLABELS VARIABLES=HH_Size_Cat FCS_4pt28 DISPLAY=LABEL
  /TABLE HH_Size_Cat BY FCS_4pt28 [C][COUNT F40.0, ROWPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=HH_Size_Cat ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CATEGORIES VARIABLES=FCS_4pt28 ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CRITERIA CILEVEL=95.

*--------------------------------------------------------------FCS21-----------------------------------------------------------------------------------------------------------------.

* Custom Tables.
CTABLES
  /VLABELS VARIABLES=ADMIN2Name FCS_4pt DISPLAY=LABEL
  /TABLE ADMIN2Name [C] BY FCS_4pt [COUNT F40.0, ROWPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=ADMIN2Name ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=FCS_4pt ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CRITERIA CILEVEL=95.

CTABLES
  /VLABELS VARIABLES=HHHSex FCS_4pt DISPLAY=LABEL
  /TABLE HHHSex [C] BY FCS_4pt [COUNT F40.0, ROWPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=HHHSex ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=FCS_4pt ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CRITERIA CILEVEL=95.

CTABLES
  /VLABELS VARIABLES=HHH_Age_Cat FCS_4pt DISPLAY=LABEL
  /TABLE HHH_Age_Cat [C] BY FCS_4pt [COUNT F40.0, ROWPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=HHH_Age_Cat ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=FCS_4pt ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CRITERIA CILEVEL=95.


CTABLES
  /VLABELS VARIABLES=HH_Size_Cat FCS_4pt DISPLAY=LABEL
  /TABLE HH_Size_Cat [C] BY FCS_4pt [COUNT F40.0, ROWPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=HH_Size_Cat ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=FCS_4pt ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES POSITION=AFTER
  /CRITERIA CILEVEL=95.

*--------------------------------------------------------------HDDS-----------------------------------------------------------------------------------------------------------------.

DATASET ACTIVATE DataSet1.
DESCRIPTIVES VARIABLES=HDDS
  /STATISTICS=MEAN STDDEV MIN MAX.

MEANS TABLES=HDDS BY ADMIN2Name
  /CELLS=MEAN COUNT STDDEV.

MEANS TABLES=HDDS BY HHHSex
  /CELLS=MEAN COUNT STDDEV.

MEANS TABLES=HDDS BY HHH_Age_Cat
  /CELLS=MEAN COUNT STDDEV.

MEANS TABLES=HDDS BY HH_Size_Cat
  /CELLS=MEAN COUNT STDDEV.

*-------------------------------------------------------------rCSi-----------------------------------------------------------------------------------------------------------------.
DESCRIPTIVES VARIABLES=RCSi
  /STATISTICS=MEAN STDDEV MIN MAX.

MEANS TABLES=RCSi BY ADMIN2Name
  /CELLS=MEAN COUNT STDDEV.

MEANS TABLES=RCSi BY HHHSex
  /CELLS=MEAN COUNT STDDEV.

MEANS TABLES=RCSi BY HHH_Age_Cat
  /CELLS=MEAN COUNT STDDEV.

MEANS TABLES=RCSi BY HH_Size_Cat
  /CELLS=MEAN COUNT STDDEV.
*--------------------------------------------------------------LCSi-----------------------------------------------------------------------------------------------------------------.

CTABLES
  /VLABELS VARIABLES=Max_coping_behaviour DISPLAY=LABEL
  /TABLE Max_coping_behaviour [C][COUNT F40.0, COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=Max_coping_behaviour ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES 
    POSITION=AFTER
  /CRITERIA CILEVEL=95.


* Custom Tables.
CTABLES
  /VLABELS VARIABLES=ADMIN2Name Max_coping_behaviour DISPLAY=LABEL
  /TABLE ADMIN2Name [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] BY Max_coping_behaviour
  /CATEGORIES VARIABLES=ADMIN2Name ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=Max_coping_behaviour ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES 
    POSITION=AFTER
  /CRITERIA CILEVEL=95.

CTABLES
  /VLABELS VARIABLES=HHHSex Max_coping_behaviour DISPLAY=LABEL
  /TABLE HHHSex [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] BY Max_coping_behaviour
  /CATEGORIES VARIABLES=HHHSex ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=Max_coping_behaviour ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES 
    POSITION=AFTER
  /CRITERIA CILEVEL=95.

CTABLES
  /VLABELS VARIABLES=HHH_Age_Cat Max_coping_behaviour DISPLAY=LABEL
  /TABLE HHH_Age_Cat [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] BY Max_coping_behaviour
  /CATEGORIES VARIABLES=HHH_Age_Cat ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=Max_coping_behaviour ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES 
    POSITION=AFTER
  /CRITERIA CILEVEL=95.

CTABLES
  /VLABELS VARIABLES=HH_Size_Cat Max_coping_behaviour DISPLAY=LABEL
  /TABLE HH_Size_Cat [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] BY Max_coping_behaviour
  /CATEGORIES VARIABLES=HH_Size_Cat ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES POSITION=AFTER
  /CATEGORIES VARIABLES=Max_coping_behaviour ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES 
    POSITION=AFTER
  /CRITERIA CILEVEL=95.

*-------------------------------------------------------------FES-----------------------------------------------------------------------------------------------------------------.


CTABLES
  /VLABELS VARIABLES=Foodexp_4pt DISPLAY=LABEL
  /TABLE BY Foodexp_4pt [COUNT F40.0, ROWPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=Foodexp_4pt ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CRITERIA CILEVEL=95.

* Custom Tables.
CTABLES
  /VLABELS VARIABLES=ADMIN2Name Foodexp_4pt DISPLAY=LABEL
  /TABLE ADMIN2Name [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] BY Foodexp_4pt
  /CATEGORIES VARIABLES=ADMIN2Name Foodexp_4pt ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES 
    POSITION=AFTER
  /CRITERIA CILEVEL=95.

CTABLES
  /VLABELS VARIABLES=HHHSex Foodexp_4pt DISPLAY=LABEL
  /TABLE HHHSex [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] BY Foodexp_4pt
  /CATEGORIES VARIABLES=HHHSex Foodexp_4pt ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES 
    POSITION=AFTER
  /CRITERIA CILEVEL=95.  

CTABLES
  /VLABELS VARIABLES=HHH_Age_Cat Foodexp_4pt DISPLAY=LABEL
  /TABLE HHH_Age_Cat [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] BY Foodexp_4pt
  /CATEGORIES VARIABLES=HHH_Age_Cat Foodexp_4pt ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES 
    POSITION=AFTER
  /CRITERIA CILEVEL=95.  

CTABLES
  /VLABELS VARIABLES=HH_Size_Cat Foodexp_4pt DISPLAY=LABEL
  /TABLE HH_Size_Cat [C][COUNT F40.0, ROWPCT.COUNT PCT40.1] BY Foodexp_4pt
  /CATEGORIES VARIABLES=HH_Size_Cat Foodexp_4pt ORDER=A KEY=VALUE EMPTY=EXCLUDE TOTAL=YES 
    POSITION=AFTER
  /CRITERIA CILEVEL=95.


*---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------.

DATASET ACTIVATE DataSet1.
DESCRIPTIVES VARIABLES=HHHAge
  /STATISTICS=MEAN STDDEV MIN MAX.

FREQUENCIES VARIABLES=RESPAge
  /STATISTICS=RANGE MINIMUM MAXIMUM MEAN
  /ORDER=ANALYSIS.

FREQUENCIES VARIABLES=FCS
  /STATISTICS=RANGE MINIMUM MAXIMUM MEAN
  /ORDER=ANALYSIS.

FREQUENCIES VARIABLES=FES
  /STATISTICS=RANGE MINIMUM MAXIMUM MEAN
  /ORDER=ANALYSIS.

FREQUENCIES VARIABLES=HHExpFood_1M
  /STATISTICS=RANGE MINIMUM MAXIMUM MEAN
  /ORDER=ANALYSIS.

FREQUENCIES VARIABLES=HDDS
  /STATISTICS=RANGE MINIMUM MAXIMUM MEAN
  /ORDER=ANALYSIS.

