use "C:\Users\christopher\Desktop\NHA_PM\PM_NHANES_2011_2016.dta"

*Survey weights
gen wtmec6yr = wtmec2yr/3
gen wtsaf6yr = wtsaf2yr/3
svyset [pw = wtsaf6yr], psu(sdmvpsu) strata(sdmvstra) singleunit(centered)

***FRAMINGHAM***
*Gender 1=male 2= female
gen sex = riagendr
recode sex (2=.)

*AGE - ages between 30-65
rename ridageyr age
gen age_s1 = . 
replace age_s1 = . if (age<30)
replace age_s1 = 0 if (age>=30)&(age<35)
replace age_s1 = 2 if (age>=35)&(age<40)
replace age_s1 = 5 if (age>=40)&(age<45)
replace age_s1 = 6 if (age>=45)&(age<50)
replace age_s1 = 8 if (age>=50)&(age<55)
replace age_s1 = 10 if (age>=55)&(age<60)
replace age_s1 = 11 if (age>=60)&(age<65)
replace age_s1 = 12 if (age>=65)&(age<70)
replace age_s1 = 14 if (age>=70)&(age<75)
replace age_s1 = 15 if (age>=75)&(age!=.)


*ANTI-HTN MEDS
rename bpq040a anti_htnmeds
recode anti_htnmeds (9=.)

*Blood Pressure
gen sysbp = (bpxsy2+bpxsy3)/2

gen bloodpressure_s1 = .
replace bloodpressure_s1 = 1 if (sysbp<120)
replace bloodpressure_s1 = 2 if (sysbp>=120)&(sysbp<130)
replace bloodpressure_s1 = 3 if (sysbp>=130)&(sysbp<140)
replace bloodpressure_s1 = 4 if (sysbp>=140)&(sysbp<160)
replace bloodpressure_s1 = 5 if (sysbp>=160)&(sysbp!=.)
label define bloodpressure_label 1 "Optimal" 2 "Normal" 3 "High BP" 4 "HTN1" 5 "HTN2"
label values bloodpressure_s1 bloodpressure_label

*BP Treated
gen bpTx = .
replace bpTx = 0 if (bloodpressure_s1==1)&(anti_htnmeds==1)
replace bpTx = 2 if (bloodpressure_s1==2)&(anti_htnmeds==1)
replace bpTx = 3 if (bloodpressure_s1==3)&(anti_htnmeds==1)
replace bpTx = 4 if (bloodpressure_s1==4)&(anti_htnmeds==1)
replace bpTx = 5 if (bloodpressure_s1==5)&(anti_htnmeds==1)

*BP Untreated
gen bpnTx = .
replace bpnTx = -2 if (bloodpressure_s1==1)|(anti_htnmeds==2)
replace bpnTx = 0 if (bloodpressure_s1==2)|(anti_htnmeds==2)
replace bpnTx = 1 if (bloodpressure_s1==3)|(anti_htnmeds==2)
replace bpnTx = 2 if (bloodpressure_s1==4)|(anti_htnmeds==2)
replace bpnTx = 3 if (bloodpressure_s1==5)|(anti_htnmeds==2)

*SMOKE
gen smoke =. 
replace smoke= 0 if smq020==2
replace smoke= 1 if smq020==1

rename smq040 c_smoke
recode c_smoke (7=.)(3=0)(2=1)
label define c_smoke_label 0 "Non-Smoker" 1 "Smoker"
label values c_smoke c_smoke_label 
recode c_smoke (1=4) 

gen smoke_1= .
replace smoke_1=0 if smoke==0
replace smoke_1=4 if smoke==1 & c_smoke==4

tab smoke_1
*Self-report diabetes
rename diq010 dm_selfr
recode dm_selfr (9=.)(7=.)(3=.)(2=0) (1=3)
label define dm_selfr_label 0 "No Diabetes" 1 "Diabetes"
label values dm_selfr dm_selfr_label
 

*Fasting Blood Glucose
rename lbxglu fbgluc
gen fbgluc_s1 = . 
replace fbgluc_s1 = 0 if (fbgluc<126)
replace fbgluc_s1 = 1 if (fbgluc>=126)&(fbgluc!=.)
label define fbgluc_s1_label 0 "no diabetes" 1 "diabetes"
label values fbgluc_s1 fbgluc_s1_label 

*Total Cholesterol Levels
rename lbxtc tot_chol
gen tot_chol_1=.
replace tot_chol_1 = 0 if (tot_chol<160)
replace tot_chol_1 = 1 if (tot_chol>=160)&(tot_chol<200)
replace tot_chol_1 = 2 if (tot_chol>=200)&(tot_chol<240)
replace tot_chol_1 = 3 if (tot_chol>=240)&(tot_chol<280)
replace tot_chol_1 = 4 if (tot_chol>=280)&(tot_chol!=.)

*HDL
rename lbdhdd hdl
gen hdl_s1=. 
replace hdl_s1 = -2 if (hdl<35)
replace hdl_s1 = -1 if (hdl<45)&(hdl>=35)
replace hdl_s1 = 0 if (hdl<50)&(hdl>=45)
replace hdl_s1 = 1 if (hdl<60)&(hdl>=50)
replace hdl_s1 = 2 if (hdl>=60)&(hdl!=.)

egen SF= rowtotal(age_s1 hdl_s1 bpTx bpnTx smoke_1 tot_chol_1 fbgluc_s1)

***PROCAM***
*Triglycerides
rename lbxstr trigly
gen trigly_1=. 
replace trigly_1=0 if trigly < 100
replace trigly_1=2 if trigly >= 100 & trigly <= 149
replace trigly_1=3 if trigly >= 150 & trigly <= 199
replace trigly_1=4 if trigly >= 200 & trigly != .
*HDL
gen hdl_s2=. 
replace hdl_s2 = 11 if hdl <35
replace hdl_s2 = 8 if (hdl <45) &(hdl >= 35)
replace hdl_s2 = 5 if (hdl < 54) & (hdl >= 45)
replace hdl_s2 = 0 if (hdl >= 50) & (hdl!=.)

*LDL 
gen ldl = tot_chol - trigly/5 - hdl
gen ldl_1 = .
replace ldl_1 = 0 if (ldl<100)
replace ldl_1 = 5 if (ldl>=100)&(ldl<130)
replace ldl_1 = 10 if (ldl>=130)&(ldl<160)
replace ldl_1 = 14 if (ldl>=160)&(ldl<190)
replace ldl_1 = 20 if (ldl>190)&(ldl!=.)
*fasting blood glucose (120)
gen fbgluc_s2 = . 
replace fbgluc_s2 = 0 if (fbgluc<120)
replace fbgluc_s2 = 1 if (fbgluc>=120)&(fbgluc!=.)
label define fbgluc_s2_label 0 "no diabetes" 1 "diabetes"
label values fbgluc_s2 fbgluc_s2_label 

*smoke2
gen smoke_2 = smoke_1
recode smoke_2(4=8)
*dm2
gen dm_selfr_2 = dm_selfr
recode dm_selfr_2(2=0) (1=6)	

*BP
gen bloodpressure_s2 = (bpxsy2+bpxsy3)/2
gen bloodpressure_s2_1=. 
replace bloodpressure_s2_1 = 0 if (bloodpressure_s2<120)
replace bloodpressure_s2_1 = 2 if (bloodpressure_s2>=120)&(bloodpressure_s2<130)
replace bloodpressure_s2_1 = 3 if (bloodpressure_s2>=130)&(bloodpressure_s2<140)
replace bloodpressure_s2_1 = 5 if (bloodpressure_s2>=140)&(bloodpressure_s2<160)
replace bloodpressure_s2_1 = 8 if (bloodpressure_s2>=160)&(bloodpressure_s2 !=.)

*history
rename mcq300a hist_MI
recode hist_MI (9=.)
recode hist_MI (7=.)
recode hist_MI (2=0) // No
recode hist_MI (1=4) //yes
*AgeS2
gen age_s2 = . 
replace age_s2 = 0 if (age>=35)&(age<40)
replace age_s2 = 6 if (age>=40)&(age<45)
replace age_s2 = 11 if (age>=45)&(age<50)
replace age_s2 = 16 if (age>=50)&(age<55)
replace age_s2 = 21 if (age>=55)&(age<60)
replace age_s2 = 26 if (age>=60)&(age<=65)

**
egen SP = rowtotal(age_s2 ldl_1 hdl_s2 trigly_1 smoke_2 dm_selfr_2 hist_MI bloodpressure_s2_1)


*HTN
rename bpq020 htn
recode htn (9=.)(7=.)(2=0)

rename bpq030 htn_1
recode htn_1 (9=.)(7=.)(2=0)
***CVD****
rename mcq160b chf
rename mcq160c chd
rename mcq160d angina
rename mcq160e heartattack
rename mcq160f stroke

recode chf (9=.)(7=.)(2=0)
recode chd (9=.)(7=.)(2=0)
recode angina (9=.)(7=.)(2=0)
recode heartattack (9=.)(7=.)(2=0)
recode stroke (9=.)(7=.)(2=0)

egen cvd = rowtotal(chf chd angina heartattack stroke htn)

***rescale SF and SP

*SP
gen SP_1 = .
replace SP_1 = . if (cvd>=1)&(cvd<=6)
replace SP_1 = 1 if (SP>=0)&(SP<=44)
replace SP_1 = 2 if (SP>44)&(SP<=53)
replace SP_1 = 3 if (SP>53)&(SP<=80)


*SF
gen SF_1 = .
replace SF_1 = . if (cvd>=1)&(cvd<=6)
replace SF_1 = 1 if (SF<10)
replace SF_1 = 2 if (SF>=10)&(SF<=20)
replace SF_1 = 3 if (SF>20)&(SF<=33)


pwcorr cvd age_s1 hdl_s1 bpTx bpnTx smoke_1 tot_chol_1 fbgluc_s1, sig star(0.05)
pwcorr cvd age_s2 ldl_1 hdl_s2 trigly_1 smoke_2 dm_selfr_2 hist_MI bloodpressure_s2_1, sig star(0.05)

gen cvd_di = . 
replace cvd_di = 0 if cvd == 0 
replace cvd_di = 1 if cvd == 1
replace cvd_di = 1 if cvd == 2
replace cvd_di = 1 if cvd == 3
replace cvd_di = 1 if cvd == 4
replace cvd_di = 1 if cvd == 5
replace cvd_di = 1 if cvd == 6

**not a scale its an index







gen cvd_di = cvd
recode cvd_di (2=1)(3=1)(4=1)(5=1)(6=1)

gen FP = .
replace FP = 1 == SF_1
replace FP = 2 == SP_1

tab FP

validscale chf chd angina heartattack stroke htn, partition(6) scorename(score1) graphs
alpha chf chd angina heartattack stroke htn

factor age_s2 ldl_1 hdl_s2 trigly_1 smoke_2 dm_selfr_2 hist_MI bloodpressure_s2_1
rotate, varimax horst blanks(.3)

factor age_s1 hdl_s1 bpTx bpnTx smoke tot_chol_1 dm_selfr
rotate, varimax horst blanks(.3)
// STOP SHELLY ONLY RUN ABOVE//

*Scale calculation:

***FRAMINGHAM***
*ICC(1):
icc cvd age_s1 anti_htnmeds sysbp diabp bpTx bpnTx dm_selfr smoke fbgluc tot_chol

*ICC(2):


*ICC(3):
icc cvd sysbp diabp bpTx bpnTx dm_selfr smoke fbgluc tot_chol, mixed

*Kappa:
kap cvd SF

*Cronbat's alpha:
alpha cvd age_s1 anti_htnmeds sysbp diabp bpTx bpnTx dm_selfr smoke fbgluc tot_chol

***PROCAM***
*ICC(1):
icc cvd trigly hdl_s2 ldl_1 fbgluc_s2 age_s2 bloodpressure_s2_1 hist_MI

*ICC(2):


*ICC(3):
icc cvd trigly hdl_s2 ldl_1 fbgluc_s2 age_s2 bloodpressure_s2_1 hist_MI, mixed

*Kappa:
kap cvd SP

*Cronbat's alpha:
alpha cvd trigly hdl_s2 ldl_1 fbgluc_s2 age_s2 bloodpressure_s2_1 hist_MI










