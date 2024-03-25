clear
set mem 500m
* Please remember to change to your own directory
cd "C:\Users\yuerl\Desktop\Gu Econ\econ 554 stata"
use "DA Final Re.dta", clear


* GENERATE INTERACTION TERMS AND VARIABLES
* exchange rate as of jan1 2005

replace tinc = tinc *0.009456

gen consentany2004= T_consenthiv
replace consentany2004=1 if T_consentsti==1 & consentany!=1


gen inter98 = 1 if m1out==2
gen inter01 =1 if m2out==2


foreach var of varlist hadsex12 havesex_fo {
gen got_`var' = got*`var'
gen got_hiv_`var' = got*`var'*hiv2004
gen any_hiv_`var' = any*`var'*hiv2004
gen tinc_hiv_`var' = tinc*`var'*hiv2004
gen under_hiv_`var' = under*`var'*hiv2004
gen any_male_`var' = any*`var'*male
}

foreach var of varlist any tinc under{
gen `var'_male = `var'* male
}

gen male_under = male*under

drop any_male tinc_male under_male 

foreach var of varlist got hiv2004 male havesex hadsex   {
gen any_`var' = any*`var' 
gen tinc_`var'  = tinc*`var' 
gen under_`var'  = under * `var' 
gen over_`var'  = over* `var' 
}

foreach var of varlist distvct  got hadsex male havesex {
gen male_`var' = male*`var'
gen any_`var'_hiv = any*`var' * hiv2004
gen tinc_`var'_hiv = tinc*`var' * hiv2004
gen under_`var'_hiv = under*`var' * hiv2004
gen over_`var'_hiv = over*`var' * hiv2004
gen hiv_`var' = hiv2004*`var'
gen `var'_hiv = hiv2004*`var'
gen any_`var'_male= any*`var' * male
gen tinc_`var'_male= tinc*`var' * male
gen under_`var'_male = under*`var' * male
}
gen tinc_male_hadsex12 =tinc_male*hadsex12 
gen under_male_hadsex12 =under_male*hadsex12

* Generate the main sample for the paper
gen MainSample = 1 if test2004==1 & age!=. & villnum!=. & tinc!=. & distvct!=. & hiv2004!=-1 & followup_test!=1

keep if Main==1

gen male_any = male*any
gen never = 1 if eversex==0 
replace never=0 if eversex==1 
gen any_never=any*never 
gen over_any=over*any 
gen hiv_any=hiv2004*any

gen distvcts = distvct*distvct
gen tinc2 = tinc * tinc


gen tinc2_male = male*tinc2
gen distvct_male = male*distvct
gen distvct2_male = male*distvcts
gen tinc2_male_hiv = male*tinc2*hiv2004
gen distvct_male_hiv = male*distvct*hiv2004
gen distvct2_male_hiv = male*distvcts*hiv2004
gen tinc2_hiv2004 = hiv2004*tinc2
gen distvct_hiv2004 = distvct * hiv2004
gen distvct2_hiv2004 = distvcts * hiv2004

ssc install outreg2



** TABLE 7: Effects of Learning HIV Results among sexually active
ivreg anycond  hiv_got got hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using Table7.xls, se replace bra symbol(***, **, *) coefastr bdec(3) ctitle(anycond )
ivreg anycond  ( hiv_got  got= any tinc tinc2 distvct distvcts any_male tinc_male tinc2_male distvct_male distvct2_male any_male_hiv tinc_male_hiv tinc2_male_hiv distvct_male_hiv distvct2_male_hiv any_hiv2004 tinc_hiv2004 tinc2_hiv2004 distvct_hiv2004 distvct2_hiv2004 hiv2004 male) hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using Table7.xls, se append bra symbol(***, **, *) coefastr bdec(3) ctitle(anycond )

ivreg numcond hiv_got got hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using Table7.xls, se append bra symbol(***, **, *) coefastr bdec(3) ctitle(num)
ivreg numcond ( hiv_got  got= any tinc tinc2 distvct distvcts any_male tinc_male tinc2_male distvct_male distvct2_male any_male_hiv tinc_male_hiv tinc2_male_hiv distvct_male_hiv distvct2_male_hiv any_hiv2004 tinc_hiv2004 tinc2_hiv2004 distvct_hiv2004 distvct2_hiv2004 hiv2004 male) hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using Table7.xls, se append bra symbol(***, **, *) coefastr bdec(3) ctitle(num)

ivreg bought hiv_got  got hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using Table7.xls, se append bra symbol(***, **, *) coefastr bdec(3) ctitle(bought)
ivreg bought ( hiv_got got= any tinc tinc2 distvct distvcts any_male tinc_male tinc2_male distvct_male distvct2_male any_male_hiv tinc_male_hiv tinc2_male_hiv distvct_male_hiv distvct2_male_hiv any_hiv2004 tinc_hiv2004 tinc2_hiv2004 distvct_hiv2004 distvct2_hiv2004 hiv2004 male) hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using Table7.xls, se append bra symbol(***, **, *) coefastr bdec(3) ctitle(bought)

ivreg havesex_fo hiv_got got hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using Table7.xls, se append bra symbol(***, **, *) coefastr bdec(3) ctitle(havesex)
ivreg havesex_fo ( hiv_got got= any tinc tinc2 distvct distvcts any_male tinc_male tinc2_male distvct_male distvct2_male any_male_hiv tinc_male_hiv tinc2_male_hiv distvct_male_hiv distvct2_male_hiv any_hiv2004 tinc_hiv2004 tinc2_hiv2004 distvct_hiv2004 distvct2_hiv2004 hiv2004 male) hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using Table7.xls, se append bra symbol(***, **, *) coefastr bdec(3) ctitle(havesex)

sum anycond  numcond bought havesex_fo if hadsex12==1



** TABLE 9: Interaction with Sexual Behavior: Only HIV Negatives
ivreg anycond  got got_hadsex12 hadsex12 male age age2 simave rumphi if hiv2004==0 , robust cluster(villnum) 
outreg2 using Table9.xls, se replace 2aster coefastr bdec(3) bra
ivreg anycond  (got got_hadsex12 = any_male tinc_male under_male  under any tinc under_hadsex12 tinc_hadsex12 any_hadsex12 any_male_hadsex12 tinc_male_hadsex12 under_male_hadsex12  ) hadsex12 male age age2 simave rumphi if hiv2004==0 , robust cluster(villnum) 
outreg2 using Table9.xls, se append 2aster coefastr bdec(3) bra

ivreg numcond got got_hadsex12 hadsex12 male age age2 simave rumphi if hiv2004==0 , robust cluster(villnum) 
outreg2 using Table9.xls, se append 2aster coefastr bdec(3) bra
ivreg numcond (got got_hadsex12 = any_male tinc_male under_male  under any tinc under_hadsex12 tinc_hadsex12 any_hadsex12 any_male_hadsex12 tinc_male_hadsex12 under_male_hadsex12  ) hadsex12 male age age2 simave rumphi if hiv2004==0 , robust cluster(villnum) 
outreg2 using Table9.xls, se append 2aster coefastr bdec(3) bra

ivreg bought got got_hadsex12 hadsex12 male age age2 simave rumphi if hiv2004==0 , robust cluster(villnum) 
outreg2 using Table9.xls, se append 2aster coefastr bdec(3) bra
ivreg bought (got got_hadsex12 = any_male tinc_male under_male  under any tinc under_hadsex12 tinc_hadsex12 any_hadsex12 any_male_hadsex12 tinc_male_hadsex12 under_male_hadsex12  ) hadsex12 male age age2 simave rumphi if hiv2004==0 , robust cluster(villnum) 
outreg2 using Table9.xls, se append 2aster coefastr bdec(3) bra

ivreg havesex_fo got got_hadsex12 hadsex12 male age age2 simave rumphi if hiv2004==0 , robust cluster(villnum) 
outreg2 using Table9.xls, se append 2aster coefastr bdec(3) bra 
ivreg havesex_fo (got got_hadsex12 = any_male tinc_male under_male  under any tinc under_hadsex12 tinc_hadsex12 any_hadsex12 any_male_hadsex12 tinc_male_hadsex12 under_male_hadsex12  ) hadsex12 male age age2 simave rumphi if hiv2004==0 , robust cluster(villnum) 
outreg2 using Table9.xls, se append 2aster coefastr bdec(3) bra
 
sum anycond numcond bought havesex_fo  if hiv2004==0 & hadsex12!=.



*sensitivity test
keep if site==2

ivreg anycond  hiv_got got hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using TableSen.xls, se replace bra symbol(***, **, *) coefastr bdec(3) ctitle(anycond )
ivreg anycond  ( hiv_got  got= any tinc tinc2 distvct distvcts any_male tinc_male tinc2_male distvct_male distvct2_male any_male_hiv tinc_male_hiv tinc2_male_hiv distvct_male_hiv distvct2_male_hiv any_hiv2004 tinc_hiv2004 tinc2_hiv2004 distvct_hiv2004 distvct2_hiv2004 hiv2004 male) hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using TableSen.xls, se append bra symbol(***, **, *) coefastr bdec(3) ctitle(anycond )

ivreg numcond hiv_got got hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using TableSen.xls, se append bra symbol(***, **, *) coefastr bdec(3) ctitle(num)
ivreg numcond ( hiv_got  got= any tinc tinc2 distvct distvcts any_male tinc_male tinc2_male distvct_male distvct2_male any_male_hiv tinc_male_hiv tinc2_male_hiv distvct_male_hiv distvct2_male_hiv any_hiv2004 tinc_hiv2004 tinc2_hiv2004 distvct_hiv2004 distvct2_hiv2004 hiv2004 male) hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using TableSen.xls, se append bra symbol(***, **, *) coefastr bdec(3) ctitle(num)

ivreg bought hiv_got  got hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using TableSen.xls, se append bra symbol(***, **, *) coefastr bdec(3) ctitle(bought)
ivreg bought ( hiv_got got= any tinc tinc2 distvct distvcts any_male tinc_male tinc2_male distvct_male distvct2_male any_male_hiv tinc_male_hiv tinc2_male_hiv distvct_male_hiv distvct2_male_hiv any_hiv2004 tinc_hiv2004 tinc2_hiv2004 distvct_hiv2004 distvct2_hiv2004 hiv2004 male) hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using TableSen.xls, se append bra symbol(***, **, *) coefastr bdec(3) ctitle(bought)

ivreg havesex_fo hiv_got got hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using TableSen.xls, se append bra symbol(***, **, *) coefastr bdec(3) ctitle(havesex)
ivreg havesex_fo ( hiv_got got= any tinc tinc2 distvct distvcts any_male tinc_male tinc2_male distvct_male distvct2_male any_male_hiv tinc_male_hiv tinc2_male_hiv distvct_male_hiv distvct2_male_hiv any_hiv2004 tinc_hiv2004 tinc2_hiv2004 distvct_hiv2004 distvct2_hiv2004 hiv2004 male) hiv2004 male  age age2 simave rumphi if hadsex12==1, robust cluster(villnum) 
outreg2 using TableSen.xls, se append bra symbol(***, **, *) coefastr bdec(3) ctitle(havesex)

sum anycond  numcond bought havesex_fo if hadsex12==1



** Alternative outcome

keep if followupsurvey == 1

reg numsex_fo got got_hiv hiv2004 male age age2 if havesex_fo == 1, robust cluster(villnum) 
outreg2 using Alternative.xls, se replace 2aster coefastr bdec(3) bra

ivreg numsex_fo (hiv_got got = any tinc tinc2 distvct distvcts any_male tinc_male tinc2_male distvct_male distvct2_male any_male_hiv tinc_male_hiv tinc2_male_hiv distvct_male_hiv distvct2_male_hiv any_hiv2004 tinc_hiv2004 tinc2_hiv2004 distvct_hiv2004 distvct2_hiv2004 hiv2004 male)hiv2004 male age age2 if havesex_fo == 1, robust cluster(villnum) 
outreg2 using Alternative.xls, se append 2aster coefastr bdec(3) bra


