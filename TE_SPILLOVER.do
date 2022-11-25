clear all
clear matrix
clear mata
cap log close
set memory 900m
set matsize 11000
set maxvar 32000
set more off

*cd "D:\KHOA\FDISpillover"
use VSICMfIndustryFirms1016FULLDW.dta

**Panel format
gen t = year
gen t0 = 2009
summarize t

tab year, gen(DYEAR)

gen dyr1216 = 0
replace dyr1216 = 1 if year >= 2012

keep if t > t0

drop if wsdftotalrev == .
drop if wsdffxassets == .
drop if wsemployees == .
drop if wsdfmaterial == .

keep if wsdftotalrev > 0
keep if wsdffxassets > 0
keep if wsemployees > 0
keep if wsdfmaterial > 0

*count the unique entities
gen count1 = group(firmcd)
sum count1
* Total assets
gen size = log(wsdftotalassets)

gen lny = log(wsdftotalrev)
gen lnk = log(wsdffxassets)
gen lnl = log(wsemployees)
gen lnm = log(wsdfmaterial)
gen Trend = t - t0

summarize t count1 wsdftotalrev lny wsdffxassets lnk wsemployees lnl wsdfmaterial lnm

gen dezmiddle = 0
gen dezsouth = 0
replace dezmiddle = 1 if ecozone == 3 | ecozone == 4
replace dezsouth = 1 if ecozone == 5 | ecozone == 6

gen dstateowned = 0
gen dprivatelyowned = 0
gen dforeignowned = 0
replace dstateowned = 1 if inlist(firmtype, 1,2,3,4)
replace dprivatelyowned = 1 if inlist(firmtype, 5,6,7,8,9,10)
replace dprivatelyowned = 1 if firmtype == 11 & (year == 2010 | year == 2011) 
replace dforeignowned = 1 if firmtype == 12 & (year == 2010 | year == 2011) 
replace dforeignowned = 1 if firmtype == 11 & inlist(year, 2012,2013,2014,2015,2016)
* base group: joint-venture firm: dstateowned = 0 & dprivatelyowned = 0 & dforeignowned = 0

*0.Than, nhiên liệu
drop if inlist(vsic, 19100,19200,19200)

*1. Lương thực thực phẩm - BASEGROUP
gen dmanufacturinggr = 1
*2. Dệt may-Da-Giày
replace dmanufacturinggr = 2 if inlist(vsic, 13110,13120,13130,13210,13220,13230,13240,13290,14100)
replace dmanufacturinggr = 2 if inlist(vsic, 14200,14300,15110,15120,15200)
*3. Sản phẩm từ gỗ, tre, nứa, giường
replace dmanufacturinggr = 3 if inlist(vsic, 16101,16102,16210,16220,16230,16291,16292,31001,31009)
*4. Sản phẩm giấy
replace dmanufacturinggr = 4 if inlist(vsic, 17010,17021,17022,17090)
*5. Sản phẩm in
replace dmanufacturinggr = 5 if inlist(vsic, 18110,18120,18200)
*6. Hóa chất, cao su, Plastic
replace dmanufacturinggr = 6 if inlist(vsic, 20110,20120,20131,20132,20210,20221,20222,20231,20232)
replace dmanufacturinggr = 6 if inlist(vsic, 20290,20300,21001,21002,22110,22120,22201,22209)
*7. Vật liệu xây dựng
replace dmanufacturinggr = 7 if inlist(vsic, 23100,23910,23920,23930,23941)
*8. Sản phẩm phi kim loại và Kim loại
replace dmanufacturinggr = 8 if inlist(vsic, 23942,23943,23950,23960,23990,24100,24200,24310,24320)
replace dmanufacturinggr = 8 if inlist(vsic, 25110,25120,25130,25200,25910,25920,25930,25991,25999)
*9. Điện, điện tử, công nghệ TT
replace dmanufacturinggr = 9 if inlist(vsic, 26100,26200,26300,26400,26510,26520,26600,26700,26800)
replace dmanufacturinggr = 9 if inlist(vsic, 27101,27102,27200,27310,27320,27330,27400,27500,27900)
*10. Cơ khí-Máy móc thiết bị
replace dmanufacturinggr = 10 if inlist(vsic, 28110,28120,28130,28140,28150,28160,28170,28180,28190)
replace dmanufacturinggr = 10 if inlist(vsic, 28210,28220,28230,28240,28250,28260,28291,28299,29100)
replace dmanufacturinggr = 10 if inlist(vsic, 29200,29300,30110,30120,30200,30300,30400,30910,30920)
replace dmanufacturinggr = 10 if inlist(vsic, 30990,32110,32120,32200,32300,32400,32501,32502,32900)
replace dmanufacturinggr = 10 if inlist(vsic, 33110,33120,33130,33140,33150,33190,33200)
*11. Sản xuất điện, nước, Khí và PP ĐN
replace dmanufacturinggr = 11 if inlist(vsic, 35101,35102,35200,35301,35302,36000)

duplicates report firmcd year
duplicates drop firmcd year, force

drop if wsblk == . | wsflk == . | wsh == . | wsdftotalassets == . | wsdftotalcost == . 

xtset firmcd t
by firmcd: drop if _N < 4
tsspell, f(L.year == .)
by firmcd: egen _maxseq = max(_seq)
by firmcd: drop if _maxseq < 4
drop _*

sum count1
*Variables for constraints of the optimization problem solving
gen bxk = lnk 
gen bxl = lnl
gen bxm = lnm
gen bxt = Trend
gen bh = h
global xvar bxk bxl bxm bxt

gen deznorth = 0
replace deznorth = 1 if dezmiddle == 0 & dezsouth == 0



summarize blk wsblk flk wsflk h size equityratio wsequityratio wsdftotalrev wsdffxassets wsemployees wsdfmaterial
tabstat blk wsblk flk wsflk h size equityratio wsequityratio wsdftotalrev wsdffxassets wsemployees wsdfmaterial, stat(iqr q)
	
*winsor2 lnk lnl lnm blk wsblk flk wsflk h size equityratio wsequityratio wsdftotalrev wsdffxassets wsemployees wsdfmaterial, replace cuts(2 98) trim

***STEP 1: SFACobbDouglas
*eststo SFACobbDouglas: sfpanel lny $xvar, technique(bhhh 20 nr 20) iterate(1000) model(bc95) dist(tnormal) cluster(manufacturingcd)
sfpanel lny $xvar, technique(bhhh 20 nr 20) iterate(1000) model(bc95) dist(tnormal) 

*estimates of technical efficiency via E[exp(-u)|e] (Battese and Coelli, 1988)
predict te_cb, bc 
*br firmcd t te_cb


***BASELINE
***COBB-DOUGLAS PRODUCTION FUNCTION
*sfpanel: Stochastic frontier models for panel data
xtreg te_cb L.wsblk i.t, ro cluster(firmcd)
xtreg te_cb  L.wsflk  i.t, ro cluster(firmcd)
xtreg te_cb  L.wsh i.t, ro cluster(firmcd)

xtreg te_cb L.(wsblk wsflk wsh) i.t, ro cluster(firmcd)
xtreg te_cb L.(wsblk wsflk wsh size wsequityratio dexp dimp dindpark) i.t, ro cluster(firmcd)


xtreg te_cb wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t if dezsouth == 1, ro cluster(firmcd) 
xtreg te_cb wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t if dezmiddle == 1, ro cluster(firmcd) 
xtreg te_cb wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t if dezmiddle == 0 & dezsouth == 0, ro cluster(firmcd) 

xtreg te_cb wsblk c.wsflk##i.dezsouth wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 
xtreg te_cb wsblk c.wsflk##i.dezmiddle wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 
xtreg te_cb wsblk c.wsflk##i.deznorth wsh size wsequityratio dexp dimp dindpark deznorth dezmiddle dezsouth i.t, ro cluster(firmcd) 

***STEP 2: TRANSLOG PRODUCTION FUNCTION

***TRANSLOG PRODUCTION FUNCTION
*sfpanel: Stochastic frontier models for panel data
*Variables for constraints of the optimization problem solving

gen bxkk = 0.5*lnk*lnk 
gen bxll = 0.5*lnl*lnl
gen bxmm = 0.5*lnm*lnm
gen bxtt = 0.5*Trend*Trend

gen bxkl = lnk*lnl
gen bxkm = lnk*lnm
gen bxlm = lnl*lnm

gen bxtk = Trend*lnk
gen bxtl = Trend*lnl
gen bxtm = Trend*lnm

global xvar_tlsfp bxk bxl bxm bxt bxkk bxll bxmm bxtt bxkl bxkm bxlm bxtk bxtl bxtm
sfpanel lny $xvar_tlsfp, technique(bhhh 20 nr 20) iterate(1000) model(bc95) dist(tnormal) robust 

*estimates of technical efficiency via E[exp(-u)|e] (Battese and Coelli, 1988)
predict te_tr, bc 
*br firmcd t te_tr

scatter te_cb wsflk
scatter te_tr wsflk
winsor2 te_cb te_tr, replace cuts(1 99) trim

*sfpanel: Stochastic frontier models for panel data
xtreg te_tr wsblk i.t, ro cluster(firmcd)
xtreg te_tr  wsflk  i.t, ro cluster(firmcd)
xtreg te_tr  wsh i.t, ro cluster(firmcd)

xtreg te_cb wsblk wsflk wsh i.t, ro cluster(firmcd)
xtreg te_tr wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth, ro cluster(firmcd)


xtreg te_tr wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t if dezsouth == 1, ro cluster(firmcd) 
xtreg te_tr wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t if dezmiddle == 1, ro cluster(firmcd) 
xtreg te_tr wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t if dezmiddle == 0 & dezsouth == 0, ro cluster(firmcd) 

xtreg te_tr wsblk c.wsflk##i.dezsouth wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 
xtreg te_tr wsblk c.wsflk##i.dezmiddle wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 
xtreg te_tr wsblk c.wsflk##i.deznorth wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 

*Export baseline 1

eststo clear
***COBB-DOUGLAS PRODUCTION FUNCTION

eststo:  xtreg te_cb wsblk i.t, ro cluster(firmcd)
eststo:  xtreg te_cb  wsflk  i.t, ro cluster(firmcd)
eststo:  xtreg te_cb  wsh i.t, ro cluster(firmcd)

eststo:  xtreg te_cb wsblk wsflk wsh i.t, ro cluster(firmcd)
eststo:  xtreg te_cb wsblk wsflk wsh size wsequityratio dexp dimp dindpark deznorth dezmiddle dezsouth i.t, ro cluster(firmcd)

***TRANSLOG PRODUCTION FUNCTION

eststo: xtreg te_tr wsblk i.t, ro cluster(firmcd)
eststo: xtreg te_tr  wsflk  i.t, ro cluster(firmcd)
eststo: xtreg te_tr  wsh i.t, ro cluster(firmcd)

eststo: xtreg te_tr wsblk wsflk wsh i.t, ro cluster(firmcd)
eststo: xtreg te_tr wsblk wsflk wsh size wsequityratio dexp dimp dindpark deznorth dezmiddle dezsouth i.t, ro cluster(firmcd)

esttab using baseline-20190213.csv, se r2 ar2  star(* .1 ** .05 *** .01) replace
eststo clear
***

eststo clear
***COBB-DOUGLAS PRODUCTION FUNCTION

eststo:  xtreg te_cb L.wsblk i.t, ro cluster(firmcd)
eststo:  xtreg te_cb  L.wsflk  i.t, ro cluster(firmcd)
eststo:  xtreg te_cb  L.wsh i.t, ro cluster(firmcd)

eststo:  xtreg te_cb L.(wsblk wsflk wsh) i.t, ro cluster(firmcd)
eststo:  xtreg te_cb L.(wsblk wsflk wsh size wsequityratio dexp dimp dindpark) i.t, ro cluster(firmcd)

***TRANSLOG PRODUCTION FUNCTION

eststo: xtreg te_tr  L.wsblk i.t, ro cluster(firmcd)
eststo: xtreg te_tr  L.wsflk  i.t, ro cluster(firmcd)
eststo: xtreg te_tr  L.wsh i.t, ro cluster(firmcd)

eststo: xtreg te_tr L.(wsblk wsflk wsh) i.t, ro cluster(firmcd)
eststo: xtreg te_tr L.(wsblk wsflk wsh size wsequityratio dexp dimp dindpark) i.t, ro cluster(firmcd)

esttab using baseline-20190213-lag1.csv, se r2 ar2  star(* .1 ** .05 *** .01) replace
eststo clear

	
xtset firmcd t 
****
*ROBUSTNESS 1
xtreg te_cb L.(c.wsflk##i.dezsouth  size wsequityratio dexp dimp dindpark) i.t, ro cluster(firmcd) 
xtreg te_cb c.wsflk##i.dezmiddle size wsequityratio dexp dimp dindpark i.t, ro cluster(firmcd) 
xtreg te_cb c.wsflk##i.deznorth  size wsequityratio dexp dimp dindpark i.t, ro cluster(firmcd)	
	
xtreg te_tr c.wsflk##i.dezsouth  size wsequityratio dexp dimp dindpark i.t, ro cluster(firmcd) 
xtreg te_tr c.wsflk##i.dezmiddle size wsequityratio dexp dimp dindpark i.t, ro cluster(firmcd) 
xtreg te_tr c.wsflk##i.deznorth  size wsequityratio dexp dimp dindpark i.t, ro cluster(firmcd)	
	
	
eststo clear
***COBB-DOUGLAS PRODUCTION FUNCTION

eststo: xtreg te_cb L.(c.wsflk##i.dezsouth  size wsequityratio dexp dimp dindpark) i.t, ro cluster(firmcd) 
eststo: xtreg te_cb L.(c.wsflk##i.dezmiddle size wsequityratio dexp dimp dindpark) i.t, ro cluster(firmcd) 
eststo: xtreg te_cb L.(c.wsflk##i.deznorth  size wsequityratio dexp dimp dindpark) i.t, ro cluster(firmcd)	

***TRANSLOG PRODUCTION FUNCTION
	
eststo: xtreg te_tr L.(c.wsflk##i.dezsouth  size wsequityratio dexp dimp dindpark) i.t, ro cluster(firmcd) 
eststo: xtreg te_tr L.(c.wsflk##i.dezmiddle size wsequityratio dexp dimp dindpark) i.t, ro cluster(firmcd) 
eststo: xtreg te_tr L.(c.wsflk##i.deznorth  size wsequityratio dexp dimp dindpark) i.t, ro cluster(firmcd)
	
esttab using robust-20190213-1.csv, se r2 ar2  star(* .1 ** .05 *** .01) replace
eststo clear	
	
	
save splillover_20190216.dta	
use 	splillover_20190216.dta
	
*****************************************

*****************************************
***ROBUST 2	
***BY FIRM TYPE
gen dstateowned = 0
gen dprivatelyowned = 0
gen dforeignowned = 0
replace dstateowned = 1 if inlist(firmtype, 1,2,3,4)
replace dprivatelyowned = 1 if inlist(firmtype, 5,6,7,8,9,10)
replace dprivatelyowned = 1 if firmtype == 11 & (year == 2010 | year == 2011) 
replace dforeignowned = 1 if firmtype == 12 & (year == 2010 | year == 2011) 
replace dforeignowned = 1 if firmtype == 11 & inlist(year, 2012,2013,2014,2015,2016)
* base group: joint-venture firm: dstateowned = 0 & dprivatelyowned = 0 & dforeignowned = 0
	
*Private ownership
xtreg te_cb  L.wsflk  i.t if dprivatelyowned ==1 , ro cluster(firmcd)
xtreg te_cb L.(wsflk size wsequityratio dexp dimp dindpark) i.t if dprivatelyowned ==1, ro cluster(firmcd)
	
*State ownership
xtreg te_cb  L.wsflk  i.t if dstateowned ==1 , ro cluster(firmcd)
xtreg te_cb L.(wsflk size wsequityratio dexp dimp dindpark) i.t if dstateowned ==1, ro cluster(firmcd)	
	
*FDI ownership
xtreg te_cb  L.wsflk  i.t if dforeignowned ==1 , ro cluster(firmcd)
xtreg te_cb L.(wsflk size wsequityratio dexp dimp dindpark) i.t if dforeignowned ==1, ro cluster(firmcd)	

*Joint-venture ownership
xtreg te_cb  L.wsflk  i.t if dstateowned == 0 & dprivatelyowned == 0, ro cluster(firmcd)
xtreg te_cb L.(wsflk size wsequityratio dexp dimp dindpark) i.t if dstateowned == 0 & dprivatelyowned == 0, ro cluster(firmcd)	

	
eststo clear
***COBB-DOUGLAS PRODUCTION FUNCTION
*Private ownership
eststo: xtreg te_cb L.(wsflk size wsequityratio dexp dimp dindpark) i.t if dprivatelyowned ==1, ro cluster(firmcd)
*State ownership
eststo: xtreg te_cb L.(wsflk size wsequityratio dexp dimp dindpark) i.t if dstateowned ==1, ro cluster(firmcd)	
*Foreign ownership
eststo: xtreg te_cb L.(wsflk size wsequityratio dexp dimp dindpark) i.t if dstateowned == 0 & dprivatelyowned == 0, ro cluster(firmcd)	

***TRANSLOG PRODUCTION FUNCTION
	*Private ownership
eststo: xtreg te_tr L.(wsflk size wsequityratio dexp dimp dindpark) i.t if dprivatelyowned ==1, ro cluster(firmcd)
*State ownership
eststo: xtreg te_tr L.(wsflk size wsequityratio dexp dimp dindpark) i.t if dstateowned ==1, ro cluster(firmcd)	
*Foreign ownership
eststo: xtreg te_tr L.(wsflk size wsequityratio dexp dimp dindpark) i.t if dstateowned == 0 & dprivatelyowned == 0, ro cluster(firmcd)	
	
esttab using robust-20190216-2.csv, se r2 ar2  star(* .1 ** .05 *** .01) replace
eststo clear	

	
	
****************************
********CAPITAL INTENSIVE

***COBB-DOUGLAS PRODUCTION FUNCTION

xtreg te_cb L.(wsflk) i.t if ou_capital == 0, ro cluster(firmcd) 
	
xtreg te_cb L.(wsflk size wsequityratio dexp dimp dindpark) i.t if ou_capital == 0, ro cluster(firmcd) 

xtreg te_cb L.(wsflk ou_capital c.wsflk##i.ou_capital size wsequityratio dexp dimp dindpark) i.t, ro cluster(firmcd) 
*a can e ve chart interaction sau ham nay

xtset firmcd t
gen L1flk_ou_capital = L.wsflk*L.ou_capital
eststo interflkoucap: xtreg te_cb L.wsflk L.ou_capital L1flk_ou_capital L.size L.wsequityratio L.dexp L.dimp L.dindpark i.t, ro cluster(firmcd) 

matrix b=e(b)
matrix V=e(V)

scalar b1=b[1,1]
scalar b3=b[1,3]

scalar varb1=V[1,1]
scalar varb3=V[3,3]
scalar covb1b3=V[1,3]

local N = _N
local N1 = 2
gen MVZ=_n - 1
replace MVZ = . if _n > `N1'

gen conbx=b1+b8*MVZ

gen consx=sqrt(varb1+varb3*(MVZ^2)+2*covb1b3*MVZ)

gen ax=1.96*consx

gen upperx=conbx+ax

gen lowerx=conbx-ax

gen yline=0 if _n <= `N1' 

graph twoway (hist ou_capital, width(0.2) percent color(gs14) yaxis(2)) ///
			||  line conbx MVZ, clpattern(solid) clwidth(medium) clcolor(black) ///
			||  line upperx MVZ, clpattern(dash) clwidth(thin) clcolor(black) ///
			||  line lowerx MVZ, clpattern(dash) clwidth(thin) clcolor(black) ///
			||  line yline MVZ, clwidth(thin) clcolor(black) clpattern(solid) ///
			|| , ///
			xlabel(0(1)1, nogrid labsize(2)) ///
			ylabel(, axis(1) nogrid labsize(2)) ///
			yscale(noline alt) yscale(noline alt axis(2)) ///
            xscale(noline) ///
            legend(off) ///
			xtitle("Capital oversuse", size(2.5)) ///
			ytitle("Marginal Effect of FLK on the TE index", axis(1) size(2.5)) ///
			ytitle("Percentage of Observations", axis(2) size(2.5)) ylabel(, axis(2) nogrid labsize(2)) yscale(noline axis(2)) ///
            xsca(titlegap(2)) ///
            ysca(titlegap(2)) ///
			scheme(s2mono) subtitle("Marginal Effect of FLK on the TE index", size(3)) ///
			graphregion(fcolor(white) ilcolor(white) lcolor(white))

graph save Graph "INTER_TE_flk_oucapital.gph", replace

drop MVZ conbx consx ax upperx lowerx yline

***TRANSLOG PRODUCTION FUNCTION

xtreg te_tr L.(wsflk) i.t if ou_capital == 0, ro cluster(firmcd) 

xtreg te_tr L.(wsflk size wsequityratio dexp dimp dindpark) i.t if ou_capital == 0, ro cluster(firmcd) 
xtreg te_tr L.(c.wsflk##i.ou_capital size wsequityratio dexp dimp dindpark) i.t, ro cluster(firmcd) 
		
***COBB-DOUGLAS PRODUCTION FUNCTION
*sfpanel: Stochastic frontier models for panel data

* Olley and Pakes method
prodest lny, free(bxl) state(bxk) proxy(bxm) va met(op) poly(4) endo(wsflk) reps(40) id(firmcd) t(year) fsresiduals(op)
predict omega_op, omega
predict tfp_op, resid
sum tfp_op omega_op

xtset firmcd t 

xtreg tfp_op L.wsblk i.t, ro cluster(firmcd)
xtreg tfp_op L.wsflk i.t, ro cluster(firmcd)
xtreg tfp_op L.wsh i.t, ro cluster(firmcd)
 
xtreg tfp_op L.(wsblk wsflk wsh) i.t, ro cluster(firmcd)
xtreg tfp_op wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 
 
xtreg tfp_op wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t if dezsouth == 1, ro cluster(firmcd) 
xtreg tfp_op wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t if dezmiddle == 1, ro cluster(firmcd) 
xtreg tfp_op wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t if dezmiddle == 0 & dezsouth == 0, ro cluster(firmcd) 


xtreg tfp_op wsblk c.wsflk##i.dezsouth wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 
xtreg tfp_op wsblk c.wsflk##i.dezmiddle wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 
xtreg tfp_op wsblk c.wsflk##i.deznorth wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 


 *Ackerberg, Caves and Frazer correction
 prodest lny, free(bxl) state(bxk) proxy(bxm) va met(op) acf opt(nm) reps(50) id(firmcd) t(year) fsresiduals(fs_acf_op)

 predict tfp_acf, residuals
 predict te_acf, omega
 sum tfp_acf te_acf
 
xtreg te_acf wsblk i.t, ro cluster(firmcd)
xtreg te_acf wsflk i.t, ro cluster(firmcd)
xtreg te_acf wsh i.t, ro cluster(firmcd)
 
xtreg te_acf wsblk wsflk wsh i.t, ro cluster(firmcd)
xtreg te_acf wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 

xtreg te_acf wsblk c.wsflk##i.dezsouth wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 
xtreg te_acf wsblk c.wsflk##i.dezmiddle wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 
xtreg te_acf wsblk c.wsflk##i.deznorth wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 
 
 
*Levinsohn and Petrin method
 prodest lny, free(bxl) state(bxk) proxy(bxm) va met(lp) opt(dfp) reps(50) id(firmcd) t(year) fsresiduals(fs_lp)
 
   predict tfp_lp, residuals
   predict te_lp, omega
   sum tfp_lp te_lp
   
xtreg te_lp wsblk i.t, ro cluster(firmcd)
xtreg te_lp wsflk i.t, ro cluster(firmcd)
xtreg te_lp wsh i.t, ro cluster(firmcd)
 
xtreg te_lp wsblk wsflk wsh i.t, ro cluster(firmcd)
xtreg te_lp wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd)   
   
xtreg te_lp wsblk c.wsflk#i.dezsouth wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 
xtreg te_lp wsblk c.wsflk#i.dezmiddle wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 
xtreg te_lp wsblk c.wsflk#i.deznorth wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 
 
********************************************** 
 
********************************************* 
 
*Export baseline 1

eststo clear
** Olley and Pakes method

eststo:  xtreg te_op wsblk i.t, ro cluster(firmcd)
eststo:  xtreg te_op wsflk i.t, ro cluster(firmcd)
eststo:  xtreg te_op wsh i.t, ro cluster(firmcd)
 
eststo:  xtreg te_op wsblk wsflk wsh i.t, ro cluster(firmcd)
eststo:  xtreg te_op wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 

**Ackerberg, Caves and Frazer correction
eststo: xtreg te_acf wsblk i.t, ro cluster(firmcd)
eststo: xtreg te_acf wsflk i.t, ro cluster(firmcd)
eststo: xtreg te_acf wsh i.t, ro cluster(firmcd)
 
eststo: xtreg te_acf wsblk wsflk wsh i.t, ro cluster(firmcd)
eststo:xtreg te_acf wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd) 
 
**Levinsohn and Petrin method
eststo: xtreg te_lp wsblk i.t, ro cluster(firmcd)
eststo: xtreg te_lp wsflk i.t, ro cluster(firmcd)
eststo: xtreg te_lp wsh i.t, ro cluster(firmcd)
 
eststo: xtreg te_lp wsblk wsflk wsh i.t, ro cluster(firmcd)
eststo: xtreg te_lp wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd)   

esttab using baseline.csv, se r2 ar2  star(* .1 ** .05 *** .01) replace
eststo clear
  
 *********************************
 *Wooldridge method
 prodest lny, free(bxl) state(bxk) proxy(bxm) va met(wrdg) poly(2) id(firmcd) t(year) 
 
 predict tfp_w, residuals
 predict te_w, omega
 sum tfp_w te_w
 
 
reg tfp_w wsblk, cluster(firmcd)
xtreg tfp_w wsflk i.t, re 
xtreg tfp_w wsh i.t, fe cluster(firmcd)
 
xtreg tfp_w wsblk wsflk wsh i.t, ro cluster(firmcd)
xtreg tfp_w wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd)
 
 
*MrEst method
 prodest lny, free(bxl) state(bxk) proxy(bxm ) va met(mr) lags(1) poly(2) max(100) id(firmcd) t(year) fsresiduals(mr)
 predict tfp_mr, residuals
 predict te_mr, omega
 sum tfp_mr te_wmr


***TRANSLOG PRODUCTION FUNCTION
*sfpanel: Stochastic frontier models for panel data
*Variables for constraints of the optimization problem solving

gen bxkk = 0.5*lnk*lnk 
gen bxll = 0.5*lnl*lnl
gen bxmm = 0.5*lnm*lnm
gen bxtt = 0.5*Trend*Trend

gen bxkl = lnk*lnl
gen bxkm = lnk*lnm
gen bxlm = lnl*lnm

gen bxtk = Trend*lnk
gen bxtl = Trend*lnl
gen bxtm = Trend*lnm

global xvar_tlsfp bxk bxl bxm bxt bxkk bxll bxmm bxtt bxkl bxkm bxlm bxtk bxtl bxtm
eststo SFATranslog_SFPanel: sfpanel lny $xvar_tlsfp, technique(bhhh 20 nr 20) iterate(1000) model(bc95) dist(tnormal) robust cluster(manufacturingcd)

*estimates of technical efficiency via E[exp(-u)|e] (Battese and Coelli, 1988)
predict te_tr, bc 
br firmcd t te_tr

save FDISpillover1RCInd, replace

summarize te_cb te_tr blk wsblk flk wsflk h wsh size equityratio wsequityratio wsdftotalrev wsdffxassets wsemployees wsdfmaterial
tabstat te_cb te_tr blk wsblk flk wsflk wsh size equityratio wsequityratio wsdftotalrev wsdffxassets wsemployees wsdfmaterial, stat(iqr q)


xtreg te_tr wsblk i.t, re cluster(firmcd)
xtreg te_tr  wsflk  i.t, re cluster(firmcd)
xtreg te_tr  wsh i.t, re cluster(firmcd)

xtreg te_tr wsblk wsflk wsh i.t, fe cluster(firmcd)
xtreg te_tr wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth  i.t, re i(firmcd)

***ROBUSTNESS
eststo clear
*SFACobbDouglas
eststo: xtreg te_cb wsblk i.t, ro cluster(firmcd)
eststo: xtreg te_cb  wsflk  i.t, ro cluster(firmcd)
eststo: xtreg te_cb  wsh i.t, ro cluster(firmcd)

eststo: xtreg te_cb wsblk wsflk wsh i.t, ro cluster(firmcd)
eststo: xtreg te_cb wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth  i.t, ro cluster(firmcd) 

***TRANSLOG PRODUCTION FUNCTION
eststo: xtreg te_tr wsblk i.t, ro cluster(firmcd)
eststo: xtreg te_tr  wsflk  i.t, ro cluster(firmcd)
eststo: xtreg te_tr  wsh i.t, ro cluster(firmcd)

eststo: xtreg te_tr wsblk wsflk wsh i.t, ro cluster(firmcd)
eststo: xtreg te_tr wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth i.t, ro cluster(firmcd)
 
esttab using robust.csv, se r2 ar2  star(* .1 ** .05 *** .01) replace
eststo clear

***********************************
sfpanel lny $xvar, technique(bhhh 20) model(bc95) emean(wsblk) dist(tnormal) cluster(manufacturingcd)
sfpanel lny $xvar, technique(bhhh 20 nr 20) model(bc95) emean(wsflk) dist(tnormal) cluster(manufacturingcd)
sfpanel lny $xvar, technique(bhhh 20 nr 20) model(bc95) emean(wsflk) dist(tnormal) cluster(manufacturingcd)
sfpanel lny $xvar, technique(bhhh 20 nr 20) model(bc95) emean(wsblk) dist(tnormal) cluster(manufacturingcd)
sfpanel lny $xvar, technique(bhhh 20) model(bc95) emean(wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth) dist(tnormal) cluster(manufacturingcd)

************************************

sfpanel lny $xvar_tlsfp, technique(nr 20 bhhh 20 ) iterate(1000) model(bc95) emean(wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth) dist(tnormal) robust cluster(manufacturingcd)


******REGRESSION
**CB
eststo RgCobbD_REG: reg te_cb wsblk wsflk wsh i.t, ro cluster(firmcd)
eststo RgCobbD_REG2: reg te_cb wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth dyr1216 i.t, ro cluster(firmcd)

eststo RgCobbD_2SLS: ivregress 2sls te_cb (wsh = femaleemp province) size wsequityratio dexp dimp dindpark dezmiddle dezsouth dyr1216 i.t, robust first
eststo RgCobbD_2SLS2: ivregress 2sls te_cb (wsblk = femaleemp province) size wsequityratio dexp dimp dindpark dezmiddle dezsouth dyr1216 i.t, robust first
eststo RgCobbD_2SLS3: ivregress 2sls te_cb (wsflk = wsdftotalassets wsdfinventory) size wsequityratio dexp dimp dindpark dezmiddle dezsouth dyr1216 i.t, robust first

eststo RgCobbD_2SLS4: ivregress 2sls te_cb (wsblk wsflk wsh = femaleemp province ward wsdfinventory) size wsequityratio dexp dimp dindpark dezmiddle dezsouth dyr1216 i.t, robust first

eststo RgCobbD_2SLSC: ivreg2 te_cb (wsblk wsflk wsh = femaleemp province ward firmtype wsdftotalassets ) size wsequityratio dexp dimp dindpark dezmiddle dezsouth dyr1216 i.t, robust cluster(firmcd) endog(wsblk wsflk wsh) first

**TR
eststo RgTlog_REG: reg te_tr wsblk wsflk wsh i.t, ro cluster(firmcd)
eststo RgTlog_REG2: reg te_tr wsblk wsflk wsh size wsequityratio dexp dimp dindpark dezmiddle dezsouth dyr1216 i.t, ro cluster(firmcd)

eststo RgTlog_2SLS: ivregress 2sls te_tr (wsh = wsdfexp wsdfimp) size wsequityratio dexp dimp dindpark dezmiddle dezsouth dyr1216 i.t, robust first
eststo RgTlog_2SLS2: ivregress 2sls te_tr (wsblk = wsdfreceivable wsdftotalcost) size wsequityratio dexp dimp dindpark dezmiddle dezsouth dyr1216 i.t, robust first
eststo RgTlog_2SLS3: ivregress 2sls te_tr (wsflk = wsdfinventory wsdfexp) size wsequityratio dexp dimp dindpark dezmiddle dezsouth dyr1216 i.t, robust first

eststo RgTlog_2SLS4: ivregress 2sls te_tr (wsblk wsflk wsh = wsdftotalasset wsdfinventory wsdfexp imp) size wsequityratio dexp dimp dindpark dezmiddle dezsouth dyr1216 i.t, robust first

eststo RgTlog_2SLSC: ivreg2 te_tr (wsblk wsflk wsh = wsdftotalasset wsdfinventory exp imp impfdi) size wsequityratio dimp dexp dindpark dezmiddle dezsouth dyr1216 i.t, robust cluster(firmcd) endog(wsblk wsflk wsh) first

*eststo RgTlog_SW: simarwilson te_tr wsblk wsflk size wsequityratio dexp dimp dindpark dezmiddle dezsouth dyr1216DYEAR*, cinormal reps(1999) dots

mata : st_numscalar("direxists", direxists("reports"))
if !scalar(direxists) {
	mkdir "reports"
}

copy LatexOutputTemplate_port.tex reports/SFA.tex, replace
copy LatexOutputTemplate_land.tex reports/REGRESSIONS.tex, replace

*esttab SFA* using reports/SFA.tex, append starlevels(* 0.10 ** 0.05 *** 0.01) se title("Stochastic Frontier Analysis") mtitles longtable page
*esttab SFA* using reports/SFA.csv, append starlevels(* 0.10 ** 0.05 *** 0.01) se title("Stochastic Frontier Analysis") mtitles longtable page

esttab Rg* using reports/REGRESSIONS2.tex, append starlevels(* 0.10 ** 0.05 *** 0.01) se title("TE Analysis") mtitles longtable page
esttab Rg* using reports/REGRESSIONS2.csv, append starlevels(* 0.10 ** 0.05 *** 0.01) se title("TE Analysis") mtitles longtable page
