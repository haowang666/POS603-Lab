version 11.0
#delimit ;
log using "C:\matt\publications\jop2\webpage\interaction1.log", replace;
set more off;

*     ***************************************************************** *;
*     ***************************************************************** *;
*       File-Name:      interaction1.do                                 *;
*       Date:           1/6/2013                                        *;
*       Author:         MG                                              *;
*       Purpose:        Do-file to produce a marginal effect plot for   *;
*                       X when the interaction model is:                *;
*                       Y = b0 + b1X + b2Z + b3XZ + e                   *;
*       Input File:     alexseev.dta 	                                *;
*       Output File:    interaction1.log                                *;
*       Data Output:    None	                                        *;
*       Previous file:  None                                            *;
*       Machine:        laptop                                          *;
*     ****************************************************************  *;
*     ****************************************************************  *;

use "C:\matt\publications\jop2\webpage\alexseev.dta", clear;

*     ****************************************************************  *;
*     ****************************************************************  *;
*	  Get stuff ready to produce a marginal effect for X.               *;
*     ****************************************************************  *;
*     ****************************************************************  *;

set obs 10000;

regress xenovote slavicshare changenonslav slavicshare_changenonslav inc9903 eduhi02 unemp02 apt9200 vsall03 brdcont, cluster(region);

matrix b=e(b);
matrix V=e(V);

scalar b1=b[1,1];
scalar b3=b[1,3];

scalar varb1=V[1,1];
scalar varb3=V[3,3];

scalar covb1b3=V[1,3];

scalar list b1 b3 varb1 varb3 covb1b3;

*     ****************************************************************  *;
*       Calculate data necessary for top marginal effect plot.          *;
*     ****************************************************************  *;

generate MVZ=((_n-200)/100);

replace  MVZ=. if _n>1501;

gen conbx=b1+b3*MVZ if _n<1501;

gen consx=sqrt(varb1+varb3*(MVZ^2)+2*covb1b3*MVZ) if _n<1501;

gen ax=1.96*consx;

gen upperx=conbx+ax;

gen lowerx=conbx-ax;

*     ****************************************************************  *;
*       Construct stuff to produce the rug plot.  Need to create an     *;
*       offset position for the pipe marker, which will depend on       *;
*       where the histogram sits on the y-axis. This will requie some   *;
*       trial and error.                                                *;
*     ****************************************************************  *;

gen where=-0.045;

gen pipe = "|";

egen tag_nonslav = tag(changenonslav);

*     ****************************************************************  *;
*       Construct variable to produce y=0 line.                         *;
*     ****************************************************************  *;

gen yline=0;

*     ****************************************************************  *;
*     ****************************************************************  *;
*       Produce marginal effect plot for X.                             *;
*     ****************************************************************  *;
*     ****************************************************************  *;

graph twoway hist changenonslav, width(0.5) percent color(gs14) yaxis(2)
		||   scatter where changenonslav if tag_nonslav, plotr(m(b 4)) ms(none) mlabcolor(gs5) mlabel(pipe) mlabpos(6) legend(off)
	    ||   line conbx   MVZ, clpattern(solid) clwidth(medium) clcolor(black) yaxis(1)
        ||   line upperx  MVZ, clpattern(dash) clwidth(thin) clcolor(black)
        ||   line lowerx  MVZ, clpattern(dash) clwidth(thin) clcolor(black)
        ||   line yline  MVZ,  clwidth(thin) clcolor(black) clpattern(solid)
	    ||   ,
             xlabel(-2 0 2 4 6 8 10 12, nogrid labsize(2))
		     ylabel(-0.05 0 .05 .1 .15 .2 .25, axis(1) nogrid labsize(2))
		     ylabel(0 2 4 6 8 10 12 14 16, axis(2) nogrid labsize(2))
	         yscale(noline alt)
		     yscale(noline alt axis(2))	
             xscale(noline)
             legend(off)
             xtitle("" , size(2.5)  )
             ytitle("" , axis(2) size(2.5))
             xsca(titlegap(2))
             ysca(titlegap(2))
			 scheme(s2mono) graphregion(fcolor(white) ilcolor(white) lcolor(white));

log close;






