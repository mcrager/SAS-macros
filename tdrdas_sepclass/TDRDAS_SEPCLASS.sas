/***********************************************************************************************************************

Program Name           : TDRDAS_SEPCLASS.sas

Path                   : 

Program Language       : SAS

Purpose                : Given a set of predictors with estimates (and standard errors) of the numerical degree of
                         association of each with clinical outcome or state, TDRDAS_SEPCLASS performs a separate class 
                         true discovery rate degree of association (TDRDA) set analysis and produces an output data set
                         containing class identifier, predictor name, the maximum lower bound (MLB) absolute degree of association
                         for which each predictor will be included in a TDRDA set, the direction (positive or negative) of association,
                         and a regression-to-the-mean-corrected estimate of the degree of association.  The program also produces
                         a TDRDA bar chart showing the MLB and RM-corrected estimates for all identified predictors.  At the user's 
                         option, this bar chart may be sorted by class or not.
                         
Notes                  : 

Validated              : Tested and verified

Run Dependencies       :  None

Input Datasets/Views   : Specified SAS data set indsn containing predictor name, class identifier, estimate of degree of association (such as log hazard ratio
                         or log odds ratio), and standard error of the estimate.

Output Datasets/Views  : Specified SAS data set outdsn containing maximum absolute association for which each predictor will be included in a TDRDA
                         set, direction of association, and a regression-to-the-mean-corrected estimate of the degree of association
                         

Other Output           : A graph saved as a PNG file containing a bar chart of the results.  Multiple graphs will be produced if the number
                         of associated predictors exceeds 150.

Macro calls internal   : Tmp_SubLibL, SEPCLASS_THETA

Macro calls external   : None

***********************************************************************************************************************/
%macro TDRDAS_SEPCLASS(
       /* Input Specification*/    indsn=,predictorname=,class=,estimate=,stderr=,estzero=,
       /* Analysis Parameters */   accuracy=0.001,oneminusq=0.9,lambda=0.5,
       /* Output Specification */  outdsn=, transformx=exp(x),MLBvar=MLB,RMCEstvar=RMCEst,
       /* Graph Options*/ refinterval=.05, measure=Degree of Association, graphlabel=%str(Standardized Degree of Association of Predictor with Outcome),
                          goutpath=, graphname=,maxpredictorsppg=150, graphcolor1=black, graphcolor2=gray,sortbyclass=yes,
                          class_value_graphname=
              );

/***********************************************************************************************
| DEFINITION OF PARAMETERS
|
| Name           : indsn
| Required (Y/N) : Y
| Default Value  : 
| Type ($/#)     : $
| Purpose        : Input data set name
|
|-----------------------------------------------------------------------------------------------
|
| Name           : predictorname
| Required (Y/N) : Y
| Default Value  :
| Type ($/#)     : $
| Purpose        : Name of variable that contains predictor names 
|
|-----------------------------------------------------------------------------------------------
|
| Name           : class
| Required (Y/N) : R
| Default Value  :
| Type ($/#)     : $ or #
| Purpose        : Name of variable that contains class identifiers.  The variable can be 
|                  character or numeric.  
|
|-----------------------------------------------------------------------------------------------
|
| Name           : estimate
| Required (Y/N) : Y
| Default Value  : 
| Type ($/#)     : $
| Purpose        : Name of input data set variable that contains association estimates. 
|
|-----------------------------------------------------------------------------------------------
|
| Name           : stderr
| Required (Y/N) : Y
| Default Value  : 
| Type ($/#)     : $
| Purpose        : Name of input data set variable that contains association estimate standard errors. 
|
|-----------------------------------------------------------------------------------------------
|
| Name           : estzero
| Required (Y/N) : Y
| Default Value  : 
| Type ($/#)     : #
| Purpose        : Value of the association estimate that corresponds to no association.  If the association
|                  is measured by log hazard ratio or log odds, then estzero = 0. 
|
|-----------------------------------------------------------------------------------------------
|
| Name           : accuracy
| Required (Y/N) : N
| Default Value  : 0.001
| Type ($/#)     : #
| Purpose        : Accuracy to which MLB degree of association is calculated.  The result for for the variable
|                  MLB will be accurate to within +/- the specified value.
|
|-----------------------------------------------------------------------------------------------
|
| Name           : oneminusq
| Required (Y/N) : N
| Default Value  : 0.9
| Type ($/#)     : #
| Purpose        : True discovery rate.  Equal to one minus the acceptable false discovery rate. 
|
|-----------------------------------------------------------------------------------------------
|
| Name           : lambda
| Required (Y/N) : N
| Default Value  : 0.5
| Type ($/#)     : #
| Purpose        : Tuning parameter lambda for use in computation of Storey's estimate of the proportion
|                  of true null hypotheses.  The value 0.5 is recommended.  Values near 0 or 1 may
|                  results in increased variability in the calculation.  
|
|-----------------------------------------------------------------------------------------------
|
| Name           : outdsn
| Required (Y/N) : Y
| Default Value  : (at temporary library)
| Type ($/#)     : $
| Purpose        : Libname reference and the output data set name.  The dataset 
|                  name must conform to the rules for SAS names.   
|
|-----------------------------------------------------------------------------------------------
|
| Name           : transformx
| Required (Y/N) : N
| Default Value  : exp(x)
| Type ($/#)     : $
| Purpose        : Transformation of the estimates to make graph more understandable.  The parameter must
|                  include an "(x)" with no spaces between x and each parenthesis.  Everytime x appears                     
|                  in the function, it must be encased in parentheses.  
|-----------------------------------------------------------------------------------------------
|
| Name           : refinterval
| Required (Y/N) : N
| Default Value  : .05
| Type ($/#)     : $
| Purpose        : Spacing of reference lines on the graph.  Used in the axis statement as in 
|                     axis1 order=(1 to 1.75 by &refinterval.)
|
|-----------------------------------------------------------------------------------------------
|
| Name           : MLBvar
| Required (Y/N) : N
| Default Value  : MLB
| Type ($/#)     : $
| Purpose        : Name of output data set variable that will contain the maximum lower bound (MLB)
|                  (transformed) degree of association.  
|
|-----------------------------------------------------------------------------------------------
|
| Name           : RMCEstvar
| Required (Y/N) : N
| Default Value  : RMCest
| Type ($/#)     : $
| Purpose        : Name of output data set variable that will contain estimate of (transformed) degree
|                  of association corrected for regression to the mean.  
|
|-----------------------------------------------------------------------------------------------
|
| Name           : measure
| Required (Y/N) : N
| Default Value  : Degree of Association
| Type ($/#)     : $
| Purpose        : The measure of the degree of association. This is used in 
|                  the legend for the graph and in variable labels.  If you change this,
|                  you will also need to change the value for graphlabel.  The value selected 
|                  for this parameter is case sensitive.
|
|-----------------------------------------------------------------------------------------------
|
| Name           : graphlabel
| Required (Y/N) : N
| Default Value  : Standardized Degree of Association for Predictor with Recurrence
| Type ($/#)     : $
| Purpose        : This parameter allows the user to modify the label on the graph.
|                  
|-----------------------------------------------------------------------------------------------
|
| Name           : goutpath
| Required (Y/N) : Y if graphname specified.
| Default Value  : .
| Type ($/#)     : $
| Purpose        : The path for the directory into which
|                  the graph will be saved.
|
|-----------------------------------------------------------------------------------------------
|
| Name           : graphname
| Required (Y/N) : N
| Default Value  : TDRDAS
| Type ($/#)     : $
| Purpose        : If this parameter is specified, a TDRDAS graph will be produced using the specified
|                  value as a file name.  The file extension will be added by the macro.
|
|-----------------------------------------------------------------------------------------------
|
| Name           : maxpredictorsppg
| Required (Y/N) : N
| Default Value  : 150
| Type ($/#)     : #
| Purpose        : The maximum number of predictors to be shown on each graph. The value selected should be in 
|                  the range of 1-150. 
|
|----------------------------------------------------------------------------------------------- 
|
| Name           : graphcolor1
| Required (Y/N) : N
| Default Value  : black
| Type ($/#)     : $
| Purpose        : The color of the bars on the graph representing the predictors with negative association.  
| 
|-----------------------------------------------------------------------------------------------
|
| Name           : graphcolor2
| Required (Y/N) : N
| Default Value  : gray
| Type ($/#)     : $
| Purpose        : The color of the bars on the graph representing the predictors with positive association.  
|-----------------------------------------------------------------------------------------------
|
| Name           : sortbyclass
| Required (Y/N) : N
| Default Value  : yes
| Type ($/#)     : $
| Purpose        : If this variable is set to yes, the TDRDA set bar chart is sorted first by class, then  
|                  by MLB and RM-corrected estimate.  If no, the TDRDA set bar chart is sorted only by 
|                  MLB and RM-corrected estimate.
|
|-----------------------------------------------------------------------------------------------
|
| Name           : class_value_graphname
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : Optional filename of graph of logistic regression estimates of class probabilities
|                  given z-score values.  These plots can be useful to make sure the cubic spline fit is reasonable,
|                  and to show which classes are enriched for truly associated genes.  Also, the slope of the fitted lines
|                  should be close to zero around z=0.  A substantial departure from this indicates that the null 
|                  distribution of the test statistic is not the same across classes, which suggests a possible study design 
|                  or conduct issue.  If this parameter is not specified, no graph is produced.
|
**********************************************************************************************************/;

 %local t EstZeroMult MaxEstMult fontsize numpages maxpredictorsppg j flag legendlocation xmax firstobs obs
        npredictor k vartype maxlength;


%macro Tmp_SubLibL(tmplib=);

   %local temp;
 
   %let workpath=%sysfunc(pathname(WORK));
 
   %let temp = %trim(%bquote(&workpath.))\%left(&tmplib.);
   
   %if %sysfunc(fileexist(&temp.)) = 0 %then %do;
      systask command "mkdir ""&temp.""" wait;
   %end;
   %if %sysfunc(libref(&tmplib.)) %then %do;
      libname &tmplib. "&temp." filelockwait=60;
   %end;

%mend Tmp_SubLibL;

/* Generate a subdir at &workspace for any temp dataset.*/

   %let t = _tdrdasc;
   
   %Tmp_SubLibL(tmplib=&t.);

/*  Capture status of mergenoby option so it can be reset to its current value at the end of the macro */

proc optsave out=&t..options;
run;

proc sql noprint;
    select OPTVALUE into :mergenobyoption
    from &t..options where (OPTNAME = 'MERGENOBY');
quit;

options mergenoby=nowarn;



/***********************************************************************************************************************/

%macro sepclass_theta(
       /* Input Specification*/    indsn=,predictorname=,class=,estimate=,stderr=,pvalue=,direction=,
       /* Analysis Parameters */   lambda=0.5,theta=,
       /* Output Specification */  outdsn=,overallq=,sepclassq=,p0class=,
                                   goutpath=, class_value_graphname=
              );

%let trace = *;  /* Trace variables to debug */

%local classname1 classname2 classname3 classname4 classname5 classname6 classname7;

/************************************************************************/
/*  Count number of genes and number of classes in data set             */
/************************************************************************/

proc sql noprint;
    select count(distinct &predictorname) into :ngene from &indsn;
quit; 
run;

proc sql noprint;
    select count(distinct &class) into :k from &indsn;
quit;
run;

%let k = %left(&k);

%if &k = 1 %then %do;
      %put ERROR: GHI Note: SEPCLASS macro found only 1 class (variable &class) in data set &indsn..;
      %abort;
   %end;

/************************************************************************/
/*  Create numerical class variable and determine proportion of genes   */
/*  in each class.                                                      */
/************************************************************************/

%let dsid=%sysfunc(open(&indsn,is));
%let varid=%sysfunc(varnum(&dsid,&class));
%let vartyp=%sysfunc(vartype(&dsid,&varid));
%if &dsid>0 %then %let zclose=%sysfunc(close(&dsid));

data &t..indata;
      set &indsn;
      a = 1;   /*  Dummy variable for merges  */
run;

/************************************************************************/
/*  Compute q-values for the overall analysis (ignoring classes)        */
/************************************************************************/

proc sort data=&t..indata;
     by &pvalue;
run;

/* Estimate pi0, the proportion of interval hypotheses that truly satisfy the 
   interval null hypothesis */

data &t..pi0;

   set &t..indata end=eof;

   if &pvalue >  &lambda then w = w + 1;

   if eof then do;
       pi0 = min(1,w / ((1-&lambda)*&ngene));  
       output;
       end;

   keep pi0 a;
   retain w 0;
   run;

data &t..combine;
    merge &t..indata &t..pi0;
    by a;

    count = count + increment;
    if &pvalue = 1 then increment = 0; /*  Account for p-values equal to 1 by
                                           setting all empirical CDF values to
                                           the same (conservative) value of 
                                           (m - #(p=1) + 1) / m.  */
    empcdf = count / &ngene;  

    q_overall = pi0 * &pvalue / empcdf;

    retain count 0 increment 1;
    run;

/************************************************************************/
/*  Now compute the modification factors to get the separate class      */
/*  q-values.  Transform the p-values to z-values, accounting for       */
/*  direction of the estimated association.  Then model the probability */
/*  that the observation came from each class using a logistic          */
/*  regression applied to the nominal classes with a cubic spline of    */
/*  the z-statistic as the predictor variables.                         */
/************************************************************************/
      
data &t..combine;
     set &t..combine;

     zplus  = ( &estimate - &theta) / &stderr;
     zminus = (-&estimate - &theta) / &stderr;
         
     if estimate >  &theta then zvalue =  zplus;  else
     if estimate < -&theta then zvalue = -zminus; else
                                zvalue =  0;
     abszvalue = abs(zvalue);          
run;

/*************************************************************************/
/* Determine minimum, maximum, .25, .50, and .75 quantiles of zplus      */
/* and zminus and compute the natural cubic spline basis function values.*/             
/*************************************************************************/

proc univariate data=&t..combine noprint;
     var zplus zminus;
     by a;
     output out=&t..zquant min    = zplusmin zminusmin
                           q1     = zplusq1  zminusq1
                           median = zplusq2  zminusq2
                           q3     = zplusq3  zminusq3
                           max    = zplusmax zminusmax;
run;

&trace. proc print data=&t..zquant;
&trace. title2 data set &t..zquant;
&trace. run;

/***********************************************************************************************/
%macro natspline5(var=,knot1=,knot2=,knot3=,knot4=,knot5=,
                       basis1=,basis2=,basis3=,basis4=);

/*  Macro to compute 4 df natural spline basis function values

**  Parameters:
**  var = Name of variable containing covariate value
**  knot1-knot5 = Positions of the 4 knots
**  basis1-basis4 = Names of variables that will contain the basis function values
*/

    qqlambda2 = (&knot5. - &knot2.) / (&knot5. - &knot1.);
    qqlambda3 = (&knot5. - &knot3.) / (&knot5. - &knot1.);
    qqlambda4 = (&knot5. - &knot4.) / (&knot5. - &knot1.); 
   
    &basis1 = &var;
    &basis2 = max(0,(&var.-&knot2.)**3) - qqlambda2*max(0,(&var.-&knot1.)**3) - (1-qqlambda2)*max(0,(&var.-&knot5.)**3);
    &basis3 = max(0,(&var.-&knot3.)**3) - qqlambda3*max(0,(&var.-&knot1.)**3) - (1-qqlambda3)*max(0,(&var.-&knot5.)**3);
    &basis4 = max(0,(&var.-&knot4.)**3) - qqlambda4*max(0,(&var.-&knot1.)**3) - (1-qqlambda4)*max(0,(&var.-&knot5.)**3);

    drop qqlambda2 qqlambda3 qqlambda3 qqlambda4;

%mend natspline5;
/***************************************************************************************************/

data &t..combine;
     merge &t..combine end=eof &t..zquant;
     by a;

**  Compute 4 df natural spline basis function values for the zplus and zminus scores;

     %natspline5(var=zplus,knot1=zplusmin,knot2=zplusq1,knot3=zplusq2,knot4=zplusq3,knot5=zplusmax,
                            basis1=zplusb1,basis2=zplusb2,basis3=zplusb3,basis4=zplusb4);

     %natspline5(var=zminus,knot1=zminusmin,knot2=zminusq1,knot3=zminusq2,knot4=zminusq3,knot5=zminusmax,
                            basis1=zminusb1,basis2=zminusb2,basis3=zminusb3,basis4=zminusb4);

     zeroflag = 0;
     output;

     if eof then do;

**  Output an extra observation with z-score of exactly 0 and missing class variable.  This
**  observation will be used in estimating the class probabilities at zero.  It does not affect
**  the model fit.;

          zeroflag = 1;
          zplus = 0;
          zvalue = 0;
 
    %natspline5(var=zplus,knot1=zplusmin,knot2=zplusq1,knot3=zplusq2,knot4=zplusq3,knot5=zplusmax,
                            basis1=zplusb1,basis2=zplusb2,basis3=zplusb3,basis4=zplusb4);

    %natspline5(var=zminus,knot1=zminusmin,knot2=zminusq1,knot3=zminusq2,knot4=zminusq3,knot5=zminusmax,
                            basis1=zminusb1,basis2=zminusb2,basis3=zminusb3,basis4=zminusb4);

          nclass = .;
          output;
          end;   
run;

&trace. proc print data=&t..combine;
&trace. title2 Data set &t..combine ready for logistic regression;
&trace. run;

/************************************************************************/
/* Run logistic regression for zplus and zminus scores                  */
/************************************************************************/

proc logistic data=&t..combine noprint;
      model nclass = zplusb1 zplusb2 zplusb3 zplusb4 / link=glogit; 
      output out=&t..logoutplus predprobs=individual;
run;

proc sort data=&t..logoutplus;
      by &predictorname;
run;

data &t..logoutplus;
     array ip(&k) ip_1-ip_&k;
     array PAplus(&k) PAplus1-PAplus&k;

     set &t..logoutplus;
  
     do i = 1 to &k;
        PAplus(i) = ip(i);
        end;
 
     drop i;
run;
     
&trace. proc print data=&t..logoutplus;
&trace. title data set logoutplus;
&trace. run;

proc logistic data=&t..combine noprint;
      model nclass = zminusb1 zminusb2 zminusb3 zminusb4 / link=glogit; 
      output out=&t..logoutminus predprobs=individual;
run;

proc sort data=&t..logoutminus;
      by &predictorname;
run;

data &t..logoutminus;
     array ip(&k) ip_1-ip_&k;
     array PAminus(&k) PAminus1-PAminus&k;

     set &t..logoutminus;
  
     do i = 1 to &k;
        PAminus(i) = ip(i);
        end;
 
     drop i;
run;

&trace. proc print data=&t..logoutminus;
&trace. title data set logoutminus;
&trace. run;

/************************************************************************/
/* Compute the mixture estimates using the zplus and zminus estimates   */
/************************************************************************/

data &t..logout;
      array PAplus(&k)  PAplus1-PAplus&k;
      array PAminus(&k) PAminus1-PAminus&k;
      array PAmix(&k)   PAmix1-PAmix&k;


      merge &t..logoutplus &t..logoutminus;
      by &predictorname;

      if zeroflag = 0 then do;
         wplus = CDF('NORMAL',estimate/stderr,0,1);
         do i = 1 to &k;
            PAmix(i) = wplus * PAplus(i)  +  (1-wplus) * PAminus(i);
            end;
         end;
                      else do;
 /*  Calculate the estimate to use if no association estimates are in [-theta,theta].   */
         do i = 1 to &k;
            PAmix(i) = 0.5 * PAplus(i) + 0.5 * PAminus(i);            
            end;
         zvalue = 0;
         end;
  
       drop i ip_1-ip_&k;
run;

&trace. proc print data=&t..logout;
&trace. title2 data set &t..logout;
&trace. run;

/****************************************************************************************/
/* Capture the proportions of observations in each class.                               */
/****************************************************************************************/

proc sort data=&indsn out=&t..Allz nodupkeys;
     by nclass;
run;

data &t..Allz;
      array Allzclassprop(&k) Allzclassprop1-Allzclassprop&k;
      set &t..Allz end=eof;
      a = 1;
      Allzclassprop(nclass) = classprop;
      if eof then output;
      keep Allzclassprop1-Allzclassprop&k a;
run;

/************************************************************************
** Using predicted probabilities from logistic regression models, estimate
** the conditional probabilities of the various classes given |Z|>=z.     
*************************************************************************/

/************************************************************************
** Start with left tail probabilities
************************************************************************/

proc sort data=&t..logout;
     by zvalue;
run;

data &t..lefttail (keep = zvalueleft abszvalue zprobleft condprobleft1-condprobleft&k a);
 
     array pclass(&k) PAmix1-PAmix&k;
     array lagpclass(&k) lagpclass1-lagpclass&k;
     array runtotleft(&k) runtotleft1-runtotleft&k;
     array condprobleft(&k) condprobleft1-condprobleft&k;

     set &t..logout;
     where zeroflag = 0;  * Do not include the added dummy observation with z = 0;
     
     zvalueleft = zvalue;  /* A non-missing value for this variable will indicate that     */
                           /* that there is a contribution from the left (negative) side   */
                           /* when combining the left and right sides later on.            */
     abszvalue = abs(zvalue);
     nleft = nleft + 1;
     zprobleft = nleft / &ngene;

     if zvalue <= 0 then do;
        do k = 1 to &k;
          runtotleft(k) = runtotleft(k) + pclass(k) / &ngene;
          condprobleft(k) = runtotleft(k) / zprobleft;
          end;
        output;
        end;

      retain runtotleft1-runtotleft&k 0 nleft 0;
run;

&trace. proc print data=&t..lefttail;
&trace. title data set lefttail;
&trace. run;

/************************************************************************
** Now do the right tail probabilities
************************************************************************/

proc sort data=&t..logout;
      by descending zvalue;
run;

data &t..righttail (keep = zvalueright abszvalue zprobright condprobright1-condprobright&k. a);
     array pclass(&k) PAmix1-PAmix&k;
     array lagpclass(&k) lagpclass1-lagpclass&k;
     array runtotright(&k) runtotright1-runtotright&k;
     array condprobright(&k) condprobright1-condprobright&k;

     set &t..logout;
     where zeroflag = 0;  * Do not include the added dummy observation with z = 0;
     
     zvalueright = zvalue;  /* A non-missing value for this variable will indicate that     */
                            /* that there is a contribution from the left (negative) side   */
                            /* when combining the left and right sides later on.            */

     abszvalue = abs(zvalue);
     nright = nright + 1;
     zprobright = nright / &ngene;

     if zvalue >  0 then do;
        do k = 1 to &k;
          runtotright(k) = runtotright(k) + pclass(k) / &ngene;
          condprobright(k) = runtotright(k) / zprobright;
          end;
        output &t..righttail;
        end;
     
      retain runtotright1-runtotright&k 0 nright 0;
run;

&trace. proc print data=&t..righttail;
&trace. title data set righttail;
&trace. run;


/************************************************************************
** Combine left tail and right tail conditional probabilities to get probabilities of each
** class conditional on absolute value of Z>=z
************************************************************************/

proc sort data=&t..lefttail;
     by descending abszvalue;
run;

proc sort data=&t..righttail;
     by descending abszvalue;
run;

data &t..bothtail;

     array condprobright(&k) condprobright1-condprobright&k;
     array condprobleft(&k) condprobleft1-condprobleft&k;
     array condprob(&k) condprob1-condprob&k;

     array cpl(&k) cpl1-cpl&k;
     array cpr(&k) cpr1-cpr&k; 

     merge &t..lefttail &t..righttail;
     by descending abszvalue;

     if zvalueleft ne . then do;
        do k = 1 to &k;
           cpl(k) = condprobleft(k);
           end;
        zpl = zprobleft;
        end;

     if zvalueright ne . then do;
        do k = 1 to &k;
           cpr(k) = condprobright(k);
           end;
        zpr = zprobright;
        end;

    do k = 1 to &k;
        condprob(k) = (zpl * cpl(k) + zpr * cpr(k)) / (zpl + zpr);
        end;

    retain zpl 0 cpl1-cpl&k 0
           zpr 0 cpr1-cpr&k 0;
run;

&trace. proc print data=&t..bothtail;
&trace. title data set bothtail;
&trace. run;

/***********************************************************************************/
/** If the Z statistic is 0, then we are conditioning on an event with probability */
/** 1, so just use the observed class proportions for the conditional probabilities*/
/***********************************************************************************/

data &t..bothtail;
     array Allzclassprop(&k) Allzclassprop1-Allzclassprop&k;
     array condprob(&k) condprob1-condprob&k;

     merge &t..bothtail &t..Allz;
     by a;

    if abszvalue = 0 then do k = 1 to &k;
      condprob(k) = Allzclassprop(k);
      end;
run;   

/************************************************************************
**  Merge the conditional probabilities with the p-value data set
************************************************************************/

proc sort data=&t..combine;
     by descending abszvalue;
run;

data &t..combine;

     array condprobright(&k) condprobright1-condprobright&k;
     array condprobleft(&k) condprobleft1-condprobleft&k;
     array condprob(&k) condprob1-condprob&k;

     merge &t..combine  &t..bothtail ;
     by descending abszvalue;
run;

/************************************************************************
**  Get the class probability estimates for estimates that are less than 
**  or equal to theta in absolute value (estimated class probabilities
**  for the null genes)
************************************************************************/     

/*====take out tcount*/
data &t..zero (keep = pclasszero1-pclasszero&k tcount a); 
     array pclass(&k) PAmix1-PAmix&k;
     array sumpclass(&k) sumpclass1-sumpclass&k;
     array pclasszero(&k) pclasszero1-pclasszero&k;
     array zeroest(&k) zeroest1-zeroest&k;

     set &t..logout  end=eof;

     if zeroflag = 0 and abs(estimate) <= &theta then do;
       tcount = tcount + 1;
       do k = 1 to &k;
           sumpclass(k) = sumpclass(k) + pclass(k);
           end;
       end;

     if zeroflag = 1 then do k = 1 to &k;
  /*  Pick up the default estimate in case there */
  /*  are no estimates in the range [-theta,+theta] */
          zeroest(k) = pclass(k);
          end;

     if eof then do;
       if tcount > 0 then do k = 1 to &k;
          pclasszero(k) = sumpclass(k) / tcount;
          end;
                     else do k = 1 to &k;
          pclasszero(k) = zeroest(k);
          end;
       output;
       end;

     drop k;
                            
     retain sumpclass1-sumpclass&k tcount 0 zeroest1-zeroest&k;    
run;


&trace. proc print data=&t..zero;
&trace. title data set zero;
&trace. run;

/************************************************************************
**  Merge in the estimated class probabilities for the null genes and compute
**  the separate class analysis q-values
************************************************************************/

data &t..sepclass;

     array condprob(&k) condprob1-condprob&k;
     array pclasszero(&k) pclasszero1-pclasszero&k;
     
     merge &t..combine  &t..zero;
     by a;
     
     if zeroflag = 1 then delete;  /* Get rid of extra observation at z=0 */

** Compute separate class q-value;
     
     q_sepclass = q_overall * pclasszero(nclass) / condprob(nclass);

%if %length(&p0class.)>0 %then %do;
** Compute estimate of truly null hypotheses in each class;
     p0class = pclasszero(nclass) * pi0 / classprop;
     label p0class = Estimated proportion of truly null hypotheses in class;
%end;

run;    
      

*proc print data=&t..sepclass;
*     title data set sepclass;
*run;

/************************************************************************
**  Put together output data set
************************************************************************/

data &outdsn;
      set &t..sepclass;

      &overallq = min(q_overall,1);
      label &overallq = Overall (ignoring classes) q-value;
      
      &sepclassq = min(q_sepclass,1);
      label &sepclassq = Separate class analysis q-value;

%if %length(&p0class.)>0 %then %do;
      keep &predictorname &class &pvalue &overallq &sepclassq &p0class &theta;
%end;                   
                          %else %do;
      keep &predictorname &class &pvalue &overallq &sepclassq &theta;
%end;      
run;

proc sort data=&outdsn;
      by &class &sepclassq;
run;

%if %length(&class_value_graphname.)>0 %then %do;

/************************************************************************
**  Plot the estimated logistic regression estimates of the class        
**  probabilities for the user to check.
************************************************************************/

/***********************************************************************/
/**  First, get class names for legend                                 */
/***********************************************************************/

proc sort data=&t..indata out=&t..legendkey (keep = nclass &class) nodupkeys;
    by nclass;
run;

data &t..legendkey;

%if &vartyp=N %then %do;
     array qqclassname(&k) qqclassname1-qqclassname&k;
%end;
%if &vartyp=C %then %do;
     array qqclassname(&k) $ qqclassname1-qqclassname&k;
%end;

    set &t..legendkey end=eof;
    
    qqclassname(nclass) = &class;
    a = 1;
    if eof then output;
   
    retain qqclassname1-qqclassname&k;
    keep   qqclassname1-qqclassname&k a;
run;

data &t..legendkey;

%if &vartyp=N %then %do;
     array qqclassname(&k) qqclassname1-qqclassname&k;
%end;
%if &vartyp=C %then %do;
     array qqclassname(&k) $ qqclassname1-qqclassname&k;
%end;

    set &t..legendkey;

%if &vartyp=N %then %do i = 1 %to &k;
call symput("classname&i.",trim(left(put(qqclassname(&i),best.))));
%end; 

%if &vartyp=C %then %do i = 1 %to &k;
call symput("classname&i.",trim(left(qqclassname(&i))));
%end; 

run;


data &t..forplot;

     array pclasszero(&k) pclasszero1-pclasszero&k;
     array PAmix(&k) PAmix1-PAmix&k;

%if &vartyp=N %then %do;
     array qqclassname(&k) qqclassname1-qqclassname&k;
%end;
%if &vartyp=C %then %do;
     array qqclassname(&k) $ qqclassname1-qqclassname&k;
%end;

    merge &t..logout &t..legendkey &t..zero;
    by a;

/***  Get rid of added observation with z = 0  ***/
    if nclass = . then delete;  
    
/***  Plot the interval null hypothesis class probability estimates ***/
/***  for the plot at Z=0.                                          ***/
    if zvalue = 0 then do k = 1 to &k;
        PAmix(k) = pclasszero(k);
        end;

%do i = 1 %to &k;
    label PAmix&i = "Pr{Class=&&classname&i|Z score}";
%end;

     drop k;
run;

/********************************************************************************/
/**  Now generate the plot                                                      */
/********************************************************************************/     

filename gout "&goutpath.\&class_value_graphname..PNG";
 
goptions reset=all  
    device=PNG
    ftext="Arial Rounded MT Bold"
    htext=10 pt
    gsfmode=replace
    gsfname=gout;

axis1 label=("Z Score");
axis2 label=("Probability" position=top justify=right);
legend1 label=NONE;
     
proc gplot data=&t..forplot;
    plot (PAmix1-PAmix&k) * zvalue / overlay legend=legend1 href=0
                                     haxis=axis1 vaxis=axis2;
run; 

%end;

%mend sepclass_theta;

/**********************************************************************************************
**  Main macro.                                                                               *
**********************************************************************************************/


/**********************************************************************************************
**  Set default values of parameters not specified.                                           *
**********************************************************************************************/

%if %length(&accuracy.) = 0 %then %let accuracy = 0.001;
%if %length(&oneminusq.) = 0 %then %let oneminusq = 0.9;
%if %length(&lambda.) = 0 %then %let lambda=0.5;
%if %length(&transformx.) = 0 %then %let transformx = exp(x);
%if %length(&MLBvar.) = 0 %then %let MLBvar = MLB;
%if %length(&RMCEstvar.) = 0 %then %let RMCEstvar = RMCEst;
%if %length(&measure.) = 0 %then %let measure = Degree of Association;
%if %length(&graphlabel.) = 0 %then %let graphlabel = Standardized Degree of Association of Predictor with Outcome;
%if %length(&maxpredictorsppg.) = 0 %then %let maxpredictorsppg = 150;
%if %length(&graphcolor1.) = 0 %then %let graphcolor1 = black;
%if %length(&graphcolor2.) = 0 %then %let graphcolor2 = gray;


/**********************************************************************************************
**  Check for errors in macro parameter specification.                                        *
**********************************************************************************************/

   %if %length(&estzero.)=0 %then %do;
      %put ERROR : TDRDAS_SEPCLASS macro parameter estzero must be specified.;
      %abort;
   %end;

   %if %sysfunc(sign(&lambda.))=-1 %then %do;
      %put ERROR : TDRDAS_SEPCLASS macro parameter lambda must be >=0 and <1.;
      %abort;
   %end;

   %if %sysevalf(&lambda. < 0) or %eval(&lambda >= 1) %then %do;
      %put ERROR : TDRDAS_SEPCLASS macro parameter lambda must be >=0 and <1.;
      %abort;
   %end;

   %if %sysevalf(&lambda. < 0.2) or %sysevalf(&lambda > 0.8) %then %do;
      %put WARNING : TDRDAS_SEPCLASS macro parameter lambda is set at &lambda.  Lambda of 0.5 may perform better.;
   %end;

   %if %sysfunc(sign(&oneminusq.))=-1 %then %do;
      %put ERROR : TDRDAS_SEPCLASS macro parameter oneminusq must be >0 and <1.;
      %abort;
   %end;
 
   %if %sysevalf(&oneminusq. <= 0) or %sysevalf(&oneminusq >= 1) %then %do;
      %put ERROR :TDRDAS_SEPCLASS macro parameter oneminusq must be >0 and <1.;
      %abort;
   %end;

   %if %sysevalf(&accuracy. <= 0) %then %do;
      %put ERROR :  TDRDAS_SEPCLASS macro parameter accuracy must be >0.;
      %abort;
   %end;

   %if %length(&graphname.) > 0 and %length(&goutpath.) = 0 %then %do;
      %put ERROR :  TDRDAS_SEPCLASS macro:  specification of parameter goutpath is required when parameter graphname is specified.;
      %abort;
   %end;

   %if %length(&class.)=0 %then %do;
      %put ERROR :  TDRDAS_SEPCLASS macro parameter class must be specified.;
      %abort;
   %end;

/**********************************************************************************************
**  Check for nonexistent input data sets and nonexistent variables in the input data sets.   *
**********************************************************************************************/

  %let errcode = 0;
  %let dsid = %sysfunc(open(&indsn.,i));
  %if &dsid. = 0 %then %do;
     %put ERROR : TDRDAS_SEPCLASS macro found that specified input data set &indsn. does not exist.;
     %abort;
     %end;
   %else %do;
     %if %sysfunc(varnum(&dsid.,&predictorname.)) = 0 %then %do;
            %put ERROR : TDRDAS_SEPCLASS macro input data set &indsn. does not contain the variable &predictorname. specified in parameter predictorname.;
            %let errcode = 1;
            %end;
     %if %sysfunc(varnum(&dsid.,&estimate.)) = 0 %then %do;
            %put ERROR : TDRDA_SEPCLASSS macro input data set &indsn. does not contain the variable &estimate. specified in parameter estimate.;
            %let errcode = 1;
            %end;
     %if %sysfunc(varnum(&dsid.,&stderr.)) = 0 %then %do;
            %put ERROR : TDRDA_SEPCLASSS macro input data set &indsn. does not contain the variable &stderr. specified in parameter stderr.;
            %let errcode = 1;
            %end;
     %if %sysfunc(varnum(&dsid.,&class.)) = 0 %then %do;
            %put ERROR : TDRDA_SEPCLASSS macro input data set &indsn. does not contain the variable &class. specified in parameter class.;
            %let errcode = 1;
            %end;

     %let rc = %sysfunc(close(&dsid.));
     %if &errcode. = 1 %then %abort;
     %end;


/************************************************************************/
/*  Count number of predictors and number of classes in data set.       */
/************************************************************************/

proc sql noprint;
    select count(distinct &predictorname) into :npredictor from &indsn;
quit; 
run;

proc sql noprint;
    select count(distinct &class) into :k from &indsn;
quit;
run;

%let k = %left(&k);

%if &k = 1 %then %do;
      %put ERROR: GHI Note: TDRDAS_SEPCLASS macro found only 1 class (variable &class) in data set &indsn..;
      %abort;
   %end;

/************************************************************************/
/*  Capture required statistics from input data set.  Create numerical  */
/*   class variable and determine proportion of predictors in each class.    */
/************************************************************************/

%let dsid=%sysfunc(open(&indsn,is));
%let varid=%sysfunc(varnum(&dsid,&class));
%let vartyp=%sysfunc(vartype(&dsid,&varid));
%if &dsid>0 %then %let zclose=%sysfunc(close(&dsid));

data &t..allest;
    set &indsn (rename=(&estimate=Estimate &stderr=StdErr));
    VarEst = StdErr * StdErr;

    AbsDiffEstZero = abs(Estimate-&estzero.);   /*  This will be used to find the largest
                                                absolute difference from the estimate value
                                                representing no association */

    dummyby = 1;  *  Dummy variable for merges;

    keep Estimate StdErr VarEst AbsDiffEstZero &predictorname &class dummyby;
run;

proc sort data=&t..allest;
     by &class;

/*  Delete any observations with a missing class variable.    */

%if &vartyp=N %then %do;
     where &class ne .;
%end;
%if &vartyp=C %then %do;
     where &class ne ' ';
%end;

run;


data &t..allest (drop = classprop)
     &t..classcount (keep = &class classprop);

     set &t..allest;
     by &class;
 
     if first.&class then do;
         nclass + 1;
         classcount = 0;
         end;

      classcount = classcount + 1;

      if last.&class then do;
         classprop = classcount / &npredictor;
         label classprop = Proportion of predictors in class;
         output &t..classcount;
         end;

     output &t..allest;

     retain nclass classcount;
run;

data &t..allest;
      merge &t..allest &t..classcount;
      by &class;
run;


proc univariate data=&t..allest noprint;
/* Compute sample variance of degree of association estimates and the mean of the estimate variances */
     by &class;
     var Estimate VarEst AbsDiffEstZero dummyby;
     output out=&t..varstat 
            mean = sampmeanest sampmeanvarest meanaz dummyby
            var = sampvarest sampvarvarest varaz vara
            max = MaxEst MaxVarest MaxAbsDiffEstZero Maxa;
run;

data &t..varstat;
     set &t..varstat;
     sigmahatbeta = sqrt(max(sampvarest-sampmeanvarest,0));  * Estimate of within-class population SD of true degrees of association;

**  Create macro variable for minimum estimate value for loop below;

     EstZeroMult = int(&estzero / &accuracy);
     EstZeroText = put(EstZeroMult,6.0);
     call symput('EstZeroMult',EstZeroText);

**  Create macro variable for maximum absolute estimate value for loop below;

     MaxAbsDiffEstZeroMult = int(MaxAbsDiffEstZero / &accuracy);
     MaxEstText = put(MaxAbsDiffEstZeroMult,6.0);
     call symput('MaxEstMult',MaxEstText);
     
run;

/*****************************************************************************************************/
/*  Compute the regression-to-the-mean-corrected estimate of the degree of association for each      */
/*  predictor.                                                                                       */
/*****************************************************************************************************/

data &t..correctest;
     merge &t..allest &t..varstat;
     by &class;

     AbsEst = abs(Estimate);

     if Estimate > 0 then direction = 1;
     if Estimate < 0 then direction = -1;
     if Estimate = 0 then direction = 0;

     &RMCEstvar = sampmeanest + ((sigmahatbeta**2) / (sigmahatbeta**2 + VarEst)) * (Estimate - sampmeanest);
     AbsEstCorrect = abs(&RMCEstvar);

     keep &predictorname &class direction &RMCEstvar AbsEst AbsEstCorrect;

     label direction = Direction of association;
run; 


/*******************************************************************************************************/
/*  Determine the MLB theta for each predictor using separate class FDR analysis.                           */
/*******************************************************************************************************/

%let flag = 1;  * Flag to signal need to continue the loop.  Will be reset to 0
                * when estmult gets high enough that the TDRDA set is null;

%do estmult = &EstZeroMult %to &MaxEstMult %by 1;
   %if &flag = 1 %then %do;

   data &t..pvalue;
         set &t..allest;

/* Compute p-value for the test of the null hypothesis that the estimate is less than or
   equal to theta against the alternative that is greater than theta */

         theta = &estmult * &accuracy;
 
         Waldp = min(1,2*(1-probnorm((abs(Estimate) - theta) / StdErr)));

/*  Compute direction of difference between estimate and +/- theta, which is needed for
    the separate class analysis */

     if Estimate >  theta then dirtheta =  1; else
     if Estimate < -theta then dirtheta = -1; else 
                               dirtheta =  0;
   
         dummyby = 1;   /*  Dummy variable for merge */

         keep Waldp Estimate StdErr VarEst theta &predictorname &class nclass dirtheta classprop dummyby;
     run;

%sepclass_theta(indsn=&t..pvalue,predictorname=&predictorname,class=nclass,estimate=Estimate,stderr=StdErr,
                pvalue=Waldp,direction=dirtheta,lambda=&lambda,theta=theta,
                outdsn=&t..sepclass,overallq=overallq,sepclassq=sepclassq,
                goutpath=&goutpath.,
                class_value_graphname=&class_value_graphname.);


data &t..sepclass;
    set &t..sepclass;
    dummyby = 1;  *  Dummy variable for merges;
run;

/* Now identify the TDRDA set for this value of theta */

data &t..sepclass;
    set &t..sepclass end=eof;  

    if sepclassq <= 1 - &oneminusq then nreject = nreject + 1;

/* Determine if TDRDA set is null.  If so, set flag to end loop */

    if eof and nreject = 0 then call symput('flag','0');

    label Waldp = "P-value for interval null hypothesis test";
  
    retain nreject 0;
        
run;

proc append base=&t..alltheta data=&t..sepclass;
run;

%end;
%end;

proc sort data=&t..alltheta;
  /*Determine maximum theta at which each predictor can still be included in a TDRDA set*/
   by  &predictorname theta;
run;

/* data local.tracetheta;
    set &t..alltheta;
run;
*/


data &t..MLB;
   set &t..alltheta;
   by &predictorname;

   if first.&predictorname then do;
        &MLBvar. = .;
        mxflag = 0;
        end;

/*  The MLB is set at the first theta for which the null hypothesis is not rejected.  This is equivalent */
/*  to setting the estimated FDR(theta) equal to the max FDR estimate from 0 to theta.                   */

   if sepclassq <= 1 - &oneminusq and mxflag = 0 
          then &MLBvar. = max(&MLBvar.,theta);
          else mxflag = 1; /* Failure to reject null hypothesis detected */

   if last.&predictorname then output;

   retain &MLBvar. mxflag;
   drop mxflag;
run;

proc sort data=&t..correctest;
   by &predictorname;
run;


data &t..output;
/***Create output data set including (transformed) MLB, (transformed) RM-corrected
    estimates, and for graphing, (transformed) RM-corrected estimate of absolute degree 
    of association and the direction of association ******/
   merge &t..MLB &t..correctest;
   by &predictorname;
   %if %length(&transformx) >0 %then %do;
      %let patternid_AEC=%sysfunc(prxparse(s/\(x\)/(AbsEstCorrect)/i));
      txAbsEstCorrect=%sysfunc(prxchange(&patternid_aec.,-1, &transformx));

      %let patternid_MLB=%sysfunc(prxparse(s/\(x\)/(&MLBvar.)/i));
      txMLB=%sysfunc(prxchange(&patternid_MLB.,-1, &transformx));

      if last.&predictorname then do;
         %let patternid_zero=%sysfunc(prxparse(s/\(x\)/(&estzero)/i));
         txEstZero=%sysfunc(prxchange(&patternid_zero.,-1, &transformx));
         call symputx("EstZero",TxEstZero);
      end;
      
   %end;
   %else %do;
      txMLB=&MLBvar.;
      txAbsEstCorrect=AbsEstCorrect;
   %end;

   label &MLBvar. = "Maximum Absolute &measure. for Which Predictor is Included in a &oneminusq. TDRDA Set";
   label txMLB = "Transformed Maximum Absolute &measure. for Which Predictor is Included in a &oneminusq. TDRDA Set"; 
   label &RMCEstvar. = "Estimate of &measure. Corrected for Regression to the Mean";
   label TxAbsEstCorrect= "Transformed Estimate of Absolute &measure. Corrected for Regression to the Mean";
   label direction = "Direction of Association";
run;

proc sort data=&t..output;
%if &sortbyclass = yes %then %do;
   by &class descending txMLB descending AbsEstCorrect &predictorname;
%end;
                       %else %do;
   by descending txMLB descending TxAbsEstCorrect &predictorname;
%end;
run;



proc sql noprint;
   create table &outdsn. as
   select &class, &predictorname., &MLBvar., &RMCEstvar., txMLB, TxAbsEstCorrect, direction
   from &t..output;
quit;


/*******************************************************************************************/
/** Produce TDRDAS graph.                                                                 **/
/*******************************************************************************************/

%if %length(&graphname.) %then %do;

/*  Decide issues regarding the number of pages of output */
proc sql noprint;
   select count(distinct &predictorname) into :nrows from &outdsn
   where txMLB ne .;
quit; 
   
%if &nrows >0 %then %do;   
   
   %let numpages=%sysevalf(&nrows./&maxpredictorsppg.,ceil);  /*calculate the number of pages that will be needed for the graphs*/
   %let maxpredictorsppg=%sysevalf(&nrows./&numpages.,ceil);   /*evenly divide the predictors onto the pages so the output looks consistent*/


   %if &maxpredictorsppg <= 50 %then %let fontsize=8;
   %else %if 50 < &maxpredictorsppg <= 100 %then %let fontsize=6;
   %else %if 100 < &maxpredictorsppg <= 150 %then %let fontsize=4;


   /* In 9.2 the font size looks a bit bigger than in 9.1*/
   %if %substr(&sysver, 1,3) ne 9.1 %then %let fontsize=%sysevalf(&fontsize *0.7);

   proc sort data=&outdsn out=&t..grpdata;
%if &sortbyclass = yes %then %do;
   by &class descending txMLB descending TxAbsEstCorrect &predictorname;
%end;
                       %else %do;
      by descending txMLB descending TxAbsEstCorrect &predictorname;
%end;
   run;

   %let xmin=&estzero;

   proc sql noprint;
      select max(TxAbsEstCorrect) into :xmax from &t..grpdata;
      select max(length(&predictorname.)) into :maxlength from &outdsn;  /* Determine maximum length of label dynamically */
   quit;
   
 /*  Divide data into segments for presentation on bar graph.   There will now be 2 obs per predictor. */   
   proc format;
   value  hazgrpf 9="Maximum lower bound for inclusion in &oneminusq TDRDA set: Predictors with negative association"
                 10="RM-corrected absolute estimate: Predictors with negative association"
                 11="Maximum lower bound for inclusion in &oneminusq TDRDA set: Predictors with positive association"
                 12="RM-corrected absolute estimate: Predictors with positive association";
   run;
   

   data &t..grpdata;
      set &t..grpdata (where=(txMLB ne .));
      hazgrp=10+direction;
      segment=txMLB;
      format hazgrp hazgrpf.;
      output;
      hazgrp=hazgrp+1;
      segment=txAbsEstCorrect-txMLB;
      output;
   run;

   proc sql;
      select min(direction) into :mindir from &t..grpdata;
   quit;
 
   goptions reset=all  
      device=PNG
      ftext="Arial Rounded MT Bold"
      htext=&fontsize pt
      gsfmode=replace
      hsize=6 in
      vsize=9 in
      gsfname=gout;


   %if &mindir=-1 %then %do;
      pattern1 color="&graphcolor1." ;
      pattern3   color="&graphcolor2." ;
      pattern2 value=empty color="&graphcolor1." ;
      pattern4 value=empty color="&graphcolor2.";
   %end;

   %else %if &mindir=1 %then %do;
      pattern1   color="&graphcolor2." ;
      pattern2 value=empty color="&graphcolor2.";
   %end;

   axis1 order=(&xmin. to %sysevalf(&xmax + &refinterval.)  by &refinterval.) label=(f="Arial Rounded MT Bold" h=9 pt "&graphlabel" ) value=(h=.75);
   axis2 label=none value=(j=r); 
   legend1 label=none value=(justify=l height=7.2 pt) across=1; 

   %do j=1 %to &numpages.;

      %if %eval(&numpages.=1) %then filename gout "&goutpath.\&graphname..PNG";
      %else filename gout "&goutpath.\&graphname._&j..PNG";;
   
   /* Subset data for each page of the graph */

      %let firstobs=%eval(1+(&j-1)*2*&maxpredictorsppg);
      %let obs=%eval(&j*2*&maxpredictorsppg.);

      data &t..subset;
         length &predictorname $&maxlength.;
         set &t..grpdata(firstobs=&firstobs obs=&obs.);
      run;

      proc sql noprint;
         select &predictorname into :predictorlist separated by '" "' from &t..subset
         where hazgrp = 9 or hazgrp = 11;         
      quit;
   
      proc gchart data=&t..subset /*anno=&t..leg*/;
         hbar &predictorname / subgroup=hazgrp
              sumvar=segment
              midpoints="&predictorlist"
              autoref
              nostats
              cref=ltgray
              legend=legend1
              maxis=axis2
              raxis=axis1;
      run;

   %end; /*j=1 %to &numpages.*/
   
%end; /*if &nrows >0*/

%else %do;
   %put WARNING : Graph was not produced because there was no predictor with txmlb > . ;
%end;  

%end;


/************************************************************************************
** Clean up data sets and reset mergenoby option to what is was.                    *
************************************************************************************/

proc datasets memtype=data lib=&t. kill nodetails nolist;
run;
quit;

options mergenoby=&mergenobyoption.;

%mend tdrdas_sepclass;
