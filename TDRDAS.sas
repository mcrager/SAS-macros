/***********************************************************************************************************************

Program Name           : TDRDAS.sas

Path                   : 

Program Language       : SAS

Purpose                : Given a set of predictors with estimates (and standard errors) of the numerical degree of
                         association of each with an outcome or state, TDRDAS performs a true discovery rate degree of 
                         association (TDRDA) set analysis and produces an output data set containing predictor name, the maximum  
                         lower bound (MLB) absolute degree of association for which each predictor will be included in a TDRDA set,
                         the direction (positive or negative) of association, and a regression-to-the-mean-corrected estimate
                         of the degree of association.
                         
Notes                  : 

Validated              : Tested and verified

Run Dependencies       :  None

Input Datasets/Views   : Specified SAS data set indsn containing predictor names, estimates of degree of association (such as log hazard ratios
                         or log odds ratios), and standard errors of the estimates.

Output Datasets/Views  : Specified SAS data set outdsn containing maximum absolute association for which each predictor will be included in a TDRDA
                         set, direction of association, and a regression-to-the-mean-corrected estimate of the degree of association
                         

Other Output           : A graph saved as an PNG file containing a bar chart of the results.  Multiple graphs will be produced if the number
                         of associated predictors exceeds 150.

Macro calls internal   : Tmp_SubLibL

Macro calls external   : None

***********************************************************************************************************************/

%macro tdrdas(
       /* Input Specification*/    indsn=,predictorname=,estimate=,stderr=,estzero=,
       /* Analysis Parameters */   accuracy=0.001,oneminusq=0.9,lambda=0.5,
       /* Output Specification */  outdsn=, transformx=exp(x),MLBvar=MLB,RMCEstvar=RMCEst,
       /* Graph Options*/ refinterval=.05, measure=Degree of Association, graphlabel=%str(Standardized Degree of Association of Predictor with Outcome),
                          goutpath=, graphname=,maxpredictorsppg=150, graphcolor1=black, graphcolor2=gray
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
|                  &MLBvar. will be accurate to within +/- the specified value.
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
|
**********************************************************************************************************/;

 %local t npredictor npredictorplus1 EstZeroMult MaxEstMult fontsize numpages maxpredictorsppg j flag xmax firstobs obs patternid_AEC 
   patternid_MLB patternid_zero mindir maxlength;


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

   %let t = _TDRDAS;

   %Tmp_SubLibL(tmplib=&t.);

/*  Capture status of mergenoby option so it can be reset to its current value at the end of the macro */

proc optsave out=&t..options;
run;

proc sql noprint;
    select OPTVALUE into :mergenobyoption
    from &t..options where (OPTNAME = 'MERGENOBY');
quit;

options mergenoby=nowarn;

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
      %put ERROR :  TDRDAS macro parameter estzero must be specified.;
      %abort;
   %end;

   %if %sysfunc(sign(&lambda.))=-1 %then %do;
      %put ERROR :  TDRDAS macro parameter lambda must be >=0 and <1.;
      %abort;
   %end;

   %if %sysevalf(&lambda. < 0) or %eval(&lambda >= 1) %then %do;
      %put ERROR :  TDRDAS macro parameter lambda must be >=0 and <1.;
      %abort;
   %end;

   %if %sysevalf(&lambda. < 0.2) or %sysevalf(&lambda > 0.8) %then %do;
      %put WARNING :  TDRDAS macro parameter lambda is set at &lambda.  Lambda of 0.5 may perform better.;
   %end;

   %if %sysfunc(sign(&oneminusq.))=-1 %then %do;
      %put ERROR :  TDRDAS macro parameter oneminusq must be >0 and <1.;
      %abort;
   %end;
 
   %if %sysevalf(&oneminusq. <= 0) or %sysevalf(&oneminusq >= 1) %then %do;
      %put ERROR :  TDRDAS macro parameter oneminusq must be >0 and <1.;
      %abort;
   %end;

   %if %sysevalf(&accuracy. <= 0) %then %do;
      %put ERROR :  TDRDAS macro parameter accuracy must be >0.;
      %abort;
   %end;
   
   %if %length(&graphname.) > 0 and %length(&goutpath.) = 0 %then %do;
      %put ERROR :  TDRDAS macro:  specification of parameter goutpath is required when parameter graphname is specified.;
      %abort;
   %end;


/**********************************************************************************************
**  Check for nonexistent input data sets and nonexistent variables in the input data sets.   *
**********************************************************************************************/

  %let errcode = 0;
  %let dsid = %sysfunc(open(&indsn.,i));
  %if &dsid. = 0 %then %do;
     %put ERROR : TDRDAS macro found that specified input data set &indsn. does not exist.;
     %abort;
     %end;
   %else %do;
     %if %sysfunc(varnum(&dsid.,&predictorname.)) = 0 %then %do;
            %put ERROR : TDRDAS macro input data set &indsn. does not contain the variable &predictorname. specified in parameter predictorname.;
            %let errcode = 1;
            %end;
     %if %sysfunc(varnum(&dsid.,&estimate.)) = 0 %then %do;
            %put ERROR : TDRDAS macro input data set &indsn. does not contain the variable &estimate. specified in parameter estimate.;
            %let errcode = 1;
            %end;
     %if %sysfunc(varnum(&dsid.,&stderr.)) = 0 %then %do;
            %put ERROR : TDRDAS macro input data set &indsn. does not contain the variable &stderr. specified in parameter stderr.;
            %let errcode = 1;
            %end;

     %let rc = %sysfunc(close(&dsid.));
     %if &errcode. = 1 %then %abort;
     %end;

/**********************************************************************************************
**  Main macro.                                                                               *
**********************************************************************************************/

proc sql noprint;
    select count(distinct &predictorname.) into :npredictor from &indsn.;
quit; 

data &t..qqallest;
    set &indsn (rename=(&estimate=Estimate &stderr=StdErr));
    VarEst = StdErr * StdErr;

    AbsDiffEstZero = abs(Estimate-&estzero.);   /*  This will be used to find the largest
                                                absolute difference from the estimate value
                                                representing no association */

    dummyby = 1;  *  Dummy variable for merges;


/**********************************************************************************************
**  Create macro variable that is equal to the number of predictors analyzed plus 1.          *
**********************************************************************************************/

     ngp1 = &npredictor + 1;
     ngp1a = put(ngp1,5.0);
     call symput('npredictorplus1',ngp1a);

    keep Estimate StdErr VarEst AbsDiffEstZero &predictorname dummyby;

run;


/**********************************************************************************************
**  Compute sample variance of degree of association estimates and the mean of the estimate   *
**  variances.                                                                                *
**********************************************************************************************/

proc univariate data=&t..qqallest noprint;
     var Estimate VarEst AbsDiffEstZero dummyby;
     output out=&t..qqvarstat 
            mean = sampmeanest sampmeanvarest meanaz dummyby
            var = sampvarest sampvarvarest varaz vara
            max = MaxEst MaxVarest MaxAbsDiffEstZero Maxa;
run;


data &t..qqvarstat;
     set &t..qqvarstat;
     sigmahatbeta = sqrt(max(sampvarest-sampmeanvarest,0));  * Estimate of population SD of true degrees of association;

**  Create macro variable for minimum estimate value for loop below;

     EstZeroMult = int(&estzero / &accuracy);
     EstZeroText = put(EstZeroMult,6.0);
     call symput('EstZeroMult',EstZeroText);

**  Create macro variable for maximum absolute estimate value for loop below;

     MaxAbsDiffEstZeroMult = int(MaxAbsDiffEstZero / &accuracy);
     MaxEstText = put(MaxAbsDiffEstZeroMult,6.0);
     call symput('MaxEstMult',MaxEstText);
     
run;

/**********************************************************************************************
**  Compute the regression-to-the-mean-corrected estimate of the degree of association for    *
**  each predictor.                                                                           *
**********************************************************************************************/

data &t..qqcorrectest;
     merge &t..qqallest &t..qqvarstat;
     by dummyby;

     AbsEst = abs(Estimate);

     if Estimate > 0 then direction = 1;
     if Estimate < 0 then direction = -1;
     if Estimate = 0 then direction = 0;

     &RMCEstvar = sampmeanest + ((sigmahatbeta**2) / (sigmahatbeta**2 + VarEst)) * (Estimate - sampmeanest);
     AbsEstCorrect = abs(&RMCEstvar);

     keep &predictorname. direction &RMCEstvar AbsEst AbsEstCorrect;

     label direction = Direction of association;
run; 

%let flag = 1;  * Flag to signal need to continue the loop.  Will be reset to 0
                * when estmult gets high enough that the TDR set is null;

%do estmult = &EstZeroMult. %to &MaxEstMult %by 1;
   %if &flag. = 1 %then %do;

      data &t..qqpvalue;
         set &t..qqallest;

/*******************************************************************************************/
/** Compute p-value for the test of the null hypothesis that the estimate is less than or **/
/** equal to theta against the alternative that is greater than theta.                    **/
/*******************************************************************************************/

         theta = &estmult * &accuracy;
 
         Waldppos = min(1,2*(1-probnorm((abs(Estimate) - theta) / StdErr)));
   
         dummyby = 1;   /*  Dummy variable for merge */

         keep Waldppos Estimate StdErr VarEst theta &predictorname. dummyby;
      run;

/*******************************************************************************************/
/** Estimate pi0, the proportion of interval hypotheses that truly satisfy the            **/
/** interval null hypothesis.                                                             **/
/*******************************************************************************************/

      data &t..qqpi0storey;

         set &t..qqpvalue end=eof;

         m = &npredictor.;  * Number of hypothesis tests;

         if Waldppos >  &lambda then wfix = wfix + 1;

         if eof then do;
            pi0fix = min(1,wfix / ((1-&lambda)*m));  
            output;
         end;

         keep pi0fix dummyby;
         retain wfix 0;
      run;

      proc sort data=&t..qqpvalue;
         by descending Waldppos &predictorname.;
      run;

    
      data &t..combine;
         merge &t..qqpvalue &t..qqvarstat &t..qqpi0storey;
         by dummyby;
         pi0 = pi0fix;  *  Use Storeys estimate of the proportion of true interval null hypotheses;
      run;

/*******************************************************************************************/
/** Now identify the TDRDA set for this value of theta.                                   **/
/*******************************************************************************************/

      data &t..storey;
         set &t..combine end=eof;

         i = i - 1;
         q = 1 - &oneminusq;  
         m = &npredictor.;
         if pi0>0 then crit = (i/m)*(q/pi0);

         if Waldppos le crit and ireject = 0 then ireject = i;

/* Determine if TDR set is null.  If so, set flag to end loop */

         if eof and ireject = 0 then call symput('flag','0');
              

         label Waldppos = "P-value for interval null hypothesis test";
         label ireject = "Number of hypotheses rejected";
         label pi0 = Estimated proportion of true interval null hypotheses;
        
         retain i &npredictorplus1.  ireject 0;
      run;

      proc append base=&t..alltheta data=&t..storey;
      run;

   %end;
%end;

/*******************************************************************************************/
/** Determine maximum theta at which each predictor can still be included in a TDRDA set. **/
/*******************************************************************************************/

proc sort data=&t..alltheta;
   by  &predictorname. theta;
run;

data &t..qqMLB;
   set &t..alltheta;
   by &predictorname.;

   if first.&predictorname then &MLBvar. = .;     
   if ireject > 0 then &MLBvar. = max(&MLBvar.,theta);
   if last.&predictorname. then output;
   retain &MLBvar.;
run;

proc sort data=&t..qqcorrectest;
   by &predictorname.;
run;

/*******************************************************************************************/
/** Create output data set including (transformed) MLB, (transformed) RM-corrected        **/
/** estimates, and for graphing, (transformed) RM-corrected estimate of absolute degree   **/
/** of association and the direction of association.                                      **/
/*******************************************************************************************/

data &t..output;
   merge &t..qqMLB &t..qqcorrectest;
   by &predictorname.;
   %if %length(&transformx.) >0 %then %do;
      %let patternid_AEC=%sysfunc(prxparse(s/\(x\)/(AbsEstCorrect)/i));
      txAbsEstCorrect=%sysfunc(prxchange(&patternid_aec.,-1, &transformx.));

      %let patternid_MLB=%sysfunc(prxparse(s/\(x\)/(&MLBvar.)/i));
      txMLB=%sysfunc(prxchange(&patternid_MLB.,-1, &transformx));

      if last.&predictorname. then do;
         %let patternid_zero=%sysfunc(prxparse(s/\(x\)/(&estzero.)/i));
         txEstZero=%sysfunc(prxchange(&patternid_zero.,-1, &transformx.));
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
   by descending txMLB descending AbsEstCorrect &predictorname.;
run;

proc sql noprint;
   create table &outdsn. as
   select &predictorname., &MLBvar., &RMCEstvar., txMLB, TxAbsEstCorrect, txEstZero, direction
   from &t..output;
quit;


/*******************************************************************************************/
/** Produce TDRDAS graph.                                                                 **/
/*******************************************************************************************/

%if %length(&graphname.) %then %do;

/*  Decide issues regarding the number of pages of output */
proc sql noprint;
   select count(distinct &predictorname.) into :nrows from &outdsn
   where txMLB ne .;
quit; 
   
%if &nrows. >0 %then %do;   
   
   %let numpages=%sysevalf(&nrows./&maxpredictorsppg.,ceil);  /*calculate the number of pages that will be needed for the graphs*/
   %let maxpredictorsppg=%sysevalf(&nrows./&numpages.,ceil);   /*evenly divide the predictors onto the pages so the output looks consistent*/


   %if &maxpredictorsppg. <= 50 %then %let fontsize=8;
   %else %if 50 < &maxpredictorsppg. <= 100 %then %let fontsize=6;
   %else %if 100 < &maxpredictorsppg. <= 150 %then %let fontsize=4;


   /* In 9.2 the font size looks a bit bigger than in 9.1*/
   %if %substr(&sysver, 1,3) ne 9.1 %then %let fontsize=%sysevalf(&fontsize *0.7);

   proc sort data=&outdsn out=&t..grpdata;
      by descending txMLB descending TxAbsEstCorrect &predictorname;
   run;

   %let xmin=&estzero;

   proc sql noprint;
      select max(TxAbsEstCorrect) into :xmax from &t..grpdata;
      select max(length(&predictorname.))+2 into :maxlength from &outdsn;  /* Determine maximum length of label dynamically */
   quit;
   
 /*  Divide data into segments for presentation on bar graph.   There will now be 2 obs per predictor. */   
   proc format;
   value  hazgrpf 9="Maximum lower bound for inclusion in &oneminusq TDRDA set: predictors with negative association"
                 10="RM-corrected absolute estimate: predictors with negative association"
                 11="Maximum lower bound for inclusion in &oneminusq TDRDA set: predictors with positive association"
                 12="RM-corrected absolute estimate: predictors with positive association";
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
      device=png
      ftext="Albany AMT"
      htext=&fontsize pt
      gsfmode=replace
      hsize=6 in
      vsize=9 in
      gsfname=gout;


   %if &mindir=-1 %then %do;
      pattern1 color="&graphcolor1." ;
      pattern3 color="&graphcolor2." ;
      pattern2 value=empty color="&graphcolor1." ;
      pattern4 value=empty color="&graphcolor2.";
   %end;

   %else %if &mindir=1 %then %do;
      pattern1 color="&graphcolor2." ;
      pattern2 value=empty color="&graphcolor2.";
   %end;
  

   axis1 order=(&xmin to %sysevalf(&xmax + &refinterval.) by &refinterval.) label=(f="Albany AMT" h=9 pt "&graphlabel" ) value=(h=.75);
   axis2 label=none value=(j=r); 
   legend1 label=none value=(justify=l height=7.2 pt) across=1; 

   %do j=1 %to &numpages.;

      %if %eval(&numpages.=1) %then filename gout "&goutpath.\&graphname..PNG";
      %else filename gout "&goutpath.\&graphname._&j..PNG";;
   
   /* Subset data for each page of the graph */

      %let firstobs=%eval(1+(&j-1)*2*&maxpredictorsppg.);
      %let obs=%eval(&j*2*&maxpredictorsppg.);

      data &t..subset;
         length &predictorname. $&maxlength.;
         set &t..grpdata(firstobs=&firstobs obs=&obs.);
      run;

      proc sql noprint;
         select &predictorname. into :predictorlist separated by '" "' from &t..subset
         where hazgrp = 9 or hazgrp = 11;         
      quit;
 
   
      proc gchart data=&t..subset /*anno=&t..leg*/;
         hbar &predictorname. / subgroup=hazgrp 
              sumvar=segment
              midpoints="&predictorlist."
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
   %put WARNING :  Graph was not produced because there was no predictor with txmlb > . ;
%end;  

%end;


/************************************************************************************
** Clean up data sets and reset mergenoby option to what is was.                    *
************************************************************************************/

proc datasets memtype=data lib=&t. kill nodetails nolist;
run;
quit;

options mergenoby=&mergenobyoption.;

%mend tdrdas;


