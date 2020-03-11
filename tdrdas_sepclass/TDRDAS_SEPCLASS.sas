/**********************************************************************************************************************
WARNING:
THIS PROGRAM CONTAINS CONFIDENTIAL INFORMATION OF GENOMIC HEALTH, INC. (GHI) AND MAY ONLY 
BE VIEWED BY PERSONS AUTHORIZED IN WRITING BY GHI. UNAUTHORIZED VIEWING OR DISCLOSURE IS 
STRICTLY PROHIBITED AND MAY RESULT IN SERIOUS LEGAL CONSEQUENCES.
***********************************************************************************************************************/
/***********************************************************************************************************************

Program Name           : TDRDAS_SEPCLASS.sas

Path                   : 

Program Language       : SAS

Operating System       : Server

Purpose                : Given a set of genes with estimates (and standard errors) of the numerical degree of
                         association of each with clinical outcome or state, TDRDAS_SEPCLASS performs a separate class 
                         true discovery rate degree of association (TDRDA) set analysis and produces an output data set
                         containing class identifier, gene name, the maximum lower bound (MLB) absolute degree of association
                         for which each gene will be included in a TDRDA set, the direction (positive or negative) of association,
                         and a regression-to-the-mean-corrected estimate of the degree of association.  The program also produces
                         a TDRDA bar chart showing the MLB and RM-corrected estimates for all identified genes.  At the user's 
                         option, this bar chart may be sorted by class or not.
                         
Notes                  : 

Status                 : Tested and verified

Run Dependencies       :  None

Input Datasets/Views   : Specified SAS data set indsn containing gene name, class identifier, estimate of degree of association (such as log hazard ratio
                         or log odds ratio), and standard error of the estimate.

Output Datasets/Views  : Specified SAS data set outdsn containing maximum absolute association for which each gene will be included in a TDRDA
                         set, direction of association, and a regression-to-the-mean-corrected estimate of the degree of association
                         

Other Output           : A graph saved as an png file containing a bar chart of the results.  Multiple graphs will be produced if the number
                         of associated genes exceeds 150.

Macro calls internal   : None

Macro calls external   : SEPCLASS_THETA

***********************************************************************************************************************/
%macro TDRDAS_SEPCLASS(
       /* Input Specification*/    indsn=,genename=,class=,estimate=,stderr=,estzero=,
       /* Analysis Parameters */   accuracy=0.001,oneminusq=0.9,lambda=0.5,
       /* Output Specification */  outdsn=, transformx=exp(x),MLBvar=MLB,RMCEstvar=RMCEst,
       /* Graph Options*/ refinterval=.05, measure=Degree of Association, graphlabel=%str(Standardized Degree of Association of Gene Expression with Clinical Outcome),
                          goutpath=, graphname=TDRDAS,maxgenesppg=150, graphcolor1=black, graphcolor2=gray,sortbyclass=yes
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
| Name           : genename
| Required (Y/N) : Y
| Default Value  :
| Type ($/#)     : $
| Purpose        : Name of variable that contains gene names 
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
|                  in the function, it must be encased in parentheses.  Use of this parameter will result
|                  in 2 additional variables in the output dataset, txMLB and TxAbsEstCorrect
|
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
| Default Value  : Standardized Degree of Association for Gene Expression with Recurrence
| Type ($/#)     : $
| Purpose        : This parameter allows the user to modify the label on the graph.
|                  
|-----------------------------------------------------------------------------------------------
|
| Name           : goutpath
| Required (Y/N) : Y if graphname specified.
| Default Value  : .
| Type ($/#)     : $
| Purpose        : The full path (excluding the final '\' and filename) for the directory into which
|                  the graph will be saved.
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
| Name           : maxgenesppg
| Required (Y/N) : N
| Default Value  : 150
| Type ($/#)     : #
| Purpose        : The maximum number of genes to be shown on each graph. The value selected should be in 
|                  the range of 1-150. 
|
|----------------------------------------------------------------------------------------------- 
| Name           : graphcolor1
| Required (Y/N) : N
| Default Value  : black
| Type ($/#)     : $
| Purpose        : The color of the bars on the graph representing the genes with negative association.  
| 
|-----------------------------------------------------------------------------------------------
|
| Name           : graphcolor2
| Required (Y/N) : N
| Default Value  : gray
| Type ($/#)     : $
| Purpose        : The color of the bars on the graph representing the genes with positive association.  
|
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
**********************************************************************************************************/;
/**********************************************************************************************************
Mod#    Date         Username    Test     Description
---     -------      --------    ----    -----------------------------------------------------------
000     20091214     mcrager
001     20101012     mcrager     1-4     Determine maximum length of variable &genename dynamically and
                                         Substitute where” clause to prevent duplicate records in graph
002     20200310     mcrager             Add checks for user error in specifying input parameters.  Ensure
                                         that graphs will not be obscured by a row label variable with a large
                                         length.  Automatically set mergenoby option before executing
                                         and reset it to the existing value at end of macro execution.
**********************************************************************************************************/;
 %local t EstZeroMult MaxEstMult fontsize numpages maxgenesppg j flag legendlocation xmax firstobs obs
        ngene k vartype maxlength;

/* Generate a subdir at &workspace for any temp dataset.*/

   %let t = _tdrdasc;
   
   %Tmp_SubLib(tmplib=&t.);

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
%if %length(&refinterval.) = 0 %then %let refinterval = 0.05;
%if %length(&measure.) = 0 %then %let measure = Standardized Degree of Association;
%if %length(&graphlabel.) = 0 %then %let graphlabel = Standardized Degree of Association of Gene Expression with Clinical Outcome;
%if %length(&maxgenesppg.) = 0 %then %let maxgenesppg = 150;
%if %length(&graphcolor1.) = 0 %then %let graphcolor1 = black;
%if %length(&graphcolor2.) = 0 %then %let graphcolor2 = gray;
%if %length(&sortbyclass.) = 0 %then %let sortbyclass = yes;

/* Check for errors in macro parameter specification */

   %if %length(&estzero.)=0 %then %do;
      %put ERROR : GHI Note: TDRDAS_SEPCLASS macro parameter estzero must be specified.;
      %abort;
   %end;

   %if %sysfunc(sign(&lambda.))=-1 %then %do;
      %put ERROR: GHI Note: TDRDAS_SEPCLASS macro parameter lambda must be >=0 and <1.;
      %abort;
   %end;

   %if %sysevalf(&lambda. < 0) or %eval(&lambda >= 1) %then %do;
      %put ERROR: GHI Note: TDRDAS_SEPCLASS macro parameter lambda must be >=0 and <1.;
      %abort;
   %end;

   %if %sysevalf(&lambda. < 0.2) or %sysevalf(&lambda > 0.8) %then %do;
      %put WARNING: GHI Note: TDRDAS_SEPCLASS macro parameter lambda is set at &lambda.  Lambda of 0.5 may perform better.;
   %end;

   %if %sysfunc(sign(&oneminusq.))=-1 %then %do;
      %put ERROR: GHI Note: TDRDAS_SEPCLASS macro parameter oneminusq must be >0 and <1.;
      %abort;
   %end;
 
   %if %sysevalf(&oneminusq. <= 0) or %sysevalf(&oneminusq >= 1) %then %do;
      %put ERROR: GHI Note: TDRDAS_SEPCLASS macro parameter oneminusq must be >0 and <1.;
      %abort;
   %end;

   %if %sysevalf(&accuracy. <= 0) %then %do;
      %put ERROR: GHI Note: TDRDAS_SEPCLASS macro parameter accuracy must be >0.;
      %abort;
   %end;

   %if %length(&class.)=0 %then %do;
      %put ERROR : GHI Note: TDRDAS_SEPCLASS macro parameter class must be specified.;
      %abort;
   %end;

   %if %length(&graphname.) > 0 and %length(&goutpath.) = 0 %then %do;
      %put ERROR : GHI Note: TDRDAS macro:  specification of parameter goutpath is required when parameter graphname is specified.;
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
     %if %sysfunc(varnum(&dsid.,&genename.)) = 0 %then %do;
            %put ERROR : TDRDAS_SEPCLASS macro input data set &indsn. does not contain the variable &genename. specified in parameter genename.;
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
/*  Count number of genes and number of classes in data set             */
/************************************************************************/

proc sql noprint;
    select count(distinct &genename) into :ngene from &indsn;
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
/*   class variable and determine proportion of genes in each class.    */
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

    keep Estimate StdErr VarEst AbsDiffEstZero &genename &class dummyby;
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
         classprop = classcount / &ngene;
         label classprop = Proportion of genes in class;
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
/*  Compute the regression-to-the-mean-corrected estimate of the degree of association for each gene */
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

     keep &genename &class direction EstCorrect AbsEst AbsEstCorrect;

     label direction = Direction of association;
run; 


/*******************************************************************************************************/
/*  Determine the MLB theta for each gene using separate class FDR analysis.                           */
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

         keep Waldp Estimate StdErr VarEst theta &genename &class nclass dirtheta classprop a;
     run;

%sepclass_theta(indsn=&t..pvalue,genename=&genename,class=nclass,estimate=Estimate,stderr=StdErr,
                pvalue=Waldp,direction=dirtheta,lambda=&lambda,theta=theta,
                outdsn=&t..sepclass,overallq=overallq,sepclassq=sepclassq);


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
  /*Determine maximum theta at which each gene can still be included in a TDRDA set*/
   by  &genename theta;
run;

/* data local.tracetheta;
    set &t..alltheta;
run;
*/


data &t..MLB;
   set &t..alltheta;
   by &genename;

   if first.&genename then do;
        &MLBvar. = .;
        mxflag = 0;
        end;

/*  The MLB is set at the first theta for which the null hypothesis is not rejected.  This is equivalent */
/*  to setting the estimated FDR(theta) equal to the max FDR estimate from 0 to theta.                   */

   if sepclassq <= 1 - &oneminusq and mxflag = 0 
          then &MLBvar. = max(&MLBvar.,theta);
          else mxflag = 1; /* Failure to reject null hypothesis detected */

   if last.&genename then output;

   retain &MLBvar. mxflag;
   drop mxflag;
run;

proc sort data=&t..correctest;
   by &genename;
run;


data &outdsn;
/***Create output data set including (transformed) MLB, (transformed) RM-corrected
    estimates, and for graphing, (transformed) RM-corrected estimate of absolute degree 
    of association and the direction of association ******/
   merge &t..MLB &t..correctest;
   by &genename;
   %if %length(&transformx) >0 %then %do;
      %let patternid_AEC=%sysfunc(prxparse(s/\(x\)/(AbsEstCorrect)/i));
      txAbsEstCorrect=%sysfunc(prxchange(&patternid_aec.,-1, &transformx));

      %let patternid_MLB=%sysfunc(prxparse(s/\(x\)/(MLB)/i));
      txMLB=%sysfunc(prxchange(&patternid_MLB.,-1, &transformx));

      if last.&genename then do;
         %let patternid_zero=%sysfunc(prxparse(s/\(x\)/(&estzero)/i));
         txEstZero=%sysfunc(prxchange(&patternid_zero.,-1, &transformx));
         call symputx("EstZero",TxEstZero);
      end;
      
   %end;
   %else %do;
      txMLB=MLB;
      txAbsEstCorrect=AbsEstCorrect;
   %end;

   label &MLBvar. = "Maximum Absolute &measure. for Which Predictor is Included in a &oneminusq. TDRDA Set";
   label txMLB = "Transformed Maximum Absolute &measure. for Which Predictor is Included in a &oneminusq. TDRDA Set"; 
   label &RMCEstvar. = "Estimate of &measure. Corrected for Regression to the Mean";
   label TxAbsEstCorrect= "Transformed Estimate of Absolute &measure. Corrected for Regression to the Mean";
   label direction = "Direction of Association";
run;

proc sort data=&outdsn;
%if &sortbyclass = yes %then %do;
   by &class descending txMLB descending AbsEstCorrect &genename;
%end;
                       %else %do;
   by descending txMLB descending TxAbsEstCorrect &genename;
%end;
run;

data &outdsn;
   set &outdsn;
 
   label direction = "Direction of Association";

   keep &class &genename &MLBvar. &RMCEstvar. txMLB TxAbsEstCorrect direction; 

run;

/*******************************************************************************************/
/** Produce TDRDAS graph.                                                                 **/
/*******************************************************************************************/

%if %length(&graphname.) %then %do;

/*  Decide issues regarding the number of pages of output */
proc sql noprint;
   select count(distinct &genename) into :nrows from &outdsn
   where txMLB ne .;
quit; 
   
%if &nrows >0 %then %do;   
   
   %let numpages=%sysevalf(&nrows./&maxgenesppg.,ceil);  /*calculate the number of pages that will be needed for the graphs*/
   %let maxgenesppg=%sysevalf(&nrows./&numpages.,ceil);   /*evenly divide the genes onto the pages so the output looks consistent*/


   %if &maxgenesppg <= 50 %then %let fontsize=8;
   %else %if 50 < &maxgenesppg <= 100 %then %let fontsize=6;
   %else %if 100 < &maxgenesppg <= 150 %then %let fontsize=4;


   /* In 9.2 the font size looks a bit bigger than in 9.1*/
   %if %substr(&sysver, 1,3) ne 9.1 %then %let fontsize=%sysevalf(&fontsize *0.7);

   proc sort data=&outdsn out=&t..grpdata;
%if &sortbyclass = yes %then %do;
   by &class descending txMLB descending TxAbsEstCorrect &genename;
%end;
                       %else %do;
      by descending txMLB descending TxAbsEstCorrect &genename;
%end;
   run;

   %let xmin=&estzero;

   proc sql noprint;
      select max(TxAbsEstCorrect) into :xmax from &t..grpdata;
      select max(length(&genename.))+2 into :maxlength from &outdsn;  /* Determine maximum length of label dynamically */
   quit;
   
 /*  Divide data into segments for presentation on bar graph.   There will now be 2 obs per gene. */   
   proc format;
   value  hazgrpf 9="Maximum lower bound for inclusion in &oneminusq TDRDA set: Genes with negative association"
                 10="RM-corrected absolute estimate: Genes with negative association"
                 11="Maximum lower bound for inclusion in &oneminusq TDRDA set: Genes with positive association"
                 12="RM-corrected absolute estimate: Genes with positive association";
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

      %if %eval(&numpages=1) %then filename gout "&goutpath.\&graphname..png";
      %else filename gout "&goutpath.\&graphname._&j..png";;
   
   /* Subset data for each page of the graph */

      %let firstobs=%eval(1+(&j-1)*2*&maxgenesppg);
      %let obs=%eval(&j*2*&maxgenesppg.);

      data &t..subset;
         length &genename $&maxlength.;
         set &t..grpdata(firstobs=&firstobs obs=&obs.);
	 &genename=strip(&genename);
      run;

      proc sql noprint;
         select &genename into :genelist separated by '" "' from &t..subset
         where hazgrp = 9 or hazgrp = 11;         
      quit;
   
      proc gchart data=&t..subset /*anno=&t..leg*/;
         hbar &genename / subgroup=hazgrp
              sumvar=segment
              midpoints="&genelist"
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
   %put WARNING : GHI Note: Graph was not produced because there were no genes with txmlb > . ;
%end;  

%end;

/*
   Clean Up and Reset
*/
proc datasets memtype=data lib=&t. kill nodetails nolist;
run;
quit;

options mergenoby=&mergenobyoption.;

libname &t. clear;
%put _global_;
%mend tdrdas_sepclass;
