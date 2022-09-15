/***********************************************************************************************************************

Program Name           : key_stats_logistic.sas

Path                   : 

Program Language       : SAS

Operating System       : Windows Server

Purpose                : Perform the logistic regression analysis for an individual study and capture the key summary 
                         summary statistics required to perform patient-specific meta-analysis (PSMA) risk estimation.
                         
Notes                  : 

Status                 : Not yet tested or verified.

Run Dependencies       : None

Input Datasets/Views   : Specified SAS data set indsn containing a binary response variable, covariates for the logistic
                         regression model, and an optional weight variable if cohort sampling was used.

Output Datasets/Views  : Data set containing the regression parameter estimates and covariance matrix.                         

Other Output           : None

Macro calls internal   : None

Macro calls external   : None

***********************************************************************************************************************/

/***********************************************************************************************
| DEFINITION OF PARAMETERS
|
| Name           : indsn
| Required (Y/N) : Y
| Default Value  : (at temporary library)
| Type ($/#)     : $
| Purpose        : Libname reference and the input data set name.  The dataset 
|                  name must conform to the rules for SAS names.   
|-----------------------------------------------------------------------------------------------
| Name           : response
| Required (Y/N) : Y
| Default Value  :
| Type ($/#)     : # or $
| Purpose        : The dichotomous dependent variable for the logistic regression analysis.
|-----------------------------------------------------------------------------------------------
| Name           : event
| Required (Y/N) : Y
| Default Value  :
| Type ($/#)     : # or $
| Purpose        : The value of the response variable that indicates presence of an event.
|-----------------------------------------------------------------------------------------------
| Name           : vars
| Required (Y/N) : Y
| Default Value  :
| Type ($/#)     : #
| Purpose        : List of input data set variables containing the logistic regression model covariates. 
|-----------------------------------------------------------------------------------------------
| Name           : weight  
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : #
| Purpose        : Input data set variable giving the observation's weight in the analysis. If this parameter
|                  is set, it is assumed that cohort sampling was used and resulted in the specified weights.   
|-----------------------------------------------------------------------------------------------
| Name           : sampstrata   
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : # or $
| Purpose        : If a stratified cohort sampling design was used, use this parameter to list the
|                  stratification variables.
|-----------------------------------------------------------------------------------------------
**********************************************************************************************************/;
/**********************************************************************************************************
Mod#    Date         Username    Test     Description
---     -------      --------    ----    -----------------------------------------------------------
000     20220912     mcrager             Initial version.
**********************************************************************************************************/;

%macro key_stats_logistic(
        /* Input Specification */  indsn=,response=,event=,vars=,weight=,sampstrata=,
        /* Output Specification */ keysdsn=
        );

/*  Capture status of mergenoby option so it can be reset to its current value at the end of the macro */

proc optsave out=qqoptions;
run;

proc sql noprint;
    select OPTVALUE into :mergenobyoption
    from qqoptions where (OPTNAME = 'MERGENOBY');
quit;

options mergenoby=nowarn;


/* Check for errors in macro parameter specification */

   %if %length(&indsn.)=0 %then %do;
      %put ERROR :  keys_stats_logistic macro parameter indsn must be specified.;
      %abort;
   %end;

   %if %length(&vars.)=0 %then %do;
      %put ERROR :  keys_stats_logistic macro parameter vars must be specified.;
      %abort;
   %end;

   %if %length(&response.)=0 %then %do;
      %put ERROR :  keys_stats_logistic macro parameter response must be specified.;
      %abort;
   %end;

   %if %length(&event.)=0 %then %do;
      %put ERROR :  keys_stats_logistic macro parameter event must be specified.;
      %abort;
   %end;

   %if %length(&keysdsn.)=0 %then %do;
      %put ERROR :  keys_stats_logistic macro parameter keysdsn must be specified.;
      %abort;
   %end;


                   
/**********************************************************************************************
**  Parse the input parameter vars containing the list of covariates, determine the number    *
**  of covariates, and assign them to the macro variables var1, var2, etc.  Add "INTERCEPT"   *
**  to the list of covariates if not already present.                                         *
**********************************************************************************************/

%if %index(%UPCASE(&vars.),INTERCEPT) = 0 %then %let vars = &vars. INTERCEPT;

%let vars = %sysfunc(compbl(&vars.));

%local nvar nvar2;

%let nvar = %sysfunc(countc(%bquote(&vars.), %str( )));
%let nvar = %sysfunc(ifc(%length(%bquote(&vars.)), %eval(&nvar. + 1), 0));

%do i = 1 %to &nvar.;
   %local var&i.;
   %let var&i = %scan(&vars., &i., %str( ));
%end;     

%let nvar2 = %sysevalf(&nvar.**2,integer); 



/**********************************************************************************************
**  Check for nonexistent input data set and nonexistent variables in the input data set.     *
**********************************************************************************************/

  %let errcode = 0;
  %let dsid = %sysfunc(open(&indsn.,i));
  %if &dsid. = 0 %then %do;
     %put ERROR :  keys_stats_logistic macro found that specified input data set &indsn. does not exist.;
     %abort;
     %end;
   %else %do;
     %do k = 1 %to &nvar.;
          %if %UPCASE(&&var&k..) ne INTERCEPT and %sysfunc(varnum(&dsid.,&&var&k..)) = 0 %then %do;
            %put ERROR : GHI note:  keys_stats_logistic macro input data set &indsn. does not contain the variable &&var&k.. specified in parameter vars.;
            %let errcode = 1;
            %end;
         %end;
     %if %length(&weight.) %then %do;
         %if %sysfunc(varnum(&dsid.,&weight.)) = 0 %then %do;
            %put ERROR : GHI note:  keys_stats_logistic macro input data set &indsn. does not contain variable &weight. specified in macro parameter weight.;
            %let errcode = 1; 
            %end;
         %end;
     %if %length(&sampstrata.) %then %do k = 1 %to &nstrat.;
         %if %sysfunc(varnum(&dsid.,&&strat&k..)) = 0 %then %do;
            %put ERROR : GHI note:  keys_stats_logistic macro input data set &indsn. does not contain variable &&strat&k.. specified in macro parameter strata.;
            %let errcode = 1;
            %end;
         %end;
     %if %sysfunc(varnum(&dsid.,&response.)) = 0 %then %do;
            %put ERROR : GHI note:  keys_stats_logistic macro input data set &indsn. does not contain the variable &response specified in parameter response.;
            %let errcode = 1;
            %end;
     %let rc = %sysfunc(close(&dsid.));
     %if &errcode = 1 %then %abort;
     %end;

/***************************************************************************************************/
/**  Delete records with missing covariate values.                                                **/
/***************************************************************************************************/

data qqindata;
    set &indsn.;
    INTERCEPT = 1;
    if &var1. = . %do i = 2 %to &nvar.; or &&var&i.. = . %end; then delete;
run;

/***************************************************************************************************/
/**  Perform the logistic regression analysis.                                                    **/
/***************************************************************************************************/

ods output ParameterEstimates = qqestout covB = qqcovmatrix;

%if %length(&weight.) = 0 %then %do;
/**********************************************************************************************
**  Case 1: no cohort sampling.                                                              **
**********************************************************************************************/

proc logistic data=qqindata NAMELEN=30;
     model &response.(event="&event.") = %do i = 1 %to &nvar.; &&var&i.. %end; / noint covb;   
run;

%end;

%else %do;
/**********************************************************************************************
**  Case 2: Cohort sampling design.                                                          **
**********************************************************************************************/

proc surveylogistic data=qqindata NAMELEN=30;
     weight qqweight;
     model &response.(event="&event.") = %do i = 1 %to &nvar.; &&var&i.. %end; / noint covb;    
%if %length(&sampstrata.) > 0 %then strata &sampstrata.;;  
run;

%end;

ods output close;

**********************************************************************************************;
**  Capture the variance-covariance matrix of the parameter estimates.                        ;
**********************************************************************************************;

data qqvk;
      array analvar(&nvar.) &vars.;
      array v(&nvar.,&nvar.) %do i = 1 %to &nvar.; %do j = 1 %to &nvar.; v_&&var&i.._&&var&j.. %end; %end;;
 
      set qqcovmatrix end=eof;

%do i = 1 %to &nvar.;
      if UPCASE(Parameter) = UPCASE("&&var&i..") then row = &i.;
%end;
     
      do column = 1 to &nvar.;
          v(row,column) = analvar(column);
          end;

      if eof then output;

      retain %do i = 1 %to &nvar.; %do j = 1 %to &nvar.; v_&&var&i.._&&var&j.. %end; %end;;
      keep   %do i = 1 %to &nvar.; %do j = 1 %to &nvar.; v_&&var&i.._&&var&j.. %end; %end;;
run;

**********************************************************************************************;
**  Capture the regression parameter estimates.                                               ;
**********************************************************************************************;

data qqbk;

      array b(&nvar.) %do i = 1 %to &nvar.; beta_&&var&i.. %end;;

      set qqestout end=eof;

%do i = 1 %to &nvar.;
      if UPCASE(Variable) = UPCASE("&&var&i..") then beta_&&var&i.. = Estimate;
%end;

      if eof then output;

      retain  %do i = 1 %to &nvar.; beta_&&var&i.. %end;;
      keep    %do i = 1 %to &nvar.; beta_&&var&i.. %end;;
run;

data &keysdsn.;
      merge qqbk qqvk;
      
      dummyby = 1;
run;

/************************************************************************************
** Reset mergenoby option to what is was.                                           *
************************************************************************************/

options mergenoby=&mergenobyoption.;

%mend key_stats_logistic;