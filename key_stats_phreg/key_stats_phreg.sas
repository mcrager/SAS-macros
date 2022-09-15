/***********************************************************************************************************************

Program Name           : key_stats_PHREG.sas

Path                   : 

Program Language       : SAS

Operating System       : Windows Server

Purpose                : Perform the Cox regression analysis for an individual study and capture the key summary 
                         statistics required to perform patient-specific meta-analysis (PSMA) risk estimation.                              
                         
Notes                  : 

Run Dependencies       : None

Input Datasets/Views   : Specified SAS data set indsn containing a time variable, indicator for censoring, 
                         covariates for the Cox model, and an optional weight variable if cohort sampling was used.

Output Datasets/Views  : Key statistics data set including regression parameter estimates, their covariance matrix, baseline
                         cumulative hazard and its estimated standard deviation due to the number and timing of events.                          

Other Output           : None

Macro calls internal   : None


***********************************************************************************************************************/  
/***********************************************************************************************
| DEFINITION OF PARAMETERS
|
| Name           : indsn
| Required (Y/N) : Y
| Default Value  : (at temporary library)
| Type ($/#)     : $
| Purpose        : (Libname reference and) the input data set name.  The dataset 
|                  name must conform to the rules for SAS names.   
|-----------------------------------------------------------------------------------------------
| Name           : vars
| Required (Y/N) : Y
| Default Value  :
| Type ($/#)     : #
| Purpose        : List of input data set variables containing the Cox model covariates. 
|-----------------------------------------------------------------------------------------------
| Name           : strata
| Required (Y/N) : N
| Default Value  :
| Type ($/#)     : $ or #
| Purpose        : Optional list of input data set variables to be used as strata in the Cox model. 
|-----------------------------------------------------------------------------------------------
| Name           : time
| Required (Y/N) : Y
| Default Value  : 
| Type ($/#)     : #
| Purpose        : Input data set variable containing the time to event (or censoring) 
|-----------------------------------------------------------------------------------------------
| Name           : censor
| Required (Y/N) : Y
| Default Value  : 
| Type ($/#)     : #
| Purpose        : Input data set variable indicating whether the observed time to event was censored.
|-----------------------------------------------------------------------------------------------
| Name           : censorlist
| Required (Y/N) : N
| Default Value  : 0
| Type ($/#)     : #
| Purpose        : List of values of variable censor that indicate a censored observation.  Default is
|                  the single value 0.
|-----------------------------------------------------------------------------------------------
| Name           : weight  
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : #
| Purpose        : Input data set variable giving the observation's weight in the analysis. If this parameter
|                  is set, it is assumed that cohort sampling was used and resulted in the specified weights, so
|                  the analysis is conducted using the "covariance sandwich" estimates for the covariance matrix
|                  of the regression parameter estimates.   
|-----------------------------------------------------------------------------------------------
| Name           : risktime  
| Required (Y/N) : Y
| Default Value  : 
| Type ($/#)     : #
| Purpose        : Constant giving the time at which the risk will eventually be estimated in the PSMA. 
|-----------------------------------------------------------------------------------------------
| Name           : exten  
| Required (Y/N) : N
| Default Value  : 0
| Type ($/#)     : #
| Purpose        : Constant giving the time by which the baseline cumulative hazard function estimate will
|                  will be extended using martingale extension. 
|-----------------------------------------------------------------------------------------------
| Name           : keydsn  
| Required (Y/N) : Y
| Default Value  : 
| Type ($/#)     : $
| Purpose        : (Libname reference and) output data set name.  
|-----------------------------------------------------------------------------------------------
Mod#    Date         Username    Test     Description
---     -------      --------    ----    -----------------------------------------------------------
000     20220912     mcrager             Original version.

**********************************************************************************************************/

%macro key_stats_PHREG(
        /* Input Specification */ indsn=,time=,censor=,censorlist=0,vars=,strata=,weight=,
        /* Analysis Parameters */ risktime=,exten=,
        /* Output Specification */ keysdsn=
        );

/* Check for errors in macro parameter specification */

   %if %length(&indsn.)=0 %then %do;
      %put ERROR :  key_stats_PHREG macro parameter indsn must be specified.;
      %abort;
   %end;

   %if %length(&vars.)=0 %then %do;
      %put ERROR :  key_stats_PHREG macro parameter vars must be specified.;
      %abort;
   %end;

   %if %length(&time.)=0 %then %do;
      %put ERROR :  key_stats_PHREG macro parameter time must be specified.;
      %abort;
   %end;

   %if %length(&censor.)=0 %then %do;
      %put ERROR :  key_stats_PHREG macro parameter censor must be specified.;
      %abort;
   %end;

   %if %length(&keysdsn.)=0 %then %do;
      %put ERROR :  key_stats_PHREG macro parameter keysdsn must be specified.;
      %abort;
   %end;

/*  Set default values of parameters not specified */

%if %length(&censorlist.) = 0 %then %let censorlist = 0;
%if %length(&exten.) = 0 %then %let exten = 0;
                   
/**********************************************************************************************
**  Parse the input parameter vars containing the list of covariates, determine the number    *
**  of covariates, and assign them to the macro variables var1, var2, etc.                    *
**********************************************************************************************/

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
**  Parse the string containing the stratification variables, and assign them to macro        *
**  variables strat1, strat2, etc.                                                            *
**********************************************************************************************/

%if %length(&strata.) %then %do;
   %let strata=%sysfunc(compbl(&strata.));
%end;

%local nstrata; 

%let nstrata = %sysfunc(countc(%bquote(&strata.), %str( )));
%let nstrata = %sysfunc(ifc(%length(%bquote(&strata.)), %eval(&nstrata. + 1), 0));
   
%do i = 1 %to &nstrata.;
   %local strat&i.;
   %let strat&i = %scan(&strata., &i., %str( ));
%end;  

/**********************************************************************************************
**  Check for nonexistent input data sets and nonexistent variables in the input data sets.   *
**********************************************************************************************/

  %let errcode = 0;
  %let dsid = %sysfunc(open(&indsn.,i));
  %if &dsid. = 0 %then %do;
     %put ERROR :  key_stats_PHREG macro found that specified input data set &indsn. does not exist.;
     %abort;
     %end;
   %else %do;
     %if %sysfunc(varnum(&dsid.,&time.)) = 0 %then %do;
            %put ERROR :  key_stats_PHREG macro input data set &indsn. does not contain the variable &time. specified in macro parameter time.;
            %let errcode = 1;
            %end;
     %if %sysfunc(varnum(&dsid.,&censor.)) = 0 %then %do;
            %put ERROR :  key_stats_PHREG macro input data set &indsn. does not contain the variable &censor. specified in macro parameter censor.;
            %let errcode = 1;
            %end;   
     %do k = 1 %to &nvar.;
          %if %sysfunc(varnum(&dsid.,&&var&k..)) = 0 %then %do;
            %put ERROR :  key_stats_PHREG macro input data set &indsn. does not contain the variable &&var&k.. specified in parameter vars.;
            %let errcode = 1;
            %end;
         %end;
     %if %length(&strata.) %then %do k = 1 %to &nstrata.;
          %if %sysfunc(varnum(&dsid.,&&strat&k..)) = 0 %then %do;
            %put ERROR : key_stats_PHREG macro input data set &indsn. does not contain the variable &&strat&k.. specified in parameter strata.;
            %let errcode = 1;
            %end;
         %end;         
     %if %length(&weight.) %then %do;
         %if %sysfunc(varnum(&dsid.,&weight.)) = 0 %then %do;
            %put ERROR :  key_stats_PHREG macro input data set &indsn. does not contain variable &weight. specified in macro parameter weight.;
            %let errcode = 1; 
            %end;
         %end;
     %let rc = %sysfunc(close(&dsid.));
     %if &errcode = 1 %then %abort;
     %end;

/***************************************************************************************************/
/**  Delete records with missing covariate values.                                                **/
/***************************************************************************************************/

data qqindata;
    set &indsn.;
    if &var1. = . %do i = 2 %to &nvar.; or &&var&i.. = . %end; then delete;
run;

/***************************************************************************************************/
/**  Perform the Cox regression analysis.                                                         **/
/***************************************************************************************************/

ods _all_ close;
ods output ParameterEstimates=qqestout CovB=qqcovmatrix;
ods graphics on;

proc phreg data=qqindata %if %length(&weight.) %then covs;;
%if %length(&weight.) %then %do;
     weight &weight.;
%end;
     model &time.*&censor.(&censorlist.) = &vars. / covb ties=efron;
%if %length(&strata.) %then %do;
     strata &strata.;
%end;
     output out = qqxproduct xbeta = xbeta;
run;

ods graphics off;
ods output close;

ods listing;

**********************************************************************************************;
**  Capture the variance-covariance matrix of the parameter estimates.                        ;
**********************************************************************************************;

data qqvk;
      array analvar(&nvar.) &vars.;
      array v(&nvar.,&nvar.) %do i = 1 %to &nvar.; %do j = 1 %to &nvar.; v_&&var&i.._&&var&j.. %end; %end;;
 
      set qqcovmatrix end=eof;

%do i = 1 %to &nvar.;
      if Parameter = "&&var&i.." then row = &i.;
%end;
     
      do column = 1 to &nvar.;
          v(row,column) = analvar(column);
          end;

      dummyby = 1;

      if eof then output;

      retain %do i = 1 %to &nvar.; %do j = 1 %to &nvar.; v_&&var&i.._&&var&j.. %end; %end;;
      keep   %do i = 1 %to &nvar.; %do j = 1 %to &nvar.; v_&&var&i.._&&var&j.. %end; %end; dummyby;
run;

**********************************************************************************************;
**  Capture the regression parameter estimates.                                               ;
**********************************************************************************************;

data qqbk;

      array b(&nvar.) %do i = 1 %to &nvar.; beta_&&var&i.. %end;;

      set qqestout end=eof;

%do i = 1 %to &nvar.;
      if Parameter = "&&var&i.." then beta_&&var&i.. = Estimate;
%end;
 
      dummyby = 1;

      if eof then output;

      retain  %do i = 1 %to &nvar.; beta_&&var&i.. %end;;
      keep    %do i = 1 %to &nvar.; beta_&&var&i.. %end; dummyby;
run;

**********************************************************************************************;
**  Compute the baseline cumulative hazard estimate, its variance and the vector of weighted  ;
**  average covariate values needed for risk estimate variance calculations.                  ;
**********************************************************************************************;

**  Sort observations by time.  If events have same time as censored observations, put events
**  first so that the censored observations remain in the risk set for that particular event;


proc sort data=qqxproduct;
     by &strata. &time. descending &censor.;  
 run;

data qqxproduct;
      set qqxproduct;
      dummyby = 1;  *  Dummy variable for merge;
%if %length(&weight.) = 0 %then %do;
      Sampling_weight = 1;
%end;
%else %do;
      Sampling_weight = &weight.;
%end;
run;

data qqtotal;

/*  First compute the sums of the product of the covariates with exp(xbeta) and exp(xbeta) for
**  all patients (risk set at time 0).
*/
      set qqxproduct end=eof;
%if %length(&strata.) %then %do;
      by &strata.;

      if first.&strat1. %do i = 2 %to &nstrata.; or first.&&strat&i.. %end; then do;
%do i = 1 %to &nvar.;
            sume = 0;
            sumxe&i.  = 0;
%end;      
            end;
%end;
      

%do i = 1 %to &nvar.;
      sumxe&i.  = sumxe&i. + &&var&i.. * exp(xbeta);
%end;
      sume = sume + exp(xbeta);

      dummyby  = 1;  *  Dummy variable for merge;

%if %length(&strata.) %then %do;
      if last.&strat1. %do i = 2 %to &nstrata.; or last.&&strat&i.. %end; then output;
%end;
%else %do;
      if eof then output;
%end;

      retain sumxe1-sumxe&nvar. sume;
      keep dummyby sumxe1-sumxe&nvar. sume &strata. dummyby;
run;


/*************************************************************************************/
/**  Baseline cumulative hazard.                                                    **/
/*************************************************************************************/

data qqcumhaz;

**  Now back out summands one by one from the total sum as the risk set decreases
**  to compute running total to estimate cumulative hazard, its variance and gamma;

      array ggamma(&nvar.) %do i = 1 %to &nvar.; ggamma_&&var&i.. %end;;
      array sumxe(&nvar.) sumxe1-sumxe&nvar.;

      merge qqxproduct end=eof qqtotal;
%if %length(&strata.) %then %do;
      by &strata.;
%end;
%else %do;
      by dummyby;
%end;

%if %length(&strata.) %then %do;
      if first.&strat1. %do i = 2 %to &nstrata.; or first.&&strat&i.. %end; then do;
          cumhaz = 0;
          varcumhaz = 0;
          do i = 1 to &nvar.;
               ggamma(i) = 0;
               end;
          end;
%end;

      if &time. <= &risktime. - 2 * &exten. then factor = 1;
      else factor = 2;
     
      if &censor. = 1 and &time. <= (&risktime. - &exten.) then do;
          cumhaz = cumhaz + factor * Sampling_weight / sume;
          varcumhaz = varcumhaz + (factor * Sampling_weight / sume)**2;
          do i = 1 to &nvar.;
            ggamma(i) = ggamma(i) + factor * Sampling_weight * sumxe(i) / (sume**2);
            end;
          end;

      if &time. <= (&risktime. - &exten.) then do;
%do i = 1 %to &nvar.;
           sumxe&i. = sumxe&i. - &&var&i.. * exp(xbeta);
%end;
           sume   = sume - exp(xbeta);
           end;
           
%if %length(&strata.) %then %do;
      if last.&strat1. %do i = 2 %to &nstrata.; or last.&&strat&i.. %end; then do;
           stdcumhaz = sqrt(varcumhaz);
           output;
           end;
%end;
%else %do;
      if eof then do;
           stdcumhaz = sqrt(varcumhaz);
           output;
           end;
%end;

      retain %do i = 1 %to &nvar.; ggamma_&&var&i.. %end; cumhaz varcumhaz sumxe1-sumxe&nvar. sume;
      keep   %do i = 1 %to &nvar.; ggamma_&&var&i.. %end; cumhaz stdcumhaz &strata. dummyby;
run;

data &keysdsn.;
      merge qqbk qqvk qqcumhaz;
      by dummyby;
run;

%mend key_stats_PHREG;