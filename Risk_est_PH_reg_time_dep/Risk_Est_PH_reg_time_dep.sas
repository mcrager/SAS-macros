/***********************************************************************************************************************

Program Name           : Risk_Est_PH_reg_time_dep.sas

Path                   : 

Program Language       : SAS

Operating System       : Server

Purpose                : Estimate the risk of an event by a specified time using Cox proportional hazards regression.  Cohort
                         sampling study designs and time-dependent covariates are accomodated.  Ties in event times are handled
                         using the Efron method.
                         
Notes                  : 

Status                 : Tested and verified.

Run Dependencies       : None

Input Datasets/Views   : Specified SAS data set indsn containing time-to-event data, covariates, and a variable giving the
                         sampling weights if cohort sampling was used.  Optionally, a separate data set giving covariate
                         values for which the risk is to be estimated.  

Output Datasets/Views  : Specified SAS data set outdsn containing risk estimate for each patient in the data set and a 
                         confidence interval.  The estimated cumulative hazard and confidence interval are also included,
                         as is the estimated log cumulative hazard and its standard err.
                         
Other Output           : None

Macro calls internal   : 

Macro calls external   : 

***********************************************************************************************************************/

%macro Risk_Est_PH_reg_time_dep(
        /* Input Specification */  indsn=,byvar=,vars=,time=,censor=,censorlist=0,entrytime=,weight=,
                                   programming_statements=%str(),calc_vars=,covariate_dsn=,
        /* Analysis Parameters */  risk_time=,robust=no,print=yes,alpha=0.05,strata=,CI_method=loglog,
        /* Output Specification */ outdsn=,Risk=Risk,Risk_LCL=Risk_LCL,Risk_UCL=Risk_UCL,
                                           CumHaz=CumHaz,CumHaz_LCL=CumHaz_LCL,CumHaz_UCL=CumHaz_UCL,
                                           LogCumHaz=LogCumHaz,SE_LogCumHaz=SE_LogCumHaz
                         );
            
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
| Name           : byvar
| Required (Y/N) : N
| Default Value  :
| Type ($/#)     : $ or #
| Purpose        : List of input data set variables to do the analysis by.                    
|-----------------------------------------------------------------------------------------------
| Name           : vars
| Required (Y/N) : Y
| Default Value  :
| Type ($/#)     : $ or #
| Purpose        : List of input data set variables to be used as the covariate in the Cox model 
|                  used to estimate the risk.  If the programming statements create the variables
|                  that are to be included in the model, list the variables thus created.
|-----------------------------------------------------------------------------------------------
| Name           : time
| Required (Y/N) : Y
| Default Value  : 
| Type ($/#)     : #
| Purpose        : Input data set variable containing the time to event (or censoring). 
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
| Name           : entrytime
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : #
| Purpose        : Optional input data set variable containing a left truncation time for each observation. 
|-----------------------------------------------------------------------------------------------
| Name           : weight  
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : #
| Purpose        : Input data set variable giving the observation's weight in the analysis. If this parameter
|                  is set, it is assumed that cohort sampling was used and resulted in the specified weights.
|-----------------------------------------------------------------------------------------------
| Name           : programming_statements
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : %str()-enclosed text string including programming statements that will be inserted into proc PHREG and various
|                  data steps to compute the time-dependent covariate values.  For example:
|                  programming_statements = %str(if time <= 3 then x_3 = 0; else x_3 = x;) If no programming statements
|                  are entered, the risk calculations will be made for covariates that are constant over time.
|-----------------------------------------------------------------------------------------------
| Name           : calc_vars
| Required (Y/N) : Y if time-dependent covariates are used.
| Default Value  : 
| Type ($/#)     : $
| Purpose        : List of variables that are used in the calculation of the time-dependent covariates. 
|                  IF THE MODEL ALSO INCLUDES TIME-CONSTANT COVARIATES, BE SURE TO INCLUDE THEM 
|                  IN THE LIST.  Leave the calc_vars parameter blank if time dependent variables are not used. 
|-----------------------------------------------------------------------------------------------
| Name           : covariate_dsn                
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : Libname reference and the name of a data set that contains the covariate values for which 
|                  the risk is to be estimated.  The data set must have all the variables included in the 
|                  model, or that are required to derive these variables if the model has time-dependent covariates
|                  derived using programming statements.  The data set must also include the stratification variable if
|                  the model is stratified.  No by variables should be included in the covariate data set.
|
|                  If no covariate data set is specified, the risk will be estimated for 
|                  every patient in the main input data set.
|-----------------------------------------------------------------------------------------------
| Name           : risk_time  
| Required (Y/N) : Y
| Default Value  : 
| Type ($/#)     : #
| Purpose        : This is the time at which the risk of each patient is assessed.  That is, the risk is defined
|                  as the probability that the patient will have the event on or before risk_time.
|-----------------------------------------------------------------------------------------------
| Name           : robust   
| Required (Y/N) : N
| Default Value  : no
| Type ($/#)     : $
| Purpose        : If this parameter is set to yes and cohort sampling is NOT used (parameter "weight" is not specified), 
|                  the Lin-Wei robust estimate of variance will be used in the Cox proportional hazards model.
|                  This parameter has no effect when the parameter "weight" is specified.
|-----------------------------------------------------------------------------------------------
| Name           : print   
| Required (Y/N) : N
| Default Value  : yes
| Type ($/#)     : $
| Purpose        : If this parameter is set to no, the PROC PHREG output will not be printed.                      
|-----------------------------------------------------------------------------------------------
| Name           : alpha   
| Required (Y/N) : N
| Default Value  : 0.05
| Type ($/#)     : #
| Purpose        : The macro will compute a 100(1-alpha)% confidence interval for the risk and cumulative hazard.                      
|-----------------------------------------------------------------------------------------------
| Name           : strata   
| Required (Y/N) : No
| Default Value  : 
| Type ($/#)     : 
| Purpose        : Character string giving input data set variable by which the proportional hazards regression
|                  analysis will be stratified.                       
|-----------------------------------------------------------------------------------------------
| Name           : CI_method   
| Required (Y/N) : No
| Default Value  : loglog
| Type ($/#)     : $
| Purpose        : Character string giving the method for computing the confidence intervals.  If linear is specified,
|                  the confidence interval is computed on the risk scale.  If log is specified, the confidence interval
|                  is computed on the cumulative hazard scale and transformed to the risk scale.  If loglog is 
|                  specified, the confidence interval is computed on the log cumulative hazard scale and transformed
|                  to the risk scale.  The default method is loglog.   
|-----------------------------------------------------------------------------------------------
| Name           : outdsn  
| Required (Y/N) : Y
| Default Value  : (at temporary library)
| Type ($/#)     : $
| Purpose        : Libname reference and the output data set name.  The dataset name must conform to the rules for
|                  SAS names.  This data set will contain all the records and variables of the covariate data set 
|                  (or the input data set if no separate coavariate data set is specified) plus the variables 
|                  named by the following eight macro parameters.
|-----------------------------------------------------------------------------------------------
| Name           : Risk     
| Required (Y/N) : N
| Default Value  : Risk    
| Type ($/#)     : #
| Purpose        : Name of output data set variable that will contain the risk estimate. 
|-----------------------------------------------------------------------------------------------
| Name           : Risk_LCL
| Required (Y/N) : N
| Default Value  : Risk_LCL
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the lower limit of a 1-alpha confidence
|                  interval for the risk.                                    
|-----------------------------------------------------------------------------------------------
| Name           : Risk_UCL
| Required (Y/N) : N
| Default Value  : Risk_UCL
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the upper limit of a 1-alpha confidence
|                  interval for the risk.                                
|-----------------------------------------------------------------------------------------------
| Name           : CumHaz     
| Required (Y/N) : N
| Default Value  : CumHaz    
| Type ($/#)     : #
| Purpose        : Name of output data set variable that will contain the cumulative hazard estimate. 
|-----------------------------------------------------------------------------------------------
| Name           : CumHaz_LCL
| Required (Y/N) : N
| Default Value  : CumHaz_LCL
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the lower limit of a 1-alpha confidence
|                  interval for the cumulative hazard.                                    
|-----------------------------------------------------------------------------------------------
| Name           : CumHaz_UCL
| Required (Y/N) : N
| Default Value  : CumHaz_UCL
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the upper limit of a 1-alpha confidence
|                  interval for the cumulative hazard. 
|-----------------------------------------------------------------------------------------------
| Name           : LogCumHaz     
| Required (Y/N) : N
| Default Value  : LogCumHaz    
| Type ($/#)     : #
| Purpose        : Name of output data set variable that will contain the log cumulative hazard estimate. 
|-----------------------------------------------------------------------------------------------
| Name           : SE_LogCumHaz     
| Required (Y/N) : N
| Default Value  : SE_LogCumHaz    
| Type ($/#)     : #
| Purpose        : Name of output data set variable that will contain the estimated standard error of the log
|                  cumulative hazard estimate. 
**********************************************************************************************************/;
 
%local nvar nvar2 ncalc_var i j k byvar byflag min_weight in_calc_vars L vars calc_vars dpc_var dpc_calc_var;


/* Check for errors in macro parameter specification */

   %if %length(&indsn.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_time_dep macro parameter indsn must be specified.;
      %abort;
   %end;

   %if %length(&vars.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_time_dep macro parameter vars must be specified.;
      %abort;
   %end;

   %if %length(&time.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_time_dep macro parameter time must be specified.;
      %abort;
   %end;
   
   %if %length(&censor.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_time_dep macro parameter censor must be specified.;
      %abort;
   %end;

   %if %length(&risk_time.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_time_dep macro parameter risk_time must be specified.;
      %abort;
   %end;
   
   %if %length(&programming_statements.)>0 and %length(&calc_vars.) =0 %then %do;
      %put ERROR : Risk_Est_PH_reg_time_dep macro parameter calc_vars must be specified;
      %put                   when macro parameter programming_statements is specified.;
      %abort;
   %end;

   %if %length(&outdsn.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_time_dep macro parameter outdsn must be specified.;
      %abort;
   %end;

   %if %length(&alpha.) %then %do;
   %if %sysevalf(&alpha. <= 0) or %sysevalf(&alpha. >= 1) %then %do;
      %put ERROR : Risk_Est_PH_reg_time_dep macro parameter alpha must be >0 and <1.;
      %abort;
   %end;
   %end;
   

/**********************************************************************************************
**  Set macro parameters specified as blank to their default values.                          *
**********************************************************************************************/

   %if %length(&censorlist.) = 0 %then %let censorlist = 0;
   %if %length(&robust.) = 0 %then %let robust = no;
   %if %length(&print.) = 0 %then %let print = yes;
   %if %length(&alpha.) = 0 %then %let alpha = 0.05;
   %if %length(&Risk.) = 0 %then %let Risk = Risk;
   %if %length(&Risk_LCL.) = 0 %then %let Risk_LCL = Risk_LCL;
   %if %length(&Risk_UCL.) = 0 %then %let Risk_UCL = Risk_UCL;  
   %if %length(&CumHaz.) = 0 %then %let CumHaz = CumHaz;
   %if %length(&CumHaz_LCL.) = 0 %then %let CumHaz_LCL = CumHaz_LCL;
   %if %length(&CumHaz_UCL.) = 0 %then %let CumHaz_UCL = CumHaz_UCL;
   %if %length(&LogCumHaz.) = 0 %then %let LogCumHaz = LogCumHaz;
   %if %length(&SE_LogCumHaz.) = 0 %then %let SE_LogCumHaz = SE_LogCumHaz;
   %if %length(&CI_method.) = 0 %then %let CI_method = loglog;
   
/**********************************************************************************************
**  Parse the string containing the "by variable".                                            *
**********************************************************************************************/

%let byflag = 0;

%if %length(&byvar.) %then %do;
   %let byvar=%sysfunc(compbl(&byvar.));
   %let byflag = 1;
%end;

%local nbyvar;

%let nbyvar = %sysfunc(countc(%bquote(&byvar.), %str( )));
%let nbyvar = %sysfunc(ifc(%length(%bquote(&byvar.)), %eval(&nbyvar. + 1), 0));
   
%do i = 1 %to &nbyvar.;
   %local byvar&i.;
   %let byvar&i = %scan(&byvar., &i., %str( ));
%end;         

%if %length(&byvar.) = 0 %then %do;
%let byvar = dummyby;
%let byvar1 = dummyby;
%let nbyvar = 1;
%end; 
                     
/**********************************************************************************************
**  Parse the input parameter vars containing the list of covariates, determine the number    *
**  of covariates, and assign them to the macro variables var1, var2, etc.  Include the       *
**  new variable that will be created to be the intercept term.                               *
**********************************************************************************************/

%let vars = %sysfunc(compbl(&vars.));

%let nvar = %sysfunc(countc(%bquote(&vars.), %str( )));
%let nvar = %sysfunc(ifc(%length(%bquote(&vars.)), %eval(&nvar. + 1), 0));

%do i = 1 %to &nvar.;
   %local var&i.;
   %let var&i = %scan(&vars., &i., %str( ));
%end; 

%let nvar2 = %sysevalf(&nvar.**2,integer);   

/**********************************************************************************************
**  Parse the input parameter calc_vars containing the list of variables that are used to     *
**  calculate the time-dependent covariates.  If field is blank, substitute the contents of   *
**  vars.                                                                                     *
**********************************************************************************************/

%if %length(&calc_vars.) = 0 %then %let calc_vars = &vars.;  

%let calc_vars = %sysfunc(compbl(&calc_vars.));

%let ncalc_var = %sysfunc(countc(%bquote(&calc_vars.), %str( )));
%let ncalc_var = %sysfunc(ifc(%length(%bquote(&calc_vars.)), %eval(&ncalc_var. + 1), 0));

%do i = 1 %to &ncalc_var.;
   %local calc_var&i.;
   %let calc_var&i = %scan(&calc_vars., &i., %str( ));
%end; 

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
     %put ERROR : Risk_Est_PH_reg_time_dep macro found that specified input data set &indsn. does not exist.;
     %abort;
     %end;
   %else %do;
     %if &byflag. %then %do k = 1 %to &nbyvar.;
          %if %sysfunc(varnum(&dsid.,&&byvar&k..)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_time_dep macro input data set &indsn. does not contain the variable &&byvar&k.. specified in parameter byvar.;
            %let errcode = 1;
            %end;
         %end;
     %if %sysfunc(varnum(&dsid.,&time.)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_time_dep macro input data set &indsn. does not contain the variable &time. specified in macro parameter time.;
            %let errcode = 1;
            %end;
     %if %sysfunc(varnum(&dsid.,&censor.)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_time_dep macro input data set &indsn. does not contain the variable &censor. specified in macro parameter censor.;
            %let errcode = 1;
            %end;   
     %if %length(&calc_vars.) %then %do k = 1 %to &ncalc_var.;
          %if %sysfunc(varnum(&dsid.,&&calc_var&k..)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_time_dep macro input data set &indsn. does not contain the variable &&calc_var&k.. specified in parameter vars or calc_vars.;
            %let errcode = 1;
            %end;
         %end;
     %if %length(&entrytime.) %then %do;
         %if %sysfunc(varnum(&dsid.,&entrytime.)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_time_dep macro input data set &indsn. does not contain variable &entrytime. specified in macro parameter entrytime.;
            %let errcode = 1; 
            %end;
         %end;
     %if %length(&weight.) %then %do;
         %if %sysfunc(varnum(&dsid.,&weight.)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_time_dep macro input data set &indsn. does not contain variable &weight. specified in macro parameter weight.;
            %let errcode = 1; 
            %end;
         %end;
     %if %length(&strata.) %then %do k = 1 %to &nstrata.;
         %if %sysfunc(varnum(&dsid.,&&strat&k..)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_time_dep macro input data set &indsn. does not contain variable &&strat&k.. specified in macro parameter strata.;
            %let errcode = 1;
            %end;
         %end;
     %let rc = %sysfunc(close(&dsid.));
     %if &errcode = 1 %then %abort;
     %end;

     %if %length(&covariate_dsn.) %then %do;
     %let errcode = 0;
     %let dsid = %sysfunc(open(&covariate_dsn.,i));
     %if &dsid. = 0 %then %do;
        %put ERROR : Risk_Est_PH_reg_time_dep macro found that specified covariate data set &covariate_dsn. does not exist.;
        %abort;
        %end;
      %else %do;
        %if &byflag. %then %do k = 1 %to &nbyvar.;
             %if %sysfunc(varnum(&dsid.,&&byvar&k..)) ne 0 %then %do;
               %put ERROR : Risk_Est_PH_reg_time_dep macro covariate data set &covariate_dsn. contains the variable &&byvar&k.. specified in parameter byvar. This data set may not contain by variables.;
               %let errcode = 1;
               %end;
            %end;
        %if %length(&calc_vars.) %then %do k = 1 %to &ncalc_var.;
             %if %sysfunc(varnum(&dsid.,&&calc_var&k..)) = 0 %then %do;
               %put ERROR : Risk_Est_PH_reg_time_dep macro covariate data set &covariate_dsn. does not contain the variable &&calc_var&k.. specified in parameter vars or calc_vars.;
               %let errcode = 1;
               %end;
            %end;
        %if %length(&strata.) %then %do k = 1 %to &nstrata.;
            %if %sysfunc(varnum(&dsid.,&&strat&k..)) = 0 %then %do;
               %put ERROR : Risk_Est_PH_reg_time_dep macro covariate data set &covariate_dsn. does not contain variable &&strat&k.. specified in macro parameter strata.;
               %let errcode = 1;
               %end;
            %end;
       %let rc = %sysfunc(close(&dsid.));
       %if &errcode = 1 %then %abort;
       %end; 
     %end;

/**********************************************************************************************
**  Make sure that any time-constant covariates are included in the calc_vars list.           *
**********************************************************************************************/

%if %length(&programming_statements.) %then %do;

%let dsid = %sysfunc(open(&indsn.,i));

%do k = 1 %to &nvar.;
     %if %sysfunc(varnum(&dsid.,&&var&k..)) ne 0 %then %do;
         %let in_calc_vars = 0;
	 %do L = 1 %to &ncalc_var.;
	    %if &&var&k.. eq &&calc_var&L.. %then %let in_calc_vars = 1;
	    %end;
         %if &in_calc_vars. = 0 %then %do;
	     %let calc_vars = &calc_vars. &&var&k..;
             %put WARNING : Apparent time-constant variable &&var&k.. was not specified in macro Risk_Est_PH_reg_time_dep parameter calc_vars.  This variable has been added to the list in calc_vars.;
             %end;
         %end;
     %end;

%let rc = %sysfunc(close(&dsid.));
  
%end;     

/**********************************************************************************************
**  Get the input data.  Create a version of the censoring variable where 1 = event and 0 =   *
**  no event.  (This will be needed later on.)                                                *
**********************************************************************************************/

data _qq_indata;
     set &indsn;
     dummyby = 1;  /* Dummy by variable for merges */
     
/* Delete any observation that does not have values for the time-to-event variables. */
     if &time. = . or &censor. = . then delete;

/* Delete any observation that has missing covariate values */

%do i = 1 %to &ncalc_var.;
     if &&calc_var&i.. = . then delete;
%end;

%if %length(&strata.) %then %do i = 1 %to &nstrata.;
     if vtype(&&strat&i..) = 'C' then do;
          if &&strat&i.. = ' ' then delete;
          end;
          
     if vtype(&&strat&i..) = 'N' then do;
          if &&strat&i.. = .   then delete;
          end;
%end;

     if &censor. in (&censorlist.) then qqeventvar = 0;
                                   else qqeventvar = 1;

/* Create weight variable that defaults to 1 if no weights are specified */

%if %length(&weight.)> 0 %then %do; qqweight = &weight.; %end;
                         %else %do; qqweight = 1; %end;

/* Delete any observation that has 0 weight */
     if qqweight = 0 then delete;     
  
/* If left truncation time is specified, delete any observation with  */
/* follow-up time less than the left truncation time.                 */

%if %length(&entrytime.) %then %do;
     if &time. < &entrytime. then delete;
%end;

run;

/********************************************************************************************/
/**  If by variables or stratification variables are used, sort input data set by them.    **/
/********************************************************************************************/

%if &byflag. = 1 or %length(&strata.) %then %do;
proc sort data=_qq_indata;
      by &byvar. &strata.;
run;
%end;


/**********************************************************************************************
**  Run the proportional hazards regression analysis.  If a weight is specified, incorporate  *
**  it in the analysis and use the covariate sandwich estimate of the covariance matrix.      *
**********************************************************************************************/

%if %length(&weight.) = 0 %then %do;

/**********************************************************************************************
**  Case 1: no cohort sampling.                                                              **
**********************************************************************************************/
proc phreg data=_qq_indata outest=_qq_outest covout %if &robust. = yes %then covs;
           %if &print. = no %then noprint;;
     by &byvar.;
     model &time.*qqeventvar(0) = %do i = 1 %to &nvar.; &&var&i.. %end; 
                / rl covb ties=efron %if %length(&entrytime.) %then entrytime = &entrytime.;;
%if %length(&strata.) %then %do;
     strata &strata.;
%end;
     &programming_statements.;
run;
%end;  
                         %else %do;

/**********************************************************************************************
**  Case 2: Cohort sampling design with analysis using Lin and Wei covariate sandwich method.**
**********************************************************************************************/
proc phreg data=_qq_indata covs outest=_qq_outest covout %if &print. = no %then noprint;;
     by &byvar.;
     model &time.*qqeventvar(0) = %do i = 1 %to &nvar.; &&var&i.. %end; 
                / rl covb ties=efron %if %length(&entrytime.) %then entrytime = &entrytime.;;
     weight &weight.;
%if %length(&strata.) %then %do;
     strata &strata.;
%end;
     &programming_statements.;
run;

%end;

/**********************************************************************************************
**  Capture the variance-covariance matrix of the parameter estimates.                        *
**********************************************************************************************/

data _qq_v;
      array covariate(&nvar.) %do i = 1 %to &nvar.; &&var&i.. %end;; 
      array v(&nvar.,&nvar.) v1-v&nvar2.;
 
      set _qq_outest;
      by &byvar.;
      where upcase(_NAME_) ne upcase("&time.");

/*  Initialize cov matrix to missing values */

      if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; 
        then do i = 1 to &nvar.;
               do j = 1 to &nvar.;
                  v(i,j) = .;
                  end;
               end;

%do i = 1 %to &nvar.;
      if upcase(_NAME_) = upcase("&&var&i..") then row = &i;
%end;
      
      if row ne . then do column = 1 to &nvar.;
          v(row,column) = covariate(column);
          end;

      if last.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or last.&&byvar&i.. %end; then output; 

      retain v1-v&nvar2.;   
      keep v1-v&nvar2. &byvar.;
run;


/**********************************************************************************************
**  Capture the regression parameter estimates.                                              **
**********************************************************************************************/

data _qq_b;

      array b(&nvar.) b1-b&nvar.;
      array covariate(&nvar.) %do i = 1 %to &nvar.; &&var&i.. %end;; 

      set _qq_outest;
      by &byvar.;;
      where upcase(_NAME_) = upcase("&time.");

/* Initialize regression parameter estimates to missing values */

      if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; 
        then do i = 1 to &nvar.;
               b(i) = .;
               end;

      do i = 1 to &nvar.;
        b(i) = covariate(i);
        end;  

      keep b1-b&nvar. &byvar.;
run;

/*********************************************************************************************/
/*  Sort observations by time.  If events have same time as censored observations, put events*/
/*  first so that the censored observations remain in the risk set for that particular event */
/*********************************************************************************************/

proc sort data=_qq_indata;
     by &byvar. &strata. &time. descending qqeventvar;  
 run;
 
/*********************************************************************************************/
/*  Count the observations in each stratum and by group.                                    **/
/*********************************************************************************************/

proc univariate data=_qq_indata noprint;
     by &byvar. &strata.;
     var &time.;
     output out = _qq_nobs n = npt;
run;

/*********************************************************************************************/
/**  Handle the ties in event times using the Efron approach. For each k-way tie, this can  **/
/**  be accomplished by substituting k observations with weight equal to the average weight **/
/**  and risk score equal to the average risk score at the observation time.                **/
/*********************************************************************************************/

data _qq_indata_ties;
     merge _qq_indata _qq_b;
     by &byvar.;
     
     &programming_statements.;

     xbeta = 0;             
%do i = 1 %to &nvar.;
     xbeta = xbeta + b&i. * &&var&i..;
%end;
     exp_xbeta = exp(xbeta);
     
     drop b1-b&nvar.;
run;

proc univariate data=_qq_indata_ties noprint;
     by &byvar. &strata. &time. descending qqeventvar;
     where qqeventvar = 1;
     var qqweight;
     output out=_qq_tie_mean_wt mean = mean_weight;
run;

proc univariate data=_qq_indata_ties noprint;
     by &byvar. &strata. &time. descending qqeventvar;
     weight qqweight;
     where qqeventvar = 1;
     var exp_xbeta;
     output out=_qq_tie_mean_wt_exp_xbeta mean = mean_wt_exp_xbeta;
run;

data _qq_indata;
     merge _qq_indata _qq_tie_mean_wt _qq_tie_mean_wt_exp_xbeta;
     by &byvar. &strata. &time. descending qqeventvar;   
run;  

data _qq_tie_ind;
     set _qq_indata;
     
     lag_&time. = lag(&time.);
     lag_qqeventvar = lag(qqeventvar);
     
     if qqeventvar = 1 and lag_qqeventvar = 1 and &time. = lag_&time. then output;
     
     keep &byvar. &strata. &time. qqeventvar;
run;

data _qq_indata;
     merge _qq_indata _qq_tie_ind(in=a);
     by &byvar. &strata. &time. descending qqeventvar;        
     if a then tie_ind = 1;
          else tie_ind = 0;
run;

/*********************************************************************************************/
/** Produce a horizontal data set with the observation times, event indicator,              **/
/** sampling weight, covariate values needed to calculate the time-dependent covariates,    **/
/** and weighted average risk scores (needed for handling ties) in columns.  The horizontal **/
/** data set has a record for each by-group and stratum.                                    **/
/*********************************************************************************************/

proc sql noprint;
     select max(npt) into :maxnpt from _qq_nobs;
quit;

%let maxnpt = %cmpres(&maxnpt.);
%let dpc_calc_var = %sysevalf(&maxnpt.*&ncalc_var.,integer);
%let dpc_calc_var = %cmpres(&dpc_calc_var.);
%let dpc_var = %sysevalf(&maxnpt.*&nvar.,integer);
%let dpc_var = %cmpres(&dpc_var.);

data _qq_horiz;
 
     array pttime(&maxnpt.) pttime1-pttime&maxnpt.;
     array ptevent(&maxnpt.) ptevent1-ptevent&maxnpt.;
     array ptweight(&maxnpt.) ptweight1-ptweight&maxnpt.;
     array ptcovariate(&ncalc_var.,&maxnpt.) ptcovariate1-ptcovariate&dpc_calc_var.;
     array pt_tie_wt_exp_xbeta(&maxnpt.) pt_tie_wt_exp_xbeta1-pt_tie_wt_exp_xbeta&maxnpt.; 
     array pt_tie_wt(&maxnpt.) pt_tie_wt1-pt_tie_wt&maxnpt.; 
     array pt_tie_ind(&maxnpt.) pt_tie_ind1-pt_tie_ind&maxnpt.; 
%if %length(&entrytime.) %then %do;
     array ptentrytime(&maxnpt.) ptentrytime1-ptentrytime&maxnpt.;
%end;


     merge _qq_indata _qq_nobs;
     by &byvar. &strata.; 
     
   
     if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; 
                            %if %length(&strata.) %then %do i = 1 %to &nstrata.; or first.&&strat&i.. %end;
        then  do i = 1 to &maxnpt.;
            pttime(i) = .;
            ptevent(i) = .;
            ptweight(i) = .;
%do i = 1 %to &ncalc_var.;
            ptcovariate(&i.,i) = .;
%end;
            pt_tie_wt_exp_xbeta(i) = .;
            pt_tie_wt(i) = .;
            index = 0;
            end;
        
     index = index + 1;
     pttime(index) = &time.;
     ptevent(index) = qqeventvar;
     ptweight(index) = qqweight;
%do i = 1 %to &ncalc_var.;
     ptcovariate(&i.,index) = &&calc_var&i..;
%end;
     pt_tie_wt_exp_xbeta(index) = mean_wt_exp_xbeta;
     pt_tie_wt(index) = mean_weight;
     pt_tie_ind(index) = tie_ind;
%if %length(&entrytime.) %then %do;
     ptentrytime(index) = &entrytime.;
%end;

     if last.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or last.&&byvar&i.. %end; 
                            %if %length(&strata.) %then %do i = 1 %to &nstrata.; or last.&&strat&i.. %end;
        then output;

     retain index pttime1-pttime&maxnpt. ptevent1-ptevent&maxnpt. ptweight1-ptweight&maxnpt.  
            ptcovariate1-ptcovariate&dpc_calc_var. pt_tie_wt_exp_xbeta1-pt_tie_wt_exp_xbeta&maxnpt.
            pt_tie_wt1-pt_tie_wt&maxnpt. %if %length(&entrytime.) %then ptentrytime1-ptentrytime&maxnpt.;;
            
     keep  &byvar. &strata. pttime1-pttime&maxnpt.  ptevent1-ptevent&maxnpt. ptweight1-ptweight&maxnpt.
           ptcovariate1-ptcovariate&dpc_calc_var. pt_tie_wt_exp_xbeta1-pt_tie_wt_exp_xbeta&maxnpt.
           pt_tie_wt1-pt_tie_wt&maxnpt. pt_tie_ind1-pt_tie_ind&maxnpt.
           %if %length(&entrytime.) %then ptentrytime1-ptentrytime&maxnpt.;;
run; 

/*********************************************************************************************/
/**  Calculate the numerator and denominator terms for the baseline cumulative hazard       **/
/**  calculation and and the weighted mean covariate values in each risk set.               **/
/*********************************************************************************************/

**  Create a version of the regression parameter data set that has a record for each stratum
**  within each by group;

proc sort data=_qq_indata out=_qq_bystrat nodupkeys;
     by &byvar. &strata.;
run;

data _qq_b;
     merge _qq_b _qq_bystrat;
     by &byvar.;
run;

data _qq_horiz;

     array pttime(&maxnpt.) pttime1-pttime&maxnpt.;
     array ptevent(&maxnpt.) ptevent1-ptevent&maxnpt.;
     array ptweight(&maxnpt.) ptweight1-ptweight&maxnpt.;
     array ptcovariate(&ncalc_var.,&maxnpt.) ptcovariate1-ptcovariate&dpc_calc_var.;   
     array ptdenom(&maxnpt.) ptdenom1-ptdenom&maxnpt.;
     array ptnumer(&maxnpt.) ptnumer1-ptnumer&maxnpt.;
     array ptcovmean(&nvar.,&maxnpt.) ptcovmean1-ptcovmean&dpc_var.;
     array pt_tie_wt_exp_xbeta(&maxnpt.) pt_tie_wt_exp_xbeta1-pt_tie_wt_exp_xbeta&maxnpt.;
     array pt_tie_wt(&maxnpt.) pt_tie_wt1-pt_tie_wt&maxnpt.;
     array pt_tie_ind(&maxnpt.) pt_tie_ind1-pt_tie_ind&maxnpt.;
%if %length(&entrytime.) %then %do;
     array ptentrytime(&maxnpt.) ptentrytime1-ptentrytime&maxnpt.;
%end;


     merge _qq_horiz  _qq_b _qq_nobs;
     by &byvar. &strata.;

     do index = 1 to npt;     
    
         denominator = 0;
%do i = 1 %to &nvar.;
         cov_sum&i. = 0;
%end;

         &time. = pttime(index);
         
         do pt = index to npt;
             
             if pt_tie_ind(pt) = 1 then do;
                exp_xbeta = pt_tie_wt_exp_xbeta(pt);
                qqweight = pt_tie_wt(pt);
                end;
             
             else do;
%do i = 1 %to &ncalc_var.;
                &&calc_var&i.. = ptcovariate(&i.,pt);
%end;
                &programming_statements.;

                xbeta_pt = 0;             
%do i = 1 %to &nvar.;
                xbeta_pt = xbeta_pt + b&i. * &&var&i..;
%end;
                exp_xbeta = exp(xbeta_pt);
                qqweight = ptweight(pt);
                end;

%if %length(&entrytime.) %then %do;
             if &time. >= ptentrytime(pt) then do;
                  denominator = denominator + qqweight * exp_xbeta;
%do i = 1 %to &nvar.;
                  cov_sum&i. = cov_sum&i. + qqweight * &&var&i.. * exp_xbeta;
%end;
                  end;
%end;

%else %do;
             denominator = denominator + qqweight * exp_xbeta;
%do i = 1 %to &nvar.;
             cov_sum&i. = cov_sum&i. + qqweight * &&var&i.. * exp_xbeta;
%end;
%end;
             end;

         if pt_tie_ind(index) = 1  then ptnumer(index) = pt_tie_wt(index);
                                   else ptnumer(index) = ptweight(index);
         
         ptdenom(index) = denominator;
         
%do i = 1 %to &nvar.;
         ptcovmean(&i.,index) = cov_sum&i. / denominator;
%end;
         end;

     keep &byvar. &strata. npt pttime1-pttime&maxnpt. ptevent1-ptevent&maxnpt. ptweight1-ptweight&maxnpt.
          ptcovariate1-ptcovariate&dpc_calc_var. ptnumer1-ptnumer&maxnpt. ptdenom1-ptdenom&maxnpt. 
          ptcovmean1-ptcovmean&dpc_var.
          %if %length(&entrytime.) %then ptentrytime1-ptentrytime&maxnpt.;; 
run;


/***********************************************************************************************
**  Combine regression parameter estimates and covariance matrix with the horizontal file.    **
***********************************************************************************************/

data _qq_keys;
     merge _qq_v _qq_b;
     by &byvar.;
     keep &byvar. &strata. b1-b&nvar. v1-v&nvar2.;           
run;

data _qq_horiz;
     merge _qq_horiz _qq_keys;
     by &byvar. &strata.;
run;

/***********************************************************************************************/
/**  Get the data set of covariate combinations for which the risk will be estimated.         **/
/***********************************************************************************************/

%if %length(&covariate_dsn.) %then %do;

data _qq_covnoby;
     set &covariate_dsn.;

/* Delete any observation that has missing covariate values */

%do i = 1 %to &ncalc_var.;
     if &&calc_var&i.. = . then delete;
%end;

     dummyby = 1;
     unique_record_id + 1;  /*  Unique record id to allow output data set to be in same sort
                                order as input data set */
run;

%if &byflag. = 1 %then %do;
proc sort data=_qq_indata(keep=&byvar.) out=_qq_byvalues nodupkeys;
     by &byvar.;
run;

proc sql;
     create table _qq_cov as select * from _qq_covnoby, _qq_byvalues;
quit;

%end;

%else %do;

data _qq_cov;
     set _qq_covnoby;
run;

%end;

%end;

%else %do;

data _qq_cov;
     set _qq_indata(keep = &byvar. &calc_vars. &strata.);

%do i = 1 %to &ncalc_var.;
     if &&calc_var&i.. = . then delete;
%end;
     unique_record_id + 1;  /*  Unique record id to allow output data set to be in same sort
                                order as input data set */
     run;
     
%end;

proc sort data=_qq_cov;
     by &byvar. &strata.;
run;

/***********************************************************************************************/
/**  Estimate the risk at the specified time for each combination of covariates in the        **/
/**  covariate data set, with a confidence interval.                                          **/
/***********************************************************************************************/

data _qq_riskest;
     array v(&nvar.,&nvar.) v1-v&nvar2.;
     array pttime(&maxnpt.) pttime1-pttime&maxnpt.;
     array ptevent(&maxnpt.) ptevent1-ptevent&maxnpt.;
     array ptweight(&maxnpt.) ptweight1-ptweight&maxnpt.;
     array ptcovariate(&ncalc_var.,&maxnpt.) ptcovariate1-ptcovariate&dpc_calc_var.;
     array ptnumer(&maxnpt.) ptnumer1-ptnumer&maxnpt.;
     array ptdenom(&maxnpt.) ptdenom1-ptdenom&maxnpt.;
     array ptcovmean(&nvar.,&maxnpt.) ptcovmean1-ptcovmean&dpc_var.;

     merge _qq_horiz _qq_cov _qq_nobs;
     by &byvar. &strata.;
     
     qqcumhaz = 0;
     varcumhaz = 0;
%do i = 1 %to &nvar.;
     q_vector&i. = 0;
%end;
  
 /**********************************************************************************************/
 /**  Compute cumulative hazard estimate for the combination of covariates.                   **/
 /**********************************************************************************************/

     do index = 1 to npt; 
         if ptevent(index) = 1 and pttime(index) <= &risk_time. then do; 
             &time. = pttime(index);  ** Compute the time-dependent xbeta for this patient;
             &programming_statements.;
             xbeta = 0;
 %do i = 1 %to &nvar.;
             xbeta = xbeta + b&i. * &&var&i..;
 %end; 
             exp_xbeta = exp(xbeta);
 
             qqcumhaz = qqcumhaz + exp_xbeta * ptnumer(index) / ptdenom(index);
             varcumhaz = varcumhaz + (exp_xbeta * ptnumer(index) / ptdenom(index))**2;
             
 %do i = 1 %to &nvar.;    
             q_vector&i. = q_vector&i. + exp_xbeta * (&&var&i.. - ptcovmean(&i.,index)) * ptnumer(index) / ptdenom(index); 
 %end;
             end;
         end;
       
 
 /**********************************************************************************************/                   
 /**  Compute variance of cumulative hazard estimate.                                         **/
 /**********************************************************************************************/
 
 %do i = 1 %to &nvar.;                 
     product&i. = 0;
 %end;
 
 %do i = 1 %to &nvar.;
 %do j = 1 %to &nvar.;
     product&i. = product&i. + v(&i.,&j.) * q_vector&j.;
 %end;
 %end;
 
     quadform = 0;
 %do i = 1 %to &nvar.;
     quadform = quadform + q_vector&i. * product&i.;
 %end;

     varhatch = varcumhaz + quadform;  
    
 /**  Standard error of log cumulative hazard estimate    **/
  
     if qqcumhaz > 0 then sigma = sqrt(varhatch) / qqcumhaz;
 
 /*********************************************************************************************/
 /**  Compute the risk estimate and confidence interval.                                     **/
 /*********************************************************************************************/

     if qqcumhaz > 0 then do;
          rho = log(qqcumhaz); 
          &Risk. = 1 - exp(-exp(rho));

%if %UPCASE(&CI_method.) = LINEAR or %UPCASE(&CI_method.) = LIN %then %do;

          &Risk_LCL. = &Risk. - probit(1-&alpha./2) * (1 - &Risk.) * sqrt(varhatch);
          &Risk_UCL. = &Risk. + probit(1-&alpha./2) * (1 - &Risk.) * sqrt(varhatch);

%end;

%else %if %UPCASE(&CI_method.) = LOG %then %do;

          &Risk_LCL. = 1 - exp(-qqcumhaz + probit(1-&alpha./2) * sqrt(varhatch));
          &Risk_UCL. = 1 - exp(-qqcumhaz - probit(1-&alpha./2) * sqrt(varhatch)); 

%end;
          
%else %do;

          &Risk_LCL. = 1 - exp(-exp(rho - probit(1-&alpha./2) * sigma));
          &Risk_UCL. = 1 - exp(-exp(rho + probit(1-&alpha./2) * sigma)); 

%end;
          end;
          
     else do;
          &Risk. = 0;
          &Risk_LCL. = .;
          &Risk_UCL. = .;
          end;
          
%let conf = %sysevalf(100*(1-&alpha.),integer);

       label &risk. = Estimated risk at time &risk_time.;
       label &risk_LCL. = Lower bound of &conf. pct confidence interval for risk at time &risk_time.;
       label &risk_UCL. = Upper bound of &conf. pct confidence interval for risk at time &risk_time.;

 /*********************************************************************************************/
 /**  Compute the cumulative hazard estimate and confidence interval.                        **/
 /*********************************************************************************************/      
 
     &CumHaz. = qqcumhaz;
     &CumHaz_LCL. = exp(rho - probit(1-&alpha./2) * sigma);
     &CumHaz_UCL. = exp(rho + probit(1-&alpha./2) * sigma); 

       label &CumHaz. = Estimated cumulative hazard at time &risk_time.;
       label &CumHaz_LCL. = Lower bound of &conf. pct confidence interval for cumulative hazard at time &risk_time.;
       label &CumHaz_UCL. = Upper bound of &conf. pct confidence interval for cumulative hazard at time &risk_time.;
     
     &LogCumHaz. = rho;
     &SE_LogCumHaz. = sigma;

       label &LogCumHaz. = Log cumulative hazard estimate at time &risk_time.;
       label &SE_LogCumHaz. = Standard Error of log cumulative hazard at time &risk_time.;
 
     keep unique_record_id &byvar. &risk. &risk_LCL. &risk_UCL. 
                           &CumHaz. &CumHaz_LCL. &CumHaz_UCL.
                           &LogCumHaz. &SE_LogCumHaz.;
run;

/***********************************************************************************************/
/**  Create output data set.                                                                  **/
/***********************************************************************************************/

proc sort data=_qq_riskest;
     by &byvar. unique_record_id;
run;

proc sort data=_qq_cov;
     by &byvar. unique_record_id;
run;


data &outdsn.;
     merge _qq_cov _qq_riskest;
     by &byvar. unique_record_id;
     
     drop unique_record_id %if &byflag.=0 %then dummyby;;
run;


%mend Risk_Est_PH_reg_time_dep;
