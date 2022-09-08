/***********************************************************************************************************************

Program Name           : Risk_Est_PH_reg_prop_score.sas

Path                   : 

Program Language       : SAS

Operating System       : Server

Purpose                : Estimate the risk of an event by a specified time using Cox proportional hazards regression using
                         propensity scores.  Cohort sampling study designs and external time-dependent covariates are accomodated.
                         
Notes                  : 

Run Dependencies       : None

Input Datasets/Views   : Specified SAS data set indsn containing time-to-event data, covariates, a variable giving the
                         sampling weights if cohort sampling was used, propensity outcome and covariates for modeling this outcome.
                         Optionally, a separate data set giving covariate values for which the risk is to be estimated.  

Output Datasets/Views  : Specified SAS data set outdsn containing risk estimate for each patient in the data set and a 
                         confidence interval.  The estimated cumulative hazard and confidence interval are also included,
                         as is the estimated log cumulative hazard and its standard error.  Optionally, output data sets
                         containing the Cox regression parameter estimates and their covariance matrix, a data set
                         containing standardized differences between response outcomes for specified variables 
                         and plots of these standardized differences.
                         
Other Output           : None

***********************************************************************************************************************/

%macro Risk_Est_PH_reg_prop_score(
        /* Input Specification */  indsn=,byvar=,vars=,vars_logistic=,time=,censor=,censorlist=0,entrytime=,weight=,
                                   response=,stabilize=yes,truncate_pct=0,
                                   programming_statements=%str(),calc_vars=,covariate_dsn=,
        /* Analysis Parameters */  risk_time=,print_phreg=yes,print_logistic=yes,alpha=0.05,strata=,CI_method=loglog,
        /* Output Specification */ outdsn=,Risk=Risk,Risk_LCL=Risk_LCL,Risk_UCL=Risk_UCL,
                                           CumHaz=CumHaz,CumHaz_LCL=CumHaz_LCL,CumHaz_UCL=CumHaz_UCL,
                                           LogCumHaz=LogCumHaz,SE_LogCumHaz=SE_LogCumHaz,
                                   parameter_estout=,parameter_covout=,
                                   std_diff_vars=,std_diff_out=,std_diff_graph_name=,graph_path=
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
| Type ($/#)     : #
| Purpose        : List of input data set variables to be used as the covariates in the Cox model 
|                  used to estimate the risk.  If  programming statements create variables
|                  that are to be included in the model, list the variables thus created along with
|                  any time-invariant covariates.
|-----------------------------------------------------------------------------------------------
| Name           : vars_logistic
| Required (Y/N) : Y
| Default Value  :
| Type ($/#)     : #
| Purpose        : List of input data set variables to be used as the covariates in the logistic 
|                  regression model for estimating the propensities. 
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
| Purpose        : Input data set variable giving the sampling weight if cohort sampling was used.
|-----------------------------------------------------------------------------------------------
| Name           : response  
| Required (Y/N) : Y
| Default Value  : 
| Type ($/#)     : $ or #
| Purpose        : Input data set variable giving the response for which the propensity will be estimated
|                  (such as treatment or biomarker use).  This may be a binary or multinomial outcome.
|-----------------------------------------------------------------------------------------------
| Name           : stabilize  
| Required (Y/N) : N
| Default Value  : yes
| Type ($/#)     : $
| Purpose        : If this parameter is set to yes, stabilized propensity score weights will be used (multiplying
|                  the inverse probability by the proportion of patients with the response).
|-----------------------------------------------------------------------------------------------
| Name           : truncate_pct  
| Required (Y/N) : N
| Default Value  : 0
| Type ($/#)     : #
| Purpose        : If this parameter is set to to a number, the propensity score weights will be truncated
|                  at the &truncate_pct and 100-&truncate_pct percentiles.  The truncation percentage will be rounded
|                  to the nearest tenth of a percent.  The truncation percentage must be non-negative and
|                  less than 50.  A typical value is 5 (truncating at the 5th and 95th percentiles).
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
| Name           : print_phreg   
| Required (Y/N) : N
| Default Value  : yes
| Type ($/#)     : $
| Purpose        : If this parameter is set to no, output from the phreg model fit will not be printed.                      
|-----------------------------------------------------------------------------------------------
| Name           : print_logistic   
| Required (Y/N) : N
| Default Value  : yes
| Type ($/#)     : $
| Purpose        : If this parameter is set to no, the PROC LOGISTIC output will not be printed.                      
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
|-----------------------------------------------------------------------------------------------
| Name           : parameter_estout  
| Required (Y/N) : N
| Default Value  : (at temporary library)
| Type ($/#)     : $
| Purpose        : Optional libname reference and name of data set that will contain the Cox regression
|                  parameter estimates, standard errors, chi-square statistics, p-values , hazard ratio estimates
|                  and confidence intervals, all computed using the covariance matrices accounting for the
|                  variability in the propensity score weight estimates. 
|-----------------------------------------------------------------------------------------------
| Name           : parameter_covout  
| Required (Y/N) : N
| Default Value  : (at temporary library)
| Type ($/#)     : $
| Purpose        : Optional libname reference and name of data set that will contain the Cox regression
|                  parameter estimate covariance matrix.  
|-----------------------------------------------------------------------------------------------
| Name           : std_diff_vars  
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : Optional list of variables for which the standardized differences among logistic
|                  regression outcomes will be assessed.
|-----------------------------------------------------------------------------------------------
| Name           : std_diff_out  
| Required (Y/N) : N
| Default Value  : (at temporary library)
| Type ($/#)     : $
| Purpose        : Optional libname reference and name of data set that will contain the weighted and 
|                  unweighted standardized differences among the logistic regression response outcomes
|                  for each logistic regression covariate.  
|-----------------------------------------------------------------------------------------------
| Name           : std_diff_graph_name  
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : Optional name for standardized difference graphs.  The graphs will be placed in
|                  the directory named in parameter graph_path and suffixes of the form _i_j will be
|                  affixed to the graph name specifying the number logistic regression outcomes being
|                  compared in each graph.   
|-----------------------------------------------------------------------------------------------
| Name           : graph_path  
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : Optional specification of folder in which to place the standardized difference
|                  graphs. 
|-----------------------------------------------------------------------------------------------
**********************************************************************************************************/;
**Michael Crager 20220719;
 
 
%local t nvar nvar2 nvar_logistic nvar_logstic2 ncalc_var i j k L byvar byflag min_weight in_calc_vars vars calc_vars dpc_var dpc_calc_var 
       nresponse nresponsem1 response_char response_length varxresp varxresp2 dpc_varsresp N truncate_pct_comp pctl_label comp_label 
       height scalemax unique_record_id;


proc optsave out=qqoptions;
run;

proc sql noprint;
    select OPTVALUE into :mergenobyoption
    from qqoptions where (OPTNAME = 'MERGENOBY');
quit;

options mergenoby=nowarn;


/* Check for errors in macro parameter specification */

   %if %length(&indsn.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_prop_score macro parameter indsn must be specified.;
      %abort;
   %end;

   %if %length(&vars.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_prop_score macro parameter vars must be specified.;
      %abort;
   %end;

   %if %length(&vars_logistic.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_prop_score macro parameter vars_logistic must be specified.;
      %abort;
   %end;

   %if %length(&time.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_prop_score macro parameter time must be specified.;
      %abort;
   %end;
   
   %if %length(&censor.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_prop_score macro parameter censor must be specified.;
      %abort;
   %end;

   %if %length(&response.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_prop_score macro parameter response must be specified.;
      %abort;
   %end;

   %if %length(&risk_time.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_prop_score macro parameter risk_time must be specified.;
      %abort;
   %end;
   
   %if %length(&programming_statements.)>0 and %length(&calc_vars.) =0 %then %do;
      %put ERROR : Risk_Est_PH_reg_prop_score macro parameter calc_vars must be specified;
      %put                   when macro parameter programming_statements is specified.;
      %abort;
   %end;

   %if %length(&outdsn.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_prop_score macro parameter outdsn must be specified.;
      %abort;
   %end;

   %if %length(&alpha.) %then %do;
   %if %sysevalf(&alpha. <= 0) or %sysevalf(&alpha. >= 1) %then %do;
      %put ERROR : Risk_Est_PH_reg_prop_score macro parameter alpha must be >0 and <1.;
      %abort;
   %end;
   %end;

   %if %length(&truncate_pct.) %then %do;
   %if %sysevalf(&truncate_pct. < 0) or %sysevalf(&truncate_pct. >= 50) %then %do;
      %put ERROR : Risk_Est_PH_reg_prop_score macro parameter truncate_pct must be non-negative and < 50.;
      %abort;
   %end;
   %end;   

/**********************************************************************************************
**  Set macro parameters specified as blank to their default values.                          *
**********************************************************************************************/

   %if %length(&truncate_pct.) = 0 %then %let truncate_pat = 0;
   %if %length(&censorlist.) = 0 %then %let censorlist = 0;
   %if %length(&print_phreg.) = 0 %then %let print_phreg = yes;
   %if %length(&print_logistic.) = 0 %then %let print_logistic = yes;
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
**  Parse the string containing by "by variable".                                             *
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
**  Parse the input parameter vars_logistic containing the list of covariates for the         *
**  logistic regression to estimate the propensities, determine the number of covariates, and *
**  assign them to the macro variables var_logistic1, var_logistic2, etc. Add the intercept   *
**  to the list of covariates.                                                                *
**********************************************************************************************/

%let vars_logistic = &vars_logistic. intercept;
%let vars_logistic = %sysfunc(compbl(&vars_logistic.));

%let nvar_logistic = %sysfunc(countc(%bquote(&vars_logistic.), %str( )));
%let nvar_logistic = %sysfunc(ifc(%length(%bquote(&vars_logistic.)), %eval(&nvar_logistic. + 1), 0));

%do i = 1 %to &nvar_logistic.;
   %local var_logistic&i.;
   %let var_logistic&i. = %scan(&vars_logistic., &i., %str( ));
%end; 

%let nvar_logistic2 = %sysevalf(&nvar_logistic.**2,integer);
                     
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
**  Parse the string containing the variables to assess standardized differences for.         *
**********************************************************************************************/

%if %length(&std_diff_vars.) %then %do;

%local nstd_diff_var;

%let std_diff_var = %sysfunc(compbl(&std_diff_vars.));
%let nstd_diff_var = %sysfunc(countc(%bquote(&std_diff_vars.), %str( )));
%let nstd_diff_var = %sysfunc(ifc(%length(%bquote(&std_diff_var.)), %eval(&nstd_diff_var. + 1), 0));
   
%do i = 1 %to &nstd_diff_var.;
   %local std_diff_var&i.;
   %let std_diff_var&i = %scan(&std_diff_vars., &i., %str( ));
%end;         

%end;

/**********************************************************************************************
**  Check for nonexistent input data sets and nonexistent variables in the input data sets.   *
**********************************************************************************************/

  %let errcode = 0;
  %let dsid = %sysfunc(open(&indsn.,i));
  %if &dsid. = 0 %then %do;
     %put ERROR : Risk_Est_PH_reg_prop_score macro found that specified input data set &indsn. does not exist.;
     %abort;
     %end;
   %else %do;
     %if &byflag. %then %do k = 1 %to &nbyvar.;
          %if %sysfunc(varnum(&dsid.,&&byvar&k..)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_prop_score macro input data set &indsn. does not contain the variable &&byvar&k.. specified in parameter byvar.;
            %let errcode = 1;
            %end;
         %end;
     %if %sysfunc(varnum(&dsid.,&time.)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_prop_score macro input data set &indsn. does not contain the variable &time. specified in macro parameter time.;
            %let errcode = 1;
            %end;
     %if %sysfunc(varnum(&dsid.,&censor.)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_prop_score macro input data set &indsn. does not contain the variable &censor. specified in macro parameter censor.;
            %let errcode = 1;
            %end;   
     %if %sysfunc(varnum(&dsid.,&response.)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_prop_score macro input data set &indsn. does not contain the variable &response. specified in macro parameter responsecensor.;
            %let errcode = 1;
            %end; 
     %if %length(&calc_vars.) %then %do k = 1 %to &ncalc_var.;
          %if %sysfunc(varnum(&dsid.,&&calc_var&k..)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_prop_score macro input data set &indsn. does not contain the variable &&calc_var&k.. specified in parameter vars or calc_vars.;
            %let errcode = 1;
            %end;
         %end;
     %do k = 1 %to &nvar_logistic.-1;
          %if %sysfunc(varnum(&dsid.,&&var_logistic&k..)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_prop_score macro input data set &indsn. does not contain the variable &&var_logistic&k.. specified in parameter vars_logistic;
            %let errcode = 1;
            %end;
         %end;
     %if %length(&entrytime.) %then %do;
         %if %sysfunc(varnum(&dsid.,&entrytime.)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_prop_score macro input data set &indsn. does not contain variable &entrytime. specified in macro parameter entrytime.;
            %let errcode = 1; 
            %end;
         %end;
     %if %length(&weight.) %then %do;
         %if %sysfunc(varnum(&dsid.,&weight.)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_prop_score macro input data set &indsn. does not contain variable &weight. specified in macro parameter weight.;
            %let errcode = 1; 
            %end;
         %end;
     %if %length(&strata.) %then %do k = 1 %to &nstrata.;
         %if %sysfunc(varnum(&dsid.,&&strat&k..)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_prop_score macro input data set &indsn. does not contain variable &&strat&k.. specified in macro parameter strata.;
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
        %put ERROR : Risk_Est_PH_reg_prop_score macro found that specified covariate data set &covariate_dsn. does not exist.;
        %abort;
        %end;
      %else %do;
        %if &byflag. %then %do k = 1 %to &nbyvar.;
             %if %sysfunc(varnum(&dsid.,&&byvar&k..)) ne 0 %then %do;
               %put ERROR : Risk_Est_PH_reg_prop_score macro covariate data set &covariate_dsn. contains the variable &&byvar&k.. specified in parameter byvar. This data set may not contain by variables.;
               %let errcode = 1;
               %end;
            %end;
        %if %length(&calc_vars.) %then %do k = 1 %to &ncalc_var.;
             %if %sysfunc(varnum(&dsid.,&&calc_var&k..)) = 0 %then %do;
               %put ERROR : Risk_Est_PH_reg_prop_score macro covariate data set &covariate_dsn. does not contain the variable &&calc_var&k.. specified in parameter vars or calc_vars.;
               %let errcode = 1;
               %end;
            %end;
        %if %length(&strata.) %then %do k = 1 %to &nstrata.;
            %if %sysfunc(varnum(&dsid.,&&strat&k..)) = 0 %then %do;
               %put ERROR : Risk_Est_PH_reg_prop_score macro covariate data set &covariate_dsn. does not contain variable &&strat&k.. specified in macro parameter strata.;
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
             %put WARNING : Apparent time-constant variable &&var&k.. was not specified in macro Risk_Est_PH_reg_prop_score parameter calc_vars.  This variable has been added to the list in calc_vars.;
             %end;
         %end;
     %end;

%let rc = %sysfunc(close(&dsid.));
  
%end;     

/**********************************************************************************************
**  Put the response variable values into macro variables.                                    *
**********************************************************************************************/

proc sql noprint;
     select distinct &response. into :response1-:response99 from &indsn.;
quit;

%let nresponse = &sqlobs.;
%let nresponsem1 = %sysevalf(&nresponse.-1,integer);

%do i = 1 %to &nresponse.;
%local response&i.;
%end;

%if &nresponse. = 1 %then %do;
     %put ERROR : Risk_Est_PH_reg_prop_score macro found response variable &response. has only one value.;
     %abort;
%end;


/**********************************************************************************************
**  Get the input data.  Create a version of the censoring variable where 1 = event and 0 =   *
**  no event.  (This will be needed later on.)                                                *
**********************************************************************************************/

data qqindata;
     set &indsn;
     
%if &byflag. = 0 %then %do;     
     dummyby = 1;  /* Dummy by variable for merges */
%end;

     intercept = 1;
     
/* Delete any observation that does not have values for the time-to-event variables. */
     if &time. = . or &censor. = . then delete;

/* Delete any observation that has missing covariate values */

%do i = 1 %to &ncalc_var.;
     if &&calc_var&i.. = . then delete;
%end;
%do i = 1 %to &nvar_logistic.;
     if &&var_logistic&i.. = . then delete;
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

/*  Delete any observation that has missing propensity score outcome */

     if vtype(&response.) = 'C' then do;
          call symput('response_char','1');
          call symput('response_length',trim(left(vlength(&response.))));
          if &response. = ' ' then delete;
          end;
          
     if vtype(&response.) = 'N' then do;
          call symput('response_char','0');
          if &response. = .   then delete;
          end;

run;

data qqindata;
     set qqindata;

/************************************************************************/
/*  Create a numeric response variables for the logistic regression and */
/*  indicator functions for the response categories.                    */
/************************************************************************/

%if &response_char. = 1 %then %do j = 1 %to &nresponse.;
     if &response. = "&&response&j.." then qqresponse_num = &j.;
%end;

%else %do j = 1 %to &nresponse.;
     if &response. = &&response&j.. then qqresponse_num = &j.;
%end;

run;


/********************************************************************************************/
/**  If by variables or stratification variables are used, sort input data set by them.    **/
/********************************************************************************************/

%if &byflag. = 1 or %length(&strata.) %then %do;
proc sort data=qqindata;
      by &byvar. &strata.;
run;
%end;

/********************************************************************************************/
/**  Count the records within each by group.  Set up an index variable that can be         **/
/**  used with a where clause when calling PROC IML, since IML does not support a by       **/
/**  statement.                                                                            **/
/********************************************************************************************/

data qqindata;
     set qqindata end=eof;
     by &byvar.;
     
     if %do i = 1 %to &nbyvar.-1; first.&&byvar&i. or %end; first.&&byvar&nbyvar. then qqrecnum = 0;
     
     qqrecnum = qqrecnum + 1;

    if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; then imlby + 1;
    if eof then call symput('nimlby',trim(left(put(imlby,6.0))));
     
    retain qqrecnum;
run;

proc sort data=qqindata(keep = &byvar. imlby) out=qqiml_byvars nodupkeys;
    by &byvar.;
run;

proc sql noprint;
     create table qqnum_records as
     select %do i =1 %to &nbyvar.; max(&&byvar&i..) as &&byvar&i.., %end;
            max(qqrecnum) as number_of_records
     from qqindata group by %do i = 1 %to &nbyvar.-1; &&byvar&i., %end; &&byvar&nbyvar.;
quit;

data qqindata;
      merge qqindata qqnum_records;
      by &byvar.;
run;

proc sql noprint;
      select max(number_of_records) into :max_num_rec
      from qqnum_records;
quit;

/**********************************************************************************************
**  Run the logistic regression analysis using the complete data set and capture the         **
**  estimated probabilities of the various responses for each patient.                       **
**********************************************************************************************/

ods output ParameterEstimates = qqoutest_logistic covB = qqcovB_logistic;
%if %UPCASE(&print_logistic.) = NO or %UPCASE(&print_logistic.) = N %then ods listing close;;

proc logistic data=qqindata NAMELEN=30;
     by &byvar. imlby;
%if %length(&weight.) %then %do;
     weight &weight.;     
%end;
     model qqresponse_num(reference="&nresponse.") = %do i = 1 %to &nvar_logistic.; &&var_logistic&i.. %end;
           / noint link=glogit covb;  
     output out=qqpredprobs predprobs=(i) xbeta = xbeta;
run;

%if &nresponse. > 2 %then %do;

data qqpredprobs;
     set qqpredprobs;
     by &byvar. qqrecnum;     

%do j = 1 %to &nresponsem1.;
    if _LEVEL_ = &j. then xbeta_&j. = xbeta;
%end;

    if last.qqrecnum then output;
    
    retain %do j = 1 %to &nresponsem1.; xbeta_&j. %end;;
    keep &byvar. imlby qqrecnum %do i = 1 %to &nvar_logistic.; &&var_logistic&i.. %end; 
         qqweight qqresponse_num ip_1-ip_&nresponse. xbeta_1-xbeta_&nresponsem1.;
run;

%end;

%else %do;

data qqpredprobs;
    set qqpredprobs(rename = (xbeta=xbeta_1));
    keep &byvar. imlby qqrecnum %do i = 1 %to &nvar_logistic.; &&var_logistic&i.. %end; 
         qqweight qqresponse_num ip_1 ip_2 xbeta_1;
run;

%end;

proc sort data=qqpredprobs;
    by &byvar. qqrecnum;
run;

ods output close;
%if %UPCASE(&print_logistic.) = NO or %UPCASE(&print_logistic.) = N %then ods listing;;

/**********************************************************************************************
**  Sort the variance-covariance matrix entries in order of response then covariate so they   *
**  will go into PROC IML correctly.                                                          *
**********************************************************************************************/

%let varxresp = %sysevalf(&nresponsem1.*&nvar_logistic., integer);
%let varxresp2 = %sysevalf(&varxresp.*&varxresp., integer);

data qqv_logistic;
      set qqcovB_logistic end=eof;
 
%let index = 0;
%do j = 1 %to &nresponsem1.;
%do i = 1 %to &nvar_logistic.;
%let index = %sysevalf(&index.+1, integer);
      if upcase(Parameter) = upcase("&&var_logistic&i.._&j.") then row = &index.;
%end;
%end;

run;

proc sort data=qqv_logistic;
     by &byvar. row;
run;

/**********************************************************************************************
**  Capture the variance-covariance matrix of the logistic regression in a single record      *
**  for use in the final variance calculation.                                                *
**********************************************************************************************/

data qqv_logistic_flat;
      array covariate(&varxresp.) %do j = 1 %to &nresponsem1.; %do i = 1 %to &nvar_logistic.; &&var_logistic&i.._&j. %end; %end;;
      array v_log(&varxresp.,&varxresp.) v_log1-v_log&varxresp2.;
 
      set qqcovB_logistic end=eof;
      by &byvar.;

/*  Initialize cov matrix to missing values */

      if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; 
        then do i = 1 to &varxresp.;
               do j = 1 to &varxresp.;
                  v_log(i,j) = .;
                  end;
               end;

%let index = 0;
%do j = 1 %to &nresponsem1.;
%do i = 1 %to &nvar_logistic.;
%let index = %sysevalf(&index.+1, integer);
      if upcase(Parameter) = upcase("&&var_logistic&i.._&j.") then row = &index.;
%end;
%end;

      if row ne . then do column = 1 to &varxresp.;
          v_log(row,column) = covariate(column);
          end;

      if last.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or last.&&byvar&i.. %end; then output; 

      retain v_log1-v_log&varxresp2.;   
      keep v_log1-v_log&varxresp2. &byvar.;
run;

/**********************************************************************************************
**  Set up variables to calculate the dfbetas for the logistic regression.                    *                                                                    *
**********************************************************************************************/

data qqfordfbeta_logistic;
    array dLL(&varxresp.) dLL1-dLL&varxresp.;
    
    set qqpredprobs;
    by &byvar.;
    
    one = 1;
    
%do j = 1 %to &nresponse.;

    if qqresponse_num = &j. then resp_ind_&j. = 1;
                            else resp_ind_&j. = 0;

%end;

    index = 0;
%do j = 1 %to &nresponsem1.;
%do i = 1 %to &nvar_logistic.;
    index = index + 1;
    dLL(index) = qqweight * (resp_ind_&j. - ip_&j.) * &&var_logistic&i.; 
%end;
%end;
    
    keep one &byvar. qqrecnum imlby qqweight
         ip_1-ip_&nresponse. xbeta_1-xbeta_&nresponsem1.
         resp_ind_1-resp_ind_&nresponse. dLL1-dLL&varxresp. qqresponse_num;
run;

proc sort data=qqfordfbeta_logistic;
    by &byvar. qqrecnum;
run;

/**********************************************************************************************
**  Calculate the IPTW weights using the propensity scores.  Use stabilization if specified.  *
**********************************************************************************************/

proc univariate data=qqfordfbeta_logistic noprint;
    by &byvar.;
%if %length(&weight.) %then %do;
    weight qqweight;
%end;
    var resp_ind_1-resp_ind_&nresponse.;
    output out=qqpi mean = %do i = 1 %to &nresponse.; pi_&i. %end;;
run;

data qqweights; 
    array ip(&nresponse.) ip_1-ip_&nresponse.;
    array pi(&nresponse.) pi_1-pi_&nresponse.;
 
    merge qqpredprobs qqpi;
    by &byvar.;

    prop_score_weight = 1 / ip(qqresponse_num); 
%if %UPCASE(&stabilize.) = YES or %UPCASE(&stabilize.) = Y %then %do;
    prop_score_weight = pi(qqresponse_num) * prop_score_weight;
%end;

run;    

/**********************************************************************************************
**  Apply truncation to the IPTW weights if specified.                                        *
**********************************************************************************************/

%if &truncate_pct. > 0 %then %do;

data qqtruncate_pct;
    truncate_pct = round(&truncate_pct.,0.1);
    truncate_pct_comp = 100 - truncate_pct;
    if round(truncate_pct_comp,1) = truncate_pct_comp then do;
        call symput('truncate_pct_comp',trim(left(put(truncate_pct_comp,5.0))));
        call symput('pct_label',trim(left(put(truncate_pct,5.0))));
        call symput('comp_label',trim(left(put(truncate_pct_comp,5.0))));
        end;
    else do;
        call symput('truncate_pct_comp',trim(left(put(truncate_pct_comp,7.1))));
        call symput('pct_label',trim(left(tranwrd(put(truncate_pct,7.1),'.','_'))));
        call symput('comp_label',trim(left(tranwrd(put(truncate_pct_comp,7.1),'.','_'))));
        end;
run;

proc univariate data=qqweights noprint;
   by &byvar.;
   var prop_score_weight;
   output out=qqquantile 
        pctlpts = &truncate_pct. &truncate_pct_comp. 
        pctlpre = pctl;
run;

data qqweights;
    merge qqweights qqquantile;
    by &byvar.;
    
    if prop_score_weight < pctl&pct_label.  then prop_score_weight = pctl&pct_label.; else
    if prop_score_weight > pctl&comp_label. then prop_score_weight = pctl&comp_label.;
    
    keep &byvar. qqrecnum prop_score_weight pi_1-pi_&nresponse.;
run;

%end;

/**********************************************************************************************
**  Calculate the product of the cohort sampling and propensity score weights.                *
**********************************************************************************************/

data qqindata;
    merge qqindata qqweights;
    by &byvar. qqrecnum;
    sampling_weight = qqweight;
    qqweight = qqweight  * prop_score_weight;
run;

/**********************************************************************************************
**  Calculate the standardized differences among logistic regression response outcomes       **
**  for each variable requested.                                                             **
**********************************************************************************************/

%if %length(&std_diff_vars.) %then %do;

/**************************/
/**  Weighted            **/
/**************************/

proc sql noprint;
    create table qqmean_var_weighted as
    select %do i = 1 %to &nbyvar.; max(&&byvar&i..) as &&byvar&i.., %end;
%do i = 1 %to &nstd_diff_var.; 
           sum(qqweight*&&std_diff_var&i..) / sum(qqweight) as mean_&i.,
           sum(qqweight*(&&std_diff_var&i..**2)) / sum(qqweight)
             - (sum(qqweight*&&std_diff_var&i..) / sum(qqweight))**2 as variance_&i.,
%end;
    max(qqresponse_num) as qqresponse_num, max(&response.) as &response.
    from qqindata 
    group by %do i = 1 %to &nbyvar.; &&byvar&i.., %end; qqresponse_num;
quit;

data qqstd_diff_weighted;

    length Covariate $ 32;
    
    merge 
         %do i = 1 %to &nresponse.; 
             qqmean_var_weighted(where = (qqresponse_num=&i.) 
                                   rename = (%do k = 1 %to &nstd_diff_var.; mean_&k. = mean_&k._&i. %end;
                                             %do k = 1 %to &nstd_diff_var.; variance_&k. = variance_&k._&i. %end;))
             %end;;             
    by &byvar.;
    
%do k = 1 %to &nstd_diff_var.;
    Covariate = "&&std_diff_var&k.";
%do i = 1 %to &nresponse.-1;
%do j = &i.+1 %to &nresponse.;
    std_diff_&i._&j. = 100 * (mean_&k._&i. - mean_&k._&j.) / sqrt((variance_&k._&i. + variance_&k._&j.) / 2);
        label std_diff_&i._&j. = Standardized difference (%) between &&response&i.. and &&response&j..;
%end;
%end;
    output;
%end;

    keep &byvar. Covariate 
         %do k = 1 %to &nstd_diff_var.; %do i = 1 %to &nresponse.-1; %do j = &i.+1 %to &nresponse.; std_diff_&i._&j. %end; %end; %end;;

run;

/**************************/
/**  Unweighted          **/
/**************************/

proc sql noprint;
    create table qqmean_var_unweighted as
    select %do i = 1 %to &nbyvar.; max(&&byvar&i..) as &&byvar&i.., %end;
%do i = 1 %to &nstd_diff_var.; 
           sum(&&std_diff_var&i..) / sum(intercept) as mean_&i.,
           sum((&&std_diff_var&i..**2)) / sum(intercept) 
              - (sum(&&std_diff_var&i..) / sum(intercept))**2 as variance_&i.,
%end;
    max(qqresponse_num) as qqresponse_num, max(&response.) as &response.
    from qqindata 
    group by %do i = 1 %to &nbyvar.; &&byvar&i.., %end; qqresponse_num;
quit;

data qqstd_diff_unweighted;

    length Covariate $ 32;
    
    merge 
         %do i = 1 %to &nresponse.; 
             qqmean_var_unweighted(where = (qqresponse_num=&i.) 
                                   rename = (%do k = 1 %to &nstd_diff_var.; mean_&k. = mean_&k._&i. %end;
                                             %do k = 1 %to &nstd_diff_var.; variance_&k. = variance_&k._&i. %end;))
             %end;;             
    by &byvar.;
    
%do k = 1 %to &nstd_diff_var.;
    Covariate = "&&std_diff_var&k.";
%do i = 1 %to &nresponse.-1;
%do j = &i.+1 %to &nresponse.;
    std_diff_unweighted_&i._&j. = 100 * (mean_&k._&i. - mean_&k._&j.) / sqrt((variance_&k._&i. + variance_&k._&j.) / 2);
      label std_diff_unweighted_&i._&j. = Unweighted standardized difference (%) between &&response&i.. and &&response&j..;
%end;
%end;
    output;
%end;

    keep &byvar. Covariate 
         %do k = 1 %to &nstd_diff_var.; %do i = 1 %to &nresponse.-1; %do j = &i.+1 %to &nresponse.; std_diff_unweighted_&i._&j. %end; %end; %end;;
    
run;

/**************************/
/**  Output data set.    **/
/**************************/

proc sort data=qqstd_diff_weighted;
    by &byvar. Covariate;
run;

proc sort data=qqstd_diff_unweighted;
    by &byvar. Covariate;
run;

data qqstd_diff;
    merge qqstd_diff_weighted qqstd_diff_unweighted;
    by &byvar. Covariate;
%if &byflag. = 0 %then %do;
    drop dummyby;
%end;
run;

%if %length(&std_diff_out.) %then %do;

data &std_diff_out.;
    set qqstd_diff;
run;

proc print label data=&std_diff_out.;
run;

%end;

/**************************/
/**  Produce graphs.     **/
/**************************/

%if %length(&std_diff_graph_name.) %then %do;

%let height = %sysevalf(&nstd_diff_var. * 0.5,integer);
%if &height. < 5 %then %let height = 5;

%do i = 1 %to &nresponse.-1;
%do j = &i.+1 %to &nresponse.;

data qqabs;
    set qqstd_diff;
    abs_std_diff_&i._&j. = abs(std_diff_&i._&j.);
    abs_std_diff_unweighted_&i._&j. = abs(std_diff_unweighted_&i._&j.);
run;

proc sql noprint;
    create table qqmaxabs as
    select max(abs_std_diff_&i._&j.) as weighted, max(abs_std_diff_unweighted_&i._&j.) as unweighted
    from qqabs;
quit;

data qqmaxabs;
    set qqmaxabs;
    
    scalemax = 10 * ( floor( max(weighted,unweighted) / 10) + 1 );
    scalemax = max(scalemax,100);
    call symput('scalemax',trim(left(put(scalemax,10.0))));
run;  

    title2 &&response&i.. versus &&response&j..;

ods graphics on / width=7in height=&height.in  imagename="&std_diff_graph_name._&i._&j." RESET=index ;
ods listing gpath="&graph_path.";

proc sgplot data=qqstd_diff;
%if &byflag. %then %do;
    by &byvar.;
%end;
    dot Covariate /response=std_diff_&i._&j. markerattrs=(symbol=CircleFilled color=red);
    dot Covariate /response=std_diff_unweighted_&i._&j. categoryorder=respdesc markerattrs=(symbol=Circle color=blue);
    refline 10  / axis=x lineattrs=(pattern=dot); 
    refline 0   / axis=x lineattrs=(pattern=dot); 
    refline -10 / axis=x lineattrs=(pattern=dot);
    label std_diff_&i._&j.="Weighted" std_diff_unweighted_&i._&j.="Unweighted";
    xaxis min=-&scalemax. max=&scalemax. values=(-&scalemax. to &scalemax. by 20) label="Standardized difference (%)";
run;

%end;
%end;
%end;

%end;

/**********************************************************************************************
**  Run the proportional hazards regression analysis.  Do not print the output as it uses     *
**  the variance estimates ignoring the variability of the weight estimates.  If requested,   *
**  output from the model using the correct variances will be printed later.                  *
**********************************************************************************************/

proc phreg data=qqindata covs outest=qqoutest covout noprint;
     by &byvar. imlby;
     model &time.*qqeventvar(0) = %do i = 1 %to &nvar.; &&var&i.. %end; 
                / covb ties=efron %if %length(&entrytime.) %then entrytime = &entrytime.;;
     weight qqweight;
%if %length(&strata.) %then %do;
     strata &strata.;
%end;
     &programming_statements.;
run;


/**********************************************************************************************
**  Capture the regression parameter estimates.                                              **
**********************************************************************************************/

data qqb;

      array b(&nvar.) b1-b&nvar.;
      array covariate(&nvar.) %do i = 1 %to &nvar.; &&var&i.. %end;; 

      set qqoutest;
      by &byvar. imlby;
      where upcase(_NAME_) = upcase("&time.");

/* Initialize regression parameter estimates to missing values */

      if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; 
        then do i = 1 to &nvar.;
               b(i) = .;
               end;

      do i = 1 to &nvar.;
        b(i) = covariate(i);
        end;  

      keep b1-b&nvar. &byvar. imlby;
run;

/*********************************************************************************************/
/*  Sort observations by time.  If events have same time as censored observations, put events*/
/*  first so that the censored observations remain in the risk set for that particular event */
/*********************************************************************************************/

proc sort data=qqindata;
     by &byvar. &strata. &time. descending qqeventvar;  
 run;
 
/*********************************************************************************************/
/*  Count the observations in each stratum and by group.                                    **/
/*********************************************************************************************/

proc univariate data=qqindata noprint;
     by &byvar. &strata.;
     var &time.;
     output out = qqnobs n = npt;
run;

/*********************************************************************************************/
/**  Handle the ties in event times using the Efron approach. For each k-way tie, this can  **/
/**  be accomplished by substituting k observations with weight equal to the average weight **/
/**  and risk score equal to the average risk score at the observation time.                **/
/*********************************************************************************************/

data qqindata_ties;
     merge qqindata qqb;
     by &byvar.;
     
     &programming_statements.;

     xbeta = 0;             
%do i = 1 %to &nvar.;
     xbeta = xbeta + b&i. * &&var&i..;
%end;
     exp_xbeta = exp(xbeta);
     
     drop b1-b&nvar.;
run;

proc univariate data=qqindata_ties noprint;
     by &byvar. &strata. &time. descending qqeventvar;
     where qqeventvar = 1;
     var qqweight;
     output out=qqtie_mean_wt mean = mean_weight;
run;

proc univariate data=qqindata_ties noprint;
     by &byvar. &strata. &time. descending qqeventvar;
     weight qqweight;
     where qqeventvar = 1;
     var exp_xbeta;
     output out=qqtie_mean_wt_exp_xbeta mean = mean_wt_exp_xbeta;
run;

data qqindata;
     merge qqindata qqtie_mean_wt qqtie_mean_wt_exp_xbeta;
     by &byvar. &strata. &time. descending qqeventvar;   
run;  

data qqtie_ind;
     set qqindata;
     
     lag_&time. = lag(&time.);
     lag_qqeventvar = lag(qqeventvar);
     
     if qqeventvar = 1 and lag_qqeventvar = 1 and &time. = lag_&time. then output;
     
     keep &byvar. &strata. &time. qqeventvar;
run;

data qqindata;
     merge qqindata qqtie_ind(in=a);
     by &byvar. &strata. &time. descending qqeventvar;        
     if a then tie_ind = 1;
          else tie_ind = 0;
run;

/*********************************************************************************************/
/** Produce a horizontal data set with the observation times, event indicator,              **/
/** sampling weight, covariate values needed to calculate the time-dependent covariates,    **/
/** weighted average risk scores (needed for handling ties), and the gradient of the weight **/
/** estimate with respect to the logistic regression parameter estimates in columns.  The   **/
/** horizontal data set has a record for each by-group and stratum.                         **/
/*********************************************************************************************/

proc sql noprint;
     select max(npt) into :maxnpt from qqnobs;
quit;

%let maxnpt = %cmpres(&maxnpt.);
%let dpc_calc_var = %sysevalf(&maxnpt.*&ncalc_var.,integer);
%let dpc_calc_var = %cmpres(&dpc_calc_var.);
%let dpc_var = %sysevalf(&maxnpt.*&nvar.,integer);
%let dpc_var = %cmpres(&dpc_var.);
%let dpc_varxresp = %sysevalf(&maxnpt.*&varxresp.,integer);
%let dpc_varsresp = %cmpres(&dpc_varsresp.);

proc sort data=qqindata;
     by &byvar. qqrecnum;
run;

data qqindata;
     merge qqindata qqfordfbeta_logistic(keep = &byvar. qqrecnum qqresponse_num
                         resp_ind_1-resp_ind_&nresponse. ip_1-ip_&nresponse. xbeta_1-xbeta_&nresponsem1.);
     by &byvar. qqrecnum;
     
%do j = 1 %to &nresponsem1.;
     exp_xbeta_&j. = exp(xbeta_&j.);
%end;
run;     

proc sort data=qqindata;
     by &byvar. &strata. &time. descending qqeventvar;  
run;

data qqhoriz;
 
     array exp_xbeta_logistic(&nresponsem1.) exp_xbeta_1-exp_xbeta_&nresponsem1.;
     array ip(&nresponse.) ip_1-ip_&nresponse.;
     array pi(&nresponse.) pi_1-pi_&nresponse.;
     array resp_ind(&nresponse.) resp_ind_1-resp_ind_&nresponse.;     
     array pttime(&maxnpt.) pttime1-pttime&maxnpt.;
     array ptevent(&maxnpt.) ptevent1-ptevent&maxnpt.;
     array ptweight(&maxnpt.) ptweight1-ptweight&maxnpt.;
     array ptgrad_weight(&varxresp.,&maxnpt.) ptgrad_weight1-ptgrad_weight&dpc_varxresp.;
     array ptcovariate(&ncalc_var.,&maxnpt.) ptcovariate1-ptcovariate&dpc_calc_var.;
     array pt_tie_wt_exp_xbeta(&maxnpt.) pt_tie_wt_exp_xbeta1-pt_tie_wt_exp_xbeta&maxnpt.; 
     array pt_tie_wt(&maxnpt.) pt_tie_wt1-pt_tie_wt&maxnpt.; 
     array pt_tie_ind(&maxnpt.) pt_tie_ind1-pt_tie_ind&maxnpt.; 
%if %length(&entrytime.) %then %do;
     array ptentrytime(&maxnpt.) ptentrytime1-ptentrytime&maxnpt.;
%end;
     array ptrecnum(&maxnpt.) ptrecnum1-ptrecnum&maxnpt.;


     merge qqindata qqnobs;
     by &byvar. &strata.; 
   
     if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; 
                            %if %length(&strata.) %then %do i = 1 %to &nstrata.; or first.&&strat&i.. %end;
        then  do i = 1 to &maxnpt.;
            pttime(i) = .;
            ptevent(i) = .;
            ptweight(i) = .;
%do i = 1 %to &varxresp.;
            ptgrad_weight(&i.,i) = .;
%end;
%do i = 1 %to &ncalc_var.;
            ptcovariate(&i.,i) = .;
%end;
            pt_tie_wt_exp_xbeta(i) = .;
            pt_tie_wt(i) = .;
            index = 0;
            end;
        
     index = index + 1;
     ptrecnum(index) = qqrecnum;
     pttime(index) = &time.;
     ptevent(index) = qqeventvar;
     ptweight(index) = qqweight;

%let L = 0;
%do j = 1 %to &nresponsem1.;
%do i = 1 %to &nvar_logistic.;
%let L = %sysevalf(&L.+1,integer);
     if qqresponse_num < &nresponse. then
         ptgrad_weight(&L.,index) = sampling_weight * pi(qqresponse_num) * &&var_logistic&i.. 
                * (  exp_xbeta_logistic(&j.) / exp_xbeta_logistic(qqresponse_num) 
                    - resp_ind_&j. / ip(qqresponse_num) );
     else ptgrad_weight(&L.,index) = sampling_weight * pi(qqresponse_num) * &&var_logistic&i.. 
                * exp_xbeta_logistic(&j.);
%end;
%end;

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

     retain index pttime1-pttime&maxnpt. ptevent1-ptevent&maxnpt. ptweight1-ptweight&maxnpt. ptgrad_weight1-ptgrad_weight&dpc_varxresp.  
            ptcovariate1-ptcovariate&dpc_calc_var. pt_tie_wt_exp_xbeta1-pt_tie_wt_exp_xbeta&maxnpt.
            pt_tie_wt1-pt_tie_wt&maxnpt. %if %length(&entrytime.) %then ptentrytime1-ptentrytime&maxnpt.;;
            
     keep  &byvar. &strata. pttime1-pttime&maxnpt.  ptevent1-ptevent&maxnpt. ptweight1-ptweight&maxnpt. ptgrad_weight1-ptgrad_weight&dpc_varxresp.
           ptcovariate1-ptcovariate&dpc_calc_var. pt_tie_wt_exp_xbeta1-pt_tie_wt_exp_xbeta&maxnpt.
           pt_tie_wt1-pt_tie_wt&maxnpt. pt_tie_ind1-pt_tie_ind&maxnpt.
           %if %length(&entrytime.) %then ptentrytime1-ptentrytime&maxnpt.;
           ptrecnum1-ptrecnum&maxnpt.;
run; 

/*********************************************************************************************/
/**  Calculate the numerator and denominator terms for the baseline cumulative hazard       **/
/**  calculation, the weighted mean covariate values in each risk set and the gradient of   **/
/**  the jumps in the baseline cumulative hazard estimate with respect to the logistic      **/
/**  regression parameter estimates.                                                        **/
/*********************************************************************************************/

**  Create a version of the regression parameter data set that has a record for each stratum
**  within each by group;

proc sort data=qqindata out=qqbystrat nodupkeys;
     by &byvar. &strata.;
run;

data qqb_by_strat;
     merge qqb qqbystrat;
     by &byvar.;
run;

data qqhoriz;

     array pttime(&maxnpt.) pttime1-pttime&maxnpt.;
     array ptevent(&maxnpt.) ptevent1-ptevent&maxnpt.;
     array ptweight(&maxnpt.) ptweight1-ptweight&maxnpt.;
     array ptgrad_weight(&varxresp.,&maxnpt.) ptgrad_weight1-ptgrad_weight&dpc_varxresp.;
     array ptdLda(&varxresp.,&maxnpt.) ptdLda1-ptdLda&dpc_varxresp.;
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
     array ptscore_resid(&nvar.,&maxnpt.) ptscore_resid1-ptscore_resid&dpc_var.;
     array ptrecnum(&maxnpt.) ptrecnum1-ptrecnum&maxnpt.;

     merge qqhoriz  qqb_by_strat qqnobs;
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


/************************************************************************************************
**  Compute the gradient of the increment in the cumulative hazard estimate with respect to    **
**  the logistic regression parameter estimates.                                               **
************************************************************************************************/

%let L = 0;
%do j = 1 %to &nresponsem1.;
%do i = 1 %to &nvar_logistic.;
%let L = %sysevalf(&L.+1,integer);
         sum = 0;
         do j = 1 to npt;

             if pt_tie_ind(j) = 1 then do;
                exp_xbeta_at_j = pt_tie_wt_exp_xbeta(j);
                end;
             else do;
%do k = 1 %to &ncalc_var.;
                &&calc_var&k.. = ptcovariate(&k.,j);
%end;
                &programming_statements.;
                xbeta_at_j = 0;             
%do k = 1 %to &nvar.;
                xbeta_at_j = xbeta_at_j + b&k. * &&var&k..;
%end;
                exp_xbeta_at_j = exp(xbeta_at_j);
                end;

             sum = sum + ptgrad_weight(&L.,j) * (pttime(j) >= &time. 
                        %if %length(&entrytime.) %then and &time. >= ptentrytime(j);) * exp_xbeta_at_j; 

              end;
         ptdLda(&L.,index) = ptgrad_weight(&L.,index) * ptevent(index) / ptdenom(index)
                                   - ptevent(index) * ptnumer(index) * sum / (ptdenom(index)**2); 
%end;
%end;

         end;        
         
/************************************************************************************************
**  Compute the score residuals.  Include the weight with the score residual.                  **
************************************************************************************************/

    do index = 1 to npt;
%do i = 1 %to &nvar.;     
      ptscore_resid(&i.,index) = 0;
%end;
      
      do j = 1 to index;   
         &time. = pttime(j);
         if pt_tie_ind(j) = 1 then do;
            exp_xbeta = pt_tie_wt_exp_xbeta(j);
            qqweight = pt_tie_wt(j);
            end;
             
         else do;
%do i = 1 %to &ncalc_var.;
         &&calc_var&i.. = ptcovariate(&i.,j);
%end;
         &programming_statements.;

         xbeta_at_j = 0;             
%do i = 1 %to &nvar.;
         xbeta_at_j = xbeta_at_j + b&i. * &&var&i..;
%end;
            end;
         
         dM = (j=index) * ptevent(index) - (pttime(j) <= pttime(index)) * exp(xbeta_at_j) * ptevent(j) * ptnumer(j) / ptdenom(j); 
        
%do i = 1 %to &nvar.;
         ptscore_resid(&i.,index) = ptscore_resid(&i.,index) + ptweight(index) * (&&var&i.. - ptcovmean(&i.,j)) * dM; 
%end;
         end;
         
      end;

     keep &byvar. imlby &strata. npt pttime1-pttime&maxnpt. ptevent1-ptevent&maxnpt. ptweight1-ptweight&maxnpt.
          ptcovariate1-ptcovariate&dpc_calc_var. ptnumer1-ptnumer&maxnpt. ptdenom1-ptdenom&maxnpt. 
          ptcovmean1-ptcovmean&dpc_var. ptscore_resid1-ptscore_resid&dpc_var. ptdLda1-ptdLda&dpc_varxresp.
          %if %length(&entrytime.) %then ptentrytime1-ptentrytime&maxnpt.;
          ptrecnum1-ptrecnum&maxnpt.; 
run;

/**********************************************************************************************
**  Set up the score residuals to be read into proc iml.                                      *
**********************************************************************************************/

data qqscore_resid_phreg;
     array ptscore_resid(&nvar.,&maxnpt.) ptscore_resid1-ptscore_resid&dpc_var.;
     array ptrecnum(&maxnpt.) ptrecnum1-ptrecnum&maxnpt.;
     
     set qqhoriz;
     
     do index = 1 to npt;
         qqrecnum = ptrecnum(index);
%do i = 1 %to &nvar.;
         sr_&&var&i.. = ptscore_resid(&i.,index);
%end;
         output;
         end;
         
     keep imlby qqrecnum %do i = 1 %to &nvar.; sr_&&var&i.. %end;;
 run;
 
 proc sort data=qqscore_resid_phreg;
     by imlby qqrecnum;
 run;
 
 /**********************************************************************************************
 **  Get the inverse information matrix for the regression parameters, which is equal to the   *
 **  model-based covariance matrix estimate (not using the sandwich estimate).                 *
 **********************************************************************************************/
 
 proc phreg data=qqindata outest=qqoutest_I covout noprint;
      by imlby;
      model &time.*qqeventvar(0) = %do i = 1 %to &nvar.; &&var&i.. %end; 
                 / covb ties=efron %if %length(&entrytime.) %then entrytime = &entrytime.;;
      weight qqweight;
 %if %length(&strata.) %then %do;
      strata &strata.;
 %end;
      &programming_statements.;
run;

data qqinv_i;
      array covariate(&nvar.) %do i = 1 %to &nvar.; &&var&i.. %end;; 
      array v(&nvar.,&nvar.) v1-v&nvar2.;
 
      set qqoutest_I;
      by imlby;
      where upcase(_NAME_) ne upcase("&time.");
      keep imlby %do i = 1 %to &nvar.; &&var&i.. %end;;
run;  

/**********************************************************************************************
**  Calculate the variance-covariance matrix of the parameter estimates, accounting for the   *
**  variability in the weight estimates, using the dfbetas.  Also calculate statistics        *
**  for reporting the model fit using the calculated variance-covariance matrix.              *
**********************************************************************************************/

data qqrowlabel;

length _row_ $ 32;

%do i = 1 %to &nvar.;
    _row_ = "&&var&i..";
    output;
%end;

run;

%do imlby = 1 %to &nimlby.;

proc sql noprint;
     select sum(one) into :N 
     from qqfordfbeta_logistic where imlby=&imlby.;
quit;

%let N = %cmpres(&N.);

proc iml;
      use qqfordfbeta_logistic where(imlby=&imlby.);
      read all var {%do i = 1 %to &varxresp.; dLL&i. %end;} into DLL;
      
      use qqv_logistic where(imlby=&imlby.);
      read all var {%do j = 1 %to &nresponsem1.; %do i = 1 %to &nvar_logistic.; &&var_logistic&i.._&j. %end; %end;} into V_LOGISTIC;
      
      use qqscore_resid_phreg where(imlby=&imlby.);
      read all var {%do i = 1 %to &nvar.; sr_&&var&i.. %end;} into SCORE_RESID;
      
      use qqinv_i where(imlby=&imlby.);
      read all var {%do i = 1 %to &nvar.; &&var&i.. %end;} into INV_I;
      
      use qqb where(imlby=&imlby.);
      read all var {%do i = 1 %to &nvar.; b&i. %end;} into BETA_PHREG_T;  
          
      DFBETA_LOGISTIC = DLL * V_LOGISTIC;
      
      DFBETA_PHREG = SCORE_RESID * INV_I;
  
      V_PHREG = DFBETA_PHREG` 
                 * (I(&N.) - DFBETA_LOGISTIC * INV( DFBETA_LOGISTIC` * DFBETA_LOGISTIC ) * DFBETA_LOGISTIC`)
                 * DFBETA_PHREG;
                 
      GLOBAL_WALD_CHISQ = BETA_PHREG_T * GINV(V_PHREG) * BETA_PHREG_T`;
      
      VARIANCE_BETA_PHREG_MATRIX = DIAG(V_PHREG);
      SE_BETA_PHREG = t(SQRT(VECDIAG(VARIANCE_BETA_PHREG_MATRIX)));

      column_name = {%do i = 1 %to &nvar.; "&&var&i.." %end;};

      create qqv_phreg from V_PHREG [colname = column_name];
      append from V_PHREG;
      
      create qqglobal_wald_chisq from GLOBAL_WALD_CHISQ  [colname = 'Global_Wald_Chisq'];
      append from GLOBAL_WALD_CHISQ;

      column_name_se = {%do i = 1 %to &nvar.; "se_b&i." %end;};
      
      create qqse_beta_phreg from SE_BETA_PHREG [colname = column_name_se];
      append from SE_BETA_PHREG;
         
run;

data qqv_phreg;
      merge qqv_phreg qqrowlabel;
      imlby = &imlby.;
run;

data qqtest_statistics_phreg;
     length Covariate $ 32;
     length pvalue $ 7;
     
     merge qqglobal_wald_chisq qqb(where=(imlby=&imlby.)) qqse_beta_phreg;
     imlby = &imlby.;
     
     Covariate = 'Global';
     Chisquare = Global_Wald_Chisq;     
     df = &nvar.;
     p = 1 - CDF('CHISQUARE',Global_Wald_Chisq,&nvar.);
     if p > 0.00005 then pvalue = put(round(p,0.0001),7.4);
     else pvalue = '<0.0001';
     output;
     
%do i = 1 %to &nvar.;

     Covariate = "&&var&i..";
     Parameter = b&i.;
     Standard_error = se_b&i.;
     Chisquare = (b&i. / se_b&i.)**2;
     df = 1;
     p = min(1, 2 * (1 - probnorm(abs(b&i.) / se_b&i.)));
     if p > 0.00005 then pvalue = put(round(p,0.0001),6.4);
     else pvalue = '<0.0001';
     Hazard_ratio = exp(b&i.);
     Hazard_ratio_LCL = exp(b&i. - probit(1-&alpha./2) * se_b&i.);
     Hazard_ratio_UCL = exp(b&i. + probit(1-&alpha./2) * se_b&i.);
     output;

%end;

     keep imlby Covariate Parameter Standard_error Chisquare df pvalue Hazard_ratio Hazard_ratio_LCL Hazard_ratio_UCL;
run;

 
%if &imlby. = 1 %then %do;

data qqv;
     set qqv_phreg;
run;

data qqtest_statistics;
     set qqtest_statistics_phreg;
run;

%end;

%else %do;

proc append data=qqv_phreg base=qqv;
run;

proc append data=qqtest_statistics_phreg base=qqtest_statistics;
run;

%end;

%end;

data qqtest_statistics;
    merge qqtest_statistics qqiml_byvars;
    by imlby;
run;

/**********************************************************************************************
/**  Print the parameter estimates, hazard ratios and tests from the Cox regression,          *
/**  accounting for the variance in the estimated weights.                                    *
**********************************************************************************************/

proc sql noprint;
    create table qqforprint as
    select %if &byflag. %then %do i = 1 %to &nbyvar.; &&byvar&i.., %end;
           Covariate, Parameter, Standard_error, Chisquare, df, pvalue, Hazard_ratio, Hazard_ratio_LCL, Hazard_ratio_UCL
    from qqtest_statistics;
run;

%let conf = %sysevalf(100*(1-&alpha.),integer); 

data qqforprint;
    set qqforprint;
    label Covariate = Covariate;
    label Parameter = Parameter estimate;
    label Standard_error = Standard error of parameter estimate;
    label Chisquare = Chi-square;
    label df = Degrees of freedom;
    label pvalue = P-value;
    label Hazard_ratio = Hazard ratio estimate;
    label Hazard_ratio_LCL = Lower limit of &conf.% confidence interval for hazard ratio;
    label Hazard_ratio_UCL = Upper limit of &conf.% confidence interval for hazard ratio;
run;

%if %UPCASE(&print_phreg.) = YES or %UPCASE(&print_phreg.) = Y %then %do;

proc print label data=qqforprint;
%if &byflag. %then %do;
    by &byvar.;
%end;
run;

%end;

/**********************************************************************************************
/**  Recapture the by-variables and rename the covariance matrix into a vector.  Save the     *
/**  covariance matrix for output if requested.                                               *
**********************************************************************************************/

data qqv(keep = &byvar. v1-v&nvar2.)
     qqvoutput(keep = &byvar. _row_ %do i = 1 %to &nvar.; &&var&i.. %end;);
    merge qqv qqiml_byvars;
    by imlby;
    
    output qqvoutput;
    
    array covariate(&nvar.) %do i = 1 %to &nvar.; &&var&i.. %end;; 
    array v(&nvar.,&nvar.) v1-v&nvar2.;
 
      set qqv;
      
      if first.imlby then do;
           row = 0;
           do i = 1 to &nvar.;
               do j = 1 to &nvar.;
                  v(i,j) = .;
                  end;
               end;
            end;

      row = row + 1;
      
      do column = 1 to &nvar.;
          v(row,column) = covariate(column);
          end;

    if last.imlby then output qqv; 

    retain v1-v&nvar2. row;   
run;


/***********************************************************************************************
**  Combine regression parameter estimates and covariance matrix with the horizontal file.    **
***********************************************************************************************/

data qqkeys;
     merge qqv qqb_by_strat qqv_logistic_flat;
     by &byvar.;
     keep &byvar. &strata. b1-b&nvar. v1-v&nvar2. v_log1-v_log&varxresp2.;           
run;

data qqhoriz;
     merge qqhoriz qqkeys;
     by &byvar. &strata.;
run;

/***********************************************************************************************/
/**  Get the data set of covariate combinations for which the risk will be estimated.         **/
/***********************************************************************************************/

%if %length(&covariate_dsn.) %then %do;

data qqcovnoby;
     set &covariate_dsn.;

/* Delete any observation that has missing covariate values */

%do i = 1 %to &ncalc_var.;
     if &&calc_var&i.. = . then delete;
%end;

%if %length(&strata.) %then %do i = 1 %to &nstrata.;
     if &&strat&i.. = . then delete;
%end;

     dummyby = 1;
     unique_record_id + 1;  /*  Unique record id to allow output data set to be in same sort
                                order as input data set */
run;

%if &byflag. = 1 %then %do;
proc sort data=qqindata(keep=&byvar.) out=qqbyvalues nodupkeys;
     by &byvar.;
run;

proc sql noprint;
     create table qqcov as select * from qqcovnoby, qqbyvalues;
quit;

%end;

%else %do;

data qqcov;
     set qqcovnoby;
run;

%end;

%end;

%else %do;

data qqcov;
     set qqindata(keep = &byvar. &calc_vars. &strata.);

%do i = 1 %to &ncalc_var.;
     if &&calc_var&i.. = . then delete;
%end;

%if %length(&strata.) %then %do i = 1 %to &nstrata.;
     if strat&i. = . then delete;
%end;

     unique_record_id + 1;  /*  Unique record id to allow output data set to be in same sort
                                order as input data set */
     run;
     
%end;

proc sort data=qqcov;
     by &byvar. &strata.;
run;

/***********************************************************************************************/
/**  Estimate the risk at the specified time for each combination of covariates in the        **/
/**  covariate data set, with a confidence interval.  Do this one covariate data set record   **/
/**  at a time to avoid running out of memory by replicating the horizontal data set.         **/
/***********************************************************************************************/

proc sql noprint;
     select max(unique_record_id) into :num_rec_cov from qqcov;
quit;

%if %length(&strata.) %then %do i = 1 %to &nstrata.; 
%local val_&&strat&i..;
%end;

%do i = 1 %to &nbyvar.;
%local val_&&byvar&i..;
%end;


 /**********************************************************************************************/
 /**  Loop through the covariate data set records.                                            **/
 /**********************************************************************************************/

%do unique_record_id = 1 %to &num_rec_cov.;

/**********************************************************************************************/
/**  Calculate the risk estimates for this particular set of covariate values.               **/
/**********************************************************************************************/

proc sql noprint;

%do i = 1 %to &nbyvar.; 
      select max(&&byvar&i..) into :val_&&byvar&i.. from qqcov where unique_record_id = &unique_record_id.; 
%end;

%if %length(&strata.) %then %do i = 1 %to &nstrata.; 
      select max(&&strat&i..) into :val_&&strat&i.. from qqcov where unique_record_id = &unique_record_id.;
%end;

quit;

data qqriskest_uniq_rec_id;
     array v(&nvar.,&nvar.) v1-v&nvar2.;
     array v_log(&varxresp.,&varxresp.) v_log1-v_log&varxresp2.;
     array pttime(&maxnpt.) pttime1-pttime&maxnpt.;
     array ptevent(&maxnpt.) ptevent1-ptevent&maxnpt.;
     array ptweight(&maxnpt.) ptweight1-ptweight&maxnpt.;
     array ptcovariate(&ncalc_var.,&maxnpt.) ptcovariate1-ptcovariate&dpc_calc_var.;
     array ptnumer(&maxnpt.) ptnumer1-ptnumer&maxnpt.;
     array ptdenom(&maxnpt.) ptdenom1-ptdenom&maxnpt.;
     array ptcovmean(&nvar.,&maxnpt.) ptcovmean1-ptcovmean&dpc_var.;
     array ptdLda(&varxresp.,&maxnpt.) ptdLda1-ptdLda&dpc_varxresp.;
     array dLzda(&varxresp.) dLzda1-dLzda&varxresp.;
     array varsb(&nvar.) %do i = 1 %to &nvar.; &&var&i.. %end;;
     array q_vector(&nvar.) q_vector1-q_vector&nvar.;
     array productb(&nvar.) productb1-productb&nvar.;
     array producta(&varxresp.) producta1-producta&varxresp.;
     
     merge qqhoriz(where = (&byvar1. = &&val_&byvar1.. %do i = 2 %to &nbyvar.; and &&byvar&i.. = &&val_&&byvar&i... %end;
                              %if %length(&strata.) %then %do i = 1 %to &nstrata.; and &&strat&i.. =  &&val_&&strat&i... %end;)) 
           qqnobs (where = (&byvar1. = &&val_&byvar1.. %do i = 2 %to &nbyvar.; and &&byvar&i.. = &&val_&&byvar&i... %end;
                              %if %length(&strata.) %then %do i = 1 %to &nstrata.; and &&strat&i.. =  &&val_&&strat&i... %end;))
           qqcov  (where = (unique_record_id = &unique_record_id.));
   
 /**********************************************************************************************/
 /**  Compute cumulative hazard estimate for the combination of covariates.                   **/
 /**********************************************************************************************/

     do index = 1 to npt; 
         if ptevent(index) = 1 and pttime(index) <= &risk_time. then do; 
             &time. = pttime(index);  ** Compute the time-dependent xbeta for this patient;
             &programming_statements.;
             xbeta = 0; 
             xbeta = xbeta %do i = 1 %to &nvar.; + b&i. * &&var&i.. %end;;
             exp_xbeta = exp(xbeta);             
 
             qqcumhaz = qqcumhaz + exp_xbeta * ptnumer(index) / ptdenom(index);            
             varcumhaz = varcumhaz + (exp_xbeta * ptnumer(index) / ptdenom(index))**2;  
              
             do i = 1 to &nvar.;    
                  q_vector(i) = q_vector(i) + exp_xbeta * (varsb(i) - ptcovmean(i,index)) * ptnumer(index) / ptdenom(index); 
                  end;
 
             do L = 1 to &varxresp.;   
                  dLzda(L) = dLzda(L) + exp_xbeta * ptdLda(L,index); 
                  end;                  

             end;
         end;
      

 /**********************************************************************************************/                   
 /**  Compute variance of cumulative hazard estimate.                                         **/
 /**********************************************************************************************/
 
 /************************************************************/
 /**  Variation due to Cox regression parameter estimates.  **/
 /************************************************************/

     do i = 1 to &nvar.;                 
          productb(i) = 0;
          do j= 1 to &nvar.;
               productb(i) = productb(i) + v(i,j) * q_vector(j);
               end;
          end;

     quadform = 0;     
     do i = 1 to &nvar.;
          quadform = quadform + q_vector(i) * productb(i);
          end;



 /************************************************************/
 /**  Variation due to logistic regression parameter        **/
 /**  estimates.                                            **/
 /************************************************************/

     do i = 1 to &varxresp.;                 
           producta(i) = 0;
           do j = 1 to &varxresp.;
                producta(i) = producta(i) + v_log(i,j) * dLzda(j);
                end;
           end;
 
     quadform_logistic = 0;
     do i = 1 to &varxresp.;
          quadform_logistic = quadform_logistic + dLzda(i) * producta(i);
          end;

 /************************************************************/
 /**  Add the variance due to jumps in baseline cumulative  **/
 /**  hazard function.                                      **/
 /************************************************************/

     varhatch = varcumhaz + quadform + quadform_logistic;  
    
 /**  Standard error of log cumulative hazard estimate **/
  
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


     retain qqcumhaz varcumhaz q_vector1-q_vector&nvar. dLzda1-dLzda&dpc_varxresp. 0;    ***  This statement initializes the variable to 0 without taking up a lot of space on the SAS log.;

     keep unique_record_id &byvar. &risk. &risk_LCL. &risk_UCL. 
                           &CumHaz. &CumHaz_LCL. &CumHaz_UCL.
                           &LogCumHaz. &SE_LogCumHaz.;
run;

%if &unique_record_id. = 1 %then %do;

data qqriskest;
     set qqriskest_uniq_rec_id;
run;

%end;

%else %do;

proc append data=qqriskest_uniq_rec_id base = qqriskest;
run;

%end;

%end;  /*  End of unique_record_id loop */

/***********************************************************************************************/
/**  Create output data set for the risk estimates.                                           **/
/***********************************************************************************************/

proc sort data=qqriskest;
     by &byvar. unique_record_id;
run;

proc sort data=qqcov;
     by &byvar. unique_record_id;
run;


data qqoutdsn;
     merge qqcov qqriskest;
     by &byvar. unique_record_id;
     
     drop unique_record_id %if &byflag.=0 %then dummyby;;
run;

proc sql noprint;
     create table &outdsn. as
     select %if &byflag. = 1 %then %do i = 1 %to &nbyvar.; &&byvar&i.., %end;
     %do i = 1 %to &ncalc_var.; &&calc_var&i.., %end;
     &risk., &risk_LCL., &risk_UCL., 
     &CumHaz., &CumHaz_LCL., &CumHaz_UCL., &LogCumHaz., &SE_LogCumHaz.
     from qqoutdsn;
quit;    

/***********************************************************************************************/
/**  Create output data set for Cox regression parameter estimates.                           **/
/***********************************************************************************************/

%if %length(&parameter_estout.) %then %do;

data &parameter_estout.;
    set qqforprint;
run;

%end;

/***********************************************************************************************/
/**  Create output data set for Cox regression parameter estimate covariance matrix.          **/
/***********************************************************************************************/

%if %length(&parameter_covout.) %then %do;

proc sql noprint;
    create table &parameter_covout. as
    select %if &byflag. %then %do i = 1 %to &nbyvar.; &&byvar&i.., %end;
           _row_, %do i = 1 %to &nvar.-1; &&var&i.., %end; &&var&nvar.
    from qqvoutput;
quit;

%end;

/**********************************************************************************************
** Reset mergenoby option to what is was.                                                     *
**********************************************************************************************/

options mergenoby=&mergenobyoption.;

%mend Risk_Est_PH_reg_prop_score;
