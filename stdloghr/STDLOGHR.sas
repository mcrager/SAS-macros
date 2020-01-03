/***********************************************************************************************************************

Program Name           : STDLOGHR.sas

Path                   : 

Program Language       : SAS

Operating System       : Windows Server

Purpose                : Compute the standardized absolute log hazard ratio for a multivariate Cox proportional hazards 
                         regression model,and associated variability estimates, test statistics and confidence intervals.                           
                         
Notes                  : 

Status                 : Tested and verified

Run Dependencies       : None

Input Datasets/Views   : Specified SAS data set indsn containing a time variable, indicator for censoring, 
                         covariates for the Cox model, and an optional weight variable if cohort sampling was used.

Output Datasets/Views  : Specified SAS data set outdsn containing the standardized absolute log hazard ratio, its standard error,
                         confidence intervals, hypothesis test results, and variable importance statistics.                         

Other Output           : None

Macro calls internal   : TempLibL

Macro calls external   : 

***********************************************************************************************************************/

%macro STDLOGHR(
        /* Input Specification */  indsn=,byvar=,vars=,time=,censor=,censorlist=0,weight=,adjcov=,strata=,
                                   var_combo_indsn=,combo_id=,
        /* Analysis Parameters */  robust=no,partial=,print=,alpha=,intvl_hyp=,
        /* Output Specification */ outdsn=,abs_std_log_HR=abs_std_log_HR,abs_std_HR=abs_std_HR,var_std_log_HR=var_std_log_HR,
                                   abs_std_log_HR_correct=abs_std_log_HR_correct,abs_std_HR_correct=abs_std_HR_correct,                                         
                                   chi_sq=chi_sq,df=df,p_value=p_value,p_value_int=p_value_int,p_value_int_Scheffe=p_value_int_Scheffe,
                                   min_eigenvalue=min_eigenvalue,noncentrality_Scheffe=noncentrality_Scheffe,
                                   abs_std_log_HR_LCL=abs_std_log_HR_LCL,abs_std_log_HR_UCL=abs_std_log_HR_UCL,
                                   abs_std_log_HR_LCL_Scheffe=abs_std_log_HR_LCL_Scheffe,abs_std_log_HR_UCL_Scheffe=abs_std_log_HR_UCL_Scheffe,
                                   abs_std_HR_LCL=abs_std_HR_LCL,abs_std_HR_UCL=abs_std_HR_UCL,
                                   abs_std_HR_LCL_Scheffe=abs_std_HR_LCL_Scheffe,abs_std_HR_UCL_Scheffe=abs_std_HR_UCL_Scheffe,
                                   abs_std_HR_correct_LCL=abs_std_HR_correct_LCL,
                                   abs_std_HR_correct_UCL=abs_std_HR_correct_UCL,
                                   abs_std_HR_correct_LCL_Scheffe=abs_std_HR_correct_LCL_Scheffe,
                                   abs_std_HR_correct_UCL_Scheffe=abs_std_HR_correct_UCL_Scheffe,
                                   zb_prefix=,zv_prefix=,contribution_prefix=,predictors=,maxVIF=maxVIF,Prop_Var_Expl=Prop_Var_Expl,
                                   var_combo_outdsn=
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
| Purpose        : List of input data set variables containing the Cox model covariates for which the 
|                  standardized absolute log hazard ratio will be computed.
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
| Name           : adjcov  
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : Text string giving additional proportional hazard covariates to be included in the 
|                  model but not included in the standardized hazard ratio computation.
|-----------------------------------------------------------------------------------------------
| Name           : strata   
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : Text string giving stratification variables for the proportional hazards model.                      
|-----------------------------------------------------------------------------------------------
| Name           : var_combo_indsn   
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : Optional input data set containing indicator variables for summing variable contributions
|                  to the risk score variance.  For each record in this input data set, the macro will compute the sum of the 
|                  contributions of the indicated variables to the risk score variance and the standard error of the sum.  The
|                  indicator variables must have names ind_<var_name> where <var_name> is the name of the input data set
|                  variable included in the model.  The parameter contribution_prefix must be specified to use this option. 
|-----------------------------------------------------------------------------------------------
| Name           : combo_id   
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $/#
| Purpose        : Optional variable combination identifier variable.  If specified, this variable must be included
|                  in the file var_combo_inds.
|-----------------------------------------------------------------------------------------------
| Name           : robust   
| Required (Y/N) : N
| Default Value  : no
| Type ($/#)     : $
| Purpose        : If this parameter is set to yes and weight is missing, the Lin-Wei covariance sandwich
|                  estimate of the covariance matrix of the regression parameter estimates will be used
|                  in the analysis.  If weight is present, this parameter has no effect.             
|-----------------------------------------------------------------------------------------------
| Name           : partial   
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : If this parameter is set to a list of variables that is a subset of the set of adjustment covariates
|                  and stratification variables, the partial standardized log hazard ratio will be calculated
|                  conditional on the specified stratification variables and adjustment covariates.  If the parameter is not
|                  specified,the marginal standardized log hazard ratio will be calculated.             
|-----------------------------------------------------------------------------------------------
| Name           : print   
| Required (Y/N) : N
| Default Value  : yes
| Type ($/#)     : $
| Purpose        : If this parameter is set to no, the noprint option will be invoked for PROC PHREG.                      
|-----------------------------------------------------------------------------------------------
| Name           : alpha   
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : #
| Purpose        : If this parameter is set, the macro will compute 100(1-alpha)% confidence intervals
|                  for the absolute standardized log hazard ratio.                      
|-----------------------------------------------------------------------------------------------
| Name           : intvl_hyp   
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : #
| Purpose        : If this parameter is set, the macro will compute p-values for an interval null hypothesis
|                  that the absolute standardized hazard ratio is less than &intvl_hyp.                      
|-----------------------------------------------------------------------------------------------
| Name           : outdsn  
| Required (Y/N) : Y
| Default Value  : (at temporary library)
| Type ($/#)     : $
| Purpose        : Libname reference and the output data set name.  The dataset 
|                  name must conform to the rules for SAS names.   
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_log_HR  
| Required (Y/N) : N
| Default Value  : abs_std_log_HR
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the estimate of the absolute standardized 
|                  log hazard ratio.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_HR  
| Required (Y/N) : N
| Default Value  : abs_std_HR
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the estimate of the absolute standardized 
|                  hazard ratio.
|-----------------------------------------------------------------------------------------------
| Name           : var_std_log_HR  
| Required (Y/N) : N
| Default Value  : var_std_log_HR
| Type ($/#)     : #
| Purpose        : Name of output data set variable that will contain the variance error of the estimate of the 
|                  standardized absolute log hazard ratio.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_log_HR_correct  
| Required (Y/N) : N
| Default Value  : abs_std_log_HR_correct
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the bias-corrected estimate of the standardized absolute
|                  log hazard ratio.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_HR_correct  
| Required (Y/N) : N
| Default Value  : abs_std_HR_correct
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the bias-corrected estimate of the standardized absolute
|                  hazard ratio.
|-----------------------------------------------------------------------------------------------
| Name           : chi_sq               
| Required (Y/N) : N
| Default Value  : chi_sq
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain a noncentral chi-square statistic that can be used to
|                  to test hypotheses about the absolute standardized hazard ratio.
|-----------------------------------------------------------------------------------------------
| Name           : df               
| Required (Y/N) : N
| Default Value  : df
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the degrees of freedom for the noncentral chi-square statistic.
|                  This equals the number of terms in the model.
|-----------------------------------------------------------------------------------------------
| Name           : p_value               
| Required (Y/N) : N
| Default Value  : p_value
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the p-value from a test of the hypothesis that the 
|                  absolute standardized log hazard ratio is 0.
|-----------------------------------------------------------------------------------------------
| Name           : p_value_int               
| Required (Y/N) : N
| Default Value  : p_value_int
| Type ($/#)     : #
| Purpose        : If the parameter intvl_hyp is specified, the output data set variable named by this parameter will contain 
|                  the p-value from the conservative test of the interval null hypothesis that the absolute standardized log hazard ratio 
|                  is less than or equal to &intvl_hyp.
|-----------------------------------------------------------------------------------------------
| Name           : p_value_int_Scheffe               
| Required (Y/N) : N
| Default Value  : p_value_int_Scheffe
| Type ($/#)     : #
| Purpose        : If the parameter intvl_hyp is specified, the output data set variable named by this parameter will contain 
|                  the p-value from the Scheffe-alignment test of the interval null hypothesis that the absolute standardized log hazard ratio 
|                  is less than or equal to &intvl_hyp.
|-----------------------------------------------------------------------------------------------
| Name           : min_eigenvalue               
| Required (Y/N) : N
| Default Value  : min_eigenvalue
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the minimum eigenvalue of the product of (1) the matrix square root
|                  of the covariance matrix of the covariate vector with (2) the covariance matrix of the regression
|                  parameter estimate vector with (3) the matrix square root of the covariance matrix of the covariate vector.
|                  This minimum eigen value can be used to construct tests of interval null hypotheses about the absolute
|                  standardized hazard ratio.
|-----------------------------------------------------------------------------------------------
| Name           : noncentrality_Scheffe               
| Required (Y/N) : N
| Default Value  : noncentrality_Scheffe
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the maximum noncentrality parameter consistent with a 95%
|                  Scheffe confidence ellipsoid about the transformed parameter estimate vector.  This value can be used to
|                  to construct tests of interval null hypotheses about the absolute standardized hazard ratio.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_log_HR_LCL               
| Required (Y/N) : N
| Default Value  : abs_std_log_HR_LCL
| Type ($/#)     : #
| Purpose        : Lower limit of the 100(1-alpha)% confidence interval for the absolute standardized log hazard ratio using
|                  the conservative method.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_log_HR_UCL               
| Required (Y/N) : N
| Default Value  : abs_std_log_HR_UCL
| Type ($/#)     : #
| Purpose        : Upper limit of the 100(1-alpha)% confidence interval for the absolute standardized log hazard ratio using
|                  the conservative method.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_log_HR_LCL_Scheffe               
| Required (Y/N) : N
| Default Value  : abs_std_log_HR_LCL_Scheffe
| Type ($/#)     : #
| Purpose        : Lower limit of the 100(1-alpha)% confidence interval for the absolute standardized log hazard ratio computed
|                  using Scheffe alignment.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_log_HR_UCL_Scheffe               
| Required (Y/N) : N
| Default Value  : abs_std_log_HR_UCL_Scheffe
| Type ($/#)     : #
| Purpose        : Upper limit of the 100(1-alpha)% confidence interval for the absolute standardized log hazard ratio computed
|                  using Scheffe alignement.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_HR_LCL               
| Required (Y/N) : N
| Default Value  : abs_std_HR_LCL
| Type ($/#)     : #
| Purpose        : Lower limit of the 100(1-alpha)% confidence interval for the absolute standardized hazard ratio using
|                  the conservative method.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_HR_UCL               
| Required (Y/N) : N
| Default Value  : abs_std_HR_UCL
| Type ($/#)     : #
| Purpose        : Upper limit of the 100(1-alpha)% confidence interval for the absolute standardized hazard ratio using
|                  the conservative method.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_HR_LCL_Scheffe               
| Required (Y/N) : N
| Default Value  : abs_std_HR_LCL_Scheffe
| Type ($/#)     : #
| Purpose        : Lower limit of the 100(1-alpha)% confidence interval for the absolute standardized hazard ratio computed
|                  using Scheffe alignment.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_HR_UCL_Scheffe               
| Required (Y/N) : N
| Default Value  : abs_std_HR_UCL_Scheffe
| Type ($/#)     : #
| Purpose        : Upper limit of the 100(1-alpha)% confidence interval for the absolute standardized hazard ratio computed
|                  using Scheffe alignement.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_HR_correct_LCL               
| Required (Y/N) : N
| Default Value  : abs_std_HR_correct_LCL
| Type ($/#)     : #
| Purpose        : Lower limit of the bias-corrected 100(1-alpha)% confidence interval for the absolute standardized hazard ratio using
|                  the conservative method.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_HR_correct_UCL               
| Required (Y/N) : N
| Default Value  : abs_std_HR_correct_UCL
| Type ($/#)     : #
| Purpose        : Upper limit of the bias-corrected 100(1-alpha)% confidence interval for the absolute standardized hazard ratio using
|                  the conservative method.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_HR_correct_LCL_Scheffe               
| Required (Y/N) : N
| Default Value  : abs_std_HR_correct_LCL_Scheffe
| Type ($/#)     : #
| Purpose        : Lower limit of the bias-corrected 100(1-alpha)% confidence interval for the absolute standardized hazard ratio computed
|                  using Scheffe alignment.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_HR_correct_UCL_Scheffe               
| Required (Y/N) : N
| Default Value  : abs_std_HR_correct_UCL_Scheffe
| Type ($/#)     : #
| Purpose        : Upper limit of the bias-corrected 100(1-alpha)% confidence interval for the absolute standardized hazard ratio computed
|                  using Scheffe alignement.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : zb_prefix               
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : If this parameter is specified, the vector of standardized log hazard ratios for the individual
|                  covariates will be stored in variables with the specified prefix and suffixes from 1 to the number of variables.
|                  The order of these variables will correspond to the order of the covariates specified in the macro parameter
|                  vars.
|-----------------------------------------------------------------------------------------------
| Name           : zv_prefix               
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : If this parameter is specified, the covariance matrix of the standardized log hazard ratio estimates for the 
|                  individual covariates will be stored in variables with the specified prefix and suffixes from 1 to the square of 
|                  the number of variables.  The order of these variables will correspond to the order of the covariates specified 
|                  in the macro parameter vars: v11, v12,..., v1p, v21, v22,..., v2p,...,vp1, vp2,..., vpp.
|-----------------------------------------------------------------------------------------------
| Name           : contribution_prefix               
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : If this parameter is specified, the vector consisting of the proportional contribution of each covariate to 
|                  the total squared standardized log hazard ratios will be stored in variables with the specified prefix and 
|                  suffixes consisting of the covariate names.
|-----------------------------------------------------------------------------------------------
| Name           : predictors              
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : If this parameter is specified, a variable with this name will be included in the output data set and contain
|                  the list of variables represented by the standardized log hazard ratio.                                           
|-----------------------------------------------------------------------------------------------
| Name           : maxVIF               
| Required (Y/N) : N
| Default Value  : maxVIF
| Type ($/#)     : #
| Purpose        : Name of output data set variable that will contain the maximum variance inflation factor (VIF) over all the covariates
|                  from the screen for multicollinearity.  If the maximum VIF is greater than 100, then no standardized log hazard ratio
|                  will be returned.
|-----------------------------------------------------------------------------------------------
| Name           : Prop_Var_Expl               
| Required (Y/N) : N
| Default Value  : Prop_Var_Expl
| Type ($/#)     : #
| Purpose        : Name of output data set variable that will contain the estimate of the proportion of variance explained (Kent-O Quigley,
|                  Biometrika 75:525-534, 1988) by the factors given in parameter vars.  The total variance is the variance remaining after 
|                  accounting for any stratification factors.  
|-----------------------------------------------------------------------------------------------                                   
| Name           : var_combo_outdsn               
| Required (Y/N) : Y if parameter var_combo_indsn specified.
| Default Value  : 
| Type ($/#)     : $
| Purpose        : Name of output data set that will contain the estimated contributions of specified combinations of variables together with
|                  the standard error of each estimate.                         
|-----------------------------------------------------------------------------------------------
**********************************************************************************************************/
 
%local mergenobyoption t nvar nvar2 nadjcov nadjcov2 npartial npadjcov npadjcov2 ncovpadjcov npstrat i j k L nimlby nbyvar byflag skipflag lpartial doublespec_flag;


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

   %let t = _STLNHR;

   %Tmp_SubLibL(tmplib=&t.);

/*  Capture status of mergenoby option so it can be reset to its current value at the end of the macro */

proc optsave out=&t..options;
run;

proc sql noprint;
    select OPTVALUE into :mergenobyoption
    from &t..options where (OPTNAME = 'MERGENOBY');
quit;

options mergenoby=nowarn;


/* Check for errors in macro parameter specification */

   %if %length(&indsn.)=0 %then %do;
      %put ERROR :  STDLOGHR macro parameter indsn must be specified.;
      %abort;
   %end;

   %if %length(&vars.)=0 %then %do;
      %put ERROR :  STDLOGHR macro parameter vars must be specified.;
      %abort;
   %end;

   %if %length(&time.)=0 %then %do;
      %put ERROR :  STDLOGHR macro parameter time must be specified.;
      %abort;
   %end;

   %if %length(&censor.)=0 %then %do;
      %put ERROR :  STDLOGHR macro parameter censor must be specified.;
      %abort;
   %end;

   %if %length(&outdsn.)=0 %then %do;
      %put ERROR :  STDLOGHR macro parameter outdsn must be specified.;
      %abort;
   %end;

   %if %length(&alpha.) %then %do;
   %if %sysevalf(&alpha. <= 0) or %sysevalf(&alpha. >= 1) %then %do;
      %put ERROR :  STDLOGHR macro parameter alpha must be >0 and <1.;
      %abort;
   %end;
   %end;

   %if %length(&var_combo_indsn.) > 0 and %length(&contribution_prefix.) = 0 %then %do;
       %put  ERROR :  STDLOGHR macro found parameter var_combo_indsn specified but parameter contribution_prefix not specified.;
       %abort;
       %end;

   %if %length(&var_combo_indsn.) > 0 and %length(&var_combo_indsn.) = 0 %then %do;
       %put  ERROR :  STDLOGHR macro found parameter var_combo_indsn specified but parameter var_combo_indsn not specified.;
       %abort;
       %end;
  
/*  Set default values of parameters not specified */

%if %length(&maxVIF.) = 0 %then %let maxVIF = maxVIF;
%if %length(&Prop_Var_Expl.) = 0 %then %let Prop_Var_Expl = Prop_Var_Expl;
%let npadjcov = 0;
%let npstrat = 0;
%if %length(&abs_std_log_HR.) = 0 %then %let abs_std_log_HR = abs_std_log_HR;
%if %length(&abs_std_log_HR_correct.) = 0 %then %let abs_std_log_HR_correct = abs_std_log_HR_correct;
%if %length(&abs_std_HR_correct.) = 0 %then %let abs_std_HR_correct = abs_std_HR_correct;
%if %length(&chi_sq.) = 0 %then %let chi_sq = chi_sq;
%if %length(&df.) = 0 %then %let df = df;
%if %length(&p_value.) = 0 %then %let p_value = p_value;
%if %length(&p_value_int.) = 0 %then %let p_value_int = p_value_int;
%if %length(&p_value_int_Scheffe.) = 0 %then %let p_value_int_Scheffe = p_value_int_Scheffe;
%if %length(&min_eigenvalue.) = 0 %then %let min_eigenvalue = min_eigenvalue;
%if %length(&noncentrality_Scheffe.) = 0 %then %let noncentrality_Scheffe = noncentrality_Scheffe;
%if %length(&abs_std_log_HR_LCL.) = 0 %then %let abs_std_log_HR_LCL = abs_std_log_HR_LCL;
%if %length(&abs_std_log_HR_UCL.) = 0 %then %let abs_std_log_HR_UCL = abs_std_log_HR_UCL;
%if %length(&abs_std_log_HR_LCL_Scheffe.) = 0 %then %let abs_std_log_HR_LCL_Scheffe = abs_std_log_HR_LCL_Scheffe;
%if %length(&abs_std_log_HR_UCL_Scheffe.) = 0 %then %let abs_std_log_HR_UCL_Scheffe = abs_std_log_HR_UCL_Scheffe;
%if %length(&abs_std_HR_LCL.) = 0 %then %let abs_std_HR_LCL = abs_std_HR_LCL;                                      
%if %length(&abs_std_HR_UCL.) = 0 %then %let abs_std_HR_UCL = abs_std_HR_UCL;   
%if %length(&abs_std_HR_LCL_Scheffe.) = 0 %then %let abs_std_HR_LCL_Scheffe = abs_std_HR_LCL_Scheffe; 
%if %length(&abs_std_HR_UCL_Scheffe.) = 0 %then %let abs_std_HR_UCL_Scheffe = abs_std_HR_UCL_Scheffe; 
%if %length(&abs_std_HR_correct_LCL.) = 0 %then %let abs_std_HR_correct_LCL = abs_std_HR_correct_LCL; 
%if %length(&abs_std_HR_correct_UCL.) = 0 %then %let abs_std_HR_correct_UCL = abs_std_HR_correct_UCL; 
%if %length(&abs_std_HR_correct_LCL.) = 0 %then %let abs_std_HR_correct_LCL = abs_std_HR_correct_LCL; 
%if %length(&abs_std_HR_correct_UCL.) = 0 %then %let abs_std_HR_correct_UCL = abs_std_HR_correct_UCL;
%if %length(&abs_std_HR_correct_LCL_Scheffe.) = 0 %then %let abs_std_HR_correct_LCL_Scheffe = abs_std_HR_correct_LCL_Scheffe; 
%if %length(&abs_std_HR_correct_UCL_Scheffe.) = 0 %then %let abs_std_HR_correct_UCL_Scheffe = abs_std_HR_correct_UCL_Scheffe; 


/**********************************************************************************************
**  Parse the string containing the "by variable".                                            *
**********************************************************************************************/

%if %length(&byvar.) %then %do;
   %let byvar=%sysfunc(compbl(&byvar.));
   %let byflag = 1;
%end;
%else %do;
   %let byvar = dummyby;
   %let byflag = 0;
%end;

%let nbyvar = %sysfunc(countc(%bquote(&byvar.), %str( )));
%let nbyvar = %sysfunc(ifc(%length(%bquote(&byvar.)), %eval(&nbyvar. + 1), 0));
   
%do i = 1 %to &nbyvar.;
   %local byvar&i.;
   %let byvar&i = %scan(&byvar., &i., %str( ));
%end;         

                     
/**********************************************************************************************
**  Parse the input parameter vars containing the list of covariates, determine the number    *
**  of covariates, and assign them to the macro variables var1, var2, etc.                    *
**********************************************************************************************/

%let vars = %sysfunc(compbl(&vars.));

%let nvar = %sysfunc(countc(%bquote(&vars.), %str( )));
%let nvar = %sysfunc(ifc(%length(%bquote(&vars.)), %eval(&nvar. + 1), 0));

%local nvar;   

%do i = 1 %to &nvar.;
   %local var&i.;
   %let var&i = %scan(&vars., &i., %str( ));
%end;     

%let nvar2 = %sysevalf(&nvar.**2,integer); 


/**********************************************************************************************
**  Parse the input parameter adjcov containing the list of adjustment covariates, determine  *
**  the number of adjustment covariates, and assign them to the macro variables adjcov1,      *
**  adjcov2, etc.                                                                             *
**********************************************************************************************/

%if %length(&adjcov.) %then %do;

%let adjcov = %sysfunc(compbl(&adjcov.));

%let nadjcov = %sysfunc(countc(%bquote(&adjcov.), %str( )));
%let nadjcov = %sysfunc(ifc(%length(%bquote(&adjcov.)), %eval(&nadjcov. + 1), 0));

%local nadjcov;   

%do i = 1 %to &nadjcov.;
   %local adjcov&i.;
   %let adjcov&i = %scan(&adjcov., &i., %str( ));
%end;     

/**********************************************************************************************
**  Check for variables specified in both vars and adjcov.  Abort procedure if this is found. *
**********************************************************************************************/

%let doublespec_flag = 0;

%do i = 1 %to &nvar.;
%do j = 1 %to &nadjcov.;
    %if %UPCASE(&&var&i..) = %UPCASE(&&adjcov&j..) %then %let doublespec_flag = &i.;
%end;
%end;

%if &doublespec_flag. > 0 %then %do;
    %put ERROR :  STDLOGHR macro found variable &&var&doublespec_flag.. listed in both parameter vars and parameter adjcov.  These two parameters must have distinct lists of variables.;
    %abort;
    %end;

%end;

/**********************************************************************************************
**  Parse the input parameter partial containing the list of adjustment covariates and        *
**  stratification variables to condition on.  Assign the adjustment covariates to the macro  *
**  variables padjcov1, padjcov2, etc. and the stratification variables to the macro variables*
**  pstrat1, pstrat2, etc.                                                                    *
**********************************************************************************************/

%if %length(&partial.) %then %do;

%let partial = %sysfunc(compbl(&partial.));

%let npartial = %sysfunc(countc(%bquote(&partial.), %str( )));
%let npartial = %sysfunc(ifc(%length(%bquote(&partial.)), %eval(&npartial. + 1), 0));

%local npartial pvar npadjcov npstrat;
%let npadcov = 0;
%let npstrat = 0;

%do i = 1 %to &npartial.;
   %let pvar = %scan(&partial., &i., %str( ));
   %if %index(&adjcov.,&pvar.) > 0 %then %do;
      %let npadjcov = %sysevalf(&npadjcov.+1,integer);
      %local padjcov&npadjcov.;
      %let padjcov&npadjcov. = &pvar.;
      %end;
   %else %if %index(&strata.,&pvar.) > 0 %then %do;
      %let npstrat = %sysevalf(&npstrat.+1,integer);
      %local pstrat&npstrat.;
      %let pstrat&npstrat. = &pvar.;
      %end;
   %else %do;
      %put ERROR :  STDLOGHR macro parameter found specified partial variable &pvar..;
      %put ERROR :  This variable is not specified as an adjustment covariate or stratification variable.;
      %if %length(&adjcov.) %then %put ERROR :  Adjustment covariates:  &adjcov..;
      %if %length(&strata.) %then %put ERROR :  Stratification variables:  &strata..;
      %abort;
      %end;        
%end; 

%let npadjcov2 = %sysevalf(&npadjcov.**2,integer); 

%let ncovpadjcov = %sysevalf(&nvar.*&npadjcov.,integer);

%end;

/**********************************************************************************************
**  Parse the input parameter strata containing the list of stratification variables,         *
**  determine the number of these variables, and assign them to the macro variables strat1,   *
**  strat2, etc.                                                                              *
**********************************************************************************************/

%if %length(&strata.) %then %do;

%let strata = %sysfunc(compbl(&strata.));

%let nstrat = %sysfunc(countc(%bquote(&strata.), %str( )));
%let nstrat = %sysfunc(ifc(%length(%bquote(&strata.)), %eval(&nstrat. + 1), 0));

%local nstrat;   

%do i = 1 %to &nstrat.;
   %local strat&i.;
   %let strat&i = %scan(&strata., &i., %str( ));
%end;   

/**********************************************************************************************
**  Check for variables specified in both vars and strata.  Abort procedure if this is found. *
**********************************************************************************************/

%let doublespec_flag = 0;

%do i = 1 %to &nvar.;
%do j = 1 %to &nstrat.;
    %if %UPCASE(&&var&i..) = %UPCASE(&&strat&j..) %then %let doublespec_flag = &i.;
%end;
%end;

%if &doublespec_flag. > 0 %then %do;
    %put ERROR :  STDLOGHR macro found variable &&var&doublespec_flag.. listed in both parameter vars and parameter strata.  These two parameters must have distinct lists of variables.;
    %abort;
    %end;

%end;


/**********************************************************************************************
**  Check for nonexistent input data sets and nonexistent variables in the input data sets.   *
**********************************************************************************************/

  %let errcode = 0;
  %let dsid = %sysfunc(open(&indsn.,i));
  %if &dsid. = 0 %then %do;
     %put ERROR : STDLOGHR macro found that specified input data set &indsn. does not exist.;
     %abort;
     %end;
   %else %do;
     %do k = 1 %to &nvar.;
          %if %sysfunc(varnum(&dsid.,&&var&k..)) = 0 %then %do;
            %put ERROR : STDLOGHR macro input data set &indsn. does not contain the variable &&var&k.. specified in parameter vars.;
            %let errcode = 1;
            %end;
         %end;
     %if %length(&weight.) %then %do;
         %if %sysfunc(varnum(&dsid.,&weight.)) = 0 %then %do;
            %put ERROR : STDLOGHR macro input data set &indsn. does not contain variable &weight. specified in macro parameter weight.;
            %let errcode = 1; 
            %end;
         %end;
     %if %length(&adjcov.) %then %do k = 1 %to &nadjcov.;
          %if %sysfunc(varnum(&dsid.,&&adjcov&k..)) = 0 %then %do;
            %put ERROR : STDLOGHR macro input data set &indsn. does not contain the variable &&adjcov&k.. specified in macro parameter adjcov.;
            %let errcode = 1;
            %end;
         %end;
     %if %length(&strata.) %then %do k = 1 %to &nstrat.;
         %if %sysfunc(varnum(&dsid.,&&strat&k..)) = 0 %then %do;
            %put ERROR : STDLOGHR macro input data set &indsn. does not contain variable &&strat&k.. specified in macro parameter strata.;
            %let errcode = 1;
            %end;
         %end;
     %let rc = %sysfunc(close(&dsid.));
     %if &errcode = 1 %then %abort;
     %end;

     %if %length(&var_combo_indsn.) %then %do;
        %let errcode = 0;
        %let dsid = %sysfunc(open(&var_combo_indsn.,i));
        %if &dsid. = 0 %then %do;
            %put ERROR : STDLOGHR macro variable found that specified combination indicator data set &var_combo_indsn. does not exist.;
            %abort;
            %end;
        %else %do k = 1 %to &nvar.;
            %if %sysfunc(varnum(&dsid.,ind_&&var&k..)) = 0 %then %do;
                %put ERROR : STDLOGHR macro variable combination indicator data set &var_combo_indsn. does not contain the required variable ind_&&var&k..;
                %let errcode = 1;
                %end;
            %end;
        %let rc = %sysfunc(close(&dsid.));
        %if &errcode = 1 %then %abort;
        %end;


/*********************************************************************************************/
/**  Get the 0.95 quantile of the central chi-square distribution.                          **/
/*********************************************************************************************/

data &t..a;
    chi_quant = CINV(0.95,&nvar.);
    call symput('chi_quant',trim(left(put(chi_quant,20.10))));
run;


/**********************************************************************************************
**  Get the input data.                                                                       *
**********************************************************************************************/

data &t..indata;
     set &indsn;
     dummyby = 1;  /* Dummy variable for merges */

/* Delete any observation that does not have values for the response variables and all covariates */
     if &time. = . or &censor. = .  %do i = 1 %to &nvar.;    or &&var&i..    = . %end; 
        %if %length(&adjcov.) %then %do i = 1 %to &nadjcov.; or &&adjcov&i.. = . %end; then delete;

/* Create weight variable that defaults to 1 if no weights are specified */

%if %length(&weight.)> 0 %then %do; qqweight = &weight.; %end;
                         %else %do; qqweight = 1; %end;

/* Delete any observation that has 0 weight */
     if qqweight = 0 then delete;
     
run;

proc sort data=&t..indata;
      by &byvar. %do i = 1 %to &npstrat.; &&pstrat&i.. %end;;
run;


/*********************************************************************************************/
/** Check for multicollinearity in the differences of the regression coefficients from their**/
/** risk set means where events occurred.  If collinearity or near collinearity found,      **/
/** do not do the standardized log hazard ratio calculation, as results will be unreliable. **/
/*********************************************************************************************/

data &t..collin;
     set &t..indata;
     if &censor. in (&censorlist.) then qqeventvar = 0;
                                   else qqeventvar = 1;
     one = 1;
     keep qqeventvar &time. &vars. qqweight one &byvar. &strata.;
run;

proc sort data=&t..collin;
    by &byvar. &strata. &time. descending qqeventvar;
run;

/**********************************************************************************************/
/**  Compute the sample size or total weighted and (weighted) sum of the covariates.         **/
/**********************************************************************************************/

proc univariate data=&t..collin noprint;
    by &byvar. &strata.;
    weight qqweight;
    var &vars. one;
    output out=&t..runsum sum = %do i = 1 %to &nvar.; rsum&i. %end; W;
run;

/**********************************************************************************************/
/**  Compute the differences of the covariates from their risk set means.                    **/
/**********************************************************************************************/

data &t..collin;
    array zvar(&nvar.) &vars.; 
    array rsum(&nvar.) rsum1-rsum&nvar.;
    array rmean(&nvar.) rmean1-rmean&nvar.;
    array zvar_diff(&nvar.) zvar_diff1-zvar_diff&nvar.;

    merge &t..collin &t..runsum;
    by &byvar. &strata.;
    if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; 
                 %if %length(&strata.) %then %do i = 1 %to &nstrat;  or first.&&strat&i.. %end; then Wr = W;

    do i = 1 to &nvar.;
        rmean(i) = rsum(i) / Wr;
        zvar_diff(i) = zvar(i) - rmean(i);
        end;

    do i = 1 to &nvar.;
       rsum(i) = rsum(i) - qqweight * zvar(i);
       end;
    Wr = Wr - qqweight;

    retain rsum1-rsum&nvar. Wr;
    keep qqeventvar zvar_diff1-zvar_diff&nvar. qqweight &byvar. &strata.;
run;

/**********************************************************************************************/
/**  Use the variance inflation factor calculation in proc reg to detect collinearity.       **/
/**********************************************************************************************/

ods output parameterestimates=&t..parmest;
ods listing close;  /* Turn off the listing for proc reg */  

proc reg data=&t..collin;
     by &byvar.;
     where qqeventvar = 1;
     weight qqweight;
     model qqeventvar = %do i = 1 %to &nvar.; zvar_diff&i. %end; / vif tol;
run;

ods output close;  
ods listing;  /* Turn the listing back on in case the user wants printed output from PHREG below. */  

/**********************************************************************************************/
/**  Create a data set with a flag to indicate if collinearity was found.                    **/
/**********************************************************************************************/

data &t..colflag;
     set &t..parmest end=eof;
     by &byvar.;
     if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; then do; 
         colflag = 0;
         &maxVIF. = .;
         imlby = imlby + 1;  /* This variable will be used at the end of the macro for a merge */
         end;

     if VarianceInflation >= 100 or DF = 0 then colflag = 1;
     &maxVIF. = max(&maxVIF.,VarianceInflation);         

     if last.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or last.&&byvar&i.. %end; then output;; 

     retain colflag imlby 0 &maxVIF.;
     keep colflag &maxVIF. imlby &byvar.;
run;

/**********************************************************************************************
**  Run the proportional hazards regression analysis.  If a weight is specified, incorporate  *
**  it in the analysis and use the covariance sandwich estimate of the covariance matrix.     *
**********************************************************************************************/

%if %length(&weight.) = 0 %then %do;
/**********************************************************************************************
**  Case 1: no cohort sampling.                                                              **
**********************************************************************************************/
proc phreg data=&t..indata outest=&t..outest covout
                                 %if &robust.=yes %then covs;  %if &print. = no %then noprint;;
     by &byvar.;
     model &time.*&censor.(&censorlist.) = %do i = 1 %to &nvar.; &&var&i.. %end;
                                     &adjcov. / covb ties=efron;
%if %length(&strata.) > 0 %then strata &strata.;;
run;
%end;  
                      %else %do;
/**********************************************************************************************
**  Case 2: Cohort sampling design with analysis using Lin and Wei covariance sandwich method.**
**********************************************************************************************/
proc phreg data=&t..indata covs outest=&t..outest covout %if &print. = no %then noprint;;
     by &byvar.;
     model &time.*&censor.(&censorlist.) = %do i = 1 %to &nvar.; &&var&i.. %end;
                                     &adjcov. / covb ties=efron;
%if %length(&strata.) > 0 %then strata &strata.;;
     weight &weight.;
run;

%end;


/**********************************************************************************************
**  Capture the variance-covariance matrix of the parameter estimates.                        *
**********************************************************************************************/

data &t..v;
      array covariate(&nvar.) %do i = 1 %to &nvar.; &&var&i.. %end;; 
      array v(&nvar.,&nvar.) v1-v&nvar2.;
 
      set &t..outest;
      by &byvar.;
      where upcase(_NAME_) ne upcase("&time.");

/* Initialize cov matrix to missing values */
 
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

data &t..b;

      array b(&nvar.) b1-b&nvar.;
      array covariate(&nvar.) %do i = 1 %to &nvar.; &&var&i.. %end;; 

      set &t..outest;
      by &byvar.;
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

/***********************************************************************************************
**  Calculate the (weighted) sample mean covariate vector, by specified strata if analysis   **
**  is stratified and the partial SLHR by stratum variables is requested.                     **
***********************************************************************************************/

proc univariate data=&t..indata noprint;
      by &byvar. %do i = 1 %to &npstrat.; &&pstrat&i.. %end;;
      weight qqweight;
      var %do i = 1 %to &nvar.; &&var&i.. %end;;
      output out = &t..mean mean =  mz1-mz&nvar.;
run;

/***********************************************************************************************
**  Calculate the (weighted) sample mean adjustment covariate vector                          **
***********************************************************************************************/

%if &npadjcov. > 0 %then %do;

proc univariate data=&t..indata noprint;
      by &byvar. %do i = 1 %to &npstrat.; &&pstrat&i.. %end;;
      weight qqweight;
      var %do i = 1 %to &npadjcov.; &&padjcov&i.. %end;;
      output out = &t..meanpadjcov mean =  mzpadjcov1-mzpadjcov&npadjcov.;
run;

%end;

/***********************************************************************************************
**  Determine the sample size (or sum of weights)                                             **
***********************************************************************************************/

proc univariate data=&t..indata noprint;
      by &byvar.;
      var qqweight;
      output out = &t..n sum = W;
run;

data &t..mean;
     merge &t..mean &t..n;
     by &byvar.;
run;

%if &npadjcov. > 0 %then %do;

data &t..meanpadjcov;
     merge &t..meanpadjcov &t..n;
     by &byvar.;
run;

%end;

/***********************************************************************************************
**  Calculate the sample covariance matrix of the covariates                                  **
***********************************************************************************************/

data &t..zz;
     array covariate(&nvar.) %do i = 1 %to &nvar.; &&var&i.. %end;; 
     array mz(&nvar.) mz1-mz&nvar.;
     array zz(&nvar.,&nvar.) zz1-zz&nvar2.;

     merge &t..indata &t..mean;
     by &byvar. %do i = 1 %to &npstrat.; &&pstrat&i.. %end;;

     if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; then do;
     do i = 1 to &nvar.;
        do j = 1 to &nvar.;
           zz(i,j) = 0;
           end;
        end;
     end;

     do i = 1 to &nvar.;
        do j = 1 to &nvar.;
           zz(i,j) = zz(i,j) + qqweight * (covariate(i) - mz(i)) * (covariate(j) - mz(j));
           end;
        end;

     if last.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or last.&&byvar&i.. %end; then do;

        do i = 1 to &nvar.;
           do j = 1 to &nvar.;
               zz(i,j) = zz(i,j) / (W-1);
               end;
           end;
        output;
        end;    

    retain zz1-zz&nvar2.;
    keep zz1-zz&nvar2. &byvar.;
run;          


/***********************************************************************************************
**  Calculate the sample covariance matrix of the adjustment covariates to be conditioned     **
**  for the partial ASLHR.                                                                    **
***********************************************************************************************/

%if &npadjcov. > 0 %then %do;

data &t..zzpadjcov;
     array padjcovariate(&npadjcov.) %do i = 1 %to &npadjcov.; &&padjcov&i.. %end;; 
     array mzpadjcov(&npadjcov.) mzpadjcov1-mzpadjcov&npadjcov.;
     array zzpadjcov(&npadjcov.,&npadjcov.) zzpadjcov1-zzpadjcov&npadjcov2.;

     merge &t..indata &t..meanpadjcov;
     by &byvar. %do i = 1 %to &npstrat.; &&pstrat&i.. %end;;

     if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; then do;
     do i = 1 to &npadjcov.;
        do j = 1 to &npadjcov.;
           zzpadjcov(i,j) = 0;
           end;
        end;
     end;

     do i = 1 to &npadjcov.;
        do j = 1 to &npadjcov.;
           zzpadjcov(i,j) = zzpadjcov(i,j) + qqweight * (padjcovariate(i) - mzpadjcov(i)) * (padjcovariate(j) - mzpadjcov(j));
           end;
        end;

     if last.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or last.&&byvar&i.. %end; then do;

        do i = 1 to &npadjcov.;
           do j = 1 to &npadjcov.;
               zzpadjcov(i,j) = zzpadjcov(i,j) / (W-1);
               end;
           end;
        output;
        end;    

    retain zzpadjcov1-zzpadjcov&npadjcov2.;
    keep zzpadjcov1-zzpadjcov&npadjcov2. &byvar.;
run;  

%end;


/***********************************************************************************************
**  Calculate the sample covariance matrix of the SLHR covariates with the partial adjustment **
**  covariates.                                                                               **
***********************************************************************************************/

%if &npadjcov. > 0 %then %do;

data &t..zzcovpadjcov;
     array covariate(&nvar.) %do i = 1 %to &nvar.; &&var&i.. %end;; 
     array mz(&nvar.) mz1-mz&nvar.;
     array padjcovariate(&npadjcov.) %do i = 1 %to &npadjcov.; &&padjcov&i.. %end;; 
     array mzpadjcov(&npadjcov.) mzpadjcov1-mzpadjcov&npadjcov.;
     array zzcovpadjcov(&nvar.,&npadjcov.) zzcovpadjcov1-zzcovpadjcov&ncovpadjcov.;

     merge &t..indata &t..mean  &t..meanpadjcov;
     by &byvar. %do i = 1 %to &npstrat.; &&pstrat&i.. %end;;

     if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; then do;
     do i = 1 to &nvar.;
        do j = 1 to &npadjcov.;
           zzcovpadjcov(i,j) = 0;
           end;
        end;
     end;

     do i = 1 to &nvar.;
        do j = 1 to &npadjcov.;
           zzcovpadjcov(i,j) = zzcovpadjcov(i,j) + qqweight * (covariate(i) - mz(i)) * (padjcovariate(j) - mzpadjcov(j));
           end;
        end;

     if last.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or last.&&byvar&i.. %end; then do;

        do i = 1 to &nvar.;
           do j = 1 to &npadjcov.;
               zzcovpadjcov(i,j) = zzcovpadjcov(i,j) / (W-1);
               end;
           end;
        output;
        end;    

    retain zzcovpadjcov1-zzcovpadjcov&ncovpadjcov.;
    keep zzcovpadjcov1-zzcovpadjcov&ncovpadjcov. &byvar.;
run;

%end;


/***********************************************************************************************
**  Combine the key summary statistics into one file                                          **
***********************************************************************************************/

data &t..keys;
     merge &t..v &t..b &t..zz
           %if &npadjcov. > 0 %then &t..zzpadjcov &t..zzcovpadjcov;;
     by &byvar.;
     keep b1-b&nvar. v1-v&nvar2. zz1-zz&nvar2.  
          %if &npadjcov. > 0 %then zzpadjcov1-zzpadjcov&npadjcov2. zzcovpadjcov1-zzcovpadjcov&ncovpadjcov.;
          &byvar.;
run;


/***********************************************************************************************
**  Estimate the square of the standardized log hazard ratio, with and without correction     **
**  for bias, and compute the chi-square test statistic for the null hypothesis that the      **
**  absolute standardized log hazard ratio is 0.                                              **
***********************************************************************************************/

data &t..foriml;
      array b(&nvar.) b1-b&nvar.;
      array v(&nvar.,&nvar.) v1-v&nvar2.;
      array zz(&nvar.,&nvar.) zz1-zz&nvar2.;

      set &t..keys end=eof;

/** PROC IML does not support a BY statement, so set up an index variable that can be used **/
/** with the WHERE clause in the USE statement.  If there is no by statement, this variable**/
/** has no effect.                                                                         **/

      imlby + 1;
      if eof then call symput('nimlby',trim(left(put(imlby,6.0))));

/**  Set up parameter estimate, parameter estimate covariance, and covariate covariance 
     matrices to be read into proc iml **/

%do i = 1 %to &nvar.;
      bentry = b(&i.); 
%do j = 1 %to &nvar.;
      ventry&j. = v(&i.,&j.);
      zzentry&j. = zz(&i.,&j.);
%end;

/**  Set up D matrices for calculating variance of proportional contributions of variables to the risk score variance **/

%do k = 1 %to &nvar.;
%do j = 1 %to &nvar.;
      d&k.entry&j. =  (&i.=&k.) * b(&j.) + (&j.=&k.) * b(&i.);
%end;
%end;
      output;
%end;

      keep bentry ventry1-ventry&nvar. zzentry1-zzentry&nvar. %do k = 1 %to &nvar.; d&k.entry1-d&k.entry&nvar. %end; 
           imlby &byvar.;
run;

%if &npadjcov. > 0 %then %do;

data &t..forimlpadjcov;
      array zzpadjcov(&npadjcov.,&npadjcov.) zzpadjcov1-zzpadjcov&npadjcov2.;
      array zzcovpadjcov(&nvar.,&npadjcov.) zzcovpadjcov1-zzcovpadjcov&ncovpadjcov.;
      
      set &t..keys;

      imlby + 1;

/**  Set up adjustment covariate covariance matrices to be read into proc iml **/

%do i = 1 %to &npadjcov.;
%do j = 1 %to &npadjcov.;
      zzpadjcoventry&j. = zzpadjcov(&i.,&j.);
%end;
%do j = 1 %to &nvar.;
      zzpadjcovcoventry&j. = zzcovpadjcov(&j.,&i.);
%end;
      output;
%end;

      keep zzpadjcoventry1-zzpadjcoventry&npadjcov. zzpadjcovcoventry1-zzpadjcovcoventry&nvar.  
           imlby &byvar.;
run;

%end;



/*  Data set to capture the by variables */

data &t..byvar;
      set &t..keys;
      imlby + 1;
      keep imlby &byvar.;
run;

%do imlby = 1 %to &nimlby.;

%let skipflag = 0;
data &t..colflag;
     set &t..colflag;
     if imlby = &imlby. and colflag = 1 then call symput('skipflag','1');
run;

%if &skipflag. = 1 %then %do;

data &t..stdlnhr;
     imlby = &imlby;
     stdlnhr2 = .;
     trzv = .;
     min_eigenvalue_SG = .;
     max_eigenvalue_SG = .;
     noncentrality_Scheffe_min = .;

     output;
run;

%end;
%else %do;

proc iml;
      use &t..foriml where(imlby=&imlby.);
      read all var {bentry} into BETA;
      read all var {%do i = 1 %to &nvar.;  ventry&i.   %end;} into V;
      read all var {%do i = 1 %to &nvar.; zzentry&i.   %end;} into SIGMAZ;
%do k = 1 %to &nvar.;
      read all var {%do i = 1 %to &nvar.; d&k.entry&i. %end;} into D&k.;
%end;


%if &npadjcov. > 0 %then %do;
      
      use &t..forimlpadjcov where(imlby=&imlby.);
      read all var {%do i = 1 %to &npadjcov.; zzpadjcoventry&i. %end;} into SIGMAZPADJCOV;
      read all var {%do i = 1 %to &nvar.; zzpadjcovcoventry&i. %end;} into SIGMAZPADJCOVCOV;

/*  Partial covariance matrix, if partial ASLHR requested */

      SIGMAZ = SIGMAZ - SIGMAZPADJCOVCOV` * INV(SIGMAZPADJCOV) * SIGMAZPADJCOVCOV;
%end;

/*  Squared standardized log hazard ratio estimate */

      stdlnhr2 = BETA` * SIGMAZ * BETA;

/*  Trace of the product of the covariance matrix of the covariates with */
/*  the covariance matrix of the parameter estimates */

      trzv = TRACE(V*SIGMAZ);

/*  Square root of SIGMAZ */

      CALL EIGEN(eigen_values_sigz,eigen_vectors_sigz,SIGMAZ);
      root_sigma_z = eigen_vectors_sigz * DIAG(SQRT(eigen_values_sigz)) * eigen_vectors_sigz`;

/*  Transformed parameter estimate and its covariance matrix */

      gamma_hat = root_sigma_z * BETA;
      SIGMA_G = root_sigma_z` * V * root_sigma_z;
      INV_SIGMA_G = INV(SIGMA_G);     



%if %length(&contribution_prefix.) %then %do;
     
/*  Vector of proportion contribution of each variable to the squared standardized hazard ratio */

      contribution = (vecdiag(gamma_hat * gamma_hat`) / stdlnhr2)`;
      

/*  Partial derivatives of vector of consisting of the proportional contribution of each variable to the risk score variance */

%do L = 1 %to &nvar.;
      partial_pi_&L. =  vecdiag(root_sigma_z * D&L. * root_sigma_z);
%end; 
      partial_pi = partial_pi_1 %do L = 2 %to &nvar.; || partial_pi_&L. %end;; 
      gradient_pi = (1 / stdlnhr2) * (partial_pi - 2 * contribution` * (SIGMAZ * BETA)` )`;
      
      GVG = gradient_pi` * V * gradient_pi;

/*  Standard errors of proportional contribution estimates. */

      se_contribution = sqrt(vecdiag(GVG)`);

%end;   

/*  Eigenvectors and eigenvalues of covariance matrix of transformed parameter estimate */

      CALL EIGEN(eigen_values_SG,eigen_vectors_SG,SIGMA_G);
      min_eigenvalue_SG = min(eigen_values_SG);
      max_eigenvalue_SG = max(eigen_values_SG);

/*  Noncentrality parameter for noncentral chi-square distribution using alignment method */

      NCV = (eigen_vectors_SG` * gamma_hat) / SQRT(eigen_values_SG); 
      noncentrality_align = NCV` * NCV;

/*  Inverse of DIAG(eigen_values) */

      LAMBDAI= DIAG( 1 / eigen_values_SG);

/*  Chi-square statistic */

      &chi_sq. = BETA` * INV(V) * BETA;

/*  Calculate maximum Scheffe-aligned noncentrality parameter.  First determine if Newton-Raphson iteration is needed */

     c_hat = eigen_vectors_SG` * gamma_hat;
     norm_c_hat = SQRT(c_hat` * c_hat);

%if &nvar. = 1 %then %do;
     c0_pos = norm_c_hat;
     c0_neg = -c0_pos;
%end;
%else %do;
     c0_pos = %do i = 1 %to &nvar.-1; 0 // %end; norm_c_hat;
     c0_neg = -c0_pos;
%end;
     chi_pos = (c0_pos - c_hat)` * LAMBDAI * (c0_pos - c_hat); 
     chi_neg = (c0_neg - c_hat)` * LAMBDAI * (c0_neg - c_hat);

     if chi_pos <= chi_neg then do;
          chi0 = chi_pos;
          c0 = c0_pos;
          end;
                           else do;
          chi0 = chi_neg;
          c0 = c0_neg;
          end;

     if chi0 <= &chi_quant. then do;
/* No Newton-Raphson iteration needed */
          noncentrality_Scheffe = c0` * LAMBDAI * c0;  
          end;
                               else do;
/*  Newton-Raphson iteration required.  Set initial values */

          c = c0; 
          LAMBDAIc = LAMBDAI * c;
          chch = c_hat` * c_hat;
          c_ch = c - c_hat;
          denom = &chi_quant. * chch - (chch - c`*c_hat) # c_ch` * LAMBDAIc;
          L1 = ((chch - c`*c_hat) * c` * LAMBDAIc - chch # c_ch` * LAMBDAIc ) / denom;
          L2 =  ((c_ch` * LAMBDAIc)##2 - &chi_quant. * c` * LAMBDAIc ) / denom;
          cL = c // L1 // L2;

/*  Newton-Raphson iteration */

          stepsize = 1;
          do iter = 1 to 1000 until (stepsize < 10**(-8));
             c = cL[1:&nvar.];
             L1 = cL[&nvar.+1];
             L2 = cL[&nvar.+2];
             c_ch = c - c_hat;
             LAMBDAIc_ch = LAMBDAI * c_ch;
/*  Objective vector */
             q = (LAMBDAI * c +  L1 # LAMBDAIc_ch + L2 # c)   //  
                 (c_ch` * LAMBDAIc_ch - &chi_quant.)          //
                 (c`*c - chch)                                 ;
/*  Jacobian */
             J = ( (1 + L1) # LAMBDAI + L2 # I(&nvar.) ||   LAMBDAIc_ch  ||  c )  //
                  ( 2 # LAMBDAIc_ch`                   ||      0         ||  0 )  //
                  ( 2 # c`                             ||      0         ||  0 )   ;

             step = GINV(J) * q;  /*  Use generalized inverse to avoid spurious findings of singular matrix */
             cL = cL - step;
             stepsize = SQRT(step` * step);
             end;

         if iter <= 1000 then do;
/*  Maximum noncentrality parameter using Scheffe method */
            noncentrality_Scheffe = c` * LAMBDAI * c;
            end;
                         else do;
/*  Algorithm failed to converge.  Use conservative initial estimate */
            noncentrality_Scheffe = c0` * LAMBDAI * c0;
            end;
         end;

/*  Calculate MINIMUM Scheffe-aligned noncentrality parameter.  First determine if Newton-Raphson iteration is needed */

     c_hat = eigen_vectors_SG` * gamma_hat;
     norm_c_hat = SQRT(c_hat` * c_hat);

%if &nvar. = 1 %then %do;
     c0_pos = norm_c_hat;
     c0_neg = -c0_pos;
%end;
%else %do;
     c0_pos = norm_c_hat %do i = 1 %to &nvar.-1;  // 0 %end;;
     c0_neg = -c0_pos;
%end;
     chi_pos = (c0_pos - c_hat)` * LAMBDAI * (c0_pos - c_hat); 
     chi_neg = (c0_neg - c_hat)` * LAMBDAI * (c0_neg - c_hat);

     if chi_pos <= chi_neg then do;
          chi0 = chi_pos;
          c0 = c0_pos;
          end;
                           else do;
          chi0 = chi_neg;
          c0 = c0_neg;
          end;

     if chi0 <= &chi_quant. then do;
/* No Newton-Raphson iteration needed */
          noncentrality_Scheffe_min = c0` * LAMBDAI * c0;  
          end;
                               else do;
/*  Newton-Raphson iteration required.  Set initial values */

          c = c0; 
          LAMBDAIc = LAMBDAI * c;
          chch = c_hat` * c_hat;
          c_ch = c - c_hat;
          denom = &chi_quant. * chch - (chch - c`*c_hat) # c_ch` * LAMBDAIc;
          L1 = ((chch - c`*c_hat) * c` * LAMBDAIc - chch # c_ch` * LAMBDAIc ) / denom;
          L2 =  ((c_ch` * LAMBDAIc)##2 - &chi_quant. * c` * LAMBDAIc ) / denom;
          cL = c // L1 // L2;

/*  Newton-Raphson iteration */

          stepsize = 1;
          do iter = 1 to 1000 until (stepsize < 10**(-8));
             c = cL[1:&nvar.];
             L1 = cL[&nvar.+1];
             L2 = cL[&nvar.+2];
             c_ch = c - c_hat;
             LAMBDAIc_ch = LAMBDAI * c_ch;
/*  Objective vector */
             q = (LAMBDAI * c +  L1 # LAMBDAIc_ch + L2 # c)   //  
                 (c_ch` * LAMBDAIc_ch - &chi_quant.)          //
                 (c`*c - chch)                                 ;
/*  Jacobian */
             J = ( (1 + L1) # LAMBDAI + L2 # I(&nvar.) ||   LAMBDAIc_ch  ||  c )  //
                  ( 2 # LAMBDAIc_ch`                   ||      0         ||  0 )  //
                  ( 2 # c`                             ||      0         ||  0 )   ;

             step = GINV(J) * q;  /*  Use generalized inverse to avoid spurious findings of singular matrix */
             cL = cL - step;
             stepsize = SQRT(step` * step);
             end;

         if iter <= 1000 then do;
/*  Minimum noncentrality parameter using Scheffe method */
            noncentrality_Scheffe_min = c` * LAMBDAI * c;
            end;
                         else do;
/*  Algorithm failed to converge.  Use conservative initial estimate */
            noncentrality_Scheffe_min = c0` * LAMBDAI * c0;
            end;
         end;


%if %length(&intvl_hyp.) %then %do;

     eta = abs(&intvl_hyp.);
     eta2 = eta * eta;

/*  Calculate conservative p-value for interval null hypothesis using noncentral chi-square distribution */

     nc = eta2 / min_eigenvalue_SG;            
     &p_value_int. = 1 - CDF('CHISQ',&chi_sq.,&nvar.,nc);  

/*  Calculate p-value for interval null hypothesis using Scheffe-aligned noncentral chi-square distribution */

     if noncentrality_Scheffe > . then do;
         nc2 = noncentrality_Scheffe * eta2 / stdlnhr2;          
         &p_value_int_Scheffe. = 1 - CDF('CHISQ',&chi_sq.,&nvar.,nc2);  
         end;
                                   else do;
         &p_value_int_Scheffe. = .;
         end;
%end;
    
      create &t..stdlnhr var {stdlnhr2 &chi_sq. trzv %if %length(&intvl_hyp.) %then &p_value_int. &p_value_int_Scheffe. ;
                              min_eigenvalue_SG max_eigenvalue_SG noncentrality_Scheffe noncentrality_Scheffe_min};
      append var {stdlnhr2 &chi_sq. trzv %if %length(&intvl_hyp.) %then &p_value_int. &p_value_int_Scheffe. ;
                              min_eigenvalue_SG max_eigenvalue_SG noncentrality_Scheffe noncentrality_Scheffe_min};

/*  Standardized log hazard ratios for individual covariates */

%if %length(&zb_prefix.) %then %do;

      b_std = SQRT(VECDIAG(SIGMAZ))#BETA;

      create &t..b_std from b_std [colname='b_std'];
      append from b_std;
%end;

/*  Covariance matrix of individual covariate standardized log hazard ratio estimates */

%if %length(&zv_prefix.) %then %do;

      v_std = SIGMAZ#V;

      create &t..v_std from v_std;
      append from v_std;
%end;

/*  Proportional contribution of each variable to the total squared standardized hazard ratio */

%if %length(&contribution_prefix.) %then %do;

      contribution_name = {%do i = 1 %to &nvar.-1; &contribution_prefix._&&var&i.., %end; &contribution_prefix._&&var&nvar.. };

      create &t..contribution from contribution [colname = contribution_name];
      append from contribution;

      se_contribution_name = {%do i = 1 %to &nvar.-1; se_&contribution_prefix._&&var&i.., %end; se_&contribution_prefix._&&var&nvar.. };
      
      create &t..se_contribution from se_contribution [colname = se_contribution_name];
      append from se_contribution;

      create &t..GVG from GVG;
      append from GVG;
      
%end;

run;

%if %length(&contribution_prefix.) %then %do;

***  Put matrix GVG in horizontal format;

data &t..GVG_horiz;
     array GVG(&nvar2.) GVG1-GVG&nvar2.;
     
     set &t..GVG end=eof;
     
     dummyby = 1;
     
%do i = 1 %to &nvar.;
     index = index + 1;
     GVG(index) = COL&i.;
%end;
     if eof then output;
     
     retain index 0 GVG1-GVG&nvar2.;
     keep dummyby GVG1-GVG&nvar2.;
run;   

**  Calculate the contribution and the standard error of the contribution of each requested combination of variables.;

%if %length(&var_combo_indsn.) %then %do;

data &t..var_ind_indata;
     set &var_combo_indsn.;
     dummyby = 1;
     combo_no = combo_no + 1;
     retain combo_no 0;
run;

data &t..sum_ind;
     merge &t..var_ind_indata &t..GVG_horiz;
     by dummyby;
run;

proc sort data=&t..sum_ind;
     by combo_no;
run;

data &t..contribution_d;
     set &t..contribution;
     dummyby = 1;
run;

data &t..sum_ind;
     merge &t..sum_ind &t..contribution_d;
     by dummyby;
run;

data &t..combo_contrib;

     array I_S(&nvar.) %do k = 1 %to &nvar.; ind_&&var&k.. %end;;
     array GVG(&nvar.,&nvar.) GVG1-GVG&nvar2.;
     array qqcontrib(&nvar.) %do i = 1 %to &nvar.; &contribution_prefix._&&var&i.. %end;;

     length combo_name $ 240;

     set &t..sum_ind;
      
     combo_name = ' ';
     
%do k = 1 %to &nvar.;
     if ind_&&var&k.. = 1 then combo_name = trim(left(combo_name)) || ' ' || "&&var&k..";
%end;
     combo_name = trim(left(combo_name));

     combo_contribution = 0;
     quadform = 0;
     do i = 1 to &nvar.;
         combo_contribution = combo_contribution + I_S(i) * qqcontrib(i);
         row_sum = 0;
         do j = 1 to &nvar.;
             row_sum = row_sum + GVG(i,j) * I_S(j);
             end;
         quadform = quadform + I_S(i) * row_sum;
         end;
      
      SE_combo_contribution = sqrt(quadform);

        label combo_name = Combination of Variables;
        label combo_contribution = Proportional Contribution to Risk Score Variance of Combination;
        label SE_combo_contribution = Standard Error of Proportional Contribution to Risk Score Variance of Combination;
      
      keep &combo_id. combo_name combo_contribution SE_combo_contribution;  
run;     

%end;

%end;


/** If macro parameters were set, capture the individual standardized log hazard ratio estimates and their covariance matrix **/

%if %length(&zb_prefix.) %then %do;
data &t..zb;
     array &zb_prefix.(&nvar.)  &zb_prefix.1-&zb_prefix.&nvar.;
     set &t..b_std end=eof;
     i + 1;
     &zb_prefix.(i) = b_std;
     if eof then output;
     retain &zb_prefix.1-&zb_prefix.&nvar.;
     keep &zb_prefix.1-&zb_prefix.&nvar.;
run;

data &t..stdlnhr;
     merge &t..stdlnhr &t..zb;
run;
%end;

%if %length(&zv_prefix.) %then %do;
data &t..zv;
     array COL(&nvar.) COL1-COL&nvar.;
     array &zv_prefix.(&nvar.,&nvar.)  &zv_prefix.1-&zv_prefix.&nvar2.;
     set &t..v_std end=eof;
     i + 1;
     do j = 1 to &nvar.;
         &zv_prefix.(i,j) = COL(j);
         end;
     if eof then output;
     retain &zv_prefix.1-&zv_prefix.&nvar2.;
     keep &zv_prefix.1-&zv_prefix.&nvar2.;
run;

data &t..stdlnhr;
     merge &t..stdlnhr &t..zv;
run;
%end;

%if %length(&contribution_prefix.) %then %do;

data &t..stdlnhr;
     merge &t..stdlnhr &t..contribution &t..se_contribution;
run;

%end;

data &t..stdlnhr;
     set &t..stdlnhr;
     imlby = &imlby;
run;

%if %length(&var_combo_indsn.) %then %do;

data &t..combo_contrib;
     set &t..combo_contrib;
     imlby = &imlby;
run;

%end;

%end;

%if &imlby. = 1 %then %do;

data &t..stdlnhrall;
    set &t..stdlnhr;
run;

%if %length(&var_combo_indsn.) %then %do;

data &t..combo_contriball;
     set &t..combo_contrib;
run;

%end;

%end;

%else %do;

data &t..stdlnhrall;
    set &t..stdlnhrall &t..stdlnhr;
run;

%if %length(&var_combo_indsn.) %then %do;

data &t..combo_contriball;
     set &t..combo_contriball &t..combo_contrib;
run;

%end;

%end;

%end;

/* Merge the by variables back in */

data &t..stdlnhrall;
    merge &t..stdlnhrall &t..byvar;
    by imlby;
run; 

/***********************************************************************************
**  Build output data set for contributions of groups of variables.                *
***********************************************************************************/

%if %length(&var_combo_outdsn.) %then %do;

data &var_combo_outdsn.;
    merge &t..combo_contriball &t..byvar;
    by imlby;
    drop imlby %if &byflag. = 0 %then dummyby;;    
run;

%end;


/***********************************************************************************
**  Build main output data set.                                                    *
***********************************************************************************/

%if %length(&partial.) %then %let lpartial = Partial;
                       %else %let lpartial = ; 

data &t..outdsn;
     merge &t..stdlnhrall &t..colflag;
     by imlby;

if colflag = 0 then do;

/*  Absolute standardized log hazard ratio estimate */


     &abs_std_log_HR. = sqrt(stdlnhr2);
        label &abs_std_log_HR. = &lpartial. Absolute Standardized Log Hazard Ratio Estimate;

/*  Standard error of standardized absolute log hazard ratio estimate */
    
     &var_std_log_HR. = trzv;
         label &var_std_log_HR. = Variance of the &lpartial. Standardized Log Hazard Ratio Estimate;

/*  Bias-corrected estimate of the absolute standardized log hazard ratio */

     &abs_std_log_HR_correct. = sqrt(max(stdlnhr2 - trzv,0));
        label &abs_std_log_HR_correct. = Bias-Corrected &lpartial. Absolute Standardized Log Hazard Ratio;

/*  Chi-square statistic, degrees of freedom, and p-value for point null hypothesis */


        label &chi_sq. = Chi-square Statistic for Test That &lpartial. Absolute Standardized Log Hazard Ratio Equals 0;

     &df. = &nvar.;
        label &df. = Degrees of Freedom;

     &p_value. = 1 - CDF('CHISQ',&chi_sq.,&nvar.); 
        label &p_value. = P-value for Test That &lpartial. Absolute Standardized Log Hazard Ratio Equals 0; 

%if %length(&intvl_hyp.) %then %do;
     label &p_value_int. = P-value for Interval Null Hypothesis Test That &lpartial. ASLHR <= &intvl_hyp.;
     label &p_value_int_Scheffe. = Scheffe-Alignment P-value for Interval Null Hypothesis Test That &lpartial. ASLHR <= &intvl_hyp.;
%end;

     &min_eigenvalue. = min_eigenvalue_SG;
        label &min_eigenvalue. = Minimum eigenvalue of sqrt(SigmaZ)-transpose * V * sqrt(SigmaZ);

     &noncentrality_Scheffe. = noncentrality_Scheffe;
        label &noncentrality_Scheffe. = Scheffe-alignment noncentrality parameter;
        
%if %length(&contribution_prefix.) %then %do i = 1 %to &nvar.; 
        label &contribution_prefix._&&var&i.. = Proportional Contribution of &&var&i.. to the &lpartial. Risk Score Variance;        
        label se_&contribution_prefix._&&var&i.. = Standard Error of Proportional Contribution of &&var&i.. to the &lpartial. Risk Score Variance;
%end;

/*  If confidence interval requested by setting macro parameter alpha, compute it by interval halving */

%if %length(&alpha.) %then %let conf = %sysevalf(100*(1-&alpha.),integer);     
%else %let conf = NA;

%if %length(&alpha.) %then %do;

/*  Lower limit */

     alpha2comp = 1 - &alpha./2;
     step = min_eigenvalue_SG;
     eta2plus = 0;
     totstep = 0;
     do while (step > 10**(-12) and totstep <= 1000);
        flag = 0;
        do while (flag=0 and totstep <= 1000);
          eta2plus = eta2plus + step;
          nc = eta2plus / min_eigenvalue_SG;            
          if &chi_sq. <= CINV(alpha2comp,&nvar.,nc) then flag = 1;
          totstep = totstep + 1;
          end;
        eta2plus = eta2plus - step;
        step = step / 2;
        end;

      &abs_std_log_HR_LCL. = sqrt(eta2plus);
         label &abs_std_log_HR_LCL. = "Lower Limit of Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Log Hazard Ratio";

/*  Upper limit */

     alpha2 = &alpha. / 2;
     step = max_eigenvalue_SG;
     eta2minus = 0;
     totstep = 0;
     do while (step > 10**(-12) and totstep <= 1000);
        flag = 0;
        do while (flag=0 and totstep <= 1000);
          eta2minus = eta2minus + step;
          nc = eta2minus / max_eigenvalue_SG;            
          if &chi_sq. < CINV(alpha2,&nvar.,nc) then flag = 1;
          totstep = totstep + 1;
          end;
        eta2minus = eta2minus - step;
        step = step / 2;
        end;

      &abs_std_log_HR_UCL. = sqrt(eta2minus);
         label &abs_std_log_HR_UCL. = "Upper Limit of Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Log Hazard Ratio";


/***********************************************************************************/
/** Compute confidence interval using Scheffe-alignment method.                    */
/***********************************************************************************/

/*  Lower limit */

     alpha2comp = 1 - &alpha./2;
     step = stdlnhr2 / 2;
     eta2plus = 0;
     totstep = 0;
     do while (step > 10**(-12) and totstep <= 1000);
        flag = 0;
        do while (flag=0 and totstep <= 1000);
          eta2plus = eta2plus + step;
          nc = noncentrality_Scheffe * eta2plus / stdlnhr2;            
          if &chi_sq. <= CINV(alpha2comp,&nvar.,nc) then flag = 1;
          totstep = totstep + 1;
          end;
        eta2plus = eta2plus - step;
        step = step / 2;
        end;

      &abs_std_log_HR_LCL_Scheffe. = sqrt(eta2plus);
         label &abs_std_log_HR_LCL_Scheffe. = "Lower Limit of Scheffe &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Log Hazard Ratio";

/*  Upper limit */

     alpha2 = &alpha. / 2;
     step = 2 * stdlnhr2;
     eta2minus = 0;
     totstep = 0;
     do while (step > 10**(-12) and totstep <= 1000);
        flag = 0;
        do while (flag=0 and totstep <= 1000);
          eta2minus = eta2minus + step;
          nc = noncentrality_Scheffe_min * eta2minus / stdlnhr2;            
          if &chi_sq. < CINV(alpha2,&nvar.,nc) then flag = 1;
          totstep = totstep + 1;
          end;
        eta2minus = eta2minus - step;
        step = step / 2;
        end;

      &abs_std_log_HR_UCL_Scheffe. = sqrt(eta2minus);
         label &abs_std_log_HR_UCL_Scheffe. = "Upper Limit of Scheffe &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Log Hazard Ratio";

%end;  

    label &maxVIF. = Max. variance inflation factor from assessment of covariate collinearity (must be < 100);
    
    &Prop_Var_Expl. =  stdlnhr2 / (stdlnhr2 + (3.141592654**2) / 6);
      label &Prop_Var_Expl. = Proportion Variance Explained (Kent-OQuigley);

end;


/* If covariate collinearity detected, do not report standardized hazard ratio */

    if colflag = 1 then do;

put "WARNING:    Collinearity or near collinearity detected";
put "among covariates &vars.";
%if &byflag. = 1 %then put "for by variable value" %do i = 1 %to &nbyvar.; "  &&byvar&i..=" &&byvar&i.. %end;;  
put "Standardized log hazard ratio not calculated.";

         &abs_std_log_HR. = .;
         &var_std_log_HR. = .;
         &abs_std_log_HR_correct. = .;
         &chi_sq. = .;
         &p_value. = .; 
%if %length(&alpha.) %then %do;
         &abs_std_log_HR_LCL. = .;
         &abs_std_log_HR_UCL. = .;
%end;
         &p_value_int = .; 
         &p_value_int_Scheffe = .;
%if %length(&zb_prefix.) %then %do;
%do i = 1 %to &nvar.;  
         &zb_prefix.&i. = .;
%end;
%end;
%if %length(&zv_prefix.) %then %do;
%do i = 1 %to &nvar2.; 
         &zv_prefix.&i. = .;
%end;
%end;
         end;

%if %length(&predictors.) %then %do;
     length &predictors. $ 240;
     &predictors. = "&vars.";
%end;


/***********************************************************************************/
/** Compute absolute standardized hazard ratios and associated confidence         **/
/** confidence intervals.                                                         **/
/***********************************************************************************/

     bc_ratio = &abs_std_log_HR_correct. / &abs_std_log_HR.;

     &abs_std_HR. = exp(&abs_std_log_HR.);
     &abs_std_HR_correct. = exp(&abs_std_log_HR_correct.);

        label &abs_std_HR. = &lpartial. Absolute Standardized Hazard Ratio Estimate;
        label &abs_std_HR_correct. = Bias-Corrected &lpartial. Absolute Standardized Hazard Ratio;

%if %length(&alpha.) %then %do;

     &abs_std_HR_LCL. = exp(&abs_std_log_HR_LCL.); 
     &abs_std_HR_UCL. = exp(&abs_std_log_HR_UCL.); 
     &abs_std_HR_LCL_Scheffe. = exp(&abs_std_log_HR_LCL_Scheffe.); 
     &abs_std_HR_UCL_Scheffe. = exp(&abs_std_log_HR_UCL_Scheffe.);    
     &abs_std_HR_correct_LCL. = exp(&abs_std_log_HR_LCL. * bc_ratio); 
     &abs_std_HR_correct_UCL. = exp(&abs_std_log_HR_UCL. * bc_ratio); 
     &abs_std_HR_correct_LCL_Scheffe. = exp(&abs_std_log_HR_LCL_Scheffe. * bc_ratio); 
     &abs_std_HR_correct_UCL_Scheffe. = exp(&abs_std_log_HR_UCL_Scheffe. * bc_ratio); 

        label &abs_std_HR_LCL. = "Lower Limit of Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Hazard Ratio";
        label &abs_std_HR_UCL. = "Upper Limit of Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Hazard Ratio";
        label &abs_std_HR_LCL_Scheffe. = "Lower Limit of Scheffe-Alignment &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Hazard Ratio";
        label &abs_std_HR_UCL_Scheffe. = "Upper Limit of Scheffe-Alignment &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Hazard Ratio";
        label &abs_std_HR_correct_LCL. = "Lower Limit of Bias-Corrected Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Hazard Ratio";
        label &abs_std_HR_correct_UCL. = "Upper Limit of Bias-Corrected Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Hazard Ratio";
        label &abs_std_HR_correct_LCL_Scheffe. = "Lower Limit of Bias-Corrected Scheffe-Alignment &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Hazard Ratio";
        label &abs_std_HR_correct_UCL_Scheffe. = "Upper Limit of Bias-Corrected Scheffe-Alignment &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Hazard Ratio";

%end;
    
     keep &abs_std_log_HR. &var_std_log_HR. &abs_std_log_HR_correct. 
          &abs_std_HR. &abs_std_HR_correct. 
          &chi_sq. &df. &p_value. 
          %if %length(&intvl_hyp.) %then &p_value_int. &p_value_int_Scheffe.;
          &maxVIF. &min_eigenvalue. &noncentrality_Scheffe.
          %if %length(&alpha.) %then %do; 
                         &abs_std_log_HR_LCL. &abs_std_log_HR_UCL. 
                         &abs_std_log_HR_LCL_Scheffe. &abs_std_log_HR_UCL_Scheffe.
                         &abs_std_HR_LCL. &abs_std_HR_UCL.
                         &abs_std_HR_LCL_Scheffe. &abs_std_HR_UCL_Scheffe.
                         &abs_std_HR_correct_LCL. &abs_std_HR_correct_UCL.
                         &abs_std_HR_correct_LCL_Scheffe. &abs_std_HR_correct_UCL_Scheffe. 
          %end;
          %if %length(&zb_prefix.) %then &zb_prefix.1-&zb_prefix.&nvar.;
          %if %length(&zv_prefix.) %then &zv_prefix.1-&zv_prefix.&nvar2.;
          &byvar.
          %if %length(&predictors.) %then &predictors.;
          %if %length(&contribution_prefix.) %then %do i = 1 %to &nvar.; &contribution_prefix._&&var&i.. se_&contribution_prefix._&&var&i.. %end;
          &maxVIF. &Prop_Var_Expl.;
run;


proc sql;
     create table &outdsn. as select
          %if &byflag. = 1 %then %do i = 1 %to &nbyvar.; &&byvar&i.., %end;
          %if %length(&predictors.) %then &predictors.,;
          &abs_std_log_HR.,&var_std_log_HR.,&abs_std_log_HR_correct.,
          &abs_std_HR.,&abs_std_HR_correct.,
          &chi_sq.,&df.,&p_value.,
          %if %length(&intvl_hyp.) %then &p_value_int.,&p_value_int_Scheffe.,;
          &min_eigenvalue.,&noncentrality_Scheffe.,
          %if %length(&alpha.) %then %do;
                         &abs_std_log_HR_LCL.,&abs_std_log_HR_UCL.,
                         &abs_std_log_HR_LCL_Scheffe.,&abs_std_log_HR_UCL_Scheffe.,
                         &abs_std_HR_LCL.,&abs_std_HR_UCL.,
                         &abs_std_HR_LCL_Scheffe.,&abs_std_HR_UCL_Scheffe.,
                         &abs_std_HR_correct_LCL.,&abs_std_HR_correct_UCL.,
                         &abs_std_HR_correct_LCL_Scheffe.,&abs_std_HR_correct_UCL_Scheffe.,   
	  %end;
          %if %length(&zb_prefix.) %then %do i = 1 %to &nvar.;  &zb_prefix.&i., %end;
          %if %length(&zv_prefix.) %then %do i = 1 %to &nvar2.; &zv_prefix.&i., %end;
          %if %length(&contribution_prefix.) %then %do i = 1 %to &nvar.; &contribution_prefix._&&var&i.., se_&contribution_prefix._&&var&i.., %end;
          &maxVIF.,&Prop_Var_Expl. 
          from &t..outdsn; 
quit;     

/************************************************************************************
** Clean up data sets and reset mergenoby option to what is was.                    *
************************************************************************************/

proc datasets memtype=data lib=&t. kill nodetails nolist;
run;
quit;

options mergenoby=&mergenobyoption.;

%mend STDLOGHR;
