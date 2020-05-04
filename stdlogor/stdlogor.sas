/***********************************************************************************************************************

Program Name           : STDLOGOR.sas

Path                   : 

Program Language       : SAS

Operating System       : Windows Server

Purpose                : Compute the standardized absolute log odds ratio for a multivariate logistic regression  
                         model of a binary response variable, and associated variability estimates, test statistics 
                         and confidence intervals.                           
                         
Notes                  : 

Status                 : Tested and verified.

Run Dependencies       : None

Input Datasets/Views   : Specified SAS data set indsn containing a binary response variable, covariates for the logistic
                         regression model, and an optional weight variable if cohort sampling was used.

Output Datasets/Views  : Specified SAS data set outdsn containing the standardized absolute log odds ratio and its standard error.                         

Other Output           : None

Macro calls internal   : Tmp_SubLibL

Macro calls external   : 

***********************************************************************************************************************/

%macro STDLOGOR(
        /* Input Specification */  indsn=,byvar=,response=,vars=,weight=,sampstrata=,adjcov=,
                                   var_combo_indsn=,combo_id=,
        /* Analysis Parameters */  partial=,print=,alpha=,intvl_hyp=,
        /* Output Specification */ outdsn=,abs_std_log_OR=abs_std_log_OR,abs_std_OR=abs_std_OR,var_std_log_OR=var_std_log_OR,
                                   abs_std_log_OR_correct=abs_std_log_OR_correct,abs_std_OR_correct=abs_std_OR_correct,                                          
                                   chi_sq=chi_sq,df=df,p_value=p_value,p_value_int=p_value_int, p_value_int_Scheffe=p_value_int_Scheffe, 
                                   min_eigenvalue=min_eigenvalue,noncentrality_Scheffe=noncentrality_Scheffe,
                                   abs_std_log_OR_LCL=abs_std_log_OR_LCL,abs_std_log_OR_UCL=abs_std_log_OR_UCL,
                                   abs_std_log_OR_LCL_Scheffe=abs_std_log_OR_LCL_Scheffe,abs_std_log_OR_UCL_Scheffe=abs_std_log_OR_UCL_Scheffe,
                                   abs_std_OR_LCL=abs_std_OR_LCL,abs_std_OR_UCL=abs_std_OR_UCL,
                                   abs_std_OR_LCL_Scheffe=abs_std_OR_LCL_Scheffe,abs_std_OR_UCL_Scheffe=abs_std_OR_UCL_Scheffe,
                                   abs_std_OR_correct_LCL=abs_std_OR_correct_LCL,
                                   abs_std_OR_correct_UCL=abs_std_OR_correct_UCL,
                                   abs_std_OR_correct_LCL_Scheffe=abs_std_OR_correct_LCL_Scheffe,
                                   abs_std_OR_correct_UCL_Scheffe=abs_std_OR_correct_UCL_Scheffe,
                                   zb_prefix=,zv_prefix=,contribution_prefix=,predictors=,maxVIF=maxVIF,
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
| Name           : response
| Required (Y/N) : Y
| Default Value  :
| Type ($/#)     : # or $
| Purpose        : The dichotomous dependent variable for the logistic regression analysis.
|-----------------------------------------------------------------------------------------------
| Name           : vars
| Required (Y/N) : Y
| Default Value  :
| Type ($/#)     : #
| Purpose        : List of input data set variables containing the logistic regression model covariates 
|                  for which the standardized absolute log odds ratio will be computed.
|-----------------------------------------------------------------------------------------------
| Name           : weight  
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : #
| Purpose        : Input data set variable giving the observation's weight in the analysis. If this parameter
|                  is set, it is assumed that cohort sampling was used and resulted in the specified weights.   
|-----------------------------------------------------------------------------------------------
| Name           : adjcov  
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : Text string giving additional covariates to be included in the logistic regression
|                  model but not included in the standardized odds ratio computation.
|-----------------------------------------------------------------------------------------------
| Name           : sampstrata   
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : # or $
| Purpose        : If a stratified cohort sampling design was used, use this parameter to list the
|                  stratification variables.
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

| Name           : partial   
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : If this parameter is set to a list of variables that is a subset of the set of adjustment covariates
|                  and stratification variables, the partial standardized log odds ratio will be calculated
|                  conditional on the specified stratification variables and adjustment covariates.  If the parameter is not
|                  specified,the marginal standardized log odds ratio will be calculated.             
|-----------------------------------------------------------------------------------------------
| Name           : print   
| Required (Y/N) : N
| Default Value  : yes
| Type ($/#)     : $
| Purpose        : If this parameter is set to no, the noprint option will be invoked when running the
|                  logistic regression procedure.                      
|-----------------------------------------------------------------------------------------------
| Name           : alpha   
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : #
| Purpose        : If this parameter is set, the macro will compute 100(1-alpha)% confidence intervals
|                  for the absolute standardized log odds ratio.                      
|-----------------------------------------------------------------------------------------------
| Name           : intvl_hyp   
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : #
| Purpose        : If this parameter is set, the macro will compute p-values for an interval null hypothesis
|                  that the absolute standardized log odds ratio is less than &intvl_hyp.                      
|-----------------------------------------------------------------------------------------------
| Name           : outdsn  
| Required (Y/N) : Y
| Default Value  : (at temporary library)
| Type ($/#)     : $
| Purpose        : Libname reference and the output data set name.  The dataset 
|                  name must conform to the rules for SAS names.   
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_log_OR  
| Required (Y/N) : N
| Default Value  : abs_std_log_OR
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the estimate of the absolute standardized 
|                  log odds ratio.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_OR  
| Required (Y/N) : N
| Default Value  : abs_std_OR
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the estimate of the absolute standardized 
|                  odds ratio.
|-----------------------------------------------------------------------------------------------
| Name           : var_std_log_OR  
| Required (Y/N) : N
| Default Value  : var_std_log_OR
| Type ($/#)     : #
| Purpose        : Name of output data set variable that will contain the variance error of the estimate of the 
|                  standardized absolute log odds ratio.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_log_OR_correct  
| Required (Y/N) : N
| Default Value  : abs_std_log_OR_correct
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the bias-corrected estimate of the standardized absolute
|                  log odds ratio.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_OR_correct  
| Required (Y/N) : N
| Default Value  : abs_std_OR_correct
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the bias-corrected estimate of the standardized absolute
|                  odds ratio.
|-----------------------------------------------------------------------------------------------
| Name           : chi_sq               
| Required (Y/N) : N
| Default Value  : chi_sq
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain a noncentral chi-square statistic that can be used to
|                  to test hypotheses about the absolute standardized odds ratio.
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
| Purpose        : Name of output data set variable will contain the p-value from a test of the test of hypothesis that the 
|                  absolute standardized log odds ratio is 0.
|-----------------------------------------------------------------------------------------------
| Name           : p_value_int               
| Required (Y/N) : N
| Default Value  : p_value_int
| Type ($/#)     : #
| Purpose        : If the parameter intvl_hyp is specified, the output data set variable named by this parameter will contain 
|                  the p-value from the conservative test of the interval null hypothesis that the absolute standardized log odds ratio 
|                  is less than or equal to &intvl_hyp.
|-----------------------------------------------------------------------------------------------
| Name           : p_value_int_Scheffe               
| Required (Y/N) : N
| Default Value  : p_value_int_Scheffe
| Type ($/#)     : #
| Purpose        : If the parameter intvl_hyp is specified, the output data set variable named by this parameter will contain 
|                  the p-value from the Scheffe-alignment test of the interval null hypothesis that the absolute standardized log odds ratio 
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
|                  standardized odds ratio.
|-----------------------------------------------------------------------------------------------
| Name           : noncentrality_Scheffe               
| Required (Y/N) : N
| Default Value  : noncentrality_Scheffe
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the maximum noncentrality parameter consistent with a 95%
|                  Scheffe confidence ellipsoid about the transformed parameter estimate vector.  This value can be used to
|                  to construct tests of interval null hypotheses about the absolute standardized odds ratio.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_log_OR_LCL               
| Name           : abs_std_log_OR_LCL               
| Required (Y/N) : N
| Default Value  : abs_std_log_OR_LCL
| Type ($/#)     : #
| Purpose        : Lower limit of the 100(1-alpha)% confidence interval for the absolute standardized log odds ratio.  Computed
|                  only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_log_OR_UCL               
| Required (Y/N) : N
| Default Value  : abs_std_log_OR_UCL
| Type ($/#)     : #
| Purpose        : Upper limit of the 100(1-alpha)% confidence interval for the absolute standardized log odds ratio using
|                  the conservative method.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_log_OR_LCL_Scheffe               
| Required (Y/N) : N
| Default Value  : abs_std_log_OR_LCL_Scheffe
| Type ($/#)     : #
| Purpose        : Lower limit of the 100(1-alpha)% confidence interval for the absolute standardized log odds ratio computed
|                  using Scheffe alignment.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_log_OR_UCL_Scheffe               
| Required (Y/N) : N
| Default Value  : abs_std_log_OR_UCL_Scheffe
| Type ($/#)     : #
| Purpose        : Upper limit of the 100(1-alpha)% confidence interval for the absolute standardized log odds ratio computed
|                  using Scheffe alignement.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_OR_correct_LCL               
| Required (Y/N) : N
| Default Value  : abs_std_OR_correct_LCL
| Type ($/#)     : #
| Purpose        : Lower limit of the bias-corrected 100(1-alpha)% confidence interval for the absolute standardized odds ratio using
|                  the conservative method.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_OR_correct_UCL               
| Required (Y/N) : N
| Default Value  : abs_std_OR_correct_UCL
| Type ($/#)     : #
| Purpose        : Upper limit of the bias-corrected 100(1-alpha)% confidence interval for the absolute standardized odds ratio using
|                  the conservative method.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_OR_correct_LCL_Scheffe               
| Required (Y/N) : N
| Default Value  : abs_std_OR_correct_LCL_Scheffe
| Type ($/#)     : #
| Purpose        : Lower limit of the bias-corrected 100(1-alpha)% confidence interval for the absolute standardized odds ratio computed
|                  using Scheffe alignment.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : abs_std_OR_correct_UCL_Scheffe               
| Required (Y/N) : N
| Default Value  : abs_std_OR_correct_UCL_Scheffe
| Type ($/#)     : #
| Purpose        : Upper limit of the bias-corrected 100(1-alpha)% confidence interval for the absolute standardized odds ratio computed
|                  using Scheffe alignement.  Computed only if the macro parameter alpha is specified.
|-----------------------------------------------------------------------------------------------
| Name           : zb_prefix               
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : If this parameter is specified, the vector of standardized log odds ratios for the individual
|                  covariates will be stored in variables with the specified prefix and suffixes from 1 to the number of variables.
|                  The order of these variables will correspond to the order of the covariates specified in the macro parameter
|                  vars.
|-----------------------------------------------------------------------------------------------
| Name           : zv_prefix               
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : If this parameter is specified, the covariance matrix of the standardized log odds ratio estimates for the 
|                  individual covariates will be stored in variables with the specified prefix and suffixes from 1 to the square of 
|                  the number of variables.  The order of these variables will correspond to the order of the covariates specified 
|                  in the macro parameter vars: v11, v12,..., v1p, v21, v22,..., v2p,...,vp1, vp2,..., vpp.
|-----------------------------------------------------------------------------------------------
| Name           : contribution_prefix               
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : If this parameter is specified, the vector consisting of the proportional contribution of each covariate to 
|                  the total squared standardized log odds ratios will be stored in variables with the specified prefix and 
|                  suffixes consisting of the covariate names.
|-----------------------------------------------------------------------------------------------
| Name           : predictors              
| Required (Y/N) : N
| Default Value  : 
| Type ($/#)     : $
| Purpose        : If this parameter is specified, a variable with this name will be included in the output data set and contain
|                  the list of variables represented by the standardized log odds ratio.                                           
|-----------------------------------------------------------------------------------------------
| Name           : maxVIF               
| Required (Y/N) : N
| Default Value  : maxVIF
| Type ($/#)     : #
| Purpose        : Name of output data set variable will contain the maximum variance inflation factor (VIF) over all the covariates
|                  from the screen for multicollinearity.  If the maximum VIF is greater than 10, then no standardized log odds ratio
|                  will be returned.
|-----------------------------------------------------------------------------------------------
| Name           : var_combo_outdsn               
| Required (Y/N) : Y if parameter var_combo_indsn specified.
| Default Value  : 
| Type ($/#)     : $
| Purpose        : Name of output data set that will contain the estimated contributions of specified combinations of variables together with
|                  the standard error of each estimate.                         
|-----------------------------------------------------------------------------------------------
**********************************************************************************************************/
 
%local mergenobyoption t nvar nvar2 nadjcov nadjcov2 npartial npadjcov npadjcov2 ncovpadjcov i j nimlby nbyvar byflag skipflag lpartial;


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

   %let t = _STLNOR;
   

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
      %put ERROR : STDLOGOR macro parameter indsn must be specified.;
      %abort;
   %end;

   %if %length(&vars.)=0 %then %do;
      %put ERROR : STDLOGOR macro parameter vars must be specified.;
      %abort;
   %end;

   %if %length(&response.)=0 %then %do;
      %put ERROR : STDLOGOR macro parameter response must be specified.;
      %abort;
   %end;

   %if %length(&outdsn.)=0 %then %do;
      %put ERROR : STDLOGOR macro parameter outdsn must be specified.;
      %abort;
   %end;

   %if %length(&alpha.) %then %do;
   %if %sysevalf(&alpha. <= 0) or %sysevalf(&alpha. >= 1) %then %do;
      %put ERROR : STDLOGOR macro parameter alpha must be >0 and <1.;
      %abort;
   %end;
   %end; 

   %if %length(&var_combo_indsn.) > 0 and %length(&contribution_prefix.) = 0 %then %do;
      %put ERROR : STDLOGOR macro found parameter var_combo_indsn set but parameter contribution_prefix not set.;
      %abort;
      %end;   
  
/*  Set default values of parameters not specified */

%if %length(&maxVIF.) = 0 %then %let maxVIF = maxVIF;
%let npadjcov = 0;
%if %length(&abs_std_log_OR.) = 0 %then %let abs_std_log_OR = abs_std_log_OR;
%if %length(&abs_std_log_OR_correct.) = 0 %then %let abs_std_log_OR_correct = abs_std_log_OR_correct;
%if %length(&abs_std_OR_correct.) = 0 %then %let abs_std_OR_correct = abs_std_OR_correct;
%if %length(&chi_sq.) = 0 %then %let chi_sq = chi_sq;
%if %length(&df.) = 0 %then %let df = df;
%if %length(&p_value.) = 0 %then %let p_value = p_value;
%if %length(&p_value_int.) = 0 %then %let p_value_int = p_value_int;
%if %length(&p_value_int_Scheffe.) = 0 %then %let p_value_int_Scheffe = p_value_int_Scheffe;
%if %length(&min_eigenvalue.) = 0 %then %let min_eigenvalue = min_eigenvalue;
%if %length(&noncentrality_Scheffe.) = 0 %then %let noncentrality_Scheffe = noncentrality_Scheffe;
%if %length(&abs_std_log_OR_LCL.) = 0 %then %let abs_std_log_OR_LCL = abs_std_log_OR_LCL;
%if %length(&abs_std_log_OR_UCL.) = 0 %then %let abs_std_log_OR_UCL = abs_std_log_OR_UCL;
%if %length(&abs_std_log_OR_LCL_Scheffe.) = 0 %then %let abs_std_log_OR_LCL_Scheffe = abs_std_log_OR_LCL_Scheffe;
%if %length(&abs_std_log_OR_UCL_Scheffe.) = 0 %then %let abs_std_log_OR_UCL_Scheffe = abs_std_log_OR_UCL_Scheffe;
%if %length(&abs_std_OR_LCL.) = 0 %then %let abs_std_OR_LCL = abs_std_OR_LCL;                                      
%if %length(&abs_std_OR_UCL.) = 0 %then %let abs_std_OR_UCL = abs_std_OR_UCL;   
%if %length(&abs_std_OR_LCL_Scheffe.) = 0 %then %let abs_std_OR_LCL_Scheffe = abs_std_OR_LCL_Scheffe; 
%if %length(&abs_std_OR_UCL_Scheffe.) = 0 %then %let abs_std_OR_UCL_Scheffe = abs_std_OR_UCL_Scheffe; 
%if %length(&abs_std_OR_correct_LCL.) = 0 %then %let abs_std_OR_correct_LCL = abs_std_OR_correct_LCL; 
%if %length(&abs_std_OR_correct_UCL.) = 0 %then %let abs_std_OR_correct_UCL = abs_std_OR_correct_UCL; 
%if %length(&abs_std_OR_correct_LCL.) = 0 %then %let abs_std_OR_correct_LCL = abs_std_OR_correct_LCL; 
%if %length(&abs_std_OR_correct_UCL.) = 0 %then %let abs_std_OR_correct_UCL = abs_std_OR_correct_UCL;
%if %length(&abs_std_OR_correct_LCL_Scheffe.) = 0 %then %let abs_std_OR_correct_LCL_Scheffe = abs_std_OR_correct_LCL_Scheffe; 
%if %length(&abs_std_OR_correct_UCL_Scheffe.) = 0 %then %let abs_std_OR_correct_UCL_Scheffe = abs_std_OR_correct_UCL_Scheffe; 

/**********************************************************************************************
**  Parse the string containing the "by variable".                                            *
**********************************************************************************************/

%if %length(&byvar.) %then %do;
   %let byflag = 1;
   %let byvar=%sysfunc(compbl(&byvar));
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

%local npartial pvar npadjcov;
%let npadcov = 0;

%do i = 1 %to &npartial.;
   %let pvar = %scan(&partial., &i., %str( ));
   %if %index(&adjcov.,&pvar.) > 0 %then %do;
      %let npadjcov = %sysevalf(&npadjcov.+1,integer);
      %local padjcov&npadjcov.;
      %let padjcov&npadjcov. = &pvar.;
      %end;
   %else %do;
      %put ERROR : STDLOGOR macro parameter found specified partial variable &pvar..;
      %put ERROR : This variable is not specified as an adjustment covariate.;
      %if %length(&adjcov.) %then %put ERROR : Adjustment covariates:  &adjcov..;
      %abort;
      %end;        
%end; 

%let npadjcov2 = %sysevalf(&npadjcov.**2,integer); 

%let ncovpadjcov = %sysevalf(&nvar.*&npadjcov.,integer);

%end;

/**********************************************************************************************
**  Parse the input parameter strata containing the list of sampling stratification variables,*
**  determine the number of these variables, and assign them to the macro variables strat1,   *
**  strat2, etc.                                                                              *
**********************************************************************************************/

%if %length(&sampstrata.) %then %do;

%let sampstrata = %sysfunc(compbl(&sampstrata.));

%let nstrat = %sysfunc(countc(%bquote(&sampstrata.), %str( )));
%let nstrat = %sysfunc(ifc(%length(%bquote(&sampstrata.)), %eval(&nstrat. + 1), 0));

%local nstrat;   

%do i = 1 %to &nstrat.;
   %local strat&i.;
   %let strat&i = %scan(&sampstrata., &i., %str( ));
%end;   

%end;

/**********************************************************************************************
**  Check for nonexistent input data sets and nonexistent variables in the input data sets.   *
**********************************************************************************************/

  %let errcode = 0;
  %let dsid = %sysfunc(open(&indsn.,i));
  %if &dsid. = 0 %then %do;
     %put ERROR : STDLOOHR macro found that specified input data set &indsn. does not exist.;
     %abort;
     %end;
   %else %do;
     %do k = 1 %to &nvar.;
          %if %sysfunc(varnum(&dsid.,&&var&k..)) = 0 %then %do;
            %put ERROR : STDLOGOR macro input data set &indsn. does not contain the variable &&var&k.. specified in parameter vars.;
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
            %put ERROR : STDLOOHR macro input data set &indsn. does not contain the variable &&adjcov&k.. specified in macro parameter adjcov.;
            %let errcode = 1;
            %end;
         %end;
     %if %length(&sampstrata.) %then %do k = 1 %to &nstrat.;
         %if %sysfunc(varnum(&dsid.,&&strat&k..)) = 0 %then %do;
            %put ERROR : STDLOGHR macro input data set &indsn. does not contain variable &&strat&k.. specified in macro parameter strata.;
            %let errcode = 1;
            %end;
         %end;
     %if %sysfunc(varnum(&dsid.,&response.)) = 0 %then %do;
            %put ERROR : STDLOGOR macro input data set &indsn. does not contain the variable &response specified in parameter response.;
            %let errcode = 1;
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
     if &response. = . %do i = 1 %to &nvar.; or &&var&i.. = . %end; 
        %if %length(&adjcov.) %then %do i = 1 %to &nadjcov.; or &&adjcov&i.. = . %end; then delete;

/* Create weight variable that defaults to 1 if no weights are specified */

%if %length(&weight.)> 0 %then %do; qqweight = &weight.; %end;
                         %else %do; qqweight = 1; %end;

/* Delete any observation that has 0 weight */
     if qqweight = 0 then delete;     
run;

proc sort data=&t..indata;
      by &byvar.;
run;

/**********************************************************************************************
**  Check to make sure the response variable is binary.                                       *
**********************************************************************************************/

proc sql noprint;
     select count (distinct &response.) into :nresponse from &t..indata;
quit;

%if &nresponse. > 2 %then %do;
     %put ERROR : STDLOGOR macro response variable has more than two values.;
     %abort;
%end;

/*********************************************************************************************/
/** Check for multicollinearity in the covariates.  If collinearity or near collinearity is **/
/** found, do not do the standardized log odds ratio calculation, as results will be        **/
/** unreliable.  The criterion for collinearity is a variance inflation factor (VIF) >= 10. **/
/*********************************************************************************************/

ods output parameterestimates=&t..parmest;
ods listing close;  /* Turn off the listing for proc reg */

proc reg data=&t..indata;
     by &byvar.;
     weight qqweight;
     model dummyby = &vars. / vif tol;
run;

ods output close;
ods listing;  /* Turn the listing back on in case the user wants printed output from PROC   */
              /* LOGISTIC or SURVEYLOGISTIC below. */

/**********************************************************************************************/
/**  Create a data set with a flag to indicate if collinearity was found.                    **/
/**********************************************************************************************/

%let gocol = 0;

data &t..colflag;
     set &t..parmest;
     by &byvar.;
     if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; then do; 
         colflag = 0;
         &maxVIF. = .;
         end;

     if VarianceInflation >= 100 then colflag = 1;
     &maxVIF. = max(&maxVIF.,VarianceInflation);         

     if last.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or last.&&byvar&i.. %end; then do;
          imlby = imlby + 1;
          if colflag = 0 then call symput('gocol','1');
          if colflag = 1 then do;
              put "WARNING:   Collinearity or near collinearity detected";
              put "among covariates &vars.";
%if &byflag. = 1 %then put "for by &byvar. = " &byvar.;;
              put "Standardized log odds ratio not calculated.";
              end;
          output;
          end;

     retain colflag imlby 0 &maxVIF.;
     keep colflag &maxVIF. imlby &byvar.;
run;

%if &gocol. = 1 %then %do;

/**********************************************************************************************
**  Run the logistic  regression analysis.  If a weight is specified, incorporate it in the  **
**  analysis and use the jackknife estimate from proc surveylogistic.                        **
**********************************************************************************************/

%if %length(&weight.) = 0 %then %do;
/**********************************************************************************************
**  Case 1: no cohort sampling.                                                              **
**********************************************************************************************/

ods output ParameterEstimates = &t..outest covB = &t..covB;
%if &print. = no %then ods listing close;;

proc logistic data=&t..indata NAMELEN=30;
     by &byvar.;;
     model &response. = %do i = 1 %to &nvar.; &&var&i.. %end; &adjcov. / covb;   
run;

ods output close;
%if &print. = no %then ods listing;;

%end;

%else %do;
/**********************************************************************************************
**  Case 2: Cohort sampling design.                                                          **
**********************************************************************************************/

ods output ParameterEstimates = &t..outest covB = &t..covB;
%if &print. = no %then ods listing close;;

proc surveylogistic data=&t..indata NAMELEN=30;
     by &byvar.;;
     weight qqweight;
     model &response. = %do i = 1 %to &nvar.; &&var&i.. %end; &adjcov. / covb;    
%if %length(&sampstrata.) > 0 %then strata &sampstrata.;;  
run;

ods output close;
%if &print. = no %then ods listing;;

%end;

/**********************************************************************************************
**  Capture the variance-covariance matrix of the parameter estimates.                        *
**********************************************************************************************/

data &t..v;
      array covariate(&nvar.) %do i = 1 %to &nvar.; &&var&i.. %end;; 
      array v(&nvar.,&nvar.) v1-v&nvar2.;
 
      set &t..covB end=eof;
      by &byvar.;;
      where upcase(Parameter) ne 'INTERCEPT';

/* Initialize cov matrix to missing values */
 
      if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; 
        then do i = 1 to &nvar.;
               do j = 1 to &nvar.;
                  v(i,j) = .;
                  end;
               end;

%do i = 1 %to &nvar.;
      if upcase(Parameter) = upcase("&&var&i..") then row = &i;
%end;
      
      if row ne . then do column = 1 to &nvar.;
          v(row,column) = covariate(column);
          end;

     if last.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or last.&&byvar&i.. %end; then output;

     retain v1-v&nvar2.;   
     keep v1-v&nvar2. &byvar.;;
run;


/**********************************************************************************************
**  Capture the regression parameter estimates.                                              **
**********************************************************************************************/

data &t..b;

      array b(&nvar.) b1-b&nvar.;

      set &t..outest end=eof;
      by &byvar.;;

      if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; 
        then do i = 1 to &nvar.;
               b(i) = .;
               end;

%do i = 1 %to &nvar.;
      if Variable = "&&var&i.." then b&i. = Estimate;
%end;

      if last.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or last.&&byvar&i.. %end; 
        then output;

      retain b1-b&nvar.;
      keep b1-b&nvar. &byvar.;
run;

/***********************************************************************************************
**  Calculate the (weighted) sample mean covariate vector                                     **
***********************************************************************************************/

proc univariate data=&t..indata noprint;
      by &byvar.;
      weight qqweight;
      var %do i = 1 %to &nvar.; &&var&i.. %end;;
      output out = &t..mean mean =  mz1-mz&nvar.;
run;

/***********************************************************************************************
**  Calculate the (weighted) sample mean adjustment covariate vector                          **
***********************************************************************************************/

%if &npadjcov. > 0 %then %do;

proc univariate data=&t..indata noprint;
      by &byvar.;
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
     by &byvar.;

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
**  for the partial ASLOR.                                                                    **
***********************************************************************************************/

%if &npadjcov. > 0 %then %do;

data &t..zzpadjcov;
     array padjcovariate(&npadjcov.) %do i = 1 %to &npadjcov.; &&padjcov&i.. %end;; 
     array mzpadjcov(&npadjcov.) mzpadjcov1-mzpadjcov&npadjcov.;
     array zzpadjcov(&npadjcov.,&npadjcov.) zzpadjcov1-zzpadjcov&npadjcov2.;

     merge &t..indata &t..meanpadjcov;
     by &byvar.;

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
**  Calculate the sample covariance matrix of the SLOR covariates with the partial adjustment **
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
     by &byvar.;

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
     merge &t..v &t..b &t..mean &t..n &t..zz
     %if &npadjcov. > 0 %then &t..zzpadjcov &t..zzcovpadjcov;;
     by &byvar.;
     keep mz1-mz&nvar. b1-b&nvar. v1-v&nvar2. zz1-zz&nvar2.
     %if &npadjcov. > 0 %then zzpadjcov1-zzpadjcov&npadjcov2. zzcovpadjcov1-zzcovpadjcov&ncovpadjcov.; 
     W &byvar.;
run;

/*********************************************************************************************/
/**  Check for any missing values for the parameter estimates or covariance matrix elements,**/
/**  which indicate that the logistic regression model could not be fit as specified.  If   **/
/**  missing values are found, alert user and do not do the standardized log odds ratio     **/
/**  calculation.                                                                           **/
/*********************************************************************************************/

%let go = 0;
data &t..keys;
     set &t..keys end=eof;
     missflag = 0;
%do i = 1 %to &nvar.;
     if b&i. = . then missflag = 1;
%end;
%do i = 1 %to &nvar2.; 
     if v&i. = . then missflag = 1;
%end;

     if missflag = 1 then do;
        put "WARNING:   Full logistic regression model could not be fit for";
        put "covariates &vars.";
%if &byflag. = 1 %then put "for &byvar. = " &byvar.;;
        put "Standardized log odds ratio not calculated.";
        end;

     if missflag = 0 then do;
          call symput('go','1');
          output;
          end;
run;     

%if &go. = 1 %then %do; 

/***********************************************************************************************
**  Estimate the square of the standardized log odds ratio, with and without correction       **
**  for bias, and compute the chi-square test statistic for the null hypothesis that the      **
**  standardized odds ratio is 0.                                                             **                                                                      **
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

%if &skipflag. = 0 %then %do;

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

/*  Partial covariance matrix, if partial ASLOR requested */

      SIGMAZ = SIGMAZ - SIGMAZPADJCOVCOV` * INV(SIGMAZPADJCOV) * SIGMAZPADJCOVCOV;
%end;

/*  Squared standardized log odds ratio estimate */

      stdlnor2 = BETA` * SIGMAZ * BETA;

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
     
/*  Vector of proportion contribution of each variable to the squared standardized odds ratio */

%if %length(&contribution_prefix.) %then %do;

      contribution = (vecdiag(gamma_hat * gamma_hat`) / stdlnor2)`;
      

/*  Partial derivatives of vector of consisting of the proportional contribution of each variable to the risk score variance */

%do L = 1 %to &nvar.;
      partial_pi_&L. =  vecdiag(root_sigma_z * D&L. * root_sigma_z);
%end; 
      partial_pi = partial_pi_1 %do L = 2 %to &nvar.; || partial_pi_&L. %end;; 
      gradient_pi = (1 / stdlnor2) * (partial_pi - 2 * contribution` * (SIGMAZ * BETA)` )`;
      
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
         nc2 = noncentrality_Scheffe * eta2 / stdlnor2;          
         &p_value_int_Scheffe. = 1 - CDF('CHISQ',&chi_sq.,&nvar.,nc2);  
         end;
                                   else do;
         &p_value_int_Scheffe. = .;
         end;
%end;
    
      create &t..stdlnor var {stdlnor2 &chi_sq. trzv %if %length(&intvl_hyp.) %then &p_value_int. &p_value_int_Scheffe. ;
                              min_eigenvalue_SG max_eigenvalue_SG noncentrality_Scheffe noncentrality_Scheffe_min};
      append var {stdlnor2 &chi_sq. trzv %if %length(&intvl_hyp.) %then &p_value_int. &p_value_int_Scheffe. ;
                              min_eigenvalue_SG max_eigenvalue_SG noncentrality_Scheffe noncentrality_Scheffe_min};

/*  Standardized log odds ratios for individual covariates */

%if %length(&zb_prefix.) %then %do;

      b_std = SQRT(VECDIAG(SIGMAZ))#BETA;

      create &t..b_std from b_std [colname='b_std'];
      append from b_std;
%end;

/*  Covariance matrix of individual covariate standardized log odds ratio estimates */

%if %length(&zv_prefix.) %then %do;

      v_std = SIGMAZ#V;

      create &t..v_std from v_std;
      append from v_std;
%end;

/*  Proportional contribution of each variable to the total squared standardized odds ratio */

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


/** If macro parameters were set, capture the individual standardized log odds ratio estimates and their covariance matrix **/

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

data &t..stdlnor;
     merge &t..stdlnor &t..zb;
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

data &t..stdlnor;
     merge &t..stdlnor &t..zv;
run;
%end;

%if %length(&contribution_prefix.) %then %do;

data &t..stdlnor;
     merge &t..stdlnor &t..contribution &t..se_contribution;
run;

%end;


data &t..stdlnor;
     set &t..stdlnor;
     imlby = &imlby;
run;

%if %length(&var_combo_indsn.) %then %do;

data &t..combo_contrib;
     set &t..combo_contrib;
     imlby = &imlby;
run;

%end;

%end;

/********************************************************************************/
/**  If collinearity flag raised, set output to missing values.                 */
/********************************************************************************/

%else %do;  
data &t..stdlnor;
        imlby = &imlby.;
        
        stdlnor2 = .;
        &chi_sq. = .;
        trzv = .;
%if %length(&intvl_hyp.) %then %do; 
        &p_value_int. = .; 
        &p_value_int_Scheffe. = .;
%end;
        min_eigenvalue_SG = .;
        max_eigenvalue_SG = .;
        noncentrality_Scheffe = .;
        noncentrality_Scheffe_min = .;
        
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

%end;

proc append base=&t..stdlnorall data=&t..stdlnor force;
run;

%if %length(&var_combo_indsn.) %then %do;

proc append base=&t..combo_contriball data=&t..combo_contrib;
run;

%end;

%end;

%if &byflag. = 1 %then %do;

/* Merge the by variables back in */

data &t..stdlnorall;
    merge &t..stdlnorall &t..byvar;
    by imlby;
run; 

%end;

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

data &t..outdsn;
     merge &t..stdlnorall &t..colflag;
     by imlby;

/*  Absolute standardized log odds ratio estimate */

%if %length(&partial.) %then %let lpartial = Partial;
                       %else %let lpartial = ;  

     &abs_std_log_OR. = sqrt(stdlnor2);
        label &abs_std_log_OR. = &lpartial. Absolute Standardized Log Odds Ratio Estimate;

/*  Standard error of standardized absolute log odds ratio estimate */
    
     &var_std_log_OR. = trzv;
         label &var_std_log_OR. = Variance of the &lpartial. Standardized Log Odds Ratio Estimate;

/*  Bias-corrected estimate of the absolute standardized log odds ratio */

     &abs_std_log_OR_correct. = sqrt(max(stdlnor2 - trzv,0));
        label &abs_std_log_OR_correct. = Bias-Corrected Absolute &lpartial. Standardized Log Odds Ratio;

/*  Chi-square statistic, degrees of freedom, and p-value */

        label &chi_sq. = Chi-square Statistic for Test That &lpartial. Absolute Standardized Log Odds Ratio Equals 0;

     &df. = &nvar.;
        label &df. = Degrees of Freedom;

     &p_value. = 1 - CDF('CHISQ',&chi_sq.,&nvar.); 
        label &p_value. = P-value for Test That &lpartial. Absolute Standardized Log Odds Ratio Equals 0;    

%if %length(&intvl_hyp.) %then %do;
     label &p_value_int. = P-value for Interval Null Hypothesis Test That &lpartial. ASLOR <= &intvl_hyp.;
     label &p_value_int_Scheffe. = Scheffe-Alignment P-value for Interval Null Hypothesis Test That &lpartial. ASLOR <= &intvl_hyp.;
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

      &abs_std_log_OR_LCL. = sqrt(eta2plus);
         label &abs_std_log_OR_LCL. = "Lower Limit of Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";

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

      &abs_std_log_OR_UCL. = sqrt(eta2minus);
         label &abs_std_log_OR_UCL. = "Upper Limit of Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
         
/***********************************************************************************/
/** Compute confidence interval using Scheffe-alignment method.                    */
/***********************************************************************************/

/*  Lower limit */

     alpha2comp = 1 - &alpha./2;
     step = stdlnor2 / 2;
     eta2plus = 0;
     totstep = 0;
     do while (step > 10**(-12) and totstep <= 1000);
        flag = 0;
        do while (flag=0 and totstep <= 1000);
          eta2plus = eta2plus + step;
          nc = noncentrality_Scheffe * eta2plus / stdlnor2;            
          if &chi_sq. <= CINV(alpha2comp,&nvar.,nc) then flag = 1;
          totstep = totstep + 1;
          end;
        eta2plus = eta2plus - step;
        step = step / 2;
        end;

      &abs_std_log_OR_LCL_Scheffe. = sqrt(eta2plus);
         label &abs_std_log_OR_LCL_Scheffe. = "Lower Limit of Scheffe &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";

/*  Upper limit */

     alpha2 = &alpha. / 2;
     step = 2 * stdlnor2;
     eta2minus = 0;
     totstep = 0;
     do while (step > 10**(-12) and totstep <= 1000);
        flag = 0;
        do while (flag=0 and totstep <= 1000);
          eta2minus = eta2minus + step;
          nc = noncentrality_Scheffe_min * eta2minus / stdlnor2;            
          if &chi_sq. < CINV(alpha2,&nvar.,nc) then flag = 1;
          totstep = totstep + 1;
          end;
        eta2minus = eta2minus - step;
        step = step / 2;
        end;

      &abs_std_log_OR_UCL_Scheffe. = sqrt(eta2minus);
         label &abs_std_log_OR_UCL_Scheffe. = "Upper Limit of Scheffe &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
%end; 

%if %length(&maxVIF.) %then %do;
    label &maxVIF. = Max. variance inflation factor from assessment of covariate collinearity (must be < 100);
%end;

%if %length(&predictors.) %then %do;
     length &predictors. $ 240;
     &predictors. = "&vars.";
%end;

/***********************************************************************************/
/** Compute absolute standardized odds ratios and associated confidence           **/
/** confidence intervals.                                                         **/
/***********************************************************************************/

     bc_ratio = &abs_std_log_OR_correct. / &abs_std_log_OR.;

     &abs_std_OR. = exp(&abs_std_log_OR.);
     &abs_std_OR_correct. = exp(&abs_std_log_OR_correct.);

        label &abs_std_OR. = &lpartial. Absolute Standardized Odds Ratio Estimate;
        label &abs_std_OR_correct. = Bias-Corrected &lpartial. Absolute Standardized Odds Ratio;

%if %length(&alpha.) %then %do;

     &abs_std_OR_LCL. = exp(&abs_std_log_OR_LCL.); 
     &abs_std_OR_UCL. = exp(&abs_std_log_OR_UCL.); 
     &abs_std_OR_LCL_Scheffe. = exp(&abs_std_log_OR_LCL_Scheffe.); 
     &abs_std_OR_UCL_Scheffe. = exp(&abs_std_log_OR_UCL_Scheffe.);    
     &abs_std_OR_correct_LCL. = exp(&abs_std_log_OR_LCL. * bc_ratio); 
     &abs_std_OR_correct_UCL. = exp(&abs_std_log_OR_UCL. * bc_ratio); 
     &abs_std_OR_correct_LCL_Scheffe. = exp(&abs_std_log_OR_LCL_Scheffe. * bc_ratio); 
     &abs_std_OR_correct_UCL_Scheffe. = exp(&abs_std_log_OR_UCL_Scheffe. * bc_ratio); 

        label &abs_std_OR_LCL. = "Lower Limit of Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
        label &abs_std_OR_UCL. = "Upper Limit of Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
        label &abs_std_OR_LCL_Scheffe. = "Lower Limit of Scheffe-Alignment &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
        label &abs_std_OR_UCL_Scheffe. = "Upper Limit of Scheffe-Alignment &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
        label &abs_std_OR_correct_LCL. = "Lower Limit of Bias-Corrected Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
        label &abs_std_OR_correct_UCL. = "Upper Limit of Bias-Corrected Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
        label &abs_std_OR_correct_LCL_Scheffe. = "Lower Limit of Bias-Corrected Scheffe-Alignment &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
        label &abs_std_OR_correct_UCL_Scheffe. = "Upper Limit of Bias-Corrected Scheffe-Alignment &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";

%end;

     keep &abs_std_log_OR. &var_std_log_OR. &abs_std_log_OR_correct. 
          &abs_std_OR. &abs_std_OR_correct. 
          &chi_sq.  &df. &p_value. &min_eigenvalue. &noncentrality_Scheffe.  %if %length(&intvl_hyp.) %then &p_value_int. &p_value_int_Scheffe.; &maxVIF. 
          %if %length(&alpha.) %then %do;
	        &abs_std_log_OR_LCL. &abs_std_log_OR_UCL. 
                &abs_std_log_OR_LCL_Scheffe. &abs_std_log_OR_UCL_Scheffe.
                &abs_std_OR_LCL. &abs_std_OR_UCL.
                &abs_std_OR_LCL_Scheffe. &abs_std_OR_UCL_Scheffe.
                &abs_std_OR_correct_LCL. &abs_std_OR_correct_UCL.
                &abs_std_OR_correct_LCL_Scheffe. &abs_std_OR_correct_UCL_Scheffe.
          %end;
          %if %length(&zb_prefix.) %then &zb_prefix.1-&zb_prefix.&nvar.;
          %if %length(&zv_prefix.) %then &zv_prefix.1-&zv_prefix.&nvar2.;
          &byvar.
          %if %length(&predictors.) %then &predictors.;
          %if %length(&contribution_prefix.) %then %do i = 1 %to &nvar.; &contribution_prefix._&&var&i.. se_&contribution_prefix._&&var&i.. %end;
          &maxVIF.;
run;

%end;

%end;

%if &go. = 0 or &gocol. = 0 %then %do;

/***********************************************************************************/
/**  Missing values for parameter estimates or covariance matrices or             **/
/**  multicollinearity detected (for all by-group values, if by group variable    **/
/**  specified)   Output data set  with standardized log odds ratio missing.      **/
/***********************************************************************************/

data &t..outdsn;
     set &t..colflag;

     &abs_std_log_OR. = .;
        label &abs_std_log_OR. = Absolute &lpartial. Standardized Log Odds Ratio Estimate;
     &var_std_log_OR. = .;
         label &var_std_log_OR. = Variance of the &lpartial. Standardized Log Odds Ratio Estimate;
     &abs_std_log_OR_correct. = .;
        label &abs_std_log_OR_correct. = Bias-Corrected Absolute &lpartial. Standardized Log Odds Ratio;
     &chi_sq. = .;
        label &chi_sq. = Chi-square Statistic for Test That Squared &lpartial. Standardized Log Odds Ratio Equals 0;
     &p_value. = .; 
        label &p_value. = P-value for Test That Squared &lpartial. Standardized Log Odds Ratio Equals 0;   
     &df. = &nvar.;
        label &df. = Degrees of Freedom;
     &min_eigenvalue. = .;

/***********************************************************************************/
/** Compute absolute standardized odds ratios and associated confidence           **/
/** confidence intervals.                                                         **/
/***********************************************************************************/

     &abs_std_OR. = .;
     &abs_std_OR_correct. = .;

        label &abs_std_OR. = &lpartial. Absolute Standardized Odds Ratio Estimate;
        label &abs_std_OR_correct. = Bias-Corrected &lpartial. Absolute Standardized Odds Ratio;

%if %length(&alpha.) %then %do;

     &abs_std_OR_LCL. = .; 
     &abs_std_OR_UCL. = .; 
     &abs_std_OR_LCL_Scheffe. = .; 
     &abs_std_OR_UCL_Scheffe. = .;    
     &abs_std_OR_correct_LCL. = .; 
     &abs_std_OR_correct_UCL. = .; 
     &abs_std_OR_correct_LCL_Scheffe. = .; 
     &abs_std_OR_correct_UCL_Scheffe. = .; 

        label &abs_std_OR_LCL. = "Lower Limit of Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
        label &abs_std_OR_UCL. = "Upper Limit of Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
        label &abs_std_OR_LCL_Scheffe. = "Lower Limit of Scheffe-Alignment &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
        label &abs_std_OR_UCL_Scheffe. = "Upper Limit of Scheffe-Alignment &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
        label &abs_std_OR_correct_LCL. = "Lower Limit of Bias-Corrected Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
        label &abs_std_OR_correct_UCL. = "Upper Limit of Bias-Corrected Conservative &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
        label &abs_std_OR_correct_LCL_Scheffe. = "Lower Limit of Bias-Corrected Scheffe-Alignment &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";
        label &abs_std_OR_correct_UCL_Scheffe. = "Upper Limit of Bias-Corrected Scheffe-Alignment &conf. Percent Confidence Interval for &lpartial. Absolute Standardized Odds Ratio";

%end;

%if %length(&maxVIF.) %then %do;
    label &maxVIF. = Max. variance inflation factor from assessment of covariate collinearity (must be < 100);
%end;

     keep &abs_std_log_OR. &var_std_log_OR. &abs_std_log_OR_correct. 
          &abs_std_OR. &abs_std_OR_correct. 
          &chi_sq.  &df. &p_value. &min_eigenvalue. &noncentrality_Scheffe. &maxVIF. 
          %if %length(&alpha.) %then %do;
	        &abs_std_log_OR_LCL. &abs_std_log_OR_UCL. 
                &abs_std_log_OR_LCL_Scheffe. &abs_std_log_OR_UCL_Scheffe.
                &abs_std_OR_LCL. &abs_std_OR_UCL.
                &abs_std_OR_LCL_Scheffe. &abs_std_OR_UCL_Scheffe.
                &abs_std_OR_correct_LCL. &abs_std_OR_correct_UCL.
                &abs_std_OR_correct_LCL_Scheffe. &abs_std_OR_correct_UCL_Scheffe.
          %end;
          %if %length(&zb_prefix.) %then &zb_prefix.1-&zb_prefix.&nvar.;
          %if %length(&zv_prefix.) %then &zv_prefix.1-&zv_prefix.&nvar2.;
          &byvar.
          %if %length(&predictors.) %then &predictors.;
          %if %length(&contribution_prefix.) %then %do i = 1 %to &nvar.; &contribution_prefix._&&var&i.. se_&contribution_prefix._&&var&i.. %end;
          &maxVIF.;
run;

%end;

proc sql;
     create table &outdsn. as select
          %if &byflag. = 1 %then %do i = 1 %to &nbyvar.; &&byvar&i.., %end;
          %if %length(&predictors.) %then &predictors.,;
          &abs_std_log_OR.,&var_std_log_OR.,&abs_std_log_OR_correct.,
          &abs_std_OR.,&abs_std_OR_correct.,
          &chi_sq.,&df.,&p_value.,
          %if %length(&intvl_hyp.) %then &p_value_int.,&p_value_int_Scheffe.,;
          &min_eigenvalue.,&noncentrality_Scheffe.,
          %if %length(&alpha.) %then %do;
                         &abs_std_log_OR_LCL.,&abs_std_log_OR_UCL.,
                         &abs_std_log_OR_LCL_Scheffe.,&abs_std_log_OR_UCL_Scheffe.,
                         &abs_std_OR_LCL.,&abs_std_OR_UCL.,
                         &abs_std_OR_LCL_Scheffe.,&abs_std_OR_UCL_Scheffe.,
                         &abs_std_OR_correct_LCL.,&abs_std_OR_correct_UCL.,
                         &abs_std_OR_correct_LCL_Scheffe.,&abs_std_OR_correct_UCL_Scheffe.,   
          %end;
          %if %length(&zb_prefix.) %then %do i = 1 %to &nvar.;  &zb_prefix.&i., %end;
          %if %length(&zv_prefix.) %then %do i = 1 %to &nvar2.; &zv_prefix.&i., %end;
          %if %length(&contribution_prefix.) %then %do i = 1 %to &nvar.; &contribution_prefix._&&var&i.., %end;
          &maxVIF. 
          from &t..outdsn; 
quit;     

/************************************************************************************
** Clean up data sets and reset mergenoby option to what is was.                    *
************************************************************************************/

proc datasets memtype=data lib=&t. kill nodetails nolist;
run;
quit;

options mergenoby=&mergenobyoption.;

%mend STDLOGOR;