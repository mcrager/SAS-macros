/***********************************************************************************************************************

Program Name           : Risk_Est_PH_reg_comp_risk.sas

Path                   : 

Program Language       : SAS

Operating System       : Server

Purpose                : Estimate the absolute risk of an event by a specified time using Cox proportional hazards regression
                         of cause-specific hazards in a competing risk setting.  Cohort sampling study designs and time-dependent 
                         covariates are accomodated.  Ties in event times are handled using the Efron method.
                         
Notes                  : 

Status                 : Tested and verified.

Run Dependencies       : None

Input Datasets/Views   : Specified SAS data set indsn containing time and event status variables for the event of interest
                         and for up to 9 competing risk events, covariates for the event of interest, covariates for the competing 
                         risk events, and a variable giving the sampling weights if cohort sampling was used.  Optionally, 
                         a separate data set giving covariate values for which the risk is to be estimated.  

Output Datasets/Views  : Specified SAS data set outdsn containing the absolute risk estimate for each patient in the data set 
                         or, if specified, the covariate data set, and a confidence interval.
                         
Other Output           : None

Macro calls internal   : 

Macro calls external   : None

***********************************************************************************************************************/

%macro Risk_Est_PH_reg_comp_risk(
        /* Input Specification */  indsn=,byvar=,
                                   vars_main=,time_main=,censor_main=,censorlist_main=0,
                                   vars_cr=, time_cr=, censor_cr=, censorlist_cr=0,
                                   vars_cr2=,time_cr2=,censor_cr2=,censorlist_cr2=0,
                                   vars_cr3=,time_cr3=,censor_cr3=,censorlist_cr3=0,
                                   vars_cr4=,time_cr4=,censor_cr4=,censorlist_cr4=0,
                                   vars_cr5=,time_cr5=,censor_cr5=,censorlist_cr5=0,
                                   vars_cr6=,time_cr6=,censor_cr6=,censorlist_cr6=0,
                                   vars_cr7=,time_cr7=,censor_cr7=,censorlist_cr7=0,
                                   vars_cr8=,time_cr8=,censor_cr8=,censorlist_cr8=0,
                                   vars_cr9=,time_cr9=,censor_cr9=,censorlist_cr9=0,
                                   weight=,
                                   covariate_dsn=,
                                   programming_statements=%str(),programming_time=,calc_vars=,
        /* Analysis Parameters */  risk_time=,robust=no,print=yes,alpha=0.05,strata=,CI_method=loglog,window_main=,
        /* Output Specification */ outdsn=,Risk=Risk,Risk_LCL=Risk_LCL,Risk_UCL=Risk_UCL
                         );
            
/***********************************************************************************************
| DEFINITION OF PARAMETERS
|
| Name           : indsn
| Required (Y/N) : Y
| Default Value  : 
| Purpose        : Libname reference and the input data set name.  
|-----------------------------------------------------------------------------------------------
| Name           : byvar
| Required (Y/N) : N
| Default Value  :
| Purpose        : List of input data set variables to do the analysis by.                    
|-----------------------------------------------------------------------------------------------
| Name           : vars_main
| Required (Y/N) : Y
| Default Value  :
| Purpose        : List of input data set variables to be used as the covariate in the Cox model 
|                  for the event of interest.  If the programming statements create the variables
|                  that are to be included in the model, list the variables thus created.
|-----------------------------------------------------------------------------------------------
| Name           : time_main
| Required (Y/N) : Y
| Default Value  : 
| Purpose        : Input data set variable containing the time to event of interest (or censoring) 
|-----------------------------------------------------------------------------------------------
| Name           : censor_main
| Required (Y/N) : Y
| Default Value  : 
| Purpose        : Input data set variable indicating whether the observed time to event of interest was censored.
|-----------------------------------------------------------------------------------------------
| Name           : censorlist_main
| Required (Y/N) : N
| Default Value  : 0
| Purpose        : List of values of variable censor that indicate a censored observation for the event of interest.
|                  Default is the single value 0.
|-----------------------------------------------------------------------------------------------
| Name           : vars_cr, vars_crK, K=2,3,....,9
| Required (Y/N) : N
| Default Value  :
| Purpose        : List of input data set variables to be used as the covariate in the Cox model 
|                  for competing risk event K.  If the programming statements create the variables
|                  that are to be included in the model, list the variables thus created.  This parameter may be
|                  left blank, in which case the Nelson-Aalen estimate of the distribution of the competing
|                  risk is used.
|-----------------------------------------------------------------------------------------------
| Name           : time_cr, time_crK, K=2,3,....,9
| Required (Y/N) : time_cr is required, time_cr2-time_cr9 are not.
| Default Value  : 
| Purpose        : Input data set variable containing the time to competing risk event K (or censoring) 
|-----------------------------------------------------------------------------------------------
| Name           : censor_cr, censorK, K=2,3,....,9
| Required (Y/N) : censor_cr is required, censor_cr2-censor_cr9 are not.
| Default Value  : 
| Purpose        : Input data set variable indicating whether the observed time to competing risk event K was censored.
|-----------------------------------------------------------------------------------------------
| Name           : censorlist_cr, censorlist_crK, K=2,3,....,9
| Required (Y/N) : N
| Default Value  : 0
| Purpose        : List of values of variable censor that indicate a censored observation for the competing risk event K.
|                  Default is the single value 0.
|-----------------------------------------------------------------------------------------------
| Name           : weight  
| Required (Y/N) : N
| Default Value  : 
| Purpose        : Input data set variable giving the observation's weight in the analysis. If this parameter
|                  is set, it is assumed that cohort sampling was used and resulted in the specified weights.
|-----------------------------------------------------------------------------------------------
| Name           : programming_statements
| Required (Y/N) : N
| Default Value  : 
| Purpose        : %str()-enclosed text string including programming statements that will be inserted into proc PHREG and various
|                  data steps to compute the time-dependent covariate values for both the event of interest and competing
|                  risk event.  For example: programming_statements = %str(if time <= 3 then x_3 = 0; else x_3 = x;) If 
|                  no programming statements are entered, the risk calculations will be made for covariates that are constant 
|                  over time.
|-----------------------------------------------------------------------------------------------
| Name           : programming_time
| Required (Y/N) : Y if programming_statements are specified.
| Default Value  : 
| Purpose        : Variable that represents time in the programming statements.  
|-----------------------------------------------------------------------------------------------
| Name           : calc_vars
| Required (Y/N) : Y if time-dependent covariates are used.
| Default Value  : 
| Purpose        : List of variables that are used in the calculation of the time-dependent covariates for the event of interest. 
|                  and/or the competing risk event.  IF THE MODEL ALSO INCLUDES TIME-CONSTANT COVARIATES, BE SURE TO INCLUDE THEM 
|                  IN THE LIST.  Leave the calc_vars parameter blank if time dependent variables are not used. 
|-----------------------------------------------------------------------------------------------
| Name           : covariate_dsn                
| Required (Y/N) : N
| Default Value  : 
| Purpose        : Libname reference and the name of a data set that contains the covariate values for both the event interest
|                  and the competing risk event for which the risk is to be estimated.  The data set must have all the variables
|                  included in the model, or that are required to derive these variables if the model has time-dependent covariates
|                  derived using programming statements.  The data set must also include the stratification variable if
|                  the model is stratified.  No by variables should be included in this data set.
|
|                  If no covariate data set is specified, the risk will be estimated for every patient in the main input data set.
|-----------------------------------------------------------------------------------------------
| Name           : risk_time  
| Required (Y/N) : Y
| Default Value  : 
| Purpose        : This is the time at which the risk of each patient is assessed.  That is, the risk is defined
|                  as the probability that the patient will have the event on or before risk_time.
|-----------------------------------------------------------------------------------------------
| Name           : robust   
| Required (Y/N) : N
| Default Value  : no
| Purpose        : If this parameter is set to yes and cohort sampling is NOT used (parameter "weight" is not specified), 
|                  the Lin-Wei robust estimate of variance will be used in the Cox proportional hazards model.
|                  This parameter has no effect when the parameter "weight" is specified.
|-----------------------------------------------------------------------------------------------
| Name           : print   
| Required (Y/N) : N
| Default Value  : yes
| Purpose        : If this parameter is set to no, the PROC PHREG output will not be printed.                      
|-----------------------------------------------------------------------------------------------
| Name           : alpha   
| Required (Y/N) : N
| Default Value  : 0.05
| Purpose        : The macro will compute a 100(1-alpha)% confidence interval for the risk and cumulative hazard.                      
|-----------------------------------------------------------------------------------------------
| Name           : strata   
| Required (Y/N) : N
| Default Value  : 
| Purpose        : Character string giving input data set variables by which the proportional hazards regression
|                  analysis will be stratified.                       
|-----------------------------------------------------------------------------------------------
| Name           : CI_method   
| Required (Y/N) : N
| Default Value  : loglog
| Purpose        : Character string giving the method for computing the confidence intervals.  If linear is specified
|                  the confidence interval is computed on the CIF scale.  If log is specified, the confidence interval
|                  is computed on the cumulative hazard scale and back-transformed to the CIF scale.  If loglog is 
|                  specified, the confidence interval is computed on the log cumulative hazard scale and back-transformed
|                  to the CIF scale.  The default method is loglog.   
|-----------------------------------------------------------------------------------------------
| Name           : window_main   
| Required (Y/N) : Y
| Default Value  : 
| Purpose        : Constant giving a time window after censoring for the event of interest within which competing risk
|                  events will be counted.  The planned time interval of follow-up for the main event or a value slightly
|                  larger is a reasonable window. 
|-----------------------------------------------------------------------------------------------
| Name           : outdsn  
| Required (Y/N) : Y
| Default Value  : (at temporary library)
| Purpose        : Libname reference and the output data set name.  The dataset name must conform to the rules for
|                  SAS names.  This data set will contain all the records and variables of the covariate data set 
|                  (or the input data set if no separate coavariate data set is specified) plus the variables 
|                  named by the following eight macro parameters.
|-----------------------------------------------------------------------------------------------
| Name           : Risk     
| Required (Y/N) : N
| Default Value  : Risk    
| Purpose        : Name of output data set variable that will contain the risk estimate. 
|-----------------------------------------------------------------------------------------------
| Name           : Risk_LCL
| Required (Y/N) : N
| Default Value  : Risk_LCL
| Purpose        : Name of output data set variable will contain the lower limit of a 1-alpha confidence
|                  interval for the risk.                                    
|-----------------------------------------------------------------------------------------------
| Name           : Risk_UCL
| Required (Y/N) : N
| Default Value  : Risk_UCL
| Purpose        : Name of output data set variable will contain the upper limit of a 1-alpha confidence
|                  interval for the risk.                                
**********************************************************************************************************/;
/**********************************************************************************************************
Mod#    Date         Username    Test     Description
---     -------      --------    ----    -----------------------------------------------------------
000     20201022     mcrager             
**********************************************************************************************************/;
 
%local byvar nvar_main nvar_main2 num_cr ncalc_var i j k L byflag dpc_var_main dpc_calc_var
       crreg_flag dsid rc errcode in_calc_vars;

/* Check for errors in macro parameter specification */

   %if %length(&indsn.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_comp_risk macro parameter indsn must be specified.;
      %abort;
   %end;

   %if %length(&vars_main.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_comp_risk macro parameter vars_main must be specified.;
      %abort;
   %end;

   %if %length(&time_main.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_comp_risk macro parameter time_main must be specified.;
      %abort;
   %end;
   
   %if %length(&censor_main.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_comp_risk macro parameter censor_main must be specified.;
      %abort;
   %end;

   %if %length(&time_cr.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_comp_risk macro parameter time_cr must be specified.;
      %abort;
   %end;
   
   %if %length(&censor_cr.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_comp_risk macro parameter censor_cr must be specified.;
      %abort;
   %end;

   %if %length(&risk_time.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_comp_risk macro parameter risk_time must be specified.;
      %abort;
   %end;
   
   %if %length(&programming_statements.)>0 and %length(&calc_vars.) =0 %then %do;
      %put ERROR : Risk_Est_PH_reg_comp_risk macro parameter calc_vars must be specified;
      %put                   when macro parameter programming_statements is specified.;
      %abort;
   %end;

   %if %length(&programming_statements.)>0 and %length(&programming_time.) =0 %then %do;
      %put ERROR : Risk_Est_PH_reg_comp_risk macro parameter programming_time must be specified;
      %put                   when macro parameter programming_statements is specified.;
      %abort;
   %end;

   %if %length(&window_main.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_comp_risk macro parameter window_main must be specified.;
      %abort;
   %end;

   %if %length(&outdsn.)=0 %then %do;
      %put ERROR : Risk_Est_PH_reg_comp_risk macro parameter outdsn must be specified.;
      %abort;
   %end;

   %if %length(&alpha.) %then %do;
   %if %sysevalf(&alpha. <= 0) or %sysevalf(&alpha. >= 1) %then %do;
      %put ERROR : Risk_Est_PH_reg_comp_risk macro parameter alpha must be >0 and <1.;
      %abort;
   %end;
   %end;

/**********************************************************************************************
**  Set all parameters with default values to those values if blank is specified.             *
**********************************************************************************************/

%if %length(&alpha.) = 0 %then %let alpha = 0.05;
%if %length(&censorlist_main.)=0 %then %let censorlist_main = 0;
%if %length(&censorlist_cr.)=0 %then %let censorlist_cr = 0;
%do i = 2 %to 9;
%if %length(&&time_cr&i..) and %length(&&censorlist_cr&i..) = 0 %then %let censorlist_cr&i. = 0;
%end;
%if %length(&robust.) = 0 %then %let robust = no;
%if %length(&CI_method.) = 0 %then %let CI_method = loglog;
%if %length(&Risk.) = 0 %then %let Risk = Risk;
%if %length(&Risk_LCL.) = 0 %then %let Risk = Risk_LCL;
%if %length(&Risk_UCL.) = 0 %then %let Risk = Risk_UCL;
 
   
/**********************************************************************************************
**  Parse the string containing by "by variable".                                             *
**********************************************************************************************/

%let byflag = 0;

%if %length(&byvar) %then %do;
   %let byvar=%sysfunc(compbl(&byvar));
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
**  Parse the input parameter vars containing the list of covariates for the event of         *
**  of interest, determine the number of covariates, and assign them to the macro variables   *
**  var_main1, var_main2, etc.                                                                *
**********************************************************************************************/

%let vars_main = %sysfunc(compbl(&vars_main.));

%let nvar_main = %sysfunc(countc(%bquote(&vars_main.), %str( )));
%let nvar_main = %sysfunc(ifc(%length(%bquote(&vars_main.)), %eval(&nvar_main. + 1), 0));

%do i = 1 %to &nvar_main.;
   %local var_main&i.;
   %let var_main&i = %scan(&vars_main., &i., %str( ));
%end; 

%let nvar_main_squared = %sysevalf(&nvar_main.**2,integer);  

/**********************************************************************************************
**  Parse the input parameter vars containing the list of covariates for the competing risk   *
**  events, determine the number of covariates, and assign them to the macro variables        *
**  var_crK_1, var_crK_2, etc, where K is the number of the competing risk event type.  If no *
**  variables are specified for a particular event type, substitute a dummy variable that     *
**  always equals 0.                                                                          *
**********************************************************************************************/

%local time_cr1 censor_cr1 censorlist_cr1;
%let time_cr1 = &time_cr.;
%let censor_cr1 = &censor_cr.;
%let censorlist_cr1 = &censorlist_cr.;
%let vars_cr1 = &vars_cr.;

%let num_cr = 0;

%do k = 1 %to 9;

%if  %length(&&time_cr&k..) > 0 and %length(&&censor_cr&k..) = 0 %then %do;
   %put ERROR :  macro Risk_Est_PH_reg_comp_risk found time_cr&k. specified but no specification for censor_cr&k.;
   %abort;
   %end;

%if  %length(&&censor_cr&k..) > 0 and %length(&&time_cr&k..) = 0 %then %do;
   %put ERROR :  macro Risk_Est_PH_reg_comp_risk found censor_cr&k. specified but no specification for time_cr&k.;
   %abort;
   %end;

%if %length(&&time_cr&k..) %then %do;

%let num_cr = %sysevalf(&num_cr.+1,integer);

%if %length(&&vars_cr&k..) %then %do;

%let vars_cr&k. = %sysfunc(compbl(&&vars_cr&k..));

%let nvar_cr&k. = %sysfunc(countc(%bquote(&&vars_cr&k..), %str( )));
%let nvar_cr&k. = %sysfunc(ifc(%length(%bquote(&&vars_cr&k..)), %eval(&&nvar_cr&k.. + 1), 0));

%do i = 1 %to &&nvar_cr&k..;
   %local var_cr&k._&i.;
   %let var_cr&k._&i. = %scan(&&vars_cr&k.., &i., %str( ));
%end; 

%let nvar_cr&k._squared = %sysevalf(&&nvar_cr&k..**2,integer); 

%let crreg_flag&k. = 1;

%local nvar_cr&k. nvar_cr&k._squared;

%end;

%else %do;

%let vars_cr&k. = qqzero;
%local var_cr&k._1; 
%let var_cr&k._1 = qqzero;
%let nvar_cr&k. = 1;
%let nvar_cr&k._squared = 1;

%local crreg_flag&k.;
%let crreg_flag&k. = 0;

%end;

%end;

%end;

%let crreg_flag = 1;
%do k = 1 %to &num_cr.;
%if &&crreg_flag&k.. = 0 %then %let crreg_flag = 0;
%end;


/**********************************************************************************************
**  Parse the input parameter calc_vars, which contains the list of variables that are        *
**  used to calculate the time-dependent covariates.  If field is blank, substitute the       *
**  variables contained in vars_main and vars_cr, vars_crK.                                   *
**********************************************************************************************/

%if %length(&calc_vars.) = 0 %then %do;

data _qq_calc_vars;
     length calc_var $ 40;

%do i = 1 %to &nvar_main.;
     calc_var = "&&var_main&i..";
     output;
%end;

%do k = 1 %to &num_cr.;
%do i = 1 %to &&nvar_cr&k..;
     calc_var = "%scan(&&vars_cr&k.., &i., %str( ))";
     output;
%end;
%end;

%if &crreg_flag. = 0 %then %do;
     calc_var = 'qqzero';
     output;
%end;

run;

proc sql noprint;
     select distinct calc_var into :calc_var1-:calc_var9999
     from _qq_calc_vars;
quit;

%let ncalc_var = &sqlobs.;

%let calc_vars = ;
%do i = 1 %to &ncalc_var.;
   %let calc_vars = &calc_vars. &&calc_var&i..;
   %local calc_var&i.;
%end;

%end;

%else %do;

%if &crreg_flag. = 0 %then %let calc_vars = &calc_vars. qqzero;
%let calc_vars = %sysfunc(compbl(&calc_vars.));

%let ncalc_var = %sysfunc(countc(%bquote(&calc_vars.), %str( )));
%let ncalc_var = %sysfunc(ifc(%length(%bquote(&calc_vars.)), %eval(&ncalc_var. + 1), 0));

%do i = 1 %to &ncalc_var.;
   %local calc_var&i.;
   %let calc_var&i = %scan(&calc_vars., &i., %str( ));
%end; 

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
     %put ERROR : Risk_Est_PH_reg_comp_risk macro input data set &indsn. does not exist.;
     %abort;
     %end;
   %else %do;
     %if %sysfunc(varnum(&dsid.,&time_main.)) = 0 %then %do;
       %put ERROR : Risk_Est_PH_reg_comp_risk macro input data set &indsn. does not contain the variable &time_main. specified in macro parameter time_main.;
       %let errcode = 1;
       %end;
     %if %sysfunc(varnum(&dsid.,&censor_main.)) = 0 %then %do;
       %put ERROR : Risk_Est_PH_reg_comp_risk macro input data set &indsn. does not contain the variable &censor_main. specified in macro parameter censor_main.;
       %let errcode = 1;
       %end;
     %if %sysfunc(varnum(&dsid.,&time_cr.)) = 0 %then %do;
       %put ERROR : Risk_Est_PH_reg_comp_risk macro input data set &indsn. does not contain the variable &time_cr. specified in macro parameter time_cr.;
       %let errcode = 1;
       %end;
     %if %sysfunc(varnum(&dsid.,&censor_cr.)) = 0 %then %do;
       %put ERROR : Risk_Est_PH_reg_comp_risk macro input data set &indsn. does not contain the variable &censor_cr. specified in macro parameter censor_cr.;
       %let errcode = 1;
       %end;
     %do k = 2 %to &num_cr.;
          %if %sysfunc(varnum(&dsid.,&&time_cr&k..)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_comp_risk macro input data set &indsn. does not contain the variable &&time_cr&k.. specified in macro parameter time_cr&k..;
            %let errcode = 1;
            %end;
          %if %sysfunc(varnum(&dsid.,&&censor_cr&k..)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_comp_risk macro input data set &indsn. does not contain the variable &&censor_cr&k.. specifed in macro parameter censor_cr&k..;
            %let errcode = 1;
            %end;
         %end; 
     %do k = 1 %to &ncalc_var.;
          %if %sysfunc(varnum(&dsid.,&&calc_var&k..)) = 0 and &&calc_var&k.. ne qqzero %then %do;
            %put ERROR : Risk_Est_PH_reg_comp_risk macro input data set &indsn. does not contain the variable &&calc_var&k.. specified as a covariate.;
            %let errcode = 1;
            %end;
         %end;
     %if %length(&weight.) %then %do;
         %if %sysfunc(varnum(&dsid.,&weight.)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_comp_risk macro input data set &indsn. does not contain variable &weight. specified in macro parameter weight.;
            %let errcode = 1; 
            %end;
         %end;
     %if %length(&strata.) %then %do;
         %if %sysfunc(varnum(&dsid.,&strata.)) = 0 %then %do;
            %put ERROR : Risk_Est_PH_reg_comp_risk macro input data set &indsn. does not contain variable &strata. specified in macro parameter strata.;
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
            %put ERROR : Risk_Est_PH_reg_comp_risk macro covariate data set &covariate_dsn. does not exist.;
            %abort;
            %end;
        %else %do k = 1 %to &ncalc_var.;
           %if &byflag. %then %do k = 1 %to &nbyvar.;
                %if %sysfunc(varnum(&dsid.,&&byvar&k..)) ne 0 %then %do;
                  %put ERROR : Risk_Est_PH_reg_comp_risk macro covariate data set &covariate_dsn. contains the variable &&byvar&k.. specified in parameter byvar. This data set may not contain by variables.;
                  %let errcode = 1;
                  %end;
               %end;
            %if %sysfunc(varnum(&dsid.,&&calc_var&k..)) = 0 and &&calc_var&k.. ne qqzero %then %do;
                %put ERROR : Risk_Est_PH_reg_comp_risk macro covariate data set &covariate_dsn. does not contain the variable &&calc_var&k.. specified as a covariate.;
                %let errcode = 1;
                %end;
            %end;
        %if %length(&strata.) %then %do;
            %if %sysfunc(varnum(&dsid.,&strata.)) = 0 %then %do;
               %put ERROR : Risk_Est_PH_reg_comp_risk macro covariate data set &covariate_dsn. does not contain variable &strata. specified in macro parameter strata.;
               %let errcode = 1;
               %end;
            %end;
        %let rc = %sysfunc(close(&dsid.));
        %if &errcode = 1 %then %abort;
        %end;

/**********************************************************************************************
**  Make sure that any time-constant covariates are included in the calc_vars list.           *
**********************************************************************************************/

%if %length(&programming_statements.) %then %do;

%let dsid = %sysfunc(open(&indsn.,i));

%do i = 1 %to &nvar_main.;
     %if %sysfunc(varnum(&dsid.,&&var_main&i..)) ne 0 %then %do;
         %let in_calc_vars = 0;
	 %do L = 1 %to &ncalc_var.;
	    %if &&var_main&i.. eq &&calc_var&L.. %then %let in_calc_vars = 1;
	    %end;
         %if &in_calc_vars. = 0 %then %do;
	     %let calc_vars = &calc_vars. &&var_main&i..;
             %put WARNING : Apparent time-constant covariate &&var_main&i.. for endpoint &time_main. was not specified in macro Risk_Est_PH_reg_time_dep parameter calc_vars.  This covariate has been added to the list in calc_vars.;
             %end;
         %end;
     %end;

%do k = 1 %to &num_cr.;
     %do i = 1 %to &&nvar_cr&k..;
        %if %sysfunc(varnum(&dsid.,%scan(&&vars_cr&k.., &i., %str( )))) ne 0 %then %do;
            %let in_calc_vars = 0;
            %do L = 1 %to &ncalc_var.;
  	       %if %scan(&&vars_cr&k.., &i., %str( )) eq &&calc_var&L.. %then %let in_calc_vars = 1;
	       %end;
            %if &in_calc_vars. = 0 %then %do;
	        %let calc_vars = &calc_vars. %scan(&&vars_cr&k.., &i., %str( ));
                %put WARNING : Apparent time-constant covariate %scan(&&vars_cr&k.., &i., %str( )) for endpoint &&time_cr&k.. was not specified in macro Risk_Est_PH_reg_time_dep parameter calc_vars.  This covariate has been added to the list in calc_vars.;
                %end;
            %end;
        %end;
     %end;

%let rc = %sysfunc(close(&dsid.));
  
%end;    


/**********************************************************************************************
**  Get the input data.  Create a version of the censoring variables where 1 = event and 0 =  *
**  no event.  (This will be needed later on.)                                                *
**********************************************************************************************/

data _qq_indata;
     set &indsn.;
     dummyby = 1;  /* Dummy by variable for merges */


/* Add a variable that is always 0 in case no predictors were specified */
/* for the competing risk event.                                        **/

     qqzero = 0;  

     
/* Delete any observation that does not have values for the time-to-event variables. */
     if &time_main. = . or &censor_main. = . 
%do k = 1 %to &num_cr.;
        or &&time_cr&k.. = . or &&censor_cr&k.. = . 
%end;
        then delete;
        
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

     if &censor_main. in (&censorlist_main.) then qqeventvar_main = 0;
                                             else qqeventvar_main = 1;

%do k = 1 %to &num_cr.;

     if &&censor_cr&k.. in (&&censorlist_cr&k..) then qqeventvar_cr&k. = 0;
                                                 else qqeventvar_cr&k. = 1;
                                                 
%end;

/* Create weight variable that defaults to 1 if no weights are specified */

%if %length(&weight.)> 0 %then %do; qqweight = &weight.; %end;
                         %else %do; qqweight = 1; %end;

/* Delete any observation that has 0 weight */
     if qqweight = 0 then delete;    
run;

/********************************************************************************************/
/**  If by variables or stratification variables are used, sort input data set by them.    **/
/********************************************************************************************/

%if &byflag. = 1 or %length(&strata.) %then %do;

proc sort data=_qq_indata;
      by &byvar. &strata.;
run;

%end;

/********************************************************************************************/
/**  Censor the occurrence of each event type if any other event type occurred first or    **/
/**  if follow-up for another event was censored first (applying the time window after     **/
/**  censored main events).  If all event types are censored, use the minimum of the       **/
/**  censoring times for all.                                                              **/
/********************************************************************************************/

data _qq_indata;
    set _qq_indata;

    qqtmm = &time_main.;
    qqevm = qqeventvar_main;
%do k = 1 %to &num_cr.;
    qqtmc&k. = &&time_cr&k..;
    qqevc&k. = qqeventvar_cr&k.;
%end;
    cr_first = 0;

%do k = 1 %to &num_cr.;

    if &&time_cr&k.. < &time_main. then do;
        qqtmm = min(qqtmm,&&time_cr&k..);
        qqevm = 0;
        cr_first = 1;
        end;

%do L = 1 %to &num_cr.;

    if qqeventvar_cr&L. = 1 and &&time_cr&L.. < &&time_cr&k.. then do;
        qqtmc&k. = min(qqtmc&k.,&&time_cr&L..);
        qqevc&k. = 0;
        end;
        
%end;    

%end;


    if qqeventvar_main = 0 then do;
%do k = 1 %to &num_cr.;
         if &time_main. + &window_main. < &&time_cr&k.. then do;
             qqtmc&k. = min(qqtmc&k.,&time_main.+&window_main.);
             qqevc&k. = 0;
             end;
%end;
         end;
         
    if qqeventvar_main = 1 then do;
%do k = 1 %to &num_cr.;
         if &time_main. < &&time_cr&k.. then do;
             qqtmc&k. = min(qqtmc&k.,&time_main.);
             qqevc&k. = 0;
             end;
%end;
         end;
    
    if cr_first then do;
        mintime = min(&time_main. %do k = 1 %to &num_cr.; , &&time_cr&k.. %end;);
        qqtmm = mintime;
        qqeventvar_main = 0;
%do k = 1 %to &num_cr.;
        if &&time_cr&k.. > mintime then do;
            qqtmc&k. = mintime;
            qqeventvar_cr&k. = 0;
            end;
%end;
        end;
        
    if qqeventvar_main = 0 %do k = 1 %to &num_cr.; and qqeventvar_cr&k. = 0 %end; 
              then do;
        mintime = min(&time_main. + &window_main. %do k = 1 %to &num_cr.; , &&time_cr&k.. %end;);
%do k = 1 %to &num_cr.;
        &&time_cr&k.. = mintime;
%end;
        &time_main. = min(&time_main.,mintime);
        end;
     else do;
        &time_main. = qqtmm;
        qqeventvar_main = qqevm;
%do k = 1 %to &num_cr.;
        &&time_cr&k.. = qqtmc&k.;
        qqeventvar_cr&k. = qqevc&k.;
%end;
        end;
    
     drop mintime qqtmm qqevm %do k = 1 %to &num_cr.; qqtmc&k. qqevc&k. %end;;
run;

/**********************************************************************************************
**  Run the proportional hazards regression analysis for the event of interest.  If a weight  *
**  is specified, use it in the analysis and use the covariance sandwich estimate of the      *
**  covariance matrix.                                                                        *
**********************************************************************************************/

%if %length(&weight.) = 0 %then %do;

/**********************************************************************************************
**  Case 1: no cohort sampling.                                                              **
**********************************************************************************************/

proc phreg data=_qq_indata outest=_qq_outest covout %if &robust. = yes %then covs;
           %if &print. = no %then noprint;;
     by &byvar.;
     model &time_main.*qqeventvar_main(0) = %do i = 1 %to &nvar_main.; &&var_main&i.. %end; / covb ties=efron rl;
%if %length(&strata.) %then %do;
     strata &strata.;
%end;
%if %length(&programming_statements.) %then %do;
     &programming_time. = &time_main.;
     &programming_statements.;
%end;
run;
%end;  
                         %else %do;

/**********************************************************************************************
**  Case 2: Cohort sampling design with analysis using Lin and Wei covariance sandwich       **
**  method.                                                                                  **
**********************************************************************************************/

proc phreg data=_qq_indata covs outest=_qq_outest covout %if &print. = no %then noprint;;
     by &byvar.;
     model &time_main.*qqeventvar_main(0) = %do i = 1 %to &nvar_main.; &&var_main&i.. %end; / covb ties=efron rl;
     weight qqweight;
%if %length(&strata.) %then %do;
     strata &strata.;
%end;
%if %length(&programming_statements.) %then %do;
     &programming_time. = &time_main.;
     &programming_statements.;
%end;
run;

%end;

/**********************************************************************************************
**  Capture the variance-covariance matrix of the parameter estimates for the event of        *
**  interest.                                                                                 *
**********************************************************************************************/

data _qq_v_main;
      array covariate(&nvar_main.) %do i = 1 %to &nvar_main.; &&var_main&i.. %end;; 
      array v(&nvar_main.,&nvar_main.) v_main1-v_main&nvar_main_squared.;

      set _qq_outest;
      by &byvar.;
      where upcase(_NAME_) ne upcase("&time_main.");

/*  Initialize cov matrix to missing values */

      if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; 
        then do i = 1 to &nvar_main.;
               do j = 1 to &nvar_main.;
                  v(i,j) = .;
                  end;
               end;

%do i = 1 %to &nvar_main.;
      if upcase(_NAME_) = upcase("&&var_main&i..") then row = &i;
%end;
      
      if row ne . then do column = 1 to &nvar_main.;
          v(row,column) = covariate(column);
          end;

      if last.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or last.&&byvar&i.. %end; then output; 

      retain v_main1-v_main&nvar_main_squared.;   
      keep v_main1-v_main&nvar_main_squared. &byvar.;
run;


/**********************************************************************************************
**  Capture the regression parameter estimates for the event of interest.                    **
**********************************************************************************************/

data _qq_b_main;

      array b(&nvar_main.) b_main1-b_main&nvar_main.;
      array covariate(&nvar_main.) %do i = 1 %to &nvar_main.; &&var_main&i.. %end;; 

      set _qq_outest;
      by &byvar.;
      where upcase(_NAME_) = upcase("&time_main.");

/* Initialize regression parameter estimates to missing values */

      if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; 
        then do i = 1 to &nvar_main.;
               b(i) = .;
               end;

      do i = 1 to &nvar_main.;
        b(i) = covariate(i);
        end;  

      keep b_main1-b_main&nvar_main. &byvar.;
run;

/**********************************************************************************************
**  Run the proportional hazards regression analysis for each competing risk event.  If a     *
**  weight is specified, use it in the analysis and use the covariance sandwich estimate of   *
**  the covariance matrix.                                                                    *
**********************************************************************************************/

%do k = 1 %to &num_cr.;

%if &&crreg_flag&k. = 1 %then %do;

%if %length(&weight.) = 0 %then %do;

/**********************************************************************************************
**  Case 1: no cohort sampling.                                                              **
**********************************************************************************************/

proc phreg data=_qq_indata outest=_qq_outest covout %if &robust. = yes %then covs;
           %if &print. = no %then noprint;;
     by &byvar.;
     model &&time_cr&k..*qqeventvar_cr&k.(0) = %do i = 1 %to &&nvar_cr&k..; %scan(&&vars_cr&k.., &i., %str( )) %end; / covb ties=efron rl;
%if %length(&strata.) %then %do;
     strata &strata.;
%end;
%if %length(&programming_statements.) %then %do;
     &programming_time. = &&time_cr&k..;
     &programming_statements.;
%end;
run;
%end;  
                         %else %do;

/**********************************************************************************************
**  Case 2: Cohort sampling design with analysis using Lin and Wei covariance sandwich       **
**  method.                                                                                  **
**********************************************************************************************/

proc phreg data=_qq_indata covs outest=_qq_outest covout %if &print. = no %then noprint;;
     by &byvar.;
     model &&time_cr&k..*qqeventvar_cr&k.(0) = %do i = 1 %to &&nvar_cr&k..; %scan(&&vars_cr&k.., &i., %str( )) %end; / covb ties=efron rl;
     weight qqweight;
%if %length(&strata.) %then %do;
     strata &strata.;
%end;
%if %length(&programming_statements.) %then %do;
     &programming_time. = &&time_cr&k..;
     &programming_statements.;
%end;
run;

%end;

/**********************************************************************************************
**  Capture the variance-covariance matrix of the parameter estimates for the competing risk  *
**  events.                                                                                   *
**********************************************************************************************/

data _qq_v_cr&k.;
      array covariate(&&nvar_cr&k..) %do i = 1 %to &&nvar_cr&k..; %scan(&&vars_cr&k.., &i., %str( )) %end;; 
      array v(&&nvar_cr&k..,&&nvar_cr&k..) v_cr&k._1-v_cr&k._&&nvar_cr&k._squared.;

      set _qq_outest;
      by &byvar.;
      where upcase(_NAME_) ne upcase("&&time_cr&k..");

/*  Initialize cov matrix to missing values */

      if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; 
        then do i = 1 to &&nvar_cr&k..;
               do j = 1 to &&nvar_cr&k..;
                  v(i,j) = .;
                  end;
               end;

%do i = 1 %to &&nvar_cr&k..;
      if upcase(_NAME_) = upcase("%scan(&&vars_cr&k.., &i., %str( ))") then row = &i.;   
%end;
      
      if row ne . then do column = 1 to &&nvar_cr&k..;
          v(row,column) = covariate(column);
          end;

      if last.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or last.&&byvar&i.. %end; then output; 

      retain v_cr&k._1-v_cr&k._&&nvar_cr&k._squared.;   
      keep v_cr&k._1-v_cr&k._&&nvar_cr&k._squared. &byvar.;
run;


/**********************************************************************************************
**  Capture the regression parameter estimates for the competing risk event.                 **
**********************************************************************************************/

data _qq_b_cr&k.;
      array b(&&nvar_cr&k..) b_cr&k._1-b_cr&k._&&nvar_cr&k..;
      array covariate(&&nvar_cr&k..) %do i = 1 %to &&nvar_cr&k..; %scan(&&vars_cr&k.., &i., %str( )) %end;;   

      set _qq_outest;
      by &byvar.;
      where upcase(_NAME_) = upcase("&&time_cr&k..");

/* Initialize regression parameter estimates to missing values */

      if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; 
        then do i = 1 to &&nvar_cr&k..;
               b(i) = .;
               end;

      do i = 1 to &&nvar_cr&k..;
        b(i) = covariate(i);
        end;  

      keep b_cr&k._1-b_cr&k._&&nvar_cr&k.. &byvar.;
run;

%end;

/**********************************************************************************************
**  If no covariates were specified for the competing risk event, substitute 0's for the     **
**  parameter estimate and its estimated variance.                                           **
**********************************************************************************************/

%else %do;

proc sort data=_qq_indata(keep=&byvar.) out=_qq_byvar nodupkeys;
     by &byvar.;
run;

data _qq_b_cr&k.;
     set _qq_byvar;
     b_cr&k._1 = 0;
run;

data _qq_v_cr&k.;
     set _qq_byvar;
     v_cr&k._1 = 0;
run;

%end;

%end;

/*********************************************************************************************/
/*  Sort observations by time of the event of interest.  If events have same time as         */
/*  censored observations, put events first so that the censored observations remain in the  */
/*  risk set for that particular event                                                       */
/*********************************************************************************************/

proc sort data=_qq_indata
            (keep = &byvar. &strata. &time_main. qqeventvar_main &calc_vars. qqweight)
            out=_qq_eventtime_main;
     by &byvar. &strata. &time_main. descending qqeventvar_main; 
run;
 
/*********************************************************************************************/
/**  Handle the ties in event times using the Efron approach. For each k-way tie, this can  **/
/**  be accomplished by substituting k observations with weight equal to the average weight **/
/**  and risk score equal to the average risk score at the observation time.                **/
/*********************************************************************************************/

data _qq_eventtime_main_ties;
     merge _qq_eventtime_main _qq_b_main;
     by &byvar.;

%if %length(&programming_statements.) %then %do;     
     &programming_time. = &time_main.;
     &programming_statements.;
%end;

     xbeta = 0;             
%do i = 1 %to &nvar_main.;
     xbeta = xbeta + b_main&i. * &&var_main&i..;
%end;
     exp_xbeta = exp(xbeta);
     
     drop b_main1-b_main&nvar_main.;
run;

proc univariate data=_qq_eventtime_main_ties noprint;
     by &byvar. &strata. &time_main. descending qqeventvar_main;
     where qqeventvar_main = 1;
     var qqweight;
     output out=_qq_tie_mean_wt_main mean = mean_weight_main;
run;

proc univariate data=_qq_eventtime_main_ties noprint;
     by &byvar. &strata. &time_main. descending qqeventvar_main;
     weight qqweight;
     where qqeventvar_main = 1;
     var exp_xbeta;
     output out=_qq_tie_mean_wt_exp_xbeta_main mean = mean_wt_exp_xbeta_main;
run;

data _qq_eventtime_main;
     merge _qq_eventtime_main _qq_tie_mean_wt_main _qq_tie_mean_wt_exp_xbeta_main;
     by &byvar. &strata. &time_main. descending qqeventvar_main;   
run;  

data _qq_tie_ind_main;
     set _qq_eventtime_main;
     
     lag_&time_main. = lag(&time_main.);
     lag_qqeventvar_main = lag(qqeventvar_main);
     
     if qqeventvar_main = 1 and lag_qqeventvar_main = 1 and &time_main. = lag_&time_main. then output;
     
     keep &byvar. &strata. &time_main. qqeventvar_main;
run;

data _qq_eventtime_main;
     merge _qq_eventtime_main _qq_tie_ind_main(in=a);
     by &byvar. &strata. &time_main. descending qqeventvar_main;        
     if a then tie_ind_main = 1;
          else tie_ind_main = 0;
     recind_main = 1;
run;

/*********************************************************************************************/
/*  Sort observations by time of each competing risk event.  If events have same time as     */
/*  censored observations, put events first so that the censored observations remain in the  */
/*  risk set for that particular event                                                       */
/*********************************************************************************************/

%do k = 1 %to &num_cr.;

proc sort data=_qq_indata
            (keep = &byvar. &strata. &&time_cr&k.. qqeventvar_cr&k. &calc_vars. qqweight)
            out=_qq_eventtime_cr&k.;
     by &byvar. &strata. &&time_cr&k.. descending qqeventvar_cr&k.; 
run;
 
/*********************************************************************************************/
/**  Handle the ties in event times using the Efron approach. For each k-way tie, this can  **/
/**  be accomplished by substituting k observations with weight equal to the average weight **/
/**  and risk score equal to the average risk score at the observation time.                **/
/*********************************************************************************************/

data _qq_eventtime_cr&k._ties;
     merge _qq_eventtime_cr&k. _qq_b_cr&k.;
     by &byvar.;

%if %length(&programming_statements.) %then %do;     
     &programming_time. = &&time_cr&k..;
     &programming_statements.;
%end;

     xbeta = 0;             
%do i = 1 %to &&nvar_cr&k..;
     xbeta = xbeta + b_cr&k._&i. * %scan(&&vars_cr&k.., &i., %str( ));
%end;
     exp_xbeta = exp(xbeta);
     
     drop b_cr&k._1-b_cr&k._&&nvar_cr&k..;
run;

proc univariate data=_qq_eventtime_cr&k._ties noprint;
     by &byvar. &strata. &&time_cr&k.. descending qqeventvar_cr&k.;
     where qqeventvar_cr&k. = 1;
     var qqweight;
     output out=_qq_tie_mean_wt_cr&k. mean = mean_weight_cr&k.;
run;

proc univariate data=_qq_eventtime_cr&k._ties noprint;
     by &byvar. &strata. &&time_cr&k.. descending qqeventvar_cr&k.;
     weight qqweight;
     where qqeventvar_cr&k. = 1;
     var exp_xbeta;
     output out=_qq_tie_mean_wt_exp_xbeta_cr&k. mean = mean_wt_exp_xbeta_cr&k.;
run;

data _qq_eventtime_cr&k.;
     merge _qq_eventtime_cr&k. _qq_tie_mean_wt_cr&k. _qq_tie_mean_wt_exp_xbeta_cr&k.;
     by &byvar. &strata. &&time_cr&k.. descending qqeventvar_cr&k.;   
run;  

data _qq_tie_ind_cr&k.;
     set _qq_eventtime_cr&k.;
     
     lag_&&time_cr&k.. = lag(&&time_cr&k..);
     lag_qqeventvar_cr&k. = lag(qqeventvar_cr&k.);
     
     if qqeventvar_cr&k. = 1 and lag_qqeventvar_cr&k. = 1 and &&time_cr&k.. = lag_&&time_cr&k.. then output;
     
     keep &byvar. &strata. &&time_cr&k.. qqeventvar_cr&k.;
run;

data _qq_eventtime_cr&k.;
     merge _qq_eventtime_cr&k. _qq_tie_ind_cr&k.(in=a);
     by &byvar. &strata. &&time_cr&k.. descending qqeventvar_cr&k.;        
     if a then tie_ind_cr&k. = 1;
          else tie_ind_cr&k. = 0;
     recind_cr&k. = 1;
run;

%end;  

/*********************************************************************************************/
/** Combine the event time files for the event of interest and the competing risk event.    **/
/*********************************************************************************************/

data _qq_event_time;
     set _qq_eventtime_main (rename = (&time_main.   = qqtime)) 
%do k = 1 %to &num_cr.;
         _qq_eventtime_cr&k.(rename = (&&time_cr&k.. = qqtime))
%end;;
 
     if recind_main  ne 1 then recind_main  = 0;
%do k = 1 %to &num_cr.;
     if recind_cr&k. ne 1 then recind_cr&k. = 0;
%end;
run;

proc sort data=_qq_event_time;
     by &byvar. &strata. qqtime descending qqeventvar_main %do k = 1 %to &num_cr.; descending qqeventvar_cr&k. %end;;
run;

/*********************************************************************************************/
/*  Count the observations in each stratum and by group.                                    **/
/*********************************************************************************************/

proc univariate data=_qq_event_time noprint;
     by &byvar. &strata.;
     var qqtime;
     output out = _qq_nobs n = npt;
run;


/*********************************************************************************************/
/** Produce a horizontal data set with the observation times, event indicators,             **/
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
%let dpc_var_main = %sysevalf(&maxnpt.*&nvar_main.,integer);
%let dpc_var_main = %cmpres(&dpc_var_main.);

%do k = 1 %to &num_cr.;
%local dpc_var_cr&k.;
%let dpc_var_cr&k. = %sysevalf(&maxnpt.*&&nvar_cr&k..,integer);
%let dpc_var_cr&k. = %cmpres(&&dpc_var_cr&k..);
%end;

data _qq_horiz;
 
     array ptweight(&maxnpt.) ptweight1-ptweight&maxnpt.;
     array ptcovariate(&ncalc_var.,&maxnpt.) ptcovariate1-ptcovariate&dpc_calc_var.;
     array pt_tie_wt_exp_xbeta_main(&maxnpt.) pt_tie_wt_exp_xbeta_main1-pt_tie_wt_exp_xbeta_main&maxnpt.;
     array pt_tie_wt_main(&maxnpt.) pt_tie_wt_main1-pt_tie_wt_main&maxnpt.; 
     array pt_tie_ind_main(&maxnpt.) pt_tie_ind_main1-pt_tie_ind_main&maxnpt.; 
     array pt_recind_main(&maxnpt.) pt_recind_main1-pt_recind_main&maxnpt.;
     array pttime(&maxnpt.) pttime1-pttime&maxnpt.;
     array ptevent_main(&maxnpt.) ptevent_main1-ptevent_main&maxnpt.;
%do k = 1 %to &num_cr.;
     array pt_tie_wt_exp_xbeta_cr&k.(&maxnpt.) pt_tie_wt_exp_xbeta_cr&k._1-pt_tie_wt_exp_xbeta_cr&k._&maxnpt.; 
     array ptevent_cr&k.(&maxnpt.) ptevent_cr&k._1-ptevent_cr&k._&maxnpt.;
     array pt_tie_wt_cr&k.(&maxnpt.) pt_tie_wt_cr&k._1-pt_tie_wt_cr&k._&maxnpt.;
     array pt_tie_ind_cr&k.(&maxnpt.) pt_tie_ind_cr&k._1-pt_tie_ind_cr&k._&maxnpt.; 
     array pt_recind_cr&k.(&maxnpt.) pt_recind_cr&k._1-pt_recind_cr&k._&maxnpt.;
%end;
     
     merge _qq_event_time _qq_nobs; 
     by &byvar. &strata.;      
   
     if first.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or first.&&byvar&i.. %end; 
                            %if %length(&strata.) %then %do i = 1 %to &nstrata.; or first.&&strat&i.. %end;
        then  do i = 1 to &maxnpt.;
            pttime(i) = .;
            ptweight(i) = .;
%do i = 1 %to &ncalc_var.;
            ptcovariate(&i.,i) = .;
%end;
            ptevent_main(i) = .;
            pt_tie_wt_exp_xbeta_main(i) = .;
            pt_tie_wt_main(i) = .;
%do k = 1 %to &num_cr.;
            ptevent_cr&k.(i) = .;
            pt_tie_wt_exp_xbeta_cr&k.(i) = .;
            pt_tie_wt_cr&k.(i) = .;
%end;
            index = 0;
            end;
        
     index = index + 1;
     pttime(index) = qqtime;
     ptevent_main(index) = qqeventvar_main;
     pt_tie_wt_exp_xbeta_main(index) = mean_wt_exp_xbeta_main;
     pt_tie_wt_main(index) = mean_weight_main;
     pt_tie_ind_main(index) = tie_ind_main;
     pt_recind_main(index) = recind_main;
%do k = 1 %to &num_cr.;
     pt_tie_wt_exp_xbeta_cr&k.(index) = mean_wt_exp_xbeta_cr&k.;
     ptevent_cr&k.(index) = qqeventvar_cr&k.;
     pt_tie_wt_cr&k.(index) = mean_weight_cr&k.; 
     pt_tie_ind_cr&k.(index) = tie_ind_cr&k.;
     pt_recind_cr&k.(index) = recind_cr&k.;
%end;
     ptweight(index) = qqweight;
%do i = 1 %to &ncalc_var.;
     ptcovariate(&i.,index) = &&calc_var&i..;
%end;

     if last.&byvar1. %if &nbyvar. > 1 %then %do i = 2 %to &nbyvar.; or last.&&byvar&i.. %end; 
                            %if %length(&strata.) %then %do i = 1 %to &nstrata.; or last.&&strat&i.. %end;
        then output;

     retain index pttime1-pttime&maxnpt. ptevent_main1-ptevent_main&maxnpt.  
            ptweight1-ptweight&maxnpt.   ptcovariate1-ptcovariate&dpc_calc_var. 
            pt_tie_wt_exp_xbeta_main1-pt_tie_wt_exp_xbeta_main&maxnpt. 
            pt_tie_ind_main1-pt_tie_ind_main&maxnpt.  
            pt_tie_wt_main1-pt_tie_wt_main&maxnpt. 
            pt_recind_main1-pt_recind_main&maxnpt. 
%do k = 1 %to &num_cr.;
            ptevent_cr&k._1-ptevent_cr&k._&maxnpt.
            pt_tie_ind_cr&k._1-pt_tie_ind_cr&k._&maxnpt.
            pt_tie_wt_exp_xbeta_cr&k._1-pt_tie_wt_exp_xbeta_cr&k._&maxnpt.
            pt_tie_wt_cr&k._1-pt_tie_wt_cr&k._&maxnpt.
            pt_recind_cr&k._1-pt_recind_cr&k._&maxnpt.
%end;;

     keep   &byvar. &strata. pttime1-pttime&maxnpt. ptevent_main1-ptevent_main&maxnpt.  
            ptweight1-ptweight&maxnpt.   ptcovariate1-ptcovariate&dpc_calc_var. 
            pt_tie_wt_exp_xbeta_main1-pt_tie_wt_exp_xbeta_main&maxnpt. 
            pt_tie_ind_main1-pt_tie_ind_main&maxnpt.  
            pt_tie_wt_main1-pt_tie_wt_main&maxnpt. 
            pt_recind_main1-pt_recind_main&maxnpt. 
%do k = 1 %to &num_cr.;
            ptevent_cr&k._1-ptevent_cr&k._&maxnpt.
            pt_tie_ind_cr&k._1-pt_tie_ind_cr&k._&maxnpt.
            pt_tie_wt_exp_xbeta_cr&k._1-pt_tie_wt_exp_xbeta_cr&k._&maxnpt.
            pt_tie_wt_cr&k._1-pt_tie_wt_cr&k._&maxnpt.
            pt_recind_cr&k._1-pt_recind_cr&k._&maxnpt.
%end;;
run;

/*********************************************************************************************/
/**  Calculate the baseline hazard estimates and the weighted mean covariate values in each **/
/**  risk set.                                                                              **/
/*********************************************************************************************/

**  Create versions of the regression parameter data sets that have a record for each stratum
**  within each by group;

proc sort data=_qq_indata out=_qq_bystrat nodupkeys;
     by &byvar. &strata.;
run;

data _qq_b_main;
     merge _qq_b_main _qq_bystrat;
     by &byvar.;
run;

%do k = 1 %to &num_cr.;

data _qq_b_cr&k.;
     merge _qq_b_cr&k. _qq_bystrat;
     by &byvar.;
run;

%end;

data _qq_horiz;

     array pttime(&maxnpt.) pttime1-pttime&maxnpt.;
     array ptweight(&maxnpt.) ptweight1-ptweight&maxnpt.;
     array ptcovariate(&ncalc_var.,&maxnpt.) ptcovariate1-ptcovariate&dpc_calc_var.;
     array pt_tie_wt_exp_xbeta_main(&maxnpt.) pt_tie_wt_exp_xbeta_main1-pt_tie_wt_exp_xbeta_main&maxnpt.;
     array pt_tie_wt_main(&maxnpt.) pt_tie_wt_main1-pt_tie_wt_main&maxnpt.; 
     array pt_tie_ind_main(&maxnpt.) pt_tie_ind_main1-pt_tie_ind_main&maxnpt.; 
     array pt_recind_main(&maxnpt.) pt_recind_main1-pt_recind_main&maxnpt.;
     array ptevent_main(&maxnpt.) ptevent_main1-ptevent_main&maxnpt.;
     array ptblhaz_main(&maxnpt.) ptblhaz_main1-ptblhaz_main&maxnpt.;
     array ptcovmean_main(&nvar_main.,&maxnpt.) ptcovmean_main1-ptcovmean_main&dpc_var_main.;
%do k = 1 %to &num_cr.;
     array ptevent_cr&k.(&maxnpt.) ptevent_cr&k._1-ptevent_cr&k._&maxnpt.;
     array pt_tie_wt_cr&k.(&maxnpt.) pt_tie_wt_cr&k._1-pt_tie_wt_cr&k._&maxnpt.;
     array pt_tie_ind_cr&k.(&maxnpt.) pt_tie_ind_cr&k._1-pt_tie_ind_cr&k._&maxnpt.; 
     array pt_tie_wt_exp_xbeta_cr&k.(&maxnpt.) pt_tie_wt_exp_xbeta_cr&k._1-pt_tie_wt_exp_xbeta_cr&k._&maxnpt.; 
     array pt_recind_cr&k.(&maxnpt.) pt_recind_cr&k._1-pt_recind_cr&k._&maxnpt.;
     array ptblhaz_cr&k.(&maxnpt.) ptblhaz_cr&k._1-ptblhaz_cr&k._&maxnpt.;
     array ptcovmean_cr&k.(&&nvar_cr&k..,&maxnpt.) ptcovmean_cr&k._1-ptcovmean_cr&k._&&dpc_var_cr&k..;
%end;

     merge _qq_horiz  _qq_b_main %do k = 1 %to &num_cr.; _qq_b_cr&k. %end; _qq_nobs;
     by &byvar. &strata.;

/**************************************************/
/**  Baseline hazard and weighted mean covariate **/
/**  for the event of interest.                  **/
/**************************************************/

     do index = 1 to npt;     
    
         denominator = 0;
%do i = 1 %to &nvar_main.;
         cov_sum&i. = 0;
%end;

%if %length(&programming_statements.) %then %do;
         &programming_time. = pttime(index);
%end;
         
         do pt = index to npt;

%do i = 1 %to &ncalc_var.;
            &&calc_var&i.. = ptcovariate(&i.,pt);
%end;

%if %length(&programming_statements.) %then %do;
            &programming_statements.;
%end;
             
             if pt_tie_ind_main(pt) = 1 then do;
                exp_xbeta_main = pt_tie_wt_exp_xbeta_main(pt);
                qqweight = pt_tie_wt_main(pt);
                end;
             
             else do;
                xbeta_pt = 0;             
%do i = 1 %to &nvar_main.;
                xbeta_pt = xbeta_pt + b_main&i. * &&var_main&i..;
%end;
                exp_xbeta_main = exp(xbeta_pt);
                qqweight = ptweight(pt);
                end;

             denominator = denominator + pt_recind_main(pt) * qqweight * exp_xbeta_main;
%do i = 1 %to &nvar_main.;
             cov_sum&i. = cov_sum&i. + pt_recind_main(pt) * qqweight * &&var_main&i.. * exp_xbeta_main;
%end;
             end;

         if denominator > 0 then do;        
             if pt_tie_ind_main(index) = 1  then do;
                ptblhaz_main(index) = pt_recind_main(index) * ptevent_main(index) * pt_tie_wt_main(index) / denominator;
                end;
             else do; 
                ptblhaz_main(index) = pt_recind_main(index) * ptevent_main(index) * ptweight(index)  / denominator;
                end;
             end;
             
         else do; 
             ptblhaz_main(index) = 0;
             end;
             
         if ptblhaz_main(index) = . then ptblhaz_main(index) = 0; **  Fill in 0 if there was no event;
         
%do i = 1 %to &nvar_main.;
         if denominator > 0 then ptcovmean_main(&i.,index) = cov_sum&i. / denominator;
%end;
         end;

/**************************************************/
/**  Baseline hazard and weighted mean covariate **/
/**  for each competing risk event.              **/
/**************************************************/

%do k = 1 %to &num_cr.;

     do index = 1 to npt;     
    
         denominator = 0;
%do i = 1 %to &&nvar_cr&k..;
         cov_sum&i. = 0;
%end;

%if %length(&programming_statements.) %then %do;
         &programming_time. = pttime(index);
%end;

         do pt = index to npt;

%do i = 1 %to &ncalc_var.;
             &&calc_var&i.. = ptcovariate(&i.,pt);
%end;

%if %length(&programming_statements.) %then %do;
             &programming_statements.;
%end;
             
             if pt_tie_ind_cr&k.(pt) = 1 then do;
                exp_xbeta_cr&k. = pt_tie_wt_exp_xbeta_cr&k.(pt);
                qqweight = pt_tie_wt_cr&k.(pt);
                end;
             
             else do;
                xbeta_pt = 0;             
%do i = 1 %to &&nvar_cr&k..;
                xbeta_pt = xbeta_pt + b_cr&k._&i. * %scan(&&vars_cr&k.., &i., %str( ));
%end;
                exp_xbeta_cr&k. = exp(xbeta_pt);
                qqweight = ptweight(pt);
                end;

             denominator = denominator + pt_recind_cr&k.(pt) * qqweight * exp_xbeta_cr&k.;
%do i = 1 %to &&nvar_cr&k..;
             cov_sum&i. = cov_sum&i. + pt_recind_cr&k.(pt) * qqweight * %scan(&&vars_cr&k.., &i., %str( )) * exp_xbeta_cr&k.;
%end;
             end;

         if denominator > 0 then do;
             if pt_tie_ind_cr&k.(index) = 1  then do;
                ptblhaz_cr&k.(index) = pt_recind_cr&k.(index) * ptevent_cr&k.(index) * pt_tie_wt_cr&k.(index) / denominator;
                end;
             else do; 
                ptblhaz_cr&k.(index) = pt_recind_cr&k.(index) * ptevent_cr&k.(index) * ptweight(index)  / denominator;
                end;
             end;
             
         else do;
             ptblhaz_cr&k.(index) = 0;
             end;
             
         if ptblhaz_cr&k.(index) = . then ptblhaz_cr&k.(index) = 0;  **  Fill in 0 if there was no event;
         
%do i = 1 %to &&nvar_cr&k..;
         if denominator > 0 then ptcovmean_cr&k.(&i.,index) = cov_sum&i. / denominator;
%end;
         end;         

%end;

     keep &byvar. &strata. npt 
          pttime1-pttime&maxnpt. ptevent_main1-ptevent_main&maxnpt. 
          ptweight1-ptweight&maxnpt. ptcovariate1-ptcovariate&dpc_calc_var. 
          ptblhaz_main1-ptblhaz_main&maxnpt. 
          ptcovmean_main1-ptcovmean_main&dpc_var_main. 
%do k = 1 %to &num_cr.;
          ptevent_cr&k._1-ptevent_cr&k._&maxnpt.
          ptblhaz_cr&k._1-ptblhaz_cr&k._&maxnpt.
          ptcovmean_cr&k._1-ptcovmean_cr&k._&&dpc_var_cr&k..
%end;;
run;

/***********************************************************************************************
**  Combine regression parameter estimates and covariance matrix with the horizontal file.    **
***********************************************************************************************/

data _qq_keys;
     merge _qq_v_main _qq_b_main %do k = 1 %to &num_cr.; _qq_v_cr&k. _qq_b_cr&k. %end;;
     by &byvar.;
     keep &byvar. &strata. 
          b_main1-b_main&nvar_main. v_main1-v_main&nvar_main_squared.
%do k = 1 %to &num_cr.;
          b_cr&k._1-b_cr&k._&&nvar_cr&k.. v_cr&k._1-v_cr&k._&&nvar_cr&k._squared.        
%end;;
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
%if &crreg_flag. = 0 %then %do;
     qqzero = 0;
%end;

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
     array v_main(&nvar_main.,&nvar_main.) v_main1-v_main&nvar_main_squared.;
     array pttime(&maxnpt.) pttime1-pttime&maxnpt.;
     array ptevent_main(&maxnpt.) ptevent_main1-ptevent_main&maxnpt.;
     array ptweight(&maxnpt.) ptweight1-ptweight&maxnpt.;
     array ptcovariate(&ncalc_var.,&maxnpt.) ptcovariate1-ptcovariate&dpc_calc_var.;
     array ptblhaz_main(&maxnpt.) ptblhaz_main1-ptblhaz_main&maxnpt.;
     array ptcovmean_main(&nvar_main.,&maxnpt.) ptcovmean_main1-ptcovmean_main&dpc_var_main.;
     array pthaz_main(&maxnpt.) pthaz_main1-pthaz_main&maxnpt.;
%do k = 1 %to &num_cr.;     
     array ptevent_cr&k.(&maxnpt.) ptevent_cr&k._1-ptevent_cr&k._&maxnpt.;
     array v_cr&k.(&&nvar_cr&k..,&&nvar_cr&k..) v_cr&k._1-v_cr&k._&&nvar_cr&k._squared.;
     array ptblhaz_cr&k.(&maxnpt.) ptblhaz_cr&k._1-ptblhaz_cr&k._&maxnpt.;
     array ptcovmean_cr&k.(&&nvar_cr&k..,&maxnpt.) ptcovmean_cr&k._1-ptcovmean_cr&k._&&dpc_var_cr&k..;
     array pthaz_cr&k.(&maxnpt.) pthaz_cr&k._1-pthaz_cr&k._&maxnpt.;
     array psi_cr&k.(&&nvar_cr&k..,&maxnpt.) psi_cr&k._1-psi_cr&k._&&dpc_var_cr&k..;
     array dCIFddN_cr&k.(&maxnpt.) dCIFddN_cr&k._1-dCIFddN_cr&k._&maxnpt.; 
%end;
     array surv(&maxnpt.) surv1-surv&maxnpt.;
     array psi_main(&nvar_main.,&maxnpt.) psi_main1-psi_main&dpc_var_main.;
     array dCIFddN_main(&maxnpt.) dCIFddN_main1-dCIFddN_main&maxnpt.; 

     merge _qq_horiz _qq_cov _qq_nobs;
     by &byvar. &strata.;

/**********************************************************************************************/
/**  Compute hazard estimates, overall survival function and psi functions for the specified **/
/**  combination of covariates.                                                              **/
/**********************************************************************************************/

     cumhaz_main = 0;
%do k = 1 %to &num_cr.;
     cumhaz_cr&k.   = 0;
%end;

%do i = 1 %to &nvar_main.;
     psi_runsum_main&i. = 0;
%end;

%do k = 1 %to &num_cr.;
%do i = 1 %to &&nvar_cr&k..;
     psi_runsum_cr&k._&i. = 0;
%end; 
%end;

     do index = 1 to npt; 

%if %length(&programming_statements.) %then %do;
        &programming_time. = pttime(index);  ** Compute the time-dependent xbeta for this patient;
        &programming_statements.;
%end;

        xbeta_main = 0;
%do i = 1 %to &nvar_main.;
        xbeta_main = xbeta_main + b_main&i. * &&var_main&i..;
%end; 

%do k = 1 %to &num_cr.;
        xbeta_cr&k.   = 0; 
%do i = 1 %to &&nvar_cr&k..;
        xbeta_cr&k. = xbeta_cr&k. + b_cr&k._&i. * %scan(&&vars_cr&k.., &i., %str( ));
%end;
%end;
 
        exp_xbeta_main = exp(xbeta_main);
%do k = 1 %to &num_cr.;
        exp_xbeta_cr&k. = exp(xbeta_cr&k.);
%end;

        pthaz_main(index) = exp_xbeta_main * ptblhaz_main(index);
%do k = 1 %to &num_cr.;
        pthaz_cr&k.(index) = exp_xbeta_cr&k. * ptblhaz_cr&k.(index);
%end;

        cumhaz_main = cumhaz_main + pthaz_main(index); 
%do k = 1 %to &num_cr.;
        cumhaz_cr&k.   = cumhaz_cr&k. + pthaz_cr&k.(index);
%end;

%do i = 1 %to &nvar_main.;
        psi_runsum_main&i. = psi_runsum_main&i. + (&&var_main&i.. - ptcovmean_main(&i.,index)) * pthaz_main(index);
%end;

%do k = 1 %to &num_cr.;
%do i = 1 %to &&nvar_cr&k..;
        psi_runsum_cr&k._&i. = psi_runsum_cr&k._&i. + (%scan(&&vars_cr&k.., &i., %str( )) - ptcovmean_cr&k.(&i.,index)) * pthaz_cr&k.(index);
%end;            
%end;

        surv(index) = exp(-cumhaz_main - sum(of cumhaz_cr1-cumhaz_cr&num_cr.));  
        
%do i = 1 %to &nvar_main.;
        psi_main(&i.,index) = psi_runsum_main&i.;
%end;

%do k = 1 %to &num_cr.;
%do i = 1 %to &&nvar_cr&k..;
        psi_cr&k.(&i.,index) = psi_runsum_cr&k._&i.;
%end;
%end;

        end;
        
/**********************************************************************************************/                   
/**  Compute the gradients of the CIF with respect to the regression parameter estimates.    **/
/**********************************************************************************************/       

%do i = 1 %to &nvar_main.;
     gradCIF_main&i. = 0;
%end;

%do k = 1 %to &num_cr.;
%do i = 1 %to &&nvar_cr&k..;
     gradCIF_cr&k._&i. = 0;
%end;
%end;

     do index = 1 to npt;
         if pttime(index) <= &risk_time. then do;       

%if %length(&programming_statements.) %then %do;
             &programming_time. = pttime(index);  ** Compute the time-dependent xbeta for this patient;
             &programming_statements.;
%end;

%do i = 1 %to &nvar_main.;
             gradCIF_main&i. = gradCIF_main&i. + surv(index) *
                      (&&var_main&i.. - ptcovmean_main(&i.,index) - psi_main(&i.,index)) * pthaz_main(index);
%end; 

%do k = 1 %to &num_cr.;
%do i = 1 %to &&nvar_cr&k..;
             gradCIF_cr&k._&i. = gradCIF_cr&k._&i. - surv(index) * psi_cr&k.(&i.,index) * pthaz_main(index);
%end;
%end;
             end;
         end;

/**********************************************************************************************/                   
/**  Compute the partial derivatives of the CIF with respect to the jumps in the cumulative  **/
/**  hazard functions.                                                                       **/
/**********************************************************************************************/
 
      do index = 1 to npt; 
      
          if ptevent_main(index) = 0 or pttime(index) >= &risk_time. then dCIFddN_main(index) = 0; 
          else do;
             
              dCddN_main = surv(index) * pthaz_main(index);
              
              do index_t = index to npt;
                 if pttime(index_t) <= &risk_time. then 
                     dCddN_main = dCddN_main - surv(index_t) * pthaz_main(index) * pthaz_main(index_t);
                 end;
                 
              dCIFddN_main(index) = dCddN_main;
              
              end;

%do k = 1 %to &num_cr.;

          if ptevent_cr&k.(index) = 0 or pttime(index) >= &risk_time. then dCIFddN_cr&k.(index) = 0; 
          else do;
             
              dCddN_cr&k. = 0;
              
              do index_t = index to npt;
                 if pttime(index_t) <= &risk_time. then 
                     dCddN_cr&k. = dCddN_cr&k. - surv(index_t) * pthaz_cr&k.(index) * pthaz_main(index_t);
                 end;
                 
              dCIFddN_cr&k.(index) = dCddN_cr&k.;
              
              end;

%end;
              
          end;
 
/**********************************************************************************************/                   
/**  Compute variance of cumulative incidence function estimate.                             **/
/**********************************************************************************************/
 
/**********************************************/
/** Event of interest regression parameters. **/
/**********************************************/

 %do i = 1 %to &nvar_main.;                 
     product&i. = 0;
 %end;
 
 %do i = 1 %to &nvar_main.;
 %do j = 1 %to &nvar_main.;
     product&i. = product&i. + v_main(&i.,&j.) * gradCIF_main&j.;
 %end;
 %end;
 
     quadform_main = 0;
 %do i = 1 %to &nvar_main.;
     quadform_main = quadform_main + gradCIF_main&i. * product&i.;
 %end;

/************************************************/
/** Competing risk event regression parameters **/
/************************************************/

%do k = 1 %to &num_cr.;
 
%do i = 1 %to &&nvar_cr&k..;                 
    product&i. = 0;
%end;
  
%do i = 1 %to &&nvar_cr&k..;
%do j = 1 %to &&nvar_cr&k..;
   product&i. = product&i. + v_cr&k.(&i.,&j.) * gradCIF_cr&k._&j.;
%end;
%end;
  
   quadform_cr&k. = 0;
%do i = 1 %to &&nvar_cr&k..;
   quadform_cr&k. = quadform_cr&k. + gradCIF_cr&k._&i. * product&i.;
%end;

%end;


/**********************************************/
/** Jumps in the cumulative hazard curves.   **/
/**********************************************/

    jump_var_main  = 0;
%do k = 1 %to &num_cr.;
    jump_var_cr&k. = 0;
%end;

    do index = 1 to npt;
         jump_var_main  = jump_var_main  + (dCIFddN_main(index)**2)  * max(0,ptevent_main(index));
%do k = 1 %to &num_cr.;
         jump_var_cr&k. = jump_var_cr&k. + (dCIFddN_cr&k.(index)**2) * max(0,ptevent_cr&k.(index));
%end;
         end;

/**********************************************/
/** Total variance.                          **/
/**********************************************/         

    var_CIF = quadform_main + sum(of quadform_cr1-quadform_cr&num_cr.) 
                        + jump_var_main + sum(of jump_var_cr1-jump_var_cr&num_cr.);
    SD_CIF = sqrt(var_CIF);

/*********************************************************************************************/
/**  Compute the risk estimate and confidence interval.                                     **/
/*********************************************************************************************/

%let conf = %sysevalf(100*(1-&alpha.),integer);

     &Risk. = 0;
     
     do index = 1 to npt; 
        if pttime(index) <= &risk_time. then &Risk. = &Risk. + surv(index) * pthaz_main(index);
        end;

%if %UPCASE(&CI_method.) = LINEAR or %UPCASE(&CI_method.) = LIN %then %do;

     &Risk_LCL. = &Risk. - probit(1-&alpha./2) * SD_CIF;
     &Risk_UCL. = &Risk. + probit(1-&alpha./2) * SD_CIF;; 

%end;

%else %if %UPCASE(&CI_method.) = LOG %then %do;

     if 0 < &Risk. < 1 then do;
          rho = -log(1-&Risk.); 
          SD_rho = SD_CIF / (1 - &Risk.);
          &Risk_LCL. = 1 - exp(- rho + probit(1-&alpha./2) * SD_rho);
          &Risk_UCL. = 1 - exp(- rho - probit(1-&alpha./2) * SD_rho); 
          end;

%end;

%else %do;

     if 0 < &Risk. < 1 then do;
          rho = log(-log(1-&Risk.)); 
          SD_rho = SD_CIF / abs( (1 - &Risk.) * log(1 - &Risk.) );
          &Risk_LCL. = 1 - exp(-exp(rho - probit(1-&alpha./2) * SD_rho));
          &Risk_UCL. = 1 - exp(-exp(rho + probit(1-&alpha./2) * SD_rho)); 
          end;

%end;

       label &Risk. = Estimated risk at time &risk_time.;
       label &Risk_LCL. = Lower bound of &conf. pct confidence interval for risk at time &risk_time.;
       label &Risk_UCL. = Upper bound of &conf. pct confidence interval for risk at time &risk_time.;
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
     merge _qq_cov _qq_riskest(keep = &byvar. unique_record_id &Risk. &Risk_LCL. &Risk_UCL.);
     by &byvar. unique_record_id;
     
     drop unique_record_id 
          %if &byflag. = 0 %then dummyby; 
          %if &crreg_flag. = 0 %then qqzero;;

run;

%mend Risk_Est_PH_reg_comp_risk;
