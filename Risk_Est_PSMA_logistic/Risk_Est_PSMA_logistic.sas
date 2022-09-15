/***********************************************************************************************************************

Program Name           : risk_est_PSMA_logistic

Path                   : 

Program Language       : SAS

Operating System       : Server

Purpose                : Macro to perform risk estimation using fixed-effects patient-specific meta-analysis and logistic  
                         regression with up to nine studies, accommodating studies with special populations.                       
                         
Notes                  : 

Status                 : Tested but not yet verified.

Run Dependencies       : None

Input Datasets/Views   : Data set containing covariate values at which to estimate the event risk.
                         Key statistics data sets for each study

Output Datasets/Views  : Data set containing risk estimates

Other Output           : None

Macro calls internal   : None

Macro calls external   : None

***********************************************************************************************************************/

%macro risk_est_PSMA_logistic(
  /* Input parameters */   covdsn=,common_vars=,num_studies=,
                           special_pop_inds_1=,special_pop_inds_2=,special_pop_inds_3=,
                           special_pop_inds_4=,special_pop_inds_5=,special_pop_inds_6=,
                           special_pop_inds_7=,special_pop_inds_8=,special_pop_inds_9=,
                           key_stats1=,key_stats2=,key_stats3=,
                           key_stats4=,key_stats5=,key_stats6=,
                           key_stats7=,key_stats8=,key_stats9=,
  /* Analysis settings */  alpha=0.05,
  /* Output parameters */  outdsn=,risk_est=risk_est,risk_LCL=risk_LCL,risk_UCL=risk_UCL  
                             );

/***********************************************************************************************
| DEFINITION OF PARAMETERS
|
| Name           : covdsn
| Required (Y/N) : Y
| Default Value  : (at temporary library)
| Type ($/#)     : $
| Purpose        : Name of data set containing covariate values at which to estimate the event risk.  
|                  This data set must contain the variables specified in parameters common_vars and special_pop_inds.
|                  The input data set may have multiple rows. 
|-----------------------------------------------------------------------------------------------
| Name           : common_vars
| Required (Y/N) : Y
| Default Value  :
| Type ($/#)     : $ or #
| Purpose        : List of covariates common to all studies that are used to estimate the risk of the event.      
|-----------------------------------------------------------------------------------------------
| Name           : num_studies
| Required (Y/N) : Y
| Default Value  :
| Type ($/#)     : #
| Purpose        : Constant giving the number of studies in the meta-analysis.
|-----------------------------------------------------------------------------------------------
| Name           : special_pop_inds_<k>, <k> = 1, 2, . . . &num_studies.
| Required (Y/N) : N
| Default Value  :
| Type ($/#)     : #
| Purpose        : Names of indicator functions for special populations, if any, in study <k>.      
|-----------------------------------------------------------------------------------------------
| Name           : key_stats<k>, <k> = 1, 2, . . . &num_studies.
| Required (Y/N) : N
| Default Value  :
| Type ($/#)     : #
| Purpose        : Input data set giving key summary statistics for study <k>.  This data set must contain
|                  the following variables:        
|                    beta_<var>, the Cox regression parameter estimate for each covariate <var> 
|                    v_<var1>_<var2>, the covariance of the regression parameter estimates for         
|                      each pair of covariates <var1> and <var2>.  "Intercept" must be one of the variables.
|-----------------------------------------------------------------------------------------------
| Name           : alpha        
| Required (Y/N) : N
| Default Value  : 0.05
| Type ($/#)     : #
| Purpose        : Type I error rate for confidence intervals.
|-----------------------------------------------------------------------------------------------
| Name           : outdsn
| Required (Y/N) : Y
| Default Value  : (at temporary library)
| Type ($/#)     : $
| Purpose        : Name of data set that will contain the risk estimates.                               
|-----------------------------------------------------------------------------------------------
| Name           : riskest
| Required (Y/N) : N
| Default Value  : riskest
| Type ($/#)     : $
| Purpose        : Name of data set variable that will contain the risk estimates.                               
|-----------------------------------------------------------------------------------------------
| Name           : risk_LCL
| Required (Y/N) : N
| Default Value  : risk_LCL
| Type ($/#)     : $
| Purpose        : Name of data set variable that will contain the lower limit of the confidence interval
                   for the risk.                               
|-----------------------------------------------------------------------------------------------
| Name           : risk_UCL
| Required (Y/N) : N
| Default Value  : risk_UCL
| Type ($/#)     : $
| Purpose        : Name of data set variable that will contain the lower limit of the confidence interval
                   for the risk.                               
|-----------------------------------------------------------------------------------------------
***********************************************************************************************************
Mod#    Date         Username    Test     Description
---     -------      --------    ----    -----------------------------------------------------------
000     20220912     mcrager             Initial version.
**********************************************************************************************************/;

%local t allvars nvar nvar2 nsp_ind ntotvar ntotvar2 
       ntotvar_1 ntotvar_2 ntotvar_3
       ntotvar_4 ntotvar_5 ntotvar_6
       ntotvar_7 ntotvar_8 ntotvar_9 
       nsp_ind_1 nsp_ind_2 nsp_ind_3 
       nsp_ind_4 nsp_ind_5 nsp_ind_6 
       nsp_ind_7 nsp_ind_8 nsp_ind_9 
       i j k L conf;

/*  Capture status of mergenoby option so it can be reset to its current value at the end of the macro */

proc optsave out=qqoptions;
run;

proc sql noprint;
    select OPTVALUE into :mergenobyoption
    from qqoptions where (OPTNAME = 'MERGENOBY');
quit;

options mergenoby=nowarn;


/* Check for errors in macro parameter specification */

   %if %length(&covdsn.)=0 %then %do;
      %put ERROR :  Risk_Est_PSMA_logistic macro parameter covdsn must be specified.;
      %abort;
   %end;

   %if %length(&common_vars.)=0 %then %do;
      %put ERROR :  Risk_Est_PSMA_logistic macro parameter common_vars must be specified.;
      %abort;
   %end;

   %if %length(&num_studies.)=0 %then %do;
      %put ERROR :  Risk_Est_PSMA_logistic macro parameter num_studies must be specified.;
      %abort;
   %end;

/*  Set default values of parameters not specified */

%if %length(&alpha.) = 0 %then %let alpha = 0.05;
%if %length(&risk_est.) = 0 %then %let risk_est = risk_est;
%if %length(&risk_LCL.) = 0 %then %let risk_LCL = risk_LCL;
%if %length(&risk_UCL.) = 0 %then %let risk_UCL = risk_UCL;


/**********************************************************************************************
**  Count the common covariates.  Add the intercept to the list of common covariates if not   *
**  already specified there.                                                                  *
**********************************************************************************************/

%if %index(%UPCASE(&common_vars.),INTERCEPT) = 0 %then %let common_vars = &common_vars. Intercept;

%let vars = %sysfunc(compbl(&common_vars.));

%let nvar = %sysfunc(countc(%bquote(&common_vars.), %str( )));
%let nvar = %sysfunc(ifc(%length(%bquote(&common_vars.)), %eval(&nvar. + 1), 0));

%let nvar2 = %sysevalf(&nvar.**2,integer); 

%do k = 1 %to &num_studies.;

/**********************************************************************************************
**  Count the special population indicators in each study and the total number of covariates **
**  in each study.                                                                           **
**********************************************************************************************/

%if %length(&&special_pop_inds_&k..) = 0 %then %let nsp_ind_&k. = 0;  
%else %do;

%let special_pop_inds_&k. = %sysfunc(compbl(&&special_pop_inds_&k..));

%let nsp_ind_&k. = %sysfunc(countc(%bquote(&&special_pop_inds_&k..), %str( )));
%let nsp_ind_&k. = %sysfunc(ifc(%length(%bquote(&&special_pop_inds_&k..)), %eval(&&nsp_ind_&k.. + 1), 0));

%end;

%let ntotvar_&k. = %sysevalf(&nvar. + &&nsp_ind_&k.,integer);

/**********************************************************************************************
**  Assemble the special population indicators across the studies and set flags for which     *
**  study has which.                                                                          *
**********************************************************************************************/
  
%do j = 1 %to &&nsp_ind_&k..;
   %local sp_ind_&k._&j.;
   %let sp_ind_&k._&j. = %scan(&&special_pop_inds_&k.., &j., %str( ));
%end;         

data qqspec_pop_ind_&k.;
    length indicator $ 200;
    indicator = ' ';
%do j = 1 %to &&nsp_ind_&k..;
    indicator = "&&sp_ind_&k._&j.";
    output;
%end;
run;

proc sort data=qqspec_pop_ind_&k.;
     by indicator;
run;

%end;

data qqspec_pop_ind;
     set 
%do k = 1 %to &num_studies.;
%if &&nsp_ind_&k.. > 0 %then qqspec_pop_ind_&k.;
%end;;
run;

proc sort data=qqspec_pop_ind nodupkeys;
     by indicator;
run;

proc sql noprint;
     select distinct indicator into :sp_ind_1-:sp_ind_99
     from qqspec_pop_ind;
run;

%let nsp_ind = &sqlobs.;

%let special_pop_inds = ;
%do j = 1 %to &nsp_ind.;
%let special_pop_inds = &special_pop_inds. &&sp_ind_&j..;
%end;

%let nsp_ind_xn = %sysevalf(&num_studies.*&nsp_ind.,integer);

data qqsp_ind_flag;
    array sp_ind_flag(&num_studies.,&nsp_ind.) spif1-spif&nsp_ind_xn.;
    merge qqspec_pop_ind end=eof
%do k = 1 %to &num_studies.;
          qqspec_pop_ind_&k.(in=a&k.)
%end;;
    by indicator;
    
    if indicator ne ' ' then do;
    
        index = index + 1;

%do k = 1 %to &num_studies.;
        sp_ind_flag(&k.,index) = a&k.;
%end;
        end;
        
    if eof then do;
        dummyby = 1;
        output;
        end;
    
    retain index 0 spif1-spif&nsp_ind_xn.;
    keep dummyby spif1-spif&nsp_ind_xn.;
run;  

/***********************************************************************************************/
/**  Combine the covariates and special population indicators into a single list.  Compute    **/
/**  the total number of variables in the combined list.  Parse the string and assign the     **/
/**  variable names to macro variables.                                                       **/
/***********************************************************************************************/

%let allvars = &vars. &special_pop_inds;

%let ntotvar =  %sysevalf(&nvar.+&nsp_ind.,integer);
%let ntotvar2 = %sysevalf(&ntotvar.**2,integer); 

%do i = 1 %to &ntotvar.;
   %local var&i.;
   %let var&i. = %scan(&allvars., &i., %str( ));
%end; 

/**********************************************************************************************
**  Check for nonexistent input data sets and nonexistent variables in the input data sets.   *
**********************************************************************************************/
 
  %let errcode = 0;
 
  %do k = 1 %to &num_studies.;
      %let dsid = %sysfunc(open(&&key_stats&k..,i));
      %if &dsid. = 0 %then %do;
         %put ERROR :  Risk_Est_PSMA_logistic macro found that specified key statistics data set &&key_stats&k... does not exist.;
         %abort;
         %end;
      %else %do;
         %do i = 1 %to &&ntotvar_&k..;
             %if %sysfunc(varnum(&dsid.,beta_&&var&i..)) = 0 %then %do;
                 %put ERROR :  Risk_Est_PSMA_logistic macro key statistics data set &&key_stats&k... does not contain the required variable beta_&&var&i..;
                 %let errcode = 1;
                 %end;

             %do j = 1 %to &&ntotvar_&k..;
                 %if %sysfunc(varnum(&dsid.,v_&&var&i.._&&var&j..)) = 0 %then %do;
	             %put ERROR :  Risk_Est_PSMA_logistic macro key statistics data set &&key_stats&k... does not contain the required variable v_&&var&i.._&&var&j..;
	             %let errcode = 1;
                     %end;
                 %end;
             %end;
         %end;
       %let rc = %sysfunc(close(&dsid.));
       %end;

   %let dsid = %sysfunc(open(&covdsn.,i));
   %if &dsid. = 0 %then %do;
       %put ERROR :  Risk_Est_PSMA_logistic macro found that specified covariate data set &covdsn. does not exist.;
       %abort;
       %end;
   %else %do k = 1 %to &ntotvar.;
       %if %UPCASE(&&var&k..) ne INTERCEPT and %sysfunc(varnum(&dsid.,&&var&k..)) = 0 %then %do;
           %put ERROR :  Risk_Est_PSMA_logistic macro found that covariate data set &covdsn. does not contain the required variable &&var&k..;
           %let errcode = 1;
           %end;
       %end;
   %let rc = %sysfunc(close(&dsid.));

   %if &errcode. = 1 %then %abort;

              
/***********************************************************************************************/
/**  Get the input data containing the sets of covariates values for which to produce the     **/
/**  risk estimates.                                                                          **/
/***********************************************************************************************/

data qqindata;    
    set &covdsn.;
    Intercept = 1;
    dummyby = 1;
run;
   
/***********************************************************************************************/
/**  Get the regression parameter estimates and their covariance matrix for each study and    **/
/**  combine them into one file.                                                              **/
/***********************************************************************************************/

%do k = 1 %to &num_studies.;

data qqparam&k.;   
    set &&key_stats&k..;
    
%do i = 1 %to &&ntotvar_&k..;

    beta_&k._&i. = beta_&&var&i..;
    if beta_&k._&i. = . then beta_&k._&i. = 0;
    
%do j = 1 %to &&ntotvar_&k..;
    v_&k._&i._&j. = v_&&var&i.._&&var&j..;
    if v_&k._&i._&j. = . then v_&k._&i._&j. = 0;
%end;

%end;

    retain beta_&k._1-beta_&k._&ntotvar. %do i = 1 %to &ntotvar.; %do j = 1 %to &ntotvar.; v_&k._&i._&j. %end; %end; 0; 
        ** This retain statement fills in 0 for the variables that do not exist in each study.;
    keep   beta_&k._1-beta_&k._&ntotvar. %do i = 1 %to &ntotvar.; %do j = 1 %to &ntotvar.; v_&k._&i._&j. %end; %end;;
run;

%end;

data qqparam;
    merge %do k = 1 %to &num_studies.; qqparam&k. %end;;
    dummyby = 1;
run;

/***********************************************************************************************/
/**  Estimate the risk for each set of covariate values using fixed effects meta-analysis.    **/
/***********************************************************************************************/

data qqriskest;
    
    array z(&ntotvar.) %do i = 1 %to &ntotvar.; &&var&i.. %end;;
%do k = 1 %to &num_studies.;
    array beta_&k.(&ntotvar.)%do i = 1 %to &ntotvar.; beta_&k._&i. %end;;
    array v_&k.(&ntotvar.,&ntotvar.) %do i = 1 %to &ntotvar.; %do j = 1 %to &ntotvar.; v_&k._&i._&j. %end; %end;;
%end;
    array product(&ntotvar.) product1-product&ntotvar.;
    array sp_ind_flag(&num_studies.,&nsp_ind.) spif1-spif&nsp_ind_xn.;
    array factor(&ntotvar.) factor1-factor&ntotvar.;
    
    merge qqindata qqparam qqsp_ind_flag;
    by dummyby;

    
%do k = 1 %to &num_studies.;

/***********************************************************************************************/
/**  Variance of log odds estimate with special population indicators set to 0.               **/
/***********************************************************************************************/       

    do i = 1 to &nvar.;
            product(i) = 0;
            do j = 1 to &nvar.;
                product(i) = product(i) + v_&k.(i,j) * z(j); 
                end;
            end;

    sum = 0;
    do i = 1 to &nvar.;
        sum = sum + z(i) * product(i);
        end;
    
    variance0&k. = sum;

%end;

/***********************************************************************************************/
/**  Compute the patient-specific study weights, normalizing the total weight to 1.           **/
/***********************************************************************************************/

    vinvtot = 0;
%do k = 1 %to &num_studies.;
    vinvtot = vinvtot + 1 / variance0&k.;
%end;

%do k = 1 %to &num_studies.;
    weight&k. = (1 / variance0&k.) / vinvtot;
%end;

%do k = 1 %to &num_studies.;

/***********************************************************************************************/
/**  Compute the factor to apply to the indicator variables for the special populations.      **/
/**  First determine if the current set of parameters places the patient in the common        **/
/**  population or a special population.  If the patient is in the common population, set     **/
/**  factor = 1.                                                                              **/
/***********************************************************************************************/ 

%do j = 1 %to &ntotvar.; 
    if &j. > &nvar. and &&var&j.. > 0 then do;
        weightsum_special = 0;
%do L = 1 %to &num_studies.;
        weightsum_special = weightsum_special + sp_ind_flag(&L.,&j.-&nvar.) * weight&L.; 
%end;
        factor&j. = 1 / weightsum_special;
        end;
     else factor&j. = 1;
%end;

/***********************************************************************************************/
/**  Estimate the log odds and its variance for each study with the factors applied to the    **/
/**  special population indicators.                                                           **/
/***********************************************************************************************/    

    betatx&k. = 0;
    do i = 1 to &ntotvar.;
       betatx&k. = betatx&k. + beta_&k.(i) * z(i) * factor(i);
       end; 
       
    rho&k. = betatx&k.; 

    do i = 1 to &ntotvar.;
            product(i) = 0;
            do j = 1 to &ntotvar.;
                product(i) = product(i) + v_&k.(i,j) * z(j) * factor(j); 
                end;
            end;

    sum = 0;
    do i = 1 to &ntotvar.;
        sum = sum + z(i) * factor(i) * product(i);
        end;
    
    variance&k. = sum; 
    
%end;

/***********************************************************************************************/
/**  Fixed effects meta-analysis estimate of log odds and its variance.                       **/
/***********************************************************************************************/
  
    rho = 0;
    variance_rho = 0;
    
%do k = 1 %to &num_studies.;

    rho = rho + weight&k. * rho&k.;
    variance_rho = variance_rho + (weight&k.**2) * variance&k.;
    
%end;
  
    SD_rho = sqrt(variance_rho);
    
/***********************************************************************************************/
/**  Transform the log odds estimate to get the risk estimate and CI.                         **/
/***********************************************************************************************/

%let conf = %sysevalf(100*(1-&alpha.),integer);   
  
   &risk_est. = 1 - exp(-exp(rho));
   &risk_LCL. = 1 - exp(-exp(rho - probit(1-&alpha./2) * SD_rho));  
   &risk_UCL. = 1 - exp(-exp(rho + probit(1-&alpha./2) * SD_rho));  
     label &risk_est. = Risk estimate;
     label &risk_LCL. = Lower limit of &conf.% confidence interval for risk;
     label &risk_UCL. = Upper limit of &conf.% confidence interval for risk; 
run;

proc sql noprint;
    create table &outdsn. as
    select %do i = 1 %to &ntotvar.;    &&var&i..,    %end;
           &risk_est., &risk_LCL., &risk_UCL.
    from qqriskest;
quit;

/***********************************************************************************************/
/**  Reset mergenoby option to what is was.                                                   **/
/***********************************************************************************************/

options mergenoby=&mergenobyoption.;

%mend risk_est_PSMA_logistic;     
    
    


    

    
    
    
    


    
