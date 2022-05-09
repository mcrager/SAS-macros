/***********************************************************************************************************************

Program Name           : id_power.sas

Path                   : 

Program Language       : SAS

Operating System       : Server

Purpose                : Computes the identification power for an individual gene using Storey's false-discovery-rate (FDR)-
                         controlling procedure, defined as the probability of identifying the gene, given hypothetical degree
                         of association of the individual gene, the true proportion of truly null association genes, and the true
                         distribution of degrees of association among genes that are associated with clinical outcome.  It is
                         assumed that the true non-null associations are distributed as an equal mixture of two normal distributions
                         N(mu,sd) and N(-mu,sd) and that the estimation error is normally distributed, and that the
                         test statistic is two-sided.  The user specifies the distribution of non-null genes using the median
                         and 75th percentile (rather than the mean and standard deviation).

                         Typical measures of degree of association are log standardized hazard ratios or log standardized
                         odds ratios. 

                         Reference:  Crager MR (2009).  Prospective Calculation of Identification Power for Individual Genes
                         in Analyses Controlling the False Discovery Rate.  Manuscript in preparation.
                                                  
Notes                  : Records with missing p-values are ignored in the calculation of q-values.  The q-values and estimated
                         proportion of true null hypotheses will be missing for the corresponding records in the output data set. 

Status                 : Tested and verified

Run Dependencies       : None

Input Datasets/Views   : Data set with one record per study scenario and variables containing the required input parameters 

Output Datasets/Views  : Data set with one record per study scenario and variables containing input parameters and the identification power for each record                         

Other Output           : None

Macro calls internal   : None

Macro calls external   : None

***********************************************************************************************************************/

%macro id_power(
       /* Input Specification */   indsn=,fdr=,method=,trueassoc=,se_est=,altmed=,alt75pctl=,p0=,id=,
       /* Output Specification */  idpower=idpower,outdsn=
              );

/***********************************************************************************************
| DEFINITION OF PARAMETERS
|
| Name           : indsn
| Required (Y/N) : Y
| Default Value  : 
| Purpose        : Input data set name  
|
|-----------------------------------------------------------------------------------------------
|
| Name           : fdr
| Required (Y/N) : Y
| Default Value  : 
| Purpose        : Name of input data set variable containing the false discovery rate. 
|
|-----------------------------------------------------------------------------------------------
|
| Name           : method
| Required (Y/N) : N
| Default Value  : storey
| Purpose        : FDR-controlling method to be used in the study.  Default is Storey s method.  
|                  Specify method = benjamini or method = bh for the Benjamini-Hochberg method. 
|
|-----------------------------------------------------------------------------------------------

|
| Name           : trueassoc
| Required (Y/N) : Y
| Default Value  : 
| Purpose        : Name of input data set variable that contains the hypothetical true degree of 
|                  association of the individual gene. 
|
|-----------------------------------------------------------------------------------------------
|
| Name           : se_est
| Required (Y/N) : Y
| Default Value  : 
| Purpose        : Name of input data set variable that contains the expected standard error of 
|                  estimate of degree of association.  This can in general be determined based on
|                  the proposed sample size or number of events in the study.
|
|-----------------------------------------------------------------------------------------------
|
| Name           : altmed
| Required (Y/N) : Y
| Default Value  : 
| Purpose        : Name of input data set variable that contains the median true absolute degree of 
|                  association for the non-null gene population. 
|
|-----------------------------------------------------------------------------------------------
|
| Name           : alt75pctl
| Required (Y/N) : Y
| Default Value  : 
| Purpose        : Name of input data set variable that contains the 75th percentile of the true 
|                  absolute degree of association for the non-null gene population.
|
|-----------------------------------------------------------------------------------------------
|
| Name           : p0
| Required (Y/N) : Y
| Default Value  : 
| Purpose        : Input data set variable containing the hypothetical proportion of truly null
|                  association genes.  Typical values range from 0.6 to 0.9.  Values less than
|                  0.5 are uncommon.
|
|-----------------------------------------------------------------------------------------------
|
| Name           : id   
| Required (Y/N) : N
| Default Value  : 
| Purpose        : Optional list of variables used to identify records in the output data set.
|-----------------------------------------------------------------------------------------------
|
| Name           : idpower   
| Required (Y/N) : N
| Default Value  : idpower
| Purpose        : Name of output data set variable that will contain the identification power
|-----------------------------------------------------------------------------------------------
|
| Name           : outdsn
| Required (Y/N) : Y
| Default Value  : 
| Purpose        : Output data set name  
|
|-----------------------------------------------------------------------------------------------
**********************************************************************************************************/

%local t;

%let t = _idpower;
   

/* Check for errors in macro parameter specification */

   %if %length(&indsn.)=0 %then %do;
      %put ERROR : idpower macro parameter fdr must be specified.;
      %abort;
   %end;

   %if %length(&fdr.)=0 %then %do;
      %put ERROR : idpower macro parameter fdr must be specified.;
      %abort;
   %end;

   %if %length(&trueassoc.)=0 %then %do;
      %put ERROR : idpower macro parameter trueassoc must be specified.;
      %abort;
   %end;

   %if %length(&se_est.)=0 %then %do;
      %put ERROR : idpower macro parameter se_est must be specified.;
      %abort;
   %end;

   %if %length(&altmed.)=0 %then %do;
      %put ERROR : idpower macro parameter altmed must be specified.;
      %abort;
   %end;

   %if %length(&alt75pctl.)=0 %then %do;
      %put ERROR : idpower macro parameter alt75pctl must be specified.;
      %abort;
   %end;

   %if %length(&p0.)=0 %then %do;
      %put ERROR : idpower macro parameter p0 must be specified.;
      %abort;
   %end;

   %if %length(&outdsn.)=0 %then %do;
      %put ERROR : idpower macro parameter outdsn must be specified.;
      %abort;
   %end;


/**********************************************************************************************
**  Check for nonexistent input data sets and nonexistent variables in the input data sets.   *
**********************************************************************************************/

  %let errcode = 0;
  %let dsid = %sysfunc(open(&indsn.,i));
  %if &dsid. = 0 %then %do;
     %put ERROR : ID_power macro found that specified input data set &indsn. does not exist.;
     %abort;
     %end;
   %else %do;
     %if %sysfunc(varnum(&dsid.,&fdr.)) = 0 %then %do;
            %put ERROR : ID_power macro input data set &indsn. does not contain the variable &fdr. specified in macro parameter fdr.;
            %let errcode = 1;
            %end;
     %if %sysfunc(varnum(&dsid.,&trueassoc.)) = 0 %then %do;
            %put ERROR : ID_power macro input data set &indsn. does not contain the variable &trueassoc. specified in macro parameter trueassoc.;
            %let errcode = 1;
            %end;   
     %if %sysfunc(varnum(&dsid.,&se_est.)) = 0 %then %do;
            %put ERROR : ID_power macro input data set &indsn. does not contain the variable &se_est. specified in parameter se_est.;
            %let errcode = 1;
            %end;
     %if %sysfunc(varnum(&dsid.,&altmed.)) = 0 %then %do;
            %put ERROR : ID_power macro input data set &indsn. does not contain the variable &altmed. specified in parameter altmed.;
            %let errcode = 1;
            %end;
     %if %sysfunc(varnum(&dsid.,&alt75pctl.)) = 0 %then %do;
            %put ERROR : ID_power macro input data set &indsn. does not contain the variable &alt75pctl. specified in parameter alt75pctl.;
            %let errcode = 1;
            %end;
     %if %sysfunc(varnum(&dsid.,&p0.)) = 0 %then %do;
            %put ERROR : ID_power macro input data set &indsn. does not contain the variable &p0. specified in parameter p0.;
            %let errcode = 1;
            %end;
     %if %length(&id.) %then %do;
         %if %sysfunc(varnum(&dsid.,&id.)) = 0 %then %do;
            %put ERROR : ID_power macro input data set &indsn. does not contain variable &id. specified in macro parameter id.;
            %let errcode = 1; 
            %end;
         %end;
     %let rc = %sysfunc(close(&dsid.));
     %if &errcode = 1 %then %abort;
     %end;

data _qq_output;
   set &indsn;

/****Check that the values of fdr and p0 are between 0 and 1 and that the standard error of the estimate is positive.  Skip
*****the identification power calculation for any record for which the parameters do not satisfy these conditions.*********/

      if &fdr. <= 0 or &fdr. >= 1 then do;
         put "ERROR : idpower macro: &fdr. value of " &fdr. ' is not between 0 and 1.  No id power calculated for this record.';
         delete;
         end;

      if &p0. <= 0 or &p0. >=1 then do;
         put "ERROR : idpower macro: &p0. value of " &p0. ' is not between 0 and 1.  No id power calculated for this record.';
         delete;
         end;

      if &se_est. <= 0 then do;
         put "ERROR : idpower macro: &se_est. value of " &se_est. ' is not > 0.  No id power calculated for this record.';
         delete;
         end;

      if &alt75pctl. <= &altmed. then do;
         put 'ERROR : idpower macro: Specified 75th percentile ' &alt75pctl. ' is less than specified median ' &altmed. '. No id power calculated for this record.';
         delete;
         end;

/***Compute probability that the q-value is less than the specified FDR criterion using
****numerical integration (trapezoidal rule)************************************************/

/****Determine mean and standard deviation of true absolute degrees of association**********/

      sdalt = (&alt75pctl. - &altmed.) / probit(0.75);

      s0 = &se_est;                        /* Standard error of estimate of magnitude of association */
      s1 = sqrt(&se_est.**2 + sdalt**2);   /* Standard deviation of sum of true magnitude
                                                     of association and estimation error        */  

      lower = &trueassoc - 6*s0;  /* Plausible range of estimate (mean +/- 6 standard errors)  */
      upper = &trueassoc + 6*s0;

      lowerabs = 0;               /* Corresponding plausible range of absolute value of estimate */       
      upperabs = max(abs(lower),abs(upper));
      step = upperabs / 10000;

/***Compute mean and standard error of Wald test statistic*****************************************/

      mu1 = &altmed. / s0;
      sigma1 = s1 / s0;

      sum = 0;
      do absest = lowerabs to upperabs by step;

          absz = absest / s0;

          F0 = cdf('NORMAL',absz,0,1) - cdf('NORMAL',-absz,0,1);
          F1 = 0.5 * cdf('NORMAL',absz,mu1,sigma1) + 0.5 * cdf('NORMAL',absz,-mu1,sigma1)
               - (0.5 * cdf('NORMAL',-absz,mu1,sigma1) + 0.5 * cdf('NORMAL',-absz,-mu1,sigma1));

          absz_density = pdf('NORMAL',absz,&trueassoc./s0,1) + pdf('NORMAL',-absz,&trueassoc./s0,1);

          if F1<1 then do;  *  Check to avoid division by 0.  If F1=1, the rejection criterion will not be satisfied
                            *  so there is no contribution to the integral;
            if absz = lowerabs or absz = upperabs then trap_factor = 0.5;
                                         else trap_factor = 1.0;
%if &method = benjamini or &method = bh %then %do;
            if (1-F0) / ((1-&p0.)*(1-F1)) < &fdr./(1-&fdr.) then sum = sum + trap_factor * absz_density;
%end;
                        %else %do;
            if (&p0.*(1-F0)) / ((1-&p0.)*(1-F1)) < &fdr./(1-&fdr.) then sum = sum + trap_factor * absz_density;
%end;
            end;
          end;

/***Compute identification power**********************************************************************/
      
      stepz = step / s0;  *  Step size scaled to z-score;
      &idpower = sum * stepz;

/***Create output data set***************************************************************************/

      v&t._fdr = &fdr;  
      v&t._trueassoc = &trueassoc;
      v&t._se_est = &se_est;
      v&t._altmed = &altmed;
      v&t._alt75pctl = &alt75pctl;
      v&t._p0 = &p0;  

      label v&t._fdr = False discovery rate;
      label v&t._trueassoc = Hypothetical true degree of association of individual gene;
      label v&t._se_est = Standard error of estimate of association;
      label v&t._altmed = Median absolute degree of association for non-null genes;
      label v&t._alt75pctl = 75th percentile of absolutedegree of association for non-null genes;
      label v&t._p0 = Proportion of truly null association genes;
      label &idpower = Identification power;

      keep v&t._fdr v&t._trueassoc v&t._se_est v&t._altmed v&t._alt75pctl v&t._p0 &idpower
           %if %length(&id.) %then &id.;;
run;

data &outdsn.;
      set _qq_output(rename=(v&t._fdr=&fdr.
                      v&t._trueassoc=&trueassoc.
                      v&t._se_est = &se_est.
                      v&t._altmed = &altmed.
                      v&t._alt75pctl = &alt75pctl.
                      v&t._p0 = &p0.)); 
run;

%mend id_power;
