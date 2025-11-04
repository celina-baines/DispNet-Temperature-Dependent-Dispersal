# DispNet-Temperature-Dependent-Dispersal
Data and code files for Temperature-dependent dispersal: a distributed experiment

RMarkdown file "Species-specific methods and analysis" provides template for supplementary methods and statistical analysis for researchers to conduct on each species.

## Instructions for researchers

### Using git/github

We will share all files on github. The RMarkdown is written so that if you upload your raw data files to the raw_data folder in the repo, knitting the RMarkdown will pull and push from the correct folders in the repo. You should have received an invitation to join the repo on github.

If you are new to git/github, this is a great resource: https://happygitwithr.com/

*[placeholder for git instructions]*

### Using the RMarkdown file

The RMarkdown file contains 1) a template for the methods for each species, to be filled in by each research group (feel free to add more information and more headers, as needed for your species) and 2) code to run the statistical analysis for each species. The code is designed to work for all the species/data structures we have in the study. At the beginning of the RMarkdown, you are asked to fill out information about the data structure for that species. Once that information is filled in, knitting the RMarkdown will run only the code that is appropriate for that species/data structure. (note: do not attempt to “run all” the code in R. You will get an error saying that some of the code can’t be run).

You do not have to alter the RMarkdown except to fill in the information at the top. The goal is that we are consistent in how we are running the analysis. BUT, if you have a reason to change the code for the analysis, please do so. If you make a substantive change, please also change the description of the methods in the RMarkdown and flag the change for Celina.

### Estimating population growth, r

To estimate population growth, r, use the ‘fit_easylinear’ function in the *growthrates* package in R. This method “fits segments of linear models to the log-transformed data and tries to find the maximum growth rate”. This has the benefit of being flexible enough to work for all species, including those where we don’t have a good estimate of K, or where we don’t have that many time points.

Package/function info here:
https://cran.r-project.org/web/packages/growthrates/vignettes/Introduction.html#easy-linear-method [5.1 Easy Linear Method]

### Mortality during dispersal assay
Currently, the default is to exclude individuals that died during the dispersal assay, though of course this is meaningless for some organisms. Make sure to describe in the methods if some experimental subjects were excluded due to mortality.

### Moderators
Below is the list of possible moderators, their description, and options to choose from for your species. We are collecting data on all of them, but only a subset will be used in the analysis.

*Taxonomic.group* =	Broad taxonomic grouping
- arthropod  
- ciliate  
- microalgae

*Rearing.environment* =	Temperature at which experiments subjects for the dispersal assay were reared
- CommonT = Reared at common temperature (specify temp in methods)  
- TreatmentT = Reared in treatment temperatures (i.e., Topt, Tcold, Thot)  
- Field = caught as adults from the field

*Comp.strength* =	Approximate strength of competition in the dispersal assay; note K may change with temperature, provide approximate competition strength across treatments
- Low = density of dispersal assay = 1 or very low  
- Moderate = density below ½ K  
- High = density ½ K and above  

*Source*	= Were experimental organisms sourced from lab population or caught in the field	
- Lab = sourced from lab population (many generations in lab)  
- Field = sourced from the field (within ~2-5 generations)  

*Habitat* =	Type of natural habitat  
- Aquatic 
-Terrestrial  

*Tvariation* = The amount of spatiotemporal variation in temperature that exists in the natural environment of the organism (note: for populations that have been housed in the lab for many generations, “natural environment” is probably the lab)
- Homogenous = within the radius of the maximum dispersal distance of the organism, temperature is homogenous  
- Heterogenous = within the radius of the maximum dispersal distance of the organism, temperature is at least somewhat heterogenous  

*Species.body.size* =	Species mean body length in mm	

*Passive.dispersal*	= Does the organism also engage in passive dispersal  
- Yes  
- No  

*Inbred*	= Are experimental subjects inbred lines or mixed lines/field caught
- Inbred  
- Mixed  

*Disp.assay.duration* =	Order of magnitude for dispersal assay duration / generation time, i.e., approximate number of generations the dispersal assay lasted
- 0.01  
- 0.1  
- 1  
- 10  

*Fitness.proxy.type* =	Fitness proxy category; for many species, population growth, r, is used as the proxy. Grouping all other proxies into “other”.	
- r  
- other  

*Thermal niche* =	Measure of the width of the species’ thermal niche; personal data or collecting from the literature	


### Notes on analysis

#### Overdispersion
I decided to use binomial distribution as default
-	Fitting negative binomial to data that is not overdispersed may lead to unreliable parameter estimates.
-	Prefer to check models for overdispersion and then deal with it only for species that have overdispersion problem.
-	RMarkdown script currently checks for overdispersion and will run appropriate model if overdispersion is significant
-	Using lme4::glmer.nb() for fitting negative binomial in lme4 for species with overdispersion problem

#### Model fitting
All linear mixed models were fit using the ‘lmer’ function in the lme4 package (Bates et al. 2015). All generalized linear mixed models were fit using the ‘glmer’ function in the lme4 package.
Model diagnostics
We checked model fit for both fitness and dispersal models using the ‘simulateResiduals’ function in the DHARMa package (Hartig 2024). This function creates residuals by simulating new data from the full fitted mixed model and generates a QQ plot of the residuals (to check for deviations from normality in the residuals) and a plot of residuals against predicted values (to check for homogeneity of residuals). It also conducts tests for uniformity on the simulated residuals, over (or under) dispersion, and extreme outliers.

#### Intraspecific variation
In order to estimate intraspecific variation in these temperature responses, we will have to conduct a separate analysis just for species with multiple lines/populations included in the experiment. This will come later.  
