# invert-soundscape
Meta-analysis of aquatic invertebrate soundscapes

#### A practical tutorial on conducting meta-analysis in R ####

# Packages
library(compute.es) # to compute effect sizes
library(MAd) # meta-analysis package
library(metafor) # meta-analysis package

# Practice data
data(dat.sim.raw, dat.sim.es)

# Calculate Hedge's g for outcome one using the mes function which computes ESs from means and SDs
res1 <- mes(m.1 = m1T, m.2 = m1C, sd.1 = sd1T, sd.2 = sd1C, n.1 = nT,n.2 = nC,id = id, data = dat.sim.raw) # res1. m.1 and m.2 are the arguments of the function for means of both groups, control and treatment
# sd.1 and sd.2 are for standard deviations of both groups
# n.1 and n.2 are for sample sizes of both groups

# Aggregate dependent ESs based on the BHHR procedure
dat.sim.agg <- agg(id = id, es = g, var = var.g, n.1 = nT, n.2 = nC, cor = 0.5, method = "BHHR", data = dat.sim.es)
# The dependent dataset dat.sim.es is now aggregated into one composite ES per study dataset dat.sim.agg

# Study-level characteristics (moderators of dose and stress) were not retained after running agg(), so add these moderator variables back into the dataset
dat.sim.final <- cbind(dat.sim.agg, dat.sim.raw[, c(12:13)])

# Display a sample of 3 rows of the updated data
dat.sim.final[sample(nrow(dat.sim.final),3),] 
# es is the aggregated ES and var is the variance of ES

# Conduct a random-effects omnibus test using the mareg function in the MAd package
m0 <- mareg(es ~ 1, var =  var, method  = "REML", data = dat.sim.final)
# es is the composite g, var is the variance of ES, method is restricted maximum likelihood REML, and data is dataset

# Display output of omnibus test
summary(m0)

## Summary statistics
# estimate is the summary ES for the included studies
# se is the standard error of ES
# z is the z-statistic
# ci.l and ci.u are the lower and upper 95% confidence interval CI
# p is the p-value for the summary statistic

## Heterogeneity output
# QE is the Q-statistic value
# QEp is the p-value for the Q-statistic
# QM is the Q-statistic for model fit
# QMp is the p-value for QM
# The results of the omnibus model indicate there is between-study variation among ESs based on the statistically significant Q-statistic QEp = 0.02

# Although the Q-statistic indicates the presence of heterogeneity between ESs, it does not provide information about the extent of that heterogeneity - to do so, I^2 and its uncertainty CIs from the omnibus model can be displayed
confint(m0, digits = 2)
# I^2 = 56% [1%, 88%], indicating there is a moderate degree of true between-study heterogeneity
# There is a large degree of uncertainty in this estimate — 95% certain the true value of I^2 is between 1% (all heterogeneity is within-study from sampling error) and 88% (most heterogeneity is due to true between study differences)

## Can depict visually with a forest plot ##

## ESs are heterogenous so can determine what study characteristics might account for the dispersion in the summary effect

# Examine the dose moderator continuous variable under a mixed-effects model
m1 <- mareg(es ~ dose, var  =  var, data  =  dat.sim.final)
summary(m1) # dose moderator

# We expect that for each additional psychotherapy session, the ES will increase by 0.15 and we are 95% certain that the true effect lies somewhere between 0.07 and 0.23, so a participant who completes 10 sessions of this fictional therapy is expected to improve by g = 0.79 above that of the control group
y0 <- m1$b[1] # Intercept
y1 <- m1$b[2] # Slope
# Average number of sessions = 10
x1 <- mean(dat.sim.final$dose)
# To calculate expected ES with ten sessions
y0 + y1 * x1 # = 0.79 nonsignificant

# This moderator accounts for a large proportion of the between-study variance in ESs
confint(m1) 
# With a real dataset it is unlikely that a single moderator will account for all of the between-study heterogeneity

# Examine the stress moderator under a mixed-effects model
m2 <- mareg(es ~ stress, var  =  var, data  =  dat.sim.final)
summary(m2) # stress moderator
# Therapeutic effects appear to be moderated by stress levels

# Examine the correlation between variables - if study characteristics are correlated, results from single moderator models need to be interpreted with caution
with(dat.sim.final, cor(dose, as.numeric(stress))) # = -0.6
# There is a strong negative correlation between these variables, although they are not multicollinear

# Examine both moderators under a mixed-effects model
m3 <- mareg(es ~ dose + stress, var  =  var, data  =  dat.sim.final)
summary(m3) # multiple moderator
# With both moderator variables in the model dose remains statistically significant but stress becomes nonsignificant

## Should always examine for publication bias ##

# Use a funnel plot to examine publication bias visually
funnel(m0) # no visual indication of publication bias
