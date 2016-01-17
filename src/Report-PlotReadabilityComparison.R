#install.packages('randomForest'); install.packages('party'); install.packages('rattle'); install.packages('rpart.plot'); install.packages('RColorBrewer'); install.packages('curl'); library(devtools); install_github('krlmlr/kimisc');


################################################################################
## Note:
## This file is only here to generate two figures needed for the report, it does
## NOT contain any usefull knowledge about the customer behaviour.
################################################################################


library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest); library(rpart); library(party)
library(kimisc) # For dataset sampling.

source("../../Library/Util.R")
source("../../Misc/TreeTypeComparison/CustomerJourney.R")
source("../../Misc/TreeTypeComparison/EngagementData.R")
source("../../Misc/TreeTypeComparison/tools.R")

remoteData = FALSE

if (remoteData) {
  # Get data from both engagement and journey datasets.
  engData <- getEngagementData()
  cjDatafull <- getCustomerJourneyData()
} else {
  engData <- read_tsv("EngagementData.tsv")
  cjDatafull <- read_tsv("CustomerJourney.tsv")
}

# filter only simplesite
#engData <- subset(engData, marketname == "US" & siteverkey == "US")

engData <- mutate(engData,
                  islogins1 = factor(islogins1),
                  islogins2 = factor(islogins2),
                  islogins3 = factor(islogins3),
                  islogins4 = factor(islogins4),
                  isedit30m = factor(isedit30m),
                  isedit24h = factor(isedit24h),
                  isaddpage30m = factor(isaddpage30m),
                  isaddpage24h = factor(isaddpage24h),
                  isimgupload30m = factor(isimgupload30m),
                  isimgupload24h = factor(isimgupload24h),
                  iseditdesign30m = factor(iseditdesign30m),
                  iseditdesign24h = factor(iseditdesign24h), # Was design edited between 30-minutes and first 24 hours.
                  iso14 = factor(iso14),
                  ispayer = factor(ispayer)
)

cjDatafull$iscjtrial <- factor(cjDatafull$iscjtrial)
cjDatafull$iscjonboarded <- factor(cjDatafull$iscjonboarded)
cjDatafull$iscjactivated <- factor(cjDatafull$iscjactivated)
cjDatafull$iscjengaged <- factor(cjDatafull$iscjengaged)
cjDatafull$iscjinvested <- factor(cjDatafull$iscjinvested)
cjDatafull$iscjretained <- factor(cjDatafull$iscjretained)
cjDatafull$isimgupload1d <- factor(cjDatafull$isimgupload1d)
cjDatafull$iseditdesign1d <- factor(cjDatafull$iseditdesign1d) # Was design edited within the first 24 hours, including first 30 minutes
cjDatafull$isaddpage1d <- factor(cjDatafull$isaddpage1d)
cjDatafull$isedit1d <- factor(cjDatafull$isedit1d)

# Will join the data on "customerid", which is what we want.
combi <- join(engData, cjDatafull)

# Clear original datasets from memory, they're redundant.
rm(cjDatafull)
rm(engData)

# Remove a bunch of things we do not care about.
# loginsw2w4 looks relevant, but it completely trumphs all the other variables
# so we remove it to discover other patterns.
# logins14 excipits similar behaviour.
combi <- subset(combi, TRUE, c(-customerid,
                               -loginsw2w4,
                               -siteverkey,
                               -ispayer,
                               -culturekey,
                               -iso14,
                               -marketname,
                               -islogins1,
                               -islogins2,
                               -islogins3,
                               -islogins4,
                               #-logins14,
                               -iscjtrial,
                               -iscjonboarded,
                               -iscjactivated,
                               -iscjengaged,
                               -iscjinvested
))
# The reason logins trumph everything is in the definition of cjretained, which
# is what we look for. It is defined as at least one login in week 3-4. logins14
# is not directly used, but a user with many logins there will most likely also
# have logins on week 3-4 (loginsw2w4).



################################################################################
# get same random seed each time for reproducibility
set.seed(1337)

# Pick a sample, we don't need all of it, it's just a demo.
dataset <- sample.rows(combi, size = 10000)

# Plot a small ctree.
fit <- ctree(iscjretained ~ . , data=dataset, controls = ctree_control(maxdepth = 2))
plot(fit)
simple_plot(fit, "Better plot")

# Plot a small rpart tree.
fit <- rpart(iscjretained ~ . , data=dataset, method="class", control=rpart.control(maxdepth = 2))
plot(fit)
fancyRpartPlot(fit)
