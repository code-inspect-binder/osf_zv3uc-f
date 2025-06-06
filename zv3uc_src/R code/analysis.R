##################################################
####     Open Science & Terrorism Studies      ###
####  Schumann, van der Vegt, Schuurman, Gill  ###  
####            R code for analysis            ###
##################################################

#################################################################
# Pre-processing
#################################################################

# Read in data from SPSS
library(foreign)
data = read.spss('openscience_noconsentdeleted_291018.sav'
                 , use.value.labels = F
                 , to.data.frame = T)

# Read in variable names
library(data.table)
variables = fread('variables_openscience_terrorismstudies_2018-10-23_14-45.csv', sep = ';')
colnames(data) = variables$LABEL
View(variables)

#################################################################
# Demographics
#################################################################

# Position
position = data$career_level
position = table(position)
position = data.frame(position)
# 1 = undergrad, 2 = postgrad, 3 = phd student ,4 = postdoc, 5 = assistant professor,
# 6 = associate professor, 7 = reader (none in this sample), 8 = professor, 9 = other 

# Publications
publications = as.data.frame(as.numeric(data[, 67]))
colnames(publications) = "publications"
# 1 = 1-5 terrorism publication, 2 = 6-10, 3 = 10-20, 4 = 20+, 5 = none
publications = table(publications)
publications = data.frame(publications)
publications$percentage = (publications$Freq / nrow(data))*100

#################################################################
# Results
#################################################################

## Make dataframe 'Do' with engagement in open science practices
do = data[, 7:15] # columns with engagement
colnames(do) = paste("do", c("preregister", "sharedatas", "sharecode", "openaccess","preprint", "openpeer", "opensw", "replicate", "other"), sep = "_") # add names
do_complete = do[,-9] # remove missing data
do = apply(do, 2, as.numeric)
do = as.data.frame(unlist(do))
do = t(do)

# Calculate percentage of respondents who engaged in each open science practice
do_perc = matrix(ncol = 5, nrow = 9)

for(j in 1:nrow(do)) {
for(i in 1:5) {
  do_perc[j, i] = round(length(which(do[j,] == i))/ncol(do), 4)*100
}
}

row.names(do_perc) = row.names(do)
# 1 = Never, 2 = I tried it but don’t do it systematically, 3 = I do it when it feels convenient, 4 = I do it for most research projects/studies, 5 = I do it for every research project/study.

## Make dataframe 'Wish' with intentions to engage in open science practices
wish = data[, 17:25]
wish_complete = wish[,-9] # remove missing data
wish = apply(wish, 2, as.numeric)
wish = as.data.frame(unlist(wish))
colnames(wish) = c("preregister", "sharedatas", "sharecode", "openaccess", "preprint",
                   "openpeer", "opensw", "replicate", "other")
wish = t(wish)
rowMeans(wish, na.rm = T) # mean 
apply(wish, 1, sd, na.rm = T) # standard deviation
# 1 = not at all, 5 = completely

## Calculate general attitude (df 'Atti') towards open science
atti = as.data.frame(as.numeric(data$`attitude_test: [Keine Beschreibung] 01`))
atti[atti == -9] = NA
colnames(atti) = "attitude"
atti = unlist(atti)
mean(atti, na.rm = T) # mean attitude
sd(atti, na.rm = T) # sd attitude

## Make dataframe 'Reason' with reasons for engaging in open science
reason = data[, 29:36]
reason[reason == -9] <- NA
reason = apply(reason, 2, as.numeric)
reason = reason[, -8]
colnames(reason) = variables$LABEL[29:35]
reason = t(reason)

# calculate average response per reason
rowMeans(reason, na.rm = T)
apply(reason, 1, sd, na.rm = T) # standard deviation

# calculate percentage of respondents who give each reason
reason_perc = matrix(ncol = 5, nrow = 7)

for(j in 1:nrow(reason)) {
  for(i in 1:5) {
    reason_perc[j, i] = round(length(which(reason[j,] == i))/ncol(reason), 4)*100
  }
}
rownames(reason_perc) = variables$LABEL[29:35]
# 1 = strongly disagree, 5 = strongly agree

## Make dataframe 'limits' with  limits to engaging in open science
limits = data[, 57:61]
limits = apply(limits, 2, as.numeric)
colnames(limits) = variables$LABEL[39:43]
limits = t(limits)

# calculate average response per limit
rowMeans(limits, na.rm = T) # mean
apply(limits, 1, sd, na.rm = T) # standard deviation
# 1 = not at all, 5 = very much so 


