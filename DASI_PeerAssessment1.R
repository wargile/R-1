# DASI round 2, Peer Assessment 1
# https://class.coursera.org/statistics-002/human_grading

# (1) General Social Survey (GSS):
# Codebook: https://d396qusza40orc.cloudfront.net/statistics%2Fproject%2Fgss1.html
load(url("http://bit.ly/dasi_gss_data"))
names(gss)

plot(gss$educ ~ gss$race, col="wheat")
plot(gss$tvhours ~ gss$race, col="powderblue")

# (2) American National Elections Study (ANES):
# Codebook: https://d396qusza40orc.cloudfront.net/statistics%2Fproject%2Fanes1.html
load(url("http://bit.ly/dasi_anes_data"))
names(anes)

# Idea: Use Bayes theorem P(A given B) = P(A and B) / P(B) to find out if there is a
# dependency between voter's properties and election result, for instance
# See video: 2 - 6 - Unit 2, Part 2- (1) Conditional Probability (12-40)

plot(anes$israel_support ~ anes$sample_state, las=2, cex.axis=.8, cex.lab=8)
plot(anes$israel_support ~ anes$sample_region, las=2, cex.axis=.8, cex.lab=8)
plot(anes$envir_gwarm ~ anes$sample_state, las=2, cex.axis=.8, cex.lab=8)
plot(anes$envir_gwarm ~ anes$sample_region, las=2, cex.axis=.8, cex.lab=8)
plot(anes$women_works_x ~ anes$sample_region, las=2, cex.axis=.8, cex.lab=8)
plot(anes$trad_famval ~ anes$sample_region, las=2, cex.axis=.8, cex.lab=8)
plot(anes$ftgr_liberals ~ anes$sample_state, las=2, cex.axis=.8, cex.lab=8, na.rm=T)
plot(anes$prevote_intpres ~ anes$dem_raceeth)
plot(anes$prevote_intpres ~ anes$dem_edugroup)
plot(anes$nonmain_born ~ anes$dem_edugroup)
plot(anes$mediapo_site ~ anes$dem_edugroup)
