# 1. Print your name at the top of the script and load these libraries: FSA, FSAdata, magrittr, dplyr, tidyr plyr and tidyverse
print("Chia-Yun Chiang")
library(FSA)
library(FSAdata)
library(magrittr)
library(plotrix)
library(ggplot2)
library(moments)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)

# 2. Import the inchBio.csv and name the table <bio>
bio <- read.csv("inchBio.csv")
bio

# 3. Display the head, tail and structure of <bio>
headtail(bio,3)
str(bio)
# Display summary statistic
summary(bio)

# 4. Create an object, <counts>, that counts and lists all the species records
# reference: https://stackoverflow.com/questions/15589601/print-string-and-variable-contents-on-the-same-line-in-r
attach(bio)
counts <- table(species)
counts
print(paste0("There are total ",nrow(bio)," records of all the species."))


# 5. Display just the 8 levels (names) of the species
# reference: https://www.datamentor.io/r-programming/factor/
levels(as.factor(species))

# 6. Create a <tmp> object that displays the different species and the number of record of each species in the dataset. Include this information in your report.-
# NOTES: can skip this one
#table(species)

#   7. Create a subset, <tmp2>, of just the species variable and display the first five records
# reference: https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
# reference: https://statisticsglobe.com/extract-certain-columns-of-data-frame-in-r
tmp2 <- subset(bio, select="species")
head(tmp2,5)

#tmp2 <- bio[,c("species")]
#head(tmp2,5)
#tmp2 <- bio %>% select(species)
#head(tmp2, 5)


# 8. Create a table, <w>, of the species variable. Display the class of w
# reference: https://www.datanovia.com/en/lessons/select-data-frame-columns-in-r/#select-columns-by-names
# reference: https://stackoverflow.com/questions/58994237/change-data-frame-to-table
# reference: https://www.cyclismo.org/tutorial/R/tables.html
w <- table(species)
w
class(w)
#library(data.table)
#w <- data.table(species)
#class(w)

# 9. Convert <w> to a data frame named <t> and display the results
t <-as.data.frame(w)
class(t)
headtail(t,3)

# 10. Extract and display the frequency values from the <t> data frame
# reference: https://chemicalstatistician.wordpress.com/2015/02/03/how-to-get-the-frequency-table-of-a-categorical-variable-as-a-data-frame-in-r/
t[,"Freq"]

# 11. Create a table named <cSpec> from the bio species attribute (variable) and confirm that you created a table which displays the number of species in the dataset <bio>
cSpec <- table(bio$species)
class(cSpec)
cSpec

# 12. Create a table named <cSpecPct> that displays the species and percentage of records for each species. Confirm you created a table class.
# reference: https://stackoverflow.com/questions/9623763/extend-contigency-table-with-proportions-percentages
cSpecPct <- prop.table(table(bio$species))
class(cSpecPct)
cSpecPct

detach(bio)

# 13. Convert the table, <cSpecPct>, to a data frame named <u> and confirm that <u> is a data frame
u <- as.data.frame(cSpecPct)
u
class(u)

# 14. Create a barplot of <cSpec> with the following: titled Fish Count with the following specifications:
# • Title: Fish Count
# • Y axis is labeled “COUNTS”
# • Color the bars Light Green
# • Rotate Y axis to be horizontal
# • Set the X axis font magnification to 60% of nominal
# reference: https://www.statmethods.net/graphs/bar.html
# reference: https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/barplot.html
# reference: https://stackoverflow.com/questions/1828742/rotating-axis-labels-in-r

barplot(cSpec,main="Fish Count", 
        xlab = "Species",
        ylab = "COUNTS",
        col = "lightgreen",
        cex.names=0.6,
        ylim = c(0,250),
        las=1)

# 15. Create a barplot of <cSpecPct>, with the following specifications:
# • Y axis limits of 0 to .4
# • Y axis label color of Light Blue
# • Title of “Fish Relative Frequency”
# reference: http://howtoinr.weebly.com/customize-labels1.html
cSpecPct
barplot(cSpecPct,main="Fish Relative Frequency", 
        xlab = "Species",
        ylab = "Relative Frequency",
        cex.names=0.9,
        ylim = c(0, 0.4), col.lab="lightblue")

# 16. Rearrange the <u> cSpec Pct data frame in descending order of relative frequency. Save
# the rearranged data frame as the object <d>
# reference: https://www.guru99.com/r-sort-data-frame.html
d <- u[order(-u$Freq), ]
d

# 17. Rename the <d> columns Var 1 to Species, and Freq to RelFreq
names(d)[1] = "Species"
names(d)[2] = "RelFreq"
d

# 18. Add new variables to <d> and call them cumfreq, counts, and cumcounts
# reference: https://stackoverflow.com/questions/30608228/r-cumulative-sum-based-upon-other-columns
# reference: https://stackoverflow.com/questions/37034242/r-add-a-new-column-to-a-dataframe-using-matching-values-of-another-dataframe

# Adding new column cumfreq
d$cumfreq = ave(d$RelFreq,FUN = cumsum)
d

# Creating col3 dataframe (counts of the species) used for further merging 
col3 <- as.data.frame(cSpec)
# Rename the column name
names(col3)[1]= "Species"
names(col3)[2]= "counts"
col3

# Merge the d dataframe to col3 datafame
d <- merge(d, col3, by = "Species")
# Sort data
d <- d[order(-d$RelFreq),]
d
# Adding new column cumcounts 
d$cumcounts = ave(d$counts, FUN=cumsum)
d

# 19. Create a parameter variable <def_par> to store parameter variables
def_par <- par(no.readonly=TRUE)

# 20. Create a barplot, <pc>, with the following specifications:
# • d$counts of width 1, spacing of .15
# • no boarder
# • Axes: F
# • Yaxis limit 0,3.05*max
# • d$counts na.rm is true
# • y label is Cummulative Counts
# • scale x axis to 70%
# • names.arg: d$Species
# • Title of the barplot is “Species Pareto”
# • las: 2)
# reference: https://stackoverflow.com/questions/24212739/how-to-find-the-highest-value-of-a-column-in-a-data-frame-in-r/24212879
# reference: https://stackoverflow.com/questions/31149590/r-barplot-how-to-control-space-and-width-of-bars
# reference: https://stat.ethz.ch/pipermail/r-help//2006-October/116039.html
# reference: https://stackoverflow.com/questions/30147358/storing-plot-objects-in-r
# reference: https://statisticsglobe.com/save-plot-in-data-object-in-base-r

# Find max value in "counts" column
count_max <- max(d$counts, na.rm = TRUE)
# Change margin
par(mar = c(7.1, 4.1, 2.1, 5.1))
# draw barplot and assign midpoint
m_point <- barplot(d$counts,main="Species Pareto", 
        width = 1, space = 0.15, 
        border = NA, axes=FALSE,
        xlab = "Species", 
        ylab = "Cummulative Counts",
        names.arg = d$Species,
        ylim = c(0, count_max*3.05),
        cex.names = 0.7,las=2)
# save plot into pc variable
pc <- recordPlot()

par(def_par)

# 21. Add a cumulative counts line to the <pc> plot with the following:
# • Spec line type is b
# • Scale plotting text at 70%
# • Data values are solid circles with color cyan4
# reference: https://stat.ethz.ch/pipermail/r-help/2000-July/007454.html
# reference: https://stackoverflow.com/questions/22341019/midpoints-returned-by-barplot-function-do-not-actually-line-up-with-midpoints-of

plot.new()
pc
# Draw the new plot based on original plot
#par(new=TRUE)
lines(m_point,d$cumcounts, type="b", col="cyan4", cex=0.7, pch=19)


# 22. Place a grey box around the pareto plot (hint:https://www.statmethods.net/advgraphs/parameters.html)
# reference: https://stat.ethz.ch/R-manual/R-patched/library/graphics/html/box.html
box(col="grey")

# 23. Add a left side axis with the following specifications
# • Horizontal values at tick marks at cumcounts on side 2
# • Tickmark color of grey62
# • Color of axis is grey62
# • Axis scaled to 80% of normal
# (hint: https://www.statmethods.net/advgraphs/axes.html)
# reference: https://www.statmethods.net/advgraphs/axes.html

axis(2, at = d$cumcounts, labels=d$cumcounts, las=2, col="grey62", col.axis="grey62", cex.axis=0.8)

# 24. Add axis details on right side of box with the specifications:
# • Spec: Side 4
# • Tickmarks at cumcounts with labels from 0 to cumfreq with %,
# • Axis color of cyan5 and label color of cyan4
# • Axis font scaled to 80% of nominal
# reference: https://stackoverflow.com/questions/22082511/how-do-i-add-percentage-signs-to-an-axis-in-barplot-in-r


axis(4, at = d$cumcounts, labels= paste0(round(d$cumfreq, 2) * 100, "%"), las=2, col="grey62", col.axis="cyan4", cex.axis=0.8)

# 25. Display the finished Species Pareto Plot (without the star watermarks). Have your last name on the plot
# reference: https://bookdown.org/ndphillips/YaRrr/plot-margins.html
text(9.2,50,"Chiang")
par(def_par)

# ---------------------------------------------------------------------------
# Start my own analysis

# Quick view of bio dataset
headtail(bio,5)
# Quick view of d dataset
d
# Store species order based on the amount of species
# reference: https://www.r-graph-gallery.com/22-order-boxplot-labels-by-names.html
s_order = c("Largemouth Bass", "Bluegill","Bluntnose Minnow",
            "Yellow Perch","Black Crappie","Iowa Darter",
            "Pumpkinseed","Tadpole Madtom")
bio$species <- factor(bio$species , levels=s_order)

# Generate boxplot of total length species specific species order
plot.new()
par(cex.axis=0.85)
boxplot(tl~species, data=bio,
        main="Total Length of Species",
        xlab="Species", ylab="Total Length (mm)", 
        varwidth=TRUE)
# calculated the mean value of length and point it on the plot
# reference: https://stackoverflow.com/questions/19876505/boxplot-show-the-value-of-mean
tl_mean <- tapply(bio$tl,bio$species,mean)
tl_mean 
points(tl_mean ,col="red",pch=18)
# add mean value in the plot
text(1.35:8.35, tl_mean, labels = round(tl_mean ,1), col="blue")

# Generate boxplot weight species specific species order
plot.new()
par(cex.axis=0.85)
boxplot(w~species, data=bio,
        main="Weight of Species",
        xlab="Species", ylab="Weight (g)", 
        varwidth=TRUE)

boxplot(w~species, data=bio,
        main="Weight of Species",
        xlab="Species", ylab="Weight (g)", 
        varwidth=TRUE)

# calculated the mean value of weight and point it on the plot (ignore NA value)
# reference: https://stackoverflow.com/questions/14172556/how-to-pass-na-rm-as-argument-to-tapply
w_mean <- tapply(bio$w,bio$species,mean,na.rm=TRUE)
w_mean 
points(w_mean ,col="red",pch=18)
# add mean value in the plot
text(1.35:8.35, w_mean, labels = round(w_mean ,1), col="blue")

# Adjust y-axis range to look into the area that not showed in the whole plot
boxplot(w~species, data=bio,
        main="Weight of Species (Zoom in)",
        xlab="Species", ylab="Weight (g)", 
        varwidth=TRUE, ylim=c(0,6))
points(w_mean ,col="red",pch=18)
text(1.35:8.35, w_mean, labels = round(w_mean ,1), col="blue")

# descriptive statistics for each species (numeric value for box plot)
# reference: https://stackoverflow.com/questions/9847054/how-to-get-summary-statistics-by-group
tapply(bio$tl, bio$species, summary)
tapply(bio$w, bio$species, summary)


plot.new()
# Assign color to differentiate species
cols <-c("1","2","3","4","5","6","7","8")
col_value <- cols[as.numeric(bio$species)]
# Generate Scatter plot (use color distinguish species)
plot(bio$tl, bio$w, col=col_value, pch = 19,
     main="Scatter Plot of Weight versus Total Length",
     xlab = "Total Length (mm)", ylab = "Weight (g)")  
legend("topleft", inset = 0.05, title = "Species", s_order,
 col = cols , pch = 19)


# Adjust x,y axis in order to take a close look at specific area
plot(bio$tl, bio$w, col=col_value, pch = 19,
     main="Scatter Plot of Weight versus Total Length (Zoom in)",
     xlab = "Total Length (mm)", ylab = "Weight (g)",xlim=c(20,100), ylim=c(0,15))  
legend("topleft", inset = 0.05, title = "Species", s_order,
       col = cols , pch = 19)


# Calculate correlation between tl & w (overall)
# reference: https://stackoverflow.com/questions/31412514/na-values-not-being-excluded-in-cor
cor_value <- round(cor(bio$w, bio$tl, use="complete.obs"),2)
print(paste0("correlation betweeen weight and total length is ",cor_value))


# Calculate correlation between tl & w in different species group
# reference: https://stats.stackexchange.com/questions/4040/r-compute-correlation-by-group?fbclid=IwAR3bwyfE5V4BsVO5LSxsIMxdkyzB2qReyNb-26JAeIIuVz1Pi7gxwPLIygg
require(plyr)
func <- function(bio)
{
  return(data.frame(COR = cor(bio$w, bio$tl, use = "pairwise.complete.obs")))
}

ddply(bio, .(bio$species), func)

