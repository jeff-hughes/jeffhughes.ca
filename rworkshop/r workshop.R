#### ORIENTING TO R STUDIO ####

# -Bottom-left panel is console, where you can type in one-line code, and also
#     where output appears
# -Top-left panel is for scripts -- similar to SPSS syntax files, you can highlight
#     a line or set of lines and run that
# -Bottom-right panel has help panel -- very useful!
#     -also has tabs for plots, file manager, list of installed packages
# -This layout is customizable, so you can rearrange these panels any way you wish

# Code completion:
# -Start typing a function name, and then hit *Tab*, and RStudio will pop up with
#     a list of function names, plus some information for those functions
# -e.g., type in "read", then hit *Tab*
# -Personally, I tend to use the help panel more frequently than this, but it can
#     be useful if you forget how to spell a function name!


#---- comments ----
# lines after a hash mark (#) are comments -- ignored by R

# in RStudio, any comment ending with 4 or more hyphens (-), equals signs (=),
# or hash marks (#) creates a "code section", which you can jump to with the
# bottom left menu of the script panel


#### THE BASICS ####

# The console is where you talk to R, and R talks to you. The console is in the
# bottom-left section of the screen. You can type commands after the command
# prompt (">"), and then hit Enter to run the command. R will then display the
# output in the console as well.

# For R code that takes longer to run, you will see a stop sign in the top-right
# corner of the console. This means R is thinking! If you want it to stop
# thinking, you can click the stop sign.


#---- basic math operators ----

3 + 2   # addition
7 - 2   # subtraction
3 * 5   # multiplication
8 / 2   # division
3^2     # exponent
3**2    # another way to represent exponents
5 %% 2  # modulus (remainder)


#---- assignment operator ----

# to save the output of a command and give it a name to refer to later, use the
# assignment arrow

result <- 7 + 5

result
print(result)


#### INSTALLING AND USING PACKAGES ####

# install/update a package; this only needs to be done once, ever
install.packages("psych")  # psych-related functions

# load a package; this needs to be done each session
library("psych")


#### USING THE WORKING DIRECTORY ####

# When working with files, R uses a "working directory", which allows you to
# specify files relative to the working directory (e.g., "Data/data_file.Rdata")
# instead of "C:/Users/Jeff/Documents/Research/Data/data_file.Rdata").

# You can see the working directory with getwd(), and set it with setwd(), or in
# RStudio you can use the Session > Set Working Directory drop-down menu.

# Usually, it is best practice to either always use absolute filepaths (i.e.,
# starting with "C:/..." etc.), or to set the working directory at the top of
# each script file. This just makes it easier to keep track of where the script
# is pulling files from.

# NOTE: Backslashes (\) have an important meaning in R, so file paths cannot
# contain them! Windows machines use backslashes for paths by default, but if
# you use forward slashes instead, R will figure it out just fine. Alternately,
# you can double each backslash like so:
# "C:\\Users\\Jeff\\Documents\\Research\\Data\\data_file.Rdata"


# set working directory
setwd("C:/Users/Jeff/My Documents/r workshop")


#### SAVE AND LOAD DATA ####

# reading and writing .csv files
data <- read.csv("maximizing_data.csv", header=TRUE, stringsAsFactors=FALSE)
write.csv(data, file="maximizing_data.csv", row.names=FALSE)


# reading and writing native R format (.Rdata)
load("maximizing_data.Rdata")
save(data, file="maximizing_data.Rdata")


# reading SPSS files
install.packages("foreign")  # loads other data types, including SPSS
library("foreign")           # also functions for SAS, Stata, etc.
data <- read.spss("maximizing_data.sav", use.value.labels=FALSE, to.data.frame=TRUE)


#### CHECKING YOUR DATA ####

names(data)  # prints a list of all the column names in the data frame

str(data)  # a useful summary showing you the data type for each column

View(data)  # note the capital V! view your data; note that data will be
            # truncated to first 100 columns, first 1000 rows
# Usually I find it easier to open a .csv file in Excel for getting a sense of
# the data.

head(data)        # shows first six rows by default
head(data, n=10)  # shows first 10 rows

tail(data)        # shows last six rows by default
tail(data, n=10)  # shows last 10 rows


#### BASIC DESCRIPTIVES ####

#---- individual descriptives ----
mean(data$regret, na.rm=TRUE)
median(data$regret, na.rm=TRUE)
var(data$regret, na.rm=TRUE)
sd(data$regret, na.rm=TRUE)
quantile(data$regret, na.rm=TRUE)
range(data$regret, na.rm=TRUE)

# without the "na.rm=TRUE" in these, the function will return NA if there is any
# missing data


#---- summary information ----
describe(data$regret)  # provides a whole set of summary statistics for a
                       # variable; requires the "psych" package, which we
                       # loaded above
describe(subset(data, select=c(max_score, choice, regret)))
    # you can get summary statistics for multiple variables all at once
describeBy(data$regret, group=data$condition)
    # summary statistics for "regret", split by variable "condition"

table(data$condition)  # count number of cases in categorical data

table(data$condition) / sum(table(data$condition)) * 100  # percentage of cases

table(data$choice, data$condition)  # distribution of choice across condition


#### BASIC TRANSFORMATIONS ####

data$satisfaction <- 8 - data$regret  # reverse scores a variable

data$closeness.diff <- data$closeness_after - data$closeness_before
    # calculates a difference score

data$closeness.avg <- rowMeans(subset(data, select=c(closeness_before, closeness_after)), na.rm=TRUE)
    # calculates the average score of the selected columns across rows (i.e.,
    # creates a composite score)

data$max_score.center <- scale(data$max_score, scale=FALSE)
    # centers a variable by its mean
data$max_score.std <- scale(data$max_score, scale=TRUE)
    # standardizes a variable

data$regret.log <- log(data$regret)  # natural log (ln) transform
data$regret.log <- log10(data$regret) # base-10 log transform


#---- using categorical variables ----
data$condition <- factor(data$condition)
    # to use categorical predictors in t-tests, ANOVAs, or regression, they must
    # be factored first, which just creates the groups of responses necessary to
    # be treated as a categorical variable

contrasts(data$condition)
    # these are the contrasts that R creates for you in the background;
    # we will learn how to change these later when we cover factors in more detail

data$choice <- factor(data$choice, labels=c("no change", "change"))
    # labels can be useful when your data are coded numerically


#### BASIC INFERENTIAL STATISTICS ####

#---- correlation ----
cor(subset(data, select=c(max_score, regret, closeness_before, closeness_after)),
    use="pairwise.complete.obs")
    # correlation table for multiple variables

cor.test(data$max_score, data$regret)
    # tests correlation between two variables, which includes p-value and
    # confidence intervals

# requires "psych" package; note that this one is "corr" with two r's!
corr.test(subset(data, select=c(max_score, regret, closeness_before, closeness_after)))
    # correlation table for multiple variables, with table of p-values for all
    # correlations; can do Pearson, Spearman, or Kendall correlations


#---- reliability ----
alpha(subset(data, select=c(max_score, regret, closeness_before, closeness_after)))
    # calculates reliability of scale items; requires "psych" package


#---- t-test ----
t.test(data$regret, mu=0)  # one-sample t-test, where "mu" is set to the
                           # value of the population mean you wish to test
                           # against

data_rum <- subset(data, condition == "Rumination")
data_con <- subset(data, condition == "Control")
t.test(data_rum$regret, data_con$regret, var.equal=TRUE)  # independent samples t-test

t.test(data$closeness_after, data$closeness_before, paired=TRUE)
    # dependent samples t-test; where each case has one row with pre and post columns


#---- chi-square ----
observed <- table(data$choice)  # need a table of observed values
chisq.test(observed)  # chi-squared test compares observed to expected values

chisq.test(table(data$choice))  # can do this in one line as well


#### LINEAR MODELS ####

#---- regression ----
regression_model <- lm(regret ~ max_score + condition, data)
summary(regression_model)

coef <- coef(regression_model)  # coefficients (also seen in summary)
resid <- resid(regression_model)  # residuals for regression model

plot(regression_model)  # diagnostic plots

# Note: the "car" package (Companion to Applied Regression) has lots of other
# useful functions for regression diagnostics (e.g., VIF, leverage, Cook's distance)


# use anova() to get overall effects of variables
model2 <- lm(regret ~ max_score + condition, data)
summary(model2)  # shows effect of each contrast set up for condition
anova(model2)  # shows overall effect of condition

# can also use anova() to compare models
model3 <- lm(regret ~ condition, data)
summary(model3)
anova(model3, model2)


# useful package for standardized betas, graphing interactions, and testing
# simple slopes
install.packages("reghelper")
library("reghelper")

beta(regression_model)  # standardizes all vars in model and reruns

graph_model(regression_model, y=regret, x=max_score, lines=condition)
    # graphs continuous vars at +/- 1 SD, and categorical variables at each level

simple_slopes(regression_model)  # tests all simple slopes of an interaction


#---- more information on formulas ----
model1 <- lm(regret ~ condition + max_score, data)
summary(model1)  # + includes variable in model

model2 <- lm(regret ~ condition * max_score, data)
summary(model2)  # * adds interaction AND main effects

model3 <- lm(regret ~ max_score + condition : max_score, data)
summary(model3)  # : adds JUST interaction, no main effects

model4a <- lm(regret ~ condition * max_score, data)
summary(model4a)
model4b <- lm(regret ~ condition + max_score + condition:max_score, data)
summary(model4b)  # thus, these two models are identical

model5 <- lm(regret ~ condition * max_score - condition, data)
summary(model5)  # - removes a variable from the model

model6 <- lm(regret ~ condition * scale(max_score), data)
summary(model6)  # you can use functions within a formula

model7a <- lm(regret ~ condition * closeness_after-closeness_before, data)
summary(model7a)  # error: trying to do arithmetic calculations (like a
                  # difference score) in a formula won't work
model7b <- lm(regret ~ condition * I(closeness_after-closeness_before), data)
summary(model7b)  # you must use the I() (for Isolate) function to do this

# for more information about formulas, here's a good overview:
# http://ww2.coastal.edu/kingw/statistics/R-tutorials/formulae.html


#---- ANOVA ----
install.packages("ez")  # best package for running ANOVA
library("ez")

# one-way ANOVA
ezANOVA(data, dv=regret, wid=id, between=condition, type=3)

# factorial ANOVA
ezANOVA(data, dv=regret, wid=id, between=.(condition, choice), type=3)

# ANCOVA
ezANOVA(data, dv=regret, wid=id, between=condition, between_covariates=choice, type=3)


# get adjusted means (plus SD, and Fisher's LSD)
ezStats(data, dv=regret, wid=id, between=condition, type=3)
ezStats(data, dv=regret, wid=id, between=.(condition, choice), type=3)
ezStats(data, dv=regret, wid=id, between=condition, between_covariates=choice, type=3)


# ezANOVA can also handle repeated measures and mixed designs; however, this
# requires restructuring the data to long format, i.e., one observation per row.
# There are several methods for restructuring the data like this:
#    reshape() in core packages
#    melt() and cast() from "reshape2" package
#    gather() and spread() from "tidyr" package
# Once the data is in the correct format, you can easily specify within-person
# variables in ezANOVA() using the "within" and "within_covariates" arguments


#---- quick MLM example ----
install.packages("tidyr")
library("tidyr")

# within-person centre closeness variables
# we already calculated mean closeness for each participant above: data$closeness.avg
data$closeness_before_wpc <- data$closeness_before - data$closeness.avg
data$closeness_after_wpc <- data$closeness_after - data$closeness.avg

# convert to long format
data_long <- gather(data, key=time, value=closeness, closeness_before_wpc, closeness_after_wpc)
data_long$time <- ifelse(data_long$time == "closeness_before_wpc", 0, 1)

library("nlme")

# random intercepts model
mlm1 <- lme(closeness ~ condition * time, random=~1|id, data=data_long)
summary(mlm1)

# this doesn't converge with this simplified data, but this is how you specify
# a random intercepts and slopes model
mlm2 <- lme(closeness ~ condition * time, random=~time|id, data=data_long)


#### DATA STRUCTURES ####

#---- basic data types ----
number <- 42
boolean <- TRUE  # must be in all-caps
string <- "string"  # strings (text) can be designated with single or double quotes
missing <- NA  # missing value


#---- vectors ----
v <- c(1, 2, 3)  # the c() combine function creates a vector of any type, as
                 # long as all values are of the SAME type
v <- c("a", "b", "c")
v <- seq(5, 9)  # creates a sequence of numbers: 5, 6, 7, 8, 9
v <- 5:9  # short-hand for a sequence of numbers: 5, 6, 7, 8, 9

# accessing vector values
v[3]  # access the third value (7)
v[5] <- 2  # re-assign a value to a vector index, or add extra indices if index
           # does not already exist
v[c(1, 3)]  # access multiple values: 5, 7
v[1:3]  # access sequence of values: 5, 6, 7

# vector arithmetic
v + 1  # returns a new vector with the arithmetic applied to each value:
       # 6, 7, 8, 9, 10
v - c(1, 2, 3, 4, 5)  # returns a new vector with arithmetic applied to each
                      # index of both vectors: 4, 4, 4, 4, 4
v == c(5, 5, 6, 8, 7)  # returns a vector with logic values for each index:
                       # TRUE FALSE FALSE TRUE FALSE


#---- matrices ----
m <- matrix(c(1, 3, 5, 2, 7, 3), nrow=2)  # a matrix is just a vector with rows
                                          # and columns specified

m <- matrix(c(1, 3, 5, 2, 7, 3), nrow=2, byrow=TRUE)
    # by default, matrices are filled down each column one by one; can set
    # byrow=TRUE to fill across each row instead

# accessing matrix values
m[2, 3]  # access value in row 2, column 3
m[1, 2] <- 9  # re-assign a value to row 1, column 2
m[1, ]  # outputs vector of all values in the first row
m[, 2]  # outputs vector of all values in the second column


#---- factors ----
f <- factor(c("Drug A", "Drug B", "Control", "Drug A", "Control", "Drug B"))
    # creates a factor that groups a vector into categories
levels(f)  # prints the factor levels: "Control" "Drug A" "Drug B"
as.integer(f)  # prints integer values of the factor: 2 3 1 2 1 3
as.character(f)  # converts factor back into character vector


contrasts(f)  # when you create a factor, R creates contrasts in the background

# you can set the contrasts to be whatever you want like so:
contrasts(f) <- matrix(c(1, 0, 0, 0, 0, 1), nrow=3)  # sets Drug A as reference group

# there are also some built-in contrasts; in each below, the 3 refers to the
# number of conditions
contr.treatment(3, base=3)  # dummy coding; base sets the reference group
contr.sum(3)  # effect coding ("sum to zero" contrasts)
contr.poly(3)  # polynomial contrasts (linear, quadratic, etc.)
contr.helmert(3)  # Helmert contrasts

# for example, set effect coding:
contrasts(f) <- contr.sum(3)


#---- lists ----
# lists are a general-purpose container to put anything into
l1 <- list(a=c(1, 3, 9), b=c("a", "b", "c"))
    # can mix data types: first element is a numeric vector, second element is a
    # character vector

# you can even nest lists inside other lists
l2 <- list(
    list(1:5, 6:10, 11:15),
    list(data.frame(a=1:3, b=4:6))
)

# accessing list values
l2[[1]]  # grabs the first element (in this case, the first list)
l1[["a"]]  # if elements are named, you can reference by element
l1$a  # shorthand for accessing a named element
l2[[1]][[3]][3]  # navigating nested lists can be...challenging

str(l2)  # the str() can be really helpful when lists get complex


#### SUBSETTING DATA ####

#---- selecting subsets ----
data[[5]]  # accesses a particular column of a data frame; in this case, the 3rd
           # column
data[["max_score"]]  # accesses a particular column of a data frame by its
                     # column name
data$max_score  # shorthand for accessing a particular column of a data frame
                # by name

data[1, 2]  # selecting row(s) and column(s) of the data frame; in this case,
            # the cell in row 1 and column 2
data[, 5]  # this selects all rows of the 5th column
data[1, ]  # this selects the 1st row of all columns
data[3, -3]  # this selects all columns in the 3rd row EXCEPT for column 3


# selecting multiple rows/columns
data[1, 3:5]  # this selects the 1st row of columns 3 to 5; the ":" operator
              # acts to create a range (in this case: 3, 4, 5)
data[15, c("id", "condition", "max_score")]
    # this selects the 15th row of columns named "id", "condition", and "max_score"


#---- subsetting with logical operators ----
# you can select rows and columns using a vector of TRUE and FALSE, which will
# return all the rows/columns that were identified as TRUE

# as a simple example:
littledata <- data.frame(a=1:5, b=6:10)

littledata[c(TRUE, TRUE, FALSE, TRUE, FALSE), ]

# but it is more useful to create an expression that will create a logical vector
# of TRUE and FALSE
data[data$max_score > 5, c("id", "max_score")]
    # data frames can be filtered by certain criteria; this shows the id and
    # maximizing score for all rows in which "max_score" is greater than 5
data[data$choice == "change", "id"]
    # this selects all rows in column "choice" that are equal to "change"
data[data$max_score > 4 & data$regret < 5, "choice"]
    # the ampersand (&) means logical "AND", so result will be TRUE only if
    # both conditions return TRUE
data[data$max_score > 6 | data$regret < 5, "choice"]
    # the vertical bar (|) means logical "OR", so result will be TRUE if either
    # condition (or both) returns TRUE
data[!(data$condition == "Control"), "regret"]
    # the exclamation point (!) means logical "NOT", which essentially reverses
    # the statement (TRUE becomes FALSE and FALSE becomes TRUE)
data[data$condition != "Control", "regret"]
    # does the same thing as above; the "!=" means "not equal to"

data[!is.na(data$max_score), "regret"]
    # you can filter by rows that do not have missing data; this code uses the
    # "!" operator to select rows in column "regret" for which the data in
    # column "max_score" is NOT missing; the is.na() function returns TRUE if
    # the data is NA (missing), and FALSE if it is anything else
nrow(data[!is.na(data$max_score), ])
    # tells you the number of rows with non-missing data in column "max_score"


# if it helps, you can think of subsetting being about filtering and selecting;
# the first parameter filters rows, while the second paramater selects columns:
# data[FILTER, SELECT]
# for a detailed explanation of how to use subsetting effectively, see:
# http://adv-r.had.co.nz/Subsetting.html


#### VISUALIZING DATA ####

# There are functions within base R to create plots, but ggplot2 is the most
# commonly used graphing package and offers very powerful graphing tools, so it
# is useful to learn it sooner rather than later

# requires "ggplot2" package
install.packages("ggplot2")
library("ggplot2")

#---- building up a plot ----
# empty plot
ggplot(data, aes(x=max_score, y=regret))

# add scatterplot layer
ggplot(data, aes(x=max_score, y=regret)) +
    geom_point()

# add smoothed regression line using "lm" (linear model)
ggplot(data, aes(x=max_score, y=regret)) +
    geom_point() +
    geom_smooth(method="lm")

# add colour aesthetic just to scatterplot
ggplot(data, aes(x=max_score, y=regret)) +
    geom_point(aes(colour=condition)) +
    geom_smooth(method="lm")

# add colour aesthetic to both layers
ggplot(data, aes(x=max_score, y=regret, colour=condition)) +
    geom_point() +
    geom_smooth(method="lm", fullrange=TRUE)

# change to a different theme, and add title and labels
ggplot(data, aes(x=max_score, y=regret, colour=condition)) +
    geom_point() +
    geom_smooth(method="lm", fullrange=TRUE) +
    theme_minimal() +
    labs(title="Regret by Maximizing Score and Condition", x="Maximizing Score",
        y="Regret", colour="Condition")


#---- other useful types of plots ----
# histograms
ggplot(data, aes(x=regret)) + geom_histogram()
ggplot(data, aes(x=regret)) + geom_histogram(binwidth=.3, colour=I("black"))

# density plots, similar to histograms
ggplot(data, aes(x=regret)) + geom_density()
ggplot(data, aes(x=regret, colour=condition)) + geom_density()

# boxplots
ggplot(data, aes(x=condition, y=regret)) + geom_boxplot()

# violin plots
ggplot(data, aes(x=condition, y=regret)) + geom_violin()
ggplot(data, aes(x=condition, y=regret)) + geom_violin() + geom_boxplot(width=.2)
    # combination violin plot and boxplot

# facets and pairs
ggplot(data, aes(x=regret)) + geom_histogram() + facet_grid(condition ~ .)  # rows
ggplot(data, aes(x=regret)) + geom_histogram() + facet_grid(. ~ condition)  # columns

install.packages("GGally")
library("GGally")
ggpairs(data, aes(color=condition, alpha=.6), columns=c(2, 5, 7))
    # creates a set of scatterplots looking at pairwise relationships between
    # each variable; similar in concept to a correlation table


#### THE "plyr" PACKAGE ####

# The *ply() family of functions (from the "plyr" package) are used to split
# data, apply some function, and then put the pieces back together again. The
# functions are named according to their input and output, with the following
# types: a="array", d="data frame", l="list", and _="discarded" (output only).
# So to manipulate a data frame and output another data frame, use ddply(). To
# manipulate a list, then discard the combined pieces, use l_ply().

install.packages("plyr")
library("plyr")

# find mean regret for each condition: split by condition, apply function
# to each piece, then put back together into data frame
ddply(data, .(condition), summarise, mean = mean(regret))
    # "summarise" lets you calculate new columns, and it outputs a NEW data frame
    # with just those columns


# let's say we had already converted data to long format WITHOUT within-person
# centering closeness variables, and we wanted to do that now
data_long2 <- gather(data, key=time, value=closeness, closeness_before, closeness_after)
    # redo conversion to long format to get uncentered values

data_long2 <- ddply(data_long2, .(id), mutate, closeness_wp_mean = mean(closeness))
    # split by participant ID, apply mean function to each chunk, then put back
    # together into data frame;
    # "mutate" is like "summarise", but after calculating new columns it inserts
    # them back into the original data frame (i.e., all existing columns are kept)

data_long2$closeness_wpc <- data_long2$closeness - data_long2$closeness_wp_mean
    # here we use the within-person means we just calculated to center


#### R PROGRAMMING CONSTRUCTS ####

#---- conditional statements ----
# if-else statements evaluate a condition, and IF it is true, it runs the IF
# code block; ELSE (if it is false), it executes the ELSE code block
if (length(v) > 3) {
    print("The length of 'v' is greater than 3!")
}

if (length(v) > 3) {
    print("The length of 'v' is greater than 3!")
} else {
    print("The length of 'v' is less than or equal to 3.")
}

# you can string together statements
if (length(v) > 3) {
    print("The length of 'v' is greater than 3!")
} else if (length(v) >= 2) {
    # if-else statements are mutually exclusive and handled sequentially, so if
    # length(v) == 5, this code will NOT be run -- only the code in the "if"
    # statement above will be run
    print("The length of 'v' is between 2 and 3.")
} else {
    print("The length of 'v' is less than 2.")
}


#---- loops ----
# loops are very useful tools for repeating code with different inputs
conditions <- levels(data$condition)
output <- list()
for (i in 1:length(conditions)) {
    # do something here that repeats;
    # the first time this code runs, the variable "i" will equal 1; the next, it
    # will equal 2, and so on, up to the value of length(conditions)
    data_subset <- data[data$condition == conditions[i], ]
    output[[i]] <- summary(lm(regret ~ max_score, data_subset))
}


#---- creating your own functions ----
# functions are a great way to abstract out commonly-used processes. The basic
# model is: input -> function(x) -> output. Thus, by creating a function, you
# abstract out the implementational details, so someone (even you) using the
# function only has to know three things:
#   1. What is the purpose of the function (i.e., what, in abstract terms, does
#      it do)?
#   2. What type of input(s) do I need to give it?
#   3. What type of output(s) will it give me back?
# Theoretically, the details inside the function could completely change, but as
# long as the answers to the three questions above don't change, it doesn"t
# matter to the person using the function.

# Learning all the nuances of functions is outside the scope of this workshop,
# but for more details, here's a good tutorial: http://adv-r.had.co.nz/Functions.html

find_outliers <- function(vector, multiplier=3) {
    # the first line tells you the name of the function, and gives the names of
    # any input variables (parameters) that are necessary; in this case,
    # "vector" and "multiplier" are the parameters

    # if a parameter is set to a particular value, that is its default value if
    # a user does not set it manually
    
    mad <- mad(vector, na.rm=TRUE)  # calculate median absolute deviation
    lcrit <- median(vector, na.rm=TRUE) - multiplier*mad  # get lower limit
    ucrit <- median(vector, na.rm=TRUE) + multiplier*mad  # get upper limit
    outliers <- which(!is.na(vector) & (vector < lcrit | vector > ucrit))
        # look for any values that are below the lower limit or above the
        # upper limit
    return(outliers)  # this is the output of the function; once a function
                      # returns a value, it stops running, so generally you see
                      # return() at the end of the function
}

sample <- c(1, 5, 3, 18, 3, 9, 13, 2, 1, 27)

find_outliers(sample)  # custom functions are called the same way as any other
                       # function
find_outliers(sample, multiplier=2)

# here's one way you could use this function
if (length(find_outliers(data$regret)) > 0) {  # check that there ARE outliers
    data <- data[-find_outliers(data$regret), ]
        # we add a minus sign in front of find_outliers, so that whatever it
        # returns will be negative; this way, we can tell data[rows, cols] to
        # exclude those rows
}






