# Tom Maltese
# Stat 3155 - Prof. Elbarmi
# Project 1 - Model Building; testing for outliers
# and multicolinearity with VIF, and model selection with stepwise regression,
# forward selection, and backward elimination
yname = "Sales"
xnames = c("Age","HS","Income","Black","Female")
cnames = c(yname, xnames)
data = read.table("data-cigarette.csv", sep=",", header=TRUE)
data = data[,cnames]
naive_model = lm(Sales ~ Age + HS + Income + Black + Female, data=data)
# try to filter out the outliers
k = length(xnames)
n = nrow(data)
leverage = hat(model.matrix(naive_model))
outliers_x = which(leverage > (2 * (k + 1) / n))
rs = as.vector(rstudent(naive_model))
outliers_y = which(abs(rs) > 2)
outliers = sort(c(outliers_x, outliers_y))
df = data[-outliers,]
removed_outliers_model = lm(Sales ~ Age + HS + Income + Black + Female, data=df)
cat("Naive model (full data-set, nrow = ", nrow(data), ")\n", sep="")
print(summary(naive_model))
cat("Removed outliers model (nrow = ", nrow(df), ")\n", sep="")
print(summary(removed_outliers_model))
# the full model (with no outliers removed from the data) realizes a stderr
# of 30.85 on 45 degrees of freedom
# the model with the filtered data realizes a stderr of 19.34 on 39 degrees of freedom
# and also has a slightly better R-squared value (0.167 -> 0.172)
# However, the p-values for the t-statistic for every variable (except Income)
# were greater in the second model than they were in the first.
# additionally, the p-value for the F-statistic was slightly higher (0.13 -> 0.17)

# next, we try to detect multicollinearity
makeFormula = function(y, xs)
{
	if (is.vector(xs, mode="character") && length(xs) == 1) {
		return (paste(y, "~", xs))
	}
	f = paste(y, "~", xs[1])
	for (i in 2:length(xs)) {
		f = paste(f, "+", xs[i])
	}
	return (f)
}
VIF = function(data, xnames)
{
	vif = list() # map of variable name -> variance inflation factor
	for (i in 1:length(xnames)) {
		response = xnames[i]
		predictors = xnames[-i]
		ftext = makeFormula(response, predictors)
		f = as.formula(ftext)
		fit = lm(f, data=data)
		s = summary(fit)
		r2 = s$r.squared
		vif[response] = 1.0 / (1.0 - r2)
	}
	return (vif)
}
# check the VIFs for each variable to see if any are above 'maxval'
# or if the mean(VIFs) is greater than 'meanval'
VIFcheck = function(vifs, maxval, meanval)
{
	if (is.list(vifs)) {
		vifs = unlist(vifs)
	}
	mx = max(vifs)
	mn = mean(vifs)
	if (mx > maxval)
		return (1)
	if (mn > meanval)
		return (2)
	return (0)
}
cat("Variance Inflation factor analysis (with trimmed dataset)\n")
vif = VIF(df, xnames)
vifvalues = unlist(vif)
maxvif = max(vifvalues)
meanvif = mean(vifvalues)
for (k in names(vif)) {
	cat("VIF of", k, ":", vif[[k]], "\n")
}
status = VIFcheck(vif, 10, 3)
if (status) {
	if (status == 1) {
		cat("Maximum VIF exceeds threshold (10)", maxvif)
	} else if (status == 2) {
		cat("Mean VIF exceeds threshold (3)", meanvif)
	}
} else {
	cat("Maximum VIF:", maxvif, "\n")
	cat("Mean    VIF:", meanvif, "\n")
	cat("VIF checks passed\n")
}
# checking for multicollinearity between the independent variables
# is inconclusive; the max is only 3.82, with a mean of 2.75
dataset = df
# dataset = data
null_model = lm(Sales ~ 1, data=dataset)
full_model = lm(Sales ~ Age + HS + Income + Black + Female, data=dataset)
cat("\nForward Selection\n")
step(null_model, scope=list(lower=null_model, upper=full_model), data=dataset, direction="forward")
cat("\nBackward Selection\n")
step(full_model, data=dataset , direction="backward")
cat("\nStepwise regression\n")
step(null_model, scope=list(upper=full_model), data=dataset, direction="both")
# Looking at the output from all three variable selection methods
# we observe that all the algorithms conclude Sales ~ Income to be the best model
optimal_model = lm(Sales ~ Income, data=dataset)
print(summary(optimal_model))
print(confint(optimal_model))
