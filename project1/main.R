# Tom Maltese
# Project 1 - Model Building
#
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

# try to detect multicollinearity
makeFormula = function(y, xs)
{
	if (is.vector(xs, mode="character") && length(xs) == 1) {
		return (paste(y, "~", xs))
		#return (as.formula(paste(y, "~", xs)))
	}
	f = paste(y, "~", xs[1])
	for (i in 2:length(xs)) {
		f = paste(f, "+", xs[i])
	}
	return (f)
	#return (as.formula(f))
}
for (i in 1:length(xnames)) {
	response = xnames[i]
	predictors = xnames[-i]
	ftext = makeFormula(response, predictors)
	f = as.formula(ftext)
	fit = lm(f, data=data)
	cat("Formula: ", ftext, "\n")
	print(summary(fit))
	#summaries = append(summaries, summary(fit))
	#print(summary(fit))
}
