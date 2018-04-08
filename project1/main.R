# Tom Maltese
# Project 1 - Model Building
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
vif = VIF(data, xnames)
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
