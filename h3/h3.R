# Tom Maltese
# Homework 3

cat("Problem 4.3\n")
data = read.table("data4-10.txt", sep=" ")
colnames(data) = c("SalesPrice", "HomeSize", "Rating")

model = lm(SalesPrice ~ HomeSize + Rating, data=data) 
print(model)
print(summary(model))

x = data.frame(HomeSize=c(20), Rating=c(8))
cat(paste0("Prediction of SalesPrice: ", predict(model, x) * 1000), "\n")

cat("Problem 4.4\n")
data = read.table("data4-11.correct.txt", sep=",", header=TRUE)
# columns  =  Xray, BedDays, Length, Hours
model = lm(Hours ~ Xray + BedDays + Length, data=data)
print(summary(model))

# point prediction for labor hours
x = data.frame(Xray=56194, BedDays=14077.88, Length=6.89)
yhat = predict(model, x)
cat(paste0("Point prediction for labor hours: ", yhat, "\n"))
y = 17207.31
cat(paste0("residual, (actual - predicted): ", y - yhat, "\n"))

cat("Problem 4.5", "\n")
data = read.table("data4-10.txt", sep=" ")
colnames(data) = c("SalesPrice", "HomeSize", "Rating")
model = lm(SalesPrice ~ HomeSize + Rating, data=data) 
print(summary(model))

k = ncol(data) - 1
n = nrow(data)
sse = sum((data$SalesPrice - predict(model))^2)
mse = sse / (n - k - 1)
s = sqrt(mse)
cat(paste0("S^2 : ", mse, "\n"))
cat(paste0("S   : ", s, "\n"))
ssr = sum((predict(model) - mean(data$SalesPrice))^2)
sst = ssr + sse
r2 = ssr / sst
adjr2 = (r2 - (k / (n - 1))) * ((n - 1) / (n - (k + 1)))
msr = ssr / k
F = msr / mse
cat(paste0("SSR : ", ssr, "\n"))
cat(paste0("SSE : ", sse, "\n"))
cat(paste0("SST : ", sst, "\n"))
cat(paste0("R^2 : ", r2, "\n"))
cat(paste0("Adj R^2 : ", adjr2, "\n"))
cat(paste0("F-stat: ", F, "\n"))
cat("For alpha = 0.10, 0.05, alpha = 0.01 and alpha = 0.001\n")
cat("we reject the null hypothesis because p-value < alpha\n")
