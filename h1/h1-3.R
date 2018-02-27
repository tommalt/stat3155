# Tom Maltese
# HW 1
# problem 3
df = read.table("data", sep=",")
colnames(df) = c("rent","size")
covar = cov(df)
correl = cor(df)
print(covar[1,2])
print(correl[1,2])
y=df$rent
x=df$size
model = lm(y~x)
print(summary(model))
coeffs = coef(model)
yint = unname(coeffs[1])
slope = unname(coeffs[2]) 
print(yint)
print(slope)
input = 1050
print((yint + slope * input))
