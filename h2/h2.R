## Tom Maltese
## Stat 3155. Prof. Elbarmi
## HW 2.

## 3.3
##
cat("problem 3.3\n")
df = read.table("data3.3.csv", sep=',')
gpa=df[,1]
salary=df[,2]

model = lm(salary ~ gpa)
print(summary(model))

# From the summary output, b0 (intercept) = 14.8156
#                     and, b1 (slope)     =  5.7066
# this means that, the average Salary when GPA = 0 is about 14.8156 * $1,000
#                                                  is about $14,815.6
# this interpretation is not very useful, because it is silly to consider
# the rare occurrence where a student as a 0 GPA.

# the b1 coefficient tells us that there is an approximate change of
# 5.7066 * $1,000 = 5,706.6 in the mean starting salary for every one unit
# change in GPA. Increasing GPA's lead to increasing starting salaries.

b0 = unname(model$coefficients[1])
b1 = unname(model$coefficients[2])
x = 3.25
pest = b0 + b1 * x

print(paste0("point estimate: ",pest))
# the point estimation and point prediction are 33.36196

## verifying the point estimates using equations in 3.2, p. 88
mx = mean(gpa)
my = mean(salary)
xdiff = gpa - mx
ydiff = salary - my
ssxy = (xdiff %*% ydiff)[1,1]
ssxx = sum((gpa - mx)^2)
b1_est = ssxy / ssxx
print(paste0("b1 point estimate: ",b1_est))
b0_est = my - b1_est * mx 
print(paste0("b0 point estimate: ", b0_est)) 

## 3.7
##
cat("\nproblem 3.7\n")
df = read.table("data3.7.csv", sep=',')
serv = df[,1]
minutes = df[,2]

model = lm(minutes ~ serv)
print(summary(model))
# the least squares point estimates b0 and b1 are 11.4641
# and 24.6022 respectively

# the point prediction and estimate is given by:
#       y^ = b0 + b1 * x0
# so:
# with x = 4
#       y^ = 11.4641 + 24.6022 * 4
#       y^ = 109.8729 

## verifying the point estimates using equations in 3.2, p. 88
mx = mean(serv)
my = mean(minutes)
xdiff = serv - mx
ydiff = minutes - my
ssxy = (xdiff %*% ydiff)[1,1]
ssxx = sum((serv - mx)^2)
b1_est = ssxy / ssxx
print(paste0("b1 point estimate: ",b1_est))
b0_est = my - b1_est * mx 
print(paste0("b0 point estimate: ", b0_est)) 

sse = sum((predict(model) - minutes)^2)
n = length(serv)
s2 = sse / (n - 2)
print(paste0("SSE: ", sse))
print(paste0("S^2: ", s2))
print(paste0("S  : ", sqrt(s2)))

## 3.11
##
cat("\nproblem 3.11\n")
df = read.table("data3.11.csv", sep=',')

x1 = df[,1]
x2 = df[,2]
x = x2 - x1
y  = df[,3]

model = lm(y ~ x)
print(summary(model))

# b0 and b1 are estimated to be 7.6932 and 3.3026, respectively
# here, b0 means that, when the price difference, x == 0, the mean
# demand is approximately 7.3932. This makes sense because we can
# have a price difference of 0.

# again, we have:
#         y^ = b0 + b1 * x
# with x = 0.1
#         y^ = 7.6932 + 3.3026
b0 = unname(model$coefficients[1])
b1 = unname(model$coefficients[2])
print(paste0("Prediction/estimate: ", b0 + b1 * 0.1))

# we want y^ = 8.5
#         y^ = b0 + b1 x
#         x = (y^ - b0) / b1
ty = 8.5
tx = (ty - b0) / b1
print(paste0("xvalue to produce y^ = 8.5 : ", tx))

# given SSE = 2.8059, compute s2 and s
sse = sum((y - mean(y))^2)
n = length(x)
s2 = sse / (n - 2)
s = sqrt(s2)
print(paste0("S^2 : ",s2))
print(paste0("S   : ",s))
