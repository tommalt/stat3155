# Tom Maltese
# Stat 3155 HW 1
# problem 2
set.seed(200)
x=rnorm(25,10,4)

print(t.test(x, mu=10))

# the p-value is = 0.9143
# we need a p-value < 0.05 to reject the null hypothesis
# therefore we fail to reject Ho
# the large p-value means that, if the null hypothesis were
# true, there is a 'p-value' probability of observing the 
# test statistic. since we observed a test statistic of 0.10881
# it is very likely that the null hypothesis is true
