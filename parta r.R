#Loading data
nT  = c(63, 75, 61, 71, 68, 86, 76, 75, 69, 63, 67, 80, 70, 68, 57, 
        68, 69, 86, 64, 75, 60, 63, 70, 77, 78, 67, 70, 73, 72, 69)

xT = c(46, 16, 43, 37, 38, 27, 68, 61, 22, 58, 25, 16, 35, 15, 36, 
       25, 39, 61, 37, 35, 33, 18, 22, 66, 13, 56, 49, 38, 53, 1)

nC = c(63, 75, 61, 71, 68, 86, 76, 75, 69, 63, 67, 80, 70, 68, 57, 
       68, 69, 86, 64, 75, 60, 63, 70, 77, 78, 67, 70, 73, 72, 69)

xC = c(51, 12, 30, 22, 5, 26, 33, 68, 0, 8, 57, 21, 69, 23, 0, 6, 
       1, 4, 59, 4, 2, 5, 1, 1, 16, 6, 15, 42, 0, 13)

#Computing sums
sum.nT = sum(nT)
sum.xT = sum(xT)
sum.nC = sum(nC)
sum.xC = sum(xC)

#Initialising sampling
Niter = 10000
theta.T_init = 0.5
theta.C_init = 0.5
out_theta.T = matrix(nrow = Niter,ncol = 1)
out_theta.C = matrix(nrow = Niter,ncol = 1)
theta.T = theta.T_init
theta.C = theta.C_init

#Sampling from posterior distributions
for (iter in 1:Niter){
out_theta.T[iter] = theta.T
out_theta.C[iter] = theta.C

theta.T = rbeta(1, 0.5+sum.xT, 0.5+sum.nT-sum.xT)
theta.C = rbeta(1, 0.5+sum.xC, 0.5+sum.nC-sum.xC)
}

#Plotting sample trace (history output in WinBUGS)
plot(out_theta.T[1001:10000],type="l")
plot(out_theta.C[1001:10000],type="l")

#Plotting posterior sample density (density output in WinBUGS)
plot(density(out_theta.T[1001:10000]))
plot(density(out_theta.C[1001:10000]))

#Checking estimates and credible intervals (statistics output in WinBUGS)
summary(out_theta.T[1001:10000])
summary(out_theta.C[1001:10000])
quantile(out_theta.T[1001:10000], probs = c(0.025, 0.975))
quantile(out_theta.C[1001:10000], probs = c(0.025, 0.975))