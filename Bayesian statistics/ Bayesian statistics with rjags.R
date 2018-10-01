## which factor are most important for overall statisfaction?
## load attitude data in R
data("attitude")
?attitude
plot(rating ~ complaints, data = dat)
hist(dat$rating)
hist(dat$complaints)
hist(dat$critical)

## delete all NAs
dat = na.omit(attitude)

## check the colinearity between variables
pairs(dat)
library("corrplot")
Cor = cor(dat)
corrplot(Cor, type="upper", method="ellipse", tl.pos="d")
corrplot(Cor, type="lower", method="number", col="black", 
         add=TRUE, diag=FALSE, tl.pos="n", cl.pos="n")

# non informative linear regression. only complaint matters
mod_lm = lm(rating ~ ., data = dat)
summary(mod_lm)
plot(resid(mod_lm))
plot(predict(mod_lm),resid(mod_lm))
qqnorm(resid(mod_lm))

# rjags
library("rjags")

# test all factors
mod0_string = " model {
for (i in 1:n) {
rating[i] ~ dnorm(mu[i], prec)
mu[i] = b[1] + b[2]*complaints[i] + b[3]*privileges[i] + b[4]*learning[i] + b[5]*raises[i] + b[6]*critical[i] + b[7]*advance[i]
}

for (i in 1:7) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(5/2.0, 5*10.0/2.0)
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

set.seed(72)
data0_jags = list(rating = dat$rating, n = nrow(dat), complaints = dat$complaints, privileges = dat$privileges, learning=dat$learning, raises = dat$raises, critical = dat$critical, advance = dat$advance)

params0 = c("b", "sig")

inits0 = function() {
  inits = list("b"=rnorm(7,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod0 = jags.model(textConnection(mod0_string), data=data0_jags, inits=inits0, n.chains=3)
update(mod0, 1000) # burn-in

mod0_sim = coda.samples(model=mod0,
                        variable.names=params0,
                        n.iter=8000)

mod0_csim = as.mcmc(do.call(rbind, mod0_sim)) # combine multiple chains

effectiveSize(mod0_sim)
plot(mod0_sim)
gelman.diag(mod0_sim)
autocorr.diag(mod0_sim)
autocorr.plot(mod0_sim)
plot(mod0_sim, ask=TRUE)
summary(mod0_sim)
par(mfrow=c(2,3))
densplot(mod0_csim[,2:7])

# model with most significant three variables
mod1_string = " model {
for (i in 1:n) {
rating[i] ~ dnorm(mu[i], prec)
mu[i] = b[1] + b[2]*complaints[i] + b[3]*learning[i] + b[4]*advance[i]
}

for (i in 1:4) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(5/2.0, 5*10.0/2.0)
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

set.seed(72)
data1_jags = list(rating = dat$rating, n = nrow(dat), complaints = dat$complaints, learning = dat$learning, advance = dat$advance)

params1 = c("b", "sig")

inits1 = function() {
  inits = list("b"=rnorm(4,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod1 = jags.model(textConnection(mod1_string), data=data1_jags, inits=inits1, n.chains=3)
update(mod1, 1000) # burn-in

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=8000)

mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains

effectiveSize(mod1_csim)
plot(mod1_sim)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
autocorr.plot(mod1_sim)
summary(mod1_sim)

# test the residual
X1 = cbind(rep(1.0, data1_jags$n), data1_jags$complaints, data1_jags$learning,data1_jags$advance)
head(X1)
(pm_params1 = colMeans(mod1_csim)) # posterior mean
yhat1 = drop(X1 %*% pm_params1[1:4])
resid1 = data1_jags$rating - yhat1
par(mfrow=c(1,3))
plot(resid1) # against data index
plot(yhat1, resid1) # against predicted values
qqnorm(resid1) # checking normality of residuals
dic1 = dic.samples(mod1, n.iter=1e3)

# model with the most significant variable: compalints
mod2_string = " model {
for (i in 1:n) {
rating[i] ~ dnorm(mu[i], prec)
mu[i] = b[1] + b[2]*complaints[i] 
}

for (i in 1:2) {
b[i] ~ dnorm(0.0, 1.0/1.0e6)
}

prec ~ dgamma(5/2.0, 5*10.0/2.0)
sig2 = 1.0 / prec
sig = sqrt(sig2)
} "

set.seed(72)
data2_jags = list(rating = dat$rating, n = nrow(dat), complaints = dat$complaints)

params2 = c("b", "sig")

inits2 = function() {
  inits = list("b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod2 = jags.model(textConnection(mod2_string), data=data2_jags, inits=inits2, n.chains=3)
update(mod2, 1000) # burn-in

mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=8000)

mod2_csim = do.call(rbind, mod2_sim) # combine multiple chains

gelman.diag(mod2_sim)
autocorr.diag(mod2_sim)
autocorr.plot(mod2_sim)

summary(mod2_sim)

# test the residual
X2 = cbind(rep(1.0, data1_jags$n), data2_jags$complaints)
head(X2)
(pm_params2 = colMeans(mod2_csim)) # posterior mean
yhat2 = drop(X2 %*% pm_params2[1:2])
resid2 = data2_jags$rating - yhat2
plot(resid2) # against data index
plot(yhat2, resid2) # against predicted values
qqnorm(resid2) # checking normality of residuals

dic2 = dic.samples(mod2, n.iter=1e3)

