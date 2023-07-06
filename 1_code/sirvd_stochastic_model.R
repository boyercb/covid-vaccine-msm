# this is great but need to figure out MSM
## Core equations for transitions between compartments:
update(S) <- S - n_SIs + n_RS - n_SV + n_VS
update(Is) <- Is + n_SIs - n_IsR + n_import_I
update(Iv) <- Iv + n_VIvbreak - n_IvR 
update(R) <- R + n_IsR + n_IvR - n_RS
update(V) <- V + n_SV - n_VS - n_VIvbreak
update(Ds) <- Ds + n_IsD
update(Dv) <- Dv + n_IvD 

## Individual probabilities of transition:
p_SIs <- 1 - exp(-beta * (Is + Iv) / N)
p_VIv <- 1 - exp(-beta * (Is + Iv) / N)
p_IsRD <- 1 - exp(-gamma) # Ir to R or D
p_IvRD <- 1 - exp(-gamma_v) # Iv to R or D
p_RS <- 1 - exp(-omega) # R to S
p_SV <- 1 - exp(-nu) # S to V
p_VS <- 1 - exp(-sigma) # V to S

## Draws from binomial distributions for numbers changing between
## compartments:
n_SIs <- rbinom(S, p_SIs)
n_SV <- rbinom(S, p_SV)
n_VS <- rbinom(V, p_VS)
n_RS <- rbinom(R, p_RS)

n_Isout <- rbinom(Is, p_IsRD)
n_IsDR[] <- rmultinom(n_Isout, p_mu)
p_mu[1] <- 1 - mu
p_mu[2] <- mu
dim(p_mu) <- 2
dim(n_IsDR) <- 2
n_IsR <- n_IsDR[1]
n_IsD <- n_IsDR[2]

n_Ivout <- rbinom(Iv, p_IvRD)
n_IvDR[] <- rmultinom(n_Ivout, p_mu_v)
p_mu_v[1] <- 1 - mu_v
p_mu_v[2] <- mu_v
dim(p_mu_v) <- 2
dim(n_IvDR) <- 2
n_IvR <- n_IvDR[1]
n_IvD <- n_IvDR[2]

n_VIv <- rbinom(V, p_VIv)
n_break[] <- rmultinom(n_VIv, p_theta)
p_theta[1] <- 1 - theta
p_theta[2] <- theta
dim(p_theta) <- 2
dim(n_break) <- 2
n_VIvbreak <- n_break[1]

n_import_I <- rpois(epsilon)

## Total population size, and number of infecteds
I <- Is + Iv
D <- Ds + Dv
N <- S + I + R + V + D

## Initial states
initial(S) <- S_ini
initial(Is) <- I_ini
initial(Iv) <- 0
initial(R) <- 0
initial(V) <- 0
initial(Ds) <- 0
initial(Dv) <- 0

## User defined parameters - default in parentheses:
S_ini <- user(1000) # susceptibles
I_ini <- user(1) # infected
beta <- user(0.3) # infection rate
gamma <- user(0.08) # recovery rate
gamma_v <- user(0.08) # recovery rate for breakthroughs
mu <- user(0.7) # CFR
mu_v <- user(0.35) # CFR for breakthroughs
omega <- user(0.005) # rate of waning immunity
epsilon <- user(0.1) # import case rate
sigma <- user(0.01) # rate of waning immunity among vaccinated
nu <- user(0.005) # vaccination rate 
theta <- user(0.9) # vaccine efficacy
