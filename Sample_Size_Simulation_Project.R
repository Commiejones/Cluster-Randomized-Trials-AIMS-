library(pwr)
library(WebPower)
library(ggplot2)
library(stargazer)


# ---- Calculating sample size using an R package ----
#shorter way of calculating sample size of a CRT
wp.crt2arm(f=0.2,n=60, icc=0.05,alpha=0.05, power=0.84, alternative="two.sided")


# ---- Calculating sample size using formulas ----
# from the meniscus data we have sample size of 60 schools with  each school
#having 60 students and ICC 0.05


#calculation for standard deviation
c=60    #sample size
m=60    #cluster size
alpha=0.05
beta=0.16
DE=3.95     #design effect
d=0.2       #effect size
n=(c*m)/DE   #no. of individuals for a IRT
z_a=qnorm(1-(alpha/2))
z_b=qnorm(1-beta)
sigma = sqrt(((d^2)*n)/(2*(z_a+z_b)^2))
sigma

# ---- Calculating sample size with varying ICCs----

# Fixed parameters
alpha <- 0.05           
power <- 0.80             
m <- 60     # vary the cluster size to check differences              
#effect_size <- 0.2       
sigma <- 1.445             

# Function to calculate sample size for varying ICC,Effect size
calculate_sample_size <- function(icc,effect_size) {
  # Calculate design effect
  design_effect <- 1 + (m - 1) * icc
  
  # Calculate required sample size
  n <- ceiling(design_effect*(qnorm(1 - alpha/2) + qnorm(power))^2 *2* sigma^2 / (effect_size^2))
  k <- ceiling(n/m)
  return(k)
}

# Varying ICC values
icc_values <- seq(0.01, 0.1, by = 0.01)  # ICC values from 0 to 0.1
effect_size_values <- seq(0.1, 1.0, by = 0.1)

# Create a data frame to hold results
results_1 <- data.frame()
# Calculate effect sizes for each combination of sample size and power
for (icc in icc_values) {
  sample_sizes <- sapply(icc, calculate_sample_size,effect_size_values)
  results_1 <- rbind(results_1, data.frame(ICC = icc, SampleSize = sample_sizes, EffectSize=effect_size_values))
}


print(results_1)

# Plot the results
ggplot(results_1, aes(x = EffectSize, y = SampleSize,color=as.factor(ICC))) +
  geom_line() +
  labs(x = "Effect Size",
       y = "Number of Clusters",
       color="ICC") +
  theme_minimal()+
  theme(axis.title.x = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size = 16,face = "bold"),
        axis.text.x = element_text(size = 16,face = "bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        legend.title = element_text(size = 16,face = "bold"),
        legend.text = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))




# ---- Calculating Power varying the Sample size and cluster size for small effect size(0.2) ----

# Fixed parameters
alpha <- 0.05           
#m <- 60 
icc<-0.05
effect_size <- 0.2 # vary the effect size to see the difference
#sample_size
sigma <- 1.445             

# Function to calculate power for varying Sample size and cluster size
calculate_power <- function(sample_size,m) {
  # Calculate design effect
  design_effect <- 1 + (m - 1) * icc
  
  # Calculate required power 
  Z_power <- round((effect_size/sigma)*sqrt((m*sample_size)/(2*design_effect))- qnorm(1 - alpha/2),4)
  power <- pnorm(Z_power)
  
  return(power)
}

# Varying sample_size values
sample_size_values <- seq(10, 100, by = 10)  # sample_size values from 10 to 100
m_values <- seq(10, 100, by = 10)  # m values from 10 to100

# Create a data frame to hold results
results_2 <- data.frame()
# Calculate effect sizes for each combination of sample size and power
for (sample in sample_size_values) {
  power_values <- sapply(sample, calculate_power,m_values)
  results_2 <- rbind(results_2, data.frame(Sample_Size = sample,Power = power_values, ClusterSize = m_values))
}

print(results_2)


# Plot the results
ggplot(results_2, aes(x =ClusterSize , y = Power, color=as.factor(Sample_Size))) +
  geom_line() +
  labs(x = "Cluster Size",
       y = "Power", 
       color="Number of Clusters") +
  theme_minimal()+
  theme(axis.title.x = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size = 16,face = "bold"),
        axis.text.x = element_text(size = 16,face = "bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        legend.title = element_text(size = 16,face = "bold"),
        legend.text = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))




# ---- Calculating Power varying the ICC and cluster size for sample size(60) ----

# Fixed parameters
alpha <- 0.05           
#m <- 60 
#icc<-0.05
effect_size <- 0.2 #vary to see the patttern
sample_size<-60  ##check for other sample sizes
sigma <- 1.445             

# Function to calculate power for varying ICC and cluster size
calculate_power <- function(icc,m) {
  # Calculate design effect
  design_effect <- 1 + (m - 1) * icc
  
  # Calculate required power 
  Z_power <- round((effect_size/sigma)*sqrt((m*sample_size)/(2*design_effect))- qnorm(1 - alpha/2),4)
  power <- pnorm(Z_power)
  
  return(power)
}

# Varying sample_size values
icc_values <- seq(0.01, 0.1, by = 0.01)  # sample_size values from 10 to 100
m_values <- seq(10, 100, by = 10)  # m values from 10 to100

# Create a data frame to hold results
results_3 <- data.frame()
# Calculate effect sizes for each combination of sample size and power
for (icc in icc_values) {
  power_values <- sapply(icc, calculate_power,m_values)
  results_3 <- rbind(results_3, data.frame(ICC = icc,Power = power_values, ClusterSize = m_values))
}

print(results_3)


# Plot the results
ggplot(results_3, aes(x =ClusterSize , y = Power, color=as.factor(ICC))) +
  geom_line() +
  labs(x = "Cluster Size",
       y = "Power", 
       color="ICC") +
  theme_minimal()+
  theme(axis.title.x = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size = 16,face = "bold"),
        axis.text.x = element_text(size = 16,face = "bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        legend.title = element_text(size = 16,face = "bold"),
        legend.text = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))



# ---- Calculating sample size varying the size of each cluster and effect size----
# Fixed parameters
alpha <- 0.05           
power <- 0.84             
#m <- 60 
icc<-0.05
effect_size <- 0.2      
#sigma <- 1.445             

# Function to calculate sample size for varying cluster size 
calculate_sample_size <- function(m,sigma) {
  # Calculate design effect
  design_effect <- 1 + (m - 1) * icc
  
  # Calculate required sample size
  n <- ceiling(design_effect*(qnorm(1 - alpha/2) + qnorm(power))^2 *2* sigma^2 / (effect_size^2))
  k <- ceiling(n/m)
  return(k)
}

# Varying cluster size values
m_values <- seq(10, 100, by = 10)  
#varyijg standard deviations
sigma_values <- seq(1, 5, by = 0.2)

# Create a data frame to hold results
results_4 <- data.frame()
# Calculate effect sizes for each combination of sample size and power
for (m in m_values) {
  sample_sizes <- sapply(m, calculate_sample_size,sigma_values)
  results_4 <- rbind(results_4, data.frame(Cluster_sizes = m, SampleSize = sample_sizes, Sigma=sigma_values))
}

print(results_4)

# Plot the results
ggplot(results_4, aes(x = Sigma, y = SampleSize,colour = as.factor(Cluster_sizes))) +
  geom_line() +
  labs(x = "Standard deviation",
       y = "Number of Clusters",
       color="Cluster Size") +
  theme_minimal()+
  theme(axis.title.x = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size = 16,face = "bold"),
        axis.text.x = element_text(size = 16,face = "bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        legend.title = element_text(size = 16,face = "bold"),
        legend.text = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))


calculate_sample_size(60,1.445)


# ---- Calculating Effect size size varying the ICC  and Sample size ----

# Fixed parameters
alpha <- 0.05           
power <- 0.80            
m <- 60    # vary the cluster size to see the differences
#icc<-0.05
#effect_size <- 0.2
#sample_size=60
sigma <- 1.445      # vary the std deviation too       

# Function to calculate Effect size for varying Sample size and cluster size 
calculate_effect_size <- function(icc,sample_size) {
  # Calculate design effect
  design_effect <- 1 + (m - 1) * icc
  
  # Calculate required sample size
  effect_size <- round(sqrt((design_effect*(qnorm(1 - alpha/2) + qnorm(power))^2 *2* sigma^2) / (m*sample_size)),2)
  
  return(effect_size)
}
#varying icc values
icc_values <- seq(0.01, 0.1, by = 0.01)

# Varying sample_size values
sample_size_values <- seq(10, 100, by = 10)  # sample_size values from 10 to 100



# Create a data frame to hold results
results_5 <- data.frame()
# Calculate effect sizes for each combination of sample size and power
for (icc in icc_values) {
  effect_sizes <- sapply(icc, calculate_effect_size, sample_size_values)
  results_5 <- rbind(results_5, data.frame(ICC =  icc,  Sample_sizes= sample_size_values, EffectSize = effect_sizes))
}

print(results_5)


# Plot the results
ggplot(results_5, aes(x = ICC, y = EffectSize, color=as.factor(Sample_sizes))) +
  geom_line() +
  labs(x = "ICC",
       y = "Effect Size",
       color="Number of Clusters") +
  theme_minimal()+
  theme(axis.title.x = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size = 16,face = "bold"),
        axis.text.x = element_text(size = 16,face = "bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        legend.title = element_text(size = 16,face = "bold"),
        legend.text = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))




# ---- Calculating sample size with varying ICCs----

# Fixed parameters
alpha <- 0.05           
#power <- 0.80             
#m <- 60     # vary the cluster size to check differences              
effect_size <- 0.2 
icc<-0.05
sigma <- 1.445             

# Function to calculate sample size for varying ICC,Effect size
calculate_sample_size <- function(m,power) {
  # Calculate design effect
  design_effect <- 1 + (m - 1) * icc
  
  # Calculate required sample size
  n <- ceiling(design_effect*(qnorm(1 - alpha/2) + qnorm(power))^2 *2* sigma^2 / (effect_size^2))
  k <- ceiling(n/m)
  return(k)
}

# Varying cluster size values
m_values <- seq(10, 100, by = 10)  
#varying power values
power_values <- seq(0.1, 1.0, by = 0.1)

# Create a data frame to hold results
results_6 <- data.frame()
# Calculate effect sizes for each combination of sample size and power
for (m in m_values) {
  sample_sizes <- sapply(m, calculate_sample_size,power_values)
  results_6 <- rbind(results_6, data.frame(ClusterSize = m, SampleSize = sample_sizes, Power=power_values))
}


print(results_6)


# Plot the results
ggplot(results_6, aes(x = Power, y = SampleSize,color=as.factor(ClusterSize))) +
  geom_line() +
  labs(x = "Power",
       y = "Number of Clusters",
       color = "Cluster Size") +
  theme_minimal()+
  theme(axis.title.x = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size = 16,face = "bold"),
        axis.text.x = element_text(size = 16,face = "bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        legend.title = element_text(size = 16,face = "bold"),
        legend.text = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))


# ---- Calculating cluster size with varying ICCs----

# Fixed parameters
alpha <- 0.05           
power <- 0.84             
c<- 60     # vary the cluster size to check differences   
#effect_size <- 0.2 
#m<-60
#icc<-0.05
sigma <- 1.445 


# Function to calculate cluster size for varying ICC,Effect size
calculate_cluster_size <- function(effect_size, icc, c) {
  # Calculate required sample size
  
  n <- 2 * sigma^2 * ((qnorm(1 - alpha / 2) + qnorm(power))^2 / effect_size^2)
  cluster_size <- numeric(length(effect_size))
  
  for (i in seq_along(effect_size)) {
    if (c[i] > n[i] * icc[i]) {
      cluster_size[i] <- ceiling((n[i] * (1 - icc[i])) / (c[i] - (n[i] * icc[i])))
    } else {
      cluster_size[i] <- NA
    }
  }
  
  return(cluster_size)
}

# Varying effect_size values
effect_size_values <- seq(0.1, 1.0, by = 0.1)
# Varying ICC values
icc_values <- seq(0.01, 0.1, by = 0.01)
# Varying c values
c_values <- rep(60, length(effect_size_values))

# Create a data frame to hold results
results_7 <- data.frame(
  EffectSize = effect_size_values,
  ICC = rep(icc_values, length(effect_size_values)),
  ClusterSize = calculate_cluster_size(effect_size_values, icc_values, c_values)
)

print(results_7)

calculate_cluster_size(effect_size = 0.20,icc=0.05, c= 60)

   # Plot the results
ggplot(results_7, aes(x = EffectSize, y = ClusterSize,color=as.factor(ICC))) +
  geom_line() +
  labs(title = "Effect Size vs. Cluster Size for a CRT",
       x = "Effect Size",
       y = "Cluster Size",
       color = "ICC") +
  theme_minimal()+
  theme(axis.title.x = element_text(size = 16,face = "bold"),
        axis.title.y = element_text(size = 16,face = "bold"),
        axis.text.x = element_text(size = 16,face = "bold"),
        axis.text.y = element_text(size = 16,face = "bold"),
        legend.title = element_text(size = 16,face = "bold"),
        legend.text = element_text(size = 16,face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))


ggplot(results_7, aes(x = ICC, y = ClusterSize, color = factor(EffectSize))) +
  geom_point(na.rm = TRUE) +
  scale_color_discrete(name = "Effect Size") +
  labs(
    title = "Cluster Size vs Effect Size and ICC",
    x = "ICC",
    y = "Cluster Size"
  ) +
  theme_minimal()






























