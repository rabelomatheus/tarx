#rm(list=ls())

################################################### 
# Reproduzindo a figura 7.2 do Enders (panel(d)) #
###################################################

# Coeficiente autoregressivo de primeira ordem
a1 <- 0.7
# Gerando 200 números aleatórios de uma Normal(0,1)
et <- rnorm(200)
# Gerando o processo AR(1), com primeiro elemento igual ao primeiro choque
y <- numeric(200)
y[1] <- et[1]
  
    for (i in 1:199){
      y[i+1] = a1*y[i] + et[i+1]
    }

#plot(y, type="l")

# Criando a dummy "i" para usar no TAR
i  <- numeric(200)

    for (j in 1:200){
  
      if(y[j] > 0){
          i[j] = 1
            } else {
              i[j] = 0
          }
    }
# Alternativa ao loop para criar as indicadoras
ind1 <-ifelse(y>0,1,0)
ind2 <- ifelse(y>0,0,1)

# Gerando o processo TAR
ytar <- numeric(200)
ytar[1] <- et[1]

    for (k in 1:199){
      ytar[k+1] <- (1-a1)*ind1[k]*y[k] + a1*ind2[k]*y[k] + et[k]
    }


#plot(ytar, type="l")

par(mfrow=c(2,1))
plot(y, type="l", main="AR(1)")
plot(ytar, type="l", main="TAR")


# estimando um TAR arbitrário
z <- ts.intersect(as.ts(y), lag(as.ts(y), -1))
x1 <- ind1[2:length(ind1)]*z[,2]
x2 <- ind2[2:length(ind1)]*z[,2]

fit <- lm(z[,1]~ x1+x2 -1)
summary(fit)

############################################################################################ 

y = ytar

t_grid <- numeric(length(y))
for(i in 1:length(t_grid)){
  t_grid[i] = ifelse((y[i]>quantile(y,0.15))&(y[i]<quantile(y,0.85)),y[i],NA)
  
}
t_grid = na.exclude(t_grid)

###########################################################################################

sse = numeric(length(t_grid))
for(i in 1:length(t_grid)){
 sse[i] = 
   sum((lm(y ~ ifelse(y>t_grid[i],1,0)*lag(y,1) + (1-ifelse(y>t_grid[i],1,0))*lag(y,1) -1)$residuals)^2)
}
###########################################################################################


#rm(list=ls())
#########################################################
# USANDO O PACOTE rbcb p/ download de séries do SGS/BCB #
#########################################################

# Para instalar o package:
#     install.packages('devtools')
#     install.packages('stringi')
#     install.packages('magrittr')
#     install.packages('tibble')
#     install.packages('xts')
#     devtools::install_github('wilsonfreitas/rbcb')

# Códigos das séries:
#     IPCA (var % mensal): 433
#     IBC-Br sem ajuste sazonal: 24363
#     IBC-Br com ajuste sazonal: 24364

library(rbcb)

ipca <- rbcb::get_series(433, last=48)

plot(ipca, type="l")

qlqr merda
