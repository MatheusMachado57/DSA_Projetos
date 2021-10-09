
### File descriptions

# train.csv - the training set

# train_sample.csv - 100,000 randomly-selected rows of
# training data, to inspect data before downloading full set

# test.csv - the test set

# sampleSubmission.csv - a sample submission file 
# in the correct format

# UPDATE: test_supplement.csv - This is a larger test
# set that was unintentionally released at the start of
# the competition. It is not necessary to use this data, 
# but it is permitted to do so. The official test data is
# a subset of this data.

### Data fields

# Each row of the training data contains a click record,
# with the following features.

# ip: ip address of click.

# app: app id for marketing.

# device: device type id of user mobile phone 
# (e.g., iphone 6 plus, iphone 7, huawei mate 7, etc.)

# os: os version id of user mobile phone

# channel: channel id of mobile ad publisher

# click_time: timestamp of click (UTC)

# attributed_time: if user download the app for after 
# clicking an ad, this is the time of the app download

# is_attributed: the target that is to be predicted, 
# indicating the app was downloaded

### Note that ip, app, device, os, and channel are encoded.

### The test data is similar, with the following differences:
  
# click_id: reference for making predictions

# is_attributed: not included

###########################################

### Campos de dados

# Cada linha dos dados de treinamento contam um registro de clique,
# com os seguintes recursos.

# ip: endere?o IP do clique.

# app: ID do aplicativo para marketing.

# device: ID do tipo de dispositivo do celular do usu?rio
# (por exemplo, iphone 6 plus, iphone 7, huawei mate 7 etc.)

# os: ID da vers?o do telefone m?vel do usu?rio

# channel: ID do canal do editor de an?ncios para celular

# click_time: registro de data e hora do clique (UTC)

# attribute_time: se o usu?rio baixar o aplicativo 
# para depois de clicar em um an?ncio, ? o momento 
# do download do aplicativo

# is_attributed: o destino a ser previsto, indicando 
# que o aplicativo foi baixado

require(readr)
require(dplyr)
library(lubridate)
require(stringr)

Train <- read_csv("train_sample.csv")

set.seed(219054066)

# Separação entre Treino e Teste
inTrain <- caret::createDataPartition(Train$is_attributed, 
                                      p = 0.75, 
                                      list = F)

treino  <- Train[ inTrain,]

treino <- treino[,-7]

# Mudando a classe das variáveis

treino$ip       <- as.factor(treino$ip)
treino$app      <- as.factor(treino$app)
treino$device   <- as.factor(treino$device)
treino$os       <- as.factor(treino$os)
treino$channel  <- as.factor(treino$channel)
treino$is_attributed <- as.factor(treino$is_attributed)

# Ordenando pelos Ip´s
treino$ip <- sort(treino$ip)

##### Criando uma nova variável baseada na quantidade 
#     de vezes que um mesmo ID apareceu

aux1 <- treino %>% group_by(ip)
aux2 <- count(aux1)

aux2    <- aux2 %>% group_by(ip)
aux2$ip <- as.numeric(aux2$ip)

quant <- c()

aux2$ip = as.integer(aux2$ip)

for(i in 1:nrow(aux2)){quant <- c(quant, 
                                  rep(aux2$n[i], aux2$n[i]))}

treino <- cbind(treino, quant)

rm(aux1)
rm(aux2)
rm(quant)

# Separando a variável click_time em duas: data e hora

treino$click_time <- (gsub(" UTC", "", treino$click_time))

time0 <- str_extract_all(treino$click_time, "\\d+")

Dt <- c()
Hr <- c()
Mi <- c()

for(i in 1:length(time0)){
    Dt[i] <- str_glue(time0[[i]][[1]],"-",
                      time0[[i]][[2]],"-",
                      time0[[i]][[3]])
    
    Hr[i] <- time0[[i]][[4]]
}

Dt <- as.Date(Dt)

treino <- cbind(treino, Hr)

treino <- treino[,-6]

treino$quant <- as.factor(treino$quant)

###################################################

# Agrupando todas as variantes das variáveis qualitativas em 6 grupos;
# as cinco variantes mais frequentes e todos os outros serão agrupados em Outros;
# Isso está sendo feito no intuito de facilitar a criação e utilização do modelo;

r <- list()

for(i in 1:ncol(treino)){
    for(j in 1:length(rownames(as.matrix(sort(summary(treino[,1]), 
                                              decreasing = TRUE))))){
        if(is.na(rownames(as.matrix(sort(summary(treino[,i]), 
                                   decreasing = TRUE)))[j])){}
        else{
        r[i][[1]][[j]] <- rownames(as.matrix(sort(summary(treino[,i]), 
                                   decreasing = TRUE)))[j]
}}}

treino <- as.matrix(treino)

# Transformando todas as variantes que não sejam as 5 mais frequentes em "Outros";

for(i in 1:ncol(treino)){
  # O valor i varia entre 1 e o total de colunas
  
  if(length(r[[i]]) > 5){
    # Só realizar o processo se a variável possuir mais de 5 variantes
    
    for(j in 1:nrow(treino)){
      # O valor j varia entre 1 e o total de linhas, que é 18904;
      
      if(is.na(treino[j,i])){a = 1}
      else{
        
        if(treino[j,i] == r[[i]][[1]] || 
           treino[j,i] == r[[i]][[2]] || 
           treino[j,i] == r[[i]][[3]] || 
           treino[j,i] == r[[i]][[4]] ||
           treino[j,i] == r[[i]][[5]]){a = 1}
        
        else{treino[j,i] = "Outro"}
      }}
  }
}

treino <- as.data.frame(treino)

# Criando modelo 

ctrl      <- caret::trainControl(method = "oob")

model_rf  <- caret::train(is_attributed ~ ., 
                          data      = treino,
                          method    = "rf",
                          ntree     = 100,
                          trControl = ctrl)

save(model_rf, r, file = "Projeto_1.RData")

###########################################################
###########################################################
###########################################################

Train <- read_csv("train_sample.csv")

set.seed(219054066)

# Separação entre Treino e Teste
inTrain <- caret::createDataPartition(Train$is_attributed, 
                                      p = 0.75, 
                                      list = F)

teste <- Train[-inTrain,]

load("Projeto_1.RData")

# Realizando o tratamento feito na base treino na base teste

teste <- teste[,-7]

# Mudando a classe das variáveis

teste$ip       <- as.factor(teste$ip)
teste$app      <- as.factor(teste$app)
teste$device   <- as.factor(teste$device)
teste$os       <- as.factor(teste$os)
teste$channel  <- as.factor(teste$channel)
teste$is_attributed <- as.factor(teste$is_attributed)

# Ordenando pelos Ip´s
teste$ip <- sort(teste$ip)

##### Criando uma nova variável baseada na quantidade 
#     de vezes que um mesmo ID apareceu

aux1 <- teste %>% group_by(ip)
aux2 <- count(aux1)

aux2    <- aux2 %>% group_by(ip)
aux2$ip <- as.numeric(aux2$ip)

quant <- c()

aux2$ip = as.integer(aux2$ip)

for(i in 1:nrow(aux2)){quant <- c(quant, 
                                  rep(aux2$n[i], aux2$n[i]))}

teste <- cbind(teste, quant)

rm(aux1)
rm(aux2)
rm(quant)

# Separando a variável click_time em duas: data e hora

teste$click_time <- (gsub(" UTC", "", teste$click_time))

time0 <- str_extract_all(teste$click_time, "\\d+")

Hr <- c()

for(i in 1:length(time0)){
  Hr[i] <- time0[[i]][[4]]
}

teste <- cbind(teste, Hr)

teste <- teste[,-6]

teste$quant <- as.factor(teste$quant)

teste <- as.matrix(teste)

# Transformando todas as variantes que não sejam as 5 mais frequentes em "Outros";

for(i in 1:ncol(teste)){
  # O valor i varia entre 1 e o total de colunas
  
  if(length(r[[i]]) > 5){
    # Só realizar o processo se a variável possuir mais de 5 variantes
    
    for(j in 1:nrow(teste)){
      # O valor j varia entre 1 e o total de linhas, que é 18904;
      
      if(is.na(teste[j,i])){a = 1}
      else{
        
        if(teste[j,i] == r[[i]][[1]] || 
           teste[j,i] == r[[i]][[2]] || 
           teste[j,i] == r[[i]][[3]] || 
           teste[j,i] == r[[i]][[4]] ||
           teste[j,i] == r[[i]][[5]]){a = 1}
        
        else{teste[j,i] = "Outro"}
      }}
  }
}

teste <- as.data.frame(teste)

#################################################
#################################################
#################################################

# Fazendo previsão e conferindo se deu certo;

pred <- predict(model_rf, teste)

caret::confusionMatrix(pred, teste$is_attributed)

