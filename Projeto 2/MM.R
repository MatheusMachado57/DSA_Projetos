# base = read.csv("test.csv")

# set.seed(219054066)

# Separação entre Treino e Teste
# inTrain <- caret::createDataPartition(base$id, 
#                                       p = 0.1, 
#                                       list = F)

# treino  <- base[ inTrain,]
# write.csv(treino, "MM.csv")

base1 <- read.csv("MM.csv"); base1 <- base1[,-1]
base2 <- read.csv("cliente_tabla.csv")
base3 <- read.csv("producto_tabla.csv")
base4 <- read.csv("sample_submission.csv")
base5 <- read.csv("town_state.csv")

require(dplyr)

base0 <- left_join(base1, base2, by = "Cliente_ID")
base0 <- left_join(base0, base3, by = "Producto_ID")
base0 <- left_join(base0, base4, by = "id")
base0 <- left_join(base0, base5, by = "Agencia_ID")

rm(base1)
rm(base2)
rm(base3)
rm(base4)
rm(base5)

write.csv(base0, "Base0.csv")

##################################################
################################################## Here begins !!!
##################################################

base <- read.csv("Base0.csv")
base <- base[,-c(1,11)]

# Análise Exploratória

base$id          <- as.factor(base$id)
base$Semana      <- as.factor(base$Semana)
base$Agencia_ID  <- as.factor(base$Agencia_ID)
base$Canal_ID    <- as.factor(base$Canal_ID)
base$Ruta_SAK    <- as.factor(base$Ruta_SAK)
base$Cliente_ID  <- as.factor(base$Cliente_ID)
base$Producto_ID <- as.factor(base$Producto_ID)

summary(base)

###################################################

# Agrupando todas as variantes das variáveis qualitativas em 6 grupos;
# as cinco variantes mais frequentes e todos os outros serão agrupados em Outros;
# Isso está sendo feito no intuito de facilitar a criação e utilização do modelo;

r <- list()

for(i in 1:ncol(base)){
  for(j in 1:length(rownames(as.matrix(sort(summary(base[,1]), 
                                            decreasing = TRUE))))){
      
      if(is.na(rownames(as.matrix(sort(summary(base[,i]), 
                                     decreasing = TRUE)))[j])){}
      
      else{r[i][[1]][[j]] <- rownames(as.matrix(sort(summary(base[,i]), 
                                                decreasing = TRUE)))[j]
    }}}

head(r)

treino <- as.matrix(base)

# Transformando todas as variantes que não sejam as 10 mais frequentes em "Outros";
for(i in 1:ncol(treino)){
  # O valor i varia entre 1 e o total de colunas
  
  if(length(r[[i]]) > 10){
    # Só realizar o processo se a variável possuir mais de 10 variantes
    
    for(j in 1:nrow(treino)){
      # O valor j varia entre 1 e o total de linhas;
      
      if(is.na(treino[j,i])){a = 1}
      else{
        if(any(treino[j,i] == c(r[[i]][[1]],
                              r[[i]][[2]], 
                              r[[i]][[3]], 
                              r[[i]][[4]],
                              r[[i]][[5]],
                              r[[i]][[6]],
                              r[[i]][[7]],
                              r[[i]][[8]],
                              r[[i]][[9]],
                              r[[i]][[10]]))){a = 1}
        
        else{treino[j,i] = "Outro"}
      }}
  }
}

# Transformando todas as variantes que não sejam as 5 mais frequentes em "Outros";
for(i in 1:ncol(treino)){
  # O valor i varia entre 1 e o total de colunas
  
  if(length(r[[i]]) <= 10 & length(r[[i]]) > 5){
    # Só realizar o processo se a variável possuir mais de 5 variantes
    
    for(j in 1:nrow(treino)){
      # O valor j varia entre 1 e o total de linhas;
      
      if(is.na(treino[j,i])){a = 1}
      else{
        if(any(treino[j,i] == c(r[[i]][[1]],
                                r[[i]][[2]], 
                                r[[i]][[3]], 
                                r[[i]][[4]],
                                r[[i]][[5]]))){a = 1}
        
        else{treino[j,i] = "Outro"}
      }}
  }
}

treino <- as.data.frame(treino)

summary(treino)

write.csv(treino, "Treino.csv")


































