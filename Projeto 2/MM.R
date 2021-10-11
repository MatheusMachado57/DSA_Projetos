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

base <- read.csv("Base0.csv"); base[,-1]

# Análise Exploratória

str(base0)
