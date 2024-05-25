# Leitura de csv e reorganizando das colunas de interesse
dataset = read.csv2(file="crab_age.csv", sep=",")
dataset = dataset[,c(9, 1, 2, 3, 4, 5, 6, 7, 8)]
#dataset = dataset[!(dataset$Sex %in% "I"),]

# Tratando colunas categórias que deveriam ser numéricas
dataset$Length = as.numeric(dataset$Length)
dataset$Diameter = as.numeric(dataset$Diameter)
dataset$Height = as.numeric(dataset$Height)
dataset$Weight = as.numeric(dataset$Weight)
dataset$Shucked.Weight = as.numeric(dataset$Shucked.Weight)
dataset$Viscera.Weight = as.numeric(dataset$Viscera.Weight)
dataset$Shell.Weight = as.numeric(dataset$Shell.Weight)

# Plotando gráficos de dispersão entre cada variável
plot(dataset)

# Analisando correlação entre variáveis
dataset_no_sex = dataset
dataset_no_sex$Sex = NULL
cor(dataset_no_sex)

# Transformando dados
plot(dataset$Weight, dataset$Age)
dataset$Weight = I(dataset$Weight^2)
plot(dataset$Weight, dataset$Age)

# Modelo
modelo = lm(Age ~ Length + Diameter + Height + Weight + Shucked.Weight + Viscera.Weight + Shell.Weight + Sex, data=dataset)
summary(modelo)
plot(fitted(modelo), rstandard(modelo))

