# Leitura dos pacotes necessários
library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)
library(scales)
library(ggformula)
library(microdatasus)
library(writexl)
library(tibble)
library(crosstable)

# Definir o diretório onde estarão salvos os bancos de dados 
# setwd("C:/Users/TCC Gabriel Pozza Estivalete/Codigos_para_Camila")

################################################################################
# Extração dos dados do DATASUS
# No comando abaixo, escolha os anos e a unidade da federação brasileira de interesse,
# e não altere os outros argumentos. 
ano_inicial <- 2010
ano_final <- 2011
dados_sus1 <- fetch_datasus(year_start = ano_inicial, year_end = ano_final, uf = "RS", information_system = "SIM-DO")

# Função do pacote microdatasus para processar os dados lidos no comando anterior 
dados_sus2 <- process_sim(dados_sus1)

# Filtro para a cidade de Porto Alegre
dados_sus <- dados_sus2 %>% filter(munResNome == "Porto Alegre")

# Transformação da variável idade
# A variável IDADE extraida do DATASUS é codificada da seguinte maneira:
# O primeiro algarismo representa a unidade de tempo, sendo 
# 0 = minutos, 1 = horas, 2 = dias, 3 = meses, 4 = anos, 5 = anos acima de 100
# O comando abaixo transforma os valores que começam com os algarismos 0,1,2 ou 3 em idade = 0,
# os valores que começam com o algarismo 1 são a idade igual aos dois últimos números,
# e os valores que começam com o algarismo 5 vão começar com 1, indicando uma centena
dados_sus$idade <- case_when(startsWith(dados_sus$IDADE, "0") ~ "0",
                             startsWith(dados_sus$IDADE, "1") ~ "0",
                             startsWith(dados_sus$IDADE, "2") ~ "0",
                             startsWith(dados_sus$IDADE, "3") ~ "0",
                             startsWith(dados_sus$IDADE, "4") ~ sub('.','',dados_sus$IDADE),
                             startsWith(dados_sus$IDADE, "5") ~ sub('.','1',dados_sus$IDADE))

# Transformação da variável idade criada em variável numérica
dados_sus$idade <- as.numeric(dados_sus$idade)
# Extraindo o ano da variável data de óbito para criar a variável ano do óbito
dados_sus$ano_obito <- as.numeric(substr(dados_sus$DTOBITO,1,4))

# Definindo o número de colunas a serem criadas para o banco de mortalidade
n_colunas <- ano_final - ano_inicial + 1

# Criação dos bancos de mortalidade para cada sexo
### Filtrando o banco para o sexo masculino
dados_sus_masc <- dados_sus %>% filter(SEXO == "Masculino")

# Matriz com 111 linhas, uma para cada idade de 0 até 110 anos, e 2 colunas, com base nos anos indicados
masc=matrix(rep(0,111*n_colunas),ncol = n_colunas)

# Preenchendo as colunas com o número de óbitos registrados em cada ano, para cada idade de 0 a 110 
for (i in 1:111) {
  for (j in 1:n_colunas) {
    masc[i,j] = sum(dados_sus_masc$idade==i-1 & dados_sus_masc$ano_obito == ano_inicial-1+j,na.rm=TRUE)
  }
}

# Dataframe com coluna indicado a idade
obitos_masc = data.frame(idade = seq(0,110))

# Completando o dataframe com as colunas de óbitos para cada ano, e nomeando as colunas no padrão "a20xx"
for (i in 1:n_colunas) {
  obitos_masc = cbind(obitos_masc, masc[,i])
  colnames(obitos_masc)[i+1] <- paste0("a", ano_inicial+i-1)
}

### Repetição do processo para o sexo feminino
dados_sus_fem <- dados_sus %>% filter(SEXO == "Feminino")

fem=matrix(rep(0,111*n_colunas),ncol=n_colunas)
for (i in 1:111) {
  for (j in 1:n_colunas) {
    fem[i,j] = sum(dados_sus_fem$idade==i-1 & dados_sus_fem$ano_obito==ano_inicial-1+j,na.rm=TRUE)
  }
}

obitos_fem = data.frame(idade = seq(0,110))
for (i in 1:n_colunas) {
  obitos_fem = cbind(obitos_fem, fem[,i])
  colnames(obitos_fem)[i+1] <- paste0("a", ano_inicial+i-1)
}

# Comando para salvar os bancos criados no formato csv
write.csv2(obitos_masc,"Porto_Alegre_masc_2010_2021_obitos.csv")
write.csv2(obitos_fem, "Porto_Alegre_fem_2010_2021_obitos.csv")

################################################################################
################################################################################

# Leitura do banco de população residente extraído do site do SIDRA
# Neste banco, a idade é apresentada até a faixa de 100 anos ou mais
sidra_2010 <- read_excel("Tabela_1378.xlsx", 
                               skip = 7, n_max = 102)

# Nomeando a primeira coluna para Idade
colnames(sidra_2010)[1] <- "Idade"

# Retirando a linha e a coluna representando a população total
sidra_2010 <- sidra_2010[-1,-2]

# Criando objeto com a população com 100 anos ou mais, e retirando a linha do banco
pop_100mais = sidra_2010[101,]
sidra_2010 <- sidra_2010[-101,]

# Adicionando as linhas para as idades maiores de 99 anos 
sidra_2010[101:111,1] <- c("100 anos", "101 anos", "102 anos", "103 anos", 
                           "104 anos", "105 anos", "106 anos", "107 anos", 
                           "108 anos", "109 anos", "110 anos")

# Visualizar quantas pessoas possuem 100 anos ou mais para cada sexo
pop_100mais

# Dividindo a população de 100 anos ou mais em idades individuais até 110 anos, para o sexo masculino
criacaoM <- c(6,5,4,3,2,1,1,1,1,0,0)
# Conferir se soma o valor do objeto "pop_100mais"
sum(criacaoM)
# Preenchendo as linhas vazias com as idades de 100 a 110 anos
sidra_2010[101:111,2] <- criacaoM

# Criação de vetor com 31 valores
masc=rep(0,31)

# Criação do vetor com as proporções de pessoas para cada idade acima de 79 anos 
# em relação ao total de pessoas com 80 anos ou mais
for (i in 1:31) masc[i] <- sidra_2010[i+80, 2]/sum(sidra_2010$Homens[81:111])

# Incluindo o vetor criado no banco de dados
sidra_2010$perc_masc=as.numeric(c(rep(1,80),masc))

# Conferindo se as proporções somam 1
sum(sidra_2010$perc_masc[81:111])

# Dividindo a população de 100 anos ou mais em idades individuais até 110 anos, para o sexo feminino
criacaoF <- c(39,30,23,15,7,6,3,2,1,0,0)

# Conferir se soma o valor do objeto "pop_100mais"
sum(criacaoF)

# Preenchendo as linhas vazias com as idades de 100 a 110 anos
sidra_2010[101:111,3] <- criacaoF

# Criação de vetor com 31 valores
fem=rep(0,31)

# Criação do vetor com as proporções de pessoas para cada idade acima de 79 anos 
# em relação ao total de pessoas com 80 anos ou mais
for (i in 1:31) fem[i] <- sidra_2010[i+80,3]/sum(sidra_2010$Mulheres[81:111])

# Incluindo o vetor criado no banco de dados
sidra_2010$perc_fem=as.numeric(c(rep(1,80),fem))

# Conferindo se as proporções somam 1
sum(sidra_2010$perc_fem[81:111])

# Leitura do banco de dados de população residente do DATASUS, para os anos de 2010 adiante
# Neste banco, as idades são apresentadas somente até a faixa de 80 anos ou mais
# Masculino
datasus_masc <- read.csv2("datasus_2010_2021_masc_poa.csv", skip = 5)
# Retirando linhas vazias
datasus_masc <- datasus_masc[-c(82:85),]
# Retirando a linha de "80 anos ou mais", para completar até os 110 anos
datasus_masc1 <- datasus_masc[-81,]

# Dataframe com uma coluna de idades, coluna com a população até os 79 anos, baseado no banco do SIDRA,
# e uma coluna com as proporções que serão utilizadas para estimar a população para cada idade acima de 79 anos
valores_masc <- data.frame(Idade_simples = sidra_2010[,1],
                           p2010=sidra_2010[,2],
                           prop=c(sidra_2010[,4]))

# Criando as colunas para os anos além de 2010, até o ano final indicado
# Para os valores até a idade de 79 anos, são utilizadas as informações do banco do DATASUS
# Para os valores acima de 79 anos, as linhas são preenchidas com a população de 80 anos ou mais para
# o respectivo ano
for (i in 1:(n_colunas-1)) {
  valores_masc <- cbind(valores_masc, c(datasus_masc1[,i+2],rep(datasus_masc[81,i+2],31)))
  colnames(valores_masc)[i+3] <- paste0("p", ano_inicial+i)
}

# Renomeando a coluna de proporções
valores_masc=rename(valores_masc,perc=perc_masc)

# Multiplicando os valores para cada idade acima de 79 anos pela respectiva proporção
for (i in 1:(n_colunas-1)) {
  valores_masc[,i+3] = valores_masc$perc*valores_masc[i+3]
}

# Renomeando a coluna para o ano de 2010
valores_masc=rename(valores_masc,p2010=Homens)
# Retirando a coluna das proporções
pop_masc = valores_masc[,-3]

# Repetindo o processo para o sexo feminino
datasus_fem <- read.csv2("datasus_2010_2021_fem_poa.csv", skip = 5)
datasus_fem <- datasus_fem[-c(82:85),]
datasus_fem1 <- datasus_fem[-81,]

valores_fem <- data.frame(Idade_simples = sidra_2010[,1],
                           p2010=sidra_2010[,3],
                           prop=c(sidra_2010[,5]))

for (i in 1:(n_colunas-1)) {
  valores_fem <- cbind(valores_fem, c(datasus_fem1[,i+2],rep(datasus_fem[81,i+2],31)))
  colnames(valores_fem)[i+3] <- paste0("p", ano_inicial+i)
}

valores_fem=rename(valores_fem,perc=perc_fem)

for (i in 1:(n_colunas-1)) {
  valores_fem[,i+3] = valores_fem$perc*valores_fem[i+3]
}

valores_fem=rename(valores_fem,p2010=Mulheres)

valores_fem = valores_fem[,-3]
pop_fem=valores_fem[,c(1:(n_colunas+1))]


# Comando para baixar as tabelas em formato csv
write.csv2(pop_fem,"Porto_Alegre_fem_2010_2021_populacao.csv")
write.csv2(pop_masc,"Porto_Alegre_masc_2010_2021_populacao.csv")



