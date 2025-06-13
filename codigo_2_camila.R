# Função para criar as tabelas de vida
# Para o argumento obito, completar com o ano desejado, ex. "a2010", para o argumento populacao,
# completar com o ano desejado, ex. "p2010"
funcao_tabela_expectativa <- function(pop_fem= NULL, obito_fem = NULL, pop_masc= NULL, obito_masc = NULL,ano = 2010) {
  ################################################################################
  
  # Variáveis indicando o ano, no padrão dos bancos de dados utilizados
  obito = paste0("a",ano)
  populacao = paste0("p", ano)
  
  # SEXO FEMININO

  # Dataframe com as colunas de idade, óbitos, e população, para o ano escolhido
  VT_FE_fem=data.frame(Idade=seq(0,110),  
                       D=obito_fem[[obito]],
                       N=pop_fem[[populacao]])
  
  # Criação das faixas etárias
VT_FE_fem$faixa_etaria <- ifelse(VT_FE_fem$Idade== 0,0,
                                ifelse(VT_FE_fem$Idade<= 5,5,
                                ifelse(VT_FE_fem$Idade<=10,10,
                                ifelse(VT_FE_fem$Idade<=15,15,
                                ifelse(VT_FE_fem$Idade<=20,20,
                                ifelse(VT_FE_fem$Idade<=25,25,
                                ifelse(VT_FE_fem$Idade<=30,30,
                                ifelse(VT_FE_fem$Idade<=35,35,
                                ifelse(VT_FE_fem$Idade<=40,40,
                                ifelse(VT_FE_fem$Idade<=45,45,
                                ifelse(VT_FE_fem$Idade<=50,50,
                                ifelse(VT_FE_fem$Idade<=55,55,
                                ifelse(VT_FE_fem$Idade<=60,60,
                                ifelse(VT_FE_fem$Idade<=65,65,
                                ifelse(VT_FE_fem$Idade<=70,70,
                                ifelse(VT_FE_fem$Idade<=75,75,
                                ifelse(VT_FE_fem$Idade<=80,80,
                                ifelse(VT_FE_fem$Idade<=90,90,
                                ifelse(VT_FE_fem$Idade<=100,100,110)))))))))))))))))))
  
# Agrupamento das informações por faixas etárias 
  VT_FE_fem=VT_FE_fem %>% group_by(faixa_etaria) %>% summarise(D=sum(D),N=sum(N))
  
  # Transformando a tabela em um data.frame
  VT_FE_fem=data.frame(VT_FE_fem)
  
  # Criando a variável m = Taxa de mortalidade, calculada como óbitos (D) divido pela população (N)
  VT_FE_fem$m=VT_FE_fem$D/VT_FE_fem$N
  # Transformando possíveis valores não-numéricos em 0
  VT_FE_fem$m=ifelse(is.nan(VT_FE_fem$m), 0, VT_FE_fem$m)
  
  
  ########################################
  # Suavizacao Spline Cubica na escala log
  # Criando a variável y, que representa a taxa de mortalidade na escala log, multiplicada por 100.000
  VT_FE_fem$y <- log(VT_FE_fem$m)*100000
  
  # Criação de uma função de spline cúbica com o método natural
  spline_original_fem <- splinefun(VT_FE_fem$faixa_etaria, VT_FE_fem$y, method = "natural")
  
  # Interpolação de valores para cada idade entre as faixas etárias
  new_x_fem <- seq(0, 110, length.out = 111)
  interpolated_fem_original <- spline_original_fem(new_x_fem)
  
  # Dataframe com uma coluna indicando o ano, coluna indicando a idade, coluna com a taxa de mortalidade,
  # coluna com a taxa de mortalidade na escala logaritmica, e coluna indicando o sexo
  VT_S_fem=data.frame(Year=rep(ano,length.out = 111), 
                      Age=new_x_fem,
                      mx=exp(interpolated_fem_original/100000),
                      mx2=interpolated_fem_original/100000,
                      sexo = "fem")
  
  # Com a tabela inclundo as taxas de mortalidade suavizadas, podemos criar a tabela de vida com todas as informações
  # Probabilidade de morte
  VT_S_fem$q= 1 - exp(- VT_S_fem$mx)
  # aumentando um linha na base de dados
  VT_S_fem[nrow(VT_S_fem)+1,]=rep(0,ncol(VT_S_fem))
  
  # Criando as colunas para as variáveis sobreviventes e óbitos esperados, respectivamente
  VT_S_fem$l=rep(0,nrow(VT_S_fem))
  VT_S_fem$d=rep(0,nrow(VT_S_fem))
  
  # Iniciando a variável "l" com uma populacao hipotetica de 100.000 pessoas
  VT_S_fem$l[1]=100000
  
  # Completando as colunas "l" e "d"
  for (i in 1:(nrow(VT_S_fem)-1)) {
    VT_S_fem$d[i]=VT_S_fem$l[i] * VT_S_fem$q[i]
    VT_S_fem$l[i+1]=VT_S_fem$l[i] - VT_S_fem$d[i]
  }
  
  # Criando a variável "L" = anos-pessoa vividos entre as idades x e x+n
  VT_S_fem$L=rep(0,nrow(VT_S_fem))
  for (i in 1:nrow(VT_S_fem)-1) VT_S_fem$L[i] = (VT_S_fem$l[i] + VT_S_fem$l[i+1])/2
  # Criando a variável "T" = anos-pessoa vividos acima da idade x
  VT_S_fem$T=rep(0,nrow(VT_S_fem))
  soma=0
  for (i in nrow(VT_S_fem):1) {
    soma = soma + VT_S_fem$L[i]
    VT_S_fem$T[i] = soma
  }
  # Criação da variável "e" = expectativa de vida na idade x
  for (i in 1:nrow(VT_S_fem)) VT_S_fem$e[i] = VT_S_fem$T[i] / VT_S_fem$l[i]
  # Excluindo a linha adicional 
  VT_S_fem=VT_S_fem[-112,]
  
  ################################################################################
  ################################################################################
  # SEXO MASCULINO
  
  # Dataframe com as colunas de idade, óbitos, e população, para o ano escolhido
  VT_FE_masc=data.frame(Idade=seq(0,110),  
                        D=obito_masc[[obito]],
                        N=pop_masc[[populacao]])
  
  # Criação das faixas etárias
  VT_FE_masc$faixa_etaria=ifelse(VT_FE_masc$Idade== 0,0,
                          ifelse(VT_FE_masc$Idade<= 5,5,
                          ifelse(VT_FE_masc$Idade<=10,10,
                          ifelse(VT_FE_masc$Idade<=15,15,
                          ifelse(VT_FE_masc$Idade<=20,20,
                          ifelse(VT_FE_masc$Idade<=25,25,
                          ifelse(VT_FE_masc$Idade<=30,30,
                          ifelse(VT_FE_masc$Idade<=35,35,
                          ifelse(VT_FE_masc$Idade<=40,40,
                          ifelse(VT_FE_masc$Idade<=45,45,
                          ifelse(VT_FE_masc$Idade<=50,50,
                          ifelse(VT_FE_masc$Idade<=55,55,
                          ifelse(VT_FE_masc$Idade<=60,60,
                          ifelse(VT_FE_masc$Idade<=65,65,
                          ifelse(VT_FE_masc$Idade<=70,70,
                          ifelse(VT_FE_masc$Idade<=75,75,
                          ifelse(VT_FE_masc$Idade<=80,80,
                          ifelse(VT_FE_masc$Idade<=90,90,
                          ifelse(VT_FE_masc$Idade<=100,100,110)))))))))))))))))))
  
  # Agrupamento das informações por faixas etárias 
  VT_FE_masc=VT_FE_masc %>% group_by(faixa_etaria) %>% summarise(D=sum(D),N=sum(N))
  # Transformando a tabela em um data.frame
  VT_FE_masc=data.frame(VT_FE_masc)

  # Criando a variável m = Taxa de mortalidade, calculada como óbitos (D) divido pela população (N)
  VT_FE_masc$m=VT_FE_masc$D/VT_FE_masc$N
  # Transformando possíveis valores não-numéricos em 0
  VT_FE_masc$m=ifelse(is.nan(VT_FE_masc$m), 0, VT_FE_masc$m)
  
  ################################################################################
  # Suavizacao Spline Cubica na escala log
  # Criando a variável y, que representa a taxa de mortalidade na escala log, multiplicada por 100.000
  VT_FE_masc$y <- log(VT_FE_masc$m)*100000
  
  # Criação de uma função de spline cúbica com o método natural
  spline_original_masc <- splinefun(VT_FE_masc$faixa_etaria, VT_FE_masc$y, method = "natural")
  
  # Interpolação de valores para cada idade entre as faixas etárias
  new_x_masc <- seq(0, 110, length.out = 111)
  interpolated_masc_original <- spline_original_masc(new_x_masc)
  
  # Dataframe com uma coluna indicando o ano, coluna indicando a idade, coluna com a taxa de mortalidade,
  # coluna com a taxa de mortalidade na escala logaritmica, e coluna indicando o sexo
  VT_S_masc=data.frame(Year=rep(ano,length.out = 111), 
                       Age=new_x_masc,
                       mx=exp(interpolated_masc_original/100000),
                       mx2=interpolated_masc_original/100000,
                       sexo = "masc")
  
  
  # Com a tabela inclundo as taxas de mortalidade suavizadas, podemos criar a tabela de vida com todas as informações
  # Probabilidade de morte
  VT_S_masc$q= 1 - exp(- VT_S_masc$mx)

  # aumentando um linha na base de dados
  VT_S_masc[nrow(VT_S_masc)+1,]=rep(0,ncol(VT_S_masc))
  
  # Criando as colunas para as variáveis sobreviventes e óbitos esperados, respectivamente
  VT_S_masc$l=rep(0,nrow(VT_S_masc))
  VT_S_masc$d=rep(0,nrow(VT_S_masc))
  # Iniciando a variável "l" com uma populacao hipotetica de 100.000 pessoas
  VT_S_masc$l[1]=100000
  
  # Completando as colunas "l" e "d"
  for (i in 1:(nrow(VT_S_masc)-1)) {
    VT_S_masc$d[i]=VT_S_masc$l[i] * VT_S_masc$q[i]
    VT_S_masc$l[i+1]=VT_S_masc$l[i] - VT_S_masc$d[i]
  }
  # Criando a variável "L" = anos-pessoa vividos entre as idades x e x+n
  VT_S_masc$L=rep(0,nrow(VT_S_masc))
  for (i in 1:nrow(VT_S_masc)-1) VT_S_masc$L[i] = (VT_S_masc$l[i] + VT_S_masc$l[i+1])/2
  
  # Criando a variável "T" = anos-pessoa vividos acima da idade x
  VT_S_masc$T=rep(0,nrow(VT_S_masc))
  soma=0
  for (i in nrow(VT_S_masc):1) {
    soma = soma + VT_S_masc$L[i]
    VT_S_masc$T[i] = soma
  }
  # Criação da variável "e" = expectativa de vida na idade x
  for (i in 1:nrow(VT_S_masc)) VT_S_masc$e[i] = VT_S_masc$T[i] / VT_S_masc$l[i]
  
  #Excluindo a linha adicional
  VT_S_masc=VT_S_masc[-112,]
  
  ######
  
  # Juntando as duas tabelas de vida em uma única tabela
  VT_S <- rbind(VT_S_fem, VT_S_masc)
  
  # Comando que define o objeto que será retornado pela função.
  # O objeto será uma lista, onde o primeiro elemento é a tabela do sexo feminino, 
  # o segundo elemento é a tabela do sexo masculino, e o terceiro elemento é 
  # a tabela com os dois sexos juntos
  return(list(VT_S_fem, VT_S_masc, VT_S))
}

library(dplyr)
library(scales)
library(ggplot2)
library(KernSmooth)
library(readxl)
library(openxlsx)

# Definir o diretório onde estão salvos os bancos de dados
# setwd("C:/Users/TCC Gabriel Pozza Estivalete/Codigos_para_Camila")

# Leitura dos bancos populacionais criados no código anterior, que serão usados na função da tabela de vida
banco_pop_fem = read.csv2("Porto_Alegre_fem_2010_2021_populacao.csv")
banco_pop_masc = read.csv2("Porto_Alegre_masc_2010_2021_populacao.csv")

# Leitura dos bancos de mortalidade criados no código anterior, que serão usados na função da tabela de vida
banco_mort_fem = read.csv2("Porto_Alegre_fem_2010_2021_obitos.csv")
banco_mort_masc = read.csv2("Porto_Alegre_masc_2010_2021_obitos.csv")

# Aplicação da função para criação da tabela de vida.

# No argumento "pop_fem", coloque o banco da população feminina
# No argumento "pop_masc", coloque o banco da população masculina
# No argumento "obito_fem", coloque o banco de mortalidade feminina
# No argumento "obito_masc", coloque o banco de mortalidade masculina
# No argumento "ano", indique o ano desejado
# Após o parênteses da função, indique entre colchetes qual tabela você deseja salvar no objeto que está sendo criado,
# 1 para a tabela feminina, 2 para a tabela masculina, e 3 para a tabela com ambos os sexos
poa_masc_2010 <- funcao_tabela_expectativa(pop_fem = banco_pop_fem, pop_masc = banco_pop_masc, obito_fem = banco_mort_fem, obito_masc = banco_mort_masc,2010)[[2]]
poa_masc_2011 <- funcao_tabela_expectativa(pop_fem = banco_pop_fem, pop_masc = banco_pop_masc, obito_fem = banco_mort_fem, obito_masc = banco_mort_masc,2011)[[2]]

# Unindo duas tabelas de anos diferentes
tabela_poa_2010_2011_masc <- rbind(poa_masc_2010, 
                                   poa_masc_2011)

poa_fem_2010 <- funcao_tabela_expectativa(pop_fem = banco_pop_fem, pop_masc = banco_pop_masc, obito_fem = banco_mort_fem, obito_masc = banco_mort_masc,2010)[[1]]
poa_fem_2011 <- funcao_tabela_expectativa(pop_fem = banco_pop_fem, pop_masc = banco_pop_masc, obito_fem = banco_mort_fem, obito_masc = banco_mort_masc,2011)[[1]]

tabela_poa_2010_2011_fem <- rbind(poa_fem_2010, 
                                  poa_fem_2011)

#################################################

### Código para criação do gráfico de taxas de mortalidade

# Comando para evitar notação científica nos eixos do gráfico
options(scipen = 999)

## Aplicando a função da tabela de vida para criar uma tabela de 2010 com ambos os sexos
tabela_2010 <- funcao_tabela_expectativa(pop_fem = banco_pop_fem, pop_masc = banco_pop_masc, obito_fem = banco_mort_fem, obito_masc = banco_mort_masc,2010)[[3]]

# Criando objetos que identificam os índices na tabela de vida representando os pontos de corte para as faixas
# etárias.
# A primeira sequência corresponde ao sexo feminino, a segunda ao sexo masculino
sequencia <- seq(1,111,5)
sequencia2 <- seq(112,222,5)

# Comando do ggplot para criar o gráfico
# aes(eixo x é a idade, eixo y é a taxa de mortalidade, agrupando pelo sexo)
ggplot(tabela_2010, aes(Age, mx2, group = sexo, colour = sexo)) +
  # Plotando pontos no gráfico somente em cima dos pontos de corte das faixas etárias
  geom_point(data = ~filter(tabela_2010[c(sequencia,sequencia2),])) +
  # Plotando a linha
  geom_line() +
  # Identificando os pontos de corte no eixo Y, referente à taxa de mortalidade na escala log, e atribuindo
  # um rótulo para aparecer no gráfico
  scale_y_continuous(
    breaks = c(-9.21034,-6.907755,-4.60517,-2.303, -0.693, 0),labels = c(10,100,1000, 10000, 50000,100000)) +
  # Quebras no eixo X para identificar os anos
  scale_x_continuous(breaks = c(0,20,40,60,80,100)) +
  # Elementos estéticos
  theme_bw() + labs(y = "Taxas de Mortalidade por 100.000", x = "Idade") + expand_limits(y=c(-9.3,0)) +
  ggtitle("Porto Alegre - 2010") + theme(panel.grid.minor.y = element_blank(), axis.text.y = element_text(size = 7))
