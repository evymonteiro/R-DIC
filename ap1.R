
##QUESTÃO 1)  Avaliar a produção em volume de madeira (m^3)

clone_a <- c(13, 19, 31, 15, 20, 30, 22, 35, 18, 33)
clone_b <- c(31, 36, 39, 27, 26, 29, 45, 30, 28, 43)

dados1 <- data.frame(
  Produção = c(clone_a, clone_b),
  Clone = rep(c("A", "B"), each = 10)
)

dados1

##A)

##Teste t para duas amostras independentes:

#Testes de normalidade e homocedasticidade para os dados:

#Teste de homocedasticidade: H0: as variâncias são iguais vs H1: As variâncias são diferentes

bartlett.test(Produção ~ Clone, data = dados1)

#O p-value = 0,6753 indica que não há evidências estatísticas para rejeitar H0, ao nível de significância
#de 5%, concluindo assim, que as variâncias são iguais. 

#Teste de normalidade: H0: A distribuição é normal vs H1: A distribuição não é normal.

#A função tapply é utilizada para aplicar uma outra função a um subconjunto de dados
#neste caso, aos valores de produção de cada clone.

tapply(dados1$Produção, dados1$Clone, shapiro.test)

#Para o clone A, p-value = 0,2908 e para o clone B, p-value = 0,1473
#em ambos os casos, não há evidências para rejeitar H0, ao nível de significância de 5%. 

#Aplicando o teste T: H0: as médias de produção dos clones A e B são iguais vs H1: as médias são diferentes.

t.test(clone_a, clone_b, var.equal = TRUE)


#Com p-value = 0,008 Ao nível de significância de 5%, H0 é rejeitada, concluindo que existe diferença entre as médias de produção. Os valores encontrados de média para o clone A e para o clone B foram 23,6 e 33,4 metros cúbicos de madeira, respectivamente.

#B) 

#ANOVA: Supondo que os pressupostos de normalidade e homocedasticidade de resíduos foi atendida:

#Hipóteses: H0: as médias de produção dos clones A e B são iguais vs H1: as médias são diferentes.

anova1<- aov(Produção ~ Clone, data = dados1)
anova1

summary(anova1)

#De acordo com o p-value obtido na ANOVA (0.0086), rejeita-se H0, ao nível de significância de 5%, concluindo que as médias de produção são diferentes para os diferentes tipos de clone. 

#Verificando se t2 é igual a F:


T_squared <- t.test(clone_a, clone_b, var.equal = TRUE)$statistic^2
T_squared
F_anova <- summary(anova1)[[1]]["Clone", "F value"]
F_anova


#Como observado, o valor t2 é igual ao valor de F, o que confirma a igualdade entre os testes quando se compara apenas dois tratamentos.



#Conclusões

#A interpretação básica é: Existe diferença entre a média de produção entre o Clone A e o Clone B, o que implica que as práticas de manejo ou seleção do clone para produção florestal devem considerar as diferenças observadas, possivelmente preferindo o clone com maior média de produção.

#Quanto a equivalência de t2 com o teste F, ocorre porque, com dois grupos, a soma de quadrados total pode ser dividida diretamente entre os quadrados entre os grupos (tratamentos) e os quadrados dentro dos grupos (erro), resultando na mesma fórmula subjacente.

#Quando há mais de dois grupos A ANOVA generaliza o conceito para comparar mais de dois tratamentos simultaneamente, avaliando se pelo menos um dos grupos difere dos outros. Nesse caso, o teste t deixa de ser aplicável diretamente porque ele só compara dois grupos por vez. Para múltiplos tratamentos, usar o teste t repetidamente aumentaria o risco de erro tipo I (rejeitar 𝐻0 quando ele é verdadeiro).

#Em outras palavras, no contexto de dois grupos, o teste t está testando se a diferença entre as médias dos dois grupos é significativa, enquanto o teste F em uma ANOVA testa se as diferenças observadas nas médias dos grupos podem ser atribuídas à variação entre os grupos ou se são resultado da variação aleatória dentro de cada grupo.



#QUESTÃO 2)

dados2 <- data.frame(
  Azul = c(16, 11, 20, 21, 14, 17),
  Verde = c(37, 32, 20, 29, 37, 32),
  Branco = c(21, 12, 14, 17, 13, 20),
  Amarelo = c(45, 59, 48, 46, 38, 47)
)
print(dados2)


# A)

# Croqui com as cores e quantidades de cada repetição:

library(ggplot2)

# Preparando os dados para o gráfico
croqui <- tidyr::pivot_longer(dados2, cols = c(Azul, Verde, Branco, Amarelo),  ##reorganiza colunas de cores em duas colunas (Cor e Valor)
                                  names_to = "Cor", values_to = "Quantidade")
# Adicionando uma coluna de repetição para cada linha
croqui$Repeticao <- rep(1:6, each = 4)   #criando a coluna de repetições no df Croqui

# Gráfico para visualização da distribuição do experimento
ggplot(croqui, aes(x = Repeticao, y = Quantidade, fill = Cor)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Azul" = "lightblue", "Verde" = "lightgreen", "Branco" = "lightyellow", "Amarelo" = "yellow")) +
  labs(title = "Croqui", x = "Repetições", y = "Insetos Capturados", fill = "Tratamento") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), plot.title = element_text(hjust = 0.5))

# B) 

#Análise exploratória dos dados:

summary(dados2)
apply(dados2, 2, sd)

boxplot(dados2, main = "Distribuição dos insetos capturados", 
        xlab = "Cor do papelão", ylab = "Número de insetos",
        col = c("lightblue", "lightgreen", "lightyellow", "yellow"))


#Histogramas: 

par(mfrow = c(2, 2))

hist(dados2$Azul, main = "Densidade - Azul", xlab = "Valor", probability = TRUE, col = "lightblue", border = "black")
lines(density(dados2$Azul), col = "red", lwd = 2)
hist(dados2$Verde, main = "Densidade - Verde", xlab = "Valor", probability = TRUE, col = "lightgreen", border = "black")
lines(density(dados2$Verde), col = "red", lwd = 2)
hist(dados2$Branco, main = "Densidade - Branco", xlab = "Valor", probability = TRUE, col = "lightyellow", border = "black")
lines(density(dados2$Branco), col = "red", lwd = 2)
hist(dados2$Amarelo, main = "Densidade - Amarelo", xlab = "Valor", probability = TRUE, col = "yellow", border = "black")
lines(density(dados2$Amarelo), col = "red", lwd = 2)


#Em relação as médias o papelão amarelo capturou mais insetos (47,17), enquanto o azul e o branco capturaram menos (16,50 e 16,17, respectivamente). O desvio padrão é maior para o amarelo, sugerindo maior variação no número de insetos capturados. Pelo boxplot observamos que o papelão amarelo apresenta maior número de insetos capturados e uma menor variação acerca do valor médio, visto pelo achatamento do boxplot, também é possível identificar a presença de 2 outliers, o que justificaria um desvio padrão maior mesmo que o boxplot esteja achatado, azul e branco possuem números de insetos similares, porém com uma maior variação vista no papelão branco, o que pode ser confirmado na terceira casa decimal do desvio padrão e no histograma, ambos capturaram as menores quantidade de insetos, sendo que o azul o que possui o menor valor de mínimo. 
#O papelão verde apresentou uma maior variabilidade nos dados, visto pelo valor de seu desvio padrão e o achatamento do gráfico, não apresentou nenhum outlier. Aqui, cabe analisar como a presença de outliers contribuiu para o aumento do
#desvio padrão do papelão amarelo, que é próximo do valor observado no papelão verde, entretanto, o achatamento de ambos boxplot é diferente, em que, no verde, é possível observar uma maior dispersão, pois é menos achatado que o amarelo e não possui nenhum outlier.

# C)

dados_2 <- stack(dados2)  ##Função stack transforma todas as colunas do df em duas colunas, uma com valores e outra com nomes.
colnames(dados_2) <- c("Insetos", "Tratamento")
dados_2

#ANOVA:
#Hipóteses: H0: as médias de caputra de insetos são iguais vs H1: as médias são diferentes.

anova2 <- aov(Insetos ~ Tratamento, data = dados_2)
anova2
summary(anova2)

df1 <- 3   # (k-1) 4 tipos de papelão (tratamento) - 1
df2 <- 20  # (N - k), em que N = 6rep * 4trat = 24. 24 - 4 = 20

# Valor crítico F para alpha = 0.05
f_critico <- qf(0.95, df1, df2)
f_critico

##Se o valor de F obtido na ANOVA é maior que o Fcrítico (Ftabelado), rejeita-se H0. 
#Como F = 45,57 > 3,098, ao nível de significância de 5%, existem evidências para rejeitar H0. Isso significa que existe diferença no número médio de insetos capturados entre as cores de papelão.
#O valor de p-value = 3,99e-09 corrobora com o valor obtido no teste F. 

# D)

#Análise gráfica dos resíduos:

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(anova2)

##O gráfico Q-Q residuals indica normalidade de resíduos. 


#E) 

#Teste de homocedasticidade de resíduos:

#Hipoteses: H0: As variâncias dos resíduos entre os grupos são iguais vs H1: Pelo menos uma das variâncias dos resíduos entre os grupos é diferente.
bartlett.test(residuals(anova2) ~ dados_2$Tratamento)

#Ao nível de significância de 5% e p-value = 0,4274, não há evidências para rejeitar H0, concluindo assim que os resíduos são homocedásticos. 

#F)

#Teste de normalidade de resíduos:

#Hipoteses: H0: Os resíduos seguem distribuição normal vs H1: Os resíduos não seguem distribuição normal

shapiro.test(residuals(anova2))

#Ao nível de significância de 5% e p-value = 0,7212, não há evidências para rejeitar H0, concluindo assim que os resíduos são normais. 


#Teste de comparações múltiplas:


#Hipoteses: H0: Não há diferença significativa entre as médias dos dois tratamentos comparados vs. H1: Há uma diferença significativa entre as médias dos dois tratamentos comparados.

tukey <- TukeyHSD(anova2)
print(tukey)
plot(tukey)

##Criando um data frame para visualizar o melhor tratamento e os testes individualmente:

tukey2 <- as.data.frame(tukey$Tratamento)

tukey2$Conclusao <- ifelse(tukey2$`p adj` < 0.05, "Rejeita H0", "Aceita H0")

#coluna para identificar o melhor tratamento
tukey2$Melhor_Tratamento <- ifelse(tukey2$diff > 0, 
                                       sub("-.*", "", rownames(tukey2)), 
                                       sub(".*-", "", rownames(tukey2)))

# Ordenando do melhor para o pior
tukey2 <- tukey2[order(-abs(tukey2$diff)), ]
print(tukey2)


#O papelão amarelo capturou significativamente mais insetos do que os outros tratamentos. O papelão verde capturou mais insetos do que azul e branco. Não há diferença significativa entre as capturas dos papelões azul e branco.

#Conclusões finais:

#O papelão amarelo foi o mais eficiente na captura de insetos, seguido pelo verde. Azul e branco tiveram desempenhos similares, mas inferiores aos demais. Todos os pressupostos da ANOVA foram atendidos (normalidade e homogeneidade de variâncias). As comparações múltiplas confirmaram as diferenças significativas entre os tratamentos.



###ANOVA MANUALMENTE: 

#Hipóteses: H0: as médias de caputra de insetos são iguais vs H1: as médias são diferentes.


#Calcular a média geral
media_geral <- mean(c(dados2$Azul, dados2$Verde, dados2$Branco, dados2$Amarelo))
media_geral

#SQT
sqt <- sum((c(dados2$Azul, dados2$Verde, dados2$Branco, dados2$Amarelo) - media_geral)^2)
sqt

#SQR
media_azul <- mean(dados2$Azul)
media_azul
media_verde <- mean(dados2$Verde)
media_verde
media_branco <- mean(dados2$Branco)
media_branco
media_amarelo <- mean(dados2$Amarelo)
media_amarelo

# Número de observações
n <- 6

#SQR
sqr <- n * ((media_azul - media_geral)^2 + (media_verde - media_geral)^2 + 
              (media_branco - media_geral)^2 + (media_amarelo - media_geral)^2)
sqr

#SQE
sqe <- sqt - sqr
sqe

#graus de liberdade
##Já definidos anteriormente. 

#MQE
mqr <- sqr / df1  
mqe <- sqe / df2  

#F calculado:
f_calculado <- mqr / mqe
f_calculado

#F critico =  F tabelado
f_critico  ##calculado anteriormente//F tabelado


##Resultado:
f_calculado
f_critico

##Se o valor de F obtido na ANOVA é maior que o Fcrítico (Ftabelado), rejeita-se H0. 
#Como F = 45,57 > 3,098, ao nível de significância de 5%, existem evidências para rejeitar H0. Isso significa que existe diferença no número médio de insetos capturados entre as cores de papelão.


Anova_manual <- data.frame(
  Estatística = c("Média Geral", "Média Azul", "Média Verde", "Média Branco", "Média Amarelo",
                  "Soma de Quadrados Total (SQT)", "Soma de Quadrados Entre Tratamentos (SQR)",
                  "Soma de Quadrados Dentro dos Tratamentos (SQE)", "Graus de Liberdade Entre Tratamentos (df1)",
                  "Graus de Liberdade Dentro dos Tratamentos (df2)", "Média Quadrática Entre Tratamentos (MQR)",
                  "Média Quadrática Dentro dos Tratamentos (MQE)", "Valor Calculado de F", "Valor Crítico de F"),
  Valor = c(media_geral, media_azul, media_verde, media_branco, media_amarelo,
            sqt, sqr, sqe, df1, df2, mqr, mqe, f_calculado, f_critico)
)

print(Anova_manual)


####Teste de Tukey Manual:

erro_padrao <- sqrt(mqe * (2 / n))  

#diferenças entre as médias

dif_verde_azul <- media_verde - media_azul
dif_branco_azul <- media_branco - media_azul
dif_amarelo_azul <- media_amarelo - media_azul

dif_branco_verde <- media_branco - media_verde
dif_amarelo_verde <- media_amarelo - media_verde
dif_amarelo_branco <- media_amarelo - media_branco

# Valores de q
q_verde_azul <- dif_verde_azul / erro_padrao
q_branco_azul <- dif_branco_azul / erro_padrao
q_amarelo_azul <- dif_amarelo_azul / erro_padrao

q_branco_verde <- dif_branco_verde / erro_padrao
q_amarelo_verde <- dif_amarelo_verde / erro_padrao

q_amarelo_branco <- dif_amarelo_branco / erro_padrao

# Valor crítico de q (tabelado)
q_critico <- qtukey(0.95, 4, df2)  # Usando 4 tratamentos e df2 graus de liberdade

# Comparando os valores de q com o valor crítico
tukey_manual <- data.frame(
  Comparação = c("Verde vs Azul", "Branco vs Azul", "Amarelo vs Azul",
                 "Branco vs Verde", "Amarelo vs Verde", "Amarelo vs Branco"),
  Diferença_Média = c(dif_verde_azul, dif_branco_azul, dif_amarelo_azul,
                      dif_branco_verde, dif_amarelo_verde, dif_amarelo_branco),
  q_Calculado = c(q_verde_azul, q_branco_azul, q_amarelo_azul, q_branco_verde, q_amarelo_verde, q_amarelo_branco),
  q_Crítico = rep(q_critico, 6),
  Significativo = c(abs(q_verde_azul) > q_critico, abs(q_branco_azul) > q_critico, abs(q_amarelo_azul) > q_critico,
                    abs(q_branco_verde) > q_critico, abs(q_amarelo_verde) > q_critico, abs(q_amarelo_branco) > q_critico)
)   ##abs garante que a comparação seja feita em módulo. 

tukey_manual

