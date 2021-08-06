#install.packages("rio")
#install.packages('AER')
library(pdftools)
library(tidyverse)
library(rio)
library(AER)


options(OutDec = ",")

### lê o texto e transforma em uma base de dados
texto_bruto <- pdf_text("PASSubprograma2018.pdf") %>% 
  strsplit("\n") %>% 
  unlist() %>% 
  enframe(name = NULL, value = "Linha")

### limpeza dos dados

texto_limpo <- texto_bruto %>% 
  tail(-15) %>%    # retira as primeiras linhas
  head(-10) %>%    # retira as últimas linhas
  filter(!str_detect(Linha, "^\\s+[0-9]+\\r$")) %>%   # retira número da página
  filter(!str_detect(Linha, "^1\\.1\\.1.+")) %>%    # retira frases no fim do doc
  filter(!str_detect(Linha, "^candidatos")) %>% 
  filter(!str_detect(Linha, "^escore")) %>% 
  filter(!str_detect(Linha, "^nos")) %>% 
  mutate(Linha = str_replace_all(Linha, "18117113", "/ 18117113"))   # insere a barra para ser utilizada como delimitador posteriormente

## transforma em uma linha só

texto_final_bruto <- tibble(Candidatos = stringi::stri_paste(texto_limpo$Linha, collapse = ""))

## quebra por aluno
texto_final_limpo <- texto_final_bruto %>% 
  separate_rows(Candidatos, sep = "/") %>%   # quebra uma coluna em diversas linhas
  mutate(Candidatos = trimws(str_replace_all(Candidatos, "\r", " ")))   # retira os espaços em branco desnecessários

## encontra o candidato 18117113

temp <- texto_final_limpo %>%
  filter(str_detect(Candidatos, "18117113"))

print(temp)

## separa em variáveis

texto_final_limpo <- texto_final_limpo %>% 
  separate(Candidatos, 
           c("Inscrição", "Nome", "EscoreBruto1", "EscoreBruto2", "SomaEB", "NotaD", "NotaRedação"), 
           sep = ",")


View(texto_final_limpo)


### té aqui o arquivo que estava em PDF, Transformou em excel 


#sALVANDO OS ARQUIVOS 
write.csv(texto_final_limpo,'F:/R/NotaPas/NotasPAS_2018.csv')

