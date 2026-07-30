
##################### Função p/ extração GOSAT #####################

library(dplyr)

my_gosat_h5_download <- function(url_unique,
                                 user = "input your user",
                                 password = "input your password"){

  if(is.character(user) & is.character(password)){

    # Extrai o nome do arquivo a partir da URL (funciona para .h5 ou .nc4)
    filename <- basename(url_unique)

    # Define o caminho de destino (Criei uma pasta específica para o GOSAT)
    dest_path <- paste0("data-raw/GOSAT_Data/", filename)

    # Cria a pasta se ela não existir para evitar erro de 'caminho não encontrado'
    if(!dir.exists("data-raw/GOSAT_Data/")) dir.create("data-raw/GOSAT_Data/", recursive = TRUE)

    repeat{
      # O método wget é excelente para o Earthdata da NASA
      dw <- try(download.file(url_unique,
                              destfile = dest_path,
                              method = "wget",
                              extra = c(paste0("--user=", user,
                                               " --password=", password,
                                               " --auth-no-challenge"))
      ))

      # Se o download terminar sem erro e o arquivo existir, sai do loop
      if(!(inherits(dw, "try-error"))) break
    }
  } else {
    print("input a string")
  }
}

# 1. Localizar o arquivo de texto com as URLs
url_filename <- list.files("url/",
                           pattern = ".txt",
                           full.names = TRUE)

# 2. Ler e filtrar (removendo PDFs e garantindo que pegamos arquivos .h5)
urls <- read.table(url_filename, stringsAsFactors = FALSE) |>
  dplyr::filter(!stringr::str_detect(V1, ".pdf")) |>
  dplyr::filter(stringr::str_detect(V1, ".tar")) # Garante que estamos baixando HDF5

# 3. Configurar o processamento paralelo (furrr)
library(future)
plan(multisession) # Abre os núcleos do seu PC para baixar vários ao mesmo tempo

tictoc::tic()
# Note que ajustei para passar as colunas corretamente
furrr::future_walk(urls$V1, ~my_gosat_h5_download(.x, user="witoria.araujo@unesp.br", password="Wictoria1234."))
tictoc::toc()










