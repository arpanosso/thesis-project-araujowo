#Função p/ extração OCO-2

my_ncdf4_download <- function(url_unique,
                              user="input your user",
                              password="input your password"){
  if(is.character(user)==TRUE & is.character(password)==TRUE){
    n_split <- length(
      stringr::str_split(url_unique,
                         "/",
                         simplify=TRUE))
    filenames_nc <- stringr::str_split(url_unique,
                                       "/",
                                       simplify = TRUE)[,n_split]
    repeat{
      dw <- try(download.file(url_unique,
                              paste0("data-raw/OCO-2 Sif/",filenames_nc),
                              method="wget",
                              extra= c(paste0("--user=", user,
                                              " --password ",
                                              password))
      ))
      if(!(inherits(dw,"try-error")))
        break
    }
  }else{
    print("input a string")
  }
}



url_filename <- list.files("url/",
                           pattern = ".txt",
                           full.names = TRUE)

urls <- read.table(url_filename) |>
  dplyr::filter(!stringr::str_detect(V1,".pdf"))
n_urls <- nrow(urls)

tictoc::tic()
furrr::future_pmap(list(urls[,1],"Seu Usuario","Sua Senha"),my_ncdf4_download)
tictoc::toc()

