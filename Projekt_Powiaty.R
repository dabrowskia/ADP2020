library(shiny)
library(tidyverse)
library(sf)
library(dplyr)
library(ggplot2)
library(raster)
library(spData)
#library(spDataLarge)
library(caret)


#####################################################################################################################################################
# Wczytujemy dane

# Wczytywanie danych, zamiana nazw, dodanie zer do kodów powiatów:
powiaty <- st_read(dsn = 'Powiaty.shp')
#as.tibble(powiaty)
powiaty$JPT_KOD_JE <- paste0(powiaty$JPT_KOD_JE, '000')

wynagrodzenia <- readxl::read_xlsx('Wynagrodzenia.xlsx', sheet = 2)
#as.tibble(wynagrodzenia)
#wynagrodzenia$Kod <- paste0(wynagrodzenia$Kod, '000000')
wynagrodzenia %>% mutate(wynagrodzenia_ogolem = Ogolem) -> nowewynagrodzenia
#as.tibble(nowewynagrodzenia)

malzenstwa <- readxl::read_xlsx('Malzenstwa.xlsx', sheet = 2)
#as.tibble(malzenstwa)
malzenstwa %>% mutate(malzenstwa_ogolem = `ogółem`) -> nowemalzenstwa
#nowemalzenstwa

rozwody  <- readxl::read_xlsx('Rozwody.xlsx', sheet = 2)
#as.tibble(rozwody)
rozwody %>% mutate(rozwody_ogolem = `ogółem`) -> nowerozwody
#as.tibble(nowerozwody)

skolaryzacja  <- readxl::read_xlsx('Skolaryzacja.xlsx', sheet = 2)
#as.tibble(skolaryzacja)

# Złączenia tabel
tab1 <- powiaty %>% left_join(nowemalzenstwa,by = c('JPT_KOD_JE' = 'Kod'))
tab2 <- tab1 %>% left_join(nowerozwody,by = c('JPT_KOD_JE' = 'Kod'))
tab3 <- tab2 %>% left_join(skolaryzacja, by = c('JPT_KOD_JE' = 'Kod'))
polaczona <- tab3 %>% left_join(nowewynagrodzenia,by = c('JPT_KOD_JE' = 'Kod'))

# Zamiana typów danych
polaczona$ogółem.x <- as.integer(polaczona$ogółem.x)
polaczona$malzenstwa_ogolem <- as.integer(polaczona$malzenstwa_ogolem)
polaczona$ogółem.y <- as.integer(polaczona$ogółem.y)
polaczona$rozwody_ogolem <- as.integer(polaczona$rozwody_ogolem)

polaczona %>% dplyr::select(JPT_NAZWA_,Wynagrodzenia = wynagrodzenia_ogolem,
                            Malzenstawa = malzenstwa_ogolem,
                            Rozwody = rozwody_ogolem,
                            Szkoly_Podstawowe = `brutto szkoły podstawowe`,
                            Gimnazja = `brutto gimnazja`) -> Dane
Dane


#Odrzucenie geometrii, zeby model dzialal
Dane.df <- st_set_geometry(Dane,NULL)
summary(Dane.df)


# Wykresy:
malzenstawawykres <- ggplot(polaczona, aes(fill = polaczona$malzenstwa_ogolem)) + 
  geom_sf()

rozwodywykres <- ggplot(polaczona, aes(fill = polaczona$rozwody_ogolem)) + 
  geom_sf()

wynagrodzeniawykres <- ggplot(polaczona, aes(fill = polaczona$wynagrodzenia_ogolem)) + 
  geom_sf()

podstawowewykres <- ggplot(polaczona, aes(fill = polaczona$`brutto szkoły podstawowe`)) + 
  geom_sf()

gimnazjawykres <- ggplot(polaczona, aes(fill = polaczona$`brutto gimnazja`)) + 
  geom_sf()

summary(Dane)

#####################################################################################################################################################
## Podajemy dane wejsciowe
ui <- fluidPage(
  titlePanel("Informacje o wynagrodzeniu w Polsce w 2018 roku"),
  
  sidebarLayout(
    sidebarPanel(weight = 100,
        
                 selectInput(inputId = "WybranyPowiat", 
                             label = "Wybierz powiat",
                             choices = polaczona$JPT_NAZWA_,
                             selected = "powiat łukowski"),
                 
                 # albo taki: checkboxGroupInput
                 #selectInput("Zmienne",
                  #                  label = "Zmiene do wyboru",
                   #                 choices = list("Małżeństwa" = Dane$Malzenstawa,
                    #                               "Wynagrodzenia" = Dane$Wynagrodzenia),
                     #               selected = Dane$Malzenstawa),

                 # albo taki: checkboxGroupInput
                 selectInput("Zmienne",
                             label = "Zmiene do wyboru",
                             choices = names(Dane),
                             selected = names(Dane)[1]),
                 
                 
                 
                 br(),
                 actionButton("Pokaz", label = "Pokaż wyniki")
                 
    ),
    
    mainPanel(
      

      fluidRow(
        h4("Wykres zależności rozwodów od wybranej zmiennej"),
        plotOutput("wykres")
      ),
      
      fluidRow(
        h4("Podstawowe informacje o wybranym powiecie:"),
        h5("Średnie wynagrodzenie, liczba małżeństw i rozwodów, szkół podstawowych oraz gimnajów"),
        verbatimTextOutput("SumPowiat")
        ),
        
      fluidRow(  
        h4("Model opisujący wpływ wybranych zmiennych na ilość rozwodów i korelacje między nimi."),
        h5("Jak można zauważyć istotny wpływ na ilość rozwodów ma wysokość zarobków: w przedziałe od 0 do 6 tys brutto zauważamy wzrost liczby rozwodów,
           która następnie nie zmienia sie znacząco wraz ze wzrostem wynagrodzneia. Wpływ ma też ilośc zwieranych małżeństw, co wydaje się oczywiste.
           W bardzo małym stopniu wpływ ma ilość gimnazjów, a więc liczba dzieci w wieku 13 - 16 lat. Natomiast nie widzimy korelacji między ilością
           rozwodów a ilością szkół podstawowcyh."),
        verbatimTextOutput("Modelowanie")
        )
      
      
      
      
      
        )
        )
  )
#####################################################################################################################
# Kod z R

## Robimy model

cvCtrl <- trainControl(method = 'repeatedcv',number = 10, repeats = 2)

Model <- train(data = Dane.df,
               Rozwody ~ Wynagrodzenia + Malzenstawa + Szkoly_Podstawowe + Gimnazja,
               method = "glm", 
               metric = "RMSE",
               tuneLength = 5, trControl = cvCtrl)

summary(Model)






#capture.output(summary(Model),file = NULL)-> WynikModelowania
#WynikModelowania

#####################################################################################################################
# Ta czesc odpowiada za wyrzucenie danych na server

server <- function(input, output){
  
  output$SumPowiat <- renderPrint(
    if (input$Pokaz) {isolate({
      dplyr::filter(Dane.df,JPT_NAZWA_ == input$WybranyPowiat)
      
    })})
  
  output$Modelowanie <- renderPrint(
    if (input$Pokaz) {isolate({
      summary((Model))
      })})
  
  output$wykres <- renderPlot(
    if (input$Pokaz) {isolate({
      
      a <- input$Zmienne
      
      wykres <- ggplot(Dane, aes(Rozwody,get(a))) +
        geom_smooth()
      
      wykres
      
    })})
  
  
}

shinyApp(ui = ui, server = server)