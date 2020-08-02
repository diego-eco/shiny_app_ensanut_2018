#### Global R frame 

pacman::p_load(tidyverse,
               ggplot2,
               shiny,
               viridis,
               sf
               #gmodels,
               #tmap,
               #leaflet,
               #foreign,
               #expss,
               #fishualize,
               #raster,
               #cowplot
               #ggspatial,
               #colorspace
            )


#Shape Municipal
mun_nac <- st_read("data/municipal.shp")

mun_nac <- mun_nac %>%
    dplyr::select(CVEGEO,NOM_ENT,NOM_MUN)

# Datos de ensanut a nivel municipal

ensanut_ap <- read.csv(file = "data/ensanut_areas_peq.csv", 
                       sep=",", 
                       colClasses=c(rep('factor', 6), 'numeric','numeric','numeric')
                        )

ensanut_ap <- ensanut_ap %>%
    rename(CVEGEO = mun,Obesidad=obesidad,Hipertension=hipertension,Diabetes=diabetes)

#Unir ambas bases

datos <- dplyr::left_join(x=mun_nac, y=ensanut_ap, by=c("CVEGEO"))


#### End Global R frame 


### Frontpage
ui <- fluidPage( theme = "bootswatch-cerulean.css",
    titlePanel("Mapas Encuesta Nacional de Salud y Nutrición 2018."),
    h3("Principales enfermedades a nivel municipal."),
    sidebarLayout(
        sidebarPanel(
            helpText("Seleccione la variable y el número de clases para mostrar"),
            
            selectInput("var", 
                        label = "Enfermedad",
                        choices = c("Diabetes", 
                                    "Hipertension",
                                    "Obesidad"),
                        selected = "Hipertension"),
            
            sliderInput(inputId = "class", 
                        label = "Número de clases", 
                        value = 5, min = 2, max = 10),
            helpText("Fuente: Elaborado por Diego López, con datos de INEGI/ENSANUT a nivel muncipal."),
            p("Para más herramientas, visite",
              a(href="https://diego-eco.github.io/", "diego-eco.github.io")
        )
        ),
        
        mainPanel(
            h3(textOutput("selected_var")),
            h4(textOutput("selected_range")),
            plotOutput("map")
        )
    ) #End Sidebar Layout
    
) #End FluidPAge
### End Frontpage


### Backend
server <- function(input, output){
    output$selected_var <- renderText({ 
        paste("Prevalencia de ", input$var, "en México")
    })
    output$selected_range <- renderText({ 
        paste("Rangos calculados para ", input$class , " clases")
    })
    output$map <- renderPlot({
        

        # ¿Cuántas clases quiero?
        no_classes <- input$class
        variable <- input$var
        
        # Extraer cuantiles
        cuantil <- datos %>%
            pull(variable) %>%
            quantile(probs = seq(0, 1, length.out = no_classes + 1)) %>%
            as.vector() # to remove names of quantiles, so idx below is numeric
        # Así se crean las etiquetas
        labels <- imap_chr(cuantil, function(., idx){
            return(paste0(round(cuantil[idx] , 0),
                          "%",
                          " – ",
                          round(cuantil[idx + 1] , 0),
                          "%"))
        })
        # Se elimina la última etiqueta
        # En caso contrario sería hasta NA
        labels <- labels[1:length(labels) - 1]
        
        # Crear la variable en datos
        
        q_var = datos %>%
            pull(variable)
        
        datos <- datos %>%
            mutate(q_variable = cut(q_var,
                                    breaks = cuantil,
                                    labels = labels,
                                    include.lowest = T))
        # Creamos el mapa
        
        ggplot(data = datos) +
            # Shape principal
            geom_sf(
                mapping = aes(
                    fill = q_variable
                ),
                color = "white",
                size = 0
            ) +
            scale_fill_viridis(
                option = "cividis",
                name = " ",
                alpha = 0.8, 
                begin = 0.3, 
                end = 1,
                discrete = T, # True para clases discretas
                direction = -1, 
                guide = guide_legend(
                    keyheight = unit(5, units = "mm"),
                    title.position = "top",
                    reverse = T # El valor más alto hasta arriba
                )) +

            # Agregar títulos
            labs(x = NULL,
                 y = NULL,
                 title = "% de población de 20 años y más con diagnóstico previo",
                 subtitle = "",
                 caption = "Fuente: Elaborado por Diego López con datos de INEGI/ENSANUT a nivel muncipal.",
                 fill = "%") +
             theme_bw()
        
    }) # End render plot map
    
} ### End Server function

### End Backend

# runApp("~/Desktop/Code/R Pro/Shiny/test_maps/census-app",display.mode = "showcase")

shinyApp(ui = ui, server = server)




