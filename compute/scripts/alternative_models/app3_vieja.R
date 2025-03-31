#app3 vieja primer prototipo

Ignacio Campon

# Importación
mun <- st_read("dataSP2.shp")

mun <- st_transform(mun, 4326)

mun$CANT_BI <- as.factor(mun$CANT_BI)

# Creamos una lista con el nombre de los estados y su numero indice:
opciones <- c(
  "CCZ01"  = "1",
  "CCZ02"  = "2",
  "CCZ03"  = "3",
  "CCZ04"  = "4",
  "CCZ05"  = "5",   
  "CCZ06"  = "6",
  "CCZ07"  = "7",   
  "CCZ08"  = "8",
  "CCZ09"  = "9",
  "CCZ10"  = "10",
  "CCZ11"  = "11",
  "CCZ12"  = "12",
  "CCZ13"  = "13",
  "CCZ14"  = "14", 
  "CCZ15"  = "15",
  "CCZ16"  = "16",
  "CCZ17"  = "17",
  "CCZ18"  = "18") 


# UI: Generamos la interfaz de usuario 
ui <- fluidPage(
  
  selectInput(inputId = "selESTADO", label = "Seleccione CCZ",
              choices = opciones), 
  textInput(inputId = "buscador", label = "Ingrese la cantidad de bimestres con deuda que se desean filtrar"),
  actionButton(inputId = "buscar", label = "Buscar"),
  textOutput("data"), 
  leafletOutput("leaflet")
  
)

# Funcion Server (el funcionamiento del programa)
server <- function(input, output){
  output$data <- renderText({
    print(paste0("Centro Comunal Zonal ",input$selESTADO ))
  })
  
  datos_filtrados <- reactive({
    mun_estado <- mun
    mun_estado$CANT_BI <- as.numeric(mun_estado$CANT_BI)
    mun_estado <- mun_estado %>%
      filter(CCZ == input$selESTADO) %>%
      filter(CANT_BI >= input$buscador)
    return(mun_estado)
  })
  
  
  qpal <- colorBin("Blues", domain = c(min(mun_estado$CANT_BI),max(mun_estado$CANT_BI)), n = 4)
  
  popup <- paste0("<b>", "Padron: ", "</b>", as.character(mun_estado$PADRON), 
                  "<br>", "<b>", "Cantidad de Unidades: ", "</b>", mun_estado$CANT_UN, "<br>", 
                  "<b>", "Categoria: ", "</b>", as.character(mun_estado$CATEGOR), "<br>", "<b>", 
                  "Cantidad de TDS: ", "</b>", mun_estado$cantTDS, "<br>", "<b>", 
                  "Cantidad de BIM impagos: ", "</b>", mun_estado$CANT_BIM, "<br>")
  
  # Rendereamos el texto
  output$leaflet <- renderLeaflet({
    leaflet(datos_filtrados()) %>%
      addTiles() %>%
      addPolygons(color = ~qpal(datos_filtrados()$CANT_BI), 
                  layerId = ~datos_filtrados()$PADRON,                  
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE), #highlight cuando pasas el cursor
                  label = ~datos_filtrados()$PADRON ,                                  # etiqueta cuando pasas el cursor
                  labelOptions = labelOptions(direction = "auto"),
                  popup = popup) %>% 
      addLegend(pal = qpal, values = datos_filtrados()$CANT_BI, opacity = .5, title = 'Bimestres Impagos') %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView( lng = -56.1881600,lat = -34.90328, zoom = 12)
  })
  
}

# Corremos la aplicacion web
shinyApp(ui, server) 