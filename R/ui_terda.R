
###############################################.
## Header ---- 
###############################################.

dashboardPage(title = "Datatón", skin= "purple",
              dashboardHeader(title = "Datatón ITLP", 
                              dropdownMenu(type = "notifications",
                                           notificationItem(text = "Repositorio", href="https://drive.google.com/drive/folders/1hpxXcZTYVu7XwdgQhsYzgAKg686JBI45?usp=sharing",icon = shiny::icon("cat")),
                                           icon = shiny::icon("cat"))),
              dashboardSidebar(collapsed = TRUE,
                               sidebarMenu(id="sidebarID",
                                           menuItem("Introducción", icon = shiny::icon("journal-whills"),
                                                    menuSubItem("Planteamiento", tabName = "intro1", icon = shiny::icon("book-reader")),
                                                    
                                                    menuSubItem("Método", tabName = "method", icon = shiny::icon("people-carry"))
                                           ),
                                           
                                           menuItem("Pobreza laboral", icon = shiny::icon("journal-whills"),
                                                    menuSubItem("Por Genero", tabName = "gen", icon = shiny::icon("venus-mars")),
                                                    menuSubItem("Por Educacion", tabName = "edu", icon = shiny::icon("user-graduate")),
                                                    menuSubItem("Por posicion", tabName = "pos", icon = shiny::icon("address-card")),
                                                    menuSubItem("Por Acceso a SS", tabName = "seg", icon = shiny::icon("star-of-life"))                                        ),
                                           menuItem("Gasto y programas sociales", icon = shiny::icon("journal-whills"),
                                                    menuSubItem("Transferencias a Alcaldías", tabName = "transfer", icon = shiny::icon("funnel-dollar")),
                                                    menuSubItem("Gasto en desempleo",tabName = "gdes", icon = shiny::icon("search-dollar")),
                                                    menuSubItem("Solicitudes de desempleo", tabName = "maps", icon = shiny::icon("users")),
                                                    menuSubItem("Programas Sociales", tabName = "ss", icon = shiny::icon("star-of-life"))
                                           ),
                                           menuItem("Equipo", icon = shiny::icon("users"),
                                                    menuSubItem("Equipo", tabName = "Equipo", icon = shiny::icon("users"))
                                           )
                               )
              ),
              
              
              
              dashboardBody(
                tabItems(tabItem(tabName= "intro1",
                                 title = "Pobreza laboral en la CDMX",
                                 fluidRow(
                                   column(
                                     5, sidebarPanel(
                                       pickerInput("radio1","Método", choices=list("Hotdeck"=0, "Salarios Mínimos"=1, "Mean Matching"=2), 
                                                   selected = 0, options = list(`actions-box` = TRUE),multiple = FALSE), width=30
                                     )
                                     #radioButtons("radio1", label = h4(strong('Método')),
                                     #               choices = list( "Hotdeck"=0, "Salarios Mnimos"=1, "MM"=2),  
                                     #              selected = 0)
                                   ), 
                                   column(
                                     6, sliderInput("slider1", label = h4("Años"), min = 2005,
                                                    max = 2021, value = c(2005, 2021), width=450)
                                   )
                                 ),
                                 highchartOutput("intro1"),
                                 br(""),
                                 
                                 fluidRow(
                                 
                                 box(title = "¿Qué es la pobreza laboral?", status = "primary", width = 4,  solidHeader = TRUE,
                                     p("La pobreza laboral es una situación en la que el ingreso laboral de un hogar no es 
                       suficiente para alimentar a todos sus miembros. Hogares en pobreza laboral pueden lograr 
                       alimentarse a partir de ingresos no laborales como remesas, transferencias o acceso a 
                       programas sociales. (México ¿Cómo Vamos?, 2021). Derivado de la crisis por la pandemia 
                       resulta interesante observar los efectos sobre este indicador. Según la Organización Internacional del Trabajo (OIT), 
                       a partir de 2017 la participación laboral fue en aumento en México, esto debido probablemente al 
                       aumento de la actividad laboral de las mujeres."),
                                     ),
                       box(title = "¿Por qué diferenciar la pobreza laboral por género?", status = "info", width = 4, solidHeader = TRUE,
                           p("Según la OIT, este aumento en la participación económica de las mujeres no ha reflejado una brecha significativa a nivel global,
                       aun están presentes algunas brechas de acceso y de oferta de empleo, es especial para algunos grupos específicos.
                       Siguiendo esta idea se torna importante mostrar si existe un efecto diferenciado de la crisis
                       entre hombres y mujeres respectivo a la pobreza laboral, esto permitirá tomar consideraciones
                       al momento de asignar recursos en la Ciudad de México."),
                       br(""),
                       ),
                       box(status = "info", width = 4, solidHeader = F,
                           p("En este trabajo se planteó observar las variaciones en la tendencia de la pobreza laboral (TLP) principalmente a través 
                     del género auque se añaden diferentes segmentaciones para contextualizar mejor la situación, 
                     en general 
                     esta presentación se divide en tres partes:"),
                     br(),
                     "⚫ Insstroducción: se describen las bases empleadas así como los métodos seguidos",
                     br(),
                     "⚫ Pobreza laboral: Presentación de los diferentes cálculos por segmentos",
                     br(),
                     "⚫ Gasto y programas: Una visualización sobre las acciones de la CDMX para contrarrestar el golpe de la crisis",
                     br(),
                     br(""),
                       ),
                       
                                 ),

     
                ),
                ######################################GENDER########################################
                tabItem(tabName= "gen",
                        title = "Pobreza laboral por género",
                        fluidRow(
                          column(
                            5, sidebarPanel(
                              pickerInput("radio2","Método", choices=list( "Hotdeck"=0, "Salarios Mínimos"=1, "Mean Matching"=2), 
                                          selected = 0, options = list(`actions-box` = TRUE),multiple = FALSE), width=30
                            )
                          ), 
                          column(
                            6, sliderInput("slider2", label = h4("Años"), min = 2005,
                                           max = 2021, value = c(2005, 2021), width=450)
                          )
                        ),
                        highchartOutput("gen"),
                       
                        br(""),
                        box(title = "Efectos diferenciados de la pandemia", status = "primary", width = 12, solidHeader = TRUE,
                            p("A inicios del 2020 la pobreza laboral aumentó significativamente en poco tiempo, aumentando mucho más rápido para las mujeres. Es a inicios del 2021 que comienza una recuperación progresiva para ambos grupos, aunque esta recuperación no se da con la misma velocidad. Poniendo especial atención al método de Salarios Mínimos es donde más queda marcada esta diferencia. La disminución del ITLP se da de forma mucha más lenta, incluso llegando a mantenerse por encima de niveles históricos anteriores al 2020. Con estos datos se muestra, en primera instancia, que la crisis derivada de la pandemia ha tenido efectos diferenciados entre hombres y mujeres en el mercado laboral, tanto en la intensidad, así como en el tiempo de recuperación.")
                         
                               
                        ),
                        br(""),
                        br(""),
                        br(""),
                        br(""),
                        br(""),
                        br(""),
                        
                        ),
                ######################################EDUCATION########################################
                tabItem(tabName= "edu",
                        title = "Pobreza laboral según el último grado de estudios",
                        fluidRow(
                          column(
                            5, sidebarPanel(
                              pickerInput("radio3","Método", choices=list( "Hotdeck"=0, "Salarios Mínimos"=1, "Mean Matching"=2), 
                                          selected = 0, options = list(`actions-box` = TRUE),multiple = FALSE), width=30
                            )
                          ), 
                          column(
                            6, sliderInput("slider3", label = h4("Años"), min = 2005,
                                           max = 2021, value = c(2005, 2021), width=450)
                          )
                        ),
                        highchartOutput("edu"),
                        br(""),
                        box(title = "Pobreza laboral y nivel educativo", status = "primary", width = 12, solidHeader = TRUE,
                            p("Se dividió a la población en cinco segmentos, personas que no terminaron la primaria, personas que terminaron la primaria, que terminaron la secundaria, que terminaron el nivel medio superior o educación técnica y personas que terminaron la universidad o algún posgrado. Históricamente el segmento <Primaria fue el de la población más vulnerable teniendo los niveles más altos de pobreza laboral, el segmento de Primaria es el segundo con los niveles históricos más altos. Los tres segmentos restantes se mantienen cercanos, con la población que termina educación superior tendiendo a tener los niveles más bajos de pobreza laboral. Es a inicios del 2020 que la pobreza laboral aumenta para todos los segmentos, la diferenciación comienza en el 3er trimestre del 2020 cuando los niveles de pobreza laboral tienden a disminuir para los segmentos Secundaria, Medio superior y Universidad, mientras que los otros dos dejan de aumentar. En cualquier método se observa un mismo resultado, solo el grupo Universidad y posgrado ha logrado normalizar su nivel de pobreza laboral teniendo niveles a los presentados antes del 2020, el resto de grupos aún no se recuperan por completo.")
                            
                        ),
                        br(""),
                        br(""),
                        br(""),
                        br(""),
                        br(""),
                        br(""),
                        
                ),
                ######################################Position########################################
                
                tabItem(tabName= "pos",
                        title = "Pobreza laboral por posición en el trabajo",
                        fluidRow(
                          column(
                            5, sidebarPanel(
                              pickerInput("radio4","Método", choices=list( "Hotdeck"=0, "Salarios Mínimos"=1, "Mean Matching"=2), 
                                          selected = 0, options = list(`actions-box` = TRUE),multiple = FALSE), width=30
                            )
                          ), 
                          column(
                            6, sliderInput("slider4", label = h4("Años"), min = 2005,
                                           max = 2021, value = c(2005, 2021), width=450)
                          )
                        ),
                        highchartOutput("pos"),
                        br(""),
                        box(title = "Pobreza laboral según la posición", status = "primary", width = 12, solidHeader = TRUE,
                            p("Para esta sección se divide a la población en tres segmentos según su posición en su trabajo, pueden ser Subordinados remunerados, Empleadores o Trabajadores por cuenta propia.
                               En esta gráfica se puede observar que los datos son un poco más variantes. Los niveles históricos más altos de pobreza laboral los tienen los trabajadores por cuenta propia,
                               mientras que los empleadores tienen los niveles más bajos. Es a partir de 2020 que se comienzan a observar los efectos de la crisis por la pandemia, afectando fuertemente a
                               trabajadores subordinados y empleadores alcanzando estos últimos su nivel más alto en pobreza laboral. Esto puede deberse al cierre de múltiples pequeños negocios en
                               la Ciudad de México.")
                        ),
                        br(""),
                        br(""),
                        br(""),
                        br(""),
                        br(""),
                        br(""),
                      
                ),
                ######################################Social########################################
                
                tabItem(tabName= "seg",
                        title = "Pobreza laboral por posición en el trabajo",
                        fluidRow(
                          column(
                            5, sidebarPanel(
                              pickerInput("radio5","Método", choices=list( "Hotdeck"=0, "Salarios Mínimos"=1, "Mean Matching"=2), 
                                          selected = 0, options = list(`actions-box` = TRUE),multiple = FALSE), width=30
                            )
                          ), 
                          column(
                            6, sliderInput("slider5", label = h4("Años"), min = 2005,
                                           max = 2021, value = c(2005, 2021), width=450)
                          )
                        ),
                        highchartOutput("seg"),
                        br(""),
                        box(title = "Pobreza laboral y acceso a seguridad social", status = "primary", width = 12, solidHeader = TRUE,
                            p("Los niveles de pobreza laboral según el acceso a seguridad social, después del primer trimestre del 2020 la pobreza laboral para personas que cuentan con acceso a seguridad social aumenta mucho más rápido que su contraparte.")
                            
                        ),
                        br(""),
                        br(""),
                        br(""),
                        br(""),
                        br(""),
                        br(""),
                        
                ),
                
                ######################################transfer########################################
                
                tabItem(tabName = "transfer",
                        title = "Transferencias a alcaldías para desarrollo social",
                        br(""),
                        box(title = "Transferencias a alcaldías para desarrollo social", status = "primary", width = 12, solidHeader = TRUE,
                            p("Transferencias realizadas a alcaldías durante 2020 para atender el deje de desarrollo social, de forma histórica una de las alcaldías con mayores transferencias es Iztapalapa")
                            
                        ),
                        br(""),
                        fluidRow(
                          column(
                            1, radioButtons("radio_t", label = h4(strong('Año')),
                                            choices = list( "2018"=0, "2019"=1, "2020"=2),  
                                            selected = 0)
                          ),
                          column(
                            10, highchartOutput("transfer")
                          )
                        ),
                        
                        br(""),

                        
                ),
                
                
                
                #radioButtons("radio1", label = h4(strong('Método')),
                #               choices = list( "Hotdeck"=0, "Salarios Mnimos"=1, "MM"=2),  
                #              selected = 0)
                ######################################gasto des########################################
                
                tabItem(tabName= "gdes",
                        title = "Monto ejercido con subfunción para combatir el desempleo",
                        br(""),
                        box(title = "Gastos en contrarrestar el desempleo", status = "primary", width = 12, solidHeader = TRUE,
                            p("Egresos ejercidos por las dos principales unidades que combaten el desempleo, en 2020 el monto ejercido supera por mucho al aprobado a raíz de la pandemia, a esto se le deben sumar 26.6 millones de pesos transferidos de forma directa a las alcaldías para el mismo fin")
                            
                        ),
                        br(""),
                        fluidRow(
                          column(
                            1, radioButtons("radio_des", label = h4(strong('Año')),
                                            choices = list( "2018"=0, "2019"=1, "2020"=2, "2021"=3),  
                                            selected = 0)
                          ),
                          column(
                            10, highchartOutput("gdes")
                          ),
                          column(
                            12, h3("", align = "center")
                          )
                        ),
                       
                        br(""),
                        
                         ),
                ######################################MAPS########################################
                
                tabItem(tabName = "maps",
                        title = "Solicitudes de desempleo",
                        br(""),
                        box(title = "Solicitudes de desempleo",status = "primary", width = 12, solidHeader = TRUE,
                            p("Las alcaldías con más solicitudes aprobadas son Xochimilco e Iztapalapa, en contraste son Cuajimalpa y la Magdalena Contreras que tienen el menor número de solicitudes aprobadas")
                            
                        ),
                        br(""),
                        leafletOutput("maps"),
  

                        br(""),
                        ),
                
                ######################################ss########################################
                
                tabItem(tabName= "ss",
                        title = "Seguridad social en la CDMX",
                        br(""),
                        box(title = "Programas sociales en la Ciudad de México",status = "primary", width = 12, solidHeader = TRUE,
                            p("Estos son los programas sociales vigentes que atienden el derecho al trabajo y derechos humanos laborales, el programa con mayor alcance es el seguro de desempleo y de ellos solo dos programas toman en cuenta aspectos de género que son; jefas de familia para su inclusión laboral con un presupuesto de $11,520,000 de pesos y 900 beneficiarias y La Empleadora con un presupuesto de $99,990,000 de pesos y 7,809 beneficiarias.")
                            
                        ),
                        br(""),
                        fluidRow(
                          column(
                            12, highchartOutput("ss1")
                          ),
                          ),
                        
                        
                        br(""),
                        fluidRow(
                          column(
                            6, highchartOutput("ss2")
                          ),
                          column(
                            6, 
                            box(title = "Presupuesto por programa",status = "primary",width = 12, solidHeader = TRUE,
                                p("El seguro de desempleo fue el programa que más recursos públicos consumió, un total de 500 millones de pesos, seguido por el programa La empleadora con 99.9 millones de pesos y fomento al trabajo digno, con 51.7 millones")
                                
                            ),
                      
                          )
                        ),
                        
                        br(""),
                        
                        fluidRow(
                          column(
                            6, highchartOutput("ss3")
                          ),
                          column(
                            6, 
                            box(title = "Índice de Desarrollo Social en la Ciudad",status = "primary",width = 12, solidHeader = TRUE,
                                p("La población se concentra en los estratos medios y en pobreza moderada, habiendo una ligera cantidad mayor de mujeres que de hombres, el estrato alto es el único con más hombres que mujeres")
                                
                            ),
                          )
                        ),
                        
                ),
                
                tabItem(tabName = "method",
                        h1("Métodos de imputación", align = "center"),
                        p(""),
                        p(""),
                        fluidRow(
                          box(title = "Hotdeck",status = "primary",width = 4, solidHeader = TRUE,
                              p("La imputación por Hotdeck implica reemplazar los valores faltantes de una o más variables para un no encuestado (llamado receptor) con valores observados de un encuestado (el donante) que es similar al no encuestado con respecto a las características observadas por ambos casos."),
                              br(""),
                              br(""),
                              br(""),
                          ), 
                          box(title = "Match por promedios predictivos (Mean Matching)",status = "primary",width = 4, solidHeader = TRUE,
                              p("Para cada entrada faltante, el método forma un pequeño conjunto de donantes candidatos a partir de todos los casos completos que han predicho valores más cercanos al valor predicho para la entrada faltante. Un donante se extrae al azar de los candidatos, y el valor observado del donante se toma para reemplazar el valor faltante. La suposición es que la distribución de la ''celda'' faltante es la misma que los datos observados de los donantes candidatos."),
                              
                          ),
                          box(title = "Salarios Minimos",status = "primary",width = 4, solidHeader = TRUE,
                              p("Se utilizo la metodologia empleada por CONEVAL que emplea los salarios minimos para el calculo de la pobreza laboral"),
                              br(""),
                              br(""),
                              br(""),
                              br(""),
                              br(""),
                          ),
                        )
                       
                ),
                
                 tabItem(tabName = "Equipo",
                         box(
                           background = "purple",width = 12, solidHeader = TRUE,
                           h1("DATATON. Tu dinero, tus datos", align = "center"),
                         ),
                         br(""),
                         box(
                           title="Equipo: Terdashianos",background = "maroon",width = 12, solidHeader = F,
                           h3("El efecto de la pandemia sobre la Pobreza laboral", align = "center"),
                         )
            )
                
                ))
              
)
