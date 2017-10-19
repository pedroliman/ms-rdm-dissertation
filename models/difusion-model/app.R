# Este é um app que possui a interface para a calculadora oshcba.
## Se está rodando o app pela primeira vez, rode estes comando antes de executar o aplicativo: (Antes de rodar o comando, exclua o "#")
# install.packages(c("shiny","ggplot2","readxl","mc2d","dplyr","devtools"))
#Se precisar atualizaro app, rode este comando:
# library(devtools)
# install_github("pedroliman/oshcba")
library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(readxl)
library(dplyr)
library(akima)

# Carregando Funções Úteis
source('funcoes.R', encoding = 'UTF-8')

## Carregando o Modelo, e outros objetos
source('modelo-difusao.R', encoding = 'UTF-8')

opcoes = list(
  VarResposta = "Cash",
  VarCenarios = "Scenario",
  VarEstrategias = "Lever",
  N = 300,
  VarTempo = "Tempo",
  VarCriterio = "RegretPercPercentil75",
  SentidoCriterio = "min"
)

### Definindo uma seed fixa fora do app para ter replicações mehor comparáveis.
# set.seed(1000)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "Calculadora RDM",fluid = TRUE,
    tabPanel("Início - Input das Incertezas e Estratégias",
             sidebarPanel(
               "Faca Upload de seus dados de Input",
                   fileInput("Dados de Input",
                             inputId = "DadosInput",buttonLabel = "Arquivo.xlsx"),
               "Apos informar os dados de inputs, verifique na guia ao lado se suas informacoes foram carregadas. Caso contrario, verifique se o arquivo de dados esta correto."
             ),
             mainPanel(
               "Abaixo serao exibidos os inputs que voce inseriu no arquivo de dados.<br>",
               tabsetPanel(
                 tabPanel("X - Incertezas"
                          ,tableOutput("parametrostable")
                 ),
                 tabPanel("L - Estratégias"
                          ,tableOutput("leverstable")
                 )
                 )
               )
             #)
    ),
    tabPanel("Resultados das Simulações",
             "Esta aba apresenta os resultados do modelo de dinâmica de sistemas utilizado.",
             mainPanel(
             tabsetPanel(
               tabPanel("Resultados das Simulações",
                        "Mostrando primeiras 100 linhas dos resultados"
                        ,tableOutput("dados_simulados_table")
                        ),
               tabPanel("Comparação das Estratégias",
                        "Mostrando primeiras 100 linhas dos resultados"
                        ,tableOutput("analise_regret_table")
                        )
              )
             )
             ),
    tabPanel("Análise dos Resultados",
             
             
             mainPanel(width = 12,
               tabsetPanel(
                 tabPanel("Gráficos - Tempo",
                          column(6,
                                 selectInput("gr1_estrategia_selecionada", choices = 1:20, label = "Selecione uma Estratégia", selected = 1),
                                 plotOutput("plot_clientes1"),
                                 plotOutput("plot_cash1"),
                                 plotOutput("plot_taxa1")
                          ),
                          column(6,
                                 selectInput("gr2_estrategia_selecionada", choices = 1:20, label = "Selecione uma Estratégia", selected = 2),
                                 plotOutput("plot_clientes2"),
                                 plotOutput("plot_cash2"),
                                 plotOutput("plot_taxa2")
                                 )
                 ),
                 tabPanel("Gráficos - Arrependimento",
                          "Nesta Aba todas as estratégias são comparadas de acordo com quatro critérios de análise.",
                          plotOutput("plot_whisker_lever_cash"),
                          plotOutput("plot_whisker_lever_adopters"),
                          plotOutput("plot_whisker_lever_regretperc"),
                          plotOutput("plot_whisker_lever_regret")),
               tabPanel("Grafico - Superficie",
                        selectInput("estrategia_superficie", choices = 1:20, label = "Selecione uma Estratégia", selected = 1),
                        plotlyOutput("plot_superficie")),
               tabPanel("Grafico - Tradeoff",
                        plotlyOutput("plot_tradeoff"))

             )
             )
    )
    # ,tabPanel("Descoberta de Cenários",
    #          "Esta aba apresenta os cenários para os quais uma dada estratégia é vulnerável.",
    #          mainPanel(width = 12
    #            
    #          )
    #          
    # )
    # ,tabPanel("Análise de Tradeoffs",
    #          "Esta aba apresenta resultados da análise de Tradeoffs.",
    #          mainPanel(width = 12
    #          )
    #          
    # )
  )
  )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Esta função apenas retorna o arquivo de Dados
  CarregaDados <- reactive({
    validate(
      need(input$DadosInput != "", "Escolha o arquivo de simulacao de dados corretamente!")
    )
    arquivodados <- input$DadosInput
    if (is.null(arquivodados))
      return(NULL)
    file.copy(arquivodados$datapath,
              paste(arquivodados$datapath, ".xlsx", sep=""))
    return(arquivodados)
  })
  
  # Esta função retorna a lista inputs
  inputs = reactive({
    inputs = CarregaDados()
    if (is.null(inputs))
      return(NULL)
    withProgress(message = 'Carregando...', value = 0.3, {
      #dados = simular_cba(paste(inputs$datapath, ".xlsx", sep=""), modo = "completo")
      objeto_inputs = carregar_inputs(paste(inputs$datapath, ".xlsx", sep=""))
      incProgress(1, detail = "Finalizando")
    })
    
    # if (is.null(arquivoinputs))
    #   return(NULL)
    # inputs = carregar_inputs(arquivoinputs)
    return(objeto_inputs)
  })
  
  # Inputs - Lista de Levers
  inputs_vetor_levers = reactive({
    inputs()$Lever$Levers
  })
  
  
  

  # Tentativa de deixar a escolha de estratégias dinâmica.
  # observe({
  #   # Can also set the label and select items
  #   updateSelectInput("gr1_estrategia_selecionada",
  #                     label = "Estratégia",
  #                     choices = output$inputs_vetor_levers(),
  #                     selected = tail(inputs_vetor_levers, 1)
  #   )
  # })
  
  
  # Dados de Absenteismo simulados
  output_rdm = reactive({
      inputs = CarregaDados()
      if (is.null(inputs))
        return(NULL)
      withProgress(message = 'Calculando...', value = 0.3, {
        #dados = simular_cba(paste(inputs$datapath, ".xlsx", sep=""), modo = "completo")
        dados = simularRDM_e_escolher_estrategia(inputs = paste(inputs$datapath, ".xlsx", sep=""), sdmodel = sdmodel, opcoes = opcoes)
        incProgress(1, detail = "Finalizando")
      })
      return(dados)
    })
  
  
  # Parametros
  resultados_dados_simulados = reactive({
    output_rdm()$DadosSimulados
    })
  
  # Resultados Último Períodos
  resultados_dados_ultimo_periodo = reactive({
    output_rdm()$DadosUltimoPeriodo
  })
  
  # Estratégia Candidata
  resultados_estrategia_candidata = reactive({
    output_rdm()$EstrategiaCandidata
  })
  
  # Resumo das Estrategias
  resultados_resumo_estrategias = reactive({
    output_rdm()$AnaliseRegret$ResumoEstrategias
  })
  
  resultados_analise_regret_dados = reactive({
    output_rdm()$AnaliseRegret$Dados
  })
  
  ###### OUTPUTS ######
  output$dados_simulados_table <- renderTable({
    head(resultados_dados_simulados(),n = 100)
  })
  
  output$analise_regret_table <- renderTable({
    resultados_resumo_estrategias()
  })
  
  output$plot_clientes1 = renderPlot({
    dados = resultados_dados_simulados()
    plot_clientes_uma_estrategia(dados = dados,estrategia = input$gr1_estrategia_selecionada)
  })
  
  output$plot_cash1 = renderPlot({
    dados = resultados_dados_simulados()
    plot_cash_uma_estrategia(dados = dados,estrategia = input$gr1_estrategia_selecionada)
  })
  
  output$plot_taxa1 = renderPlot({
    dados = resultados_dados_simulados()
    plot_taxa_adocao_uma_estrategia(dados = dados,estrategia = input$gr1_estrategia_selecionada)
  })
  
  output$plot_clientes2 = renderPlot({
    dados = resultados_dados_simulados()
    plot_clientes_uma_estrategia(dados = dados,estrategia = input$gr2_estrategia_selecionada)
  })
  
  output$plot_cash2 = renderPlot({
    dados = resultados_dados_simulados()
    plot_cash_uma_estrategia(dados = dados,estrategia = input$gr2_estrategia_selecionada)
  })
  
  output$plot_taxa2 = renderPlot({
    dados = resultados_dados_simulados()
    plot_taxa_adocao_uma_estrategia(dados = dados,estrategia = input$gr2_estrategia_selecionada)
  })
  
  output$plot_whisker_lever_cash = renderPlot({
    grafico_whisker_por_lever(dados_regret = resultados_analise_regret_dados(), variavel = "Cash")
  })
  
  output$plot_whisker_lever_adopters = renderPlot({
    grafico_whisker_por_lever(dados_regret = resultados_analise_regret_dados(), variavel = "Adopters")
  })
  
  output$plot_whisker_lever_regretperc = renderPlot({
    grafico_whisker_por_lever(dados_regret = resultados_analise_regret_dados(), variavel = "CashRegretPerc")
  })
  
  output$plot_whisker_lever_regret = renderPlot({
    grafico_whisker_por_lever(dados_regret = resultados_analise_regret_dados(), variavel = "CashRegret")
  })
  
  output$plot_tradeoff = renderPlotly({
    plot_fronteira_tradeoff_estrategia(results = output_rdm(), opcoes = opcoes) %>%
      layout(autosize = F, width = 800, height = 800, margin = 50)
  })
  
  
  
  
  output$plot_superficie = renderPlotly({
    dados_ultimo_ano = resultados_dados_ultimo_periodo()
    variaveis = c("AdoptionFraction", "ContactRate", "Cash")
    estrategia = input$estrategia_superficie
    gerar_grafico_superficie(dados_ultimo_ano, variaveis, estrategia = estrategia)  %>%
      layout(autosize = F, width = 800, height = 800, margin = 50)
  })
  
  output$leverstable <- renderTable({
    tabela = as.data.frame(inputs()$Levers) 
    tabela
  })
  
  output$parametrostable <- renderTable({
    inputs()$Parametros
  })

  output$downloadData <- downloadHandler(
    filename = function() { paste("output_simulacao", '.csv', sep='') },
    content = function(file) {
      write.table(resultados_cbr(),file,sep=";",dec=",",row.names = FALSE)
    }
  )
  
}


# Run the application
shinyApp(ui = ui, server = server)