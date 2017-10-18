# Este é um app que possui a interface para a calculadora oshcba.
## Se está rodando o app pela primeira vez, rode estes comando antes de executar o aplicativo: (Antes de rodar o comando, exclua o "#")
# install.packages(c("shiny","ggplot2","readxl","mc2d","dplyr","devtools"))
#Se precisar atualizaro app, rode este comando:
# library(devtools)
# install_github("pedroliman/oshcba")
library(shiny)
library(shinythemes)
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
  N = 100,
  VarTempo = "Tempo",
  VarCriterio = "RegretPercPercentil75",
  SentidoCriterio = "min"
)

### Definindo uma seed fixa fora do app para ter replicações mehor comparáveis.
# set.seed(1000)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("yeti"),
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "Calculadora RDM",fluid = TRUE,
    tabPanel("Início - XLRM",
             sidebarPanel(
               "Faca Upload de seus dados de Input",
                   fileInput("Dados de Input",
                             inputId = "DadosInput",buttonLabel = "Arquivo.xlsx"),
               "Apos informar os dados de inputs, verifique na guia ao lado se suas informacoes foram carregadas. Caso contrario, verifique se o arquivo de dados esta correto."
             ),
             mainPanel(
               "Abaixo serao exibidos os inputs que voce inseriu no arquivo de dados.<br>",
               tabsetPanel(
                 tabPanel("Parametros"
                          ,tableOutput("parametrostable")
                 ),
                 tabPanel("Estrategias"
                          ,tableOutput("leverstable")
                 )
               )
             )
    ),
    tabPanel("Resultados das Simulações",
             "Esta aba apresenta os resultados do modelo de dinâmica de sistemas utilizado.",
             mainPanel(
             tabsetPanel(
               tabPanel("Resultados das Simulacoes",
                        "Mostrando primeiras 100 linhas dos resultados"
                        ,tableOutput("dados_simulados_table")
                        ),
               tabPanel("Resultados dos último ano de simulação em todos os cenários e estratégias",
                        "Mostrando primeiras 100 linhas dos resultados"
                        ,tableOutput("analise_regret_table")
                        )
              )
             )
             ),
    tabPanel("Análise dos Resultados",
             "Esta aba apresenta resultados para análise da Simulação.",
             mainPanel(
               tabsetPanel(
                 tabPanel("Resultados das Simulacoes",
                          "Gráfico com os resultados de uma determinada estratégia.",
                          plotOutput("plot_clientes"),
                          plotOutput("plot_cash")
                 )
               )
             )
    ),
    tabPanel("Descoberta de Cenários",
             "Esta aba apresenta resultados da descoberta de cenários.",
             mainPanel(width = 12
               
             )
             
    ),
    tabPanel("Análise de Tradeoffs",
             "Esta aba apresenta resultados da análise de Tradeoffs.",
             mainPanel(width = 12
                       
             )
             
    )
  )
  )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  plot_clientes_uma_estrategia = function(dados, estrategia) {
    gr2_dados = subset(dados, (Lever == estrategia))
    ggplot2::ggplot(gr2_dados,
                    aes(x=Tempo, y=Adopters, color=factor(Lever), group=Scenario)) + 
      geom_line() + 
      ylab("Clientes") + 
      xlab("Tempo") +
      labs(color = "Estratégia")
  }
  
  plot_cash_uma_estrategia = function(dados, estrategia) {
    gr2_dados = subset(dados, (Lever == estrategia))
    ggplot2::ggplot(gr2_dados,
                    aes(x=Tempo, y=Cash, color=factor(Lever), group=Scenario)) + 
      geom_line() + 
      ylab("Valor Presente") + 
      xlab("Tempo") +
      labs(color = "Estratégia")
  }
  
  
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
  
  
  
  ###### OUTPUTS ######
  output$dados_simulados_table <- renderTable({
    head(resultados_dados_simulados(),n = 100)
  })
  
  output$analise_regret_table <- renderTable({
    resultados_resumo_estrategias()
  })
  
  output$plot_clientes = renderPlot({
    dados = resultados_dados_simulados()
    plot_clientes_uma_estrategia(dados = dados,estrategia = 1)
  })
  
  output$plot_cash = renderPlot({
    dados = resultados_dados_simulados()
    plot_cash_uma_estrategia(dados = dados,estrategia = 1)
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