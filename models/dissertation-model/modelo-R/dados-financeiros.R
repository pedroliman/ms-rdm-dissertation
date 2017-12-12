library(RCurl)

# Quandl API:
# RsCuvs4_WjRPP_zzSzfv

# Us Fundamentals API
api_us_fundamentals = "AzfxuwuOWMDrCA28nAEUcw"

empresas = c("Stratasys Inc", "3D Systems Corp", "Exone CO", "HP Inc", "Proto Labs")
codigos = c(915735, 910638, 1561627, 47217, 1443669)
codigos_api = paste(codigos, collapse = ",")

indicadores = c("Revenues")
indicadores_api = paste(indicadores, collapse = ",")


call_api = paste(
  "https://api.usfundamentals.com/v1/indicators/xbrl?",
  "&companies=",
  codigos_api,
  "&period_type=yq",
  "&token=",
  api_us_fundamentals
  , sep = ""
)

csv_dados_us_fundamentals <- getURL(call_api)

dados_us_fundamentals = read.csv(textConnection(csv_dados_us_fundamentals))

write.csv2(x = dados_us_fundamentals, file = "./fundamentals-data/dados_us_fundamentals.csv")

# Gerando Gráficos e Dados:
library(dplyr)
dados_us_fundamentals %>% dplyr::filter(indicator_id == "GrossProfit")

sales_revenue = dados_us_fundamentals %>% dplyr::filter(indicator_id == "SalesRevenueNet")


#### Dados Quandl ####

library(Quandl)

obter_fundamentos_financeiros_quandl = function(company_code = "DDD") {
  
  # Base de Dados: - 
  # https://www.quandl.com/data/SF0-Free-US-Fundamentals-Data
  # Com algumas alterações é possível usar outras bases do quandl.
  
  # Definindo Chave de Acesso - Esta chave pertence a Pedro Lima, não utilizar:
  Quandl.api_key("RsCuvs4_WjRPP_zzSzfv")
  
  prefixo_base = "SF0/"
  
  variable_codes = c("REVENUE_MRY",
                     "INVENTORY_MRY",
                     "ASSETS_MRY",
                     "CAPEX_MRY",
                     "NETINC_MRY",
                     "GP_MRY",
                     "COR_MRY",
                     "TANGIBLES_MRY",
                     "EBT_MRY", 
                     "FCF_MRY",
                     "INTANGIBLES_MRY",
                     "NCFI_MRY",
                     "NCFF_MRY",
                     'NCFO_MRY',
                     "RND_MRY", 
                     "EBITDA_MRY")
  
  
  variable_names = c("Revenue",
                     "Inventory",
                     "Assets",
                     "Capex",
                     "NetIncome",
                     "GrossProfit",
                     "CostOfRevenue",
                     "TangibleAssets",
                     "EBT", 
                     "FreeCashFlow",
                     "IntangibleAssets",
                     "NetCashFlowFromInvestment",
                     "NetCashFlowFromFinancing",
                     'NetCashFlowFromOperations',
                     "ResearchAndDevelopmentExpenses", 
                     "EBITDA")
  
  
  variable_descriptions = c(
    "[Revenues]: Amount of Revenue recognized from goods sold, services rendered, insurance premiums, or other activities that constitute an earning process. Interest income for financial institutions is reported net of interest expense and provision for credit losses."
    ,"[Inventory]: A component of [ASSETS] representing the amount after valuation and reserves of inventory expected to be sold, or consumed within one year or operating cycle, if longer."
    ,"[Total Assets]: Sum of the carrying amounts as of the balance sheet date of all assets that are recognized. Major components are [CASHNEQ], [INVESTMENTS],[INTANGIBLES], [PPNENET],[TAXASSETS] and [RECEIVABLES]."
    ,"[Capital Expenditure]: A component of [NCFI] representing the net cash inflow (outflow) associated with the acquisition & disposal of long-lived, physical & intangible assets that are used in the normal conduct of business to produce goods and services and are not intended for resale. Includes cash inflows/outflows to pay for construction of self-constructed assets & software."
    ,"[Net Income]: The portion of profit or loss for the period, net of income taxes, which is attributable to the parent after the deduction of [NETINCNCI] from [CONSOLINC], and before the deduction of [PREFDIVIS]."
    ,"[Gross Profit]: Aggregate revenue [REVENUE] less cost of revenue [COR] directly attributable to the revenue generation activity."
    ,"[Cost of Revenue]: The aggregate cost of goods produced and sold and services rendered during the reporting period."
    ,"[Tangible Asset Value]: The value of tangibles assets calculated as the difference between [ASSETS] and [INTANGIBLES]."
    ,"[Earnings before Tax]: Earnings Before Tax is calculated by adding [TAXEXP] back to [NETINC]."
    ,"[Free Cash Flow]: Free Cash Flow is a measure of financial performance calculated as [NCFO] minus [CAPEX]."
    ,"[Goodwill and Intangible Assets]: A component of [ASSETS] representing the carrying amounts of all intangible assets and goodwill as of the balance sheet date, net of accumulated amortization and impairment charges."
    ,"[Net Cash Flow from Investing]: A component of [NCF] representing the amount of cash inflow (outflow) from investing activities, from continuing and discontinued operations. Principal components of investing cash flow are: capital (expenditure) disposal of equipment [CAPEX], business (acquisitions) disposition [NCFBUS] and investment (acquisition) disposal [NCFINV]."
    ,"[Net Cash Flow from Financing]: A component of [NCF] representing the amount of cash inflow (outflow) from financing activities, from continuing and discontinued operations. Principal components of financing cash flow are: issuance (purchase) of equity shares, issuance (repayment) of debt securities, and payment of dividends & other cash distributions."
    ,"[Net Cash Flow from Operations]: A component of [NCF] representing the amount of cash inflow (outflow) from operating activities, from continuing and discontinued operations."
    ,"[Research and Development Expense]: A component of [OPEX] representing the aggregate costs incurred in a planned search or critical investigation aimed at discovery of new knowledge with the hope that such knowledge will be useful in developing a new product or service."
    ,"[Earnings Before Interest, Taxes & Depreciation Amortization (EBITDA)]: EBITDA is a non-GAAP accounting metric that is widely used when assessing the performance of companies, calculated by adding [DEPAMOR] back to [EBIT]."
  )
  
  
  df_variaveis = data.frame(
    VariableCodes = variable_codes,
    VariableNames = variable_names,
    VariableDescriptions = variable_descriptions
  )
  
  sep = "_"
  
  queries = paste(prefixo_base, company_code,sep, variable_codes, sep = "")
  
  list_company = list()
  for (q in queries){
    message(paste("Queriying Quandl for variable", q))
    qnumber = which(queries == q)
    list_company[[variable_names[qnumber]]] = Quandl(q, collapse="annual", start_date="1900-01-01", type="ts") 
  }
  
  df_company = data.frame(time = as.vector(time(list_company[[1]])),
                          as.data.frame(list_company))
  
  ## Salvar Dados Coletados
  write.csv2(df_company, file = paste("./fundamentals-data/financial_data", company_code, ".csv", sep = ""))
  
  message(paste("Finalizada coleta de dados da empresa", company_code))
  
  ## Gerar List com descrição das variáveis e resultados
  list(Variaveis = df_variaveis, Dados = df_company)
}


fundamentos_ddd = obter_fundamentos_financeiros_quandl("DDD")

library(Quandl)

fundamentos_mtls = obter_fundamentos_financeiros_quandl("PRLB")


plot_receita_investimento_3dsystems = plot_linha_duas_variaveis(fundamentos_ddd$Dados, variavel1 = "Revenue", nome_amigavel_variavel1 = "Receita", variavel2 = "ResearchAndDevelopmentExpenses", nome_amigavel_variavel2 = "Invesitmento em P & D")

plot_cash_net_income_3dsystems = plot_linha_duas_variaveis(fundamentos_ddd$Dados, variavel1 = "NetIncome", nome_amigavel_variavel1 = "Lucro Líquido", variavel2 = "GrossProfit", nome_amigavel_variavel2 = "Lucro Bruto")


### Carregando Dados do data.world
library(data.world)


