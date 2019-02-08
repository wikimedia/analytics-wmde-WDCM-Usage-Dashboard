### ---------------------------------------------------------------------------
### --- WDCM Usage Dashboard, v. Beta 0.1
### --- Script: ui.R, v2.0
### ---------------------------------------------------------------------------

### --- Setup
### --- general
library(shiny)
library(shinydashboard)
library(shinycssloaders)
### --- outputs
library(visNetwork)
library(rbokeh)
library(networkD3)
library(DT)


# - options
options(warn = -1)

### --- User Interface w. {shinydashboard}

shinyUI(
  
  ### --- dashboardPage
  ### --------------------------------
  
  dashboardPage(skin = "black",
                
                ### --- dashboarHeader
                ### --------------------------------
                
                dashboardHeader(
                  # - Title
                  title = "WDCM: Wikidata Usage",
                  titleWidth = 300
                ), 
                ### ---- END dashboardHeader
                
                ### --- dashboardSidebar
                ### --------------------------------
                
                dashboardSidebar(
                  sidebarMenu(
                    id = "tabsWDCM",
                    menuItem(text = "Wikidata Usage", 
                             tabName = "usage", 
                             icon = icon("barcode"),
                             selected = TRUE,
                             menuSubItem('Project Summary',
                                         tabName = 'projectSummary',
                                         icon = icon('line-chart')
                                         ),
                             menuSubItem('Category Summary',
                                         tabName = 'categorySummary',
                                         icon = icon('line-chart')
                                         )

                             ),
                    menuItem(text = "Tabs/Crosstabs", 
                             tabName = "tabs", 
                             icon = icon("barcode")
                             ),
                    menuItem(text = "Tables",
                             tabName = "tables",
                             icon = icon("barcode")
                             ),
                    menuItem(text = "Documentation",
                             tabName = "documentation",
                             icon = icon("barcode")
                             ),
                    menuItem(text = "Navigate WDCM",
                             tabName = "navigation",
                             icon = icon("barcode")
                             )
                  )
                ),
                ### --- END dashboardSidebar
                
                ### --- dashboardBody
                ### --------------------------------
                
                dashboardBody(
                  
                  # - style
                  tags$head(tags$style(HTML('.content-wrapper, .right-side {
                                              background-color: #ffffff;
                                            }'))),
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }"
                  ),
                  
                  tabItems(
                    
                    ### --- TAB: Overview
                    ### --------------------------------
                    
                    tabItem(tabName = "usage"
                            ),
                    tabItem(tabName = "projectSummary",
                            fluidRow(
                              column(width = 6,
                                     HTML('<p style="font-size:80%;"><b>Project Summary. </b>Wikidata usage overview for a specific 
                                          project, including: the distribution of usage in Wikidata semantic categories, 
                                          total Wikidata usage volume, similar projects, and the top Wikidata items.
                                          Use this tab to get a quick overview of WD usage on the project of interest.</p>'),
                                     hr()
                              ),
                              column(width = 3),
                              column(width = 3,
                                     HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Usage-Dashboard" target = "_blank">GitHub</a></p>'),
                                     htmlOutput('updateString')
                              )
                            ),
                            fluidRow(
                              column(width = 4, 
                                     selectizeInput('projects',
                                                    'Search projects:',
                                                    choices = NULL,
                                                    multiple = FALSE)
                                     )
                              ),
                            fluidRow(
                              column(width = 6,
                                     withSpinner(htmlOutput('projectOverview_Report'))
                                     ),
                              column(width = 6, 
                                     withSpinner(htmlOutput('projectOverview_relativeRank_Title'))
                                     )
                            ),
                            fluidRow(
                              column(width = 6,
                                     withSpinner(plotOutput('projectOverview_Category',
                                                            width = "100%",
                                                            height = "500px"))
                              ),
                              column(width = 6,
                                     withSpinner(plotOutput('projectOverview_relativeRank',
                                                            width = "100%",
                                                            height = "500px"))
                              ),
                              column(width = 12, 
                                     hr()
                              )
                            ),
                            fluidRow(
                              column(width = 6,
                                     htmlOutput('projectOverview_semantics_Title'),
                                     br(), 
                                     HTML('<p style="font-size:80%;"align="left"><b>Note: </b>We study the distribution of Wikidata usage across the semantic categories to 
                                     determine which client projects use Wikidata in a similar way. In this graph, each project points towards the one 
                                     most similar to it. The selected projects has a different color. The results are relevant only in the context 
                                     of the current selection: the selected project and its 20 nearest semantic neighboors.</p>'),
                                     withSpinner(visNetwork::visNetworkOutput('projectOverview_semantics',
                                                                              width = "100%",
                                                                              height = "600px"))
                                     ),
                              column(width = 6, 
                                     htmlOutput('projectOverview_topItems_Title'),
                                     br(), br(),
                                     HTML('<p style="font-size:80%;"align="left"><b>Note: </b>In the absence of English item label the Wikidata item ID
                                          is used in place of it.</p>'),
                                     withSpinner(plotOutput('projectOverview_topItems',
                                                            width = "100%",
                                                            height = "600px"))
                              )
                            ),
                            fluidRow(
                              hr(),
                              column(width = 1,
                                     br(),
                                     img(src = 'Wikidata-logo-en.png',
                                         align = "left")
                              ),
                              column(width = 11,
                                     hr(),
                                     HTML('<p style="font-size:80%;"><b>WDCM Usage :: Wikidata, WMDE 2019</b><br></p>'),
                                     HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, 
                                          WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                     br(),
                                     br()
                              )
                            )
                            ), ### --- END projectSummary
                    tabItem(tabName = "categorySummary",
                            fluidRow(
                              column(width = 6,
                                    HTML('<p style="font-size:80%;"><b>Category Summary. </b>Wikidata usage overview for a specific category,
                                         including the distribution of usage in category accross projects and the top Wikidata items per category.</p>'),
                                    hr()
                                    ),
                              column(width = 3),
                              column(width = 3,
                                     HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Usage-Dashboard" target = "_blank">GitHub</a></p>')
                                     )
                              ),
                            fluidRow(
                              column(width = 4,
                                     selectizeInput('categories',
                                                    'Search categories:',
                                                    choices = NULL,
                                                    multiple = FALSE)
                                     )
                              ),
                            fluidRow(
                              column(width = 6,
                                     htmlOutput('categoryProjects_overview_Title'),
                                     br(),
                                     withSpinner(plotOutput('categoryProjects_overview',
                                                            width = "100%",
                                                            height = "600px")
                                                 )
                                     ),
                              column(width = 6,
                                     htmlOutput('categoryItems_overview_Title'),
                                     HTML("<p style=\"font-size:80%;\"><b>Note: </b>In the absence of English item label the Wikidata item ID
                                          is used in place of it.</p>"),
                                     br(), br(),
                                     withSpinner(plotOutput('categoryItems_overview',
                                                            width = "100%",
                                                            height = "600px")
                                                 )
                                     )
                              ),
                            fluidRow(
                              column(width = 6,
                                     hr(),
                                     h3('Categories General Overview'),
                                     HTML('<b>Wikidata item usage per semantic category</b><br>
                                      <p style="font-size:80%;"><b>Note:</b> The current selection of semantic categories does not 
                                          encompass all Wikidata items.</p>'),
                                     br(), br(),
                                     withSpinner(plotOutput('basicFacts_CategoryLine',
                                                            width = "100%",
                                                            height = "700px")
                                                 )
                                     ),
                              column(width = 6,
                                     br(), br(), br(), br(),
                                     HTML('<b>Wikidata item usage per semantic category in each project type</b><br>
                                          <p style="font-size:80%;"><b>Note:</b> Item usage count is given on a logarithmic scale.</p>'),
                                     br(), br(),
                                     withSpinner(plotOutput('basicFacts_ProjectTypeCategory',
                                                            width = "100%",
                                                            height = "700px")
                                                 )
                                     )
                              ),
                            fluidRow(
                              hr(),
                              column(width = 1,
                                     br(),
                                     img(src = 'Wikidata-logo-en.png',
                                         align = "left")
                              ),
                              column(width = 11,
                                     hr(),
                                     HTML('<p style="font-size:80%;"><b>WDCM Usage :: Wikidata, WMDE 2019</b><br></p>'),
                                     HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                      <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                     br(), br()
                                     )
                              )
                            ), ### --- END categorySummary
                    tabItem(tabName = "tabs",
                            fluidRow(
                              column(width = 6,
                                     HTML('<p style="font-size:80%;"><b>WD Usage Tabs/Crosstabs. </b>Here you can make <b>selections</b> of client projects and semantic categories to learn about Wikidata 
                                          usage across them.<br> <b>Note:</b> You can search and add projects into the <i>Search projects</i> field by 
                                          using (a) <b>project names</b> (e.g. <i>enwiki</i>, <i>dewiki</i>, <i>sawikiquote</i>, and similar or (b) by using 
                                          <b>project types</b> that start with <b>"_"</b> (underscore, e.g. <i>_Wikipedia</i>, <i>_Wikisource</i>, <i>_Commons</i>, and 
                                          similar; try typing anything into the Select projects field that starts with an underscore). Please note that by selecting 
                                          a project type (again: <i>_Wikipedia</i>, <i>_Wikiquote</i>, and similar) you are selecting <b>all</b> client 
                                          projects of the respective type, and that\'s potentially a lot of data. The Dashboard will pick unique 
                                          projects from whatever you have inserted into the Search projects field. The selection of projects will be intesected 
                                          with the selection of semantic categories from the Select categories field, and the obtained results will refer only 
                                          to the Wikidata items from the current selection of client projects <i>and</i> semantic categories. 
                                          In other words: <i>disjunction</i> operates inside the two search fields, while <i>conjunction</i> operates 
                                          across the two search fields.<br> <b>Note:</b> The Dashboard will initialize a choice of three project types 
                                          (<i>Wikipedia</i>, <i>Wikinews</i>, and <i>Wiktionary</i>) and a random choice of six semantic categories. All charts will present at 
                                          most 25 top projects in respect to the Wikidata usage and relative to the current selection; however, <b>complete 
                                          selection data sets</b> are available for download (<i>.csv</i>) beneath each chart.</p>'),
                                     hr()
                                     ),
                              column(width = 3),
                              column(width = 3,
                                     HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Usage-Dashboard" target = "_blank">GitHub</a></p>')
                                     )
                              ),
                            fluidRow(
                              column(width = 3,
                                     selectizeInput('selectProject',
                                                    'Search projects:',
                                                    choices = NULL,
                                                    multiple = TRUE)
                                     ),
                              column(width = 3,
                                     selectizeInput('selectCategories',
                                                    'Search categories:',
                                                    choices = NULL,
                                                    multiple = TRUE)
                                     )
                              ),
                            fluidRow(
                              column(width = 2,
                                     actionButton('applySelection',
                                                  label = "Apply Selection",
                                                  width = '70%',
                                                  icon = icon("database", 
                                                              class = NULL, 
                                                              lib = "font-awesome")
                                                  )
                                     )
                              ),
                            fluidRow(
                              column(width = 12,
                                     hr()
                              )
                            ),
                            fluidRow(
                              column(width = 6,
                                     h4('Projects'),
                                     withSpinner(plotOutput('tabulations_projectsChart', 
                                                            height = "600px")),
                                     downloadButton('tabulations_projectsDownload_Frame',
                                                    'Data (csv)')
                              ),
                              column(width = 6,
                                     h4('Categories'),
                                     withSpinner(plotOutput('tabulations_categoriesChart', 
                                                            height = "600px")),
                                     downloadButton('tabulations_categoriesDownload_Frame',
                                                    'Data (csv)')
                                     )
                              ),
                            fluidRow(
                              column(width = 12,
                                     hr()
                                     )
                            ),
                            fluidRow(
                              column(width = 6,
                                     h4('Project Types'),
                                     withSpinner(plotOutput('tabulations_projectTypesChart', 
                                                            height = "600px")),
                                     downloadButton('tabulations_projectTypesChart_Frame',
                                                    'Data (csv)')
                              ),
                              column(width = 6
                              )
                            ),
                            fluidRow(
                              column(width = 12,
                                     hr()
                              )
                            ),
                            fluidRow(
                              column(width = 12,
                                     h4('Project vs Categories'),
                                     withSpinner(plotOutput('crosstabulations_projectsCategoriesChart', 
                                                            height = "850px")),
                                     downloadButton('crosstabulations_projectsCategoriesFrame',
                                                    'Data (csv)')
                                     )
                              ),
                            fluidRow(
                              column(width = 12,
                                     hr()
                                     )
                              ),
                            fluidRow(
                              column(width = 12,
                                     h4('Project Types vs Categories'),
                                     withSpinner(plotOutput('crosstabulations_projectTypesCategoriesChart', 
                                                            height = "850px")),
                                     downloadButton('crosstabulations_projectTypeCategoriesChartFrame',
                                                    'Data (csv)')
                                     )
                              ),
                            fluidRow(
                              hr(),
                              column(width = 1,
                                     br(),
                                     img(src = 'Wikidata-logo-en.png',
                                         align = "left")
                              ),
                              column(width = 11,
                                     hr(),
                                     HTML('<p style="font-size:80%;"><b>WDCM Usage :: Wikidata, WMDE 2019</b><br></p>'),
                                     HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                      <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                     br(), br()
                              )
                            )
                            ), ### --- END tabs
                    tabItem(tabName = "tables",
                            fluidRow(
                              column(width = 6,
                                     HTML('<p style="font-size:80%;"><b>WD Usage Tables. </b>Here you can access <b> some tabulated and cross-tabulated 
                                            raw data</b> on Wikidata usage. <br> All tables can be searched and sorted by any of the respective columns.</p>'),
                                     hr()
                                          ),
                              column(width = 3),
                              column(width = 3,
                                     HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Usage-Dashboard" target = "_blank">GitHub</a></p>')
                                     )
                              ),
                            fluidRow(
                              column(width = 4,
                                     HTML('<font size = 2><b>Table A. Project Totals.</b></font>'),
                                     br(), br(),
                                     withSpinner(DT::dataTableOutput('projectTable', width = "100%"))
                              ),
                              column(width = 4,
                                     HTML('<font size = 2><b>Table B. Category Totals.</b></font>'),
                                     br(), br(),
                                     withSpinner(DT::dataTableOutput('CategoryTable', width = "100%"))
                              ),
                              column(width = 4,
                                     HTML('<font size = 2><b>Table C. Project vs Category Cross-Tabulation.</b></font>'),
                                     br(), br(),
                                     withSpinner(DT::dataTableOutput('projectCategoryDataTable', width = "100%"))
                              )
                            ),
                            fluidRow(
                              column(width = 12,
                                     hr()
                              )
                            ),
                            fluidRow(
                              column(width = 4,
                                     HTML('<font size = 2><b>Table D. Project Type Totals.</b></font>'),
                                     br(), br(),
                                     withSpinner(DT::dataTableOutput('projectType', width = "100%"))
                              ),
                              column(width = 6,
                                     HTML('<font size = 2><b>Table E. Project Type vs Category Cross-Tabulation.</b></font>'),
                                     br(), br(),
                                     withSpinner(DT::dataTableOutput('projectTypeCategory', width = "100%"))
                              )
                            ),
                            fluidRow(
                              hr(),
                              column(width = 1,
                                     br(),
                                     img(src = 'Wikidata-logo-en.png',
                                         align = "left")
                              ),
                              column(width = 11,
                                     hr(),
                                     HTML('<p style="font-size:80%;"><b>WDCM Usage :: Wikidata, WMDE 2019</b><br></p>'),
                                     HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                      <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                     br(), br()
                                     )
                              )
                            ), ### --- END tables
                    tabItem(tabName = "documentation",
                            fluidRow(
                              column(width = 8,
                                     HTML('<h2>WDCM Usage Dashboard</h2>
                                          <h4>Description</h4>
                                          <hr>
                                          <h4>Introduction</h4>
                                          <br>
                                          <p style="font-size:85%;">This Dashboard is a part of the <b>Wikidata Concepts Monitor (WDMC)</b>. The WDCM system provides analytics on Wikidata usage
                                          across the Wikimedia sister projects. The WDCM Usage Dashboard focuses on providing the detailed statistics on Wikidata usage in particular sister projects or
                                          the selected subsets of them. Three tabs that present analytical results in this Dashboard receive a description here: (1) <b><i>WD Usage</i></b>, (2) <b><i>Tabs/Crosstabs</i></b>,
                                          and (3) <b><i>Tables</i></b>. But first, definitions.</p>
                                          <hr>
                                          <h4>Definitions</h4>
                                          <br>
                                          <p style="font-size:85%;"><b>N.B.</b> The current <b>Wikidata item usage statistic</b> definition is <i>the count of the number of pages in a particular client project
                                          where the respective Wikidata item is used</i>. Thus, the current definition ignores the usage aspects completely. This definition is motivated by the currently
                                          present constraints in Wikidata usage tracking across the client projects
                                          (see <a href = "https://www.mediawiki.org/wiki/Wikibase/Schema/wbc_entity_usage" target = "_blank">Wikibase/Schema/wbc entity usage</a>).
                                          With a more mature Wikidata usage tracking system, the definition will become a subject
                                          of change. The term <b>Wikidata usage volume</b> is reserved for total Wikidata usage (i.e. the sum of usage statistics) in a particular
                                          client project, group of client projects, or semantic categories. By a <b>Wikidata semantic category</b> we mean a selection of Wikidata items that is
                                          that is operationally defined by a respective SPARQL query, returning a selection of items that intuitivelly match a human, natural semantic category.
                                          The structure of Wikidata does not necessarily match any intuitive human semantics. In WDCM, an effort is made to select the semantic categories so to match
                                          the intuitive, everyday semantics as much as possible, in order to assist anyone involved in analytical work with this system. However, the choice of semantic
                                          categories in WDCM is not necessarily exhaustive (i.e. they do not necessarily cover all Wikidata items), neither the categories are necessarily
                                          mutually exclusive. The Wikidata ontology is very complex and a product of work of many people, so there is an optimization price to be paid in every attempt to
                                          adapt or simplify its present structure to the needs of a statistical analytical system such as WDCM. The current set of WDCM semantic categories is thus not
                                          normative in any sense and a subject  of change in any moment, depending upon the analytical needs of the community.</p>
                                          <p style="font-size:85%;">The currently used <b>WDCM Taxonomy</b> of Wikidata items encompasses the following 14 semantic categories: <i>Geographical Object</i>, <i>Organization</i>, 
                                          <i>Architectural Structure</i>, <i>Human</i>, <i>Wikimedia</i>, <i>Work of Art</i>, <i>Book</i>, <i>Gene</i>, <i>Scientific Article</i>, 
                                          <i>Chemical Entities</i>, <i>Astronomical Object</i>, <i>Thoroughfare</i>, <i>Event</i>, and <i>Taxon</i>.</p>
                                          <hr>
                                          <h4>Usage</h4>
                                          <br>
                                          <p style="font-size:90%;">The Usage tab provides elementary statistics on Wikidata usage across the semantic categories (left column) and sister projects
                                          (right column).<br>
                                          <b><i>To the left</i></b>, we first encounter a general overview of <i>Basic Facts</i>: the number of Wikidata items that are encompassed by the current WDCM taxonomy (in effect,
                                          this is the number of items that are encompassed by all WDCM analyses), the number of sister projects that have client-side Wikidata usage tracking enabled (currently,
                                          that means that the <a href = "https://www.mediawiki.org/wiki/Wikibase/Schema/wbc_entity_usage" target = "_blank">Wikibase/Schema/wbc entity usage</a>) is present there),
                                          the number of semantic categories in the current version of the WDCM Taxonomy, and the number of different sister project types (e.g. <i>Wikipedia</i>, <i>Wikinews</i>, etc).
                                          <br>
                                          The <b>Category Report</b> subsection allows you to select a specific semantic category and generate two charts beneath the selection: (a) the category top 30 projects chart, and
                                          (b) the category top 30 Wikidata items chart. The first chart will display 30 sister projects that use Wikidata items from this semantic category the most, with the usage data
                                          represented on the horizontal axis, and the project labels on the vertical axis. The percentages next to the data points in this chart refer to the proportion of total category usage
                                          that takes place in the respective project. The next chart will display the 30 most popular items from the selected semantic category: item usage is again placed on the horizontal axis,
                                          item labels are on the vertical axis, and item IDs are placed next to the data points themselves.
                                          <br>
                                          The <b>Categories General Overview</b> subsection is static and allows no selection; it introduces two concise overviews of Wikidata usage across the semantic categories of
                                          Wikidata items. The <i>Wikidata Usage per Semantic Cateory</i> chart provides semantic categories on the vertical and item usage statistics on the horizontal axis; the percentages
                                          tells us about the proportion of total Wikidata usage that the respective semantic category carries. Beneath, the <i>Wikidata item usage per semantic category in each project type</i>
                                          provides a cross-tabulation of semantic categories vs. sister project types. The categories are color-coded and represented on the horizontal axes, while each chart represents one project
                                          type. The usage scale, represented on the vertical axes, is logarithmic to ease the comparison and enable practical data visualization.
                                          <br>
                                          <b><i>To the right</i></b>, an opportunity to inspect Wikidata usage in a single Wikimedia project is provided. The <b>Project Report</b> section allows you to select a single Wikimedia
                                          project and obtain results on it. The first section that will be generated upon making a selection provides a concise narrative summary of Wikidata usage in the selected project alongside
                                          a chart presenting an overview of Wikidata usage per semantic category. The next chart, <i>Wikidata usage rank</i>, show the rank position of the selected project among other sister projects
                                          in respect to the Wikidata usage volume. Beneath, a more complex structure, <i>Semantic Neighbourhood</i>, is given. In this network, or a directed graph if you prefere, each project points
                                          towards the one most similar to it. The selected projects has a different color. The results are relevant only in the context of the current selection: the selected project and its 20 nearest
                                          semantic neighboors only are presented. Once again: each project points to the one which utilizes Wikidata in a way most similar to it. The <i>top 30 Wikidata items</i> chart presents the top 30
                                          Wikidata items in the selected project: item labels are given on the vertical axis, Wikidata usage on the horizontal axis, and the item IDs are labeled close to the data points themselves.
                                          </p>
                                          <hr>
                                          <h4>Tabs/Crosstabs</h4>
                                          <br>
                                          <p style="font-size:90%;">
                                          Here we have the most direct opportunity to study the Wikidata usage statistics across the sister projects. A selection of projects and semantic categories will be intersected and only results in
                                          the scope of the intersection will be returned. The charts should be self-explanatory: the usage statistic is always represented by the vertical axis, while the horizontal axis and sub-panels play
                                          various roles in the context of whether a category vs project or a category vs project type crosstabulation is provided. Data points are labeled in million (M) or thousand (K) pages (see Wikidata usage)
                                          definition above). While charts can display a limited number of data points only, relative to the size of the selection, each of them is accompanied by a <b>Data (csv)</b> button that will initiate a
                                          download of the full respective data set as a comma separated file.
                                          </p>
                                          <hr>
                                          <h4>Tables</h4>
                                          <br>
                                          <p style="font-size:90%;">The section presents searchable and sortable tables and crosstabulations with self-explanatory semantics. Access full WDCM usage datasets from here.</p>
                                          ')
                                     ),
                              column(width = 1),
                              column(width = 3,
                                     HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Usage-Dashboard" target = "_blank">GitHub</a></p>')
                                     )
                                     ),
                            fluidRow(
                              hr(),
                              column(width = 1,
                                     br(),
                                     img(src = 'Wikidata-logo-en.png',
                                         align = "left")
                              ),
                              column(width = 11,
                                     hr(),
                                     HTML('<p style="font-size:80%;"><b>WDCM Usage :: Wikidata, WMDE 2019</b><br></p>'),
                                     HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                      <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                     br(), br()
                                     )
                              )
                            ), ### --- END documentation
                    tabItem(tabName = "navigation",
                            fluidRow(
                              column(width = 6,
                                     includeHTML(file.path("www", "wdcmNavigate.html"))
                                     ),
                              column(width = 3),
                              column(width = 3,
                                     HTML('<p style="font-size:80%;"align="right">
                                          <a href = "https://wikitech.wikimedia.org/wiki/Wikidata_Concepts_Monitor" target="_blank">Documentation</a><br>
                                          <a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/" target = "_blank">Public datasets</a><br>
                                          <a href = "https://github.com/wikimedia/analytics-wmde-WDCM-Usage-Dashboard" target = "_blank">GitHub</a></p>')
                                     )
                                     ),
                            fluidRow(
                              hr(),
                              column(width = 1,
                                     br(),
                                     img(src = 'Wikidata-logo-en.png',
                                         align = "left")
                              ),
                              column(width = 11,
                                     hr(),
                                     HTML('<p style="font-size:80%;"><b>WDCM Usage :: Wikidata, WMDE 2019</b><br></p>'),
                                     HTML('<p style="font-size:80%;"><b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br>
                                          <b>e-mail:</b> goran.milovanovic_ext@wikimedia.de<br><b>IRC:</b> goransm</p>'),
                                     br(), br()
                                     )
                              )
                            )
                    
                    ) ### --- END tab Navigate
                  
                  ) ### --- END dashboardBody
                
                ) ### --- END dashboardPage
  
  ) # END shinyUI

