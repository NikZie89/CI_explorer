library(dplyr)
library(ggplot2)
library(shiny)

#Simulating a large population and then draw various samples, calculate the standardized regression coefficients
#Next, calculate the confidence intervals around the beta of each sample and compare this to 

ui <- fluidPage(
  titlePanel("95%-confidence intervals around different samples"),
  sidebarLayout(
    sidebarPanel(
        sliderInput(inputId = "n_samples", label = "Number of samples", min = 1  ,max=100, value = 10, step = 1)
                ),
    
    mainPanel(
  plotOutput("sample_plot")
             )
                )
  
)

server <- function(input, output, session){

set.seed(1234)
x_var<-rnorm(100000, mean=10, sd=1)
y_var<-x_var/10+rnorm(100000, mean=0, sd=0.05)    #basically just adding a bit of random noise, to remove the perfect correlation.

population<-cbind.data.frame(x_var, y_var)%>%
            mutate(scaled_x=scale(x_var), scaled_y=scale(y_var))

true_cor<-cor(x_var, y_var) #high correlation of 0.895, as we only have one predictor, this is the same as the standardized beta coefficient
summary(lm(scaled_y~scaled_x, data = population))


samples <- reactive({ out <- vector("list", input$n_samples)
              set.seed(1234)
              for(i in 1:length(out)){out[[i]] <- dplyr::sample_n(population, size=200, replace=FALSE)}
                  out })

#test<-observe(samples())
#samples <- reactive({   
#samples<-list(NULL)
#for(i in 1:100){samples[[i]]<-dplyr::sample_n(population, size=200, replace=FALSE)}     #using dplyr to take 100 samples with 200 cases each from the population 
#})                                                                     

output$sample_plot<-renderPlot({

hundred_regressions<-samples()%>%lapply(lm, formula=scaled_y~scaled_x)
hundred_regression_summaries<-lapply(hundred_regressions, summary)
hundred_coefficients<-lapply(hundred_regressions, function(x) coef(x)[2])%>%unlist()
hundred_CIs<-lapply(hundred_regressions, confint.lm, parm="scaled_x", level=0.95)


CI_vector<-unlist(hundred_CIs) #for sure the following can be done more efficiently
lower_boundary<-CI_vector[seq(from=1, to=length(CI_vector), by=2)]
upper_boundary<-CI_vector[seq(from=2, to=length(CI_vector), by=2)]


CIs<-cbind.data.frame(lower_boundary, upper_boundary)%>%
                              mutate(beta_coef=hundred_coefficients)%>%
                              arrange(beta_coef)%>%
                              mutate(sample=row_number(), 
                                    includes_true_beta=case_when(lower_boundary<=true_cor & upper_boundary>=true_cor~"YES",
                                                                                                                TRUE ~"NO"),
                                    CI_color=case_when(includes_true_beta=="YES"~"green",
                                                                            TRUE~"red"))

cols<-c("YES"="forestgreen", "NO"="red")

ggplot(CIs, aes(y=as.factor(sample)))+geom_segment(aes(x=lower_boundary, xend=upper_boundary, y=sample, yend=sample, col=as.character(includes_true_beta)))+
  geom_vline(aes(xintercept=true_cor), colour="red")+
  geom_point(aes(beta_coef, col=as.character(includes_true_beta)))+
  xlab("Estimated beta")+ ylab("Number of samples")+
  labs(title="Confidence intervals around estimated betas of the samples", 
       caption="The vertical red line indicates the true beta of the population",
       color = "Population beta included in CI?")+
  scale_colour_manual(values=cols)+
  #ylim(NA, 100)+
  xlim(0.6, 1.2)+
  #scale_y_continuous(breaks = seq(0, max(CIs$sample), 10))+
  scale_y_discrete(breaks=function(n) n[floor(length(n)/5)*1:5])+
  theme_classic()
                        })

}

shinyApp(ui = ui, server = server)