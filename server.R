library(shiny)

# Define server logic to read selected file ----
shinyServer(ffunction(input, output) {
  
  output$ppp <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    if(input$disp == "head") {
      return(head(df))
    } else {return(df)}
  })
  
  output$exp <- renderTable({
    req(input$file2)
    df <- read.csv(input$file2$datapath,
                   header = input$header,sep = input$sep,quote = input$quote)
    if(input$disp == "head") {
      return(head(df))
    } else {return(df)}
  })
  
  output$bsh <- renderTable({
    req(input$file3)
    df <- read.csv(input$file3$datapath,
                   header = input$header,sep = input$sep,quote = input$quote)
    if(input$disp == "head") {
      return(head(df))
    } else {return(df)}
  })
  
  ####use uploaded files to plot
  newdata <- reactive({
    ppp_ken <- input$file1
    exp_ken <- input$file2
    bs_ken <- input$file3 
    if (is.null(ppp_ken)| is.null(exp_ken)| is.null(bs_ken)){
      return(NULL)
    } else {
      ###Load all data before the plot
      ppp_ken <- read.csv(input$file1$datapath,
                     header = input$header,sep = input$sep,quote = input$quote)
      exp_ken <- read.csv(input$file2$datapath,
                          header = input$header,sep = input$sep,quote = input$quote)
      bs_ken <- read.csv(input$file3$datapath,
                          header = input$header,sep = input$sep,quote = input$quote)
      ###Per capita real income ($)----
      q_ken=exp_ken/ppp_ken #per capita consumption = real income 
      #sort countries (column) by real income p.c----
      csort<-names(sort(colSums(q_ken),decreasing=TRUE))
      q_ken1<-q_ken[,match(csort,colnames(q_ken))]
      ppp_ken1<-ppp_ken[,match(csort,colnames(ppp_ken))]
      bs_ken1<-bs_ken[,match(csort,colnames(bs_ken))]
      
      
      #
      #p_sd<-100*sqrt(as.numeric(sigma2_P_mat[upper.tri(sigma2_P_mat,diag=FALSE)]))
      #q_sd<-100*sqrt(as.numeric(sigma2_mat[upper.tri(sigma2_mat,diag=FALSE)]))
    }
  })
  
  output$newplot <- renderPlot(
    if (is.null(input$file1)| is.null(input$file2)| is.null(input$file3)){
      return(NULL)
    } else {
      #[Important: Must use for loop here, not within reactive{}]
      ###The quantity dispersion matrix ----
      #compute the logExp_cd and sigma2_cd matrices----
      #[sigma is the bivariate variance matrix]
      logq_list=list();bs_list=list(); 
      logQ_mat=NULL;sigma2_mat=NULL;
      for (c in 1:ncol(q_ken1)){
        #logq_icd and w_icd
        logq_mat=NULL;bs_mat=NULL;
        for (d in 1:ncol(q_ken1)){
          logq_icd=log(q_ken1[,c])-log(q_ken1[,d])
          logq_mat=cbind(logq_mat,logq_icd);
          #
          bs_icd=(0.5*(bs_ken1[,c]+bs_ken1[,d]))/100 
          bs_mat=cbind(bs_mat,bs_icd);
        }
        rownames(logq_mat)=rownames(bs_mat)=rownames(q_ken1);
        colnames(logq_mat)=colnames(bs_mat)=colnames(q_ken1);
        #
        logq_list[[c]]=logq_mat; names(logq_list)[c]=colnames(q_ken1)[c];
        #budget share matrix
        bs_list[[c]]=bs_mat; names(bs_list)[c]=colnames(q_ken1)[c];
        #logq_cd matrix
        logQ_cd=colSums(logq_mat*bs_mat); 
        logQ_mat=rbind(logQ_mat,logQ_cd);
        #sigma2_cd matrix
        sigma2_cd=colSums(bs_mat*sweep(logq_mat,2,logQ_cd,FUN='-')^2) #logq_cd does not change across items
        sigma2_mat<-rbind(sigma2_mat,sigma2_cd)
      }
      colnames(logQ_mat)=rownames(logQ_mat)=colnames(q_ken1)
      colnames(sigma2_mat)=rownames(sigma2_mat)=colnames(q_ken1)
      
      ###The price dispersion matrix ----
      #compute the logP_cd and sigma2_P_cd matrices----
      logp_list=list(); 
      logP_mat=NULL;sigma2_P_mat=NULL;
      for (c in 1:ncol(ppp_ken1)){
        #logp_icd and w_icd
        logp_mat=NULL;bs_mat=NULL;
        for (d in 1:ncol(ppp_ken1)){
          logp_icd=log(ppp_ken1[,c])-log(ppp_ken1[,d])
          logp_mat=cbind(logp_mat,logp_icd);
          #
          bs_icd=(0.5*(bs_ken1[,c]+bs_ken1[,d]))/100 
          bs_mat=cbind(bs_mat,bs_icd);
        }
        rownames(logp_mat)=rownames(ppp_ken1);
        colnames(logp_mat)=colnames(ppp_ken1);
        #
        logp_list[[c]]=logp_mat; names(logp_list)[c]=colnames(ppp_ken1)[c];
        #logExp_cd matrix
        logP_cd=colSums(logp_mat*bs_mat); 
        logP_mat=rbind(logP_mat,logP_cd);
        #sigma2_cd matrix
        sigma2_P_cd=colSums(bs_mat*sweep(logp_mat,2,logP_cd,FUN='-')^2) #logExp_cd does not change across items
        sigma2_P_mat<-rbind(sigma2_P_mat,sigma2_P_cd)
      }
      colnames(logP_mat)=rownames(logP_mat)=colnames(ppp_ken1)
      colnames(sigma2_P_mat)=rownames(sigma2_P_mat)=colnames(ppp_ken1)  
      #dispersion measure  
      p_sd<-100*sqrt(as.numeric(sigma2_P_mat[upper.tri(sigma2_P_mat,diag=FALSE)]))
      q_sd<-100*sqrt(as.numeric(sigma2_mat[upper.tri(sigma2_mat,diag=FALSE)]))
    ###ggplot contour 
      library(ggplot2)
      commonTheme = list(labs(color="",fill="",
                              x="",
                              y=""),
                         theme_bw(),
                         theme(axis.line = element_line(colour = "black"),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_blank(),
                               panel.background = element_blank()), 
                         theme(legend.position="none"),
                         theme(text = element_text(size=15),
                               axis.title=element_text(size=12)
                               #axis.text = element_text(size=40)
                         ))
      
      #Brew a colorscale
      library(RColorBrewer)
      #colorscale = scale_fill_gradientn(
      #  colors = rev(brewer.pal(9, "YlGnBu")),
      #  values = c(0, exp(seq(-3, 0, length.out = 10))))
      colorscale = scale_fill_gradientn(colours =  rev(rainbow(7)[-7]))
      #contour plot----
      x=p_sd;y=q_sd;
      xy=data.frame(cbind(x,y));#xlab='Prices'; ylab='Quantities'
      #xlab=expression(paste(Delta, "log S",sep='')[ct]);ylab=expression(paste(Delta, "log r",sep='')[ict])
      ggplot(data=xy,aes(x,y)) + 
        geom_point(shape = 16,size=1.5, color="blue", fill="blue") +
        stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',bins=40,n=40) + #, h = c(1, 1)
        scale_x_continuous(limits = c(0, 150))+
        scale_y_continuous(limits = c(0, 150))+
        colorscale +
        coord_fixed() +
        geom_abline(slope=1,size=1,linetype=2,size=1.5,colour="darkred") +
        guides(alpha="none") +
        commonTheme +
        labs(x='Prices', y='Quantities')
    #plot(sigma2_mat[,1],sigma2_P_mat[,2])
    }
  )
})
