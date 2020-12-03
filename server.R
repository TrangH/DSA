server <- function(input, output) {
####original data to plot----
     ppp <- reactive({
      if (is.null(input$file1)) {
        #pre-load data if no input
        id1<- "1SEOKJ4ULpweqIA6wAATgNT3fO8-6j987"# google file ID
        read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id1),
                 header=TRUE,check.names=FALSE,stringsAsFactors=FALSE)
      } else {
        read.csv(input$file1$datapath, header = input$header, sep = input$sep,
                 quote = input$quote)
      }
    })
    
    exp <- reactive({
      if (is.null(input$file2)) {
        id2<- "1Cyr41rV7I7hI1kKxawyaas90pzKrTqvy" 
        read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id2),
                 header=TRUE,check.names=FALSE,stringsAsFactors=FALSE)
      } else {
        read.csv(input$file2$datapath, header = input$header, sep = input$sep,
                 quote = input$quote)
      }
    })
    
    bs <- reactive({
      if (is.null(input$file3)) {
        id3<- "1H3O7-VK7JTP_JT-OYKHPgoZLJEq7hj7Z"
        read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id3),
                 header=TRUE,check.names=FALSE,stringsAsFactors=FALSE)
      } else {
        read.csv(input$file3$datapath, header = input$header, sep = input$sep,
                 quote = input$quote)
      }
    })

###original data to display----
  #[need to change this]
  output$ppp <- renderTable({
    df <- ppp()
      if(input$disp == "all"){
        return(df)
        } else {return(head(df))}
  })

  output$exp <- renderTable({
    df <- exp()
    if(input$disp == "all"){
      return(df)
    } else {return(head(df))}
  })
  
  output$bs <- renderTable({
    df <- bs()
    if(input$disp == "all"){
      return(df)
    } else {return(head(df))}
  })
  

  ###The q dispersion matrix----
  output$q_disp <- renderTable({
      ppp_ken<-ppp();exp_ken<-exp();bs_ken<-bs();
      ###Per capita real income ($)----
      q_ken=exp_ken[,-1]/ppp_ken[,-1] #per capita consumption = real income 
      #sort countries (column) by real income p.c----
      csort<-names(sort(colSums(q_ken),decreasing=TRUE))
      q_ken1<-q_ken[,match(csort,colnames(q_ken))]
      ppp_ken1<-ppp_ken[,match(csort,colnames(ppp_ken))]
      bs_ken1<-bs_ken[,match(csort,colnames(bs_ken))]
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
        #round(colSums(bs_mat*(sweep(logq_mat,2,logq_cd,FUN='-'))),2) #Check: all zero
        #round(colSums(bs_mat),2) #Check: all 1
      }
      colnames(logQ_mat)=rownames(logQ_mat)=colnames(q_ken1)
      colnames(sigma2_mat)=rownames(sigma2_mat)=colnames(q_ken1)
      
      #[among quintiles]
      c_q1<-csort[1:35];c_q2<-csort[36:71];c_q3<-csort[72:107];c_q4<-csort[108:143];c_q5<-csort[144:176]
      c_qlist<-list(c_q1,c_q2,c_q3,c_q4,c_q5)
      #
      sigma_mat<-NULL;
      for (i in 1:5){
        rowpos<-match(c_qlist[[i]],rownames(sigma2_mat))
        vec=NULL;
        for (j in 1:5) {
          colpos<-match(c_qlist[[j]],colnames(sigma2_mat))
          vec<-c(vec,mean(sqrt(sigma2_mat[rowpos,colpos])))
        }
        sigma_mat<-rbind(sigma_mat,round(vec,2))
      }
      colnames(sigma_mat)=rownames(sigma_mat)=c('Q1 (Richest)','Q2','Q3','Q4','Q5 (Poorest)')
      return(sigma_mat)
  })

  ###Distribution of prices and quantities----
  output$density <- renderPlot({
      ppp_ken<-ppp();exp_ken<-exp();bs_ken<-bs();
      q_ken=exp_ken[,-1]/ppp_ken[,-1] #per capita consumption = real income 
      csort<-names(sort(colSums(q_ken),decreasing=TRUE))
      q_ken1<-q_ken[,match(csort,colnames(q_ken))]
      ppp_ken1<-ppp_ken[,match(csort,colnames(ppp_ken))]
      #
      logP_mat_food=NULL;logQ_mat_food=NULL;
      for (c in 1:ncol(ppp_ken1)){
        logp_mat_food=NULL;logq_mat_food=NULL; 
        for (d in 1:ncol(ppp_ken1)){
          logp_icd=log(ppp_ken1[i,c])-log(ppp_ken1[i,d])
          logp_mat_food=cbind(logp_mat_food,logp_icd);
          #
          logq_icd=log(q_ken1[i,c])-log(q_ken1[i,d])
          logq_mat_food=cbind(logq_mat_food,logq_icd);
        }
        logP_mat_food=rbind(logP_mat_food,logp_mat_food);
        logQ_mat_food=rbind(logQ_mat_food,logq_mat_food);
        #
      }
      colnames(logP_mat_food)=rownames(logP_mat_food)=colnames(logQ_mat_food)=rownames(logQ_mat_food)=colnames(ppp_ken1)
      #
      logP_mat_food<-logP_mat_food-logP_mat; logQ_mat_food<-logQ_mat_food-(1/2)*logQ_mat
      #plot data
      x=100*as.vector(logP_mat_food[upper.tri(logP_mat_food,diag=FALSE)]); #length(x)
      y=100*as.vector(logQ_mat_food[upper.tri(logQ_mat_food,diag=FALSE)]); #length(y)
      colorscale = scale_fill_gradientn(colours =  rev(rainbow(7)[-7]))
      xy=data.frame(cbind(x,y)); 
      #
      commonTheme = list(labs(color="",fill="",
                              x="",
                              y=""),
                         theme_bw(),
                         theme(axis.line = element_line(colour = "black"),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_blank(),
                               panel.background = element_blank()), 
                         theme(legend.position="top"),
                         theme(text = element_text(size=20),
                               axis.title=element_text(size=20)
                               #axis.text = element_text(size=40)
                         ))
      # 
      dfGamma=data.frame(Value=rbind(as.matrix(xy$x),as.matrix(xy$y)),
                         Variable=c(rep('Prices',nrow(xy)),rep('Quantities',nrow(xy))))
      p <- ggplot(dfGamma, aes(x = Value, fill = Variable, color = Variable)) +
        geom_density(aes(group = Variable), alpha=0.5) +
        scale_color_manual(values=c('blue','red')) +
        scale_fill_manual(values=c('blue','red')) +
        geom_vline(xintercept = 0, linetype="dashed", 
                   color = "darkgreen", size=0.8) +
        commonTheme + labs(x='Price/Quantity', y='Density')
      p
      #ggplotly(p) #interactive plot not working currently
  })  
  
###The demand function plot----
output$demand_plot <- renderPlot({
    ppp_ken<-ppp();exp_ken<-exp();bs_ken<-bs();
    q_ken=exp_ken[,-1]/ppp_ken[,-1] #per capita consumption = real income 
    csort<-names(sort(colSums(q_ken),decreasing=TRUE))
    q_ken1<-q_ken[,match(csort,colnames(q_ken))]
    ppp_ken1<-ppp_ken[,match(csort,colnames(ppp_ken))]
    #
    i=1 #food
    logP_mat_food=NULL;logQ_mat_food=NULL;
    for (c in 1:ncol(ppp_ken1)){
      logp_mat_food=NULL;logq_mat_food=NULL; 
      for (d in 1:ncol(ppp_ken1)){
        logp_icd=log(ppp_ken1[i,c])-log(ppp_ken1[i,d])
        logp_mat_food=cbind(logp_mat_food,logp_icd);
        #
        logq_icd=log(q_ken1[i,c])-log(q_ken1[i,d])
        logq_mat_food=cbind(logq_mat_food,logq_icd);
      }
      logP_mat_food=rbind(logP_mat_food,logp_mat_food);
      logQ_mat_food=rbind(logQ_mat_food,logq_mat_food);
      #
    }
    colnames(logP_mat_food)=rownames(logP_mat_food)=colnames(logQ_mat_food)=rownames(logQ_mat_food)=colnames(ppp_ken1)
    #
    logP_mat_food<-logP_mat_food-logP_mat; logQ_mat_food<-logQ_mat_food-(1/2)*logQ_mat
    #plot data
    x=100*as.vector(logP_mat_food[upper.tri(logP_mat_food,diag=FALSE)]); #length(x)
    y=100*as.vector(logQ_mat_food[upper.tri(logQ_mat_food,diag=FALSE)]); #length(y)
    colorscale = scale_fill_gradientn(colours =  rev(rainbow(7)[-7]))
    xy=data.frame(cbind(x,y)); 
    # 
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
                       theme(text = element_text(size=20),
                             axis.title=element_text(size=20)
                             #axis.text = element_text(size=40)
                       ))
    
    #Brew a colorscale
    colorscale = scale_fill_gradientn(colours =  rev(rainbow(7)[-7]))
    #contour plot----
    p<-ggplot(data=xy,aes(x,y)) + 
      geom_point(shape = 16,size=1.5, color="blue", fill="blue") +
      stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',bins=40,n=40) + #, h = c(1, 1)
      scale_x_continuous(limits = c(-100,100)) +
      scale_y_continuous(limits = c(-200,300)) +
      colorscale + #coord_fixed() +
      #
      stat_smooth(method="lm", se=FALSE, color = 'darkred') +
      #geom_abline(slope=1,size=1,linetype=2,size=1.5,colour="darkred") +
      guides(alpha="none") +
      commonTheme +
      labs(x='Quantity', y='Prices')
    p
  })

  
###The P vs Q dispersion plot----
output$disp_plot <- renderPlot({
    ppp_ken<-ppp();exp_ken<-exp();bs_ken<-bs();
    ###Per capita real income ($)----
    q_ken=exp_ken[,-1]/ppp_ken[,-1] #per capita consumption = real income 
    #sort countries (column) by real income p.c
    csort<-names(sort(colSums(q_ken),decreasing=TRUE))
    q_ken1<-q_ken[,match(csort,colnames(q_ken))]
    ppp_ken1<-ppp_ken[,match(csort,colnames(ppp_ken))]
    bs_ken1<-bs_ken[,match(csort,colnames(bs_ken))]
    #[Important: Must use for loop here, not within reactive{}]
    ###The quantity dispersion matrix ----
    #compute the logExp_cd and sigma2_cd matrices
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
    #compute the logP_cd and sigma2_P_cd matrices
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
    #
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
                       theme(text = element_text(size=20),
                             axis.title=element_text(size=20)
                             #axis.text = element_text(size=40)
                       ))
    colorscale = scale_fill_gradientn(colours =  rev(rainbow(7)[-7]))
    #Contour plot----
    x=p_sd;y=q_sd;
    xy=data.frame(cbind(x,y));
    p<-ggplot(data=xy,aes(x,y)) + 
      geom_point(shape = 16,size=1.5, color="blue", fill="blue") +
      stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',bins=40,n=40) + #, h = c(1, 1)
      scale_x_continuous(limits = c(0, 150))+
      scale_y_continuous(limits = c(0, 150))+
      colorscale +
      coord_fixed() +
      geom_abline(slope=1,size=1,linetype=2,size=1.5,colour="darkred") +
      guides(alpha="none") +
      commonTheme +
      labs(x='Price Dispersion', y='Quantity Dispersion')
     p
  })
