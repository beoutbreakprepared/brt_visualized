############## Helper Functions for Tree Plotting Func ###############
#Order:
#1. g_legend: function extracts legend from ggplot object
#2. wrap_text: when strings have spaces puts on different lnes
#3. capitalize: capitalizes first letter of string
#4. decide_coords: decides the coordinates of where each row of the pretty tree table will be in the plot matrix
#5. create_tree_plots: creates a list of the individual plots that make up the tree as well as the arrow plots
#6. tree_plot_matrix: lays out the matrix for the tree plot based on the plots from create_tree_plots

#extract ggplot legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}


#create wrapper to fit long titles
wrap_text <- function(txt, width) {
  paste(strwrap(txt, width), collapse = "\n")
}


#create capitalization function
capitalize <- function(string){
  first_letter <- substr(string, 1, 1)
  rest <- substr(string, 2, nchar(string))
  fl_upper <- toupper(first_letter)
  capitalized <- paste0(fl_upper, rest)
  return(capitalized)
}

#function decides the coordinates of each plot within the matrix that lays out the tree

tp_coords <- function(tree){

  # define row names
  tree$row <- rownames(tree)
  tree <- as.data.table(tree)

  # remove missing nodes from tree table
  level_deciders <- tree[LeftNode != -1 | RightNode != -1 | MissingNode != -1,]
  tree[1, c('VisRow', 'VisCol') := .(1, 1)]

  # decide which 'row' node will be in based on preceding nodes
  for (i in 1:nrow(level_deciders)){

    LN <- level_deciders[i, LeftNode] +1
    RN <- level_deciders[i, RightNode] +1
    MN <- level_deciders[i, MissingNode] +1
    rowld <- level_deciders[i, row]
    VR <- tree[row == rowld, VisRow] + 1

    VC <- tree[row == rowld, VisCol]
    VCL <- VC + VC - 1
    VCR <- VC + VC

    tree[LN, c('VisRow', 'VisCol') := .(VR, VCL)]
    tree[RN, c('VisRow', 'VisCol') := .(VR, VCR)]
    tree[MN, c('VisRow', 'VisCol') := NA ]

  }

  return(tree)
}



#function that creates the list of plots populating the regression tree

create_tps <- function(tree, model_data, names, cat_var, cat_order=NULL, pretty_names=NULL, units=NULL, colors = NULL, exclude_outliers = TRUE, transform = NULL, transformations = NULL){
  require(ggplot2)
  require(data.table)
  require(grid)
  require(gridExtra)
  plot_list <- list()

  # define maximum rows and columns to define matrix area
  maxrow <- max(na.omit(tree$VisRow))
  maxcol <- maxrow^2
  laydim <- maxrow/maxcol

  # factor to define font size
  refont <- laydim * 120


  model_data <- as.data.frame(model_data)

  # if null define categorical levels for factoring later on
  if(!is.null(cat_order)){
    cats <- cat_order
  } else {
    cats <- unique(model_data[,cat_var])
  }


  # for loop to create a plot for each tree table row
  for(i in 1:nrow(tree)){

    # split variable
    SV <- unlist(tree[i, 'SplitVar'] + 1)

    # if it's not a missing or terminal node...
    if(SV >0){

      # define split variable, error reduction, unit
      SCP <- as.data.frame(tree[i, 'SplitCodePred'])
      ER <- unlist(tree[i, 'ErrorReduction'])
      names(SCP) <- 'SCP'
      var <- as.character(names[SV])
      xlab <- units[SV]

      plot_data <- model_data


      # only do if not the first tree row (recursively partitioning the data...)
      if (i != 1){

        # set up lists that have preceding split variables, split value and right/left parentage
        pSV_list <- c()
        pSCP_list <- c()
        pRL_list <- c()
        prow <- tree[i, row]


        # until reaching last node find parent nodes and their associated information, then put in list
        while(length(prow) > 0){
          lrow <- grep(paste0('^', prow, '$'), tree$LeftNode)
          rrow <- grep(paste0('^', prow, '$'), tree$RightNode)
          if(length(lrow) >0 ){
            pSV <- unlist(tree[lrow, 'SplitVar'])
            pSCP <- unlist(tree[lrow, 'SplitCodePred'])
            pSV_list <- append(pSV_list, pSV)
            pSCP_list <- append(pSCP_list, pSCP)
            pRL_list <- append(pRL_list, 'L')
            prow <- lrow -1
          } else if (length(rrow) > 0){
            pSV <- unlist(tree[rrow, 'SplitVar'])
            pSCP <- unlist(tree[rrow, 'SplitCodePred'])
            pSV_list <- append(pSV_list, pSV)
            pSCP_list <- append(pSCP_list, pSCP)
            pRL_list <- append(pRL_list, 'R')
            prow <- rrow -1
          } else {
            prow <- NULL
          }
        }


        # split the data on each split/variable pair in the list
        for(p in 1:length(pSV_list)){

          psplit <- pSCP_list[p]
          pvar <- names[pSV_list[p]+1]
          if(pRL_list[p] == 'R'){

            # if parent is right split above split value
            plot_data <- plot_data[plot_data[,pvar] > psplit,]
          } else {
            # if parent is left split below split value
            plot_data <- plot_data[plot_data[,pvar] <= psplit,]
          }


        }

      }

      # transform the data if variable in transform list
      if(var %in% transform){
        vind <- which(transform == var)

        fun <- function(x){
          eval(parse(text = transformations[vind]))
        }

        plot_data[,var] <- fun(plot_data[,var])

        SCP$SCP <- fun(SCP$SCP)
      }

      # use interquartile range to exclude outliers
      if(exclude_outliers){
        quant <- quantile(plot_data[,var])
        IQR <- (quant[4]-quant[2])*2
        u_diff <- quant[5] - quant[4]
        l_diff <- quant[2] - quant[1]
        if(u_diff > IQR){
          p90 <- quantile(plot_data[,var], 0.9)
          plot_data <- plot_data[plot_data[,var] <= p90,]
          max <- round(p90, 2)
        } else{
          max <- round(max(plot_data[,var]), 2)
        }

        if(l_diff > IQR){
          p10 <- quantile(plot_data[,var], 0.1)
          plot_data <- plot_data[plot_data[,var] >= p10,]
          min <- round(p10, 2)
        } else {
          min <- round(min(plot_data[,var]), 2)
        }

        if(min < 0.1 & min > -0.1){
          min <- 0
        }
      } else {
        max <- round(max(plot_data[,var]), 2)
        min <- round(min(plot_data[,var]), 2)

      }

      # create split text
      split <- paste0("Split = ", round(SCP$SCP, 2))
      len <- nchar(split)

      # create ER text
      ER_text <- paste0(split , "\nER = " ,round(ER, digits=2))

      # define text size based on which row plot will be in
      if(tree[i, VisRow] == 6){
        w <- 0.029 *len
        ER_inset <- grobTree(roundrectGrob(gp = gpar(fill = 'lightgrey', alpha = 0.6, col = 'gray30'),
                                           x = 0.6, y = 0.725, just = c('left','bottom'), width = w, height = .26/1.7),
                             textGrob(ER_text, x=0.63, y=0.76, just = c('left','bottom'), hjust=0,
                                      gp=gpar(fontsize=refont/2, fontface="bold", col="gray28")))
        tsize <- 1.8
      } else {
        w <- 0.029 *len

        ER_inset <- grobTree(roundrectGrob(gp = gpar(fill = 'lightgrey', alpha = 0.6, col = 'gray30'),
                                           x = 0.6, y = 0.725, just = c('left','bottom'), width = w, height = .26),
                             textGrob(ER_text, x=0.63, y=0.76, just = c('left','bottom'), hjust=0,
                                      gp=gpar(fontsize=refont, fontface="bold", col="gray28")))
        tsize <- 2.6
      }



      #ER_df <- data.frame(x = 0.65, y = 0.8, text = paste0("Split = ", round(SCP$SCP, 2), "\nER = " ,round(ER, digits=2)))

      # define title using pretty_names
      if(!is.null(pretty_names)){
        title <- as.character(pretty_names[SV])
      } else{
        title <- var
      }


      # create the plot using histogram of partitioned values and factor based on categories
      plot <- ggplot() +
        geom_histogram(data = plot_data, aes(x = get(var), fill = factor(get(cat_var), levels=cats)), bins = 50, color = 'black', show.legend = FALSE)+
        scale_x_continuous(expand = c(0,0), labels = c(min, "", max), breaks = c(min, SCP$SCP, max))+
        scale_y_continuous(expand = c(0,0)) +
        geom_vline(data = SCP, aes(xintercept = SCP), size = 1.5) +
        scale_fill_manual(name = NULL, values = colors) +
        ggtitle(title)+
        labs(x = xlab, y = 'Count')+
        theme(plot.title=element_text(size=rel(tsize), margin = margin(b = 8)),
              axis.title.x=element_text(size=rel(2) , margin = margin(r = 25, l = 0, t = 0, b= 0)),
              axis.title.y=element_text(size=rel(2), margin = margin(t = 4)),
              axis.text.x=element_text(size= 20, face = 'plain'),
              axis.text.y=element_text(size=18),
              axis.ticks.x = element_line(size = c(.5, 1.5, .5)),
              axis.ticks.length = unit(.2, 'cm'),
              axis.line = element_line(colour = 'black'),
              panel.background = element_rect(fill="white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.margin = unit(c(2,2,2,2),"points")) +
              #geom_label(data = ER_df, aes(x=x, y=y, label = text), size = 10, color = 'gray28', fill = 'lightgrey', alpha = 0.2, fontface = 'bold')
              annotation_custom(ER_inset)

      # define row of plot in tree
      row <- tree[i, row]
      plt_name <- as.character(row)

      # grob the plot to create a static image
      plot_grobbed <- ggplotGrob(plot)
      plot_list[[plt_name]] <- plot_grobbed

      # create arrow plots to point to children
      if(tree[i, VisRow] < 6){
        la_plot <- ggplot() +
          geom_segment(aes(x=1, y=1, xend = 0, yend = 0), arrow = arrow()) +
          theme(panel.background = element_blank(),
                axis.line = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank())

        ra_plot <- ggplot() +
          geom_segment(aes(x=0, y=1, xend = 1, yend = 0), arrow = arrow()) +
          theme(panel.background = element_blank(),
                axis.line = element_blank(),
                axis.title = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank())


        # name arrow plots
        ra_name <- paste0('ra_', row)
        la_name <- paste0('la_', row)

        # add to plot list
        plot_list[[la_name]] <- la_plot
        plot_list[[ra_name]] <- ra_plot
      }






    } else if (SV == 0){

      # create a pie chart if the split variable indicates a terminal node
      row_id <- tree[i, row]

      L_row <- grep(paste0('^', row_id, '$'), tree$LeftNode)

      R_row <- grep(paste0('^', row_id, '$'), tree$RightNode)

      split_row <- integer(0)

      # find if it is terminal left or right to define where to partition data
      if(length(L_row) != 0){
        split_row <- L_row
        aorb <- 'below'
      } else if (length(R_row != 0)){
        split_row <- R_row
        aorb <- 'above'
      }



      # if not a missing node...
      if(length(split_row) != 0){

        # define split variable, split value and unit for plot
        rep_SV <- unlist(tree[split_row, 'SplitVar']) + 1
        SCP <- as.data.frame(tree[split_row, 'SplitCodePred'])
        names(SCP) <- 'SCP'
        var <- as.character(names[rep_SV])
        xlab <- units[rep_SV]
        var_ind <- which(names(model_data) == var)


        plot_data <- model_data

        # recursively partition data based on parent nodes (same process as above)
        if (i != 1){
          pSV_list <- c()
          pSCP_list <- c()
          pRL_list <- c()
          prow <- tree[i, row]

          while(length(prow) > 0){
            lrow <- grep(paste0('^', prow, '$'), tree$LeftNode)
            rrow <- grep(paste0('^', prow, '$'), tree$RightNode)
            if(length(lrow) >0 ){
              pSV <- unlist(tree[lrow, 'SplitVar'])
              pSCP <- unlist(tree[lrow, 'SplitCodePred'])
              pSV_list <- append(pSV_list, pSV)
              pSCP_list <- append(pSCP_list, pSCP)
              pRL_list <- append(pRL_list, 'L')
              prow <- lrow -1
            } else if (length(rrow) > 0){
              pSV <- unlist(tree[rrow, 'SplitVar'])
              pSCP <- unlist(tree[rrow, 'SplitCodePred'])
              pSV_list <- append(pSV_list, pSV)
              pSCP_list <- append(pSCP_list, pSCP)
              pRL_list <- append(pRL_list, 'R')
              prow <- rrow -1
            } else {
              prow <- NULL
            }
          }


          if(length(pSV_list) >1 ){
            # don't use initial split that will be done later...
            for (p in 2:(length(pSV_list))){


              psplit <- pSCP_list[p]

              pvar <- names[pSV_list[p]+1]


              if(pRL_list[p] == 'R'){
                plot_data <- plot_data[plot_data[,pvar] > psplit,]
              } else {
                plot_data <- plot_data[plot_data[,pvar] < psplit,]
              }


            }
          }



        }


        # split above or below based on predefined var
        if(aorb == 'above'){
          plot_data <- plot_data[plot_data[,var] > SCP$SCP,]
        } else {
          plot_data <- plot_data[plot_data[,var] < SCP$SCP,]
        }



        plot_data <- as.data.table(plot_data)

        # group by categorical variable and create percentage value based on number of rows
        pie_df <- plot_data[, (.N/(plot_data[,.N]))*100, by = get(cat_var)]


        # plot pie chart
        plot <- ggplot() +
          geom_bar(dat = pie_df, aes(x='', y =V1, fill= factor(get, levels = cat_order)), color = 'black', width = 1, stat = 'identity')+
          scale_fill_manual(values = colors) +
          coord_polar("y") +
          labs(caption = paste('n =', nrow(plot_data)))+
          theme(panel.background = element_rect(fill="white"),
                plot.caption =  element_text(hjust = 0.5, margin = margin(t = 0, b = 15), size = 26, face = 'bold'),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.margin = unit(c(2,2,2,2),"points"),
                axis.title = element_blank(),
                axis.text = element_blank(),
                legend.position = 'none',
                axis.ticks = element_blank())
        plt_name <- as.character(tree[i,row])
        # place plot in list
        plot_list[[plt_name]] <- plot


      } else {

      }


    }
  }
  return(plot_list)
}


#function to create the matrix that decides locations of plots within the tree

tp_matrix <- function(tree, plot_list){

  # define all max rows and columns to find span of tree
  max_row <- max(na.omit(tree$VisRow))

  max_col <- 2^max_row

  mat_layout <- matrix(ncol = max_col, nrow = 0)

  row_names <- c()

  # create matrix layout by row with fewer plots in top rows
  for(val in 1:(max_row)){
    valsq <- 2^(val-1)
    vect <- c()
    fact <- max_col/valsq
    for(i in 1:fact){
      vect <- append(vect, seq(1, valsq ,1))
    }
    vect <- sort(vect)
    mat_layout <- rbind(mat_layout, vect)
    row_name <- paste0('row_', val)
    row_names <- append(row_names, row_name)

  }

  # define rownames to reference later
  rownames(mat_layout) <- row_names

  # for each row find the plot number
  for(n in 1:nrow(mat_layout)){


    if(n != 1){
      pobs <- 2^(n-1)
    }else {
      pobs <- 1
    }



    its <- max_col/pobs



    if(its > 4){
      half <- its/2



      low <- half - 2
      high <- half + 3
      min <- 1
      max <- its
      na_range <- c()
      while(max <= max_col){
        na_range <- append(na_range, c(min:low, high:max))
        high <- high + its
        low <- low +its
        min <- min + its
        max <- max + its
      }

      mat_layout[n, na_range] <- NA
    }





  }

  mat_new <- mat_layout


  # based on rows in trees find where there will be no plot
  for(mrow in 1:nrow(mat_layout)){

    for (obs in unique(mat_layout[mrow,])){
      if(!is.na(obs)){

        tree_row <- tree[VisCol == obs & VisRow == mrow, row]

        if(length(tree_row) > 0){
          ind <- which(names(plot_list) == as.character(tree_row))

          mat_new[mrow,which(mat_layout[mrow,] == obs)] <- ind

        } else {
          mat_new[mrow,which(mat_layout[mrow,] == obs)] <- NA
        }
      }
    }
  }


  #repeat plot rows and add arrow rows
  for(name in row_names){

    row_num <- which(row_names == name)

    cur_rns <- rownames(mat_new)
    top_row <- which(cur_rns == name)
    if(row_num != length(row_names)){
      bot_row <- top_row + 1
      top <- mat_new[1:top_row,]
      bottom <- mat_new[bot_row:nrow(mat_new),]
      new_row <- matrix(ncol = max_col, nrow = 1)
      AR_name <- paste0('AR_', row_num)
      rownames(new_row) <- AR_name
      mat_new <- rbind(top, new_row, bottom)
      rownames(mat_new)[1] <- 'row_1'
      rownames(mat_new)[nrow(mat_new)] <- row_names[max_row]



      for(p in unique(mat_new[name,])){
        if(!is.na(p)){

          if(grepl('la', names(plot_list)[p+1])){
            where <- which(mat_new[name,] == p)

            to_add <- (2 ^ (max_row - row_num - 1))/2
            if(to_add < 1){
              to_add <- 1
            }

            la_spots <- c((where[2] - to_add + 1) : where[2])
            ra_spots <- c(where[3] : (where[3] + to_add - 1))
            mat_new[AR_name, la_spots] <- p + 1
            mat_new[AR_name, ra_spots] <- p + 2
          }

        }
      }
    }
  }







  # repeat rows
  for(name in row_names){
    for(int in 1:2){

      cur_rns <- rownames(mat_new)
      top_row <- which(cur_rns == name)

      if(grepl('row', name)){
        bot_row <- top_row + 1

        top <- mat_new[1:top_row,]

        if(bot_row <= nrow(mat_new)){
          bottom <- mat_new[bot_row:nrow(mat_new),]
          new_row <- mat_new[top_row,]
          mat_new <- rbind(top, new_row, bottom)
          rownames(mat_new)[1] <- 'row_1'
          rownames(mat_new)[bot_row] <- paste0('dupe_row')
          rownames(mat_new)[nrow(mat_new)] <- row_names[max_row]
        } else {

          new_row <- mat_new[top_row,]
          mat_new <- rbind(top, new_row)
          rownames(mat_new)[1] <- 'row_1'
          rownames(mat_new)[bot_row] <- paste0('dup_', bot_row)

        }

      }
    }
  }



  return(mat_new)
}












