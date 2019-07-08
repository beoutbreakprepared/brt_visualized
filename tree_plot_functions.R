############ Tree Plotting Function #############

##Function Arguments##

#model             requires a gbm model type output
#tree_num          the number of tree you wish to visualize, if NULL will choose a random tree ex. 4
#model_data        the data used to train your model including the categorical variable - names must match those stored in the gbm model
#cat_var           the name of the column containing the categorical variable
#cat_order         a string containing the unique categories in the order you wish them to be displayed - if NULL will display in alphabetical order ex. c('Present', 'Absent')
#pretty_names      a string containing the long names you wish to use as the title of the plots - if NULL will use the column names provided in the data ex. c('Elevation', 'Precipitation', ....)
#units             a string containing the unit names in the same order as the model data variables
#colors            a string of color codes matching the number of categories you wish to have displayed - to ensure colors are matched with appropriate variable order colors the same as categories or specify which color goes to which category ex. c('price' = '#9b0065', 'size' = '#CBE0CF')
#exclude_outliers  binary T/F if TRUE (default) full range of data will always be used for the histogram - if not data with outliers will show 10th or 90th percentile depending on where the outliers are
#transform         a string of variables from the test set you wish to transform for visualization ex. ('price', 'size')
#transformation    a string of the transformations you wish to perform in the same order as the vars in transform ex. ('log(x)', 'x * 50')
#save              binary T/F if TRUE will save the plot (this is the default) - if false won't save and will just return the object (this will look crumby in the plot viewer, must be saved width = 30, height = 48, units = 'in')
#file_name         where you want the file saved + the name of the file should be saved as .png ex. '~/folder/tree.png'

source('tree_helpers.R')

tree_plot <- function(model, tree_num, model_data, cat_var = NULL, cat_order = NULL, pretty_names = NULL, units = NULL, colors = NULL, exclude_outliers = TRUE, transform = NULL, transformation = NULL, save = TRUE, file_name = NULL){

  require(gbm)
  require(ggplot2)
  require(data.table)
  require(grid)
  require(gridExtra)

  model_data <- as.data.frame(model_data)
  # ensure model is the appropriate class
  if(class(model) != 'gbm'){
    stop('model provided is not of class "gbm" please provide a gbm model object')
  }

  # create pretty tree
  tree_tab <- pretty.gbm.tree(model, i.tree = tree_num)
  # define variable names based on model
  var_names <- append(model$var.names, cat_var)


  # ensure all variables are in model_data
  for(name in var_names){
    if(!name %in% colnames(model_data)){
      stop(paste0('variable names provided by model do not match model_data column names:', name, 'is missing from model_data'))
    }
  }

  # ensure pretty_names and var_names match
  if(length(pretty_names) != length(var_names) - 1){
    stop('length of pretty names does not match the number of variables used in the model')
  }
  # ensure units and var_names match
  if(length(units) != length(var_names) - 1){
    stop('length of units does not match the number of variables used in the model')
  }

  # ensure cat_order encompasses all cats
  if(length(cat_order) != length(unique(model_data[,cat_var]))){
    stop('length of cat_order does not match the number of categories used in the model')
  }

  # define file name
  if(save){
    if(is.null(file_name)){
      stop('you have not supplied a file_name, but save = TRUE, saving not possible')
    }
  }



  # cut down to relevant columns
  model_data <- model_data[,var_names]

  #define plot coordinates for each row in tree
  tree_fixed <- tp_coords(tree_tab)

  maxrow <- max(na.omit(tree_fixed$VisRow))

  # can't make plot larger than 6 rows
  if(maxrow > 6 ){
    message(paste('tree is too large to plot entirely, plotting first 6 of', maxrow, 'levels - to view the full tree try the sankey tree method'))
    tree_fixed <- tree_fixed[which(tree_fixed$VisRow < 7 | is.na(tree_fixed$VisRow)), ]

    if(maxrow == 7){
      text <- '*Level 7 is not shown*'

    } else {
      text <- paste0('*Levels 7:', maxrow, ' are not shown*')
    }

    bt <- textGrob(text, gp = gpar(fontsize = 5 * maxrow, fontface = 'bold'), hjust = 0.5,  x = .5)
  } else{
    bt <- NA
  }


  # create tree plot list
  tree_plots <- create_tps(tree = tree_fixed, model_data = model_data, names = var_names, cat_var = cat_var, cat_order = cat_order, pretty_names = pretty_names, units = units, colors = colors, exclude_outliers = exclude_outliers, transform = transform, transformations = transformation)
  # create plot layout matrix
  layout <- tp_matrix(tree_fixed, tree_plots)

  # define cat order
  model_data <- as.data.frame(model_data)
  if(is.null(cat_order)){
    cat_order <- unique(model_data[,cat_var])
  }

  # create false plot for legend
  tl <- suppressMessages(ggplot() +
    geom_histogram(data = model_data, aes(x=model_data[,1], fill = factor(get(cat_var), levels=cat_order))) +
    scale_fill_manual(name = NULL, values = colors) +
    theme(legend.key.size = unit(.7, 'in'),
          legend.text = element_text(size = 24),
          legend.spacing.y = unit(.4, 'in'),
          axis.title.x = element_text(margin = margin(r = 0, l = 10))))


  # get legend
  tleg <- suppressMessages(g_legend(tl))


  # add legend to tree plots
  tree_plots[[(length(tree_plots) + 1)]] <- tleg


  # find spot for legend
  fp_loc <- max(which(layout[1,] == 1))



  f <- fp_loc + 1
  t <- fp_loc + 2


  layout[1:2, f:t] <- (length(tree_plots))

  #return(list(tree_plots, layout))

  # arrange plots
  pretty_tree <- arrangeGrob(grobs = tree_plots, layout_matrix = layout, bottom = bt)
  # save
  if(save){
    message(paste0('saving a ', nrow(layout) * 1.5, ' x ', ncol(layout)*1.5), ' inch image')
    ggsave(pretty_tree, filename = file_name, width = ncol(layout)*1.5, height = nrow(layout) * 1.5, units = 'in', limitsize = FALSE)

  } else{
    warning(paste0('you have chosen not to save the plot, the tree plot will look terrible in the R plot viewer, you should save it using ggsave() where width = ', ncol(layout)*1.5, ' and height = ', nrow(layout)*1.5, ', units = "in" or the plots will be too small to see.'))
    return(pretty_tree)
  }



}



######## FUNCTION ARGUMENTS FOR SANKEY TREE FUNCTION #######


#model             requires a gbm model type output
#ntree             the number of tree you wish to visualize, if NULL will choose a random tree ex. 4
#model_data        the data used to train your model including the categorical variable - names must match those stored in the gbm model
#cat_var           the name of the column containing the categorical variable
#pretty_names      a string containing the long names you wish to use as the title of the plots must be in the same order as the model variables - if NULL will use the column names provided in the data ex. c('Elevation', 'Precipitation', ....)
#cat_colors        a string or data frame of HEX codes to color the categorical variable - if you want to ascribe certain colors to certain categorical values provide a data frame 'cat' and 'color'
#pred_colors      a string or data frame of HEX codes to color the predictor variables - if you want to ascribe certain colors to certain predictors provide a data frame with col names 'pred' -> should be pretty_names if using and 'color'
#labs              default is 'simple' - will label nodes with predictor variable names, can also be set to FALSE for no labels (except for the categorical variables to act as a legend)
#lab_pos           default is 'right' - alternative is 'left' will place labels to the right or left of the node



sankey_tree <- function(model, ntree, model_data, cat_var, pretty_names, cat_colors, pred_colors, labs = 'simple', lab_pos = 'right'){

  require(gbm)
  require(ggplot2)
  require(data.table)
  require(networkD3)
  require(htmlwidgets)

  # ensure model is of correct class
  if(class(model) != 'gbm'){
    stop('model provided is not of class "gbm" please provide a gbm model object')
  }

  # define var_names
  var_names <- append(model$var.names, cat_var)

  # ensure no missing model_data rows
  missing_names <- which(!model$var.names %in% names(model_data))
  if(length(missing_names) > 0){
    stop(paste0('variable names provided by model do not match model_data column names: ', model$var.names[missing_names], ' missing from model_data'))
  }


  # ensure pretty_names same length as var_names
  if(length(pretty_names) != length(var_names) - 1){
    stop('length of pretty names does not match the number of variables used in the model')
  }

  # crop model_data to necessary variables
  model_data <- as.data.frame(model_data)
  model_data <- model_data[,var_names]

  # create pretty tree and define plot coordinates
  tree <- pretty.gbm.tree(model, i.tree = ntree)
  tf <- tp_coords(tree)
  # sankey tree creation
  # create tree reference table excluding missing nodes
  tree_links_ref <- tf[which(!is.na(VisRow)),]

  # define pretty_names if missing
  if(is.null(pretty_names)){
    pretty_names <- model$var.names
  }

  # add Terminal Node
  pretty_names <- append(pretty_names, 'Terminal Node')
  tree_links_ref$SplitVar[which(tree_links_ref$SplitVar == -1)] <- length(pretty_names) -1
  tree_links_ref$SplitName <- pretty_names[tree_links_ref$SplitVar +1]

  # define categories
  cats <- unique(model_data[,cat_var])

  # create empty tree nodes & tree links dataframes with categories only
  tree_nodes <- data.frame(SplitName = cats,
                           row = seq(-1, length(cats) * -1))
  tree_nodes <- rbind(tree_nodes, tree_links_ref[,c('SplitName', 'row')])
  tree_nodes$index <- seq(0, nrow(tree_nodes) - 1)
  tree_links_ref <- tree_links_ref[which(tree_links_ref$SplitName != 'Terminal Node'),]
  tree_links <- data.frame(SourceID = integer(0), TargetID = integer(0), SplitName = character(0), VisRow = integer(0),
                           Val = integer(0), AorB = character(0), cat = character(0), stringsAsFactors = FALSE)

  vals <- c()
  model_data <- as.data.table(model_data)
  # for each category find nrows
  for(cat in cats){
    v <- nrow(model_data[get(cat_var) == cat,])
    vals <- append(vals, v)
  }

  # initial tree_links just cat links
  cat_links <- data.frame(SourceID = seq(-1, length(cats) * -1), TargetID = c(0, 0),
                         SplitName = cats, VisRow = seq(-1, length(cats) * -1),
                         Val = vals, AorB = NA,
                         cat = cats, stringsAsFactors = FALSE)
  tree_links <- rbind(tree_links, cat_links)

  tree_links_temp <- data.frame(SourceID = integer(1), TargetID = integer(1), SplitName = character(1), VisRow = integer(1),
                           Val = integer(1), AorB = character(1), cat = character(1), stringsAsFactors = FALSE)

  # find each link for above and below split and by each category and define counts with recursively splitting data (same method as before)
  for(r in 1:nrow(tree_links_ref)){
    plot_data <- model_data


    pSV_list <- c()
    pSCP_list <- c()
    pRL_list <- c()
    prow <- tree_links_ref[r, row]


    if(prow > 0){
      while(length(prow) > 0){
      lrow <- grep(paste0('^', prow, '$'), tree_links_ref$LeftNode)
      rrow <- grep(paste0('^', prow, '$'), tree_links_ref$RightNode)
      if(length(lrow) >0 ){
        pSV <- unlist(tree_links_ref[lrow, 'SplitVar'])
        pSCP <- unlist(tree_links_ref[lrow, 'SplitCodePred'])
        pSV_list <- append(pSV_list, pSV)
        pSCP_list <- append(pSCP_list, pSCP)
        pRL_list <- append(pRL_list, 'L')
        prow <- tree_links_ref[lrow, row]
      } else if (length(rrow) > 0){
        pSV <- unlist(tree_links_ref[rrow, 'SplitVar'])
        pSCP <- unlist(tree_links_ref[rrow, 'SplitCodePred'])
        pSV_list <- append(pSV_list, pSV)
        pSCP_list <- append(pSCP_list, pSCP)
        pRL_list <- append(pRL_list, 'R')
        prow <- tree_links_ref[rrow, row]
      } else {
        prow <- NULL
      }
    }

    if(length(pSV_list) >= 1){
      for (p in 1:length(pSV_list)){

        psplit <- pSCP_list[p]

        pvar <- var_names[pSV_list[p]+1]

        if(pRL_list[p] == 'R'){
          plot_data <- plot_data[get(pvar) > psplit,]
        } else {
          plot_data <- plot_data[get(pvar) < psplit,]
        }


      }
    }
    }




    tree_links_temp$SourceID <- tree_links_ref$row[r]
    tree_links_temp$SplitName <- as.character(tree_links_ref$SplitName[r])
    tree_links_temp$VisRow <- tree_links_ref$VisRow[r]
    SCP <- tree_links_ref$SplitCodePred[r]
    cn <- as.character(model$var.names[tree_links_ref$SplitVar[r] + 1])

    for(cat in cats){
      for(s in 1:2){
        if(s == 1){
          tree_links_temp$AorB <- as.character('Above')
          tree_links_temp$cat <- cat
          tree_links_temp$TargetID <- tree_links_ref$RightNode[r]

          tree_links_temp$Val <- nrow(plot_data[get(cn) > SCP & get(cat_var) == cat,])
        }
        if(s == 2){
          tree_links_temp$AorB <- as.character('Below')
          tree_links_temp$cat <- cat
          tree_links_temp$TargetID <- tree_links_ref$LeftNode[r]

          tree_links_temp$Val <- nrow(plot_data[get(cn) < SCP & get(cat_var) == cat,])
        }

        tree_links <- rbind(tree_links, tree_links_temp)
      }
    }

  }

  # merge tree node info to tree_links to define source & target
  tree_links <- merge(tree_links, tree_nodes, by.x = c('SplitName', 'SourceID'), by.y = c('SplitName', 'row'), all.x = TRUE)
  names(tree_links)[length(tree_links)] <- 'Source'
  tree_links <- merge(tree_links, tree_nodes, by.x = 'TargetID', by.y =  'row', all.x=TRUE)
  names(tree_links)[2] <- 'SplitName'
  tree_links$SplitName.y <- NULL
  names(tree_links)[length(tree_links)] <- 'Target'


  # define color scales
  if(!is.null(pred_colors)){
    if(class(pred_colors) != 'data.frame'){
      pred_colors <- data.frame(pred = unique(tree_nodes$SplitName), color = pred_colors)
    }
    if(class(cat_colors) != 'data.frame'){
      cat_colors <- data.frame(pred = cats, color = cat_colors)
    } else {
      nomcat <- which(names(cat_colors) == 'cat')
      names(cat_colors)[nomcat] <- 'pred'
    }
    pred_colors <- rbind(cat_colors, pred_colors)
    pred_colors <- cbind(pred_colors, as.character(seq(1:nrow(pred_colors))))
    names(pred_colors)[3] <- 'NG'
    tree_nodes <- merge(tree_nodes, pred_colors, by.x = 'SplitName', by.y = 'pred', all.x = TRUE)
    tree_nodes <- tree_nodes[order(NG),]
    cols <- as.character(na.omit(unique(tree_nodes$color)))
    NGs <- as.character(na.omit(unique(tree_nodes$NG)))

    cols <- append(as.character(cat_colors$color), cols)
    domain <- append(as.character(cat_colors$pred), NGs)

    col_scale <-paste0('d3.scaleOrdinal()
                        .domain([', paste(paste0('"', domain, '"'), collapse = ', '), '])
                        .range([', paste(paste0('"', cols, '"'), collapse = ', '), '])')


  } else{
    if(class(cat_colors) != 'data.frame'){
      cat_colors <- data.frame(cat = cats, color = cat_colors)
    }
    suppressWarnings(tree_nodes$NG[1:length(cats)] <- as.character(seq(1, length(cats))))
    suppressWarnings(tree_nodes$NG[length(cats) + 1:nrow(tree_links)] <- as.character(length(cats) + 1))
    tree_nodes$SplitName <- as.character(tree_nodes$SplitName)

    cols <- append(as.character(cat_colors$color), as.character(cat_colors$color))
    cols <- append(cols, "#383838")

    NGs <- na.omit(unique(tree_nodes$NG))
    domain <- append(as.character(cat_colors$cat), NGs)

    col_scale <-paste0('d3.scaleOrdinal()
                        .domain([', paste(paste0('"', domain, '"'), collapse = ', '), '])
                        .range([', paste(paste0('"', cols, '"'), collapse = ', '), '])')

  }

  if(isFALSE(labs)){
    tree_nodes[!SplitName %in% cats, SplitName := NA]
  }


  w <- max(na.omit(tf$VisRow)) * 350




  tree_nodes <- tree_nodes[order(index),]

  tree_sankey <- sankeyNetwork(Links = tree_links, Nodes = tree_nodes,
                               Source = "Source", Target = "Target",
                               Value = "Val", NodeID = "SplitName",
                               sinksRight=FALSE,
                               LinkGroup = "cat", NodeGroup = "NG", colourScale = col_scale,
                               fontSize = 14, height = 400, width = w, nodePadding = 40)

  # define JS render_text to move node labels
  if(lab_pos == 'right'){
    render_text <- '
                        function(el,x) {

                            d3.select(el)
                            .selectAll(".node text");

                            var boxScaleFactor = 1.15;
                            d3.selectAll(".node")
                            .each(function (d) {
                            const node = d3.select(this);
                            const text = node.select("text");
                            const bbox = text.node().getBBox();

                            node.insert("rect", "text")
                            .attr("x", bbox.x)
                            .attr("y", bbox.y)
                            .attr("width", bbox.width * boxScaleFactor)
                            .attr("height", bbox.height * boxScaleFactor)
                            .attr("text-anchor", "middle")
                            .attr("rx", "3px") // corner rounding
                            .attr("fill", "white") // rectangle fill
                            .attr("opacity", 0.5)
                            .attr("stroke-width", "1.5px") // rectangle line thickness
                            .attr("stroke", "rgb(56, 56, 56)"); // rectangle line color

                            text.attr("transform", `translate(${bbox.width * (boxScaleFactor - 1) / 2},${bbox.height * (boxScaleFactor - 1) / 2})`)
                            });

                            }'
  } else if(lab_pos == 'left'){
   render_text <-  '
                        function(el,x) {

                            d3.select(el)
                            .selectAll(".node text")
                            .attr("x", x.options.nodeWidth - 45)
                            .attr("text-anchor", "end");

                            var boxScaleFactor = 1.15;
                            d3.selectAll(".node")
                            .each(function (d) {
                            const node = d3.select(this);
                            const text = node.select("text");
                            const bbox = text.node().getBBox();

                            node.insert("rect", "text")
                            .attr("x", bbox.x)
                            .attr("y", bbox.y)
                            .attr("width", bbox.width * boxScaleFactor)
                            .attr("height", bbox.height * boxScaleFactor)
                            .attr("text-anchor", "middle")
                            .attr("rx", "3px") // corner rounding
                            .attr("fill", "white") // rectangle fill
                            .attr("opacity", 0.5)
                            .attr("stroke-width", "1.5px") // rectangle line thickness
                            .attr("stroke", "rgb(56, 56, 56)"); // rectangle line color

                            text.attr("transform", `translate(${bbox.width * (boxScaleFactor - 1) / 2},${bbox.height * (boxScaleFactor - 1) / 2})`)
                            });

                            }'
  }

  if(labs == 'simple'){
    tree_sankey <- onRender(tree_sankey, render_text)
  }


  return(tree_sankey)

}


######## FUNCTION ARGUMENTS FOR AGGREGATE SPLIT SANKEY FUNCTION #######

#model             requires a gbm model type output
#pretty_names      a string containing the long names you wish to use as the title
#                     of the plots must be in the same order as the model variables
#                     - if NULL will use the column names provided in the data ex.
#                     c('Elevation', 'Precipitation', ....)
#pred_colors       a string or data frame of HEX codes to color the predictor variables
#                     - if you want to ascribe certain colors to certain
#                     predictors provide a data frame with col names 'pred' ->
#                     should be pretty_names if using and 'color'
#                     if FALSE then all nodes will be grey, if NULL random colors assigned
#incl_tn           T/F include splits that end in a terminal node in the count
#labs              opts: 'half' -> labels on split node, no labels on end node
#                     (best when nodes are color coded),
#                     'full' -> labels on both sides,
#                      FALSE -> no labels

aggregate_split_sankey <- function(model, pretty_names = NULL, pred_colors = NULL, incl_tn = TRUE, labs = 'half'){

  require(gbm)
  require(data.table)
  require(htmlwidgets)
  require(networkD3)
  require(parallel)
  require(foreach)
  require(doParallel)

  if(class(model) != 'gbm'){
    stop('model provided is not of class "gbm" please provide a gbm model object')
  }

  nvar <- length(model$var.names)

  if(is.null(pretty_names)){
    pretty_names <- model$var.names
  }

  numCores <- detectCores()
  registerDoParallel(numCores)

  end_dat <- foreach(t = 1:model$n.trees, .combine = rbind) %dopar% {
    if(t == round(model$n.trees/4)){
      print('25%')
    } else if(t == round(model$n.trees/2)){
      print('50%')
    } else if(t == round((model$n.trees/4)*3)){
      print('75%')
    }

    tree <- pretty.gbm.tree(model, i.tree = t)
    tree <- as.data.table(tree)
    tree[SplitVar != -1, 'LeftChild' := tree[(LeftNode + 1),SplitVar]]
    tree[SplitVar != -1, 'RightChild' := tree[(RightNode + 1),SplitVar]]
    ln <- tree[SplitVar != -1, .N, by = .(SplitVar, LeftChild)]
    names(ln) <- c('Cov', 'CovPrec', 'Freq')
    rn <- tree[SplitVar != -1, .N, by = .(SplitVar, RightChild)]
    names(rn) <- c('Cov', 'CovPrec', 'Freq')
    tab <- rbind(ln, rn)

  }
  stopImplicitCluster()
  end_dat <- end_dat[,sum(Freq), by = .(Cov, CovPrec)]
  names(end_dat) <- c('Cov', 'CovPrec', 'Freq')

  if(incl_tn){

    end_dat[CovPrec == -1, CovPrec := (nvar *2)]
    bnodes <- data.table(name = append(pretty_names, append(pretty_names, 'Terminal Node')),
                         index = seq(0, nvar*2))
    pretty_names <- append(pretty_names, 'Terminal Node')

  } else{

    end_dat <- end_dat[CovPrec != -1,]
    bnodes <- data.table(name = append(pretty_names, pretty_names),
                         index = seq(0, (nvar*2)-1))
  }

  end_dat[CovPrec != (nvar *2), CovPrec := CovPrec + nvar]


  if(!is.null(pred_colors) && !isFALSE(pred_colors)){
    if(class(pred_colors) != 'data.frame'){
      pred_colors <- as.data.frame(cbind(pretty_names, pred_colors, as.character(seq(length(pretty_names)))))
      names(pred_colors) <- c('pretty_names', 'colors', 'nodeID')
    } else{
      pred_colors <- cbind(pred_colors, as.character(seq(length(pretty_names))))
      names(pred_colors) <- c('pretty_names', 'colors', 'nodeID')
    }

    bnodes <- merge(bnodes, pred_colors, by.x = 'name', by.y = 'pretty_names', all.x = TRUE, all.y = FALSE)

    domain <- unique(bnodes$nodeID)
    cols <- unique(bnodes$colors)

    col_scale <-paste0('d3.scaleOrdinal()
                        .domain([', paste(paste0('"', domain, '"'), collapse = ', '), '])
                        .range([', paste(paste0('"', cols, '"'), collapse = ', '), '])')
  } else if(isFALSE(pred_colors)){
    bnodes$nodeID <- '1'
    domain <- NA
    cols <- NA
    col_scale <-paste0('d3.scaleOrdinal()
                        .domain([', paste(paste0('"', domain, '"'), collapse = ', '), '])
                        .range([', paste(paste0('"', cols, '"'), collapse = ', '), '])')
  } else if(is.null(pred_colors)){
    pnID <- as.data.frame(cbind(pretty_names, as.character(seq(length(pretty_names)))))
    names(pnID) <- c('pretty_names', 'nodeID')
    bnodes <- merge(bnodes, pnID, by.x = 'name', by.y = 'pretty_names', all.x = TRUE, all.y = FALSE)
  }

  if(labs == 'half'){
    bnodes[index < nvar*2 & index > nvar-1, name := NA]
  } else if(isFALSE(labs)){
    bnodes[, name := NA]
  } else if(labs == 'full'){
    bnodes[index > nvar-1, name := paste0(name, ' ')]
  }


  bnodes <- bnodes[order(index),]

  if(!is.null(pred_colors)){
    bsankey <- sankeyNetwork(Links = end_dat, Nodes = bnodes,
                             Source = "Cov", Target = "CovPrec",
                             Value = "Freq", NodeID = "name",
                             sinksRight=FALSE,
                             NodeGroup = "nodeID",
                             fontSize = 14, nodePadding = 10,
                             iterations = 0, fontFamily = 'Arial',
                             colourScale = col_scale)

  } else{
    bsankey <- sankeyNetwork(Links = end_dat, Nodes = bnodes,
                             Source = "Cov", Target = "CovPrec",
                             Value = "Freq", NodeID = "name",
                             sinksRight=FALSE,
                             NodeGroup = "nodeID",
                             fontSize = 14, nodePadding = 10,
                             iterations = 0, fontFamily = 'Arial')
  }

  if(!isFALSE(labs)){
    nom <- bnodes$name[1:nvar]

    bsankey <- onRender(
      bsankey,
      paste0('
             function(el,x){
             d3.select(el)
             .selectAll(".node text")
             .filter(function(d) { return (["',paste(nom,collapse = '","'),'"].indexOf(d.name) > -1);})
             .attr("x", x.options.nodeWidth - 20)
             .attr("text-anchor", "end");
             }
             ')
      )
  }

  return(bsankey)

}


######## FUNCTION ARGUMENTS FOR AGGREGATE TREE SANKEY FUNCTION #######

#model             requires a gbm model type output
#pretty_names      a string containing the long names you wish to use as the title
#                     of the plots must be in the same order as the model variables
#                     - if NULL will use the column names provided in the data ex.
#                     c('Elevation', 'Precipitation', ....)
#pred_colors       a string or data frame of HEX codes to color the predictor variables
#                     - if you want to ascribe certain colors to certain
#                     predictors provide a data frame with col names 'pred' ->
#                     should be pretty_names if using and 'color'
#                     if FALSE then all nodes will be grey, if NULL random colors assigned
#labs              opts: 'half' -> labels on first row of nodes + first TN & will automatically be to the left of the nodes
#                     (best when nodes are color coded),
#                     'full' -> labels on all nodes,
#                      FALSE -> no labels


aggregate_tree_sankey <- function(model, pretty_names = NULL, pred_colors = NULL, labs = 'half'){

  require(gbm)
  require(data.table)
  require(htmlwidgets)
  require(networkD3)
  require(parallel)
  require(foreach)
  require(doParallel)

  if(class(model) != 'gbm'){
    stop('model provided is not of class "gbm" please provide a gbm model object')
  }

  nvar <- length(model$var.names)

  if(is.null(pretty_names)){
    pretty_names <- model$var.names
  }

  #cov_links <- data.frame(Cov = integer(0), CovPrec = integer(0), VisRow = integer(0), ntree = integer(0), N = integer(0))

  numCores <- detectCores()
  registerDoParallel(numCores)
  cov_links <- foreach(t = 1:model$n.trees, .combine = rbind) %dopar% {

    tree <- pretty.gbm.tree(model.list$model, i.tree = t)
    tree <- tp_coords(tree)
    tree <- as.data.table(tree)
    tree$ntree <- t
    tree[SplitVar != -1,  'LeftChild' :=  tree[(LeftNode + 1),SplitVar]]
    tree[SplitVar != -1, 'RightChild' := tree[(RightNode + 1),SplitVar]]
    ln <- tree[SplitVar != -1, .(.N, ntree), by = .(SplitVar, LeftChild, VisRow)]
    names(ln)[1:2] <- c('Cov', 'CovPrec')
    rn <- tree[SplitVar != -1, .(.N, ntree), by = .(SplitVar, RightChild, VisRow)]
    names(rn)[1:2] <- c('Cov', 'CovPrec')
    tab <- rbind(ln, rn)
  }
  stopImplicitCluster()

  cov_links <- cov_links[, .(Val = sum(N)), by = .(Cov, CovPrec, VisRow)]

  cov_links[, VisRowPrec := VisRow + 1]
  cov_links[CovPrec == -1, CovPrec := length(pretty_names)]
  pretty_names <- append(pretty_names, 'Terminal Node')

  cnp <- unique(rbind(cov_links[, .(Cov, VisRow)], cov_links[, .(Cov = CovPrec, VisRow = VisRowPrec)]))
  cnp <- cnp[order(VisRow, Cov),]
  cnp$index <- seq(0, nrow(cnp) - 1)
  cov_links <- merge(cov_links, cnp, by.x = c('Cov', 'VisRow'), by.y = c('Cov', 'VisRow'), all.x = TRUE)
  names(cov_links)[which(names(cov_links) == 'index')] <- 'source'
  cov_links <- merge(cov_links, cnp, by.x = c('CovPrec', 'VisRowPrec'), by.y = c('Cov', 'VisRow'), all.x = TRUE)
  names(cov_links)[which(names(cov_links) == 'index')] <- 'target'
  cnp <- merge(cnp, data.frame(Cov = seq(0, length(pretty_names) - 1), name = pretty_names))

  # cov_links[, Cov := Cov + ((VisRow-1) * length(pretty_names))]
  # cov_links[, CovPrec := CovPrec + ((VisRowPrec-1) * length(pretty_names))]


  if(!is.null(pred_colors) && !isFALSE(pred_colors)){
    if(class(pred_colors) != 'data.frame'){
      pred_colors <- as.data.frame(cbind(pretty_names, pred_colors, as.character(seq(length(pretty_names)))))
      names(pred_colors) <- c('pretty_names', 'colors', 'nodeID')
    } else{
      pred_colors <- cbind(pred_colors, as.character(seq(length(pretty_names))))
      names(pred_colors) <- c('pretty_names', 'colors', 'nodeID')
    }

    cnp <- merge(cnp, pred_colors, by.x = 'name', by.y = 'pretty_names', all.x = TRUE, all.y = FALSE)

    domain <- unique(cnp$nodeID)
    cols <- unique(cnp$colors)

    col_scale <-paste0('d3.scaleOrdinal()
                        .domain([', paste(paste0('"', domain, '"'), collapse = ', '), '])
                        .range([', paste(paste0('"', cols, '"'), collapse = ', '), '])')
  } else if(isFALSE(pred_colors)){
    cnp$nodeID <- '1'
    domain <- NA
    cols <- NA
    col_scale <-paste0('d3.scaleOrdinal()
                        .domain([', paste(paste0('"', domain, '"'), collapse = ', '), '])
                        .range([', paste(paste0('"', cols, '"'), collapse = ', '), '])')
  } else if(is.null(pred_colors)){
    pnID <- as.data.frame(cbind(pretty_names, as.character(seq(length(pretty_names)))))
    names(pnID) <- c('pretty_names', 'nodeID')
    cnp <- merge(cnp, pnID, by.x = 'name', by.y = 'pretty_names', all.x = TRUE, all.y = FALSE)
  }

  if(labs == 'half'){
    cnp[duplicated(name), name := NA]
  } else if(isFALSE(labs)){
    cnp$name <- NA
  }

  cnp <- cnp[order(VisRow, Cov),]

  if(is.null(pred_colors)){
    cov_sankey <- sankeyNetwork(Links = cov_links, Nodes = cnp,
                                Source = "source", Target = "target",
                                Value = "Val", NodeID = "name", NodeGroup = 'nodeID',
                                sinksRight=FALSE, fontFamily = 'Arial',
                                fontSize = 14, nodePadding = 10,
                                iterations = 0, width = 1200, height = 400)
  } else{
    cov_sankey <- sankeyNetwork(Links = cov_links, Nodes = cnp,
                                Source = "source", Target = "target",
                                Value = "Val", NodeID = "name", NodeGroup = 'nodeID',
                                sinksRight=FALSE, fontFamily = 'Arial',
                                fontSize = 14, nodePadding = 10,
                                iterations = 0, width = 1200, height = 400,
                                colourScale = col_scale)
  }

  if(labs == 'half'){
    nom <- as.character(na.omit(cnp$name))

    cov_sankey <- onRender(
      cov_sankey,
      paste0('function(el,x){
             d3.select(el)
             .selectAll(".node text")
             .filter(function(d) { return (["',paste(nom, collapse = '","'),'"].indexOf(d.name) > -1);})
             .attr("x", x.options.nodeWidth - 20)
             .attr("text-anchor", "end");
             }'))
  }

  return(cov_sankey)

}
