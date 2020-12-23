data_input_tab_view <- function() {
  data_tab <- gWidgets2::gvbox(label = 'Pedigrees', container = gui$tabs)
  
  # pedigree input
  ped_input <- pedigree_input_view(container = data_tab)
  
  # case data and frequency database input
  allele_input <- allele_input_view(container = data_tab)
  
  # launch calculation
  gWidgets2::gbutton('Calculate exclusion power',
                     handler = function(h) calculate_ep(),
                     container = data_tab)
  
  data_tab
}
