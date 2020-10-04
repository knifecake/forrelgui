pedigree_plots_view <- function() {
  gui$claim_plot <- tkrplot::tkrplot(
    gWidgets2::getToolkitWidget(gui$peds),
    function() custom_ped_plot(model$claim_ped, model$available, get_genotyped_ids(model$claim_ped))
  )
  gWidgets2::add(gui$peds, gui$claim_plot)
  
  gui$true_plot <- tkrplot::tkrplot(
    gWidgets2::getToolkitWidget(gui$peds),
    function() custom_ped_plot(model$true_ped, model$available, get_genotyped_ids(model$claim_ped))
  )
  gWidgets2::add(gui$peds, gui$true_plot)
}

redraw_pedigrees <- function() {
  tkrplot::tkrreplot(gui$claim_plot)
  tkrplot::tkrreplot(gui$true_plot)
}