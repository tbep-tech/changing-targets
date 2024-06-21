#' @title Create a colorized table for indicator reporting
#'
#' @description Create a colorized table for indicator reporting
#'
#' @param epcdata data frame of epc data returned by \code{\link{read_importwq}}
#' @param txtsz numeric for size of text in the plot, applies only if \code{tab = FALSE}
#' @param trgs optional \code{data.frame} for annual bay segment water quality targets, defaults to \code{\link{targets}}
#' @param yrrng numeric vector indicating min, max years to include, defaults to range of years in \code{epcdata}
#' @param bay_segment chr string for bay segments to include, one to all of "OTB", "HB", "MTB", "LTB"
#' @param asreact logical indicating if a \code{\link[reactable]{reactable}} object is returned
#' @param nrows if \code{asreact = TRUE}, a numeric specifying number of rows in the table
#' @param abbrev logical indicating if text labels in the plot are abbreviated as the first letter
#' @param family optional chr string indicating font family for text labels
#' @param plotly logical if matrix is created using plotly
#' @param partialyr logical indicating if incomplete annual data for the most recent year are approximated by five year monthly averages for each parameter
#' @param width numeric for width of the plot in pixels, only applies of \code{plotly = TRUE}
#' @param height numeric for height of the plot in pixels, only applies of \code{plotly = TRUE}
#'
#' @concept show
#'
#' @return A static \code{\link[ggplot2]{ggplot}} object is returned if \code{asreact = FALSE}, otherwise a \code{\link[reactable]{reactable}} table is returned
#'
#' @seealso \code{\link{show_wqmatrix}}, \code{\link{show_segmatrix}}
#' @export
#'
#' @importFrom dplyr "%>%"
#' @importFrom reactable colDef
#'
#' @import ggplot2
#'
#' @examples
#' show_matrixotb(epcdata)
show_matrixotb <- function(epcdata, txtsz = 3, trgs = NULL, yrrng = NULL, bay_segment = c('NW', 'NE', 'CW', 'CE', 'SW', 'SE'), 
                        asreact = FALSE,
                        nrows = 10, abbrev = FALSE, family = NA, plotly = FALSE, partialyr = FALSE, width = NULL,
                        height = NULL){
  
  # default targets from data file
  if(is.null(trgs))
    trgs <- targets
  
  # get year range from data if not provided
  if(is.null(yrrng))
    yrrng <- c(1975, max(epcdata$yr, na.rm = T))
  
  # process data to plot
  avedat <- anlz_avedatotb(epcdata, partialyr = partialyr)
  toplo <- anlz_attainotb(avedat, trgs = trgs) %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::mutate(
      bay_segment = factor(bay_segment, levels = c('NW', 'NE', 'CW', 'CE', 'SW', 'SE'))
    )
  
  # add abbreviations if true
  if(abbrev)
    toplo <- toplo %>%
    dplyr::mutate(
      outcometxt = dplyr::case_when(
        outcome == 'red' ~ 'R',
        outcome == 'yellow' ~ 'Y',
        outcome == 'green' ~ 'G'
      )
    )
  if(!abbrev)
    toplo <- toplo %>%
    dplyr::mutate(
      outcometxt = outcome
    )
  
  # reactable object
  if(asreact){
    
    totab <- toplo %>%
      dplyr::select(bay_segment, yr, outcometxt) %>%
      tidyr::spread(bay_segment, outcometxt)
    
    colfun <- function(x){
      
      out <- dplyr::case_when(
        x %in% c('R', 'red') ~ '#CC3231',
        x %in% c('Y', 'yellow') ~ '#E9C318',
        x %in% c('G', 'green') ~ '#2DC938'
      )
      
      return(out)
      
    }
    
    
    # make reactable
    out <- show_reactable(totab, colfun, nrows = nrows, txtsz = txtsz)
    
    return(out)
    
  }
  
  # add descriptive labels, Action
  lbs <- dplyr::tibble(
    outcome = c('red', 'yellow', 'green'),
    Action = c('On Alert', 'Caution', 'Stay the Course')
  )
  toplo <- toplo %>%
    dplyr::left_join(lbs, by = 'outcome') %>%
    tidyr::separate(chl_la, c('chl', 'la'), sep = '_', remove = F) %>%
    dplyr::mutate(
      chl = paste0('chla: ', chl),
      la = paste0('la: ', la)
    ) %>%
    tidyr::unite(chl_la, c('chl', 'la'), sep = ', ') %>%
    dplyr::mutate(
      chl_la = paste0('(', chl_la, ')')
    ) %>%
    unite(Action, c('Action', 'chl_la'), sep = ' ')
  
  # ggplot
  p <- ggplot(toplo, aes(x = bay_segment, y = yr, fill = outcome)) +
    geom_tile(aes(group = Action), colour = 'black') +
    scale_y_reverse(expand = c(0, 0), breaks = toplo$yr) +
    scale_x_discrete(expand = c(0, 0), position = 'top') +
    scale_fill_manual(values = c(red = '#CC3231', yellow = '#E9C318', green = '#2DC938')) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
  if(!is.null(txtsz))
    p <- p +
    geom_text(aes(label = outcometxt), size = txtsz, family = family)
  
  if(partialyr)
    p <- p +
    labs(caption = paste0('*Incomplete data for ', max(yrrng), ' estimated\nby five year average'))
  
  if(plotly)
    p <- show_matrixplotly(p, family = family, tooltip = 'Action', width = width, height = height)
  
  return(p)
  
}

#' Estimate annual means
#'
#' Estimate annual means for chlorophyll and secchi data
#'
#' @param epcdata \code{data.frame} formatted from \code{read_importwq}
#' @param partialyr logical indicating if incomplete annual data for the most recent year are approximated by five year monthly averages for each parameter
#'
#' @return Mean estimates for chlorophyll and secchi
#'
#' @concept analyze
#'
#' @import dplyr tidyr
#'
#' @export
#'
#' @examples
#' # view average estimates
#' anlz_avedatotb(epcdata)
anlz_avedatotb <- function(epcdata, partialyr = FALSE){

  # year month averages
  # long format, separate bay_segment for MTB into sub segs
  # mtb year month averages are weighted
  moout <- epcdata %>%
    dplyr::select(yr, mo, bay_segment, epchc_station, chla, sd_m) %>%
    tidyr::gather('var', 'val', chla, sd_m) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      bay_segment = dplyr::case_when(
        epchc_station %in% c(46, 64) ~ "NW",
        epchc_station %in% c(47, 60) ~ "NE",
        epchc_station %in% c(65, 66) ~ "CW",
        epchc_station %in% c(40, 41, 63) ~ "CE",
        epchc_station %in% c(38, 67, 68) ~ "SW",
        epchc_station %in% c(36, 50, 51) ~ "SE",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(bay_segment)) |> 
    dplyr::group_by(bay_segment, yr, mo, var) %>%
    dplyr::summarise(val = mean(val)) %>%
    dplyr::ungroup() %>%
    drop_na() %>%
    dplyr::group_by(bay_segment, yr, mo, var) %>%
    dplyr::summarise(
      val = sum(val)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(val)) %>%
    dplyr::filter(!is.infinite(val)) %>%
    dplyr::arrange(var, yr, mo, bay_segment)
  
  # add partial year
  if(partialyr){
    
    # years to averge, last five complete
    maxyr <- max(moout$yr)
    yrfl <- c(maxyr - 5, maxyr - 1)
    
    # months to fill
    mofl <- moout %>%
      dplyr::filter(yr %in% maxyr) %>%
      dplyr::pull(mo) %>%
      unique %>%
      setdiff(1:12, .)
    
    # month averages
    moave <- moout %>%
      dplyr::filter(yr >= yrfl[1] & yr <= yrfl[2]) %>%
      dplyr::group_by(bay_segment, mo, var) %>%
      summarise(val = mean(val, na.rm = TRUE)) %>%
      dplyr::filter(mo %in% mofl) %>%
      dplyr::mutate(yr = maxyr)
    
    # join missing months to
    moout <- moout %>%
      dplyr::bind_rows(moave) %>%
      dplyr::arrange(var, yr, mo, bay_segment)
    
  }
  
  # annual data
  anout <- moout %>%
    dplyr::group_by(yr, bay_segment, var) %>%
    dplyr::summarise(val = mean(val)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      var = dplyr::case_when(
        var == 'chla' ~ 'mean_chla',
        TRUE ~ var
      )
    ) %>%
    tidyr::spread('var', 'val') %>%
    dplyr::rename(
      mean_sdm = sd_m
    ) %>%
    dplyr::mutate(
      mean_la = 1.49 / mean_sdm
    ) %>%
    tidyr::gather('var', 'val', mean_chla, mean_la, mean_sdm) %>%
    dplyr::filter(!is.na(val)) %>%
    dplyr::filter(!is.infinite(val)) %>%
    dplyr::arrange(var, yr, bay_segment)
  
  # mo dat to light attenuation
  moout <- moout %>%
    dplyr::mutate(
      var = dplyr::case_when(
        var == 'chla' ~ 'mean_chla',
        var == 'sd_m' ~ 'mean_la'
      ),
      val = dplyr::case_when(
        var == 'mean_la' ~ 1.49 / val,
        TRUE ~ val
      )
    )
  
  # combine all
  out <- list(
    ann = anout,
    mos = moout
  )
  
  return(out)
  
}

#' Get attainment categories
#'
#' Get attainment categories for each year and bay segment using chlorophyll and light attenuation
#'
#' @param avedat result returned from \code{\link{anlz_avedat}}
#' @param magdurout logical indicating if the separate magnitude and duration estimates are returned
#' @param trgs optional \code{data.frame} for annual bay segment water quality targets, defaults to \code{\link{targets}}
#'
#' @return A \code{data.frame} for each year and bay segment showing the attainment category
#' @export
#'
#' @concept analyze
#'
#' @examples
#' avedat <- anlz_avedatotb(epcdata)
#' anlz_attainotb(avedat)
anlz_attainotb <- function(avedat, magdurout = FALSE, trgs = NULL){

  # default targets from data file
  if(is.null(trgs))
    trgs <- targets
  
  # format targets
  trgs <- trgs %>%
    tidyr::gather('var', 'val', -bay_segment, -name) %>%
    tidyr::separate(var, c('var', 'trgtyp'), sep = '_') %>%
    spread(trgtyp, val) %>%
    dplyr::select(bay_segment, var, target, smallex, thresh)
  
  # get annual averages, join with targets
  annave <- avedat$ann %>%
    dplyr::filter(!var %in% 'mean_sdm') %>%
    dplyr::mutate(var = gsub('mean\\_', '', var)) %>%
    dplyr::left_join(trgs, by = c('bay_segment', 'var'))
  
  # get magnitude and durations
  magdur <- annave %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      mags = findInterval(val, c(smallex, thresh))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(bay_segment) %>%
    tidyr::nest() %>%
    mutate(
      data = purrr::map(data, function(data){
        
        out <- data %>%
          dplyr::mutate(
            durats = stats::filter(val > target, filter = rep(1, 4), sides = 1),
            durats = as.numeric(durats)
          )
        
        return(out)
        
      })
    ) %>%
    unnest(cols = c(data)) %>%
    mutate(
      outcome = dplyr::case_when(
        is.na(durats) & mags == 2 ~ 3L,
        is.na(durats) & mags == 1 ~ 2L,
        is.na(durats) & mags == 0 ~ 0L,
        mags == 2 & durats == 4 ~ 3L,
        mags == 2 & durats < 4 ~ 2L,
        mags == 1 & durats == 4 ~ 2L,
        mags == 1 & durats < 4 ~ 1L,
        mags == 0 ~ 0L
      )
    ) %>%
    ungroup
  
  if(magdurout)
    return(magdur)
  
  # get final outcomes
  out <- magdur %>%
    dplyr::select(bay_segment, yr, var, outcome) %>%
    tidyr::spread(var, outcome) %>%
    na.omit %>%
    tidyr::unite('chl_la', chla, la) %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        chl_la %in% '0_0' ~ 'green',
        chl_la %in% c('1_0', '2_0', '3_0', '0_1', '1_1', '2_1', '0_2', '1_2', '0_3') ~ 'yellow',
        chl_la %in% c('3_1', '2_2', '3_2', '1_3', '2_3', '3_3') ~ 'red'
      )
    )
  
  return(out)
  
}

#' @title Plot annual water quality values, targets, and thresholds for a segment
#'
#' @description Plot annual water quality values, targets, and thresholds for a bay segment
#'
#' @param epcdata data frame of epc data returned by \code{\link{read_importwq}}
#' @param bay_segment chr string for the bay segment, one of "OTB", "HB", "MTB", "LTB"
#' @param thr chr string indicating which water quality value and appropriate target/threshold to plot, one of "chl" for chlorophyll and "la" for light availability
#' @param trgs optional \code{data.frame} for annual bay segment water quality targets/thresholds, defaults to \code{\link{targets}}
#' @param yrrng numeric vector indicating min, max years to include
#' @param family optional chr string indicating font family for text labels
#' @param labelexp logical indicating if y axis and target labels are plotted as expressions, default \code{TRUE}
#' @param txtlab logical indicating if a text label for the target value is shown in the plot
#' @param thrs logical indicating if reference lines are shown only for the regulatory threshold
#' @param partialyr logical indicating if incomplete annual data for the most recent year are approximated by five year monthly averages for each parameter
#'
#' @concept show
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @export
#'
#' @import ggplot2
#' @importFrom dplyr "%>%"
#'
#' @examples
#' show_thrplototb(epcdata, bay_segment = 'OTB', thr = 'chl')
show_thrplototb <- function(epcdata, bay_segment = c('NW', 'NE', 'CW', 'CE', 'SW', 'SE'), thr = c('chla', 'la'), trgs = NULL, yrrng = c(1975, 2023),
                         family = NA, labelexp = TRUE, txtlab = TRUE, thrs = FALSE, partialyr = FALSE){
  
  # default targets from data file
  if(is.null(trgs))
    trgs <- targets
  
  # yrrng must be in ascending order
  if(yrrng[1] >= yrrng[2])
    stop('yrrng argument must be in ascending order, e.g., c(1975, 2023)')
  
  # segment
  bay_segment <- match.arg(bay_segment)
  
  # wq to plot
  thr <- match.arg(thr)
  
  # colors
  cols <- c("Annual Mean"="red", "Management Target"="blue", "+1 se (small exceedance)"="blue", "+2 se (large exceedance)"="blue")
  
  # averages
  aves <- anlz_avedatotb(epcdata, partialyr = partialyr)
  
  # axis label
  if(labelexp)
    axlab <- ifelse(thr == 'chla', expression("Mean Ann. Chl-a ("~ mu * "g\u00B7L"^-1 *")"),
                    ifelse(thr == 'la', expression("Mean Ann. Light Att. (m  " ^-1 *")"), NA))
  if(!labelexp)
    axlab <- dplyr::case_when(
      thr == 'chla' ~ "Mean Ann. Chl-a (ug/L)",
      thr == 'la' ~ "Mean Ann. Light Atten. (m-1)"
    )
  
  # get lines to plot
  toln <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment)
  trgnum <- toln %>% dplyr::pull(!!paste0(thr, '_target'))
  smlnum <- toln %>% dplyr::pull(!!paste0(thr, '_smallex'))
  thrnum <- toln %>% dplyr::pull(!!paste0(thr, '_thresh'))
  
  
  # change label location if thrs is true
  if(!thrs)
    num <- trgnum
  if(thrs)
    num <- thrnum
  
  # threshold label
  if(labelexp)
    trglab <- dplyr::case_when(
      thr == 'chla' ~ paste(num, "~ mu * g%.%L^{-1}"),
      thr == 'la' ~ paste(num, "~m","^{-1}")
    )
  if(!labelexp)
    trglab <- dplyr::case_when(
      thr == 'chla' ~ paste(num, "ug/L"),
      thr == 'la' ~ paste(num, "m-1")
    )
  
  # bay segment plot title
  ttl <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::pull(name)
  
  if(partialyr)
    ttl <- paste0(ttl, '*')
  
  # get data to plo
  toplo <- aves$ann %>%
    dplyr::filter(grepl(paste0('_', thr, '$'), var)) %>%
    mutate(var = 'yval') %>%
    dplyr::filter(bay_segment == !!bay_segment) %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    tidyr::spread(var, val)
  
  p <- ggplot() +
    geom_point(data = toplo, aes(x = yr, y = yval, colour = "Annual Mean"), size = 3) +
    geom_line(data = toplo, aes(x = yr, y = yval, colour = "Annual Mean"), linetype = 'solid', size = 0.75) +
    labs(y = axlab, title = ttl) +
    scale_x_continuous(breaks = seq(yrrng[1], yrrng[2], by = 1)) +
    theme_grey(base_family = family) +
    theme(axis.title.x = element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          panel.background = element_rect(fill = '#ECECEC'),
          legend.position = 'top',#c(0.85, 0.95),
          legend.background = element_rect(fill=NA),
          legend.key = element_rect(fill = '#ECECEC'),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, size = 7, hjust = 1)
    )
  
  # all targets/thresholds
  if(!thrs)
    p <- p +
    geom_hline(aes(yintercept = trgnum, colour = 'Management Target'), linetype = 'solid') +
    geom_hline(aes(yintercept = smlnum, colour = '+1 se (small exceedance)'), linetype = 'dashed') +
    geom_hline(aes(yintercept = thrnum, colour = '+2 se (large exceedance)'), linetype = 'dotted') +
    scale_colour_manual(values = cols, labels = factor(names(cols), levels = names(cols))) +
    guides(colour = guide_legend(
      override.aes = list(
        shape = c(19, NA, NA, NA),
        colour = cols,
        linetype = c('solid', 'solid', 'dashed', 'dotted'),
        linewidth = c(0.75, 0.5, 0.5, 0.5)
      )
    ))
  
  # thresholds only
  if(thrs)
    p <- p +
    geom_hline(aes(yintercept = thrnum, colour = '+2 se (large exceedance)'), linetype = 'dotted') +
    scale_colour_manual(values = cols[c(1, 4)], labels = factor(names(cols[c(1, 4)]), levels = names(cols[c(1, 4)]))) +
    guides(colour = guide_legend(
      override.aes = list(
        shape = c(19, NA),
        colour = cols[c(1, 4)],
        linetype = c('solid', 'dotted'),
        linewidth = c(0.75, 0.5)
      )
    ))
  
  if(txtlab & !thrs)
    p <- p +
    geom_text(aes(yrrng[1], num, label = trglab), parse = labelexp, hjust = 0.2, vjust = 1, family = family, colour = 'blue')
  
  if(txtlab & thrs)
    p <- p +
    geom_text(aes(yrrng[1], max(toplo$yval), label = trglab), parse = labelexp, hjust = 0.2, vjust = 1, family = family, colour = 'blue')
  
  
  if(partialyr)
    p <- p +
    labs(caption = paste0('*Incomplete data for ', max(yrrng), ' estimated by five year average'))
  
  return(p)
  
}