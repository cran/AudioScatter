#' load dataset to dTable.
#' @import tibble

dTable <- tibble::tibble(read.csv('data/AudioData.csv'))

#' AudioData
#'
#' An example dataset containing the preop PTA, preop WRS, postop PTA, and postop WRS.
#' You can load your own dataset containing the preop PTA, preop WRS, and potentially postop PTA and postop WRS.
#' The variables are as follows:
#'
#' \itemize{
#'   \item PrePTA. preop pure tone average
#'   \item PreWRS. preop word recognition score
#'   \item PostPTA. postop pure tone average
#'   \item PostWRS. postop word recognition score
#' }
#'
#' @docType data
#' @keywords datasets
#' @name AudioData
#' @usage data(AudioData)
#' @format A data frame with rows and 4 columns
NULL

globalVariables(c("PrePTA", "PreWRS", "PostPTA", "PostWRS", "dPTA", "dWRS", "dWRSCol", "dPTARow"))

#' SavePreScatter
#'
#' @param dTable table loaded from csv file including PrePTA, PreWRS, and possibly PostPTA, PostWRS.
#' @import ggplot2
#'
#' @returns png file for preop scattergram.
#' @export

SavePreScatter <- function(dTable) {

    ### Scatterplots
  #'SavePreScatter
    # Pre-treatment PTA vs WRS

    ###WRS bins are closed on right, open on left e.g. 0-9, 10-19, 20-29, 30-39, 40-49, 50-59, 60-69, 70-79, 80-89, 90-100
    ###PTA bins are closed on left, e.g 0-10, 11-20, 21-30, 31-40, 41-50, 51-60, 61-70, 71-80, 81-90, >/=91
  ggplot2::ggplot(data=dTable, mapping = aes(x = PreWRS, y = PrePTA)) +
    geom_text(aes(label = after_stat(n)), color = "black", stat = "sum", size = 7) + #plot numbers of patients in each bin
    labs(tag = "Pan and Oghalai 2025\nOtology & Neurotology") +
    theme_bw(base_size = 18) +
    theme(axis.text=element_text(size=14.4),
          axis.title=element_text(size=18),
          plot.tag = element_text(size=6, vjust=-10, hjust=-0.001),
          plot.tag.location = "plot") +
    theme(panel.grid.major = element_line(color = "black", size = 0.7), panel.grid.minor = element_blank()) + #make grid
    theme(axis.line = element_line(size = 0.7, color = "black")) +
    theme(axis.text.y = element_text(vjust = 1.8)) + #shift y axis numbers so it is centered with the box it reflects
    theme(axis.text.x = element_text(angle = 35, hjust = -0.2)) + #shift x axis numbers so it is centered over the box it reflects
    scale_y_binned(name = "Pure Tone Average (dB)\n ", #y axis title
                   limits = c(100,0), #y axis limits 0-100
                   oob = scales::squish, #squish include out of range numbers in upper most bin
                   expand = c(0,0), #don't add space around 0,0
                   n.breaks = 10, #bin the y axis
                   show.limits = TRUE,
                   right = FALSE, #bins are closed on the left
                   transform = "reverse", #start with smallest numbers at top instead of the bottom
                   labels = c('', expression("">=91), '81 \u{2013} 90', '71 \u{2013} 80', '61 \u{2013} 70', '51 \u{2013} 60', '41 \u{2013} 50', '31 \u{2013} 40', '21 \u{2013} 30', '11 \u{2013} 20', '0 \u{2013} 10')) + #re-label the axes numbers with the bin range
    scale_x_binned(name = "Word Recognition Score (%)\n \n ", #x axis title
                   limits = c(100,0), #x axis limits 0-100
                   expand = c(0,0), #don't add space around 0,0
                   n.breaks = 10, #bin the x axis
                   show.limits = TRUE,
                   right = TRUE, #bins are closed on the right
                   transform = "reverse", #start with largest numbers at the left instead of on the right
                   position = "top", #put the x axis at the top
                   labels = c('100 \u{2013} 90', '89 \u{2013} 80', '79 \u{2013} 70', '69 \u{2013} 60', '59 \u{2013} 50','49 \u{2013} 40', '39 \u{2013} 30', '29 \u{2013} 20', '19 \u{2013} 10', '9 \u{2013} 0 ', '')) #re-label the axes numbers with the bin range

  ggplot2::ggsave('PreScattergram.png', width = 7, height = 7, dpi = 300, path = tempdir())
}



`%>%` = dplyr::`%>%`

#' CalcChange
#' Calculate Change between Post and Pre PTA and WRS and put in appropriate column and row for scattergram.
#'
#' Post-treatment change in PTA and WRS
#' Calculate change in PTA
#' Calculate change in WRS
#' create new columns relating the change in WRS to the correct column for the scattergram.
#' create new columns relating the change in PTA to the correct row for the scattergram.

#' @param dTable table loaded from csv file including PrePTA, PreWRS, and possibly PostPTA, PostWRS.
#' @import dplyr
#' @importFrom dplyr %>%
#' @import tidyverse
#' @import scales
#' @returns new table with change in PTA and WRS and columns/rows they should go in for PostScattergram.
#' @export
#'

CalcChange <- function(dTable) {
  dTable <- dTable %>% dplyr::mutate(dPTA = PostPTA - PrePTA,
                              dWRS = PostWRS-PreWRS,
                              dWRSCol = dplyr::case_when(dWRS>40~1,(dWRS>30&dWRS<41)~2,(dWRS>20&dWRS<31)~3,(dWRS>10&dWRS<21)~4,(dWRS>0&dWRS<11)~5,(dWRS==0)~6,
                                                  (dWRS<0&dWRS>-11)~7,(dWRS<-10&dWRS>-21)~8,(dWRS<-20&dWRS>-31)~9,(dWRS<-30&dWRS>-41)~10,TRUE~11),

                              dPTARow = dplyr::case_when(dPTA>40~1,(dPTA>30&dPTA<41)~2,(dPTA>20&dPTA<31)~3,(dPTA>10&dPTA<21)~4,(dPTA>0&dPTA<11)~5,(dPTA==0)~6,
                                                  (dPTA<0&dPTA>-11)~7,(dPTA<-10&dPTA>-21)~8,(dPTA<-20&dPTA>-31)~9,(dPTA<-30&dPTA>-41)~10,TRUE~11))
}

#' Calculate the audiogram changes and write to new table dTablePost
dTablePost <- CalcChange(dTable)

#' SavePostScatter
#'
#' @param dTablePost table resulting from CalcChange with change in PTA and WRS, dPTA and dWRS, respectively, as well as the column and row dPTA and dWRS should go.
#'
#' @import ggplot2
#' @import scales
#' @returns png file of postop scattergram
#' @export

SavePostScatter <- function(dTablePost) {

  ggplot2::ggplot(data=dTablePost, mapping = aes(x = dWRSCol, y = dPTARow)) +
    geom_text(aes(label = after_stat(n)), color = "black", stat = "sum", size = 7) + #plot number of patients in each bin
    labs(tag = "Pan and Oghalai 2025\nOtology & Neurotology") +
    theme_bw(base_size = 18) +
    theme(axis.text=element_text(size=14.4),
          axis.title=element_text(size=18),
          plot.tag = element_text(size=6, vjust=-10, hjust=0),
          plot.tag.location = "plot") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_line(color = "black", size = 0.7)) + #make grid
    theme(axis.line = element_line(size = 0.7, color = "black")) +
    theme(axis.text.y = element_text(angle = 0, vjust = 0)) + #shift y axis numbers so it is centered next to the bin
    theme(axis.text.x = element_text(angle = 55, hjust = 0)) + #shift x axis numbers so it is centered over the bin
    scale_y_continuous(name = "Pure Tone Average (post - pre, dB)\n <\u{2013} Worse      Improved \u{2013}>", #y axis title
                       limits = c(0.5,11.5), #y axis limits 1-11
                       expand = c(0,0), #don't add extra space around 0,0
                       breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), #custom breaks for bins
                       labels = c(expression("">=41), '31 \u{2013} 40', '21 \u{2013} 30', '11 \u{2013} 20', '1 \u{2013} 10', '0', '-10 \u{2013} -1', '-20 \u{2013} -11', '-30 \u{2013} -21', '-40 \u{2013} -31', expression(""<=-41))) + #relabel axis numbers with bin range
    scale_x_continuous(name = "Word Recognition Score (post-pre, %)\n <\u{2013} Improved      Worse \u{2013}>", position = "top", #x axis title
                       limits = c(0.5,11.5), #x axis limits 1-11
                       expand = c(0,0), #don't add extra space around 0,0
                       breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), #custom breaks for bins
                       labels = c(expression("">=41), '31 \u{2013} 40 ', '21 \u{2013} 30 ', '11 \u{2013} 20 ', '1 \u{2013} 10 ', '0', '-1 \u{2013} -10 ', '-11 \u{2013} -20 ', '-21 \u{2013} -30 ', '-31 \u{2013} -40 ', expression(""<=-41)) #relabel axis numbers with bin range
    )

  ggplot2::ggsave('PostScattergram.png', width = 7, height = 7, dpi = 300, path = tempdir())
}


#' @examples
#' # basic usage of AudioScatter.
#'
#' # load example dataset to dTable.
#' dTable <- tibble(read.csv('data/AudioData.csv'))
#'
#' Make PreScattergram png file from preop PTA and WRS.
#' SavePreScatter(dTable)
#'
#' Make new table dTablePost with CalcChange to calculate the change and the columns/rows for WRS/PTA change for the scattergram.
#' dTablePost <- CalcChange(dTable)
#'
#' Make PostScattergram png file from change in PTA and WRS.
#' SavePostScatter(dTablePost)
