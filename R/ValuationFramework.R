

  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2016  Bond Lab Technologies, Inc.
  # 
  # This program is free software: you can redistribute it and/or modify
  # it under the terms of the GNU General Public License as published by
  # the Free Software Foundation, either version 3 of the License, or
  # (at your option) any later version.
  # 
  # This program is distributed in the hope that it will be useful,
  # but WITHOUT ANY WARRANTY; without even the implied warranty of
  # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  # GNU General Public License for more details.
  #
  # You should have received a copy of the GNU General Public License
  # along with this program.  If not, see <http://www.gnu.org/licenses/>.
  
  #' This function graphically depicts the valuation framework used by BondLab®
  #' @importFrom termstrc spr_ns
  #' @importFrom reshape2 melt
  #' @export
  Valuation <- function(){
    cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                    "#D55E00", "#CC79A7", "#000000","#E69F00")

  SpotCurve <- spr_ns(c(4.26591, -3.54742, 2.46095, 7.49599), m = 1:30)
  Maturity <- as.numeric(seq(1:30))
  Coupon <- SpotCurve - .25

  Data <- data.frame(Maturity = Maturity, 
                     CouponCurve = Coupon, 
                     SpotRateCurve = SpotCurve, 
                     ZVSpread = SpotCurve +.20, 
                     YTM = 3.25)
  Data <- melt(Data, id = "Maturity")
  framework <-
  ggplot(Data, aes(x= Maturity, y = value/100, colour = variable))+
  geom_line(aes(linetype = variable), size = 1)+
  theme_minimal()+
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "twodash")) +
  scale_y_continuous(breaks = seq(0, .04, .005), labels = percent_format())+
  geom_vline(xintercept = 10)+
  theme_minimal()+
  ylab("Yield") +
  xlab("Maturity (years)") +
  scale_colour_manual(values = cbbPalette) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title = element_text(size = 20)) +
  theme(panel.grid.major = element_line(size = .25, color = "grey")) +
  theme(legend.position=c(.80,.25), 
        legend.background = element_rect(fill = "transparent", 
                                         colour = "transparent"))+
  theme(legend.title=element_blank())+
  
  annotate("pointrange", x = 9, y = 2.95/100, ymin = 2.65/100, 
           ymax = 3.25/100, colour = "black", size = .5)+
  annotate("text", x=4.35, y = 2.9/100, label = c("Nominal Spread")) +
  
  annotate("pointrange", x= 19, y = 3.78/100, ymin = 3.67/100, ymax = 3.89/100, 
           colour = "black", size = .5) +
  
  geom_segment(aes(x= 1, y = 2.8/100, xend = 8.75, yend = 2.8/100), 
               arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  annotate("text", x=14.75, y = 3.85/100, label = c("ZV-Spread")) +
  geom_segment(aes(x= 12.5, y = 3.75/100, xend = 18.5, yend = 3.75/100), 
               arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  annotate("text", x=4.35, y = 4.25/100, 
           label = c("Cheap Cash Flows\n PV Spot > PV YTM"), size = 4) +
  annotate("text", x=16, y = 4.25/100, 
           label = c("Rich Cash Flows\n PV Spot < PV YTM"), size = 4)
  plot(framework)
}