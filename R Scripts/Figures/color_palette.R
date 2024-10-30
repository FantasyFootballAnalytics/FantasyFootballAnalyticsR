
# How I Generated the Color Schemes ---------------------------------------

# Specify the color schemes (qualitative categories) based on the number of tiers so that the colors are distinguishable

# 8 or fewer colors 
RColorBrewer::brewer.pal(8, name = "Dark2") # uses ColorBrewer: https://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=8
c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666") # replace #E6AB02 with black (#000000)
c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#000000","#A6761D","#666666")

# 9 colors
RColorBrewer::brewer.pal(9, name = "Set1") # uses ColorBrewer: https://colorbrewer2.org/#type=qualitative&scheme=Set1&n=9
c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF","#999999") # replace #FFFF33 with black (#000000)
c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#000000","#A65628","#F781BF","#999999")

# 10-12 colors
RColorBrewer::brewer.pal(12, name = "Paired") # uses ColorBrewer: https://colorbrewer2.org/#type=qualitative&scheme=Paired&n=12
c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99","#B15928") # replace #FFFF99 with black (#000000)
c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#000000","#B15928")

# 13-24 colors; uses Polychrome: https://www.jstatsoft.org/article/view/v090c01
set.seed(9641)
Dark24 <- Polychrome::createPalette(
  24,
  c("#2A95E8", "#E5629C"),
  range = c(10, 60),
  M = 100000)

# 24-36 colors; uses Polychrome: https://www.jstatsoft.org/article/view/v090c01
Polychrome::palette36.colors(36)
c("#5A5156","#E4E1E3","#F6222E","#FE00FA","#16FF32","#3283FE","#FEAF16","#B00068","#1CFFCE","#90AD1C","#2ED9FF","#DEA0FD","#AA0DFE","#F8A19F","#325A9B","#C4451C","#1C8356","#85660D","#B10DA1","#FBE426","#1CBE4F","#FA0087",
"#FC1CBF","#F7E1A0","#C075A6","#782AB6","#AAF400","#BDCDFF","#822E1C","#B5EFB5","#7ED7D1","#1C7F93","#D85FF7","#683B79","#66B0FF","#3B00FB")

# Select the Color Palette Based on Number of Tiers -----------------------

# Code to select color palette based on number of tiers

numTiers <- length(unique(tiers)) # I don't know what the variable name is for the tiers, but modify to the correct variable

if(numTiers <= 8){
  colorPalette <- c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#A6761D","#666666","#000000")
} else if(numTiers == 9){
  colorPalette <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628","#F781BF","#999999","#000000")
} else if(numTiers >= 10 & numTiers <= 12){
  colorPalette <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#B15928","#000000")
} else if(numTiers >= 13 & numTiers <= 24){
  colorPalette <- c("#3292E6","#E05D99","#16A300","#FE0D26","#DE00FF","#262E26","#BB8700","#760D88","#5C16FF","#E96638","#00A19F","#FE00D4","#712A22","#FC0081","#87965A","#998AAE","#AD73F3","#0D0DB3","#16225F","#DC5DD7","#944577","#00925D","#AD8580","#9F0D35")
} else if(numTiers >= 25 & numTiers <= 36){
  colorPalette <- c("#5A5156","#E4E1E3","#F6222E","#FE00FA","#16FF32","#3283FE","#FEAF16","#B00068","#1CFFCE","#90AD1C","#2ED9FF","#DEA0FD","#AA0DFE","#F8A19F","#325A9B","#C4451C","#1C8356","#85660D","#B10DA1","#FBE426","#1CBE4F","#FA0087",
    "#FC1CBF","#F7E1A0","#C075A6","#782AB6","#AAF400","#BDCDFF","#822E1C","#B5EFB5","#7ED7D1","#1C7F93","#D85FF7","#683B79","#66B0FF","#3B00FB")
} else{ # i.e., numTiers > 36
  # continue to use cividis
}

# Also, please remove the gray backdround from the plot, to help make the colors easier to see
