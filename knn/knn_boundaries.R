#!/usr/local/bin/Rscript
# ---------------------------------------------------------------------------------------------------------------------
#
#                             			Florida International University
#                                          		Machine Learning
#                                                   Homework
#
#   This software is a "Camilo Valdes Work" under the terms of the United States Copyright Act.
#   Please cite the author(s) in any work or product based on this material.
#
#   OBJECTIVE:
#	The purpose of this program is to implement question No. 1 of Homework 1, i.e., the KNN decision boundaries
#
#	To run this code you have to adjust the variables 'working_directory' and 'homeworkDirectory' to match the
#	directory structure of the machine you are working on.
#
#
#   NOTES:
#   Please see the dependencies section below for the required libraries (if any).
#
#   DEPENDENCIES:
#
#		• ggplot2
#       • reshape2
#		• class
#		• RColorBrewer
#
#   The above libraries & modules are required.
#
#
#   AUTHOR:	Camilo Valdes (cvalde03@fiu.edu)
#			Bioinformatics Research Group,
#			School of Computing and Information Sciences, 
#			Florida International University (FIU)
#
#
# ---------------------------------------------------------------------------------------------------------------------


# 	Import any necessary libraries here
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(class)

# 	Enable printing of large numbers. R defaults to "rounding" for display, and
# 	Cause the warnings to be printed as they occur.
options( width=512, digits=15, warn=1 )

#
#	Custom Colors for Color Blind folks
#
# 	The palette with black:
cbbPalette_black = c( "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7" )

# 	The palette with grey:
cbPalette_grey = c( "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7" )


# ------------------------------------------------- Project Setup -----------------------------------------------------

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] R Starting... ", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )


working_directory="/path/to/your/working/directory"
setwd( file.path( working_directory ) )

homeworkNumber = "1"
homeworkDirectory = paste( working_directory, "/homework_", homeworkNumber, "_code", sep="" )

figures_base_dir=paste( homeworkDirectory, "/figures", sep="")
ggplot_output_dir=paste( figures_base_dir, "/pdf", sep="" )
png_output_dir=paste( figures_base_dir, "/png", sep="" )

dir.create( file.path( ggplot_output_dir ), showWarnings=FALSE, recursive=TRUE )
dir.create( file.path( png_output_dir ), showWarnings=FALSE, recursive=TRUE )



# ------------------------------------------------------ Main ---------------------------------------------------------

#
#	Training Set
#
trainingSet = as.data.frame( cbind( c( 0, 2, 4, 6 ),	# x-coordinate
									c( 1, 0, 4, 3 ),	# y-coordinate	
									c( "black", "white", "black", "white" ),
									c( 1.0, 1.0, 1.0, 1.0 ) ) )
									
colnames(trainingSet) 	= c( "x", "y", "labels", "prob" )
print(trainingSet)


#
#	Test Set
#	We'll use the 'rep' and 'seq' R functions to automatically populate the entire grid.
#
testSet = as.data.frame( cbind( c( rep(0:6,each=5) ), # x-coordinate
	 							c( seq(0,4), seq(0,4), seq(0,4), seq(0,4), seq(0,4), seq(0,4), seq(0,4) ) ) ) # y-coordinate

colnames(testSet) 	= c( "x", "y")
print(testSet)

#
#	This is for part 1.C, when the Y-coordinate is multiplied by 5
#
# testSet$y 		= testSet$y * 5
# trainingSet$y 	= as.numeric(trainingSet$y) * 5

print(trainingSet)
print(testSet)


#
#	KNN
#
valueForK = 1
matrixWithTrainingCoordinates 	= as.matrix(trainingSet[,1:2])
matrixWithTestCoordinates		= as.matrix(testSet[,1:2])

#
#
knn_results = knn( 	train=matrixWithTrainingCoordinates, 
					test=matrixWithTestCoordinates, 
					cl=as.factor(trainingSet$labels), 
					k=valueForK,
					prob=TRUE )
knn_results


#
#	Data Structures for plot
#

testSet = cbind( testSet, knn_results, as.numeric(as.character(attributes(knn_results)$prob)) )
colnames(testSet) 	= c( "x", "y", "labels", "prob" )
print( testSet )

vectorOfLabelsForTraining 	= data.frame(trainingSet[["labels"]])
colnames(vectorOfLabelsForTraining) = "labels"
dataFrameOfLabelsForTest	= data.frame(testSet[["labels"]])
colnames(dataFrameOfLabelsForTest) = "labels"

#	Stack all the labels together so we can use them in the final figure
allLabels = rbind( vectorOfLabelsForTraining, dataFrameOfLabelsForTest )


#	Combine the training and testing points so that we can plot them all in one graph
dataFrameWithAllPoints = as.data.frame( rbind(
										matrixWithTrainingCoordinates,
										matrixWithTestCoordinates
										) )


dataFrameWithAllPoints = as.data.frame( cbind( dataFrameWithAllPoints, allLabels ) )
dataFrameWithAllPoints$x = as.numeric(dataFrameWithAllPoints$x)
dataFrameWithAllPoints$y = as.numeric(as.character(dataFrameWithAllPoints$y))

print(dataFrameWithAllPoints)


#
#	Dot Plot
#
decisionBoundaryPlot = ggplot( dataFrameWithAllPoints, aes(x=x, y=y, colour=labels, size=1.25 ) ) + 
							geom_point() + 
							scale_colour_manual( values=c( "black"="black", "white"="white" ) ) +
							ggtitle( paste( "KNN Decision Boundaries, K=", valueForK, sep="" ) ) +
							xlab("X Coordinate") + ylab( "Y Coordinate" ) +
							theme(legend.position="top") +
							scale_size(guide="none")

decisionBoundaryPlot

outputFileFoDecisionBoundaryPlot	= paste( png_output_dir, "/decision_boundaries_k_", valueForK, ".png", sep="" )
ggsave(outputFileFoDecisionBoundaryPlot)


# ------------------------------------------------------ END ---------------------------------------------------------

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Done.", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )

