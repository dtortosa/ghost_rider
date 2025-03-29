#!/usr/bin/env Rscript

#This is done to have the possibility to run this script as an executable: 'chmod +x myscript.R' and then ' ./myscript.R'. If you run the script as 'R CMD BATCH myscript.R', i THINK this is not used, because it is annotated. 
	#https://www.jonzelner.net/statistics/make/docker/reproducibility/2016/05/31/script-is-a-program/

#In case you run this script as an executable, you can save the output without warnings "./myscript.R > myscript.Rout" or with errors "./myscript.R &> myscript.Rout"
	#https://askubuntu.com/questions/420981/how-do-i-save-terminal-output-to-a-file




#############################################################################
####################### SECOND CHALLENGE ####################################
#############################################################################



#################################################################
####################### REQUIRED PACKAGES #######################
#################################################################

require(plyr) #for apply functions across lists and data.frames. This is better than apply, because split rows of a data.frame without converting into matrix or array. In that way you can use "$" to call columns. In addition, you can save the output as a data frame or a list.
require(dplyr) #for using the function "bind_rows", which binds the data.frames of a list by rows



#####################################
######## CODE THE SIMULATOR #########
#####################################

##set the seed for reproducibility
set.seed(7645354)

##write the function
#for debugging
#n_sim = 10
#grid_size = c(5,5)
#start_point = c(3,3)
#step_size = c(1,2,5,6)
#n_steps = 5
#additional_output = TRUE
#steps_output=c(2,4)
ghost_rider = function(n_sim, grid_size, start_point, step_size, n_steps, additional_output=FALSE, steps_output=NULL){

	#Ghost Rider: Function to simulate the movement of one Pac-man's ghost:
		#n_sim: Number of simulations.
		#grid_size: Size of the grid where the ghost moves. The grid can have similar or different number of rows and columns.
		#start_point: Starting point of the ghost within the grid.
		#step_size: Size of the steps. The size can be smaller, equal or higher than the grid size. The ghost has the same probability to use any of the sizes specified.
		#n_steps: Number of steps in each simulation.
		#additional_output=FALSE: Logical for the need to save the output (final distance) of additional steps, not just for the last step.
		#steps_output=NULL: The additional steps for which the distance respect to the start has to be saved.

	##set all the possibilities of movement
	#directions
	ghost_directions = c("forward", "back", "left", "right")

	#get all possible combinations of step size and direction of movement
	mov_options = expand.grid(ghost_directions, step_size, stringsAsFactors=TRUE)
		#stringsAsFactors: specifying if character vectors are converted to factors.
	colnames(mov_options) = c("direction", "step_size")
	#check
	check_1 = nrow(mov_options) == length(ghost_directions) * length(step_size)

	#shuffle the order of the rows to reduce the clusters of directions and step sizes categories
	mov_options = mov_options[sample(x=1:nrow(mov_options), size=nrow(mov_options), prob=NULL, replace=FALSE),]
		#prob is null, so we do not set different weights for obtaining elements from the vector
			#table(sample(1:2, size=100, prob=c(0.5, 0.5), replace=TRUE))
			#table(sample(1:2, size=100, prob=NULL, replace=TRUE))
			#in both cases, around half of the times 1 is selected.
		#we do not want replacement because we just want to peak each row (i.e., each direction / step size combination) one time.

	##start the simulations	
	#create a dataframe with the simulation ids
	sim_ids_df = data.frame(sim_id=1:n_sim)

	#write function for performing each simulation
	#for debugging
	#selected_sim = sim_ids_df[1,]
	sim_function = function(selected_sim){

		#save the sim id
		selected_sim_id = selected_sim

		#open object for checking wrapping
		wrapping = FALSE #default is false, unless we detect a walk crossing any of the edges

		#create and empty data.frame to save the output for additional steps if indicated in the arguments
		if(additional_output == TRUE){

			#create a data.frame with as many columns as the number of steps for which we need the output
			additional_output_df = data.frame(matrix(data=NA, ncol=length(steps_output)))
			
			#change the column names
			colnames(additional_output_df) = paste("dist_step_", steps_output, sep="")
		}

		#simulate all the steps for the selected simulation
		for(i in 1:n_steps){

			#select the [i] step
			selected_step = i

			#We are setting an N by N grid, which is 5 by 5 in the example used for debugging. Left-right and top-bottom are connected as this is a torus. Therefore, the ghost can move from 5 to 1 and from 1 to 5. For example: i) in (1,5), 1 step to the right would lead to (1,1); ii) in (5,2) 1 step forward would lead to (1,2).
			#Note that for calculating the distance respect to the starting point, we will use the closest path connecting the two points independently of the wrapping. For example, starting at (3,3) and ending at (5,5) would be 2 rows + 2 columns = 4 steps. 

			#index of the selected direction and step size
			index_mov = sample(x=1:nrow(mov_options), size=1, prob=NULL, replace=FALSE)
				#prob is null, so we do not set different weights for obtaining elements from the vector
					#table(sample(1:2, size=100, prob=c(0.5, 0.5), replace=TRUE))
					#table(sample(1:2, size=100, prob=NULL, replace=TRUE))
					#in both cases, around half of the times 1 is selected.
				#we do not care about replacement in this case because we are just taking 1 value of "x", so no replacement is needed. In the next iteration (i.e., next step), sample will be run again with all the movement options intact.

			#extract the direction and step size
			selected_dir = mov_options[index_mov,]$direction
			selected_step_size_raw = mov_options[index_mov,]$step_size

			#we calculate the modular division of step size and grid size in case the step is bigger than the grid
			selected_step_size_row = selected_step_size_raw%%grid_size[1]
			selected_step_size_col = selected_step_size_raw%%grid_size[2]
				#we are using modular division (%%), i.e., divide the step size by the grid size (number of rows and columns) and return the remainder. 
					#If the step is smaller than the grid, the step size remains unchanged.
						#2%%5=2
					#if the step size is equal or bigger than the grid size, the result would be the difference respect to the previous multiple of the grid size:
						#10%%5=0: In this case, 10 is multiple of 5 (grid size), the next position is exactly the same than the original because it just did two cycles around the torus.
						#11%%5=11: In this case, the next position is one before or after the original one, because the ghost did two cycles and then one more step in the selected direction.
			
			##select the current position
			#if [i] step is the first step
			if(selected_step==1){

				#the current position is the starting position
				current_pos = start_point
			} else { #if not

				#current position is the previous new position obtained in the previous iteration
				current_pos = new_position
			}


			##select the next coordinate after the selected step
			#if the selected direction is back
			if(selected_dir == "back"){

				#subtract the selected step size from the first coordinate (we are moving backwards, less rows)
				new_row = current_pos[1] - selected_step_size_row
				
				#if the new row is 1 or higher, i.e., above the lower grid limit
				if(new_row >= 1){

					#save this as the new first coordinate or row
					current_pos[1] = new_row

					#indicate that the extreme was reached for the cases with a step size equal or higher than the grid
					if(selected_step_size_raw>=grid_size[1]){
						wrapping = TRUE
					}
				} else {

					#if not, the ghost has surpass the lower limit of the grid and has reached the other side
					current_pos[1] = grid_size[1] - abs(new_row)

					#indicate that the extreme was reached
					wrapping = TRUE
				}
			}

			#if the selected direction is forward
			if(selected_dir == "forward"){

				#subtract the selected step size from the first coordinate (we are moving forward, more rows)
				new_row = current_pos[1] + selected_step_size_row
				
				#if the new row is equal or lower than the upper limit of the grid
				if(new_row <= grid_size[1]){

					#save this as the new first coordinate or row
					current_pos[1] = new_row
					
					#indicate that the extreme was reached for the cases with a step size equal or higher than the grid
					if(selected_step_size_raw>=grid_size[1]){
						wrapping = TRUE
					}
				} else {

					#if not, the ghost has surpass the upper limit of the grid and has reached the other side
					current_pos[1] = abs(new_row) - grid_size[1]

					#indicate that the extreme was reached
					wrapping = TRUE
				}
			}
			#if the selected direction is back
			if(selected_dir == "left"){

				#subtract the selected step size from the second coordinate (we are moving to the left, less columns)
				new_column = current_pos[2] - selected_step_size_col
				
				#if the new column is 1 or higher, i.e., right of the left limit of the grid
				if(new_column >= 1){

					#save this as the new second coordinate or column
					current_pos[2] = new_column

					#indicate that the extreme was reached for the cases with a step size equal or higher than the grid
					if(selected_step_size_raw>=grid_size[2]){
						wrapping = TRUE
					}
				} else {

					#if not, the ghost has surpass the left limit of the grid and has reached the other side
					current_pos[2] = grid_size[2] - abs(new_column)

					#indicate that the extreme was reached
					wrapping = TRUE
				}
			}

			#if the selected direction is right
			if(selected_dir == "right"){

				#subtract the selected step size from the second coordinate (we are moving to the right, more columns)
				new_column = current_pos[2] + selected_step_size_col
				
				#if the new row is equal or lower than the upper limit of the grid
				if(new_column <= grid_size[2]){

					#save this as the new first coordinate or row
					current_pos[2] = new_column

					#indicate that the extreme was reached for the cases with a step size equal or higher than the grid
					if(selected_step_size_raw>=grid_size[2]){
						wrapping = TRUE
					}
				} else {

					#if not, the ghost has surpass the upper limit of the grid and has reached the other side
					current_pos[2] = abs(new_column) - grid_size[2]

					#indicate that the extreme was reached
					wrapping = TRUE
				}
			}

			#save the new position
			new_position = current_pos

			#if additional outputs for intermediate steps need to be saved
			if(additional_output == TRUE){

				#and the [i] step is one of these intermediate steps to be saved
				if(selected_step %in% steps_output){

					#calculate distance respect the starting point
					intermediate_distance = abs(new_position[1] - start_point[1]) + abs(new_position[2] - start_point[2])

					#save the distance of the corresponding intermediate step
					additional_output_df[,paste("dist_step_", selected_step, sep="")] = intermediate_distance
				}
			}
		}

		#calculate distance respect the starting point
		final_distance = abs(new_position[1] - start_point[1]) + abs(new_position[2] - start_point[2])

		#extract the last coordinates
		last_row = new_position[1]
		last_column = new_position[2]

		##combine all the results
		#if additional outputs for intermediate steps need to be saved
		if(additional_output == TRUE){

			#add the additional output df
			final_results = cbind.data.frame(selected_sim_id, wrapping, additional_output_df, last_row, last_column, final_distance, check_1)
		} else { #if not

			#not add the additional output df
			final_results = cbind.data.frame(selected_sim_id, wrapping, last_row, last_column, final_distance, check_1)
		}

		#return these results
		return(final_results)
	}

	##run the function with ddply
	#apply
	sim_results = ddply(.data=sim_ids_df, .variables="sim_id", .fun=sim_function, .inform=TRUE, .parallel=FALSE, .paropts=NULL)
		#".inform=TRUE" generates and shows the errors. This increases the computation time, BUT is very useful to detect problems in your analyses.
		#".parallel" to paralelize with foreach. 
		#".paropts" is used to indicate additional arguments in for each, specially interesting for using the .export and .packages arguments to supply them so that all cluster nodes have the correct environment set up for computing. 

	#return the resulting data frame
	return(sim_results)
}



####################################
####### RUN THE SIMULATIONS ########
####################################

##run simulations for the two first questions
#run the simulation
results_sim_1 = ghost_rider(n_sim=10000, grid_size=c(9,9), start_point=c(5,5), step_size=c(1), n_steps=10)
summary(results_sim_1)

#check the stability of the average distance
stb_avg_sim_1 = NULL
for(i in 1:nrow(results_sim_1)){ #for each simulation

	#calculate the average of the final distance considering all the previous simulations
	stb_avg_sim_1 = append(stb_avg_sim_1, mean(results_sim_1[1:i,]$final_distance))

}
plot(stb_avg_sim_1, type="l") #high stability after 4000 simulations, so more simulations are not informative

#calculate the mean of the distance respect to the starting point across simulations
mean(results_sim_1$final_distance)

#calculate the SD of the distance respect to the starting point across simulations
sd(results_sim_1$final_distance)

#save
write.table(results_sim_1, file=gzfile("/media/dftortosa/Windows/Users/dftor/Documents/diego_docs/industry/data_incubator/coding_challenge/challenge_2/results/results_sim_1.txt.gz"), col.names=TRUE, row.names=FALSE, sep="\t")
	#results_sim_1 = read.table("/media/dftortosa/Windows/Users/dftor/Documents/diego_docs/industry/data_incubator/coding_challenge/challenge_2/results/results_sim_1.txt.gz", sep="\t", header=TRUE)


##run simulations for the third and fourth questions
#run the simulation
results_sim_2 = ghost_rider(n_sim=10000, grid_size=c(9,9), start_point=c(2,2), step_size=c(1,2), n_steps=10)
summary(results_sim_2)

#check the stability of the average distance
stb_avg_sim_2 = NULL
for(i in 1:nrow(results_sim_2)){ #for each simulation

	#calculate the average of the final distance considering all the previous simulations
	stb_avg_sim_2 = append(stb_avg_sim_2, mean(results_sim_2[1:i,]$final_distance))

}
plot(stb_avg_sim_2, type="l") #high stability after 4000 simulations, so more simulations are not informative

#the average distance is ~ twice larger compared to the first simulation
mean(results_sim_2$final_distance)
	#This makes sense because the step size can be twice larger in this case.

#calculate the proportion of simulations for which the ghost crossed the edges respect to the total number of simulations
length(which(results_sim_2$wrapping == TRUE)) / nrow(results_sim_2)
	#the probability is high, probably due to the small grid size and larger step size

#indeed, increasing the grid size decreases the probability of crossing the edges
results_sim_2_1 = ghost_rider(n_sim=10000, grid_size=c(18,18), start_point=c(2,2), step_size=c(1,2), n_steps=10)
length(which(results_sim_2_1$wrapping == TRUE)) / nrow(results_sim_2_1)

#calculate the average distance for those walks crossing the edges
avg_edge_walks = mean(results_sim_2[which(results_sim_2$wrapping == TRUE),]$final_distance) 

#calculate the average distance for those walks not crossing the edges
avg_no_edge_walks = mean(results_sim_2[which(results_sim_2$wrapping == FALSE),]$final_distance)

#calculate the difference
avg_edge_walks - avg_no_edge_walks
	#larger distance for walks that wrap. This also makes sense because these walks are more likely to more far away from the starting point.

#save
write.table(results_sim_2, file=gzfile("/media/dftortosa/Windows/Users/dftor/Documents/diego_docs/industry/data_incubator/coding_challenge/challenge_2/results/results_sim_2.txt.gz"), col.names=TRUE, row.names=FALSE, sep="\t")
	#results_sim_2 = read.table("/media/dftortosa/Windows/Users/dftor/Documents/diego_docs/industry/data_incubator/coding_challenge/challenge_2/results/results_sim_2.txt.gz", sep="\t", header=TRUE)


##run simulations for the fifth question
#run the simulation
results_sim_3 = ghost_rider(n_sim=10000, grid_size=c(15,15), start_point=c(2,7), step_size=c(1,2), n_steps=25)
summary(results_sim_3)

#check the stability of the average distance
stb_avg_sim_3 = NULL
for(i in 1:nrow(results_sim_3)){ #for each simulation

	#calculate the average of the final distance considering all the previous simulations
	stb_avg_sim_3 = append(stb_avg_sim_3, mean(results_sim_3[1:i,]$final_distance))

}
plot(stb_avg_sim_3, type="l") #high stability after 4000 simulations, so more simulations are not informative

#select those simulations with a final distance >= 6
subset_six_dist = results_sim_3[which(results_sim_3$final_distance >= 6),]

#select those simulations for which the final distance >= 6 AND the ghost crossed the edges
subset_six_dist_wr = results_sim_3[which(results_sim_3$final_distance >= 6 & results_sim_3$wrapping == TRUE),]

#calculate the conditional probability of crossing the edges given that the final distance respect to the start is 6 or higher
cond_prob_wrap = nrow(subset_six_dist_wr) / nrow(subset_six_dist)
cond_prob_wrap
#check by calculating using a different approach
cond_prob_wrap_check = (nrow(subset_six_dist_wr) / nrow(results_sim_3)) / (nrow(subset_six_dist) / nrow(results_sim_3))
	#the probability of reaching a distance of 6 AND cross the edges divided by the probability of just reaching a distance of 6.
		#https://www.statology.org/conditional-probability-in-r/
round(cond_prob_wrap_check, 5) == round(cond_prob_wrap, 5)
	#the probability of wrapping for walks reaching 6 of distance is high. This makes sense because walks reaching the edges are more likely to end up farther away from the start.

#indeed the probability of wrapping is smaller for walks reaching less than 6 of distance
nrow(results_sim_3[which(results_sim_3$final_distance < 6 & results_sim_3$wrapping == TRUE),]) / nrow(results_sim_3[which(results_sim_3$final_distance < 6),])

#save
write.table(results_sim_3, file=gzfile("/media/dftortosa/Windows/Users/dftor/Documents/diego_docs/industry/data_incubator/coding_challenge/challenge_2/results/results_sim_3.txt.gz"), col.names=TRUE, row.names=FALSE, sep="\t")
	#results_sim_3 = read.table("/media/dftortosa/Windows/Users/dftor/Documents/diego_docs/industry/data_incubator/coding_challenge/challenge_2/results/results_sim_3.txt.gz", sep="\t", header=TRUE)


##run simulations for the sixth question
#run the simulation
results_sim_4 = ghost_rider(n_sim=10000, grid_size=c(25,25), start_point=c(13,13), step_size=c(1,2,3), n_steps=100, additional_output=TRUE, steps_output=c(50))
summary(results_sim_4)

#check the stability of the average distance
stb_avg_sim_4 = NULL
for(i in 1:nrow(results_sim_4)){ #for each simulation

	#calculate the average of the final distance considering all the previous simulations
	stb_avg_sim_4 = append(stb_avg_sim_4, mean(results_sim_4[1:i,]$final_distance))

}
plot(stb_avg_sim_4, type="l") #high stability after 5000 simulations, so more simulations are not informative

#calculate the average of the difference between the distance at 50 steps respect to final distance at 100 steps
mean(results_sim_4$final_distance - results_sim_4$dist_step_50)

#the distance is small, but this is expected because the grid is not very big, so the ghost can easily reach the edges
length(which(results_sim_4$wrapping == TRUE)) / nrow(results_sim_4)
	#The proportion of simulations reaching the border is 0.9344

#indeed, a decrease in the step size increases the difference between the starting point and the final distance
results_sim_4_1 = ghost_rider(n_sim=10000, grid_size=c(25,25), start_point=c(13,13), step_size=c(1,2), n_steps=100, additional_output=TRUE, steps_output=c(50))
mean(results_sim_5$final_distance - results_sim_5$dist_step_50)

#even more increase after increasing the size of the grid
results_sim_4_2 = ghost_rider(n_sim=10000, grid_size=c(50,50), start_point=c(13,13), step_size=c(1,2), n_steps=100, additional_output=TRUE, steps_output=c(50))
results_sim_4_3 = ghost_rider(n_sim=10000, grid_size=c(70,70), start_point=c(13,13), step_size=c(1,2), n_steps=100, additional_output=TRUE, steps_output=c(50))
mean(results_sim_6$final_distance - results_sim_6$dist_step_50)
mean(results_sim_7$final_distance - results_sim_7$dist_step_50)

#all this suggests that the grid in the fourth simulation is too small to let the ghost separate from the starting point without reaching the edges

#save
write.table(results_sim_4, file=gzfile("/media/dftortosa/Windows/Users/dftor/Documents/diego_docs/industry/data_incubator/coding_challenge/challenge_2/results/results_sim_4.txt.gz"), col.names=TRUE, row.names=FALSE, sep="\t")
	#results_sim_4 = read.table("/media/dftortosa/Windows/Users/dftor/Documents/diego_docs/industry/data_incubator/coding_challenge/challenge_2/results/results_sim_4.txt.gz", sep="\t", header=TRUE)