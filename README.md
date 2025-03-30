# Ghost Rider

In this project, I developed a R program to simulate the movement of a Pac-man's ghost. This program is able to simulate thousands of iterations to explore the probability of certain outcomes. This project is associated with the Data Science Certification I completed at the Data Incubator ([Pragmatic Institue](https://www.credly.com/badges/f789cc0f-a7fc-4c04-adaf-8b72ca5f421c)).

The program includes the following arguments:

- n_sim: Number of simulations.
- grid_size: Size of the grid where the ghost moves. The grid can have similar or different number of rows and columns.
- start_point: Starting point of the ghost within the grid.
- step_size: Size of the steps. The size can be smaller, equal or higher than the grid size. The ghost has the same probability to use any of the sizes specified.
- n_steps: Number of steps in each simulation.
- additional_output=FALSE: Logical for the need to save the output (final distance) of additional steps, not just for the last step.
- steps_output=NULL: The additional steps for which the distance respect to the start has to be saved.

The [script](scripts/challenge_2.R) can be found in the ```script``` folder, along with the outputs ([Rout file](scripts/challenge_2.Rout) and [plots](scripts/Rplots.pdf)).
