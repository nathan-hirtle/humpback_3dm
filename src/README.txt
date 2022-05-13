The src folder contains all of the code files used in the analyses, which reference files in the data_minimized folder.

	The 'code-and-guidelines-to-develop-and-scale-3DMs' subfolder contains code and instructions for building, scaling, and identifying optimal 3D models for a marine mammal species.
		This is the folder that instructs a user in constructing, scaling, and evaluating 3D models.

	The 'adults-vs-juvenile_body-proportions' subfolder contains code used to compare the body proportions between adults and juveniles of the scaled humpback whale models and generate figure 3.

	The 'BAI-vs-volume' subfolder contains:
		a Blender file to generate scaled body volume and body area index values for a whale of poor body condition and high body condition
		an R file to compare the difference between the poor/high body condition whales using the 2 different metrics of body condition, and also generate figure S1

	The 'elliptical-vs-3DM-method' subfolder contains:
		an R file which generates a dataset that can be ingested by a Blender file to scale an elliptical model using the optimal models selected
		a Blender file which scales "full" elliptical models and elliptical models using the 'optimal models' generated in the above file
		an R file which calculates the error of the elliptical models relative to the full 3D models
		an R file which compares the full 3D and optimal 3D models to the full elliptical and optimal elliptical models, and generates figure 5.


	The 'heights-vs-HW-ratios' subfolder contains:
		an R file to calculate the mean HW ratios of the Australian dataset and generate a dataset containing both heights and widths.
		a Blender file which scales a 3D model with the Australian dataset (1) using true heights and widths and (2) using widths and height-width ratios and calculates volume from them.
		an R file which calculate the difference in volume between the two methods of scaling described above.

	The 'optimal-model-selection' subfolder contains:
		an R file to calculate mean morphometric values for adults and juveniles in the adult dataset and generate 131072 combinations of these measurements for adults and juveniles
		a Blender file which scales the base model using each of the combinations described above, recording their volume for analyses
		an R file which identifies which of the above models have <5% error relative to the full model, identifies desired candidate models for further scaling, generates datasets which contain all desired combinations for all individuals in the NY dataset, and then evaluates the error of those candidate models after they are scaled in blender.
		a Blender file which scales the base 3D model using all individuals in the dataset using the candidate models described above
		a minimalist R file which generates figure 4
		a mnimialist R file which generates table S2

	The 'generate_figs' subfolder contains file which recreate all the figures and tables in the manuscript that are generated using code.