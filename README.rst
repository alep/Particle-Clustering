Particle-Clustering

This program is used to find clusters of molecules in configurations created in MD simulations. The program has two input files. The first file has 4 columns:

 * particle_id  particle_type  molecule_id  molecule_type

where all the columns are integer, in particular particle_id are in ascending id order starting from 1 until the number of lines in the file. Breaks otherwise (mental note: make it robust)


The second file has the following format:

 * particle_id pos_x pos_y pos_z particle_area list_of_neighbouring_particles

The list of neighbouring particles is a list of particle_id (integers -- the neighbours are calculated using voronoi). This file has a 64 limit on the 
columns.

Install: just run the make file

Running: ./runcluster.sh particle-file configuration-file
For example: ./runcluster.sh testdata/outtable.dat testdata/com_tail.dat

