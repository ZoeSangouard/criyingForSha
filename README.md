# CryingForSha

Alfalfa (Medicago sativa) is commonly used as a companion species in intercropping systems around the world due to its nitrogen-fixing ability and other benefits it brings. Predicting alfalfa growth stages accurately is essential for an effective farm intercropping management. This study investigates the effects of shade on alfalfa varieties with different dormancy levels. It aims to observe different responses of alfalfa’s vegetative growth under low light conditions. This experiment is designed to  mimic field observations of alfalfa under durum wheat canopy. Morphological traits affected by a shading effect, including leaf area index, plant maximum height, numbers of ramification, leaf count and biomass were analyzed for different alfalfa varieties with two different dormancy levels. Understanding these dynamics could help re-think alfalfa varietal choices in intercropping systems, potentially reducing herbicide and fertilizer use while enhancing yields. This research addresses a crucial knowledge gap in alfalfa's response to low light conditions. The potential alfalfa development model presented in this study could serve as a framework to integrate post- cutting and light responses in alfalfa mixed croping  models. 

## Data acquisition

The long pocess of gathering,manipulating data goes through tow main parts:

- **Taking photos:** Explain the process of taking this photos 
- **Acquisition:** by using the **SupAgroBot** [scrit](https://github.com/oualidlamrini/SupAgroBot.git) 
  
## Data deployment 
Different tests and statistical models
 
 ## R-Script use
Script_distribution_Nb_feuilles : It is the script that, for each variety exposed to each light treatment, gives, for all the plants concerned, the number of ramifications according to the number of leaves they have (bar diagram). A normal law was fitted on for modeling.

Script_InLength_NbLeaves : This is the script that, for each variety exposed to each light treatment, gives, for all the plants concerned, the average length of internoeuds on a branch according to the number of leaves it has (graph with dots). A linear regression was fitted on for modeling.

Script_proportion_visual_phenotype : It is the script that gives representations in terms of percentages as to the visual phenotype of plants. It gives the distribution between the three phenotypes (B,M,E) between Control or Shadow plants and similarly for a fixed treatment between the different varieties.

Script_ramifications : It is the script that gives the number of branches per plant on all plants of the same variety exposed to the same light treatment (bar diagram). We could fit a normal law for modeling.

Script_simulation : It is the script that simulates, for each variety exposed to each light treatment, 15 plants with a certain cumulative branching length. It does so each time 10000 times and gives the average and standard deviation for each variety exposed to each light treatment.

Script_somme_ramifications : It is the script that for each variety exposed to each light treatment (comprising 15 plants)as the length of each branch of the plants to then give the total cumulative length of the branches and compare it to that obtained in the modeling simulation.