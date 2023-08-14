# gPKPDviz


## Mission

gPKPDviz is a dedicated R Shiny application (app) for real-time simulation, visualization, and assessment of  Pharmacokinetic/Pharmacodynamic (PK/PD) models, with mrgsolve as the simulation engine.

Packaged in an easy-to-use R-Shiny interface, gPKPDviz generates a virtual population with complex dosing & sampling scenarios, which is designed to effectively assess the impact of covariates and dosing regimens on PK/PD endpoints.

## Getting Started

The user has the option of using a local or web version of the application. If running a large amount of simulations we recommend setting up a local version since this will allow for best performance of the application. However, if interested in familiriazing yourself with the tool feel free to use the web version without having to install the local version. 

Web Version - https://cpmns-shiny.shinyapps.io/gPKPDviz/

### Setting Up Local Version - Basics

To get started using the tool, please clone the code to your local machine using standard Git clone command: 

```
git clone https://code.roche.com/velasqe2/cp-pkpd-sim-app.git

```

Alternatively, you could click download and download a compressed version of the app. Once downloaded, uncompress and save at desired location. 

### Docker

In order to create a reproducible and easy to load application, gPKPDviz is locally distributed as a docker image. A docker image contains all the code, libraries, tools and dependencies to make an application work and docker as a program allows you to use these instructions to render a container which ultimately hosts the application. 

If interested in learning more about Docker please review the following documentation: 

https://docs.docker.com/get-started/overview/

You will need to have Docker installed on your system to run gPKPDviz. 

#### Install Docker

Please install Docker and Docker Desktop using the following link: 

https://www.docker.com/products/docker-desktop/

#### Build and Render Docker Image Using Compose YAML 

In order to automatically build and render the application, open a terminal and navigate to where you have the local version of the application saved. You should see a docker-compose.yml file, next run the command on the terminal: 

```
docker-compose -f docker-compose.yml --project-name gpkpdviz-app up -d --build

```

This should initiate the app building/rendering process which depending on the speed of the system could take ~30min. Once the app has been built, you can launch the application under a new container using docker desktop.


Once running, using your preferred browser (we recommend chrome) navigate to:

http://localhost:3838/

Where you will be able to access the app. 


#### Turning on/off Docker Container

Turning off: 
To close the application and the docker container running the application open Docker Desktop and navigate to the containers tab. Under actions and next to the running container click the stop button to stop the container. 

Turning on:
After initial setup to turn on the application/container open Docker Desktop and navigate to the containers tab. Next to the container and under actions click the start button which will start your application under a local Docker container. Then using your local browser navigate to:  

http://localhost:3838/


## Use Case Examples

To demonstrate how to use the application, we have included a couple of examples that can help the user get started. All the demonstrations are based on the PK simulation of Perjeta (Pertuzumab), which is a monoclonal antibody for the treatment of HER2-positive metastatic breast cancer. The relevant models and data used in the examples can be found in the supplementary file of the tutorial in CPT-PST (link will be available once the tutorial is published).  

### Example 1: Covariate Impact Based on User-Provided Data

The first example assesses the impact of covariate on PK based on the user-uploaded or user-defined data. 

<img src="1.gif">

**Description of Example 1:** 
 
1. Upload the Mrgsolve model for perjeta popPK simulation; select time unit of the model; select model covariates (albumin [ALBU] and lean body weight [LBW]); set seed.
2. Upload population data from the external dataset in csv format (ALBU and LBW for 477 actual individuals); expand to 500 individuals by sampling-with-replacement; create binned variables for ALBU and LBW.
3. Upload dosing regimen from the external dataset in csv format (8 cycles of BASE regimen, i.e., 420 mg every 3-week cycle following an initial loading dose of 840 mg, given as IV infusions over 1-hour for 840 mg or 30-min for 420 mg for a total of 8 cycles).
4. Define sampling time manually (unequal time interval: 0, 30-min, 90-min, and each day from day-1 to day-21 after each dose).
5. Run and customize simulations.
6. Compare PK profile (median with 90% CI) and exposure metrics (AUC, Cmax, Ctrough) at cycle 8 across 4 bins of ALBU; calculate and compare the % of patients with Ctrough above 20 μg/mL threshold across 4 bins of ALBU.


### Example 2: Covariate Impact Based on App-Generated Data

The second example assesses the impact of covariate on PK based on the app-generated data.

<img src="2.gif">

**Description of Example 2:** 

1. Upload the Mrgsolve model for perjeta popPK simulation; select time unit of the model; select model covariates (ALBU and LBW); set seed.
2. Simulate population data within the app, assuming multivariate normal distribution (ALBU and LBW for 500 virtual patients); create binned variables for ALBU and LBW.
3. Simulate dosing regimen within the app (8 cycles of BASE regimen). 
4. Simulate sampling time within the app (equal time interval: every 12 hours for 24 weeks).
5. Run and customize simulations.
6. Compare PK profile (median with 90% CI) and exposure metrics (AUC, Cmax, Ctrough) at cycle 8 across 4 bins of ALBU; calculate and compare the % of patients with Ctrough above 20 μg/ml threshold across 4 bins of ALBU.

### Example 3: Dose Delay Impact Based on User-Provided Data


The third example assesses the impact of dose delay on PK based on user-uploaded or user-defined data.

<img src="3.gif">

**Description of Example 3:** 

1. Upload the Mrgsolve model for perjeta popPK simulation; select time unit of the model; select model covariates (ALBU and LBW); set seed.
2. Upload population data from the external dataset in csv format (ALBU and LBW for 477 actual individuals).
3. Upload dosing regimens from the external datasets in csv format (8 cycles of BASE regimen).
4. Define sampling time manually (BASE regimen: 0, 30-min, 90-min, and each day from day-1 to day-21 after each dose).
5. Repeat step 3 and 4 for DL6W regimen (8 cycles of dose delay regimen DL6W; dosing interval between the 3rd and 4th dose is extended to 6-week, and a reloading dose of 840 mg is given at 4th dose; one sample right before the delayed 4th dose was added automatically by the app to capture cycle 3 Ctrough).
6. Run and customize simulations.
7. Compare PK profile (median with 90% CI) and exposure metrics (Ctrough) at cycle 3 and cycle 8 between BASE and DL6W regimens; calculate and compare the % of patients with Ctrough above 20 μg/ml threshold at cycle 3 and cycle 8 between BASE and DL6W regimens.

### Example 4: Individual Simulations

The fourth example performs individual simulations based on post-hoc parameters, individual dosing log and the actual sampling time points.


<img src="4.gif">

**Description of Example 4:** 

1. Upload the Mrgsolve model for perjeta individual PK simulation; select time unit of the model; select model covariates (in this case, select CLind, Qind, V1ind, V2ind as post-hoc parameters).
2. Upload population data from the external dataset in csv format (CLind, Qind, V1ind, V2ind from 4 individuals).
3. Upload dosing and sampling data from the external dataset in csv format (individual dosing log composes of the evid = 1 records; actual sampling time points composes of the evid = 0 records)
4. Simulate sampling time within the app (equal interval with fine time grid: every 0.5 hours for 120 days). The individual sampling time thus includes both the actual sampling time points and the app-simulated fine time grid.
5. Run and customize simulations. 
6. Plot individual PK simulations at the actual sampling time points, augmented with the smooth line from the fine time grid.




