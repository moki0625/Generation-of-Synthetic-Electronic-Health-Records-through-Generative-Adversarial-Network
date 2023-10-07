# Generation-of-Synthetic-Electronic-Health-Records-through-Generative-Adversarial-Network

This work aims to implement a generative model for Type 2 diabetes electronic health records (EHRs) generation, the model is constructed by a **Kernelized AutoEncoder** and a **Generative Adversarial Network (GANs)**, it can be used to generate 10 continuous clinical variables *(triglycerides (TG), Creatinine (CREAT), High-density lipoprotein (COLHDL), Low-density lipoprotein (COLLDL), Total cholesterol (COLTOT), glycated hemoglobin (HbA1c), albumin/creatinine ratio (CAC), systolic blood pressure (EK201), diastolic blood pressure (EK202), and BMI (TT103))*.

The original data was collected from the **Information System for the Development of Research in Primary Care(SIDIAP)**, the study period was from *Jan 1st, 2013 to Dec 31st, 2017*, the whole process of implementation and results can be found in following [thesis](https://drive.google.com/file/d/1Fxl0pxWkKYTn53rL0XzoLHE1hyskkFWx/view?usp=sharing)

## Code Description

### Configuration
This work was implemented to run on [Tensorflow](https://www.tensorflow.org/) 2.9.1, the **data preparation** process was implemented by [R](https://www.r-project.org/) version 4.2.1 (2022-06-23) and the GANs model was implemented by [Python](https://www.python.org/) 3.10.12.

The model training was processed on NVIDIA GeForce RTX 3080 Ti gpu version.

### Data Preparation
1. Download the code in `~/Model/Data preparation/`.
2. Run `load_data.R`, `predict_variables.R`, `impute_variables.R`, `set_na_exitus.R` in order.

### AutoEncoder
1. Download the code file `autoencoder_imputed_data_kernel_matrix_masked_deaths_stacked_LSTM.ipynb` in `~/Model/`.
2. Run the code on the server.

### GANs
1. Download the code file `GANs_Model.ipynb` in `~/Model/`.
2. Run the code on the server.
