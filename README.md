# dsa_projeto01_talkingdata
Classification Model for https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection/data

# Files
- relatorio.RMD: RMarkDown script for generating the report.
- relatorio.pdf: pdf report.
- relatorio.html: html report.
- script.R: R script for all the steps in the project.
- plot_utils.R: R script containing R functions for generating ROC curve plots.
- predictions.R: R script for generating Kaggle competitions predictions using all 7 models.

# Google Drive
## At https://drive.google.com/drive/folders/1K8VgW-XENmoXwwRqB3otBFXuIBSluqqD?usp=sharing, there are 3 folders:
- Arquivos .rds: folder containing .rds files of the models generated (rf.mod.rds, reg.log.rds, reg.log2.rds, reg.log3.rds, knn.preds.rds, boost.mod.rds, bag.mod.rds) and intermediate data.frames used in the R script (df2.rds, df_treino.rds, df_teste.rds, final.rds, teste_final.rds).
- Bancos de Dados: folder containing a zip file with the complete train and test csv datasets from Kaggle.
- Envios Kaggle: folder containg a zip file with a csv file for the predictions of each model.
