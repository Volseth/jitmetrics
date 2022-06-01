#!/usr/bin/env Rscript
library("pROC")
library("randomForest")
library("naivebayes")
library("reshape")
library("e1071")
library("ScottKnottESD")
library("caret")
library("pracma")
library("PRROC")

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  stop("Provide path to SZZ-TSE directory.", call.=FALSE)
}
# set up environment directory etc.
DIR_PATH <- args[1]
print(DIR_PATH)
ROOT_PATH <- "data_results/"

SZZ_LABELS <- c("buggy_B2", "buggy_AG", "buggy_MA", "buggy_RA")
SZZ_BASELINE <- "buggy_RA"
CALCULATED_MEASURES <- c("auc", "precision", "recall", "f1measure", "gmean", "recall20", "fp", "fn", "waste_effort", "all_effort")
PROJECTS <- c("activemq", "camel", "derby", "geronimo", "hcommon", "hbase", "mahout", "openjpa", "pig", "tuscany")
CLASSIFIERS <- c("naive_bayes", "logistic_regression", "random_forest")
BOOTSTRAP_TIMES <- 1000

setwd(DIR_PATH)

source("code/packages/measures.R")
source("code/packages/imbalance.R")
source("code/packages/CBS.R")
source("code/packages/VarImportance.R")

banlist <- args[-1]
solution_dir <- paste(c(DIR_PATH, "/", ROOT_PATH, "results_", "balance_without_", paste(banlist, collapse = "_")), collapse="")
dir.create(file.path(solution_dir), showWarnings = FALSE)
collinearity_features <- read.csv(paste(ROOT_PATH, "collinearity.csv", sep=""))

store_result_to_frame <- function(result_frame, scores_vector){
	temp_frame <- data.frame(scores_vector)
	if (is.null(result_frame)){
		result_frame <- temp_frame
	}
	else {
		result_frame <- cbind(result_frame, temp_frame)
	}
	return(result_frame)
}

print("Banlist:")
print(banlist)
print("Your results will be saved in:")
print(solution_dir)

for (classifier in CLASSIFIERS){
	for (p in PROJECTS){
		# result preparation
		importance_rank_frame <- NULL

		filter_features <- as.vector(collinearity_features[p][,1])
		filter_features <- append(filter_features, SZZ_LABELS)
		filter_features <- append(filter_features, banlist)
		
		fn <- paste(c(ROOT_PATH, "data_csvs/", p, ".csv"), collapse="")
		fn2 <- paste(c(ROOT_PATH, "data_csvs2/", p, ".csv"), collapse = "")
		
		data <- read.csv(fn)
		raw_data <- read.csv(fn2)
		raw_data$lt <- raw_data$lt * raw_data$nf
		raw_data$nuc <- raw_data$nuc * raw_data$nf
		
		var_names <- names(data)
		var_names1 <- var_names[!var_names %in% filter_features]
		var_names_str <- paste(var_names1, collapse="+")

		for (szz_label in SZZ_LABELS){
			print(szz_label)
			result_frame <- NULL

			form <- as.formula(paste(szz_label, var_names_str, sep=" ~ "))
			print(paste(szz_label, var_names_str, sep=" ~ "))
			var_names2 <- append(var_names1, szz_label)
			var_names2 <- append(var_names2, SZZ_BASELINE)

			temp_data <- data[var_names2]
			temp_data$real_la <- raw_data$la #It's not used in model training
			temp_data$real_ld <- raw_data$ld #It's not used in model training

			auc_scores <- c()
			precision_scores <- c()
			recall_scores <- c()
			F1_scores <- c()
			recall20_scores <- c()
			gmean_scores <- c()
			oneway_r20_scores <- c()
			fp_scores <- c()
			fn_scores <- c()
			waste_lines_scores <- c()
			all_lines_scores <- c()

			# factorise labels
			buggy_labels <- factor(temp_data[szz_label][,1], order=TRUE, levels=c("clean", "buggy"))
			temp_data[szz_label][,1] <- buggy_labels
			buggy_real_labels <- factor(temp_data[SZZ_BASELINE][,1], order=TRUE, levels=c("clean", "buggy"))
			temp_data[SZZ_BASELINE][,1] <- buggy_real_labels

			print(paste(c("start experiment for", szz_label, classifier), collapse = " "))
			#print(colnames(temp_data))
			# start bootstrap runs
			for (i in 1:BOOTSTRAP_TIMES){
				set.seed(i); train_indices <- sample(nrow(temp_data), replace=TRUE)
				train_data <- temp_data[train_indices,]
				test_data <- temp_data[-unique(train_indices),]

				
				# Undersampling
				train_data <- undersampling(train_data, szz_label)
				

				# calculate the likelihood scores being "buggy" for changes in testing set
				if (classifier == "random_forest"){
					fit <- randomForest(form, train_data, ntree=100)
					prediction <- predict(fit, test_data, type="prob")
					prob <- prediction[,2]
				}
				
				if (classifier == "logistic_regression"){
					fit <- glm(form, train_data, family=binomial)
					prediction <- predict(fit, test_data, type="response")
					prob <- prediction
				}

				if (classifier == "naive_bayes"){
					fit <- naive_bayes(form, train_data)
					prediction <- predict(fit, test_data, type="prob")
					prob <- prediction[,2]
				}
					
				# calculate auc
				result <- roc(test_data[SZZ_BASELINE][,1], prob)
				auc_scores <- append(auc_scores, result["auc"][[1]][1])


				# calculate precision
				precision_score <- precision(test_data, prob, SZZ_BASELINE)
				precision_scores <- append(precision_scores, precision_score)

				# calculate recall
				recall_score <- recall(test_data, prob, SZZ_BASELINE)
				recall_scores <- append(recall_scores, recall_score)

				# calculate f1
				f_score <- F1(test_data, prob, SZZ_BASELINE)
				F1_scores <- append(F1_scores, f_score)

				# calculate gomeric mean
				gmean_score <- gmean(test_data, prob, SZZ_BASELINE)
				gmean_scores <- append(gmean_scores, gmean_score)

				# calculate cost effectiveness measure
				ordered_data <- get_ordered_data(test_data, prob)
				total_churn <- sum(test_data$real_la+test_data$real_ld)
				
				results <- calculate_cost_effectiveness2(ordered_data, total_churn, 0.2, "real_la", "real_ld", SZZ_BASELINE, "buggy")
				recall20 <- results[2]
				recall20_scores <- append(recall20_scores, recall20)
				
				# calculate wastes and misses
				waste_miss_results <- waste_miss(test_data, prob, SZZ_BASELINE)
				fp_scores <- append(fp_scores, waste_miss_results[1])
				fn_scores <- append(fn_scores, waste_miss_results[2])
				waste_lines_scores <- append(waste_lines_scores, waste_miss_results[3])
                  all_lines_scores <- append(all_lines_scores, waste_miss_results[4])					
			}

			
			# store auc results
			result_frame <- store_result_to_frame(result_frame, auc_scores)

			# store precision
			result_frame <- store_result_to_frame(result_frame, precision_scores)

			# store recall
			result_frame <- store_result_to_frame(result_frame, recall_scores)

			# store F1
			result_frame <- store_result_to_frame(result_frame, F1_scores)

			# store gmean
			result_frame <- store_result_to_frame(result_frame, gmean_scores)

			# store recall20
			result_frame <- store_result_to_frame(result_frame, recall20_scores)
			
			# store false positive, false negative, waste effort and overall effort
			result_frame <- store_result_to_frame(result_frame, fp_scores)
			result_frame <- store_result_to_frame(result_frame, fn_scores)
			result_frame <- store_result_to_frame(result_frame, waste_lines_scores)
			result_frame <- store_result_to_frame(result_frame, all_lines_scores)
			
			
			names(result_frame) <- CALCULATED_MEASURES
			result_fn <- paste(c(solution_dir, "/", p, "_", classifier, "_", szz_label, ".csv"), collapse="")
			
			write.csv(result_frame, result_fn, row.names=FALSE)
			
		}
	}
}
