# config.R

UPLOADFILETYPE_CONF<-c('text/csv',
                          'text/comma-separated-values',
                          'text/tab-separated-values',
                          'text/plain',
                          '.csv',
                          '.tsv'
                          )

UPLOADFILESEP_CONF<-c('Coma'=',',
                         'Punto y coma'=';',
                         'Tab'='\t')

UPLOADCOMILLAS_CONF<-c('Ninguna'='',
                          'Comilla doble'='"',
                          'Comilla simple'="'")

DEMOFILEURL_CONF<-"2tech.csv"
DEMOFILETEXT_CONF<-"2tech.csv"


RETURNTYPE_CONF<-c("Discreto"="discrete", "Continuo"="continuous")

QUANTILECALCTYPE_CONF<-c("1"="1", "2"="2", "3"="3",
                         "4"="4", "5"="5", "6"="6",
                         "7"="7", "8"="8", "9"="9")

COVCALCTYPE_CONF<-c("cov"="cov", "mve"="mve",
                    "mcd"="mcd", "MCD"="MCD",
                    "OGK"="OGK", "nnve"="nnve",
                    "shrink"="shrink", "bagged"="bagged")

# covEstimator uses standard covariance estimation,
# mveEstimator uses the function "cov.mve" from the MASS package,
# mcdEstimator uses the function "cov.mcd" from the MASS package,
# lpmEstimator returns lower partial moment estimator,
# kendallEstimator returns Kendall's rank estimator,
# spearmanEstimator returns Spearman's rankestimator,
# covMcdEstimator requires "covMcd" from package robustbase,
# covOGKEstimator requires "covOGK" from package robustbase,
# nnveEstimator uses builtin from package covRobust,
# shrinkEstimator uses builtin from package corpcor.
COVESTIMATOR_CONF<-c("covEstimator"="covEstimator", "mveEstimator"="mveEstimator",
                     "mcdEstimator"="mcdEstimator", "lpmEstimator"="lpmEstimator",
                     "kendallEstimator"="kendallEstimator", "spearmanEstimator"="spearmanEstimator",
                     "covMcdEstimator"="covMcdEstimator", "covOGKEstimator"="covOGKEstimator",
                     "nnveEstimator"="nnveEstimator", "shrinkEstimator"="shrinkEstimator")


