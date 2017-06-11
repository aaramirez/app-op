# config.R
#
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


#"MV" mean-variance (Markowitz) portfolio
#"CVAR" mean-conditional Value at Risk portfolio
#"QLPM" mean-quadratic-lower-partial-moment portfolio
#"SPS" Stone, Pedersen and Satchell type portfolios
#"MAD" mean-absolute-deviance Portfolio

PORTTYPE_CONF<-c("MV"="MV",
            "CVAR"="CVAR",
            "QLPM"="QLPM",
            "SPS"="SPS",
            "MAD"="MAD")

#"minRisk" minimizes the risk for a given target return
#"maxReturn" maximizes the return for a given target risk
#"objRisk" gives the name of an alternative objective function

PORTOPTIMIZE_CONF<-c("minRisk"="minRisk",
                     "maxReturn"="maxReturn",
                     "objRisk"="objRisk")


#Model Slot: specifies the type of estimator
#List Entry:
#  estimator
#"covEstimator" Covariance sample estimator
#"kendallEstimator" Kendall's rank estimator
#"spearmanEstimator" Spearman's rank estimator
#"mcdEstimator" Minimum covariance determinant estimator
#"mveEstimator" Minimum volume ellipsoid estimator
#"covMcdEstimator" Minimum covariance determinant estimator
#"covOGKEstimator" Orthogonalized Gnanadesikan-Kettenring
#"shrinkEstimator" Shrinkage estimator
#"baggedEstimator" Bagged Estimator
#"nnveEstimator" Nearest neighbour variance estimator
#The list entry $estimator from the @model slot requires a string denoting
#the function name of the covariance estimator that should be used for
#estimating risk. InMarkowitz’ mean-variance portfolio model, type="MV",
#the default function covEstimator() is used, which computes the sample
#column means and the sample covariance matrix of the multivariate
#assets data series.

PORTCOVESTIMATOR_CONF<-c("covEstimator"="covEstimator",
                         "kendallEstimator"="kendallEstimator",
                         "spearmanEstimator"="spearmanEstimator",
                         "mcdEstimator"="mcdEstimator",
                         "mcdEstimator"="mcdEstimator",
                         "covMcdEstimator"="covMcdEstimator",
                         "covOGKEstimator"="covOGKEstimator",
                         "shrinkEstimator"="shrinkEstimator",
                         "baggedEstimator"="baggedEstimator",
                         "nnveEstimator"="nnveEstimator")


#"solveRquadprog" Rmetrics default QP solver
#"solveRglpk" Rmetrics default LP solver
#"solveRshortExact" analytical short selling QP solver
#"solveRipop" alternative QP solver
#"solveRlpSolveAPI" alternative LP solver
#"solveRsymphony" alternative LP solver
#"solveRsocp" QP solver for quadratic constraints
#"solveRdonlp2" NL solver for non-linear constraints

SOLVER_CONF<-c("solveRquadprog"="solveRquadprog",
               "solveRquadprog.CLA"="solveRquadprog.CLA",
               "solveRglpk.CVAR"="solveRglpk.CVAR",
               "solveRglpk.MAD"="solveRglpk.MAD",
               "solveRshortExact"="solveRshortExact",
               "solveRipop"="solveRipop",
               "solveRlpSolveAPI"="solveRlpSolveAPI",
               "solveRsymphony"="solveRsymphony",
               "solveRsocp"="solveRsocp",
               "solveRsolnp"="solveRsolnp",
               "solveRdonlp2"="solveRdonlp2",
               "solveRampl.MV"="solveRampl.MV",
               "solveRampl.CVAR"="solveRampl.CVAR")


CONSTRAINS_CONF<-c("LongOnly"="LongOnly",
                   "Short"="Short")

PARAMS_CONF<-c("Portafolio Eficiente"="ep",
               "Portafolio mayor Retorno/Riesgo"="tp",
               "Portafolio Varianza Mínima"="mv",
               "Portafolio Retorno Máximo"="mr")






