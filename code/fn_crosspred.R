#---
# fn_crosspred.R
#
# This Rscript: is a modified version of dlnm::crosspred() from the dlnm package. 
#
# Dependencies...
#
# Produces...
# fn_crosspred()
#---

fn_crosspred <-function(basis,
                        model = NULL,
                        coef = NULL,
                        vcov = NULL,
                        model.link = NULL,
                        at = NULL,
                        from = NULL,
                        to = NULL,
                        by = NULL,
                        lag,
                        bylag = 1,
                        cen = NULL,
                        ci.level = 0.95,
                        cumul = FALSE) {

                ################################################################################
                # TYPE OF PREDICTION: PENALIZED GAM
                type <- "gam"
                
                #  EXTRACT ORIGINAL lag (DEPENDENT ON TYPE)
                origlag <- c(0,0)
                
                lag <- if(missing(lag)) origlag else mklag(lag)
                #
                # CHECKS ON lag AND bylag
                if(!all(lag==origlag) && cumul)
                        stop("cumulative prediction not allowed for lag sub-period")
                
                lagfun <- if(basis$dim==1L) NULL else basis$margin[[2]]$fun
                
                if(bylag!=1L && !is.null(lagfun) && lagfun=="integer")
                        stop("prediction for non-integer lags not allowed for type 'integer'")
                #
                # OTHER COHERENCE CHECKS
                if(is.null(model) && (is.null(coef) || is.null(vcov)))
                        stop("At least 'model' or 'coef'-'vcov' must be provided")
                if(!is.numeric(ci.level) || ci.level>=1 || ci.level<=0)
                        stop("'ci.level' must be numeric and between 0 and 1")
                #
                ################################################################################
                # SET COEF, VCOV CLASS AND LINK FOR EVERY TYPE OF MODELS
                #
                #
                # IF MODEL PROVIDED, EXTRACT FROM HERE, OTHERWISE DIRECTLY FROM COEF AND VCOV
                model.class <- NA
                #
                # CHECK
                if(length(coef)!=dim(vcov)[1] || any(is.na(coef)) || any(is.na(vcov)))
                        stop("coef/vcov not consistent with basis matrix. See help(crosspred)")
                #
                ##########################################################################
                # AT, PREDVAR, PREDLAG AND CENTERING
                #
                # RANGE
                range <- range(at)
                #
                # SET at, predvar AND predlag
                at <- dlnm:::mkat(at,from,to,by,range,lag,bylag)
                predvar <- if(is.matrix(at)) rownames(at) else at
                predlag <- dlnm:::seqlag(lag,bylag)
                #
                # DEFINE CENTERING VALUE (NULL IF UNCENTERED), AND REMOVE INFO FROM BASIS
                cen <- dlnm:::mkcen(cen, type, basis, range)
                #
                ################################################################################
                # PREDICTION OF LAG-SPECIFIC EFFECTS
                #
                # CREATE THE MATRIX OF TRANSFORMED CENTRED VARIABLES (DEPENDENT ON TYPE)
                Xpred <- dlnm:::mkXpred(type,basis,at,predvar,predlag,cen)
                #
                # CREATE LAG-SPECIFIC EFFECTS AND SE
                matfit <- matrix(Xpred%*%coef, length(predvar), length(predlag)) 
                matse <- matrix(sqrt(pmax(0,rowSums((Xpred%*%vcov)*Xpred))), length(predvar),
                                length(predlag)) 
                #
                # NAMES
                rownames(matfit) <- rownames(matse) <- predvar
                colnames(matfit) <- colnames(matse) <- outer("lag",predlag,paste,sep="")
                #
                ################################################################################
                # PREDICTION OF OVERALL+CUMULATIVE EFFECTS
                #
                # RE-CREATE LAGGED VALUES (NB: ONLY LAG INTEGERS)
                predlag <- dlnm:::seqlag(lag)
                #
                # CREATE THE MATRIX OF TRANSFORMED VARIABLES (DEPENDENT ON TYPE)
                Xpred <- dlnm:::mkXpred(type,basis,at,predvar,predlag,cen)
                #
                # CREATE OVERALL AND (OPTIONAL) CUMULATIVE EFFECTS AND SE
                Xpredall <- 0
                if(cumul) {
                        cumfit <- cumse <- matrix(0,length(predvar),length(predlag))
                }
                for (i in seq(length(predlag))) {
                        ind <- seq(length(predvar)) + length(predvar)*(i-1)
                        Xpredall <- Xpredall + Xpred[ind,,drop=FALSE]
                        if(cumul) {
                                cumfit[, i] <- Xpredall %*% coef
                                cumse[, i] <- sqrt(pmax(0,rowSums((Xpredall%*%vcov)*Xpredall)))
                        }
                }
                allfit <- as.vector(Xpredall %*% coef)
                allse <- sqrt(pmax(0,rowSums((Xpredall%*%vcov)*Xpredall)))
                #
                # NAMES
                names(allfit) <- names(allse) <- predvar
                if(cumul) {
                        rownames(cumfit) <- rownames(cumse) <- predvar
                        colnames(cumfit) <- colnames(cumse) <- outer("lag",seqlag(lag),paste,sep="")
                }
                #
                ################################################################################
                # CREATE THE OBJECT
                #
                # INITIAL LIST, THEN ADD COMPONENTS
                list <- list(predvar=predvar)
                if(!is.null(cen)) list$cen <- cen
                list <- c(list, list(lag=lag, bylag=bylag, coefficients=coef, vcov=vcov,
                                     matfit=matfit, matse=matse, allfit=allfit, allse=allse))
                if(cumul) list <- c(list, list(cumfit=cumfit, cumse=cumse))
                #
                # MATRICES AND VECTORS WITH EXPONENTIATED EFFECTS AND CONFIDENCE INTERVALS
                z <- qnorm(1-(1-ci.level)/2)
                if(!is.null(model.link) && model.link %in% c("log","logit")) {
                        list$matRRfit <- exp(matfit)
                        list$matRRlow <- exp(matfit-z*matse)
                        list$matRRhigh <- exp(matfit+z*matse)
                        list$allRRfit <- exp(allfit)
                        list$allRRlow <- exp(allfit-z*allse)
                        names(list$allRRlow) <- names(allfit)
                        list$allRRhigh <- exp(allfit+z*allse)
                        names(list$allRRhigh) <- names(allfit)
                        if(cumul) {
                                list$cumRRfit <- exp(cumfit)
                                list$cumRRlow <- exp(cumfit-z*cumse)
                                list$cumRRhigh <- exp(cumfit+z*cumse)
                        }
                } else {
                        list$matlow <- matfit-z*matse
                        list$mathigh <- matfit+z*matse
                        list$alllow <- allfit-z*allse
                        names(list$alllow) <- names(allfit)
                        list$allhigh <- allfit+z*allse
                        names(list$allhigh) <- names(allfit)
                        if(cumul) {
                                list$cumlow <- cumfit-z*cumse
                                list$cumhigh <- cumfit+z*cumse
                        }
                }
                #
                list$ci.level <- ci.level
                list$model.class <- model.class
                list$model.link <- model.link
                #
                class(list) <- "crosspred"
                #
                return(list)
        }
