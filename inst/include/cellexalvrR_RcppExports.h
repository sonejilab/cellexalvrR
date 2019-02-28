// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_cellexalvrR_RCPPEXPORTS_H_GEN_
#define RCPP_cellexalvrR_RCPPEXPORTS_H_GEN_

#include <RcppEigen.h>
#include <Rcpp.h>

namespace cellexalvrR {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("cellexalvrR", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("cellexalvrR", "_cellexalvrR_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in cellexalvrR");
            }
        }
    }

    inline std::vector<double> toColNums(Eigen::SparseMatrix<double> data) {
        typedef SEXP(*Ptr_toColNums)(SEXP);
        static Ptr_toColNums p_toColNums = NULL;
        if (p_toColNums == NULL) {
            validateSignature("std::vector<double>(*toColNums)(Eigen::SparseMatrix<double>)");
            p_toColNums = (Ptr_toColNums)R_GetCCallable("cellexalvrR", "_cellexalvrR_toColNums");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_toColNums(Shield<SEXP>(Rcpp::wrap(data)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<std::vector<double> >(rcpp_result_gen);
    }

}

#endif // RCPP_cellexalvrR_RCPPEXPORTS_H_GEN_
