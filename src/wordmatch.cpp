#include <Rcpp.h>
#include "levenshtein.h"

using namespace Rcpp;

// [[Rcpp::export]]
List word_match(List div, CharacterVector target, int threshold = 0) {
  List ret = List::create();
  
  for (List::iterator i = div.begin(); i != div.end(); i++) {
    NumericVector matched = NumericVector::create();
    
    CharacterVector words = *i;
    
    for (CharacterVector::iterator k = target.begin(); k != target.end(); k++) {
      bool all = true;
      
      for (CharacterVector::iterator j = words.begin(); j != words.end(); j++) {
        std::string word = as<std::string>(*j);
        std::string cand = as<std::string>(*k);
        
        if (threshold == 0) {
          all = all && (cand.find(word) != std::string::npos);
        } else {
          std::string buf;
          std::stringstream ss(cand);
          
          bool mtch = false;
          while (ss >> buf) {
            if (mtch) break;
            mtch = levenshtein(word.c_str(), buf.c_str()) <= threshold;
          }
          
          all = all && mtch;
        }
      }
      
      if (all && words.size() != 0) {
        matched.push_back(k - target.begin() + 1);
      }
    }
    
    ret.push_back(matched);
  }
  
  return ret;
}