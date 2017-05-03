## ---- load-packages ----

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(data.table)
# pacman::p_load(xlsx)
pacman::p_load(magrittr)
pacman::p_load(tidyverse)
pacman::p_load(bit64)
pacman::p_load(Hmisc)
pacman::p_load(rms)
pacman::p_load(rpart)
pacman::p_load(mice)
pacman::p_load(cowplot)
pacman::p_load(pROC)
