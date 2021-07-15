(use-package oeis
  :load-path "vendor/"
  :general
  (:states 'normal :prefix ef-prefix
   "ho" '(nil :wk "OEIS")
   "hos" '(oeis-search :wk "OEIS Search")
   "hob" '(oeis-browse-anum :wk "OEIS Browse ANUM")))

(provide 'util-oeis)
