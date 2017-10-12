(ns runners.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [lisperati-simulated-annealing-in-cljs.utils-test]))

(doo-tests 'lisperati-simulated-annealing-in-cljs.utils-test)

