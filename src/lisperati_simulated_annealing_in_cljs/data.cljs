(ns lisperati-simulated-annealing-in-cljs.data)

;; taken from http://lisperati.com/haskell/people.txt
(defonce people 
  [
   [2,3,3,4,4,3,5,2,2,3,2,2,2,3,2,5,3,1,3,5,2,5,2,2,2,3,2,5,2,3],
   [2,3,3,2,3,3,5,2,3,4,2,2,1,1,1,2,1,5,1,4,2,5,2,2,2,2,4,1,1,1],
   [2,3,4,3,3,5,5,2,3,5,2,3,2,1,2,4,4,3,3,1,2,5,3,5,2,3,4,1,1,2],
   [1,3,3,3,3,3,3,3,4,4,3,3,4,4,2,3,3,3,1,4,3,4,3,3,2,1,3,2,3,3],
   [4,1,3,3,3,3,4,1,3,4,3,3,1,2,1,4,4,2,2,4,2,2,2,1,2,3,4,2,5,1],
   [2,1,1,2,3,5,4,1,1,1,2,3,5,1,3,2,4,2,3,5,2,5,2,2,2,3,4,2,2,4],
   [1,3,4,2,3,5,4,1,3,1,2,2,2,3,1,1,1,2,2,2,2,1,2,1,2,3,3,1,3,3],
   [3,3,3,1,3,2,2,1,2,3,2,3,2,3,2,3,3,5,2,1,2,5,2,1,2,1,4,2,2,2],
   [1,3,4,3,3,5,5,3,1,4,2,4,1,1,5,1,1,4,2,5,2,5,3,1,2,3,4,2,1,4],
   [1,3,4,2,4,5,4,3,2,1,3,4,1,1,4,5,4,5,3,4,2,2,2,1,2,3,3,4,1,4],
   [1,1,1,4,4,1,4,3,3,3,4,3,1,2,1,4,1,5,2,2,2,4,2,5,2,1,4,4,1,3],
   [2,2,1,3,4,3,4,2,5,2,3,3,3,2,5,2,3,5,2,3,1,1,4,1,2,3,4,1,3,2],
   [2,3,3,1,3,4,4,2,4,1,3,3,1,2,1,5,1,1,2,4,2,1,2,1,2,2,3,1,1,3],
   [1,3,2,1,3,2,3,2,3,3,3,3,4,2,5,1,4,5,3,4,2,5,2,1,2,3,4,5,1,4],
   [1,2,1,2,1,5,4,2,4,4,2,1,1,1,5,4,4,3,3,2,2,1,3,2,2,3,3,5,1,2],
   [5,3,4,4,3,2,4,3,1,5,4,2,5,1,1,2,4,3,3,4,2,5,4,2,2,3,3,3,1,4],
   [5,3,4,2,3,2,4,3,4,1,2,3,1,2,2,2,4,3,2,5,2,1,2,2,2,3,4,4,2,4],
   [3,3,3,1,3,2,5,2,5,3,3,2,2,3,5,3,4,5,2,4,1,5,2,2,2,2,4,3,2,3],
   [2,1,3,4,4,4,5,1,1,3,3,2,2,1,5,4,4,4,3,4,2,5,4,2,1,3,3,5,4,4],
   [1,3,3,1,3,2,3,2,1,5,4,4,1,3,5,2,4,5,2,5,2,5,1,2,2,2,3,2,1,4],
   [5,3,4,2,3,2,5,1,3,4,2,3,2,3,2,3,4,5,2,2,2,1,4,5,2,3,4,3,4,4],
   [5,3,3,4,4,3,5,3,1,1,3,3,1,1,2,4,4,1,2,1,2,4,2,2,1,3,3,2,1,4],
   [4,3,3,4,4,3,5,2,3,3,2,4,2,3,5,2,4,5,3,4,2,5,1,2,2,3,4,2,1,4],
   [1,3,4,3,4,4,5,3,5,3,3,1,1,2,1,4,4,2,2,5,2,1,3,2,2,3,3,5,1,2],
   [1,3,2,3,2,3,5,1,4,3,4,3,2,2,4,2,4,5,3,2,3,1,3,1,1,3,4,3,1,2],
   [1,2,3,2,3,1,4,3,1,5,3,2,5,2,4,2,1,2,2,5,2,5,2,2,2,3,4,4,1,3],
   [1,4,4,1,4,3,4,2,5,4,3,2,1,1,3,4,1,3,2,4,2,4,3,1,2,3,2,2,5,3],
   [3,4,3,2,4,3,5,2,2,4,3,2,1,1,3,2,4,3,2,5,2,2,2,1,2,3,4,5,1,4],
   [3,3,4,3,1,2,3,2,2,1,3,2,1,1,2,2,2,3,2,1,2,1,2,1,2,3,3,5,4,2],
   [3,3,4,1,4,5,2,2,3,4,3,3,1,2,3,4,3,4,3,2,3,5,1,1,2,3,4,2,1,1],
   [1,2,2,3,3,2,5,3,5,1,2,4,2,4,5,1,3,3,3,2,5,5,2,2,2,2,2,4,5,4],
   [1,4,3,2,3,5,3,3,4,3,4,4,5,3,1,4,2,1,3,4,1,1,4,1,2,3,3,3,1,1],
   [1,3,4,2,4,1,4,3,4,4,3,3,2,2,3,2,1,1,2,4,2,5,2,1,2,3,4,1,1,2],
   [1,3,5,2,4,2,2,1,1,1,3,2,3,2,2,3,1,3,3,4,4,1,4,2,2,3,3,3,4,1],
   [2,3,1,1,3,4,5,1,5,5,2,1,2,1,5,2,4,2,2,4,2,5,2,2,2,3,1,1,1,2],
   [4,3,2,2,4,3,5,2,4,3,3,4,5,2,3,2,1,3,2,5,2,5,2,2,2,3,3,5,2,2],
   [3,3,1,1,4,5,2,2,1,4,2,3,4,1,3,2,2,3,2,4,2,5,3,2,2,3,2,2,1,3],
   [1,2,2,4,2,2,4,2,4,3,2,3,1,1,5,2,4,3,3,4,1,5,1,1,2,3,2,4,4,4],
   [2,3,3,2,3,2,5,3,3,1,4,1,1,1,2,2,1,5,1,5,2,5,2,1,2,3,3,5,1,1],
   [1,1,5,1,3,2,4,2,5,1,3,4,1,2,5,4,3,4,2,4,4,1,2,1,2,1,3,5,1,4],
   [5,2,5,4,3,3,4,2,5,1,4,4,1,2,4,2,1,2,2,4,4,4,2,1,1,3,3,5,1,3],
   [4,3,3,1,4,2,3,2,3,4,3,3,1,2,2,5,1,3,2,4,4,2,1,5,2,2,4,5,1,4],
   [1,3,4,2,1,5,2,2,4,3,3,3,1,2,5,1,1,2,2,4,2,5,3,2,2,3,2,5,5,4],
   [1,2,4,4,2,3,5,1,3,3,2,3,2,3,3,2,4,3,2,2,2,1,2,1,2,3,4,5,1,3],
   [2,3,4,4,4,2,3,2,1,1,4,4,5,2,2,2,3,3,3,4,2,1,2,5,2,3,3,5,2,4],
   [1,1,2,4,5,2,5,2,3,4,2,1,3,1,3,5,3,3,2,2,1,5,4,5,2,3,3,4,5,2],
   [1,3,3,4,4,2,4,2,2,3,3,4,1,1,1,4,4,4,3,4,1,1,2,5,1,3,3,2,4,4],
   [2,3,2,4,2,2,4,3,1,1,3,3,5,2,3,5,1,5,2,4,2,5,3,2,1,3,3,3,1,4],
   [5,4,2,3,1,2,5,2,2,3,4,2,2,1,3,1,4,1,2,3,2,5,4,1,2,2,2,2,3,2],
   [5,1,4,3,1,2,3,2,4,3,2,3,2,1,3,2,4,4,3,1,2,2,2,2,1,3,3,5,1,1],
   [4,3,1,2,2,5,4,3,4,3,3,4,1,2,3,2,1,2,3,4,1,1,3,5,2,3,3,5,1,4],
   [1,3,3,3,3,2,5,2,4,4,2,2,2,3,2,4,4,5,2,4,2,5,4,2,1,3,4,3,4,2],
   [1,4,4,2,3,2,3,1,2,2,2,2,3,2,5,3,1,3,2,4,2,2,2,1,2,2,3,4,2,4],
   [5,3,2,3,4,5,4,3,1,3,3,3,5,2,4,5,5,4,3,2,2,1,3,2,1,3,3,2,2,2],
   [3,4,1,3,1,2,5,1,2,4,2,3,1,3,5,4,4,4,1,5,4,5,4,2,2,2,2,2,5,4],
   [3,4,4,2,2,1,4,2,5,3,3,3,1,3,5,3,4,3,1,5,2,1,2,5,2,3,4,5,1,3],
   [5,1,2,3,5,5,1,2,4,3,3,2,1,1,4,4,4,5,3,2,2,5,2,2,1,3,3,4,3,2],
   [5,2,2,3,5,2,3,2,3,3,3,4,3,4,5,1,3,5,3,4,1,1,4,1,2,3,2,2,4,2],
   [2,3,4,1,1,5,3,3,5,3,3,3,1,2,2,1,3,3,2,1,1,5,3,2,2,3,1,5,1,2],
   [1,3,3,2,1,3,5,2,3,1,4,2,3,2,3,2,1,5,2,2,2,1,2,2,1,2,3,2,4,2],
   [5,1,2,3,1,3,4,2,2,3,3,3,2,2,5,1,5,4,1,5,4,2,4,2,2,3,3,5,2,4],
   [3,2,4,3,4,3,2,2,4,1,1,2,1,3,1,5,1,3,2,2,1,5,2,2,2,3,4,4,2,4],
   [1,3,4,2,1,5,5,1,1,4,4,2,5,2,2,4,2,3,2,4,2,4,3,2,2,3,3,3,1,2],
   [2,2,4,3,3,2,5,2,4,4,2,4,2,3,5,3,4,5,2,2,2,5,2,2,2,3,4,1,3,1],
   [1,3,2,1,4,2,4,2,1,3,2,4,2,3,5,1,4,1,2,5,2,5,3,5,2,3,1,2,2,3],
   [2,3,1,2,4,2,4,2,4,1,3,3,1,1,2,5,2,2,2,2,5,1,2,2,1,2,3,5,5,3],
   [1,1,4,2,4,4,4,3,1,3,2,4,1,1,5,5,4,2,2,1,2,5,2,2,2,1,2,5,1,4],
   [3,3,3,3,4,3,2,2,3,3,3,1,3,2,1,1,1,5,1,5,3,2,4,2,2,2,3,4,1,1],
   [2,3,4,2,3,2,5,1,1,4,3,3,1,3,2,5,3,3,2,5,4,5,3,2,2,3,4,2,5,4],
   [1,1,5,1,1,5,5,2,1,4,3,1,2,1,3,5,4,1,3,2,2,5,1,2,2,2,3,1,1,4],
   [1,1,3,4,3,2,2,2,5,3,2,3,1,1,5,4,3,4,2,2,1,1,2,1,1,2,4,1,3,3],
   [3,3,5,2,2,2,4,2,1,2,2,3,1,2,5,4,1,1,2,5,2,5,2,1,2,3,3,2,1,3],
   [2,3,5,2,1,3,4,2,3,1,3,2,5,2,5,1,4,2,2,4,2,5,2,1,1,3,3,5,1,3],
   [3,1,4,4,4,2,5,2,1,1,3,4,1,2,3,4,3,4,3,2,2,2,3,1,1,3,2,5,4,3],
   [1,2,4,2,4,5,4,2,2,3,3,3,1,2,5,1,5,3,3,4,4,2,2,1,2,2,3,5,1,3],
   [3,3,5,2,4,5,4,2,4,1,3,2,2,1,1,1,4,3,2,4,2,1,1,1,2,3,3,3,1,3],
   [4,3,1,1,1,5,3,2,5,1,3,2,1,1,1,4,4,3,3,1,2,5,1,2,2,2,2,4,4,4],
   [2,3,4,4,5,2,3,2,4,3,4,3,1,4,1,2,1,3,2,1,4,4,3,1,2,3,3,5,4,4],
   [2,1,5,2,4,2,5,2,2,1,3,3,2,2,1,4,1,4,3,5,2,2,1,1,2,1,4,2,2,1],
   [2,1,4,2,5,5,2,1,2,3,3,2,1,1,5,4,3,1,2,5,1,1,3,1,1,3,2,5,2,4],
   [1,3,1,2,4,3,5,2,2,1,4,3,1,1,4,5,1,1,3,4,4,2,3,1,2,3,3,3,1,4],
   [1,1,4,2,5,5,2,2,5,2,3,2,1,2,5,4,1,1,2,4,1,2,2,5,2,2,3,1,1,3],
   [5,3,4,3,1,5,4,1,5,5,4,1,1,2,4,5,4,2,1,5,2,2,2,1,2,3,3,4,5,5],
   [1,1,2,3,5,5,4,1,5,4,2,2,4,4,5,4,4,4,2,4,2,5,3,4,2,3,2,5,2,3],
   [2,3,5,2,4,5,5,1,1,4,3,2,5,2,3,5,1,3,2,4,2,5,3,1,1,3,3,1,3,4],
   [3,3,1,2,4,4,4,3,4,1,4,1,1,2,4,2,1,4,2,4,2,1,1,1,2,3,3,5,1,1],
   [2,3,4,2,3,5,5,3,4,1,4,2,5,2,5,2,1,1,3,5,1,1,4,1,2,3,4,1,1,3],
   [2,1,3,2,4,2,4,1,4,3,4,4,2,2,5,3,5,5,1,2,1,4,2,2,1,2,3,5,5,2],
   [3,3,1,2,3,1,5,1,1,3,3,3,2,3,2,3,1,3,2,5,2,5,1,2,2,3,4,5,1,3],
   [1,2,3,3,1,3,5,2,5,3,4,3,5,2,1,3,4,1,2,5,2,1,1,2,2,3,3,3,1,1],
   [1,3,5,2,1,3,4,1,4,4,3,4,1,2,4,3,4,1,2,4,2,5,2,2,2,2,3,2,2,2],
   [3,1,3,2,5,3,3,1,4,2,3,3,1,2,5,3,4,5,2,4,5,5,4,2,2,3,3,3,4,1],
   [2,3,4,2,4,2,4,2,4,1,3,2,1,2,5,4,1,1,2,4,2,1,2,1,2,3,3,4,2,1],
   [2,3,4,2,3,1,2,2,1,1,2,3,5,2,5,1,1,5,2,5,3,1,2,2,2,3,3,5,4,1],
   [5,3,3,3,2,5,5,2,4,3,2,4,1,2,1,2,1,1,2,4,2,5,2,1,2,3,2,5,1,3],
   [1,1,3,2,4,5,4,1,4,3,3,4,5,2,4,2,4,3,2,4,5,2,2,2,1,2,2,4,1,1],
   [5,3,4,2,1,5,4,2,3,1,3,4,5,2,5,5,5,1,2,2,2,5,2,1,2,3,2,5,1,2],
   [1,2,3,2,3,2,4,1,4,1,3,3,1,3,5,1,1,5,1,2,2,1,4,1,1,2,2,4,4,3],
   [3,3,2,2,4,2,5,1,1,3,3,3,1,1,4,4,3,3,3,4,2,1,2,1,2,3,2,3,2,4],
   [5,2,1,2,1,5,2,1,2,5,1,2,1,2,3,5,4,3,2,4,1,5,2,1,1,2,2,2,3,3],
  [5,3,4,3,5,5,1,2,2,3,4,4,1,2,4,1,4,5,2,1,1,5,2,1,2,3,3,2,2,4],
  [3,3,4,3,4,3,1,1,3,1,2,3,1,2,3,5,5,3,2,4,2,2,1,1,2,2,2,2,1,2],
  [3,2,4,4,2,5,4,3,4,3,4,1,2,3,1,5,1,3,1,4,1,5,2,1,2,3,3,5,2,1],
  [2,3,4,3,3,5,5,1,4,1,2,3,5,2,2,1,1,2,1,5,2,5,2,1,2,3,1,2,1,3],
  [2,3,3,1,3,5,4,1,4,1,3,3,1,2,4,1,1,5,1,5,1,5,4,1,2,2,4,1,1,3],
  [5,1,4,3,1,4,4,2,5,3,2,4,2,2,2,1,2,3,2,2,1,1,3,2,2,3,3,3,1,4],
  [5,2,4,3,5,2,3,2,1,3,3,4,5,1,3,2,4,1,2,4,1,4,3,5,2,2,3,4,2,4],
  [1,3,3,2,5,3,2,1,2,5,4,2,5,2,5,1,1,3,1,5,4,4,2,1,2,2,3,1,2,3],
  [2,2,3,4,3,1,4,2,1,1,3,3,5,2,1,2,5,3,2,5,2,5,1,1,2,3,3,1,1,3],
  [2,3,3,2,3,3,4,2,1,4,2,4,1,1,5,2,1,5,1,4,2,5,2,1,2,3,3,3,2,3],
  [1,3,4,3,4,2,4,2,1,1,3,2,1,1,2,4,1,3,3,5,2,1,3,1,2,3,3,4,3,4],
  [4,1,3,4,4,5,4,2,5,1,3,3,5,2,5,3,5,1,3,2,3,2,3,1,2,3,3,5,1,3],
  [5,3,3,3,3,1,5,2,1,4,1,2,4,4,4,4,5,5,3,5,5,1,2,5,2,2,3,2,3,3],
  [1,4,3,2,1,2,2,3,4,3,3,2,1,2,4,1,1,2,1,4,5,1,2,2,1,3,2,5,3,3],
  [1,1,4,3,2,5,5,3,4,1,4,3,5,3,2,4,5,3,3,4,5,2,2,1,2,3,3,4,1,1],
  [1,4,5,2,4,2,4,3,3,2,2,4,1,2,1,2,3,1,2,4,5,2,4,1,2,3,3,5,4,1],
  [1,3,4,4,2,2,4,1,5,3,3,2,1,1,5,2,1,5,2,4,2,1,2,1,2,3,3,1,4,4],
  [4,3,3,2,4,3,4,1,4,3,2,3,5,2,5,5,1,2,2,4,2,5,2,3,1,3,2,5,5,4],
  [3,3,4,2,3,5,2,2,3,5,2,3,5,2,4,2,1,5,2,5,3,5,2,1,1,3,3,3,5,1],
  [3,3,5,2,1,3,5,2,4,5,4,4,4,1,1,2,1,2,1,4,4,2,3,5,1,1,3,2,1,1],
  [3,2,4,1,3,5,3,2,1,5,3,4,2,1,3,2,4,5,3,4,2,5,1,5,2,3,3,5,2,1],
  [2,3,5,2,4,4,4,2,4,1,2,2,5,2,2,2,4,3,2,1,2,2,2,1,2,3,2,5,2,1],
  [5,3,4,4,3,1,4,3,1,1,3,1,2,2,1,4,1,2,2,2,2,5,4,2,2,3,3,5,1,4],
  [1,3,4,2,4,2,2,2,4,3,3,3,1,1,1,2,4,1,3,2,2,5,2,1,2,3,4,4,4,4],
  [3,1,4,3,4,1,5,3,4,4,3,2,1,2,2,4,2,3,2,2,2,5,2,1,2,3,3,2,1,1],
  [3,4,3,2,4,2,4,2,5,1,3,3,5,3,5,1,4,5,3,2,2,2,2,2,2,2,4,5,2,4],
  [2,3,4,4,1,3,3,3,4,1,2,3,5,1,1,3,4,5,3,4,3,2,2,2,2,2,3,5,2,4],
  [2,2,4,3,2,4,5,2,3,3,3,3,2,2,4,5,5,1,2,5,2,1,4,5,2,3,1,5,4,4],
  [5,3,3,3,5,5,4,2,4,5,4,4,1,1,5,1,1,3,2,5,4,4,4,1,1,3,3,4,5,1],
  [1,1,4,4,2,3,4,1,2,3,3,3,4,2,4,1,4,5,2,2,2,4,4,2,2,2,3,5,2,4],
  [4,3,3,3,4,5,4,1,3,3,4,1,1,2,4,1,5,2,1,3,2,2,3,1,2,3,3,4,5,4],
  [1,2,4,1,3,3,3,2,2,5,2,3,2,2,3,1,4,3,2,5,2,1,3,2,2,3,2,5,2,3],
  [1,2,4,2,4,1,2,2,4,1,2,3,1,2,4,5,1,1,2,5,1,5,3,1,2,3,1,5,1,3],
  [2,3,5,2,2,1,4,3,5,4,4,1,1,2,3,5,4,1,2,4,5,5,4,1,1,3,3,5,5,1],
  [4,3,1,1,5,2,3,2,5,1,3,3,2,1,2,4,1,5,3,4,2,2,1,1,1,3,3,2,2,2],
  [4,3,4,2,3,3,2,2,4,3,3,2,1,2,5,2,1,4,2,2,5,2,3,1,2,2,4,5,4,2],
  [1,2,3,2,3,5,5,1,4,5,2,3,1,2,4,3,4,4,2,5,2,1,4,1,1,3,2,5,4,3],
  [4,3,3,1,2,3,5,1,5,1,3,3,4,2,1,1,2,3,1,4,2,2,4,5,2,3,4,4,4,3],
  [2,3,3,2,4,1,4,2,3,1,4,2,5,2,4,2,4,1,2,5,1,2,2,1,2,3,3,5,1,3],
  [5,3,4,2,4,5,4,2,1,3,3,2,1,2,5,5,5,1,2,2,1,1,2,2,2,3,2,4,1,4],
  [5,3,4,4,3,5,2,1,1,3,4,3,1,1,1,4,4,2,1,4,1,2,2,1,2,3,2,5,1,3],
  [5,4,4,4,2,5,2,1,5,3,4,2,4,2,3,3,1,3,2,5,4,2,4,4,2,1,3,5,1,3],
  [4,3,5,2,4,2,3,2,4,4,4,1,1,2,3,2,3,1,2,4,2,2,2,1,2,3,2,4,1,4],
  [1,1,4,2,2,1,5,2,1,1,2,2,1,2,5,4,1,2,2,2,2,5,2,1,2,3,3,1,2,3],
  [2,3,4,3,3,5,4,3,1,5,2,3,1,2,5,4,1,2,1,5,2,4,2,1,2,3,4,1,1,3],
  [3,3,1,1,2,4,4,2,4,3,2,3,1,2,3,2,5,2,2,2,1,5,2,1,1,3,4,3,1,3],
  [5,3,3,3,5,2,3,2,5,3,4,3,1,1,3,5,1,1,2,4,1,4,2,1,2,3,4,3,1,4],
  [1,1,4,3,3,5,2,1,4,5,3,3,5,2,2,1,1,2,2,5,2,5,2,1,2,2,3,1,5,3],
  [1,1,4,4,2,5,3,2,1,5,3,3,1,1,1,2,4,1,2,5,2,1,2,2,2,3,3,3,1,3],
  [3,3,4,3,4,3,4,2,3,1,4,4,1,2,1,5,1,3,3,4,2,1,2,2,2,3,3,4,2,1],
  [2,2,4,2,1,4,5,2,5,4,3,3,1,2,1,4,1,2,3,4,5,5,3,1,2,3,3,1,1,4],
  [2,3,3,4,5,5,3,2,2,5,3,1,1,1,3,5,4,1,1,4,2,5,2,5,2,3,4,4,2,4],
  [1,3,3,2,3,5,4,3,1,3,3,3,5,2,5,2,1,3,2,2,2,1,3,1,2,3,3,4,1,3],
  [1,2,2,4,4,5,5,2,5,3,3,3,1,2,2,5,1,1,3,4,1,1,1,1,2,3,2,5,2,4],
  [4,3,4,2,3,4,3,3,5,3,4,3,1,3,5,1,4,5,1,4,2,5,2,1,2,3,4,5,1,2],
  [1,3,4,4,5,4,3,2,2,1,2,1,1,1,3,5,4,5,2,4,2,5,4,2,2,3,2,5,5,4],
  [3,3,1,1,1,5,2,2,4,5,2,4,2,1,5,5,4,1,1,1,2,5,2,2,1,3,1,3,1,4],
  [3,2,4,3,4,5,5,2,3,4,4,1,1,1,2,4,4,3,2,5,2,1,2,2,2,3,2,4,1,5],
  [2,4,4,4,4,2,2,2,1,1,3,2,1,2,5,2,4,3,1,1,2,5,2,1,2,3,3,5,1,4],
  [3,3,3,1,1,2,4,1,4,3,2,2,1,2,3,5,1,5,2,5,2,5,3,1,2,3,2,5,2,2],
  [4,3,4,3,4,3,5,1,4,3,3,3,5,2,4,3,5,2,2,2,2,5,2,1,2,3,2,4,1,2],
  [1,3,1,1,2,4,3,3,1,1,3,2,1,1,2,5,4,3,3,1,2,5,3,1,1,3,3,3,2,3],
  [1,1,3,3,5,3,3,2,2,5,4,2,3,2,2,1,4,3,2,4,2,5,1,1,2,3,4,3,3,2],
  [2,1,4,1,5,2,5,1,3,5,2,3,2,1,4,5,5,1,2,4,2,5,3,5,2,3,2,2,1,4],
  [1,3,4,1,3,2,5,2,3,1,4,3,2,1,3,3,4,5,3,5,2,5,2,1,2,3,4,3,4,3],
  [1,2,4,2,4,2,4,2,2,2,3,4,1,2,2,5,4,4,3,4,5,5,2,1,2,3,3,5,2,3],
  [1,1,4,1,5,2,3,2,1,3,2,3,1,3,2,4,3,4,3,4,4,5,2,2,2,3,2,5,2,4],
  [4,1,3,4,3,5,4,2,1,1,3,3,5,2,5,2,1,3,2,1,2,1,1,1,2,2,2,5,1,4],
  [1,1,3,1,3,5,5,2,4,4,3,3,1,1,5,3,3,3,2,5,1,1,1,1,2,3,3,2,2,4],
  [5,1,4,2,4,3,4,2,1,3,3,3,5,1,1,5,3,1,2,4,2,1,2,2,2,3,3,3,3,4],
  [1,1,5,1,2,4,5,3,5,4,3,4,1,2,4,5,3,2,2,4,2,1,2,2,2,3,3,5,4,3],
  [1,2,4,1,2,4,2,2,4,4,2,2,4,3,3,1,4,1,3,5,2,5,2,1,2,2,3,5,5,3],
  [1,1,4,4,5,3,4,1,3,1,2,3,1,1,3,4,4,5,2,1,2,5,2,1,2,3,2,2,1,4],
  [1,3,1,4,1,5,3,2,4,5,3,3,1,1,3,2,1,3,2,4,2,5,2,1,2,3,4,2,1,2],
  [5,4,2,2,2,2,4,2,1,3,3,1,1,2,1,4,3,3,3,5,2,1,2,1,2,3,3,2,2,4],
  [2,3,4,3,1,3,3,2,3,1,3,3,1,1,2,4,5,3,2,2,1,2,2,2,2,2,3,2,2,2],
  [1,3,4,1,4,3,4,3,3,3,2,4,5,1,5,4,4,5,2,1,1,5,4,4,2,1,4,2,5,4],
  [2,2,4,1,5,5,5,2,1,2,2,3,3,1,4,4,1,3,3,1,4,5,4,2,2,3,2,4,2,2],
  [4,2,2,4,5,2,3,2,4,1,3,4,1,2,4,5,3,5,2,4,2,5,4,2,2,3,3,5,1,2],
  [3,3,4,4,4,2,3,2,3,3,3,2,5,1,3,4,3,5,1,4,2,1,2,1,2,3,3,5,1,2],
  [3,3,4,2,4,2,5,1,2,4,3,3,1,2,3,4,1,3,3,4,5,2,2,2,2,3,3,4,1,1],
  [4,3,2,2,4,4,4,2,2,2,3,2,5,1,1,2,1,1,2,4,2,2,2,1,2,3,3,5,1,3],
  [1,3,3,1,2,5,1,3,2,3,3,3,1,2,1,5,1,2,2,5,2,2,1,1,2,3,4,2,2,4],
  [2,3,4,2,1,4,3,2,1,3,3,3,1,2,1,4,4,2,2,1,2,5,2,1,2,3,2,5,2,2],
  [1,3,4,1,3,4,2,2,4,3,3,4,2,2,5,2,1,4,2,5,2,5,2,5,1,3,3,5,1,3],
  [1,3,3,3,5,3,5,1,1,3,4,4,3,3,2,3,2,5,2,1,2,5,4,4,2,3,3,5,2,2],
  [2,3,1,1,3,5,5,3,1,3,2,4,2,2,3,3,1,3,2,3,3,4,3,4,2,2,3,3,4,4],
  [5,3,2,2,2,4,5,2,2,3,3,3,1,2,3,5,1,1,2,1,2,2,2,2,1,3,2,5,2,2],
  [3,3,4,2,2,4,5,2,2,3,3,3,1,2,2,5,3,1,2,4,2,5,2,1,1,3,2,5,4,4],
  [1,1,4,3,4,5,5,2,5,3,3,3,4,2,1,3,4,5,2,3,2,5,2,2,2,3,3,3,4,4],
  [1,3,2,4,5,3,4,1,4,1,2,2,1,2,2,4,1,5,2,2,2,5,3,1,2,3,2,5,2,4],
  [2,3,3,3,4,3,5,3,4,3,2,3,1,2,3,2,1,4,2,2,2,1,3,5,2,3,2,3,1,4],
  [3,3,3,3,4,2,4,2,5,1,3,3,1,3,5,5,1,3,2,2,2,5,3,4,2,2,4,5,1,4],
  [3,4,2,1,4,3,5,1,5,1,2,3,5,2,5,2,4,5,3,5,2,5,1,5,2,3,2,5,4,4],
  [2,1,4,2,4,5,4,2,3,4,2,1,2,2,4,5,1,1,3,4,2,5,2,1,2,3,3,1,1,3],
  [1,4,3,1,4,4,5,3,5,1,2,3,2,2,5,5,4,5,2,5,5,5,2,1,2,2,4,5,5,4],
  [1,3,4,2,1,2,3,3,1,3,3,4,1,3,3,3,4,3,2,1,2,5,3,1,2,3,2,3,1,4],
  [3,3,4,2,4,5,2,2,4,1,2,4,5,2,1,4,1,1,3,1,2,5,2,1,1,3,3,4,1,2],
  [2,4,5,2,1,3,4,3,5,4,3,4,4,2,5,1,1,4,2,1,4,2,4,5,1,3,3,5,1,4],
  [3,3,2,4,5,2,1,2,1,3,2,4,1,2,3,5,4,3,2,4,2,1,2,2,2,3,3,5,2,2]
  ])


(defonce polygon-list
  [
   [ {:x 106.7619, :y 131.19048} {:x 17.190475, :y 96.809525} {:x 22.619047, :y 71.47619} 
     {:x 34.38095, :y 47.95238} {:x 46.142857, :y 39.809525} {:x 157.42857, :y 105.85714} 
     {:x 106.7619, :y 131.19048} ]
   [ {:x 17.190475, :y 96.809525} {:x 106.7619, :y 131.19048} {:x 85.04762, :y 188.19048} 
     {:x 17.190475, :y 187.2857} {:x 17.190475, :y 96.809525} ]
   [ {:x 142.04762, :y 250.61905} {:x 76.0, :y 197.2381} {:x 16.285713, :y 199.04762} 
     {:x 15.380952, :y 265.09525} {:x 26.238094, :y 287.7143} {:x 45.238094, :y 298.57144} 
     {:x 142.04762, :y 250.61905} ]
   [ {:x 53.38095, :y 314.85715} {:x 72.38095, :y 328.42856} {:x 96.809525, :y 332.9524} 
     {:x 272.33334, :y 329.33334} {:x 291.33334, :y 244.2857} {:x 242.4762, :y 211.7143} 
     {:x 53.38095, :y 314.85715} ]
   [ {:x 298.57144, :y 252.42857} {:x 324.8095, :y 254.2381} {:x 332.9524, :y 327.5238} 
     {:x 285.0, :y 328.42856} {:x 298.57144, :y 252.42857} ]
   [ {:x 352.85715, :y 325.7143} {:x 337.4762, :y 248.80952} {:x 376.38095, :y 214.42857} 
     {:x 545.5714, :y 303.09525} {:x 544.6667, :y 328.42856} {:x 527.4762, :y 331.14285} 
     {:x 352.85715, :y 325.7143} ]
   [ {:x 237.95238, :y 126.66667} {:x 289.5238, :y 92.28571} {:x 276.85715, :y 13.571429} 
     {:x 86.85714, :y 14.47619} {:x 66.95238, :y 20.809525} {:x 56.095238, :y 32.57143} 
     {:x 237.95238, :y 126.66667} ]
   [ {:x 299.4762, :y 89.57143} {:x 319.38095, :y 88.666664} {:x 334.7619, :y 17.190475} 
     {:x 288.61905, :y 16.285713} {:x 299.4762, :y 89.57143} ]
   [ {:x 377.2857, :y 127.57143} {:x 325.7143, :y 92.28571} {:x 345.61905, :y 16.285713} 
     {:x 532.9048, :y 14.47619} {:x 559.1428, :y 33.476192} {:x 523.8571, :y 54.285713} {:x 377.2857, :y 127.57143} ]
   [ {:x 393.57144, :y 171.90475} {:x 522.9524, :y 158.33333} {:x 514.8095, :y 72.38095} 
     {:x 385.42856, :y 139.33333} {:x 393.57144, :y 171.90475} ]
   [ {:x 522.9524, :y 158.33333} {:x 393.57144, :y 171.90475} {:x 380.90475, :y 203.57143} 
     {:x 468.66666, :y 243.38095} {:x 508.4762, :y 210.80952} {:x 522.9524, :y 158.33333} ]
   [ {:x 532.0, :y 191.80952} {:x 585.3809, :y 189.09525} {:x 586.2857, :y 150.19048} 
     {:x 531.0952, :y 152.0} {:x 532.0, :y 191.80952} ]
   [ {:x 523.8571, :y 67.85714} {:x 572.7143, :y 39.809525} {:x 588.0952, :y 58.809525} 
     {:x 590.8095, :y 103.14286} {:x 584.4762, :y 141.14285} {:x 531.0952, :y 140.2381} 
     {:x 523.8571, :y 67.85714} ]
   [ {:x 528.3809, :y 251.5238} {:x 587.1905, :y 257.85715} {:x 579.9524, :y 285.90475} 
     {:x 560.9524, :y 300.38095} {:x 517.5238, :y 273.2381} {:x 528.3809, :y 251.5238} ]
   [ {:x 587.1905, :y 257.85715} {:x 528.3809, :y 251.5238} {:x 532.0, :y 199.04762} 
     {:x 587.1905, :y 197.2381} {:x 587.1905, :y 257.85715} ]
   [ {:x 477.7143, :y 250.61905} {:x 507.57144, :y 267.8095} {:x 517.5238, :y 247.0} 
     {:x 518.4286, :y 217.14285} {:x 477.7143, :y 250.61905} ]
   [ {:x 268.7143, :y 139.33333} {:x 284.09525, :y 125.7619} {:x 309.42856, :y 120.33333} 
     {:x 341.09525, :y 133.90475} {:x 359.1905, :y 161.95238} {:x 360.09525, :y 188.19048} 
     {:x 347.42856, :y 207.19048} {:x 321.1905, :y 221.66667} {:x 294.0476, :y 222.57143} 
     {:x 274.14285, :y 209.90475} {:x 257.85715, :y 188.19048} {:x 258.7619, :y 157.42857} 
     {:x 268.7143, :y 139.33333} ]])