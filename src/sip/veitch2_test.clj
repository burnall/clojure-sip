(ns sip.veitch2_test
  (:require [sip.veitch2 :refer [vei]]))

(defn tst [input expected] 
  (let [actual (vei input)]
  	(if (= actual expected)
  	  (print "Good\n")
  	  (print "Bad:\n\tInput:\n\t" input "\n\tExpected:" expected "\n\tActual:" actual "\n"))))


(tst #{#{'a 'B 'C 'd}
         #{'A 'b 'c 'd}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}
         #{'A 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'B 'C 'd}}
   #{#{'A 'c} 
     #{'A 'b}
     #{'B 'C 'd}})

(tst #{#{'A 'B 'C 'D}
         #{'A 'B 'C 'd}}
   #{#{'A 'B 'C}})

(tst #{#{'a 'b 'c 'd}
         #{'a 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'B 'c 'D}
         #{'A 'B 'C 'd}
         #{'A 'B 'C 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}}
   #{#{'a 'c}
     #{'A 'C}})

(tst #{#{'a 'b 'c 'd}
         #{'a 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'B 'c 'D}
         #{'A 'B 'C 'd}
         #{'A 'B 'C 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}}
   #{#{'a 'c}
     #{'A 'C}})     

(tst #{#{'a 'b 'c} 
         #{'a 'B 'c}
         #{'a 'b 'C}
         #{'a 'B 'C}}
   #{#{'a}})

(tst #{#{'a 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'b 'C 'D}
         #{'a 'b 'c 'D}
         #{'a 'B 'C 'D}
         #{'A 'B 'C 'd}}
   #{#{'a 'B 'c 'd}
     #{'A 'B 'c 'D}
     #{'A 'b 'C 'D}
     #{'a 'b 'c 'D}
     #{'a 'B 'C 'D}
     #{'A 'B 'C 'd}})

(tst #{#{'a 'b 'c 'd}
         #{'a 'B 'c 'd}
         #{'A 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'B 'c 'D}
         #{'A 'B 'c 'D}}
   #{#{'a 'c}
     #{'B 'c}})

(tst #{#{'a 'B 'c 'd}
         #{'A 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'b 'C 'D}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'D}
         #{'a 'B 'C 'd}
         #{'A 'B 'C 'd}}
   #{#{'B 'd}
     #{'b 'D}})

(tst #{#{'a 'b 'c 'd}
         #{'A 'b 'c 'd}
         #{'a 'B 'c 'D}
         #{'A 'B 'c 'D}
         #{'a 'B 'C 'D}
         #{'A 'B 'C 'D}
         #{'a 'b 'C 'd}
         #{'A 'b 'C 'd}}
   #{#{'B 'D}
     #{'b 'd}})
