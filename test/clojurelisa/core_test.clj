(ns clojurelisa.core-test
  (:use [clojurelisa.core] :reload-all)
  (:use [clojure.test]))

(deftest testpoint ;; test the point constructor
  (let [ p (point 1 2)]
    (is (:x p) 1)
    (is (:y p) 2)))

(deftest test-randomdouble
         (let [d (random-double)]
           (is (instance? Double d))
           (is (and (< d 1) (> d -1)))))

(deftest test-removeitem
         (let [orglist '(1 2 3 4 5 6)
               proc-list (remove-item orglist 2)]
           (is proc-list '(1 2 4 5 6))))


(deftest test-replace-item
         (let [orglist '(1 2 3 4 5 6)
               proc-list (replace-item orglist 2 99)]
           (is proc-list '(1 2 99 4 5 6))))

(deftest test-mutate-point
         (let [p (point 1 2)]
           (is (not (= p (mutate p))))))

(deftest test-mutate-color
         (let [c (color 125 255 0 10)]
           (is (not (= c (mutate c))))))
