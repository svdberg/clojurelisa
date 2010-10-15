(ns clojurelisa.core-test
  (:use [clojurelisa.core] :reload-all)
  (:use [clojure.test]))

(def test-image (source-image "/Users/svdberg/Documents/Projects/Clojure/JFall/clojurelisa/mona_lisa_crop.jpg"))

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
         (let [p (point 7 8)]
           (is (not (= p (mutate p test-image))))))

(deftest test-mutate-color
         (let [c (color 125 255 0 10)]
           (is (not (= c (mutate c test-image))))))

(deftest test-grab-pixels
         (let [pix (grab-pixels test-image)]
           (is (not (= (count pix) 0)))))

(deftest test-best-fit
         (let [original-pixel-list (grab-pixels test-image)
               empty-list []]
           (is (= (best-fit original-pixel-list empty-list) 0))
           (is (= (best-fit original-pixel-list original-pixel-list) 0))
            ))

(deftest test-clamp
         (let [expected-value 10]
           (is (= (clamp 9 10 20) expected-value))
           (is (= (clamp 15 10 20) 15))
           (is (= (clamp 22 10 20) 20))))

(deftest test-fitness
         (let [start-individual initial-program
               third-gen-individual (mutate (mutate (mutate start-individual test-image) test-image) test-image)
               start-value (fitness start-individual test-image)]
           (is (> (:fitness start-value) 0))
           (is (< (:fitness (fitness third-gen-individual test-image)) (:fitness start-value)))))
