(ns clojurelisa.core
  (:gen-class))

(import
  '(java.awt Graphics Graphics2D Color Polygon)
  '(java.awt.image BufferedImage PixelGrabber)
  '(java.io File)
  '(javax.imageio ImageIO)
  '(javax.swing JFrame JPanel JFileChooser))

;helpers
(defn random-double
  []
  (- (* 2 (rand)) 1))
;end helpers

;polygon and drawing building blocks
(defn color [red green blue alpha] 
  {:type :Color :red red :green green :blue blue :alpha alpha})

(defn point [x y] {:type :Point :x x :y y})

(defn polygon [color points] {:type :Polygon :color color :points points})

(defn draw-polygon 
  "draws a single polygon, using Java fillPolygon.
  returns nothing (nil)"
  [graphics polygon]
  (doto graphics
    (.setColor (new Color (:red (:color polygon))
                          (:blue (:color polygon))
                          (:green (:color polygon))
                          (:alpha (:color polygon))))
    (.fillPolygon (let [jpolygon (new Polygon)]
                    (doseq [p (:points polygon)] (. jpolygon (addPoint (:x p)
                                                                       (:y
                                                                          p)))) 
                    jpolygon)))
    nil)
;end polygon and drawing building blocks


;graphics helper functions
(defn grab-pixels
  "returns an array of pixels"
  [image]
  (let [w (. image (getWidth))
        h (. image (getHeight))
        pixels (make-array (. Integer TYPE) (* w h))]
    (doto (new PixelGrabber image 0 0 w h pixels 0 w)
      (.grabPixels))
    pixels))

(defn source-image
  "returns an image instance of the original image"
  []
  (let [image (ImageIO/read (File. "/Users/maurits/code/clojure/clojurelisa/mona_lisa_crop.jpg"))]
    image))
;end graphics helper functions

;program building blocks
(defn program
  "defines and constructs a single program"
  [code fitness image]
  {:type :Program :code code :fitness fitness :image image}
  )

(defn program-header
  "returns the header (definition) of a program"
  [prog]
  (take 2 (:code prog))
  )

(defn program-expressions
  "returns the actual program expressions, minus the header"
  [prog]
  (drop (count (program-header prog)) (:code prog))
  )

(defn remove-item
  "removes a single item out of the list of expressions"
  [s n]
  (concat (take n s) (drop (inc n) s)))

(defn replace-item
  "returns a list with the n-th item of l replaced by v."
  [l n v]
  (concat (take n l) (list v) (drop (inc n) l)))

(def initial-program (program '(fn [graphics]) nil nil))
;end program building blocks

;"bio" functions
(defn fitness
  "determines the fitness of a single element.
  fitness is determined by how close a rendered image is to the original,
  using LMS as a guide, looping over all pixels of the source image"
  [individual]
  (if (:fitness individual)
    individual
    (let [gen-image (new BufferedImage (.getWidth (source-image)) (.getHeight (source-image)) BufferedImage/TYPE_INT_ARGB)
          src-pixels (grab-pixels (source-image))]
      (apply (eval (:code individual)) [(. gen-image (createGraphics))])
      (def gen-pixels (grab-pixels gen-image))
      (loop [ i (int 0)
              lms (int 0)]
        (if (< i (alength gen-pixels))
          (let [src-color (new Color (aget src-pixels i))
                gen-color (new Color (aget gen-pixels i))
                dr (- (. src-color (getRed)) (. gen-color (getRed)))
                dg (- (. src-color (getGreen)) (. gen-color (getGreen)))
                db (- (. src-color (getBlue)) (. gen-color (getBlue)))]
            (recur (unchecked-inc i) (int (+ lms (* dr dr) (* dg dg) (* db
                                                                        db)))))
          (assoc individual :fitness lms :image gen-image))))))

(defn select
  "Selects the (configurable) n fittests out of a generation"
  [population n]
  (take n
        (sort-by :fitness
                 (pmap (fn [i] (fitness i)) population))
  ))


(defmulti mutate
  "mutates a single element using random disturbances"
  :type
  )
(defmethod mutate :Color [c]
  (let [dr (int (* (:red c) (random-double)))
        dg (int (* (:green c) (random-double)))
        db (int (* (:blue c) (random-double)))
        da (int (* (:alpha c) (random-double)))]
    (assoc c :red (max (min (- (:red c) dr) 255) 0)
           :green (max (min (- (:green c) dg) 255) 0)
           :blue (max (min (- (:blue c) db) 255) 0)
           :alpha (max (min (- (:alpha c) da) 255) 0))))

(defmethod mutate :Polygon [p] 
  (defn mutate-point [p]
    (let [n (rand-int (count (:points p)))]
      (assoc p :points (assoc (:points p) n (mutate (get (:points p) n))))))

  (defn mutate-color [p] (assoc p :color (mutate (:color p))))
  
  (let [roulette (rand-int 2)]
    (cond
      (= 0 roulette) (mutate-point p)
      (= 1  roulette) (mutate-color p))))

(defmethod mutate :Point [p]
  (let [dx (int (* (:x p) (random-double)))
        dy (int (* (:y p) (random-double)))]
    (assoc p :x (max (min (- (:x p) dx) (.getWidth (source-image))) 0)
             :y (max (min (- (:y p) dy) (.getHeight (source-image))) 0)))
  )
(defmethod mutate :Program [p]
  (defn add-polygon [p]
    (assoc p :code
           (concat (:code p)
                   [(list 'clojurelisa.core/draw-polygon
                          (first (nth (:code initial-program) 1))
                          (polygon
                            (color (rand-int 255) (rand-int 255) (rand-int
                                                                   255)
                                   (rand-int 255))
                            (vec (map
                                   (fn [n]
                                     (point
                                       (rand-int (.getWidth (source-image)))
                                       (rand-int (.getHeight (source-image)))))
                                   (range 5)))))])
           :fitness nil :image nil))
  (defn remove-polygon [p]
    (let [n (rand-int (count (program-expressions p)))]
      (assoc p :code (concat (program-header p)
                             (remove-item (program-expressions p) n))
             :fitness nil :image nil))
  )
  (defn mutate-polygon 
    "triggers mutations on polygon code + childen"
    [p]
    (let [expressions (program-expressions p)
          n (rand-int (count expressions))
          target (nth expressions n)]
      (assoc p :code
             (concat (program-header p)
                     (replace-item expressions
                                   n
                                   (list (nth target 0)
                                         (nth target 1)
                                         (mutate (nth target 2)))))
             :fitness nil :image nil))
  )
  (let [polygon-count (count (program-expressions p))
        roulette (cond
                   (empty? (program-expressions p)) 4
                   (>= polygon-count 50) (rand-int 4)
                   :else (rand-int 5))]
    (cond
      (> 3 roulette) (mutate-polygon p)
      (= 3 roulette) (remove-polygon p)
      (= 4 roulette) (add-polygon p))))

(defn evolve 
  "Evolves a population, applying the random evolution function"
  [settings]
   (loop [i 0
         population (list initial-program)]
    (let [fittest (select population 1)
          newborns (map (fn [i] (mutate i)) fittest)]
      ((:new-generation-callback settings (fn [a b])) i fittest)
      (when-not (= (first population) (first fittest))
        ((:new-fittest-callback settings (fn [a b])) i fittest))
      (recur (inc i) (concat fittest newborns)))))
;end bio functions

(defn -main [& args]
  (let [jframe (new JFrame "Fittest Program")
        fittest (atom (list initial-program))
    image (source-image)
    image-width (.getWidth image)
    image-height (.getHeight image)
        settings {
                  :new-fittest-callback (fn [i f]
                      (swap! fittest (fn [o n] n) f)
                      (.repaint jframe))}]
    (doto jframe
      (.setSize image-width image-height)
      (.add (proxy [JPanel] []
        (paint [g]
          (doto g 
              (.setColor Color/white)
              (.fillRect 0 0 image-width image-height)
              (.drawImage (:image (first @fittest)) nil 0 0)))))
      (.setVisible true))
    (evolve settings)))

