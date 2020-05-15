;; Mount phrases
(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun PP ()          (append (Prep) (noun-phrase)))

;; List of words
(defun Article () (one-of '(the a)))
(defun Noun ()    (one-of '(man ball woman table)))
(defun Verb ()    (one-of '(hit took saw liked)))
(defun Adj ()     (one-of '(big little blue green adiabatic)))
(defun Prep ()    (one-of '(to in by with on)))

;; Choose words at random
(defun one-of (set)
  "Pick on element of set and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

;; Adjetives and prepositions
(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

