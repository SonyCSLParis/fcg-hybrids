;; Copyright 2019 Sony Computer Science Laboratories Paris
;;                Remi van Trijp (http://www.remivantrijp.eu)

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================
;;;;;
;;;;; ------------------------------------------------------------------------------------------
;;;;; This file uses the English grammar together with the spaCy syntactic dependency parser
;;;;; to outsource syntactic processing and to only focus on semantics.
;;;;; The main idea is to translate the syntactic dependency in a transient structure and
;;;;; and to let the (unchanged) grammatical constructions operate on those.
;;;;;
;;;;; Helper functions and specifications for the dependency-tags can be found in the file
;;;;; clear-dependency-tags.lisp in this folder.
;;;;; ------------------------------------------------------------------------------------------

(in-package :fcg)

;;;;; --------------------------------------------------------------------------------------------
;;;;; Node-test:
;;;;; The hybrid is based on dependency trees. This node-test allows you to enforce this structure
;;;;; to remain unchanged during comprehension.
;;;;; --------------------------------------------------------------------------------------------
(defmethod cip-node-test ((node cip-node) (mode (eql :connected-structure)))
  "Ensures that the grammar will not alter the structure of the dependency parser."
  (if (formulation-p node) ;; Only test in comprehension.
    t
    (fcg::connected-syntactic-structure
     (fcg-get-transient-unit-structure node)
     :grammar-hierarchy-features (fcg::hierarchy-features (construction-inventory node)))))

;;;;; ------------------------------------------------------------------------------------------
;;;;; Translation functions.
;;;;; ------------------------------------------------------------------------------------------

(defun search-for-head-id (function head-word-spec word-specs)
  "Calculate the head-id, and allow for some corrections such as for PrepNPs."
  (if (and (member (word-dependency-spec-pos-tag head-word-spec)
                   '("IN")
                   :test #'equal)
           (not (equal function (word-dependency-spec-syn-role head-word-spec))))
    (search-for-head-id function (loop for word-spec in word-specs
                                       when (= (word-dependency-spec-head-id head-word-spec)
                                               (word-dependency-spec-node-id word-spec))
                                       return word-spec)
                        word-specs)
    (word-dependency-spec-node-id head-word-spec)))

(defun handle-functional-unit (function head-word-spec unit-name parent-name word-specs &optional preprocessed-units)
  "Handle functional units such as nsubj, dobj, dative, and so on."
  (let* ((head-id (word-dependency-spec-node-id head-word-spec))
         (subunit-names (list (word-dependency-spec-unit-name head-word-spec)))
         ;; Now we get the other subunits.
         (subunits (cons
                    ;; First the head of the phrase (e.g. the head Noun of an NP)
                    (cons (word-dependency-spec-unit-name head-word-spec)
                          (find-all-features-for-category
                           (english-retrieve-category head-word-spec)
                           *english-grammar-categories*
                           :features-so-far `((parent ,unit-name)
                                              ,@(unit-body (assoc (word-dependency-spec-unit-name head-word-spec)
                                                                                 preprocessed-units)))))
                    ;; Then we collect other subunits in a way that is as flat as possible.
                    (loop for spec in word-specs
                          ;; We found a subunit other than the Head
                          when (and (= (word-dependency-spec-head-id spec) head-id) ;; We found a subunit
                                    (not (equal spec head-word-spec)))
                          ;; We check whether this unit is lexical or complex (i.e. functional or clausal).
                          append (if (and (functional-dependent-p (word-dependency-spec-syn-role spec))
                                          (not (adverbial-modifier-p spec)))
                                   ;; If the unit itself is a functional unit, recursively collect its subunits.
                                   (let ((new-unit-name (make-const (word-dependency-spec-syn-role spec))))
                                     (push new-unit-name subunit-names)
                                     ;; If it is a clausal unit, build a subclause.
                                     (if (clausal-dependent-p (word-dependency-spec-syn-role spec)
                                                              (word-dependency-spec-pos-tag spec))
                                       (handle-verbal-root spec word-specs
                                                           (if (subject-p (word-dependency-spec-syn-role spec))
                                                             'clausal-subject 'subclause)
                                                           unit-name new-unit-name preprocessed-units)
                                       ;; Else treat it as a simple functional unit.
                                       (handle-functional-unit (word-dependency-spec-syn-role spec)
                                                               spec
                                                               new-unit-name
                                                               unit-name
                                                               word-specs
                                                               preprocessed-units)))
                                   ;; If not, collect the lexical features.
                                   (progn
                                     (push (word-dependency-spec-unit-name spec) subunit-names)
                                     `(,(cons (word-dependency-spec-unit-name spec)
                                              (find-all-features-for-category
                                               (english-retrieve-category spec)
                                               *english-grammar-categories*
                                               :features-so-far `((parent ,unit-name)
                                                                  ,@(unit-body (assoc (word-dependency-spec-unit-name spec)
                                                                                      preprocessed-units))))))))))))
        (cons `(,unit-name
                ,@(cond
                   ((core-function-p function)
                    (find-all-features-for-category 'NP *english-grammar-categories*
                                                    :features-so-far `((constituents ,subunit-names)
                                                                       (head ,(word-dependency-spec-unit-name head-word-spec))
                                                                       (parent ,parent-name))))
                   (t
                    (let ((phrase-type (cond ((dependency-root-p head-word-spec) 'phrase)
                                             ((string= (word-dependency-spec-syn-role head-word-spec)
                                                       "dative")
                                              'dativeNP)
                                             ((adjective-p head-word-spec) 'AdjP)
                                             ((by-phrase-p head-word-spec) 'By-Phrase)
                                             ((prepnp-p head-word-spec) 'PrepNP)
                                             (t
                                              'NP))))
                      (find-all-features-for-category phrase-type *english-grammar-categories*
                                                      :features-so-far `((constituents ,subunit-names)
                                                                         (head
                                                                          ,(word-dependency-spec-unit-name head-word-spec))
                                                                         (parent ,parent-name)))))))
              subunits)))

(defun handle-functional-conjuncts (function head-word-spec unit-name parent-name word-specs &optional preprocessed-units)
  "Handle conjuncts."
  (let* ((starter-units (handle-functional-unit function head-word-spec unit-name parent-name word-specs preprocessed-units))
         (mother-unit (assoc unit-name starter-units))
         (head-word-id (word-dependency-spec-head-id head-word-spec))
         (constituents nil)
         (other-conjuncts (loop for spec in word-specs
                                when (and (= (word-dependency-spec-head-id spec) head-word-id)
                                          (string= "conj" (word-dependency-spec-syn-role spec)))
                                collect spec))
         (conjunctors (loop for spec in word-specs
                            when (and (= (word-dependency-spec-head-id spec) head-word-id)
                                      (string= "cc" (word-dependency-spec-syn-role spec)))
                            collect (progn (push (word-dependency-spec-unit-name spec) constituents)
                                      `(,(word-dependency-spec-unit-name spec)
                                        (parent ,(unit-name mother-unit))
                                        (syn-cat ((lex-class conjunctor)))))))
         (units-to-append (loop for other-conjunct in (cons head-word-spec other-conjuncts)
                                for phrase-unit-name = (make-const "conj")
                                do (push phrase-unit-name constituents)
                                append (handle-functional-unit function other-conjunct phrase-unit-name (unit-name mother-unit)
                                                               word-specs preprocessed-units))))
    (cons `(,(unit-name mother-unit)
            ,@(loop for feature in (unit-body mother-unit)
                            collect (if (eql 'constituents (feature-name feature))
                                      `(constituents ,constituents)
                                      feature)))
          (append conjunctors units-to-append))))
;; (comprehend "I like Mickey Mouse and Donald Duck.")
;; (comprehend "Do you prefer Mickey Mouse, Donald Duck or Goofy?")

;;;;; ----------------------------------------------------------------------------------------------------------------------
;;;;; CLAUSAL units
;;;;; ----------------------------------------------------------------------------------------------------------------------
;;;;;
;;;;; When we have a CLAUSE-building node in the dependency tree, the function handle-verbal-root
;;;;; is used. A CLAUSE-building node is when the ROOT of the dependency tree is verbal, or when
;;;;; a clausal dependency label was encountered.
;;;;; 1. Build a Clause Unit
;;;;; 2. Build a VP unit
;;;;; 3. Find the subunits of the VP unit:
;;;;;    (a) The verbal unit itself, which is the HEAD of the VP
;;;;;    (b) Auxiliaries and Negation modifiers
;;;;;    (c) Mid-position adverbs
;;;;; 4. Search whether there are core dependents: subject, object and dative.
;;;;;    Make them subunits of the CLAUSE and keep a pointer to them in the valence of the VP.
;;;;; 5. Search for other functional units (non-core dependents) and make them subunits of the CLAUSE.

(defun handle-verbal-root (root-spec &optional word-specs
                                     (clause-type 'matrix-clause)
                                     (parent '-)
                                     (clause-unit-name (make-const "clause"))
                                     preprocessed-units)
  "If the root is verbal, we build a CLAUSE."
  (let* (;; We take the node-id of the root verb as the CLAUSE-NUMBER.
         (clause-number (word-dependency-spec-node-id root-spec))
         ;; Handle the VP and its subunits.
         (VP-unit-name (make-const "VP"))
         ;; Now we figure out which units should be subunits of the VP
         (verb-units-1 (loop for word-spec in word-specs
                             when (or (equal word-spec root-spec)
                                      ;; We're dealing with a dependent node
                                      (and (= clause-number (word-dependency-spec-head-id word-spec))
                                           ;; It can be an auxiliary or a negation word or a particle
                                           (or (auxiliary-p word-spec)
                                               (negation-p word-spec)
                                               (particle-p word-spec))))
                             collect `(,(word-dependency-spec-unit-name word-spec)
                                       ,@(find-all-features-for-category
                                          (english-retrieve-category word-spec)
                                          *english-grammar-categories*
                                          :features-so-far `((parent ,vp-unit-name)
                                                             ,@(unit-body (assoc (word-dependency-spec-unit-name word-spec)
                                                                                 preprocessed-units)))))))
         ;; We also look for midposition adverbs.
         (verb-units-2 (loop for word-spec in word-specs
                             when (and (adverbial-modifier-p word-spec)
                                       (string= "RB" (word-dependency-spec-pos-tag word-spec))
                                       (or (= (word-dependency-spec-node-id word-spec) (1- clause-number))
                                           (and (< (word-dependency-spec-node-id word-spec) clause-number)
                                                (loop for x in verb-units-1
                                                    when (> (word-dependency-spec-node-id word-spec)
                                                            (word-dependency-spec-node-id
                                                             (find (first x) word-specs
                                                                   :key #'word-dependency-spec-unit-name)))
                                                    return t))))
                             collect `(,(word-dependency-spec-unit-name word-spec)
                                       ,@(find-all-features-for-category
                                          (english-retrieve-category word-spec)
                                          *english-grammar-categories*
                                          :features-so-far `((parent ,vp-unit-name)
                                                             ,@(unit-body (assoc (word-dependency-spec-unit-name word-spec)
                                                                                 preprocessed-units)))))))
         (verb-units (append verb-units-1 verb-units-2))
         (verb-unit-names (mapcar #'unit-name verb-units))
         ;; Now handle the functional units.
         (subject-unit-name nil)
         (object-unit-name nil)
         (dative-unit-name nil)
         (other-functional-unit-names nil)
         (functional-units (loop for word-spec in (remove-if #'(lambda(x)
                                                                 (assoc (word-dependency-spec-unit-name x)
                                                                        verb-units))
                                                             word-specs)
                                 for function = (word-dependency-spec-syn-role word-spec)
                                 for unit-name = (when (and (functional-dependent-p function)
                                                            (= clause-number
                                                               (word-dependency-spec-head-id word-spec)))
                                                   (cond ((subject-p function)
                                                          (setf subject-unit-name (make-const function)))
                                                         ((object-p function)
                                                          (setf object-unit-name (make-const function)))
                                                         ((dative-p function)
                                                          (setf dative-unit-name (make-const function)))
                                                         (t
                                                          (first (push (make-const function)
                                                                       other-functional-unit-names)))))
                                 when unit-name
                                 append (cond ((clausal-dependent-p function)
                                               (handle-verbal-root word-spec
                                                                   word-specs
                                                                   (if (subject-p function)
                                                                     'clausal-subject 'subclause)
                                                                   clause-unit-name unit-name preprocessed-units))
                                              ((word-dependency-spec-conjunct-type word-spec)
                                               (handle-functional-conjuncts function word-spec unit-name
                                                                            clause-unit-name word-specs
                                                                            preprocessed-units))
                                              (t
                                               (handle-functional-unit
                                                function word-spec unit-name
                                                clause-unit-name word-specs preprocessed-units)))))
         (clause-subunits (append (listify subject-unit-name)
                                  (listify object-unit-name)
                                  (listify dative-unit-name)
                                  (list vp-unit-name)
                                  other-functional-unit-names)))
    (append `(;; The main clause.
              (,clause-unit-name
               ,@(find-all-features-for-category clause-type *english-grammar-categories*
                                                 :features-so-far `((constituents ,clause-subunits)
                                                                    (parent ,parent))))
              ;; The VP and its subunits.
              (,vp-unit-name
               ,@(find-all-features-for-category
                  'VP *english-grammar-categories*
                  :features-so-far `((constituents ,verb-unit-names)
                                     (parent ,clause-unit-name)
                                     (head ,(word-dependency-spec-unit-name root-spec))))))
            verb-units
            ;; The functional units.
            functional-units)))
;; (comprehend "I see Mickey Mouse and Donald Duck.")

(defun nominal-phrase-p (unit)
  (find '(phrase-type NP) (unit-feature-value unit 'syn-cat) :test #'unify))

(defun lexical-unit-p (unit)
  (find '(lex-class ?lex-class) (unit-feature-value unit 'syn-cat) :test #'unify))

(defun english-link-args-and-referent (words referent-var)
  (let ((unit-1 `(,(unit-name (first words))
                  (referent ,referent-var)
                  ,@(loop for feature in (unit-body (first words))
                          unless (string= "REFERENT" (symbol-name (feature-name feature)))
                          collect feature))))
    (if (null (rest words))
      (list unit-1)
      (let* ((arg-var (last-elt (unit-feature-value unit-1 'args)))
             (args-feature `(args (,arg-var ,(or (second (unit-feature-value (second words) 'args))
                                                 (make-var)))))
             (unit-2 `(,(unit-name (second words))
                       ,args-feature
                       ,@(loop for feature in (unit-body (second words))
                               unless (string= "ARGS" (symbol-name (feature-name feature)))
                               collect feature))))
        (cons unit-1 (english-link-args-and-referent (cons unit-2 (cddr words)) referent-var))))))

(defun english-link-args-in-preprocessing (units)
  (dolist (unit-boundaries (fcg-get-boundaries units))
    (let ((unit (assoc (first unit-boundaries) units)))
      (when (nominal-phrase-p unit)
        (let* ((referent-var (or (unit-feature-value unit 'referent) (make-var 'ref)))
               (words (loop for boundary in (fcg-get-boundaries units)
                            for unit = (assoc (first boundary) units)
                            when (and (>= (second boundary) (second unit-boundaries))
                                      (<= (third boundary) (third unit-boundaries))
                                      (= 1 (- (third boundary) (second boundary)))
                                      (not (nominal-phrase-p unit)))
                            collect unit))
               (new-words (english-link-args-and-referent words referent-var)))
          (setf units (loop for unit in units
                            collect (or (assoc (unit-name unit) new-words)
                                        unit)))))))
  units)
; (comprehend "the pretty girl")

;;; The translation of the English grammar looks at the root and decides whether it represents
;;; a clausal unit (i.e. we have a sentence with full propositional content) or a functional
;;; unit (such as a noun phrase).
;;; --------------------------------------------------------------------------------------------
(defmethod translate-dependency-tree ((base-transient-structure coupled-feature-structure)
                                      (dependency-tree list)
                                      (conversion-table clear-dependency)
                                      &key &allow-other-keys)
  "Given a dependency tree analysis, generate an 'initial' transient structure."
  (let* ((boundaries (fcg::fcg-get-boundaries base-transient-structure))
         (word-specs (make-word-specs-for-boundaries boundaries dependency-tree))
         (dependency-view-spec
          ;; we want to include the dependency tree as a separate view.
          (loop for word-spec in word-specs
                for dependents = (loop for other-word-spec in (remove word-spec word-specs :test #'equal)
                                       when (and (= (word-dependency-spec-node-id word-spec)
                                                    (word-dependency-spec-head-id other-word-spec)))
                                       collect (word-dependency-spec-unit-name other-word-spec))
                when dependents
                collect (list (word-dependency-spec-unit-name word-spec) dependents)))
         (dependency-root (find "ROOT" word-specs :key #'word-dependency-spec-syn-role :test #'string=))
         ;; Start with either a verbal root (clausal) or a non-verbal root (non-clausal)
         (structure-to-append (if (verbal-root-p dependency-root)
                                (handle-verbal-root dependency-root word-specs)
                                (handle-functional-unit "ROOT" dependency-root (make-const 'unit) '-
                                                        word-specs)))
         ;; Recalculate the boundaries based on the result.
         (new-root (calculate-boundaries-and-form-constraints base-transient-structure
                                                              structure-to-append)))
    (setf (left-pole-structure base-transient-structure)
          ;; For including the dependency tree view:
          (cons new-root (loop for unit in structure-to-append
                               collect (let ((dependency-view (assoc (unit-name unit) dependency-view-spec)))
                                         (if dependency-view
                                           (cons (unit-name unit)
                                                 (cons `(dependents ,(second dependency-view))
                                                       (unit-body unit)))
                                           unit)))))
    ;; Now pre-equalize variables in Noun Phrases. (commented out because it is slow)
    ;;(setf (left-pole-structure base-transient-structure)
    ;;      (english-link-args-in-preprocessing (left-pole-structure base-transient-structure)))
    ;; Return the translated transient structure:
    base-transient-structure))
