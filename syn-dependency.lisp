;;;;; Copyright (c) Sony Computer Science Laboratories Paris
;;;;;               VUB Artificial Intelligence Laboratory, Brussels
;;;;;
;;;;; File written by Remi van Trijp (remi.vantrijp@sony.com)
;;;;; -----------------------------------------------------------------------------------

; (ql:quickload :fcg-hybrids)

(in-package :fcg)

(export '(fcg-get-dependency-conversion-table
          fcg-set-dependency-conversion-table
          translate-dependency-tree
          make-word-specs-for-boundaries
          show-translated-sentence))

;;; Translate dependency tree.
;;; -------------------------------------------------------------------------------------
(defgeneric translate-dependency-tree (base-transient-structure
                                       dependency-tree
                                       conversion-table
                                       &key cxn-inventory &allow-other-keys))

(defun make-word-specs-for-boundaries (boundaries dependency-tree)
  "A useful function to have for customizing the translate-dependency-tree method."
  (loop for boundary in boundaries
        for dependency in dependency-tree
        collect (make-word-dependency-spec :string (nlp-tools::dp-get-token dependency)
                                           :unit-name (first boundary)
                                           :syn-role (nlp-tools::dp-get-dependency dependency)
                                           :pos-tag (nlp-tools::dp-get-tag dependency)
                                           :node-id (parse-integer
                                                     (nlp-tools::dp-get-node-id dependency))
                                           :head-id (nlp-tools::dp-get-head-id dependency))))

(defmethod translate-dependency-tree ((base-transient-structure coupled-feature-structure)
                                      (dependency-tree list)
                                      (conversion-table t)
                                      &key cxn-inventory &allow-other-keys)
  (declare (ignore cxn-inventory))
  (let* ((boundaries (fcg-get-boundaries base-transient-structure))
         (word-specs (make-word-specs-for-boundaries boundaries dependency-tree))
         (units-id-and-name (loop for spec in word-specs
                                  collect (list (word-dependency-spec-node-id spec)
                                                (word-dependency-spec-unit-name spec))))
         (structure-to-append (loop for word-spec in word-specs
                                    for parent = (unless (= (word-dependency-spec-node-id word-spec)
                                                            (word-dependency-spec-head-id word-spec))
                                                   (second (assoc (word-dependency-spec-head-id word-spec)
                                                                  units-id-and-name)))
                                    for subunits = (loop for other-word-spec in word-specs
                                                         when (and
                                                               (not (equal word-spec other-word-spec))
                                                               (= (word-dependency-spec-head-id other-word-spec)
                                                                  (word-dependency-spec-node-id word-spec)))
                                                         collect (word-dependency-spec-unit-name other-word-spec))
                                    collect (make-unit :name (word-dependency-spec-unit-name word-spec)
                                                       :features `((parent ,parent)
                                                                   (subunits ,subunits)
                                                                   (form ((string ,(word-dependency-spec-unit-name word-spec)
                                                                                  ,(word-dependency-spec-string word-spec))))
                                                                   (dependency
                                                                    ((pos-tag
                                                                      ,(intern (upcase (word-dependency-spec-pos-tag word-spec)) :fcg))
                                                                     (edge
                                                                      ,(intern (upcase (word-dependency-spec-syn-role word-spec)) :fcg))))))))
         (new-root (calculate-boundaries-and-form-constraints base-transient-structure structure-to-append)))
    (setf (left-pole-structure base-transient-structure)
          (cons new-root structure-to-append))
    base-transient-structure))

;;; Showing the tree.
;;; -------------------------------------------------------------------------------------
(defun show-translated-sentence (utterance de-render-mode &key (cxn-inventory *fcg-constructions*))
  "For showing the translated transient structure in the web interface."
  (let ((transient-structure (de-render utterance de-render-mode
                                        :cxn-inventory cxn-inventory)))
    (add-element (make-html-fcg-light transient-structure :cxn-inventory cxn-inventory
                                      :configuration (visualization-configuration cxn-inventory)
                                      :feature-types (feature-types cxn-inventory)))
    transient-structure))
