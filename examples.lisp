;;;;; Copyright (c) Sony Computer Science Laboratories Paris
;;;;;               VUB Artificial Intelligence Laboratory, Brussels
;;;;;
;;;;; File written by Remi van Trijp (remi.vantrijp@sony.com)
;;;;; -----------------------------------------------------------------------------------

(in-package :fcg)

;;;;; Example of the basic syntactic dependency parser.
;;;;; -------------------------------------------------
(defmethod de-render ((utterance string) (mode (eql :example-dependency-translation))
                      &key cxn-inventory &allow-other-keys)
  (declare (ignorable mode))
  (multiple-value-bind (dependency-tree utterance-as-list base-transient-structure)
      (preprocess-using-dependency-tree utterance :cxn-inventory cxn-inventory)
    (translate-dependency-tree base-transient-structure
                               dependency-tree
                               (fcg-get-dependency-conversion-table cxn-inventory))))
;; (show-translated-sentence "Oxygen levels in oceans have fallen 2%, affecting marine habitat and large fish." :example-dependency-translation)

(defmethod de-render ((utterance string) (mode (eql :example-dependency-translation-with-preprocessing))
                      &key cxn-inventory &allow-other-keys)
  (declare (ignorable mode))
  (multiple-value-bind (dependency-tree utterance-as-list base-transient-structure)
      (preprocess-using-dependency-tree utterance
                                        ;; These steps are executed in order!!
                                        :preprocessing-steps (list #'dependency-string-append-compounds-in-np
                                                                   #'dependency-string-append-named-entities
                                                                   #'dependency-string-append-compounds)
                                        :cxn-inventory cxn-inventory)
    (translate-dependency-tree base-transient-structure
                               dependency-tree
                               (fcg-get-dependency-conversion-table cxn-inventory))))
;; (show-translated-sentence "The ever-popular Barack Obama warned about the falling oxygen levels in oceans." :example-dependency-translation-with-preprocessing)