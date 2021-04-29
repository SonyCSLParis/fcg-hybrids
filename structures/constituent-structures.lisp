;; Copyright 2019-present
;;           Sony Computer Science Laboratories Paris
;;           Remi van Trijp (http://www.remivantrijp.eu)

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

(in-package :fcg)

(export '(represent-constituent-structure))

;; ICA = Immediate Constituent Analysis
;;
;; Contents:
;; 1- Helper Functions

;; 1- Helper functions
;; -------------------------------------------------------------------------
(defun convert-ica-string-to-ica-list (string)
  "Converts a string representing a constituent analysis into a list representation."
  (loop for pair in '(("." "\\.")
                      ("," "\\,")
                      ("''" "PARENTH")
                      ("``" "PARENTH")
                      ("\"" "PARENTH"))
        do (setf string (string-replace string (first pair) (second pair))))
  (read-from-string string))

;; 2- Representing Constituent Structures
;; -------------------------------------------------------------------------

(defgeneric represent-constituent-structure (analysis transient-structure key cxn-inventory &optional language)
  (:documentation "Given a constituent structure analysis, expand the transient structure with units for phrases and constituency relations."))
