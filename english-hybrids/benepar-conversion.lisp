;; Copyright 2020 Sony Computer Science Laboratories Paris
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

(in-package :fcg)

(defparameter *benepar-conversion-table* nil)

(setf *benepar-conversion-table* '((np np)))

(defun fetch-benepar-category (category)
  "Find a conversion category, if not return itself."
  (or (second (assoc category *benepar-conversion-table* :test #'string= :key #'symbol-name))
      category))
;; (fetch-benepar-category 'np)
