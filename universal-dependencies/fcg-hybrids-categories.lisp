;; Copyright 2022 Sony Computer Science Laboratories Paris
;; Authors:       Remi van Trijp (http://www.remivantrijp.eu)

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

(defvar *fcg-hybrids-categories* nil
  "A basic category set to be used with universal dependencies.")

(setf *fcg-hybrids-categories*
      '((noun ()
              (referent ?ref))
        (adjective ()
                   (referent ?ref))
        (proper-noun (noun))
        (verb ()
              (referent ?ev-ref))
        (determiner ()
                    (referent ?ref))
        (pronoun (noun))))