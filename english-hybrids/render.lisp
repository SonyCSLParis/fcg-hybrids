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

(in-package :fcg)

(defmethod render ((cfs coupled-feature-structure)
                   (mode (eql :english-render))
                   &key node &allow-other-keys)
  (render (left-pole-structure cfs) mode :node node))

;; Very simplistic render, should be updated for including punctuation, etc.
(defmethod render ((unit-structure list)
                   (mode (eql :english-render))
                   &key node &allow-other-keys)
  ;; First we get the utterance as we would normally get.
  (let ((utterance (render unit-structure :render-with-scope :node node)))
    (when utterance
      (let ((first-char (first (coerce (first utterance) 'list))))
        (if (char= (char-upcase first-char) first-char)
          utterance
          (cons (string-capitalize (first utterance))
                (rest utterance)))))))
