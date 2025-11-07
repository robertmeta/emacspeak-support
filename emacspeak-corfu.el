;;; emacspeak-corfu.el --- Speech-enable Corfu -*- lexical-binding: t; -*-
;; Description: Speech-enable Corfu, a modern in-buffer completion interface
;; Keywords: Emacspeak, Audio Desktop, Corfu, completion

;;;   Copyright:
;; This file is not part of GNU Emacs, but the same permissions apply.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Corfu is a modern in-buffer completion UI that uses Emacs's native
;; completion engine. This module speech-enables Corfu's UI to provide
;; auditory feedback for completion candidates.

;;; Code:

;;   Required modules:

(eval-when-compile (require 'cl-lib))
(cl-declaim (optimize (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'corfu nil 'noerror)

;;;  Map faces to voices:

(voice-setup-add-map
 '((corfu-default voice-smoothen)
   (corfu-current voice-bolden)
   (corfu-bar voice-monotone)
   (corfu-border voice-smoothen)
   (corfu-annotations voice-annotate)
   (corfu-deprecated voice-monotone-extra)))

;;;  Define bookkeeping variables for UI state:

(defvar-local emacspeak-corfu--prev-candidate nil
  "Previously spoken candidate.")

(defvar-local emacspeak-corfu--prev-index nil
  "Index of previously spoken candidate.")

;;;  Helper functions:

(defun emacspeak-corfu--current-candidate ()
  "Return the currently selected candidate in Corfu."
  (when (and (bound-and-true-p corfu--candidates)
             (bound-and-true-p corfu--index)
             (>= corfu--index 0)
             (< corfu--index (length corfu--candidates)))
    (nth corfu--index corfu--candidates)))

(defun emacspeak-corfu--candidate-with-annotation ()
  "Return current candidate with its annotation if available."
  (let ((candidate (emacspeak-corfu--current-candidate)))
    (when candidate
      (let* ((metadata (and (boundp 'corfu--metadata) corfu--metadata))
             (annotate (and metadata
                            (completion-metadata-get metadata 'annotation-function)))
             (annotation (and annotate (funcall annotate candidate))))
        (if annotation
            (concat candidate " " (propertize annotation 'personality 'voice-annotate))
          candidate)))))

;;;  Advice interactive commands:

(defadvice corfu-next (after emacspeak pre act comp)
  "Speak the newly selected candidate."
  (when (ems-interactive-p)
    (emacspeak-icon 'select-object)
    (let ((candidate (emacspeak-corfu--candidate-with-annotation)))
      (when candidate
        (dtk-speak candidate)))))

(defadvice corfu-previous (after emacspeak pre act comp)
  "Speak the newly selected candidate."
  (when (ems-interactive-p)
    (emacspeak-icon 'select-object)
    (let ((candidate (emacspeak-corfu--candidate-with-annotation)))
      (when candidate
        (dtk-speak candidate)))))

(defadvice corfu-insert (after emacspeak pre act comp)
  "Speak the inserted candidate."
  (when (ems-interactive-p)
    (emacspeak-icon 'complete)
    (dtk-speak (or emacspeak-corfu--prev-candidate "completed"))))

(defadvice corfu-complete (after emacspeak pre act comp)
  "Speak completion feedback."
  (when (ems-interactive-p)
    (emacspeak-icon 'complete)))

(defadvice corfu-quit (after emacspeak pre act comp)
  "Speak quit feedback."
  (when (ems-interactive-p)
    (emacspeak-icon 'close-object)
    (dtk-speak "completion cancelled")))

(defadvice corfu-insert-separator (after emacspeak pre act comp)
  "Speak separator insertion."
  (when (ems-interactive-p)
    (emacspeak-icon 'item)
    (dtk-speak "separator")))

;;;  Advice internal update function to speak candidate changes:

(defadvice corfu--update (after emacspeak pre act comp)
  "Speak candidate updates as they happen."
  (cl-declare (special corfu--index corfu--candidates))
  (when (and (boundp 'corfu--index)
             (boundp 'corfu--candidates)
             corfu--candidates)
    (let ((new-cand (emacspeak-corfu--current-candidate))
          (to-speak nil))
      (unless (equal emacspeak-corfu--prev-candidate new-cand)
        (setq to-speak (emacspeak-corfu--candidate-with-annotation))
        ;; Play icon when selection changes
        (when (and (not (equal corfu--index emacspeak-corfu--prev-index))
                   (>= corfu--index 0))
          (emacspeak-icon 'select-object)))
      (when to-speak
        (dtk-speak to-speak))
      (setq-local
       emacspeak-corfu--prev-candidate new-cand
       emacspeak-corfu--prev-index corfu--index))))

;;;  Provide the module:

(provide 'emacspeak-corfu)

;;; emacspeak-corfu.el ends here
