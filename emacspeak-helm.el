;;; emacspeak-helm.el --- Speech-enable Helm -*- lexical-binding: t; -*-
;; Description: Speech-enable Helm completion framework
;; Keywords: Emacspeak, Audio Desktop, Helm, completion
;; Author: Parham Doustdar (patch), Robert Melton (packaging)

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
;; Helm is a powerful incremental completion and selection narrowing
;; framework for Emacs. This module provides improved speech support
;; for Helm, specifically addressing issues in helm-help mode.
;;
;; This module fixes two major issues with Helm help:
;; 1. Emacspeak kept reading the helm help default prompt while
;;    navigating (now properly silenced via emacspeak-speak-messages)
;; 2. Emacspeak prefix key (C-e) didn't work in helm help mode
;;    (now has custom dispatcher)
;;
;; Based on patch by Parham Doustdar to the Emacspeak mailing list
;; (December 26, 2025). This module implements BOTH the helm-specific
;; improvements AND the emacspeak-advice.el changes from the original
;; patch, making it a complete standalone solution.
;;
;; Implementation notes:
;; - Advises helm-help-event-loop to set/unset emacspeak-speak-messages
;; - Advises dtk-notify and ems--log-message to respect emacspeak-speak-messages
;; - Adds Emacspeak prefix key dispatcher to helm-help-keymap

;;; Code:

;;   Required modules:

(require 'emacspeak-preamble)
(require 'helm nil 'noerror)

;;;  Read-key prompt silencing:
;; These advices implement the emacspeak-advice.el changes from Parham's
;; patch, which add emacspeak-speak-messages guards to prevent read-key
;; prompts from being spoken when emacspeak-speak-messages is nil.
;;
;; Note: These advices affect ALL uses of dtk-notify and ems--log-message,
;; not just in helm contexts. This is intentional - when emacspeak-speak-messages
;; is nil (as set by helm-help-event-loop), we want to silence all automatic
;; speech, which is exactly what these functions do.

(defadvice dtk-notify (around emacspeak-helm-respect-silence pre act comp)
  "Respect emacspeak-speak-messages variable.
When emacspeak-speak-messages is nil, skip notification entirely.
This prevents helm help prompts and other automatic messages from being
spoken when speech has been intentionally silenced."
  (when emacspeak-speak-messages
    ad-do-it))

(defadvice ems--log-message (around emacspeak-helm-respect-silence pre act comp)
  "Respect emacspeak-speak-messages variable.
When emacspeak-speak-messages is nil, skip message logging.
Works in conjunction with dtk-notify advice to fully silence
automatic messages in contexts like helm help mode."
  (when emacspeak-speak-messages
    ad-do-it))

;;;  Helm help improvements:

(defadvice helm-help-event-loop (around emacspeak pre act comp)
  "Silence messages during helm help loop.
This advice properly sets emacspeak-speak-messages to nil during
the helm help event loop, preventing the prompt from being spoken
repeatedly while navigating help text."
  (setq emacspeak-speak-messages nil)
  (emacspeak-icon 'open-object)
  (unwind-protect
      ad-do-it
    (setq emacspeak-speak-messages t)
    (emacspeak-icon 'close-object)))

(defun emacspeak-helm-help-dispatch-prefix ()
  "Dispatch Emacspeak prefix (C-e) commands in helm help.
Helm help has its own command dispatcher which normally prevents
the Emacspeak prefix key from working. This function reads the
next key after the prefix and executes the corresponding Emacspeak
command from the emacspeak-keymap."
  (interactive)
  (let* ((key (read-key-sequence-vector
               (format "%s " (key-description emacspeak-prefix))))
         (cmd (lookup-key emacspeak-keymap key)))
    (when (commandp cmd)
      (call-interactively cmd))))

;;;  Setup:

(defun emacspeak-helm-setup ()
  "Setup Emacspeak support for Helm."
  (when (and (featurep 'helm-lib)
             (boundp 'emacspeak-prefix))
    ;; Add Emacspeak prefix key dispatcher to helm help keymap
    (helm-help-define-key
     (key-description emacspeak-prefix)
     #'emacspeak-helm-help-dispatch-prefix)))

(eval-after-load "helm-lib"
  '(emacspeak-helm-setup))

;;;  Enable/Disable support:

(defvar emacspeak-helm--advice-list
  '((helm-help-event-loop around emacspeak)
    (dtk-notify around emacspeak-helm-respect-silence)
    (ems--log-message around emacspeak-helm-respect-silence))
  "List of advised functions for Emacspeak Helm support.
Each entry is (FUNCTION CLASS ADVICE-NAME).")

(defun emacspeak-helm-enable ()
  "Enable Emacspeak support for Helm."
  (interactive)
  (dolist (advice emacspeak-helm--advice-list)
    (ad-enable-advice (nth 0 advice) (nth 1 advice) (nth 2 advice))
    (ad-activate (nth 0 advice)))
  (emacspeak-helm-setup)
  (message "Enabled Emacspeak Helm support"))

(defun emacspeak-helm-disable ()
  "Disable Emacspeak support for Helm."
  (interactive)
  (dolist (advice emacspeak-helm--advice-list)
    (ad-disable-advice (nth 0 advice) (nth 1 advice) (nth 2 advice))
    (ad-activate (nth 0 advice)))
  ;; Note: Cannot easily remove key from helm-help-keymap
  (message "Disabled Emacspeak Helm support"))

;;;  Provide the module:

(provide 'emacspeak-helm)

;;; emacspeak-helm.el ends here
