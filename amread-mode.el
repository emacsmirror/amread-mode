;;; amread-mode.el --- A minor mode helper user speed-reading -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-06-23 23:44:49 stardiviner>

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "24.3") (cl-lib "0.6.1"))
;; Package-Version: 0.1
;; Keywords: wp
;; homepage: https://repo.or.cz/amread-mode.git

;; amread-mode is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; amread-mode is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; 
;;; Usage
;;;
;;; 1. Launch amread-mode with command `amread-mode'.
;;; 2. Stop amread-mode by pressing [q].

;;; Code:
(require 'cl-lib)


(defcustom amread-word-speed 3.0
  "Read words per second."
  :type 'float
  :safe #'floatp
  :group 'amread-mode)

(defcustom amread-line-speed 4.0
  "Read one line using N seconds in average."
  :type 'float
  :safe #'floatp
  :group 'amread-mode)

(defcustom amread-scroll-style nil
  "Set amread auto scroll style by word or line."
  :type '(choice (const :tag "scroll by word" word)
                 (const :tag "scroll by line" line))
  :safe #'symbolp
  :group 'amread-mode)

(defcustom amread-voice-reader-enabled t
  "The initial state of voice reader."
  :type 'boolean
  :safe #'booleanp
  :group 'amread-mode)

(defcustom amread-voice-reader-command
  (cl-case system-type
    (darwin "say")
    (gnu/linux (or (executable-find "espeak") (executable-find "festival")))
    (windows-nt ))                      ; TODO
  "The command for reading text."
  :type 'string
  :safe #'stringp
  :group 'amread-mode)

(defcustom amread-voice-reader-command-options ""
  "Specify options for voice reader command."
  :type 'string
  :safe #'stringp
  :group 'amread-mode)

(defface amread-highlight-face
  '((t :foreground "black" :background "ForestGreen"))
  "Face for amread-mode highlight."
  :group 'amread-mode)

(defvar amread--timer nil)
(defvar amread--current-position nil)
(defvar amread--overlay nil)

(defvar amread--voice-reader-proc-finished nil
  "A process status variable indicate whether voice reader finished reading.
It has three status values:
- 'not-started :: process not started
- 'running     :: process still running
- 'finished    :: process finished")

(defmacro amread--voice-reader-status-wrapper (body)
  "A wrapper macro for detecting voice reader process status and execute BODY."
  `(if amread-voice-reader-enabled
       ;; wait for process finished, then jump to next word.
       (cl-case amread--voice-reader-proc-finished
         (not-started ,body)
         (running (ignore))
         (finished ,body)
         (t (setq amread--voice-reader-proc-finished 'not-started)))
     ,body))

;; (macroexpand-1
;;  '(amread--voice-reader-status-wrapper (amread--word-update)))

(defun amread--voice-reader-read-text (text)
  "Read TEXT with voice command-line tool."
  (when (and amread-voice-reader-enabled
             (not (null text))
             (not (string-empty-p text)))
    (setq amread--voice-reader-proc-finished 'running)

    ;; detect language and switch language/voice.
    (amread-voice-reader-switch-language-voice)
    
    ;; Synchronous Processes
    ;; (call-process-shell-command
    ;;  amread-voice-reader-command
    ;;  nil nil nil
    ;;  amread-voice-reader-command-options
    ;;  (shell-quote-argument text))
    
    ;; Async Process
    (make-process
     :name "amread-voice-reader"
     :command (list amread-voice-reader-command amread-voice-reader-command-options text)
     :sentinel (lambda (proc event)
                 (if (string= event "finished\n")
                     (setq amread--voice-reader-proc-finished 'finished)))
     :buffer " *amread-voice-reader*"
     :stderr " *amread-voice-reader*")))

(defun amread--word-update ()
  "Scroll forward by word as step."
  (let* ((begin (point))
         ;; move point forward. NOTE This forwarding must be here before moving overlay forward.
         (_length (+ (skip-chars-forward "^\s\t\n—") (skip-chars-forward "—")))
         (end (point))
         (word (buffer-substring-no-properties begin end)))
    (if (eobp)
        (progn
          (amread-mode -1)
          (setq amread--current-position nil))
      ;; create the overlay if does not exist
      (unless amread--overlay
        (setq amread--overlay (make-overlay begin end)))
      ;; move overlay forward
      (when amread--overlay
        (move-overlay amread--overlay begin end))
      (setq amread--current-position (point))
      (overlay-put amread--overlay 'face 'amread-highlight-face)
      ;; read word text
      (amread--voice-reader-read-text word)
      (skip-chars-forward "\s\t\n—"))))

(defun amread--line-update ()
  "Scroll forward by line as step."
  (let* ((line-begin (line-beginning-position))
         (line-end (line-end-position))
         (line-text (buffer-substring-no-properties line-begin line-end)))
    (if (eobp) ; reached end of buffer.
        (progn
          (amread-mode -1)
          (setq amread--current-position nil))
      ;; create line overlay to highlight current reading line.
      (unless amread--overlay
        (setq amread--overlay (make-overlay line-begin line-end)))
      ;; scroll down line
      (when amread--overlay
        (move-overlay amread--overlay line-begin line-end))
      (overlay-put amread--overlay 'face 'amread-highlight-face)
      ;; read line text
      (amread--voice-reader-read-text line-text)
      (forward-line 1))))

(defun amread--update ()
  "Update and scroll forward under Emacs timer."
  (cl-case amread-scroll-style
    (word
     (amread--voice-reader-status-wrapper (amread--word-update)))
    (line
     (amread--voice-reader-status-wrapper (amread--line-update))
     ;; Auto modify the running timer REPEAT seconds based on next line words length.
     (let* ((next-line-words (amread--get-next-line-words)) ; for English
            ;; TODO: Add Chinese text logic.
            ;; (next-line-length (amread--get-next-line-length)) ; for Chinese
            (amread--next-line-pause-secs (truncate (/ next-line-words amread-word-speed))))
       (when (> amread--next-line-pause-secs 0)
         (setf (timer--repeat-delay amread--timer) amread--next-line-pause-secs))))
    (t (user-error "Seems amread-mode is not normally started or not running."))))

(defun amread--scroll-style-ask ()
  "Ask which scroll style to use."
  (let ((style (intern (completing-read "amread-mode scroll style: " '("word" "line")))))
    (setq amread-scroll-style style)
    style))

(defun amread--get-line-words (&optional pos)
  "Get the line words of position."
  (save-excursion
    (and pos (goto-char pos))
    (beginning-of-line)
    (count-words (line-end-position) (line-beginning-position))))

(defun amread--get-next-line-words ()
  "Get the next line words."
  (amread--get-line-words (save-excursion (forward-line) (point))))

(defun amread--get-line-length (&optional pos)
  "Get the line length of position."
  (save-excursion
    (and pos (goto-char pos))
    (- (line-end-position) (line-beginning-position))))

(defun amread--get-next-line-length ()
  "Get the next line length."
  (amread--get-line-words (save-excursion (forward-line) (point))))

;;;###autoload
(defun amread-start ()
  "Start / resume amread."
  (interactive)
  (read-only-mode 1)
  ;; if quit `amread--scroll-style-ask', then don't enable `amread-mode'.
  (or amread-scroll-style (amread--scroll-style-ask))
  (setq amread--voice-reader-proc-finished 'not-started)
  (if (null amread-scroll-style)
      (user-error "User quited entering amread-mode.")
    ;; resume from paused position
    (cl-case amread-scroll-style
      (word
       (when amread--current-position
         (goto-char amread--current-position))
       (setq amread--timer
             (run-with-timer 0 (/ 1.0 amread-word-speed) #'amread--update)))
      (line
       (when amread--current-position
         (goto-char (point-min))
         (forward-line amread--current-position))
       (setq amread--timer
             (run-with-timer 1 amread-line-speed #'amread--update)))
      (t (user-error "Seems amread-mode is not normally started because of not selecting scroll style OR just not running.")))
    (message "[amread] start reading...")))

;;;###autoload
(defun amread-stop ()
  "Stop amread."
  (interactive)
  (when amread--timer
    (cancel-timer amread--timer)
    (setq amread--timer nil)
    (when amread--overlay
      (delete-overlay amread--overlay)))
  (setq amread-scroll-style nil)
  (read-only-mode -1)
  (message "[amread] stopped."))

;;;###autoload
(defun amread-pause-or-resume ()
  "Pause or resume amread."
  (interactive)
  (if amread--timer
      (amread-stop)
    (amread-start)))

;;;###autoload
(defun amread-mode-quit ()
  "Disable `amread-mode'."
  (interactive)
  (amread-mode -1))

;;;###autoload
(defun amread-speed-up ()
  "Speed up `amread-mode'."
  (interactive)
  (setq amread-word-speed (cl-incf amread-word-speed 0.2))
  (message "[amread] word speed increased -> %s" amread-word-speed))

;;;###autoload
(defun amread-speed-down ()
  "Speed down `amread-mode'."
  (interactive)
  (setq amread-word-speed (cl-decf amread-word-speed 0.2))
  (message "[amread] word speed decreased -> %s" amread-word-speed))

;;;###autoload
(defun amread-voice-reader-toggle ()
  "Toggle text voice reading."
  (interactive)
  (if amread-voice-reader-enabled
      (progn
        (setq amread-voice-reader-enabled nil)
        (message "[amread] voice reader disabled."))
    (setq amread-voice-reader-enabled t)
    (message "[amread] voice reader enabled.")))

(defun amread--voice-reader-detect-language (&optional string)
  "Detect text language."
  ;; Return t if STRING is a Chinese string.
  (let ((string (or string (word-at-point))))
    (cond
     ((string-match (format "\\cC\\{%s\\}" (length string)) string)
      'chinese)
     (t 'english))))

;; (amread--voice-reader-detect-language "测试")
;; (amread--voice-reader-detect-language "测试test")

;;;###autoload
(defun amread-voice-reader-switch-language-voice (&optional language)
  "Switch voice reader LANGUAGE or voice."
  (interactive)
  (let ((language (or language (amread--voice-reader-detect-language))))
    (pcase amread-voice-reader-command
      ("say"
       (cl-case language
         (chinese
          (setq amread-voice-reader-command-options "--voice=Ting-Ting")
          (message "[amread] voice reader switched to Chinese language/voice."))
         (english
          (setq amread-voice-reader-command-options "--voice=Ava")
          (message "[amread] voice reader switched to English language/voice."))
         (t
          (let ((voice (completing-read "[amread] Select language/voice: " '("Ting-Ting" "Ava"))))
            (setq amread-voice-reader-command-options (format "--voice=%s" voice))
            (message "[amread] voice reader switched to language/voice <%s>." voice))))))))

;;;###autoload
(defun amread-voice-reader-read-buffer ()
  "Read current buffer text without timer highlight updating."
  (interactive)
  ;; loop over all lines of buffer.
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (not (eobp))               ; <- main code
        (let* ((line-begin (line-beginning-position))
               (line-end (line-end-position))
               (line-text (buffer-substring-no-properties line-begin line-end)))
          ;; line processiqng
          (let ((amread-voice-reader-enabled t))
            (amread--voice-reader-status-wrapper (amread--voice-reader-read-text line-text)))
          (forward-line 1))))))

(defvar amread-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'amread-mode-quit)
    (define-key map (kbd "SPC") #'amread-pause-or-resume)
    (define-key map [remap keyboard-quit] #'amread-mode-quit)
    (define-key map (kbd "+") #'amread-speed-up)
    (define-key map (kbd "-") #'amread-speed-down)
    (define-key map (kbd "v") #'amread-voice-reader-toggle)
    (define-key map (kbd "L") #'amread-voice-reader-switch-language-voice)
    map)
  "Keymap for `amread-mode' buffers.")

;;;###autoload
(define-minor-mode amread-mode
  "I'm reading mode."
  :init nil
  :lighter " amreading"
  :keymap amread-mode-map
  (if amread-mode
      (amread-start)
    (amread-stop)))



(provide 'amread-mode)

;;; amread-mode.el ends here
