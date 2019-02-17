;;; yo.el
;;;
;;; Copyright (C) 1997, 1998, 2004, 2007, 2008 Thien-Thi Nguyen
;;;
;;; This file is part of ttn's personal elisp library, released under
;;; the terms of the GNU General Public License as published by the
;;; Free Software Foundation; either version 3, or (at your option) any
;;; later version.  There is NO WARRANTY.  See file COPYING for details.

;;; Description: Alternative notification.

(require 'cl)

;;;###autoload
(defun starfield-string (len)
  (let ((s (make-string (max 0 len) ?*)))
    (dotimes (i len)
      (when (= 0 (random 2))
        (aset s i ?-)))
    s))

;;;###autoload
(defun yo! (msg &optional style)
  "Reworks mode line to display MSG in high STYLE.
Styles are: simple-message, message, funky, funky-flash, bounce, t."
  (let ((standard-message (concat ">>> " msg " <<<")))
    (case style
      (simple-message
       (message msg)
       (sit-for 1))
      (message
       (message standard-message)
       (sit-for 1))
      (funky
       (let ((mode-line-format (concat "YO! " standard-message)))
         (force-mode-line-update)
         (sit-for 1))
       (force-mode-line-update))
      (funky-flash
       (let ((i 10))
         (while (> i 0)
           (yo! msg 'funky)
           (sit-for 0.5)
           (decf i)
           (when (input-pending-p)
             (setq i 0)))))
      (bounce
       (let* ((len-msg (length msg))
              (fr-width (frame-width))
              (m (/ (- len-msg fr-width) 2)) ; magnitude
              (o (abs m))               ; offset
              done)
         (do ((x 0.0 (+ x (* 0.01 pi))))
             (done nil)
           (let* ((ns (truncate (+ o (* m (sin x))))) ; num spaces
                  (mode-line-format
                   (concat
                    (starfield-string (1- ns))
                    " "
                    msg
                    " "
                    (starfield-string (- fr-width len-msg (1+ ns))))))
             (setq done (not (sit-for 0.02)))
             (force-mode-line-update)))))
      ;; Add styles (don't forget doscstring) here.
      (t
       (let ((mode-line-format standard-message))
         (force-mode-line-update)
         (sit-for 1))
       (force-mode-line-update)))))

(provide 'yo)

;;; yo.el ends here
