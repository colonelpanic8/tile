;;; tile.el --- Tile windows with layouts -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: tile tiling window manager dynamic
;; URL: https://github.com/IvanMalison/tile
;; Package-Requires: ((emacs "25.1") (s "1.9.0") (dash "2.12.0") (stream "2.2.3"))
;; Version: 0.1.4

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; tile provides a way to interactively cycle through pre-defined custom layout
;; definitions à la XMonad.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'eieio)
(require 's)
(require 'stream)


;; General functions

(defun tile-buffer-filter (buffer)
  (not (s-matches? (buffer-name buffer) "\*Minibuf-?[0-9]*\*")))

(defvar tile-buffer-filter 'tile-buffer-filter)

(cl-defun tile-get-buffers
    (target-num-buffers &optional (buffer-filter tile-buffer-filter))
  (setq target-num-buffers (or target-num-buffers 1))
  (let* ((visible-buffers (mapcar 'window-buffer (window-list nil -1 nil)))
         (the-stream (stream-append
                      (stream visible-buffers)
                      (seq-filter (lambda (x)
                                    (not (memq x visible-buffers)))
                                  (stream (buffer-list)))
                      (stream-iterate-function 'identity (current-buffer)))))
    (seq-into-sequence
     (seq-take (seq-filter buffer-filter the-stream) target-num-buffers))))

(defun tile-split-evenly (split-function buffers)
  (when buffers
    (set-window-buffer nil (car buffers))
    (cl-loop for buffer in (cdr buffers)
             do
             (progn
               (funcall split-function)
               (other-window 1)
               (set-window-buffer nil buffer)))
    (balance-windows)
    (other-window 1)))


;; Buffer fetchers

(defclass tile-buffer-fetcher nil
  ((layout :initarg :layout)))

(cl-defmethod tile-execute ((strategy tile-buffer-fetcher) target-num-buffers)
  (let ((layout (oref strategy layout))
        (buffers (tile-strategy-get-buffers strategy target-num-buffers)))
    ;; This isn't the greatest place for this... but it needs to happen after
    ;; the buffers are obtained.
    (delete-other-windows)
    (if (functionp layout)
        (funcall layout buffers)
      (tile-do-layout layout buffers))))

(defclass tile-argument-buffer-fetcher (tile-buffer-fetcher) nil)

(cl-defmethod tile-strategy-get-buffers
  ((_strategy tile-argument-buffer-fetcher) target-num-buffers)
  (tile-get-buffers target-num-buffers))

(defclass tile-n-buffer-fetcher (tile-buffer-fetcher)
  ((n :initarg :n)))

(cl-defmethod tile-strategy-get-buffers ((strategy tile-n-buffer-fetcher) _)
  (tile-get-buffers (oref strategy n)))


;; Layout classes

(defalias 'tile-wide (-partial 'tile-split-evenly 'split-window-vertically))
(defalias 'tile-tall (-partial 'tile-split-evenly 'split-window-horizontally))

(defclass tile-master-layout nil
  ((master-fn :initarg :master-fn)
   (other-fn :initarg :other-fn)))

(cl-defmethod tile-do-layout ((strategy tile-master-layout) buffers)
  (set-window-buffer nil (car buffers))
  (with-slots (master-fn other-fn) strategy
    ;; NOTE: Master function should execute a split, and put focus in the window
    ;; that should be used for NON-MASTER tiling.
    (funcall master-fn)
    (tile-split-evenly other-fn (cdr buffers))))


;; Default instances and convenience functions

(defvar tile-master-left
  (make-instance 'tile-master-layout
                 :master-fn (lambda () (split-window-horizontally) (other-window 1))
                 :other-fn 'split-window-vertically))

(defvar tile-master-right
  (make-instance 'tile-master-layout
                 :master-fn 'split-window-horizontally
                 :other-fn 'split-window-vertically))

(defvar tile-master-top
  (make-instance 'tile-master-layout
                 :master-fn (lambda () (split-window-vertically) (other-window 1))
                 :other-fn 'split-window-horizontally))

(defvar tile-master-bottom
  (make-instance 'tile-master-layout
                 :master-fn 'split-window-vertically
                 :other-fn 'split-window-horizontally))

(defvar tile-wide
  (make-instance 'tile-argument-buffer-fetcher :layout 'tile-wide))

(defvar tile-tall
  (make-instance 'tile-argument-buffer-fetcher :layout 'tile-tall))

(defvar tile-master-default
  (make-instance 'tile-n-buffer-fetcher :n 4 :layout tile-master-left))

(defvar tile-one (make-instance 'tile-argument-buffer-fetcher :layout 'identity))

(defun tile-split-n-tall (n)
  (make-instance 'tile-n-buffer-fetcher :n n :layout 'tile-tall))

(defun tile-split-n-wide (n)
  (make-instance 'tile-n-buffer-fetcher :n n :layout 'tile-wide))


;; Global variables and interactive functions

(defvar tile-current-strategy nil)
(defvar tile-strategies (list tile-master-default (tile-split-n-tall 3) tile-wide tile-one))

(cl-defun tile-get-next-strategy
    (&optional (current-strategy (or tile-current-strategy (car (last tile-strategies)))))
  (let ((current-index (--find-index (equal current-strategy it) tile-strategies)))
    (if current-index
        (nth (mod (1+ current-index) (length tile-strategies)) tile-strategies)
      (car tile-strategies))))

(cl-defun tile (&key (window-count (length (window-list nil -1 nil)))
                     (strategy (tile-get-next-strategy)))
  "Tile WINDOW-COUNT windows using STRATEGY.

STRATEGY defaults to the return value
of `(tile-get-next-strategy)' and WINDOW-COUNT defaults to the
current window count."
  (interactive)
  (tile-execute strategy window-count)
  (setq tile-current-strategy strategy))

(provide 'tile)
;;; tile.el ends here
