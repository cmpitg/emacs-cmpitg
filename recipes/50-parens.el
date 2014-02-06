;;
;; Copyright (C) 2014 Duong Nguyen ([@cmpitg](https://github.com/cmpitg/))
;;
;; This project is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This project is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;
;; Auto pairing brackets
;;

(use-package smartparens
  :init (progn
          (smartparens-global-mode)))

(use-package paredit
  :init (progn
          (defadvice paredit-mode (around disable-otherparenslib-around (arg))
            "Disable autopairs mode if paredit-mode is turned on."
            ad-do-it
            (cond ((null ad-return-value)
                   (smartparens-mode 1))
                  (t
                   (smartparens-mode 0))))

          (ad-activate 'paredit-mode)

          ;;; Use with SLIME REPL
          (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

          ;; Stop SLIME's REPL from grabbing DEL,
          ;; which is annoying when backspacing over a '('
          (defun override-slime-repl-bindings-with-paredit ()
            (define-key slime-repl-mode-map
              (read-kbd-macro paredit-backward-delete-key) nil))

          (add-hook 'slime-repl-mode-hook
                    'override-slime-repl-bindings-with-paredit)

          (add-hook 'emacs-lisp-mode-hook       '$load-paredit-mode)
          (add-hook 'lisp-mode-hook             '$load-paredit-mode)
          (add-hook 'lisp-interaction-mode-hook '$load-paredit-mode)
          (add-hook 'scheme-mode-hook           '$load-paredit-mode)))

