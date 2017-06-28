;;; winner-mode-enable.el --- enable winner-mode if it is available

;; Copyright (C) 2014  Jason Lewis

;; Author: Jason Lewis <jason@dickson.st>
;; Maintainer: Jason Lewis <jason@dickson.st>
;; URL: https://github.com/jasonblewis/winner-mode-enable
;; Created: 23rd January 2015
;; Keywords: winner-mode
;; Version: 0.0.5

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

;; Winner mode has been distributed with emacs since version 20
;; however it is not enabled by default. This package simply enables
;; it as per the winner mode instructions:
;; http://www.emacswiki.org/emacs/WinnerMode

;; This is the first package I have created that implements an idea I
;; have had for some time, that is to make packages that provide micro
;; features or turn features on and off in emacs.
;;
;; 
;;; Code:
(when (fboundp 'winner-mode)
      (winner-mode 1))

;;;###autoload

(provide 'winner-mode-enable)

;;; winner-mode-enable.el ends here
