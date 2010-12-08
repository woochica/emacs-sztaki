;;; sztaki.el --- SZTAKI dictionary lookup for Emacs

;;; Commentary:
;; 
;; Emacs extension to reach the services of the SZTAKI online dictionary.
;; http://szotar.sztaki.hu/
;;
;; Install:
;;
;; Put the following into your .emacs:
;;
;;   (add-to-list 'load-path "/path/to/emacs-sztaki")
;;   (require 'sztaki)
;;   (global-set-key (kbd "C-?") 'sztaki-lookup-phrase)
;;   (sztaki-change-dictionary "EN:HU")

;;; History:
;; 

;;; Code:

;(require 'w3m)

(defconst sztaki-service-url-format
  "http://szotar.sztaki.hu/dict_search.php?O=HUN&E=1&L=%s&in_emacs=1&W=%s"
  "URL format for the SZTAKI dictionary web service.")

(defconst sztaki-dictionary-alist
  '(("EN:HU" . "ENG:HUN:EngHunDict")
    ("HU:EN" . "HUN:ENG:EngHunDict")
    ("DE:HU" . "GER:HUN:GerHunDict")
    ("HU:DE" . "HUN:GER:GerHunDict")
    ("FR:HU" . "FRA:HUN:FraHunDict")
    ("HU:FR" . "HUN:FRA:FraHunDict")
    ("IT:HU" . "ITA:HUN:ItaHunDict")
    ("HU:IT" . "HUN:ITA:ItaHunDict")
    ("NL:HU" . "HOL:HUN:HolHunDict")
    ("HU:NL" . "HUN:HOL:HolHunDict")
    ("PL:HU" . "POL:HUN:PolHunDict")
    ("HU:PL" . "HUN:POL:PolHunDict"))
  "List of dictionaries can be used with SZTAKI.")

(defgroup sztaki nil
  "Customizations for webma-instance."
  :group 'applications)

(defcustom sztaki-local-dictionary
  nil
  "Dictionary name to be used for dictionary functions."
  :type 'string
  :group 'sztaki)

(defun sztaki-dictionary-code (dict)
  "Return SZTAKI URL code for dictionary name DICT."
  (cdr (assoc dict sztaki-dictionary-alist)))

(defun sztaki-dictionary-opposite (dict)
  "Return opposite dictionary name for DICT."
  (mapconcat 'identity (nreverse (split-string dict ":")) ":"))

(defun sztaki-local-dictionary ()
  "Return currently used dictionary name."
  (or
   sztaki-local-dictionary
   (caar sztaki-dictionary-alist)))

(defun sztaki-change-dictionary (dict)
  "Change to dictionary name DICT for SZTAKI."
  (interactive
   (list
    (or (completing-read
         "Use new dictionary (RET for current, SPC to complete): "
         (mapcar 'car sztaki-dictionary-alist) nil t)
        (sztaki-local-dictionary))))
  (setq sztaki-local-dictionary dict))

(defun sztaki-lookup-phrase (phrase &optional reverse)
  "Look up the PHRASE and display responsed translation if any.

If called interactively, use region or look up word under the
cursor if region is inactive.

If REVERSE is non-nil, use opposite dictionary.

If there is an exact match for PHARSE, it will be displayed in
the echo area, otherwise a buffer will be pop up that displays
the translations. You can simply hit `q' to exit and return."
  (interactive
   (list (downcase (if (region-active-p)
                       (filter-buffer-substring (region-beginning) (region-end) nil t)
                     (thing-at-point 'word)))
	 current-prefix-arg))
  (let* ((dict (if reverse
		   (sztaki-dictionary-opposite (sztaki-local-dictionary))
		 (sztaki-local-dictionary)))
	 (url (format sztaki-service-url-format
		      (sztaki-dictionary-code dict)
                      (w3m-url-encode-string phrase nil t))))
    (with-current-buffer (get-buffer-create "*sztaki*")
      (if (w3m-process-with-wait-handler
        (w3m-retrieve-and-render url nil nil nil nil handler))
          (cond
           ((re-search-forward (concat "^" phrase ":.*") nil t)
            (message "SZTAKI (%s): %s" dict (match-string-no-properties 0)))
           ((progn
              (goto-char (point-min))
              (re-search-forward "kifejezést találtam:" nil t))
            (beginning-of-line)
            (delete-region (point-min) (point))
            (when (re-search-forward "^--" nil t)
              (delete-region (match-beginning 0) (point-max)))
            (goto-char (point-min))
            (view-buffer "*sztaki*"))
           (t
            (message "phrase '%s' not found" phrase)))
        (message "Failed to fetch page.")))))

(provide 'sztaki)

;;; sztaki.el ends here
