;;; sztaki.el --- SZTAKI dictionary lookup for Emacs

;;; Emacs extension to reach the services of the SZTAKI online dictionary.
;;; http://szotar.sztaki.hu/

(require 'w3m)

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

(defcustom sztaki-local-dictionary
  nil
  "Dictionary name to be used for dictionary functions."
  :type 'string)

(defun sztaki-dictionary-code (dict)
  "Return SZTAKI URL code for dictionary name DICT."
  (cdr (assoc dict sztaki-dictionary-alist)))

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

(defun sztaki-lookup-phrase (phrase)
  "Look up the PHRASE and echo responsed translation if any.

If called interactivly, look up word under the cursor."
  (interactive
   (list (downcase (thing-at-point 'word))))
  (let ((url (format sztaki-service-url-format
                     (sztaki-dictionary-code (sztaki-local-dictionary))
                     (w3m-url-encode-string phrase)))
        (match (format "phrase '%s' not found" phrase)))
    (with-temp-buffer
      (w3m-process-with-wait-handler
        (w3m-retrieve-and-render url nil nil nil nil handler))
      (when (re-search-forward (concat phrase ":.*") nil t)
        (setq match (match-string-no-properties 0))))
    (message "SZTAKI (%s): %s" (sztaki-local-dictionary) match)))

(provide 'sztaki)
