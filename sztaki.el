;;; sztaki.el --- SZTAKI dictionary lookup for Emacs

;;; Emacs extension to reach the services of the SZTAKI online dictionary.
;;; http://szotar.sztaki.hu/

(require 'w3m)

(defun sztaki-lookup-phrase (phrase)
  "Look up the PHRASE and echo responsed translation if any.

If called interactivly, look up word under the cursor."
  (interactive
   (list (downcase (thing-at-point 'word))))
  (let ((url (concat "http://szotar.sztaki.hu/dict_search.php"
                     "?O=HUN&E=1&L=ENG%3AHUN%3AEngHunDict&in_emacs=1&W="
                     (w3m-url-encode-string phrase)))
        (match (format "phrase '%s' not found" phrase)))
    (with-temp-buffer
      (w3m-process-with-wait-handler
        (w3m-retrieve-and-render url nil nil nil nil handler))
      (when (re-search-forward (concat phrase ":.*") nil t)
        (setq match (match-string-no-properties 0))))
    (message (concat "SZTAKI (ENG->HUN): " match))))

(provide 'sztaki)
