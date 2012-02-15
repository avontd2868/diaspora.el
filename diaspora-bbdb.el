(defvar  diaspora-webfinger-list nil
  "")

(defvar  diaspora-resource-descriptor-webfinger-string nil
  "")
;; webfinger
;; see: http://devblog.joindiaspora.com/2012/01/22/how-diaspora-connects-users/
;; Probably this is not the simplest way to go...

(defun diaspora-resource-descriptor-webfinger (pod)
  "Get host resource descriptor webfinger."
  (url-retrieve (concat "https://" pod "/.well-known/host-meta") 
		(lambda (arg)
		  (save-excursion
		    (goto-char (point-min))
		    (search-forward-regexp diaspora-regexp-webfinger-query))
		  (setq diaspora-resource-descriptor-webfinger-string (match-string-no-properties 1))))
  diaspora-resource-descriptor-webfinger-string)

(defun diaspora-webfinger (pod user)
  "Returns a list with webfinger with the form PROFILE-PAGE GUID HCARD ATOM D*PUBLICKEY"
  (diaspora-resource-descriptor-webfinger pod)
  (url-retrieve (concat diaspora-resource-descriptor-webfinger-string user "@" pod)
		(lambda (arg) 
		  (setq diaspora-webfinger-list 
			(mapcar (lambda (x)
				  (save-excursion
				    (goto-char (point-min))
				    (search-forward-regexp x)
				    (match-string-no-properties 1)))
				diaspora-regexp-webfinger-all))))
		    diaspora-webfinger-list)


(defcustom diaspora-regexp-webfinger-query
  "<Link rel=\'lrdd\'\n[\s-]*template=\'\\(.*\\)\{uri\}\'>"
  "Regular expression for resource-descriptor-webfinger."
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-webfinger-hcard
  "<Link rel=\"http://microformats.org/profile/hcard\" type=\"text/html\" href=\"\\(.*\\)\"/>"
  "regex-webfinger-hcard"
  :type 'regexp
  :group 'diaspora)


(defcustom diaspora-regexp-webfinger-guid
"<Link rel=\"http://joindiaspora.com/guid\" type = \'text/html\' href=\"\\(.*\\)\"/>"
  "regexp-webfinger-guid"
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-webfinger-profile-page
"<Link rel=\'http://webfinger.net/rel/profile-page\' type=\'text/html\' href=\"\\(.*\\)\"/>"
  ""
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-webfinger-atom
    "<Link rel=\"http://schemas.google.com/g/2010#updates-from\" type=\"application/atom\\+xml\" href=\"\\(.*\\)\"/>" 
    "regex-webfinger-atom"
  :type 'regexp
  :group 'diaspora)

(defcustom diaspora-regexp-webfinger-publickey
  "<Link rel=\"diaspora-public-key\" type = \'RSA\' href=\"\\(.*\\)\"/>"
  "webfinger-publickey"
  :type 'regexp
  :group 'diaspora)

(defvar diaspora-regexp-webfinger-all
  (list diaspora-regexp-webfinger-profile-page
	diaspora-regexp-webfinger-guid
	diaspora-regexp-webfinger-hcard
	diaspora-regexp-webfinger-atom
	diaspora-regexp-webfinger-publickey)
  "List of all the regexp used to webfinger.")


(defcustom diaspora-regexp-pod-user
  "Regular expression for user pod and user name: 1-> user, 2-> pod."
  :type 'regexp
  :group 'diaspora)

(defun diaspora-get-all-regexp (regexp &optional opt)
  (save-excursion
    (cond ((search-forward-regexp regexp (point-max) t)
	   (cons (match-string-no-properties 
		  (if (not opt) 2 opt))
		 (diaspora-get-all-regexp regexp (if (not opt) 2 opt))))
	  (t nil))))

(defun diaspora-pod-user (str)
  (list
   (cdr (cons (string-match "\\([.a-zA-Z0-9 \\-_]+\\)" str)
	      (match-string-no-properties 0 str)))
   (cdr  (cons (string-match "@\\([.a-zA-Z0-9 \\-_-]+\\)" str)
	       (match-string-no-properties 1 str)))))

(defun diaspora-webfinger-user-at-pod (str)
  (diaspora-webfinger (cadr (diaspora-pod-user str))
		      (car (diaspora-pod-user str))))

(diaspora-webfinger-user-at-pod "tca@joindiaspora.com")
;(mapcar 'diaspora-webfinger-user-at-pod
;	(diaspora-get-all-regexp diaspora-regexp-pod-user 1))
