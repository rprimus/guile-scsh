(define-module (scsh ascii))
(export ascii->char char->ascii)

;; used in char-set, network, rx/parse, rx/posixstr, rx/rx-lib, rx/spencer,
;; lib/ccp.
(define ascii->char integer->char)
(define char->ascii char->integer)   ;; also in glob.
