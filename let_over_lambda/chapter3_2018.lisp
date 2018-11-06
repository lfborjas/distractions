;; Re-reading Let Over Lambda starting at chapter 3, this time with a proper Emacs + SLIME config going on to interact with the code faster.

(defmacro unit-of-time (value unit)
  `(* ,value
    ,(case unit
       ((s) 1)
       ((m) 60)
       ((h) 3600)
       ((d) 86400)
       ((ms) 1/1000)
       ((us) 1/1000000))))

;; one interesting note: in the book they typeset the backquote to look like a smart single quote, but it's just the backtick!
