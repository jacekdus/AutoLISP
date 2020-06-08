;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get Attribute Value  -  Lee Mac
;; Returns the value held by the specified tag within the supplied block, if present.
;; blk - [ent] Block (Insert) Entity Name
;; tag - [str] Attribute TagString
;; Returns: [str] Attribute value, else nil if tag is not found.

(defun LM:getattributevalue ( blk tag / val enx )
    (while
        (and
            (null val)
            (setq blk (entnext blk))
            (= "ATTRIB" (cdr (assoc 0 (setq enx (entget blk)))))
        )
        (if (= (strcase tag) (strcase (cdr (assoc 2 enx))))
            (setq val (cdr (assoc 1 (reverse enx))))
        )
    )
)

;; Set Attribute Value  -  Lee Mac
;; Sets the value of the first attribute with the given tag found within the block, if present.
;; blk - [ent] Block (Insert) Entity Name
;; tag - [str] Attribute TagString
;; val - [str] Attribute Value
;; Returns: [str] Attribute value if successful, else nil.

(defun LM:setattributevalue ( blk tag val / end enx )
    (while
        (and
            (null end)
            (setq blk (entnext blk))
            (= "ATTRIB" (cdr (assoc 0 (setq enx (entget blk)))))
        )
        (if (= (strcase tag) (strcase (cdr (assoc 2 enx))))
            (if (entmod (subst (cons 1 val) (assoc 1 (reverse enx)) enx))
                (progn
                    (entupd blk)
                    (setq end val)
                )
            )
        )
    )
)

;; Read CSV  -  Lee Mac
;; Parses a CSV file into a matrix list of cell values.
;; csv - [str] filename of CSV file to read
 
(defun LM:readcsv ( csv / des lst sep str )
    (if (setq des (open csv "r"))
        (progn
            (setq sep (cond ((vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International" "sList")) (",")))
            (while (setq str (read-line des))
                (setq lst (cons (LM:csv->lst str sep 0) lst))
            )
            (close des)
        )
    )
    (reverse lst)
)
 
;; CSV -> List  -  Lee Mac
;; Parses a line from a CSV file into a list of cell values.
;; str - [str] string read from CSV file
;; sep - [str] CSV separator token
;; pos - [int] initial position index (always zero)
 
(defun LM:csv->lst ( str sep pos / s )
    (cond
        (   (not (setq pos (vl-string-search sep str pos)))
            (if (wcmatch str "\"*\"")
                (list (LM:csv-replacequotes (substr str 2 (- (strlen str) 2))))
                (list str)
            )
        )
        (   (or (wcmatch (setq s (substr str 1 pos)) "\"*[~\"]")
                (and (wcmatch s "~*[~\"]*") (= 1 (logand 1 pos)))
            )
            (LM:csv->lst str sep (+ pos 2))
        )
        (   (wcmatch s "\"*\"")
            (cons
                (LM:csv-replacequotes (substr str 2 (- pos 2)))
                (LM:csv->lst (substr str (+ pos 2)) sep 0)
            )
        )
        (   (cons s (LM:csv->lst (substr str (+ pos 2)) sep 0)))
    )
)
 
(defun LM:csv-replacequotes ( str / pos )
    (setq pos 0)
    (while (setq pos (vl-string-search  "\"\"" str pos))
        (setq str (vl-string-subst "\"" "\"\"" str pos)
              pos (1+ pos)
        )
    )
    str
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; db.csv - example
;  
;  typeDB      PODWALINA        LP1XLP2
;  A1          4.00x0.50/0.60   0.40x0.40
;  A2          3.00x0.50/0.60   0.40x0.40
;

(defun getTypeDB ( file typeDWG / typeDB)
    (setq data (LM:readcsv file))
    (foreach line data
        (if (and (= (cadr line) (car typeDWG)) (= (caddr line) (cadr typeDWG)) )
            (setq typeDB (car line))
        )
    )
    (princ typeDB)
)

(defun getTypeDWG ( ename / lp1xlp2 podwalina)
    (setq podwalina (LM:getattributevalue ename "PODWALINA"))
    (setq lp1xlp2 (LM:getattributevalue ename "LP1XLP2"))
    (list podwalina lp1xlp2)
)

(defun setType ( ename attrName typeDB / )
    (if typeDB
        (LM:setattributevalue ename attrName typeDB)
    )
)

(defun c:setTypes ( / file typeDWG typeDB ename i n s x )
    (princ "setTypes initialized.")

    ;; CONNECT CSV DATABASE FILE
    ;(setq file (getfiled "Select CSV File" "" "csv" 16)) ;
    (setq file "C:\\Users\\user\\Desktop\\db.csv")
    
    ;; MAIN LOOP
    (if (setq s (ssget))
        (progn
            (setq i 0
                  n (sslength s)
            )
            (while (< i n)
                (setq ename (ssname s i)
                      x (cdr (assoc 0 (entget ename)))
                      i (1+ i)
                )

                ;; PUT HERE THE MAGIC ;(print ename)

                ;; 1. Get data from object
                (setq typeDWG (getTypeDWG ename)) ;(princ typeDWG)
                
                ;; 2. Get corresponding type from db
                (setq typeDB (getTypeDB file typeDWG)) ;
                (princ typeDB)

                ;; 3. Set attribute in a block with value acquired from db
                (setType ename "TYP_POD" typeDB)
                
                ;; END MAGIC
            )
        )
    ) ;; END MAIN LOOP
    (princ)
)

(defun c:clearTypes ( / )
    (if (setq s (ssget))
        (progn
            (setq i 0
                  n (sslength s)
            )
            (while (< i n)
                (setq ename (ssname s i)
                      x (cdr (assoc 0 (entget ename)))
                      i (1+ i)
                )

                ;; PUT HERE THE MAGIC ;(print ename)

                (setType ename "TYP_POD" "")
                
                ;; END MAGIC
            )
        )
    ) ;; END MAIN LOOP
)
