;;;============================================================
;;; NEZ.lsp - Coordinate Label Tool
;;; Command: NEZ
;;; 
;;; Features:
;;; - Pick point and show N, E, EL with arrow
;;; - Change font/text style
;;; - Change text size
;;; - Change line weight
;;; - Change color
;;;============================================================

;;; Global Settings
(setq *NEZ-TEXTHEIGHT* 2.5)    ; Text height
(setq *NEZ-COLOR* 7)           ; Color (7=white)
(setq *NEZ-STYLE* "Standard")  ; Text style
(setq *NEZ-DECIMAL* 3)         ; Decimal places
(setq *NEZ-LANDING* 8)         ; Landing line length

;;;============================================================
;;; Helper: Get color name from number
;;;============================================================
(defun nez-color-name (colnum)
  (cond
    ((= colnum 1) "Red")
    ((= colnum 2) "Yellow")
    ((= colnum 3) "Green")
    ((= colnum 4) "Cyan")
    ((= colnum 5) "Blue")
    ((= colnum 6) "Magenta")
    ((= colnum 7) "White")
    ((= colnum 8) "Gray")
    ((= colnum 9) "Light Gray")
    (T "Custom")
  )
)

;;;============================================================
;;; Main Command - NEZ
;;;============================================================
(defun c:NEZ (/ opt colorname)
  ;; Get color name
  (setq colorname (nez-color-name *NEZ-COLOR*))
  
  (princ "\n")
  (princ "\n+==========================================+")
  (princ "\n|      NEZ - Coordinate Label Tool        |")
  (princ "\n+==========================================+")
  (princ (strcat "\n|  Height : " (rtos *NEZ-TEXTHEIGHT* 2 2)))
  (princ (strcat "\n|  Color  : " (itoa *NEZ-COLOR*) " (" colorname ")"))
  (princ (strcat "\n|  Style  : " *NEZ-STYLE*))
  (princ (strcat "\n|  Decimal: " (itoa *NEZ-DECIMAL*)))
  (princ "\n+==========================================+")
  
  (initget "Pick Height Color Style Decimal")
  (setq opt (getkword "\n[Pick/Height/Color/Style/Decimal] <Pick>: "))
  (if (null opt) (setq opt "Pick"))
  
  (cond
    ((= opt "Pick") (nez-pick))
    ((= opt "Height") (nez-set-height))
    ((= opt "Color") (nez-set-color))
    ((= opt "Style") (nez-set-style))
    ((= opt "Decimal") (nez-set-decimal))
  )
  
  (princ)
)

;;;============================================================
;;; Pick Point and Create Label
;;;============================================================
(defun nez-pick (/ pt pt2 x y z txtstr leaderinfo landingpt landingdir textpt)
  (princ "\nPick point to label (ESC to exit)...")
  
  (while (setq pt (getpoint "\nPick point: "))
    (setq x (car pt))
    (setq y (cadr pt))
    (setq z (if (caddr pt) (caddr pt) 0.0))
    
    (setq pt2 (getpoint pt "\nPick label position: "))
    
    (if pt2
      (progn
        ;; Draw leader and get landing point
        (setq leaderinfo (nez-draw-leader pt pt2))
        (setq landingpt (car leaderinfo))
        (setq landingdir (cadr leaderinfo))
        ;; Format coordinate strings as single MTEXT
        (setq txtstr (strcat 
          "N: " (rtos y 2 *NEZ-DECIMAL*) "\\P"
          "E: " (rtos x 2 *NEZ-DECIMAL*) "\\P"
          "EL: " (rtos z 2 *NEZ-DECIMAL*)
        ))
        
        ;; Position text with larger gap from landing line
        (setq textpt (polar landingpt landingdir (* *NEZ-TEXTHEIGHT* 1.0)))
        
        ;; Create single MTEXT block
        (nez-create-mtext textpt txtstr landingdir)
        
        (princ (strcat "\nN=" (rtos y 2 *NEZ-DECIMAL*) 
                      " E=" (rtos x 2 *NEZ-DECIMAL*) 
                      " EL=" (rtos z 2 *NEZ-DECIMAL*)))
      )
      (princ "\nCancelled")
    )
  )
  (princ "\nDone.")
)

;;;============================================================
;;; Settings Functions
;;;============================================================
(defun nez-set-height (/)
  (setq newht (getdist (strcat "\nNew text height <" (rtos *NEZ-TEXTHEIGHT* 2 2) ">: ")))
  (if newht 
    (progn
      (setq *NEZ-TEXTHEIGHT* newht)
      (setq *NEZ-LANDING* (* newht 3))
    )
  )
  (princ (strcat "\nHeight = " (rtos *NEZ-TEXTHEIGHT* 2 2)))
  (c:NEZ)
)

(defun nez-set-color (/ colopt)
  (princ "\n")
  (princ "\n+---------------------------+")
  (princ "\n|      SELECT COLOR         |")
  (princ "\n+---------------------------+")
  (princ "\n|  1 = Red                  |")
  (princ "\n|  2 = Yellow               |")
  (princ "\n|  3 = Green                |")
  (princ "\n|  4 = Cyan                 |")
  (princ "\n|  5 = Blue                 |")
  (princ "\n|  6 = Magenta              |")
  (princ "\n|  7 = White                |")
  (princ "\n|  8 = Gray                 |")
  (princ "\n|  0 = Other (custom)       |")
  (princ "\n+---------------------------+")
  
  (initget "1 2 3 4 5 6 7 8 0")
  (setq colopt (getkword (strcat "\nSelect color [1-8/0] <" (itoa *NEZ-COLOR*) ">: ")))
  
  (cond
    ((= colopt "1") (setq *NEZ-COLOR* 1))
    ((= colopt "2") (setq *NEZ-COLOR* 2))
    ((= colopt "3") (setq *NEZ-COLOR* 3))
    ((= colopt "4") (setq *NEZ-COLOR* 4))
    ((= colopt "5") (setq *NEZ-COLOR* 5))
    ((= colopt "6") (setq *NEZ-COLOR* 6))
    ((= colopt "7") (setq *NEZ-COLOR* 7))
    ((= colopt "8") (setq *NEZ-COLOR* 8))
    ((= colopt "0")
      (setq newcol (getint "\nEnter color number (1-255): "))
      (if newcol (setq *NEZ-COLOR* newcol))
    )
  )
  
  (princ (strcat "\nColor = " (itoa *NEZ-COLOR*)))
  (c:NEZ)
)

(defun nez-set-style (/ styleopt)
  (princ "\n")
  (princ "\n+---------------------------+")
  (princ "\n|    SELECT TEXT STYLE      |")
  (princ "\n+---------------------------+")
  (princ "\n|  1 = Standard             |")
  (princ "\n|  2 = Arial                |")
  (princ "\n|  3 = Romans               |")
  (princ "\n|  4 = Romantic             |")
  (princ "\n|  5 = Simplex              |")
  (princ "\n|  6 = Txt                  |")
  (princ "\n|  0 = Other (custom)       |")
  (princ "\n+---------------------------+")
  
  (initget "1 2 3 4 5 6 0")
  (setq styleopt (getkword "\nSelect style [1-6/0] <1>: "))
  
  (cond
    ((= styleopt "1") (setq *NEZ-STYLE* "Standard"))
    ((= styleopt "2") (setq *NEZ-STYLE* "Arial"))
    ((= styleopt "3") (setq *NEZ-STYLE* "Romans"))
    ((= styleopt "4") (setq *NEZ-STYLE* "Romantic"))
    ((= styleopt "5") (setq *NEZ-STYLE* "Simplex"))
    ((= styleopt "6") (setq *NEZ-STYLE* "Txt"))
    ((= styleopt "0")
      (setq newstyle (getstring T "\nEnter text style name: "))
      (if (and newstyle (/= newstyle ""))
        (setq *NEZ-STYLE* newstyle)
      )
    )
  )
  
  (princ (strcat "\nStyle = " *NEZ-STYLE*))
  (c:NEZ)
)

(defun nez-set-decimal (/)
  (setq newdec (getint (strcat "\nDecimal places <" (itoa *NEZ-DECIMAL*) ">: ")))
  (if newdec (setq *NEZ-DECIMAL* newdec))
  (princ (strcat "\nDecimal = " (itoa *NEZ-DECIMAL*)))
  (c:NEZ)
)


;;;============================================================
;;; Draw Leader with Arrow and Landing Line (Grouped)
;;;============================================================
(defun nez-draw-leader (pt1 pt2 / arrowsize arrowang landingdir landingpt p2 p3 ss ent1 ent2 ent3 ent4)
  (setq arrowsize (* *NEZ-TEXTHEIGHT* 0.8))
  (setq arrowang (angle pt2 pt1))
  
  ;; Determine landing direction
  (if (> (car pt2) (car pt1))
    (setq landingdir 0)     ; Right
    (setq landingdir pi)    ; Left
  )
  
  ;; Calculate landing point
  (setq landingpt (polar pt2 landingdir *NEZ-LANDING*))
  
  ;; Create selection set for grouping
  (setq ss (ssadd))
  
  ;; Draw main line
  (entmake
    (list
      '(0 . "LINE")
      (cons 10 pt1)
      (cons 11 pt2)
      (cons 62 *NEZ-COLOR*)
    )
  )
  (setq ent1 (entlast))
  (ssadd ent1 ss)
  
  ;; Draw landing line
  (entmake
    (list
      '(0 . "LINE")
      (cons 10 pt2)
      (cons 11 landingpt)
      (cons 62 *NEZ-COLOR*)
    )
  )
  (setq ent2 (entlast))
  (ssadd ent2 ss)
  
  ;; Draw arrowhead
  (setq p2 (polar pt1 (+ arrowang 2.8) arrowsize))
  (setq p3 (polar pt1 (- arrowang 2.8) arrowsize))
  
  (entmake
    (list
      '(0 . "SOLID")
      (cons 10 pt1)
      (cons 11 p2)
      (cons 12 p3)
      (cons 13 pt1)
      (cons 62 *NEZ-COLOR*)
    )
  )
  (setq ent3 (entlast))
  (ssadd ent3 ss)
  
  ;; Draw circle at point
  (entmake
    (list
      '(0 . "CIRCLE")
      (cons 10 pt1)
      (cons 40 (* *NEZ-TEXTHEIGHT* 0.25))
      (cons 62 *NEZ-COLOR*)
    )
  )
  (setq ent4 (entlast))
  (ssadd ent4 ss)
  
  ;; Store selection set globally for grouping later if needed
  (setq *NEZ-LAST-SS* ss)
  
  ;; Return landing point and direction
  (list landingpt landingdir)
)

;;;============================================================
;;; Create MTEXT as single block
;;;============================================================
(defun nez-create-mtext (insertpt txtstr landingdir / justify)
  ;; Determine justification based on direction
  ;; landingdir = 0 means right (use TL), landingdir = pi means left (use TR)
  (if (> landingdir 1.5)
    (setq justify "TR")  ; Top-right for left direction
    (setq justify "TL")  ; Top-left for right direction
  )
  
  ;; Use command to create MTEXT with correct justification
  (command "_.MTEXT" insertpt "_H" *NEZ-TEXTHEIGHT* "_J" justify "_W" "0" txtstr "")
  
  ;; Change color
  (command "_.CHPROP" (entlast) "" "_C" *NEZ-COLOR* "")
  
  ;; Add to selection set
  (if (and (boundp '*NEZ-LAST-SS*) *NEZ-LAST-SS*)
    (ssadd (entlast) *NEZ-LAST-SS*)
  )
)

;;;============================================================
;;; Load Message
;;;============================================================
(princ "\n")
(princ "\n+==========================================+")
(princ "\n|   NEZ - Coordinate Label Tool Loaded!   |")
(princ "\n+==========================================+")
(princ "\n|   Type NEZ to start                     |")
(princ "\n|                                         |")
(princ "\n|   Options:                              |")
(princ "\n|   P = Pick point                        |")
(princ "\n|   H = Change height                     |")
(princ "\n|   C = Change color                      |")
(princ "\n|   S = Change text style                 |")
(princ "\n|   D = Change decimal places             |")
(princ "\n+==========================================+")
(princ "\n")
(princ)
