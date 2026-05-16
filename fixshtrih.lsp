; SWIFT-START
(defun c:FixHatchGB (/ *error* ss i ent cnt)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nОшибка FixHatchGB: " msg)))
    (princ))

  (setenv "MaxHatch" "10000000")
  (princ "\nMaxHatch увеличен до 10 млн.")

  (if (not (setq ss (ssget '((0 . "HATCH")))))
    (progn (princ "\nШтриховки не выбраны.") (princ) (exit)))

  (setq i 0 cnt 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (command "_.HATCHGENERATEBOUNDARY" ent "")
    (setq i   (1+ i)
          cnt (1+ cnt)))

  (command "_.REGEN")
  (princ (strcat "\nГотово! Создано " (itoa cnt) " контуров."))
  (princ))
; SWIFT-END
