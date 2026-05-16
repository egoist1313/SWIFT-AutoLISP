(defun c:FlipToPoint ( / ss pt i en edata p10 p11 d1 d2 cnt)
  (princ "\nВыберите отрезки (LINE) для унификации направления: ")
  (setq ss (ssget '((0 . "LINE"))))
  (if (not ss)
    (progn (princ "\nНичего не выбрано.") (exit))
  )

  (initget 1)
  (setq pt (getpoint "\nУкажите ТОЧКУ с той стороны, где должно быть НАЧАЛО отрезков: "))
  ;; Переводим в МСК, но для расстояния будем использовать только X и Y
  (setq pt (trans pt 1 0))

  (setq cnt 0
        i   (sslength ss))
  (princ (strcat "\nОбрабатывается отрезков: " (itoa i)))

  (while (>= (setq i (1- i)) 0)
    (setq en    (ssname ss i)
          edata (entget en)
          p10   (cdr (assoc 10 edata))   ; начало
          p11   (cdr (assoc 11 edata))   ; конец
    )

    ;; Сравниваем ТОЛЬКО горизонтальные расстояния (X, Y)
    (setq d1 (distance (list (car pt) (cadr pt)) (list (car p10) (cadr p10)))
          d2 (distance (list (car pt) (cadr pt)) (list (car p11) (cadr p11))))

    (princ (strcat "\nОтрезок " (itoa (1+ i))
                   ": начало XY=(" (rtos (car p10) 2 2) "," (rtos (cadr p10) 2 2) ")"
                   ", конец XY=(" (rtos (car p11) 2 2) "," (rtos (cadr p11) 2 2) ")"
                   ", d1(гор)=" (rtos d1 2 4) ", d2(гор)=" (rtos d2 2 4)))

    (cond
      ((< d2 d1)   ; конец ближе к указанной точке → перевернуть
       (setq edata (subst (cons 10 p11) (assoc 10 edata) edata))
       (setq edata (subst (cons 11 p10) (assoc 11 edata) edata))
       (if (entmod edata)
         (progn
           (setq cnt (1+ cnt))
           (princ "  --> ПЕРЕВЁРНУТ"))
         (princ "  --> ОШИБКА изменения"))
      )
      ((equal d1 d2 1e-6)
       (princ "  --> расстояния равны, без изменений"))
      (t
       (princ "  --> уже правильное направление"))
    )
  )
  (princ (strcat "\nГотово. Перевёрнуто отрезков: " (itoa cnt)))
  (princ)
)