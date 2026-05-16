; SWIFT-START
; v 1.0
;;; ============================================================
;;; MLXY — Мультивыноски с координатами X/Y/Z
;;; Команда: MLXY
;;; Переключатели прямо во время ввода точки:
;;;   Z — вкл/выкл подпись Z
;;;   P — вкл/выкл ПСК (по умолчанию МСК)
;;;   Enter без точки — выход
;;; Состояние сохраняется между вызовами команды
;;; ============================================================

(if (null *mlxy_show_z*)  (setq *mlxy_show_z*  nil))
(if (null *mlxy_use_psk*) (setq *mlxy_use_psk* nil))

(defun mlxy:prompt_str ()
  (strcat "\nMLXY [Z=" (if *mlxy_show_z* "ВКЛ" "выкл")
          " | ПСК=" (if *mlxy_use_psk* "ВКЛ" "выкл")
          "] [Z/P] Точка стрелки <Выход>: "))

(defun mlxy:get_xyz (pt)
  (if *mlxy_use_psk* pt (trans pt 1 0)))

(defun mlxy:text (pt / xyz)
  (setq xyz (mlxy:get_xyz pt))
  (strcat "X=" (rtos (cadr xyz) 2 3)
          "\nY=" (rtos (car  xyz) 2 3)
          (if *mlxy_show_z*
            (strcat "\nZ=" (rtos (caddr xyz) 2 3))
            "")))

(defun c:MLXY (/ old_echo arrow_pt text_pt)

  (defun *error* (msg)
    (setvar "cmdecho" old_echo)
    (princ))

  (setq old_echo (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (while
    (progn
      (initget "Z P")
      (setq arrow_pt (getpoint (mlxy:prompt_str)))
      (cond
        ((= arrow_pt "Z")
         (setq *mlxy_show_z* (not *mlxy_show_z*))
         T)
        ((= arrow_pt "P")
         (setq *mlxy_use_psk* (not *mlxy_use_psk*))
         T)
        ((null arrow_pt)
         nil)
        (T
         (initget 1)
         (setq text_pt (getpoint arrow_pt "\n  Точка полки: "))
         (if text_pt
           (VL-cmdf "_mleader" arrow_pt text_pt (mlxy:text arrow_pt) ""))
         T)
      )
    )
  )

  (setvar "cmdecho" old_echo)
  (princ))

(princ "\nMLXY загружен. Команда: MLXY  |  Z — вкл/выкл Z  |  P — ПСК/МСК")
(princ)
; SWIFT-END
