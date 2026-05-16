; SWIFT-START
(vl-load-com)
; v 1.6 - 2024-06-01
; Автор: SWIFT (https://t.me/LispGeo)
; Описание: Набор утилит для работы с ПСК и видами в AutoCAD, а также для интеграции с Excel через Data Links.
; Команды:
; - ПЛАН1 / PLAN1: Центрирует вид на указанной точке с сохранением масштаба. В пространстве листа учитывает поворот ПСК и блокировку видового экрана.
; - ПСК2Д / UCS2D: Устанавливает ПСК по 2 точкам в плоскости XY. Точка 1 = начало ПСК (Origin). Точка 2 = направление оси X. Ось Y = автоматически
(defun c:ПЛАН1 (/ *error* pt vs oldcmd tile cvp ucsang ss vpobj locked)
  (defun *error* (msg)
    (setvar "CMDECHO" oldcmd)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nОшибка ПЛАН1: " msg)))
    (princ))
  (setq oldcmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq tile (getvar "TILEMODE")
        cvp  (getvar "CVPORT"))
  (if (and (= tile 0) (> cvp 1))
    (progn
      (setq locked nil)
      (if (setq ss (ssget "_X" (list '(0 . "VIEWPORT") (cons 69 cvp))))
        (progn
          (setq vpobj (vlax-ename->vla-object (ssname ss 0)))
          (if (= (vla-get-displaylocked vpobj) :vlax-true)
            (setq locked t))))
      (if locked
        (progn
          (alert "Видовой экран заблокирован!\n\nРазблокируйте его:\n1. Выберите рамку видового экрана\n2. Свойства -> Display Locked -> No\n\nЗатем повторите ПЛАН1.")
          (setvar "CMDECHO" oldcmd)
          (princ "\nВидовой экран заблокирован!")
          (princ)
          (exit)))))
  (setq vs (getvar "VIEWSIZE"))
  (princ "\nУкажите новый центр вида: ")
  (if (setq pt (getpoint))
    (progn
      (if (and (= tile 0) (> cvp 1))
        (progn
          (setq ucsang (angle '(0.0 0.0) (getvar "UCSXDIR")))
          (if vpobj
            (vla-put-twistangle vpobj (- ucsang))
            (command "_.DVIEW" "" "_TW" (- (* ucsang (/ 180.0 pi))) ""))
          (command "_.ZOOM" "_C" pt vs))
        (progn
          (command "_.PLAN" "_C")
          (command "_.ZOOM" "_C" pt vs)))
      (princ "\nВид центрирован на указанной точке.")))
  (setvar "CMDECHO" oldcmd)
  (princ))
(defun c:PLAN1 () (c:ПЛАН1))
; ПСК2Д / UCS2D
; Устанавливает ПСК по 2 точкам в плоскости XY.
; Точка 1 = начало ПСК (Origin).
; Точка 2 = направление оси X.
; Ось Y = автоматически 90 градусов влево от X.
; Z не используется - ПСК остаётся горизонтальной.
(defun c:ПСК2Д (/ *error* p1 p2 pt1 pt2 pt3 oldcmd)
  (defun *error* (msg)
    (setvar "CMDECHO" oldcmd)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nОшибка ПСК2Д: " msg)))
    (princ))
  (setq oldcmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (princ "\nНачало ПСК (точка 1): ")
  (if (setq p1 (getpoint))
    (progn
      (princ "\nНаправление оси X (точка 2): ")
      (if (setq p2 (getpoint p1))
        (progn
          (setq pt1 (list (car p1) (cadr p1) 0.0))
          (setq pt2 (list (car p2) (cadr p2) 0.0))
          (setq pt3 (polar pt1 (+ (angle pt1 pt2) (/ pi 2.0)) 1.0))
          (command "_.UCS" "_3" pt1 pt2 pt3)
          (princ "\nПСК установлена по двум точкам.")))))
  (setvar "CMDECHO" oldcmd)
  (princ))
(defun c:UCS2D () (c:ПСК2Д))
(princ)
; SWIFT-END
