; SWIFT-START
(vl-load-com)
;;====================================================================
;;  ОБЩАЯ ФУНКЦИЯ (вызывается из двух команд)
;;====================================================================
(defun Pipes-To-3DPoly (with-offset / selSS obj p0 p1 i ms diameter_var diameter
                        z_start z_end z_floor_start z_floor_end
                        pt_start pt_end old_osmode old_elevation
                        segs endpoints tol d j
                        group entries zs xy min_z endpoint_z_map
                        processed_segs final_plines coords pline
                        pipe_name new_coords short_start short_end
                        xy_start xy_end z_start_col z_end_col
                        pt_start_orig pt_end_orig
                        need_short_start need_short_end
                        changed_start changed_end)
  (setq old_osmode (getvar "OSMODE"))
  (setq old_elevation (getvar "ELEVATION"))
  (setvar "OSMODE" 0)
  (setvar "ELEVATION" 0)
  (setq tol 1e-6)
  (setq d 0.001) ; 1 мм в метрах
  (princ "\nВыберите трубы (нажмите Enter для завершения): ")
  (setq ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (if (setq selSS (ssget '((0 . "AECC_PIPE"))))
    (progn
      (setq i 0 segs nil)
      (princ (strcat "\nНайдено: " (itoa (sslength selSS)) " труб(ы). Обработка..."))
      ;;====================================================================
      ;; 1. СБОР ДАННЫХ О ТРУБАХ
      ;;====================================================================
      (while (< i (sslength selSS))
        (setq obj (vlax-ename->vla-object (ssname selSS i)))
        (if (= (vla-get-objectname obj) "AeccDbPipe")
          (progn
            (setq pipe_name (vlax-get obj 'Name))
            (if (null pipe_name) (setq pipe_name "Без имени"))
            (setq p0 (vlax-safearray->list (vlax-variant-value (vlax-get-property obj 'PointAtParam 0))))
            (setq p1 (vlax-safearray->list (vlax-variant-value (vlax-get-property obj 'PointAtParam 1))))
            (setq diameter_var (vlax-get-property obj 'InnerDiameterOrWidth))
            (setq diameter (cond
                             ((= (type diameter_var) 'VARIANT) (vlax-variant-value diameter_var))
                             ((= (type diameter_var) 'REAL) diameter_var)
                             ((= (type diameter_var) 'INT) (float diameter_var))
                             (T 0.0)))
            (setq z_start (safe-get obj "StartCenterlineElevation"))
            (setq z_end   (safe-get obj "EndCenterlineElevation"))
            (if (null z_start) (setq z_start (caddr p0)))
            (if (null z_end)   (setq z_end   (caddr p1)))
            (setq z_floor_start (- z_start (/ diameter 2.0)))
            (setq z_floor_end   (- z_end   (/ diameter 2.0)))
            (setq z_floor_start (atof (rtos z_floor_start 2 3)))
            (setq z_floor_end   (atof (rtos z_floor_end   2 3)))
            (setq pt_start (list (car p0) (cadr p0) z_floor_start))
            (setq pt_end   (list (car p1) (cadr p1) z_floor_end))
            (setq segs (cons (list pt_start pt_end pipe_name) segs))
          )
        )
        (setq i (1+ i))
      )
      (setq segs (reverse segs))
      ;;====================================================================
      ;; ЕСЛИ ВЫБРАН РЕЖИМ БЕЗ ОТСТУПА — ПРОСТО СОЗДАЁМ 2-ТОЧЕЧНЫЕ ПОЛИЛИНИИ
      ;;====================================================================
      (if (not with-offset)
        (progn
          (setq final_plines 0)
          (foreach seg segs
            (setq pt_start (car seg) pt_end (cadr seg) pipe_name (caddr seg))
            (setq pline (vla-Add3DPoly ms (vlax-safearray-fill
                          (vlax-make-safearray vlax-vbDouble '(0 . 5))
                          (append pt_start pt_end))))
            (princ (strcat "\nТруба: " pipe_name " | Z: " (rtos (caddr pt_start) 2 3) " \U+2192 " (rtos (caddr pt_end) 2 3)))
            (setq final_plines (1+ final_plines))
          )
          (princ (strcat "\n\nСоздано " (itoa final_plines) " 3D-полилиний БЕЗ отступов (по 2 вершины).\n"))
        )
        ;;====================================================================
        ;; РЕЖИМ С ОТСТУПОМ — СТАРАЯ ЛОГИКА (умное укорачивание + красный цвет)
        ;;====================================================================
        (progn
          ;; Сбор всех конечных точек
          (setq endpoints nil j 0)
          (foreach seg segs
            (add-endpoint (list (caar seg) (cadar seg)) (caddar seg) j t)
            (add-endpoint (list (caadr seg) (cadadr seg)) (caddr (cadr seg)) j nil)
            (setq j (1+ j))
          )
          ;; Находим минимальный Z только в конфликтных узлах
          (setq endpoint_z_map nil)
          (foreach group endpoints
            (setq xy (car group) entries (cdr group))
            (if (> (length entries) 1)
              (progn
                (setq zs (mapcar 'car entries))
                (setq min_z (apply 'min zs))
                (setq min_z (atof (rtos min_z 2 3)))
                (setq endpoint_z_map (cons (cons xy min_z) endpoint_z_map))
              )
            )
          )
          ;; Обработка каждого сегмента
          (setq processed_segs nil)
          (foreach seg segs
            (setq pt_start_orig (car seg) pt_end_orig (cadr seg) pipe_name (caddr seg))
            (setq xy_start (list (car pt_start_orig) (cadr pt_start_orig)))
            (setq xy_end   (list (car pt_end_orig)   (cadr pt_end_orig)))
            (setq z_start_col (cdr (assoc xy_start endpoint_z_map)))
            (setq z_end_col   (cdr (assoc xy_end   endpoint_z_map)))
            (setq need_short_start (and z_start_col (not (equal (caddr pt_start_orig) z_start_col 1e-3))))
            (setq need_short_end   (and z_end_col   (not (equal (caddr pt_end_orig)   z_end_col   1e-3))))
            (setq changed_start need_short_start changed_end need_short_end)
            (setq new_coords nil)
            ;; Начало
            (if need_short_start
              (progn
                (setq short_start (shorten-end pt_start_orig pt_end_orig d))
                (setq new_coords (list (list (car xy_start) (cadr xy_start) z_start_col) short_start))
              )
              (setq new_coords (list pt_start_orig))
            )
            ;; Конец
            (if need_short_end
              (progn
                (setq short_end (shorten-end pt_end_orig pt_start_orig d))
                (setq new_coords (append new_coords (list short_end (list (car xy_end) (cadr xy_end) z_end_col))))
              )
              (setq new_coords (append new_coords (list pt_end_orig)))
            )
            ;; Убираем лишние совпадающие вершины посередине
            (if (and (= (length new_coords) 4)
                     (approx-equal (nth 1 new_coords) (nth 2 new_coords) (* 2 d)))
              (setq new_coords (list (nth 0 new_coords) (nth 1 new_coords) (nth 3 new_coords)))
            )
            (setq processed_segs (cons (list new_coords pipe_name changed_start changed_end) processed_segs))
          )
          (setq processed_segs (reverse processed_segs))
          ;; Создание полилиний с отступом
          (setq final_plines 0)
          (foreach item processed_segs
            (setq coords_list (car item) pipe_name (cadr item)
                  changed_start (caddr item) changed_end (cadddr item))
            (if (>= (length coords_list) 2)
              (progn
                (setq flat_coords (apply 'append coords_list))
                (setq pline (vla-Add3DPoly ms
                             (vlax-safearray-fill
                               (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length flat_coords))))
                               flat_coords)))
                (if (or changed_start changed_end)
                  (vla-put-Color pline acRed) ; красный — была корректировка Z
                )
                (princ (strcat "\nТруба: " pipe_name
                               " | Вершин: " (itoa (length coords_list))
                               " | Z: " (rtos (caddr (car coords_list)) 2 3)
                               " \U+2192 " (rtos (caddr (last coords_list)) 2 3)))
                (setq final_plines (1+ final_plines))
              )
            )
          )
          (princ (strcat "\n\nСоздано " (itoa final_plines) " 3D-полилиний С 1-мм отступом. Красный цвет — Z был поднят.\n"))
        )
      )
    )
    (princ "\nОшибка: Трубы не выбраны или их нет в выборе.")
  )
  (setvar "OSMODE" old_osmode)
  (setvar "ELEVATION" old_elevation)
  (princ)
)
;;====================================================================
;;  ФУНКЦИЯ ВЫБОР РЕЖИМА
;;====================================================================
(defun ask-mode (/ choice)
  (initget "Да Нет Y N")
  (setq choice (getkword "\n[Да/Нет] <Да>: "))
  (if (or (null choice) (= choice "Да") (= choice "Y"))
    t
    nil
  )
)
;;====================================================================
;;  ДВЕ КОМАНДЫ ПОЛЬЗОВАТЕЛЯ С ВЫБОРОМ
;;====================================================================
(defun c:Pire3dpoly () 
  (princ "\n=== Pire3dpoly ===")
  (princ "\nСоздание 3D-полилиний из труб")
  (princ "\nДелать отступ 1мм для создания общей точки?")
  (Pipes-To-3DPoly (ask-mode))
  (princ)
)
(defun c:Труба3дполилиния () 
  (princ "\n=== Труба3дполилиния ===")
  (princ "\nСоздание 3D-полилиний из труб")
  (princ "\nДелать отступ 1мм для создания общей точки?")
  (Pipes-To-3DPoly (ask-mode))
  (princ)
)
;;====================================================================
;;  ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ (без изменений)
;;====================================================================
(defun safe-get (obj propname / val)
  (if (vlax-property-available-p obj propname)
    (progn
      (setq val (vlax-get-property obj propname))
      (cond ((= (type val) 'VARIANT) (vlax-variant-value val))
            (T val)))
    nil))
(defun add-endpoint (xy z idx is_start / found g newlist)
  (setq found nil)
  (foreach g endpoints
    (if (approx-equal xy (car g) tol)
      (progn
        (setq newlist (append (cdr g) (list (list z idx is_start))))
        (setq endpoints (subst (cons xy newlist) g endpoints))
        (setq found t)
      )
    )
  )
  (if (not found)
    (setq endpoints (cons (cons xy (list (list z idx is_start))) endpoints))
  )
)
(defun approx-equal (p1 p2 tol)
  (and (< (abs (- (car p1) (car p2))) tol)
       (< (abs (- (cadr p1) (cadr p2))) tol)))
(defun distance2d (p1 p2)
  (sqrt (+ (expt (- (car p2) (car p1)) 2)
           (expt (- (cadr p2) (cadr p1)) 2))))
(defun shorten-end (pt_this pt_other d / dx dy len2d unit_dx unit_dy dz new_x new_y new_z)
  (setq dx (- (car pt_other) (car pt_this))
        dy (- (cadr pt_other) (cadr pt_this))
        len2d (distance2d pt_this pt_other))
  (if (> len2d d)
    (progn
      (setq unit_dx (/ dx len2d)
            unit_dy (/ dy len2d)
            dz (- (caddr pt_other) (caddr pt_this))
            new_x (+ (car pt_this) (* unit_dx d))
            new_y (+ (cadr pt_this) (* unit_dy d))
            new_z (+ (caddr pt_this) (* (/ dz len2d) d)))
      (list new_x new_y (atof (rtos new_z 2 3))))
    pt_this))
(princ)
(defun c:Трубы3дполилиния () (c:Pire3dpoly))
; SWIFT-END
