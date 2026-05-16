(defun c:SNAP_VERT (/ ss_pts ss_pl pt_list pl_list i j
                      pt ename edata verts v dist min_dist
                      best_pt best_v dx dy move_vec)

  ;; ── 1. Собираем точки ──────────────────────────────────────
  (princ "\nВыберите точки: ")
  (setq ss_pts (ssget '((0 . "POINT"))))
  (if (null ss_pts)
    (progn (princ "\nТочки не выбраны.") (exit)))

  ;; ── 2. Собираем полилинии ──────────────────────────────────
  (princ "\nВыберите полилинии: ")
  (setq ss_pl (ssget '((0 . "LWPOLYLINE,POLYLINE,2DPOLYLINE,3DPOLYLINE"))))
  (if (null ss_pl)
    (progn (princ "\nПолилинии не выбраны.") (exit)))

  ;; ── 3. Список координат точек ──────────────────────────────
  (setq pt_list '())
  (setq i 0)
  (repeat (sslength ss_pts)
    (setq ename (ssname ss_pts i))
    (setq edata (entget ename))
    (setq pt (cdr (assoc 10 edata)))          ; координата POINT
    (setq pt_list (append pt_list (list pt)))
    (setq i (1+ i))
  )

  ;; ── 4. Обрабатываем каждую полилинию ──────────────────────
  (setq i 0)
  (repeat (sslength ss_pl)
    (setq ename  (ssname ss_pl i))
    (setq edata  (entget ename))
    (setq i (1+ i))

    ;; --- получаем вершины -----------------------------------
    (setq verts '())

    (cond
      ;; LWPOLYLINE — вершины кодом 10 в самой записи
      ((= (cdr (assoc 0 edata)) "LWPOLYLINE")
       (foreach pair edata
         (if (= (car pair) 10)
           (setq verts (append verts (list (cdr pair))))
         )
       )
      )
      ;; Старая POLYLINE — обходим субэнтити VERTEX
      (T
       (setq sub (entnext ename))
       (while (and sub
                   (/= (cdr (assoc 0 (entget sub))) "SEQEND"))
         (setq sdata (entget sub))
         (if (= (cdr (assoc 0 sdata)) "VERTEX")
           (setq verts (append verts
                         (list (cdr (assoc 10 sdata)))))
         )
         (setq sub (entnext sub))
       )
      )
    )

    (if (null verts)
      (progn (princ "\nНет вершин, пропускаю."))

      (progn
        ;; --- ищем ближайшую пару (вершина ↔ точка) -----------
        (setq min_dist 1e38
              best_v   nil
              best_pt  nil)

        (foreach v verts
          (foreach pt pt_list
            ;; дистанция 2D
            (setq dx   (- (car  pt) (car  v))
                  dy   (- (cadr pt) (cadr v))
                  dist (sqrt (+ (* dx dx) (* dy dy))))
            (if (< dist min_dist)
              (setq min_dist dist
                    best_v   v
                    best_pt  pt)
            )
          )
        )

        ;; --- смещение = best_pt – best_v ---------------------
        (setq move_vec
          (list (- (car  best_pt) (car  best_v))
                (- (cadr best_pt) (cadr best_v))
                0.0))

        ;; --- команда MOVE ------------------------------------
        (command "._MOVE"
                 ename ""
                 '(0 0 0)
                 move_vec)

        (princ (strcat "\nПолилиния перемещена на "
                       (rtos (car  move_vec) 2 4) ", "
                       (rtos (cadr move_vec) 2 4)))
      )
    )
  ) ; repeat полилиний

  (princ "\nГотово.")
  (princ)
)