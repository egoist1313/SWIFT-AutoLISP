;;; ====================================================================
;;; VERT.LSP  –  Проектные контуры и автоматическая посадка по съёмке
;;; Чертёж в МЕТРАХ, размеры вводятся в мм (автоконвертация /1000)
;;; Команды: VERT-SECT (построение проектных сечений)
;;;          VERT-FIT  (вписывание по съёмочным точкам + центры)
;;; Учитывает ПСК, корректная классификация точек для двутавра.
;;; ====================================================================

;; ---------- Глобальное хранилище параметров колонн ----------
(setq *vrt-columns* nil)

;; ---------- Пересечения осей (ActiveX) ----------
(defun vrt:get-axes-intersections (ss / i j objs n intpoints pt-lst)
  (setq objs '())
  (repeat (setq i (sslength ss))
    (setq objs (cons (vlax-ename->vla-object (ssname ss (setq i (1- i)))) objs)))
  (setq n (length objs)
        intpoints '())
  (setq i 0)
  (while (< i (1- n))
    (setq j (1+ i))
    (while (< j n)
      (if (setq pt-variant
                (vl-catch-all-apply 'vla-IntersectWith
                  (list (nth i objs) (nth j objs) acExtendNone)))
        (if (and (not (vl-catch-all-error-p pt-variant))
                 (> (vlax-safearray-get-u-bound
                      (vlax-variant-value pt-variant) 1) 0))
          (progn
            (setq pt-lst (vlax-safearray->list (vlax-variant-value pt-variant)))
            (while pt-lst
              (setq intpoints (cons (list (car pt-lst) (cadr pt-lst) (caddr pt-lst))
                                    intpoints)
                    pt-lst (cdddr pt-lst))))))
      (setq j (1+ j)))
    (setq i (1+ i)))
  ;; Убираем дубликаты
  (setq pt-lst '())
  (foreach p intpoints
    (if (not (vl-some '(lambda (x) (< (distance x p) 0.001)) pt-lst))
      (setq pt-lst (cons p pt-lst))))
  pt-lst)

;; ---------- Поворот точки ----------
(defun vrt:rotate-pt (pt ang)
  (list (- (* (car pt) (cos ang)) (* (cadr pt) (sin ang)))
        (+ (* (car pt) (sin ang)) (* (cadr pt) (cos ang)))
        0.0))

;; ---------- Рисование контуров ----------
(defun vrt:draw-rect (cx cy w h lay ang / pts)
  (setq pts (list (list (- (/ w 2.0)) (- (/ h 2.0)))
                  (list (+ (/ w 2.0)) (- (/ h 2.0)))
                  (list (+ (/ w 2.0)) (+ (/ h 2.0)))
                  (list (- (/ w 2.0)) (+ (/ h 2.0)))))
  (setq pts (mapcar '(lambda (p) (vrt:rotate-pt p ang)) pts))
  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay)
                         '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1))
                   (mapcar '(lambda (p) (list 10 (+ cx (car p)) (+ cy (cadr p)))) pts))))

(defun vrt:draw-circle (cx cy r lay)
  (entmake (list '(0 . "CIRCLE") '(100 . "AcDbEntity") (cons 8 lay)
                 '(100 . "AcDbCircle")
                 (list 10 cx cy 0.0) (cons 40 r))))

(defun vrt:draw-i-beam (cx cy h tw lay ang / b tf pts)
  ;; Автоматические пропорции: B = 0.5·H, tf = tw
  (setq b  (* 0.5 h)  tf tw)
  (setq pts
    (list
      (list (- (/ b 2.0))  (/ h 2.0))
      (list (+ (/ b 2.0))  (/ h 2.0))
      (list (+ (/ b 2.0))  (- (/ h 2.0) tf))
      (list (+ (/ tw 2.0)) (- (/ h 2.0) tf))
      (list (+ (/ tw 2.0)) (+ (/ h -2.0) tf))
      (list (+ (/ b 2.0))  (+ (/ h -2.0) tf))
      (list (+ (/ b 2.0))  (/ h -2.0))
      (list (- (/ b 2.0))  (/ h -2.0))
      (list (- (/ b 2.0))  (+ (/ h -2.0) tf))
      (list (- (/ tw 2.0)) (+ (/ h -2.0) tf))
      (list (- (/ tw 2.0)) (- (/ h 2.0) tf))
      (list (- (/ b 2.0))  (- (/ h 2.0) tf))
    ))
  (setq pts (mapcar '(lambda (p) (vrt:rotate-pt p ang)) pts))
  (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay)
                         '(100 . "AcDbPolyline") '(90 . 12) '(70 . 1))
                   (mapcar '(lambda (p) (list 10 (+ cx (car p)) (+ cy (cadr p)))) pts)))
  b)

;; ---------- Стрелка ГОСТ ----------
(defun vrt:draw-arrow (p-from p-to layer draw-scale
                       / dx dy len ang tip uscale base-pt)
  (setq dx  (- (car p-to)  (car p-from))
        dy  (- (cadr p-to) (cadr p-from))
        len (sqrt (+ (* dx dx) (* dy dy))))
  (if (> len 1e-9)
    (progn
      (setq ang    (atan dy dx)
            tip    (* 0.003 draw-scale)
            uscale (/ tip 0.3))
      (setq base-pt (list (- (car p-to)  (* tip (cos ang)))
                          (- (cadr p-to) (* tip (sin ang)))))
      (if (> (distance p-from base-pt) 1e-9)
        (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") (cons 8 layer)
                       '(100 . "AcDbLine")
                       (list 10 (car p-from)   (cadr p-from)   0.0)
                       (list 11 (car base-pt)  (cadr base-pt)  0.0))))
      (entmake (list '(0 . "INSERT") '(100 . "AcDbEntity") (cons 8 layer)
                     '(100 . "AcDbBlockReference")
                     '(2 . "VRT_ARROW")
                     (list 10 (car p-to) (cadr p-to) 0.0)
                     (cons 41 uscale) (cons 42 uscale) (cons 43 uscale)
                     (cons 50 ang))))))

;; ---------- Аннотативный текст ----------
(defun vrt:draw-annot-text (x y txt draw-scale attach layer / h)
  (setq h (* 0.0025 draw-scale))
  (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 layer)
                 '(100 . "AcDbMText")
                 (list 10 x y 0.0) (cons 40 h) (cons 41 0.0)
                 (cons 71 attach) (cons 1 txt))))

;; ---------- Блок стрелки ГОСТ ----------
(defun vrt:ensure-arrow-block (/ )
  (if (not (tblsearch "BLOCK" "VRT_ARROW"))
    (progn
      (entmake '((0 . "BLOCK") (100 . "AcDbEntity") (8 . "0")
                 (100 . "AcDbBlockBegin") (2 . "VRT_ARROW") (70 . 4) (10 0.0 0.0 0.0)))
      (entmake '((0 . "SOLID") (100 . "AcDbEntity") (8 . "0")
                 (100 . "AcDbTrace")
                 (10 -0.3  0.075 0.0)
                 (11 -0.3 -0.075 0.0)
                 (12  0.0  0.0   0.0)
                 (13  0.0  0.0   0.0)))
      (entmake '((0 . "ENDBLK") (100 . "AcDbEntity") (8 . "0") (100 . "AcDbBlockEnd"))))))

;; ---------- Слой ----------
(defun vrt:ensure-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name) '(70 . 0) (cons 62 color) '(6 . "Continuous")))))

;; ---------- Расстояние от точки до отрезка ----------
(defun vrt:dist-pt-to-seg (p seg1 seg2 / x0 y0 x1 y1 x2 y2 dx dy len t param)
  (setq x0 (car p) y0 (cadr p)
        x1 (car seg1) y1 (cadr seg1)
        x2 (car seg2) y2 (cadr seg2))
  (setq dx (- x2 x1) dy (- y2 y1))
  (setq len (sqrt (+ (* dx dx) (* dy dy))))
  (if (< len 1e-12)
    (distance p seg1)
    (progn
      (setq t (/ (+ (* (- x0 x1) dx) (* (- y0 y1) dy)) (* len len)))
      (if (< t 0.0)
        (distance p seg1)
        (if (> t 1.0)
          (distance p seg2)
          (distance p (list (+ x1 (* t dx)) (+ y1 (* t dy)))))))))

;; ---------- Классификация пары точек для двутавра ----------
(defun vrt:classify-ibeam-pair (pair cx cy H tw ang stand-pt
                                / B tf loc-stand vis-y vis-x
                                seg-top-left seg-top-right seg-bot-left seg-bot-right
                                seg-wright-top seg-wright-bot seg-wleft-top seg-wleft-bot
                                calc-dists d1 d2 pl1 w1 pl2 w2)
  (setq B  (* 0.5 H)  tf tw)
  ;; Видимые грани (по направлению стоянки в локальной системе)
  (setq loc-stand (vrt:rotate-pt (list (- (car stand-pt) cx) (- (cadr stand-pt) cy)) (- ang)))
  (setq vis-y (if (> (cadr loc-stand) 0) (/ H 2.0) (/ H -2.0)))
  (setq vis-x (if (> (car loc-stand) 0) (/ tw 2.0) (/ tw -2.0)))

  ;; Координаты отрезков в локальной системе
  (setq seg-top-left   (list (- (/ B 2.0)) (/ H 2.0))
        seg-top-right  (list (/ B 2.0)     (/ H 2.0))
        seg-bot-left   (list (- (/ B 2.0)) (/ H -2.0))
        seg-bot-right  (list (/ B 2.0)     (/ H -2.0))
        seg-wright-top (list (/ tw 2.0)    (- (/ H 2.0) tf))
        seg-wright-bot (list (/ tw 2.0)    (+ (/ H -2.0) tf))
        seg-wleft-top  (list (- (/ tw 2.0)) (- (/ H 2.0) tf))
        seg-wleft-bot  (list (- (/ tw 2.0)) (+ (/ H -2.0) tf)))

  ;; Функция вычисления расстояний для одной точки
  (defun calc-dists (pt / loc)
    (setq loc (vrt:rotate-pt (list (- (car pt) cx) (- (cadr pt) cy)) (- ang)))
    (list
      ;; расстояния до полок
      (min (vrt:dist-pt-to-seg loc seg-top-left seg-top-right)
           (vrt:dist-pt-to-seg loc seg-bot-left seg-bot-right))
      ;; расстояния до стенок
      (min (vrt:dist-pt-to-seg loc seg-wright-top seg-wright-bot)
           (vrt:dist-pt-to-seg loc seg-wleft-top seg-wleft-bot))))

  (setq p1 (car pair) p2 (cadr pair))
  (setq d1 (calc-dists p1) d2 (calc-dists p2))
  (setq pl1 (car d1) w1 (cadr d1)
        pl2 (car d2) w2 (cadr d2))

  ;; Кто полка?
  (cond
    ((and (< pl1 w1) (>= pl2 w2)) (list p1 p2))   ; p1 - полка, p2 - стенка
    ((and (>= pl1 w1) (< pl2 w2)) (list p2 p1))   ; p2 - полка, p1 - стенка
    ;; Обе ближе к полкам или обе к стенкам — ситуация нештатная, но пытаемся угадать по отношению
    ((< (/ pl1 w1) (/ pl2 w2)) (list p1 p2))
    (T (list p2 p1)))
)

;; ======================================================================
;;  VERT-SECT  (учёт угла ПСК)
;; ======================================================================
(defun C:VERT-SECT (/ ss pts type bw bh diam tw ang ucs-ang calc-b)
  (vl-load-com)
  (if (not (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE")))))
    (progn (princ "\nОси не выбраны.") (exit)))
  (setq pts (vrt:get-axes-intersections ss))
  (if (not pts)
    (progn (princ "\nПересечения не найдены.") (exit)))
  (princ (strcat "\nНайдено " (itoa (length pts)) " пересечений."))

  (initget "Прямоугольник Круг Двутавр")
  (setq type (getkword "\nТип сечения [Прямоугольник/Круг/Двутавр] <Прямоугольник>: "))
  (if (not type) (setq type "Прямоугольник"))

  (setq ucs-ang (angle '(0 0) (getvar 'ucsxdir)))
  (setq ang 0.0)

  (if (= type "Двутавр")
    (progn
      (initget 4)
      (setq ang (getreal (strcat "\nУгол поворота сечения относительно ПСК (градусы) <0>: ")))
      (if (not ang) (setq ang 0.0))
      (setq ang (+ (* pi (/ ang 180.0)) ucs-ang))))

  (vrt:ensure-layer "ПРОЕКТ-КОЛОННА" 5)
  (setq *vrt-columns* nil)

  (cond
    ((= type "Прямоугольник")
      (setq bw (getreal "\nШирина B, мм: "))
      (setq bh (getreal "\nВысота H, мм: "))
      (if (or (not bw) (not bh) (<= bw 0) (<= bh 0))
        (progn (princ "\nНекорректные размеры.") (exit)))
      (setq bw (/ bw 1000.0) bh (/ bh 1000.0))
      (foreach p pts
        (vrt:draw-rect (car p) (cadr p) bw bh "ПРОЕКТ-КОЛОННА" 0.0)
        (setq *vrt-columns* (cons (list (list (car p) (cadr p)) "rect" bw bh 0.0 0.0) *vrt-columns*)))
      (princ "\nПрямоугольные сечения построены."))

    ((= type "Круг")
      (setq diam (getreal "\nДиаметр D, мм: "))
      (if (or (not diam) (<= diam 0))
        (progn (princ "\nНекорректный диаметр.") (exit)))
      (setq diam (/ diam 1000.0))
      (foreach p pts
        (vrt:draw-circle (car p) (cadr p) (/ diam 2.0) "ПРОЕКТ-КОЛОННА")
        (setq *vrt-columns* (cons (list (list (car p) (cadr p)) "circ" diam 0.0 0.0 0.0) *vrt-columns*)))
      (princ "\nКруглые сечения построены."))

    ((= type "Двутавр")
      (setq bh (getreal "\nВысота сечения H, мм: "))
      (setq tw (getreal "\nТолщина стенки tw, мм: "))
      (if (or (not bh) (not tw) (<= bh 0) (<= tw 0) (> tw (* 0.5 bh)))
        (progn (princ "\nНекорректные параметры (tw > H/2).") (exit)))
      (setq bh (/ bh 1000.0)  tw (/ tw 1000.0))
      (foreach p pts
        (setq calc-b (vrt:draw-i-beam (car p) (cadr p) bh tw "ПРОЕКТ-КОЛОННА" ang))
        (setq *vrt-columns* (cons (list (list (car p) (cadr p)) "ibeam" calc-b bh tw ang) *vrt-columns*)))
      (princ (strcat "\nДвутавры построены (B=" (rtos (* 0.5 bh 1000) 2 0)
                     " мм, tf=" (rtos (* tw 1000) 2 0) " мм, угол в МСК "
                     (angtos ang 0 1) "°).")))
  )
  (setq *vrt-columns* (reverse *vrt-columns*))
  (princ (strcat "\nЗапомнено " (itoa (length *vrt-columns*)) " колонн."))
  (princ)
)

;; ======================================================================
;;  VERT-FIT  (исправлена классификация для двутавра)
;; ======================================================================
(defun C:VERT-FIT ( / stand-pt ss-allpts allpts col-pars cx cy type B H tw ang
                      R zone-pts low high signX signY 
                      pair-low pair-high Ppl_loc Pw_loc dx dy cf_low cf_high draw-scale)
  (vl-load-com)
  (if (null *vrt-columns*)
    (progn (princ "\nНет данных о колоннах. Сначала выполните VERT-SECT.") (exit)))

  (vrt:ensure-arrow-block)
  (vrt:ensure-layer "ФАКТ-КОЛОННА" 1)
  (vrt:ensure-layer "ФАКТ-СТРЕЛКА" 1)
  (vrt:ensure-layer "ФАКТ-ТЕКСТ" 2)
  (vrt:ensure-layer "Низ центр" 3)
  (vrt:ensure-layer "Верх центр" 4)

  (if (not (setq stand-pt (getpoint "\nУкажите точку стоянки тахеометра: ")))
    (progn (princ "\nТочка стоянки не задана.") (exit)))

  (princ "\nВыберите ВСЕ съёмочные точки (POINT, CIRCLE, INSERT): ")
  (if (not (setq ss-allpts (ssget '((0 . "POINT,CIRCLE,INSERT")))))
    (progn (princ "\nТочки не выбраны.") (exit)))

  (setq allpts '())
  (repeat (setq i (sslength ss-allpts))
    (setq ent (ssname ss-allpts (setq i (1- i)))
          pt  (cdr (assoc 10 (entget ent))))
    (setq allpts (cons pt allpts)))
  (setq allpts (reverse allpts))

  (setq draw-scale 50.0)

  (foreach col *vrt-columns*
    (setq cx (caar col)  cy (cadar col)
          type (nth 1 col)  B (nth 2 col)  H (nth 3 col)  tw (nth 4 col)  ang (nth 5 col))
    (if (= type "rect") (setq tw 0.0))

    (setq R (+ (sqrt (+ (* (/ B 2.0) (/ B 2.0)) (* (/ H 2.0) (/ H 2.0)))) 0.2))
    (setq zone-pts (vl-remove-if-not
                     '(lambda (p) (<= (distance (list cx cy) (list (car p) (cadr p))) R))
                     allpts))

    (if (/= (length zone-pts) 4)
      (princ (strcat "\nКолонна (" (rtos cx 2 4) "," (rtos cy 2 4) ") – найдено "
                     (itoa (length zone-pts)) " точек, нужно 4. Пропущена."))
      (progn
        (setq zone-pts (vl-sort zone-pts '(lambda (a b) (< (caddr a) (caddr b)))))
        (setq low  (list (car zone-pts) (cadr zone-pts))
              high (list (caddr zone-pts) (cadddr zone-pts)))

        (cond
          ;; --- Двутавр ---
          ((= type "ibeam")
            ;; Классификация по близости к отрезкам
            (setq pair-low (vrt:classify-ibeam-pair low cx cy H tw ang stand-pt))
            (setq pair-high (vrt:classify-ibeam-pair high cx cy H tw ang stand-pt))

            ;; Вычисляем видимые грани (смещение именно до них)
            (setq loc-stand (vrt:rotate-pt (list (- (car stand-pt) cx) (- (cadr stand-pt) cy)) (- ang)))
            (setq vis-y (if (> (cadr loc-stand) 0) (/ H 2.0) (/ H -2.0)))
            (setq vis-x (if (> (car loc-stand) 0) (/ tw 2.0) (/ tw -2.0)))

            ;; Фактический центр низа
            (setq Ppl_loc (vrt:rotate-pt (list (- (caar pair-low) cx) (- (cadar pair-low) cy)) (- ang)))
            (setq Pw_loc  (vrt:rotate-pt (list (- (caadr pair-low) cx) (- (cadadr pair-low) cy)) (- ang)))
            (setq dy (- (cadr Ppl_loc) vis-y))
            (setq dx (- (car Pw_loc) vis-x))
            (setq cf_low (list (+ cx (car (vrt:rotate-pt (list dx dy) ang)))
                               (+ cy (cadr (vrt:rotate-pt (list dx dy) ang)))))

            ;; Фактический центр верха
            (setq Ppl_loc (vrt:rotate-pt (list (- (caar pair-high) cx) (- (cadar pair-high) cy)) (- ang)))
            (setq Pw_loc  (vrt:rotate-pt (list (- (caadr pair-high) cx) (- (cadadr pair-high) cy)) (- ang)))
            (setq dy (- (cadr Ppl_loc) vis-y))
            (setq dx (- (car Pw_loc) vis-x))
            (setq cf_high (list (+ cx (car (vrt:rotate-pt (list dx dy) ang)))
                                (+ cy (cadr (vrt:rotate-pt (list dx dy) ang)))))

            ;; Рисуем фактический двутавр
            (vrt:draw-i-beam (car cf_low) (cadr cf_low) H tw "ФАКТ-КОЛОННА" ang)
            (vrt:draw-i-beam (car cf_high) (cadr cf_high) H tw "ФАКТ-КОЛОННА" ang))

          ;; --- Прямоугольник / круг (упрощённая классификация) ---
          (T
            (setq signX (if (> (car (vrt:rotate-pt (list (- (car stand-pt) cx) (- (cadr stand-pt) cy)) (- ang))) 0) 1.0 -1.0))
            (setq signY (if (> (cadr (vrt:rotate-pt (list (- (car stand-pt) cx) (- (cadr stand-pt) cy)) (- ang))) 0) 1.0 -1.0))
            ;; Кто полка?
            (defun classify-rect (pair / loc1 loc2)
              (setq loc1 (vrt:rotate-pt (list (- (caar pair) cx) (- (cadar pair) cy)) (- ang)))
              (setq loc2 (vrt:rotate-pt (list (- (caadr pair) cx) (- (cadadr pair) cy)) (- ang)))
              (if (< (abs (- (cadr loc1) (* signY (/ H 2.0))))
                     (abs (- (car loc2) (* signX (/ B 2.0)))))
                pair
                (list (cadr pair) (car pair))))
            (setq low (classify-rect low))
            (setq high (classify-rect high))
            ;; Смещение
            (setq Ppl_loc (vrt:rotate-pt (list (- (caar low) cx) (- (cadar low) cy)) (- ang)))
            (setq Pw_loc  (vrt:rotate-pt (list (- (caadr low) cx) (- (cadadr low) cy)) (- ang)))
            (setq dy (- (cadr Ppl_loc) (* signY (/ H 2.0))))
            (setq dx (- (car Pw_loc)  (* signX (/ B 2.0))))
            (setq cf_low (list (+ cx dx) (+ cy dy)))
            (setq Ppl_loc (vrt:rotate-pt (list (- (caar high) cx) (- (cadar high) cy)) (- ang)))
            (setq Pw_loc  (vrt:rotate-pt (list (- (caadr high) cx) (- (cadadr high) cy)) (- ang)))
            (setq dy (- (cadr Ppl_loc) (* signY (/ H 2.0))))
            (setq dx (- (car Pw_loc)  (* signX (/ B 2.0))))
            (setq cf_high (list (+ cx dx) (+ cy dy)))
            ;; Рисуем
            (if (= type "rect")
              (progn
                (vrt:draw-rect (car cf_low) (cadr cf_low) B H "ФАКТ-КОЛОННА" 0.0)
                (vrt:draw-rect (car cf_high) (cadr cf_high) B H "ФАКТ-КОЛОННА" 0.0)))
            (if (= type "circ")
              (progn
                (vrt:draw-circle (car cf_low) (cadr cf_low) (/ B 2.0) "ФАКТ-КОЛОННА")
                (vrt:draw-circle (car cf_high) (cadr cf_high) (/ B 2.0) "ФАКТ-КОЛОННА"))))
        )

        ;; Точки центров
        (entmake (list '(0 . "POINT") (cons 8 "Низ центр") (cons 10 (append cf_low '(0.0)))))
        (entmake (list '(0 . "POINT") (cons 8 "Верх центр") (cons 10 (append cf_high '(0.0)))))

        ;; Стрелки
        (if (> (distance (list cx cy) cf_low) 0.001)
          (progn
            (vrt:draw-arrow (list cx cy) cf_low "ФАКТ-СТРЕЛКА" draw-scale)
            (vrt:draw-annot-text (car cf_low) (+ (cadr cf_low) (* 0.003 draw-scale))
              (strcat "Δ=" (rtos (* (distance (list cx cy) cf_low) 1000) 2 1) "мм") draw-scale 8 "ФАКТ-ТЕКСТ")))
        (if (> (distance (list cx cy) cf_high) 0.001)
          (progn
            (vrt:draw-arrow (list cx cy) cf_high "ФАКТ-СТРЕЛКА" draw-scale)
            (vrt:draw-annot-text (car cf_high) (+ (cadr cf_high) (* 0.003 draw-scale))
              (strcat "Δ=" (rtos (* (distance (list cx cy) cf_high) 1000) 2 1) "мм") draw-scale 8 "ФАКТ-ТЕКСТ")))
        (if (> (distance cf_low cf_high) 0.001)
          (progn
            (vrt:draw-arrow cf_low cf_high "ФАКТ-СТРЕЛКА" draw-scale)
            (vrt:draw-annot-text (car cf_high) (+ (cadr cf_high) (* 0.006 draw-scale))
              (strcat "Верт=" (rtos (* (distance cf_low cf_high) 1000) 2 1) "мм") draw-scale 8 "ФАКТ-ТЕКСТ")))

        (princ (strcat "\nКолонна (" (rtos cx 2 4) "," (rtos cy 2 4) ") – вписана."))
      )
    )
  )
  (princ "\nГотово.")
  (princ)
)

(princ "\nVERT.LSP загружен. Команды: VERT-SECT, VERT-FIT")
(princ)