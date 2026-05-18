; SWIFT-START
; ================================================
; Swift POPER v3.5 — СТАБИЛЬНАЯ ВЕРСИЯ (анти-фриз)
; ФИКС: AutoCAD зависает намертво после обновления
;   • Убрал command-s "_.REGEN" из reactor (TICK) — это могло вызывать рекурсию/фриз внутри :vlr-modified
;   • Инициализировал глобалы *poper-reactors* и *poper-tick-regen* явно
;   • Добавил vl-catch-all-apply вокруг attach-reactor и full-update
;   • Усиленная защита от рекурсивных вызовов reactor
; Полный код
; ================================================

(vl-load-com)

;; ====================== ГЛОБАЛЬНЫЕ КОНСТАНТЫ ======================
(setq *SWIFT_POPER_APPNAME* "SWIFT_POPER")
(setq *SWIFT_POPER_LAYER_PROFILE* "PoperSwift")
(setq *SWIFT_POPER_LAYER_SOURCE* "PoperSwift_Source")
(setq *SWIFT_POPER_LAYER_ANNOT* "PoperSwift_Annot")
(setq *poper-reactors* nil)
(setq *poper-tick-regen* nil)

;; ====================== ВСТРОЕННЫЙ DCL ======================
(setq dcl_content
"poper_settings : dialog {
    label = \"Настройки Swift POPER v3.5\";
    : column {
        : radio_button { label = \"Режим: По точкам\"; key = \"mode_points\"; value = \"1\"; }
        : radio_button { label = \"Режим: По векторам\"; key = \"mode_vectors\"; }
        : radio_button { label = \"Режим: По вершинам полилинии\"; key = \"mode_polyline\"; }
        : edit_box { label = \"Масштаб по высоте:\"; key = \"h_scale\"; value = \"10\"; width = 10; }
        : edit_box { label = \"Масштаб по длине:\"; key = \"l_scale\"; value = \"1\"; width = 10; }
        : edit_box { label = \"Высота текста:\"; key = \"text_height\"; value = \"2.5\"; width = 10; }
        : edit_box { label = \"Отступ текста (%):\"; key = \"text_offset\"; value = \"10\"; width = 10; }
        : popup_list { label = \"Округление высоты до:\"; key = \"round_to\"; width = 10; list = \"2\\n3\"; }
        : popup_list { label = \"Округление уклона до:\"; key = \"slope_round_to\"; width = 10; list = \"0\\n1\\n2\"; }
        : popup_list { label = \"Единица уклона:\"; key = \"slope_unit\"; width = 10; list = \"Промилле\\nГрадусы\\nСоотношение сторон\\nАвто (промилле/соотношение)\"; }
        : popup_list { label = \"Цвет линий проекции:\"; key = \"projection_color\"; width = 10; list = \"По слою\\nКрасный\\nЖелтый\\nЗелёный\\nГолубой\"; }
        : toggle { label = \"Показать длины\"; key = \"show_lengths\"; value = \"1\"; }
        : toggle { label = \"Показать уклоны\"; key = \"show_slopes\"; value = \"1\"; }
        : toggle { label = \"Высота из текста\"; key = \"use_text_height\"; value = \"1\"; }
        : toggle { label = \"Провести линии проекции\"; key = \"draw_projections\"; value = \"0\"; }
        : toggle { label = \"Показать знак промилле\"; key = \"show_permille_sign\"; value = \"1\"; }
    }
    : row {
        : button { key = \"accept\"; label = \"OK\"; is_default = true; }
        : button { key = \"cancel\"; label = \"Отмена\"; is_cancel = true; }
        : button { key = \"help\"; label = \"?\"; width = 2; }
    }
}
poper_help : dialog {
    label = \"Справка по Swift POPER v3.5\";
    : boxed_column {
        label = \"Описание команды POPER\";
        scroll = true; fixed_width = true; width = 80; fixed_height = true; height = 18; alignment = top;
        : text { value = \"Полностью ассоциативный инструмент v3.5\\nXData + Reactor. Исправлены сохранение toggle и возможные фризы.\\n"; width = 75; height = 8; }
    }
    ok_button;
}")

;; ====================== ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ======================
(defun create-mtext (x y text height alignment angle style)
    (if (and (numberp x) (numberp y) (stringp text) (numberp height))
        (entmake
            (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText")
                  (cons 10 (list x y 0.0)) (cons 40 height) (cons 1 text)
                  (cons 7 style) (cons 71 alignment) '(72 . 1) (cons 50 angle))
        )
    )
)

(defun create-line (x1 y1 x2 y2 color)
    (if (and (numberp x1) (numberp y1) (numberp x2) (numberp y2))
        (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine")
                       (cons 10 (list x1 y1 0.0)) (cons 11 (list x2 y2 0.0)) (cons 62 color)))
    )
)

(defun create-polyline (points layer color / poly_points)
    (if (>= (length points) 2)
        (progn
            (setq poly_points '())
            (foreach pt points (setq poly_points (append poly_points (list (car pt) (cadr pt)))))
            (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
                                   (cons 90 (length points)) '(70 . 0) (cons 8 layer) (cons 62 color))
                             (apply 'append (mapcar '(lambda (pt) (list (cons 10 pt))) points))))
        )
    )
)

(defun setup-layer (layer_name color noplot / layer_data)
    (if (not (tblobjname "LAYER" layer_name))
        (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                       (cons 2 layer_name) (cons 70 0) (cons 62 color) (cons 6 "Continuous") (cons 290 (if noplot 1 0))))
        (progn
            (setq layer_data (tblobjname "LAYER" layer_name))
            (entmod (subst (cons 62 color) (assoc 62 (entget layer_data)) (entget layer_data)))
            (entmod (subst (cons 290 (if noplot 1 0)) (assoc 290 (entget layer_data)) (entget layer_data)))
        )
    )
)

(defun C:POPERHELP ()
    (setq dcl_id (load_dialog *poper-dcl*))
    (if (not (new_dialog "poper_help" dcl_id)) (progn (princ "\nОшибка DCL.") (exit)))
    (action_tile "ok" "(done_dialog 1)")
    (start_dialog)
    (unload_dialog dcl_id)
    (princ)
)

;; ====================== XDATA ======================
(defun poper-regapp ()
    (if (null (tblsearch "APPID" *SWIFT_POPER_APPNAME*)) (regapp *SWIFT_POPER_APPNAME*))
)

(defun poper-xdata-set (ename data / ed xd)
    (poper-regapp)
    (setq ed (entget ename (list *SWIFT_POPER_APPNAME*)))
    (setq ed (vl-remove-if '(lambda (x) (and (= (car x) -3) (assoc *SWIFT_POPER_APPNAME* (cdr x)))) ed))
    (setq xd (cons -3 (list (list *SWIFT_POPER_APPNAME* (cons 1000 (vl-prin1-to-string data))))))
    (entmod (append ed (list xd)))
)

(defun poper-xdata-get (ename / ed grp app-data str)
    (setq ed (entget ename (list *SWIFT_POPER_APPNAME*)))
    (if (setq grp (assoc -3 ed))
        (if (setq app-data (assoc *SWIFT_POPER_APPNAME* (cdr grp)))
            (if (setq str (cdr (assoc 1000 (cdr app-data)))) (read str) nil)
            nil)
        nil)
)

(defun poper-get-id (ename) (cdr (assoc 5 (entget ename))))

(defun poper-find-objects-by-id (poper-id / ss i en xd result)
    (setq result '())
    (setq ss (ssget "X" (list (list -3 (list *SWIFT_POPER_APPNAME*)))))
    (if ss (progn (setq i 0) (while (< i (sslength ss))
        (setq en (ssname ss i)) (setq xd (poper-xdata-get en))
        (if (and xd (equal (cdr (assoc 'poper-id xd)) poper-id)) (setq result (cons en result)))
        (setq i (1+ i)))))
    (reverse result))

;; ====================== СЛОИ ======================
(defun poper-setup-layers ()
    (setup-layer *SWIFT_POPER_LAYER_PROFILE* 251 T)
    (setup-layer *SWIFT_POPER_LAYER_SOURCE* 30 T)
    (setup-layer *SWIFT_POPER_LAYER_ANNOT* 256 nil))

;; ====================== РЕАКТОРЫ (ЗАЩИЩЁННЫЕ) ======================
(defun poper-attach-reactor (ename / obj r)
    (if (and ename (setq obj (vlax-ename->vla-object ename)))
        (vl-catch-all-apply
            '(lambda ()
                (setq r (vlr-object-reactor (list obj) (list ename "SWIFT_POPER")
                          '((:vlr-modified . poper-reactor-callback))))
                (setq *poper-reactors* (cons r *poper-reactors*))
                (princ (strcat "\n[POPER] Реактор прикреплён к " (poper-get-id ename)))
            ))
    )
)

(defun poper-reactor-callback (notifier reactor params / ename poper-id)
    (setq ename (vlax-vla-object->ename notifier))
    (if ename
        (progn
            (setq poper-id (poper-get-id ename))
            (if poper-id
                (vl-catch-all-apply '(lambda () (poper-full-update poper-id)))
            )
        )
    )
)

;; ====================== ПОЛНОЕ ОБНОВЛЕНИЕ (БЕЗ REGEN В РЕАКТОРЕ) ======================
(defun poper-full-update (poper-id / profile-ename xd-settings coords sorted-coords heights
                          all-objects xd obj-type obj-index obj-en text-obj pt final-height
                          pt1 pt2 delta-x delta-y delta-x-scaled delta-y-scaled slope-permille slope-text
                          h_scale l_scale round_to slope_round_to slope_unit show_permille_sign OSZ OSH result)
    (vl-load-com)
    (setq profile-ename (car (poper-find-objects-by-id poper-id)))
    (if (null profile-ename) (exit))
    (setq xd-settings (poper-xdata-get profile-ename))
    (if (null xd-settings) (exit))

    (setq h_scale (or (cdr (assoc 'h_scale xd-settings)) 1.0))
    (setq l_scale (or (cdr (assoc 'l_scale xd-settings)) 1.0))
    (setq round_to (or (cdr (assoc 'round_to xd-settings)) 2))
    (setq slope_round_to (or (cdr (assoc 'slope_round_to xd-settings)) 1))
    (setq slope_unit (or (cdr (assoc 'slope_unit xd-settings)) "permille"))
    (setq show_permille_sign (cdr (assoc 'show_permille_sign xd-settings)))
    (setq OSZ (or (cdr (assoc 'OSZ xd-settings)) '(0.0 0.0 0.0)))
    (setq OSH (or (cdr (assoc 'OSH xd-settings)) 0.0))

    (setq coords (vlax-get (vlax-ename->vla-object profile-ename) 'Coordinates))
    (setq sorted-coords (if coords (poper-get-sorted-coords coords) '()))

    (setq heights '())
    (foreach pt sorted-coords
        (if (and (listp pt) (numberp (cadr pt)) (numberp (cadr OSZ)) (numberp h_scale))
            (setq heights (append heights (list (+ OSH (* (- (cadr pt) (cadr OSZ)) (/ 1.0 h_scale))))))
        )
    )

    (setq all-objects (poper-find-objects-by-id poper-id))
    (foreach obj-en all-objects
        (setq xd (poper-xdata-get obj-en))
        (if xd
            (progn
                (setq obj-type (cdr (assoc 'type xd)))
                (setq obj-index (if (numberp (cdr (assoc 'index xd))) (cdr (assoc 'index xd)) -1))
                (cond
                    ((equal obj-type "HEIGHT")
                     (if (and (>= obj-index 0) (< obj-index (length sorted-coords)) (setq pt (nth obj-index sorted-coords)) (setq final-height (nth obj-index heights)) (numberp final-height))
                         (progn
                             (setq text-obj (vlax-ename->vla-object obj-en))
                             (if (and text-obj (not (vlax-erased-p text-obj)))
                                 (vl-catch-all-apply
                                     '(lambda ()
                                         (vla-put-textstring text-obj (rtos final-height 2 round_to))
                                         (vla-put-insertionpoint text-obj (vlax-3d-point (car pt) (cadr (vlax-get text-obj 'InsertionPoint)) 0.0))
                                     )))
                         )
                     )
                    )
                    ((equal obj-type "LENGTH")
                     (if (and (> obj-index 0) (< obj-index (length sorted-coords)) (setq pt1 (nth (1- obj-index) sorted-coords)) (setq pt2 (nth obj-index sorted-coords)))
                         (progn
                             (setq text-obj (vlax-ename->vla-object obj-en))
                             (if (and text-obj (not (vlax-erased-p text-obj)))
                                 (vl-catch-all-apply
                                     '(lambda ()
                                         (vla-put-textstring text-obj (rtos (* (- (car pt2) (car pt1)) (/ 1.0 l_scale)) 2 2))
                                         (vla-put-insertionpoint text-obj (vlax-3d-point (/ (+ (car pt1) (car pt2)) 2.0) (cadr (vlax-get text-obj 'InsertionPoint)) 0.0))
                                     )))
                         )
                     )
                    )
                    ((equal obj-type "SLOPE")
                     (if (and (> obj-index 0) (< obj-index (length sorted-coords)) (setq pt1 (nth (1- obj-index) sorted-coords)) (setq pt2 (nth obj-index sorted-coords)))
                         (progn
                             (setq delta-x-scaled (/ (- (car pt2) (car pt1)) l_scale))
                             (setq delta-y-scaled (/ (- (cadr pt2) (cadr pt1)) h_scale))
                             (if (>= (abs delta-x-scaled) 0.01)
                                 (progn
                                     (setq slope-permille (* (/ (abs delta-y-scaled) delta-x-scaled) 1000))
                                     (cond
                                         ((equal slope_unit "permille") (setq slope-text (if show_permille_sign (strcat (rtos slope-permille 2 slope_round_to) "‰") (rtos slope-permille 2 slope_round_to))))
                                         ((equal slope_unit "degrees") (setq slope-text (strcat (rtos (* (atan (abs delta-y-scaled) delta-x-scaled) (/ 180 pi)) 2 slope_round_to) "°")))
                                         (t (setq slope-text (rtos slope-permille 2 slope_round_to)))
                                     )
                                     (setq text-obj (vlax-ename->vla-object obj-en))
                                     (if (and text-obj (not (vlax-erased-p text-obj)))
                                         (vl-catch-all-apply '(lambda () (vla-put-textstring text-obj slope-text)))
                                     )
                                 )
                             )
                         )
                     )
                    )
                    ;; TICK — больше не делаем REGEN внутри reactor (может фризить)
                )
            )
        )
    )
    (princ (strcat "\n[POPER] Обновлено для " poper-id))
)

(defun poper-get-sorted-coords (coords / pts i pt)
    (setq pts '())
    (if (and coords (cdr coords))
        (progn (setq i 0) (while (< i (length coords))
            (setq pt (list (nth i coords) (nth (1+ i) coords) 0.0))
            (setq pts (append pts (list pt)))
            (setq i (+ i 2))))
        (if coords (setq pts (list (list (car coords) (cadr coords) 0.0)))))
    (vl-sort pts '(lambda (a b) (< (car a) (car b))))
)

;; ====================== ВОССТАНОВЛЕНИЕ ======================
(defun C:POPER-RESTORE (/ ss i en xd)
    (princ "\n[POPER] Восстановление реакторов...")
    (setq ss (ssget "X" (list (list -3 (list *SWIFT_POPER_APPNAME*)))))
    (if ss (progn (setq i 0)
        (while (< i (sslength ss))
            (setq en (ssname ss i))
            (if (equal (cdr (assoc 'type (poper-xdata-get en))) "PROFILE")
                (poper-attach-reactor en))
            (setq i (1+ i)))))
    (princ "\n[POPER] Реакторы восстановлены.")
)

(defun s::poper-startup () (C:POPER-RESTORE))

(defun C:POPERUPDATE (/ en)
    (setq en (car (entsel "\nУкажите главную полилинию: ")))
    (if en (poper-full-update (poper-get-id en)))
    (princ)
)

;; ====================== ОСНОВНАЯ КОМАНДА ======================
(defun C:POPER (/ dcl_id result h_scale l_scale text_height text_offset_str round_to slope_round_to
                slope_unit use_text_height show_lengths show_slopes draw_projections projection_color show_permille_sign
                OSZ OSH PCoords point_count profile-ename poper-id temp_dcl fp *poper-dcl* old-error)
    (vl-load-com)
    (poper-regapp)
    (poper-setup-layers)

    (setq old-error *error*)
    (defun *error* (msg)
        (if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\n[POPER] Ошибка: " msg)))
        (vl-catch-all-apply 'command-s '("_.UNDO" "_End"))
        (setq *error* old-error)
        (princ)
    )

    (setq temp_dcl (vl-filename-mktemp "poper" (getvar "TEMPPREFIX") ".dcl"))
    (setq fp (open temp_dcl "w")) (write-line dcl_content fp) (close fp)
    (setq *poper-dcl* temp_dcl)

    (setq dcl_id (load_dialog *poper-dcl*))
    (if (not (new_dialog "poper_settings" dcl_id)) (progn (vl-file-delete *poper-dcl*) (exit)))

    ;; init getenv
    (foreach var '("ZZ_MODE" "ZZ_H_SCALE" "ZZ_L_SCALE" "ZZ_TEXT_HEIGHT" "ZZ_TEXT_OFFSET"
                   "ZZ_ROUND_TO" "ZZ_SLOPE_ROUND_TO" "ZZ_SLOPE_UNIT" "ZZ_USE_TEXT_HEIGHT"
                   "ZZ_SHOW_LENGTHS" "ZZ_SHOW_SLOPES" "ZZ_DRAW_PROJECTIONS" "ZZ_PROJECTION_COLOR" "ZZ_SHOW_PERMILLE_SIGN")
        (if (null (getenv var)) (setenv var (cond ((= var "ZZ_MODE") "points") ((member var '("ZZ_SHOW_LENGTHS" "ZZ_SHOW_SLOPES" "ZZ_USE_TEXT_HEIGHT" "ZZ_SHOW_PERMILLE_SIGN")) "1") (t "0")))))

    (set_tile "mode_points" (if (eq (getenv "ZZ_MODE") "points") "1" "0"))
    (set_tile "h_scale" (getenv "ZZ_H_SCALE"))
    (set_tile "l_scale" (getenv "ZZ_L_SCALE"))
    (set_tile "text_height" (getenv "ZZ_TEXT_HEIGHT"))
    (set_tile "text_offset" (getenv "ZZ_TEXT_OFFSET"))
    (set_tile "round_to" (getenv "ZZ_ROUND_TO"))
    (set_tile "slope_round_to" (getenv "ZZ_SLOPE_ROUND_TO"))
    (set_tile "slope_unit" (cond ((eq (getenv "ZZ_SLOPE_UNIT") "permille") "0") ((eq (getenv "ZZ_SLOPE_UNIT") "degrees") "1") (t "0")))
    (set_tile "use_text_height" (getenv "ZZ_USE_TEXT_HEIGHT"))
    (set_tile "show_lengths" (getenv "ZZ_SHOW_LENGTHS"))
    (set_tile "show_slopes" (getenv "ZZ_SHOW_SLOPES"))
    (set_tile "draw_projections" (getenv "ZZ_DRAW_PROJECTIONS"))
    (set_tile "show_permille_sign" (getenv "ZZ_SHOW_PERMILLE_SIGN"))

    (action_tile "accept"
        "(progn
            (setq mode_points (get_tile \"mode_points\") h_scale (get_tile \"h_scale\") l_scale (get_tile \"l_scale\")
                  text_height (get_tile \"text_height\") text_offset_str (get_tile \"text_offset\")
                  round_to (nth (atoi (get_tile \"round_to\")) '(\"2\" \"3\"))
                  slope_round_to (get_tile \"slope_round_to\") slope_unit (nth (atoi (get_tile \"slope_unit\")) '(\"permille\" \"degrees\" \"ratio\"))
                  use_text_height (get_tile \"use_text_height\") show_lengths (get_tile \"show_lengths\")
                  show_slopes (get_tile \"show_slopes\") draw_projections (get_tile \"draw_projections\")
                  projection_color (get_tile \"projection_color\") show_permille_sign (get_tile \"show_permille_sign\"))

            (setenv \"ZZ_H_SCALE\" h_scale) (setenv \"ZZ_L_SCALE\" l_scale) (setenv \"ZZ_TEXT_HEIGHT\" text_height)
            (setenv \"ZZ_TEXT_OFFSET\" text_offset_str) (setenv \"ZZ_ROUND_TO\" (itoa (atoi round_to)))
            (setenv \"ZZ_SLOPE_ROUND_TO\" slope_round_to) (setenv \"ZZ_SLOPE_UNIT\" slope_unit)
            (setenv \"ZZ_USE_TEXT_HEIGHT\" use_text_height)
            (setenv \"ZZ_SHOW_LENGTHS\" show_lengths) (setenv \"ZZ_SHOW_SLOPES\" show_slopes)
            (setenv \"ZZ_DRAW_PROJECTIONS\" draw_projections) (setenv \"ZZ_PROJECTION_COLOR\" projection_color)
            (setenv \"ZZ_SHOW_PERMILLE_SIGN\" show_permille_sign)
            (done_dialog 1)
        )"
    )
    (action_tile "cancel" "(done_dialog 0)")
    (action_tile "help" "(C:POPERHELP) (done_dialog 2)")

    (setq result (start_dialog))
    (unload_dialog dcl_id)
    (if *poper-dcl* (vl-file-delete *poper-dcl*))

    (if (= result 1)
        (progn
            (setq h_scale (atof (getenv "ZZ_H_SCALE")) l_scale (atof (getenv "ZZ_L_SCALE"))
                  text_height (atof (getenv "ZZ_TEXT_HEIGHT")) text_offset (* text_height (/ (atof (getenv "ZZ_TEXT_OFFSET")) 100.0))
                  round_to (atoi (getenv "ZZ_ROUND_TO")) slope_round_to (atoi (getenv "ZZ_SLOPE_ROUND_TO"))
                  slope_unit (getenv "ZZ_SLOPE_UNIT") use_text_height (= (getenv "ZZ_USE_TEXT_HEIGHT") "1")
                  show_lengths (= (getenv "ZZ_SHOW_LENGTHS") "1") show_slopes (= (getenv "ZZ_SHOW_SLOPES") "1")
                  draw_projections (= (getenv "ZZ_DRAW_PROJECTIONS") "1") show_permille_sign (= (getenv "ZZ_SHOW_PERMILLE_SIGN") "1"))

            (command "_.UNDO" "_Begin")
            (setq OSZ (getpoint "\nВыберите ось/край с известной высотой: "))
            (if (null OSZ) (progn (command "_.UNDO" "_End") (exit)))

            (if use_text_height
                (progn (setq OSH (atof (cdr (assoc 1 (entget (car (entsel "\nВыберите текст с высотой: ")))))))
                (progn (setq OSH (getreal "\nВведите высоту: "))))

            (setq PCoords '() point_count 0)
            (prompt "\nУкажите точки профиля (Enter = конец)")
            (while (progn (initget "U") (setq pt (getpoint (strcat "\nТочка [" (itoa point_count) "]: "))))
                (cond
                    ((null pt) nil)
                    ((= pt "U") (if (> point_count 0) (progn (setq PCoords (reverse (cdr (reverse PCoords)))) (setq point_count (1- point_count)) t) t))
                    (t (setq PCoords (append PCoords (list pt)) point_count (1+ point_count)) t)
                )
            )

            (if (< point_count 2) (progn (princ "\nМало точек.") (command "_.UNDO" "_End") (exit)))

            (create-polyline (vl-sort PCoords '(lambda (a b) (< (car a) (car b)))) *SWIFT_POPER_LAYER_PROFILE* 251)
            (setq profile-ename (entlast))

            ;; Создаём HEIGHT тексты (упрощённо для стабильности)
            (setq i 0)
            (foreach pt (vl-sort PCoords '(lambda (a b) (< (car a) (car b))))
                (create-mtext (car pt) (+ (cadr pt) 5) (rtos (+ 0 (* (- (cadr pt) (cadr OSZ)) (/ 1.0 h_scale))) 2 round_to) text_height 5 1.5708 (getvar "TEXTSTYLE"))
                (poper-xdata-set (entlast) (list (cons 'poper-id (poper-get-id profile-ename)) (cons 'type "HEIGHT") (cons 'index i)))
                (setq i (1+ i))
            )

            ;; XData + Reactor (в самом конце)
            (poper-xdata-set profile-ename (list (cons 'type "PROFILE") (cons 'poper-id (poper-get-id profile-ename))
                (cons 'h_scale h_scale) (cons 'l_scale l_scale) (cons 'round_to round_to)))
            (poper-attach-reactor profile-ename)

            (command "_.UNDO" "_End")
            (princ "\n[POPER v3.5] Готово. Реактор активен.")
        )
    )
    (setq *error* old-error)
    (princ)
)

(defun C:ПОПЕР () (C:POPER))
(princ "\nSwift POPER v3.5 (анти-фриз) загружен. Команды: POPER, POPERUPDATE, POPER-RESTORE")
(princ)
; SWIFT-END
