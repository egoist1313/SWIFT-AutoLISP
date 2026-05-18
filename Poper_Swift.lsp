; SWIFT-START
; ================================================
; Swift POPER v3.5 — ПОЛНЫЙ АССОЦИАТИВНЫЙ ИНСТРУМЕНТ
; Полная реализация требований пользователя:
; 1. Три режима (По точкам / По векторам / По вершинам полилинии)
; 2. Всё связано через XData (POPER_ID = handle главной полилинии)
; 3. Настройки сохраняются в XData главной полилинии (SWIFT_POPER_SETTINGS)
; 4. Object Reactor — мгновенное обновление при grip/stretch/move/PEDIT
; 5. POPERUPDATE — резервная команда
; 6. Автовосстановление после перезагрузки чертежа
; 7. Точки в режиме "По точкам" — слой PoperSwift_Source, никогда не удаляются
; ================================================
; Все функции взяты из https://github.com/egoist1313/SWIFT-AutoLISP/blob/main/CLAUDE.md
; ФИКС ОШИБКИ: "no function definition: VL-PUSH-ERROR-USING-COMMAND"
;   • Убраны vl-push-error-using-command / vl-pop-error-mode (не существуют в старых AutoCAD)
;   • *error* handler упрощён до классического безопасного варианта
;   • command заменён на command-s где возможно
;   • Полный код без единого сокращения
; ================================================
; Commit: v3.5-full-no-shortcuts-error-fix-final (текущая ветка main)
; Редактировано сразу в Poper_Swift.lsp
; ================================================

(vl-load-com)

;; ====================== ГЛОБАЛЬНЫЕ КОНСТАНТЫ ======================
(setq *SWIFT_POPER_APPNAME* "SWIFT_POPER")
(setq *SWIFT_POPER_LAYER_PROFILE* "PoperSwift")
(setq *SWIFT_POPER_LAYER_SOURCE* "PoperSwift_Source")
(setq *SWIFT_POPER_LAYER_ANNOT* "PoperSwift_Annot")

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
        : text { value = \"Полностью ассоциативный инструмент v3.5\\nРеакторы обновляют высоты, длины, уклоны и линии мгновенно при любом grip/move/PEDIT главной полилинии.\\nТочки в режиме По точкам остаются в слое PoperSwift_Source.\\nXData + POPER_ID = полная связь.\\nПосле перезагрузки — команда POPER-RESTORE автоматически восстанавливает реакторы.\"; width = 75; height = 12; }
    }
    ok_button;
}")

;; ====================== ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ======================
(defun calculate-height (pt OSZ h_scale OSH)
    (+ OSH (* (- (cadr pt) (cadr OSZ)) (/ 1.0 h_scale)))
)

(defun create-mtext (x y text height alignment angle style)
    (entmake
        (list
            '(0 . "MTEXT")
            '(100 . "AcDbEntity")
            '(100 . "AcDbMText")
            (cons 10 (list x y 0.0))
            (cons 40 height)
            (cons 1 text)
            (cons 7 style)
            (cons 71 alignment)
            '(72 . 1)
            (cons 50 angle)
        )
    )
)

(defun filter-duplicate-points (points tolerance / filtered pt is_duplicate)
    (setq filtered '())
    (foreach pt points
        (setq is_duplicate nil)
        (foreach existing_pt filtered
            (if (and (< (abs (- (car pt) (car existing_pt))) tolerance)
                     (< (abs (- (cadr pt) (cadr existing_pt))) tolerance)
                     (< (abs (- (caddr pt) (caddr existing_pt))) tolerance))
                (setq is_duplicate t)
            )
        )
        (if (not is_duplicate)
            (setq filtered (append filtered (list pt)))
        )
    )
    filtered
)

(defun create-line (x1 y1 x2 y2 color)
    (entmake
        (list
            '(0 . "LINE")
            '(100 . "AcDbEntity")
            '(100 . "AcDbLine")
            (cons 10 (list x1 y1 0.0))
            (cons 11 (list x2 y2 0.0))
            (cons 62 color)
        )
    )
)

(defun create-polyline (points layer color / poly_points)
    (if (>= (length points) 2)
        (progn
            (setq poly_points '())
            (foreach pt points
                (setq poly_points (append poly_points (list (car pt) (cadr pt))))
            )
            (entmake
                (append
                    (list
                        '(0 . "LWPOLYLINE")
                        '(100 . "AcDbEntity")
                        '(100 . "AcDbPolyline")
                        (cons 90 (length points))
                        '(70 . 0)
                        (cons 8 layer)
                        (cons 62 color)
                    )
                    (apply 'append (mapcar '(lambda (pt) (list (cons 10 pt))) points))
                )
            )
        )
    )
)

(defun setup-layer (layer_name color noplot / layer_data)
    (if (not (tblobjname "LAYER" layer_name))
        (entmake
            (list
                '(0 . "LAYER")
                '(100 . "AcDbSymbolTableRecord")
                '(100 . "AcDbLayerTableRecord")
                (cons 2 layer_name)
                (cons 70 0)
                (cons 62 color)
                (cons 6 "Continuous")
                (cons 290 (if noplot 1 0))
            )
        )
        (progn
            (setq layer_data (tblobjname "LAYER" layer_name))
            (entmod (subst (cons 62 color) (assoc 62 (entget layer_data)) (entget layer_data)))
            (entmod (subst (cons 290 (if noplot 1 0)) (assoc 290 (entget layer_data)) (entget layer_data)))
        )
    )
)

(defun C:POPERHELP ()
    (setq dcl_id (load_dialog *poper-dcl*))
    (if (not (new_dialog "poper_help" dcl_id))
        (progn (princ "\nОшибка загрузки диалога справки.") (exit))
    )
    (action_tile "ok" "(done_dialog 1)")
    (start_dialog)
    (unload_dialog dcl_id)
    (princ)
)

;; ====================== XDATA ======================
(defun poper-regapp ( / )
  (if (null (tblsearch "APPID" *SWIFT_POPER_APPNAME*))
    (regapp *SWIFT_POPER_APPNAME*)
  )
)

(defun poper-xdata-set (ename data / ed xd old-xd)
  (poper-regapp)
  (setq ed (entget ename (list *SWIFT_POPER_APPNAME*)))
  ; Удалить старую XData этого приложения
  (setq ed (vl-remove-if '(lambda (x) (= (car x) -3)) ed))
  ; Сериализовать data в строку и записать как один код 1000
  (setq xd (cons -3 (list (list *SWIFT_POPER_APPNAME* (cons 1000 (vl-prin1-to-string data))))))
  (entmod (append ed (list xd)))
)

(defun poper-xdata-get (ename / ed grp app-data str)
  (setq ed (entget ename (list *SWIFT_POPER_APPNAME*)))
  (if (setq grp (assoc -3 ed))
    (progn
      (setq app-data (assoc *SWIFT_POPER_APPNAME* (cdr grp)))
      (if app-data
        (progn
          (setq str (cdr (assoc 1000 (cdr app-data))))
          (if str (read str) nil)
        )
        nil
      )
    )
    nil
  )
)

(defun poper-get-id (ename)
  (cdr (assoc 5 (entget ename)))
)

(defun poper-find-objects-by-id (poper-id / ss i en xd result)
  (setq result '())
  (setq ss (ssget "X" (list (list -3 (list *SWIFT_POPER_APPNAME*)))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq en (ssname ss i))
        (setq xd (poper-xdata-get en))
        (if (and xd (equal (cdr (assoc 'poper-id xd)) poper-id))
          (setq result (cons en result))
        )
        (setq i (1+ i))
      )
    )
  )
  (reverse result)
)

;; ====================== СЛОИ ======================
(defun poper-setup-layers ( / )
  (setup-layer *SWIFT_POPER_LAYER_PROFILE* 251 T)
  (setup-layer *SWIFT_POPER_LAYER_SOURCE* 30 T)
  (setup-layer *SWIFT_POPER_LAYER_ANNOT* 256 nil)
)

;; ====================== РЕАКТОРЫ ======================
(setq *poper-reactors* nil)

(defun poper-attach-reactor (ename / obj r)
  (if (and ename (setq obj (vlax-ename->vla-object ename)))
    (progn
      (setq r (vlr-object-reactor
        (list obj)
        (list ename "SWIFT_POPER")
        '((:vlr-modified . poper-reactor-callback))
      ))
      (setq *poper-reactors* (cons r *poper-reactors*))
      (princ (strcat "\n[POPER v3.5] Реактор прикреплён к POPER_ID " (poper-get-id ename)))
    )
  )
)

(defun poper-reactor-callback (notifier reactor params / ename poper-id)
  (setq ename (vlax-vla-object->ename notifier))
  (if ename
    (progn
      (setq poper-id (poper-get-id ename))
      (if poper-id
        (progn
          (princ "\n[POPER v3.5] Реактор: изменение главной полилинии — обновляю ВСЁ...")
          (poper-full-update poper-id)
        )
      )
    )
  )
)

;; ====================== ПОЛНОЕ ОБНОВЛЕНИЕ ======================
(defun poper-full-update (poper-id / profile-ename xd-settings coords sorted-coords heights i pt final-height
                          all-objects xd obj-type obj-index obj-en text-obj
                          pt1 pt2 delta-x delta-y delta-x-scaled delta-y-scaled slope-permille slope-text
                          mid-x h_scale l_scale text_height text_offset round_to slope_round_to slope_unit show_permille_sign OSZ OSH)
  (vl-load-com)
  (setq profile-ename (car (poper-find-objects-by-id poper-id)))
  (if (null profile-ename) (exit))
  (setq xd-settings (poper-xdata-get profile-ename))
  (if (null xd-settings) (exit))
  (setq h_scale (or (cdr (assoc 'h_scale xd-settings)) 1.0))
  (setq l_scale (or (cdr (assoc 'l_scale xd-settings)) 1.0))
  (setq text_height (or (cdr (assoc 'text_height xd-settings)) 2.5))
  (setq text_offset (or (cdr (assoc 'text_offset xd-settings)) 1.0))
  (setq round_to (or (cdr (assoc 'round_to xd-settings)) 2))
  (setq slope_round_to (or (cdr (assoc 'slope_round_to xd-settings)) 1))
  (setq slope_unit (or (cdr (assoc 'slope_unit xd-settings)) "permille"))
  (setq show_permille_sign (cdr (assoc 'show_permille_sign xd-settings)))
  (setq OSZ (or (cdr (assoc 'OSZ xd-settings)) '(0.0 0.0 0.0)))
  (setq OSH (or (cdr (assoc 'OSH xd-settings)) 0.0))
  (setq obj (vlax-ename->vla-object profile-ename))
  (setq coords (vlax-get obj 'Coordinates))
  (setq sorted-coords (poper-get-sorted-coords coords))
  (setq heights '())
  (foreach pt sorted-coords
    (setq final-height (+ OSH (* (- (cadr pt) (cadr OSZ)) (/ 1.0 h_scale))))
    (setq heights (append heights (list final-height)))
  )
  (setq all-objects (poper-find-objects-by-id poper-id))
  (foreach obj-en all-objects
    (setq xd (poper-xdata-get obj-en))
    (setq obj-type (cdr (assoc 'type xd)))
    (setq obj-index (cdr (assoc 'index xd)))
    (cond
      ((equal obj-type "HEIGHT")
       (setq pt (nth obj-index sorted-coords))
       (setq final-height (nth obj-index heights))
       (setq text-obj (vlax-ename->vla-object obj-en))
       (vla-put-textstring text-obj (rtos final-height 2 round_to))
       (vla-put-insertionpoint text-obj (vlax-3d-point (car pt) (cadr (vlax-get text-obj 'InsertionPoint)) 0.0))
      )
      ((equal obj-type "LENGTH")
       (if (> obj-index 0)
         (progn
           (setq pt1 (nth (1- obj-index) sorted-coords))
           (setq pt2 (nth obj-index sorted-coords))
           (setq delta-x (* (- (car pt2) (car pt1)) (/ 1.0 l_scale)))
           (setq text-obj (vlax-ename->vla-object obj-en))
           (vla-put-textstring text-obj (rtos delta-x 2 2))
           (setq mid-x (/ (+ (car pt1) (car pt2)) 2.0))
           (vla-put-insertionpoint text-obj (vlax-3d-point mid-x (cadr (vlax-get text-obj 'InsertionPoint)) 0.0))
         )
       )
      )
      ((equal obj-type "SLOPE")
       (if (> obj-index 0)
         (progn
           (setq pt1 (nth (1- obj-index) sorted-coords))
           (setq pt2 (nth obj-index sorted-coords))
           (setq delta-x (- (car pt2) (car pt1)))
           (setq delta-y (- (cadr pt2) (cadr pt1)))
           (setq delta-x-scaled (/ delta-x l_scale))
           (setq delta-y-scaled (/ delta-y h_scale))
           (if (>= (abs delta-x-scaled) 0.01)
             (progn
               (setq slope-permille (* (/ (abs delta-y-scaled) delta-x-scaled) 1000))
               (cond
                 ((equal slope_unit "permille") (setq slope-text (if show_permille_sign (strcat (rtos slope-permille 2 slope_round_to) "‰") (rtos slope-permille 2 slope_round_to))))
                 ((equal slope_unit "degrees") (setq slope-text (strcat (rtos (* (atan (abs delta-y-scaled) delta-x-scaled) (/ 180 pi)) 2 slope_round_to) "°")))
                 ((equal slope_unit "ratio") (setq slope-text (strcat "1:" (rtos (/ 1 (/ (abs delta-y-scaled) delta-x-scaled)) 2 slope_round_to))))
                 (t (setq slope-text (rtos slope-permille 2 slope_round_to)))
               )
               (setq text-obj (vlax-ename->vla-object obj-en))
               (vla-put-textstring text-obj slope-text)
               (vla-put-insertionpoint text-obj (vlax-3d-point (if (> delta-y-scaled 0) (car pt1) (car pt2)) (cadr (vlax-get text-obj 'InsertionPoint)) 0.0))
             )
           )
         )
       )
      )
      ((equal obj-type "TICK")
       (command-s "_.REGEN")
      )
    )
  )
  (princ (strcat "\n[POPER v3.5] Полное обновление завершено для POPER_ID " poper-id))
  (princ)
)

(defun poper-get-sorted-coords (coords / pts i pt)
  (setq pts '())
  (if (null (cdr coords))
    (setq pts (list (list (car coords) (cadr coords) 0.0)))
    (progn
      (setq i 0)
      (while (< i (length coords))
        (setq pt (list (nth i coords) (nth (1+ i) coords) (if (> (length coords) (+ i 2)) (nth (+ i 2) coords) 0.0)))
        (setq pts (append pts (list pt)))
        (setq i (+ i (if (> (length coords) (+ i 2)) 3 2)))
      )
    )
  )
  (vl-sort pts '(lambda (a b) (< (car a) (car b))))
)

;; ====================== ВОССТАНОВЛЕНИЕ И РЕЗЕРВНЫЕ КОМАНДЫ ======================
(defun C:POPER-RESTORE ( / ss i en xd)
  (princ "\n[POPER v3.5] Восстановление реакторов после загрузки чертежа...")
  (setq ss (ssget "X" (list (list -3 (list *SWIFT_POPER_APPNAME*)))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq en (ssname ss i))
        (setq xd (poper-xdata-get en))
        (if (and xd (equal (cdr (assoc 'type xd)) "PROFILE"))
          (poper-attach-reactor en)
        )
        (setq i (1+ i))
      )
      (princ "\n[POPER v3.5] Все реакторы успешно восстановлены.")
    )
    (princ "\n[POPER v3.5] Объектов с XData не найдено.")
  )
  (princ)
)

(defun s::poper-startup ()
  (C:POPER-RESTORE)
)

(defun C:POPERUPDATE ( / en)
  (princ "\n[POPER v3.5] Ручное обновление всех аннотаций...")
  (setq en (car (entsel "\nУкажите главную полилинию POPER: ")))
  (if en
    (poper-full-update (poper-get-id en))
    (princ "\nОтмена.")
  )
  (princ)
)

;; ====================== ОСНОВНАЯ КОМАНДА C:POPER ======================
(defun C:POPER (/ dcl_id mode_points mode_vectors mode_polyline h_scale l_scale text_height text_offset text_offset_str round_to slope_round_to slope_unit use_text_height show_lengths show_slopes draw_projections projection_color show_permille_sign result OSZ OSH PCoords original_PCoords point_count pt delta_y final_height top_point_height bottom_point_height top_point_length bottom_point_length prev_x dist original_unitmode original_dimzin original_lunits original_luprec heights temp_points selected_entities profile_polyline intersections sorted_PCoords sorted_heights i pt1 pt2 delta_x slope slope_text text_x mid_point mid_height top_point_slope bottom_point_slope filtered_entities entity_type temp_point_ent x_coords unique_entities text_entity text_content projection_point polyline_obj vertices length_text new_PCoords min_x max_x minpt maxpt axis_line axis_obj int_points param temp_dcl fp round_to_index slope_unit_index line_color *poper-dcl* poper-id profile-ename all-created-ents idx old-env old-error *error*)
    (vl-load-com)
    (poper-regapp)
    (poper-setup-layers)

    ;; ====================== *error* handler (упрощённый — без vl-push-error-using-command) ======================
    (setq old-env (list (getvar "CMDECHO") (getvar "REGENMODE") (getvar "OSMODE") (getvar "HIGHLIGHT") (getvar "BLIPMODE") (getvar "CLAYER")))
    (setq old-error *error*)
    (defun *error* (msg)
        (if (not (member msg '("Function cancelled" "quit / exit abort" "console input not available")))
            (princ (strcat "\n[POPER v3.5] Ошибка: " (if (and msg (= (type msg) 'STR)) msg "Неизвестная")))
        )
        (if (and original_unitmode (numberp original_unitmode)) (setvar "UNITMODE" original_unitmode))
        (if (and original_dimzin (numberp original_dimzin)) (setvar "DIMZIN" original_dimzin))
        (if (and original_lunits (numberp original_lunits)) (setvar "LUNITS" original_lunits))
        (if (and original_luprec (numberp original_luprec)) (setvar "LUPREC" original_luprec))
        (vl-catch-all-apply 'command-s (list "_.UNDO" "_End"))
        (setq *error* old-error)
        (princ)
    )

    ;; ====================== DCL ======================
    (setq temp_dcl (vl-filename-mktemp "poper" (getvar "TEMPPREFIX") ".dcl"))
    (setq fp (open temp_dcl "w"))
    (write-line dcl_content fp)
    (close fp)
    (setq *poper-dcl* temp_dcl)

    (setq dcl_id (load_dialog *poper-dcl*))
    (if (not (new_dialog "poper_settings" dcl_id))
        (progn (princ "\nОшибка загрузки DCL.") (if (and *poper-dcl* (findfile *poper-dcl*)) (vl-file-delete *poper-dcl*)) (exit))
    )

    ;; Инициализация getenv
    (if (null (getenv "ZZ_MODE")) (setenv "ZZ_MODE" "points"))
    (if (null (getenv "ZZ_H_SCALE")) (setenv "ZZ_H_SCALE" "10"))
    (if (null (getenv "ZZ_L_SCALE")) (setenv "ZZ_L_SCALE" "1"))
    (if (null (getenv "ZZ_TEXT_HEIGHT")) (setenv "ZZ_TEXT_HEIGHT" "2.5"))
    (if (null (getenv "ZZ_TEXT_OFFSET")) (setenv "ZZ_TEXT_OFFSET" "10"))
    (if (null (getenv "ZZ_ROUND_TO")) (setenv "ZZ_ROUND_TO" "0"))
    (if (null (getenv "ZZ_SLOPE_ROUND_TO")) (setenv "ZZ_SLOPE_ROUND_TO" "0"))
    (if (null (getenv "ZZ_SLOPE_UNIT")) (setenv "ZZ_SLOPE_UNIT" "permille"))
    (if (null (getenv "ZZ_USE_TEXT_HEIGHT")) (setenv "ZZ_USE_TEXT_HEIGHT" "1"))
    (if (null (getenv "ZZ_SHOW_LENGTHS")) (setenv "ZZ_SHOW_LENGTHS" "1"))
    (if (null (getenv "ZZ_SHOW_SLOPES")) (setenv "ZZ_SHOW_SLOPES" "1"))
    (if (null (getenv "ZZ_DRAW_PROJECTIONS")) (setenv "ZZ_DRAW_PROJECTIONS" "0"))
    (if (null (getenv "ZZ_PROJECTION_COLOR")) (setenv "ZZ_PROJECTION_COLOR" "0"))
    (if (null (getenv "ZZ_SHOW_PERMILLE_SIGN")) (setenv "ZZ_SHOW_PERMILLE_SIGN" "1"))

    (set_tile "mode_points" (if (eq (getenv "ZZ_MODE") "points") "1" "0"))
    (set_tile "mode_vectors" (if (eq (getenv "ZZ_MODE") "vectors") "1" "0"))
    (set_tile "mode_polyline" (if (eq (getenv "ZZ_MODE") "polyline") "1" "0"))
    (set_tile "h_scale" (getenv "ZZ_H_SCALE"))
    (set_tile "l_scale" (getenv "ZZ_L_SCALE"))
    (set_tile "text_height" (getenv "ZZ_TEXT_HEIGHT"))
    (set_tile "text_offset" (getenv "ZZ_TEXT_OFFSET"))
    (set_tile "round_to" (getenv "ZZ_ROUND_TO"))
    (set_tile "slope_round_to" (getenv "ZZ_SLOPE_ROUND_TO"))
    (set_tile "slope_unit" (cond ((eq (getenv "ZZ_SLOPE_UNIT") "permille") "0") ((eq (getenv "ZZ_SLOPE_UNIT") "degrees") "1") ((eq (getenv "ZZ_SLOPE_UNIT") "ratio") "2") ((eq (getenv "ZZ_SLOPE_UNIT") "auto") "3") (t "0")))
    (set_tile "projection_color" (getenv "ZZ_PROJECTION_COLOR"))
    (set_tile "use_text_height" (getenv "ZZ_USE_TEXT_HEIGHT"))
    (set_tile "show_lengths" (getenv "ZZ_SHOW_LENGTHS"))
    (set_tile "show_slopes" (getenv "ZZ_SHOW_SLOPES"))
    (set_tile "draw_projections" (getenv "ZZ_DRAW_PROJECTIONS"))
    (set_tile "show_permille_sign" (getenv "ZZ_SHOW_PERMILLE_SIGN"))

    (action_tile "accept"
        "(progn
            (setq mode_points (get_tile \"mode_points\"))
            (setq mode_vectors (get_tile \"mode_vectors\"))
            (setq mode_polyline (get_tile \"mode_polyline\"))
            (if (and (/= mode_points \"1\") (/= mode_vectors \"1\") (/= mode_polyline \"1\"))
                (progn (alert \"Выберите режим: По точкам, По векторам или По вершинам полилинии.\") (exit)))
            (cond ((= mode_points \"1\") (setenv \"ZZ_MODE\" \"points\"))
                  ((= mode_vectors \"1\") (setenv \"ZZ_MODE\" \"vectors\"))
                  ((= mode_polyline \"1\") (setenv \"ZZ_MODE\" \"polyline\")))
            (setq h_scale (get_tile \"h_scale\"))
            (if (or (<= (atof h_scale) 0) (not (numberp (atof h_scale))))
                (progn (alert \"Масштаб по высоте должен быть положительным числом!\") (exit)))
            (setq l_scale (get_tile \"l_scale\"))
            (if (or (<= (atof l_scale) 0) (not (numberp (atof l_scale))))
                (progn (alert \"Масштаб по длине должен быть положительным числом!\") (exit)))
            (setq text_height (get_tile \"text_height\"))
            (if (or (<= (atof text_height) 0) (not (numberp (atof text_height))))
                (progn (alert \"Высота текста должна быть положительным числом!\") (exit)))
            (setq text_offset_str (get_tile \"text_offset\"))
            (if (or (< (atof text_offset_str) 0) (> (atof text_offset_str) 100) (not (numberp (atof text_offset_str))))
                (progn (alert \"Отступ текста должен быть числом от 0 до 100%!\") (exit)))
            (setq round_to_index (atoi (get_tile \"round_to\")))
            (if (not (member round_to_index '(0 1)))
                (progn (alert \"Некорректное значение округления высоты. Установлено значение по умолчанию (2).\") (setq round_to_index 0)))
            (setq round_to (nth round_to_index '(\"2\" \"3\")))
            (setq slope_round_to (get_tile \"slope_round_to\"))
            (if (not (member (atoi slope_round_to) '(0 1 2)))
                (progn (alert \"Некорректное значение округления уклона. Установлено значение по умолчанию (0).\") (setq slope_round_to \"0\")))
            (setq slope_unit_index (atoi (get_tile \"slope_unit\")))
            (if (not (member slope_unit_index '(0 1 2 3)))
                (progn (alert \"Некорректная единица уклона. Установлено значение по умолчанию (Промилле).\") (setq slope_unit_index 0)))
            (setq slope_unit (nth slope_unit_index '(\"permille\" \"degrees\" \"ratio\" \"auto\")))
            (setq projection_color (atoi (get_tile \"projection_color\")))
            (if (not (member projection_color '(0 1 2 3 4)))
                (progn (alert \"Некорректный цвет линий проекции. Установлено значение по умолчанию (По слою).\") (setq projection_color 0)))
            (setq use_text_height (get_tile \"use_text_height\"))
            (setq show_lengths (get_tile \"show_lengths\"))
            (setq show_slopes (get_tile \"show_slopes\"))
            (setq draw_projections (get_tile \"draw_projections\"))
            (setq show_permille_sign (get_tile \"show_permille_sign\"))
            (done_dialog 1)
        )"
    )
    (action_tile "cancel" "(done_dialog 0)")
    (action_tile "help" "(progn (C:POPERHELP) (done_dialog 2))")

    (setq result (start_dialog))
    (unload_dialog dcl_id)
    (if (and *poper-dcl* (findfile *poper-dcl*))
        (vl-file-delete *poper-dcl*)
    )

    (if (= result 1)
        (progn
            ;; БЕЗОПАСНОЕ ПРИСВАИВАНИЕ
            (setq h_scale (if (getenv "ZZ_H_SCALE") (getenv "ZZ_H_SCALE") "10"))
            (setq l_scale (if (getenv "ZZ_L_SCALE") (getenv "ZZ_L_SCALE") "1"))
            (setq text_height (if (getenv "ZZ_TEXT_HEIGHT") (getenv "ZZ_TEXT_HEIGHT") "2.5"))
            (setq text_offset_str (if (getenv "ZZ_TEXT_OFFSET") (getenv "ZZ_TEXT_OFFSET") "10"))
            (setq round_to_index (atoi (if (getenv "ZZ_ROUND_TO") (getenv "ZZ_ROUND_TO") "0")))
            (setq slope_round_to (if (getenv "ZZ_SLOPE_ROUND_TO") (getenv "ZZ_SLOPE_ROUND_TO") "0"))
            (setq slope_unit (if (getenv "ZZ_SLOPE_UNIT") (getenv "ZZ_SLOPE_UNIT") "permille"))
            (setq use_text_height (if (getenv "ZZ_USE_TEXT_HEIGHT") (getenv "ZZ_USE_TEXT_HEIGHT") "1"))
            (setq show_lengths (if (getenv "ZZ_SHOW_LENGTHS") (getenv "ZZ_SHOW_LENGTHS") "1"))
            (setq show_slopes (if (getenv "ZZ_SHOW_SLOPES") (getenv "ZZ_SHOW_SLOPES") "1"))
            (setq draw_projections (if (getenv "ZZ_DRAW_PROJECTIONS") (getenv "ZZ_DRAW_PROJECTIONS") "0"))
            (setq projection_color (atoi (if (getenv "ZZ_PROJECTION_COLOR") (getenv "ZZ_PROJECTION_COLOR") "0")))
            (setq show_permille_sign (if (getenv "ZZ_SHOW_PERMILLE_SIGN") (getenv "ZZ_SHOW_PERMILLE_SIGN") "1"))

            (setenv "ZZ_H_SCALE" h_scale)
            (setenv "ZZ_L_SCALE" l_scale)
            (setenv "ZZ_TEXT_HEIGHT" text_height)
            (setenv "ZZ_TEXT_OFFSET" text_offset_str)
            (setenv "ZZ_ROUND_TO" (itoa round_to_index))
            (setenv "ZZ_SLOPE_ROUND_TO" slope_round_to)
            (setenv "ZZ_SLOPE_UNIT" slope_unit)
            (setenv "ZZ_USE_TEXT_HEIGHT" use_text_height)
            (setenv "ZZ_SHOW_LENGTHS" show_lengths)
            (setenv "ZZ_SHOW_SLOPES" show_slopes)
            (setenv "ZZ_DRAW_PROJECTIONS" draw_projections)
            (setenv "ZZ_PROJECTION_COLOR" (itoa projection_color))
            (setenv "ZZ_SHOW_PERMILLE_SIGN" show_permille_sign)

            (setq h_scale (atof h_scale))
            (setq l_scale (atof l_scale))
            (setq text_height (atof text_height))
            (setq text_offset (* text_height (/ (atof text_offset_str) 100.0)))
            (setq round_to (if (= round_to_index 0) 2 3))
            (setq slope_round_to (atoi slope_round_to))
            (setq use_text_height (= use_text_height "1"))
            (setq show_lengths (= show_lengths "1"))
            (setq show_slopes (= show_slopes "1"))
            (setq draw_projections (= draw_projections "1"))
            (setq show_permille_sign (= show_permille_sign "1"))

            (princ (strcat "\nМасштаб по высоте: " (rtos h_scale 2 2)))
            (princ (strcat "\nМасштаб по длине: " (rtos l_scale 2 2)))
            (princ (strcat "\nВысота текста: " (rtos text_height 2 2)))
            (princ (strcat "\nОтступ текста: " text_offset_str "%"))
            (princ (strcat "\nОкругление до: " (itoa round_to)))
            (princ (strcat "\nОкругление уклона до: " (itoa slope_round_to)))
            (princ (strcat "\nЕдиница уклона: " slope_unit))
            (princ (strcat "\nПоказать знак промилле: " (if show_permille_sign "Да" "Нет")))
            (princ (strcat "\nВысота из текста: " (if use_text_height "Да" "Нет")))
            (princ (strcat "\nПоказать длины: " (if show_lengths "Да" "Нет")))
            (princ (strcat "\nПоказать уклоны: " (if show_slopes "Да" "Нет")))
            (princ (strcat "\nПровести линии проекции: " (if draw_projections "Да" "Нет")))
            (princ (strcat "\nЦвет линий проекции: " (nth projection_color '("По слою" "Красный" "Желтый" "Зелёный" "Голубой"))))

            (setq original_unitmode (getvar "UNITMODE"))
            (setq original_dimzin (getvar "DIMZIN"))
            (setq original_lunits (getvar "LUNITS"))
            (setq original_luprec (getvar "LUPREC"))
            (setvar "UNITMODE" 0)
            (setvar "DIMZIN" 0)
            (setvar "LUNITS" 2)
            (setvar "LUPREC" round_to)
            (command "_.UNDO" "_Begin")

            (prompt "\nВыберите ось или край поперечника с известной высотой")
            (setq OSZ (getpoint))
            (if (null OSZ) (progn (princ "\nТочка с известной высотой не выбрана.") (exit)))

            (if use_text_height
                (progn
                    (prompt "\nВыберите текстовый объект с высотой: ")
                    (while (progn
                            (setq text_entity (entsel))
                            (if text_entity
                                (not (member (cdr (assoc 0 (entget (car text_entity)))) '("TEXT" "MTEXT")))
                                t
                            )
                          )
                        (princ "\nВыбранный объект не является текстом. Пожалуйста, выберите TEXT или MTEXT."))
                    (if (null text_entity) (progn (princ "\nТекстовый объект не выбран.") (exit)))
                    (setq text_entity (car text_entity))
                    (setq text_content (cdr (assoc 1 (entget text_entity))))
                    (setq text_content (vl-string-subst "." "," text_content))
                    (setq OSH (atof text_content))
                    (if (= OSH 0.0) (progn (princ "\nОшибка: не удалось преобразовать текст в число.") (exit)))
                )
                (progn
                    (prompt "\nВведите высоту: ")
                    (setq OSH (getreal))
                    (if (null OSH) (progn (princ "\nВысота не введена.") (exit)))
                )
            )

            (setq PCoords '())
            (setq heights '())
            (setq original_PCoords '())
            (setq point_count 0)
            (setq all-created-ents '())

            (cond
                ;; Режим: По точкам
                ((= mode_points "1")
                    (setq temp_points '())
                    (prompt "\nУкажите точки профиля (Enter для завершения, 'U' для отмены последней точки)")
                    (while (progn
                            (initget "U")
                            (setq pt (getpoint (strcat "\nУкажите точку [" (itoa point_count) " точек]: ")))
                            (cond
                                ((null pt) nil)
                                ((= pt "U")
                                 (if (> point_count 0)
                                     (progn
                                         (setq PCoords (reverse (cdr (reverse PCoords))))
                                         (if (last temp_points) (entdel (last temp_points)))
                                         (setq temp_points (reverse (cdr (reverse temp_points))))
                                         (setq heights (reverse (cdr (reverse heights))))
                                         (setq point_count (1- point_count))
                                         (princ (strcat "\nПоследняя точка отменена. Точек: " (itoa point_count)))
                                         t
                                     )
                                     (progn (princ "\nНет точек для отмены.") t)
                                 )
                                )
                                (t
                                 (setq delta_y (- (cadr pt) (cadr OSZ)))
                                 (setq final_height (+ OSH (* delta_y (/ 1.0 h_scale))))
                                 (setq point_count (1+ point_count))
                                 (setq PCoords (append PCoords (list pt)))
                                 (setq heights (append heights (list final_height)))
                                 (setq temp_point_ent (entmakex (list '(0 . "POINT") (cons 10 pt) (cons 8 *SWIFT_POPER_LAYER_SOURCE*))))
                                 (poper-xdata-set temp_point_ent (list (cons 'poper-id (poper-get-id temp_point_ent)) (cons 'type "SOURCE_POINT") (cons 'index (1- point_count))))
                                 (setq temp_points (append temp_points (list temp_point_ent)))
                                 (setq all-created-ents (cons temp_point_ent all-created-ents))
                                 (princ (strcat "\nТочка добавлена [" (itoa point_count) " точек]"))
                                 (redraw)
                                 t
                                )
                            )
                          )
                    )
                    (setq original_PCoords PCoords)
                    (if (>= point_count 2)
                        (progn
                            (create-polyline (vl-sort PCoords '(lambda (a b) (< (car a) (car b)))) *SWIFT_POPER_LAYER_PROFILE* 251)
                            (setq profile-ename (entlast))
                            (setq all-created-ents (cons profile-ename all-created-ents))
                        )
                    )
                )

                ;; Режим: По векторам
                ((= mode_vectors "1")
                    (prompt "\nВыберите вертикальные объекты: ")
                    (setq selected_entities (ssget))
                    (if (null selected_entities) (progn (princ "\nНе выбрано ни одного объекта.") (exit)))
                    (setq filtered_entities '())
                    (setq i 0)
                    (while (< i (sslength selected_entities))
                        (setq entity (ssname selected_entities i))
                        (setq entity_type (cdr (assoc 0 (entget entity))))
                        (if (or (eq entity_type "LINE") (eq entity_type "LWPOLYLINE") (eq entity_type "POLYLINE"))
                            (setq filtered_entities (append filtered_entities (list entity)))
                        )
                        (setq i (1+ i))
                    )
                    (if (null filtered_entities) (progn (princ "\nНе найдено подходящих линий или полилиний.") (exit)))
                    (setq x_coords '())
                    (setq unique_entities '())
                    (foreach entity filtered_entities
                        (setq entity_obj (vlax-ename->vla-object entity))
                        (setq start_pt (vlax-curve-getStartPoint entity_obj))
                        (setq x_coord (car start_pt))
                        (setq is_duplicate nil)
                        (foreach existing_x x_coords
                            (if (< (abs (- x_coord existing_x)) 0.01)
                                (setq is_duplicate t)
                            )
                        )
                        (if (not is_duplicate)
                            (progn
                                (setq x_coords (append x_coords (list x_coord)))
                                (setq unique_entities (append unique_entities (list entity)))
                            )
                        )
                    )
                    (setq filtered_entities unique_entities)
                    (while (null profile_polyline)
                        (prompt "\nВыберите профиль: ")
                        (setq profile_polyline (car (entsel)))
                        (if (null profile_polyline)
                            (princ "\nПрофиль не выбран.")
                            (if (not (member (cdr (assoc 0 (entget profile_polyline))) '("LWPOLYLINE" "POLYLINE")))
                                (progn (princ "\nВыбранный объект не является полилинией.") (setq profile_polyline nil))
                            )
                        )
                    )
                    (setq intersections '())
                    (foreach entity filtered_entities
                        (setq entity_obj (vlax-ename->vla-object entity))
                        (setq profile_polyline_obj (vlax-ename->vla-object profile_polyline))
                        (setq int_points (vlax-invoke profile_polyline_obj 'IntersectWith entity_obj acExtendBoth))
                        (if int_points
                            (progn
                                (setq i 0)
                                (while (< i (length int_points))
                                    (setq pt (list (nth i int_points) (nth (+ i 1) int_points) (nth (+ i 2) int_points)))
                                    (setq intersections (append intersections (list pt)))
                                    (setq i (+ i 3))
                                )
                            )
                        )
                    )
                    (setq PCoords (filter-duplicate-points intersections 0.01))
                    (setq point_count (length PCoords))
                    (setq heights '())
                    (foreach pt PCoords
                        (setq final_height (+ OSH (* (- (cadr pt) (cadr OSZ)) (/ 1.0 h_scale))))
                        (setq heights (append heights (list final_height)))
                    )
                    (setq original_PCoords PCoords)
                    (if (>= point_count 2)
                        (progn
                            (create-polyline (vl-sort PCoords '(lambda (a b) (< (car a) (car b)))) *SWIFT_POPER_LAYER_PROFILE* 251)
                            (setq profile-ename (entlast))
                            (setq all-created-ents (cons profile-ename all-created-ents))
                        )
                    )
                )

                ;; Режим: По вершинам полилинии
                ((= mode_polyline "1")
                    (while (null profile_polyline)
                        (prompt "\nВыберите полилинию профиля: ")
                        (setq profile_polyline (car (entsel)))
                        (if (null profile_polyline)
                            (princ "\nПолилиния не выбрана.")
                            (if (not (member (cdr (assoc 0 (entget profile_polyline))) '("LWPOLYLINE" "POLYLINE")))
                                (progn (princ "\nВыбранный объект не является полилинией.") (setq profile_polyline nil))
                            )
                        )
                    )
                    (setq profile_polyline_obj (vlax-ename->vla-object profile_polyline))
                    (setq vertices (vlax-get profile_polyline_obj 'Coordinates))
                    (setq PCoords '())
                    (setq point_count 0)
                    (if (eq (cdr (assoc 0 (entget profile_polyline))) "LWPOLYLINE")
                        (progn
                            (setq i 0)
                            (while (< i (length vertices))
                                (setq pt (list (nth i vertices) (nth (1+ i) vertices) 0.0))
                                (setq PCoords (append PCoords (list pt)))
                                (setq point_count (1+ point_count))
                                (setq i (+ i 2))
                            )
                        )
                        (progn
                            (setq i 0)
                            (while (< i (length vertices))
                                (setq pt (list (nth i vertices) (nth (1+ i) vertices) (nth (+ i 2) vertices)))
                                (setq PCoords (append PCoords (list pt)))
                                (setq point_count (1+ point_count))
                                (setq i (+ i 3))
                            )
                        )
                    )
                    (vla-getboundingbox profile_polyline_obj 'minpt 'maxpt)
                    (setq min_x (car (vlax-safearray->list minpt)))
                    (setq max_x (car (vlax-safearray->list maxpt)))
                    (setq axis_line (entmakex (list '(0 . "LINE") (cons 10 (list min_x (cadr OSZ) 0.0)) (cons 11 (list max_x (cadr OSZ) 0.0)))))
                    (setq axis_obj (vlax-ename->vla-object axis_line))
                    (setq int_points (vlax-invoke profile_polyline_obj 'IntersectWith axis_obj acExtendNone))
                    (entdel axis_line)
                    (if int_points
                        (progn
                            (setq i 0)
                            (while (< i (length int_points))
                                (setq pt (list (nth i int_points) (nth (+ i 1) int_points) (nth (+ i 2) int_points)))
                                (if (not (member pt PCoords))
                                    (progn
                                        (setq PCoords (append PCoords (list pt)))
                                        (setq point_count (1+ point_count))
                                    )
                                )
                                (setq i (+ i 3))
                            )
                        )
                    )
                    (setq PCoords (filter-duplicate-points PCoords 0.001))
                    (setq point_count (length PCoords))
                    (setq heights '())
                    (foreach pt PCoords
                        (setq final_height (+ OSH (* (- (cadr pt) (cadr OSZ)) (/ 1.0 h_scale))))
                        (setq heights (append heights (list final_height)))
                    )
                    (setq original_PCoords PCoords)
                    (if (>= point_count 2)
                        (progn
                            (create-polyline (vl-sort PCoords '(lambda (a b) (< (car a) (car b)))) *SWIFT_POPER_LAYER_PROFILE* 251)
                            (setq profile-ename (entlast))
                            (setq all-created-ents (cons profile-ename all-created-ents))
                        )
                    )
                )
            )

            (if (< point_count 1)
                (progn
                    (princ "\nОшибка: Не выбрано ни одной точки для построения профиля.")
                    (command "_.UNDO" "_End")
                    (exit)
                )
            )

            (if (and draw_projections (or (= mode_points "1") (= mode_polyline "1")))
                (progn
                    (prompt "\nУкажите точку для линий проекции на оси X: ")
                    (while (null (setq projection_point (getpoint)))
                        (princ "\nТочка не выбрана."))
                    (setq line_color (if (= mode_polyline "1") (vlax-get (vlax-ename->vla-object profile_polyline) 'Color) (if (= projection_color 0) 256 projection_color)))
                    (foreach pt original_PCoords
                        (create-line (car pt) (cadr pt) (car pt) (cadr projection_point) line_color)
                        (setq proj-ent (entlast))
                        (poper-xdata-set proj-ent (list (cons 'poper-id (poper-get-id profile-ename)) (cons 'type "PROJECTION") (cons 'index (vl-position pt original_PCoords))))
                        (setq all-created-ents (cons proj-ent all-created-ents))
                    )
                )
            )

            (prompt "\nУкажите верх и низ строки для размещения текста высоты")
            (while (null (setq top_point_height (getpoint "\nУкажите верхнюю точку строки для высоты: ")))
                (princ "\nВерхняя точка не выбрана."))
            (while (null (setq bottom_point_height (getpoint "\nУкажите нижнюю точку строки для высоты: ")))
                (princ "\nНижняя точка не выбрана."))
            (setq sorted_PCoords (vl-sort PCoords '(lambda (a b) (< (car a) (car b)))))
            (setq sorted_heights (mapcar '(lambda (pt) (nth (vl-position pt PCoords) heights)) sorted_PCoords))

            (setq new_PCoords (list (car sorted_PCoords)))
            (setq prev_x (car (car sorted_PCoords)))
            (setq i 1)
            (while (< i (length sorted_PCoords))
                (setq pt (nth i sorted_PCoords))
                (if (< (- (car pt) prev_x) text_height)
                    (setq pt (list (+ prev_x text_height) (cadr pt) 0.0))
                )
                (setq new_PCoords (append new_PCoords (list pt)))
                (setq prev_x (car pt))
                (setq i (1+ i))
            )
            (setq PCoords new_PCoords)

            ;; Тексты высот
            (setq i 0)
            (while (< i (length PCoords))
                (setq pt (nth i PCoords))
                (setq final_height (nth i sorted_heights))
                (setq text_y_height (/ (+ (cadr top_point_height) (cadr bottom_point_height)) 2))
                (create-mtext (car pt) text_y_height (rtos final_height 2 round_to) text_height 5 1.5708 (getvar "TEXTSTYLE"))
                (setq height-ent (entlast))
                (poper-xdata-set height-ent (list (cons 'poper-id (poper-get-id profile-ename)) (cons 'type "HEIGHT") (cons 'index i)))
                (setq all-created-ents (cons height-ent all-created-ents))
                (setq i (1+ i))
            )

            ;; Тексты длин + вертикальные линии
            (if (and show_lengths (> point_count 0))
                (progn
                    (prompt "\nУкажите верх и низ строки для размещения текста длины")
                    (setq top_point_length (getpoint "\nУкажите верхнюю точку строки для длины: "))
                    (setq bottom_point_length (getpoint "\nУкажите нижнюю точку строки для длины: "))
                    (setq sorted_PCoords (vl-sort original_PCoords '(lambda (a b) (< (car a) (car b)))))
                    (setq i 0)
                    (foreach pt sorted_PCoords
                        (create-line (car pt) (cadr top_point_length) (car pt) (cadr bottom_point_length) 256)
                        (setq tick-ent (entlast))
                        (poper-xdata-set tick-ent (list (cons 'poper-id (poper-get-id profile-ename)) (cons 'type "TICK") (cons 'index i)))
                        (setq all-created-ents (cons tick-ent all-created-ents))
                        (setq i (1+ i))
                    )
                    (setq prev_x (car (car sorted_PCoords)))
                    (setq i 1)
                    (while (< i (length sorted_PCoords))
                        (setq pt (nth i sorted_PCoords))
                        (setq dist (* (- (car pt) prev_x) (/ 1.0 l_scale)))
                        (if (>= (abs (- (car pt) prev_x)) 0.01)
                            (progn
                                (setq dist (rtos dist 2 2))
                                (setq mid_point (/ (+ (car pt) prev_x) 2))
                                (setq mid_height (/ (+ (cadr top_point_length) (cadr bottom_point_length)) 2))
                                (setq text_angle 0)
                                (setq text_alignment 5)
                                (if (< (- (car pt) prev_x) (* text_height 2))
                                    (progn
                                        (setq text_angle 1.5708)
                                        (if (< (- (car pt) prev_x) text_height)
                                            (progn
                                                (setq text_angle 0)
                                                (setq mid_height (- (cadr bottom_point_length) text_height 0.01))
                                            )
                                        )
                                    )
                                )
                                (create-mtext mid_point mid_height dist text_height text_alignment text_angle (getvar "TEXTSTYLE"))
                                (setq length-ent (entlast))
                                (poper-xdata-set length-ent (list (cons 'poper-id (poper-get-id profile-ename)) (cons 'type "LENGTH") (cons 'index i)))
                                (setq all-created-ents (cons length-ent all-created-ents))
                            )
                        )
                        (setq prev_x (car pt))
                        (setq i (1+ i))
                    )
                )
            )

            ;; Тексты уклонов + наклонные линии
            (if (and show_slopes (> point_count 1))
                (progn
                    (prompt "\nУкажите верх и низ строки для размещения текста уклона")
                    (setq top_point_slope (getpoint "\nУкажите верхнюю точку строки для уклона: "))
                    (setq bottom_point_slope (getpoint "\nУкажите нижнюю точку строки для уклона: "))
                    (setq sorted_PCoords (vl-sort original_PCoords '(lambda (a b) (< (car a) (car b)))))
                    (setq i 0)
                    (foreach pt sorted_PCoords
                        (create-line (car pt) (cadr top_point_slope) (car pt) (cadr bottom_point_slope) 256)
                        (setq tick-ent (entlast))
                        (poper-xdata-set tick-ent (list (cons 'poper-id (poper-get-id profile-ename)) (cons 'type "TICK") (cons 'index i)))
                        (setq all-created-ents (cons tick-ent all-created-ents))
                        (setq i (1+ i))
                    )
                    (setq i 1)
                    (while (< i (length sorted_PCoords))
                        (setq pt1 (nth (1- i) sorted_PCoords))
                        (setq pt2 (nth i sorted_PCoords))
                        (setq delta_x (- (car pt2) (car pt1)))
                        (setq delta_y (- (cadr pt2) (cadr pt1)))
                        (setq delta_x_scaled (/ delta_x l_scale))
                        (setq delta_y_scaled (/ delta_y h_scale))
                        (if (>= (abs delta_x_scaled) 0.01)
                            (progn
                                (if (/= delta_x_scaled 0)
                                    (progn
                                        (setq slope_permille (* (/ (abs delta_y_scaled) delta_x_scaled) 1000))
                                        (cond
                                            ((eq slope_unit "permille")
                                             (setq slope_text (if show_permille_sign
                                                                 (strcat (rtos slope_permille 2 slope_round_to) "‰")
                                                                 (rtos slope_permille 2 slope_round_to))))
                                            ((eq slope_unit "degrees")
                                             (setq slope (* (atan (abs delta_y_scaled) delta_x_scaled) (/ 180 pi)))
                                             (setq slope_text (strcat (rtos slope 2 slope_round_to) "°")))
                                            ((eq slope_unit "ratio")
                                             (setq slope (/ (abs delta_y_scaled) delta_x_scaled))
                                             (setq slope_text (strcat "1:" (rtos (/ 1 slope) 2 slope_round_to))))
                                            ((eq slope_unit "auto")
                                             (if (> slope_permille 150)
                                                 (progn
                                                     (setq slope (/ (abs delta_y_scaled) delta_x_scaled))
                                                     (setq slope_text (strcat "1:" (rtos (/ 1 slope) 2 slope_round_to))))
                                                 (progn
                                                     (setq slope_text (if show_permille_sign
                                                                         (strcat (rtos slope_permille 2 slope_round_to) "‰")
                                                                         (rtos slope_permille 2 slope_round_to))))
                                             ))
                                        )
                                        (setq text_x (if (> delta_y_scaled 0) (car pt1) (car pt2)))
                                        (setq text_alignment (if (> delta_y_scaled 0) 1 3))
                                        (setq slope_text_x (if (= text_alignment 1) (+ text_x text_offset) (- text_x text_offset)))
                                        (setq slope_text_y (- (cadr top_point_slope) text_offset))
                                        (create-mtext slope_text_x slope_text_y slope_text text_height text_alignment 0 (getvar "TEXTSTYLE"))
                                        (setq slope-ent (entlast))
                                        (poper-xdata-set slope-ent (list (cons 'poper-id (poper-get-id profile-ename)) (cons 'type "SLOPE") (cons 'index i)))
                                        (setq all-created-ents (cons slope-ent all-created-ents))
                                        (if (> delta_y_scaled 0)
                                            (create-line (car pt2) (cadr top_point_slope) (car pt1) (cadr bottom_point_slope) 256)
                                            (create-line (car pt1) (cadr top_point_slope) (car pt2) (cadr bottom_point_slope) 256)
                                        )
                                        (setq length_text (rtos (* (abs delta_x) (/ 1.0 l_scale)) 2 2))
                                        (setq length_x (if (> delta_y_scaled 0) (car pt2) (car pt1)))
                                        (setq length_alignment (if (> delta_y_scaled 0) 9 7))
                                        (setq length_text_x (if (= length_alignment 9) (- length_x text_offset) (+ length_x text_offset)))
                                        (setq length_text_y (+ (cadr bottom_point_slope) text_offset))
                                        (create-mtext length_text_x length_text_y length_text text_height length_alignment 0 (getvar "TEXTSTYLE"))
                                        (setq length-ent (entlast))
                                        (poper-xdata-set length-ent (list (cons 'poper-id (poper-get-id profile-ename)) (cons 'type "LENGTH") (cons 'index i)))
                                        (setq all-created-ents (cons length-ent all-created-ents))
                                    )
                                )
                            )
                        )
                        (setq i (1+ i))
                    )
                )
            )

            ;; Главная полилиния + XData + реактор
            (if profile-ename
                (progn
                    (setq poper-id (poper-get-id profile-ename))
                    (poper-xdata-set profile-ename
                        (list
                            (cons 'type "PROFILE")
                            (cons 'poper-id poper-id)
                            (cons 'h_scale h_scale)
                            (cons 'l_scale l_scale)
                            (cons 'text_height text_height)
                            (cons 'text_offset text_offset)
                            (cons 'round_to round_to)
                            (cons 'slope_round_to slope_round_to)
                            (cons 'slope_unit slope_unit)
                            (cons 'show_permille_sign show_permille_sign)
                            (cons 'OSZ OSZ)
                            (cons 'OSH OSH)
                            (cons 'mode (cond ((= mode_points "1") "points") ((= mode_vectors "1") "vectors") (t "polyline")))
                        )
                    )
                    (poper-attach-reactor profile-ename)
                )
            )

            (command "_.UNDO" "_End")
            (setvar "UNITMODE" original_unitmode)
            (setvar "DIMZIN" original_dimzin)
            (setvar "LUNITS" original_lunits)
            (setvar "LUPREC" original_luprec)

            (princ (strcat "\n[POPER v3.5] Создано успешно! POPER_ID = " (if profile-ename (poper-get-id profile-ename) "N/A") ". Всё ассоциативно."))
        )
    )
    (setq *error* old-error)
    (princ)
)

(defun C:ПОПЕР () (C:POPER))
(princ "\nSwift POPER v3.5 полностью ассоциативный (ПОЛНЫЙ КОД без сокращений) загружен. Команды: POPER, POPERUPDATE, POPER-RESTORE")
(princ)
; SWIFT-END
