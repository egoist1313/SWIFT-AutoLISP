; SWIFT-START
;; ========================================================
;; DPOD - Подготовка объектов разметки v5.2
;; Команды: DPOD или ДПОД
;; ========================================================
;; Исправления v5.2 (исчерпывающие):
;; - Корректное смещение TEXT->MTEXT (сдвиг вверх + AttachmentPoint)
;; - Убран сброс текущего слоя UCS при конверсии
;; - Убран двойной масштаб данных для размера (Measurement уже готовое значение)
;; - Убрано дублирование назначения якоря текста
;; - raw_part корректно вычисляется
;; - format-number надёжная функция для форматирования
;; - <> в MTEXT заменялось неверно
;; - исправлено условие диалога
;; - split_code теперь корректен для всех типов объектов
;; ========================================================
(defun make-dpod-dcl ( / dcl_file fp )
  (setq dcl_file (strcat (getvar "TEMPPREFIX") "DPOD_" (rtos (getvar "TDUSRTIMER") 2 6) ".dcl"))
  (if (setq fp (open dcl_file "w"))
    (progn
      (write-line "dpod_dialog : dialog {" fp)
      (write-line " label = \"ДПОД\";" fp)
      (write-line " width = 78;" fp)
      (write-line " : row {" fp)
      (write-line " : column {" fp)
      (write-line " : row {" fp)
      (write-line " : edit_box { key = \"prefix\"; label = \"Префикс:\"; edit_width = 12; value = \"л\"; }" fp)
      (write-line " : toggle { key = \"use_prefix\"; label = \"Добавить префикс\"; value = \"1\"; }" fp)
      (write-line " }" fp)
      (write-line " : row {" fp)
      (write-line " : edit_box { key = \"suffix\"; label = \"Суффикс:\"; edit_width = 12; value = \"\"; }" fp)
      (write-line " : toggle { key = \"use_suffix\"; label = \"Добавить суффикс\"; value = \"1\"; }" fp)
      (write-line " }" fp)
      (write-line " : radio_row {" fp)
      (write-line " : radio_button { key = \"mode_random\"; label = \"Случайное отклонение\"; value = \"1\"; }" fp)
      (write-line " : radio_button { key = \"mode_project\"; label = \"Проектный размер\"; value = \"0\"; }" fp)
      (write-line " }" fp)
      (write-line " : row {" fp)
      (write-line " : edit_box { key = \"min_dev\"; label = \"Отклонение от (м):\"; edit_width = 10; value = \"-0.3\"; }" fp)
      (write-line " : edit_box { key = \"max_dev\"; label = \"до (м):\"; edit_width = 10; value = \"0.2\"; }" fp)
      (write-line " }" fp)
      (write-line " : edit_box { key = \"actual_size\"; label = \"Проектный размер (верхняя строка):\"; edit_width = 25; value = \"\"; }" fp)
      (write-line " : toggle { key = \"underline_text\"; label = \"Подчеркнуть исходное в нижней\"; value = \"1\"; }" fp)
      (write-line " : toggle { key = \"single_line\"; label = \"Одна строка (изменить в первой строке)\"; value = \"0\"; }" fp)
      (write-line " : toggle { key = \"second_line\"; label = \"Изменить 2-ю строку (данные во второй строке)\"; value = \"0\"; }" fp)
      (write-line " : toggle { key = \"replace_mode\"; label = \"Режим замены (новое не зависит от исходного)\"; value = \"0\"; }" fp)
      (write-line " }" fp)
      (write-line " : boxed_column {" fp)
      (write-line " label = \"История\";" fp)
      (write-line " : list_box {" fp)
      (write-line " key = \"history_list\";" fp)
      (write-line " width = 35;" fp)
      (write-line " height = 14;" fp)
      (write-line " fixed_height = true;" fp)
      (write-line " }" fp)
      (write-line " }" fp)
      (write-line " }" fp)
      (write-line " spacer;" fp)
      (write-line " ok_cancel;" fp)
      (write-line "}" fp)
      (close fp)
      dcl_file
    )
    nil
  )
)
;; ==================== Вспомогательные функции ====================
;; Назначение: извлечь число и точность, считывая mtext-строку
(defun extract-number-and-precision (str / num prec decChar tmp dec_pos digits_after pos code end exact_num_str)
  (if (null str) (setq str ""))
  (princ (strcat "\nDEBUG extract: входная строка = [" str "]"))
  (setq tmp str)
  (while (setq pos (vl-string-search "\\" tmp))
    (setq code (substr tmp (1+ pos) 1))
    (if (member (strcase code) '("L" "l" "O" "o" "P" "X" "~" "K" "k" "U" "u"))
      (setq tmp (strcat (substr tmp 1 pos) (substr tmp (+ pos 2))))
      (progn
        (setq end (vl-string-search ";" tmp pos))
        (if end
          (setq tmp (strcat (substr tmp 1 pos) (substr tmp (+ end 2))))
          (setq tmp (strcat (substr tmp 1 pos) (substr tmp (+ pos 2))))
        )
      )
    )
  )
  (setq tmp (vl-string-trim " \t\r\n" tmp))
  (princ (strcat "\nDEBUG extract: после clean = [" tmp "]"))
  (while (and (> (strlen tmp) 0) (not (vl-string-search (substr tmp 1 1) "0123456789-.,+")))
    (setq tmp (substr tmp 2)))
  (while (and (> (strlen tmp) 1) (not (vl-string-search (substr tmp (strlen tmp) 1) "0123456789.,-")))
    (setq tmp (substr tmp 1 (1- (strlen tmp)))))
  (if (= tmp "") (setq tmp nil))
  (if tmp
    (progn
      (setq decChar ".")
      (if (vl-string-search "," tmp) (setq decChar ","))
      (setq exact_num_str tmp)
      (if (= decChar ",") (setq tmp (vl-string-subst "." "," tmp)))
      (setq num (distof tmp 2))
      (if num
        (progn
          (setq dec_pos (vl-string-search "." tmp))
          (setq digits_after (if dec_pos (- (strlen tmp) dec_pos 1) 0))
          (setq prec digits_after)
          (princ (strcat "\nDEBUG extract: успех! num=" (rtos num 2 6)))
          (list num prec decChar exact_num_str)
        )
        nil
      )
    )
    nil
  )
)
;; Назначение: удалить префикс / суффикс из строки текста
(defun clean-prefix-suffix (text pref suff use_p use_s / )
  (if (null text) (setq text ""))
  (setq text (vl-string-trim " \t\r\n" text))
  (if (and (= use_p 0) pref (/= pref ""))
    (if (and (> (strlen text) (strlen pref)) (= (substr text 1 (strlen pref)) pref))
      (setq text (substr text (1+ (strlen pref))))))
  (if (and (= use_s 0) suff (/= suff ""))
    (if (and (> (strlen text) (strlen suff)) (= (substr text (- (strlen text) (strlen suff) -1)) suff))
      (setq text (substr text 1 (- (strlen text) (strlen suff))))))
  text
)
;; Назначение: убрать \L \l из строки
(defun remove-underline (str)
  (if (null str) (setq str ""))
  (while (vl-string-search "\\L" str) (setq str (vl-string-subst "" "\\L" str)))
  (while (vl-string-search "\\l" str) (setq str (vl-string-subst "" "\\l" str)))
  str
)
;; Назначение: очистить от mtext-форматирования
(defun clean-mtext (str / tmp pos code end)
  (if (null str) (setq str ""))
  (setq tmp str)
  (while (setq pos (vl-string-search "\\" tmp))
    (setq code (substr tmp (1+ pos) 1))
    (if (member (strcase code) '("L" "l" "O" "o" "P" "X" "~" "K" "k" "U" "u"))
      (setq tmp (strcat (substr tmp 1 pos) (substr tmp (+ pos 2))))
      (progn
        (setq end (vl-string-search ";" tmp pos))
        (if end
          (setq tmp (strcat (substr tmp 1 pos) (substr tmp (+ end 2))))
          (setq tmp (strcat (substr tmp 1 pos) (substr tmp (+ pos 2))))
        )
      )
    )
  )
  tmp
)
;; Назначение: форматировать число в строку (надёжная замена rtos)
;; Параметры: val - число, prec - точность, decCh - разделитель ("." или ",")
;; Возвращает: отформатированную строку
(defun format-number (val prec decCh / str dot-pos)
  (setq str (rtos val 2 prec))
  (if (> prec 0)
    (progn
      (setq dot-pos (vl-string-search "." str))
      ;; Если точки нет, добавляем
      (if (null dot-pos)
        (progn
          (setq str (strcat str "."))
          (setq dot-pos (1- (strlen str)))
        )
      )
      ;; Добиваем нулями
      (while (< (- (strlen str) dot-pos 1) prec)
        (setq str (strcat str "0"))
      )
    )
  )
  ;; Заменяем разделитель
  (if (and decCh (/= decCh "."))
    (setq str (vl-string-subst decCh "." str))
  )
  str
)
;; Назначение: скорректированное значение
;; Параметры: base - исходное значение, rmin/rmax - диапазон, scale - масштаб,
;;            prec/decCh - параметры, repl - режим замены
;; Возвращает: отформатированную строку
(defun calc-adjusted-value (base rmin rmax scale prec decCh repl / rnd adj)
  (setq rnd (random-between rmin rmax))
  (setq adj (if (= repl 1) rnd (+ base rnd)))
  (format-number (* adj scale) prec decCh)
)
;; Назначение: конвертация TEXT -> MTEXT (корректное смещение + якорь)
;; ИСПРАВЛЕНИЕ №1+№2: точный сдвиг точки, устанавливаем AttachmentPoint=TopLeft,
;;                    сохраняем угол без перевычисления
(defun convert-text-to-mtext (textObj modelSpace / newMText insertPt height adjPt rot)
  (setq insertPt (vlax-get textObj 'InsertionPoint))
  (setq height   (vla-get-Height textObj))
  (setq rot      (vla-get-Rotation textObj))
  ;; TEXT стоит левее снизу, MTEXT - сверху слева.
  ;; Сдвиг вверх нужен так, чтобы baseline остался неизменным.
  ;; Если текст повёрнут, сдвиг тоже поворачивается: сдвиг перпендикулярен.
  (setq adjPt
    (list
      (- (car insertPt)  (* height (sin rot)))
      (+ (cadr insertPt) (* height (cos rot)))
      (caddr insertPt)
    )
  )
  (setq newMText
    (vla-AddMText modelSpace (vlax-3d-point adjPt) 0.0
                  (vla-get-TextString textObj))
  )
  (vla-put-AttachmentPoint newMText 1)   ;; 1 = TopLeft — якорь сверху слева
  (vla-put-StyleName  newMText (vla-get-StyleName textObj))
  (vla-put-Height     newMText height)
  (vla-put-Rotation   newMText rot)      ;; угол текста тот же — AutoCAD хранит в WCS
  (vla-put-Layer      newMText (vla-get-Layer textObj))
  (vla-put-Color      newMText (vla-get-Color textObj))
  (vla-Delete textObj)
  newMText
)
;; ==================== Генератор ====================
(defun random-between (a b / seed)
  (setq seed (+ (getvar "MILLISECS")
                (fix (* (getvar "TDUSRTIMER") 1000000.0))
                (random-seed-shift)))
  (+ a (* (- b a) (/ (rem (abs seed) 1000000000) 1000000000.0)))
)
(defun random-seed-shift ()
  (setq *random-counter* (if (boundp '*random-counter*) (1+ *random-counter*) 0))
  (* *random-counter* 123456789)
)
;; ==================== История ====================
(defun load-dpod-history ( / fp line hist)
  (setq hist '())
  (if (setq fp (open (strcat (getvar "TEMPPREFIX") "DPOD_History.txt") "r"))
    (progn (while (setq line (read-line fp)) (if line (setq hist (cons line hist)))) (close fp))
  )
  (reverse hist)
)
(defun add-to-dpod-history (is-project actual minDev maxDev / entry hist fp)
  (setq entry (if is-project
                (strcat "П: " (vl-string-trim " \t\r\n" (if actual actual ""))
                (strcat "Откл: " (rtos minDev 2 3) " .. " (rtos maxDev 2 3))))
  (setq hist (load-dpod-history))
  (if (not (member entry hist))
    (progn
      (setq hist (cons entry hist))
      (if (> (length hist) 10) (setq hist (reverse (cdr (reverse hist)))))
      (if (setq fp (open (strcat (getvar "TEMPPREFIX") "DPOD_History.txt") "w"))
        (progn (foreach item hist (write-line item fp)) (close fp))
      )
      (setq *dpod-history* hist)
    )
  )
)
(defun update-history-list ( / filtered mode)
  (setq mode (atoi (get_tile "mode_random")))
  (setq filtered (vl-remove-if-not
                   (if (= mode 1)
                     '(lambda (x) (vl-string-search "Откл:" x))
                     '(lambda (x) (vl-string-search "П:" x)))
                   *dpod-history*))
  (setq *dpod-current-list* filtered)
  (start_list "history_list")
  (mapcar 'add_list filtered)
  (end_list)
)
(defun fill-from-history (idx / hist entry colonpos)
  (setq hist *dpod-current-list*)
  (if (and (>= idx 0) (< idx (length hist)))
    (progn
      (setq entry (nth idx hist))
      (cond
        ((vl-string-search "Откл:" entry)
         (set_tile "mode_random" "1")
         (set_tile "mode_project" "0")
         (setq colonpos (vl-string-search ":" entry))
         (set_tile "min_dev" (vl-string-trim " \t" (substr entry (+ colonpos 2) (- (vl-string-search ".." entry) (+ colonpos 2)))))
         (set_tile "max_dev" (vl-string-trim " \t" (substr entry (+ (vl-string-search ".." entry) 3))))
        )
        ((vl-string-search "П:" entry)
         (set_tile "mode_random" "0")
         (set_tile "mode_project" "1")
         (setq colonpos (vl-string-search ":" entry))
         (set_tile "actual_size" (vl-string-trim " \t" (substr entry (+ colonpos 2))))
        )
      )
      (update-history-list)
    )
  )
)
;; ==================== Основная функция ====================
(defun c:DPOD ( / dcl_id dcl_file prefix suffix minDev maxDev underline single replace_mode
                use_prefix use_suffix actual_size mode_random mode_project second_line
                objList i obj meas_m random_m adj_m_in_meters
                precision decChar formattedText old_num_str final_prefix final_suffix
                fullText split_pos rest split_code original_content raw_original_content cleaned_orig
                num_and_prec processed scaleFactor doc modelSpace replaced new_first_line
                p_pos x_pos min_str max_str override is_dim_text_like pos formatted has_decimal
                is_project_mode has_custom_text underline_applicable upper_str lower_text lower_str underline_on
                first_part new_second_line second_raw second_cleaned second_info new_second
                second_meas_m second_precision second_decChar second_old_num_str second_cleaned_orig
                second_random_m second_adj_m_in_meters second_formatted
                second_formattedText second_pos
                first_formattedText first_cleaned_orig first_old_num_str first_random_m first_formatted
                first_meas_m first_precision first_decChar visible_text
                raw_part first_raw_part first_num_and_prec second_num_and_prec
                *dpod-history* *dpod-current-list*)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq modelSpace (vla-get-ModelSpace doc))
  (if (not (setq dcl_file (make-dpod-dcl)))
    (progn (princ "\nОшибка создания DCL!\n") (exit))
  )
  (setq dcl_id (load_dialog dcl_file))
  ;; ИСПРАВЛЕНИЕ №8: единое простое условие открытия диалога (без двойной проверки)
  (if (and (> dcl_id 0) (new_dialog "dpod_dialog" dcl_id))
    (progn
      (setq *dpod-history* (load-dpod-history))
      (update-history-list)
      (set_tile "prefix"       (cond ((getenv "DIMFUDGE_PREFIX"))      ("л")))
      (set_tile "suffix"       (cond ((getenv "DIMFUDGE_SUFFIX"))      ("")))
      (set_tile "min_dev"      (cond ((getenv "DIMFUDGE_MIN_DEV"))     ("-0.3")))
      (set_tile "max_dev"      (cond ((getenv "DIMFUDGE_MAX_DEV"))     ("0.2")))
      (set_tile "underline_text" (cond ((getenv "DIMFUDGE_UNDERLINE")) ("1")))
      (set_tile "single_line"  (cond ((getenv "DIMFUDGE_SINGLE"))      ("0")))
      (set_tile "second_line"  (cond ((getenv "DIMFUDGE_SECOND_LINE")) ("0")))
      (set_tile "replace_mode" (cond ((getenv "DIMFUDGE_REPLACE"))     ("0")))
      (set_tile "use_prefix"   (cond ((getenv "DIMFUDGE_USE_PREFIX"))  ("1")))
      (set_tile "use_suffix"   (cond ((getenv "DIMFUDGE_USE_SUFFIX"))  ("1")))
      (set_tile "actual_size"  (cond ((getenv "DIMFUDGE_ACTUAL_SIZE")) ("")))
      (set_tile "mode_random"  (cond ((getenv "DIMFUDGE_MODE_RANDOM")) ("1")))
      (set_tile "mode_project" (cond ((getenv "DIMFUDGE_MODE_PROJECT")) ("0")))
      (action_tile "mode_random"
        "(update-history-list) (mode_tile \"single_line\" 0) (mode_tile \"second_line\" 0) (mode_tile \"replace_mode\" 0)")
      (action_tile "mode_project"
        "(update-history-list) (mode_tile \"single_line\" 1) (mode_tile \"second_line\" 1) (mode_tile \"replace_mode\" 1)")
      (action_tile "single_line" "")
      (action_tile "second_line" "")
      (action_tile "history_list"
        "(if (get_tile \"history_list\") (fill-from-history (atoi (get_tile \"history_list\"))))")
      (action_tile "accept"
        "(setq prefix (get_tile \"prefix\")
               suffix (get_tile \"suffix\")
               min_str (get_tile \"min_dev\")
               max_str (get_tile \"max_dev\")
               actual_size (get_tile \"actual_size\")
               underline (atoi (get_tile \"underline_text\"))
               single (atoi (get_tile \"single_line\"))
               second_line (atoi (get_tile \"second_line\"))
               replace_mode (atoi (get_tile \"replace_mode\"))
               use_prefix (atoi (get_tile \"use_prefix\"))
               use_suffix (atoi (get_tile \"use_suffix\"))
               mode_random (atoi (get_tile \"mode_random\"))
               mode_project (atoi (get_tile \"mode_project\")))
         (if (vl-string-search \",\" min_str) (setq min_str (vl-string-subst \".\" \",\" min_str)))
         (if (vl-string-search \",\" max_str) (setq max_str (vl-string-subst \".\" \",\" max_str)))
         (setq minDev (atof min_str) maxDev (atof max_str))
         (if (= mode_project 1) (setq single 0 second_line 0 replace_mode 0))
         (setenv \"DIMFUDGE_PREFIX\" prefix)
         (setenv \"DIMFUDGE_SUFFIX\" suffix)
         (setenv \"DIMFUDGE_MIN_DEV\" (get_tile \"min_dev\"))
         (setenv \"DIMFUDGE_MAX_DEV\" (get_tile \"max_dev\"))
         (setenv \"DIMFUDGE_UNDERLINE\" (get_tile \"underline_text\"))
         (setenv \"DIMFUDGE_SINGLE\" (get_tile \"single_line\"))
         (setenv \"DIMFUDGE_SECOND_LINE\" (get_tile \"second_line\"))
         (setenv \"DIMFUDGE_REPLACE\" (get_tile \"replace_mode\"))
         (setenv \"DIMFUDGE_USE_PREFIX\" (get_tile \"use_prefix\"))
         (setenv \"DIMFUDGE_USE_SUFFIX\" (get_tile \"use_suffix\"))
         (setenv \"DIMFUDGE_ACTUAL_SIZE\" actual_size)
         (setenv \"DIMFUDGE_MODE_RANDOM\" (get_tile \"mode_random\"))
         (setenv \"DIMFUDGE_MODE_PROJECT\" (get_tile \"mode_project\"))
         (done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")
      (if (= (start_dialog) 1)
        (progn
          (unload_dialog dcl_id)
          (vl-file-delete dcl_file)
          (add-to-dpod-history (= mode_project 1) actual_size minDev maxDev)
          (princ "\nВыберите размеры, тексты, мтекст и многовыносные: ")
          (setq objList (ssget '((0 . "TEXT,MTEXT,DIMENSION,MULTILEADER"))))
          (if objList
            (progn
              (setq i 0)
              (repeat (sslength objList)
                (setq obj (vlax-ename->vla-object (ssname objList i)))
                (setq processed nil scaleFactor 1.0 is_dim_text_like nil is_project_mode nil)
                (setq actual_size   (if (null actual_size)   "" actual_size))
                (setq prefix        (if (null prefix)        "" prefix))
                (setq suffix        (if (null suffix)        "" suffix))
                (setq cleaned_orig "" final_prefix "" final_suffix ""
                      first_formattedText nil second_formattedText nil
                      raw_part nil first_raw_part nil
                      first_num_and_prec nil second_num_and_prec nil
                      first_meas_m 0.0 first_precision 0 first_decChar "." first_old_num_str "" first_cleaned_orig ""
                      second_meas_m 0.0 second_precision 0 second_decChar "." second_old_num_str "" second_cleaned_orig ""
                      first_random_m 0.0 second_random_m 0.0)
                (princ (strcat "\n\n=== DEBUG объект " (itoa (1+ i)) " ==="))
                (princ (strcat "\nDEBUG настройки > режим: "
                               (if (= mode_project 1) "проектный" "случайный")
                               " | single=" (itoa single)
                               " second_line=" (itoa second_line)
                               " | actual_size=[" actual_size
                               "] prefix=[" prefix "] suffix=[" suffix "]"))
                (princ (strcat "\nDEBUG ObjectName = " (vla-get-ObjectName obj)))
                ;; ==================== Считывание данных ====================
                (cond
                  ((wcmatch (vla-get-ObjectName obj) "AcDb*Dimension")
                   (setq override (vla-get-TextOverride obj))
                   (princ (strcat "\nDEBUG override = [" (if override override "nil") "]"))
                   (if (and override (/= override ""))
                     (progn
                       ;; Если есть override — читаем его как текст объекта
                       (setq is_dim_text_like t)
                       (setq fullText override)
                       (setq processed t)
                       (setq scaleFactor 1.0)
                       ;; Получаем meas_m/precision/decChar из размера — нужны для случайного режима
                       (setq meas_m (vla-get-Measurement obj))
                       (setq precision
                         (cond
                           ((vlax-property-available-p obj 'TextPrecision)
                            (vlax-get obj 'TextPrecision))
                           ((vlax-property-available-p obj 'PrimaryUnitsPrecision)
                            (vlax-get obj 'PrimaryUnitsPrecision))
                           (t (getvar "DIMDEC"))
                         )
                       )
                       (setq decChar
                         (if (vlax-property-available-p obj 'DecimalSeparator)
                           (vlax-get obj 'DecimalSeparator)
                           "."
                         )
                       )
                       (princ "\nDEBUG: override обрабатывается")
                     )
                     (progn
                       ;; ИСПРАВЛЕНИЕ №4: Measurement уже готовое масштабированное значение.
                       ;; Не нужно ещё раз умножать на scaleFactor — это двойной масштаб.
                       (setq meas_m (vla-get-Measurement obj))
                       (setq scaleFactor 1.0)
                       (setq precision
                         (cond
                           ((vlax-property-available-p obj 'TextPrecision)
                            (vlax-get obj 'TextPrecision))
                           ((vlax-property-available-p obj 'PrimaryUnitsPrecision)
                            (vlax-get obj 'PrimaryUnitsPrecision))
                           (t (getvar "DIMDEC"))
                         )
                       )
                       (setq decChar
                         (if (vlax-property-available-p obj 'DecimalSeparator)
                           (vlax-get obj 'DecimalSeparator)
                           "."
                         )
                       )
                       (setq processed t)
                       (setq fullText "<>")
                       (if (= mode_project 1)
                         (setq is_project_mode t)
                       )
                       (princ (strcat "\nDEBUG: обычный размер meas_m=" (rtos meas_m 2 6)))
                     )
                   )
                  )
                  (t
                   ;; TEXT, MTEXT, MULTILEADER
                   (setq fullText (vla-get-TextString obj))
                   (setq processed t)
                   (princ "\nDEBUG: текст/мтекст/многовыносной")
                  )
                )
                (princ (strcat "\nDEBUG fullText = [" (if fullText fullText "nil") "]"))
                ;; ==================== Поиск разделителя: \P (MTEXT) или \X (DIMENSION) ====================
                (setq p_pos (vl-string-search "\\P" (if fullText fullText "")))
                (setq x_pos (vl-string-search "\\X" (if fullText fullText "")))
                (setq split_pos
                  (cond
                    ((and p_pos x_pos) (min p_pos x_pos))
                    (p_pos p_pos)
                    (x_pos x_pos)
                    (t nil)
                  )
                )
                ;; ИСПРАВЛЕНИЕ (split_code): выбрать правильный тег для всех типов объектов
                (if split_pos
                  (progn
                    (setq first_part (substr (if fullText fullText "") 1 split_pos))
                    (setq rest       (substr (if fullText fullText "") (+ split_pos 3)))
                    ;; Определяем: для DIMENSION нужен \X, для MTEXT — \P
                    (setq split_code
                      (if (wcmatch (vla-get-ObjectName obj) "AcDb*Dimension")
                        "\\X"
                        "\\P"
                      )
                    )
                  )
                  (progn
                    (setq first_part (if fullText fullText ""))
                    (setq rest "")
                    (setq split_code
                      (if (wcmatch (vla-get-ObjectName obj) "AcDb*Dimension") "\\X" "\\P")
                    )
                  )
                )
                (princ (strcat "\nDEBUG first_part = [" first_part "]"))
                ;; Выбрать часть строки для обработки
                (cond
                  ((and (= second_line 1) (null split_pos))
                   (princ "\nDEBUG: нет второй строки!")
                   (setq processed nil)
                  )
                  ((and (= second_line 1) split_pos)
                   (setq raw_part (substr (if fullText fullText "") (+ split_pos 3)))
                   (princ "\nDEBUG: обрабатываем вторую строку")
                  )
                  (t
                   (setq raw_part
                     (if split_pos
                       (substr (if fullText fullText "") 1 split_pos)
                       (if fullText fullText ""))
                     )
                   )
                   (princ "\nDEBUG: обрабатываем первую строку")
                  )
                )
                (princ (strcat "\nDEBUG raw_part = [" (if raw_part raw_part "") "]"))
                ;; ==================== Извлечение числа ====================
                (if (and processed (wcmatch (vla-get-ObjectName obj) "AcDb*Dimension"))
                  (progn
                    ;; Для DIMENSION: если raw_part содержит явное число (override с числом) —
                    ;; парсим его как текст. Если raw_part = "" или "<>" — берём meas_m от размера.
                    (setq raw_original_content (vl-string-trim " \t\r\n" (if raw_part raw_part "")))
                    (if (or (= raw_original_content "")
                            (= raw_original_content "<>"))
                      (progn
                        ;; Нет явное число — используем измерение размера
                        (setq cleaned_orig "<>")
                        (setq old_num_str "<>")
                        (setq raw_original_content "<>")
                        (setq processed t)
                        (princ "\nDEBUG размер: raw_part пустой или <>, используем meas_m")
                      )
                      (progn
                        ;; Есть явное число в override — парсим как текст
                        (setq original_content (remove-underline raw_original_content))
                        (setq num_and_prec (extract-number-and-precision original_content))
                        (if num_and_prec
                          (progn
                            (setq meas_m      (car num_and_prec)
                                  precision   (cadr num_and_prec)
                                  decChar     (caddr num_and_prec)
                                  old_num_str (cadddr num_and_prec)
                                  cleaned_orig (clean-mtext original_content))
                            (setq processed t)
                            (princ (strcat "\nDEBUG размер override с числом: meas_m=" (rtos meas_m 2 6)
                                           " cleaned_orig=[" cleaned_orig "]"))
                          )
                          (progn
                            ;; Не распознали число — откатываемся к meas_m от размера
                            (setq cleaned_orig "<>")
                            (setq old_num_str "<>")
                            (setq raw_original_content "<>")
                            (setq processed t)
                            (princ "\nDEBUG размер: число не распознали, используем meas_m")
                          )
                        )
                      )
                    )
                  )
                  (progn
                    ;; Для TEXT/MTEXT/MULTILEADER — парсим число из строки
                    (setq raw_original_content (vl-string-trim " \t\r\n" (if raw_part raw_part "")))
                    (setq original_content (remove-underline raw_original_content))
                    (setq num_and_prec (extract-number-and-precision original_content))
                    (if num_and_prec
                      (progn
                        (setq meas_m    (car num_and_prec)
                              precision (cadr num_and_prec)
                              decChar   (caddr num_and_prec)
                              old_num_str (cadddr num_and_prec)
                              cleaned_orig (clean-mtext original_content))
                        (setq processed t)
                        (princ (strcat "\nDEBUG успешно распознано: cleaned_orig=[" cleaned_orig "]"))
                      )
                      (progn
                        (princ "\nDEBUG пропускаем: нет числа")
                        (setq processed nil)
                      )
                    )
                  )
                )
                ;; ==================== Защита от NIL ====================
                (if (null cleaned_orig)          (setq cleaned_orig ""))
                (if (null old_num_str)           (setq old_num_str ""))
                (if (null raw_original_content)  (setq raw_original_content ""))
                (setq final_prefix (if (= use_prefix 1) prefix ""))
                (setq final_suffix (if (= use_suffix 1) suffix ""))
                (princ (strcat "\nDEBUG final_prefix=[" final_prefix "] final_suffix=[" final_suffix "]"))
                ;; ==================== Убрать дубль префикса / суффикса (визуально) ====================
                (setq visible_text
                  (clean-mtext (remove-underline (if raw_original_content raw_original_content ""))))
                (if (and (= use_prefix 1)
                         (> (strlen visible_text) (strlen prefix))
                         (= (substr visible_text 1 (strlen prefix)) prefix))
                  (progn
                    (setq final_prefix "")
                    (princ "\nDEBUG: префикс уже есть и не добавляется")
                  )
                )
                (if (and (= use_suffix 1)
                         (> (strlen visible_text) (strlen suffix))
                         (= (substr visible_text (- (strlen visible_text) (strlen suffix) -1)) suffix))
                  (progn
                    (setq final_suffix "")
                    (princ "\nDEBUG: суффикс уже есть и не добавляется")
                  )
                )
                ;; ==================== Обработка: обоих строк (%5+%6) ====================
                (if (and processed (= single 1) (= second_line 1) split_pos)
                  (progn
                    ;; --- Первая строка ---
                    (setq first_raw_part (substr (if fullText fullText "") 1 split_pos))
                    (setq first_num_and_prec
                      (extract-number-and-precision
                        (remove-underline (vl-string-trim " \t\r\n" first_raw_part))))
                    (if first_num_and_prec
                      (progn
                        (setq first_meas_m      (car first_num_and_prec)
                              first_precision   (cadr first_num_and_prec)
                              first_decChar     (caddr first_num_and_prec)
                              first_old_num_str (cadddr first_num_and_prec)
                              first_cleaned_orig
                                (clean-mtext
                                  (remove-underline
                                    (vl-string-trim " \t\r\n" first_raw_part))))
                      )
                      (progn
                        (setq first_meas_m      meas_m
                              first_precision   precision
                              first_decChar     decChar
                              first_old_num_str "<>"
                              first_cleaned_orig "<>")
                      )
                    )
                    (if (null first_meas_m)      (setq first_meas_m meas_m))
                    (if (null first_precision)   (setq first_precision precision))
                    (if (null first_decChar)     (setq first_decChar decChar))
                    (if (null first_cleaned_orig)(setq first_cleaned_orig ""))
                    (princ (strcat "\nDEBUG первая строка meas_m=" (rtos first_meas_m 2 6)))
                    ;; ИСПРАВЛЕНИЕ №4/#5: для размера scaleFactor=1.0, двойной масштаб нет
                    (if (wcmatch (vla-get-ObjectName obj) "AcDb*Dimension")
                      (progn
                        ;; Измерение: первая строка = измерение, отклонение = 0
                        (setq first_formatted
                          (format-number first_meas_m first_precision first_decChar))
                        (princ "\nDEBUG: размер > первая строка = измерение")
                      )
                      (progn
                        ;; ИСПРАВЛЕНИЕ №5: scaleFactor=1.0 для TEXT/MTEXT, двойной масштаб нет
                        (setq first_formatted
                          (calc-adjusted-value
                            first_meas_m minDev maxDev 1.0
                            first_precision first_decChar replace_mode))
                      )
                    )
                    (setq first_formattedText
                      (vl-string-subst first_formatted first_old_num_str first_raw_part))
                    (setq first_formattedText
                      (clean-prefix-suffix first_formattedText prefix suffix use_prefix use_suffix))
                    (if (and (= use_prefix 1) (/= final_prefix ""))
                      (setq first_formattedText (strcat final_prefix first_formattedText)))
                    (if (and (= use_suffix 1) (/= final_suffix ""))
                      (setq first_formattedText (strcat first_formattedText final_suffix)))
                    (princ (strcat "\nDEBUG первая строка итого: " first_formattedText))
                    ;; --- Вторая строка ---
                    (setq second_raw (substr (if fullText fullText "") (+ split_pos 3)))
                    (setq second_num_and_prec
                      (extract-number-and-precision
                        (remove-underline (vl-string-trim " \t\r\n" second_raw))))
                    (if second_num_and_prec
                      (progn
                        (setq second_meas_m      (car second_num_and_prec)
                              second_precision   (cadr second_num_and_prec)
                              second_decChar     (caddr second_num_and_prec)
                              second_old_num_str (cadddr second_num_and_prec)
                              second_cleaned_orig
                                (clean-mtext
                                  (remove-underline
                                    (vl-string-trim " \t\r\n" second_raw))))
                      )
                      (progn
                        (setq second_meas_m      meas_m
                              second_precision   precision
                              second_decChar     decChar
                              second_old_num_str old_num_str
                              second_cleaned_orig cleaned_orig)
                      )
                    )
                    (if (null second_meas_m)      (setq second_meas_m meas_m))
                    (if (null second_precision)   (setq second_precision precision))
                    (if (null second_decChar)     (setq second_decChar decChar))
                    (if (null second_cleaned_orig)(setq second_cleaned_orig ""))
                    ;; ИСПРАВЛЕНИЕ №5: scaleFactor=1.0 — двойной масштаб нет
                    (setq second_formatted
                      (calc-adjusted-value
                        second_meas_m minDev maxDev 1.0
                        second_precision second_decChar replace_mode))
                    (setq second_formattedText
                      (vl-string-subst second_formatted second_old_num_str second_raw))
                    (setq second_formattedText
                      (clean-prefix-suffix second_formattedText prefix suffix use_prefix use_suffix))
                    (if (and (= use_prefix 1) (/= final_prefix ""))
                      (setq second_formattedText (strcat final_prefix second_formattedText)))
                    (if (and (= use_suffix 1) (/= final_suffix ""))
                      (setq second_formattedText (strcat second_formattedText final_suffix)))
                    (princ (strcat "\nDEBUG вторая строка итого: " second_formattedText))
                  )
                )
                ;; ==================== Обработка нижней части (первая строка + \X) ====================
                ;; Только для: если есть, если <> + \X присутствует
                (if (and processed
                         (wcmatch (vla-get-ObjectName obj) "AcDb*Dimension")
                         (not (= mode_project 1))
                         (not (and (= single 1) (= second_line 1) split_pos))
                         (vl-string-search "<>" fullText)
                         (vl-string-search "\\X" fullText))
                  (progn
                    (princ "\nDEBUG: обрабатываем <> + \\X")
                    ;; ИСПРАВЛЕНИЕ №5: scaleFactor=1.0, двойной масштаб нет
                    (setq formattedText
                      (strcat final_prefix
                              (calc-adjusted-value meas_m minDev maxDev 1.0 precision decChar replace_mode)
                              final_suffix))
                  )
                )
                ;; ==================== Применение ====================
                (if processed
                  (progn
                    (if (and (= mode_project 1) (/= (vl-string-trim " \t\r\n" actual_size) ""))
                      (progn
                        ;; --- Проектный режим ---
                        (setq is_project_mode t)
                        (princ "\nDEBUG --- проектный режим ---")
                        (setq has_custom_text
                          (or (not (wcmatch (vla-get-ObjectName obj) "AcDb*Dimension"))
                              is_dim_text_like))
                        (setq underline_applicable
                          (or (wcmatch (vla-get-ObjectName obj) "AcDbText,AcDbMText")
                              (and (wcmatch (vla-get-ObjectName obj) "AcDb*Dimension")
                                   is_dim_text_like)))
                        (setq upper_str actual_size)
                        (setq underline_on (and (= underline 1) underline_applicable))
                        ;; ИСПРАВЛЕНИЕ (см. пункт 2): lower_text для DIMENSION должен <>,
                        ;; для MTEXT/TEXT — реальное значение
                        (if (wcmatch (vla-get-ObjectName obj) "AcDb*Dimension")
                          (progn
                            ;; Для размера <> — AutoCAD подставит измерение
                            (setq lower_text (strcat final_prefix "<>" final_suffix))
                          )
                          (progn
                            ;; Для MTEXT/TEXT <> бессмысленно: пишем реальное отформатированное значение
                            (setq lower_text
                              (strcat final_prefix
                                      (format-number meas_m precision decChar)
                                      final_suffix))
                          )
                        )
                        (princ (strcat "\nDEBUG lower_text = [" lower_text "]"))
                        (setq lower_str
                          (if underline_on
                            (strcat "\\L" lower_text "\\l")
                            lower_text
                          )
                        )
                      )
                      (progn
                        ;; --- Случайный режим ---
                        (setq is_project_mode nil)
                        (if (not (and (= single 1) (= second_line 1) split_pos))
                          (progn
                            ;; ИСПРАВЛЕНИЕ №5: scaleFactor=1.0 — двойной масштаб нет
                            (setq formattedText
                              (strcat final_prefix
                                      (calc-adjusted-value meas_m minDev maxDev 1.0 precision decChar replace_mode)
                                      final_suffix))
                            (princ (strcat "\nDEBUG случайный: formattedText = [" formattedText "]"))
                          )
                        )
                      )
                    )
                    ;; ==================== Запись ====================
                    (princ "\nDEBUG === применяем изменение ===")
                    (cond
                      ;; --- DIMENSION ---
                      ((wcmatch (vla-get-ObjectName obj) "AcDb*Dimension")
                       (if is_project_mode
                         (progn
                           (princ (strcat "\nDEBUG запись размера (dimension): ["
                                          (strcat upper_str "\\X" lower_str) "]"))
                           (vla-put-TextOverride obj (strcat upper_str "\\X" lower_str))
                         )
                         (cond
                           ((and (= single 1) (= second_line 1) split_pos)
                            (princ (strcat "\nDEBUG обе строки (dimension): "
                                           first_formattedText split_code second_formattedText))
                            (vla-put-TextOverride obj
                              (strcat
                                (if (= underline 1) "\\L" "") first_formattedText (if (= underline 1) "\\l" "")
                                split_code
                                (if (= underline 1) "\\L" "") second_formattedText (if (= underline 1) "\\l" "")))
                           )
                           ((= single 1)
                            (setq replaced (vl-string-subst formattedText old_num_str cleaned_orig))
                            (vla-put-TextOverride obj
                              (strcat
                                (if (= underline 1) "\\L" "") replaced (if (= underline 1) "\\l" "")
                                (if (and rest (/= rest "")) (strcat split_code rest) "")))
                           )
                           ((= second_line 1)
                            (setq new_second_line (vl-string-subst formattedText old_num_str cleaned_orig))
                            (vla-put-TextOverride obj
                              (strcat first_part split_code
                                      (if (= underline 1) "\\L" "")
                                      new_second_line
                                      (if (= underline 1) "\\l" "")))
                           )
                           (t
                            ;; Обычный режим: <> \X скорректированный_текст
                            (if (vl-string-search "<>" fullText)
                              (progn
                                (princ (strcat "\nDEBUG <> > override: <> \\X " formattedText))
                                (vla-put-TextOverride obj (strcat "<>" "\\X" formattedText))
                              )
                              (if is_dim_text_like
                                (vla-put-TextOverride obj
                                  (strcat
                                    (if (= underline 1) "\\L" "") cleaned_orig (if (= underline 1) "\\l" "")
                                    "\\X" formattedText))
                                (vla-put-TextOverride obj (strcat "<>" "\\X" formattedText))
                              )
                            )
                           )
                         )
                       )
                      )
                      ;; --- TEXT / MTEXT / MULTILEADER ---
                      ((wcmatch (vla-get-ObjectName obj) "AcDbText,AcDbMText,AcDbMLeader*")
                       ;; ИСПРАВЛЕНИЕ №1+№2: конвертировать TEXT -> MTEXT правильно
                       (if (= (vla-get-ObjectName obj) "AcDbText")
                         (setq obj (convert-text-to-mtext obj modelSpace))
                       )
                       ;; MULTILEADER: настройка выравнивания
                       (if (wcmatch (vla-get-ObjectName obj) "AcDbMLeader*")
                         (progn
                           (if (vlax-property-available-p obj 'TextRightAttachmentType)
                             (vla-put-TextRightAttachmentType obj 1))
                           (if (vlax-property-available-p obj 'TextAlignmentType)
                             (vla-put-TextAlignmentType obj 2))
                         )
                       )
                       ;; ИСПРАВЛЕНИЕ №3: AttachmentPoint не должен переопределяться у MTEXT.
                       ;; Для преобразованных MTEXT расположение верное — необходимости нет.
                       ;; convert-text-to-mtext уже устанавливает AttachmentPoint=1 (TopLeft).
                       (if is_project_mode
                         (progn
                           (princ (strcat "\nDEBUG запись мтекст (проект): ["
                                          (strcat "\\A2;" upper_str "\\P" lower_text) "]"))
                           (vla-put-TextString obj (strcat "\\A2;" upper_str "\\P" lower_text))
                         )
                         (cond
                           ((and (= single 1) (= second_line 1) split_pos)
                            (vla-put-TextString obj
                              (strcat "\\A2;"
                                      (if (= underline 1) "\\L" "") first_formattedText  (if (= underline 1) "\\l" "")
                                      split_code
                                      (if (= underline 1) "\\L" "") second_formattedText (if (= underline 1) "\\l" "")))
                           )
                           ((= single 1)
                            (setq new_first_line
                              (vl-string-subst formattedText old_num_str raw_original_content))
                            (vla-put-TextString obj
                              (strcat "\\A2;"
                                      (if (= underline 1) "\\L" "") new_first_line (if (= underline 1) "\\l" "")
                                      (if (and rest (/= rest "")) (strcat split_code rest) "")))
                           )
                           ((= second_line 1)
                            (setq new_second_line
                              (vl-string-subst formattedText old_num_str raw_original_content))
                            (vla-put-TextString obj
                              (strcat "\\A2;" first_part split_code
                                      (if (= underline 1) "\\L" "")
                                      new_second_line
                                      (if (= underline 1) "\\l" "")))
                           )
                           (t
                            ;; Обычный режим: исходный текст \P скорректированный
                            (vla-put-TextString obj
                              (strcat "\\A2;"
                                      (if (= underline 1) "\\L" "")
                                      cleaned_orig
                                      (if (= underline 1) "\\l" "")
                                      "\\P" formattedText))
                           )
                         )
                       )
                      )
                    )
                    (vla-update obj)
                    (princ "\nDEBUG === изменение применено ===")
                    (if is_project_mode
                      (princ (strcat "\nБыло исходное: " (rtos meas_m 2 6)
                                     " > [проект: " actual_size "]"))
                      (if (and (= single 1) (= second_line 1))
                        (princ (strcat "\nБыло (обе строки): 1-я=" first_formattedText
                                       " | 2-я=" second_formattedText))
                        (princ (strcat "\nБыло: " (rtos meas_m 2 6)
                                       " > " (if formattedText formattedText "?")))
                      )
                    )
                  )
                )
                (setq i (1+ i))
              )
              (princ "\n\nГотово! Обработка завершена (v5.2).\n")
            )
            (princ "\nОбъекты не выбраны.\n")
          )
        )
      )
    )
    (progn
      (if (> dcl_id 0) (unload_dialog dcl_id))
      (if dcl_file (vl-file-delete dcl_file))
      (princ "\nНе удалось открыть диалог.\n")
    )
  )
  (princ)
)
;; Русский алиас
(defun c:ДПОД () (c:DPOD))
(princ "\nDPOD v5.2 загружен!")
; SWIFT-END
